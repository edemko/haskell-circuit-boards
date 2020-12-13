{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

-- FIXME this is a lot of layers of abstraction in one file
module Language.Gerber.Stackup where

import Language.Gerber.Data

import Control.Monad (forM_,when,unless)
import Control.Monad.State (State,gets,modify,execState)
import Data.Bifunctor (second)
import Language.Gerber.Antiparse (antiparse)
import Language.Gerber.Syntax (Command)
import Language.Gerber.Writer (Brush,GbrWriter,GbrWriterMonad(..),execGbrWriter)

data LayerInfo = LayerInfo
    { ty :: LayerType
    , name :: String -- NOTE used as filepath
    }

-- FIXME use a GADT to index for what part of the stackup each layer is valid in
data LayerType
    = Profile
    | PlatedDrill
    -- TODO more drill layer types
    | SignalCopper
    -- PlaneCopper
    | SolderMask
    -- TODO silkscreen

data Stackup a = Stackup
    { profile :: (LayerInfo, a)
    , mechanical :: [(LayerInfo, a)]
    , topSurface :: [(LayerInfo, a)]
    , copper :: [(LayerInfo, a)]
    , botSurface :: [(LayerInfo, a)]
    }


-- -- TODO allow configuration of the number of various kinds of layers
-- -- but for now, a two-layer pcb is sufficient for my purposes
-- data Stackup = Stackup
--     -- TODO a list of top silkscreen layers
--     { topSoldermask :: [Command]
--     , topCopper :: [Command]
--     -- TODO a list of internal copper layers
--     , botCopper :: [Command]
--     , botSoldermask :: [Command]
--     -- TODO a list of bottom silkscreen layers
--     , profile :: [Command]
--     , platedDrill :: [Command]
--     }


data BoardSide = Top | Bot
data LeadPin = N | S | E | W


writeToDir :: FilePath -> Stackup [Command] -> IO ()
writeToDir dirpath stackup = do
    writeLayer stackup.profile
    writeFile (filepath stackup.profile._1.name) (antiparse $ stackup.profile._2)
    mapM_ writeLayer stackup.mechanical
    mapM_ writeLayer stackup.copper
    mapM_ writeLayer stackup.topSurface
    mapM_ writeLayer stackup.botSurface
    where
    filepath name = concat [dirpath, "/", name, ".gbr"]
    writeLayer layer = writeFile (filepath layer._1.name) (antiparse layer._2)


------------------------------------





newtype PcbLayout a = PcbLayout { unlayout :: State LayoutState a }
    deriving (Functor, Applicative, Monad)

data LayoutState = St
    { _here :: (Float, Float) -- FIXME this is a higher layer of abstraction
    , _layers :: Stackup (GbrWriter ())
    }

runPcbLayout :: GbrWriter () -> PcbLayout a -> Stackup [Command]
runPcbLayout commonLayout layout =
    let s = execState (unlayout layout) (emptyLayout commonLayout)
    in Stackup
        { profile = second execGbrWriter s._layers.profile
        , mechanical = second execGbrWriter <$> s._layers.mechanical
        , copper = second execGbrWriter <$> s._layers.copper
        , topSurface = second execGbrWriter <$> s._layers.topSurface
        , botSurface = second execGbrWriter <$> s._layers.botSurface
        }

emptyLayout :: GbrWriter () -> LayoutState
emptyLayout commonLayout = St
    { _here = (0, 0)
    , _layers = Stackup -- FIXME this set of layers should be configured somewhere
        { profile = (LayerInfo Profile "profile",) $ do
            commonLayout
            setFileAttribute ".FileFunction" ["Profile", "NP"]
            setFileAttribute ".FilePolarity" ["Positive"]
        , mechanical =
            [ (LayerInfo PlatedDrill "drill-plated",) $ do
                commonLayout
                setFileAttribute ".FileFunction" ["Plated", "1", "2", "PTH", "Drill"] -- 1,2 are the start/end of the layers (this board has two layers)
                setFileAttribute ".FilePolarity" ["Positive"]
            ]
        , topSurface =
            [ (LayerInfo SolderMask "top-soldermask",) $ do
                commonLayout
                setFileAttribute ".FileFunction" ["Soldermask", "Top"]
                setFileAttribute ".FilePolarity" ["Negative"]
            ]
        , copper =
            [ (LayerInfo SignalCopper "top-copper",) $ do
                commonLayout
                setFileAttribute ".FileFunction" ["Copper", "L1", "Top"]
                setFileAttribute ".FilePolarity" ["Positive"]
            , (LayerInfo SignalCopper "bot-copper",) $ do
                commonLayout
                setFileAttribute ".FileFunction" ["Copper", "L2", "Bot"]
                setFileAttribute ".FilePolarity" ["Positive"]
            ]
        , botSurface =
            [ (LayerInfo SolderMask "bot-soldermask",) $ do
                commonLayout
                setFileAttribute ".FileFunction" ["Soldermask", "Bot"]
                setFileAttribute ".FilePolarity" ["Negative"]
            ]
        }
    }


abscoords :: (Float, Float) -> PcbLayout (Float, Float)
abscoords (x, y) = PcbLayout $ do
    (x0, y0) <- gets _here
    pure (x0 + x, y0 + y)


edge :: Brush -> [(Float, Float)] -> PcbLayout ()
edge brush xys0 = do
    when (length xys0 < 3) $ error "too few points to define edge"
    ~(xy:xys) <- abscoords `mapM` xys0
    PcbLayout $ modify $ \s -> s{
        _layers.profile._2 = do
            s._layers.profile._2
            setBrush brush
            setInterpolationMode Linear
            region $ do
                move xy
                forM_ (xys `snoc` xy) $ flip interpolate Nothing
        }

trace :: Brush -> Int -> [(Float, Float)] -> PcbLayout ()
trace brush ix xysRel = PcbLayout $ do
    when (length xysRel < 2) $ error "too few points to define trace"
    stackup <- gets _layers
    unless (0 <= ix && ix < length (copper stackup)) $ error "layer out of bounds"
    let info = fst $ stackup.copper !! ix
    ~(xy:xys) <- unlayout $ abscoords `mapM` xysRel
    let cmds = case info.ty of
            SignalCopper -> do
                setInterpolationMode Linear
                setBrush brush
                move xy
                forM_ xys $ flip interpolate Nothing
            _ -> pure ()
    modify $ \s -> s{
        _layers.copper = modList ix (second (>> cmds)) s._layers.copper
    }

via :: (Brush, Brush) -> (Float, Float) -> PcbLayout ()
via (pad, drill) xyRel = do
    xy <- abscoords xyRel
    PcbLayout $ modify $ \s -> s{
          _layers.copper = flip map s._layers.copper $ \(info, cmds0) -> (info, cmds0 >> cmds info xy)
        , _layers.mechanical = flip map s._layers.mechanical $ \(info, cmds0) -> (info, cmds0 >> cmds info xy)
        }
    where
    cmds info xy = case info.ty of
        SignalCopper -> setBrush pad >> flash xy
        PlatedDrill -> setBrush drill >> flash xy
        _ -> pure ()


thPad :: (Brush, Brush, Brush) -> (Float, Float) -> PcbLayout ()
thPad (mask, pad, drill) xyRel = do
    xy <- abscoords xyRel
    PcbLayout $ modify $ \s -> s{
          _layers.copper = flip map s._layers.copper $ \(info, cmds0) -> (info, cmds0 >> cmds info xy)
        , _layers.mechanical = flip map s._layers.mechanical $ \(info, cmds0) -> (info, cmds0 >> cmds info xy)
        , _layers.topSurface = flip map s._layers.topSurface $ \(info, cmds0) -> (info, cmds0 >> cmds info xy)
        , _layers.botSurface = flip map s._layers.botSurface $ \(info, cmds0) -> (info, cmds0 >> cmds info xy)
        }
    where
    cmds info xy = case info.ty of
        SolderMask -> setBrush mask >> flash xy
        SignalCopper -> setBrush pad   >> flash xy
        PlatedDrill -> setBrush drill >> flash xy
        _ -> pure ()

smdPad :: (Brush, Brush) -> (Float, Float) -> PcbLayout ()
-- -- FIXME make orientable and top/bottom selectable
smdPad (pad, mask) xyRel = do
    xy <- abscoords xyRel
    PcbLayout $ modify $ \s -> s{
          _layers.copper = flip map s._layers.copper $ \(info, cmds0) -> (info, cmds0 >> cmds info xy)
        , _layers.topSurface = flip map s._layers.topSurface $ \(info, cmds0) -> (info, cmds0 >> cmds info xy)
        , _layers.botSurface = flip map s._layers.botSurface $ \(info, cmds0) -> (info, cmds0 >> cmds info xy)
        }
    where
    cmds info xy = case info.ty of
        SignalCopper -> setBrush pad >> flash xy
        SolderMask -> setBrush mask >> flash xy
        _ -> pure ()





------------------------------------





snoc :: [a] -> a -> [a]
xs `snoc` x = xs ++ [x]

modList :: Int -> (a -> a) -> [a] -> [a]
modList i f xs = [if j == i then f x else x | (j, x) <- zip [0..] xs]
