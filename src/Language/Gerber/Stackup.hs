-- FIXME this is a lot of layers of abstraction in one file
module Language.Gerber.Stackup where

import Language.Gerber.Data
import Language.Gerber.Syntax
import Language.Gerber.State
import Language.Gerber.Writer
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map


-- TODO I'm not really sure what to do with these if I want to have standardized components
-- data Layer a = Layer
--     { layerType :: LayerType
--     , layerName :: String -- NOTE used as filepath
--     , value :: a
--     }

-- data LayerType
--     = Power
--     | Signal
--     | Soldermask
--     | Silkscreen
--     -- mechanical "layers"
--     | Profile
--     | Drill

-- TODO allow configuration of the number of various kinds of layers
-- but for now, a two-layer pcb is sufficient for my purposes
data Stackup = Stackup
    -- TODO a list of top silkscreen layers
    { topSoldermask :: [Command]
    , topCopper :: [Command]
    -- TODO a list of internal copper layers
    , botCopper :: [Command]
    , botSoldermask :: [Command]
    -- TODO a list of bottom silkscreen layers
    , profile :: [Command]
    , platedDrill :: [Command]
    }


data BoardSide = Top | Bot
data LeadPin = N | S | E | W





------------------------------------





newtype PcbLayout a = PcbLayout { unlayout :: State LayoutState a }
    deriving (Functor, Applicative, Monad)

data LayoutState = St
    { _here :: (Float, Float) -- FIXME this is a higher layer of abstraction

    -- FIXME these fields shuold be configurable layers
    , _profile :: GbrWriter ()
    , _topSoldermask :: GbrWriter ()
    , _topCopper :: GbrWriter ()
    , _botCopper :: GbrWriter ()
    , _botSoldermask :: GbrWriter ()
    , _platedDrill :: GbrWriter ()
    }

runPcbLayout :: GbrWriter () -> PcbLayout a -> Stackup
runPcbLayout commonLayout layout =
    let St{..} = execState (unlayout layout) (emptyLayout commonLayout)
    in Stackup
        { topSoldermask = execGbrWriter _topSoldermask
        , topCopper     = execGbrWriter _topCopper
        , botCopper     = execGbrWriter _botCopper
        , botSoldermask = execGbrWriter _botSoldermask
        , profile       = execGbrWriter _profile
        , platedDrill   = execGbrWriter _platedDrill
        }

emptyLayout :: GbrWriter () -> LayoutState
emptyLayout commonLayout = St
    { _here = (0, 0)
    , _profile = do
        commonLayout
        setFileAttribute ".FileFunction" ["Profile", "NP"]
        setFileAttribute ".FilePolarity" ["Positive"]
    , _topSoldermask = do
        commonLayout
        setFileAttribute ".FileFunction" ["Soldermask", "Top"]
        setFileAttribute ".FilePolarity" ["Negative"]
    , _topCopper = do
        commonLayout
        setFileAttribute ".FileFunction" ["Copper", "L1", "Top"]
        setFileAttribute ".FilePolarity" ["Positive"]
    , _botCopper = do
        commonLayout
        setFileAttribute ".FileFunction" ["Copper", "L2", "Bot"]
        setFileAttribute ".FilePolarity" ["Positive"]
    , _botSoldermask = do
        commonLayout
        setFileAttribute ".FileFunction" ["Soldermask", "Bot"]
        setFileAttribute ".FilePolarity" ["Negative"]
    , _platedDrill = do
        commonLayout
        setFileAttribute ".FileFunction" ["Plated", "1", "2", "PTH", "Drill"]
        setFileAttribute ".FilePolarity" ["Positive"]
    }


abscoords :: (Float, Float) -> PcbLayout (Float, Float)
abscoords (x, y) = PcbLayout $ do
    (x0, y0) <- gets _here
    pure (x0 + x, y0 + y)


edge :: Brush -> [(Float, Float)] -> PcbLayout ()
edge brush xys | length xys < 3 = error "too few points to define edge"
         | otherwise = do
    ~(xy:xys) <- abscoords `mapM` xys
    PcbLayout $ modify $ \s@St{..} -> s
        { _profile = (_profile >>) $ do
            setBrush brush
            setInterpolationMode Linear
            region $ do
                move xy
                forM_ (xys `snoc` xy) $ \xy -> interpolate xy Nothing
        }

trace :: Brush -> BoardSide -> [(Float, Float)] -> PcbLayout ()
trace _ _ xys | length xys < 2 = error "too few points to define trace"
trace brush w xys = do
    ~(xy0:xys) <- abscoords `mapM` xys
    let cmds = do
            setInterpolationMode Linear
            setBrush brush
            move xy0
            forM_ xys $ \xy -> interpolate xy Nothing
    PcbLayout $ modify $ \s@St{..} -> case w of
        Top -> s { _topCopper = _topCopper >> cmds }
        Bot -> s { _botCopper = _botCopper >> cmds }

via :: (Brush, Brush) -> (Float, Float) -> PcbLayout ()
via (pad, drill) xy = do
    xy <- abscoords xy
    PcbLayout $ modify $ \s@St{..} -> s
        { _topCopper = do
            _topCopper
            setBrush pad
            flash xy
        , _botCopper = do
            _botCopper
            setBrush pad
            flash xy
        , _platedDrill = do
            _platedDrill
            setBrush drill
            flash xy
        }

thPad :: (Brush, Brush, Brush) -> (Float, Float) -> PcbLayout ()
thPad (mask, pad, drill) xy = do
    xy <- abscoords xy
    PcbLayout $ modify $ \s@St{..} -> s
        { _topSoldermask = _topSoldermask >> setBrush mask  >> flash xy
        , _topCopper     = _topCopper     >> setBrush pad   >> flash xy
        , _botCopper     = _botCopper     >> setBrush pad   >> flash xy
        , _botSoldermask = _botSoldermask >> setBrush mask  >> flash xy
        , _platedDrill   = _platedDrill   >> setBrush drill >> flash xy
        }

-- FIXME make orientable and top/bottom selectable
smdPad :: (Brush, Brush) -> (Float, Float) -> PcbLayout ()
smdPad (pad, mask) xy = do
    xy <- abscoords xy
    PcbLayout $ modify $ \s@St{..} -> s
        { _topCopper = do
            _topCopper
            setBrush pad
            flash xy
        , _topSoldermask = do
            _topSoldermask
            setBrush mask
            flash xy
        }





------------------------------------





xs `snoc` x = xs ++ [x]