{-# LANGUAGE UndecidableInstances #-}
module Language.Gerber.Writer
    ( GbrWriterMonad(..)
    , GbrWriter
    , runGbrWriter
    , execGbrWriter
    , GbrWriterT
    , runGbrWriterT
    , execGbrWriterT
    , Brush(..) -- FIXME if I have to re-export, maybe that's a sign it's actually a thing for its own module, small as it is
    ) where

import Language.Gerber.Data
import Language.Gerber.Syntax
import Language.Gerber.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Writer



newtype GbrWriterT m a = GbrWriterT { unGbrWriterT :: StateT GbrState (WriterT [Command] m) a }

instance (Functor m) => Functor (GbrWriterT m) where
    fmap f (GbrWriterT x) = GbrWriterT (f <$> x)
instance (Monad m) => Applicative (GbrWriterT m) where
    pure = GbrWriterT . pure
    (GbrWriterT a) <*> (GbrWriterT b) = GbrWriterT (a <*> b)
instance (Monad m) => Monad (GbrWriterT m) where
    return = GbrWriterT . pure
    x >>= k = GbrWriterT $ do
        x <- unGbrWriterT x
        unGbrWriterT $ k x

instance MonadTrans GbrWriterT where
    lift = GbrWriterT . lift . lift

-- FIXME I thought I could quantify over s to the left of the arrow
runGbrWriterT :: (Monad m) => GbrWriterT m a -> m (a, [Command])
runGbrWriterT preaction =
    let action = do
            a <- unGbrWriterT preaction
            tell1 EndOfFile
            pure a
    in runWriterT $ evalStateT action startGbrState

execGbrWriterT :: (Monad m) => GbrWriterT m a -> m [Command]
execGbrWriterT action = snd <$> runGbrWriterT action

type GbrWriter = GbrWriterT Identity
runGbrWriter :: GbrWriter a -> (a, [Command])
runGbrWriter = runIdentity . runGbrWriterT
execGbrWriter :: GbrWriter a -> [Command]
execGbrWriter = snd . runGbrWriter

class (Monad m) => GbrWriterMonad m where
    specifyFormat :: (Int, Int) -> m ()
    setUnits :: Unit -> m ()
    -- defAperture :: ApertureTemplate -> m DCode
    -- TODO apertureMacro
    -- TODO blockAperture
    setBrush :: Brush -> m ()
    -- setAperture :: DCode -> m ()
    interpolate :: (Float, Float) -> Maybe (Float, Float) -> m ()
    move :: (Float, Float) -> m ()
    flash :: (Float, Float) -> m ()
    setInterpolationMode :: InterpolationMode -> m ()
    setQuadrantMode :: QuadrantMode -> m ()
    setPolarity :: Polarity -> m ()
    setMirroring :: Mirroring -> m ()
    setRotation :: GbrDecimal -> m ()
    setScaling :: GbrDecimal -> m ()
    region :: m () -> m ()
    -- TODO step&repeat
    comment :: GbrString -> m ()
    {-  NOTE endOfFile is specifically omitted
        It is emmitted at runGbrWriter.
    -}
    setFileAttribute :: GbrString -> [GbrString] -> m ()

instance Monad m => GbrWriterMonad (GbrWriterT m) where
    specifyFormat fs = GbrWriterT $ do
        gets gbrCoordFormat >>= \case
            Just _ -> error "cannot set format specification twice"
            Nothing -> burnbaby
        where
        burnbaby = do
            tell1 $ uncurry FormatSpecification fs
            modify $ \s@GbrState{..} -> s{ gbrCoordFormat = Just fs }

    setUnits unit = GbrWriterT $ do
        gets gbrUnit >>= \case
            Just _ -> error "cannot set units twice"
            Nothing -> burnbaby
        where
        burnbaby = do
            tell1 $ Mode unit
            modify $ \s@GbrState{..} -> s{ gbrUnit = Just unit }

    setBrush brush = GbrWriterT $ do
        state (lookupBrush brush) >>= \case
            Just dcode -> do
                setAperture dcode
            Nothing -> do
                dcode <- defAperture brush
                setAperture dcode
        where
        defAperture Brush{..} = do
            dcode <- state $ insertBrush brush
            tell $ Comment <$> docstring
            (sets, dels) <- diffApertureAttrs attributes <$> get
            forM_ sets $ \(k, v) -> setAttribute ApertureAttr k v
            forM_ dels $ \k -> delAttribute (Just k)
            tell1 $ ApertureDefinition dcode template
            pure dcode
        setAperture dcode = do
            gets ((Just dcode ==) . gbrCurrentAperture) >>= \case
                True -> pure ()
                False -> burnbaby
            where
            burnbaby = do
                tell1 $ SetAperture dcode
                modify $ \s@GbrState{..} -> s{ gbrCurrentAperture = Just dcode }

    interpolate xy Nothing = GbrWriterT $ do
        gets gbrInterpolationMode >>= \case
            Just Linear -> burnbaby
            _ -> error "attempt to draw line while not in linear interpolation mode"
        where
        burnbaby = unGbrWriterT $ _pointy Interpolate xy
    interpolate xy (Just ij) = GbrWriterT $ do
        gets gbrInterpolationMode >>= \case
            Just Clockwise -> burnbaby
            Just Counterclockwise -> burnbaby
            _ -> error "attempt to draw line while not in an arc interpolation mode"
        where
        burnbaby = unGbrWriterT $ do
            (i, j) <- floatsToCoord ij
            let f = Interpolate . (++ [I i, J j])
            _pointy f xy

    move = _pointy Move

    flash = _pointy Flash

    setInterpolationMode mode = GbrWriterT $ do
        gets ((Just mode ==) . gbrInterpolationMode) >>= \case
            True -> pure ()
            False -> burnbaby
        where
        burnbaby = do
            tell1 $ SetInterpolationMode mode
            modify $ \s@GbrState{..} -> s{ gbrInterpolationMode = Just mode }

    setQuadrantMode mode = GbrWriterT $ do
        gets ((Just mode ==) . gbrQuadrantMode) >>= \case
            True -> pure ()
            False -> burnbaby
        where
        burnbaby = do
            tell1 $ SetQuadrantMode mode
            modify $ \s@GbrState{..} -> s{ gbrQuadrantMode = Just mode }

    setPolarity pol = GbrWriterT $ do
        gets ((pol ==) . gbrPolarity) >>= \case
            True -> pure ()
            False -> burnbaby
        where
        burnbaby = do
            tell1 $ LoadPolarity pol
            modify $ \s@GbrState{..} -> s{ gbrPolarity = pol }

    setMirroring mir = GbrWriterT $ do
        gets ((mir ==) . gbrMirroring) >>= \case
            True -> pure ()
            False -> burnbaby
        where
        burnbaby = do
            tell1 $ LoadMirroring mir
            modify $ \s@GbrState{..} -> s{ gbrMirroring = mir }

    setRotation rot = GbrWriterT $ do
        gets ((rot ==) . gbrRotation) >>= \case
            True -> pure ()
            False -> burnbaby
        where
        burnbaby = do
            tell1 $ LoadRotation rot
            modify $ \s@GbrState{..} -> s{ gbrRotation = rot }

    setScaling scale = GbrWriterT $ do
        gets ((scale ==) . gbrScaling) >>= \case
            True -> pure ()
            False -> burnbaby
        where
        burnbaby = do
            tell1 $ LoadScaling scale
            modify $ \s@GbrState{..} -> s{ gbrScaling = scale }

    region body = GbrWriterT $ do
        tell1 StartRegion
        unGbrWriterT body
        tell1 EndRegion

    comment = GbrWriterT . tell1 . Comment

    setFileAttribute name value = GbrWriterT $ do
        setAttribute FileAttr name value

setAttribute ty name value = do
    gets (Map.lookup name . gbrCurrentAttrs) >>= \case
        Just (FileAttr, _) -> error "cannot redefine a file attribute"
        Just (ty', _)
            | ty /= ty' -> error "cannot change the type of an attribute"
            | ty == ty' -> burnbaby
        Nothing -> burnbaby
            
    where
    burnbaby = do
        tell1 $ SetAttribute ty name value
        modify $ \s@GbrState{..} -> s
            { gbrCurrentAttrs = Map.insert name (ty, value) gbrCurrentAttrs }

delAttribute Nothing = do
    gets (null . gbrCurrentAttrs) >>= \case
        True -> pure ()
        False -> burnbaby
    where
    burnbaby = do
        tell1 $ DeleteAttribute Nothing
        modify $ \s@GbrState{..} -> s
            { gbrCurrentAttrs = Map.empty }-- FIXME really? the standard says "the whole dictionary is cleared", but does that include file attributes?
delAttribute (Just name) = do
    gets (Map.lookup name . gbrCurrentAttrs) >>= \case
        Nothing -> pure ()
        Just _ -> burnbaby
    where
    burnbaby = do
        tell1 $ DeleteAttribute (Just name)
        modify $ \s@GbrState{..} -> s
            { gbrCurrentAttrs = Map.delete name gbrCurrentAttrs }-- FIXME the standard makes no note that file attrs can't be deleted…


tell1 :: (Monad m) => Command -> StateT GbrState (WriterT [Command] m) ()
tell1 cmd = tell [cmd]

-- FIXME maybe the logic here should be a function on GbrState
floatsToCoord :: (Monad m) => (Float, Float) -> GbrWriterT m (GbrCoordinate, GbrCoordinate)
floatsToCoord (x, y) = GbrWriterT $ do
    it <- gets gbrCoordFormat
    case it of
        Nothing -> error "undefined coordinate format"
        Just (_, decimalDigits) -> do
            -- FIXME check if the input floats are too large for the format
            let xform = fromIntegral . round . (*10^decimalDigits)
            pure (xform x, xform y)

-- FIXME do nothing if the new coordinates are the same as the last coordinates?
_pointy :: (Monad m) => (GbrCoordinates -> Command) -> (Float, Float) -> GbrWriterT m ()
_pointy f xy = GbrWriterT $ do
    (x, y) <- unGbrWriterT $ floatsToCoord xy
    (x0, y0) <- gets gbrCurrentPoint
    let coords = (if Just x == x0 then [] else [X x]) ++ (if Just y == y0 then [] else [Y y])
    tell1 $ f coords
    modify $ \s@GbrState{..} -> s{ gbrCurrentPoint = (Just x, Just y) }





instance (MonadState s m) => MonadState s (GbrWriterT m) where
    get = lift get
    put = lift . put
