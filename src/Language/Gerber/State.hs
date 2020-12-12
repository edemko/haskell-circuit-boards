{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Language.Gerber.State
    ( Brush(..)
    , GbrState(..)
    , startGbrState
    , insertBrush
    , lookupDCode
    , lookupBrush
    , diffApertureAttrs
    ) where

import Language.Gerber.Data

import Data.Map (Map)
import Data.String (fromString)
import Language.Gerber.Syntax (ApertureTemplate, Command)

import qualified Data.Map as Map


{-  The idea behind a brush is to hold all of the information about an aperture:
        - the template
        - the attributes
        - any in-file documentation
    If brushes are held in the aperture dictionary, it becomes easy to see if a
    brush has already been defined.
-}
data Brush = Brush
    { docstring :: [GbrString]
    , template :: ApertureTemplate
    , attributes :: [(GbrString, [GbrString])]
    -- TODO name
    }
    deriving (Eq)


data GbrState = GbrState
    { gbrCommands :: [Command]
    -- Coordinate parameters
    , gbrCoordFormat :: Maybe (Int, Int)
    , gbrUnit :: Maybe Unit
    -- Generation parameters
    , gbrCurrentPoint :: (Maybe GbrCoordinate, Maybe GbrCoordinate)
    , gbrCurrentAperture :: Maybe DCode
    , gbrInterpolationMode :: Maybe InterpolationMode
    , gbrQuadrantMode :: Maybe QuadrantMode
    -- Aperture transformation parameters
    , gbrPolarity :: Polarity
    , gbrMirroring :: Mirroring
    , gbrRotation :: GbrDecimal
    , gbrScaling :: GbrDecimal
    -- dictionaries
    -- TODO aperture templates dictionary
    -- TODO aperture dictionary
    , gbrApertureDict :: [(DCode, Brush)]
    , _freeDCodes :: [DCode]
    , gbrCurrentAttrs :: Map GbrString (AttributeType, [GbrString])
    -- TODO used macro names
    }

startGbrState :: GbrState
startGbrState = GbrState
    { gbrCommands = []

    , gbrCoordFormat = Nothing
    , gbrUnit = Nothing
    , gbrCurrentPoint = (Nothing, Nothing)
    , gbrCurrentAperture = Nothing
    , gbrInterpolationMode = Nothing
    , gbrQuadrantMode = Nothing
    , gbrPolarity = Dark
    , gbrMirroring = NoMirror
    , gbrRotation = 0.0
    , gbrScaling = 0.0

    , gbrApertureDict = []
    , _freeDCodes = (fromString . show @Integer) <$> [10..]
    , gbrCurrentAttrs = Map.empty
    }

insertBrush :: Brush -> GbrState -> (DCode, GbrState)
insertBrush brush st = (dcode, st')
    where
    dcode : _freeDCodes' = _freeDCodes st
    gbrApertureDict' = (dcode, brush) : gbrApertureDict st
    st' = st
        { gbrApertureDict = gbrApertureDict'
        , _freeDCodes = _freeDCodes'
        }

lookupDCode :: DCode -> GbrState -> (Maybe Brush, GbrState)
lookupDCode dcode st = (r, st)
    where
    r = lookup dcode (gbrApertureDict st)

lookupBrush :: Brush -> GbrState -> (Maybe DCode, GbrState)
lookupBrush brush st = (r, st)
    where
    r = rLookup brush (gbrApertureDict st)
    rLookup k = lookup k . map swap
    swap (a, b) = (b, a)

diffApertureAttrs :: [(GbrString, [GbrString])] -> GbrState -> ([(GbrString, [GbrString])], [GbrString])
diffApertureAttrs kvs' GbrState{gbrCurrentAttrs} =
    let kvs = adapt $ Map.toList gbrCurrentAttrs
        add = do
            (k, v') <- kvs'
            case lookup k kvs of
                Just v | v == v' -> []
                _ -> [(k, v')]
        del = do
            (k, _) <- kvs
            case lookup k kvs' of
                Nothing -> [k]
                _ -> []
    in (add, del)
    where
    adapt = map (\(k, (_, v)) -> (k, v)) . filter isApAttr
    isApAttr (_, (ApertureAttr, _)) = True
    isApAttr _ = False
