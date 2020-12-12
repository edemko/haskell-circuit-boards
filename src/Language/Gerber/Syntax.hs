{-# LANGUAGE DuplicateRecordFields #-}

module Language.Gerber.Syntax
    ( Command(..)
    , ApertureTemplate(..)
    , MacroContent
    , MacroStatement(..)
    ) where

import Language.Gerber.Data


data Command
    = FormatSpecification -- TODO validation
        { intDigits :: Int -- up to six
        , decimalDigits :: Int -- must be 5 or 6
        }
    | Mode Unit
    | ApertureDefinition
        { apertureName :: DCode
        , apertureTemplate :: ApertureTemplate
        }
    | ApertureMacro MacroName MacroContent
    -- TODO Block Aperture
    | SetAperture DCode
    | Interpolate GbrCoordinates -- FIXME can have i,j components only when in arc interpolation modes
    | Move GbrCoordinates -- FIXME can onely have x,y components
    | Flash GbrCoordinates -- FIXME can only have x,y components
    | SetInterpolationMode InterpolationMode
    | SetQuadrantMode QuadrantMode
    | LoadPolarity Polarity
    | LoadMirroring Mirroring
    | LoadRotation GbrDecimal
    | LoadScaling GbrDecimal -- FIXME greater than zero
    | StartRegion -- FIXME only some commands are allowed in a region
    | EndRegion
    -- TODO step&repeat
    | Comment GbrString
    | EndOfFile
    | SetAttribute
        { attrType :: AttributeType
        , attrName :: GbrString
        , attrValueFields :: [GbrString]
        }
    | DeleteAttribute (Maybe GbrString)


data ApertureTemplate
    = Circle
        { diameter :: GbrDecimal
        , holeDiam :: Maybe GbrDecimal
        }
    | Rectangle
        { xSize :: GbrDecimal -- FIXME > 0
        , ySize :: GbrDecimal -- FIXME > 0
        , holeDiam :: Maybe GbrDecimal -- FIXME > 0
        }
    | Obround
        { xSize :: GbrDecimal -- FIXME > 0
        , ySize :: GbrDecimal -- FIXME > 0
        , holeDiam :: Maybe GbrDecimal -- FIXME > 0
        }
    | Polygon
        { od :: GbrDecimal
        , verts :: GbrInteger -- FIXME 3 ≤ verts ≤ 12
        , rot :: Maybe GbrDecimal
        , holeDiam :: Maybe GbrDecimal
        }
    | Macro MacroName -- TODO I expect modifiers can be applied to a macro template
    deriving (Eq)


type MacroContent = [MacroStatement]
data MacroStatement
    = VarDef VarName Arith
    -- | MacroComment -- TODO
    -- | Circle -- TODO
    -- | VectorLine -- TODO
    -- | CenterLine -- TODO
    -- | Outline -- TODO
    -- | Polygon -- TODO
    | Moire
        { cx :: GbrDecimal
        , cy :: GbrDecimal
        , odRings :: GbrDecimal
        , ringThickness :: GbrDecimal
        , gap :: GbrDecimal
        , maxRings :: GbrInteger
        , crossThick :: GbrDecimal
        , crossLength :: GbrDecimal
        , rot :: GbrDecimal
        }
    | Thermal
        { cx :: GbrDecimal
        , cy :: GbrDecimal
        , od :: GbrDecimal -- FIXME must be > id
        , id :: GbrDecimal -- FIXME must be > 0
        , gap :: GbrDecimal -- FIXME must be < od/√2
        , rot :: GbrDecimal
        }

data Arith -- TODO
