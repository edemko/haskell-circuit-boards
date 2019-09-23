module Language.Gerber.Data where

import Data.Int
import Data.String (IsString(..))


------------ Primitive Types (from §3.6) ------------

newtype GbrInteger = GbrInt Int32
    deriving (Eq, Ord)

newtype GbrDecimal = GbrDecimal Double
    deriving (Eq, Ord)

newtype GbrCoordinate = GbrCoord Integer -- FIXME this is just a hack to avoid having to pass around data about how many decimal places should be written
    deriving (Eq, Ord)

-- TODO hexadecimal

newtype GbrName = GbrName String -- FIXME names have syntax restrictions
    deriving (Eq)

newtype GbrString = GbrStr String -- FIXME there are restrictions on the allowable characters
    deriving (Eq, Ord)

-- TODO fields

type GbrCoordinates = [GbrComponent] -- FIXME each axis should appear at most once
data GbrComponent
    = X GbrCoordinate
    | Y GbrCoordinate
    | I GbrCoordinate
    | J GbrCoordinate

------------ Enumerated Types (various sections) ------------

data Unit = Millimeters | Inches
    deriving (Read, Show, Eq)

data Polarity = Clear | Dark
    deriving (Read, Show, Eq)

data Mirroring
    = NoMirror
    | XMirror
    | YMirror
    | XYMirror
    deriving (Read, Show, Eq)

data InterpolationMode
    = Linear
    | Clockwise
    | Counterclockwise
    deriving (Read, Show, Eq)

data QuadrantMode = SingleQuad | MultiQuad
    deriving (Read, Show, Eq)

data AttributeType
    = FileAttr
    | ApertureAttr
    | ObjectAttr
    deriving (Read, Show, Eq)


------------ Identifiers (varois sections) ------------

newtype DCode = DCode Int32 -- FIXME must between 10 and max_int32 (though 0-9 are /reserved/, so maybe those are restricted only for defining apertures?)
    deriving (Eq)

newtype MacroName = MacroName GbrName
    deriving (Eq)

newtype VarName = VarName Int32 -- FIXME must be greater than zero
    deriving (Eq)


------------ instances for the above ------------

instance Num GbrInteger where
    fromInteger = GbrInt . fromInteger
    (GbrInt a) + (GbrInt b) = GbrInt (a + b)
    (GbrInt a) * (GbrInt b) = GbrInt (a * b)
    abs (GbrInt a) = GbrInt (abs a)
    signum (GbrInt a) = GbrInt (signum a)
    negate (GbrInt a) = GbrInt (negate a)

instance Num GbrDecimal where
    fromInteger = GbrDecimal . fromInteger
    (GbrDecimal a) + (GbrDecimal b) = GbrDecimal (a + b)
    (GbrDecimal a) * (GbrDecimal b) = GbrDecimal (a * b)
    abs (GbrDecimal a) = GbrDecimal (abs a)
    signum (GbrDecimal a) = GbrDecimal (signum a)
    negate (GbrDecimal a) = GbrDecimal (negate a)
instance Fractional GbrDecimal where
    fromRational = GbrDecimal . fromRational
    (GbrDecimal a) / (GbrDecimal b) = GbrDecimal (a / b)

instance Num GbrCoordinate where
    fromInteger = GbrCoord . fromInteger
    (GbrCoord a) + (GbrCoord b) = GbrCoord (a + b)
    (GbrCoord a) * (GbrCoord b) = GbrCoord (a * b)
    abs (GbrCoord a) = GbrCoord (abs a)
    signum (GbrCoord a) = GbrCoord (signum a)
    negate (GbrCoord a) = GbrCoord (negate a)

instance IsString GbrString where
    fromString = GbrStr


instance IsString DCode where
    fromString = DCode . fromInteger . read

instance IsString MacroName where
    fromString = MacroName . GbrName