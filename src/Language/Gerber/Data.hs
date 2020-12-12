{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Gerber.Data
    (
    -- * Primitive Types
      GbrInteger
    , GbrDecimal
    , GbrCoordinate
    , GbrName
    , GbrString
    , GbrCoordinates
    , GbrComponent(..)
    -- * Enumerated Types
    , Unit(..)
    , Polarity(..)
    , Mirroring(..)
    , InterpolationMode(..)
    , QuadrantMode(..)
    , AttributeType(..)
    -- * Identifiers
    , DCode
    , dcodeToIntegral
    , MacroName
    , VarName
    ) where

import Data.Int (Int32)
import Data.String (IsString(..))
import Language.Gerber.Antiparse.Class (Antiparse(..))


------------ Primitive Types (from §3.6) ------------

newtype GbrInteger = GbrInt Int32
    deriving (Eq, Ord, Enum, Num, Real, Integral)

newtype GbrDecimal = GbrDecimal Double
    deriving (Eq, Ord, Num, Fractional,Real)

newtype GbrCoordinate = GbrCoord Integer -- FIXME this is just a hack to avoid having to pass around data about how many decimal places should be written
    deriving (Eq, Ord, Num, Enum, Real, Integral)

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


------------ Identifiers (various sections) ------------

newtype DCode = DCode Int32 -- FIXME must between 10 and max_int32 (though 0-9 are /reserved/, so maybe those are restricted only for defining apertures?)
    deriving (Eq)

dcodeToIntegral :: Num a => DCode -> a
dcodeToIntegral (DCode n) = fromIntegral n

newtype MacroName = MacroName GbrName
    deriving (Eq)

newtype VarName = VarName Int32 -- FIXME must be greater than zero
    deriving (Eq)


------------ instances for the above ------------

instance IsString GbrString where
    fromString = GbrStr
instance Show GbrString where
    show (GbrStr s) = show s
instance Antiparse GbrString where
    antiparse (GbrStr s) = s

instance Show GbrName where
    show (GbrName s) = show s
instance Antiparse GbrName where
    antiparse (GbrName s) = s

instance IsString DCode where
    fromString = DCode . fromInteger . read

instance IsString MacroName where
    fromString = MacroName . GbrName
instance Show MacroName where
    show (MacroName s) = show s
instance Antiparse MacroName where
    antiparse (MacroName s) = antiparse s
