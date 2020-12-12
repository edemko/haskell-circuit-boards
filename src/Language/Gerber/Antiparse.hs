{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Gerber.Antiparse
    ( Antiparse(..)
    ) where

import Language.Gerber.Data
import Language.Gerber.Syntax
import Prelude hiding (id)

import Data.List (intercalate)
import Language.Gerber.Antiparse.Class
import Numeric (showFFloat)


instance Antiparse [Command] where
    antiparse = intercalate "\n" . (antiparse <$>)

instance Antiparse Command where
    antiparse (fs@FormatSpecification{}) = _extCmd [["FSLA", "X", format, "Y", format]]
        where format = show (intDigits fs) ++ show (decimalDigits fs)
    antiparse (Mode units) = _extCmd [["MO", antiparse units]]
    antiparse (ApertureDefinition name template) = _extCmd [["AD", antiparse name, antiparse template]]
    antiparse (ApertureMacro name body) = _extCmd $ ["AM", antiparse name]:((:[]) . antiparse <$> body)
    antiparse (SetAperture name) = _stdCmd [antiparse name]
    antiparse (Interpolate coords) = _stdCmd $ (antiparse <$> coords) ++ ["D01"]
    antiparse (Move coords) = _stdCmd $ (antiparse <$> coords) ++ ["D02"]
    antiparse (Flash coords) = _stdCmd $ (antiparse <$> coords) ++ ["D03"]
    antiparse (SetInterpolationMode Linear) = _stdCmd ["G01"]
    antiparse (SetInterpolationMode Clockwise) = _stdCmd ["G02"]
    antiparse (SetInterpolationMode Counterclockwise) = _stdCmd ["G03"]
    antiparse (SetQuadrantMode SingleQuad) = _stdCmd ["G74"]
    antiparse (SetQuadrantMode MultiQuad) = _stdCmd ["G75"]
    antiparse (LoadPolarity p) = _extCmd [["LP", antiparse p]]
    antiparse (LoadMirroring m) = _extCmd [["LM", antiparse m]]
    antiparse (LoadRotation deg) = _extCmd [["LR", antiparse deg]]
    antiparse (LoadScaling s) = _extCmd [["LS", antiparse s]]
    antiparse StartRegion = _stdCmd ["G36"]
    antiparse EndRegion = _stdCmd ["G37"]
    antiparse (Comment s) = _stdCmd ["G04 ", antiparse s]
    antiparse EndOfFile = _stdCmd ["M02"]
    antiparse (SetAttribute ty name vals) = _extCmd [[antiparse ty, antiparse name, concatMap ((","++) . antiparse) vals]]
    antiparse (DeleteAttribute name) = _extCmd [["TD", maybe "" antiparse name]]


instance Antiparse GbrInteger where
    antiparse = show . toInteger

instance Antiparse GbrDecimal where
    antiparse = flip (showFFloat @Double Nothing) "" . realToFrac

instance Antiparse GbrCoordinate where
    antiparse = show . toInteger

instance Antiparse Unit where
    antiparse Millimeters = "MM"
    antiparse Inches = "IN"

instance Antiparse Polarity where
    antiparse Clear = "C"
    antiparse Dark = "D"

instance Antiparse Mirroring where
    antiparse NoMirror = "N"
    antiparse XMirror = "X"
    antiparse YMirror = "Y"
    antiparse XYMirror = "XY"

instance Antiparse AttributeType where
    antiparse FileAttr = "TF"
    antiparse ApertureAttr = "TA"
    antiparse ObjectAttr = "TO"

instance Antiparse GbrComponent where
    antiparse (X x) = "X" ++ antiparse x
    antiparse (Y y) = "Y" ++ antiparse y
    antiparse (I i) = "I" ++ antiparse i
    antiparse (J j) = "J" ++ antiparse j


instance Antiparse DCode where
    antiparse = ('D':) . show @Integer . dcodeToIntegral

instance Antiparse ApertureTemplate where
    antiparse (Circle d hd) = concat ["C,", antiparse d, maybe "" _by hd]
    antiparse (Rectangle x y hd) = concat ["R,", antiparse x, "X", antiparse y, maybe "" _by hd]
    antiparse (Obround x y hd) = concat ["O,", antiparse x, "X", antiparse y, maybe "" _by hd]
    antiparse (Polygon od verts rot hd) = concat ["P,", antiparse od, "X", antiparse verts, rotStr, hdStr]
        where
        rotStr = maybe (maybe "" (const "X0") hd) _by rot
        hdStr = maybe "" _by hd
    antiparse (Macro name) = antiparse name

instance Antiparse MacroStatement where
    antiparse (VarDef _ _) = error "TODO"
    antiparse (Moire{..}) = intercalate ","
        [ "6"
        , antiparse cx, antiparse cy
        , antiparse odRings, antiparse ringThickness, antiparse gap, antiparse maxRings
        , antiparse crossThick, antiparse crossLength
        , antiparse rot
        ]
    antiparse (Thermal{..}) = intercalate ","
        [ "7"
        , antiparse cx, antiparse cy
        , antiparse od, antiparse id
        , antiparse gap
        , antiparse rot
        ]


_stdCmd :: [String] -> String
_stdCmd = (++"*") . concat
_extCmd :: [[String]] -> String
_extCmd = ("%"++) . (++"%") . intercalate "\n" . (_stdCmd <$>)
_by :: Antiparse a => a -> String
_by = ("X"++) . antiparse
