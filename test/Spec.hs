import Language.Gerber.Data
import Language.Gerber.Syntax
import Language.Gerber.Writer

main :: IO ()

main = do
    let out = write regionExperiment
    putStrLn "\n\n------------"
    putStrLn out
    putStrLn "------------\n"
    writeFile "examples/region-experiment.gbr" out

regionExperiment =
    [ FormatSpecification 2 5
    , Mode Millimeters
    , SetAttribute FileAttr ".Part" ["Other", "example"]
    , LoadPolarity Dark
    , ApertureDefinition "10" (Circle 1 Nothing)
    , SetAperture "10"
    , Region
        [ SetInterpolationMode Linear
        , Move [X 0, Y 0]
        , Interpolate [X 500000]
        , Interpolate [Y 300000]
        , Interpolate [X 0]
        , Interpolate [Y 0]
        ]
    , Flash [X 700000, Y 0]
    , EndOfFile
    ]

ex01 =
    [ Comment  "Ucamco ex. 1: Two square boxes"
    , FormatSpecification 2 5
    , Mode Millimeters
    , SetAttribute FileAttr ".Part" ["Other", "example"]
    , LoadPolarity Dark
    , ApertureDefinition "10" (Circle 0.01 Nothing)
    , SetAperture "10"
    , Move [X 0, Y 0]
    , SetInterpolationMode Linear
    , Interpolate [X 500000, Y 0]
    , Interpolate [Y 500000]
    , Interpolate [X 0]
    , Interpolate [Y 0]
    , Move [X 600000]
    , Interpolate [X 1100000]
    , Interpolate [Y 500000]
    , Interpolate [X 600000]
    , Interpolate [Y 0]
    , EndOfFile
    ]

ex02 =
    [ Comment $ "Ucamco ex. 2: shapes"
    , FormatSpecification 2 6
    , Mode Inches
    , SetAttribute FileAttr ".Part" ["Other", "example"]

    , Comment $ "Define Apertures"
    , ApertureMacro "TARGET125" [Moire
        { cx=0, cy=0
        , odRings=0.125, ringThickness=0.01, gap=0.01, maxRings=3
        , crossThick=0.003, crossLength=0.150
        , rot=0
        }]
    , ApertureMacro "THERMAL80" [Thermal
        { cx=0, cy=0
        , od=0.080, id=0.055
        , gap=0.0125
        , rot=45
        }]
    , ApertureDefinition "10" $ Circle 0.01 Nothing
    , ApertureDefinition "11" $ Circle 0.06 Nothing
    , ApertureDefinition "12" $ Rectangle 0.06 0.06 Nothing
    , ApertureDefinition "13" $ Rectangle 0.04 0.10 Nothing
    , ApertureDefinition "14" $ Rectangle 0.10 0.04 Nothing
    , ApertureDefinition "15" $ Obround 0.04 0.10 Nothing
    , ApertureDefinition "16" $ Polygon 0.1 3 Nothing Nothing
    , ApertureDefinition "18" $ Macro "TARGET125"
    , ApertureDefinition "19" $ Macro "THERMAL80"

    , Comment "Start image generation"
    , SetAperture "10"
    , Move [X 0, Y 250000]
    , SetInterpolationMode Linear
    , Interpolate [X 0, Y 0]
    , Interpolate [X 250000, Y 0]
    , Move [X 1000000, Y 1000000]
    , Interpolate [X 1500000]
    , Interpolate [X 2000000, Y 1500000]
    , Move [X 2500000, Y 1500000]
    , Interpolate [X 2500000, Y 1000000]
    , SetAperture "11"
    , Flash [X 1000000, Y 1000000]
    , Flash [X 2000000]
    , Flash [X 2500000]
    , Flash [Y 1500000]
    , Flash [X 2000000]
    , SetAperture "12"
    , Flash [X 1000000, Y 1500000]
    , SetAperture "13"
    , Flash [X 3000000, Y 1500000]
    , SetAperture "14"
    , Flash [Y 1250000]
    , SetAperture "15"
    , Flash [Y 1000000]
    , SetAperture "10"
    , Move [X 3750000, Y 1000000]
    , SetQuadrantMode MultiQuad
    , SetInterpolationMode Counterclockwise
    , Interpolate [X 3750000, Y 1000000, I 250000, J 0]
    , SetAperture "16"
    , Flash [X 3400000, Y 1000000]
    , Flash [X 3500000, Y 900000]
    , SetAperture "10"
    , Region
        [ Move [X 500000, Y 2000000]
        , SetInterpolationMode Linear
        , Interpolate [X 500000, Y 3750000]
        , Interpolate [X 3750000, Y 3750000]
        , Interpolate [X 3750000, Y 2000000]
        , Interpolate [X 500000, Y 2000000]
        ]
    , SetAperture "18"
    , Flash [X 0, Y 3875000]
    , Flash [X 3875000, Y 3875000]
    , LoadPolarity Clear
    , Region
        [ Move [X 1000000, Y 2500000]
        , Interpolate [X 1000000, Y 3000000]
        , SetQuadrantMode SingleQuad
        , SetInterpolationMode Clockwise
        , Interpolate [X 1250000, Y 3250000, I 250000, J 0]
        , SetInterpolationMode Linear
        , Interpolate [X 3000000, Y 3250000]
        , SetQuadrantMode MultiQuad
        , SetInterpolationMode Clockwise
        , Interpolate [X 3000000, Y 2500000, I 0, J (-375000)]
        , SetInterpolationMode Linear
        , Interpolate [X 1000000, Y 2500000]
        ]
    , LoadPolarity Dark
    , SetAperture "10"
    , Move [X 1500000, Y 2875000]
    , Interpolate [X 2000000, Y 2875000]
    , SetAperture "11"
    , Flash [X 1500000, Y 2875000]
    , Flash [X 2000000, Y 2875000]
    , SetAperture "19"
    , Flash [X 2875000, Y 2875000]
    , EndOfFile
    ]
