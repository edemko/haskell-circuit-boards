module Main where

import Language.Gerber.Data
import Language.Gerber.Syntax
import Language.Gerber.Antiparse
import Language.Gerber.Stackup
import Language.Gerber.Writer


gridUnit :: Float
gridUnit = 2.54

profileBrush = Brush
    { docstring = ["generic contour aperture"]
    , template = Circle 0.01 Nothing
    , attributes = [(".AperFunction", ["Profile"])]
    }
signal = Brush
    { docstring = ["signal trace"]
    , template = Circle 0.25 Nothing
    , attributes = [(".AperFunction", ["Conductor"])]
    }
thStd = (thMask, thCopper, thDrill)
thSq = (thMaskSq, thCopperSq, thDrill)
thMask = Brush
    { docstring = []
    , template = Circle 1.3 Nothing
    , attributes = [(".AperFunction", ["Material"])]
    }
thCopper = Brush
    { docstring = []
    , template = Circle 1.2 Nothing
    , attributes = [(".AperFunction", ["ComponentPad"])]
    }
thMaskSq = Brush
    { docstring = []
    , template = Rectangle 1.3 1.3 Nothing
    , attributes = [(".AperFunction", ["Material"])]
    }
thCopperSq = Brush
    { docstring = []
    , template = Rectangle 1.2 1.2 Nothing
    , attributes = [(".AperFunction", ["ComponentDrill"])]
    }
thDrill = Brush
    { docstring = []
    , template = Circle 0.7 Nothing
    , attributes =
        [ (".AperFunction", ["ComponentDrill"])
        -- TODO .DrillTolerance
        ]
    }
viaPad = Brush
    { docstring = []
    , template = Circle 0.7 Nothing
    , attributes = [(".AperFunction", ["ViaPad"])]
    }
viaDrill = Brush
    { docstring = []
    , template = Circle 0.35 Nothing
    , attributes =
        [ (".AperFunction", ["ViaDrill"])
        -- TODO .DrillTolerance
        ]
    }
viaStd = (viaPad, viaDrill)
-- FIXME sod323 is rotated upwards
sod323pad = Brush
    { docstring = []
    , template = Rectangle 0.69 0.56 Nothing
    , attributes = [(".AperFunction", ["SMDPad", "CuDef"])]
    }
sod323mask = Brush
    { docstring = []
    , template = Rectangle 0.79 0.66 Nothing
    , attributes = [(".AperFunction", ["Material"])]
    }


diodePads :: (Float, Float) -> PcbLayout ()
diodePads (x, y) = do
    smdPad (sod323pad, sod323mask) (x, y)
    smdPad (sod323pad, sod323mask) (x, y + 0.56 + 1.88)

diodeConnect :: (Int, Int) -> PcbLayout ()
diodeConnect (fromIntegral -> x, twoFuncs ((+1) . fromIntegral, isEven) -> (y, e)) = do
    let halfDiodeLength = (0.56 + 1.88) / 2
        xOff = if e then 0.3 else 0.7
    if e
    then 
        trace signal Top [ (x*gridUnit, y*gridUnit-halfDiodeLength)
                         , ((x+xOff)*gridUnit, y*gridUnit-halfDiodeLength) ]
    else
        trace signal Top [ (x*gridUnit, y*gridUnit)
                         , ((x+xOff)*gridUnit-halfDiodeLength, y*gridUnit)
                         , ((x+xOff)*gridUnit, y*gridUnit-halfDiodeLength) ]
    diodePads ((x+xOff)*gridUnit, y*gridUnit-halfDiodeLength)
    trace signal Top [ ((x+xOff)*gridUnit, y*gridUnit+halfDiodeLength)
              , ((x+xOff)*gridUnit, y*gridUnit) ]
    via viaStd ((x+xOff)*gridUnit, y*gridUnit)


diodeMatrix = do
    thPad thStd `mapM_` [(x*gridUnit, 1*gridUnit) | x <- [1..16]]
    trace signal Top `mapM_` [[(x*gridUnit, 1*gridUnit), (x*gridUnit, 8*gridUnit)] | x <- [1..16]]
    thPad thStd `mapM_` [(17*gridUnit, y*gridUnit) | y <- [2..8]]
    trace signal Bot `mapM_` [[(1*gridUnit, y*gridUnit), (17*gridUnit, y*gridUnit)] | y <- [2..8]]
    diodeConnect `mapM_` connections
    where
    connections = concat
        [ [(1, y) | y <- [7]]
        , [(2, y) | y <- [1, 4, 5, 6, 7]]
        , [(3, y) | y <- [3, 6]]
        , [(4, y) | y <- [5, 6]]
        , [(5, y) | y <- [1, 4, 5]]
        , [(6, y) | y <- [2, 5]]
        , [(7, y) | y <- [2]]
        , [(8, y) | y <- [4, 5, 6, 7]]
        , [(9, y) | y <- []]
        , [(10, y) | y <- [4, 5]]
        , [(11, y) | y <- [4]]
        , [(12, y) | y <- [1, 2]]
        , [(13, y) | y <- [1, 2, 3, 6]]
        , [(14, y) | y <- [1, 6]]
        , [(15, y) | y <- [2, 3]]
        , [(16, y) | y <- [2, 3, 4]]
        ]

commonLayout :: (GbrWriterMonad m) => m ()
commonLayout = do
    specifyFormat (2, 6)
    setUnits Millimeters
    setFileAttribute ".Part" ["Single"]
    setFileAttribute ".SameCoordinates" []


board = do
    edge profileBrush
        [ (18*gridUnit, 0)
        , (18*gridUnit, 8*gridUnit)
        , (0, 8*gridUnit)
        , (0, 0)
        ]
    diodeMatrix

main :: IO ()
main = do
    let Stackup{..} = runPcbLayout commonLayout board
    -- writeFile "out/top-silkscreen.gbr" (antiparse $ )
    writeFile "out/top-soldermask.gbr" (antiparse $ topSoldermask)
    writeFile "out/top-copper.gbr" (antiparse $ topCopper)
    writeFile "out/bot-copper.gbr" (antiparse $ botCopper)
    writeFile "out/bot-soldermask.gbr" (antiparse $ botSoldermask)
    -- writeFile "out/bot-silkscreen.gbr" (antiparse $ )
    writeFile "out/drill-plated.gbr" (antiparse $ platedDrill)
    -- writeFile "out/drill-nonplated.gbr" (antiparse $ )
    writeFile "out/profile.gbr" (antiparse $ profile)


twoFuncs :: (a -> b, a -> c) -> a -> (b, c)
twoFuncs (f, g) a = (f a, g a)
isEven x = x `mod` 2 == 0