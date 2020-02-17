-- | A module for rendering lines as an HTML5 file containing an SVG image.
--   This can be used to visualize the denotational semantics of a MiniLogo
--   program.
--
--   NOTE: You should not change the definitions in this file!
--
module Render (Point,FPoint,Line,FLine,toHTML,toFHTML,toGridHTML,toGridFHTML) where
import Data.List (intercalate)

-- | A point is a cartesian pair (x,y).
type Point = (Int,Int)
type FPoint = (Double,Double)

-- | A line is defined by its endpoints.
type Line = (Point,Point)
type FLine = (FPoint,FPoint)

-- | Output a list of lines as an HTML5 file containing an SVG image.
toHTML :: [Line] -> IO ()
toHTML ls = writeFile "tesseract.html" (header ++ content ls ++ footer)

toFHTML :: [FLine] -> IO ()
toFHTML ls = writeFile "tesseract.html" (header ++ floatingContent ls ++ footer)

-- | Alternate version of 'toHTML' that adds a grid to the background.
toGridHTML :: [Line] -> IO ()
toGridHTML ls = writeFile "tesseract.html" (header ++ grid ++ content ls ++ footer)

toGridFHTML :: [FLine] -> IO ()
toGridFHTML ls = writeFile "tesseract.html" (header ++ grid ++ floatingContent ls ++ footer)

--
-- Private definitions. All definitions below this point will not be visible
-- from within a module that imports this module.
--

scale, margin, width, height :: Int
dscale, dmargin, dwidth, dheight :: Double
scale   = 10
dscale  = 10.0
margin  = 10
dmargin = 10.0
width   = 800
dwidth  = 800.0
height  = 400
dheight = 400.0

gridStep = 5
maxX = width `div` scale
maxY = height `div` scale

gridStyle = "fill:none;stroke:lightgrey;stroke-width:1"
drawStyle = "fill:none;stroke:red;stroke-width:2"

title  = "<head><title>MiniLogo Semantics Viewer</title></head>"
view   = "<svg width='100%' viewBox='0 0 "
         ++ show (width + 2*margin) ++ " "
         ++ show (height + 2*margin) ++ "'>"
border = "<rect x='" ++ show (margin-3) ++
             "' y='" ++ show (margin-3) ++
         "' width='" ++ show (width +6) ++
        "' height='" ++ show (height+5) ++
         "' style='fill:none;stroke:black;stroke-width:2'/>"

header = unlines ["<!DOCTYPE html>", "<html>", title, "<body>", view, border]
footer = unlines ["</svg>","</body>","</html>"]

grid = unlines (map (poly gridStyle) lines)
  where lines = [ [(x,0), (x,maxY)] | x <- [0,gridStep..maxX] ]
             ++ [ [(0,y), (maxX,y)] | y <- [0,gridStep..maxY] ]

content :: [Line] -> String
content = unlines . map (poly drawStyle) . chunk

floatingContent :: [FLine] -> String
floatingContent = unlines . map (floatingPoly drawStyle) . floatingChunk

-- | A canvas-adjusted point as a string.
point :: Point -> String
point (x,y) = show xp ++ "," ++ show yp
  where xp = x*scale + margin
        yp = height - y*scale + margin

floatingPoint :: FPoint -> String
floatingPoint (x,y) = show xp ++ "," ++ show yp
  where xp = x * dscale + dmargin
        yp = dheight - y * dscale + dmargin

-- | Chunk a bunch of lines into sequences of connected points.
chunk :: [Line] -> [[Point]]
chunk []         = []
chunk [(p,q)]    = [[p,q]]
chunk ((p,q):ls) | q == head ps = (p:ps) : pss
                 | otherwise    = [p,q] : ps : pss
  where (ps:pss) = chunk ls

floatingChunk :: [FLine] -> [[FPoint]]
floatingChunk []         = []
floatingChunk [(p,q)]    = [[p,q]]
floatingChunk ((p,q):ls) | q == head ps = (p:ps) : pss
                 | otherwise    = [p,q] : ps : pss
  where (ps:pss) = floatingChunk ls


-- | Draw a sequence of connected points.
poly :: String -> [Point] -> String
poly style ps = "<polyline points='"
             ++ intercalate " " (map point ps)
             ++ "' style='" ++ style ++ "'/>"

floatingPoly :: String -> [FPoint] -> String
floatingPoly style fps = "<polyline points='"
             ++ intercalate " " (map floatingPoint fps)
             ++ "' style='" ++ style ++ "'/>"
