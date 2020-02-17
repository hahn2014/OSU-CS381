-- Teams: Bryce Hahn hahnb@oregonstate.edu
   --     Brenden Smith smitbre2@oregonstate.edu
   --     Sheldon Roberts robeshel@oregonstate.edu
module HW4 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--    functions for generating MiniMiniLogo programs. It contains the type
--    definitions for Mode, Cmd, and Prog.
--  * Render.hs contains code for rendering the output of a MiniMiniLogo
--    program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)
-- | A type to represent a 3D point in space.
type TriPoint = (Int,Int,Int)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toGridHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Up) (_,(x,y))         = ((Up,(x,y)),Nothing)
cmd (Pen Down) (_,(x,y))       = ((Down,(x,y)),Nothing)
cmd (Move x y) (Up,(i,j))      = ((Up,(x,y)),Nothing)
cmd (Move x y) (Down,(i,j))    = ((Down,(x,y)),Just ((i,j),(x,y)))
cmd (MoveP (x,y)) (Up,(i,j))   = ((Up,(x,y)),Nothing)
cmd (MoveP (x,y)) (Down,(i,j)) = ((Down,(x,y)),Just ((i,j),(x,y)))



-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] s     = (s, [])
prog (p:ps) s = case (cmd p s) of
              ((m,(x,y)), Just ((i,j),(z,v))) -> (prettyHelper (((i,j),(z,v))) (prog ps (m,(x,y))))
              ((m,(x,y)), Nothing)            -> (prog ps (m,(x,y)))

prettyHelper :: Line -> (State, [Line]) -> (State, [Line])
prettyHelper (line) (state, listLine) = (state, line : listLine)
--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = cube (10,10) 10 45








-- Helper function for drawing a cube
--   start xy - sidelen - yrot
cube :: Point -> Int -> Int -> Prog
cube (sx,sy) len 0    = [Pen Up, Move sx sy] ++ boxRot (sx,sy) len 0 -- since it has a rotation of 0, just draw a box
cube (sx,sy) len yrot = [Pen Up, Move sx sy] ++ boxRot (sx,sy) len yrot --front box

-- Helper function for drawing a box (at current x,y) of size sidelen
--      and a rotation factor of yrot
--   start xy - sidelen - yrot
boxRot :: Point -> Int -> Int -> Prog
boxRot (sx,sy) len 0    = [Pen Down, Move sx (sy + len), Move (sx + len) (sy + len),
                           Move (sx + len) sy, Move sx sy, Pen Up] --draw a box without rotation
boxRot (sx,sy) len yrot = [Pen Down, Move sx (sy + len), -- p1 and p2 will never rotate
                           Move (getXRotation (sx + len) len yrot) (getYRotation (sy + len) len yrot),--p3 rotation calc
                           Move (getXRotation (sx + len) len yrot) (getYRotation sy len yrot), --p4 rotation calc
                           Move sx sy, Pen Up]

-- Helper function to calculate the new rotation coords
--        start xy - sidelen - yrot
getXRotation :: Int -> Int -> Int -> Int
getXRotation x len r = x - (getDifference (getHeight len r) (getBangle len r))

getYRotation :: Int -> Int -> Int -> Int
getYRotation y len r = y + (getHeight len r)

--
--         sidelen - r
getBangle :: Int -> Int -> Int
getBangle len r = round(acos(getZ len r) `div` (2 * len))
--
-- --    sidelen - r
getZ :: Int -> Int -> Int
getZ len r = round(sqrt((2 * (len ^ 2)) - 2 * (len ^ 2) * cos(r)))
-- --
-- --             height - r2
getDifference :: Int -> Int -> Int
getDifference h r = round (h * tan(r))
--
-- -- Helper function to calculate the height of the difference triangle
-- --        sidelen - yrot
getHeight :: Int -> Int -> Int
getHeight len r = round (len * sin(r))












to2DCoords :: TriPoint -> Point
to2DCoords (x,y,z) = undefined
