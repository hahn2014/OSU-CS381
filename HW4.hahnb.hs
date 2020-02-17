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
type FState = (Mode,FPoint)
-- | A type to represent a 3D point in space.
type TriPoint = (Int,Int,Int)
type TriDouble = (Double,Double,Double)

-- | The initial state of the pen.
fstart :: FState
fstart = (Up,(0.0,0.0))

start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toGridHTML ls

fdraw :: FProg -> IO ()
fdraw p = let (_,ls) = fprog p fstart in toGridFHTML ls


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


fcmd :: FCmd -> FState -> (FState, Maybe FLine)
fcmd (FPen Up) (_,(x,y))          = ((Up,(x,y)),Nothing)
fcmd (FPen Down) (_,(x,y))        = ((Down,(x,y)),Nothing)
fcmd (FMove x y) (Up,(i,j))      = ((Up,(x,y)),Nothing)
fcmd (FMove x y) (Down,(i,j))    = ((Down,(x,y)),Just ((i,j),(x,y)))
fcmd (FMoveP (x,y)) (Up,(i,j))   = ((Up,(x,y)),Nothing)
fcmd (FMoveP (x,y)) (Down,(i,j)) = ((Down,(x,y)),Just ((i,j),(x,y)))


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

fprog :: FProg -> FState -> (FState, [FLine])
fprog [] s     = (s, [])
fprog (p:ps) s = case (fcmd p s) of
              ((m,(x,y)), Just ((i,j),(z,v))) -> (prettyFHelper (((i,j),(z,v))) (fprog ps (m,(x,y))))
              ((m,(x,y)), Nothing)            -> (fprog ps (m,(x,y)))

prettyHelper :: Line -> (State, [Line]) -> (State, [Line])
prettyHelper (line) (state, listLine) = (state, line : listLine)

prettyFHelper :: FLine -> (FState, [FLine]) -> (FState, [FLine])
prettyFHelper (line) (state, listLine) = (state, line : listLine)


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: FProg
amazing = cube (40.0,10.0) 10.0 33.0








-- Helper function for drawing a cube
--   start xy - sidelen - yrot
cube :: FPoint -> Double -> Double -> FProg
cube (sx,sy) len 0    = [FPen Up, FMove sx sy] ++ boxRot (sx,sy) len 0 -- since it has a rotation of 0, just draw a box
cube (sx,sy) len yrot = [FPen Up, FMove sx sy] ++ boxRot (sx,sy) len yrot --front box
                        ++ boxRot (sx,sy) len (-180.0 + yrot)

-- Helper function for drawing a box (at current x,y) of size sidelen
--      and a rotation factor of yrot
--   start xy - sidelen - yrot
boxRot :: FPoint -> Double -> Double -> FProg
boxRot (sx,sy) len 0    = [FPen Down, FMove sx (sy + len), FMove (sx + len) (sy + len),
                           FMove (sx + len) sy, FMove sx sy, FPen Up] --draw a box without rotation
boxRot (sx,sy) len yrot = [FPen Down, FMove sx (sy + len), -- p1 and p2 will never rotate
                           FMove (getXRotation (sx + len) len yrot) (getYRotation (sy + len) len yrot),--p3 rotation calc
                           FMove (getXRotation (sx + len) len yrot) (getYRotation sy len yrot), --p4 rotation calc
                           FMove sx sy, FPen Up]


-- Helper function to calculate the new rotation coords
--        start xy - sidelen - yrot
getXRotation :: Double -> Double -> Double -> Double
getXRotation x len r = (x + len) - (getDifference (getHeight len r) (getBangle len r))

getYRotation :: Double -> Double -> Double -> Double
getYRotation y len r = y + (getHeight len r)

--
--         sidelen - r
getBangle :: Double -> Double -> Double
getBangle len r = acos((getZ len r) / (2 * len))
--
-- --    sidelen - r
getZ :: Double -> Double -> Double
getZ len r = sqrt((2 * (len ^ 2)) - 2 * (len ^ 2) * cos(r))
-- --
-- --             height - r2
getDifference :: Double -> Double -> Double
getDifference h r = h * tan(r)
--
-- -- Helper function to calculate the height of the difference triangle
-- --        sidelen - yrot
getHeight :: Double -> Double -> Double
getHeight len r = len * sin(r)












to2DCoords :: TriPoint -> Point
to2DCoords (x,y,z) = undefined
