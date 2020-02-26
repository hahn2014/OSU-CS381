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
type FState = (Mode,FPoint) --floating point state
-- | A type to represent a 3D point in space.
type TriPoint = (Int,Int,Int) --for xyz axis int points
type TriDouble = (Double,Double,Double) -- for xyz floating points

cameraDistance :: Double
cameraDistance = 10.0

-- | calling start for a floating point program
fstart :: FState
fstart = (Up,(0.0,0.0))

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toGridHTML ls

--draw with floating points instead of integers
fdraw :: FProg -> IO ()
fdraw p = let (_,ls) = fprog p fstart in toGridFHTML ls

--
--  ,-----.,--.   ,--.,------.
-- '  .--./|   `.'   ||  .-.  \
-- |  |    |  |'.'|  ||  |  \  :
-- '  '--'\|  |   |  ||  '--'  /
--  `-----'`--'   `--'`-------'
--
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

-----------------------
--    Int Command    --
-----------------------
-- Defining the paths for a command datatype in a form of minilogo program
--
-- Globals: command -> pen state
-- Returns: (pen state, if possible, a line to move)
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Up) (_,(x,y))         = ((Up,(x,y)),Nothing)
cmd (Pen Down) (_,(x,y))       = ((Down,(x,y)),Nothing)
cmd (Move x y) (Up,(i,j))      = ((Up,(x,y)),Nothing)
cmd (Move x y) (Down,(i,j))    = ((Down,(x,y)),Just ((i,j),(x,y)))
cmd (MoveP (x,y)) (Up,(i,j))   = ((Up,(x,y)),Nothing)
cmd (MoveP (x,y)) (Down,(i,j)) = ((Down,(x,y)),Just ((i,j),(x,y)))

-----------------------
--   Float Command   --
-----------------------
-- Defining the paths for a floating point command datatype in a form of
--      minilogo program
--
-- Globals: floating point command -> floating point pen state
-- Returns: (floating point pen state, if possible, a line to move)
fcmd :: FCmd -> FState -> (FState, Maybe FLine)
fcmd (FPen Up) (_,(x,y))         = ((Up,(x,y)),Nothing)
fcmd (FPen Down) (_,(x,y))       = ((Down,(x,y)),Nothing)
fcmd (FMove x y) (Up,(i,j))      = ((Up,(x,y)),Nothing)
fcmd (FMove x y) (Down,(i,j))    = ((Down,(x,y)),Just ((i,j),(x,y)))
fcmd (FMoveP (x,y)) (Up,(i,j))   = ((Up,(x,y)),Nothing)
fcmd (FMoveP (x,y)) (Down,(i,j)) = ((Down,(x,y)),Just ((i,j),(x,y)))


--
-- ,------. ,------.  ,-----.  ,----.
-- |  .--. '|  .--. ''  .-.  ''  .-./
-- |  '--' ||  '--'.'|  | |  ||  | .---.
-- |  | --' |  |\  \ '  '-'  ''  '--'  |
-- `--'     `--' '--' `-----'  `------'
--
-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])

-----------------------
--    Int Program    --
-----------------------
-- Given a list of commands, return a pretty print version of the commands in form
--      of a MiniLogo program.
--
-- Globals: Prog a list of commands, and a pen state
-- Returns: (pen state, list of line moves)
prog :: Prog -> State -> (State, [Line])
prog [] s     = (s, [])
prog (p:ps) s = case (cmd p s) of
              ((m,(x,y)), Just ((i,j),(z,v))) -> (prettyHelper (((i,j),(z,v))) (prog ps (m,(x,y))))
              ((m,(x,y)), Nothing)            -> (prog ps (m,(x,y)))

-----------------------
--   Float Program   --
-----------------------
-- Given a list of commands, return a pretty print version of the commands in form
--      of a MiniLogo program.
--
-- Globals: FProg a list of floating point commands, and a floating point pen state
-- Returns: (floating point pen state, list of floating point line moves)
fprog :: FProg -> FState -> (FState, [FLine])
fprog [] s     = (s, [])
fprog (p:ps) s = case (fcmd p s) of
              ((m,(x,y)), Just ((i,j),(z,v))) -> (prettyFHelper (((i,j),(z,v))) (fprog ps (m,(x,y))))
              ((m,(x,y)), Nothing)            -> (fprog ps (m,(x,y)))

-----------------------
--  Pretty Helper I  --
-----------------------
-- return a formated version of the state and list of line moves as a helper function
--      of pretty print.
--
-- Globals: Line movement, and a (state and list of line movements)
-- Returns: (pen state, list of line moves)
prettyHelper :: Line -> (State, [Line]) -> (State, [Line])
prettyHelper (line) (state, listLine) = (state, line : listLine)

-----------------------
--  Pretty Helper F  --
-----------------------
-- return a formated version of the floating point state and list of line moves
--     as a helper function of pretty print.
--
-- Globals: Line movement, and a (state and list of line movements)
-- Returns: (pen state, list of line moves)
prettyFHelper :: FLine -> (FState, [FLine]) -> (FState, [FLine])
prettyFHelper (line) (state, listLine) = (state, line : listLine)




--
--   ,---.  ,--.   ,--.  ,---.  ,-------.,--.,--.  ,--. ,----.
--  /  O  \ |   `.'   | /  O  \ `--.   / |  ||  ,'.|  |'  .-./
-- |  .-.  ||  |'.'|  ||  .-.  |  /   /  |  ||  |' '  ||  | .---.
-- |  | |  ||  |   |  ||  | |  | /   `--.|  ||  | `   |'  '--'  |
-- `--' `--'`--'   `--'`--' `--'`-------'`--'`--'  `--' `------'
--
-- | * Extra credit
-- | Semantic function for amazing.
--
--   >>> fprog amazing fstart
--    --> amazing MiniLogo floating point program
--
--   >>> fdraw amazing
--    --> Open tesseract.html in browser

-----------------------
--  Pretty Helper F  --
-----------------------
-- return a formated version of the floating point state and list of line moves
--     as a helper function of pretty print.
--
-- Globals: Line movement, and a (state and list of line movements)
-- Returns: (pen state, list of line moves)
amazing :: FProg
amazing = cube (0.5,0.5) 10.0 (0.0, 0.0, 0.0) 1 ++ smiley (5.5,5.5) 2.5 -- a square and a smiley
       ++ cube (20.0,5.0) 10.0 (0.0,20.0,0.0) 3 ++ [FPen Up] --3D rotated cube not working
       ++ boxRot (40.0,10.0) 20 33.99


-----------------------
--    Smiley Face    --
-----------------------
-- Returns a smiley face MiniLogo floating point program
--
-- Globals: (x,y) representing the midpoint of the face, r for the radius
-- Returns: Floating point program of the face
smiley :: FPoint -> Double -> FProg
smiley (mx,my) r = circle ((mx - r),(my + r)) (r / 4.0) 8.0 ++ circle ((mx + r),(my + r)) (r / 4.0) 8.0
                ++ [FPen Up, FMove (mx - r) (my - (r/2)), FPen Down, FMove (mx - (r/2)) (my - r),
                    FMove (mx + (r/2)) (my - r), FMove (mx + r) (my - (r/2)), FPen Up]

-----------------------
--      Circle       --
-----------------------
-- Returns a (sort of) circle as minilogo floating point program
--
-- Globals: (x,y) representing the midpoint of the circle, r for the radius,
--              e for the edge count forming the circle (higher = less boxy)
-- Returns: Floating point program of the circle
circle :: FPoint -> Double -> Double -> FProg
circle (mx,my) r e = [FPen Up, FMove (mx - r) my, FPen Down] ++ circleHelper ((mx - r),my) (getZ r (360.0 / e)) e 90.0
                   ++ [FPen Up, FMove (mx - r) (my - (r/2)), FPen Down, FMove (mx - (r/2)) (my - r),
                      FMove (mx + (r/2)) (my - r), FMove (mx + r) (my - (r/2)), FPen Up]
-- circle (mx,my) r = [FPen Up, FMove (mx - r) (my + r), FPen Down, FMove (mx + r) (my + r),
--                     FMove (mx + r) (my - r), FMove (mx - r) (my - r), FMove (mx - r) (my + r), FPen Up]

-----------------------
--   Circle Helper   --
-----------------------
-- Helps with the circle calling
--
-- Globals: (x,y) representing the last calls position, len for the edge length,
--              i for the edges left to draw (iter), a for angle from last call
-- Returns: Floating point program of the circle
circleHelper :: FPoint -> Double -> Double -> Double -> FProg
circleHelper _ _ 0 _         = []
circleHelper (lx,ly) len i a = [FMove (getXRotation lx len a) (getYRotation ly len a)]
                             ++ circleHelper ((getXRotation lx len a),(getYRotation ly len a)) len (i - 1) (a + ((360)/i))



--
--  ,-----.,--. ,--.,-----.  ,------.
-- '  .--./|  | |  ||  |) /_ |  .---'
-- |  |    |  | |  ||  .-.  \|  `--,
-- '  '--'\'  '-'  '|  '--' /|  `---.
--  `-----' `-----' `------' `------'
--


-- Helper function for drawing a cube
-- cube :: FPoint -> Double -> Double -> FProg
-- cube (sx,sy) len 0    = [FPen Up, FMove sx sy] ++ boxRot (sx,sy) len 0 -- since it has a rotation of 0, just draw a box
-- cube (sx,sy) len yrot = [FPen Up, FMove sx sy] ++ boxRot (sx,sy) len yrot --front box
--                         ++ boxRot ((getXRotation (sx + len) len yrot), (getYRotation sy len yrot)) len (180.0 + yrot)
--                         ++ boxRot ((getXRotation (sx + len) len yrot), (getYRotation sy len yrot)) len (180.0 + yrot)

-----------------------
--        Cube       --
-----------------------
-- Our attempt to draw a 3-Dimentional cube
--
-- Globals: (x,y) representing the start xy where the cube will be drawn, len for
--              the sidelength, (a,b,c) representing the desired xyz rotation i
--              for the iterator to avoid non-exhaustive looping.
-- Returns: Floating point program of the cube
cube :: FPoint -> Double -> TriDouble -> Int -> FProg
cube _ _ _ 0               = [] --end case, iterator hit 0, finish the floating point program
cube (sx,sy) len (0,0,0) i = [FPen Up, FMove sx sy] ++ boxRot (sx,sy) len 0 ++ [FPen Up]-- since it has a rotation of 0, just draw a box
cube (sx,sy) len (a,b,c) i = [FPen Up, FMove sx sy] ++ boxRot (sx,sy) len b
                        ++ boxRot (getPoint (boxRot (sx,sy) len b)) len (b + 270.0) ++ [FPen Up]
                        -- ++ boxRot (sx,sy) len (-b)
                        -- ++ cube (getPoint (boxRot (sx,sy) len b)) len (a,b + 270.0,c) (i - 1)

--cube (sx,sy) len r i       = [FPen Up, FMove sx sy, FPen Down] ++ drawCoords (getCubeCoords (sx,sy,cameraDistance) r len) r
--           -> This pattern match was our attempt at implementing a 3D to 2D
--                  conversion system to draw a 3D object in terms of on a screen.


-----------------------
--    Draw Coords    --
-----------------------
-- Recursively interprate a list of xyz coordinates as a floating point program of
--      moves to be drawn by minilogo
--
-- Globals: List of xyz coordinates, (a,b,c) representing the desired xyz rotation
-- Returns: Floating point program of the cube
--             start xyz  - xyz rotation
drawCoords :: [TriDouble] -> TriDouble -> FProg
drawCoords [] _     = []
drawCoords (c:cs) r = [FMoveP (to2DCoords c r)] ++ drawCoords cs r

-----------------------
--      Box Rot      --
-----------------------
-- Draws a box at a desired y rotation in terms of a floating point MiniLogo program
--
-- Globals: (x,y) representing the start xy where the box will be drawn, len
--              for the sidelength, yrot for the desired y rotation
-- Returns: Floating point program of the box
boxRot :: FPoint -> Double -> Double -> FProg
boxRot (sx,sy) len 0    = [FPen Down, FMove sx (sy + len), FMove (sx + len) (sy + len),
                           FMove (sx + len) sy, FMove sx sy, FPen Up] --draw a box without rotation
boxRot (sx,sy) len yrot = if (yrot > -90.0 && yrot < 90) --if the rotation is to the right
                            then
                                [FPen Down, FMove (getXRotation (sx + len) len yrot) (getYRotation sy len yrot),
                               FMove (getXRotation (sx + len) len yrot) (getYRotation (sy + len) len yrot),--p3 rotation calc
                               FMove sx (sy + len), FMove sx sy, FMove (getXRotation (sx + len) len yrot) (getYRotation sy len yrot),
                               FPen Up]
                          else --the rotation is to the left
                              [FPen Down, FMove (getXRotation (sx - len) len yrot) (getYRotation sy len yrot),
                             FMove (getXRotation (sx - len) len yrot) (getYRotation (sy + len) len yrot),--p3 rotation calc
                             FMove sx (sy + len), FMove sx sy, FMove (getXRotation (sx - len) len yrot) (getYRotation sy len yrot),
                             FPen Up]
-- boxRot (sx,sy) len yrot = [FPen Down, FMove (getXRotation (sx + len) len yrot) (getYRotation sy len yrot),
--                            FMove (getXRotation (sx + len) len yrot) (getYRotation (sy + len) len yrot),--p3 rotation calc
--                            FMove sx (sy + len), FMove sx sy, FMove (getXRotation (sx + len) len yrot) (getYRotation sy len yrot),
--                            FPen Up]

--           start xyz - xyz rotation - sidelen
-- boxCoords :: TriDouble -> TriDouble -> Double -> [TriDouble]
-- boxCoords (sx,sy,sz) (a,b,c) len = [(sx,sy,sz)]
--
-- --           start xyz - xyz rotation - sidelen
-- rotateLine :: TriDouble -> TriDouble -> Double -> TriDouble
-- rotateLine (sx,sy,sz) (a,b,c) len = (getXRotation sx len a, getYRotation sy len b, getHeight len b)
--
--
-- --               start xyz - xyz rotation - sidelen
-- getCubeCoords :: TriDouble -> TriDouble -> Double -> [TriDouble]
-- getCubeCoords (sx,sy,sz) (a,b,c) len =
--     [(sx,sy,sz)] ++ (boxCoords (sx,sy,sz) (a,b,c) len)
--                 -- ++ (boxCoords () ())

getPoint :: FProg -> FPoint
getPoint []     = (0.0,0.0)
getPoint (x:xs) = case x of
    FMove  i j -> (i,j)
    FMoveP p   -> p
    _          -> getPoint xs





--
-- ,----. ,------.      ,--.   ,--.          ,--.  ,--.
-- '.-.  ||  .-.  \     |   `.'   | ,--,--.,-'  '-.|  ,---.
--   .' < |  |  \  :    |  |'.'|  |' ,-.  |'-.  .-'|  .-.  |
-- /'-'  ||  '--'  /    |  |   |  |\ '-'  |  |  |  |  | |  |
-- `----' `-------'     `--'   `--' `--`--'  `--'  `--' `--'
--

-----------------------
--   Get X Rotation  --
-----------------------
-- Helper function to calculate the new x coords with a y rotation
--
-- Globals: x where the line starts at, len for length of the line, yrot for the
--              desired y rotation
-- Returns: the new x position based on the rotation
getXRotation :: Double -> Double -> Double -> Double
getXRotation x len r = (x + len) - (getDifference (getHeight len r) (getBangle len r))

-----------------------
--   Get Y Rotation  --
-----------------------
-- Helper function to calculate the new y coords with a y rotation
--
-- Globals: y where the line starts at, len for length of the line, yrot for the
--              desired y rotation
-- Returns: the new y position based on the rotation
getYRotation :: Double -> Double -> Double -> Double
getYRotation y len r = y + (getHeight len r)

-----------------------
--    Get B Angle    --
-----------------------
-- Helper function to calculate bottom right angle 'b'
--
-- Globals: len for the length of the lines, r for the bottom left 'a' angle
-- Returns: the 'b' angle of the triangle
getBangle :: Double -> Double -> Double
getBangle len r = acos((getZ len r) / (2 * len))

-----------------------
--       Get Z       --
-----------------------
-- Helper function to calculate the (possibly not right triangle) "hypotenuse" side length
--
-- Globals: len for the length of the lines, r for the bottom left 'a' angle
-- Returns: the length of the z side
getZ :: Double -> Double -> Double
getZ len r = sqrt((2 * (len ^ 2)) - 2 * (len ^ 2) * cos(r))

-----------------------
--   Get Difference  --
-----------------------
-- Helper function to calculate the change in position due to the y rotation
--
-- Globals: h as the height of the rotation triangle, r as the 'b' angle
-- Returns: the difference in x position
getDifference :: Double -> Double -> Double
getDifference h r = h * tan(r)

-----------------------
--    Get Height     --
-----------------------
-- Helper function to calculate the (possibly not right triangle) "hypotenuse" side length
--
-- Globals: len for the length of the lines, r for the bottom left 'a' angle
-- Returns: the length of the z side
-- -- Helper function to calculate the height of the difference triangle
-- --        sidelen - yrot
getHeight :: Double -> Double -> Double
getHeight len r = len * sin(r)


--
-- ,----. ,------.        ,--.              ,---. ,------.
-- '.-.  ||  .-.  \     ,-'  '-. ,---.     '.-.  \|  .-.  \
--   .' < |  |  \  :    '-.  .-'| .-. |     .-' .'|  |  \  :
-- /'-'  ||  '--'  /      |  |  ' '-' '    /   '-.|  '--'  /
-- `----' `-------'       `--'   `---'     '-----'`-------'
--
-- Algorithms graciously provided by https://en.wikipedia.org/wiki/3D_projection#Mathematical_formula

to2DCoords :: TriDouble -> TriDouble -> FPoint
to2DCoords c r = ((cameraDistance / (getDz c r)) * (getDx c r),(cameraDistance / (getDz c r)) * (getDy c r))

getDx :: TriDouble -> TriDouble -> Double
getDx (x,y,z) (a,b,c) = cos(b) * (sin(c) * y + cos(c) * x) - sin(b) * z

getDy :: TriDouble -> TriDouble -> Double
getDy (x,y,z) (a,b,c) = sin(a) * (cos(b) * z + sin(b) * (sin(c) * y + cos(c) * x)) + cos(a) * (cos(c) * y - sin(c) * x)

getDz :: TriDouble -> TriDouble -> Double
getDz (x,y,z) (a,b,c) = cos(c) * (cos(b) * z + sin(b) * (sin(c) * y + cos(c) * x)) - sin(a) * (cos(c) * y - sin(c) * x)
