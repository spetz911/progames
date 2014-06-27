import Graphics.UI.GLUT hiding (position)
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Control.OldException
import Control.Monad
import System.Exit
import Prelude hiding (catch)
import Data.IORef
import Data.List
import Data.Maybe
import System.Process 




-- import Graphics.Rendering.OpenGL (GLdouble)

main = do
  keystate <- newIORef []
  cp       <- newIORef (openingProc keystate)
  initialWindowSize $= Size 640 480
  initialDisplayMode $= [RGBMode,DoubleBuffered]

  wnd <- createWindow "Shu-thing"

  {-
  gameModeCapabilities $= [
    Where' GameModeWidth IsEqualTo 640,
    Where' GameModeHeight IsEqualTo 480,
    Where' GameModeBitsPerPlane IsEqualTo 32,
    Where' GameModeRefreshRate IsAtLeast 30,
    Where' GameModeNum IsAtLeast 2
    ]

  (wnd,flg) <- enterGameMode
  -}

  displayCallback $= dispProc cp
  keyboardMouseCallback $= Just (keyProc keystate)
  --closeCallback $= Just closeProc

  addTimerCallback 16 (timerProc (dispProc cp))

  attachMenu LeftButton (Menu [
    MenuEntry "&Exit" exitLoop])

  initMatrix
  --do
  --  runCommand "start.mp3"
  mainLoop
  destroyWindow wnd

  `catch` (\exc -> return ())

exitLoop = throwIO $ ExitException ExitSuccess

initMatrix = do
  viewport $= (Position 0 0,Size 640 480)
  matrixMode $= Projection
--  loadIdentity
--  ortho (-320) 320 (-240) 240 (-1000) 1000
  loadIdentity
  perspective 30.0 (4/3) 600 1400
  lookAt (Vertex3 0 0 (927::GLdouble)) (Vertex3 0 0 (0::GLdouble)) (Vector3 0 1 (0::GLdouble))

dispProc cp = do
  m <- readIORef cp
  Scene next <- m
  writeIORef cp next

data Scene = Scene (IO Scene)

openingProc :: IORef [Key] -> IO Scene
openingProc ks = do
  keystate <- readIORef ks

  clear [ColorBuffer,DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  color $ Color3 (1.0::GLdouble) 1.0 1.0
  preservingMatrix $ do
    translate $ Vector3 (-250::GLdouble) 0 0
    scale (0.8::GLdouble) 0.8 0.8
    renderString Roman "shu-thing"
  preservingMatrix $ do
    translate $ Vector3 (-180::GLdouble) (-100) 0
    scale (0.4::GLdouble) 0.4 0.4
    renderString Roman "Press Z key"

  swapBuffers

  if Char 'z' `elem` keystate then do
      gs <- newIORef initialGameState
      --runCommand "stage1.mp3"
      return $ Scene $ mainProc gs ks
   else return $ Scene $ openingProc ks

endingProc :: IORef [Key] -> IORef Double -> IO Scene
endingProc ks ctr= do
  keystate <- readIORef ks
  counter <- readIORef ctr
  modifyIORef ctr (min 2420.(+1.5))
  clear [ColorBuffer,DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  color $ Color3 (1.0::GLdouble) 1.0 1.0
  zipWithM_ (\str pos -> preservingMatrix $ do
    translate $ Vector3 (-180::Double) (-240+counter-pos) 0
    scale (0.3::GLdouble) 0.3 0.3
    renderString Roman str)
    stuffRoll [0,60..]

  swapBuffers

  if Char 'x' `elem` keystate then do
      gs <- newIORef initialGameState
      return $ Scene $ openingProc ks
   else return $ Scene $ endingProc ks ctr

  where
    stuffRoll = [
     "",
     "",
     "Game Design",
     "     T. Muranushi",
     "",
     "Main Programmer",
     "     H. Tanaka",
     "",
     "Enemy Algorithm",
     "     M. Takayuki",
     "",
     "Graphics Designer",
     "     Euclid",
     "",
     "Monad Designer",
     "     tanakh",
     "",
     "Lazy Evaluator",
     "     GHC 6.4",
     "",
     "Cast",
     "  Player Dodecahedron",
     "  Bullet Tetrahedron",
     "  Enemy  Octahedron",
     "  Boss   Teapot",
     "",
     "Special thanks to",
     "     Simon Marlow",
     "     Haskell B. Curry",
     "",
     "Presented by",
     "     team combat",
     "",
     "",
     "",
     "",
     "",
     "WE LOVE HASKELL!!",
     "",
     "    press x key"]

mainProc gs ks = do
  keystate <- readIORef ks
  modifyIORef gs (updateGameState keystate)
  gamestate <- readIORef gs

  clear [ColorBuffer,DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  renderGameState gamestate
  swapBuffers
  if (isGameover gamestate) then return $ Scene $ openingProc ks else
   if (isClear gamestate) then do 
      counter <- newIORef (0.0::Double)
      --runCommand "ending.mp3"
      return $ Scene $ endingProc ks counter else
    return $ Scene $ mainProc gs ks

timerProc m = m >> addTimerCallback 16 (timerProc m)

keyProc keystate key ks mod pos =
  case (key,ks) of
    (Char 'q',_) -> exitLoop
    (_,Down) -> modifyIORef keystate (nub.(++[key]))
    (_,Up) -> modifyIORef keystate (filter (/=key))


closeProc = do
  putStrLn "closed"
  throwIO $ ExitException ExitSuccess

bosstime=6600
bosstime2=7200

data GameObject = Player {position::Point,shotEnergy::Double,hp::Double}| 
                  Bullet {position::Point} | 
                  EnemyMaker {timer::Int,deathtimer::Int}| 
                  Enemy {position::Point,hp::Double,anime::Int,enemySpec::EnemySpec} |
                  Explosion {position::Point,hp::Double,size::Double}| 
                  EnemyBullet {position::Point,velocity::Point} | 
                  GameoverSignal | 
                  ClearSignal 
                  deriving(Eq)
data EnemySpec = EnemySpec {ways::Int,spread::Double,speed::Double,freq::Int,endurance::Double,boss::Bool} deriving(Eq)
updateObject::GameState->[Key]->GameObject->[GameObject]
updateObject gs ks (Player{position=pos,shotEnergy=sen,hp=oldhp})
 = [(Player{position=newPos,shotEnergy=nsen,hp=newhp})]++shots where
  newPos::Point
  newPos = if(oldhp>0) then (nx,ny) +++ v
                       else (nx,ny)
  newhp=oldhp
  (x,y)=pos
  nx = if(x<(-310)) then -310 else if(x>310) then 310 else x
  ny = if(y<(-230)) then -230 else if(y>200) then 200 else y
  v = (vx,vy) *++ (5.0::Double)
  shots = replicate shotn $ Bullet pos
  nsen = if(shotn/=0) then (-1.0) else 
    if(shotmode==1 && shotn==0) then (sen+0.25) else
    if(shotmode==0) then 0.0 else
    sen
  vx::Double
  vx =
   if((SpecialKey KeyLeft) `elem` ks) then -1 else 0 + 
   if((SpecialKey KeyRight) `elem` ks) then 1 else 0 
  vy =
   if((SpecialKey KeyUp) `elem` ks) then 1 else 0 + 
   if((SpecialKey KeyDown) `elem` ks) then -1 else 0 
  shotmode::Int
  shotmode = 
   if((Char 'z') `elem` ks ) then 1 else 0
  shotn::Int
  shotn = if(oldhp<=0) then 0 else if(shotmode==0) then 0 else
    if(sen>=0) then 1 else 0 
updateObject gs ks (Bullet{position=pos}) = replicate n (Bullet newpos) where
  newpos = pos +++ (0.0,15.0)
  n = if( (\(x,y)->y>250) pos)then 0 else 1
updateObject gs ks (EnemyMaker{timer=t,deathtimer=dtime}) = 
 [EnemyMaker{timer=t+1,deathtimer=newdtime}] ++ enemies ++ deatheffects where
  enemies = replicate n (Enemy{position = (320*sin(dt*dt),240),hp=1.0,anime=0,enemySpec = spec})
  dt::Double
  dt = fromInteger $ toInteger t
  newdtime = dtime + if (hp p<=0 || (bossExist&&hp b<=0)) then 1 else 0
  n = if((t`mod`120==0 && t<=bosstime) || t==bosstime2) then 1 else 0
  deatheffects = if(dtime==0) then [] else 
    if(dtime==120) then [Explosion{position=position deadone,hp=1.0,size=3.0*deathradius}] else
    if(dtime==130) then [Explosion{position=position deadone,hp=1.0,size=3.0*deathradius}] else
    if(dtime==140) then [Explosion{position=position deadone,hp=1.0,size=3.0*deathradius}] else
    if(dtime==240) then [if(hp p<=0) then GameoverSignal else ClearSignal] else
    if(dtime>120) then [] else
    if(dtime`mod`15/=0)then [] else
    [Explosion{position=position deadone +++ ((sin(dt),cos(dt))*++ (16*deathradius)),hp=1.0,size=0.3*deathradius}]
  p = findplayer gs
  b = fromJust mayb
  deadone::GameObject
  deadone = if(hp p<=0) then p else b
  deathradius = if(hp p<=0) then 1 else 3
  bossExist = isJust mayb
  mayb = findBoss gs
  spec = if(t==bosstime2) then (EnemySpec{ways=0,spread=0.1,speed=3.0,freq=10,endurance=300.0,boss=True})
    else speclist!!(t `div` 600)
  speclist = [
     EnemySpec {ways=0,spread=0.1,speed=3.0,freq=30,endurance=2.0,boss=False},
     EnemySpec {ways=1,spread=0.3,speed=5.0,freq=60,endurance=4.0,boss=False},
     EnemySpec {ways=3,spread=0.7,speed=0.2,freq=90,endurance=8.0,boss=False},
     EnemySpec {ways=45,spread=0.069,speed=8.0,freq=450,endurance=1.0,boss=False},
     EnemySpec {ways=0,spread=0.1,speed=1.0,freq=10,endurance=10.0,boss=False},
     EnemySpec {ways=0,spread=0.1,speed=1.0,freq=10,endurance=10.0,boss=False},
     EnemySpec {ways=3,spread=0.1,speed=3.0,freq=60,endurance=6.0,boss=False},
     EnemySpec {ways=1,spread=0.5,speed=7.0,freq=45,endurance=3.0,boss=False},
     EnemySpec {ways=(10),spread=0.3,speed=15.0,freq=115,endurance=5.0,boss=False}
    ] ++ 
     map (\n -> EnemySpec {ways=n,spread=0.1,speed=4.0,freq=20,endurance=3.0,boss=False}) [0,1 ..]

updateObject gs ks oldenemy@(Enemy{position=pos,hp=oldhp,anime=oldanime,enemySpec=spec})  = 
  replicate n (oldenemy{position=newpos,hp=newhp,anime=newanime,enemySpec=newspec}) 
   ++ shots ++ explosions where
    newpos = if isBoss then (200 * sin(danime/100),200 + 40 * cos(danime/80))
      else pos +++ (0.0,-1.0)
    newhp = oldhp
    newanime = oldanime + 1
    newspec = if(not isBoss) then spec else 
     if (oldhp>0.75) then EnemySpec{ways=0,spread=0.1,speed=5.0,freq=10,endurance=300.0,boss=True} else
      if (oldhp>0.50) then EnemySpec{ways=8,spread=0.15,speed=3.0,freq=30,endurance=300.0,boss=True} else
       if (oldhp>0.25) then EnemySpec{ways=2,spread=1.2,speed=15.0,freq=10,endurance=300.0,boss=True} else
        if (oldhp>0.05) then EnemySpec{ways=40,spread=0.075,speed=3.0,freq=60,endurance=400.0,boss=True} else
         if (oldhp>0.00) then EnemySpec{ways=15,spread=0.2,speed=16.0,freq=20,endurance=900.0,boss=True} else
          EnemySpec{ways=(-1),spread=0.1,speed=3.0,freq=10,endurance=300.0,boss=True} 
    danime::Double
    danime = fromInteger (toInteger oldanime)
    explosions = if(oldhp<=0 && not isBoss) then [Explosion{position=pos,hp=1.0,size=1.0}] else []
    shots = if(oldanime`mod` frq /=(frq-1)) then [] else
      map (\v -> EnemyBullet{position=pos,velocity=v}) vs
    vs = (take (wa+1) $ iterate (vdistr***) centerv) ++
         (take wa $ tail $ iterate (vdistrc***) centerv)
    centerv = (pp -+- pos) *++ (spd / (distance pp pos))
    vdistr::Point
    vdistr = (cos(sprd),sin(sprd))
    vdistrc::Point
    vdistrc = (cos(sprd),-sin(sprd))
    pp = playerpos gs
    dv = (playerpos gs) -+- pos
    n = if( (\(x,y)->y<(-250)) pos || (not isBoss && oldhp<=0))then 0 else 1
    wa = ways spec
    spd= speed spec
    frq = freq spec
    sprd= spread spec
    isBoss = boss spec
updateObject gs ks e@(Explosion{}) = if(hp e>0) then [e{hp=hp e - (0.024/(size e))}] else []
updateObject gs ks eb@(EnemyBullet{}) = if(outofmap (position eb)) then [] else 
  [eb{position=position eb+++velocity eb}]
updateObject gs ks go = [go]

watcher os = np ++ ne ++ nb ++ neb ++ others where

  nb = foldr ($) bullets (map bulletEraser enemies)
  ne = foldr ($) enemies (map enemyDamager bullets)
  np = foldr ($) players (map playerDamager ebullets)
  neb= foldr ($) ebullets (map ebEraser players)
  bulletEraser::GameObject->[GameObject]->[GameObject]
  bulletEraser e = filter (\b -> (distance2 (position b) (position e)) > hitr(e))
  enemyDamager::GameObject->[GameObject]->[GameObject]
  enemyDamager b = map (\e ->
    if((distance2 (position b) (position e)) > hitr(e)) 
      then e
      else (\e-> e{hp=hp e-(1.0 / (endurance (enemySpec e)))}) e)
  hitr e = if(boss $ enemySpec e) then sq 100 else sq 32
  sq x = x*x
  playerDamager::GameObject->[GameObject]->[GameObject]
  playerDamager eb = map (\p ->
    if((distance2 (position p) (position eb)) > 70) 
      then p
      else (\p -> p{hp=hp p-0.3}) p)
  ebEraser p = filter (\eb -> (distance2 (position eb) (position p)) > 70)
  (enemies,bullets,ebullets,players,others) = foldl f ([],[],[],[],[]) os
  f (e,b,eb,p,x) o = case o of
    Enemy{}       -> (o:e,b,eb,p,x)
    Bullet{}      -> (e,o:b,eb,p,x)
    EnemyBullet{} -> (e,b,o:eb,p,x)
    Player{}      -> (e,b,eb,o:p,x)
    _             -> (e,b,eb,p,o:x)

renderGameObject Player{position=pos,hp=h} = preservingMatrix $ do
  let (x,y) = pos
  color (Color3 (1.0::Double) h h)
  translate (Vector3 x y 0::Double) 
  scale (10::Double) 10 10
  rotate (x) (Vector3 0 1 0)
  rotate (30::Double) (Vector3 0 0 1)
  renderObject Wireframe  Dodecahedron
renderGameObject Bullet{position=pos} = preservingMatrix $ do
  let (x,y) = pos
  color (Color3 (0.6::Double) 0.6 1.0)
  translate (Vector3 x y 0) 
  scale (4::Double) 18 8
  rotate (45::Double) (Vector3 0 1 0)
  rotate (90::Double) (Vector3 1 0 0)
  renderObject Wireframe Tetrahedron
{-
renderGameObject EnemyMaker{timer=t} 
  | t==bosstime2 = do
    runCommand "boss.mp3"
    return ()
  |otherwise = do
    return ()
-}
renderGameObject Enemy{position=pos,anime=a,hp=h,enemySpec=EnemySpec{boss=False}} = preservingMatrix $ do
  let (x,y) = pos
  color (Color3 (cos rho) (sin rho) (0.0::Double))
  translate (Vector3 x y 0) 
  rotate (2*(theta::Double)) (Vector3 0 1.0 0)
  scale (32::Double) 32 8
  renderObject Wireframe Octahedron where
    theta = fromInteger $ toInteger a
    rho = h * 3.14 / 2
renderGameObject Enemy{position=pos,anime=a,hp=h,enemySpec=EnemySpec{boss=True}} = preservingMatrix $ do
  let (x,y) = pos
  color (Color3 (cos rho) (sin rho) (0.0::Double))
  translate (Vector3 x y 0) 
  rotate (2*(theta::Double)) (Vector3 0 1.0 0)
  scale (120::Double) 120 120
  renderObject Wireframe (Teapot 1.0) where
    theta = fromInteger $ toInteger a
    rho = h * 3.14 / 2
renderGameObject Explosion{position=pos,hp=h,size=s}= preservingMatrix $ do
  let (x,y) = pos
  color (Color3 h 0.0 0.0)
  translate (Vector3 x y 0) 
  rotate (720*h) (Vector3 0 1.0 0)
  rotate (540*h) (Vector3 1.0 0 0)
  scale r r r
  renderObject Wireframe Icosahedron where
    r = s*(100 - h*h*80)
renderGameObject EnemyBullet{position=pos}   = preservingMatrix $ do
  let (x,y) = pos
  color (Color3 (1.0::Double) 1.0 1.0)
  translate (Vector3 x y 0) 
  scale (5::Double) 5 5
  rotate (45::Double) (Vector3 0 0 (1.0::Double))
  renderObject Wireframe  Tetrahedron
renderGameObject go = return()


data GameState = GameState {objects::[GameObject]}
initialGameState = GameState{objects=
  [(Player{position=(0.0,0.0),shotEnergy=0.0,hp=1.0}),(EnemyMaker{timer=0,deathtimer=0})]}
renderGameState GameState{objects=os} = 
  mapM_ renderGameObject os

updateGameState ks gs@(GameState { objects=os }) = newgs where
  newgs = GameState{objects = watcher $ concat $ map (updateObject gs ks) os}

playerpos::GameState->Point
playerpos = position.findplayer
findplayer::GameState->GameObject
findplayer GameState{objects=os} = player where
  [player] = filter(\o -> case o of
    Player{} -> True
    _        -> False) os
findBoss::GameState->Maybe GameObject
findBoss GameState{objects=os} = if(length bosses==0) then Nothing
                                  else Just (head bosses) where
  bosses = filter(\o -> case o of
    Enemy{} -> boss( enemySpec o)
    _        -> False) os

isGameover GameState{objects=os} = (GameoverSignal `elem` os)
isClear GameState{objects=os} = (ClearSignal `elem` os)


type Point = (Double,Double)
data Shape = Edge (Point,Point) | Combo [Shape]

(ax,ay) +++ (bx,by) = (ax+bx,ay+by)
(ax,ay) -+- (bx,by) = (ax-bx,ay-by)
(ax,ay) *++ s       = (ax*s,ay*s)
(ax,ay) *** (bx,by) = (ax*bx-ay*by,ay*bx+ax*by)
distance2::Point->Point->Double
distance2 (ax,ay) (bx,by) = sq(ax-bx)+sq(ay-by) where
  sq x=x*x
distance a b= sqrt(distance2 a b)
outofmap::Point->Bool
outofmap (x,y) = not(abs(x) < 320 && abs(y) < 240)
 
draw::Shape->IO()
draw (Edge ((ax,ay),(bx,by))) = do
  renderPrimitive Lines $ do
    color $ Color3 (1.0::Double) 1.0 1.0
    vertex $ Vertex2 ax ay
    vertex $ Vertex2 bx by
draw (Combo shapes) = do
  mapM_ draw shapes

magnify::Point->Shape->Shape
magnify pz (Edge (pa,pb)) = 
  Edge (pz***pa , pz***pb)
magnify pz (Combo shapes) =
  Combo $ map (magnify pz) shapes

translat::Point->Shape->Shape
translat pz (Edge (pa,pb)) = 
  Edge (pz+++pa , pz+++pb)
translat pz (Combo shapes) =
  Combo $ map (translat pz) shapes

koch::Int->Shape
koch 1 = Edge ((0.0, 0.0),(1.0, 0.0))
koch n = Combo [
  (magnify (0.33,0.0) $ koch (n-1)) ,
  (translat (0.33,0.0) $ magnify (0.16,0.29) $ koch (n-1)) ,
  (translat (0.5,0.29) $ magnify (0.16,-0.29) $ koch (n-1)) ,
  (translat (0.67,0.0) $ magnify (0.33,0.0) $ koch (n-1)) ]

sampleShape = translat(-0.5,-0.25) $ magnify (1.0,0.0) $ koch 5

{-

dispProc = do
  putStrLn "display"
  clear [ColorBuffer,DepthBuffer]
  draw $ sampleShape
  -- koch 5 (-0.9 , -0.4) (0.9 , -0.4)
  swapBuffers

  where
    koch::Int->(Double,Double)->(Double,Double)->IO()
    koch 0 _ _ = return()
    koch 1 (ax,ay) (bx,by)=
      renderPrimitive Lines $ do
        color $ Color3 (1.0::Double) 1.0 1.0
        vertex $ Vertex2 ax ay
        vertex $ Vertex2 bx by
      
    koch n (ax,ay) (bx,by)= do
      koch (n-1) (ax,ay) (ax + 0.33*hx,ay+0.33*hy)
      koch (n-1) (ax + 0.33*hx,ay+0.33*hy) (ax + 0.5*hx + 0.29*vx,ay + 0.5*hy + 0.29*vy)
      koch (n-1) (ax + 0.67*hx,ay+0.67*hy) (bx,by) 
      koch (n-1) (ax + 0.5*hx + 0.29*vx,ay + 0.5*hy + 0.29*vy) (ax + 0.67*hx,ay+0.67*hy)
      where
        vx = -hy
        vy = hx
        hx = bx-ax
        hy = by-ay
-}
