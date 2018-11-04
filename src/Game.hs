{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, FlexibleContexts #-}

module Game(Game, runGame, GameState, Reason(..)
           , makeGS
           , initGame
           , gameOver
           , checkEnergyLevels
           , commandHelp
           , getMaxWarp
           , validateWarp
           , badWarp
           , navigation
           , shortRangeScan
           , longRangeScan
           , validateCanFirePhotons
           , getMaxPhaserEnergy
           , firePhasers
           , fireTorpedo
           , damageControlReport
           , getMaxShieldEnergy
           , validateShields 
           , shieldControl
           , starbaseCanRepairDamage
           , starbaseRepairDamage
           , validateComputerAvailable 
           , computerHelp
           , galacticRecord
           , statusReport
           , photonTorpedoData
           , starbaseNavData
           , galaxyMap
           , dirDistCalc
           , getEntCoords) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Array
import Data.List
import System.Random

data GameState = GameState 
  { gsTime :: Float
  , gsInitTime :: Float
  , gsMaxTime :: Float
  , gsTotKlingons :: Int
  , gsInitTotKlingons :: Int
  , gsTotBases :: Int
  , gsEntQuad :: (Int, Int)
  , gsEntSect :: (Int, Int)
  , gsGalaxy :: Array (Int, Int) Population
  , gsGalaxyRecord :: Array (Int, Int) Population
  , gsMaxShields :: Float
  , gsShields :: Float
  , gsQuadrant :: Array (Int, Int) Entity
  , gsKlingons :: Array Int KlingonData
  , gsCondition :: String
  , gsDocked :: Bool
  , gsEnergy :: Float
  , gsInitEnergy :: Float
  , gsPhotons :: Int
  , gsInitPhotons :: Int
  , gsDamage :: Array DamageType Float
  , gsCourse :: Array Int (Float, Float)
  } deriving (Show)

data Population = Population
  { pKlingons :: Int
  , pBases :: Int
  , pStars :: Int
  } deriving (Show)

data KlingonData = KlingonData
  { kPos :: (Int, Int)
  , kShields :: Float
  } deriving (Show)

data Entity = Empty | Klingon | Base | Star | Enterprise
  deriving (Show, Eq)

data DamageType = WarpEngines | ShortRangeSensors | LongRangeSensors | PhaserControl
                | PhotonTubes | DamageControl | ShieldControl | Computer
  deriving (Show, Ix, Ord, Eq, Enum)

data Reason = Resigned | Stranded | Destroyed | TimeExceeded | BasesGone | KlingonsGone
            | InputError [String] | OutOfBounds (Int, Int) (Int, Int) | BadNavigation (Int, Int)
  deriving (Show, Eq)

newtype Game a = Game {
    runG :: ExceptT Reason (WriterT [String] (State (GameState, StdGen))) a
  } deriving (Monad, Applicative, Functor, MonadError Reason, MonadWriter [String], MonadState (GameState, StdGen))

runGame :: Game a -> (GameState, StdGen) -> ((Either Reason a, [String]), (GameState, StdGen))
runGame k = runState (runWriterT $ runExceptT $ runG k)

zipWithPos :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipWithPos f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)

zipWithPos3 :: (a -> b -> c -> d) -> (a, a) -> (b, b) -> (c, c) -> (d, d)
zipWithPos3 f (a1, a2) (b1, b2) (c1, c2) = (f a1 b1 c1, f a2 b2 c2)

mapPos :: (a -> b) -> (a, a) -> (b, b)
mapPos f (a1, a2) = (f a1, f a2)

inside (s1, s2) = s1 > 0 && s1 < 9 && s2 > 0 && s2 < 9
truncPos (s1, s2) = (truncate s1, truncate s2)
floatPos (s1, s2) = (fromIntegral s1, fromIntegral s2)

makeGS :: GameState
makeGS = GameState 0 0 0 0 0 0 zpos zpos zpop zpop s9 0 zquad zks "GREEN" False e e p p zdmg course
  where zpos = (0, 0)
        quad = ((1, 1), (8, 8))
        zpop = listArray quad $ repeat $ Population 0 0 0
        zquad = listArray quad $ repeat Empty
        zks = listArray (1, 3) $ repeat $ KlingonData zpos 0
        s9 = 200
        e = 3000
        p = 10
        zdmg = listArray (WarpEngines, Computer) $ repeat 0
        course = makeCourse

makeCourse = listArray (1, 9) $
  [ ( 0,  1)
  , (-1,  1)
  , (-1,  0)
  , (-1, -1)
  , ( 0, -1)
  , ( 1, -1)
  , ( 1,  0)
  , ( 1,  1)
  , ( 0,  1)
  ]

getGS :: MonadState (GameState, StdGen) m => m GameState
getGS = get >>= return . fst

putGS :: GameState -> Game ()
putGS gs = get >>= put . (gs ,) . snd 

rnd :: Game Float
rnd = do
  (a, g) <- get
  let (v, g') = randomR (0, 1) g
  put (a, g')
  return v

rndN :: Int -> Game [Float]
rndN n = sequence $ replicate n rnd

rndRN n = map rndR <$> rndN n
rndR r = truncate $ r * 7.98 + 1.01

gameOver :: Reason -> GameState -> (Bool, [String])
gameOver reason gs = 
  runWriter $ do 
      let rating = 1000 * ((fromIntegral $ gsInitTotKlingons gs) / ((gsTime gs) - (gsInitTime gs)))^2
      tell $ case reason of
               Resigned     -> []
               Stranded     -> [ ""
                               , "** FATAL ERROR ** You've just stranded your ship in space"
                               , "You have insufficient manuevering energy, and shield control"
                               , "is presently incapable of cross-circuting to engine room!!"
                               ]
               Destroyed    -> [ ""
                               , "The Enterprise has been destroyed. The Federation will be conquered" 
                               ]
               BasesGone    -> [ ""
                               , "That does it, Captain!! You are hereby relieved of command"
                               , "and sentenced to 99 stardates of hard labor on Cygnus 12!!"
                               ]
               KlingonsGone -> [ "Congratulations, Captain! The last klingon battle cruiser"
                               , "menacing the Federation has been destroyed."
                               , "Your efficiency rating is " ++ (show rating)
                               ]
      let k9 = show $ gsTotKlingons gs
          b9 = gsTotBases gs
      if reason /= KlingonsGone
        then tell [ "There were " ++ k9 ++ " klingon battle cruisers left at the end of your mission." ]
        else return ()
      tell [ "" ]
      tell [ "" ]
      return $ b9 /= 0 

commandHelp :: Game ()
commandHelp = do
  tell [ "Enter one of the following:" 
       , "  NAV (to set course)"
       , "  SRS (for short range sensor scan)"
       , "  LRS (for long range sensor scan)"
       , "  PHA (to fire phasers)"
       , "  TOR (to fire photon torpedos)"
       , "  SHE (to raise or lower shields)"
       , "  DAM (for damage control reports)"
       , "  COM (to call on library-computer)"
       , "  XXX (to resign your command)"
       , ""
       ]

dist :: Int -> Game Float
dist i = do
  gs <- getGS
  let (k1, k2) = kPos $ (gsKlingons gs)!i
      (s1, s2) = gsEntSect gs
  return $ sqrt $ fromIntegral $ (k1 - s1)^2 + (k2 - s2)^2

initGame = setStartTime   >> 
           setStartPos    >>
           setPopulation  >>
           fixPopulation  >>
           showOrders     >>
           enterQuadrant

showOrders = do
  gs <- getGS
  let k9 = show $ gsTotKlingons gs
      b9 = gsTotBases gs
      b9s = show b9
      t = show $ (gsInitTime gs) + (gsMaxTime gs)
      t9 = show $ gsMaxTime gs
      (x0, x) = if b9 /= 1 then ("are", "s") else ("is", "")
  tell [ "Your orders are as follows:"
       , "    Destroy the " ++ k9 ++ " klingon warships which have invaded"
       , "    the galaxy before they can attack federation headquarters"
       , "    on stardate " ++ t ++ ". This gives you " ++ t9 ++ " days."
       , "    There " ++ x0 ++ " " ++ b9s ++ " starbase" ++ x ++ " in the galaxy for resupplying your ship"
       , ""
       ]

setStartTime :: Game ()
setStartTime = do
  gs <- getGS
  [r1, r2] <- rndN 2
  let t = fromIntegral $ truncate $ r1 * 20 + 20
      t9 = fromIntegral $ 25 + truncate (r2 + 10)
  putGS gs { gsTime = t, gsInitTime = t, gsMaxTime = t9 }

setStartPos :: Game ()
setStartPos = do
  gs <- getGS
  [r1, r2, r3, r4] <- rndRN 4
  putGS gs { gsEntQuad = (r1, r2), gsEntSect = (r3, r4) }

setPopulation :: Game ()
setPopulation = do
  gs <- getGS
  population <- zipWith3 Population <$> (map klingonCnt <$> rndN 64) 
                                    <*> (map baseCnt    <$> rndN 64) 
                                    <*> rndRN 64
  let (k9, b9) = foldl (\(k9, b9) (Population k3 b3 _) -> (k9 + k3, b9 + b3)) (0, 0) population
      t9 = gsMaxTime gs
  putGS gs { gsGalaxy = listArray ((1, 1), (8, 8)) population 
           , gsTotKlingons = k9
           , gsInitTotKlingons = k9
           , gsTotBases = b9
           , gsMaxTime = if fromIntegral (k9) > t9 then fromIntegral (k9 + 1) else t9
           }

fixPopulation :: Game ()
fixPopulation = do
  gs <- getGS
  [r1, r2] <- rndRN 2
  let k9 = gsTotKlingons gs
      b9 = gsTotBases gs
      r = (r1, r2)
      g = gsGalaxy gs
      Population k3 b3 s3 = g!r
      k3' = if k3 < 2 then k3 + 1 else k3
      pop' = Population k3' 1 s3
  putGS $ if b9 /= 0 
          then gs
          else gs { gsGalaxy = g//[(r, pop')]
                  , gsTotKlingons = k9 + 1
                  , gsTotBases = 1
                  }

klingonCnt r
  | r > 0.98  = 3
  | r > 0.95  = 2
  | r > 0.80  = 1
  | otherwise = 0

baseCnt r
  | r > 0.96  = 1
  | otherwise = 0

enterQuadrant = do
  gs <- getGS
  if inside (gsEntSect gs)
    then do showQuadrantInfo
            resetKlingonPositions
    else return ()
  populateQuadrant
  setKlingonShields
  shortRangeScan

showQuadrantInfo = do
  gs <- getGS
  let t = gsTime gs
      t0 = gsInitTime gs
      q = gsEntQuad gs
      g2 = (quadrantName q) ++ " " ++ (sectorName q)
      Population k3 b3 s3 = (gsGalaxy gs)!q
      s = gsShields gs
  if t == t0
    then tell [ "Your mission begins with your starship located in the galactic quadrant " ++ g2 ]
    else tell [ "Now entering " ++ g2 ++ " quadrant . . ." ]
  tell [""]
  tell $ if k3 /= 0
    then [ "Combat Area       Condition RED" ] ++ 
         if s <= 200 then
         [ "    Shields dangerously low" ] 
         else []
    else []

resetKlingonPositions = do
  gs <- getGS
  let ks' = map (\(p, k) -> (p, k { kPos = (0, 0) }))  $ assocs $ gsKlingons gs
  putGS gs

populateQuadrant = do
  gs <- getGS
  let q = gsEntQuad gs
      s = gsEntSect gs
      Population k3 b3 s3 = (gsGalaxy gs)!q
      empty = [(r, c) | r <- [1..8], c <- [1..8]]\\[s]
      init = ([(s, Enterprise)], empty, 63)
  (population, _, _) <- populateEntity Klingon k3 init >>=
                        populateEntity Base    b3      >>=
                        populateEntity Star    s3 
  let ks = gsKlingons gs
      kps = map fst $ filter ((Klingon==) . snd) population
      ks' = zipWith (\kp i -> (i, (ks!i) { kPos = kp })) kps [1..k3]
  putGS gs { gsQuadrant = (listArray ((1, 1), (8, 8)) $ repeat Empty)//population 
           , gsKlingons = (gsKlingons gs)//ks'
           , gsGalaxyRecord = (gsGalaxyRecord gs)//[(q, (gsGalaxy gs)!q)]
           }

populateEntity e n init = do 
  let randPart (ps, eps, n) r = ((ep, e):ps, leps ++ reps, n - 1)
          where (leps, (ep:reps)) = splitAt sp eps
                sp = truncate $ r * (fromIntegral n - 1)
  rndN n >>= return . foldl randPart init

checkEnergyLevels :: Game ()
checkEnergyLevels = do
  gs <- getGS
  let s = gsShields gs
      e = gsEnergy gs
      dmg = (gsDamage gs)!ShieldControl
  if s + e > 10 && (e > 10 || dmg == 0)
    then return ()
    else throwError Stranded

setKlingonShields = do
  gs <- getGS
  let s9 = gsMaxShields gs
      ks = gsKlingons gs
      Population k3 _ _ = (gsGalaxy gs)!(gsEntQuad gs)
      setShield s i = (i, (ks!i) { kShields = s })
      shldAmt r = s9 * (0.5 + r)
  shldAmts <- rndN k3 >>= return . map shldAmt
  let restZero = replicate (3 - k3) 0
      ks' = zipWith setShield (shldAmts ++ restZero) [1..3]
  putGS gs { gsKlingons = ks//ks' }

getMaxWarp :: Game Float
getMaxWarp = do
  gs <- getGS
  let damaged = (gsDamage gs)!WarpEngines < 0 
  return $ if damaged then 0.2 else 8

validateWarp :: Float -> Game (Float, Float)
validateWarp w1 = do
  gs <- getGS
  let e = gsEnergy gs
      s = gsShields gs
      n = w1 * 8 + 0.5
      damaged = (gsDamage gs)!WarpEngines < 0 
      damagedErr = throwError $ InputError
        [ "Warp engines are damaged. Maximum speed = Warp 0.2" ] 
      energyErr n = throwError $ InputError $
        [ "Engineering reports 'Insufficient energy available for maneuvering at warp " ++ (show w1) ++ "!" ]
          ++ if s < (n - e) || damaged
               then []
               else [ "Deflector control room acknowledges " ++ (show s) ++ 
                      " units of energy presently deployed to shields." ] 
  if w1 < 0 || w1 > 8 
    then badWarp $ show w1
  else if damaged && w1 > 0.2
    then damagedErr
  else if e - n < 0
    then energyErr n
  else return (w1, n)

badWarp w1 = throwError $ InputError 
  [ "   Chief Engineer Scott reports, 'The engines won't take warp " ++ w1 ++ "!'" ] 

navigation :: Float -> Float -> Float -> Game ()
navigation c1 w1 n = do
  moveKlingons
  klingonAttack
  repairDamage w1
  moveEnterprise c1 w1 (truncate n)

moveKlingons = do
  gs <- getGS
  let kps = filter ((0/=) . kShields . snd) $ assocs $ gsKlingons gs
      n = length kps
      quad = (gsQuadrant gs)//(map ((, Empty) . kPos . snd) kps)
      empty = filter ((Empty==) . snd) $ assocs quad
      init = foldl (\(es, eps, n) (ep, _) -> (es, ep:eps, n + 1)) ([], [], 0) empty
  (kpop, _, _) <- populateEntity Klingon n init
  let kps' = zipWith (\(i, KlingonData _ ks) (p', _) -> (i, KlingonData p' ks)) kps kpop 
  putGS gs { gsQuadrant = quad//kpop, gsKlingons = (gsKlingons gs)//kps' }

repairDamage w1 = do
  gs <- getGS
  let d6 = if w1 >= 1 then 1 else w1
      repDmg = map (\(dt, amt) -> 
              let amt' = amt + d6 
                  amt'' = if amt' > -0.1 && amt' < 0 then -0.1 else amt'
              in (dt, amt'')) $ 
            filter ((<0) . snd) $ assocs $ gsDamage gs
  putGS gs { gsDamage = (gsDamage gs)//repDmg }
  reportCompletedRepairs repDmg
  reportAdditionalDamage

reportCompletedRepairs repDmg = do
  let repaired = map (\dt -> (showDamageType dt) ++ " repair completed") $ 
                 map fst $ filter ((>=0) . snd) repDmg
  if null repaired
    then return ()
    else tell $ "Damage Control Report":repaired

reportAdditionalDamage = do
  gs <- getGS
  [r1, r2, r3] <- rndN 3
  let dt = randDamageType r1
      (amt, outcome) = if r2 >= 0.2
        then (r3 * 3 + 1, "state of repair improved")
        else (-(r3 * 5 + 1), "damaged") 
  tell [ "Damage Control Report", (showDamageType dt) ++ " " ++ outcome ]
  putGS gs { gsDamage = (gsDamage gs)//[(dt, (gsDamage gs)!dt + amt)] }

course c1 gs = 
  let dc = c1 - fromIntegral (truncate c1)
      c = gsCourse gs
  in zipWithPos (\a b -> a + (b - a) * dc) (c!(truncate c1)) (c!(truncate $ c1 + 1))

moveEnterprise c1 w1 n = do
  gs <- getGS
  let q = gsEntQuad gs
      s = gsEntSect gs
      c = gsCourse gs
      quad = (gsQuadrant gs)//[(s, Empty)]
      x = course c1 gs
  (s', q') <- foldM (move n x quad s) (floatPos s, q) [1..n]
      `catchError` (\e -> case e of
          OutOfBounds q' s' -> do
            tell [ "Lt. Uhura reports message from Starfleet Command:" 
                 , "  'Permission to attempt crossing galactic perimeter"
                 , "   is hereby *DENIED*. Shut down your engines.'"
                 , "Chief Engineer Scott reports 'Warp engines shut down"
                 , "at sector " ++ (show s) ++ " of quadrant " ++ (show q)
                 ]
            getGS >>= checkTimeExceeded . gsTime
            return (floatPos s', q')
          BadNavigation s' -> do
             tell [ "Warp engines shut down at sector " ++ (show s) ++ " due to bad navigation." ]
             return (floatPos s', q)
          e -> throwError e)
  let s'' = truncPos s'
      quad' = quad//[(s'', Enterprise)]
  putGS gs { gsEntQuad = q', gsEntSect = s'', gsQuadrant = quad' }
  if q == q'
    then do consumeEnergy n
            consumeTime w1
            shortRangeScan
    else do advanceStardate
            consumeEnergy n
            enterQuadrant

move n x quad s0 (s, q) _ = do
  let s' = zipWithPos (+) s x
  if not (inside s')
    then nextQuadrant s0 q n x
  else if quad!(truncPos s') /= Empty
    then throwError $ BadNavigation (truncPos s)
    else return (s', q) 

nextQuadrant s q n x = do
  let x' = zipWithPos3 (\q s x -> 8 * q + s + (fromIntegral n) * x)
           (floatPos q) (floatPos s) x
      q' = truncPos $ mapPos (/8) x'
      s' = zipWithPos (\x q -> x - q * 8) x' (floatPos q')
  checkBounds $ fixCoords (s', q')

fixCoords ((s1, s2), (q1, q2)) =
  let (q1', s1') = if s1 == 0 then (q1 - 1, 8) else (q1, s1)
      (q2', s2') = if s2 == 0 then (q2 - 1, 8) else (q2, s2)
  in ((s1', s2'), (q1', q2'))

checkBounds ((s1, s2), (q1, q2)) = do
  let (q1', s1', x1') = 
          if q1 < 1 
            then (1, 1, True)
          else if q1 > 8
            then (8, 8, True)
            else (q1, s1, False)
      (q2', s2', x2') =
          if q2 < 1
            then (1, 1, True)
          else if q2 > 8
            then (8, 8, True)
            else (q2, s2, False)
  if x1' || x2' 
    then throwError $ OutOfBounds (q1', q2') (truncate s1', truncate s2')
    else return ((s1, s2), (q1, q2)) 

consumeEnergy n = do
  gs <- getGS
  let e' = (gsEnergy gs) - (fromIntegral n) - 10
  if e' >= 0
    then putGS gs { gsEnergy = e' }
    else do tell [ "Shield control supplies energy to complete the maneuver" ]
            let s' = max 0 $ (gsShields gs) + e'
            putGS gs { gsEnergy = 0, gsShields = s' }

consumeTime w1 = do
  gs <- getGS
  let t8 = if w1 < 1
             then 0.1 * (fromIntegral $ truncate (10 * w1))
             else 1
      t' = (gsTime gs) + t8
  checkTimeExceeded t'
  putGS gs { gsTime = t' }

advanceStardate = do
  gs <- getGS
  putGS gs { gsTime = (gsTime gs) + 1 }

checkTimeExceeded t = do
  gs <- getGS
  let t0 = (gsInitTime gs)
      t9 = (gsMaxTime gs)
  if t > t0 + t9
    then throwError TimeExceeded 
    else return ()

longRangeScan = do
  gs <- getGS
  let o1 = "-------------------"
      (q1, q2) = gsEntQuad gs
      g = gsGalaxy gs
      gr = gsGalaxyRecord gs
      (disp, grs) = foldl rows ([], []) [q1-1..q1+1]
      rows (rs, grs) r = (rs ++ [cs ++ ":" ], grs') 
          where (cs, grs') = foldl (cols r) ("", grs) [q2-1..q2+1]
      cols r (cs, grs) c = if r > 0 && r < 9 && c > 0 && c < 9
          then let s = (r, c)
               in (cs ++ ": " ++ (showPopulation $ g!s) ++ " ", 
                   (s, g!s):grs)
          else (cs ++ ": *** ", grs)
  if (gsDamage gs)!LongRangeSensors < 0 
    then tell [ "Long Range Sensors are inoperable" ]
    else do tell $ [ "Long Range Scan for Quadrant " ++ (show (q1, q2)), o1 ]
                   ++ (intersperse o1 disp)
                   ++ [ o1 ]
            putGS gs { gsGalaxyRecord = (gsGalaxyRecord gs)//grs }

showPopulation (Population k3 b3 s3) = (show k3) ++ (show b3) ++ (show s3)

isZeroPop (Population 0 0 0) = True
isZeroPop _                  = False

getMaxPhaserEnergy :: Game Float
getMaxPhaserEnergy = do
  gs <- getGS
  let dmg = gsDamage gs
      Population k3 _ _ = (gsGalaxy gs)!(gsEntQuad gs)
      e = gsEnergy gs
  if dmg!PhaserControl < 0
    then throwError $ InputError [ "Phasers Inoperative" ]
  else if k3 == 0
    then throwError $ InputError [ "Science Officer Spock reports, 'Sensors show no enemy ships in this quadrant" ]
  else do tell $ if dmg!Computer < 0
                   then [ "Computer failure hampers accuracy" ]
                   else []
          return e

firePhasers :: Float -> Game ()
firePhasers x = do
  gs <- getGS
  [r1, r2] <- rndN 2
  let e = gsEnergy gs
      e' = e - x
      x' = if (gsDamage gs)!ShieldControl < 0
             then x * r1
             else x
      Population k3 _ _ = (gsGalaxy gs)!(gsEntQuad gs)
      h1 = fromIntegral $ truncate $ x' / (fromIntegral k3)
      hit (i, k) = do 
        d <- dist i
        let h = (fromIntegral $ truncate (h1 / d)) * (r2 + 2)
        return ((i, k), h)
      ks = filter ((> 0) . kShields . snd) $ assocs $ gsKlingons gs
  if x <= 0
    then return ()
    else do hs <- mapM hit ks 
            kds' <- foldM (\kds ((i, KlingonData kp ks), h) -> do
                      if h > 0.15 * ks
                        then do tell [ (show h) ++ " unit hit on klingon at sector " ++ (show kp) ]
                                let ks' = ks - h
                                if ks' <= 0
                                  then do tell [ "*** KLINGON DESTROYED ***" ]
                                          destroyKlingon kp
                                  else tell [ "   (sensors show " ++ (show ks') ++ " units remaining)" ]
                                return $ (i, KlingonData kp ks'):kds 
                        else do tell [ "Sensors show no damage to enemy at " ++ (show kp) ]
                                return kds) [] hs
            gs' <- getGS
            putGS gs' { gsKlingons = (gsKlingons gs')//kds', gsEnergy = e' }
            klingonAttack

validateCanFirePhotons :: Game ()
validateCanFirePhotons = do
  gs <- getGS
  let p = gsPhotons gs
  if p <= 0 
    then throwError $ InputError [ "All photon torpedos expended" ]
  else if (gsDamage gs)!PhotonTubes < 0
    then throwError $ InputError [ "Photon tubes are not operational" ]
  else
    return ()

fireTorpedo :: Float -> Game ()
fireTorpedo c1 = do
  consumeTorpedo
  gs <- getGS
  let c = gsCourse gs
      dx = course c1 gs
      quad = gsQuadrant gs
      xs = map (\p -> (p, quad!p)) $ takeWhile inside $ map (truncPos . mapPos (+0.5)) 
                                   $ iterate (zipWithPos (+) dx) (floatPos $ gsEntSect gs)
  tell [ "Torpedo Track:" ]
  track xs

consumeTorpedo = do
  gs <- getGS
  putGS gs { gsEnergy = (gsEnergy gs) - 2
           , gsPhotons = (gsPhotons gs) - 1
           }

track [] = do
  tell [ "Torpedo Missed" ]
  klingonAttack
track ((p, Star):xs) = do
  tell [ showTrack p, "Star at " ++ (show p) ++ " absorbed torpedo energy" ]
  klingonAttack
track ((p, Base):xs) = do
  tell [ showTrack p, "*** STARBASE DESTROYED ***" ]
  destroyStarbase p
  klingonAttack
track ((p, Klingon):xs) = do
  tell [ showTrack p, "*** KLINGON DESTROYED ***" ]
  destroyKlingon p
  klingonAttack
track ((p, _):xs) = tell [ showTrack p ] >> track xs

showTrack p = "    " ++ show p

destroyKlingon :: (Int, Int) -> Game ()
destroyKlingon p = do
  gs <- getGS
  let q = gsEntQuad gs
      g = gsGalaxy gs
      k9 = gsTotKlingons gs - 1
      Population k3 b3 s3 = g!q 
      pop' = Population (k3 - 1) b3 s3
      ks = map (\(i, _) -> (i, KlingonData p 0)) $ filter ((p==) . kPos . snd) $ assocs $ gsKlingons gs
  if k9 <= 0
    then throwError KlingonsGone
    else do putGS gs { gsTotKlingons = k9, gsKlingons = (gsKlingons gs)//ks }
            updatePopulation pop' q p

destroyStarbase :: (Int, Int) -> Game ()
destroyStarbase p = do
  gs <- getGS
  let q = gsEntQuad gs
      g = gsGalaxy gs
      Population k3 b3 s3 = g!q 
      b9 = gsTotBases gs - 1
      k9 = gsTotKlingons gs
      t = gsTime gs
      t0 = gsInitTime gs
      t9 = gsMaxTime gs
      pop' = Population k3 (b3 - 1) s3
  putGS gs { gsTotBases = b9, gsDocked = False }
  updatePopulation pop' q p
  if b9 > 0 || fromIntegral k9 > t - t0 - t9
    then tell [ "Starfleet Command reviewing your record to consider court martial!" ]
  else
    throwError BasesGone

updatePopulation pop q p = do
  gs <- getGS
  putGS gs { gsGalaxy = (gsGalaxy gs)//[(q, pop)]
           , gsGalaxyRecord = (gsGalaxyRecord gs)//[(q, pop)]
           , gsQuadrant = (gsQuadrant gs)//[(p, Empty)]
           }

getMaxShieldEnergy :: Game Float
getMaxShieldEnergy = do
  gs <- getGS
  let e = gsEnergy gs
      s = gsShields gs
  if (gsDamage gs)!ShieldControl < 0
    then throwError $ InputError [ "Shield Control inoperable" ]
    else return $ e + s

validateShields :: Float -> Game Float
validateShields x = do
  gs <- getGS
  let e = gsEnergy gs
      s = gsShields gs
  if x > 0 && x > e + s
    then throwError $ InputError 
      [ "Shield Control reports, 'This is not the Federation Treasury.'" 
      , "<Shields Unchanged>"
      ]
    else return x

shieldControl :: Float -> Game ()
shieldControl x = do
  gs <- getGS
  let s = gsShields gs
      e = gsEnergy gs
  if x < 0 || s == x
    then tell [ "<Shields Unchanged>" ]
    else do putGS gs { gsEnergy = e + s - x, gsShields = x }
            tell [ "Deflector Control Room Report:"
                 , "  'Shields now at " ++ (show x) ++ " units per your command.'"
                 ]

damageControlReport :: Game ()
damageControlReport = do
  gs <- getGS
  if (gsDamage gs)!DamageControl < 0
    then tell [ "Damage Control Report not available" ]
    else tell $ [ ""
                , "Device                  State of Repair"
                ]
                ++ (map (\(dt, amt) -> 
                           let dts = showDamageType dt
                               spc = (replicate (24 - length dts) ' ')
                               dispAmt = (fromIntegral $ truncate $ amt * 100) * 0.01
                           in dts ++ spc ++ show dispAmt) $ assocs $ gsDamage gs)

damageAmount :: Game (Maybe Float)
damageAmount = do
  gs <- getGS
  r <- rnd
  let d3 = foldl (\d3 _ -> d3 + 0.1) 0 $ filter (<0) $ elems $ gsDamage gs
      d4 = 0.5 * r 
      d3' = d3 + d4
      d3'' = if d3' >=1 then 0.9 else d3'
  return $ if d3 == 0
    then Nothing
    else Just d3''

repairTimeEstimate d3 = 0.1 * fromIntegral (truncate $ 100 * d3)

starbaseCanRepairDamage :: Game (Maybe Float)
starbaseCanRepairDamage = do 
  gs <- getGS
  if gsDocked gs
    then do d3m <- damageAmount
            case d3m of
              Just d3 -> do tell $ [ ""
                                   , "Technicians standing by to effect repairs to your ship;"
                                   , "Estimated time to repair: " ++ (show $ repairTimeEstimate d3) ++ " stardates"
                                   ]
                            return $ Just d3
              Nothing -> return Nothing
    else return Nothing

starbaseRepairDamage :: Float -> Game ()
starbaseRepairDamage d3 = do
  gs <- getGS
  let dmg' = map ((, 0) . fst) $ filter ((<0) . snd) $ assocs $ gsDamage gs
      t' = (gsTime gs) + d3 + 0.1
  putGS gs { gsDamage = (gsDamage gs)//dmg', gsTime = t' }

klingonAttack = do
  gs <- getGS
  let Population k3 _ _ = (gsGalaxy gs)!(gsEntQuad gs)
      ks = filter ((>0) . kShields . snd) $ assocs $ gsKlingons gs
  if k3 <= 0
    then return ()
  else if gsDocked gs
    then tell [ "Starbase shields protect the Enterprise" ]
  else 
    foldM hit gs ks >>= putGS

hit gs (i, KlingonData kp ks) = do
  d <- dist i 
  [r1, r2, r3, r4] <- rndN 4
  let h = (ks / d) * (2 + r1)
      s' = (gsShields gs) - h
      ks' = ks / (3 + r1)
      dmg = gsDamage gs
      dt = randDamageType r3
  tell [ (show h) ++ " unit hit on Enterprise from sector " ++ (show kp) ]
  if s' <= 0
    then throwError Destroyed
    else do tell [ "    <Shields down to " ++ (show s') ++ " units>" ]
            dmg' <- if h >= 20 && r2 <= 0.60 && s' > 0.2 
                      then do tell [ "Damage control reports " ++ (showDamageType dt) ++ " damaged by the hit" ]
                              return $ dmg//[(dt, dmg!dt - h / s' - 0.5 * r4)]
                      else return dmg
            return gs { gsShields = s'
                      , gsKlingons = (gsKlingons gs)//[(i, KlingonData kp ks')] 
                      , gsDamage = dmg'
                      }

shortRangeScan = do
  gs <- getGS
  setCondition
  let quad = gsQuadrant gs
      o = "--------------------------------"
      row rs r = rs ++ [(foldl (col r) "" [1..8]) ++ (info r)]
      col r cs c = cs ++ " " ++ (showEntity $ quad!(r, c))
      info r = case r of
                 1 -> "   Stardate           " ++ (show $ (gsTime gs) * 10 * 0.10)
                 2 -> "   Condition          " ++ (gsCondition gs)
                 3 -> "   Quadrant           " ++ (show $ gsEntQuad gs)
                 4 -> "   Sector             " ++ (show $ gsEntSect gs)
                 5 -> "   Photon Torpedos    " ++ (show $ gsPhotons gs)
                 6 -> "   Total Energy       " ++ (show $ (gsEnergy gs) + (gsShields gs))
                 7 -> "   Shields            " ++ (show $ gsShields gs)
                 8 -> "   Klingons Remaining " ++ (show $ gsTotKlingons gs)
                 _ -> ""
  tell $ if (gsDamage gs)!ShortRangeSensors >= 0 
    then [o] ++ (foldl row [] [1..8]) ++ [o]
    else [ "*** Short Range Sensors are out ***" ]

showEntity Empty      = "   "
showEntity Enterprise = "<*>"
showEntity Klingon    = "+K+"
showEntity Base       = ">!<"
showEntity Star       = " * "

setCondition = do
  replenishShip
  gs <- getGS
  let docked = gsDocked gs
      Population k3 _ _ = (gsGalaxy gs)!(gsEntQuad gs)
      e0 = gsInitEnergy gs
      e = gsEnergy gs
      condition = if docked
                    then "DOCKED"
                  else if k3 > 0
                    then "*RED*"
                  else if e < e0 * 0.10
                    then "YELLOW"
                    else "GREEN"
  putGS gs { gsCondition = condition }

replenishShip = do
  gs <- getGS
  let docked = isDocked gs
      e0 = gsInitEnergy gs
      p0 = gsInitPhotons gs
      e = gsEnergy gs
      p = gsPhotons gs
      s = gsShields gs
      (e', p', s') = if docked then (e0, p0, 0) else (e, p, s)
  tell $ if docked 
           then [ "SHIELDS DROPPED FOR DOCKING PURPOSES" ]
           else []
  putGS gs { gsEnergy = e'
           , gsPhotons = p'
           , gsShields = s'
           , gsDocked = docked
           }

isDocked gs = not $ null $ filter (Base==) ents
  where ents = map (quad!) coords
        coords = [(r, c) | r <- [s1-1 .. s1+1], c <- [s2-1 .. s2+1]] `intersect` (indices quad)
        (s1, s2) = gsEntSect gs
        quad = gsQuadrant gs

validateComputerAvailable :: Game ()
validateComputerAvailable = do
  gs <- getGS
  if (gsDamage gs)!Computer < 0
    then throwError $ InputError [ "Computer Disabled" ]
    else return ()

computerHelp :: Game ()
computerHelp = do
  tell [ "Functions available from library computer:"
       , "    0 = Cumulative galactic record"
       , "    1 = Status report"
       , "    2 = Photon torpedo data"
       , "    3 = Starbase nav data"
       , "    4 = Direction/distance calculator"
       , "    5 = Galaxy 'region name' map"
       , ""
       ]

galacticRecord :: Game ()
galacticRecord = do
  gs <- getGS
  let o1 = "     ----- ----- ----- ----- ----- ----- ----- ----- "
      q = gsEntQuad gs
      disp = foldl rows [] [1..8]
      rows rs r = rs ++ [ (show r) ++ "   " ++ cs ]
          where cs = foldl (cols r) "" [1..8]
      cols r cs c = cs ++ "   " ++ if isZeroPop pop
                      then "***"
                      else showPopulation pop
          where pop = (gsGalaxyRecord gs)!(r, c)

  tell $ [ ""
         , "         Computer Record of Galaxy For Quadrant " ++ (show q)
         ] 
         ++ galaxyReport disp

galaxyMap :: Game ()
galaxyMap = do
  let disp = foldl rows [] [1..8]
      rows rs r = rs ++ [foldl (cols r) "     " [1, 5]]
      cols r cs c = cs ++ (center $ quadrantName (r, c)) ++ " "
      center s = let n = 23 - length s
                     left = n `div` 2
                     right = n - left
                 in (replicate left ' ') ++ s ++ (replicate right ' ')
  tell $ [ ""
         , "                      The Galaxy"
         ] 
         ++ galaxyReport disp

galaxyReport disp = 
  let o1 = "     ----- ----- ----- ----- ----- ----- ----- ----- "
  in [ ""
     , "        1     2     3     4     5     6     7     8  "
     , o1 
     ]
     ++ (intersperse o1 disp)
     ++ [ o1 ]

statusReport :: Game ()
statusReport = do
  gs <- getGS
  let k9 = gsTotKlingons gs
      x = if k9 > 1 then "s" else ""
      t0 = gsInitTime gs
      t9 = gsMaxTime gs
      t = gsTime gs
      ts = show $ 0.1 * (fromIntegral $ truncate $ (t0 + t9 - t) * 10)
      b9 = gsTotBases gs
      x' = if b9 > 1 then "s" else ""
  tell [ "  Status Report"
       , "Klingon" ++ x ++ " left: " ++ (show k9)
       , "Mission must be completed in " ++ ts ++ " stardates"
       , (if b9 >= 1
            then "The Federation is maintaining " ++ (show b9) ++ " starbase" ++ x' ++ " in the galaxy"
            else "Your stupidity has left you on your own in the galaxy -- you have no starbases left!")
       ]

photonTorpedoData :: Game ()
photonTorpedoData = do
  gs <- getGS
  let q = gsEntQuad gs
      Population k3 _ _ = (gsGalaxy gs)!q
      xs = if k3 > 1 then "s" else ""
      ds = map ((dirDist s) . floatPos . kPos) $ filter ((> 0) . kShields) $ elems $ gsKlingons gs
      s = floatPos $ gsEntSect gs
  if k3 <= 0
    then tell [ "Science Officer Spock reports: "
              , "  'Sensors show no enemy ships in this quadrant'"
              ]
    else do tell [ "From enterprise to klingon battle cruiser" ++ xs ]
            showDirDist ds

starbaseNavData = do
  gs <- getGS
  let q = gsEntQuad gs
      Population _ b3 _ = (gsGalaxy gs)!q
      ds = map ((dirDist s) . floatPos . fst) $ filter ((Base==) . snd) $ assocs $ gsQuadrant gs
      s = floatPos $ gsEntSect gs
  if b3 <= 0
    then tell [ "Science Officer Spock reports: "
              , "  'Sensors show no starbases in this quadrant'"
              ]
    else do tell [ "From enterprise to starbase: " ]
            showDirDist ds

getEntCoords :: Game ((Int, Int), (Int, Int))
getEntCoords = do
  gs <- getGS
  return (gsEntQuad gs, gsEntSect gs)

dirDistCalc :: (Float, Float) -> Game ()
dirDistCalc d = do
  gs <- getGS
  let s = floatPos $ gsEntSect gs
  showDirDist [dirDist s d]

showDirDist = foldM (\_ (dir, dist) -> 
  tell [ "Direction = " ++ (show dir)
       , "Distance = " ++ (show dist)
       ]) ()

dirDist (sr, sc) (kr, kc) = 
  let dc = kc - sc
      dr = sr - kr
      s1 c1 = if abs dr <= abs dc
                then c1 + (abs dr / abs dc)
                else c1 + (((abs dr - abs dc) + abs dr) / (abs dr))
      s2 c1 = if abs dr >= abs dc
                then c1 + (abs dc / abs dr)
                else c1 + (((abs dc - abs dr) + abs dc) / (abs dc))
      dir = if dc < 0
              then if dr > 0
                     then s2 3
                   else if dc /= 0
                     then s1 5
                     else s2 7
              else if dr < 0
                     then s2 7 
                   else if dc > 0
                     then s1 1
                   else if dr == 0
                     then s1 5
                     else s1 1
  in (dir, sqrt $ dc^2 + dr^2)

showDamageType WarpEngines       = "Warp Engines"
showDamageType ShortRangeSensors = "Short Range Sensors"
showDamageType LongRangeSensors  = "Long Range Sensors"
showDamageType PhaserControl     = "Phaser Control"
showDamageType PhotonTubes       = "Photon Tubes"
showDamageType DamageControl     = "Damage Control"
showDamageType ShieldControl     = "Shield Control"
showDamageType Computer          = "Library-Computer"

randDamageType r = 
  let dts = enumFrom WarpEngines
      in head $ drop (truncate $ r * fromIntegral ((length dts) - 1)) dts

quadrantName (z4, z5) =
  if z5 <= 4
    then case z4 of
      1 -> "Antares"
      2 -> "Rigel"
      3 -> "Procyon"
      4 -> "Vega"
      5 -> "Canopus"
      6 -> "Altair"
      7 -> "Sagittarius"
      8 -> "Pollux"
    else case z4 of
      1 -> "Sirius"
      2 -> "Deneb"
      3 -> "Capella"
      4 -> "Betelguese"
      5 -> "Aldebaran"
      6 -> "Regulus"
      7 -> "Arcturus"
      8 -> "Spica"

sectorName (z4, z5) =
  case ((z5 - 1) `mod` 4) of
    0 -> "I"
    1 -> "II"
    2 -> "III"
    3 -> "IV"
