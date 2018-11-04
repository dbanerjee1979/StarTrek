{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameController (execGame) where

import Game
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Array
import Data.List
import System.Random
import Text.Read (readMaybe)

newtype GameIO a = GameIO {
    runGIO :: ExceptT Reason (StateT (GameState, StdGen) IO) a
  } deriving (Monad, Applicative, Functor, MonadError Reason, MonadState (GameState, StdGen), MonadIO)

runGameIO :: GameIO a -> (GameState, StdGen) -> IO (Either Reason a, (GameState, StdGen))
runGameIO k = runStateT (runExceptT $ runGIO k)

withIO :: Game a -> GameIO a
withIO k = do 
  s <- get
  let ((result, msgs), s') = runGame k s
  put s'
  liftIO $ putStrLn $ intercalate "\n" msgs
  case result of
    Left a  -> throwError a
    Right a -> return a

execGame = do
  g <- newStdGen
  (result, (gs', _)) <- runGameIO (withIO initGame >>
                                   dispatchCommand) (makeGS, g)
  case result of
    Left reason -> return $ gameOver reason gs'
    Right _     -> return (False, [])

dispatchCommand = do
  withIO checkEnergyLevels
  cmd <- liftIO $ putStr "Command: " >> getLine
  case cmd of
    "NAV" -> handleInput $ do c1      <- askCourse 
                              (w1, n) <- askWarp
                              withIO $ navigation c1 w1 n
    "SRS" -> withIO shortRangeScan
    "LRS" -> withIO longRangeScan
    "PHA" -> handleInput $ do x <- askPhaserEnergy
                              withIO $ firePhasers x
    "TOR" -> handleInput $ do c1 <- askTorpedoCourse
                              withIO $ fireTorpedo c1
    "SHE" -> handleInput $ do x <- askShields
                              withIO $ shieldControl x
    "DAM" -> handleDamageReport
    "COM" -> dispatchComputer
    "XXX" -> withIO $ throwError Resigned
    _     -> withIO commandHelp
  dispatchCommand

handleInput :: GameIO () -> GameIO ()
handleInput k = k `catchError` (\e ->
  case e of
    InputError msg -> liftIO $ putStrLn $ intercalate "\n" msg
    other          -> throwError other)

askCourse :: GameIO Float
askCourse = do
  let badCourse = throwError $ InputError ["   Lt. Sulu reports, 'Incorrect course data, sir!'"]
  s <- liftIO $ do putStr "Course (0-9): "
                   getLine
  let m = readMaybe s :: Maybe Float
  case m of
    Just c1 -> if c1 >= 1 && c1 < 9 
                 then return c1
                 else badCourse
    Nothing -> badCourse

askWarp :: GameIO (Float, Float)
askWarp = do
  x <- withIO $ getMaxWarp
  w1s <- liftIO $ do putStr $ "Warp Factor (0-" ++ (show x) ++ "): "
                     getLine
  case readMaybe w1s :: Maybe Float of
    Just w1 -> withIO $ validateWarp w1
    Nothing -> badWarp w1s

askShields :: GameIO Float
askShields = do
  e <- withIO $ getMaxShieldEnergy
  inp <-liftIO $ do putStr $ "Energy available = " ++ (show e) ++ ", number of units to shields: "
                    getLine
  case readMaybe inp :: Maybe Float of
    Just x  -> withIO $ validateShields x
    Nothing -> throwError $ InputError []

askPhaserEnergy :: GameIO Float
askPhaserEnergy = do
  e <- withIO $ getMaxPhaserEnergy
  liftIO $ putStr "Phasers locked on target; "
  let readPhaserEnergy = do          
      s <- liftIO $ do putStrLn $ "Energy available = " ++ (show e) ++ " units"  
                       putStr "Number of units to fire: "
                       getLine 
      case readMaybe s :: Maybe Float of
        Just x  -> if e - x < 0 
                     then readPhaserEnergy
                     else return x
        Nothing -> return 0
  readPhaserEnergy

askTorpedoCourse :: GameIO Float
askTorpedoCourse = do
  let badCourse = throwError $ InputError ["Ensign Chekov reports, 'Incorrect course data, sir!'"]
  withIO $ validateCanFirePhotons
  s <- liftIO $ do putStr "Photon torpedo course (1-9): " 
                   getLine               
  case readMaybe s :: Maybe Float of
    Just c1 -> if c1 >= 1 && c1 < 9 
                 then return c1
                 else badCourse
    Nothing -> badCourse

handleDamageReport :: GameIO ()
handleDamageReport = do
  withIO $ damageControlReport
  canRepair <- withIO $ starbaseCanRepairDamage
  case canRepair of
    Just d3 -> do cmd <- liftIO $ do putStr "Will you authorize the repair order (Y/N)? "
                                     getLine
                  if cmd == "Y"
                    then withIO $ starbaseRepairDamage d3
                    else return ()
    Nothing -> return ()

dispatchComputer :: GameIO ()
dispatchComputer = do
  withIO $ validateComputerAvailable
  cmd <- liftIO $ putStr "Computer active and awaiting command: " >> getLine
  case cmd of
    "0" -> withIO $ galacticRecord 
    "1" -> do withIO $ statusReport
              handleDamageReport
    "2" -> withIO $ photonTorpedoData
    "3" -> withIO $ starbaseNavData
    "4" -> handleDirDistCalc
    "5" -> withIO $ galaxyMap
    _   -> do withIO $ computerHelp
              dispatchComputer

handleDirDistCalc :: GameIO ()
handleDirDistCalc = do
  (quad, sect) <- withIO $ getEntCoords
  s <- liftIO $ do putStrLn "Direction/Distance Calculator:"
                   putStrLn $ "You are quadrant " ++ (show quad) ++ " sector " ++ (show sect)
                   putStr "Initial coordinates (x, y) "
                   getLine
  case readMaybe s :: Maybe (Float, Float) of
    Just c  -> withIO $ dirDistCalc c
    Nothing -> return ()
