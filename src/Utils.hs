module Utils where

import Control.Concurrent.STM

-- | wait until the TVar changes it's value from specified outer value
waitTVarChangeFrom :: (Eq a) => a -> (TVar a) -> IO a
waitTVarChangeFrom t tv = do
  tNew <- atomically $ do
                      t' <- readTVar tv
                      if (t' /= t) then return t' else retry
  return tNew

-- | wait until the TVar changes it's value from it's current value
waitTVarChange :: (Eq a) => (TVar a) -> IO a
waitTVarChange tv = readTVarIO tv >>= flip waitTVarChangeFrom tv

-- | block for two TVars to change their value from specified tuple value
waitTVarChangePairFrom :: (Eq a,Eq b) => (a,b) -> ((TVar a),(TVar b)) -> IO (a,b)
waitTVarChangePairFrom (ta,tb) (tva,tvb) = do
  tt <- atomically $ do
                      ta' <- readTVar tva
                      tb' <- readTVar tvb
                      let tt' = (ta',tb')
                      if tt' /= (ta,tb) then return tt' else retry
  return tt

-- | block for two TVars to change their value from their current values
waitTVarChangePair :: (Eq a,Eq b) => ((TVar a),(TVar b)) -> IO (a,b)
waitTVarChangePair tt@(tva,tvb) = do
  ta <- readTVarIO tva
  tb <- readTVarIO tvb
  waitTVarChangePairFrom (ta,tb) tt



dontReenter :: TVar Bool -> IO a -> IO ()
dontReenter var act = do
  a' <- atomically $ do
          v <- readTVar var
          if v 
           then return (return ()) 
           else writeTVar var True >> return (act >> atomically (writeTVar var False))
  a'

callDepthCount :: TVar Int -> (Int -> IO a) -> IO ()
callDepthCount var act = do
  v <- atomically $ do
         v' <- readTVar var
         writeTVar var (v'+1)
         return v'
  act v
  atomically $ writeTVar var v
