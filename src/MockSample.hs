{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module MockSample where

import Control.Monad.State
		 
class Monad m => MockIO m where
  mGetLine :: m String
  
instance MockIO IO where
  mGetLine = Prelude.getLine
  
data MockState = MockState
  { outputs :: [String] -- getLine
  }
  deriving (Show, Eq)


type SimpleMockedIO = State MockState

runMockIO :: SimpleMockedIO a -> MockState -> (a, MockState)
runMockIO = runState

instance MockIO SimpleMockedIO where

  mGetLine = do
    st <- get
    case outputs st of
      [] -> error "MOCK ERROR: Empty stdin"
      (o:ox) -> do
         let newSt = st { outputs = ox }
         put newSt
         return o