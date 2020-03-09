{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

import Lib
import Data.Char (isPunctuation)
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Maybe(fromJust)
import GHC.Generics
import Generic.Random
import Test.HUnit
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Control.Monad.Mock
import Control.Monad.Mock.TH
import Data.Function ((&))
import MockSample
import Test.SmallCheck.Series
import Test.SmallCheck
import Control.Monad.Identity
import Test.Hspec.SmallCheck

operators :: [Bop]
operators = [Plus, Minus, Times, Div, Gt, Ge, Lt, Le, Eql, And, Or]

------------------------------------------------quick check testing-----------------------------------------------------------
data Expression = Expression Value Value Bop deriving (Show)

instance Arbitrary Value where
  arbitrary = oneof
    [ I <$> arbitrary
    , B <$> arbitrary
    ]

instance Arbitrary Bop where
  arbitrary =  oneof $ fmap pure operators

instance Arbitrary Expression where
  arbitrary = oneof [genIntegerExp, genBoolExp]

genNotZeroInt :: Gen Int
genNotZeroInt = abs `fmap` (arbitrary :: Gen Int) `suchThat` (/= 0)

--Integer generators
genIntegerExp :: Gen Expression -- сделать проверку деления на ноль
genIntegerExp = do
      i1 <- arbitrary :: Gen Int
      i2 <- genNotZeroInt
      op <- elements [Plus, Minus, Times, Div, Gt, Ge, Lt, Le, Eql]
      return (Expression (I i1) (I i2) op)

--Bool generators
genBoolExp :: Gen Expression
genBoolExp = do
      b1 <- arbitrary :: Gen Bool
      b2 <- arbitrary :: Gen Bool
      op <- elements [And, Or]
      return (Expression (B b1) (B b2) op)

--property for quick check
calcProperty :: Expression -> Bool
calcProperty (Expression v1 v2 op) = calcTest v1 v2 op == calc v1 v2 op

calcTest :: Value -> Value -> Bop -> Value
calcTest (I i1) (I i2) o
  | o == Plus = I (i1+i2)
  | o == Minus = I (i1 - i2)
  | o == Times = I (i1*i2)
  | o == Div = I (i1 `div` i2)
  | o == Gt = B (i1>i2)
  | o == Ge = B (i1>=i2)
  | o == Lt = B (i1<i2)
  | o == Le = B (i1<=i2)
  | o == Eql = B (i1 == i2)
  | otherwise = error "Such operation are not implemented for integer"
calcTest (B b1) (B b2) o
  | o == And = B (b1 && b2)
  | o == Or = B (b1 || b2)
  | otherwise = error "Such operation are not implemented for boolean"
calcTest _ _ _ = error "Not correct expression"

-------------------------------------------------------------------------------------------------

------------------------SmallCheck---------------------------------------------------------------

ops 1 = take 9 operators --generate only operators for integer
ops _ = take 2 $ drop 9 operators

vals 1 = map (\x -> (I x)) [1..100]
vals _ = map (\x -> (B x)) [True, False]

instance Monad m => Serial m Bop where 
  series = Test.SmallCheck.Series.generate ops

instance Monad m => Serial m Value where 
  series = Test.SmallCheck.Series.generate vals
  
-------------------------------------------------------------------------------------------------

--hunit tests  
test1 :: Test
test1 = TestCase (assertEqual "for getValue (evStmt [..] (If ..)) x" (I 7)
     (getValue (evStmt [("y",(I 5)), ("x",(I 6)), ("V",(B True))] (If (Var "V") (Incr "x") (Incr "y"))) "x"))


--testing parser
test2 :: Test
test2 = TestCase (assertEqual "for getValue (evStmt [..] (If ..)) x in text view" (I 7) 
                (case parseSPL myExample of
                    Left e  -> error $ show e
                    Right r -> (getValue (evProgram r) "x")))
					
				  
tests :: Test
tests = TestList [TestLabel "Testing If case" test1, TestLabel "Testing If case in text view" test2]

--using another syntax
testsWithOperators = TestList [ "test1"  ~:  "for getValue (evStmt [..] (While ..)) x" ~: (I 7) ~=? (getValue (evStmt [("y",(I 5)), ("x",(I 6)), ("v",(B True))]  
								(While (Var "v")
                                 (Block []
                                   [(Incr "x"), 
                                     Assign "v" (Const (B False))
                                   ]))) "x")]
								   
main :: IO ()
main = hspec $ do
--moq tests
    describe "getName+parseFile" $ do
      it "moqing getLine method" $ do
        values <- parseFile res
        values `shouldBe` ([("a",I 317),("b",I 17)])
--hspec test
    describe "Lib.getValue" $ do
      it "returns the second element of a list" $ do
        (getValue [("y",(I 5)), ("x",(I 6)), ("V",(B True))] "x") `shouldBe` ((I 6) :: Value)
--quick check test using property
    describe "Lib.calc" $ do
      it "quick property testing with random generated values" $ do
        Test.QuickCheck.property calcProperty 
--hunit tests
    describe "HUnit tests casted to hspec using functions" $ do
      fromHUnitTest tests --cast to hspec tests
    describe "using operators" $ do
      fromHUnitTest testsWithOperators
--hspec test
    describe "Lib.evStmt" $ do
      it "testing incrementing" $ do
        getValue (evStmt test_array (Incr "x")) "x" == calcTest (getValue test_array "x") (I 1) Plus
--small check test
    describe "Lib.calc" $ do
      it "small property testing with random generated values" $ do
        Test.Hspec.SmallCheck.property $ \v1 v2 op -> calcTest (v1 :: Value) (v2 ::Value) (op :: Bop) == calc v1 v2 op
	where 
    test_array = [("y",(I 5)), ("x",(I 6)), ("V",(B True))]
    (res,_) = runMockIO (getName) (MockState ["power.txt"])
	
-- smallCheck 2 $ \v1 v2 op -> calcTest (v1 :: Value) (v2 ::Value) (op :: Bop) == calc v1 v2 op   
		
--TODO
--smallcheck
--using do in testcase in hunit
