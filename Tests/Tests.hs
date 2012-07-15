{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Main where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Bolder
import Data.DeriveTH
import Prelude hiding (Either(..))
import Data.Array.Unboxed
import Data.Word
import Test.QuickCheck.Checkers
import Control.Arrow
import Control.Applicative
import qualified Data.Map as M
--type TestName = String

--isTotal :: (Arbitrary a, Eq b, Show a) => TestName -> (a -> b) -> Test
isTotal testName f = testProperty testName test where
    test x = f x == f x -- just so it will evaluate
    
--isTotal2 :: (Arbitrary a, Show a, Arbitrary b, Show b, Eq c) 
--          => TestName -> (a -> b -> c) -> Test
isTotal2 testName f = testProperty testName test where
    test x y = f x y == f x y -- just so it will evaluate

isTotal3 testName f = testProperty testName test where
    test x y z = f x y z == f x y z -- just so it will evaluate
    
isLiftOpen' :: World -> Bool
isLiftOpen' x = isLiftOpen x (worldIndices x)


roundTrip :: Eq a => (a -> b) -> (b -> a) -> a -> Bool
roundTrip f g x = (g . f) x == x 

main = defaultMain [
    testGroup "update tests" [
        isTotal3 "advanceRobot is total" advanceRobot,
        isTotal2 "advanceWater is total" advanceWater,
        isTotal2 "advanceWorld' is total" advanceWorld',
        isTotal "isLiftOpen' is total" isLiftOpen',
        testProperty "encodeCell/decodeCell" $ roundTrip encodeCell decodeCell
    ]]

 

-------------------------------------------------------------------------------------
----                            BoilerPlate                                     -----
-------------------------------------------------------------------------------------
instance (Arbitrary a, Ord a, Arbitrary b) => Arbitrary (M.Map a b) where
    arbitrary = M.fromList <$> arbitrary

instance Arbitrary Word8Image where
    arbitrary = do 

        height <- choose (0, 100)
        width  <- choose (0, 100)

        let makeIndex index = (1 + index `mod` width, 1 + index `div` width)

        xs <- map (first makeIndex) . zip [0..] <$> vectorOf (height * width) 
                                                        (choose (1, 9))
        return $ array ((1, 1), (width, height)) xs
        


$(do
    let dataTypes = [''World         ,
                     ''Cell          ,
                     ''Action        ,
                     ''Direction     ,
                     ''Circumstance    ]

        customArbs = []

    decs <- derives [makeArbitrary] $ 
                filter (\x -> not $ any (x==) customArbs) dataTypes
    return $ decs)
