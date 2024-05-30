import FunctorsMonads
import Streams hiding (main)
import Test.Hspec
-- Раскомментируйте QuickCheck или Hegdehog, в зависимости от того, что будете использовать
-- Документация https://hspec.github.io/quickcheck.html
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Classes
import Data.Proxy
-- Документация в https://github.com/parsonsmatt/hspec-hedgehog#readme
-- import Test.Hspec.Hedgehog

-- Добавьте минимум 5 тестов свойств для функций из первых 2 лабораторных (скопируйте определения тестируемых функций сюда).
xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

xor_test :: Bool -> Bool -> Bool
xor_test x y = (xor x y) == (x && not y) || (y && not x)

max3, min3, median3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = max (max x y) z
min3 x y z = min (min x y) z
median3 x y z = (x + y + z) - (max3 x y z) - (min3 x y z)

max3_test :: Integer -> Integer -> Integer -> Bool
max3_test x y z = (maxim >= x) && (maxim >= y) && (maxim >= z)
                    where maxim = max3 x y z

min3_test :: Integer -> Integer -> Integer -> Bool
min3_test x y z = (minim <= x) && (minim <= y) && (minim <= z)
                    where minim = min3 x y z

median3_test :: Integer -> Integer -> Integer -> Bool
median3_test x y z = (min3 x y z <= media) && (max3 x y z >= media)
                    where media = median3 x y z

find :: (a -> Bool) -> [a] -> Maybe a
find f x | (length (filter f x)) > 0 = Just $ head (filter f x)  
         | otherwise = Nothing

find_test :: (Integer -> Bool) -> [Integer] -> Bool
find_test f x = if (find f x == Nothing) then True else (fmap f (find f x) == Just True)

newtype Poly a = P [a]
x :: Num a => Poly a
x = P [0, 1]

notNullElements [] = [] -- находит те начальные элементы в массиве, которые не равны 0
notNullElements p = if (last p == 0) then notNullElements (init p) else p 

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = notNullElements a == notNullElements b

polyAdd :: Num a => [a] -> [a] -> [a]
polyAdd a b = if (length a >= length b) then zipWith (+) a (b ++ repeat 0) else polyAdd b a

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (polyAdd a b)

plus_test :: [Integer] -> [Integer] -> Bool
plus_test a b = if (length a /= length b) then True else plus (P a) (P b) == P (zipWith (+) a b)

chahge_plus_test :: [Integer] -> [Integer] -> Bool
chahge_plus_test a b = plus (P a) (P b) == plus (P b) (P a)

instance Num a => Num (Poly a) where
    negate (P p) = P (map negate p)

negate_test :: [Integer] -> Bool
negate_test a = negate (P a) == P (map (*(-1)) a)

main :: IO ()
main = hspec $ do
    describe "Tests for lab 1 and 2" $ do
        prop "xor" $
            \x y -> xor_test x y == True
        prop "max3" $
            \x y z -> max3_test x y z == True
        prop "min3" $
            \x y z -> min3_test x y z == True
        prop "median3_test" $
            \x y z -> median3_test x y z == True
        prop "find" $
            \x -> find_test (> 0) x == True
        prop "polyAdd" $
            \a b -> plus_test a b == True
        prop "chahge_polyAdd" $
            \a b -> chahge_plus_test a b == True
        prop "polyNegate" $
            \a -> negate_test a == True

    describe "streams" $ do
        prop "sTake" $
            \n stream -> sTake_test n stream == True
        prop "sRepeat" $
            \a n -> sRepeat_test a n == True
        prop "sCycle" $
            \xs n -> sCycle_test xs n == True
        prop "sIterate" $
            \x n -> sIterate_test (\x' -> x'*x'*x') x n == True
        prop "sInterleave" $
            \n stream1 stream2 -> sInterleave_test n stream1 stream2 == True
        prop "nats" $
            \n -> nats_test n == True
        prop "ruler" $
            \n -> ruler_test n == True
        prop "minMax" $
            \xs -> minMax_test xs == True
        prop "minMaxBang" $
            \xs -> minMaxBang_test xs == True

    describe "functors and monads" $ do
        it "liftA2" $ do
            liftA2' (+) (Just 13) (Just 36) `shouldBe` Just 49
            liftA2' (*) (Just 123) Nothing `shouldBe` Nothing
            liftA2' (-) (Left 1) (Right 2) `shouldBe` (Left 1)
            liftA2' (-) (Right 3) (Left 2) `shouldBe` (Left 2)
            -- liftA2' (+) (Right 1) (Right 2) `shouldBe` (Right 3)
            liftA2' (-) [9, 9, 9] [3, 4] `shouldBe` [6, 5, 6, 5, 6, 5]
            (liftA2' (/) (\x -> 100*x) (\x -> 25*x)) 3 `shouldBe` 4
        it "seqA" $ do 
            seqA [Just 3, Just 5] `shouldBe` Just [3, 5]
            seqA [Just 5, Just 2, Nothing] `shouldBe` Nothing
            seqA [Left 2, Right 4] `shouldBe` (Left 2)
            seqA [[2, 4], [3, 5]] `shouldBe` [[2, 3], [2, 5], [4, 3], [4, 5]]
            (seqA [(\x -> 100*x), (\x -> x/2)]) 10 `shouldBe` [1000, 5]
        it "traverseA" $ do 
            traverseA Just [1, 2] `shouldBe` Just [1, 2]
            traverseA (\a -> if a > 2 then Just a else Nothing) [1, 3] `shouldBe` Nothing
            traverseA (\x -> if (mod x 2 == 0) then Just x else Nothing) [2, 4, 7] `shouldBe` Nothing
            traverseA (\x -> if x > 2 then Right x else Left 6) [2, 4, 7] `shouldBe` Left 6
            traverseA (\x -> if x > 0 then Right x else Left 0) [2, 4, 7] `shouldBe` Right [2, 4, 7]
        it "filterA" $ do
            filterA (\a -> if a > 10 then Nothing else Just (a > 0)) [-1, -2, 1, 2] `shouldBe` Just [1, 2]
            filterA (\a -> if a < 0 then Nothing else Just (a > 1)) [-1, -2, 1, 2] `shouldBe` Nothing
            filterA (\x -> if mod x 2 == 0 then Just True else Just False) [1, 2, 3, 4] `shouldBe` Just [2, 4]
            filterA (\x -> if x > 2 then Right (x > 0) else Left False) [2, 4, 6, 8] `shouldBe` Left False
            filterA (\x -> if x > 0 then Right (x > 0) else Left False) [3, 4, 7] `shouldBe` Right [3, 4, 7]
        it "composeM" $ do
            composeM Just Just 5 `shouldBe` Just 5 
            composeM (\x -> if x > 0 then Just (x * 2) else Nothing) (\x -> if x < 10 then Just (x + 1) else Nothing) 5 `shouldBe` Just 12
