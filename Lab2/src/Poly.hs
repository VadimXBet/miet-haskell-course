-- Не забудьте добавить тесты.

module Poly where

import Data.List (intercalate)
-- Многочлены
-- a -- тип коэффициентов, список начинается со свободного члена.
-- Бонус: при решении следующих заданий подумайте, какие стали бы проще или
-- сложнее при обратном порядке коэффициентов (и добавьте комментарий).
newtype Poly a = P [a]

-- Задание 1 -----------------------------------------

-- Определите многочлен $x$.
x :: Num a => Poly a
x = P [0, 1]

-- Задание 2 -----------------------------------------

-- Функция, считающая значение многочлена в точке
applyPoly :: Num a => Poly a -> a -> a
applyPoly (P []) _ = 0
applyPoly (P (p:ps)) x = p + x * applyPoly (P ps) x

-- Задание 3 ----------------------------------------

-- Определите равенство многочленов
-- Заметьте, что многочлены с разными списками коэффициентов
-- могут быть равны! Подумайте, почему.
notNullElements [] = [] -- находит те начальные элементы в массиве, которые не равны 0
notNullElements p = if (last p == 0) then notNullElements (init p) else p 

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = notNullElements a == notNullElements b
 
-- Задание 4 -----------------------------------------

-- Определите перевод многочлена в строку. 
-- Это должна быть стандартная математическая запись, 
-- например: show (3 * x * x + 1) == "3 * x^2 + 1").
-- (* и + для многочленов можно будет использовать после задания 6.)
showPoly [] = show 0 -- вывод list с коэффициентами
showPoly p =  let cOs = zip p [0..]
                  nonZeroCOs = filter (\(c,_) -> c /= 0) cOs
                  cShow c = if c == 1 then "" else show c ++ " *"
                  nShow n = case n of 
                              0 -> ""
                              1 -> "x" 
                              m -> "x^" ++ show m
                  cnShow c n = if c == 1 && n == 0 then show 1 
                               else intercalate " " $ filter (/="") [cShow c, nShow n]            
                  terms = map (\(c,n) -> cnShow c n) nonZeroCOs
              in intercalate " + " (reverse terms)    

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = showPoly p

-- Задание 5 -----------------------------------------

-- Определите сложение многочленов
polyAdd :: Num a => [a] -> [a] -> [a] -- сложение двух list с коэффициентами
polyAdd a b = if (length a >= length b) then zipWith (+) a (b ++ repeat 0) else polyAdd b a

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (polyAdd a b)

-- Задание 6 -----------------------------------------

-- Определите умножение многочленов
polyMultByCoef :: Num a => a -> [a] -> [a] -- умножение list на коэффициен
polyMultByCoef k p = map (k*) p

polyMultByX :: Num a => [a] -> [a] -- умножение list на x
polyMultByX p = 0:p

polyMult :: Num a => [a] -> [a] -> [a] -- умножение двух list с коэффициентами
polyMult [] p2 = []
polyMult (p:p1) p2 = polyAdd (polyMultByCoef p p2) (polyMultByX (polyMult p1 p2))

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = P (polyMult a b)

-- Задание 7 -----------------------------------------

-- Сделайте многочлены числовым типом
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P p) = P (map negate p)
    fromInteger p = P [fromIntegral p]
    -- Эти функции оставить как undefined, поскольку для 
    -- многочленов они не имеют математического смысла
    abs    = undefined
    signum = undefined

-- Задание 8 -----------------------------------------

-- Реализуйте nderiv через deriv
class Num a => Differentiable a where
    -- взятие производной
    deriv  :: a -> a
    -- взятие n-ной производной
    nderiv :: Int -> a -> a
    nderiv n p = iterate deriv p !! n

-- Задание 9 -----------------------------------------

-- Определите экземпляр класса типов
polyDiffer :: Num a => [a] -> a -> [a] -- взятие первой производной от полинома
polyDiffer [] _ = []
polyDiffer (p:ps) n = (p*n) : polyDiffer ps (n + 1)

instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P p) = P (polyDiffer (tail p) 1)
