module Luhn where

-- Проверка корректности номера банковской карты алгоритмом Луна https://ru.wikipedia.org/wiki/Алгоритм_Луна.
-- Алгоритм:
-- 1. Все цифры, стоящие на чётных местах (считая с конца), удваиваются. Если при этом получается число, большее 9, то из него вычитается 9. Цифры, стояшие на нечётных местах, не изменяются.
-- То есть: последняя цифра не меняется; предпоследнее удваивается; 3-е с конца (предпредпоследнее) не меняется; 4-е с конца удваивается и т.д.
-- 2. Все полученные числа складываются.
-- 3. Если полученная сумма кратна 10, то исходный список корректен.

-- Не пытайтесь собрать всё в одну функцию, используйте вспомогательные.
-- Например: разбить число на цифры (возможно, сразу в обратном порядке).
-- Не забудьте добавить тесты, в том числе для вспомогательных функций!
toInt :: String -> Int
toInt x = read x :: Int

numberDivider :: Int -> Int -> [Int]
numberDivider _ 0 = []
numberDivider number counter = (number `mod` 10) : numberDivider (number `div` 10) (counter-1)

doubleNumbers :: [Int] -> [Int]
doubleNumbers (first:second:mas) = first : (2*second `mod` 9) : doubleNumbers mas
doubleNumbers [var] = var : []
doubleNumbers [] = []

readNumber :: String -> [Int]
readNumber number = numberDivider (toInt number) (length number)

answer :: [Int] -> Bool
answer mas | (sum mas) `mod` 10 == 0 = True
           | otherwise = False

isLuhnValid :: Int -> Bool
isLuhnValid x = answer (doubleNumbers (readNumber (show x)))
