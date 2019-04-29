module Basics where

-- Цель первой части домашнего задания -- познакомить вас с основами синтаксиса Хаскеля
-- В этом задании запрещается использовать какие-либо функции из стандартной библиотеки.
-- Однако разрешается использовать реализованные самостоятельно

-- 1. head' возвращает первый элемент непустого списка
head' :: [a] -> a
head' (x:_) = x
head' _ = undefined

-- 2. tail' возвращает список без первого элемента, для пустого - пустой
tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' [] = []

-- 3. take' возвращает первые n >= 0 элементов исходного списка
take' :: Int -> [a] -> [a]
--take' = undefined
take'  n (x:xs) | n > 0 = x : take' (n - 1) xs
                | otherwise = []
take' _ [] = []

-- 4. drop' возвращает список без первых n >= 0 элементов; если n больше длины
-- списка, то пустой список.
drop' :: Int -> [a] -> [a]
--drop' = undefined
drop' n (x:xs) | n > 0 = drop' (n - 1) xs
               | otherwise = (x:xs)
drop' _ [] = []

-- 5. filter' возвращает список из элементов, для которых f возвращает True
filter' :: (a -> Bool) -> [a] -> [a]
--filter' f xs = undefined
filter' f (x:xs) | f x = x : filter' f xs
                 | otherwise = filter' f xs
filter' _ [] = []

-- 6. foldl'' последовательно применяет функцию f к элементу списка l и значению,
-- полученному на предыдущем шаге, начальное значение
-- foldl'' (+) 0 [1, 2, 3] == (((0 + 1) + 2) + 3)
-- foldl'' (*) 4 [] == 4

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' f z (x:xs) = foldl'' f (f z x) xs
foldl'' _ z _ = z  


-- 7. concat' принимает на вход два списка и возвращает их конкатенацию
-- concat' [1,2] [3] == [1,2,3]
concat' :: [a] -> [a] -> [a]
--concat' = undefined
concat' _ _ = undefined

-- 8. quickSort' возвращает его отсортированный список
-- quickSort' должен быть реализован через алгоритм QuickSort
-- (выбор pivot может быть любым)
quickSort' :: Ord a => [a] -> [a]
--quickSort' = undefined
quickSort' _ = undefined
