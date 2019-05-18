{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree
 
main :: IO ()
main = defaultMain testMap
 
{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.
 
  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.
 
  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
 
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "Smoke tests" [
            testCase "toAscList . fromList sorts list" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]
        ],

        testGroup "insert tests" [
            testCase "insert in case value replaced" $
                let map = insert 5 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "xxx",

            testCase "insert in case new value" $
                let map = insert 7 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 7 map @?= Just "xxx",

            testCase "insert in empty map" $
                let map = insert 5 "xxx" (empty :: m Int String) in
                Map.lookup 5 map @?= Just "xxx"
        ],



        testGroup "insertWith tests" [
            testCase "insertWith in case value updating" $
                let map = insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "xxxa",

            testCase "insertWith in case value inserting" $
                let map = insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 7 map @?= Just "xxx",

            testCase "insertWith in empty map" $
                let map = insertWith (++) 5 "xxx" (empty :: m Int String) in
                Map.lookup 5 map @?= Just "xxx"
        ],

        let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value in
        testGroup "insertWithKey tests" [
            testCase "insertWithKey update value" $
                let map = insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "5:xxx|a",

            testCase "insertWithKey add value" $
                let map = insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 7 map @?= Just "xxx",

            testCase "insertWithKey of empty" $
                let map = insertWithKey f 5 "xxx" (empty :: m Int String) in
                Map.lookup 5 map @?= Just "xxx"
        ],

        let f x = if x == "a" then Just "new a" else Nothing in
        testGroup "update tests" [
            testCase "update change value by key, if f return Just" $
                let map = update f 5 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "new a",
 
            testCase "update changes nothing, if key notMember" $
                let map = update f 7 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                size map @?= 2,
 
            testCase "update remove value, if f returns Nothing" $
                let map = update f 3 (fromList [(5,"a"), (3,"b")]) :: m Int String in
                member 3 map @?= False
        ],

        let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing in
        testGroup "updateWithKey tests" [
            testCase "updateWithKey update value" $
                let map = updateWithKey f 5 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "5:new a",

            testCase "updateWithKey do nothing, if key notMember" $
                let map = updateWithKey f 7 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                size map @?= 2,

            testCase "update remove value, if f returns Nothing" $
                let map = updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) :: m Int String in
                member 3 map @?= False
        ],
 
        testGroup "member tests" [
            testCase "member returns true, if key in map" $
            let map = singleton 1 "one" :: m Int String in
            Map.member 1 map @?= True,
 
            testCase "member returns false, if key not map" $
            let map = singleton 1 "one" :: m Int String in
            Map.member 2 map @?= False,
 
            testCase "member returns false, on empty map" $
            let map = empty :: m Int String in
            Map.member 1 map @?= False
        ],
 
        testGroup "notMember tests" [
            testCase "notMember returns false, if key in map" $
            let map = singleton 1 "one" :: m Int String in
            Map.notMember 1 map @?= False,
 
            testCase "notMember returns true, if key not map" $
            let map = singleton 1 "one" :: m Int String in
            Map.notMember 2 map @?= True,
 
            testCase "notMember returns true, on empty map" $
            let map = empty :: m Int String in
            Map.notMember 1 map @?= True
        ],
 
        testGroup "null tests" [
            testCase "null returns false on nonempty map" $
            let map = singleton 1 "one" :: m Int String in
            Map.null map @?= False,
 
            testCase "null returns true on empty map" $
            let map = empty :: m Int String in
            Map.null map @?= True
        ]
    ]
 
testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]
 
testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList)
       --mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
       --testNaiveTree
    ]
