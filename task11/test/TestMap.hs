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

        testGroup "lookup" [
            testCase "in case member" $
                let val = Map.lookup 5 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                val @?= Just "a",

            testCase "in case notmember" $
                let val = Map.lookup 7 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                val @?= Nothing,

            testCase "in empty map" $
                let val = Map.lookup 5 (empty :: m Int String) in
                val @?= Nothing
        ],

        testGroup "alter" [
            testCase "f returns nothing with notmember" $
                let f _ = Nothing in
                let map = alter f 7 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 7 map @?= Nothing,

            testCase "f returns nothing with member" $
                let f _ = Nothing in
                let map = alter f 5 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Nothing,

            testCase "f returns Just with notmember" $
                let f _ = Just "c" in
                let map = alter f 7 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 7 map @?= Just "c",

            testCase "f returns Just with notmember" $
                let f _ = Just "c" in
                let map = alter f 5 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "c"
        ],

        testGroup "insert" [
            testCase "in case value replaced" $
                let map = insert 5 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "xxx",

            testCase "in case new value" $
                let map = insert 7 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 7 map @?= Just "xxx",

            testCase "in empty map" $
                let map = insert 5 "xxx" (empty :: m Int String) in
                Map.lookup 5 map @?= Just "xxx"
        ],



        testGroup "insertWith" [
            testCase "in case value updating" $
                let map = insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "xxxa",

            testCase "in case value inserting" $
                let map = insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 7 map @?= Just "xxx",

            testCase "in empty map" $
                let map = insertWith (++) 5 "xxx" (empty :: m Int String) in
                Map.lookup 5 map @?= Just "xxx"
        ],

        let f key new_value old_value = show key ++ ":" ++ new_value ++ "|" ++ old_value in
        testGroup "insertWithKey" [
            testCase "update value" $
                let map = insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "5:xxx|a",

            testCase "add value" $
                let map = insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 7 map @?= Just "xxx",

            testCase "on empty" $
                let map = insertWithKey f 5 "xxx" (empty :: m Int String) in
                Map.lookup 5 map @?= Just "xxx"
        ],

        testGroup "delete" [
            testCase "with member key" $
                let map = delete 5 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.notMember 5 map @?= True,

            testCase "notMember" $
                let map = delete 7 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.size map @?= 2,

            testCase "on empty" $
                let map = delete 5 (empty :: m Int String) in
                Map.null map @?= True
        ],

        testGroup "adjust" [
            testCase "update value" $
                let map = adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "new a",

            testCase "do nothing if key notMember" $
                let map = adjust ("new" ++ ) 7 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 7 map @?= Nothing,

            testCase "on empty" $
                let map = adjust ("new" ++) 5 (empty :: m Int String) in
                Map.null map @?= True
        ],

        let f key x = show key ++ ":new " ++ x in
        testGroup "adjustWithKey" [
            testCase "update value" $
                let map = adjustWithKey f 5 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "5:new a",

            testCase "do nothing if key notMember" $
                let map = adjustWithKey f 7 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 7 map @?= Nothing,

            testCase "on empty" $
                let map = adjustWithKey f 5 (empty :: m Int String) in
                Map.null map @?= True
        ],


        let f x = if x == "a" then Just "new a" else Nothing in
        testGroup "update" [
            testCase "change value by key, if f return Just" $
                let map = update f 5 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "new a",
 
            testCase "changes nothing, if key notMember" $
                let map = update f 7 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                size map @?= 2,
 
            testCase "remove value, if f returns Nothing" $
                let map = update f 3 (fromList [(5,"a"), (3,"b")]) :: m Int String in
                member 3 map @?= False
        ],

        let f k x = if x == "a" then Just (show k ++ ":new a") else Nothing in
        testGroup "updateWithKey" [
            testCase "update value" $
                let map = updateWithKey f 5 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                Map.lookup 5 map @?= Just "5:new a",

            testCase "do nothing, if key notMember" $
                let map = updateWithKey f 7 (fromList [(5,"a"), (3,"b")] :: m Int String) in
                size map @?= 2,

            testCase "remove value, if f returns Nothing" $
                let map = updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) :: m Int String in
                member 3 map @?= False
        ],
 
        testGroup "member" [
            testCase "returns true, if key in map" $
            let map = singleton 1 "one" :: m Int String in
            Map.member 1 map @?= True,
 
            testCase "returns false, if key not map" $
            let map = singleton 1 "one" :: m Int String in
            Map.member 2 map @?= False,
 
            testCase "returns false, on empty map" $
            let map = empty :: m Int String in
            Map.member 1 map @?= False
        ],
 
        testGroup "notMember" [
            testCase "returns false, if key in map" $
            let map = singleton 1 "one" :: m Int String in
            Map.notMember 1 map @?= False,
 
            testCase "returns true, if key not map" $
            let map = singleton 1 "one" :: m Int String in
            Map.notMember 2 map @?= True,
 
            testCase "returns true, on empty map" $
            let map = empty :: m Int String in
            Map.notMember 1 map @?= True
        ],
 
        testGroup "null" [
            testCase "returns false on nonempty map" $
            let map = singleton 1 "one" :: m Int String in
            Map.null map @?= False,
 
            testCase "returns true on empty map" $
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
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
