import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "head' works on large non-empty list" $
        head' [0, 1, 2, 3, 4, 5] @?= 0

    , testCase "head' works on infinite list" $
        head' [1..] @?= 1

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "tail' works on large non-empty list too" $
        tail' [1,2,3,4,5,6] @?= [2,3,4,5,6]

    , testCase "tail' works on infinite list too" $
        head' (tail' [0..]) @?= 1

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "take' takes 2 element from 5-element list" $
        take' 2 [1,2,3,4,5] @?= [1,2]

    , testCase "take' takes 3 element from infinite list" $
        take' 3 [1..] @?= [1,2,3]

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]
   
    , testCase "drop' drops 2 element from 5-element list" $
        drop' 2 [1,2,3,4,5] @?= [3,4,5]

    , testCase "drop' drops 3 element from infinite list" $
        take' 2 (drop' 3 [1..]) @?= [4,5]

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]

    , testCase "filter' selects only (>4) from 0 to 10" $
        filter' (>4) [0..10] @?= [5..10]

    , testCase "filter' selects only (>4) from infinite list" $
            head' (filter' (>4) [0..]) @?= 5

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "foldl'' show list in correct order" $
        foldl'' (\x y-> concat ["(",x,"+",y,")"]) "0" (map show [1..4]) @?= "((((0+1)+2)+3)+4)"

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works on infinite lists as expected" $
        head' (concat' [0] [1..]) @?= 0

    , testCase "concat' works with empty lists as expected" $
        concat' [] [1,2,3,4,5,6] @?= [1..6]

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    
    , testCase "quickSort actualy sorts large list" $
        quickSort' [5,2,3,4,1,6,7,8,9,11,534,121321312231] @?= [1,2,3,4,5,6,7,8,9,11,534,121321312231]
    ]
