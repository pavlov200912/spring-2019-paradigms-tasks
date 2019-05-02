import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots =
    let
        walter = robot "Walter" 50 50
        optimus = robot "Optimus Prime" 10 10
        bee = robot "Bumblebee" 8 8
        mega = robot "Mega Tron" 15 15
        fat = robot "Fat Guy" 5 1000
        killer = robot "Strong Guy" 100 5
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"
        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"
        , testCase "Test for getHealth" $
            getHealth walter @?= 50
        , testCase "Test for getAttack" $
            getAttack walter @?= 50
        , testCase "Test for setName" $
            setName "Wolter" walter @?= robot "Wolter" 50 50
        , testCase "Test for setHealth" $
            setHealth 51 walter @?= robot "Walter" 50 51
        , testCase "Test for setAttack" $
            setAttack 49 walter @?= robot "Walter" 49 50
        ]
