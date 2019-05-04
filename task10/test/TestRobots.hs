import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots =
    let
        walter = robot "Walter" 50 50
        optimus = robot "Optimus Prime" 10 20
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
        , testCase "Test printRobot function" $
            printRobot optimus @?= "Optimus Prime, attack: 10, health: 20"
        , testCase "Test damage without deads" $
            damage mega 8 @?= robot "Mega Tron" 15 (15 - 8)
        , testCase "Test damage with MegaTron's death" $
            damage mega 16 @?= robot "Mega Tron" 15 (15 - 16)  -- HOORAY, Mega Tron defeated
        , testCase "Test is Alive tobot isAlive" $
            isAlive optimus @?= True
        , testCase "Test is dead robot !isAlive" $
            isAlive (robot "Dead" 10 (-2)) @?= False
        , testCase "Test fight without deaths" $
            fight bee mega @?= robot "Mega Tron" 15 (15 - 8)  -- Unfortunately, Bumblebee isn't strong enough
        , testCase "Test fight with death" $
            fight mega bee @?= robot "Bumblebee" 8 (8 - 15)  -- Oh, no, Bumblebee..
        , testCase "Test figth with deadman" $
            fight (robot "Dead" 10 (-2)) walter @?= walter
        , testCase "Test threeRoundFight attacker wins" $
            threeRoundFight optimus mega @?= robot "Optimus Prime" 10 5  -- It was a great battle.
        , testCase "Test threeRoundFight defender wins" $
            threeRoundFight bee fat @?= robot "Fat Guy" 5 (1000 - 2 * 8)
        , testCase "Test survivors is empty" $
            survivors @?= []  -- No one can escape from Barinov Vadim
        , testCase "Test fightWithNeue" $
            fightWithNeue optimus [fat, mega, walter] @?= [robot "Fat Guy" 5 (1000 - 10),
            robot "Mega Tron" 15 (15 - 10), robot "Walter" 50 (50 - 10)]
        ]
