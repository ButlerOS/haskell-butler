module Butler.Core.HistoryTest where

import Test.Tasty
import Test.Tasty.HUnit

import Butler.Core.History
import Butler.Prelude

test_history :: TestTree
test_history =
    testGroup
        "History"
        [ testCase "Read oldest and recent" do
            h <- atomically do
                h <- newHistory 42
                addHistory h (1 :: Int)
                addHistory h 2
                addHistory h 3
                pure h
            oldest <- atomically $ oldestHistory h
            oldest @?= [1, 2, 3]
            recent <- atomically $ recentHistory h
            recent @?= [3, 2, 1]
        ]
