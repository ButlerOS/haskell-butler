module MemoryTest where

import Test.Tasty
import Test.Tasty.HUnit

import Butler.Memory
import Butler.Prelude
import Butler.Storage

test_memory :: TestTree
test_memory = testCase "Memory" do
    storage <- newStorage ".butler-tmp"
    (v, mv) <- newMemoryVar storage "test" (pure True)
    v @?= True

    -- read memory
    rm <- atomically $ readMemoryVar mv
    rm @?= True

-- todo: check storage
