module MemoryTest where

import Test.Tasty
import Test.Tasty.HUnit

import Butler.Core.Memory
import Butler.Core.Storage
import Butler.Prelude

test_memory :: TestTree
test_memory = testCase "Memory" do
    storage <- newStorage ".butler-tmp"
    (v, mv) <- newMemoryVar storage "test" (pure True)
    v @?= True

    -- read memory
    rm <- atomically $ readMemoryVar mv
    rm @?= True

    -- update memory
    atomically $ modifyMemoryVar mv (const False)

    -- check if creating a new memory var get last data
    (_, mv2) <- newMemoryVar storage "test" (pure True)
    rm2 <- atomically $ readMemoryVar mv2
    rm2 @?= False

-- todo: check storage
