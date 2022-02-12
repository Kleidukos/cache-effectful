{-# LANGUAGE NoOverloadedStrings #-}
module Main where

import Effectful
import Prelude hiding (lookup)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Cache as C
import qualified Utils as U

import Effectful.Cache

main :: IO ()
main = defaultMain $ testGroup "effectful-cache"
  [ testCase "Insert & Lookup" $ testInsertAndLookup =<< initIntCache
  , testCase "Listing keys" $ testListKeys =<< initIntCache
  , testCase "Deleting keys" $ testDeleteKeys =<< initIntCache
  , testCase "Filter with key" $ testFilterWithKey =<< initIntCache
  ]

initStringCache :: IO (C.Cache String String)
initStringCache = C.newCache Nothing

initIntCache :: IO (C.Cache Int Int)
initIntCache = C.newCache Nothing

populateIntCache :: (Cache Int Int :> es) => Eff es ()
populateIntCache =
  mapM_ (\(k,v) -> insert @Int @Int k v) [(2,4),(3,6),(4,8),(5,10)]

---

testInsertAndLookup :: C.Cache Int Int -> Assertion
testInsertAndLookup cache = runEff $ do
  result <- runCacheIO cache insertAndLookup
  U.assertEqual "Looking up key 3 yields the value 12" (Just 12) result
    where
    insertAndLookup :: (Cache Int Int :> es) => Eff es (Maybe Int)
    insertAndLookup = do
      insert @Int @Int 3 12
      lookup @Int 3

testListKeys :: C.Cache Int Int -> Assertion
testListKeys cache = runEff $ do
  result <- runCacheIO cache listKeys
  U.assertEqual "Correct list of key in the cache" [2,3,4,5] result
    where
      listKeys :: (Cache Int Int :> es) => Eff es [Int]
      listKeys = do
        populateIntCache
        keys @Int @Int

testDeleteKeys :: C.Cache Int Int -> Assertion
testDeleteKeys cache = runEff $ do
  result <- runCacheIO cache deleteKeys
  U.assertEqual "Keys are deleted" [2,4] result
    where
      deleteKeys :: (Cache Int Int :> es) => Eff es [Int]
      deleteKeys = do
        populateIntCache
        delete @Int @Int 3
        delete @Int @Int 5
        keys @Int @Int

testFilterWithKey :: C.Cache Int Int -> Assertion
testFilterWithKey cache = runEff $ do
  result <- runCacheIO cache filterKeys
  U.assertEqual "Keys are properly filtered" [2,4,5] result
    where
      filterKeys :: (Cache Int Int :> es) => Eff es [Int]
      filterKeys = do
        populateIntCache
        filterWithKey @Int @Int (\k _ -> k /= 3)
        keys @Int @Int -- [2,4,5]
