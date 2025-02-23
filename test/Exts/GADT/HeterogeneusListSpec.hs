module Exts.GADT.HeterogeneusListSpec where

import Test.Hspec
import Exts.GADT.HeterogeneusList

spec :: Spec
spec = do
  let user1 = UserByName "Marisol" "Tagarro"
      user2 = UserByName "Javier" "Saenz"
      user3 = UserByName "Mario" "Saenz"
      user4 = UserByID 1

  describe "usersWithFirstName" $ do
    it "returns only users by name" $ do
      [user2] `shouldBe` usersWithFirstName "Javier" [user1, user2, user3]

  describe "nameUsers" $ do
    it "returns only users by name" $ do
      let heterogeneusList = user1 :++: user4 :++: user2 :++: user3 :++: EmptyUsers
      [user1, user2, user3] `shouldBe` nameUsers heterogeneusList
