module B9.RepositorySpec (spec) where

import Test.Hspec
import Test.QuickCheck 

import B9.Repository 

spec :: HasCallStack => Spec
spec =
  describe "Repository" $ 
    describe "filterSharedImages" $       
      it "is idempotent" $ property $
        \repoPred sharedImgPred repoImgMap ->
          let expected = filterSharedImages (applyFun repoPred) (applyFun sharedImgPred) repoImgMap
              actual = filterSharedImages (applyFun repoPred) (applyFun sharedImgPred) expected
          in expected === actual
