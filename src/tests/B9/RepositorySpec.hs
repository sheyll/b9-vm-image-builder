module B9.RepositorySpec
  ( spec,
    filterRepoImagesMapReturnsAllAndOnlyImagesSatisfieingTheFilterPredicate,
    filterRepoImagesMapReturnsAllAndOnlyReposSatisfieingTheFilterPredicate,
  )
where

import B9.DiskImages
import B9.Repository
import Data.Foldable (any)
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

spec :: HasCallStack => Spec
spec =
  describe
    "Repository"
    ( describe
        "filterRepoImagesMap"
        ( do
            it
              "returns all- and only repos matching the repo predicate"
              (property filterRepoImagesMapReturnsAllAndOnlyReposSatisfieingTheFilterPredicate)
            it
              "returns all- and only images matching the image predicate"
              (property filterRepoImagesMapReturnsAllAndOnlyImagesSatisfieingTheFilterPredicate)
            it "is idempotent" $
              property
                ( \(Fun _ repoPred) (Fun _ p) repoImgMap ->
                    let expected = filterRepoImagesMap repoPred p repoImgMap
                        actual = filterRepoImagesMap repoPred p expected
                     in expected === actual
                )
        )
    )

matchesSomeButNotAll :: Foldable t => (a -> Bool) -> t a -> Bool
matchesSomeButNotAll p xs = any p xs && any (not . p) xs

filterRepoImagesMapReturnsAllAndOnlyImagesSatisfieingTheFilterPredicate :: Fun SharedImage Bool -> RepoImagesMap -> Bool
filterRepoImagesMapReturnsAllAndOnlyImagesSatisfieingTheFilterPredicate (Fun _ p) t =
  let i' = runCodeUnderTest p
      i'Complement = runCodeUnderTest (not . p)
      runCodeUnderTest = allSharedImages . flip (filterRepoImagesMap (const True)) t
   in p `all` i'
        && not (p `any` i'Complement)
        && Set.union i' i'Complement == allSharedImages t
        && null (Set.intersection i' i'Complement)

filterRepoImagesMapReturnsAllAndOnlyReposSatisfieingTheFilterPredicate :: Fun Repository Bool -> RepoImagesMap -> Bool
filterRepoImagesMapReturnsAllAndOnlyReposSatisfieingTheFilterPredicate (Fun _ p) t =
  let i' = runCodeUnderTest p
      i'Complement = runCodeUnderTest (not . p)
      runCodeUnderTest = allRepositories . flip (flip filterRepoImagesMap (const True)) t
   in p `all` i'
        && not (p `any` i'Complement)
        && Set.union i' i'Complement == allRepositories t
        && null (Set.intersection i' i'Complement)
