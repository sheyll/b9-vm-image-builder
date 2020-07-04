module B9.RepositorySpec
  ( spec,
    filterRepoImagesMapReturnsAllAndOnlyImagesSatisfieingTheFilterPredicate,
    filterRepoImagesMapReturnsAllAndOnlyReposSatisfieingTheFilterPredicate,
    allSharedImagesWithRepoReturnsAllNonEmptyRepos,
    allSharedImagesWithRepoReturnsOnlyReposContainedInTheParameter,
    allSharedImagesWithRepoReturnsAllImages,
    allSharedImagesWithRepoReturnsOnlyPairsSuchThatTheImageIsContainedInTheRepository,
    maxSharedImageOfAllReposReturnsNonNothingIfInputHasImages,
    maxSharedImageOfAllReposReturnsTheMaximumImage,
    maxSharedImageOfAllReposReturnsAValidRepoImagePair,
  )
where

import B9.DiskImages
import B9.Repository
import Data.Foldable (any)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

spec :: HasCallStack => Spec
spec =
  describe
    "Repository"
    ( do
        describe
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
        describe
          "lookupCachedImages"
          ( it
              "returns only shared images that are cached"
              (property lookupCachedImagesReturnsOnlyImagesFromCache)
          )
        describe
          "allSharedImagesWithRepo"
          ( do
              it
                "returns all repositories that are not empty"
                (property allSharedImagesWithRepoReturnsAllNonEmptyRepos)
              it
                "returns only repositories that are in the input"
                (property allSharedImagesWithRepoReturnsOnlyReposContainedInTheParameter)
              it
                "returns all images"
                (property allSharedImagesWithRepoReturnsAllImages)
              it
                "returns only pairs where the image is in the repository"
                (property allSharedImagesWithRepoReturnsOnlyPairsSuchThatTheImageIsContainedInTheRepository)
          )
        describe
          "maxSharedImageOfAllRepos"
          ( do
              it
                "returns a non-Nothing value of the input has any images and Nothing otherwise"
                (property maxSharedImageOfAllReposReturnsNonNothingIfInputHasImages)
              it
                "returns the maximum of all images"
                (property maxSharedImageOfAllReposReturnsTheMaximumImage)
              it
                "returns a pair where the image is in the repository"
                (property maxSharedImageOfAllReposReturnsAValidRepoImagePair)
          )
    )

matchesSomeButNotAll :: Foldable t => (a -> Bool) -> t a -> Bool
matchesSomeButNotAll p xs = any p xs && any (not . p) xs

filterRepoImagesMapReturnsAllAndOnlyImagesSatisfieingTheFilterPredicate ::
  Fun SharedImage Bool -> RepoImagesMap -> Property
filterRepoImagesMapReturnsAllAndOnlyImagesSatisfieingTheFilterPredicate (Fun _ p) t =
  let i' = runCodeUnderTest p
      i'Complement = runCodeUnderTest (not . p)
      i = allSharedImages t
      runCodeUnderTest = allSharedImages . flip (filterRepoImagesMap (const True)) t
   in classify
        (not (p `any` i))
        "predicate matches no image"
        (null i' && i'Complement == i)
        .||. classify
          (p `matchesSomeButNotAll` i)
          "predicate matches some images"
          ( p `all` i'
              && not (p `any` i'Complement)
              && Set.union i' i'Complement == i
              && null (Set.intersection i' i'Complement)
          )
        .||. classify
          (p `all` i)
          "predicate matches all images"
          (null i'Complement && i' == i)

filterRepoImagesMapReturnsAllAndOnlyReposSatisfieingTheFilterPredicate ::
  Fun Repository Bool -> RepoImagesMap -> Property
filterRepoImagesMapReturnsAllAndOnlyReposSatisfieingTheFilterPredicate (Fun _ p) t =
  let i' = runCodeUnderTest p
      i'Complement = runCodeUnderTest (not . p)
      i = allRepositories t
      runCodeUnderTest = allRepositories . flip (flip filterRepoImagesMap (const True)) t
   in classify
        (not (p `any` i))
        "predicate matches no repo"
        (null i' && i'Complement == i)
        .||. classify
          (p `matchesSomeButNotAll` i)
          "predicate matches some repos"
          ( p `all` i'
              && not (p `any` i'Complement)
              && Set.union i' i'Complement == i
              && null (Set.intersection i' i'Complement)
          )
        .||. classify
          (p `all` i)
          "predicate matches all repos"
          (null i'Complement && i' == i)

lookupCachedImagesReturnsOnlyImagesFromCache :: SharedImageName -> RepoImagesMap -> Property
lookupCachedImagesReturnsOnlyImagesFromCache sn table =
  let result = lookupCachedImages sn table
   in Set.intersection
        result
        (fromMaybe (Set.empty) (Map.lookup Cache table))
        === result

allSharedImagesWithRepoReturnsAllNonEmptyRepos :: RepoImagesMap -> Property
allSharedImagesWithRepoReturnsAllNonEmptyRepos t =
  let nonEmptyRepos =
        Map.foldrWithKey
          (\repo imgs acc -> if null imgs then acc else Set.insert repo acc)
          Set.empty
          t
      reposReturned =
        Set.map snd (allSharedImagesWithRepo t)
   in reposReturned === nonEmptyRepos

allSharedImagesWithRepoReturnsOnlyReposContainedInTheParameter ::
  RepoImagesMap -> Bool
allSharedImagesWithRepoReturnsOnlyReposContainedInTheParameter t =
  let allReposReturned = Set.map snd (allSharedImagesWithRepo t)
   in allReposReturned `Set.isSubsetOf` allRepositories t

allSharedImagesWithRepoReturnsAllImages :: RepoImagesMap -> Property
allSharedImagesWithRepoReturnsAllImages t =
  let allImagesReturned = Set.map fst (allSharedImagesWithRepo t)
   in allImagesReturned === allSharedImages t

allSharedImagesWithRepoReturnsOnlyPairsSuchThatTheImageIsContainedInTheRepository ::
  RepoImagesMap -> Bool
allSharedImagesWithRepoReturnsOnlyPairsSuchThatTheImageIsContainedInTheRepository t =
  let validPair (i, r) =
        maybe False (Set.member i) (Map.lookup r t)
   in all validPair (allSharedImagesWithRepo t)

maxSharedImageOfAllReposReturnsNonNothingIfInputHasImages :: RepoImagesMap -> Property
maxSharedImageOfAllReposReturnsNonNothingIfInputHasImages t =
  isJust (maxSharedImageOfAllRepos t) =/= null (allSharedImages t)

maxSharedImageOfAllReposReturnsTheMaximumImage :: RepoImagesMap -> Property
maxSharedImageOfAllReposReturnsTheMaximumImage t =
  not (null (allSharedImages t))
    ==> fmap fst (maxSharedImageOfAllRepos t) === Just (maximum (allSharedImages t))

maxSharedImageOfAllReposReturnsAValidRepoImagePair :: RepoImagesMap -> Property
maxSharedImageOfAllReposReturnsAValidRepoImagePair t =
  case maxSharedImageOfAllRepos t of
    Just (i, r) ->
      label
        "got result"
        (Just i === (Map.lookup r t >>= Set.lookupMax))
    Nothing ->
      label "got no result" True
