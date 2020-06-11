module B9.DiskImagesSpec (spec) where

import B9.DiskImages
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "DiskImages" $ do
    describe "splitToIntermediateSharedImage" $
      do
        it "puts the original source into the intermediate target" $
          property
            ( \target name ->
                itImageSource target
                  == itImageSource
                    (fst (splitToIntermediateSharedImage target name))
            )
        it "puts the original destination into the export target" $
          property
            ( \target name ->
                itImageDestination target
                  == itImageDestination
                    (snd (splitToIntermediateSharedImage target name))
            )
        it
          "puts the intermediate shared image name into both the intermediate and the export target"
          $ property
            ( \target name ->
                let (intermediateTarget, exportTarget) =
                      splitToIntermediateSharedImage target name
                 in imageDestinationSharedImageName
                      (itImageDestination intermediateTarget)
                      == imageSourceSharedImageName (itImageSource exportTarget)
            )
    context
      "inline unit tests"
      unitTests
