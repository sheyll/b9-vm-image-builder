{-# LANGUAGE ScopedTypeVariables #-}

-- | Content I/O
module B9.Dsl.Content where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Dsl.Core
import B9.Dsl.File

-- | A type to indicate _content_.
data Cnt (content :: *) = Cnt

instance Typeable c => Show (Cnt c) where
  show cnt = "Cnt_" ++ show (typeRep cnt)

-- | Create a 'Cnt c'.
mkCnt :: c -> Cnt c
mkCnt _ = Cnt

-- | A Proxy to use as parameter to 'BuildStep's.
data CntProxy (c :: *) = CntProxy

-- | Create a 'CntProxy c'.
cntProxy :: c -> CntProxy (Cnt c)
cntProxy _ = CntProxy

-- | Convert a 'CntProxy (Cnt c)' back to a 'Cnt c'.
unCntProxy :: CntProxy (Cnt c) -> Cnt c
unCntProxy _ = Cnt

instance Show (Cnt c) => Show (CntProxy (Cnt c)) where
  show = show . unCntProxy

class Typeable content => IsCnt content where
  type CntRenderArgs content -- TODO remove CntRenderArgs
  renderCnt :: CntRenderArgs content -> content -> ByteString
  readCnt :: ByteString -> Either CntReadError content
  mergeCnt :: [content] -> content

data CntReadError =
  CntReadError { creMsg :: String
               , creLocation :: String
               , creExcerpt :: Maybe ((Word16, Word16), [String])}
    deriving (Read,Show,Eq)

ignoreCntReadErr :: Either CntReadError content -> content
ignoreCntReadErr (Left e) = error (show e)
ignoreCntReadErr (Right x) = x

-- | Simple 'IsCnt' instance for 'ByteString's.
instance IsCnt ByteString where
  type CntRenderArgs ByteString = ()
  renderCnt () = id
  readCnt = Right
  mergeCnt = mconcat

-- | Simple 'IsCnt' instance for 'Text's.
instance IsCnt Text where
  type CntRenderArgs Text = ()
  renderCnt () = encodeUtf8
  readCnt = Right . decodeUtf8
  mergeCnt = mconcat

-- | Simple 'IsCnt' instance for 'String's.
instance IsCnt String where
  type CntRenderArgs String = ()
  renderCnt () = packB
  readCnt = Right . unpackUtf8
  mergeCnt = mconcat

-- * Implementation
type GeneratedContentChunk c = IoProgram c

type instance IoCompilerArtifactState (Cnt c) = [IoProgram c]

instance IsCnt c => IsBuilder IoCompiler (Cnt c) where
  data InitArgs IoCompiler (Cnt c) = (c, String)
  runCreate _px (c,title) =
    do hnd <- allocHandle Proxy title
       putArtifactState hnd
                        [return c]
       return hnd

instance IsCnt c => CanExtract IoCompiler (Cnt c) FreeFile where
  type ExtractionArg IoCompiler (Cnt c) FreeFile = CntRenderArgs c
  runExtract hnd _ env =
    do let dest = handleTitle hnd
       (destH,destFile) <- createFreeFile dest
       hnd --> destH
       addAction hnd
                 (do Just contentList <- getArtifactState hnd
                     liftIoProgram $
                       do content <- renderContents contentList
                          writeContentToFile destFile content)
       return destH
    where renderContents :: [IoProgram c] -> IoProgram ByteString
          renderContents = fmap (renderCnt env . mergeCnt) . sequence

instance IsCnt c => CanExtract IoCompiler FreeFile (Cnt c) where
  runExtract srcFileH _ () =
    do hnd <- allocHandle (Proxy :: Proxy (Cnt c)) (handleTitle srcFileH)
       srcFileH --> hnd
       srcCopy <-
         freeFileTempCopy srcFileH
                          (Just "content-reader")
       putArtifactState hnd
                        [(ignoreCntReadErr . readCnt) <$> readContentFromFile srcCopy]
       return hnd

instance IsCnt c => CanAdd IoCompiler (Cnt c) (Cnt c) where
  type AddSpec IoCompiler (Cnt c) (Cnt c) = c
  runAdd hnd _ c =
    do modificationBuilderState
         hnd
         (Just .
          maybe [return c]
                (<> [return c]))

-- | 'Add' instance for adding to content other content to which we only have a
-- 'Handle' and the implied hope, that some day, there actually will be data for
-- it of type 'Cnt o'. We will be waiting for that data to be created, and once
-- the data comes, e.g. is read from a file, we will not hesitate to convert it
-- using the given function (see 'AddSpec' type instance for this instance), and
-- 'mappend' it to the existing content.
instance (Monoid c, IsCnt c, IsCnt o) => CanAdd IoCompiler (Cnt c) (Handle (Cnt o)) where
  type AddSpec IoCompiler (Cnt c) (Handle (Cnt o)) = (Handle (Cnt o), o -> c)
  runAdd cH _ (oH, oToC) =
    mergeArtifactOutputInto oH cH (\ oCnt cCnt -> cCnt <> oToC oCnt )
