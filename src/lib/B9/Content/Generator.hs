module B9.Content.Generator where

import B9.Content.AST

data Content = RenderErlang (AST ErlangPropList)
             | RenderYaml (AST ErlangPropList)
             | FromTextFile SourceFile
  deriving (Read, Show, Typeable, Data, Eq)

printContent :: (Functor m, Applicative m, MonadIO m, MonadReader Environment m) => Content -> m B.ByteString
printContent (RenderErlang ast) = encodeSyntax <$> fromAST ast
printContent (RenderYaml ast) = encodeSyntax <$> fromAST ast
printContent (FromTextFile s) = readTemplateFile s

instance ASTish ErlangPropList where
  fromAST ASTNoOp = pure $ ErlangPropList $ ErlList []

  fromAST (AST a) = pure a

  fromAST (ASTObj pairs) = ErlangPropList . ErlList <$> mapM makePair pairs
    where
      makePair (k, ast) = do
        (ErlangPropList second) <- fromAST ast
        return $ ErlTuple [ErlAtom k, second]

  fromAST (ASTArr xs) =
        ErlangPropList . ErlList
    <$> mapM (\x -> do (ErlangPropList x') <- fromAST x
                       return x')
             xs

  fromAST (ASTString s) = pure $ ErlangPropList $ ErlString s
  fromAST (ASTEmbed c) =
    ErlangPropList . ErlString . T.unpack . E.decodeUtf8 <$> printContent c
  fromAST (ASTMerge asts) = mconcat <$> mapM fromAST asts
  fromAST (ASTParse src@(Source _ srcPath)) = do
    c <- readTemplateFile src
    case decodeSyntax srcPath c of
      Right s -> return s
      Left e -> error (printf "could not parse erlang \
                              \source file: '%s'\n%s\n"
                              srcPath
                              e)

xxx = RenderYaml
        (ASTMerge
           [ASTParse
              (Source NoConversion "common/user-data")
           ,ASTObj
               [("write_files"
                ,ASTArr
                   [ASTObj
                      [("content"
                       ,ASTEmbed
                          (RenderErlang
                             (ASTMerge
                                [ASTParse (Source ExpandVariables "COMMON/xyz1")
                                ,ASTParse (Source ExpandVariables "COMMON/xyz2")
                                ,ASTParse (Source ExpandVariables "COMMON/xyz3")
                                ,ASTParse (Source ExpandVariables "COMMON/xyz4")])))
                      ,("path", ASTString "/usr/lib64/mrfp/runtime.config")
                      ,("owner", ASTString "voice01:voice01")]])]])
