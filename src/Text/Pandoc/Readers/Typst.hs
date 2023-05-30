{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Typst
   Copyright   : Copyright (C) 2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Reads and evaluates a Typst document as a Pandoc AST.
-}
module Text.Pandoc.Readers.Typst
  ( readTypst
  )
where

import Text.Pandoc.Class
import Text.Pandoc.Sources
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Typst ( parseTypst, evaluateTypst, EvaluateM(..) )
import Typst.Pandoc (contentToPandoc)
import qualified Data.Text as T
import Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Parsing (sourceName)
import Control.Monad.Except ( MonadError(throwError) )
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Logging (LogMessage(..))

-- | Read Typst from an input string and return a Pandoc document.
readTypst :: (PandocMonad m, ToSources a)
           => ReaderOptions -> a -> m Pandoc
readTypst _opts inp = do
  let sources = toSources inp
  let inputName = case sources of
        Sources ((pos, _):_) -> sourceName pos
        _ -> ""
  case parseTypst inputName (sourcesToText sources) of
    Left e -> throwError $ PandocParseError $ T.pack $ show e
    Right parsed -> do
      result <- evaluateTypst inputName parsed >>=
                  either (throwError . PandocParseError . T.pack . show) pure >>=
                  contentToPandoc
      case result of
        Left e -> throwError $ PandocParseError $ T.pack $ show e
        Right pdoc -> pure pdoc

instance PandocMonad m => EvaluateM m where
  loadFileLazyBytes = readFileLazy
  loadFileText = fmap UTF8.toText . readFileStrict
  issueWarning msg = report $ IgnoredElement {-- TODO for now --} msg
