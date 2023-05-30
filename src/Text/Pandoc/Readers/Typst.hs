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
import Typst ( parseTypst, evaluateTypst )
import Typst.Pandoc (contentToPandoc)
import qualified Data.Text as T
import Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Parsing (sourceName)
import Control.Monad.Except ( MonadError(throwError, catchError) )
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
      result <- catchError
        (evaluateTypst (\fp -> catchError (readFileStrict fp) (\e -> throwError (show e))) inputName parsed >>=
                  either (throwError . PandocParseError . T.pack . show) pure >>=
                  contentToPandoc (report . IgnoredElement))
        (throwError . PandocParseError . T.pack)
      case result of
        Left e -> throwError e
        Right pdoc -> pure pdoc
