{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}


module Ide.Plugin.ExplicitPrescedence   (
    descriptor
  ) where

import           Control.DeepSeq            (NFData)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Binary
import           Data.Functor
import qualified Data.HashMap.Strict        as Map
import           Data.Hashable
import qualified Data.Text                  as T
import           Data.Typeable
import           Development.IDE            as D
import           Development.IDE.Core.Shake (getDiagnostics,
                                             getHiddenDiagnostics)
import           Development.IDE.GHC.Compat (GhcPs, HsExpr,
                                             ParsedModule (ParsedModule, pm_parsed_source),
                                             SrcSpan)
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           Text.Regex.TDFA.Text       ()

import           Data.Maybe                 (maybeToList)
import           Debug.Trace                (traceM)
import           GHC.Hs.Expr
import           Generics.SYB
import           GhcPlugins                 hiding ((<>))
import           SrcLoc                     (GenLocated (L), containsSpan)
import           Wingman.Debug
import           Wingman.Judgements.SYB     (everythingContaining)

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentHover hover }

-- ---------------------------------------------------------------------

findInfix :: HsExpr GhcPs -> RealSrcSpan -> Maybe (HsExpr GhcPs)
findInfix oa@(OpApp _ left (L (RealSrcSpan rss) he) right) srcSpan | containsSpan rss srcSpan = Just oa
findInfix _                             _                       = Nothing

findInfixQ :: RealSrcSpan -> GenericQ [HsExpr GhcPs]
findInfixQ srcSpan = everything (<>) $ mkQ [] (maybeToList . flip findInfix srcSpan)

hover :: PluginMethodHandler IdeState TextDocumentHover
hover is pi (HoverParams (TextDocumentIdentifier uri) pos _)
  | Just fp <- uriToNormalizedFilePath $ toNormalizedUri uri = liftIO $ do
      traceM "runnin helasldfjlsa;dk"
      Just (ast, _) <- runIdeAction
        "ExplicitPrescedence - hover"
        (shakeExtras is)
        $ useWithStaleFast GetParsedModule fp
      let ops = findInfixQ (realSrcLocSpan $ positionToRealSrcLoc fp pos) $ pm_parsed_source ast
      return $ Right $ Just Hover {_contents = HoverContentsMS (List [PlainString $ T.pack $ unsafeRender ops]), _range = Nothing}
      -- return $ Right $ Just Hover {_contents = HoverContentsMS (List [PlainString $ T.pack $ printPrescedence ops]), _range = Nothing}
  | otherwise = pure $ Left ResponseError {_code = InternalError, _message = "Error", _xdata = Nothing}

-- printPrescedence :: [HsExpr GhcPs] -> String
-- printPrescedence (hsExpr) =

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do
  f <- mf
  x <- mx
  return $ f x

sequence' :: [IO a] -> IO [a]
sequence' []     = return []
sequence' (c:cs) = return (:) `ap` c `ap` sequence' cs

a' (c:cs) = (((return (:)) `ap` c) `ap` (a' cs))
a' []     = (return [])

repeat' :: a -> [a]
repeat' x = x : repeat' x

zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _      _      = []

transpose :: [[a]] -> [[a]]
transpose []       = repeat []
transpose (xs:xss) = repeat (:) `zapp` xs `zapp` transpose xss
