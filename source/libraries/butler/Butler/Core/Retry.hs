module Butler.Core.Retry where

import Butler.Core
import Butler.Prelude

import Control.Retry qualified as Retry
import Network.HTTP.Client qualified as HTTP

-- | Retry a network action
httpRetry ::
    -- | Maximum number of retry
    Int ->
    -- | The action to retry
    ProcessIO a ->
    ProcessIO a
httpRetry limit action =
    Retry.recovering
        (Retry.exponentialBackoff backoff <> Retry.limitRetries limit)
        [handler]
        (const action)
  where
    backoff = 1000000 -- 1sec
    -- Log network error
    handler (Retry.RetryStatus num _ _) = Handler $ \case
        HTTP.HttpExceptionRequest req ctx -> do
            let url = decodeUtf8 (HTTP.host req) <> ":" <> showT (HTTP.port req) <> decodeUtf8 (HTTP.path req)
                arg = decodeUtf8 $ HTTP.queryString req
                loc = if num == 0 then url <> arg else url
            logError "NetworkFailure" ["count" .= num, "loc" .= loc, "failed" .= showT ctx]
            pure True
        HTTP.InvalidUrlException _ _ -> pure False
