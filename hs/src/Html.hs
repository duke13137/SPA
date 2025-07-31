{-# LANGUAGE OverloadedStrings #-}
module Html (hsx, module Lucid) where

import Data.Set qualified as Set
import IHP.HSX.Lucid2.QQ (customHsx)
import IHP.HSX.Parser
import Language.Haskell.TH.Quote
import Lucid

hsx :: QuasiQuoter
hsx= customHsx
    (HsxSettings
        { checkMarkup = True
        , additionalTagNames = Set.fromList ["book", "heading", "name"]
        , additionalAttributeNames = Set.fromList
            [ "hx-boost", "hx-get", "hx-post", "hx-put", "hx-patch", "hx-delete", "hx-push-url", "hx-replace-url"
            , "hx-select", "hx-select-oob" , "hx-swap", "hx-swap-oob", "hx-target", "hx-trigger", "hx-vals"
            , "hx-confirm", "hx-disable", "hx-disabled-elt", "hx-disinherit", "hx-encoding", "hx-ext"
            , "hx-headers", "hx-history", "hx-history-elt", "hx-include" , "hx-indicator", "hx-inherit"
            , "hx-params", "hx-preserve", "hx-prompt", "hx-request", "hx-sync", "hx-validate"
            , "hx-target-404", "hx-target-4xx", "hx-target-500", "hx-target-5xx"
            , "sse-connect", "sse-close", "sse-swap"
            ]
        }
    )
