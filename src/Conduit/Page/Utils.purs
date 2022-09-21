module Conduit.Page.Utils where

import Prelude
import Conduit.Data.Article (Article)
import Conduit.Data.Profile (Profile)
import Data.Lens (Traversal')
import Data.Lens.Index as LI
import Data.Lens.Record as LR
import Network.RemoteData as RemoteData
import Type.Proxy (Proxy(..))

_articles :: forall err r s. Int -> Traversal' { articles :: RemoteData.RemoteData err { articles :: Array Article | s } | r } Article
_articles i = LR.prop (Proxy :: _ "articles") <<< RemoteData._Success <<< LR.prop (Proxy :: _ "articles") <<< LI.ix i

_article :: forall err r. Traversal' { article :: RemoteData.RemoteData err Article | r } Article
_article = LR.prop (Proxy :: _ "article") <<< RemoteData._Success

_author :: forall err r. Traversal' { article :: RemoteData.RemoteData err Article | r } Profile
_author = LR.prop (Proxy :: _ "article") <<< RemoteData._Success <<< LR.prop (Proxy :: _ "author")

_profile :: forall err r. Traversal' { profile :: RemoteData.RemoteData err Profile | r } Profile
_profile = LR.prop (Proxy :: _ "profile") <<< RemoteData._Success
