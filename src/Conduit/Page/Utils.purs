module Conduit.Page.Utils where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Endpoints (FavoriteArticle, UnfavoriteArticle, UnfollowProfile, FollowProfile)
import Conduit.Api.Utils as Utils
import Conduit.AppM (AppM)
import Conduit.Data.Article (Article)
import Conduit.Data.Profile (Profile)
import Data.Foldable (for_)
import Data.Lens (Traversal')
import Data.Lens.Index as LI
import Data.Lens.Record as LR
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant as Variant
import Effect.Aff (Aff)
import Network.RemoteData as RemoteData

_articles :: forall err r s. Int -> Traversal' { articles :: RemoteData.RemoteData err { articles :: Array Article | s } | r } Article
_articles i = LR.prop (SProxy :: _ "articles") <<< RemoteData._Success <<< LR.prop (SProxy :: _ "articles") <<< LI.ix i

_article :: forall err r. Traversal' { article :: RemoteData.RemoteData err Article | r } Article
_article = LR.prop (SProxy :: _ "article") <<< RemoteData._Success

_author :: forall err r. Traversal' { article :: RemoteData.RemoteData err Article | r } Profile
_author = LR.prop (SProxy :: _ "article") <<< RemoteData._Success <<< LR.prop (SProxy :: _ "author")

_profile :: forall err r. Traversal' { profile :: RemoteData.RemoteData err Profile | r } Profile
_profile = LR.prop (SProxy :: _ "profile") <<< RemoteData._Success

toggleFavorite :: Maybe Article -> (Article -> AppM Aff Unit) -> AppM Aff Unit
toggleFavorite article setArticle =
  for_ article \{ slug, favorited } -> do
    res <-
      if favorited then
        Utils.makeSecureRequest (Apiary.Route :: UnfavoriteArticle) { slug } Apiary.none Apiary.none
      else
        Utils.makeSecureRequest (Apiary.Route :: FavoriteArticle) { slug } Apiary.none Apiary.none
    for_ res $ Variant.match { ok: setArticle <<< _.article }

toggleFollow :: Maybe Profile -> (Profile -> AppM Aff Unit) -> AppM Aff Unit
toggleFollow profile setProfile =
  for_ profile \{ username, following } -> do
    res <-
      if following then
        Utils.makeSecureRequest (Apiary.Route :: UnfollowProfile) { username } Apiary.none Apiary.none
      else
        Utils.makeSecureRequest (Apiary.Route :: FollowProfile) { username } Apiary.none Apiary.none
    for_ res $ Variant.match { ok: setProfile <<< _.profile }
