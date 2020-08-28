module Conduit.Api.Request where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Endpoints as Endpoints
import Conduit.Api.Utils as Utils
import Conduit.AppM (AppM)
import Conduit.Data.Article (Article, ArticlesQuery, ArticleRep)
import Conduit.Data.Comment (CommentId, Comment)
import Conduit.Data.Error (Error(..))
import Conduit.Data.Profile (Profile, User, ProfileRep)
import Conduit.Data.Slug (Slug)
import Conduit.Data.Username (Username)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Variant (expand, match)
import Effect.Aff (Aff)

-- | User
loginUser :: { email :: String, password :: String } -> (Either Error User -> AppM Aff Unit) -> AppM Aff Unit
loginUser user onResponse = do
  res <- Utils.makeRequest (Apiary.Route :: Endpoints.LoginUser) Apiary.none Apiary.none { user }
  onResponse $ res >>= (\success -> success # match { ok: Right <<< _.user, unprocessableEntity: Left <<< UnprocessableEntity <<< _.errors })

registerUser :: { username :: Username, email :: String, password :: String } -> (Either Error User -> AppM Aff Unit) -> AppM Aff Unit
registerUser user onResponse = do
  res <- Utils.makeRequest (Apiary.Route :: Endpoints.RegisterUser) Apiary.none Apiary.none { user }
  onResponse $ res >>= (\success -> success # match { ok: Right <<< _.user, unprocessableEntity: Left <<< UnprocessableEntity <<< _.errors })

updateUser :: { | ProfileRep ( email :: String, password :: String ) } -> (Either Error User -> AppM Aff Unit) -> AppM Aff Unit
updateUser user onResponse = do
  res <- Utils.makeSecureRequest (Apiary.Route :: Endpoints.UpdateUser) Apiary.none Apiary.none { user }
  onResponse $ res >>= (\success -> success # match { ok: Right <<< _.user, unprocessableEntity: Left <<< UnprocessableEntity <<< _.errors })

-- | Article
listArticles :: ArticlesQuery -> (Either Error { articles :: Array Article, articlesCount :: Int } -> AppM Aff Unit) -> AppM Aff Unit
listArticles query onResponse = do
  res <- Utils.makeRequest (Apiary.Route :: Endpoints.ListArticles) Apiary.none query Apiary.none
  onResponse $ res >>= \success -> success # match { ok: Right }

listFeed :: ArticlesQuery -> (Either Error { articles :: Array Article, articlesCount :: Int } -> AppM Aff Unit) -> AppM Aff Unit
listFeed query onResponse = do
  res <- Utils.makeSecureRequest (Apiary.Route :: Endpoints.ListFeed) Apiary.none query Apiary.none
  onResponse $ res >>= \success -> success # match { ok: Right }

getArticle :: Slug -> (Either Error Article -> AppM Aff Unit) -> AppM Aff Unit
getArticle slug onResponse = do
  res <- Utils.makeRequest (Apiary.Route :: Endpoints.GetArticle) { slug } Apiary.none Apiary.none
  onResponse $ res >>= (\success -> success # match { ok: Right <<< _.article, notFound: Left <<< NotFound })

submitArticle :: Maybe Slug -> { | ArticleRep () } -> (Either Error Article -> AppM Aff Unit) -> AppM Aff Unit
submitArticle slug article onResponse = do
  res <- case slug of
    Nothing -> map expand <$> Utils.makeSecureRequest (Apiary.Route :: Endpoints.CreateArticle) Apiary.none Apiary.none { article }
    Just slug' -> map expand <$> Utils.makeSecureRequest (Apiary.Route :: Endpoints.UpdateArticle) { slug: slug' } Apiary.none { article }
  onResponse $ res >>= (\success -> success # match { ok: Right <<< _.article, unprocessableEntity: Left <<< UnprocessableEntity <<< _.errors })

deleteArticle :: Slug -> (Either Error Unit -> AppM Aff Unit) -> AppM Aff Unit
deleteArticle slug onResponse = do
  res <- Utils.makeSecureRequest (Apiary.Route :: Endpoints.DeleteArticle) { slug } Apiary.none Apiary.none
  onResponse $ res >>= (\success -> success # match { ok: const $ Right unit })

-- | Favorite
toggleFavorite :: Maybe Article -> (Article -> AppM Aff Unit) -> AppM Aff Unit
toggleFavorite article onResponse =
  for_ article \{ slug, favorited } -> do
    res <-
      if favorited then
        Utils.makeSecureRequest (Apiary.Route :: Endpoints.UnfavoriteArticle) { slug } Apiary.none Apiary.none
      else
        Utils.makeSecureRequest (Apiary.Route :: Endpoints.FavoriteArticle) { slug } Apiary.none Apiary.none
    for_ res $ match { ok: onResponse <<< _.article }

-- | Comment
listComments :: Slug -> (Either Error (Array Comment) -> AppM Aff Unit) -> AppM Aff Unit
listComments slug onResponse = do
  res <- Utils.makeRequest (Apiary.Route :: Endpoints.ListComments) { slug } Apiary.none Apiary.none
  onResponse $ res >>= \success -> success # match { ok: Right <<< _.comments }

createComment :: Slug -> { body :: String } -> (Either Error Comment -> AppM Aff Unit) -> AppM Aff Unit
createComment slug comment onResponse = do
  res <- Utils.makeSecureRequest (Apiary.Route :: Endpoints.CreateComment) { slug } Apiary.none { comment }
  onResponse $ res >>= (\success -> success # match { ok: Right <<< _.comment })

deleteComment :: Slug -> CommentId -> (Either Error Unit -> AppM Aff Unit) -> AppM Aff Unit
deleteComment slug id onResponse = do
  res <- Utils.makeSecureRequest (Apiary.Route :: Endpoints.DeleteComment) { slug, id } Apiary.none Apiary.none
  onResponse $ res >>= (\success -> success # match { ok: const $ Right unit })

-- | Profile
getProfile :: Username -> (Either Error Profile -> AppM Aff Unit) -> AppM Aff Unit
getProfile username onResponse = do
  res <- Utils.makeRequest (Apiary.Route :: Endpoints.GetProfile) { username } Apiary.none Apiary.none
  onResponse $ res >>= (\success -> success # match { ok: Right <<< _.profile, notFound: Left <<< NotFound })

-- | Follow
toggleFollow :: Maybe Profile -> (Profile -> AppM Aff Unit) -> AppM Aff Unit
toggleFollow profile onResponse =
  for_ profile \{ username, following } -> do
    res <-
      if following then
        Utils.makeSecureRequest (Apiary.Route :: Endpoints.UnfollowProfile) { username } Apiary.none Apiary.none
      else
        Utils.makeSecureRequest (Apiary.Route :: Endpoints.FollowProfile) { username } Apiary.none Apiary.none
    for_ res $ match { ok: onResponse <<< _.profile }

-- | Tag
listTags :: (Either Error (Array String) -> AppM Aff Unit) -> AppM Aff Unit
listTags onResponse = do
  res <- Utils.makeRequest (Apiary.Route :: Endpoints.ListTags) Apiary.none Apiary.none Apiary.none
  onResponse $ res >>= \success -> success # match { ok: Right <<< _.tags }
