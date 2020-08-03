module Conduit.Page.Profile where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Profile (GetProfile)
import Conduit.Api.Request as Request
import Conduit.Component.App as App
import Conduit.Component.ArticleList (articleList)
import Conduit.Component.Tabs as Tabs
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Conduit.Env (Env)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.Variant as Variant
import Network.RemoteData as RemoteData
import Network.RemoteData as RemoteDate
import React.Basic.DOM as R
import React.Basic.Hooks as React

type Props
  = { username :: Username
    }

data Action
  = Initialize
  | ChangeTab Tabs.TabId

mkProfilePage :: App.Component Env Props
mkProfilePage =
  App.component "ProfilePage" { init, update } \_ store props -> React.do
    React.useEffect props.username do
      store.dispatch Initialize
      mempty
    pure $ render store props
  where
  init =
    { selectedTab: Nothing
    , profile: RemoteData.NotAsked
    , articles: RemoteData.NotAsked
    }

  update self = case _ of
    Initialize -> do
      self.setState _ { profile = RemoteData.Loading }
      res <- Request.makeRequest (Apiary.Route :: GetProfile) { username: self.props.username } Apiary.none Apiary.none
      self.setState _ { profile = res # either RemoteData.Failure (Variant.match { ok: RemoteData.Success <<< _.profile }) }
    ChangeTab selectedTab -> self.setState _ { selectedTab = Just selectedTab }

  render store props =
    container (userInfo store props)
      [ Tabs.tabs
          { className: "articles-toggle"
          , selectedTab: store.state.selectedTab
          , tabs:
              [ { id: Tabs.TabId "Articles"
                , label: "My Articles"
                , content:
                    R.div_
                      [ articleList { articles: store.state.articles }
                      ]
                }
              , { id: Tabs.TabId "Favorites"
                , label: "My Favorites"
                , content:
                    R.div_
                      [ articleList { articles: store.state.articles }
                      ]
                }
              ]
          , onChange: store.dispatch <<< ChangeTab
          }
      ]

  userInfo store props =
    let
      profile = RemoteDate.toMaybe store.state.profile
    in
      R.div
        { className: "user-info"
        , children:
            [ R.div
                { className: "container"
                , children:
                    [ R.div
                        { className: "row"
                        , children:
                            [ R.div
                                { className: "col-xs-12 col-md-10 offset-md-1"
                                , children:
                                    [ R.img
                                        { className: "user-img"
                                        , src: Avatar.toString $ Avatar.withDefault $ _.image =<< profile
                                        }
                                    , R.h4_ [ R.text $ Username.toString props.username ]
                                    , profile >>= _.bio # maybe React.empty \bio -> R.p_ [ R.text bio ]
                                    , profile # maybe React.empty followButton
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ]
        }

  followButton profile =
    if profile.following then
      R.button
        { className: "btn btn-sm action-btn btn-secondary"
        , children: [ R.text $ " Unfollow " <> Username.toString profile.username ]
        }
    else
      R.button
        { className: "btn btn-sm action-btn btn-outline-secondary"
        , children:
            [ R.i
                { className: "ion-plus-round"
                , children: []
                }
            , R.text $ " Follow " <> Username.toString profile.username
            ]
        }

  container header children =
    R.div
      { className: "profile-page"
      , children:
          [ header
          , R.div
              { className: "container"
              , children:
                  [ R.div
                      { className: "row"
                      , children:
                          [ R.div
                              { className: "col-xs-12 col-md-10 offset-md-1"
                              , children
                              }
                          ]
                      }
                  ]
              }
          ]
      }
