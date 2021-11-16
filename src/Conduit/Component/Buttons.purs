module Conduit.Component.Buttons where

import Prelude
import Conduit.Data.Username (Username, toString)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler)
import React.Basic.Hooks as React

data ButtonSize
  = Icon
  | Medium

derive instance Eq ButtonSize

-- | Favorite Button
type FavoriteButtonProps =
  { size :: ButtonSize
  , favorited :: Boolean
  , count :: Int
  , onClick :: EventHandler
  }

favoriteButton :: FavoriteButtonProps -> React.JSX
favoriteButton { size, favorited, count, onClick } =
  R.button
    { className: "btn btn-sm " <> if favorited then "btn-primary" else "btn-outline-primary"
    , onClick
    , children:
        [ R.i
            { className: "ion-heart"
            , children: []
            }
        , R.span_
            [ R.text case favorited, size of
                true, Medium -> " Unfavorite Article"
                _, Medium -> " Favorite Article"
                _, _ -> " "
            ]
        , R.span
            { className: "counter"
            , children:
                [ R.text case size of
                    Icon -> " " <> show count
                    _ -> " (" <> show count <> ")"
                ]
            }
        ]
    }

-- | Follow Button
type FollowButtonProps =
  { following :: Boolean
  , username :: Username
  , onClick :: EventHandler
  }

followButton :: FollowButtonProps -> React.JSX
followButton { following, username, onClick } =
  if following then
    R.button
      { className: "btn btn-sm action-btn btn-secondary"
      , onClick
      , children: [ R.text $ " Unfollow " <> toString username ]
      }
  else
    R.button
      { className: "btn btn-sm action-btn btn-outline-secondary"
      , onClick
      , children:
          [ R.i
              { className: "ion-plus-round"
              , children: []
              }
          , R.text $ " Follow " <> toString username
          ]
      }
