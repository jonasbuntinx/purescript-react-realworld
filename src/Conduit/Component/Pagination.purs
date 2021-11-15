module Conduit.Component.Pagination where

import Prelude
import Data.Array as Array
import Data.Int as Int
import Data.Int as Number
import Data.Monoid (guard)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (JSX, fragment)

type Props
  =
  { offset :: Int
  , limit :: Int
  , totalCount :: Int
  , onChange :: { offset :: Int, limit :: Int } -> Effect Unit
  , focusWindow :: Int
  , marginPages :: Int
  }

pagination :: Props -> JSX
pagination props =
  guard (pageCount > 1) R.ul
    { className: "pagination"
    , children:
        Array.fold
          [ guard (rangeMin > 0)
              [ pageButtons 0 (min (rangeMin - 1) leftMargin)
              ]
          , guard (rangeMin > leftMargin + 1)
              [ separator
              ]
          , [ pageButtons rangeMin rangeMax
            ]
          , guard (rangeMax < rightMargin - 1)
              [ separator
              ]
          , guard (rangeMax < pageCount - 1)
              [ pageButtons (max (rangeMax + 1) rightMargin) (pageCount - 1)
              ]
          ]
    }
  where
  currentPage = props.offset / props.limit

  pageCount = max 1 $ Number.ceil $ Int.toNumber props.totalCount / Int.toNumber props.limit

  onPageChange = \page ->
    let
      pageOffset = page * props.limit
    in
      when (pageOffset >= 0 && page /= currentPage && pageOffset < props.totalCount) do
        props.onChange { offset: pageOffset, limit: props.limit }

  boundedPage = clamp 0 (pageCount - 1) currentPage

  rangeMin = max 0 (boundedPage - (props.focusWindow / 2))

  rangeMax = min (pageCount - 1) (boundedPage + (props.focusWindow / 2))

  pageButtons start end =
    fragment
      $ Array.range start end
          <#> \i ->
            R.li
              { className: "page-item" <> guard (i == currentPage) " active"
              , children:
                  [ R.a
                      { className: "page-link"
                      , href: "#"
                      , onClick: handler preventDefault $ const $ onPageChange i
                      , children: [ R.text $ show (i + 1) ]
                      }
                  ]
              }

  leftMargin = props.marginPages - 1

  rightMargin = pageCount - props.marginPages

  separator =
    R.li
      { className: "page-item disabled"
      , children:
          [ R.span
              { className: "page-link"
              , children: [ R.text "..." ]
              }
          ]
      }
