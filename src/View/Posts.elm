module View.Posts exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts)
import Time
import Util.Time
import Model.PostsConfig as Config


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config now posts =
    let
        filteredPosts =
            filterPosts config posts
    in
    Html.table []
        ( Html.tr []
            [ Html.th [] [ text "Title" ]
            , Html.th [] [ text "Score" ]
            , Html.th [] [ text "Type" ]
            , Html.th [] [ text "Posted" ]
            , Html.th [] [ text "Link" ]
            ]
        :: List.map
            (\post ->
                Html.tr []
                    [ Html.td [ Html.Attributes.class "post-title" ] [ text post.title ]
                    , Html.td [ Html.Attributes.class "post-score" ] [ text (String.fromInt post.score) ]
                    , Html.td [ Html.Attributes.class "post-type" ] [ text post.type_ ]
                    , Html.td [ Html.Attributes.class "post-time" ]
                        [ let
                              formattedDate = Util.Time.formatDate (Util.Time.posixToDate Time.utc post.time)
                              formattedTime = Util.Time.formatTime Time.utc post.time
                              relativeTime =
                                  case Util.Time.durationBetween post.time now of
                                      Just duration ->
                                          Util.Time.formatDuration duration

                                      Nothing ->
                                          "Just now"
                          in
                          text (formattedDate ++ " " ++ formattedTime ++ " (" ++ relativeTime ++ ")")
                        ]
                    , Html.td [ Html.Attributes.class "post-url" ]
                        [ case post.url of
                            Just url ->
                                Html.a [ href url ] [ text "Visit" ]

                            Nothing ->
                                text "N/A"
                        ]
                    ]
            )
            filteredPosts
        )

{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div []
        [ Html.div []
            [ Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.id "checkbox-show-job-posts"
                , Html.Attributes.checked config.showJobs
                , Html.Events.onCheck (\_ -> ConfigChanged ToggleShowJobs)
                ]
                []
            , Html.label [ Html.Attributes.for "checkbox-show-job-posts" ] [ text "Show job posts" ]
            ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.id "checkbox-show-text-only-posts"
                , Html.Attributes.checked config.showTextOnly
                , Html.Events.onCheck (ConfigChanged << (\_ -> ToggleShowTextOnly))
                ]
                []
            , Html.label [ Html.Attributes.for "checkbox-show-text-only-posts" ] [ text "Show text-only posts" ]
            ]
        , Html.div []
            [ Html.label [ Html.Attributes.for "select-posts-per-page" ] [ text "Posts per page: " ]
            , Html.select
                [ Html.Attributes.id "select-posts-per-page"
                , Html.Events.onInput ((String.toInt >> Maybe.withDefault config.postsToShow) >> (ConfigChanged << ChangePostsToShow))
                ]
                (List.map
                    (\option ->
                        Html.option
                            [ Html.Attributes.value (String.fromInt option)
                            , if option == config.postsToShow then Html.Attributes.selected True else Html.Attributes.selected False
                            ]
                            [ text (String.fromInt option) ]
                    )
                    [10, 25, 50]
                )
            ]
        , Html.div []
            [ Html.label [ Html.Attributes.for "select-sort-by" ] [ text "Sort by: " ]
            , Html.select
                [ Html.Attributes.id "select-sort-by"
                , Html.Events.onInput ((Config.sortFromString >> Maybe.withDefault config.sortBy) >> (ConfigChanged << ChangeSortBy))
                ]
                (List.map
                    (\option ->
                        Html.option
                            [ Html.Attributes.value (Config.sortToString option)
                            , if option == config.sortBy then Html.Attributes.selected True else Html.Attributes.selected False
                            ]
                            [ text (Config.sortToString option) ]
                    )
                    Model.PostsConfig.sortOptions
                )
            ]
        ]
