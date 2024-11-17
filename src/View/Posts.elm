module View.Posts exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href, class, style)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts)
import Time
import Util.Time
import Model.PostsConfig as Config


{-| Common styles -}

tableStyle : List (Html.Attribute msg)
tableStyle =
    [ style "width" "100%"
    , style "border-collapse" "collapse"
    , style "margin" "20px 0"
    , style "font-size" "16px"
    , style "text-align" "left"
    ]

headerStyle : List (Html.Attribute msg)
headerStyle =
    [ style "background-color" "#f2f2f2"
    , style "border-bottom" "1px solid #ddd"
    ]

cellStyle : List (Html.Attribute msg)
cellStyle =
    [ style "padding" "8px"
    , style "border-bottom" "1px solid #ddd"
    ]

linkStyle : List (Html.Attribute msg)
linkStyle =
    [ style "color" "#1a73e8"
    , style "text-decoration" "none"
    ]

checkboxStyle : List (Html.Attribute msg)
checkboxStyle =
    [ style "margin-right" "8px" ]


{-| Event handlers -}

checkboxHandler : Change -> Html.Attribute Msg
checkboxHandler change =
    Html.Events.onCheck (\_ -> ConfigChanged change)

selectHandler : (String -> Msg) -> Html.Attribute Msg
selectHandler handler =
    Html.Events.onInput handler


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table) -}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config now posts =
    let
        filteredPosts =
            filterPosts config posts
    in
    Html.table (class "post-table" :: tableStyle)
        ( Html.tr (class "post-table-header" :: headerStyle)
            [ Html.th (class "post-table-title" :: cellStyle) [ text "Title" ]
            , Html.th (class "post-table-score" :: cellStyle) [ text "Score" ]
            , Html.th (class "post-table-type" :: cellStyle) [ text "Type" ]
            , Html.th (class "post-table-posted" :: cellStyle) [ text "Posted" ]
            , Html.th (class "post-table-link" :: cellStyle) [ text "Link" ]
            ]
        :: List.map
            (\post ->
                Html.tr [ class "post-row" ]
                    [ Html.td (class "post-title" :: cellStyle) [ text post.title ]
                    , Html.td (class "post-score" :: cellStyle) [ text (String.fromInt post.score) ]
                    , Html.td (class "post-type" :: cellStyle) [ text post.type_ ]
                    , Html.td (class "post-time" :: cellStyle)
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
                    , Html.td (class "post-url" :: cellStyle)
                        [ case post.url of
                            Just url ->
                                Html.a ([ href url, class "post-link" ] ++ linkStyle) [ text "Visit" ]

                            Nothing ->
                                text "N/A"
                        ]
                    ]
            )
            filteredPosts
        )


{-| Show the configuration options for the posts table -}
postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div [ class "posts-config", style "margin" "20px 0" ]
        [ Html.div [ class "config-show-jobs" ]
            [ Html.input
                ([ Html.Attributes.type_ "checkbox"
                , Html.Attributes.id "checkbox-show-job-posts"
                , Html.Attributes.checked config.showJobs
                , class "config-checkbox"
                ] ++ checkboxStyle ++ [ checkboxHandler ToggleShowJobs ])
                []
            , Html.label [ Html.Attributes.for "checkbox-show-job-posts", class "config-label" ] [ text "Show job posts" ]
            ]
        , Html.div [ class "config-show-text-only" ]
            [ Html.input
                ([ Html.Attributes.type_ "checkbox"
                , Html.Attributes.id "checkbox-show-text-only-posts"
                , Html.Attributes.checked config.showTextOnly
                , class "config-checkbox"
                ] ++ checkboxStyle ++ [ checkboxHandler ToggleShowTextOnly ])
                []
            , Html.label [ Html.Attributes.for "checkbox-show-text-only-posts", class "config-label" ] [ text "Show text-only posts" ]
            ]
        , Html.div [ class "config-posts-per-page" ]
            [ Html.label [ Html.Attributes.for "select-posts-per-page", class "config-label" ] [ text "Posts per page: " ]
            , Html.select
                [ Html.Attributes.id "select-posts-per-page"
                , class "config-select"
                , selectHandler ((String.toInt >> Maybe.withDefault config.postsToShow) >> (ConfigChanged << ChangePostsToShow))
                ]
                (List.map
                    (\option ->
                        Html.option
                            [ Html.Attributes.value (String.fromInt option)
                            , class "config-option"
                            , if option == config.postsToShow then Html.Attributes.selected True else Html.Attributes.selected False
                            ]
                            [ text (String.fromInt option) ]
                    )
                    [10, 25, 50]
                )
            ]
        , Html.div [ class "config-sort-by" ]
            [ Html.label [ Html.Attributes.for "select-sort-by", class "config-label" ] [ text "Sort by: " ]
            , Html.select
                [ Html.Attributes.id "select-sort-by"
                , class "config-select"
                , selectHandler ((Config.sortFromString >> Maybe.withDefault config.sortBy) >> (ConfigChanged << ChangeSortBy))
                ]
                (List.map
                    (\option ->
                        Html.option
                            [ Html.Attributes.value (Config.sortToString option)
                            , class "config-option"
                            , if option == config.sortBy then Html.Attributes.selected True else Html.Attributes.selected False
                            ]
                            [ text (Config.sortToString option) ]
                    )
                    Model.PostsConfig.sortOptions
                )
            ]
        ]
