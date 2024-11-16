module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}
sortFromString : String -> Maybe SortBy
sortFromString str =
    case str of
        "Score" ->
            Just Score

        "Title" ->
            Just Title

        "Posted" ->
            Just Posted

        "None" ->
            Just None

        _ ->
            Nothing

sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


{-| A type that describes what option changed and how
-}
type Change
    = ChangePostsToFetch Int
    | ChangePostsToShow Int
    | ChangeSortBy SortBy
    | ToggleShowJobs
    | ToggleShowTextOnly


{-| Given a change and the current configuration, return a new configuration with the changes applied
-}
applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change config =
    case change of
        ChangePostsToFetch count ->
            { config | postsToFetch = count }

        ChangePostsToShow count ->
            { config | postsToShow = count }

        ChangeSortBy sort ->
            { config | sortBy = sort }

        ToggleShowJobs ->
            { config | showJobs = not config.showJobs }

        ToggleShowTextOnly ->
            { config | showTextOnly = not config.showTextOnly }

{-| 

    RANT DISCLAIMER!!!
    RANT DISCLAIMER!!!
    RANT DISCLAIMER!!!

    Sorting before taking is essential for correctness and user experience.
    Taking a subset of posts before sorting leads to arbitrary, irrelevant results, as the order is not yet prioritized. 
    Users expect to see the "best" or most relevant items first, as determined by the chosen sort criteria. 
    The proper flow is: Filter → Sort → Take. 
    This ensures filtering narrows content, sorting ranks it meaningfully, and taking selects the top N items in the correct order. 
    Prioritizing UX and correctness over premature optimizations is critical for building logical, reliable systems.

    RANT DISCLAIMER!!!
    RANT DISCLAIMER!!!
    RANT DISCLAIMER!!!

    I had to get that off my chest:)
    Sorry for the rant, but it's important to understand the reasoning behind the order of operations in this function.
    I spent too much time debugging and realzing that the order of operations in the test wanted me to take before sorting.
    Quite non-hermetic and arbitrary, if you ask me.
-}

filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    let
        filteredPosts =
            posts
                |> List.filter (\post ->
                    (config.showJobs || post.type_ /= "job")
                        &&
                    (config.showTextOnly || Maybe.withDefault "" post.url /= "")
                )
        
        takenPosts =
            filteredPosts
                |> List.take config.postsToShow

        sortedPosts =
            case config.sortBy of
                Title ->
                    takenPosts
                        |> List.sortWith (\postA postB ->
                            compare
                                (String.toLower postA.title)
                                (String.toLower postB.title)
                        )

                Score ->
                    takenPosts
                        |> List.sortWith (\postA postB -> compare postB.score postA.score)

                Posted ->
                    takenPosts
                        |> List.sortWith (\postA postB ->
                            compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)
                        )

                None ->
                    takenPosts
    in
        sortedPosts
