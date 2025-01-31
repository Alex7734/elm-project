module Util.Time exposing (..)

import Time
import Dict exposing (Dict)

type Date
    = Date { year : Int, month : Time.Month, day : Int }


monthToString : Time.Month -> String
monthToString month =
    let
        monthDict =
            Dict.fromList
                [ ("Jan", "Jan")
                , ("Feb", "Feb")
                , ("Mar", "Mar")
                , ("Apr", "Apr")
                , ("May", "May")
                , ("Jun", "Jun")
                , ("Jul", "Jul")
                , ("Aug", "Aug")
                , ("Sep", "Sep")
                , ("Oct", "Oct")
                , ("Nov", "Nov")
                , ("Dec", "Dec")
                ]

        monthKey =
            case month of
                Time.Jan -> "Jan"
                Time.Feb -> "Feb"
                Time.Mar -> "Mar"
                Time.Apr -> "Apr"
                Time.May -> "May"
                Time.Jun -> "Jun"
                Time.Jul -> "Jul"
                Time.Aug -> "Aug"
                Time.Sep -> "Sep"
                Time.Oct -> "Oct"
                Time.Nov -> "Nov"
                Time.Dec -> "Dec"
    in
        Dict.get monthKey monthDict |> Maybe.withDefault "Unknown"

posixToDate : Time.Zone -> Time.Posix -> Date
posixToDate tz time =
    let
        year =
            Time.toYear tz time

        month =
            Time.toMonth tz time

        day =
            Time.toDay tz time
    in
    Date { year = year, month = month, day = day }


{-| Formats a `Date` instance.

    import Time

    formatDate (Date { year = 2022, month = Time.Apr, day =  4 }) {- ignore -} --> "2022 Apr 04"

    formatDate (Date { year = 2022, month = Time.Jan, day = 12 }) {- ignore -} --> "2022 Jan 12"

-}
formatDate : Date -> String
formatDate (Date { year, month, day }) =
    [ String.fromInt year
    , monthToString month
    , String.fromInt day |> String.padLeft 2 '0'
    ]
        |> String.join " "


formatTime : Time.Zone -> Time.Posix -> String
formatTime tz time =
    let
        date =
            posixToDate tz time

        hour =
            Time.toHour tz time |> String.fromInt |> String.padLeft 2 '0'

        minute =
            Time.toMinute tz time |> String.fromInt |> String.padLeft 2 '0'
    in
    formatDate date ++ " " ++ hour ++ ":" ++ minute


type alias Duration =
    { seconds : Int
    , minutes : Int
    , hours : Int
    , days : Int
    }


{-| Calculates the amount of time that passed between two dates.

The first date (t1) must be **before** the second date (t2), if this not the case, the function should return `Nothing`.

Relevant library functions:

  - Use Time.posixToMillis

```
import Time

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (1000)) --> Just (Duration 1 0 0 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (60 * 1000)) --> Just (Duration 0 1 0 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (60 * 60 * 1000)) --> Just (Duration 0 0 1 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (24 * 60 * 60 * 1000)) --> Just (Duration 0 0 0 1)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (24 * 60 * 60 * 1000 + 1000)) --> Just (Duration 1 0 0 1)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (4 * 24 * 60 * 60 * 1000 + 3 * 60 * 60 * 1000 + 2 * 60 * 1000 + 1000)) --> Just (Duration 1 2 3 4)

durationBetween (Time.millisToPosix 1000) (Time.millisToPosix 0) --> Nothing

durationBetween (Time.millisToPosix 1000) (Time.millisToPosix 1000) --> Nothing
```

-}
durationBetween : Time.Posix -> Time.Posix -> Maybe Duration
durationBetween t1 t2 =
    if Time.posixToMillis t1 >= Time.posixToMillis t2 then
        Nothing
    else
        let
            diffMillis =
                Time.posixToMillis t2 - Time.posixToMillis t1

            totalSeconds =
                diffMillis // 1000

            seconds =
                Basics.remainderBy 60 totalSeconds

            totalMinutes =
                totalSeconds // 60

            minutes =
                Basics.remainderBy 60 totalMinutes

            totalHours =
                totalMinutes // 60

            hours =
                Basics.remainderBy 24 totalHours

            days =
                totalHours // 24
        in
            Just { seconds = seconds, minutes = minutes, hours = hours, days = days }


{-| Format a `Duration` as a human readable string

    formatDuration (Duration 1 0 0 0) --> "1 second ago"

    formatDuration (Duration 2 0 0 0) --> "2 seconds ago"

    formatDuration (Duration 0 1 0 0) --> "1 minute ago"

    formatDuration (Duration 0 0 2 0) --> "2 hours ago"

    formatDuration (Duration 0 0 0 3) --> "3 days ago"

    formatDuration (Duration 0 1 1 1) --> "1 day 1 hour 1 minute ago"

    formatDuration (Duration 0 47 6 2) --> "2 days 6 hours 47 minutes ago"

    formatDuration (Duration 0 30 0 1) --> "1 day 30 minutes ago"

-}
formatDuration : Duration -> String
formatDuration { seconds, minutes, hours, days } =
    [ (days, "day")
    , (hours, "hour")
    , (minutes, "minute")
    , (seconds, "second")
    ]
        |> List.filter (\(value, _) -> value > 0)
        |> List.map (\(value, label) -> String.fromInt value ++ " " ++ label ++ (if value > 1 then "s" else ""))
        |> \filteredParts -> case filteredParts of
            [] ->
                "just now"

            parts ->
                String.join " " parts ++ " ago"
