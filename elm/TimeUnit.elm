module TimeUnit where

import String exposing (padLeft)

type alias Sec = Int
type alias Min = Int

minutes : Sec -> Sec
minutes s = s * 60

timeFormat : Sec -> String
timeFormat s =
    let minStr = (padLeft 2 '0' (toString (s // 60)))
        secStr = (padLeft 2 '0' (toString (s % 60)))
    in
      minStr ++ ":" ++ secStr