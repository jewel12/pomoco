module Pomoco where

import Time exposing (Time, every, second, inSeconds)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (..)
import String exposing (toInt)

import TimeUnit exposing (Sec, Min, minutes, timeFormat)

type Action = NoOp
            | Count
            | Start
            | Pause
            | Resume
            | NextStatus
            | LogDBLoad (List Log)
            | SetInterval TimerStatus Sec

type TimerStatus = Working
                 | ShortBreak
                 | LongBreak
                 | Stop
                 | PauseStatus TimerStatus

type alias State =
    { timer: Timer
    , log: List Log
    }

type alias Timer =
    { status: TimerStatus
    , interval: IntervalSetting
    , elapsed: Sec
    , longBreakPomodoroInterval: Int
    , pomodoros: Int
    }

type alias IntervalSetting =
    { working: Sec
    , shortBreak: Sec
    , longBreak: Sec
    }

type alias Log =
    { time: Sec
    , created_at: String
    }

initState : State
initState =
    { timer = initTimer
    , log = []
    }

initTimer : Timer
initTimer =
    { status = Stop
    , interval = initInterval
    , elapsed = 0
    , longBreakPomodoroInterval = 4
    , pomodoros = 1
    }

initInterval : IntervalSetting
initInterval =
    { working = minutes 15
    , shortBreak = minutes 3
    , longBreak = minutes 20
    }

update : Action -> State -> State
update action state =
    case action of
      Start ->
          { state| timer = (updateTimerStatus state.timer Working) }

      Pause ->
          { state| timer =
                (updateTimerStatus state.timer (PauseStatus state.timer.status)) }

      Resume ->
          { state| timer =
                (updateTimerStatus state.timer (resumeStatus state.timer)) }

      NextStatus ->
          { state| timer = (nextTimer state.timer) }

      Count ->
          case running state.timer of
            True -> case (timeOver state.timer) of
                   True ->
                       { state| timer = (nextTimer state.timer) }

                   False ->
                       { state| timer = (addTimerElapsed state.timer 1) }

            False -> state

      SetInterval target s -> { state| timer = updateTimerInterval state.timer target s}

      _ -> state

running : Timer -> Bool
running t =
    case t.status of
      Stop -> False
      PauseStatus a -> False
      _ -> True

updateTimerStatus : Timer -> TimerStatus -> Timer
updateTimerStatus t a = { t| status = a }

updateTimerInterval : Timer -> TimerStatus -> Sec -> Timer
updateTimerInterval t ts s =
    let updateF = case ts of
                      Working -> updateWorkingInterval
                      ShortBreak -> updateShortBreakInterval
                      LongBreak -> updateLongBreakInterval
                      _ -> (\interval _ -> interval)
    in { t| interval = updateF t.interval s }

updateWorkingInterval : IntervalSetting -> Sec -> IntervalSetting
updateWorkingInterval interval s = { interval| working = (minutes s) }

updateShortBreakInterval : IntervalSetting -> Sec -> IntervalSetting
updateShortBreakInterval interval s = { interval| shortBreak = (minutes s) }

updateLongBreakInterval : IntervalSetting -> Sec -> IntervalSetting
updateLongBreakInterval interval s = { interval| longBreak = (minutes s) }

addTimerElapsed : Timer -> Sec -> Timer
addTimerElapsed t s = { t| elapsed = t.elapsed + s }

resumeStatus : Timer -> TimerStatus
resumeStatus t = case t.status of
                   PauseStatus a -> a
                   _ -> Stop

timeOver : Timer -> Bool
timeOver t =
    case t.status of
      Working ->
          t.elapsed >= t.interval.working

      ShortBreak ->
          t.elapsed >= t.interval.shortBreak

      LongBreak ->
          t.elapsed >= t.interval.longBreak

      _ -> False

nextTimer t =
    let next = case t.status of
                 PauseStatus a -> nextStatus (updateTimerStatus t a)
                 _ -> nextStatus t
    in
      { t|
        status = next
      , elapsed = 0
      , pomodoros = case next of
                       LongBreak -> 1
                       Working -> t.pomodoros + 1
                       _ -> t.pomodoros
      }

nextStatus : Timer -> TimerStatus
nextStatus t =
    case t.status of
      Working -> case longBreakable t of
                   True -> LongBreak
                   False -> ShortBreak
      ShortBreak -> Working
      LongBreak -> Working
      PauseStatus a -> a
      Stop -> Stop

longBreakable : Timer -> Bool
longBreakable t =
    t.pomodoros >= t.longBreakPomodoroInterval

{-- Signails --}
status : Signal State
status =
    Signal.foldp update initState signals

signals : Signal Action
signals =
    Signal.mergeMany
              [ actions.signal
              , map (always Count) (every second)
              , logDbLoad
              ]

timerSignal : Signal Timer
timerSignal = map (\s -> s.timer) status

timeOverSignal : Signal Bool
timeOverSignal = map timeOver timerSignal

workingTimeOverSignal : Signal Bool
workingTimeOverSignal =
    Signal.dropRepeats (map workingTimeOver timerSignal)

workingTimeOver : Timer -> Bool
workingTimeOver t = t.status == Working && timeOver t

workingTimeSignal : Signal (Maybe Int)
workingTimeSignal =
    Signal.dropRepeats (map (\t -> case workingTimeOver t of
                                 True -> Just t.interval.working
                                 False -> Nothing
                        ) timerSignal)

logDbLoad : Signal Action
logDbLoad = map LogDBLoad logDbLoadIn

actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp

startButtonClick : Signal Bool
startButtonClick = map ((==) Start) actions.signal

{---- Views ----}
view : Address Action -> State -> Html
view address s =
    div [id "main", width 400, height 400] [ statusHeaderView s.timer
                    , timerView address s.timer
                    , logView s.log
                    ]

statusHeaderView : Timer -> Html
statusHeaderView t =
    div [id "status"] [ text (toString t.status) ]

remainSec : Timer -> Sec
remainSec t =
    case t.status of
      PauseStatus a -> remainSecWithStatus t a
      st -> remainSecWithStatus t st

remainSecWithStatus : Timer -> TimerStatus -> Sec
remainSecWithStatus t ts =
    case ts of
      Working -> t.interval.working - t.elapsed
      ShortBreak -> t.interval.shortBreak - t.elapsed
      LongBreak -> t.interval.longBreak - t.elapsed
      _ -> 0

timerView : Address Action -> Timer -> Html
timerView address t =
    div [id "remains"] [ timerTextView t
                       , (workButton t) address
                       , button [ id "nextButton"
                                , class "button"
                                , onClick address NextStatus
                                ] [text "next" ]
                       ]

workButton : Timer -> (Address Action -> Html)
workButton t =
    case t.status of
      Stop -> startButton
      PauseStatus a -> resumeButton
      _ -> pauseButton

startButton : Address Action -> Html
startButton address =
  button [ class "button", id "startButton", onClick address Start ] [text "start" ]

resumeButton : Address Action -> Html
resumeButton address =
    button [ class "button", id "resumeButton" , onClick address Resume ] [text "resume" ]

pauseButton : Address Action -> Html
pauseButton address =
    button [ class "button", id "pauseButton" , onClick address Pause ] [text "pause" ]

timerTextView : Timer -> Html
timerTextView t =
    div [ id "timerText"
        , class (timerTextClass t)
        ] [ text (timeFormat (remainSec t)) ]

timerTextClass : Timer -> String
timerTextClass t =
    case t.status of
      ShortBreak -> "breaking"
      LongBreak -> "breaking"
      PauseStatus a -> timerTextClass (updateTimerStatus t a)
      _ -> ""

logView : List Log -> Html
logView log =
    div [id "logView"] [ canvas [ id "graph", width 200, height 200 ] [] ]

{---- Main ----}
main =
    map (view actions.address) status

{---- Ports ----}
port timeStr : Signal String
port timeStr =
    map (\t ->
         let r = remainSec t
         in
           case r of
             0 -> ""
             _ -> timeFormat r
    ) timerSignal

port startButtonClickOut : Signal Bool
port startButtonClickOut = startButtonClick

port timeOverSignalOut : Signal Bool
port timeOverSignalOut = timeOverSignal

port workingTimeSignalOut : Signal (Maybe Int)
port workingTimeSignalOut = workingTimeSignal

port logDbLoadIn : Signal (List Log)

-- port getIntervalSetting : Maybe IntervalSetting

-- port setIntervalSetting : Signal IntervalSetting
-- port setIntervalSetting = settingState
