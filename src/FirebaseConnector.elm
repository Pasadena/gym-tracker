port module FirebaseConnector exposing (..)

import Types exposing (Workout, Exercise, ExerciseType, Rep)
import Json.Decode.Pipeline exposing (decode, required, optional)

import Json.Decode as Decode exposing (Decoder, int, string, list, succeed, fail, andThen)
import Json.Encode as Encode exposing (Value)

import Date
import Date.Extra exposing (toFormattedString)

port fetchWorkouts: Int -> Cmd msg

port receiveWorkouts : (Decode.Value -> msg) -> Sub msg

port saveWorkout: Encode.Value -> Cmd msg

port saveSucceeds: (Decode.Value -> msg) -> Sub msg

workoutListDecoder: Decoder (List Workout)
workoutListDecoder =
  Decode.list workoutDecoder

workoutDecoder: Decoder Workout
workoutDecoder =
  decode Workout
  |> optional "uuid" (Decode.map Just string) Nothing
  |> required "date" dateEncoder
  |> required "weekNumber" int
  |> required "exercises" (list exerciseDecoder)

exerciseDecoder: Decoder Exercise
exerciseDecoder =
  decode Exercise
  |> required "exerciseType" exerciseTypeDecoder
  |> required "sets" (list repDecoder)

exerciseTypeDecoder: Decoder ExerciseType
exerciseTypeDecoder =
  decode ExerciseType
  |> required "name" string

repDecoder: Decoder Rep
repDecoder =
    decode Rep
    |> required "weight" int
    |> required "repetitions" int

dateEncoder: Decoder Date.Date
dateEncoder =
  let
    convert: String -> Decoder Date.Date
    convert stringVal =
      case Date.fromString stringVal of
        Ok date -> succeed date
        Err error -> fail error
  in
    string |> andThen convert

workoutEncoder: Workout -> Value
workoutEncoder workout =
  let
    encodedExercises = List.map exerciseEncoder workout.exercises
  in
    Encode.object
      [( "date", Encode.string (toFormattedString "dd.MM.yyyy" workout.date) )
      , ("weekNumber", Encode.int workout.weekNumber)
      , ("exercises", Encode.list encodedExercises)
      ]

exerciseEncoder: Exercise -> Value
exerciseEncoder exercise =
  let
    encodedReps = List.map repEncoder exercise.sets
  in
    Encode.object
      [("exerciseType", exerciseTypeEncoder exercise.exerciseType)
      , ("sets", Encode.list encodedReps)
      ]

exerciseTypeEncoder: ExerciseType -> Value
exerciseTypeEncoder exerciseType =
  Encode.object
    [("name", Encode.string exerciseType.name)]

repEncoder: Rep -> Value
repEncoder rep =
  Encode.object
    [("weight", Encode.int rep.weight)
    , ("repetitions", Encode.int rep.repetitions)
    ]
