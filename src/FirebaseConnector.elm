port module FirebaseConnector exposing (..)

import Types exposing (Workout, Exercise, ExerciseType, Rep)
import Json.Decode.Pipeline exposing (decode, required)

import Json.Decode as Decode exposing (Decoder, int, string, list, succeed, fail, andThen)
import Date

port fetchWorkouts: Int -> Cmd msg

port receiveWorkouts : (Decode.Value -> msg) -> Sub msg

workoutListDecoder: Decoder (List Workout)
workoutListDecoder =
  Decode.list workoutDecoder

workoutDecoder: Decoder Workout
workoutDecoder =
  decode Workout
  |> required "date" dateEncoder
  |> required "weekNumber" int
  |> required "exercises" (list exerciseDecoder)

exerciseDecoder: Decoder Exercise
exerciseDecoder =
  decode Exercise
  |> required "exerciseType" exerciseTypeDecoder
  |> required "sets" (list repEncoder)

exerciseTypeDecoder: Decoder ExerciseType
exerciseTypeDecoder =
  decode ExerciseType
  |> required "name" string

repEncoder: Decoder Rep
repEncoder =
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
