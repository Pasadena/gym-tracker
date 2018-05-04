module Main exposing (..)

import Types exposing (Rep, ExerciseType, Exercise, Workout)
import View exposing (view)
import Model exposing (Model, model)
import Update exposing(..)

import Html exposing (Html)
import Date exposing(..)
import Task exposing(..)
import FirebaseConnector exposing (..)

import Json.Decode as Decode

--- SUBSCRIPTIONS ---

decodeSubscriptions: Decode.Value -> Result String (List Workout)
decodeSubscriptions json =
  Decode.decodeValue workoutListDecoder json

decodeSubscription: Decode.Value -> Result String Workout
decodeSubscription json =
  Decode.decodeValue workoutDecoder json

jsonToWorkout: Decode.Value -> Msg
jsonToWorkout jsonVal =
  case (decodeSubscription jsonVal) of
    Ok workout -> Workout_Created workout
    Err message -> NoOp

getJsonResult: Decode.Value -> Msg
getJsonResult jsonVal =
  case (decodeSubscriptions jsonVal) of
    Ok workouts -> Workouts_Loaded workouts
    Err message -> NoOp

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch([
    receiveWorkouts getJsonResult
    , saveSucceeds jsonToWorkout
    ])

---- PROGRAM ----

now : Cmd Msg
now =
  Task.perform (Just >> Current_Date) Date.now

init : ( Model, Cmd Msg )
init =
    ( model, now )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
