module Model exposing (..)

import Date exposing(..)

import Types exposing (Rep, ExerciseType, Exercise, Workout)

type alias Model =
  {
    workouts: List Workout,
    currentWorkout: Maybe Workout,
    currentDate: Maybe Date,
    currentExercise: Exercise,
    selectedExercise: ExerciseType,
    repWeight: String,
    repetitions: String
  }


model: Model
model =
  {
  workouts = []
  , currentWorkout = Nothing
  , currentDate = Nothing
  , currentExercise = currentExercise
  , selectedExercise = { name = "Bench press" }
  , repWeight = ""
  , repetitions = ""
  }

defaultTypes: List ExerciseType
defaultTypes =
  { name = "Bench press" } :: { name = "Deadlift" } :: { name = "Incline Dumbbell Press" }  :: []

currentExercise: Exercise
currentExercise =
  {
    exerciseType = case List.head defaultTypes of
      Just firstType -> firstType
      Nothing -> { name = "Bench press" },
    sets = []
  }