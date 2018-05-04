module Types exposing (..)

import Date exposing (..)

type alias Rep =
  {
  weight: Int
  , repetitions: Int
  }

type alias ExerciseType =
  { name: String }

type alias Exercise =
  {
  exerciseType: Maybe ExerciseType
  , sets: List Rep
  }

type alias Workout =
  { uuid: Maybe String
  , date: Date
  , weekNumber: Int
  , exercises: List Exercise
  }
