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
  exerciseType: ExerciseType
  , sets: List Rep
  }

type alias Workout =
  { date: Date
  , weekNumber: Int
  , exercises: List Exercise
  }
