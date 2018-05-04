module Update exposing (..)

import Date exposing(..)
import Date.Extra exposing (toFormattedString)

import FirebaseConnector exposing (..)
import Model exposing (..)
import Types exposing (Rep, ExerciseType, Exercise, Workout)

type Msg
    = Init_Workout
    | Current_Date (Maybe Date)
    | Exercise_Selected String
    | Weight_Changed String
    | Repetitions_Changed String
    | Add_Set
    | Add_Exercise
    | Create_Workout
    | Workout_Created Workout
    | Workouts_Loaded (List Workout)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      Init_Workout ->
        let
          newModel = startNewWorkout model
        in
          (newModel, Cmd.none)
      Current_Date date ->
        let
          currentWeek = case date of
            Just val -> Date.Extra.weekNumber val
            Nothing -> -1
          nextModel =
            { model | currentDate = date }
        in
          (nextModel, fetchWorkouts currentWeek)
      Exercise_Selected exerciseName ->
        let
          updatedExercise = model.currentExercise |> setExerciseType exerciseName
          newModel = { model | currentExercise = updatedExercise,
            selectedExercise = { name = exerciseName }
          }
        in
          (newModel, Cmd.none)
      Weight_Changed weight ->
        ({ model | repWeight = weight }, Cmd.none)
      Repetitions_Changed reps ->
        ({ model | repetitions = reps }, Cmd.none)
      Add_Set ->
        let
          newSet = { weight = Result.withDefault 0 (String.toInt model.repWeight)
            , repetitions = Result.withDefault 0 (String.toInt model.repetitions) }
          withNewSet = newSet :: model.currentExercise.sets
          nextModel = { model | currentExercise = model.currentExercise
            |> setListOfSets withNewSet,
            repWeight = "",
            repetitions = ""
          }
        in
          (nextModel, Cmd.none)
      Add_Exercise ->
        let
          existingExercises = case model.currentWorkout of
            Just workout -> workout.exercises
            Nothing -> []
          withNewExercise = model.currentExercise :: existingExercises
          nextModel = { model | currentWorkout = model.currentWorkout |> setListOfExercises withNewExercise,
            currentExercise = currentExercise
          }
        in
          (nextModel, Cmd.none)
      Create_Workout ->
        let
          newWorkout = case model.currentWorkout of
            Just workout -> workout
            Nothing -> Debug.crash "oh no :("
        in
          (model, saveWorkout (workoutEncoder newWorkout))
      Workout_Created workout ->
        let
          withNewWorkout = workout :: model.workouts
          nextModel = { model | currentWorkout = Nothing, workouts = withNewWorkout }
        in
          (nextModel, Cmd.none)
      Workouts_Loaded wos ->
        ({ model | workouts = wos }, Cmd.none)
      NoOp ->
        (model, Cmd.none)


setListOfSets: List Rep -> Exercise -> Exercise
setListOfSets sets exercise =
  { exercise | sets = sets }

setExerciseType: String -> Exercise -> Exercise
setExerciseType exerciseName exercise =
  let
    exerciseType = Debug.log "" { name = exerciseName}
  in
    { exercise | exerciseType = exerciseType }

setListOfExercises: List Exercise -> Maybe Workout -> Maybe Workout
setListOfExercises exercises workout =
  case workout of
    Just wo -> Just { wo | exercises = exercises }
    Nothing -> Nothing


startNewWorkout: Model -> Model
startNewWorkout model =
  let
    today = case model.currentDate of
      Nothing -> Date.fromString "2018/1/1" |> Result.withDefault (Date.fromTime 0)
      Just date -> date
    newWorkout =
      { uuid = Nothing
      , date = today
      , weekNumber = Date.Extra.weekNumber today
      , exercises = []
      }
    newModel = { model | currentWorkout = Just newWorkout }
  in
    newModel