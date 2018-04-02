module Main exposing (..)

import Types exposing (Rep, ExerciseType, Exercise, Workout)

import Html exposing (Html, text, div, h3, img, button, label, input, select, option, span, p, h4, h1, ul ,li)
import Html.Attributes exposing (src, class, value, type_, style)
import Html.Events exposing (onClick, onInput)

import Date exposing(..)
import Task exposing(..)
import Color exposing(..)
import Date.Extra exposing (toFormattedString)
import FirebaseConnector exposing (..)

import Json.Decode as Decode

import Material.Icons.Action exposing (check_circle)


---- MODEL ----

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

now : Cmd Msg
now =
  Task.perform (Just >> Current_Date) Date.now

init : ( Model, Cmd Msg )
init =
    ( model, now )



---- UPDATE ----


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


---- VIEW ----


view : Model -> Html Msg
view model =
    div [class "content"]
        [
        h1 [] [ text "Current week"]
        , div [class "week-container"] [  ]
        , workoutList model
        , button [ onClick Init_Workout ] [ text "New exercise"]
        , workoutForm model
      ]

workoutList: Model -> Html Msg
workoutList model =
  div [class "workout-list"] (List.map workoutItem model.workouts)

workoutItem: Workout -> Html Msg
workoutItem workout =
  div [ class "workout-item" ] [
    div [] [ text (toFormattedString "dd.MM.yyyy" workout.date) ]
    , div [] [ text (toString (List.length workout.exercises)) ]
  ]

workoutForm: Model -> Html Msg
workoutForm model =
  case model.currentWorkout of
    Nothing -> div [] []
    Just workout ->
      div [ class "workout-form" ] [
        div [ class "workout-form-header" ] [
          h3 [] [ text "Create a new workout"]
        ]
        , div [ class "workout-form-content" ] [
          div [ class "input-container" ] [
            label [] [ text "Whatcha gonna do:"]
            , select [ class "type-select", value model.selectedExercise.name, onInput Exercise_Selected ] ( List.map typeOption defaultTypes )
          ]
          , newRepItem model
          , div [] ( List.map repItem model.currentExercise.sets )
          , div [] [
            button [ onClick Add_Exercise, class "button-primary"] [ text "Done"]
          ]
          , exerciseList model.currentWorkout
        ]
        , div [ class "workout-form-footer" ] [
          button [ class "button-primary", onClick Create_Workout ] [ text "Create" ]
          , button [ class "button" ] [ text "Cancel" ]
        ]
      ]

typeOption: ExerciseType -> Html Msg
typeOption exerciseType =
  option [ value exerciseType.name ] [ text exerciseType.name ]

repItem: Rep -> Html Msg
repItem rep =
  div [ class "rep-item read-only-rep" ] [
    p [ class "rep-item-val" ] [ text (String.append (toString rep.repetitions) " reps ") ]
    , span [ style [("alignSelf", "center"), ("marginLeft", "3px"), ("marginRight", "3px")] ] [ text "w/" ]
    , p [ class "rep-item-val" ] [ text (String.append (toString rep.weight) " kg") ]
  ]

newRepItem: Model -> Html Msg
newRepItem model =
  div [ class "rep-item" ] [
    div [ class "input-container" ] [
      label [ class "input-label" ] [ text "Repetitions" ]
      , input [ type_ "text", value model.repetitions, onInput Repetitions_Changed ] []
    ]
    , div [ class "input-container" ] [
      label [ class "input-label" ] [ text "Weight" ]
      , input [ type_ "text", value model.repWeight, onInput Weight_Changed ] []
    ]
    , div [ class "rep-button-container" ] [
      button [ onClick Add_Set, class "rep-button"] [check_circle Color.red 36 ]
    ]
  ]

exerciseList: Maybe Workout -> Html Msg
exerciseList workout =
  case workout of
    Just wo -> div [] (List.map renderExercise wo.exercises)
    Nothing -> div [] []

renderExercise: Exercise -> Html Msg
renderExercise exercise =
  div [] [
    h4 [] [ text exercise.exerciseType.name ],
    ul [] (List.map renderSetInExercise exercise.sets)
  ]

renderSetInExercise: Rep -> Html Msg
renderSetInExercise set =
  li [] [ repItem set ]

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


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
