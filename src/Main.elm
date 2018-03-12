module Main exposing (..)

import Html exposing (Html, text, div, h3, img, button, label, input, select, option, span, p, h4, ul ,li)
import Html.Attributes exposing (src, class, value, type_, style)
import Html.Events exposing (onClick, onInput)

import Date exposing(..)
import Task exposing(..)
import Color exposing(..)
import Date.Extra

import Material.Icons.Action exposing (check_circle)


---- MODEL ----

type alias Rep =
  {
  weight: Int
  , repetitions: Int
  }

type alias ExerciseType =
  { name: String }

type alias Exercise =
  {
  exerciseType: String
  , sets: List Rep
  }

type alias Workout =
  { date: Date
  , weekNumber: Int
  , exercises: List Exercise
  }

type alias Model =
  {
    workouts: List Workout,
    currentWorkout: Maybe Workout,
    currentDate: Maybe Date,
    currentExercise: Exercise,
    selectedExercise: String,
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
  , selectedExercise = ""
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
      Just firstType -> firstType.name 
      Nothing -> "",
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
          nextModel =
            { model | currentDate = date }
        in
          (nextModel, Cmd.none)
      Exercise_Selected exerciseName ->
        let
          logged = Debug.log "" exerciseName
          foo = Debug.log "" model.currentExercise
          updatedExercise = Debug.log "" model.currentExercise |> setExerciseType exerciseName
          newModel = { model | currentExercise = updatedExercise }
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
            |> setListOfSets withNewSet
          }
        in
          (nextModel, Cmd.none)
      Add_Exercise ->
        let
          existingExercises = case model.currentWorkout of
            Just workout -> workout.exercises
            Nothing -> []
          withNewExercise = model.currentExercise :: existingExercises
          nextModel = { model | currentWorkout = model.currentWorkout |> setListOfExercises withNewExercise }
        in
          (nextModel, Cmd.none)


setListOfSets: List Rep -> Exercise -> Exercise
setListOfSets sets exercise =
  { exercise | sets = sets }
  
setExerciseType: String -> Exercise -> Exercise
setExerciseType exerciseType exercise =
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
      { date = today
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
        h3 [] [ text "Current week"]
        , div [class "week-container"] [  ]
        , button [ onClick Init_Workout ] [ text "New exercise"]
        , workoutForm model
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
            , select [ class "type-select", value model.selectedExercise, onInput Exercise_Selected ] ( List.map typeOption defaultTypes )
          ]
          , newRepItem
          , div [] ( List.map repItem model.currentExercise.sets )
          , div [] [
            button [ onClick Add_Exercise, class "button-primary"] [ text "Done"]
          ]
          , exerciseList model.currentWorkout
        ]
        , div [ class "workout-form-footer" ] [
          button [ class "button-primary" ] [ text "Create" ]
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

newRepItem: Html Msg
newRepItem =
  div [ class "rep-item" ] [
    div [ class "input-container" ] [
      label [ class "input-label" ] [ text "Repetitions" ]
      , input [ type_ "text", onInput Repetitions_Changed ] []
    ]
    , div [ class "input-container" ] [
      label [ class "input-label" ] [ text "Weight" ]
      , input [ type_ "text", onInput Weight_Changed ] []
    ]
    , button [ onClick Add_Set, class "rep-button"] [check_circle Color.red 36 ]
  ]

exerciseList: Maybe Workout -> Html Msg
exerciseList workout =
  case workout of
    Just wo -> div [] (List.map renderExercise wo.exercises)
    Nothing -> div [] []

renderExercise: Exercise -> Html Msg
renderExercise exercise =
  div [] [
    h4 [] [ text exercise.exerciseType ],
    ul [] (List.map renderSetInExercise exercise.sets)
  ]

renderSetInExercise: Rep -> Html Msg
renderSetInExercise set =
  li [] [ repItem set ]


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
