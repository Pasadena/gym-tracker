module View exposing (..)

import Html exposing (Html, text, div, h3, img, button, label, input, select, option, span, p, h4, h1, ul ,li)
import Html.Attributes exposing (src, class, value, type_, style)
import Html.Events exposing (onClick, onInput)
import Date.Extra exposing (toFormattedString)
import Material.Icons.Action exposing (check_circle)
import Color exposing(..)

import Model exposing (Model, defaultTypes)
import Types exposing (Rep, ExerciseType, Exercise, Workout)
import Update exposing (..)

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
