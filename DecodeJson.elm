module DecodeJson where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import Http
import Json.Decode as Json exposing ((:=))
import String exposing (length)
import Task

import Debug


-- MODEL

type alias Id = Int

type alias Item =
  { id: Id
  , title: String
  }

type alias Model =
  { url : String
  , items : List Item
  , isFetching: Bool
  }

init : String -> (Model, Effects Action)
init url =
  ( Model url [] False
  , Effects.none
  )


-- UPDATE

type Action
  = UpdateUrl String
  | SubmitUrl
  | NewJson (Result Http.Error (List Item))


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateUrl url ->
      ( {model | url <- url}
      , Effects.none
      )

    SubmitUrl ->
      if String.length model.url > 0
        then
          ( {model | isFetching <- True}
          , (getJson model.url)
          )
        else
          (model, Effects.none)

    NewJson result ->
      case result of
        Ok items ->
          ( {model | items <- items, isFetching <- False}
          , Effects.none
          )
        Err msg ->
          -- No change, as there is some error
          ( {model | isFetching <- False}
          , Effects.none
          )


-- VIEW

(=>) = (,)

itemsTableView : List Item -> Html
itemsTableView items =
  let
    th' thLabel = th [] [text thLabel]
    tr' item =
      tr [] [
        td [] [text <| toString item.id]
              , td [] [text <| item.title]
      ]
  in
    div []
    [ table []
      [ thead [] [tr [] (List.map th' ["ID", "Title"])]
      , tbody [] (List.map tr' items)
      ]
    ]

itemsView : List Item -> Bool -> Html
itemsView items isFetching =
  if isFetching == True
    then
      div [] [ text "Loading..."]
    else
      if items == []
        then
          div [] [ text "Click fetch to get a table"]
        else
          itemsTableView items


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h2 [] [text "Show Todo list data from Headless Drupal backend"]
    , input
        [ placeholder "URL with JSON"
        , value model.url
        , on "input" targetValue (Signal.message address << UpdateUrl)
        , size 40
        , required True
        ]
        []
    , button [ onClick address SubmitUrl, disabled (model.isFetching || (String.length model.url == 0)) ] [ text "Fetch" ]
    , a [ href model.url, target "_blank" ] [text "(Open)"]
    , (itemsView model.items model.isFetching)
    ]

-- EFFECTS

getJson : String -> Effects Action
getJson url =
  Http.get decodeUrl url
    |> Task.toResult
    |> Task.map NewJson
    |> Effects.task


decodeUrl : Json.Decoder (List Item)
decodeUrl =
  Json.at ["data"]
  <| Json.list
  <| Json.object2 Item
      ("id" := Json.int)
      ("title" := Json.string)
