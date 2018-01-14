module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Regex
import Json.Decode as Decode


type alias Skill =
    { id : Int
    , name : String
    , people : List Person
    }


type alias Person =
    { id : Int
    , name : String
    , email : String
    , locations : List Location
    }


type alias Location =
    { id : Int
    , name : String
    }


type alias Model =
    { query : String
    , skills : RemoteData (List Skill)
    , activeItem : Selectable
    }


init : ( Model, Cmd Msg )
init =
    ( { query = ""
      , skills = NotFetched
      , activeItem = NoSelection
      }
    , loadSkills
    )


type RemoteData a
    = NotFetched
    | Fetching
    | Fetched a
    | Error Http.Error


type Selectable
    = SelectSkill Skill
    | SelectPerson Person
    | NoSelection


type Msg
    = NoOp
    | FetchSkills
    | SkillsReceived (Result Http.Error (List Skill))
    | UpdateSearch String
    | Select Selectable


locationDecoder : Decode.Decoder Location
locationDecoder =
    Decode.map2 Location
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)


personDecoder : Decode.Decoder Person
personDecoder =
    Decode.map4 Person
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "location" (Decode.list locationDecoder))


skillDecoder : Decode.Decoder Skill
skillDecoder =
    Decode.map3 Skill
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "people" (Decode.list personDecoder))


decodeSkillPayload : Decode.Decoder (List Skill)
decodeSkillPayload =
    Decode.at [ "data" ] (Decode.list skillDecoder)


loadSkills : Cmd Msg
loadSkills =
    let
        url =
            "https://collaboreighteen-api.herokuapp.com/api/all"

        -- "http://localhost:4000/api/all"
        request =
            Http.get url decodeSkillPayload
    in
        Http.send SkillsReceived request


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FetchSkills ->
            ( { model | skills = Fetching }, loadSkills )

        SkillsReceived (Err error) ->
            ( { model | skills = Error error }, Cmd.none )

        SkillsReceived (Ok skills) ->
            ( { model | skills = Fetched skills }, Cmd.none )

        UpdateSearch query ->
            ( { model | query = query }, Cmd.none )

        Select selectable ->
            ( { model | activeItem = selectable }, Cmd.none )


searchPlaceholder : String
searchPlaceholder =
    "writing"


searchPrompt : String
searchPrompt =
    "Go ahead and search for a skill, you cool baby!"


resultsText : Int -> String
resultsText amountOfResults =
    (toString amountOfResults) ++ " Skills found!"


resultEgg : Int -> String
resultEgg amountOfResults =
    if amountOfResults > 1000 then
        ""
    else if amountOfResults > 99 && amountOfResults /= 420 then
        "Do the damn thing... Uh, thats so many results!"
    else if amountOfResults == 69 then
        "Nice!"
    else if amountOfResults == 420 then
        "The Weed Number!"
    else if amountOfResults == 0 then
        "That dog wont hunt... better search something else"
    else
        ""


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "" ]
        [ header []
            [ h1 [ class "display-3" ] [ text "Collaboreighteen" ]
            , h2 [] [ text " Stronger Together" ]
            , h5 [] [ text searchPrompt ]
            , input [ attribute "role" "search", placeholder searchPlaceholder, type_ "search", onInput UpdateSearch ] []
            ]
        ]


view : Model -> Html Msg
view model =
    case model.skills of
        NotFetched ->
            div [ class "container" ]
                [ viewHeader model
                ]

        Fetching ->
            div [ class "container" ]
                [ viewHeader model
                ]

        Fetched skills ->
            let
                filteredSkills =
                    List.filter (\skill -> Regex.contains (Regex.regex <| ".*" ++ model.query ++ ".*") skill.name) skills

                filteredCount =
                    List.length filteredSkills
            in
                div [ class "container" ]
                    [ viewHeader model
                    , div [ class "row" ] [ h3 [] [ text (resultsText filteredCount) ] ]
                    , div [ class "row" ] [ text <| resultEgg filteredCount ]
                    , div [ class "results" ] (resultView filteredSkills model)
                    ]

        Error error ->
            div [ class "container" ]
                [ viewHeader model
                , div [] [ text <| toString <| error ]
                ]


resultView : List Skill -> Model -> List (Html Msg)
resultView filteredSkills ({ skills } as model) =
    if Fetched filteredSkills == skills then
        []
    else
        (List.map skillView filteredSkills)


skillView : Skill -> Html Msg
skillView skill =
    div [ class "col" ]
        [ div [ class "card text-centered" ]
            [ div [ class "card-body" ]
                [ h2 [ class "card-title" ] [ text skill.name ]
                , div [ class "card-subtitle badge badge-info" ]
                    [ text ((toString <| List.length skill.people) ++ " people") ]
                , ul []
                    (List.map personView skill.people)
                ]
            ]
        ]


personView : Person -> Html Msg
personView person =
    div []
        [ text person.name
        , text "  < -- >  "
        , text person.email
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
