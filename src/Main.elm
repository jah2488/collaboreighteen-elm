module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Regex
import Json.Decode as Decode
import Json.Decode.Pipeline as P


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
    , skill_notes : String
    , payment_notes : String
    , notes : String
    , paid : Bool
    , skills : List String
    }


type alias Location =
    { id : Int
    , name : String
    }


type alias Model =
    { query : String
    , skills : RemoteData (List Skill)
    , activeItem : Selectable
    , activePerson : Selectable
    }


init : ( Model, Cmd Msg )
init =
    ( { query = ""
      , skills = NotFetched
      , activeItem = NoSelection
      , activePerson = NoSelection
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
    P.decode Person
        |> P.required "id" Decode.int
        |> P.required "name" Decode.string
        |> P.required "email" Decode.string
        |> P.required "location" (Decode.list locationDecoder)
        |> P.required "skill_notes" Decode.string
        |> P.required "payment_notes" Decode.string
        |> P.required "notes" Decode.string
        |> P.required "paid" Decode.bool
        |> P.required "skills" (Decode.list Decode.string)


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
            -- "http://localhost:4000/api/all"
            "https://collaboreighteen-api.herokuapp.com/api/all"

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
            ( { model | query = String.trim <| String.toLower query }, Cmd.none )

        Select selectable ->
            case selectable of
                SelectSkill skill ->
                    if selectable == model.activeItem then
                        ( { model | activeItem = NoSelection }, Cmd.none )
                    else
                        ( { model | activeItem = selectable }, Cmd.none )

                SelectPerson person ->
                    if selectable == model.activePerson then
                        ( { model | activePerson = NoSelection }, Cmd.none )
                    else
                        ( { model | activePerson = selectable }, Cmd.none )

                NoSelection ->
                    ( { model | activeItem = NoSelection, activePerson = NoSelection }, Cmd.none )


searchPlaceholder : String
searchPlaceholder =
    "writing"


searchPrompt : String
searchPrompt =
    "Go ahead and search for a skill, you cool baby!"


loadingMessage : String
loadingMessage =
    "Hold on to your butts. We're loading some sweet skills!"


errorMessage : String
errorMessage =
    "Awww Beans! Something broke. Sorry. Maybe try again?"


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


skillsForQuery : Model -> List Skill -> List Skill
skillsForQuery model skills =
    let
        foundSkills =
            List.filter (\skill -> Regex.contains (Regex.regex <| ".*" ++ model.query ++ ".*") skill.name) skills
    in
        if (List.length foundSkills) > 100 then
            List.take 100 foundSkills
        else
            foundSkills


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
                , div [] [ text loadingMessage ]
                ]

        Fetched skills ->
            let
                filteredSkills =
                    skillsForQuery model skills
                        |> List.sortBy (\skill -> -1 * List.length (skill.people))

                filteredCount =
                    List.length filteredSkills
            in
                div [ class "container" ]
                    [ viewHeader model
                    , div [ class "row" ]
                        [ h3 []
                            [ text (resultsText filteredCount) ]
                        , span
                            []
                            [ text ("  Out of " ++ (toString (List.length skills)) ++ " total skills") ]
                        ]
                    , div [ class "row" ] [ text <| resultEgg filteredCount ]
                    , div [ class "results" ] (resultView model filteredSkills)
                    ]

        Error error ->
            div [ class "container" ]
                [ viewHeader model
                , h4 [] [ text errorMessage ]
                , div [] [ text <| toString <| error ]
                ]


resultView : Model -> List Skill -> List (Html Msg)
resultView ({ skills } as model) filteredSkills =
    if Fetched filteredSkills == skills then
        []
    else
        (List.map (skillView model) filteredSkills)


skillPeople : Skill -> String
skillPeople skill =
    ((toString <| List.length skill.people) ++ " people")


skillView : Model -> Skill -> Html Msg
skillView model skill =
    let
        active =
            (model.activeItem == SelectSkill skill)
    in
        div [ class "col" ]
            [ div
                [ classList
                    [ ( "card text-centered", True )
                    , ( "border border-primary", active )
                    ]
                ]
                [ div [ class "card-body" ]
                    [ h2 [ class "card-title", onClick <| Select (SelectSkill skill) ]
                        [ span [] [ text skill.name ]
                        , span [ class "card-subtitle badge badge-info float-right" ] [ text <| skillPeople skill ]
                        ]
                    , (if active then
                        ul [ class "list-group list-group-flush" ] (List.map (personView model) skill.people)
                       else
                        div [] []
                      )
                    ]
                ]
            ]


cardView : String -> String -> Html Msg
cardView title body =
    div [ class "card", style [ ( "min-height", "15rem" ) ] ]
        [ div [ class "card-body" ]
            [ h3 [ class "card-title" ] [ text title ]
            , p [] [ text body ]
            ]
        ]


personView : Model -> Person -> Html Msg
personView model person =
    let
        fullProfile =
            if (model.activePerson == SelectPerson person) then
                div [ class "person-profile" ]
                    [ strong [ onClick <| Select (SelectPerson person) ] [ text person.name ]
                    , div [ class "row" ]
                        [ div [ class "col" ] [ cardView "Skill Notes" person.notes ]
                        , div [ class "col" ] [ cardView "Payment Notes" (person.skill_notes ++ " " ++ person.payment_notes) ]
                        , div [ class "col" ] [ cardView "Contact Info" person.email ]
                        ]
                    , div [ class "row" ]
                        [ div [ class "col" ]
                            [ div [ class "card" ]
                                [ div [ class "card-body" ]
                                    [ h3 [ class "card-title" ] [ text "Other Skills" ]
                                    , ul [ class "list-group list-group-flush" ] (List.map (\skill -> li [ class "list-group-item" ] [ text skill ]) person.skills)
                                    ]
                                ]
                            ]
                        ]
                    ]
            else
                div [ onClick <| Select (SelectPerson person) ] [ text person.name ]
    in
        li [ class "list-group-item list-group-item-hover" ]
            [ fullProfile
            ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
