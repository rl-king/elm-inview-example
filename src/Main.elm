port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import InView
import Task



-- PORT


port onScroll : (( Float, Float ) -> msg) -> Sub msg



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map InViewMsg <|
            InView.subscriptions model.inView
        , onScroll OnScroll
        ]



-- MODEL


type alias Model =
    { inView : InView.State
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        ( inViewModel, inViewCmds ) =
            InView.init (List.map .id images)
    in
    ( { inView = inViewModel }
    , Cmd.map InViewMsg inViewCmds
    )



-- IMAGE


type alias Image =
    { url : String
    , margin : Int
    , id : String
    }


images : List Image
images =
    List.indexedMap (\index image -> image (String.fromInt index))
        [ Image "images/Ambrym_South_Pacific_Ocean.jpg" 40
        , Image "images/Irrawaddy_Delta_Myanmar.jpg" 10
        , Image "images/Northwest_England.jpg" 30
        , Image "images/Snowbound_Italy.jpg" 55
        , Image "images/Uyuni_salt_flat_Bolivia.jpg" 20
        , Image "images/Ambrym_South_Pacific_Ocean.jpg" 0
        , Image "images/Irrawaddy_Delta_Myanmar.jpg" 40
        , Image "images/Northwest_England.jpg" 20
        , Image "images/Snowbound_Italy.jpg" 55
        , Image "images/Uyuni_salt_flat_Bolivia.jpg" 20
        ]



-- UPDATE


type Msg
    = OnScroll ( Float, Float )
    | InViewMsg InView.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnScroll ( x, y ) ->
            ( { model | inView = InView.updateViewportOffset x y model.inView }
            , Cmd.none
            )

        InViewMsg inViewMsg ->
            let
                ( inView, inViewCmds ) =
                    InView.update inViewMsg model.inView
            in
            ( { model | inView = inView }
            , Cmd.map InViewMsg inViewCmds
            )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ div [] (List.map (item model) images) ]
    }


item : Model -> Image -> Html msg
item model image =
    let
        distance =
            InView.centerDistance image.id model.inView

        ( opacity, scale ) =
            case InView.inView image.id model.inView of
                Just True ->
                    ( "1", "1" )

                _ ->
                    ( "0", "0.95" )
    in
    div
        [ style "margin-bottom" "10rem"
        , style "margin-left" (String.fromInt image.margin ++ "%")
        , style "width" "45%"
        , id image.id
        ]
        [ viewImage image scale opacity
        , viewDistance distance
        ]


viewImage : Image -> String -> String -> Html msg
viewImage image scale opacity =
    img
        [ src image.url
        , style "opacity" opacity
        , style "width" "100%"
        , style "transition" "opacity 1s , transform .5s"
        , style "transform" ("scale(" ++ scale ++ ")")
        ]
        []


viewDistance : Maybe InView.CenterDistance -> Html msg
viewDistance distance =
    case distance of
        Just { x, y } ->
            div [ style "position" "absolute" ]
                [ text (String.fromFloat x)
                , text " / "
                , text (String.fromFloat y)
                ]

        Nothing ->
            text ""
