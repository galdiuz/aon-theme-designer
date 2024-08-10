module AonThemeDesigner exposing (main)

import Browser
import Dict exposing (Dict)
import Float.Extra
import Hex
import Html exposing (Html)
import Html.Attributes as HA
import Html.Attributes.Extra as HAE
import Html.Events as HE
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Regex exposing (Regex)


type alias Model =
    { colors : Dict String String
    , colorInputs : Dict String String
    , scheme : String
    , shareInput : String
    }


type Msg
    = ColorChanged String String
    | ColorInputChanged String String
    | SchemeChanged String
    | ShareInputChanged String


type alias ColorDef =
    { key : String
    , label : String
    , default : String
    }


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : Decode.Value -> ( Model, Cmd Msg )
init flagsValue =
    ( { colors = Dict.empty
      , colorInputs = Dict.empty
      , scheme = "dark"
      , shareInput = ""
      }
    , Cmd.none
    )
        |> updateInput


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ColorChanged key value ->
            ( { model
                | colors = Dict.insert key value model.colors
                , colorInputs = Dict.remove key model.colorInputs
              }
            , Cmd.none
            )
                |> updateInput

        ColorInputChanged key rawValue ->
            let
                value : String
                value =
                    rawValue
                        |> String.filter Char.isHexDigit
                        |> (++) "#"
                        |> String.left 7
            in
            if isValidHex value then
                ( { model
                    | colors = Dict.insert key (String.toLower value) model.colors
                    , colorInputs = Dict.insert key value model.colors
                  }
                , Cmd.none
                )
                    |> updateInput

            else
                ( { model | colorInputs = Dict.insert key value model.colorInputs }
                , Cmd.none
                )

        SchemeChanged value ->
            ( { model | scheme = value }
            , Cmd.none
            )

        ShareInputChanged value ->
            case Decode.decodeString colorsDecoder value of
                Ok ( inputColors, scheme ) ->
                    ( { model
                        | colors = inputColors
                        , scheme = scheme
                        , shareInput = value
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | shareInput = value }
                    , Cmd.none
                    )


updateInput : ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateInput ( model, cmd ) =
    ( { model
        | shareInput =
            Encode.encode 0 (encodeColors model)
      }
    , cmd
    )


view : Model -> Html Msg
view model =
    Html.div
        [ HA.class "column"
        , HA.class "align-center"
        ]
        [ Html.node "style"
            []
            [ Html.text css ]
        , Html.node "style"
            []
            [ Html.text (previewCss model) ]
        , Html.div
            [ HA.class "row"
            , HA.class "gap-small"
            , HA.class "wrap"
            , HA.style "max-width" "1000px"
            ]
            [ Html.div
                [ HA.class "column"
                , HA.class "gap-large"
                , HA.class "flex1"
                ]
                [ viewInputs model
                , viewContrast model
                , viewShare model
                ]
            , viewPreview
            ]
        ]


viewInputs : Model -> Html Msg
viewInputs model =
    Html.div
        [ HA.class "column"
        , HA.class "gap-small"
        ]
        (List.append
            (List.map
                (viewColorInput model)
                colors
            )
            [ Html.div
                [ HA.class "row"
                , HA.class "gap-medium"
                , HA.class "align-center"
                ]
                [ Html.text "Color Scheme:"
                , Html.label
                    []
                    [ Html.input
                        [ HA.type_ "radio"
                        , HA.checked True
                        , HA.name "color-scheme"
                        , HA.checked (model.scheme == "light")
                        , HE.onClick (SchemeChanged "light")
                        ]
                        []
                    , Html.text "Light"
                    ]
                , Html.label
                    []
                    [ Html.input
                        [ HA.type_ "radio"
                        , HA.name "color-scheme"
                        , HA.checked (model.scheme == "dark")
                        , HE.onClick (SchemeChanged "dark")
                        ]
                        []
                    , Html.text "Dark"
                    ]
                ]
            ]
        )


viewColorInput : Model -> ColorDef -> Html Msg
viewColorInput model color =
    Html.div
        [ HA.class "row"
        , HA.class "align-center"
        , HA.class "gap-small"
        ]
        [ Html.input
            [ HA.type_ "color"
            , HA.value (getHex model color)
            , HE.onInput (ColorChanged color.key)
            ]
            []
        , Html.div
            [ HA.class "input-container"
            , HA.style "width" "60px"
            ]
            [ Html.input
                [ HA.type_ "text"
                , HA.pattern "#[0-9a-fA-F]{6}$"
                , HA.value
                    (Dict.get color.key model.colorInputs
                        |> Maybe.withDefault (getHex model color)
                    )
                , HA.style "font-family" "monospace"
                , HE.onInput (ColorInputChanged color.key)
                ]
                []
            ]
        , Html.text color.label
        ]


viewContrast : Model -> Html msg
viewContrast model =
    let
        getColor : String -> ColorDef
        getColor key =
            List.Extra.find (.key >> (==) key) colors
                |> Maybe.withDefault
                    { key = ""
                    , label = ""
                    , default = "#000000"
                    }

        lowContrastPairs : List ( ColorDef, ColorDef )
        lowContrastPairs =
            List.filterMap
                (\( a, b ) ->
                    if contrast (getHex model (getColor a)) (getHex model (getColor b)) < 3 then
                        Just ( getColor a, getColor b )

                    else
                        Nothing
                )
                contrastPairs
    in
    if List.isEmpty lowContrastPairs then
        Html.text ""

    else
        Html.div
            [ HA.class "column"
            , HA.class "gap-medium"
            ]
            (List.append
                [ Html.div
                    [ HA.style "font-size" "20px" ]
                    [ Html.text "Warning: Low contrast" ]
                , Html.div
                    []
                    [ Html.text "The following pairs have low contrast and might be difficult to read:" ]
                ]
                (List.map
                    (\( a, b ) ->
                        Html.div
                            [ HA.class "row"
                            , HA.class "gap-small"
                            ]
                            [ Html.div
                                []
                                [ Html.div
                                    [ HA.style "width" "20px"
                                    , HA.style "height" "20px"
                                    , HA.style "background-color" (getHex model a)
                                    , HA.style "border" "1px solid white"
                                    , HA.style "border-bottom" "none"
                                    ]
                                    []
                                , Html.div
                                    [ HA.style "width" "20px"
                                    , HA.style "height" "20px"
                                    , HA.style "background-color" (getHex model b)
                                    , HA.style "border" "1px solid white"
                                    , HA.style "border-top" "none"
                                    ]
                                    []
                                ]
                            , Html.div
                                [ HA.class "column"
                                , HA.class "gap-small"
                                ]
                                [ Html.div
                                    []
                                    [ Html.text a.label ]
                                , Html.div
                                    []
                                    [ Html.text b.label ]
                                ]
                            ]
                    )
                    lowContrastPairs
                )
            )


contrastPairs : List ( String, String )
contrastPairs =
    [ ( "bg-main", "text-1" )
    , ( "bg-main", "ext-link" )
    , ( "bg-0", "text-1" )
    , ( "head-bg", "head-fg" )
    , ( "mid-bg", "mid-fg" )
    , ( "sub-bg", "sub-fg" )
    , ( "header4-bg", "header4-fg" )
    , ( "header5-bg", "header5-fg" )
    , ( "header6-bg", "header6-fg" )
    , ( "border-1", "text-1" )
    , ( "mid-bg", "text-1" )
    , ( "sub-bg", "text-1" )
    , ( "bg-1", "text-1" )
    , ( "bg-1", "text-2" )
    , ( "bg-2", "text-2" )
    , ( "hovered-bg", "hovered-fg" )
    ]


viewShare : Model -> Html Msg
viewShare model =
    Html.div
        [ HA.class "column"
        ]
        [ Html.text "Share:"
        , Html.div
            [ HA.class "input-container"
            , HA.class "row"
            ]
            [ Html.input
                [ HA.type_ "text"
                , HA.value model.shareInput
                , HE.onInput ShareInputChanged
                ]
                []
            ]
        ]


viewPreview : Html msg
viewPreview =
    Html.div
        [ HA.class "preview"
        , HA.class "column"
        , HA.class "gap-medium"
        , HA.class "flex1"
        ]
        [ Html.div
            [ HA.class "site-header"
            , HA.class "row"
            , HA.style "justify-content" "space-between"
            ]
            [ Html.text "Archives of Nethys"
            , Html.div
                [ HA.style "width" "100px"
                , HA.style "background-color" "var(--border-2)"
                ]
                []
            ]
        , Html.h1
            []
            [ Html.text "Header 1" ]
        , Html.div
            []
            [ Html.span
                [ HA.style "font-weight" "700" ]
                [ Html.text "Source" ]
            , Html.text " "
            , Html.span
                [ HA.class "link" ]
                [ Html.text "Archives of Nethys" ]
            ]
        , Html.text
            """
            Pathfinder is a fantasy tabletop roleplaying game (RPG) where you and a group
            of friends gather to tell a tale of brave heroes and cunning villains in a world filled with
            terrifying monsters and amazing treasures. More importantly, Pathfinder is a game where your
            character's choices determine how the story unfolds.
            """
        , Html.h2
            []
            [ Html.text "Header 2" ]
        , Html.h3
            []
            [ Html.text "Header 3" ]
        , Html.h4
            []
            [ Html.text "Header 4" ]
        , Html.h5
            []
            [ Html.text "Header 5" ]
        , Html.h6
            []
            [ Html.text "Header 6" ]
        , Html.div
            [ HA.class "box"
            , HA.class "column"
            , HA.class "gap-small"
            ]
            [ Html.div
                [ HA.style "font-size" "24px"
                , HA.class "title"
                ]
                [ Html.text "The First Rule"
                ]
            , Html.text
                """
                The first rule of Pathfinder is that this game is yours. Use it to tell the stories you want to
                tell, be the character you want to be, and share exciting adventures with friends. If any other rule
                gets in the way of your fun, as long as your group agrees, you can alter or ignore it to fit your
                story. The true goal of Pathfinder is for everyone to enjoy themselves.
                """
            , Html.div
                [ HA.class "box-inner"
                , HA.class "inactive"
                ]
                [ Html.text "Inner box, used in search" ]
            ]
        , Html.div
            [ HA.class "row"
            , HA.class "align-start"
            ]
            [ Html.div
                [ HA.class "trait"
                , HA.style "background-color" "#c45500"
                ]
                [ Html.text "Uncommon" ]
            , Html.div
                [ HA.class "trait"
                , HA.style "background-color" "#0c1466"
                ]
                [ Html.text "Rare" ]
            , Html.div
                [ HA.class "trait"
                , HA.style "background-color" "#800080"
                ]
                [ Html.text "Unique" ]
            , Html.div
                [ HA.class "trait"
                , HA.style "background-color" "#4287f5"
                ]
                [ Html.text "N" ]
            , Html.div
                [ HA.class "trait"
                , HA.style "background-color" "#478c42"
                ]
                [ Html.text "Medium" ]
            , Html.div
                [ HA.class "trait"
                ]
                [ Html.text "Common" ]
            ]
        , Html.table
            []
            [ Html.tr
                []
                [ Html.th
                    []
                    [ Html.text "Table Header" ]
                , Html.th
                    []
                    [ Html.text "Table Header" ]
                ]
            , Html.tr
                []
                [ Html.td
                    []
                    [ Html.text "Table Cell" ]
                , Html.td
                    []
                    [ Html.text "Table Cell" ]
                ]
            , Html.tr
                []
                [ Html.td
                    []
                    [ Html.text "Table Cell" ]
                , Html.td
                    []
                    [ Html.text "Table Cell" ]
                ]
            , Html.tr
                []
                [ Html.td
                    []
                    [ Html.text "Table Cell" ]
                , Html.td
                    []
                    [ Html.text "Table Cell" ]
                ]
            , Html.tr
                []
                [ Html.td
                    []
                    [ Html.text "Table Cell" ]
                , Html.td
                    []
                    [ Html.text "Table Cell" ]
                ]
            ]
        , Html.div
            []
            [ Html.div
                [ HA.class "menu-item"
                , HA.style "background-color" "var(--head-bg)"
                , HA.style "color" "var(--head-fg)"
                ]
                [ Html.text "Menu 1" ]
            , Html.div
                [ HA.class "menu-item"
                , HA.style "background-color" "var(--mid-bg)"
                ]
                [ Html.text "Menu 2" ]
            , Html.div
                [ HA.class "menu-item"
                , HA.style "background-color" "var(--sub-bg)"
                ]
                [ Html.text "Menu 3" ]
            , Html.div
                [ HA.class "menu-item"
                , HA.style "background-color" "var(--bg-1)"
                ]
                [ Html.text "Menu 4" ]
            ]
        , Html.div
            [ HA.class "row"
            , HA.class "gap-small"
            ]
            [ Html.div
                [ HA.class "hover"
                , HA.class "flex1"
                ]
                [ Html.text "Hover me!"
                ]
            , Html.div
                [ HA.class "hover"
                , HA.class "hovered"
                , HA.class "flex1"
                ]
                [ Html.text "Always hovered"
                ]
            ]
        , Html.div
            [ HA.class "row"
            , HA.class "gap-medium"
            ]
            [ Html.label
                []
                [ Html.input
                    [ HA.type_ "radio"
                    , HA.checked True
                    , HA.name "radio"
                    ]
                    []
                , Html.text "Radio 1"
                ]
            , Html.label
                []
                [ Html.input
                    [ HA.type_ "radio"
                    , HA.name "radio"
                    ]
                    []
                , Html.text "Radio 2"
                ]
            , Html.label
                []
                [ Html.input
                    [ HA.type_ "checkbox"
                    , HA.checked True
                    ]
                    []
                , Html.text "Checkbox 1"
                ]
            , Html.label
                []
                [ Html.input
                    [ HA.type_ "checkbox"
                    ]
                    []
                , Html.text "Checkbox 2"
                ]
            ]
        , Html.div
            [ HA.class "row"
            , HA.class "gap-medium"
            ]
            [ Html.button
                []
                [ Html.text "Button" ]
            , Html.button
                [ HA.class "active" ]
                [ Html.text "Active button" ]
            ]
        , Html.div
            [ HA.class "input-container"
            , HA.class "row"
            ]
            [ Html.input
                [ HA.type_ "text"
                , HA.placeholder "Text input"
                ]
                []
            ]
        ]


colors : List ColorDef
colors =
    [ { key = "bg-main"
      , label = "Primary Background"
      , default = "#0f0f0f"
      }
    , { key = "text-1"
      , label = "Primary Text / Border"
      , default = "#ffffff"
      }
    , { key = "bg-0"
      , label = "Site Header Background"
      , default = "#292929"
      }
    , { key = "border-2"
      , label = "Site Header Background Alt."
      , default = "#303030"
      }
    , { key = "ext-link"
      , label = "Highlighted Link"
      , default = "#00ffff"
      }
    , { key = "head-bg"
      , label = "Header 1 Background / Table Header / Menu 1"
      , default = "#522e2c"
      }
    , { key = "head-fg"
      , label = "Header 1 Text / Menu 1"
      , default = "#cbc18f"
      }
    , { key = "mid-bg"
      , label = "Header 2 Background / Menu 2"
      , default = "#806e45"
      }
    , { key = "mid-fg"
      , label = "Header 2 Text"
      , default = "#0f0f0f"
      }
    , { key = "sub-bg"
      , label = "Header 3 Background / Menu 3"
      , default = "#627d62"
      }
    , { key = "sub-fg"
      , label = "Header 3 Text"
      , default = "#0f0f0f"
      }
    , { key = "header4-bg"
      , label = "Header 4 Background"
      , default = "#4a8091"
      }
    , { key = "header4-fg"
      , label = "Header 4 Text"
      , default = "#0f0f0f"
      }
    , { key = "header5-bg"
      , label = "Header 5 Background"
      , default = "#494e70"
      }
    , { key = "header5-fg"
      , label = "Header 5 Text"
      , default = "#0f0f0f"
      }
    , { key = "header6-bg"
      , label = "Header 6 Background"
      , default = "#623a6e"
      }
    , { key = "header6-fg"
      , label = "Header 6 Text"
      , default = "#0f0f0f"
      }
    , { key = "border-1"
      , label = "Box Background"
      , default = "#303030"
      }
    , { key = "bg-1"
      , label = "Table Background / Menu 4"
      , default = "#64542f"
      }
    , { key = "bg-2"
      , label = "Table Background Alt."
      , default = "#342c19"
      }
    , { key = "text-2"
      , label = "Table Text / Border"
      , default = "#ffffff"
      }
    , { key = "hovered-bg"
      , label = "Hover Background"
      , default = "#cbc18f"
      }
    , { key = "hovered-fg"
      , label = "Hover Text"
      , default = "#000000"
      }
    ]


getHex : Model -> ColorDef -> String
getHex model color =
    Dict.get color.key model.colors
        |> Maybe.withDefault color.default


isValidHex : String -> Bool
isValidHex string =
    let
        hexRegex : Regex
        hexRegex =
            Regex.fromString "^#[0-9a-fA-F]{6}$"
                |> Maybe.withDefault Regex.never
    in
    Regex.contains hexRegex string


encodeColors : Model -> Encode.Value
encodeColors model =
    Encode.object
        (List.append
            (List.map
                (\color ->
                    ( color.key, Encode.string (getHex model color) )
                )
                colors
            )
            [ ( "scheme", Encode.string model.scheme )
            ]
        )


colorsDecoder : Decode.Decoder ( Dict String String, String )
colorsDecoder =
    Decode.dict Decode.string
        |> Decode.andThen
            (\dict ->
                case Dict.get "scheme" dict of
                    Just scheme ->
                        Decode.succeed
                            ( Dict.remove "scheme" dict
                            , scheme
                            )

                    Nothing ->
                        Decode.fail "Missing scheme"
            )


previewCss : Model -> String
previewCss model =
    String.join
        ""
        [ ".preview {\n"
        , String.join
            "\n"
            (List.append
                (List.map
                    (\color ->
                        "--" ++ color.key ++ ": " ++ getHex model color ++ ";"
                    )
                    colors
                )
                [ "color-scheme: " ++ model.scheme ++ ";" ]
            )
        , "\n}"
        ]


hexToRgb : String -> ( Int, Int, Int )
hexToRgb hex =
    ( String.slice 1 3 hex
        |> Hex.fromString
        |> Result.withDefault 0
    , String.slice 3 5 hex
        |> Hex.fromString
        |> Result.withDefault 0
    , String.slice 5 7 hex
        |> Hex.fromString
        |> Result.withDefault 0
    )


luminance : ( Int, Int, Int ) -> Float
luminance ( r, g, b ) =
    let
        f : Float -> Float
        f v =
            if v <= 0.03928 then
                v / 12.92

            else
                ((v + 0.055) / 1.055) ^ 2.4
    in
    List.sum
        [ 0.2126 * f (toFloat r / 255)
        , 0.7152 * f (toFloat g / 255)
        , 0.0722 * f (toFloat b / 255)
        ]


contrast : String -> String -> Float
contrast a b =
    let
        lumA : Float
        lumA =
            luminance (hexToRgb a)

        lumB : Float
        lumB =
            luminance (hexToRgb b)
    in
    ((max lumA lumB) + 0.05) / ((min lumA lumB) + 0.05)


css : String
css =
    """
    :root, :host {
        background-color: var(--bg-0, #111111);
        color: var(--text-1, #eeeeee);
        font-family: "Century Gothic", CenturyGothic, AppleGothic, sans-serif;
        margin: 8px;
        color-scheme: var(--color-scheme, dark);
    }

    @font-face {
        font-family: "fairydustbregular";
        src:
            url('/Fonts/fairydustb-webfont.woff'),
            url('https://fonts.cdnfonts.com/s/8400/FairyDustB.woff');
    }

    .column {
        display: flex;
        flex-direction: column;
    }

    .row {
        display: flex;
        flex-direction: row;
    }

    .align-start {
        align-items: flex-start;
    }

    .align-center {
        align-items: center;
    }

    .gap-small {
        gap: 4px;
    }

    .gap-medium {
        gap: 8px;
    }

    .gap-large {
        gap: 16px;
    }

    .flex1 {
        flex: 1 1 0%;
    }

    .wrap {
        flex-wrap: wrap;
    }

    .preview {
        padding: 8px;
        border: 1px solid white;
        background-color: var(--bg-main);
        color: var(--text-1);
    }

    .title {
        font-variant: small-caps;
        font-weight: 700;
    }

    .site-header {
        background-color: var(--bg-0);
        color: var(--text-1);
        font-size: 42px;
        font-family: "fairydustbregular";
        overflow: hidden;
        padding-left: 4px;
    }

    h1, h2, h3, .h3, h4, .h4, h5, h6 {
        font-variant: small-caps;
        font-weight: 700;
        margin: 0;
        border-radius: 3px;
        padding: 4px 9px;
    }

    h1 {
        background-color: var(--head-bg);
        color: var(--head-fg);
        font-size: 24px;
    }

    h2 {
        background-color: var(--mid-bg);
        color: var(--mid-fg);
        font-size: 20px;
    }

    h3 {
        background-color: var(--sub-bg);
        color: var(--sub-fg);
        font-size: 18px;
    }

    h4 {
        background-color: var(--header4-bg);
        color: var(--header4-fg);
        font-size: 18px;
    }

    h5 {
        background-color: var(--header5-bg);
        color: var(--header5-fg);
        font-size: 18px;
    }

    h6 {
        background-color: var(--header6-bg);
        color: var(--header6-fg);
        font-size: 18px;
    }

    .box {
        background-color: var(--border-1);
        border: solid 1px var(--text-1);
        padding: 8px;
    }

    .box-inner {
        background-color: color-mix(in lch, 85% var(--border-1), black);
        border: 1px solid var(--text-1);
        padding: 8px;
    }

    .link {
        color: var(--ext-link);
        font-style: italic;
    }

    .link:hover {
        cursor: pointer;
        text-decoration: underline;
    }

    .inactive {
        color: color-mix(in lch, var(--text-1), #808080);
    }

    .trait {
        background-color: #531004;
        border: 2px double #d5c489;
        padding: 3px 5px;
        font-variant: small-caps;
        font-weight: 700;
        color: white;
    }

    table {
        background-color: var(--bg-1);
        color: var(--text-2);
        border-collapse: collapse;
    }

    th {
        background-color: var(--head-bg);
        color: var(--head-fg);
    }

    table tr:nth-child(odd) td {
        background-color: var(--bg-2);
    }

    td, th {
        border: solid 1px var(--text-2);
        padding: 4px 12px 4px 4px;
    }

    .hover {
        background-color: var(--bg-main);
        color: var(--text-1);
        border: 1px solid var(--text-1);
        padding: 8px;
    }

    .hover:hover, .hovered {
        background-color: var(--hovered-bg);
        color: var(--hovered-fg);
    }

    button {
        border: 1px solid color-mix(in lch, var(--text-1), #808080);
        border-radius: 4px;
        background-color: transparent;
        color: var(--text-1);
        font-size: 16px;
        padding: 1px 6px;
        font-family: inherit;
    }

    button:hover:enabled {
        border-color: var(--text-1);
        text-decoration: underline;
    }

    button.active {
        background-color: var(--text-1);
        color: var(--bg-1);
    }

    .input-container {
        background-color: var(--color-bg);
        border-style: solid;
        border-radius: 4px;
        border-width: 2px;
        border-color: #808080;
    }

    .input-container:focus-within {
        border-color: var(--color-text);
        outline: 0;
    }

    .input-container:has(input:invalid) {
        border-color: red;
    }

    .menu-item {
        font-size: 20px;
        font-variant: small-caps;
        font-weight: 700;
        padding: 8px;
    }

    input[type=text], input[type=number], input[type=date] {
        background-color: transparent;
        border-width: 0;
        color: var(--text-1);
        flex-grow: 1;
    }

    input:focus-visible {
        border-width: 0;
        border-style: none;
        border-image: none;
        outline: 0;
    }
    """
