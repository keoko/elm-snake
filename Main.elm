import Html.App as App
import Html exposing (Html, text, div)
import Html.Attributes exposing (id, style)


type alias Model = Int

type Msg = None


main =
    App.beginnerProgram { model = model
                        , view = view
                        , update = update
                        }

model : Model
model = 0

update : Msg -> Model -> Model
update msg model =
    model

view : Model -> Html Msg
view model = div [ id "rectangle"
                 , style [("width", "10px")
                         ,("height", "10px")
                         ,("background-color", "blue")
                         ]
                 ] []
