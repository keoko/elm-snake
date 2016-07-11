import Html.App as App
import Html exposing (text)


main =
    App.beginnerProgram { model = model
                        , view = view
                        , update = update
                        }

model = 0

update msg model =
    model

view model = text "Hello World!"
