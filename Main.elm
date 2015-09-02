
import Effects exposing (Never)
import DecodeJson exposing (init, update, view)
import StartApp
import Task


app =
  StartApp.start
    { init = init "https://live-todo-restful.pantheon.io/api/v1.0/todos"
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
