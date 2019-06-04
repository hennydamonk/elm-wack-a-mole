module TicTacToe exposing (main)

import Grid exposing (Grid)
import Grid.Position exposing (Position)
import PixelEngine
    exposing
        ( Area
        , Background
        , Input(..)
        , PixelEngine
        , gameWithNoControls
        )
import PixelEngine.Image
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile, Tileset)
import Random exposing (Generator)
import Time
import RAL exposing (..)

type Mole = Rando
            | Mole

type GameState = Pregame
                | Game
                | Postgame


gameBoard : RAList Position
gameBoard = empty |> cons (0,0) |> cons (0,1) |> cons (1,0) 
    |> cons (1,1) |> cons (2,0) |> cons (2,1)

lettertset : Tile.Tileset
lettertset = 
    { source = "Formula.png"
    , spriteWidth = 5
    , spriteHeight = 5
    }


type alias Model =
    { grid : Grid Mole
    , mole : Maybe Position
    , score : Int
    , tleft : Float
    , dif : Maybe Int
    , curTime : (Float, Random.Seed)
    , moleTile : (Int, Int)
    , randomFlag : Maybe (Int, Random.Seed)
    }

type Msg
    = PlaceMole Position
    | NewMole
    | MoleHit
    | Reset


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid =
            Grid.empty
                { columns = 3
                , rows = 2
                }
      , mole = Nothing
      , score = 0
      , tleft = 30
      , dif = Nothing
      , curTime = makeTime (Random.initialSeed 69)
      , moleTile = (0, 0)
      , randomFlag = Just (makeRando (Random.initialSeed 69))
      }
    , newMole gameBoard
    )

newMole : RAList Position -> Cmd Msg
newMole allSquares = 
    Random.generate 
        PlaceMole
        (Random.map
            (\i -> 
                allSquares
                    |> get i
            )
            (Random.int 0 5)
        )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ grid, mole, score, tleft, randomFlag } as model) =
    let
        defaultCase : ( Model, Cmd Msg )
        defaultCase =
            ( model, Cmd.none )
    in
    case msg of
        NewMole ->
            ({ model | tleft = tleft - (Tuple.first model.curTime) }
             , (newMole gameBoard))
        PlaceMole pos ->
            if tleft < 0 then
                --GameOver
                defaultCase
            else
                ({ model | mole = Just pos
                 , grid = grid |> Grid.insert pos Mole}
                 , Cmd.none)
        MoleHit ->
            case randomFlag of
                Nothing ->
                    ({ model | score = score + 1 }
                    -- potentially change image every hit, grid = grid |> Grid.insert (Maybe.withDefault (0,0) mole) Nought}
                     , Cmd.none)
                Just (i, seed0) -> 
                    let (i2, seed1) = makeRando seed0 in
                    ({ model | score = score + 1 
                    , randomFlag = Just (i2, seed1)}
                    -- potentially change image every hit, grid = grid |> Grid.insert (Maybe.withDefault (0,0) mole) Nought}
                     , Cmd.none)
        Reset ->
            init ()

timeProb : Generator Float
timeProb = 
    Random.float 0.4 1.2

makeTime : Random.Seed -> (Float, Random.Seed)
makeTime seed0 = 
    Random.step timeProb seed0

moleProb : Generator Int
moleProb = 
    Random.int 0 4

makeRando : Random.Seed -> (Int, Random.Seed)
makeRando seed0 = 
    Random.step moleProb seed0

subscriptions : Model -> Sub Msg
subscriptions a =
    Time.every (((Tuple.first (a.curTime)) * 1000)/(toFloat (Maybe.withDefault 1 a.dif))) (always NewMole)


moleTile : Model -> Tile Msg
moleTile model = 
    case model.randomFlag of
        Nothing ->
            Tile.fromPosition (0, 0)
                |> Tile.clickable MoleHit
            --random tile
        Just (i, seed0) ->
            Tile.fromPosition (0, i)
                |> Tile.clickable MoleHit
           


none : Position -> Tile Msg
none pos =
    Tile.fromPosition ( 0, 7 )


viewGrid : Grid Mole -> List ( Position, Tile Msg )
viewGrid grid =
    grid
        |> Grid.foldl
            (\(( x, y ) as pos) maybeMole ->
                (::)
                    ( ( 1 + x, 1 + y )
                    , none (x, y)
                    )
            )
            []

tileSize : Int
tileSize =
    16

wordtileset : Tileset
wordtileset = { source = "boxy.png"
                , spriteWidth = 16
                , spriteHeight = 16
                }

width : Float
width =
    toFloat <| 5 * tileSize

{-|
# Areas
-}
areas : Model -> List (Area Msg)
areas ({ grid, mole, score} as model) =
    [ PixelEngine.imageArea
        { background  = 
            PixelEngine.imageBackground
                { height = 30
                , width = 80
                , source = "wack_yeet.jpg"
                }
        , height = 30 }
        []
        , PixelEngine.tiledArea
        { rows = 4
        , tileset =
            { source = "Animal_Pack.png"
            , spriteWidth = tileSize
            , spriteHeight = tileSize
            }
        , background =
            PixelEngine.imageBackground
                { height = 80
                , width = 80
                , source = "grass.png"
                }
        }
        (List.concat
            [ grid |> viewGrid
            , case mole of
                Just ( x, y ) ->
                    [ ( ( x + 1, y + 1 ), moleTile model) ]

                Nothing ->
                    []
            ]
        )
        , PixelEngine.imageArea
        { background  = 
            PixelEngine.imageBackground
                { height = 22.5
                , width = 80
                , source = "pitch-black-image.png"
                }
        , height = 20 }
        [((-17 , 0), PixelEngine.Image.fromText ("SCORE:" ++ String.fromInt score)  wordtileset) ] 
    ]


{------------------------
   CONFIGURATION
------------------------}


view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "Whack a Mole"
    , options = Nothing
    , body = areas model
    }


main : PixelEngine () Model Msg
main =
    gameWithNoControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , width = width
        }
