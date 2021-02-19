module Main exposing (Message(..), main, update, view)

import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List exposing (foldl, indexedMap, map, member, repeat, reverse)
import Maybe exposing (Maybe(..))
import String exposing (toUpper)
import Tuple exposing (pair)


range : Int -> Int -> List Int
range i j =
    case compare i j of
        LT ->
            i :: range (i + 1) j

        EQ ->
            [ i ]

        GT ->
            []


get : Int -> List a -> Maybe a
get i xs =
    List.head (List.drop i xs)


get2d : Int -> Int -> List (List a) -> Maybe a
get2d i j xss =
    case get i xss of
        Just xs ->
            get j xs

        Nothing ->
            Nothing


set : Int -> a -> List a -> List a
set i x =
    indexedMap <|
        \i_ ->
            \x_ ->
                if i == i_ then
                    x

                else
                    x_


set2d : Int -> Int -> a -> List (List a) -> List (List a)
set2d i j x =
    indexedMap <|
        \i_ ->
            indexedMap <|
                \j_ ->
                    \x_ ->
                        if ( i, j ) == ( i_, j_ ) then
                            x

                        else
                            x_


type PieceKind
    = P -- pawn
    | L -- lance
    | N -- knight
    | S -- silver
    | G -- gold
    | B -- bishop
    | R -- rook
    | K -- king


type alias Piece =
    { kind : PieceKind, isBlack : Bool, isPromoted : Bool }


type alias Square =
    Maybe Piece


type alias Board =
    List (List Square)


showPiece : Piece -> String
showPiece { kind, isBlack, isPromoted } =
    (if isPromoted then
        toUpper

     else
        identity
    )
    <|
        case kind of
            P ->
                "p"

            L ->
                "l"

            N ->
                "n"

            S ->
                "s"

            G ->
                "g"

            B ->
                "b"

            R ->
                "r"

            K ->
                "k"


boardEmpty : Board
boardEmpty =
    repeat 9 (repeat 9 Nothing)


boardInitial : Board
boardInitial =
    let
        setPones isBlack board =
            foldl (\j -> set2d 2 j (Just { kind = P, isBlack = isBlack, isPromoted = False })) board (range 0 9)

        setSymmetry j kind isBlack board =
            foldl (\j_ -> set2d 0 j_ (Just { kind = kind, isBlack = isBlack, isPromoted = False })) board [ j, 8 - j ]

        setPlayer isBlack board =
            board
                |> setPones isBlack
                |> set2d 1 1 (Just { kind = B, isBlack = isBlack, isPromoted = False })
                |> set2d 1 7 (Just { kind = R, isBlack = isBlack, isPromoted = False })
                |> (\board_ ->
                        indexedMap pair [ L, N, S, G, K ] |> foldl (\( j, kind ) -> setSymmetry j kind isBlack) board_
                   )
    in
    boardEmpty
        |> setPlayer False
        |> reverse
        |> setPlayer True
        |> reverse


type alias Model =
    ( Int, Board, State )


type State
    = Moved
    | Touched Int Int


type Message
    = Touch Int Int
    | Untouch
    | Move Int Int Bool


showState : State -> String
showState state =
    case state of
        Moved ->
            "moved"

        Touched i j ->
            String.concat [ "touched (", String.fromInt i, ", ", String.fromInt j, ")" ]


init : Model
init =
    ( 0, boardInitial, Moved )


update : Message -> Model -> Model
update message ( turn, board, state ) =
    case ( message, state ) of
        ( Touch i j, Moved ) ->
            ( turn, board, Touched i j )

        ( Untouch, Touched i j ) ->
            ( turn, board, Moved )

        ( Move i_ j_ isPromoted, Touched i j ) ->
            ( turn + 1
            , updateBoard i j i_ j_ isPromoted board
            , Moved
            )

        otherwise ->
            Debug.log "error!" ( turn, board, state )



-- FIXME


updateBoard : Int -> Int -> Int -> Int -> Bool -> Board -> Board
updateBoard i j i_ j_ isPromoted board =
    case get2d i j board of
        Just (Just piece) ->
            board
                |> set2d i j Nothing
                |> set2d i_ j_ (Just { piece | isPromoted = isPromoted })

        Just Nothing ->
            Debug.log "error: move nothing" board

        Nothing ->
            Debug.log "error: move outside" board



-- FIXME


reachables : Int -> Int -> Board -> List ( Int, Int )
reachables i j board =
    [ ( i + 1, j ) ]


view ( turn, board, state ) =
    let
        styles =
            [ style "border" "1px solid"
            , style "width" "64px"
            , style "height" "64px"
            , style "text-align" "center"
            , style "vertical-align" "middle"
            ]

        squareToTd i_ j_ square =
            let
                attributes =
                    case state of
                        Moved ->
                            (onClick <| Touch i_ j_) :: styles

                        Touched i j ->
                            if member ( i_, j_ ) (reachables i j board) then
                                style "background-color" "#444" :: styles

                            else
                                onClick Untouch :: styles
            in
            case square of
                Nothing ->
                    td attributes []

                Just piece ->
                    td
                        (if .isBlack piece then
                            style "transform" "rotate(180deg)" :: attributes

                         else
                            attributes
                        )
                        [ piece |> showPiece |> text ]
    in
    div
        [ style "font-family" "Noto Sans"
        , style "font-weight" "lighter"
        , style "color" "#888"
        , style "background-color" "#000"
        ]
        [ div []
            [ text <| String.concat [ "turn ", String.fromInt turn ]
            , div []
                [ text <| showState state ]
            ]
        , table
            [ class "board"
            , style "border-collapse" "collapse"
            , style "border" "1px solid"
            ]
            (board
                |> indexedMap
                    (\i ->
                        \squares ->
                            tr []
                                (indexedMap (squareToTd i) squares)
                    )
                |> reverse
            )
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }



{- todo
   - drop
-}
