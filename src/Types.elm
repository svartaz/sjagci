module Types exposing (..)

import List exposing (sortWith)
import ListMisc exposing (..)
import Maybe.Extra


type PieceKind
    = P -- pawn
    | L -- lance
    | N -- knight
    | S -- silver
    | G -- gold
    | B -- bishop
    | R -- rook
    | K -- king


showKind : PieceKind -> String
showKind kind =
    case kind of
        P ->
            "P"

        L ->
            "L"

        N ->
            "N"

        S ->
            "S"

        G ->
            "G"

        B ->
            "B"

        R ->
            "R"

        K ->
            "K"


kindToInt : PieceKind -> Int
kindToInt k =
    case k of
        P ->
            0

        L ->
            1

        N ->
            2

        S ->
            3

        G ->
            4

        B ->
            5

        R ->
            6

        K ->
            7


compareKind : PieceKind -> PieceKind -> Order
compareKind k0 k1 =
    compare (kindToInt k0) (kindToInt k1)


sortKinds =
    sortWith compareKind


type alias Piece =
    { kind : PieceKind, isBlack : Bool, isPromoted : Bool, canPromote : Bool }


showPiece : Piece -> String
showPiece { kind, isPromoted } =
    showKind kind
        |> (if isPromoted then
                String.append "+"

            else
                identity
           )


type alias Square =
    Maybe Piece


type alias Board =
    List (List Square)


type State
    = Moved
    | Touched Int Int Piece


type Action
    = AMove Piece Int Int Bool
    | ADrop PieceKind Int Int


type Model
    = Model Int Board (List PieceKind) (List PieceKind) State (List Action)


type Message
    = Touch Int Int Piece
    | Untouch
    | Move Int Int Bool
    | Drop Int Int PieceKind


type alias Color =
    Maybe Bool


color : Square -> Color
color =
    Maybe.map .isBlack


colorFromIndice : Int -> Int -> Board -> Color
colorFromIndice i j board =
    getAt2 i j board
        |> Maybe.Extra.join
        |> Maybe.map .isBlack
