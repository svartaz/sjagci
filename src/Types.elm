module Types exposing (..)

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


type alias Piece =
    { kind : PieceKind, isBlack : Bool, isPromoted : Bool }


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
    | Touched Int Int


type Action
    = AMove Piece Int Int Bool
    | ADrop Piece Int Int


type Model
    = Model Int Board State (List Action)


type Message
    = Touch Int Int
    | Untouch
    | Move Int Int Bool


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
