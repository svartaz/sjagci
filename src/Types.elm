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


showKind : Bool -> PieceKind -> Bool -> String
showKind isJa kind isPromoted =
    if isJa then
        case kind of
            P ->
                if isPromoted then
                    "と"

                else
                    "歩"

            L ->
                if isPromoted then
                    "杏"

                else
                    "香"

            N ->
                if isPromoted then
                    "圭"

                else
                    "桂"

            S ->
                if isPromoted then
                    "全"

                else
                    "銀"

            G ->
                "金"

            B ->
                if isPromoted then
                    "馬"

                else
                    "角"

            R ->
                if isPromoted then
                    "龍"

                else
                    "飛"

            K ->
                "王"

    else
        String.append
            (if isPromoted then
                "+"

             else
                ""
            )
            (case kind of
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
            )


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
    { kind : PieceKind, isBlack : Bool, isPromoted : Bool }


showPiece : Bool -> Piece -> String
showPiece isJa { kind, isPromoted } =
    showKind isJa kind isPromoted


type alias Square =
    Maybe Piece


type alias Board =
    List (List Square)


type State
    = Moved
    | Touched Int Int Piece


type Action
    = AMove Piece Int Int Int Int Bool
    | ADrop PieceKind Int Int


type alias Model =
    { turn : Int
    , board : Board
    , capturesW : List PieceKind
    , capturesB : List PieceKind
    , state : State
    , actions : List Action
    , isJa : Bool
    , blackIsReversed : Bool
    }


type Message
    = Touch Int Int Piece
    | Untouch
    | Move Int Int Bool
    | Drop Int Int PieceKind
    | Undo Int
    | CheckboxJa Bool
    | CheckboxBlack Bool


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
