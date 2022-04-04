module Kind exposing (..)

import List exposing (sortWith)


type PieceKind
    = P -- pawn
    | L -- lance
    | N -- knight
    | S -- silver
    | G -- gold
    | B -- bishop
    | R -- rook
    | K -- king


show : Bool -> PieceKind -> Bool -> String
show isJa kind isPromoted =
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
            (if isPromoted then
                "+"

             else
                ""
            )


toInt : PieceKind -> Int
toInt k =
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


compare : PieceKind -> PieceKind -> Order
compare k0 k1 =
    Basics.compare (toInt k0) (toInt k1)


sort =
    sortWith compare
