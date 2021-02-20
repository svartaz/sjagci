module Main exposing (Message(..), main, update, view)

import Arithmetic exposing (isOdd)
import Browser
import Html exposing (div, table, td, text, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List exposing (concat, concatMap, filter, foldl, indexedMap, map, member, repeat, reverse)
import List.Extra exposing (getAt)
import Maybe exposing (Maybe(..), withDefault)
import Maybe.Extra
import String exposing (fromInt, toUpper)
import Tuple exposing (pair)


reverse2 : List (List a) -> List (List a)
reverse2 =
    map reverse >> reverse



-- FIXME


iota : Int -> List Int
iota n =
    let
        f n_ i =
            if n_ <= i then
                []

            else
                case n_ of
                    0 ->
                        []

                    n__ ->
                        i :: f n__ (i + 1)
    in
    f n 0


range : Int -> Int -> List Int
range m n =
    iota n |> filter ((<=) m)


product : List a -> List a -> List ( a, a )
product xs ys =
    concatMap
        (\x -> map (\y -> ( x, y )) ys)
        xs


setAt : Int -> a -> List a -> List a
setAt i x xs =
    xs
        |> indexedMap
            (\i_ ->
                \x_ ->
                    if i == i_ then
                        x

                    else
                        x_
            )


getAt2 : Int -> Int -> List (List a) -> Maybe a
getAt2 i j xss =
    getAt i xss |> Maybe.map (getAt j) |> Maybe.Extra.join


setAt2 : Int -> Int -> a -> List (List a) -> List (List a)
setAt2 i j x xss =
    case getAt i xss of
        Just xs ->
            setAt i (setAt j x xs) xss

        Nothing ->
            xss


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
showPiece { kind, isPromoted } =
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
    repeat 9 <| repeat 9 Nothing


boardInitial : Board
boardInitial =
    let
        setPawns isBlack board =
            iota 9 |> foldl (\j -> setAt2 2 j <| Just { kind = P, isBlack = isBlack, isPromoted = False }) board

        setSymmetry j kind isBlack board =
            foldl (\j_ -> setAt2 0 j_ <| Just { kind = kind, isBlack = isBlack, isPromoted = False }) board <| [ j, 8 - j ]

        setPlayer isBlack board =
            board
                |> setPawns isBlack
                |> setAt2 1 1 (Just { kind = B, isBlack = isBlack, isPromoted = False })
                |> setAt2 1 7 (Just { kind = R, isBlack = isBlack, isPromoted = False })
                |> (\board_ ->
                        indexedMap pair [ L, N, S, G, K ] |> foldl (\( j, kind ) -> setSymmetry j kind isBlack) board_
                   )
    in
    boardEmpty
        |> setPlayer False
        |> reverse2
        |> setPlayer True
        |> reverse2


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

        _ ->
            let
                _ =
                    Debug.log "error: can not update" ""
            in
            ( turn, board, state )



-- FIXME


updateBoard : Int -> Int -> Int -> Int -> Bool -> Board -> Board
updateBoard i j i_ j_ isPromoted board =
    case getAt2 i j board of
        Just (Just piece) ->
            board
                |> setAt2 i j Nothing
                |> setAt2 i_ j_ (Just { piece | isPromoted = isPromoted })

        _ ->
            let
                _ =
                    Debug.log "error: can not move" ""
            in
            board


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


view : Model -> Html.Html Message
view ( turn, board, state ) =
    let
        isBlackTurn =
            isOdd turn

        squareToTd iFocus jFocus square_ =
            let
                styles =
                    [ style "border" "1px solid"
                    , style "width" "64px"
                    , style "height" "64px"
                    , style "text-align" "center"
                    , style "vertical-align" "middle"
                    , style "color" "#888"
                    ]

                attributes =
                    case ( state, square_ ) of
                        ( Moved, Nothing ) ->
                            style "background-color" "#888" :: styles

                        ( Moved, Just piece ) ->
                            let
                                -- FIXME
                                canMove =
                                    isBlackTurn
                                        == .isBlack piece
                            in
                            if canMove then
                                (style "background-color" <|
                                    case color square_ of
                                        Nothing ->
                                            "#888"

                                        Just False ->
                                            "#FFF"

                                        Just True ->
                                            "#000"
                                )
                                    :: (onClick <| Touch iFocus jFocus)
                                    :: styles

                            else
                                (style "background-color" <|
                                    case color square_ of
                                        Nothing ->
                                            "#888"

                                        Just False ->
                                            "#FFF"

                                        Just True ->
                                            "#000"
                                )
                                    :: styles

                        ( Touched iTouch jTouch, _ ) ->
                            let
                                possibleMoves :
                                    Int
                                    -> Int
                                    -> (Int -> Int -> ( Int, Int ))
                                    -> List ( Int, Int )
                                possibleMoves i j f =
                                    let
                                        ( i_, j_ ) =
                                            f i j
                                    in
                                    if List.any (\n -> n < 0 || 9 <= n) [ i_, j_ ] then
                                        []

                                    else
                                        case colorFromIndice i_ j_ board of
                                            Nothing ->
                                                ( i_, j_ ) :: possibleMoves i_ j_ f

                                            Just isBlack ->
                                                if isBlack == isBlackTurn then
                                                    []

                                                else
                                                    [ ( i_, j_ ) ]

                                canReach =
                                    getAt2 iTouch jTouch board
                                        |> Maybe.map
                                            (Maybe.map
                                                (\piece ->
                                                    let
                                                        move n it =
                                                            if isBlackTurn then
                                                                it - n

                                                            else
                                                                it + n

                                                        mapMove i j =
                                                            map (\( di, dj ) -> ( move di i, move dj j ))

                                                        { kind, isPromoted } =
                                                            piece
                                                    in
                                                    -- FIXME: refactor
                                                    case ( kind, isPromoted ) of
                                                        ( P, False ) ->
                                                            [ ( 1, 0 ) ]
                                                                |> mapMove iTouch jTouch
                                                                |> filter (\( i, j ) -> colorFromIndice i j board /= Just isBlackTurn)
                                                                |> member ( iFocus, jFocus )

                                                        ( N, False ) ->
                                                            [ ( 2, -1 ), ( 2, 1 ) ]
                                                                |> mapMove iTouch jTouch
                                                                |> filter (\( i, j ) -> colorFromIndice i j board /= Just isBlackTurn)
                                                                |> member ( iFocus, jFocus )

                                                        ( S, False ) ->
                                                            [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ), ( -1, -1 ), ( -1, 1 ) ]
                                                                |> mapMove iTouch jTouch
                                                                |> filter (\( i, j ) -> colorFromIndice i j board /= Just isBlackTurn)
                                                                |> member ( iFocus, jFocus )

                                                        ( G, False ) ->
                                                            [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ), ( 0, -1 ), ( 0, 1 ), ( -1, 0 ) ]
                                                                |> mapMove iTouch jTouch
                                                                |> filter (\( i, j ) -> colorFromIndice i j board /= Just isBlackTurn)
                                                                |> member ( iFocus, jFocus )

                                                        ( K, False ) ->
                                                            product [ -1, 0, 1 ] [ -1, 0, 1 ]
                                                                |> mapMove iTouch jTouch
                                                                |> filter (\( i, j ) -> colorFromIndice i j board /= Just isBlackTurn)
                                                                |> member ( iFocus, jFocus )

                                                        ( L, False ) ->
                                                            possibleMoves iTouch jTouch (\i -> \j -> ( move 1 i, j ))
                                                                |> member ( iFocus, jFocus )

                                                        ( B, False ) ->
                                                            product [ -1, 1 ] [ -1, 1 ]
                                                                |> map (\( di, dj ) -> possibleMoves iTouch jTouch (\i -> \j -> ( move di i, move dj j )))
                                                                |> concat
                                                                |> member ( iFocus, jFocus )

                                                        ( R, False ) ->
                                                            [ ( -1, 0 ), ( 0, -1 ), ( 0, 1 ), ( 1, 0 ) ]
                                                                |> map (\( di, dj ) -> possibleMoves iTouch jTouch (\i -> \j -> ( move di i, move dj j )))
                                                                |> concat
                                                                |> member ( iFocus, jFocus )

                                                        _ ->
                                                            False
                                                )
                                            )
                                        |> Maybe.Extra.join
                                        |> withDefault False
                            in
                            if canReach then
                                -- FIXME: isPromoted
                                let
                                    backgroundColor =
                                        if isBlackTurn then
                                            "#444"

                                        else
                                            "#CCC"
                                in
                                onClick (Move iFocus jFocus False) :: style "background-color" backgroundColor :: styles

                            else
                                (style "background-color" <|
                                    case color square_ of
                                        Nothing ->
                                            "#888"

                                        Just False ->
                                            "#FFF"

                                        Just True ->
                                            "#000"
                                )
                                    :: onClick Untouch
                                    :: styles
            in
            case square_ of
                Nothing ->
                    td attributes []

                Just piece ->
                    td
                        (if .isBlack piece then
                            {- style "transform" "rotate(180deg)" :: -}
                            attributes

                         else
                            attributes
                        )
                        [ piece |> showPiece |> text ]
    in
    div
        [ style "font-family" "Noto Sans"
        , style "color" "white"
        ]
        [ div [ style "color" "black" ]
            [ text <| String.concat [ "turn ", String.fromInt turn ]
            , div []
                [ text <| showState state ]
            ]
        , table
            [ class "board"
            , style "border" "1px solid"
            ]
            (board
                |> indexedMap
                    (\i ->
                        \squares ->
                            tr []
                                (indexedMap
                                    (squareToTd i)
                                    squares
                                    |> (::)
                                        (td
                                            [ style "color" "black" ]
                                            [ i |> fromInt |> text ]
                                        )
                                )
                    )
                |> reverse
                |> (::) (tr [] (iota 9 |> List.reverse |> List.map (\j -> td [ style "color" "black", style "text-align" "center", style "vertical-align" "top" ] [ j |> fromInt |> text ])))
            )
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }



{- TODO:
   - capture and drop
   - promote
-}
