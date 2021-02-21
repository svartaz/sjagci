module Main exposing (Message(..), main, update, view)

import Arithmetic exposing (isEven, isOdd)
import Browser
import Html exposing (div, li, table, td, text, tr, ul)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List exposing (append, concat, concatMap, filter, foldl, indexedMap, map, member, repeat, reverse)
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


showPiece : Piece -> String
showPiece { kind, isPromoted } =
    showKind kind
        |> (if isPromoted then
                String.append "+"

            else
                identity
           )


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


type Model
    = Model Int Board State (List Action)


type Action
    = AMove Piece Int Int Bool
    | ADrop Piece Int Int


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
    Model 0 boardInitial Moved []


update : Message -> Model -> Model
update message (Model turn board state actions) =
    case ( message, state ) of
        ( Touch i j, Moved ) ->
            Model turn board (Touched i j) actions

        ( Untouch, Touched i j ) ->
            Model turn
                board
                Moved
                actions

        ( Move i_ j_ isPromoted, Touched i j ) ->
            case getAt2 i j board of
                Just (Just piece) ->
                    let
                        isAlreadyPromoted =
                            .isPromoted piece
                    in
                    Model
                        (turn + 1)
                        (updateBoard i j i_ j_ (isPromoted || isAlreadyPromoted) board)
                        Moved
                        (AMove piece i_ j_ isPromoted
                            :: actions
                        )

                _ ->
                    Debug.log
                        "error"
                    <|
                        Model
                            turn
                            board
                            state
                            actions

        _ ->
            let
                _ =
                    Debug.log "error: can not update" ""
            in
            Model turn board state actions



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


jToChar : Int -> Char
jToChar j =
    Char.toCode 'a' + j |> Char.fromCode


canReach : Int -> Int -> Board -> Bool -> Int -> Int -> Bool
canReach iTouch jTouch board isBlackTurn iFocus jFocus =
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
    in
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
                    let
                        gold =
                            [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ), ( 0, -1 ), ( 0, 1 ), ( -1, 0 ) ]
                                |> mapMove iTouch jTouch
                                |> filter (\( i, j ) -> colorFromIndice i j board /= Just isBlackTurn)
                                |> member ( iFocus, jFocus )
                    in
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
                            gold

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

                        ( B, True ) ->
                            -- FIXME
                            False

                        ( R, True ) ->
                            -- FIXME
                            False

                        ( _, True ) ->
                            gold
                )
            )
        |> Maybe.Extra.join
        |> withDefault False


canPromote : Int -> Int -> Board -> Bool -> Int -> Int -> Bool
canPromote iTouch jTouch board isBlackTurn iFocus jFocus =
    let
        maybeSquareTouch =
            getAt2 iTouch jTouch board
    in
    not (maybeSquareTouch |> Maybe.map (Maybe.map .isPromoted) |> Maybe.Extra.join |> withDefault False)
        && canReach iTouch jTouch board isBlackTurn iFocus jFocus
        && (if isBlackTurn then
                iFocus < 3

            else
                6 <= iFocus
           )


squareToBackgroundColor square =
    style "background-color" <|
        case color square of
            Nothing ->
                "#888"

            Just False ->
                "#FFF"

            Just True ->
                "#000"


squareToTd turn board state iFocus jFocus squareFocus =
    let
        isBlackTurn =
            isOdd turn

        styles =
            [ style "width" "4em"
            , style "height" "4em"
            , style "text-align" "center"
            , style "vertical-align" "middle"
            , style "color" "#888"
            , style "padding" "0"
            , style "box-sizing" "border-box"
            ]

        attributes =
            case state of
                Moved ->
                    case squareFocus of
                        Nothing ->
                            style "background-color" "#888" :: styles

                        Just piece ->
                            let
                                -- FIXME
                                canTouch =
                                    isBlackTurn
                                        == .isBlack piece
                            in
                            if canTouch then
                                squareToBackgroundColor squareFocus
                                    :: style "border"
                                        (if color squareFocus == Just False then
                                            "1px solid #888"

                                         else
                                            "none"
                                        )
                                    :: (onClick <| Touch iFocus jFocus)
                                    :: styles

                            else
                                squareToBackgroundColor squareFocus
                                    :: style "border"
                                        (if color squareFocus == Just False then
                                            "1px solid #888"

                                         else
                                            "none"
                                        )
                                    :: styles

                Touched iTouch jTouch ->
                    if canReach iTouch jTouch board isBlackTurn iFocus jFocus then
                        let
                            backgroundColor =
                                case ( isBlackTurn, squareFocus ) of
                                    ( False, Nothing ) ->
                                        "#CCC"

                                    ( False, Just _ ) ->
                                        "#AAA"

                                    ( True, Nothing ) ->
                                        "#444"

                                    ( True, Just _ ) ->
                                        "#666"
                        in
                        if canPromote iTouch jTouch board isBlackTurn iFocus jFocus then
                            styles

                        else
                            onClick (Move iFocus jFocus False) :: style "background-color" backgroundColor :: styles

                    else
                        squareToBackgroundColor squareFocus
                            :: onClick Untouch
                            :: style "border"
                                (if color squareFocus == Just False then
                                    "1px solid #888"

                                 else
                                    "none"
                                )
                            :: styles
    in
    case state of
        Touched iTouch jTouch ->
            if canPromote iTouch jTouch board isBlackTurn iFocus jFocus then
                let
                    backgroundColor =
                        case ( isBlackTurn, squareFocus ) of
                            ( False, Nothing ) ->
                                "#CCC"

                            ( False, Just _ ) ->
                                "#AAA"

                            ( True, Nothing ) ->
                                "#444"

                            ( True, Just _ ) ->
                                "#666"

                    stylesDiv =
                        [ style "width" "50%"
                        , style "height" "100%"
                        , style "display" "inline-block"
                        , style "top" "0"
                        , style "box-sizing" "border-box"
                        ]
                in
                td (style "position" "relative" :: style "background-color" backgroundColor :: style "border" "none" :: attributes)
                    [ Maybe.map
                        showPiece
                        squareFocus
                        |> withDefault ""
                        |> text
                    , div (onClick (Move iFocus jFocus False) :: style "left" "0" :: style "position" "absolute" :: stylesDiv) []
                    , div (onClick (Move iFocus jFocus True) :: style "right" "0" :: style "position" "absolute" :: style "border-left" "1px solid #FFF" :: stylesDiv) []
                    ]

            else
                -- FIXME: same
                case squareFocus of
                    Nothing ->
                        td attributes []

                    Just piece ->
                        td
                            (if .isBlack piece then
                                attributes

                             else
                                attributes
                            )
                            [ piece |> showPiece |> text ]

        _ ->
            -- FIXME: same
            case squareFocus of
                Nothing ->
                    td attributes []

                Just piece ->
                    td
                        (if .isBlack piece then
                            attributes

                         else
                            attributes
                        )
                        [ piece |> showPiece |> text ]


view : Model -> Html.Html Message
view (Model turn board state actions) =
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
            , style "border-spacing" "1px"
            ]
            (board
                |> indexedMap
                    (\i ->
                        \squares ->
                            tr []
                                (indexedMap
                                    (squareToTd turn board state i)
                                    squares
                                    |> (::)
                                        (td
                                            [ style "color" "black" ]
                                            [ i |> fromInt |> text ]
                                        )
                                )
                    )
                |> reverse
                |> (\it ->
                        append it [ tr [] <| td [] [] :: ([ "a", "b", "c", "d", "e", "f", "g", "h", "i" ] |> List.map (\j -> td [ style "color" "black", style "text-align" "center", style "vertical-align" "top" ] [ j |> text ])) ]
                   )
            )
        , ul [ style "color" "black" ]
            (actions
                |> Debug.log "actions"
                |> indexedMap
                    (\reversedTurn ->
                        \action ->
                            li []
                                [ text <|
                                    let
                                        turnSymbol =
                                            if isOdd turn == isOdd reversedTurn then
                                                "■"

                                            else
                                                "□"
                                    in
                                    case action of
                                        AMove piece i j isPromoted ->
                                            [ turnSymbol
                                            , ""
                                            , "move "
                                            , showPiece piece
                                            , " "
                                            , j |> jToChar |> String.fromChar
                                            , fromInt i
                                            , if isPromoted then
                                                "+"

                                              else
                                                ""
                                            ]
                                                |> String.concat

                                        ADrop piece i j ->
                                            [ turnSymbol
                                            , "drop "
                                            , showPiece piece
                                            , " "
                                            , j |> jToChar |> String.fromChar
                                            , fromInt i
                                            ]
                                                |> String.concat
                                ]
                    )
            )
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }



{- TODO:
   - capture and drop
   - promote
-}
