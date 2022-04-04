module Main exposing (main, update, view)

import Arithmetic exposing (isOdd)
import Browser
import Html exposing (a, div, input, li, table, td, text, tr, ul)
import Html.Attributes exposing (checked, colspan, style, type_)
import Html.Events exposing (onCheck, onClick)
import Kind exposing (PieceKind)
import List exposing (all, append, concat, drop, filter, foldl, indexedMap, map, member, repeat, reverse)
import List.Extra exposing (init, remove, unconsLast)
import ListMisc exposing (..)
import Maybe exposing (Maybe(..), withDefault)
import Maybe.Extra
import String exposing (fromInt)
import Tuple exposing (pair)


type alias Piece =
    { kind : PieceKind, isBlack : Bool, isPromoted : Bool }


showPiece : Bool -> Piece -> String
showPiece isJa { kind, isPromoted } =
    Kind.show isJa kind isPromoted


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


boardEmpty : Board
boardEmpty =
    repeat 9 Nothing |> repeat 9


boardInitial : Board
boardInitial =
    let
        setPawns isBlack board =
            iota 9
                |> foldl (\j -> setAt2 2 j <| Just { kind = Kind.P, isBlack = isBlack, isPromoted = False }) board

        setSymmetry j kind isBlack board =
            foldl (\j_ -> setAt2 0 j_ <| Just { kind = kind, isBlack = isBlack, isPromoted = False }) board <| [ j, 8 - j ]

        setPlayer isBlack board =
            board
                |> setPawns isBlack
                |> setAt2 1 1 (Just { kind = Kind.B, isBlack = isBlack, isPromoted = False })
                |> setAt2 1 7 (Just { kind = Kind.R, isBlack = isBlack, isPromoted = False })
                |> (\board_ ->
                        indexedMap pair [ Kind.L, Kind.N, Kind.S, Kind.G, Kind.K ] |> foldl (\( j, kind ) -> setSymmetry j kind isBlack) board_
                   )
    in
    boardEmpty
        |> setPlayer False
        |> reverse2
        |> setPlayer True
        |> reverse2


init : Model
init =
    { turn = 0
    , board = boardInitial
    , capturesW = []
    , capturesB = []
    , state = Moved
    , actions = []
    , isJa = False
    , blackIsReversed = False
    }


act : Int -> Board -> ( List PieceKind, List PieceKind ) -> Action -> ( Int, Board, ( List PieceKind, List PieceKind ) )
act turn board ( capturesW, capturesB ) action =
    let
        isBlackTurn =
            isOdd turn
    in
    case action of
        AMove piece0 i0 j0 i1 j1 isPromoted ->
            ( turn + 1
            , board
                |> setAt2 i0 j0 Nothing
                |> setAt2 i1
                    j1
                    (Just
                        { piece0 | isPromoted = piece0.isPromoted || isPromoted }
                    )
            , case getAt2 i1 j1 board of
                Just (Just piece1) ->
                    if isBlackTurn then
                        ( capturesW, piece1.kind :: capturesB )

                    else
                        ( piece1.kind :: capturesW, capturesB )

                _ ->
                    ( capturesW, capturesB )
            )

        ADrop kind i j ->
            ( turn + 1
            , setAt2 i
                j
                (Just
                    { kind = kind
                    , isBlack = isBlackTurn
                    , isPromoted = False
                    }
                )
                board
            , if isBlackTurn then
                ( capturesW, remove kind capturesB )

              else
                ( remove kind capturesW, capturesB )
            )


actAll : Int -> Board -> ( List PieceKind, List PieceKind ) -> List Action -> ( Int, Board, ( List PieceKind, List PieceKind ) )
actAll turn board ( capturesW, capturesB ) actions =
    case unconsLast actions of
        Nothing ->
            ( turn, board, ( capturesW, capturesB ) )

        Just ( action, actions_ ) ->
            let
                ( turn_, board_, ( capturesW_, capturesB_ ) ) =
                    act turn board ( capturesW, capturesB ) action
            in
            actAll turn_ board_ ( capturesW_, capturesB_ ) actions_


actAllInitial : List Action -> ( Int, Board, ( List PieceKind, List PieceKind ) )
actAllInitial actions =
    actAll 0 boardInitial ( [], [] ) actions


update : Message -> Model -> Model
update message model =
    let
        { turn, board, capturesW, capturesB, state, actions } =
            model
    in
    case ( message, state ) of
        ( CheckboxJa isJa_, _ ) ->
            { model | isJa = isJa_ }

        ( CheckboxBlack blackIsReversed_, _ ) ->
            let
                _ =
                    Debug.log "blackIsReversed" blackIsReversed_
            in
            { model | blackIsReversed = blackIsReversed_ }

        ( Touch i j piece, Moved ) ->
            { model | state = Touched i j piece }

        ( Untouch, Touched _ _ _ ) ->
            { model | state = Moved }

        ( Move i_ j_ isPromoted, Touched i j piece ) ->
            let
                action =
                    AMove piece i j i_ j_ isPromoted

                ( turn_, board_, ( capturesW_, capturesB_ ) ) =
                    act turn board ( capturesW, capturesB ) action
            in
            { model
                | turn = turn_
                , board = board_
                , capturesW = capturesW_
                , capturesB = capturesB_
                , state = Moved
                , actions = action :: actions
            }

        ( Drop i_ j_ kind, Touched _ _ _ ) ->
            let
                action =
                    ADrop kind i_ j_

                ( turn_, board_, ( capturesW_, capturesB_ ) ) =
                    act turn board ( capturesW, capturesB ) action
            in
            { model
                | turn = turn_
                , board = board_
                , capturesW = capturesW_
                , capturesB = capturesB_
                , state = Moved
                , actions = action :: actions
            }

        ( Undo n, _ ) ->
            case drop n actions of
                [] ->
                    model

                _ :: actions_ ->
                    let
                        ( turn_, board_, ( capturesW_, capturesB_ ) ) =
                            actAllInitial actions_
                    in
                    { model
                        | turn = turn_
                        , board = board_
                        , capturesW = capturesW_
                        , capturesB = capturesB_
                        , state = Moved
                        , actions = actions_
                    }

        _ ->
            let
                _ =
                    Debug.log "error" ""
            in
            model


isOpposite : Int -> Int -> Bool
isOpposite turn i =
    if isOdd turn then
        i < 3

    else
        6 <= i


canReach : Int -> Int -> Piece -> Int -> Board -> Int -> Int -> Bool
canReach i0 j0 piece0 turn board i1 j1 =
    let
        isBlackTurn =
            isOdd turn
    in
    if i0 < 0 then
        getAt2 i1 j1 board
            == Just Nothing
            -- two pawns
            && (piece0.kind
                    /= Kind.P
                    || (iota 9
                            |> all
                                (\i ->
                                    case getAt2 i j1 board of
                                        Just (Just piece1) ->
                                            ( piece1.kind, piece1.isBlack, piece1.isPromoted ) /= ( Kind.P, isBlackTurn, False )

                                        _ ->
                                            True
                                )
                       )
               )

    else
        let
            possibleMoves i j f =
                let
                    -- not include touched square
                    ( i_, j_ ) =
                        f i j
                in
                if List.any (\n -> n < 0 || 9 <= n) [ i_, j_ ] then
                    []

                else
                    case getAt2 i_ j_ board of
                        Just (Just piece_) ->
                            if piece_.isBlack == isBlackTurn then
                                []

                            else
                                [ ( i_, j_ ) ]

                        _ ->
                            ( i_, j_ ) :: possibleMoves i_ j_ f
        in
        let
            move n it =
                if isBlackTurn then
                    it - n

                else
                    it + n

            mapMove i j =
                map (\( di, dj ) -> ( move di i, move dj j ))

            { kind, isPromoted } =
                piece0

            -- FIXME: refactor
            fixed dijs =
                dijs
                    |> mapMove i0 j0
                    |> filter (\( i, j ) -> colorFromIndice i j board /= Just isBlackTurn)

            gold =
                fixed [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ), ( 0, -1 ), ( 0, 1 ), ( -1, 0 ) ]
        in
        member ( i1, j1 ) <|
            case ( kind, isPromoted ) of
                ( Kind.P, False ) ->
                    fixed [ ( 1, 0 ) ]

                ( Kind.N, False ) ->
                    fixed [ ( 2, -1 ), ( 2, 1 ) ]

                ( Kind.S, False ) ->
                    fixed [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ), ( -1, -1 ), ( -1, 1 ) ]

                ( Kind.G, False ) ->
                    gold

                ( Kind.K, False ) ->
                    fixed <| product [ -1, 0, 1 ] [ -1, 0, 1 ]

                ( Kind.L, False ) ->
                    possibleMoves i0 j0 (\i -> \j -> ( move 1 i, j ))

                ( Kind.B, False ) ->
                    product [ -1, 1 ] [ -1, 1 ]
                        |> map (\( di, dj ) -> possibleMoves i0 j0 (\i -> \j -> ( move di i, move dj j )))
                        |> concat

                ( Kind.R, False ) ->
                    [ ( -1, 0 ), ( 0, -1 ), ( 0, 1 ), ( 1, 0 ) ]
                        |> map (\( di, dj ) -> possibleMoves i0 j0 (\i -> \j -> ( move di i, move dj j )))
                        |> concat

                ( Kind.B, True ) ->
                    (product [ -1, 1 ] [ -1, 1 ]
                        |> map (\( di, dj ) -> possibleMoves i0 j0 (\i -> \j -> ( move di i, move dj j )))
                        |> concat
                    )
                        |> append
                            ([ ( 0, -1 ), ( 0, 1 ), ( -1, 0 ), ( 1, 0 ) ]
                                |> mapMove i0 j0
                                |> filter (\( i, j ) -> colorFromIndice i j board /= Just isBlackTurn)
                            )

                ( Kind.R, True ) ->
                    ([ ( -1, 0 ), ( 0, -1 ), ( 0, 1 ), ( 1, 0 ) ]
                        |> map (\( di, dj ) -> possibleMoves i0 j0 (\i -> \j -> ( move di i, move dj j )))
                        |> concat
                    )
                        |> append
                            (product [ -1, 1 ] [ -1, 1 ]
                                |> mapMove i0 j0
                                |> filter (\( i, j ) -> colorFromIndice i j board /= Just isBlackTurn)
                            )

                ( _, True ) ->
                    gold


canPromote : Int -> Int -> Piece -> Int -> Board -> Int -> Int -> Bool
canPromote i0 j0 piece0 turn board i1 j1 =
    (0 <= i0)
        && not
            (getAt2 i0 j0 board |> Maybe.map (Maybe.map .isPromoted) |> Maybe.Extra.join |> withDefault False)
        && canReach i0 j0 piece0 turn board i1 j1
        && (isOpposite turn i0 || isOpposite turn i1)


squareToBackgroundColor square =
    style "background-color" <|
        case color square of
            Nothing ->
                "#888"

            Just False ->
                "transparent"

            Just True ->
                "#000"


stylesTd =
    [ style "width" "5em"
    , style "height" "5em"
    , style "text-align" "center"
    , style "vertical-align" "middle"
    , style "padding" "0"
    , style "box-sizing" "border-box"
    ]


squareToTd turn board state isJa blackIsReversed i1 j1 square1 =
    let
        isBlackTurn =
            isOdd turn

        styles =
            style "color"
                (if color square1 == Just False then
                    "black"

                 else
                    "white"
                )
                :: stylesTd

        attributes =
            case state of
                Moved ->
                    case square1 of
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
                                squareToBackgroundColor square1
                                    :: style "border"
                                        (if color square1 == Just False then
                                            "1px solid #888"

                                         else
                                            "none"
                                        )
                                    :: (onClick <| Touch i1 j1 piece)
                                    :: styles

                            else
                                squareToBackgroundColor square1
                                    :: style "border"
                                        (if color square1 == Just False then
                                            "1px solid #888"

                                         else
                                            "none"
                                        )
                                    :: styles

                Touched i0 j0 piece0 ->
                    if canReach i0 j0 piece0 turn board i1 j1 then
                        let
                            backgroundColor =
                                case ( isBlackTurn, square1 ) of
                                    ( False, Nothing ) ->
                                        "#CCC"

                                    ( False, Just _ ) ->
                                        "#AAA"

                                    ( True, Nothing ) ->
                                        "#444"

                                    ( True, Just _ ) ->
                                        "#666"
                        in
                        if canPromote i0 j0 piece0 turn board i1 j1 then
                            styles

                        else if i0 < 0 then
                            onClick (Drop i1 j1 <| .kind piece0) :: style "background-color" backgroundColor :: styles

                        else
                            onClick (Move i1 j1 False) :: style "background-color" backgroundColor :: styles

                    else
                        squareToBackgroundColor square1
                            :: onClick Untouch
                            :: style "border"
                                (if color square1 == Just False then
                                    "1px solid #888"

                                 else
                                    "none"
                                )
                            :: styles
    in
    case state of
        Touched i0 j0 piece0 ->
            if canPromote i0 j0 piece0 turn board i1 j1 then
                let
                    backgroundColor =
                        case ( isBlackTurn, square1 ) of
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
                        , style "top" "0"
                        , style "box-sizing" "border-box"
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "justify-content" "center"
                        ]
                in
                td (style "position" "relative" :: style "background-color" backgroundColor :: style "border" "none" :: attributes)
                    (append
                        [ Maybe.map
                            (showPiece isJa)
                            square1
                            |> withDefault ""
                            |> text
                        ]
                     <|
                        if isBlackTurn then
                            [ div (onClick (Move i1 j1 True) :: style "left" "0" :: style "position" "absolute" :: style "border-right" "1px solid #FFF" :: stylesDiv) [ text "+" ]
                            , div (onClick (Move i1 j1 False) :: style "right" "0" :: style "position" "absolute" :: stylesDiv) []
                            ]

                        else
                            [ div (onClick (Move i1 j1 False) :: style "left" "0" :: style "position" "absolute" :: stylesDiv) []
                            , div (onClick (Move i1 j1 True) :: style "right" "0" :: style "position" "absolute" :: style "border-left" "1px solid #FFF" :: stylesDiv) [ text "+" ]
                            ]
                    )

            else
                -- FIXME: same
                case square1 of
                    Nothing ->
                        td attributes []

                    Just piece ->
                        td
                            (if blackIsReversed && piece.isBlack then
                                style "transform" "rotate(180deg)" :: attributes

                             else
                                attributes
                            )
                            [ piece |> showPiece isJa |> text ]

        _ ->
            -- FIXME: same
            case square1 of
                Nothing ->
                    td attributes []

                Just piece ->
                    td
                        (if blackIsReversed && piece.isBlack then
                            style "transform" "rotate(180deg)" :: attributes

                         else
                            attributes
                        )
                        [ piece |> showPiece isJa |> text ]


getPosition : PieceKind -> Board -> List ( Int, Int )
getPosition kind board =
    board
        |> indexedMap
            (\i ->
                \squares ->
                    squares
                        |> indexedMap
                            (\j ->
                                \square ->
                                    if Maybe.map .kind square == Just kind then
                                        [ ( i, j ) ]

                                    else
                                        []
                            )
            )
        |> concat
        |> concat


isSuicide : Int -> Board -> Bool
isSuicide turn board =
    Debug.todo "isSuicide not implemented"


isCheck : Int -> Board -> Bool
isCheck turn board =
    Debug.todo "isCheck not implemented"


isCheckmate : Int -> Board -> Bool
isCheckmate turn board =
    Debug.todo "isCheckmate not implemented"


turnSymbol turn =
    if isOdd turn then
        "■"

    else
        "□"


view : Model -> Html.Html Message
view { turn, board, capturesW, capturesB, state, actions, isJa, blackIsReversed } =
    let
        jToChar j =
            Char.toCode 'a' + j |> Char.fromCode

        stylesTable =
            [ style "border-spacing" "1px"
            , style "float" "left"
            , style "color" "#888"
            ]

        isBlackTurn =
            isOdd turn

        kindToScale kind =
            let
                n =
                    Kind.toInt kind
            in
            if n == 0 then
                "125%"

            else if n < 5 then
                "150%"

            else if n < 7 then
                "175%"

            else
                "200%"
    in
    div
        [ style "font-family" "Noto Sans"
        , style "color" "black"
        , style "vertical-align" "top"
        ]
        [ let
            f : List PieceKind -> List (List ( Int, PieceKind ))
            f kinds =
                iota 4
                    |> map
                        (\i ->
                            indexedMap pair (Kind.sort kinds)
                                |> filter (\( k, _ ) -> modBy 4 k == i)
                        )
          in
          table (style "border" "1px solid black" :: stylesTable) <|
            ([ f capturesW, [ [] ], f capturesB |> reverse ]
                |> concat
                |> indexedMap
                    (\i ->
                        \captures ->
                            case captures of
                                [] ->
                                    tr [] <| repeat 5 <| td stylesTd []

                                _ ->
                                    tr [] <|
                                        map
                                            (\( j, capture ) ->
                                                td
                                                    (stylesTd
                                                        |> append
                                                            (if i < 4 then
                                                                [ style "border" "1px solid #888" ]

                                                             else if 5 <= i then
                                                                append
                                                                    [ style "background-color" "#000" ]
                                                                    (if blackIsReversed then
                                                                        [ style "transform" "rotate(180deg)" ]

                                                                     else
                                                                        []
                                                                    )

                                                             else
                                                                []
                                                            )
                                                        |> append
                                                            (case state of
                                                                Touched _ _ _ ->
                                                                    [ onClick Untouch ]

                                                                Moved ->
                                                                    if (i < 4 && not isBlackTurn) || (5 <= i && isBlackTurn) then
                                                                        [ onClick (Touch -1 j { kind = capture, isBlack = isBlackTurn, isPromoted = False }) ]

                                                                    else
                                                                        []
                                                            )
                                                    )
                                                    [ capture |> (\it -> text <| Kind.show isJa it False) ]
                                            )
                                            captures
                    )
                |> reverse
            )
        , table
            stylesTable
            (board
                |> indexedMap
                    (\i ->
                        \squares ->
                            tr []
                                (indexedMap
                                    (squareToTd turn board state isJa blackIsReversed i)
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
                        append it [ tr [ style "color" "#000" ] <| td [] [] :: (iota 9 |> map (\j -> td [ style "text-align" "center", style "vertical-align" "top" ] [ j |> jToChar |> String.fromChar |> text ])) ]
                   )
            )
        , table
            [ style "float" "left"
            , style "list-style" "none"
            , style "margin-right" "4em"
            , style "text-align" "center"
            ]
            (actions
                |> indexedMap
                    (\dTurn ->
                        \action ->
                            tr [] <|
                                let
                                    turn_ =
                                        turn - 1 - dTurn

                                    f k ( content, align ) =
                                        td [ style "text-align" align ]
                                            [ if k == 1 then
                                                a [ onClick <| Undo dTurn, style "text-decoration" "underline" ]
                                                    [ text content ]

                                              else
                                                text content
                                            ]
                                in
                                case action of
                                    AMove piece i0 j0 i1 j1 isPromoted ->
                                        indexedMap f
                                            [ ( turnSymbol turn_, "center" )
                                            , ( fromInt <| turn_, "right" )
                                            , ( showPiece isJa piece, "center" )
                                            , ( j0 |> jToChar |> String.fromChar, "center" )
                                            , ( fromInt i0, "center" )
                                            , ( if j0 == j1 then
                                                    ""

                                                else
                                                    j1 |> jToChar |> String.fromChar
                                              , "center"
                                              )
                                            , ( if i0 == i1 then
                                                    ""

                                                else
                                                    fromInt i1
                                              , "center"
                                              )
                                            , ( if isPromoted then
                                                    "+"

                                                else
                                                    ""
                                              , "center"
                                              )
                                            ]

                                    ADrop kind i j ->
                                        indexedMap f
                                            [ ( turnSymbol turn_, "center" )
                                            , ( fromInt <| turn_, "right" )
                                            , ( Kind.show isJa kind False, "center" )
                                            , ( "", "center" )
                                            , ( "", "center" )
                                            , ( j |> jToChar |> String.fromChar, "center" )
                                            , ( fromInt i, "center" )
                                            ]
                    )
                |> (::)
                    (tr
                        []
                        (map
                            (\( content, align ) -> td [ style "text-align" align ] [ text content ])
                            (append
                                [ ( turnSymbol turn, "center" )
                                , ( turn |> fromInt, "right" )
                                ]
                             <|
                                let
                                    f i content =
                                        if i == -1 then
                                            ""

                                        else
                                            content
                                in
                                case state of
                                    Moved ->
                                        []

                                    Touched i j piece ->
                                        [ ( Kind.show isJa piece.kind piece.isPromoted, "center" )
                                        , ( f i (j |> jToChar |> String.fromChar)
                                          , "center"
                                          )
                                        , ( f i (fromInt i)
                                          , "center"
                                          )
                                        , ( "", "center" )
                                        , ( "", "center" )
                                        ]
                            )
                        )
                    )
                |> (::)
                    (tr []
                        [ td [ style "writing-mode" "vertical-lr", style "text-align" "right" ]
                            [ text "player" ]
                        , td
                            [ style "writing-mode" "vertical-lr", style "text-align" "right" ]
                            [ text "turn" ]
                        , td
                            [ style "writing-mode" "vertical-lr", style "text-align" "right" ]
                            [ text "piece" ]
                        , td
                            [ style "writing-mode" "vertical-lr", style "text-align" "right", colspan 2 ]
                            [ text "from" ]
                        , td
                            [ style "writing-mode" "vertical-lr", style "text-align" "right", colspan 2 ]
                            [ text "to" ]
                        , td
                            [ style "writing-mode" "vertical-lr", style "text-align" "right" ]
                            [ text "promotion" ]
                        ]
                    )
            )
        , ul [ style "clear" "both" ]
            [ li []
                [ input [ type_ "checkbox", checked isJa, onCheck CheckboxJa ] []
                , text "japanese"
                ]
            , li []
                [ input [ type_ "checkbox", checked blackIsReversed, onCheck CheckboxBlack ] []
                , text "reverse black pieces"
                ]
            ]
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
