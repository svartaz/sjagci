module Main exposing (main, update, view)

import Arithmetic exposing (isEven, isOdd)
import Browser
import Html exposing (div, li, table, td, text, tr, ul)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List exposing (append, concat, concatMap, filter, foldl, indexedMap, map, member, repeat, reverse)
import List.Extra exposing (getAt, removeAt)
import ListMisc exposing (..)
import Maybe exposing (Maybe(..), withDefault)
import Maybe.Extra
import String exposing (fromInt, toUpper)
import Tuple exposing (pair)
import Types exposing (..)


boardEmpty : Board
boardEmpty =
    repeat 9 <| repeat 9 Nothing


boardInitial : Board
boardInitial =
    let
        setPawns isBlack board =
            iota 9 |> foldl (\j -> setAt2 2 j <| Just { kind = P, isBlack = isBlack, isPromoted = False, canPromote = False }) board

        setSymmetry j kind isBlack board =
            foldl (\j_ -> setAt2 0 j_ <| Just { kind = kind, isBlack = isBlack, isPromoted = False, canPromote = False }) board <| [ j, 8 - j ]

        setPlayer isBlack board =
            board
                |> setPawns isBlack
                |> setAt2 1 1 (Just { kind = B, isBlack = isBlack, isPromoted = False, canPromote = False })
                |> setAt2 1 7 (Just { kind = R, isBlack = isBlack, isPromoted = False, canPromote = False })
                |> (\board_ ->
                        indexedMap pair [ L, N, S, G, K ] |> foldl (\( j, kind ) -> setSymmetry j kind isBlack) board_
                   )
    in
    boardEmpty
        |> setPlayer False
        |> reverse2
        |> setPlayer True
        |> reverse2


init : Model
init =
    Model 0 boardInitial [] [] Moved []


update : Message -> Model -> Model
update message (Model turn board capturesW capturesB state actions) =
    let
        isBlackTurn =
            isOdd turn
    in
    case ( message, state ) of
        ( Touch i j piece, Moved ) ->
            Model turn board capturesW capturesB (Touched i j piece) actions

        ( Untouch, Touched _ _ _ ) ->
            Model turn
                board
                capturesW
                capturesB
                Moved
                actions

        ( Move i_ j_ isPromoted, Touched i j piece ) ->
            let
                isAlreadyPromoted =
                    .isPromoted piece
            in
            case getAt2 i_ j_ board of
                Just (Just piece_) ->
                    Model
                        (turn + 1)
                        (updateBoard i j i_ j_ (isPromoted || isAlreadyPromoted) board)
                        (if .isBlack piece_ then
                            sortKinds (.kind piece_ :: capturesW)

                         else
                            capturesW
                        )
                        (if .isBlack piece_ then
                            capturesB

                         else
                            sortKinds (.kind piece_ :: capturesB)
                        )
                        Moved
                        (AMove piece i_ j_ isPromoted
                            :: actions
                        )

                _ ->
                    Model
                        (turn + 1)
                        (updateBoard i j i_ j_ (isPromoted || isAlreadyPromoted) board)
                        capturesW
                        capturesB
                        Moved
                        (AMove piece i_ j_ isPromoted
                            :: actions
                        )

        ( Drop i_ j_ kind, Touched _ j _ ) ->
            Model
                (turn + 1)
                (setAt2 i_ j_ (Just { kind = kind, isBlack = isBlackTurn, isPromoted = False, canPromote = isBlackTurn && i_ < 3 || not isBlackTurn && 6 <= i_ }) board)
                (if isBlackTurn then
                    capturesW

                 else
                    removeAt j capturesW
                )
                (if isBlackTurn then
                    removeAt j capturesB

                 else
                    capturesB
                )
                Moved
                (ADrop kind i_ j_ :: actions)

        _ ->
            let
                _ =
                    Debug.log "error" ""
            in
            Model turn board capturesW capturesB state actions



-- FIXME


updateBoard : Int -> Int -> Int -> Int -> Bool -> Board -> Board
updateBoard i j i_ j_ isPromoted board =
    case getAt2 i j board of
        Just (Just piece) ->
            board
                |> setAt2 i j Nothing
                |> setAt2 i_ j_ (Just { piece | isPromoted = isPromoted, canPromote = .canPromote piece || .isBlack piece && i_ < 3 || not (.isBlack piece) && 6 <= i_ })

        _ ->
            let
                _ =
                    Debug.log "error: can not move" ""
            in
            board


jToChar : Int -> Char
jToChar j =
    Char.toCode 'a' + j |> Char.fromCode


canReach : Int -> Int -> Piece -> Board -> Bool -> Int -> Int -> Bool
canReach iTouch jTouch pieceTouch board isBlackTurn iFocus jFocus =
    if iTouch < 0 then
        -- FIXME
        colorFromIndice iFocus jFocus board == Nothing

    else
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
        let
            move n it =
                if isBlackTurn then
                    it - n

                else
                    it + n

            mapMove i j =
                map (\( di, dj ) -> ( move di i, move dj j ))

            { kind, isPromoted } =
                pieceTouch
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
                (product [ -1, 1 ] [ -1, 1 ]
                    |> map (\( di, dj ) -> possibleMoves iTouch jTouch (\i -> \j -> ( move di i, move dj j )))
                    |> concat
                )
                    |> append
                        ([ ( 0, -1 ), ( 0, 1 ), ( -1, 0 ), ( 1, 0 ) ]
                            |> mapMove iTouch jTouch
                            |> filter (\( i, j ) -> colorFromIndice i j board /= Just isBlackTurn)
                        )
                    |> member ( iFocus, jFocus )

            ( R, True ) ->
                ([ ( -1, 0 ), ( 0, -1 ), ( 0, 1 ), ( 1, 0 ) ]
                    |> map (\( di, dj ) -> possibleMoves iTouch jTouch (\i -> \j -> ( move di i, move dj j )))
                    |> concat
                )
                    |> append
                        (product [ -1, 1 ] [ -1, 1 ]
                            |> mapMove iTouch jTouch
                            |> filter (\( i, j ) -> colorFromIndice i j board /= Just isBlackTurn)
                        )
                    |> member ( iFocus, jFocus )

            ( _, True ) ->
                gold


canPromote : Int -> Int -> Piece -> Board -> Bool -> Int -> Int -> Bool
canPromote iTouch jTouch pieceTouch board isBlackTurn iFocus jFocus =
    let
        maybeSquareTouch =
            getAt2 iTouch jTouch board
    in
    (0 < iTouch)
        && not
            (maybeSquareTouch |> Maybe.map (Maybe.map .isPromoted) |> Maybe.Extra.join |> withDefault False)
        && canReach iTouch jTouch pieceTouch board isBlackTurn iFocus jFocus
        && (.canPromote pieceTouch
                || (if isBlackTurn then
                        iFocus < 3

                    else
                        6 <= iFocus
                   )
           )


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
    [ style "width" "4em"
    , style "height" "4em"
    , style "text-align" "center"
    , style "vertical-align" "middle"
    , style "padding" "0"
    , style "box-sizing" "border-box"
    ]


squareToTd turn board state iFocus jFocus squareFocus =
    let
        isBlackTurn =
            isOdd turn

        styles =
            style "color" "#888" :: stylesTd

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
                                    :: (onClick <| Touch iFocus jFocus piece)
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

                Touched iTouch jTouch pieceTouch ->
                    if canReach iTouch jTouch pieceTouch board isBlackTurn iFocus jFocus then
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
                        if canPromote iTouch jTouch pieceTouch board isBlackTurn iFocus jFocus then
                            styles

                        else if iTouch < 0 then
                            onClick (Drop iFocus jFocus <| .kind pieceTouch) :: style "background-color" backgroundColor :: styles

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
        Touched iTouch jTouch pieceTouch ->
            if canPromote iTouch jTouch pieceTouch board isBlackTurn iFocus jFocus then
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
                            showPiece
                            squareFocus
                            |> withDefault ""
                            |> text
                        ]
                     <|
                        if isBlackTurn then
                            [ div (onClick (Move iFocus jFocus True) :: style "left" "0" :: style "position" "absolute" :: style "border-right" "1px solid #FFF" :: stylesDiv) [ text "+" ]
                            , div (onClick (Move iFocus jFocus False) :: style "right" "0" :: style "position" "absolute" :: stylesDiv) []
                            ]

                        else
                            [ div (onClick (Move iFocus jFocus False) :: style "left" "0" :: style "position" "absolute" :: stylesDiv) []
                            , div (onClick (Move iFocus jFocus True) :: style "right" "0" :: style "position" "absolute" :: style "border-left" "1px solid #FFF" :: stylesDiv) [ text "+" ]
                            ]
                    )

            else
                -- FIXME: same
                case squareFocus of
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

        _ ->
            -- FIXME: same
            case squareFocus of
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


turnSymbol turn =
    if isOdd turn then
        "■"

    else
        "□"


stylesTable =
    [ style "border" "none"
    , style "border-spacing" "1px"
    , style "float" "left"
    , style "color" "#888"
    ]


view : Model -> Html.Html Message
view (Model turn board capturesW capturesB state actions) =
    let
        isBlackTurn =
            isOdd turn
    in
    div
        [ style "width" "100%"
        , style "height" "100%"
        , style "font-family" "Noto Sans"
        , style "color" "black"
        , style "vertical-align" "top"
        ]
        [ table
            stylesTable
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
                        append it [ tr [ style "color" "#000" ] <| td [] [] :: (iota 9 |> map (\j -> td [ style "text-align" "center", style "vertical-align" "top" ] [ j |> jToChar |> String.fromChar |> text ])) ]
                   )
            )
        , let
            f : List PieceKind -> List (List ( Int, PieceKind ))
            f kinds =
                iota 4
                    |> map
                        (\n ->
                            indexedMap (\j -> \kind -> ( j, kind )) kinds
                                |> indexedFilter (\i -> \_ -> modBy 4 i == n)
                        )
          in
          table (style "margin-left" "4em" :: stylesTable) <|
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
                                                                [ style "background-color" "#000", style "transform" "rotate(180deg)" ]

                                                             else
                                                                []
                                                            )
                                                        |> append
                                                            (case state of
                                                                Touched _ _ _ ->
                                                                    [ onClick Untouch ]

                                                                Moved ->
                                                                    if (i < 4 && not isBlackTurn) || (5 <= i && isBlackTurn) then
                                                                        [ onClick (Touch -1 j { kind = capture, isBlack = isBlackTurn, canPromote = False, isPromoted = False }) ]

                                                                    else
                                                                        []
                                                            )
                                                    )
                                                    [ capture |> showKind |> (\it -> text it) ]
                                            )
                                            captures
                    )
                |> reverse
            )
        , table
            [ style "float" "left"
            , style "list-style" "none"
            , style "margin-right" "4em"
            , style "text-align" "center"
            , style "height" "100%"
            ]
            (actions
                |> indexedMap
                    (\dTurn ->
                        \action ->
                            tr [] <|
                                let
                                    turn_ =
                                        turn - 1 - dTurn
                                in
                                case action of
                                    AMove piece i j isPromoted ->
                                        map (\( it, align ) -> td [ style "text-align" align ] [ text it ])
                                            [ ( turnSymbol turn_, "right" )
                                            , ( fromInt <| turn_, "right" )
                                            , ( "move", "left" )
                                            , ( showPiece piece, "right" )
                                            , ( j |> jToChar |> String.fromChar, "center" )
                                            , ( fromInt i, "center" )
                                            , ( if isPromoted then
                                                    "+"

                                                else
                                                    ""
                                              , "left"
                                              )
                                            ]

                                    ADrop kind i j ->
                                        map (\( it, align ) -> td [ style "text-align" align ] [ text it ])
                                            [ ( turnSymbol turn_, "right" )
                                            , ( fromInt <| turn_, "right" )
                                            , ( "drop", "left" )
                                            , ( showKind kind, "right" )
                                            , ( j |> jToChar |> String.fromChar, "center" )
                                            , ( fromInt i, "center" )
                                            ]
                    )
                |> (::)
                    (tr
                        []
                        (map
                            (\( it, align ) -> td [ style "text-align" align ] [ text it ])
                            (append
                                [ ( turnSymbol turn, "right" )
                                , ( turn |> fromInt, "right" )
                                ]
                             <|
                                case state of
                                    Moved ->
                                        []

                                    Touched i j piece ->
                                        [ ( "touch", "left" )
                                        , ( showKind <| .kind piece, "right" )
                                        , ( j |> jToChar |> String.fromChar, "center" )
                                        , ( fromInt i, "center" )
                                        ]
                            )
                        )
                    )
            )
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }



{- TODO:
   - prohibited
      -- piece with no moves
      -- two pawns
      -- drop pawn mate
   -  repetition
   - check
      - checkmate
   - UI
-}
