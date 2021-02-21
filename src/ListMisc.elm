module ListMisc exposing (..)

import List exposing (concatMap, filter, indexedMap, map, reverse)
import List.Extra exposing (getAt)
import Maybe.Extra


reverse2 : List (List a) -> List (List a)
reverse2 =
    map reverse >> reverse


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
