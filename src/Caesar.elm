module Caesar exposing (crack)


let2int : Char -> Int
let2int c =
    Char.toCode c - Char.toCode 'a'


int2let : Int -> Char
int2let n =
    Char.fromCode (Char.toCode 'a' + n)


shift : Int -> Char -> Char
shift n c =
    case Char.isLower c of
        True ->
            int2let <| modBy 26 <| let2int c + n

        _ ->
            c


encode : Int -> String -> String
encode n xs =
    String.fromList <| List.map (shift n) <| String.toList xs


table : List Float
table =
    [ 8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]


percent : Int -> Int -> Float
percent n m =
    toFloat n / toFloat m * 100


lowers : String -> Int
lowers xs =
    List.length <| List.filter (\n -> n >= 'a' && n <= 'z') <| String.toList xs


count : Char -> String -> Int
count c xs =
    List.length <| List.filter (\n -> n == c) <| String.toList xs


freqs : String -> List Float
freqs xs =
    let
        n =
            lowers xs
    in
    List.map (\x -> percent (count x xs) n) (String.toList "abcdefghijklmnopqrstuvwxyz")


chisqr : List Float -> List Float -> Float
chisqr os es =
    List.sum <| List.map (\( o, e ) -> ((o - e) ^ 2) / e) <| List.map2 Tuple.pair os es


rotate : Int -> List a -> List a
rotate n xs =
    List.drop n xs ++ List.take n xs


positions : a -> List a -> List Int
positions y xs =
    List.filterMap
        (\( i, x ) ->
            if y == x then
                Just i

            else
                Nothing
        )
    <|
        List.indexedMap Tuple.pair xs


crack : String -> String
crack xs =
    let
        factor =
            List.head (positions (Maybe.withDefault 0.0 (List.minimum chitab)) chitab)

        chitab =
            List.map (\n -> chisqr (rotate n table2) table) <| List.range 0 25

        table2 =
            freqs xs
    in
    encode -(Maybe.withDefault 0 factor) xs
