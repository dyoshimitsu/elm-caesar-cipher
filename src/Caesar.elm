module Caesar exposing (count, encode, freqs, int2let, let2int, lowers, percent, shift, table)


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
