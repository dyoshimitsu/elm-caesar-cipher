module Caesar exposing (encode, int2let, let2int, shift, table)


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
