module Caesar exposing (encode, int2let, let2int, shift)


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
