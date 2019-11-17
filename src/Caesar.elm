module Caesar exposing (int2let, let2int)


let2int : Char -> Int
let2int c =
    Char.toCode c - Char.toCode 'a'


int2let : Int -> Char
int2let n =
    Char.fromCode (Char.toCode 'a' + n)
