module Caesar exposing (let2int)


let2int : Char -> Int
let2int c =
    Char.toCode c - Char.toCode 'a'
