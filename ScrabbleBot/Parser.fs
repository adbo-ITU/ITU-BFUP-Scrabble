module internal Parser

open ScrabbleUtil
open System
open Eval
open StateMonad
open FParsecLight.TextParser

type squareFun = word -> int -> int -> Result<int, Error>

let pIntToChar = pstring "intToChar"
let pPointValue = pstring "pointValue"

let pCharToInt = pstring "charToInt"
let pToUpper = pstring "toUpper"
let pToLower = pstring "toLower"
let pCharValue = pstring "charValue"

let pTrue = pstring "true"
let pFalse = pstring "false"
let pIsDigit = pstring "isDigit"
let pIsLetter = pstring "isLetter"
let pIsVowel = pstring "isVowel"

let pif = pstring "if"
let pthen = pstring "then"
let pelse = pstring "else"
let pwhile = pstring "while"
let pdo = pstring "do"
let pdeclare = pstring "declare"

let pletter =
    satisfy System.Char.IsLetter <?> "letter"

let palphanumeric =
    satisfy System.Char.IsLetterOrDigit
    <?> "alphanumeric"

let whitespaceChar =
    satisfy System.Char.IsWhiteSpace <?> "whitespace"

let spaces = many whitespaceChar <?> "spaces"
let spaces1 = many1 whitespaceChar <?> "space1"

let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
let (.>*>) p1 p2 = p1 .>> spaces .>> p2
let (>*>.) p1 p2 = p1 .>> spaces >>. p2

let parenthesise p =
    pchar '(' >*>. p .>*> pchar ')' <?> "parenthesise"

// I was lazy: https://stackoverflow.com/questions/1766020/how-to-change-list-of-chars-to-string
let charListToString (cs: char list) =
    cs |> Array.ofList |> System.String.Concat

let pid =
    (pletter <|> pchar '_')
    .>>. many (palphanumeric <|> pchar '_')
    |>> (fun (first, other) -> first :: other |> charListToString)
    <?> "id"

let unop op a = op >*>. a <?> "unop"

let binop op p1 p2 = p1 .>*> op .>*>. p2 <?> "binop"

let TermParse, tref = createParserForwardedToRef<aExp> ()
let ProdParse, pref = createParserForwardedToRef<aExp> ()
let AtomParse, aref = createParserForwardedToRef<aExp> ()
let CexpParse, cref = createParserForwardedToRef<cExp> ()

let AddParse =
    binop (pchar '+') ProdParse TermParse |>> Add
    <?> "Add"

let SubParse =
    binop (pchar '-') ProdParse TermParse |>> Sub
    <?> "Sub"

do tref.Value <- choice [ AddParse; SubParse; ProdParse ]

let MulParse =
    binop (pchar '*') AtomParse ProdParse |>> Mul
    <?> "Mul"

let DivParse =
    binop (pchar '/') AtomParse ProdParse |>> Div
    <?> "Div"

let ModParse =
    binop (pchar '%') AtomParse ProdParse |>> Mod
    <?> "Mod"

do
    pref.Value <-
        choice [ MulParse
                 DivParse
                 ModParse
                 AtomParse ]

let NParse = pint32 |>> N <?> "Int"

let ParParse = parenthesise TermParse

let NegParse =
    unop (pchar '-') AtomParse
    |>> fun n -> Mul(N -1, n)
    <?> "Neg"

let PVParse =
    unop pPointValue AtomParse |>> PV <?> "pointValue"

let VParse = pid |>> V <?> "variable"

let CharToIntParse =
    pCharToInt >*>. parenthesise CexpParse
    |>> CharToInt
    <?> "CharToInt"

do
    aref.Value <-
        choice [ PVParse
                 CharToIntParse
                 NegParse
                 NParse
                 VParse
                 ParParse ]

let AexpParse = TermParse

let CParse =
    between (pchar '\'') (pchar '\'') anyChar |>> C
    <?> "char"

let CVParse =
    pCharValue >*>. parenthesise AexpParse |>> CV
    <?> "charValue"

let IntToCharParse =
    pIntToChar >*>. parenthesise AexpParse
    |>> IntToChar
    <?> "IntToChar"

let ToUpperParse =
    pToUpper >*>. parenthesise CexpParse |>> ToUpper
    <?> "ToUpper"

let ToLowerParse =
    pToLower >*>. parenthesise CexpParse |>> ToLower
    <?> "ToLower"

do
    cref.Value <-
        choice [ CParse
                 CVParse
                 IntToCharParse
                 ToUpperParse
                 ToLowerParse ]

let bLogicalParse, bLogRef = createParserForwardedToRef<bExp> ()
let bCompareParse, bCompRef = createParserForwardedToRef<bExp> ()
let bOtherParse, bOtherRef = createParserForwardedToRef<bExp> ()

let uncurry f = fun (x, y) -> f x y

let bConjParse =
    binop (pstring "/\\") bCompareParse bLogicalParse
    |>> uncurry (.&&.)
    <?> "Conj"

let bDisjParse =
    binop (pstring "\\/") bCompareParse bLogicalParse
    |>> uncurry (.||.)
    <?> "Disj"

do
    bLogRef.Value <-
        choice [ bConjParse
                 bDisjParse
                 bCompareParse ]

let bAEqParse =
    binop (pchar '=') AexpParse AexpParse
    |>> uncurry (.=.)
    <?> "AEq"

let bANEqParse =
    binop (pstring "<>") AexpParse AexpParse
    |>> uncurry (.<>.)
    <?> "ANEq"

let bALtParse =
    binop (pchar '<') AexpParse AexpParse
    |>> uncurry (.<.)
    <?> "ALt"

let bALtEqParse =
    binop (pstring "<=") AexpParse AexpParse
    |>> uncurry (.<=.)
    <?> "ALtEq"

let bAGtParse =
    binop (pchar '>') AexpParse AexpParse
    |>> uncurry (.>.)
    <?> "AGt"

let bAGtEqParse =
    binop (pstring ">=") AexpParse AexpParse
    |>> uncurry (.>=.)
    <?> "AGtEq"

do
    bCompRef.Value <-
        choice [ bAEqParse
                 bANEqParse
                 bALtParse
                 bALtEqParse
                 bAGtParse
                 bAGtEqParse
                 bOtherParse ]

let bTrueParse = pTrue |>> (fun _ -> TT) <?> "true"
let bFalseParse = pFalse |>> (fun _ -> FF) <?> "false"

let bNotParse =
    unop (pchar '~') bLogicalParse |>> Not <?> "Not"

let bIsDigitParse =
    pIsDigit >*>. parenthesise CexpParse |>> IsDigit
    <?> "IsDigit"

let bIsLetterParse =
    pIsLetter >*>. parenthesise CexpParse |>> IsLetter
    <?> "IsLetter"

let bIsVowelParse =
    pIsVowel >*>. parenthesise CexpParse |>> IsVowel
    <?> "IsVowel"

let bParParse = parenthesise bLogicalParse

do
    bOtherRef.Value <-
        choice [ bTrueParse
                 bFalseParse
                 bNotParse
                 bIsDigitParse
                 bIsLetterParse
                 bIsVowelParse
                 bParParse ]

let BexpParse = bLogicalParse

let stmntParse, stmntParseRef = createParserForwardedToRef<stm> ()
let stmntAtomParse, stmntAtomParseRef = createParserForwardedToRef<stm> ()

let bracketise p =
    pchar '{' >*>. p .>*> pchar '}' <?> "bracketise"

let sSeqParse =
    binop (pchar ';') stmntAtomParse stmntParse
    |>> Seq
    <?> "Seq"

do stmntParseRef.Value <- choice [ sSeqParse; stmntAtomParse ]

let sDeclareParse =
    pdeclare .>>. spaces1 >>. pid |>> Declare
    <?> "Declare"

let sIfElseParse =
    pif >*>. parenthesise BexpParse .>*> pthen
    .>*>. bracketise stmntParse
    .>*> pelse
    .>*>. bracketise stmntParse
    |>> (fun ((b, s1), s2) -> ITE(b, s1, s2))
    <?> "ITE"

let sIfParse =
    pif >*>. parenthesise BexpParse .>*> pthen
    .>*>. bracketise stmntParse
    |>> (fun (b, s) -> ITE(b, s, Skip))
    <?> "IT"

let sWhileParse =
    pwhile >*>. parenthesise BexpParse .>*> pdo
    .>*>. bracketise stmntParse
    |>> While
    <?> "While"

let sAssParse =
    binop (pstring ":=") pid AexpParse |>> Ass
    <?> "Ass"

do
    stmntAtomParseRef.Value <-
        choice [ sDeclareParse
                 sIfElseParse
                 sIfParse
                 sWhileParse
                 sAssParse ]

type word = (char * int) list
type square = Map<int, squareFun>

let parseSquareProg sqp =
    Map.map (fun _ -> run stmntParse >> getSuccess >> stmntToSquareFun) sqp

let parseBoardProg s =
    run stmntParse s |> getSuccess |> stmntToBoardFun

type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun2 }

let mkBoard (bp: boardProg) =
    let m' =
        Map.map (fun _ -> parseSquareProg) bp.squares

    { center = bp.center
      defaultSquare = Map.find bp.usedSquare m'
      squares = parseBoardProg bp.prog m' }
