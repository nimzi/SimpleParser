
type BinOp = 
    And | Or | Impl | Xor 
    member x.toString() =
        match x with 
        | And -> "and"
        | Or -> "or"
        | Impl -> "->"
        | Xor -> "xor"

type Expr = 
    | Symbol of string
    | BinOp of BinOp * Expr * Expr 
    | Not of Expr 

    static member (+) (x, y) = BinOp (Or, x,y)
    static member (*) (x,y) = BinOp (And, x,y)
    static member (-->) (x,y) = BinOp (Impl, x,y)
    static member (~-) (x) = Not x

    member x.toPrettyString() =
        match x with 
        | Symbol a -> $"`{a}`"
        | BinOp (op, a, b) -> $"({a.toPrettyString()} {op.toString()} {b.toPrettyString()})"
        | Not a -> "(- " + a.toPrettyString() + ")"
    


let A = Symbol "A"
let B = Symbol "B"
let nB = - B

let rec allSymbols e = 
    seq {
        match e with 
        | Symbol s -> 
            yield s
        | BinOp(op, a, b)->
            yield! allSymbols a
            yield! allSymbols b
        | Not(a) -> 
            yield! allSymbols a
    }


type Environment = Map<string, bool>

let check (e:Environment) (syms:seq<string>) = Set.ofSeq <| Map.keys e = Set.ofSeq syms

let rec eval (e:Environment) (expr:Expr) = 
    if check e (allSymbols expr) then 
        match expr with
        | Not a -> not (eval e a)
        | _ -> failwith "not implemented"
    else 
        failwith "unbound symbols"



let (|SeqEmpty|LookAhead1|) (xs: 'a seq) = 
  if Seq.isEmpty xs then SeqEmpty
  else LookAhead1(Seq.head xs, Seq.skip 1 xs)


let (|LookAhead2|_|) (xs: 'a seq) =
    match xs with
    | SeqEmpty -> None
    | LookAhead1 (h, t) -> 
        match t with 
        | SeqEmpty -> None
        | LookAhead1 (h2, t) -> Some (h,h2, t)

let (|LookAhead3|_|) (xs: 'a seq) =
    match xs with
    | LookAhead2 (h, h2, t) -> 
        match t with 
        | SeqEmpty -> None
        | LookAhead1 (h3, t) -> Some (h,h2, h3, t)
    | _ -> None

let (|LookAhead4|_|) (xs: 'a seq) =
    match xs with
    | LookAhead3 (h, h2,h3, t) -> 
        match t with 
        | SeqEmpty -> None
        | LookAhead1 (h4, t) -> Some (h,h2,h3,h4, t)
    | _ -> None

let (|LookAhead5|_|) (xs: 'a seq) =
    match xs with
    | LookAhead4 (h, h2,h3,h4, t) -> 
        match t with 
        | SeqEmpty -> None
        | LookAhead1 (h5, t) -> Some (h,h2,h3,h4, h5, t)
    | _ -> None
    





type Token = 
    | LP   // (
    | RP   // )
    | ID of string
    | BinOp of BinOp
    | Not



[<Literal>]
let SP = ' '

[<Literal>]
let DASH = '-'

[<Literal>]
let GT = '>'

let isWhiteSpace = function 
    | ' ' | '\n' | '\t' -> true
    | _ -> false

let digits = Set.ofSeq "1234567890"
let charsStr = "abcdefghijklmnopqrstuvw"
let lowerChars = Set.ofSeq charsStr
let upperChars = Set.ofSeq <| charsStr.ToUpper()

let letters = Set.union lowerChars upperChars
let lettersAndDigits = Set.union letters digits

let isIdentifier (s:string) =
    let contains = Set.contains
    let forall = Seq.forall

    s.Length > 0 && 
    letters.Contains s[0] &&
    s[1..] |> forall (lettersAndDigits.Contains)

    

let inline (++) (a:char) (b:seq<char>) = Seq.append (Seq.singleton a) b 


let rec skipBlanks (l:seq<char>) = seq {
    match l with 
    | LookAhead2(a,b,tail) when isWhiteSpace a && isWhiteSpace b-> yield! skipBlanks (b ++ tail)
    | LookAhead1(a,tail) when isWhiteSpace a -> yield! recognize tail ""
    | _->  yield! recognize l ""}


and recognize l s :seq<Token> = 
    let nonEmptyS = [if s <> "" then yield ID s]
    let proceed (ss:Token) t = seq {
        yield! nonEmptyS
        yield ss
        yield! skipBlanks t
    }

    match l with
    | SeqEmpty -> 
        seq { yield! nonEmptyS}
    | LookAhead1(a,t) when isWhiteSpace a -> 
        seq { yield! nonEmptyS; yield! skipBlanks t}
    | LookAhead2(DASH,GT,t) -> proceed (BinOp Impl) t
    | LookAhead1('(',t) -> proceed LP t
    | LookAhead1(')',t) -> proceed RP t
    | LookAhead1('&',t) -> proceed (BinOp And) t
    | LookAhead1('|',t) -> proceed (BinOp Or) t
    | LookAhead1('-',t) -> proceed Not t
    | LookAhead1(h,t) ->  recognize t $"{s}{h}" // ?
   

type TokenizationError = BadIdentifier of string 
type TokenizationResult = Result< seq<Token>, TokenizationError >


let validate (tokensIn:seq<Token>) : TokenizationResult =
    let rec validate' (tokens:seq<Token>) : TokenizationResult = 
        match tokens with      
        | SeqEmpty -> Result.Ok (tokensIn)
        | LookAhead1 (ID ident,t) -> 
            if isIdentifier ident then 
                validate' t
            else 
                Result.Error <| BadIdentifier ident
        | LookAhead1 (_, t) ->
            validate' t
          

    validate' tokensIn


// brings in option CE
open FsToolkit.ErrorHandling

let matchToken (t:Token) (l:seq<Token>) = 
    match l with 
    | LookAhead1 (t, tail) -> tail |> Some
    | _-> None

let rec parseTerm(stream:seq<Token>): Option<Expr * seq<Token>> = 
    match stream with 
    | LookAhead2(Not, ID x, tail) -> (Expr.Not(Symbol x) , tail) |> Some
    | LookAhead1(ID x, tail) -> (Symbol x , tail) |> Some
//    | LookAhead3(ID x, BinOp op, ID y) -> 
//    | LookAhead5(LP, ID l, BinOp op, ID r, RP, tail ) -> ( Expr.BinOp (op, Symbol l, Symbol r), tail) |> Some
    | _ -> None

and parseExp(stream:seq<Token>):Option<Expr * seq<Token>> =
    let mainCase () =
        match stream with 
        | SeqEmpty -> None // maybe give it more thought
        | LookAhead1(Not, tail) ->
            option {
                let! (e,t) = parseExp(tail) 
                return (Expr.Not(e), t)
            }
        | LookAhead1(LP, tail) -> // left paren case
            option {
                let! firstExp, next = parseExp(tail)
                match next with
                | LookAhead1 (RP, rest) -> // (E) case
                    return firstExp, rest
                | LookAhead1 (BinOp op, rest) -> // (E op E) case
                    let! secondExp, penultimateTail = parseExp rest
                    let! ultimateTail = matchToken RP penultimateTail
                    return Expr.BinOp(op,firstExp,secondExp), ultimateTail
                | _-> 
                    return! None
            }
        | _ -> None
        

    match parseTerm stream with
    | Some (e, tail) -> Some (e, tail) // also should look if at end maybe?
    | None -> mainCase()



    


let input = "(--((A|-B) | D) -> -(C & A))" 

printfn "Input: %s" input 
printfn ""
printfn "Stage 1: Just tokenizing"
let tokens = skipBlanks input
for s in tokens do 
    printf " %A, " s
printfn ""
printfn ""

printfn "Stage 2: Validating identifiers"
match validate tokens with
| Error e -> 
    printfn "%A" e
| Ok x -> 
    printfn "Success"
    printfn ""
    printfn "State 3: Parsing"
    match parseExp x with
    | Some (ee, tokens) -> 
        printfn "Pretty AST: %s" (ee.toPrettyString())
        printfn "Tail: %A" tokens
    | _-> 
        printfn "Parsing error"










