
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

let xx = B + (A * (- B)) --> B

printfn "%A" xx
printfn "%s" (xx.toPrettyString())

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



// open System.Text.RegularExpressions

// let rx = Regex("\s+.+\s", RegexOptions.None )

// let str = "      (dd    dd   aa)"

// let out = rx.Match(str)

// printfn "Out %A: " out.Groups


// let s = "You win     some. You lose some.";

// let  subs = s.Split();

// printfn "%A" subs


// printfn "Index %A" m.Index
// printfn "%A" m.Success
// printfn "%A" m.Groups



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
        seq { yield ID s }
    | LookAhead1(SP,t) -> 
        seq { yield ID s; yield! skipBlanks t}
    | LookAhead2(DASH,GT,t) -> proceed (BinOp Impl) t
    | LookAhead1('(',t) -> proceed LP t
    | LookAhead1(')',t) -> proceed RP t
    | LookAhead1('&',t) -> proceed (BinOp And) t
    | LookAhead1('|',t) -> proceed (BinOp Or) t
    | LookAhead1('-',t) -> proceed Not t
    | LookAhead1(h,t) ->  recognize t $"{s}{h}"
   

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

    
    
printfn "Stage 1:"
let tokens = skipBlanks "hello -world  --(  bl | ah )  I & am  he->re   now" 

for s in tokens do 
    printfn "%A" s

printfn "Stage 2:"
let res = validate tokens 

match res with
| Error e -> 
    printfn "%A" e
| _ -> 
    printfn "Success"



// TODO:
// 1. Add tokenizing code for XOR operator as |+|
// 2. Modify validator code to report on all bad identifiers that occur
// 3. Have the program take input from the command line instead of a constant string
// 4. Think about using the same techniques to construct a syntax tree from the string








