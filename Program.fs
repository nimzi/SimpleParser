open BlackFox.ColoredPrintf
open PrecParsing

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
    | Paren of Expr
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
        | Not a -> "~" + a.toPrettyString() 
        | Paren e -> $"[{e.toPrettyString()}]"
    

let rec allSymbols e = 
    seq {
        match e with 
        | Symbol s -> 
            yield s
        | Paren e -> 
            yield! allSymbols e
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

colorprintfn "$yellow[Sample Input:] %s" input 
printfn ""
colorprintfn "$yellow[Tokenizing:]"
let tokens = skipBlanks input
for s in tokens do 
    printf " %A, " s
printfn ""
printfn ""

colorprintf "$yellow[Validating identifiers:] "
match validate tokens with
| Error e -> 
    colorprintfn "$red[%A]" e
| Ok x -> 
    colorprintfn "$green[Success]"
    printfn ""
    colorprintfn "$yellow[Parsing using LL(k) strategy...]"
    match parseExp x with
    | Some (ee, tokens) -> 
        printfn "Pretty AST: %s" (ee.toPrettyString())
        printfn "Tail: %A" tokens
    | _-> 
        colorprintfn "$red[Parsing error]"


printfn ""
colorprintfn "$yellow[Parsing using rewrite rules...]"

type TokenOrExp = Token of Token | Expr of Expr

let initial = tokens |> Seq.map Token |> List.ofSeq


let rec transform0 (stream:List<TokenOrExp>) = [
    match stream with
    | [] -> ()
    | Token LP::TokenOrExp.Expr e::Token RP::rest ->
        yield TokenOrExp.Expr (Paren e)
        yield! transform0 rest
    | Expr a::Token(BinOp op)::Expr b::rest -> 
        let xx = Expr.BinOp (op,a,b)
        yield TokenOrExp.Expr xx
        yield! transform0 rest
    | Token Not::Expr e::rest  ->
        yield Expr (Expr.Not (e)) 
        yield! transform0 rest
    | a::rest -> 
        yield a
        yield! transform0 rest
]

let rec transform (stream:List<TokenOrExp>) = [
    match stream with
    | [] -> ()
    | Token (Token.Not)::Token (ID x)::rest ->
        yield TokenOrExp.Expr <| Expr.Not(Symbol x)
        yield! transform rest      
    | Token (ID x)::Token(BinOp op)::Token(ID y)::rest -> 
        yield TokenOrExp.Expr <| Expr.BinOp (op, Symbol x, Symbol y)
        yield! transform rest
    | Token (Token.ID x)::rest ->
        yield Expr (Symbol x) 
        yield! transform rest
    | a::rest -> 
        yield a
        yield! transform rest
    ]

let rec improveExpression (e:Expr) =
    match e with
    | Expr.BinOp (op, a,b) -> Expr.BinOp (op, improveExpression a, improveExpression b)
    | Expr.Not(Expr.Not(e)) -> improveExpression e
    | Expr.Not(Expr.BinOp (BinOp.Or,a,b)) -> Expr.BinOp (And, Expr.Not (improveExpression a), Expr.Not (improveExpression b) )
    | Expr.Not(Expr.BinOp (BinOp.And,a,b)) -> Expr.BinOp (Or, Expr.Not (improveExpression a), Expr.Not (improveExpression b) )
    | _ -> e


let start = transform initial 

let rec finish l counter = 
    let ll = transform0 l
    if ll <> l then 
        finish ll (counter+1)
    else 
        ll, counter


let stuff, count = finish start 0
for e in stuff do
    match e with
    | Expr ee -> printfn "Pretty: %s" (ee.toPrettyString())
    | Token tt -> printfn "Token: %A" tt


// Gather top-level operators
let rec collectOps (ast:Expr) = // (oplist: BinOp list) = 
    match ast with
    | Expr.BinOp (op, l, r) -> 
        (collectOps l) @ [op] @ (collectOps r)
    | _ -> []
let rec collectTrees (ast:Expr) = // (oplist: BinOp list) = 
    match ast with
    | Expr.BinOp (op, l, r) -> 
        (collectTrees l) @ (collectTrees r)
    | x -> [x]

match stuff with 
| [Expr e] -> 
    colorprintfn "$green[Successfuly parsed expression:] %s" <| e.toPrettyString()
    colorprintfn "$green[Improved expression:] %s"  <| improveExpression(e).toPrettyString()
  | _ -> 
    colorprintfn "$red[Failed to parse the expression]"


colorprintfn "$green[%A passes for %A tokens]" count (List.length initial)
printfn ""

// run multipass parser in repl mode
colorprintfn "$green[REPL for predicate logic. Compose expressions using operators &, |, and ->, also unary - :]"
colorprintfn "$green[Type #quit to exit, #line for last input, #tokens for tokens]"
let mutable should = true
let mutable line = ""

while should do 
    colorprintf "$green[>>] "
    let command = System.Console.ReadLine()
    if command = "#quit" then 
        should <- false
    else if command = "#line" then
        colorprintfn "$yellow[%s]" line
    else if command = "#tokens" then
        let tokens = skipBlanks line
        colorprintf "$yellow[Tokens:] "
        for s in tokens do
            printf " %A, " s
        printfn ""
    else
        line <- command
        //printfn "You entered: %s" line
        let tokens = skipBlanks line

        match validate tokens with
        | Ok stream ->
            colorprintfn "$green[Valid identifiers]"
            let start = tokens |> Seq.map Token |> List.ofSeq |> transform

            let rec finish l counter = 
                let ll = transform0 l
                if ll <> l then 
                    finish ll (counter+1)
                else 
                    ll, counter

            let out, count = finish start 0

            match out with
            | [Expr e] -> 
                colorprintfn "$green[Parsed expression: %s]" <| e.toPrettyString()

                let ops = collectOps e
                printfn "Top-level ops: %A" (ops |> List.map (fun x -> x.toString()) |> String.concat ", ")

                if not ops.IsEmpty then
                    let trees = collectTrees e 
                    printfn "Top-level trees:"
                    for t in trees do
                        printfn "%s" <| t.toPrettyString()
                    
                    let exp = {
                        Operators = Array.ofList ops
                        Atoms = Array.ofList trees
                        }

                    let precedence = function
                    | BinOp.Impl -> 0
                    | BinOp.Or -> 1
                    | BinOp.And -> 2
                    | BinOp.Xor -> 3

                    let uu = parse precedence exp

                    let rec copyTree (t:GenTree<Expr,BinOp>) =
                        match t with
                        | Val v -> v
                        | Node (op, t1, t2) -> Expr.BinOp (op, copyTree t1, copyTree t2)

                    let tt = copyTree uu

                    colorprintfn "$green[Adjusted exp: %s]" <| tt.toPrettyString()
            | _ ->
                colorprintfn "$red[Parsing error]"

        | Error e ->
            colorprintfn "$red[%A]" e

