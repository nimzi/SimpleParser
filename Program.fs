
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



let s = ""

let l = List.ofSeq s




[<Literal>]
let SP = ' '

[<Literal>]
let DASH = '-'

[<Literal>]
let GT = '>'

let isWhiteSpace = function 
    | ' ' | '\n' | '\t' -> true
    | _ -> false

    
let rec skipBlanks (l:List<char>) = seq {
    match l with 
    | a::b::tail when isWhiteSpace a && isWhiteSpace b-> yield! skipBlanks (b::tail)
    | a::tail when isWhiteSpace a -> yield! recognize tail ""
    | _->  yield! recognize l ""}


and recognize l s = 
    let nonEmptyS = [if s <> "" then yield s]
    let proceed ss t = seq {
        yield! nonEmptyS
        yield ss
        yield! skipBlanks t
    }

    match l with
    | [] -> 
        seq { yield s }
    | SP::t -> 
        seq { yield s; yield! skipBlanks t}
    | DASH::GT::t -> 
        proceed "->" t
    | '('::t | ')'::t -> 
        proceed $"{l.Head}" t
    | h::t -> 
        recognize t $"{s}{h}"
   




for s in skipBlanks (List.ofSeq "hello world  (blah)  I am  he->re   now") do
    printfn "%A" s
    


// let qq () = seq {
//     printfn "calling q"
//     yield 55
// } 

// let rec ss (n:int) = seq {
//     printfn "Calling ss with %A" n
//     yield n
//     yield! (ss (n-1))

// }


// let uu = Seq.cache <| ss 12
// let tt = (Seq.tail (Seq.tail uu))
// let _ = Seq.head tt

// printfn "-----"

// let vv = Seq.tail tt
// let _ = Seq.head vv



let ss = "hello world"

let aa = Seq.tail (Seq.tail ss)

printfn "tail tail : %A" <| Seq.head aa












