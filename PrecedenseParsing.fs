module PrecParsing


open System
open DoublyLinkedList


//  type MyInterface<'Op> =
//     abstract member precedence : 'Op -> int
//     abstract member name: 'Op -> string
    


type Op = Plus | Minus | Mul | Div

let precedence = function
| Plus -> 0
| Minus -> 0
| Mul -> 1
| Div -> 1

let name = function
| Plus -> "+"
| Minus -> "-"
| Mul -> "*"
| Div -> "/"



type GenNodeData<'OperatorT, 'ValueT> = 'OperatorT * GenTree<'ValueT,'OperatorT> * GenTree<'ValueT,'OperatorT>
and GenTree<'ValueT,'OperatorT> = Val of 'ValueT | Node of GenNodeData<'OperatorT, 'ValueT>

// type NodeData = GenNodeData<Op, string> //Op * Tree * Tree
// type Tree = GenTree<string, Op> //Val of string | Node of NodeData

// print out parenthesized tree
let rec show<'Op, 'Val>  (nameF:'Op->string) (t:GenTree<'Val,'Op>) = 
    match t with 
    | GenTree.Node( o, l, r) -> $"({show nameF l} {nameF o} {show nameF r})" 
    | GenTree.Val v -> string v


// Pre-parsed expression into operators and atoms
// a + b - c * d  expression will be represented as 
// an opertor array [+,-,*] and atom array [a,b,c,d]
type OperatorChain<'Op,'Val> = 
    { Operators: 'Op array
      Atoms: 'Val array }

let parse<'Op,'Val> (precedence:'Op -> int) (e:OperatorChain<'Op,'Val>)  =
    // list of parsed expressions
    // building an initial list
    let top = emptyList():QList<GenNodeData<'Op,'Val>>
    let bottom = e.Atoms |> Array.map Val
    for i in 0..(e.Operators.Length-1) do
        top |> add (e.Operators.[i], bottom.[i], bottom.[i+1]) 

    // sort operators by precedence (stable sort!)
    let queue = (toNodeSeq top) |> List.ofSeq
    let sort a b = List.sortByDescending b a
    let queue = (sort queue) <| fun n -> 
        let op, _, _ = n.Content
        precedence op

    // Sift an operator down the tree
    let consolidate (n:QListNode<GenNodeData<'Op,'Val>>) =
        let t = Node(n.Content)

        match n.Left with
        | Some l -> 
            let o, lt, _ = l.Content
            l.Content <- o, lt, t
        | _ -> ()

        match n.Right with
        | Some r -> 
            let o, _, rt = r.Content
            r.Content <- o, t, rt
        | _ -> ()

        remove n
        t


    let rec processQueue:QListNode<GenNodeData<'Op,'Val>> list -> GenTree<'Val,'Op> = function
        | [h] -> consolidate h
        | h::tail -> 
            consolidate h |> ignore
            processQueue tail
        | [] -> failwith "Empty queue unsupported"

    processQueue queue



//[<EntryPoint>]
let main argv =
    let exp =
        { Operators = [| Plus; Minus; Mul; Div; Plus; Div; Minus |]
          Atoms = [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" |] }

    let stringify (e) =
        let ops = (e.Operators |> Array.map name) |> Array.append [|""|]
        let pairs = (ops, e.Atoms) ||> Array.map2 (fun x y -> $" {x} {y}")
        pairs |> String.Concat


    exp |> stringify |> printfn "Input expression: %s" 
    exp |> parse precedence |> show name |> printfn "Order of eval: %A"

    0