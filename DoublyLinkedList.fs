module DoublyLinkedList

    type QListNode<'T> = 
        { mutable Left:QListNode<'T> option
          mutable Right:QListNode<'T> option
          mutable Content:'T
          mutable List:QList<'T> option}

    and QListNonempty<'T> =
        { mutable Front:QListNode<'T>
          mutable Back: QListNode<'T> }
    and QListVariant<'T> = Empty | Saturated of QListNonempty<'T> 
    and QList<'T> = 
        { mutable Variant:QListVariant<'T> 
          mutable Count:int }

    let emptyList<'a> () = { Variant = QListVariant<'a>.Empty; Count = 0 }

    let toSeq(x:QList<'a>) = 
        let rec toSeq : Option<QListNode<'a>> -> seq<'a> = function 
            | None -> Seq.empty
            | Some n -> toSeq' n
        and toSeq' n =
            if n.Right.IsSome then 
                seq { yield n.Content
                      yield! n.Right |> toSeq }
            else 
                seq { n.Content }

        match x.Variant with
        | Saturated s -> toSeq' s.Front
        | Empty -> Seq.empty

    let toNodeSeq(x:QList<'a>) = 
        let rec toSeq : Option<QListNode<'a>> -> seq<QListNode<'a>> = function 
            | None -> Seq.empty
            | Some n -> toSeq' n
        and toSeq' n =
            if n.Right.IsSome then 
                seq { yield n
                      yield! n.Right |> toSeq }
            else 
                seq { n }

        match x.Variant with
        | Saturated s -> toSeq' s.Front
        | Empty -> Seq.empty
        

    let add (e:'a) (ql:QList<'a>):unit = 
        let n = { Left = None; Right = None; Content=e; List = Some ql}
        match ql.Variant with
        | Empty -> 
            ql.Variant <- Saturated {Front=n; Back=n}
        | Saturated s -> 
            n.Left <- Some s.Back
            s.Back.Right <- Some n
            s.Back <- n

        ql.Count <- ql.Count + 1

    let remove (n:QListNode<'a>) = 
        let fail() = failwith $"Internal {nameof(QList<'a>)} inconsistency"
        if n.List.IsSome then
            let ql = Option.get n.List
            match n.Left, n.Right with
            | Some l, Some r -> 
                l.Right <- Some r
                r.Left <- Some l
            | Some l, None -> 
                l.Right <- None
                match ql.Variant with
                | Saturated s -> 
                    s.Back <- l
                | Empty -> fail()
            | None, Some r -> 
                r.Left <- None
                match ql.Variant with
                | Saturated s -> 
                    s.Front <- r
                | Empty -> fail()
            | None, None -> 
                match ql.Variant with
                | Saturated _ -> 
                    n.List <- Some (emptyList())
                | Empty -> fail()
            
            n.Left <- None
            n.Right <- None
            n.List <- None

            ql.Count <- ql.Count - 1

