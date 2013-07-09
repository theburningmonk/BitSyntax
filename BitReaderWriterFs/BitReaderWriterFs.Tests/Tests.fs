module Tests

type LoggingBuilder () =
    let log p = printfn "Expression is %A" p

    member thid.Bind(x, f) =
        log x
        f x

    member this.Return(x) =
        x

let logger = new LoggingBuilder()

let z = 
    logger {
        let! x = 42
        let! y = 43
        let! z = x + y
        return z
    }

type State<'a, 's> = State of ('s -> 'a * 's)

let runState (State s) a = s a
let getState    = State (fun s -> (s, s))
let putState s  = State (fun _ -> ((), s))

type StateBuilder () = 
    member this.Return(a) =
        State (fun s -> (a, s))
    member this.Bind(m, k) =
        State (fun s ->
            let (a, s') = runState m s
            runState (k a) s')
    member this.ReturnFrom(m) = m

let state = new StateBuilder()

let lift f = 
    state {
        let! s = getState
        return! putState (f s)
    }

type MaybeBuilder () =
    member this.Bind(x, f) = Option.bind f x
    member this.Return(x) = Some x

let maybe = new MaybeBuilder()

let divideBy bottom top =
    if bottom = 0
    then None
    else Some(top/bottom)

let divWorkflow init x y z = 
    maybe {
        let! a = init |> divideBy x
        let! b = a |> divideBy y
        let! c = b |> divideBy z
        return c
    }

type OrElseBuilder () =
    member this.ReturnFrom(x) = x
    member this.Combine(a, b) =
        match a with
        | Some _ -> a
        | None   -> b
    member this.Delay(f) = f()

let orElse = new OrElseBuilder()

let map1 = [ ("1","One"); ("2","Two") ] |> Map.ofList
let map2 = [ ("A","Alice"); ("B","Bob") ] |> Map.ofList
let map3 = [ ("CA","California"); ("NY","New York") ] |> Map.ofList

let multiLookup key = orElse {
    return! map1.TryFind key
    return! map2.TryFind key
    return! map3.TryFind key
    }

let strToInt str = match System.Int32.TryParse str with | true, x -> Some x | _ -> None

type SomeBuilder() =
    member this.Bind(m, k) = Option.bind k m
    member this.Return(x)  = Some x
let yourWorkflow = new SomeBuilder()

let stringAddWorkflow x y z = 
    yourWorkflow {
        let! a = strToInt x
        let! b = strToInt y
        let! c = strToInt z
        return a + b + c
    }

let strAdd str i = match strToInt str with | Some x -> Some <| x + i | _ -> None
let (>>=) m f = Option.bind f m

let good = strToInt "1" >>= strAdd "2" >>= strAdd "3"
let bad  = strToInt "1" >>= strAdd "xyz" >>= strAdd "3"


