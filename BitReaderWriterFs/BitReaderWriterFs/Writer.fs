module BitReaderWriterFsWriter

open System
open System.Collections
open System.Collections.Generic
open System.IO

open BitReaderWriterFs

module Seq =
    let chunk (n : int) (source : 'a seq) =
        let enumerator = source.GetEnumerator()

        let getChunk () = 
            seq {
                for i = 1 to n do 
                    if enumerator.MoveNext() then yield enumerator.Current 
                    else failwith "Insufficient elements in sequence"
            }

        seq { while enumerator.MoveNext() do yield getChunk() }

type BitWriterBuilder () =
    member x.Bind(value : seq<bool>, cont : unit -> seq<bool> -> seq<bool>) = 
        ()
    member x.Return (bits : seq<bool>) = ()
        

let bitWriter = BitWriterBuilder()

let writeInt (n, value : int) = 
    BitConverter.GetBytes(value) 
    |> Seq.collect (fun b -> b.ToBitSequence()) 
    |> Seq.take n

let stream = new MemoryStream()
//let writeMsg = bitWriter {
//        do! writeInt(4, 119)
//
//
//        return
//    }