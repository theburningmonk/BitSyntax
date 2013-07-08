namespace BitReaderWriterFs

open System
open System.Collections
open System.Collections.Generic
open System.IO

[<AutoOpen>]
module internal Helpes =
    /// helper functions for converting a BitArray
    let copyToArray<'a> unitLen (bitArr : BitArray) = 
        let len = float bitArr.Count / float unitLen |> Math.Ceiling
        let arr = Array.zeroCreate<'a> (int len)
        bitArr.CopyTo(arr, 0)
        arr

    let convertToInt32                     = copyToArray<int32> 32 >> (fun arr -> arr.[0])
    let convertToBool (bitArr : BitArray)  = bitArr.[0]
    let convertToBytes                     = copyToArray<byte> 8
    let convertToString                    = convertToBytes >> Text.Encoding.ASCII.GetString

type BitReaderBuilder (enumerator : IEnumerator<bool>) =
    let mutable bitArrays = []
    let next () = if enumerator.MoveNext() then enumerator.Current else failwithf "Insufficient bits"

    member this.Bind((n, f : BitArray -> 'a), cont : 'a -> 'b) =
        let bitArr = BitArray([| for i = 1 to n do yield next() |])
        cont(f bitArr)
        
    member this.Return x = x

/// Container for helper functions 
type Bits =
    static member Skip (n : int)       = n, (fun _ -> ())
    static member ReadInt (n : int)    = n, convertToInt32
    static member ReadBool ()          = 1, convertToBool
    static member ReadBytes (n : int)  = n, convertToBytes
    static member ReadString (n : int) = n, convertToString

[<AutoOpen>]
module ReaderMonad = 
    let bitReader (stream : Stream) = 
        stream.Position <- 0L
        BitReaderBuilder(stream.ToBitSequence().GetEnumerator())