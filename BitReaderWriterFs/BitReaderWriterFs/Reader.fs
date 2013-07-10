namespace BitReaderWriterFs

open System
open System.Collections
open System.Collections.Generic
open System.IO

[<AutoOpen>]
module internal Helpes =
    /// Converts a BitArray to a byte array of length n
    let toByteArray n (bitArr : BitArray) =
        let bytes = Array.zeroCreate<byte> n
        for i = 0 to bitArr.Count-1 do
            if bitArr.[i] 
            then bytes.[i/8] <- bytes.[i/8] <<< 1 ||| 1uy 
            else bytes.[i/8] <- bytes.[i/8] <<< 1
        bytes

    let convertBytes f byteArray    = f(byteArray, 0)

    let convertToInt16              = toByteArray 2 >> (convertBytes BitConverter.ToInt16)
    let convertToUint16             = toByteArray 2 >> (convertBytes BitConverter.ToUInt16)
    let convertToInt32              = toByteArray 4 >> (convertBytes BitConverter.ToInt32)
    let convertToUint32             = toByteArray 4 >> (convertBytes BitConverter.ToUInt32)
    let convertToInt64              = toByteArray 8 >> (convertBytes BitConverter.ToInt64)
    let convertToUint64             = toByteArray 8 >> (convertBytes BitConverter.ToUInt64)

    let convertToFloat              = toByteArray 4 >> (convertBytes BitConverter.ToSingle)
    let convertToDouble             = toByteArray 8 >> (convertBytes BitConverter.ToDouble)

    let convertToBool (bitArr : BitArray)  = bitArr.[0]
    let convertToBytes (bitArr : BitArray) = 
        let len = bitArr.Count / 8 + (if bitArr.Count % 8 <> 0 then 1 else 0)
        toByteArray len bitArr
    let convertToChar                      = toByteArray 1  >> Text.Encoding.Default.GetChars >> (fun arr -> arr.[0])
    let convertToString                    = convertToBytes >> Text.Encoding.Default.GetString

type BitsCount = | N of int | Rest
type BitReader<'a> = BitReader of BitsCount * (BitArray -> 'a)

type BitReaderBuilder (enumerator : IEnumerator<bool>) =
    let mutable bitArrays = []
    let next () = if enumerator.MoveNext() then enumerator.Current else failwithf "Insufficient bits"
    let rest () = [| while enumerator.MoveNext() do yield enumerator.Current |]

    member this.Bind(BitReader(count, f), cont : 'a -> 'b) =
        let bitArr = match count with
                     | N n  -> BitArray([| for i = 1 to n do yield next() |])
                     | Rest -> BitArray(rest())
        cont(f bitArr)
        
    member this.Return x = x

/// Container for helper functions
type Bits =
    static member Skip n            = BitReader <| (N n, (fun _ -> ()))

    static member ReadInt16 (?n)    = BitReader <| (N(defaultArg n 16), convertToInt16)
    static member ReadUint16 (?n)   = BitReader <| (N(defaultArg n 16), convertToUint16)
    static member ReadInt32 (?n)    = BitReader <| (N(defaultArg n 32), convertToInt32)
    static member ReadUint32 (?n)   = BitReader <| (N(defaultArg n 32), convertToUint32)    
    static member ReadInt64 (?n)    = BitReader <| (N(defaultArg n 64), convertToInt64)
    static member ReadUint64 (?n)   = BitReader <| (N(defaultArg n 64), convertToUint64)

    static member ReadBool ()       = BitReader <|(N 1, convertToBool)
    static member ReadBytes n       = BitReader <|(N n, convertToBytes)
    static member ReadChar (?n)     = BitReader <|(N(defaultArg n 8), convertToChar)
    static member ReadString n      = BitReader <|(N n, convertToString)

    static member ReadAs n f        = BitReader <| (N n, convertToBytes >> f)
    static member ReadRest ()       = BitReader <| (Rest, convertToBytes)

[<AutoOpen>]
module ReaderWorkflow = 
    let bitReader (stream : Stream) = 
        stream.Position <- 0L
        BitReaderBuilder(stream.ToBitSequence().GetEnumerator())