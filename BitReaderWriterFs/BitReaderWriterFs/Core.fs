module BitReaderWriterFs

open System
open System.Collections
open System.Collections.Generic
open System.IO

open BitReaderWriterCs

/// Helper extension methods
[<AutoOpen>]
module internal Ext =
    type Byte with
        member this.ToBitSequence () = seq { for i in 7..-1..0 -> ((this >>> i) &&& 1uy) = 1uy }

    type Stream with
        member this.ToBitSequence () = 
            let readByte = ref 0
            seq {
                while (readByte := this.ReadByte(); !readByte >= 0) do
                    for i in 7..-1..0 -> ((!readByte >>> i) &&& 1) = 1
            }

    type BitArray with
        member this.Dump() =
            String.Join(",", seq { for flag in this -> flag.ToString().ToLower() } |> Seq.toArray)

module Seq =
    let chunk n (source : seq<'a>) = 
        use ie = source.GetEnumerator()
        let sourceIsEmpty = ref false
        let rec loop () =
            seq {
                if ie.MoveNext () then
                    yield [| yield ie.Current
                             for x in 2 .. n do
                                if ie.MoveNext() then yield ie.Current
                                else sourceIsEmpty := true |]
                    
                    match !sourceIsEmpty with
                    | false -> yield! loop ()
                    | true  -> ()
            }
        loop ()

type Count      = | N of int | Rest
type Reader<'a> = Reader of Count * (BitArray -> 'a)

type BitReader private () =
    static let toByteArray n (bitArr : BitArray) =
        let bytes = Array.zeroCreate<byte> n
        for i = 0 to bitArr.Count-1 do
            if bitArr.[i] 
            then bytes.[i/8] <- bytes.[i/8] <<< 1 ||| 1uy 
            else bytes.[i/8] <- bytes.[i/8] <<< 1
        bytes

    static let convertBytes f byteArray    = f(byteArray, 0)

    static let convertToInt16       = toByteArray 2 >> (convertBytes BitConverter.ToInt16)
    static let convertToUint16      = toByteArray 2 >> (convertBytes BitConverter.ToUInt16)
    static let convertToInt32       = toByteArray 4 >> (convertBytes BitConverter.ToInt32)
    static let convertToUint32      = toByteArray 4 >> (convertBytes BitConverter.ToUInt32)
    static let convertToInt64       = toByteArray 8 >> (convertBytes BitConverter.ToInt64)
    static let convertToUint64      = toByteArray 8 >> (convertBytes BitConverter.ToUInt64)
                                    
    static let convertToFloat       = toByteArray 4 >> (convertBytes BitConverter.ToSingle)
    static let convertToDouble      = toByteArray 8 >> (convertBytes BitConverter.ToDouble)

    static let convertToBool (bitArr : BitArray)  = bitArr.[0]
    static let convertToBytes (bitArr : BitArray) = 
        let len = bitArr.Count / 8 + (if bitArr.Count % 8 <> 0 then 1 else 0)
        toByteArray len bitArr
    static let convertToChar        = toByteArray 1  >> Text.Encoding.UTF8.GetChars >> (fun arr -> arr.[0])
    static let convertToString      = convertToBytes >> Text.Encoding.UTF8.GetString
    
    static member Skip n            = Reader(N n, (fun _ -> ()))

    static member ReadInt16 (?n)    = Reader(N(defaultArg n 16), convertToInt16)
    static member ReadUint16 (?n)   = Reader(N(defaultArg n 16), convertToUint16)
    static member ReadInt32 (?n)    = Reader(N(defaultArg n 32), convertToInt32)
    static member ReadUint32 (?n)   = Reader(N(defaultArg n 32), convertToUint32)
    static member ReadInt64 (?n)    = Reader(N(defaultArg n 64), convertToInt64)
    static member ReadUint64 (?n)   = Reader(N(defaultArg n 64), convertToUint64)
    static member ReadFloat (?n)    = Reader(N(defaultArg n 32), convertToFloat)
    static member ReadDouble (?n)   = Reader(N(defaultArg n 64), convertToDouble)

    static member ReadBool ()       = Reader(N 1, convertToBool)
    static member ReadBytes n       = Reader(N n, convertToBytes)
    static member ReadChar (?n)     = Reader(N(defaultArg n 8), convertToChar)
    static member ReadString n      = Reader(N n, convertToString)

    static member ReadAs (n, f)     = Reader(N n, convertToBytes >> f)
    static member ReadRest ()       = Reader(Rest, convertToBytes)

type BitWriter private () =
    static let isLittle = BitConverter.IsLittleEndian

    static let toBitArray n (bytes : byte[]) =
        let rec take n idx = 
            seq {
                if idx < bytes.Length && n > 0 then
                    let n' = min n 8
                    yield! bytes.[idx].ToBitSequence() |> Seq.skip (8 - n) |> Seq.take n'
                    yield! take (n - n') (idx + 1)
            }
        let bits = take n 0 |> Seq.toArray
        BitArray(bits)

    static member toByte (bits : bool[]) =
        let mutable byte = 0uy
        for i = 0 to (min 7 bits.Length-1) do
            if bits.[i] 
            then byte <- byte <<< 1 ||| 1uy 
            else byte <- byte <<< 1
        byte

    static member WriteInt16  (x : int16, ?n)   = toBitArray (defaultArg n 16) <| BitConverter.GetBytes(x)
    static member WriteUint16 (x : uint16, ?n)  = toBitArray (defaultArg n 16) <| BitConverter.GetBytes(x)
    static member WriteInt32  (x : int, ?n)     = toBitArray (defaultArg n 32) <| BitConverter.GetBytes(x)
    static member WriteUint32 (x : uint32, ?n)  = toBitArray (defaultArg n 32) <| BitConverter.GetBytes(x)
    static member WriteInt64  (x : int64, ?n)   = toBitArray (defaultArg n 64) <| BitConverter.GetBytes(x)
    static member WriteUint64 (x : uint64, ?n)  = toBitArray (defaultArg n 64) <| BitConverter.GetBytes(x)
    static member WriteFloat  (x : float, ?n)   = toBitArray (defaultArg n 32) <| BitConverter.GetBytes(x)
    static member WriteDouble (x : double, ?n)  = toBitArray (defaultArg n 64) <| BitConverter.GetBytes(x)
    
    static member WriteBool   (x : bool)        = BitArray([| x |]) 
    static member WriteBytes  (n, [<ParamArray>] x : byte[]) = toBitArray n x
    static member WriteChar   (x : char, ?n)    = toBitArray (defaultArg n 8) <| Text.Encoding.UTF8.GetBytes([| x |])
    static member WriteString (x : string, ?n)  = toBitArray (defaultArg n (x.Length * 8)) <| Text.Encoding.UTF8.GetBytes(x)

    /// Flushing a sequence of BitArray to the specified stream
    static member Flush (stream : Stream) (bitArrays : seq<BitArray>) =
        let bits = 
            bitArrays 
            |> Seq.collect (fun bitArray -> seq { for n = 0 to bitArray.Length - 1 do yield bitArray.[n] })
            |> Seq.toArray

        bits
        |> Seq.chunk 8 
        |> Seq.map BitWriter.toByte
        |> Seq.iter stream.WriteByte

        stream.Flush()

/// Reader workflow
[<AutoOpen>]
module ReaderWorkflow =
    type BitReaderBuilder (enumerator : IEnumerator<bool>) =
        let mutable bitArrays = []
        let next () = if enumerator.MoveNext() then enumerator.Current else failwithf "Insufficient bits"
        let rest () = [| while enumerator.MoveNext() do yield enumerator.Current |]

        let read (Reader(count, f)) =
            let bitArr = match count with
                         | N n  -> BitArray([| for i = 1 to n do yield next() |])
                         | Rest -> BitArray(rest())
            f bitArr

        member this.Bind(x, cont : 'a -> 'b) = cont(read x)
        
        member this.Return x     = x
        member this.ReturnFrom x = read x

    let bitReader (stream : Stream) = BitReaderBuilder(stream.ToBitSequence().GetEnumerator())