namespace BitReaderWriterFs

open System
open System.IO

module BitWriterWorkflow =
    open BitReaderWriterCs

    /// A writer represents the number of bits to take from the byte array
    type Writer = Writer of int * byte[]

    let inline getNumBytes n = n / 8 + (if (n % 8 > 0) then 1 else 0)
    let inline getWriter endianness n (wrapper : ^a) =
        let bytes = (^a : (member GetBytes : Endianness -> seq<byte>) (wrapper, endianness))
        Writer(n, bytes |> Seq.take (getNumBytes n) |> Seq.toArray)

    let writeInt16 endianness n x   = new Int16Wrapper(Instance = x)  |> getWriter endianness n        
    let writeUint16 endianness n x  = new Uint16Wrapper(Instance = x) |> getWriter endianness n
    let writeInt32 endianness n x   = new Int32Wrapper(Instance = x)  |> getWriter endianness n
    let writeUint32 endianness n x  = new Uint32Wrapper(Instance = x) |> getWriter endianness n
    let writeInt64 endianness n x   = new Int64Wrapper(Instance = x)  |> getWriter endianness n
    let writeUint64 endianness n x  = new Uint64Wrapper(Instance = x) |> getWriter endianness n
    let writeFloat endianness n x   = new FloatWrapper(Instance = x)  |> getWriter endianness n
    let writeDouble endianness n x  = new DoubleWrapper(Instance = x) |> getWriter endianness n
    let writeByte n x               = Writer(n, [| x |])
    let writeBytes n (x : byte[])   = Writer(n, x)
    let writeBool x                 = if x then writeByte 1 1uy else writeByte 1 0uy
    let writeChar n x               = Writer(n, Text.Encoding.UTF8.GetBytes([| x |]))
    let writeString n (x : string)  = Writer(n, Text.Encoding.UTF8.GetBytes(x))

    type BitWriter private () =
        static let defaultEndianness = Endianness.Little

        static member private Write (x : 'a, defaultSize, f, ?n, ?endianness) =            
            f (defaultArg endianness defaultEndianness) (min defaultSize <| defaultArg n defaultSize) x

        static member WriteInt16 (x, ?n, ?endianness)  = BitWriter.Write(x, 16, writeInt16, ?n = n, ?endianness = endianness)
        static member WriteUint16 (x, ?n, ?endianness) = BitWriter.Write(x, 16, writeUint16, ?n = n, ?endianness = endianness)
        static member WriteInt32 (x, ?n, ?endianness)  = BitWriter.Write(x, 32, writeInt32, ?n = n, ?endianness = endianness)
        static member WriteUint32 (x, ?n, ?endianness) = BitWriter.Write(x, 32, writeUint32, ?n = n, ?endianness = endianness)
        static member WriteInt64 (x, ?n, ?endianness)  = BitWriter.Write(x, 64, writeInt64, ?n = n, ?endianness = endianness)
        static member WriteUint64 (x, ?n, ?endianness) = BitWriter.Write(x, 64, writeUint64, ?n = n, ?endianness = endianness)
        static member WriteFloat x                     = BitWriter.Write(x, 32, writeFloat)
        static member WriteDouble x                    = BitWriter.Write(x, 64, writeDouble)
            
        static member WriteBool   x                    = writeBool x
        static member WriteByte   (x, ?n)              = writeByte (defaultArg n 8) x
        static member WriteBytes  (x : byte[], ?n)     = writeBytes (defaultArg n <| x.Length * 8) x
        static member WriteChar   x                    = writeChar 8 x
        static member WriteString (x : string)         = writeString (x.Length * 8) x

    type BitWriterBuilder(stream : Stream) =
        let buffer = Array.zeroCreate<byte> 1024
        let mutable index   = 0
        let mutable bytePos = 0

        let consumeBits count (input : byte[]) =
            let rec loop count inputIdx inputBytePos =
                if count > 0 then
                    let canConsume = min count <| min (8 - inputBytePos) (8 - bytePos)
                    let bufferByte = buffer.[index]
                    let inputByte  = input.[inputIdx]
                    let inputByte  = inputByte <<< inputBytePos >>> canConsume <<< canConsume >>> bytePos
                    buffer.[index]   <- bufferByte ||| inputByte
                    input.[inputIdx] <- inputByte <<< canConsume
                    
                    bytePos <- bytePos + canConsume
                    if bytePos = 8 then
                        index <- index + 1
                        bytePos <- 0

                    if index = buffer.Length then
                        stream.Write(buffer, 0, buffer.Length)
                        index <- 0
            
                    let leftOver = count - canConsume
                    ()
            loop count 0
            
        member x.Bind (Writer(n, arr), cont) = 
            consumeBits n arr
            cont ()
        member x.Zero () = ()
        member x.Delay (f : unit -> unit) =
            if index > 0 || bytePos > 0 then
                stream.Write(buffer, 0, index+1)

    let bitWriter stream = new BitWriterBuilder(stream)

    let memStream = new MemoryStream()
    let test = bitWriter memStream {
            do! BitWriter.WriteBool(true)
            do ()
        }