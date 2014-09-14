namespace BitReaderWriterFs

open System.IO

module BitWriterWorkflow =
    open BitReaderWriterCs

    type Writer = Writer of int * byte[]
    type State  = Writer list

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
    let writeBytes n x              = Writer(n, x)
    let writeBool x                 = if x then writeByte 1 1uy else writeByte 1 0uy
    let writeChar n x               = Writer(n, 

    type BitWriter private () =
        static let defaultEndianness = Endianness.Little

        static member private Write (x : 'a, defaultSize, f, ?n, ?endianness) =            
            f (defaultArg endianness defaultEndianness) (defaultArg n defaultSize) x

        static member WriteInt16 (x, ?n, ?endianness)  = BitWriter.Write(x, 16, writeInt16, ?n = n, ?endianness = endianness)
        static member WriteUint16 (x, ?n, ?endianness) = BitWriter.Write(x, 16, writeUint16, ?n = n, ?endianness = endianness)
        static member WriteInt32 (x, ?n, ?endianness)  = BitWriter.Write(x, 32, writeInt32, ?n = n, ?endianness = endianness)
        static member WriteUint32 (x, ?n, ?endianness) = BitWriter.Write(x, 32, writeUint32, ?n = n, ?endianness = endianness)
        static member WriteInt64 (x, ?n, ?endianness)  = BitWriter.Write(x, 64, writeInt64, ?n = n, ?endianness = endianness)
        static member WriteUint64 (x, ?n, ?endianness) = BitWriter.Write(x, 64, writeUint64, ?n = n, ?endianness = endianness)
        static member WriteFloat (x, ?n, ?endianness)  = BitWriter.Write(x, 32, writeFloat, ?n = n, ?endianness = endianness)
        static member WriteDouble (x, ?n, ?endianness) = BitWriter.Write(x, 64, writeDouble, ?n = n, ?endianness = endianness)
            
        static member WriteBool  (x)              = writeBool x
        static member WriteByte  (x, ?n)          = writeByte (defaultArg n 8) x
        static member WriteBytes (x : byte[], ?n) = writeBytes (defaultArg n (x.Length * 8)) x
        static member WriteChar  (
//        static member WriteChar   (x : char, ?n)    = toBitArray (defaultArg n 8) <| Text.Encoding.UTF8.GetBytes([| x |])
//        static member WriteString (x : string, ?n)  = toBitArray (defaultArg n (x.Length * 8)) <| Text.Encoding.UTF8.GetBytes(x)


    type BitWriterBuilder(stream : Stream) =
        member x.Bind (p : Writer, cont) = Seq.empty<Writer>
        member x.Zero () = Seq.empty<Writer>
        member x.Delay (f : unit -> Writer seq) = ()

    let bitWriter stream = new BitWriterBuilder(stream)

    let test = bitWriter (new MemoryStream()) {
            do! ()
            do ()
        }