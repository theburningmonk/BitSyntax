namespace BitReaderWriterFs

open System
open System.IO

module BitWriterWorkflow =
    open BitReaderWriterCs

    /// A writer represents the number of bits to take from the byte array
    type Writer = 
        | Writer    of int * byte[]
        | Composite of Writer * Writer

    let inline getNumBytes n = n / 8 + (if (n % 8 > 0) then 1 else 0)
    let inline getWriter n (wrapper : ^a) =
        let numBytes = getNumBytes n
        let bytes = (^a : (member GetBytes : Endianness -> seq<byte>) (wrapper, Endianness.Little))
        Writer(n, bytes |> Seq.take numBytes |> Seq.toArray)

    let writeInt16 n x              = new Int16Wrapper(Instance = x)  |> getWriter n        
    let writeUint16 n x             = new Uint16Wrapper(Instance = x) |> getWriter n
    let writeInt32 n x              = new Int32Wrapper(Instance = x)  |> getWriter n
    let writeUint32 n x             = new Uint32Wrapper(Instance = x) |> getWriter n
    let writeInt64 n x              = new Int64Wrapper(Instance = x)  |> getWriter n
    let writeUint64 n x             = new Uint64Wrapper(Instance = x) |> getWriter n
    let writeFloat n x              = new FloatWrapper(Instance = x)  |> getWriter n
    let writeDouble n x             = new DoubleWrapper(Instance = x) |> getWriter n
    let writeByte n x               = Writer(n, [| x |])
    let writeBytes n (x : byte[])   = Writer(n, x)
    let writeBool x                 = if x then writeByte 1 1uy else writeByte 1 0uy
    let writeChar n x               = Writer(n, Text.Encoding.UTF8.GetBytes([| x |]))
    let writeString n (x : string)  = Writer(n, Text.Encoding.UTF8.GetBytes(x))

    type BitWriter private () =
        static member private Write (x : 'a, defaultSize, f, ?n) =            
            f (min defaultSize <| defaultArg n defaultSize) x

        static member WriteInt16 (x, ?n)  = BitWriter.Write(x, 16, writeInt16, ?n = n)
        static member WriteUint16 (x, ?n) = BitWriter.Write(x, 16, writeUint16, ?n = n)
        static member WriteInt32 (x, ?n)  = BitWriter.Write(x, 32, writeInt32, ?n = n)
        static member WriteUint32 (x, ?n) = BitWriter.Write(x, 32, writeUint32, ?n = n)
        static member WriteInt64 (x, ?n)  = BitWriter.Write(x, 64, writeInt64, ?n = n)
        static member WriteUint64 (x, ?n) = BitWriter.Write(x, 64, writeUint64, ?n = n)
        static member WriteFloat x        = BitWriter.Write(x, 32, writeFloat)
        static member WriteDouble x       = BitWriter.Write(x, 64, writeDouble)
            
        static member WriteBool   x                = writeBool x
        static member WriteByte   (x, ?n)          = writeByte (defaultArg n 8) x
        static member WriteBytes  (x : byte[], ?n) = writeBytes (defaultArg n <| x.Length * 8) x
        static member WriteChar   x                = writeChar 8 x
        static member WriteString (x : string)     = writeString (x.Length * 8) x
        
    and BitWriterBuilder(stream : Stream) =
        let buffer = Array.zeroCreate<byte> 1024
        let mutable index   = 0
        let mutable bytePos = 0

        /// Consume the specified number of bits from the input array
        let consumeBits count (input : byte[]) =
            let rec loop count inputIdx inputBytePos =
                if count > 0 then
                    if bytePos >= 8 then
                        index <- index + 1
                        bytePos <- 0

                    if index = buffer.Length then
                        stream.Write(buffer, 0, buffer.Length)
                        index <- 0

                    let canConsume = min count <| min (8 - inputBytePos) (8 - bytePos)
                    let bufferByte = buffer.[index]
                    let inputByte  = input.[inputIdx]

                    // e.g. input byte pos = 2, byte pos = 3, count = 3, can consume = 1
                    //                                                   _
                    //                                          byte: 11100000
                    //                                                     _
                    let inputByte = inputByte                      // 00110101
                                    >>> inputBytePos               // 00001101
                                    <<< inputBytePos               // 00110100
                                    <<< (8 - canConsume 
                                           - inputBytePos)         // 10100000
                                    >>> bytePos                    // 00010100
                    buffer.[index]   <- bufferByte ||| inputByte   // 11110000
                    
                    bytePos <- bytePos + canConsume
            
                    let inputIdx, inputBytePos = 
                        match inputBytePos + canConsume with
                        | 8   -> inputIdx + 1, 0
                        | pos -> inputIdx, pos

                    loop (count - canConsume) inputIdx inputBytePos

            loop count 0 0

        let rec consumeWriter = function
            | Writer (0, _) -> ()
            | Writer (n, arr) ->
                consumeBits n arr
            | Composite (writer1, writer2) ->
                consumeWriter writer1
                consumeWriter writer2

        let bind (writer : Writer) cont =
            consumeWriter writer
            cont ()

        let combine writer1 writer2 = Composite(writer1, writer2)
            
        let zero () = ()

        let forLoop (inputs : 'a seq) (f : 'a -> Writer) =
            for x in inputs do
                f x |> consumeWriter

        member this.Bind (writer, cont)        = bind writer cont
        member this.Combine (writer1, writer2) = combine writer1 writer2
        member this.For (inputs, f)            = forLoop inputs f
        member this.Zero ()                    = zero()
        member this.Return _x                  = Writer(0, [||])
        member this.Delay (f : unit -> unit)   =
            f()
            if index > 0 || bytePos > 0 then
                stream.Write(buffer, 0, index+1)

    let bitWriter stream = new BitWriterBuilder(stream)