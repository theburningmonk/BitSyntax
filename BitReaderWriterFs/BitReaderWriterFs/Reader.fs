namespace BitReaderWriterFs

open System
open System.IO

exception InsufficientBytes

type Count      = | N of int | Rest
type Reader<'a> = Reader of Count * int * (byte[] -> 'a)

[<AutoOpen>]
module BitReaderWorkflow =
    type BitReader private () =
        
        static member ReadInt16  n  = Reader(N n,  2, fun arr -> BitConverter.ToInt16(arr, 0))
        static member ReadUint16 n  = Reader(N n,  2, fun arr -> BitConverter.ToUInt16(arr, 0))
        static member ReadInt32  n  = Reader(N n,  4, fun arr -> BitConverter.ToInt32(arr, 0))
        static member ReadUint32 n  = Reader(N n,  4, fun arr -> BitConverter.ToUInt32(arr, 0))
        static member ReadInt64  n  = Reader(N n,  8, fun arr -> BitConverter.ToInt64(arr, 0))
        static member ReadUint64 n  = Reader(N n,  8, fun arr -> BitConverter.ToUInt64(arr, 0))
        static member ReadFloat  () = Reader(N 32, 4, fun arr -> BitConverter.ToSingle(arr, 0))
        static member ReadDouble () = Reader(N 64, 8, fun arr -> BitConverter.ToDouble(arr, 0))

        static member ReadBool   () = Reader(N 1, 1, fun arr -> arr.[0] = 1uy)
        static member ReadByte   n  = Reader(N n, 1, fun arr -> arr.[0])
        static member ReadBytes  n  = Reader(N (n * 8), n, id)
        static member ReadChar   () = Reader(N 8, 1, fun arr -> Convert.ToChar(arr.[0]))
        static member ReadString n  = Reader(N (n * 8), n, fun arr -> Text.Encoding.UTF8.GetString arr)

    type BitReaderBuilder (stream : Stream) =
        let mutable buffer        = Array.zeroCreate<byte> 1024
        let mutable bufferSize    = 0
        let mutable bufferIdx     = 0
        let mutable bufferBytePos = 0

        let readIntoBuffer () =
            match stream.Read(buffer, 0, 1024) with
            | -1 -> raise InsufficientBytes
            | n  -> bufferSize    <- n
                    bufferIdx     <- 0
                    bufferBytePos <- 0

        let readBits count (output : byte[]) = 
            let rec loop count outputIdx outputBytePos = 
                if count > 0 then
                    if bufferBytePos = 8 then 
                        bufferBytePos <- 0
                        bufferIdx     <- bufferIdx + 1
                    if bufferIdx >= bufferSize then
                        readIntoBuffer()
                   
                    let outputBytePos, outputIdx = 
                        match outputBytePos with
                        | 8 -> 0, outputIdx + 1
                        | n -> n, outputIdx

                    // bufferBytePos = 3, outputBytePos = 2, count = 2, canTake = 2
                    let canTake = min count (8 - bufferBytePos - outputBytePos)            

                    let outputByte = output.[outputIdx]
                    let bufferByte = buffer.[bufferIdx]

                    //                                           _
                    // outputByte                              01000000
                    let newOutputbyte =                     //    __
                        bufferByte                          // 10011010
                        >>> (8 - bufferBytePos - canTake)   // 00010011
                        <<< (8 - canTake - outputBytePos)   // 00110000
                        ||| outputByte                      // 01110000

                    if canTake <= count then // the last byte needs to be filled in right-to-left
                        //                                     00000111
                        output.[outputIdx] <- newOutputbyte >>> (8 - outputBytePos - canTake)
                    else  // others need to leave space on the right for next byte 
                        output.[outputIdx] <- newOutputbyte // 01110000

                    bufferBytePos <- bufferBytePos + canTake
                    loop (count - canTake) outputIdx (outputBytePos + canTake)

            loop count 0 0

        let readFrom (Reader(count, arrSize, convert)) =
            match count with
            | N n ->
                // TODO : use pool buffer
                let arr = Array.zeroCreate<byte> arrSize
                readBits n arr
                convert arr
            | Rest -> 
                failwith "todo"

        let bind reader cont = cont (readFrom reader)

        member this.Bind(reader, cont) = bind reader cont
        member this.Return x           = x

    let bitReader stream = BitReaderBuilder(stream)