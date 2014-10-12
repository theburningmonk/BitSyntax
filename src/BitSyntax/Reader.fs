namespace BitSyntax

open System
open System.IO

exception InsufficientBytes

type BitsCount  = int
type ArraySize  = int

type Count      = | N of BitsCount * ArraySize | Rest
type Reader<'a> = Reader of Count * (byte[] -> 'a)

[<AutoOpen>]
module BitReaderWorkflow =
    type BitReader private () =
        
        static member ReadInt16  (?numBits)  = Reader(N (defaultArg numBits 16, 2), fun arr -> BitConverter.ToInt16(arr, 0))
        static member ReadUInt16 (?numBits)  = Reader(N (defaultArg numBits 16, 2), fun arr -> BitConverter.ToUInt16(arr, 0))
        static member ReadInt32  (?numBits)  = Reader(N (defaultArg numBits 32, 4), fun arr -> BitConverter.ToInt32(arr, 0))
        static member ReadUInt32 (?numBits)  = Reader(N (defaultArg numBits 32, 4), fun arr -> BitConverter.ToUInt32(arr, 0))
        static member ReadInt64  (?numBits)  = Reader(N (defaultArg numBits 64, 8), fun arr -> BitConverter.ToInt64(arr, 0))
        static member ReadUInt64 (?numBits)  = Reader(N (defaultArg numBits 64, 8), fun arr -> BitConverter.ToUInt64(arr, 0))
        static member ReadFloat  ()          = Reader(N (32, 4), fun arr -> BitConverter.ToSingle(arr, 0))
        static member ReadDouble ()          = Reader(N (64, 8), fun arr -> BitConverter.ToDouble(arr, 0))

        static member ReadBool   ()          = Reader(N (1, 1), fun arr -> arr.[0] = 1uy)
        static member ReadByte   (?numBits)  = Reader(N (defaultArg numBits 8, 1), fun arr -> arr.[0])
        static member ReadBytes  numBytes    = Reader(N (numBytes * 8, numBytes), id)
        static member ReadChar   ()          = Reader(N (8, 1), fun arr -> Convert.ToChar(arr.[0]))
        static member ReadString numChars    = Reader(N (numChars * 8, numChars), fun arr -> Text.Encoding.UTF8.GetString arr)
        static member Rest convert           = Reader(Rest, convert)

    type BitReaderBuilder (stream : Stream) =
        let defaultBufferSize     = 128
        let mutable buffer        = Array.zeroCreate<byte> defaultBufferSize
        let mutable bufferSize    = 0
        let mutable bufferIdx     = 0
        let mutable bufferBytePos = 0

        let incrBufferIdx () =
            if bufferBytePos = 8 then 
                bufferBytePos <- 0
                bufferIdx     <- bufferIdx + 1

        let readIntoBuffer () =
            match stream.Read(buffer, 0, defaultBufferSize) with
            | -1 -> raise InsufficientBytes
            | n  -> bufferSize    <- n
                    bufferIdx     <- 0
                    bufferBytePos <- 0

        let readBits count (output : byte[]) = 
            let rec loop count outputIdx outputBytePos = 
                if count > 0 then
                    incrBufferIdx()

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

                    if canTake = count then // the last byte needs to be filled in right-to-left
                        //                                     00000111
                        output.[outputIdx] <- newOutputbyte >>> (8 - outputBytePos - canTake)
                    else  // others need to leave space on the right for next byte 
                        output.[outputIdx] <- newOutputbyte // 01110000

                    bufferBytePos <- bufferBytePos + canTake
                    loop (count - canTake) outputIdx (outputBytePos + canTake)

            loop count 0 0

        let readRest () =
            let offset count (buffer : byte[]) = 
                let temp = buffer.[0]
                buffer.[0] <- temp <<< count

                for idx = 1 to buffer.Length-1 do
                    let byte, prevByte = buffer.[idx], buffer.[idx-1]
                    buffer.[idx-1] <- byte >>> (8 - count) ||| prevByte
                    buffer.[idx]   <- byte <<< count
            
                buffer

            incrBufferIdx()

            let leftOnBuffer = bufferSize - bufferIdx
            let leftOnStream = stream.Length - stream.Position |> int

            let arrSize = leftOnBuffer + leftOnStream
            let bytes   = Array.zeroCreate<byte> arrSize

            for idx = bufferIdx to bufferSize-1 do
                bytes.[idx - bufferIdx] <- buffer.[idx]

            if stream.Read(bytes, leftOnBuffer, leftOnStream) <> leftOnStream then
                raise InsufficientBytes

            if bufferBytePos = 0 then bytes
            else // if we're starting off with a partly consumed byte, then we'll need to 
                 // offset the bits in each byte
                 // e.g. 10|01000100|01011100|(padding)
                 // becomes 01000100|01011100|(padding, hence the Seq.take and Seq.toArray)
                 offset bufferBytePos bytes
                 |> Seq.take (arrSize - 1)
                 |> Seq.toArray

        let readFrom (Reader(count, convert)) =
            match count with
            | N (n, arrSize) ->
                // TODO : use pool buffer
                let arr = Array.zeroCreate<byte> arrSize
                readBits n arr
                convert arr
            | Rest -> 
                readRest() |> convert

        let bind reader cont = cont (readFrom reader)

        member this.Bind(reader, cont) = bind reader cont
        member this.Return x           = x
        member this.ReturnFrom reader  = readFrom reader

    let bitReader stream = BitReaderBuilder(stream)