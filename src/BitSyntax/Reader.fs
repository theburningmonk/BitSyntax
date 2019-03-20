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
    let convertEndian arr isBigEndian = if (isBigEndian <> (not BitConverter.IsLittleEndian)) then (arr |> Array.rev) else arr
    type BitReader private () =

        static member ReadInt16  (?numBits, ?isBigEndian)  = Reader(N (defaultArg numBits 16, 2), fun arr -> BitConverter.ToInt16((convertEndian arr (defaultArg isBigEndian true)), 0))
        static member ReadUInt16 (?numBits, ?isBigEndian)  = Reader(N (defaultArg numBits 16, 2), fun arr -> BitConverter.ToUInt16((convertEndian arr (defaultArg isBigEndian true)), 0))
        static member ReadInt32  (?numBits, ?isBigEndian)  = Reader(N (defaultArg numBits 32, 4), fun arr -> BitConverter.ToInt32((convertEndian arr (defaultArg isBigEndian true)), 0))
        static member ReadUInt32 (?numBits, ?isBigEndian)  = Reader(N (defaultArg numBits 32, 4), fun arr -> BitConverter.ToUInt32((convertEndian arr (defaultArg isBigEndian true)), 0))
        static member ReadInt64  (?numBits, ?isBigEndian)  = Reader(N (defaultArg numBits 64, 8), fun arr -> BitConverter.ToInt64((convertEndian arr (defaultArg isBigEndian true)), 0))
        static member ReadUInt64 (?numBits, ?isBigEndian)  = Reader(N (defaultArg numBits 64, 8), fun arr -> BitConverter.ToUInt64((convertEndian arr (defaultArg isBigEndian true)), 0))
        static member ReadFloat  ()          = Reader(N (32, 4), fun arr -> BitConverter.ToSingle(arr, 0))
        static member ReadDouble ()          = Reader(N (64, 8), fun arr -> BitConverter.ToDouble(arr, 0))

        static member ReadBool   ()          = Reader(N (1, 1), fun arr -> arr.[0] = 1uy)
        static member ReadByte   (?numBits)  = Reader(N (defaultArg numBits 8, 1), fun arr -> arr.[0])
        static member ReadBytes  (numBytes, ?isBigEndian)  = Reader(N (numBytes * 8, numBytes), fun arr -> convertEndian arr (defaultArg isBigEndian true))
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
            let offset partialConsumed count (buffer : byte[]) = 
                let mutable partialConsumed = partialConsumed

                for idx = 0 to buffer.Length-1 do
                    let temp = buffer.[idx]
                    buffer.[idx] <- partialConsumed <<< count
                                    ||| (temp >>> (8 - count))
                    partialConsumed <- temp
            
                buffer

            incrBufferIdx()
            
            let leftOnStream = stream.Length - stream.Position |> int
            let leftOnBuffer = if bufferBytePos = 0
                               then bufferSize - bufferIdx
                               else bufferSize - bufferIdx - 1

            let arrSize = leftOnBuffer + leftOnStream
            let bytes   = Array.zeroCreate<byte> arrSize

            let startBufferIdx = if bufferBytePos = 0 then bufferIdx else bufferIdx + 1
            for idx = startBufferIdx to bufferSize-1 do
                bytes.[idx - startBufferIdx] <- buffer.[idx]

            if stream.Read(bytes, leftOnBuffer, leftOnStream) <> leftOnStream then
                raise InsufficientBytes

            if bufferBytePos = 0 then bytes
            else // if we're starting off with a partly consumed byte, then we'll need to 
                 // offset the bits in each byte
                 // e.g. 10|01000100|01011100|(padding)
                 // becomes 01000100|01011100|(padding, hence the Seq.take and Seq.toArray)
                 let partialConsumed = buffer.[bufferIdx]
                 offset partialConsumed bufferBytePos bytes

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