#r "bin/BitReaderWriterFs.dll"

open BitReaderWriterFs

open System
open System.IO
open System.Collections
open System.Collections.Generic

let stream = new MemoryStream()
stream.WriteByte(255uy)
stream.WriteByte(0uy)
stream.WriteByte(129uy)
stream.Write(Text.Encoding.ASCII.GetBytes("Hello World!"), 0, 12)

let allT, allF, t, sixFOneT, bytes, world, ``!`` = bitReader stream {
        let! allT     = Bits.ReadInt32(4)
        do! Bits.Skip(8)
        let! allF     = Bits.ReadInt32(4)
        let! t        = Bits.ReadBool()
        let! sixFOneT = Bits.ReadInt32(7)
        let! bytes'   = Bits.ReadBytes(8*6)
        let! world    = Bits.ReadString(5 * 8)
        let! ``!``    = Bits.ReadChar()

        return allT, allF, t, sixFOneT, bytes', world, ``!``
    }
