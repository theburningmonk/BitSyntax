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
        let! allT     = BitReader.ReadInt32(4)
        do! BitReader.Skip(8)
        let! allF     = BitReader.ReadInt32(4)
        let! t        = BitReader.ReadBool()
        let! sixFOneT = BitReader.ReadInt32(7)
        let! bytes'   = BitReader.ReadBytes(8*6)
        let! world    = BitReader.ReadString(5 * 8)
        let! ``!``    = BitReader.ReadChar()

        return allT, allF, t, sixFOneT, bytes', world, ``!``
    }
