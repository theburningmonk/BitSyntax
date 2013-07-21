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

stream.Position <- 0L

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

let outStream = new MemoryStream()

let writeTcpHeaders (stream : Stream) =
    seq {
        yield BitWriter.WriteInt16(12345s) // source port
        yield BitWriter.WriteInt16(12321s) // destination port
        yield BitWriter.WriteInt32(1)      // sequence number
        yield BitWriter.WriteInt32(1)      // ack number
        yield BitWriter.WriteInt32(2, 4)   // data offset
        yield BitWriter.WriteInt32(0, 3)   // reserved
        yield BitWriter.WriteBool(false)   // NS
        yield BitWriter.WriteBool(true)    // CWR
        yield BitWriter.WriteBool(true)    // ECE
        yield BitWriter.WriteBool(true)    // URG
        yield BitWriter.WriteBool(true)    // ACK
        yield BitWriter.WriteBool(true)    // PSH
        yield BitWriter.WriteBool(false)   // RST
        yield BitWriter.WriteBool(true)    // SYN
        yield BitWriter.WriteBool(false)   // FIN
        yield BitWriter.WriteInt16(8s)     // Window Size
        yield BitWriter.WriteInt16(42s)    // Checksum
        yield BitWriter.WriteInt16(1208s)  // Urgent Pointer
        yield BitWriter.WriteString("Hello World!")  // payload
    } |> BitWriter.Flush stream

writeTcpHeaders outStream

// now read the TCP headers back
outStream.Position <- 0L
let srcPort = bitReader outStream {
        return! BitReader.ReadInt32(16)
    }

let destPort = bitReader outStream {
        return! BitReader.ReadInt32(16)
    }

let seqNum, ackNum, dataOffset, ns, cwr, ece, urg, ack, psh, rst, syn, fin = 
    bitReader outStream {
        let! seqNum     = BitReader.ReadInt32()
        let! ackNum     = BitReader.ReadInt32()
        let! dataOffset = BitReader.ReadInt32(4)
        do! BitReader.Skip(3)

        let! ns  = BitReader.ReadBool()
        let! cwr = BitReader.ReadBool()
        let! ece = BitReader.ReadBool()
        let! urg = BitReader.ReadBool()
        let! ack = BitReader.ReadBool()
        let! psh = BitReader.ReadBool()
        let! rst = BitReader.ReadBool()
        let! syn = BitReader.ReadBool()
        let! fin = BitReader.ReadBool()

        return seqNum, ackNum, dataOffset, ns, cwr, ece, urg, ack, psh, rst, syn, fin
    }

let winSize, chkSum, urgPtr, payload = bitReader outStream {
        let! winSize    = BitReader.ReadInt16()
        let! chkSum     = BitReader.ReadInt16()
        let! urgPtr     = BitReader.ReadInt16()
        let! payload    = BitReader.ReadRest()

        return winSize, chkSum, urgPtr, payload |> Text.Encoding.UTF8.GetString
    }

