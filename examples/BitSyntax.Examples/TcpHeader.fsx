﻿#r "bin/BitSyntaxCs.dll"
#r "bin/BitSyntax.dll"

open BitSyntax

open System.IO

let outStream = new MemoryStream()

let writeTcpHeaders stream =    
    do bitWriter stream {
        do! BitWriter.WriteInt16(12345s) // source port
        do! BitWriter.WriteInt16(12321s) // destination port
        do! BitWriter.WriteInt32(1)      // sequence number
        do! BitWriter.WriteInt32(1)      // ack number
        do! BitWriter.WriteInt32(2, 4)   // data offset
        do! BitWriter.WriteInt32(0, 3)   // reserved
        do! BitWriter.WriteBool(false)   // NS
        do! BitWriter.WriteBool(true)    // CWR
        do! BitWriter.WriteBool(true)    // ECE
        do! BitWriter.WriteBool(true)    // URG
        do! BitWriter.WriteBool(true)    // ACK
        do! BitWriter.WriteBool(true)    // PSH
        do! BitWriter.WriteBool(false)   // RST
        do! BitWriter.WriteBool(true)    // SYN
        do! BitWriter.WriteBool(false)   // FIN
        do! BitWriter.WriteInt16(8s)     // Window Size
        do! BitWriter.WriteInt16(42s)    // Checksum
        do! BitWriter.WriteInt16(1208s)  // Urgent Pointer
        do! BitWriter.WriteString("Hello World!")  // payload
    }

writeTcpHeaders outStream

// now read the TCP headers back
outStream.Position <- 0L
let srcPort, destPort, 
    seqNum, ackNum, dataOffset, 
    ns, cwr, ece, urg, ack, psh, rst, syn, fin,
    winSize, checkSum, pointer, payload = 
        bitReader outStream {
            let! srcPort    = BitReader.ReadInt32(16)
            let! destPort   = BitReader.ReadInt32(16)
            let! seqNum     = BitReader.ReadInt32(32)
            let! ackNum     = BitReader.ReadInt32(32)
            let! dataOffset = BitReader.ReadInt32(4)
            let! _          = BitReader.ReadByte(3)

            let! ns         = BitReader.ReadBool()
            let! cwr        = BitReader.ReadBool()
            let! ece        = BitReader.ReadBool()
            let! urg        = BitReader.ReadBool()
            let! ack        = BitReader.ReadBool()
            let! psh        = BitReader.ReadBool()
            let! rst        = BitReader.ReadBool()
            let! syn        = BitReader.ReadBool()
            let! fin        = BitReader.ReadBool()
            let! winSize    = BitReader.ReadInt32(16)
            let! checkSum   = BitReader.ReadInt32(16)
            let! pointer    = BitReader.ReadInt32(16)
            let! payload    = BitReader.ReadString(12)

            return srcPort, destPort, 
                    seqNum, ackNum, dataOffset, 
                    ns, cwr, ece, urg, ack, psh, rst, syn, fin,
                    winSize, checkSum, pointer, payload
        }