module UdpExample

open System.Collections.Generic
open System.Net.Sockets
open BitReaderWriterFs

type Race =
    | Human = 0uy
    | Elf   = 1uy
    | Dwarf = 2uy
    | Alien = 3uy

type Player =
    {
        Race : Race
        Age  : int

    }

let client = new UdpClient(12345)

let server  = new UdpClient(54321)
let clients = new Dictionary<int, 

let sendMovement (movement : 

