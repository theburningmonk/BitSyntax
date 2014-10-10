module Reader

open BitReaderWriterFs.BitWriterWorkflow
open FsUnit
open NUnit.Framework
open FsCheck
open System.IO

[<TestFixture>]
type ``BitReaderBuilder tests`` () =
    [<Test>]
    member test.``bitReader workflow should be able to read Int16 from stream`` () =
        42