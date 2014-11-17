namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("BitSyntax")>]
[<assembly: AssemblyProductAttribute("BitSyntax")>]
[<assembly: AssemblyDescriptionAttribute("F# workflows for working with stream of data at a bit level")>]
[<assembly: AssemblyVersionAttribute("0.2.0")>]
[<assembly: AssemblyFileVersionAttribute("0.2.0")>]
[<assembly: InternalsVisibleToAttribute("BitSyntax.Tests")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.2.0"
