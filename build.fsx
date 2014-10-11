// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"packages/FAKE/tools/FakeLib.dll"
open Fake 
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System

let buildDir = "build/"
let cliDir   = "cli/"
let testDir  = "test/"
let tempDir  = "temp/"

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package 
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project 
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project    = "Amazon.CloudWatch.Selector"
let cliProject = "cwcli"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary    = "Extension library for AWSSDK that allows you to select CloudWatch metrics with simple queries"
let cliSummary = "CLI for querying metrics in Amazon CloudWatch using a simple DSL."

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = """
This library adds easy-to-use internal and external DSLs for querying against Amazon CloudWatch 
metrics to make finding correlations in the metrics stats much easier.
"""
// List of author names (for NuGet package)
let authors = [ "Yan Cui" ]
// Tags for your project (for NuGet package)
let tags = "F# fsharp aws amazon cloudwatch dsl"

// File system information 
// (<solutionFile>.sln is built during the building process)
let projectFile     = "Amazon.CloudWatch.Selector.fsproj"
let testProjectFile = "Amazon.CloudWatch.Selector.Tests.fsproj"
let cliProjectFile  = "Amazon.CloudWatch.Selector.Cli.fsproj"
// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = ["build/Amazon.CloudWatch.Selector*Tests*.dll"]

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "https://github.com/theburningmonk"
// The name of the project on GitHub
let gitName = "Amazon.CloudWatch.Selector"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps 
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let fileName = "src/CloudWatch.Selector/AssemblyInfo.fs"
  CreateFSharpAssemblyInfo fileName
      [ Attribute.Title         project
        Attribute.Product       project
        Attribute.Description   summary
        Attribute.Version       release.AssemblyVersion
        Attribute.FileVersion   release.AssemblyVersion
        Attribute.InternalsVisibleTo "Amazon.CloudWatch.Selector.Tests" ] 

  let fileName = "src/CloudWatch.Selector.Cli/AssemblyInfo.fs"
  CreateFSharpAssemblyInfo fileName
      [ Attribute.Title         "AWS CloudWatch Selector CLI"
        Attribute.Product       cliProject
        Attribute.Description   cliSummary
        Attribute.Version       release.AssemblyVersion
        Attribute.FileVersion   release.AssemblyVersion ] 
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "RestorePackages" RestorePackages

Target "Clean" (fun _ ->
    CleanDirs [ buildDir; cliDir; testDir; tempDir ]
)

Target "CleanDocs" (fun _ ->
    CleanDirs [ "docs/output" ]
)

// --------------------------------------------------------------------------------------
// Build library & test project

let files includes = 
  { BaseDirectory = __SOURCE_DIRECTORY__
    Includes = includes
    Excludes = [] } 

Target "Build" (fun _ ->
    files [ "src/CloudWatch.Selector/" + projectFile
            "tests/CloudWatch.Selector.Tests/" + testProjectFile ]
    |> MSBuildRelease buildDir "Rebuild"
    |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

Target "RunTests" (fun _ ->
    ActivateFinalTarget "CloseTestRunner"

    { BaseDirectory = __SOURCE_DIRECTORY__
      Includes = testAssemblies
      Excludes = [] } 
    |> NUnit (fun p ->
        { p with
            DisableShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResults.xml" })
)

FinalTarget "CloseTestRunner" (fun _ ->  
    ProcessHelper.killProcess "nunit-agent.exe"
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    // Format the description to fit on a single line (remove \r\n and double-spaces)
    let description = description.Replace("\r", "")
                                 .Replace("\n", "")
                                 .Replace("  ", " ")

    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = project
            Summary = summary
            Description = description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = tags
            OutputPath = "nuget"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            Dependencies = 
                [ "AWSSDK",  GetPackageVersion "packages" "AWSSDK" ] })
        ("nuget/" + project + ".nuspec")
)

Target "Chocolatey-CLI" (fun _ ->
    // Format the description to fit on a single line (remove \r\n and double-spaces)
    let description = description.Replace("\r", "")
                                 .Replace("\n", "")
                                 .Replace("  ", " ")

    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = project
            Summary = summary
            Description = description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = tags
            OutputPath = "chocolatey"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            Dependencies = [] })
        ("chocolatey/" + project + ".CLI.nuspec")
)

// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let ghPages      = "gh-pages"
    let ghPagesLocal = "temp/gh-pages"
    Repository.clone "temp" (gitHome + "/" + gitName + ".git") ghPages
    Branches.checkoutBranch ghPagesLocal ghPages
    fullclean ghPagesLocal
    CopyRecursive "docs/output" ghPagesLocal true |> printfn "%A"
    CommandHelper.runSimpleGitCommand ghPagesLocal "add ." |> printfn "%s"
    let cmd = sprintf """commit -a -m "Update generated documentation for version %s""" release.NugetVersion
    CommandHelper.runSimpleGitCommand ghPagesLocal cmd |> printfn "%s"
    Branches.push ghPagesLocal
)

Target "Release" DoNothing

// --------------------------------------------------------------------------------------
// Build CLI

Target "BuildCLI" (fun _ ->
    files [ "src/CloudWatch.Selector.Cli/" + cliProjectFile ]
    |> MSBuildRelease cliDir "Rebuild"
    |> ignore
)

Target "CLI" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

"Clean"
  ==> "RestorePackages"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "BuildCLI"
  ==> "RunTests"
  ==> "All"

"All" 
//  ==> "CleanDocs"
//  ==> "GenerateDocs"
//  ==> "ReleaseDocs"
  ==> "NuGet"
  ==> "Chocolatey-CLI"
  ==> "Release"
  
"All"
//  ==> "BuildCLI"
  ==> "CLI"

RunTargetOrDefault "All"
