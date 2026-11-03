<#
.SYNOPSIS
    Rebuilds all EtheaSynEdit packages for Delphi 11 Alexandria.

.DESCRIPTION
    Calls BuildAllPackages.ps1 -DelphiVersion D11: runtime package
    (EtheaSynEdit) and design-time package (dclEtheaSynEdit).
    The runtime package is built for Win32 and Win64; the design-time package
    for the platforms of the IDE (Win32 only before D12).

.PARAMETER BDSRoot
    Root folder of RAD Studio installations (default: C:\BDS\Studio).

.PARAMETER Platform
    Target platform: Win32, Win64 or Both (default: Both).

.PARAMETER Config
    Build configuration: Debug or Release (default: Release).

.PARAMETER Target
    MSBuild target: Build, Clean or Rebuild (default: Rebuild).

.EXAMPLE
    .\BuildAllPackagesD11.ps1
    # Rebuilds the packages for D11 on Win32 + Win64 in Release.
#>
param(
    [string]$BDSRoot = "C:\BDS\Studio",

    [ValidateSet("Win32", "Win64", "Both")]
    [string]$Platform = "Both",

    [ValidateSet("Debug", "Release")]
    [string]$Config = "Release",

    [ValidateSet("Build", "Clean", "Rebuild")]
    [string]$Target = "Rebuild"
)

$BuildAll = Join-Path $PSScriptRoot "BuildAllPackages.ps1"
& $BuildAll -DelphiVersion "D11" -BDSRoot $BDSRoot -Platform $Platform -Config $Config -Target $Target
exit $LASTEXITCODE
