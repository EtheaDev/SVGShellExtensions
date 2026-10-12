<#
.SYNOPSIS
    Rebuilds all StyledComponents packages for Delphi 13.

.DESCRIPTION
    Calls BuildAllPackages.ps1 -DelphiVersion D13: runtime package
    (StyledComponents) and design-time package (dclStyledComponents);
    with -Animated also the animated packages (require Skia4Delphi).
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

.PARAMETER Animated
    Builds also the animated packages (require Skia4Delphi).

.EXAMPLE
    .\BuildAllPackagesD13.ps1
    # Rebuilds the packages for D13 on Win32 + Win64 in Release.
#>
param(
    [string]$BDSRoot = "C:\BDS\Studio",

    [ValidateSet("Win32", "Win64", "Both")]
    [string]$Platform = "Both",

    [ValidateSet("Debug", "Release")]
    [string]$Config = "Release",

    [ValidateSet("Build", "Clean", "Rebuild")]
    [string]$Target = "Rebuild",

    [switch]$Animated
)

$BuildAll = Join-Path $PSScriptRoot "BuildAllPackages.ps1"
& $BuildAll -DelphiVersion "D13" -BDSRoot $BDSRoot -Platform $Platform -Config $Config -Target $Target -Animated:$Animated
exit $LASTEXITCODE
