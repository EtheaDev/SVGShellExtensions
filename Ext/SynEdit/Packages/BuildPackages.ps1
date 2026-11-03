<#
.SYNOPSIS
    Builds one Ethea SynEdit package using MSBuild and RAD Studio.

.DESCRIPTION
    Configures the RAD Studio environment via rsvars.bat and invokes MSBuild
    on the specified project of a package folder (D10.4 ... D13).

.PARAMETER DelphiVersion
    Delphi version: D10.4, D11, D12 or D13 (default: D13). The package folder
    is "Delphi" + the version with the dot replaced by an underscore
    (D10.4 -> Delphi10_4, D13 -> Delphi13).

.PARAMETER BDSRoot
    Root folder of RAD Studio installations.
    Default: C:\BDS\Studio
    The script appends the version-specific subfolder (21.0 ... 37.0).

.PARAMETER Platform
    Target platform: Win32 or Win64 (default: Win32). Design-time packages
    for Win64 require D12 or later (64-bit IDE).

.PARAMETER Config
    Build configuration: Debug or Release (default: Release)

.PARAMETER Project
    Project file name without path (default: EtheaSynEdit.dproj)

.PARAMETER Target
    MSBuild target: Build, Clean, Rebuild (default: Build)

.EXAMPLE
    .\BuildPackages.ps1
    # Builds EtheaSynEdit with Delphi 13, Win32, Release

.EXAMPLE
    .\BuildPackages.ps1 -DelphiVersion D12 -Platform Win64 -Project dclEtheaSynEdit.dproj
    # Builds the design-time package with Delphi 12, Win64, Release
#>

param(
    [ValidateSet("D10.4", "D11", "D12", "D13")]
    [string]$DelphiVersion = "D13",

    [string]$BDSRoot = "C:\BDS\Studio",

    [ValidateSet("Win32", "Win64")]
    [string]$Platform = "Win32",

    [ValidateSet("Debug", "Release")]
    [string]$Config = "Release",

    [string]$Project = "EtheaSynEdit.dproj",

    [ValidateSet("Build", "Clean", "Rebuild")]
    [string]$Target = "Build"
)

$ErrorActionPreference = "Stop"

# Map Delphi version to BDS version subfolder
$bdsVersionMap = @{
    "D10.4"  = "21.0"
    "D11"    = "22.0"
    "D12"    = "23.0"
    "D13"    = "37.0"
}

$bdsVersion = $bdsVersionMap[$DelphiVersion]
$bdsPath = Join-Path $BDSRoot $bdsVersion
$rsvars = Join-Path $bdsPath "bin\rsvars.bat"

# Validate paths
if (-not (Test-Path $bdsPath)) {
    Write-Error "RAD Studio not found at: $bdsPath"
    exit 1
}
if (-not (Test-Path $rsvars)) {
    Write-Error "rsvars.bat not found at: $rsvars"
    exit 1
}

$projectDir = Join-Path $PSScriptRoot ("Delphi" + ($DelphiVersion -replace "D", "" -replace "\.", "_"))
$projectPath = Join-Path $projectDir $Project

if (-not (Test-Path $projectPath)) {
    Write-Error "Project not found: $projectPath"
    exit 1
}

Write-Host "============================================" -ForegroundColor Cyan
Write-Host " Ethea SynEdit Build" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host " Delphi:   $DelphiVersion (BDS $bdsVersion)" -ForegroundColor White
Write-Host " Project:  $Project" -ForegroundColor White
Write-Host " Platform: $Platform" -ForegroundColor White
Write-Host " Config:   $Config" -ForegroundColor White
Write-Host " Target:   $Target" -ForegroundColor White
Write-Host " BDS Path: $bdsPath" -ForegroundColor White
Write-Host "============================================" -ForegroundColor Cyan

# Build the cmd command that sources rsvars.bat then runs MSBuild
$msbuildArgs = "`"$projectPath`" /t:$Target /p:Config=$Config /p:Platform=$Platform /v:minimal /nologo"
$cmd = "call `"$rsvars`" && msbuild $msbuildArgs"

Write-Host ""
Write-Host "Executing: msbuild $Project /t:$Target /p:Config=$Config /p:Platform=$Platform" -ForegroundColor Yellow
Write-Host ""

# Run via cmd /c so rsvars.bat environment is inherited by msbuild
cmd /c $cmd

if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Host "BUILD FAILED (exit code $LASTEXITCODE)" -ForegroundColor Red
    exit $LASTEXITCODE
}
else {
    Write-Host ""
    Write-Host "BUILD SUCCEEDED" -ForegroundColor Green
    exit 0
}
