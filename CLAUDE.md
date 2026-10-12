# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SVG Shell Extensions is a Windows shell integration tool providing SVG Preview Handler, Thumbnail Handler, and a standalone SVG Text Editor for Windows 10/11. Written in Delphi (Object Pascal) using VCL framework.

## Build Commands

Requires Delphi 12 or 13 with MSBuild. The `Build.bat` script builds all components:

```batch
# Initialize Delphi environment (adjust path for your Delphi version)
call "C:\BDS\Studio\37.0\bin\rsvars.bat"

# Build 64-bit shell extension DLL
msbuild.exe "Source\SVGShellExtensions.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release

# Build 32-bit shell extension DLL
msbuild.exe "Source\SVGShellExtensions32.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release

# Build text editor (both platforms)
msbuild.exe "Source\SVGTextEditor.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release
msbuild.exe "Source\SVGTextEditor.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release
```

**Installer:** Uses Inno Setup 6 (`Setup\SVGShellExtensions.iss`)

**Manual Registration:**
- Register: `Setup\Register64bit.bat` (run as administrator)
- Unregister: `Setup\UnRegister64Bit.bat`

## Architecture

### Component Structure

The project produces two main outputs:
- **SVGShellExtensions.dll** - COM-based shell extension (64-bit and 32-bit versions)
- **SVGTextEditor.exe** - Standalone SVG editor application

### Handler Pattern (Source/)

Three COM handlers implement Windows Shell interfaces:
- `uSVGPreviewHandler.pas` - IPreviewHandler for Explorer preview panel
- `uSVGThumbnailHandler.pas` - IThumbnailProvider for Explorer thumbnails
- `uSVGContextMenuHandler.pas` - IContextMenu for right-click actions

Base classes in `uPreviewHandler.pas`, `uCommonPreviewHandler.pas` provide shared functionality.

### Registration Layer

- `uPreviewHandlerRegister.pas` - Preview handler COM registration
- `uThumbnailHandlerRegister.pas` - Thumbnail handler COM registration

### SVG Rendering

Uses multiple rendering engines via SVGIconImageList library:
- **Image32** - Default engine, pure Pascal implementation
- **Direct2D** - Optional Windows native rendering
- **Skia4Delphi** - High-quality rendering (requires sk4d.dll)

### Editor (Source/)

- `EditorMainForm.pas` - Main editor window with SynEdit integration
- `PreviewForm.pas` - SVG preview display
- `SettingsForm.pas` - User preferences dialog

## External Dependencies (Ext/)

All dependencies are included as subfolders:
- **SVGIconImageList/** - Ethea's SVG rendering library
- **SynEdit/** - PyScripter's syntax-highlighting editor component
- **StyledComponents/** - Themed VCL components
- **VCLStyleUtils/** - RRUZ's VCL styling utilities

## Output Locations

- `Bin64/` - 64-bit binaries (SVGShellExtensions.dll, SVGTextEditor.exe, sk4d.dll)
- `Bin32/` - 32-bit binaries
- `Dcu/` - Compiled unit files

## Key Implementation Notes

- Shell extensions remain loaded in Explorer until Explorer closes - close all Explorer instances before rebuilding
- Theme support (Dark/Light) follows Windows system preference
- Settings stored via Windows Registry (see `uSettings.pas`, `uRegistry.pas`)
- Logging/exception handling in `uLogExcept.pas`
