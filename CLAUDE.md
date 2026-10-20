# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SVG Shell Extensions is a Windows shell integration tool providing SVG Preview Handler, Thumbnail Handler, a standalone SVG Text Editor and the SVGToPNG command-line converter for Windows 10/11. Written in Delphi (Object Pascal) using VCL framework.

## Build Commands

Requires Delphi 12 or 13 with MSBuild. The `Build.bat` script builds all components (it stops at the first failing step, signs the executables and builds the setup; the pause happens only when started from Explorer):

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

# Build the command-line converter (both platforms)
msbuild.exe "Source\SVGToPNG.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release
msbuild.exe "Source\SVGToPNG.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release
```

**Installer:** Uses Inno Setup 6 (`Setup\SVGShellExtensions.iss`)

**Manual Registration:**
- Register: `Bin64\Register64bit.bat` (run as administrator)
- Unregister: `Bin64\UnRegister64bit.bat`

## Architecture

### Component Structure

The project produces three outputs:
- **SVGShellExtensions.dll** - COM-based shell extension (64-bit and 32-bit versions)
- **SVGTextEditor.exe** - Standalone SVG editor application
- **SVGToPNG.exe** - Command-line converter of SVG files to PNG (`SVGToPNGCLI.*` units)

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
- Color palette under the editor: `uSVGColorScanner.pas` finds the color values in the SVG text with their position (attributes, `style`, `<style>` blocks; hex, rgb/rgba, hsl/hsla, names), `uSVGPaletteBar.pas` is the owner-draw palette control (shared with SKIAShellExtensions as `uColorPaletteBar`). The palette is refreshed by the debounced rendering (`LoadTimer`), never from `OnChange`.

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
- Theme support (Dark/Light) follows Windows system preference (see "VCL Styles and Themes")
- Settings stored in INI files: `%APPDATA%\SVGPreviewHandler\Settings.ini` (preview) and `%APPDATA%\SVGTextEditor\Settings.ini` (editor), see `uSettings.pas`; `uRegistry.pas` holds the shell registration helpers and `DefaultStyleName`
- Logging/exception handling in `uLogExcept.pas`

## VCL Styles and Themes

- Linked styles (`Custom_Styles` of SVGTextEditor, SVGShellExtensions and SVGShellExtensions32): Glow, Sky, Windows Modern, Windows Modern SlateGray, Windows11 Impressive/Modern/Polar Light and Dark. The Windows10 styles are obsolete and were removed; do not add "Windows Modern Dark" (same resource name WINDOWSMODERNDARK of "Windows11 Modern Dark").
- The Theme page of Settings offers every VCL style linked into the executable (`TStyleManager.StyleNames`), except the system style "Windows". Light or dark is decided at run time by `StyleThemeType` in `uSettings.pas`: from the table of StyledComponents (`Vcl.StandardButtonStyles.GetStyleAttributes`, only when it returns an entry with the same name: for an unknown style it returns the attributes of "Windows"), otherwise from the luminance of the `clWindow` color of the style. There is no hand-kept list: adding a style to the dproj is enough.
- `DefaultStyleName` (`uRegistry.pas`): "Windows11 Modern Light/Dark" on Windows 11, "Windows Modern" / "Windows Modern SlateGray" on older systems, following the Windows light/dark theme.
- `UpdateApplicationStyle` (`EditorMainForm.pas`) applies the style with `TStyleManager.TrySetStyle(..., False)`: a stored style no longer linked falls back to `DefaultStyleName`, then to "Windows Modern", without raising.
- The same styles and code are used by SKIAShellExtensions, SVGShellExtensions, MarkdownShellExtensions and MarkdownHelpViewer: keep them aligned.

## Version Control and Documentation

- Subversion, not Git. The project `.res` files are regenerated by every build (icon and version info from the `.dproj`): they are in `svn:ignore` of `Source` and must not be committed.
- **Every time `README.md` is modified, regenerate `README.htm` with the tool** and commit the two files together; `README.htm` is never edited by hand. From the folder of the repository: `D:\ETHEA\MarkdownProcessor\Cmd\Win64\MarkDownToHTML.exe processfile -in:<repo>\README.md -out:<repo>\README.htm` with full Windows paths (from Git Bash the backslashes are lost: run it from PowerShell or cmd), in safe mode, i.e. without `-unsafe`. Raw HTML in `README.md` is dropped in safe mode (it becomes `<!-- raw HTML omitted -->`): use the Markdown syntax, also for images, and write HTML examples as code between backticks.
