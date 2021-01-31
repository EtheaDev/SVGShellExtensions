# SVG Shell Extensions [![License](https://img.shields.io/badge/License-Apache%202.0-yellowgreen.svg)](https://opensource.org/licenses/Apache-2.0)

**A collection of extensions tools for SVG files, integrated into Microsoft Windows Explorer:

- a [preview handler](http://msdn.microsoft.com/en-us/magazine/cc163487.aspx) 
- a [Thumbnail handler](https://docs.microsoft.com/en-us/windows/win32/shell/thumbnail-providers)
- an SVG Text Editor

for Windows Vista, 7, 8 and 10 which allows you to see the SVG image directly into Windows Explorer and read the xml content without open the file in the Panel preview.

### Features ###
* Supports Windows Vista, 7, 8 and 10 on 32 and 64 bits.
* Themes (Dark and Light) according to user preferences of Windows Theme

### Important Note about installing a new version ###
In order to avoid problems you must follow these steps when you install or register a new 
version of SVG Shell Extensions.

  1. Close all the windows explorer instances which have the preview handler active or the
     preview handler was used (remember the dll remains in memory until the windows explorer 
     was closed).
  2. Unregister the previous version executing the uninstaller located in 
     `C:\Program Files (x86)\Ethea\SVGShellExtensions` or 
     `C:\Program Files\Ethea\SVGShellExtensions`
  3. If you install the SVG Shell Extensions manually you must unregister using the `UnRegister.bat`
     running as admin.
  4. Now proceed with the installation of the new version.

## Release Notes ##

31 Jan 2021
- Added SVG file Preview panel
