﻿# SVG Shell Extensions [![License](https://img.shields.io/badge/License-Apache%202.0-yellowgreen.svg)](https://opensource.org/licenses/Apache-2.0)

**Latest Version 3.3.0 - 01 Mar 2025**

**A collection of extensions tools for SVG files, integrated into Microsoft Windows Explorer (for Windows 10 and 11):**

- A [Preview handler][1]  which allows you to see the SVG image and text without open it, in the "Preview Panel".
 
- A [Thumbnail handler][2] which allows you to see the SVG image into Windows Explorer.

- An [SVG-XML Text Editor][12] to manually edit and preview the text of SVG files.

### Features

- Supports Windows 10 and 11 (for 32 bits and 64 bits).

- Themes (Dark and Light) according to user preferences of Windows Theme

- Integrated also with other Shell Applications (like XYplorer)

### Setup using the Installer

Click to download the [SVGShellExtensionsSetup.exe][3] located also in the Release area. The Installer works both for 32 and 64 bit system.

![SVG Setup_Program](./Images/Setup.png)

***For a clean Setup close all the windows explorer instances which have the preview handler active or the preview handler was used (remember the dll remains in memory until the windows explorer was closed).***

### Preview Panel and Thumbnails in action ###

In Windows 10 with Light theme:

![Preview Thumbnails Light](./Images/PreviewThumbnailsLight.png)

In Windows 10 with Dark theme:

![Preview Thumbnails Dark](./Images/PreviewThumbnailsDark.png)

### SVG Text Editor

A useful Text editor with preview of SVG Image:

![SVG Text Editor Dark](./Images/SVGTextEditorDark.png)

### Support for XYplorer ###

As requested by many users of popular shell alternative, the new version supports XYplorer for Thumbnails and Preview Panel: follow the [configuration guide...](https://github.com/EtheaDev/SVGShellExtensions/wiki/XYplorer-Support)

![XYplorer](./Images/XYplorer_Preview.png)

### Manual Build and Installation (for Delphi developers) ###

If you have Delphi 12, you can manually build the project:

***Warning: To build the DLL you need also other open-source projects***

- [SVGIconImageList][4]

- [Synedit][5]

- [VCL-Style-Utils][6]

To manually install the SVGShellExtensions.dll follow these steps:

1. Close all the windows explorer instances which have the preview handler active or the preview handler was used (remember the dll remains in memory until the windows explorer was closed).
  
2. If you have already used the installer uninstall the components from system.
     
3. To install manually the dll run the `Setup\Register64bit.bat` (run-as-administrator).

4. If you wanto to uninstall the dll you use the `Setup\UnRegister64Bit.bat`

5. When it's registered, you can continue to change code and rebuild the dll (beware to close all Explorer instances).

## Release Notes ##

01 Mar 2025: ver. 3.3.0
- Added "Wordwrap option" for the editor (active by default)
- Updated the latest Image32 library
- Added Color options in Viewer GUI (Grayscale, FixedColor, ApplyFixedColorToRootOnly)
- Added Color options in Editor GUI
- Updated to Image32 4.6 Released Feb 2025 to fix some drawing issue

14 Sep 2024: ver. 3.2.3
- Built with Delphi 12.2
- Update Image32 Engine to fix rendering of some SVG Icons

27 Aug 2024: ver. 3.2.2
- Use of TFormTabsBar component (Delphi 12) for paging
- Use of Styledmessage dialogs
- Updated Setup to show errors registering dlls

28 May 2024: ver. 3.2.1
- Aligned to latest Image32 Library
- Fixed Initialization of GDI+
- Added File Changed notification and reload
- Added use of StyledComponents and Rounded Buttons in Editor and Viewer

08 Jan 2024: ver. 3.2.0
- Aligned to latest Image32 Library
- Updated Copyright
- Added Clipboard copy SVG as PNG
- Built with Delphi 12

25 Oct 2023: ver. 3.1.0
- Added "close button" over Tabs
- Removed "TSVG" rendering option
- Updated to Image32 ver. 4.4 (30 Jan 2023)
- Updated to Skia4Delphi ver. 6.0.0 beta 5
- Added support for Delphi 12

24 Sep 2022: ver. 3.0.0
- Fixed Scrollbar colors viewing text in preview
- Fixed "empty panel" viewing text in preview
- Fixed "context-menu" for export to PNG (image lost)

18 Sep 2022: ver. 2.9.0
- Fixed Preview size in multi-monitor
- Fixed flickering of Preview

13 Sep 2022: ver. 2.8.0
- Fixed load file with blanks in content menu
- Built with Delphi 11.2

10 Apr 2022 - Version 2.7.0
- Updated Image32 Library
- Built with Delphi 11.1

16 Feb 2022 - Version 2.6.1
- Updated Image32 Library

20 Jan 2022 - Version 2.6
- Added support for Windows 11
- Updated Image32 Library
 - Fixed resize content

06 Nov 2021 - Version 2.5
- Fixed Export to png files

23 Oct 2021 - Version 2.4
- Added new Windows11 Styles (Dark and Light)

04 Sep 2021 - Version 2.3
- Added support for Delphi 11
- Built with Delphi 11
- Updated Image32 Library

03 Aug 2021 - Version 2.2
- Fixed PreferDirect2D default to false

31 Jul 2021 - Version 2.1
- Updated Image32 to 3.0 version
- Updated some sample images

25 Jul 2021 - Version 2.0
- Changed default SVG engine to Image32 for better results
- Updated Setup

05 Jul 2021
- Added drag-drop support for external files into editor
- Updated Setup with certificate

21 Jun 2021
- Updated some icons
- Update icon preview when SVGFactory changes

03 Apr 2021
- Fixed Close-All (Editor): clear preview
- Fixed save to png file extensions
- Removed memory-leak check closing app

05 Mar 2021
- Fixed SVG XML Editor preview after loading file
- Added support for storing/remember Editor options
- Some minor fixes

28 Feb 2021
- Fixed Context-Menu

27 Feb 2021
- Added 32 bit support
- Fixed some issue into SVG Text Editor

22 Feb 2021
- Added SVG Text Editor
- Added new Settings dialog
- Added Context-menu to open Editor and export to PNG
- Added Direct2D Engine for Preview Panel

09 Feb 2021
- Added Setup asset
- Fixed check for active Windows Theme
- Released v.1.0.0
- Released v.1.0.1

08 Feb 2021
- Added SVG Thumbnail Provider

31 Jan 2021
- Added SVG file Preview panel

## Credits

Many thanks to **Rodrigo Ruz V.** (author of [theroadtodelphi.com][7] Blog) for his wonderful work on [delphi-preview-handler][8] from which this project has used a lot of code and inspiration.

## License

Licensed under the [Apache License, Version 2.0][9] (the "License");
Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

Third Party libraries and tools used from Ethea:

- [SVGIconImageList][4]

The Initial Developer of the [Original Code][8] is **Rodrigo Ruz V**. Portions created by him are Copyright © 2011-2021 Rodrigo Ruz V.

Third Party libraries and tools used:

- [VCLStyles Utils][6]

- [SynEdit][5]


![Delphi Support](/Setup/SupportingDelphi.jpg)

Related links: [embarcadero.com][10] - [learndelphi.org][11]


[1]: https://docs.microsoft.com/en-us/windows/win32/shell/preview-handlers

[2]: https://docs.microsoft.com/en-us/windows/win32/shell/thumbnail-providers

[3]: https://github.com/EtheaDev/SVGShellExtensions/releases/latest/download/SVGShellExtensionsSetup.exe

[4]: https://github.com/EtheaDev/SVGIconImageList

[5]: https://github.com/SynEdit/SynEdit

[6]: https://github.com/RRUZ/vcl-styles-utils

[7]: https://theroadtodelphi.com/

[8]: https://github.com/RRUZ/delphi-preview-handler

[9]: https://opensource.org/licenses/Apache-2.0

[10]: https://www.embarcadero.com/

[11]: https://learndelphi.org/

[12]: https://github.com/EtheaDev/SVGShellExtensions/wiki/Using-The-SVG-Text-Editor
