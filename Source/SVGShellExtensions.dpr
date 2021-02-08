{******************************************************************************}
{                                                                              }
{       SVG Shell Extensions: Shell extensions for SVG files                   }
{       (Preview Panel, Thumbnail Icon, SVG Editor)                            }
{                                                                              }
{       Copyright (c) 2021 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/SVGShellExtensions                         }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{  The Original Code is:                                                       }
{  Delphi Preview Handler  https://github.com/RRUZ/delphi-preview-handler      }
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright 2011-2021 Rodrigo Ruz V.   }
{  All Rights Reserved.                                                        }
{******************************************************************************}
library SVGShellExtensions;

uses
 ComServ,
  Main in 'Main.pas',
  uPreviewHandler in 'uPreviewHandler.pas',
  uMisc in 'uMisc.pas',
  uStreamPreviewHandler in 'uStreamPreviewHandler.pas',
  uFilePreviewHandler in 'uFilePreviewHandler.pas',
  uPreviewContainer in 'uPreviewContainer.pas' {PreviewContainer},
  uRegistry in 'uRegistry.pas',
  uLogExcept in 'uLogExcept.pas',
  uCommonPreviewHandler in 'uCommonPreviewHandler.pas',
  uSVGPreviewHandler in 'uSVGPreviewHandler.pas',
  uPreviewHandlerRegister in 'uPreviewHandlerRegister.pas',
  uSVGThumbnailHandler in 'uSVGThumbnailHandler.pas',
  uThumbnailHandlerRegister in 'uThumbnailHandlerRegister.pas',
  uAbout in 'uAbout.pas' {FrmAbout},
  dlgSearchText in 'dlgSearchText.pas' {TextSearchDialog},
  {$IFDEF UEDITOR}
  uEditor in 'uEditor.pas' {FrmEditor},
  {$ELSE}
  SVGEditor in 'SVGEditor.pas' {FrmEditor},
  {$ENDIF }
  //uSVGThumbnailHandler in 'uSVGThumbnailHandler.pas',
  uSVGSettings in 'uSVGSettings.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  DllInstall;

{$R *.RES}

begin
end.
