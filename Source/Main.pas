{******************************************************************************}
{                                                                              }
{       SVG Shell Extensions: Shell extensions for SVG files                   }
{       (Preview Panel, Thumbnail Icon, SVG Editor)                            }
{                                                                              }
{       Copyright (c) 2021-2025 (Ethea S.r.l.)                                 }
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
{  The Original Code is Main.pas.                                              }
{  Delphi Preview Handler  https://github.com/RRUZ/delphi-preview-handler      }
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright 2011-2021 Rodrigo Ruz V.   }
{  All Rights Reserved.                                                        }
{******************************************************************************}
unit Main;

interface

implementation

uses
  uSVGThumbnailHandler,
  uSVGPreviewHandler,
  Winapi.ShlObj;

initialization
  {$IFDEF WIN64}
  TSVGPreviewHandler.RegisterPreview(MySVG_PreviewHandlerGUID_64,
    'SVG.PreviewHandler', 'Delphi SVG Preview Handler 64bit');
  {$ELSE}
  TSVGPreviewHandler.RegisterPreview(MySVG_PreviewHandlerGUID_32,
    'SVG.PreviewHandler', 'Delphi SVG Preview Handler 32bit');
  {$ENDIF}

  {$IFDEF WIN64}
  TSVGThumbnailProvider.RegisterThumbnailProvider(MySVG_ThumbnailProviderGUID,
    'SVG.ThumbnailProvider', 'Delphi SVG Thumbnail Provider 64bit');
  {$ELSE}
  TSVGThumbnailProvider.RegisterThumbnailProvider(MySVG_ThumbnailProviderGUID,
    'SVG.ThumbnailProvider', 'Delphi SVG Thumbnail Provider 32bit');
  {$ENDIF}

  //Invalidate the shell's cache for Preview Thumbnails
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

end.

