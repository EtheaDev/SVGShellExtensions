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
{******************************************************************************}
unit FSplash;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, jpeg, SVGIconImage;

type
  TSplashForm = class(TForm)
    Image1: TImage;
    SVGIconImage1: TSVGIconImage;
    lbVersion: TLabel;
    lbLoad: TLabel;
    procedure FormShow(Sender: TObject);
  end;

var
  SplashForm: TSplashForm;

implementation

{$R *.DFM}

procedure GetVerInfo( const FileName : string;
  var MajorVersion, MinorVersion, Release, Build : integer);
var
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  MajorVersion := 0;
  MinorVersion := 0;
  Release := 0;
  Build := 0;

  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          MajorVersion := HIWORD(FI.dwFileVersionMS);
          MinorVersion := LOWORD(FI.dwFileVersionMS);
          Release := HIWORD(FI.dwFileVersionLS);
          Build := LOWORD(FI.dwFileVersionLS);
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

procedure TSplashForm.FormShow(Sender: TObject);
var
  MajorVersion : integer;
  MinorVersion : integer;
  Release : integer;
  Build : integer;
begin
  GetVerInfo( Application.ExeName,
    MajorVersion, MinorVersion, Release, Build );

  lbVersion.Caption := Format(lbVersion.Caption,
    [MajorVersion, MinorVersion]);
end;


end.
