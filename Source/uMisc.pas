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
{  The Original Code is uMisc.pas:                                             }
{  Delphi Preview Handler  https://github.com/RRUZ/delphi-preview-handler      }
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright 2011-2021 Rodrigo Ruz V.   }
{  All Rights Reserved.                                                        }
{******************************************************************************}
unit uMisc;

interface

  function GetDllPath: String;
  function GetTempDirectory: string;
  function GetSpecialFolder(const CSIDL: integer): string;
  function GetFileVersion(const FileName: string): string;
  function  GetModuleLocation: string;
  procedure Initialize_GDI; stdcall;
  procedure Finalize_GDI; stdcall;
  procedure Initialize; stdcall;
  procedure Finalize; stdcall;

resourcestring
  STextNotFound = 'Text not found';

implementation

uses
  WinAPI.GDIPObj,
  WinAPI.GDIPApi,
  ComObj,
  System.SysUtils,
  WinApi.Windows,
  WinApi.ShlObj,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.GraphUtil,
  uRegistry,
  uLogExcept;

procedure Initialize_GDI; stdcall;
begin
  //Initialize GDI+
  TLogPreview.Add('GDI+: Initialize');
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs := False;
  StartupInput.GdiplusVersion := 1;
  GdiplusStartup(gdiplusToken, @StartupInput, nil);
end;

procedure Finalize_GDI; stdcall;
begin
  TLogPreview.Add('GDI+: Finalize');
  GdiplusShutdown(gdiplusToken);
end;

function GetDllPath: String;
var
  Path: array [0 .. MAX_PATH - 1] of Char;
begin
  SetString(Result, Path, GetModuleFileName(HInstance, Path, SizeOf(Path)));
end;

function GetTempDirectory: string;
var
  lpBuffer: array [0 .. MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @lpBuffer);
  Result := StrPas(lpBuffer);
end;

function GetWindowsDirectory: string;
var
  lpBuffer: array [0 .. MAX_PATH] of Char;
begin
  WinApi.Windows.GetWindowsDirectory(@lpBuffer, MAX_PATH);
  Result := StrPas(lpBuffer);
end;

function GetSpecialFolder(const CSIDL: integer): string;
var
  lpszPath: PWideChar;
begin
  lpszPath := StrAlloc(MAX_PATH);
  try
    ZeroMemory(lpszPath, MAX_PATH);
    if SHGetSpecialFolderPath(0, lpszPath, CSIDL, False) then
      Result := lpszPath
    else
      Result := '';
  finally
    StrDispose(lpszPath);
  end;
end;



function GetFileVersion(const FileName: string): string;
var
  FSO: OleVariant;
begin
  FSO := CreateOleObject('Scripting.FileSystemObject');
  Result := FSO.GetFileVersion(FileName);
end;

function  GetModuleLocation: string;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileName(HInstance, PChar(Result), MAX_PATH);
  Result:=PChar(Result);
end;

procedure Initialize; stdcall;
begin
  Initialize_GDI;
end;

procedure Finalize; stdcall;
begin
  Finalize_GDI;
end;

initialization

end.
