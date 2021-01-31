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
unit uStreamPreviewHandler;

interface

uses
  ComObj,
  ActiveX,
  Classes,
  uPreviewHandler;

type
  TIStreamAdapter = class(TStream)
  private
    FBaseStream: IStream;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(Stream: IStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    property BaseStream: IStream read FBaseStream;
  end;

  TStreamPreviewHandler = class abstract(TPreviewHandler)
  public
    procedure DoPreview(Stream: TIStreamAdapter); virtual; abstract;
    class function GetComClass: TComClass; override; final;
  end;

implementation

uses
  PropSys,
  SysUtils;

type
  TComStreamPreviewHandler = class(TComPreviewHandler, IInitializeWithStream)
    // strict private
    function IInitializeWithStream.Initialize = IInitializeWithStream_Initialize;
    function IInitializeWithStream_Initialize(const pstream: IStream; grfMode: Cardinal): HRESULT; stdcall;
  private
    FIStream: IStream;
    FMode: Cardinal;
    function GetPreviewHandler: TStreamPreviewHandler;
  protected
    procedure InternalUnload; override;
    procedure InternalDoPreview; override;
    property PreviewHandler: TStreamPreviewHandler read GetPreviewHandler;
    property Mode: Cardinal read FMode write FMode;
    property IStream: IStream read FIStream write FIStream;
  end;

resourcestring
  sSetSizeNotImplemented = '%s.SetSize not implemented';

  { TComStreamPreviewHandler }
function TComStreamPreviewHandler.GetPreviewHandler: TStreamPreviewHandler;
begin
  Result := inherited PreviewHandler as TStreamPreviewHandler;
end;

function TComStreamPreviewHandler.IInitializeWithStream_Initialize(const pstream: IStream; grfMode: Cardinal): HRESULT;
begin
  FIStream := pstream;
  FMode := grfMode;
  Result := S_OK;
end;

procedure TComStreamPreviewHandler.InternalUnload;
begin
  FIStream := nil;
end;

procedure TComStreamPreviewHandler.InternalDoPreview;
var
  AStream: TIStreamAdapter;
begin
  AStream := TIStreamAdapter.Create(FIStream);
  try
    PreviewHandler.DoPreview(AStream);
  finally
    AStream.Free;
  end;
end;

{ TStreamPreviewHandler }
class function TStreamPreviewHandler.GetComClass: TComClass;
begin
  Result := TComStreamPreviewHandler;
end;

{ TIStreamAdapter }
constructor TIStreamAdapter.Create(Stream: IStream);
begin
  inherited Create;
  FBaseStream := Stream;
end;

function TIStreamAdapter.GetSize: Int64;
var
  statStg: TStatStg;
  grfStatFlag: Longint;
begin
  Result := -1;
  grfStatFlag := STATFLAG_NONAME;
  if BaseStream.Stat(statStg, grfStatFlag) = S_OK then
    Result := statStg.cbSize;
end;

function TIStreamAdapter.Read(var Buffer; Count: Longint): Longint;
begin
  BaseStream.Read(@Buffer, Count, @Result);
end;

function TIStreamAdapter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  LResult: LargeUInt;
begin
  BaseStream.Seek(Offset, Ord(Origin), LResult);
  Result := LResult;
end;

procedure TIStreamAdapter.SetSize(const NewSize: Int64);
begin
  raise EStreamError.CreateResFmt(@sSetSizeNotImplemented, [Classname]);
end;

procedure TIStreamAdapter.SetSize(NewSize: Longint);
begin
  raise EStreamError.CreateResFmt(@sSetSizeNotImplemented, [Classname]);
end;

end.
