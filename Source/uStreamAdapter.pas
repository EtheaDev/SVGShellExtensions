// **************************************************************************************************
//
// Unit uStreamAdapter
// unit for the Delphi Preview Handler https://github.com/RRUZ/delphi-preview-handler
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uStreamPreviewHandler.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2021 Rodrigo Ruz V.
// All Rights Reserved.
//
// *************************************************************************************************

unit uStreamAdapter;

interface

uses
  ComObj,
  ActiveX,
  Classes;

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

implementation

uses
  PropSys,
  SysUtils;

resourcestring
  sSetSizeNotImplemented = '%s.SetSize not implemented';

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
