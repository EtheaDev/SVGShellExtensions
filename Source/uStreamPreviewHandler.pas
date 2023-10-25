// **************************************************************************************************
//
// Unit uStreamPreviewHandler
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

unit uStreamPreviewHandler;

interface

uses
  ComObj,
  ActiveX,
  Classes,
  uStreamAdapter,
  uPreviewHandler;

type
  TStreamPreviewHandler = class abstract(TPreviewHandler)
  public
    class function GetComClass: TComClass; override; final;
  end;

implementation

uses
  PropSys,
  SysUtils,
  uLogExcept;

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
  TLogPreview.Add('TComStreamPreviewHandler.IInitializeWithStream_Initialize Init');
  FIStream := pstream;
  FMode := grfMode;
  Result := S_OK;
  TLogPreview.Add('TComStreamPreviewHandler.IInitializeWithStream_Initialize Done');
end;

procedure TComStreamPreviewHandler.InternalUnload;
begin
  TLogPreview.Add('TComStreamPreviewHandler.InternalUnload');
  FIStream := nil;
end;

procedure TComStreamPreviewHandler.InternalDoPreview;
var
  AStream: TIStreamAdapter;
begin
  TLogPreview.Add('TComStreamPreviewHandler.InternalDoPreview');
  AStream := TIStreamAdapter.Create(FIStream);
  try
    CheckContainer;
    Container.LoadFromStream(AStream);
    Container.Show;
  finally
    AStream.Free;
  end;
end;

{ TStreamPreviewHandler }
class function TStreamPreviewHandler.GetComClass: TComClass;
begin
  Result := TComStreamPreviewHandler;
end;

end.
