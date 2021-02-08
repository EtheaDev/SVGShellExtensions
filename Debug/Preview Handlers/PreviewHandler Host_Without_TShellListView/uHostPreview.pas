// **************************************************************************************************
//
// Unit uHostPreview
// Component for host preview handlers
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uHostPreview.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.   Copyright (C) 2013.
// All Rights Reserved.
//
// **************************************************************************************************

unit uHostPreview;

interface

uses
  ShlObj,
  Classes,
  Messages,
  Controls;

type
  THostPreviewHandler = class(TCustomControl)
  private
    FFileStream: TFileStream;
    FPreviewGUIDStr: string;
    FFileName: string;
    FLoaded: Boolean;
    FPreviewHandler: IPreviewHandler;
    procedure SetFileName(const Value: string);
    procedure LoadPreviewHandler;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure Paint; override;
  public
    property FileName: string read FFileName write SetFileName;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Windows,
  Graphics,
  ComObj,
  ActiveX,
  Registry,
  PropSys;

constructor THostPreviewHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPreviewHandler := nil;
  FPreviewGUIDStr := '';
  FFileStream := nil;
end;

procedure THostPreviewHandler.Paint;
const
  Msg = 'No preview available.';
var
  lpRect: TRect;
begin
  if (FPreviewGUIDStr <> '') and (FPreviewHandler <> nil) and not FLoaded then
  begin
    FLoaded := True;
    FPreviewHandler.DoPreview;
    FPreviewHandler.SetFocus;
  end
  else if FPreviewGUIDStr = '' then
  begin
    lpRect := Rect(0, 0, Self.Width, Self.Height);
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := clWindowText;
    DrawText(Canvas.Handle, PChar(Msg), Length(Msg), lpRect, DT_VCENTER or DT_CENTER or DT_SINGLELINE);
  end;
end;

destructor THostPreviewHandler.Destroy;
begin
  if (FPreviewHandler <> nil) then
    FPreviewHandler.Unload;

  if FFileStream <> nil then
    FreeAndNil(FFileStream);

  inherited;
end;

function GetPreviewHandlerCLSID(const AFileName: string): string;
const
  SID_IPreviewHandler = '{8895B1C6-B41F-4C1C-A562-0D564250836F}';
var
  LRegistry: TRegistry;
  LExt, LFileClass, LPerceivedType, LKey: String;
begin
  Result := '';
  LExt := ExtractFileExt(AFileName);
  LRegistry := TRegistry.Create(KEY_READ);
  try

    // Check on the registered Preview Handlers using the file extension
    LRegistry.RootKey := HKEY_CLASSES_ROOT;
    LKey := LExt + '\shellex\' + SID_IPreviewHandler;
    if LRegistry.KeyExists(LKey) and LRegistry.OpenKeyReadOnly(LKey) then
    begin
      Result := LRegistry.ReadString('');
      if Result <> '' then
        Exit;
    end;

    // Get the perceived Types
    // https://msdn.microsoft.com/en-us/library/windows/desktop/cc144150(v=vs.85).aspx
    if (LRegistry.OpenKeyReadOnly(LExt)) then
    begin
      LFileClass := LRegistry.ReadString('');
      LPerceivedType := LRegistry.ReadString('PerceivedType');
      LRegistry.CloseKey();
    end;

    // Check on the registered Preview Handlers using  the file class
    LKey := LFileClass + '\shellex\' + SID_IPreviewHandler;
    if (LFileClass <> '') and LRegistry.KeyExists(LKey) and
      LRegistry.OpenKeyReadOnly(LKey) then
    begin
      Result := LRegistry.ReadString('');
      if Result <> '' then
        Exit
      else
        LRegistry.CloseKey();
    end;

    // Finding the associated preview handler for some extensions can require an additional search
    // in the HKEY_CLASSES_ROOT\SystemFileAssociations

    LKey := 'SystemFileAssociations\' + LExt + '\shellex\' + SID_IPreviewHandler;
    if LRegistry.KeyExists(LExt) and LRegistry.OpenKeyReadOnly(LKey) then
    begin
      Result := LRegistry.ReadString('');
      if Result <> '' then
        Exit
      else
        LRegistry.CloseKey();
    end;

    LKey := 'SystemFileAssociations\' + LPerceivedType + '\shellex\' + SID_IPreviewHandler;
    if LRegistry.KeyExists(LExt) and LRegistry.OpenKeyReadOnly(LKey) then
    begin
      Result := LRegistry.ReadString('');
      if Result <> '' then
        Exit
      else
        LRegistry.CloseKey();
    end;

  finally
    LRegistry.CloseKey;
    LRegistry.Free;
  end;
end;

procedure THostPreviewHandler.LoadPreviewHandler;
const
  GUID_ISHELLITEM = '{43826d1e-e718-42ee-bc55-a1e261c37bfe}';
var
  prc: TRect;
  LPreviewGUID: TGUID;
  LInitializeWithFile: IInitializeWithFile;
  LInitializeWithStream: IInitializeWithStream;
  LInitializeWithItem: IInitializeWithItem;
  LIStream: IStream;
  LShellItem: IShellItem;
begin

  FLoaded := False;
  FPreviewGUIDStr := GetPreviewHandlerCLSID(FFileName);
  if FPreviewGUIDStr = '' then
    Exit;

  if FFileStream <> nil then
    FreeAndNil(FFileStream);

  LPreviewGUID := StringToGUID(FPreviewGUIDStr);

  FPreviewHandler := CreateComObject(LPreviewGUID) As IPreviewHandler;
  if (FPreviewHandler = nil) then
    Exit;

  if FPreviewHandler.QueryInterface(IInitializeWithFile, LInitializeWithFile) = S_OK
  then
    LInitializeWithFile.Initialize(StringToOleStr(FFileName), STGM_READ)
  else if FPreviewHandler.QueryInterface(IInitializeWithStream,
    LInitializeWithStream) = S_OK then
  begin
    FFileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    LIStream := TStreamAdapter.Create(FFileStream, soReference) as IStream;
    LInitializeWithStream.Initialize(LIStream, STGM_READ);
  end
  else if FPreviewHandler.QueryInterface(IInitializeWithItem,
    LInitializeWithItem) = S_OK then
  begin
    SHCreateItemFromParsingName(PChar(FileName), nil,
      StringToGUID(GUID_ISHELLITEM), LShellItem);
    LInitializeWithItem.Initialize(LShellItem, 0);
  end
  else
  begin
    FPreviewHandler.Unload;
    FPreviewHandler := nil;
    Exit;
  end;

  prc := ClientRect;
  FPreviewHandler.SetWindow(Self.Handle, prc);
end;

procedure THostPreviewHandler.SetFileName(const Value: string);
begin
  FFileName := Value;
  HandleNeeded;
  LoadPreviewHandler;
end;

procedure THostPreviewHandler.WMSize(var Message: TWMSize);
var
  prc: TRect;
begin
  inherited;
  if FPreviewHandler <> nil then
  begin
    prc := ClientRect;
    FPreviewHandler.SetRect(prc);
  end;
end;

end.
