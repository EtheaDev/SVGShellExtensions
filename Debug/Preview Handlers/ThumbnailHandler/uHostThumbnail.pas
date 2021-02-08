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

unit uHostThumbnail;

interface

uses
  ShlObj,
  Windows,
  Classes,
  Messages,
  Controls,
  uSVGThumbnailHandler;

type
  THostThumbnailProvider = class(TCustomControl)
  private
    FFileStream: TFileStream;
    FThumbnailGUIDStr: string;
    FFileName: string;
    FLoaded: Boolean;
    FThumbnailProvider: IThumbnailProvider;
    procedure SetFileName(const Value: string);
    procedure LoadThumbnailHandler;
    function LoadBitmapWithShellExtension(const ADllFileName: UnicodeString;
      const ACLSID: TCLSID; const AFileName: UnicodeString; ASize: Integer;
      out AAlpha: DWord): HBITMAP;
  protected
    procedure PaintWindow(DC: HDC); override;
  public
    property FileName: string read FFileName write SetFileName;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Graphics,
  ComObj,
  ActiveX,
  Registry,
  PropSys;

constructor THostThumbnailProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThumbnailProvider := nil;
  FThumbnailGUIDStr := '';
  FFileStream := nil;
end;

procedure THostThumbnailProvider.PaintWindow(DC: HDC);
const
  Msg = 'No preview available.';
var
  lpRect: TRect;
  LBitmapHandle: HBITMAP;
  LBitmapType: DWord;
begin
  if (FThumbnailGUIDStr <> '') and (FThumbnailProvider <> nil) and not FLoaded then
  begin
    FLoaded := True;
    FThumbnailProvider.GetThumbnail(Width, LBitmapHandle, LBitmapType);
  end
  else if FThumbnailGUIDStr = '' then
  begin
    lpRect := Rect(0, 0, Self.Width, Self.Height);
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := clWindowText;
    DrawText(Canvas.Handle, PChar(Msg), Length(Msg), lpRect, DT_VCENTER or DT_CENTER or DT_SINGLELINE);
  end;
end;

destructor THostThumbnailProvider.Destroy;
begin
  (*
  if (FThumbnailProvider <> nil) then
    FThumbnailProvider.Unload;
  *)

  if FFileStream <> nil then
    FreeAndNil(FFileStream);

  inherited;
end;

function GetThumbnailHandlerCLSID(const AFileName: string): string;
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
    LKey := LExt + '\shellex\' + ThumbnailProviderGUID;
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
    LKey := LFileClass + '\shellex\' + ThumbnailProviderGUID;
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

    LKey := 'SystemFileAssociations\' + LExt + '\shellex\' + ThumbnailProviderGUID;
    if LRegistry.KeyExists(LExt) and LRegistry.OpenKeyReadOnly(LKey) then
    begin
      Result := LRegistry.ReadString('');
      if Result <> '' then
        Exit
      else
        LRegistry.CloseKey();
    end;

    LKey := 'SystemFileAssociations\' + LPerceivedType + '\shellex\' + ThumbnailProviderGUID;
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

function THostThumbnailProvider.LoadBitmapWithShellExtension(const ADllFileName: UnicodeString; const ACLSID: TCLSID;
  const AFileName: UnicodeString; ASize: Integer; out AAlpha: DWord): HBITMAP;
type
  TDllGetClassObject = function(const CLSID, IID: TGUID; out Obj): HRESULT; stdcall;
var
  DllModule: HMODULE;
  DllGetClassObject: TDllGetClassObject;
  ClassFactory: IClassFactory;
  ThumbnailProvider: IThumbnailProvider;
  InitializeWithFile: IInitializeWithFile;
  PersistFile: IPersistFile;
begin
  DllModule := LoadLibraryW(PWideChar(ADllFileName));
  if DllModule = 0 then RaiseLastOSError;
  try
    @DllGetClassObject := GetProcAddress(DllModule, 'DllGetClassObject');
    if not Assigned(DllGetClassObject) then
      RaiseLastOSError;
    OleCheck(DllGetClassObject(ACLSID, IClassFactory, ClassFactory));
    try
      OleCheck(ClassFactory.CreateInstance(nil, IThumbnailProvider, ThumbnailProvider));
      try
        if Succeeded(ThumbnailProvider.QueryInterface(IInitializeWithFile, InitializeWithFile)) then
          try
            OleCheck(InitializeWithFile.Initialize(PWideChar(AFileName), STGM_READ));
          finally
            InitializeWithFile := nil;
          end
        else
          if Succeeded(ThumbnailProvider.QueryInterface(IPersistFile, PersistFile)) then
            try
              OleCheck(PersistFile.Load(PWideChar(AFileName), STGM_READ));
            finally
              PersistFile := nil;
            end
          else
            raise Exception.Create('Cannot initialize handler');

        OleCheck(ThumbnailProvider.GetThumbnail(ASize, Result, AAlpha));
      finally
        ThumbnailProvider := nil;
      end;
    finally
      ClassFactory := nil;
    end;
  finally
    FreeLibrary(DllModule);
  end;
end;

procedure THostThumbnailProvider.LoadThumbnailHandler;
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
  FThumbnailGUIDStr := GetThumbnailHandlerCLSID(FFileName);
  if FThumbnailGUIDStr = '' then
    Exit;

  if FFileStream <> nil then
    FreeAndNil(FFileStream);

  LPreviewGUID := StringToGUID(FThumbnailGUIDStr);

  FThumbnailProvider := CreateComObject(LPreviewGUID) As IThumbnailProvider;
  if (FThumbnailProvider = nil) then
    Exit;

  if FThumbnailProvider.QueryInterface(IInitializeWithFile, LInitializeWithFile) = S_OK
  then
    LInitializeWithFile.Initialize(StringToOleStr(FFileName), STGM_READ)
  else if FThumbnailProvider.QueryInterface(IInitializeWithStream,
    LInitializeWithStream) = S_OK then
  begin
    FFileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    LIStream := TStreamAdapter.Create(FFileStream, soReference) as IStream;
    LInitializeWithStream.Initialize(LIStream, STGM_READ);
  end
  else if FThumbnailProvider.QueryInterface(IInitializeWithItem,
    LInitializeWithItem) = S_OK then
  begin
    SHCreateItemFromParsingName(PChar(FileName), nil,
      StringToGUID(GUID_ISHELLITEM), LShellItem);
    LInitializeWithItem.Initialize(LShellItem, 0);
  end
  else
  begin
    //FThumbnailProvider.Unload;
    FThumbnailProvider := nil;
    Exit;
  end;

  prc := ClientRect;
  //FThumbnailProvider.SetWindow(Self.Handle, prc);
end;

procedure THostThumbnailProvider.SetFileName(const Value: string);
begin
  FFileName := Value;
  HandleNeeded;
  LoadThumbnailHandler;
end;

end.
