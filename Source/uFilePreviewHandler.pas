// **************************************************************************************************
//
// Unit uFilePreviewHandler
// unit for the Delphi Preview Handler  https://github.com/RRUZ/delphi-preview-handler
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uFilePreviewHandler.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2021 Rodrigo Ruz V.
// All Rights Reserved.
//
// *************************************************************************************************

unit uFilePreviewHandler;

interface

uses
  ComObj,
  uPreviewHandler;

type
  TFilePreviewHandler = class abstract(TPreviewHandler)
  public
    procedure DoPreview(const FilePath: String); virtual; abstract;
    class function GetComClass: TComClass; override; final;
  end;

implementation

uses
  Windows,
  PropSys,
  SysUtils;

type
  TComFilePreviewHandler = class(TComPreviewHandler, IInitializeWithFile)
    // strict private
    function IInitializeWithFile.Initialize = IInitializeWithFile_Initialize;
    function IInitializeWithFile_Initialize(pszFilePath: LPCWSTR; grfMode: DWORD): HRESULT; stdcall;
  private
    FFilePath: TFileName;
    FMode: DWORD;
    function GetPreviewHandler: TFilePreviewHandler;
  protected
    procedure InternalDoPreview; override;
    procedure InternalUnload; override;
    property PreviewHandler: TFilePreviewHandler read GetPreviewHandler;
    property FilePath: TFileName read FFilePath;
    property Mode: DWORD read FMode;
  end;

function TComFilePreviewHandler.GetPreviewHandler: TFilePreviewHandler;
begin
  Result := inherited PreviewHandler as TFilePreviewHandler;
end;

function TComFilePreviewHandler.IInitializeWithFile_Initialize(pszFilePath: LPCWSTR; grfMode: DWORD): HRESULT;
begin
  FFilePath := pszFilePath;
  FMode := grfMode;
  Result := S_OK;
end;

procedure TComFilePreviewHandler.InternalDoPreview;
begin
  PreviewHandler.DoPreview(FFilePath);
end;

procedure TComFilePreviewHandler.InternalUnload;
begin
  FFilePath := '';
end;

class function TFilePreviewHandler.GetComClass: TComClass;
begin
  Result := TComFilePreviewHandler;
end;

end.
