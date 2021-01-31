// **************************************************************************************************
//
// Unit uSettings
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
// The Original Code is uSettings.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2021 Rodrigo Ruz V.
// All Rights Reserved.
//
// *************************************************************************************************

unit uSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.StdCtrls, SynEditTypes,
  SynEdit, uSVGSettings;

type
  TFrmSettings = class(TForm)
    Label1: TLabel;
    CbFont: TComboBox;
    EditFontSize: TEdit;
    Label2: TLabel;
    UpDown1: TUpDown;
    ButtonSave: TButton;
    Button1: TButton;
    cbShowEditor: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FSettingsChanged: Boolean;
    FOldStyle: string;
    procedure FillData;
  public
    procedure LoadCurrentValues(Settings: TSettings;
      SynEdit: TSynEdit; const EditorVisible: Boolean);
    property SettingsChanged: Boolean read FSettingsChanged;
  end;

implementation

{$R *.dfm}

uses
  IniFiles,
  System.Types,
  System.TypInfo,
  System.Rtti,
  System.StrUtils,
  System.IOUtils,
  Winapi.ShlObj,
  Vcl.Themes,
  uLogExcept,
  uRegistry,
  uMisc;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric; FontType: Integer; Data: Pointer): Integer; stdcall;
var
  List: TStrings;
begin
  List := TStrings(Data);
  if ((LogFont.lfPitchAndFamily and FIXED_PITCH) <> 0) then
    if not StartsText('@', LogFont.lfFaceName) and (List.IndexOf(LogFont.lfFaceName) < 0) then
      List.Add(LogFont.lfFaceName);

  Result := 1;
end;
procedure TFrmSettings.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFrmSettings.ButtonSaveClick(Sender: TObject);
begin
  FSettingsChanged := True;
  Close;
end;

procedure TFrmSettings.FillData;
var
  sDC: Integer;
  LogFont: TLogFont;
begin
  sDC := GetDC(0);
  try
    CbFont.Items.Clear;
    ZeroMemory(@LogFont, sizeof(LogFont));
    LogFont.lfCharset := DEFAULT_CHARSET;
    EnumFontFamiliesEx(sDC, LogFont, @EnumFontsProc, Winapi.Windows.LPARAM(CbFont.Items), 0);
  finally
    ReleaseDC(0, sDC);
  end;
end;

procedure TFrmSettings.FormCreate(Sender: TObject);
begin
  FSettingsChanged := False;
  FillData;
end;

procedure TFrmSettings.LoadCurrentValues(Settings: TSettings;
  SynEdit: TSynEdit; const EditorVisible: Boolean);
begin
  FOldStyle := TStyleManager.ActiveStyle.Name;
  Settings.UpdateSettings(SynEdit, EditorVisible);
  CbFont.ItemIndex := CbFont.Items.IndexOf(Settings.FontName);
  UpDown1.Position := Settings.FontSize;
  cbShowEditor.Checked := Settings.ShowEditor;
end;

end.
