// **************************************************************************************************
//
// Unit uEditor
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
// The Original Code is uEditor.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2021 Rodrigo Ruz V.
// All Rights Reserved.
//
// *************************************************************************************************

unit SVGEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SynEdit, pngimage,
  System.Generics.Collections,
  SynEditHighlighter,
//  uDelphiIDEHighlight,
//  uDelphiVersions,
  SynHighlighterPas, ComCtrls, ToolWin, ImgList, SynHighlighterXML,
  SynHighlighterCpp, SynHighlighterAsm, uSynEditPopupEdit,
  SynHighlighterFortran, SynHighlighterEiffel, SynHighlighterPython,
  SynHighlighterPerl, SynHighlighterDfm, SynHighlighterBat,
  SynHighlighterVBScript, SynHighlighterPHP, SynHighlighterJScript,
  SynHighlighterHtml, SynHighlighterCSS, SynHighlighterCS, SynHighlighterCobol,
  SynHighlighterVB, SynHighlighterM3, SynHighlighterJava, SynHighlighterSml,
  SynHighlighterIni, SynHighlighterInno, SynHighlighterSQL,
  SynHighlighterUNIXShellScript, SynHighlighterRuby, Vcl.Menus, SynEditExport,
  SynExportHTML, SynExportRTF, SynEditRegexSearch, SynEditMiscClasses,
  SynEditSearch, uSettings, System.ImageList, SynEditCodeFolding,
  SVGIconImageListBase, SVGIconImageListBase, SVGIconImageList;

type
  //TProcRefreshSynHighlighter = procedure(FCurrentTheme: TIDETheme; SynEdit: SynEdit.TSynEdit);

  TFrmEditor = class(TForm)
    SynEdit: TSynEdit;
    PanelTop: TPanel;
    PanelEditor: TPanel;
    PanelToolBar: TPanel;
    SynXMLSyn: TSynXMLSyn;
    dlgFileSaveAs: TSaveDialog;
    StatusBar1: TStatusBar;
    SynEditSearch1: TSynEditSearch;
    SynEditRegexSearch1: TSynEditRegexSearch;
    ToolBar1: TToolBar;
    ToolButtonZoomIn: TToolButton;
    ToolButtonZommOut: TToolButton;
    ToolButtonSearch: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButton1: TToolButton;
    ToolButtonAbout: TToolButton;
    SVGIconImageList: TSVGIconImageList;
    ToolButtonEdit: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure ToolButtonZoomInClick(Sender: TObject);
    procedure ToolButtonZommOutClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
    procedure ToolButtonAboutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ToolButtonSearchClick(Sender: TObject);
    procedure ToolButtonSelectModeClick(Sender: TObject);
    procedure ToolButtonEditClick(Sender: TObject);
  private
    //FCurrentTheme: TIDETheme;
    FFileName: string;
    //FRefreshSynHighlighter: TProcRefreshSynHighlighter;
    //FListThemes: TStringList;
    fSearchFromCaret: boolean;
    FSettings: TSettings;

    class var FExtensions: TDictionary<TSynCustomHighlighterClass, TStrings>;
    class var FAParent: TWinControl;

    procedure AppException(Sender: TObject; E: Exception);
    procedure ShowSearchDialog;
    procedure DoSearchText(ABackwards: boolean);
  public
    //procedure FillThemes;
    //procedure LoadCurrentTheme;
    //property RefreshSynHighlighter: TProcRefreshSynHighlighter read FRefreshSynHighlighter write FRefreshSynHighlighter;

    class property Extensions: TDictionary<TSynCustomHighlighterClass, TStrings> read FExtensions write FExtensions;
    class property AParent: TWinControl read  FAParent write FAParent;
    procedure LoadFile(const FileName: string);
  end;


implementation

uses
  SynEditTypes,
  Vcl.Clipbrd,
  Vcl.Themes,
  uLogExcept,
  System.Types,
  Registry, uMisc, IOUtils, ShellAPI, ComObj, IniFiles, GraphUtil, uAbout,
  dlgSearchText;

const
  MaxfontSize = 30;
  MinfontSize = 8;

{$R *.dfm}
  { TFrmEditor }

procedure TFrmEditor.AppException(Sender: TObject; E: Exception);
begin
  // log unhandled exceptions (TSynEdit, etc)
  TLogPreview.Add('AppException');
  TLogPreview.Add(E);
end;

procedure TFrmEditor.DoSearchText(ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
  StatusBar1.SimpleText := '';
  Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not fSearchFromCaret then
    Include(Options, ssoEntireScope);
  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);

  if gbSearchRegex then
    SynEdit.SearchEngine := SynEditRegexSearch1
  else
    SynEdit.SearchEngine := SynEditSearch1;

  if SynEdit.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    StatusBar1.SimpleText := STextNotFound;
    if ssoBackwards in Options then
      SynEdit.BlockEnd := SynEdit.BlockBegin
    else
      SynEdit.BlockBegin := SynEdit.BlockEnd;
    SynEdit.CaretXY := SynEdit.BlockBegin;
  end;
end;

(*
procedure TFrmEditor.FillThemes;
var
  s, Theme: string;
  LMenuItem: TMenuItem;
begin
  if not TDirectory.Exists(TSettings.PathThemes) then
    exit;

  for Theme in TDirectory.GetFiles(TSettings.PathThemes, '*' + sThemesExt) do
  begin
    s := TSettings.GetThemeNameFromFile(Theme);
    FListThemes.Add(s);
    LMenuItem := TMenuItem.Create(PopupMenuThemes);
    PopupMenuThemes.Items.Add(LMenuItem);
    LMenuItem.Caption := s;
    LMenuItem.RadioItem := True;
    LMenuItem.OnClick := MenuThemeOnCLick;
    LMenuItem.Tag := FListThemes.Count - 1;
  end;

  // if ComboBoxThemes.Items.Count > 0 then
  // ComboBoxThemes.ItemIndex := ComboBoxThemes.Items.IndexOf(GetThemeNameFromFile(ThemeName));
end;
*)

procedure TFrmEditor.FormCreate(Sender: TObject);
begin
  FSettings := TSettings.Create;

  Application.OnException := AppException;
  TLogPreview.Add('FormCreate');
  SynEdit.Font.Size := FSettings.FontSize;
  SynEdit.Font.Name := FSettings.FontName;
//  SynEdit.SelectionMode := FSettings.SelectionMode;
  //FListThemes := TStringList.Create;
//  FillThemes;
end;

procedure TFrmEditor.FormDestroy(Sender: TObject);
begin
  //FListThemes.Free;
  TLogPreview.Add('FormDestroy');
end;

procedure TFrmEditor.LoadFile(const FileName: string);
begin
  TLogPreview.Add('TFrmEditor.LoadFile Init');
  FFileName := FileName;
  SynEdit.Lines.LoadFromFile(FFileName);
  TLogPreview.Add('TFrmEditor.LoadFile Done');
end;

procedure TFrmEditor.ShowSearchDialog;
var
  LTextSearchDialog: TTextSearchDialog;
  LRect: TRect;
  i: integer;
begin
  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i].ClassType = TTextSearchDialog then
    begin
      Screen.Forms[i].BringToFront;
      exit;
    end;

  StatusBar1.SimpleText := '';
  LTextSearchDialog := TTextSearchDialog.Create(Self);
  try
    LTextSearchDialog.SearchBackwards := gbSearchBackwards;
    LTextSearchDialog.SearchCaseSensitive := gbSearchCaseSensitive;
    LTextSearchDialog.SearchFromCursor := gbSearchFromCaret;
    LTextSearchDialog.SearchInSelectionOnly := gbSearchSelectionOnly;

    LTextSearchDialog.SearchText := gsSearchText;
    if gbSearchTextAtCaret then
    begin
      if SynEdit.SelAvail and (SynEdit.BlockBegin.Line = SynEdit.BlockEnd.Line) then
        LTextSearchDialog.SearchText := SynEdit.SelText
      else
        LTextSearchDialog.SearchText := SynEdit.GetWordAtRowCol(SynEdit.CaretXY);
    end;

    LTextSearchDialog.SearchTextHistory := gsSearchTextHistory;
    LTextSearchDialog.SearchWholeWords := gbSearchWholeWords;

    if Self.Parent <> nil then
    begin
      GetWindowRect(Self.Parent.ParentWindow, LRect);
      LTextSearchDialog.Left := (LRect.Left + LRect.Right - LTextSearchDialog.Width) div 2;
      LTextSearchDialog.Top := (LRect.Top + LRect.Bottom - LTextSearchDialog.Height) div 2;
    end;

    if LTextSearchDialog.ShowModal() = mrOK then
    begin
      gbSearchBackwards := LTextSearchDialog.SearchBackwards;
      gbSearchCaseSensitive := LTextSearchDialog.SearchCaseSensitive;
      gbSearchFromCaret := LTextSearchDialog.SearchFromCursor;
      gbSearchSelectionOnly := LTextSearchDialog.SearchInSelectionOnly;
      gbSearchWholeWords := LTextSearchDialog.SearchWholeWords;
      gbSearchRegex := LTextSearchDialog.SearchRegularExpression;
      gsSearchText := LTextSearchDialog.SearchText;
      gsSearchTextHistory := LTextSearchDialog.SearchTextHistory;
      fSearchFromCaret := gbSearchFromCaret;

      if gsSearchText <> '' then
      begin
        DoSearchText(gbSearchBackwards);
        fSearchFromCaret := True;
      end;
    end;
  finally
    LTextSearchDialog.Free;
  end;
end;

procedure TFrmEditor.ToolButtonAboutClick(Sender: TObject);
var
  LFrm: TFrmAbout;
  LRect: TRect;
  i: integer;
begin

  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i].ClassType = TFrmAbout then
    begin
      Screen.Forms[i].BringToFront;
      exit;
    end;

  LFrm := TFrmAbout.Create(nil);
  try
    if Self.Parent <> nil then
    begin
      GetWindowRect(Self.Parent.ParentWindow, LRect);
      LFrm.Left := (LRect.Left + LRect.Right - LFrm.Width) div 2;
      LFrm.Top := (LRect.Top + LRect.Bottom - LFrm.Height) div 2;
    end;

    LFrm.ShowModal();
  finally
    LFrm.Free;
  end;
end;

procedure TFrmEditor.ToolButtonEditClick(Sender: TObject);
begin
  ShowMessage('SVG Editor... coming soon');
end;

procedure TFrmEditor.ToolButtonSaveClick(Sender: TObject);
var
  LFrm: TFrmSettings;
  LRect: TRect;
  i: integer;
begin

  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i].ClassType = TFrmSettings then
    begin
      Screen.Forms[i].BringToFront;
      exit;
    end;

  LFrm := TFrmSettings.Create(nil);
  try
    LFrm.LoadCurrentValues(SynEdit);
    if Self.Parent <> nil then
    begin
      GetWindowRect(Self.Parent.ParentWindow, LRect);
      LFrm.Left := (LRect.Left + LRect.Right - LFrm.Width) div 2;
      LFrm.Top := (LRect.Top + LRect.Bottom - LFrm.Height) div 2;
    end;
    // if Self.Parent <> nil then
    // LFrm.ParentWindow:=Self.Parent.ParentWindow;

    LFrm.ShowModal();
    if LFrm.SettingsChanged then
    begin
      FSettings.ReadSettings;
      SynEdit.Font.Size := FSettings.FontSize;
      SynEdit.Font.Name := FSettings.FontName;
    end;

  finally
    LFrm.Free;
  end;
end;

procedure TFrmEditor.ToolButtonSearchClick(Sender: TObject);
begin
  ShowSearchDialog();
end;

procedure TFrmEditor.ToolButtonSelectModeClick(Sender: TObject);
begin
  TToolButton(Sender).CheckMenuDropdown;
end;

procedure TFrmEditor.ToolButtonZommOutClick(Sender: TObject);
begin
  if SynEdit.Font.Size > MinfontSize then
    SynEdit.Font.Size := SynEdit.Font.Size - 1;
end;

procedure TFrmEditor.ToolButtonZoomInClick(Sender: TObject);
begin
  if SynEdit.Font.Size < MaxfontSize then
    SynEdit.Font.Size := SynEdit.Font.Size + 1;
end;

end.
