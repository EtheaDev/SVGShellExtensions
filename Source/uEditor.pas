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

unit uEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SynEdit, pngimage,
  System.Generics.Collections,
  SynEditHighlighter,
  ComCtrls, ToolWin, ImgList, SynHighlighterXML,
  Vcl.Menus, SynEditExport,
  SynExportHTML, SynExportRTF, SynEditRegexSearch, SynEditMiscClasses,
  SynEditSearch, uSVGSettings, System.ImageList, SynEditCodeFolding;

type
  TFrmEditor = class(TForm)
    SynEdit: TSynEdit;
    PanelTop: TPanel;
    PanelEditor: TPanel;
    SynXMLSyn: TSynXMLSyn;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButtonZoomIn: TToolButton;
    ToolButtonZommOut: TToolButton;
    ToolButtonAbout: TToolButton;
    ToolButtonSave: TToolButton;
    PanelToolBar: TPanel;
    SynExporterHTML1: TSynExporterHTML;
    SynExporterRTF1: TSynExporterRTF;
    dlgFileSaveAs: TSaveDialog;
    ToolButtonSearch: TToolButton;
    CoolBar1: TCoolBar;
    StatusBar1: TStatusBar;
    SynEditSearch1: TSynEditSearch;
    SynEditRegexSearch1: TSynEditRegexSearch;
    procedure FormCreate(Sender: TObject);
    procedure ToolButtonZoomInClick(Sender: TObject);
    procedure ToolButtonZommOutClick(Sender: TObject);
    procedure ToolButtonAboutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ExporttoHTML1Click(Sender: TObject);
    procedure Copynativeformattoclipboard1Click(Sender: TObject);
    procedure Copyastexttoclipboard1Click(Sender: TObject);
    procedure ExporttoRTF1Click(Sender: TObject);
    procedure Normal1Click(Sender: TObject);
    procedure Columns1Click(Sender: TObject);
    procedure Lines1Click(Sender: TObject);
    procedure ToolButtonSearchClick(Sender: TObject);
  private
    FFileName: string;
    fSearchFromCaret: boolean;
    FSettings: TSettings;

    class var FExtensions: TDictionary<TSynCustomHighlighterClass, TStrings>;
    class var  FAParent: TWinControl;

    procedure AppException(Sender: TObject; E: Exception);
    procedure ShowSearchDialog;
    procedure DoSearchText(ABackwards: boolean);
  protected
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    class property Extensions: TDictionary<TSynCustomHighlighterClass, TStrings> read FExtensions write FExtensions;
    class property AParent: TWinControl read FAParent write FAParent;
    procedure LoadFromFile(const FileName: string);
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

procedure TFrmEditor.Columns1Click(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  SynEdit.SelectionMode := smColumn;
end;

procedure TFrmEditor.Copyastexttoclipboard1Click(Sender: TObject);
var
  Exporter: TSynCustomExporter;
begin
  Exporter := SynExporterRTF1;
  with Exporter do
  begin
    Title := 'Source file exported to clipboard (as text)';
    ExportAsText := True;
    Highlighter := SynEdit.Highlighter;
    ExportAll(SynEdit.Lines);
    CopyToClipboard;
  end;
end;

procedure TFrmEditor.Copynativeformattoclipboard1Click(Sender: TObject);
begin
  Clipboard.Open;
  try
    Clipboard.AsText := SynEdit.Lines.Text;
    with SynExporterRTF1 do
    begin
      Title := 'Source file exported to clipboard (native format)';
      ExportAsText := FALSE;
      Highlighter := SynEdit.Highlighter;
      ExportAll(SynEdit.Lines);
      CopyToClipboard;
    end;
  finally
    Clipboard.Close;
  end;
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

procedure TFrmEditor.ExporttoHTML1Click(Sender: TObject);
var
  FileName: string;
  Exporter: TSynCustomExporter;
begin
  dlgFileSaveAs.Filter := SynExporterHTML1.DefaultFilter;
  if dlgFileSaveAs.Execute then
  begin
    FileName := dlgFileSaveAs.FileName;
    if ExtractFileExt(FileName) = '' then
      FileName := FileName + '.html';
    Exporter := SynExporterHTML1;
    if Assigned(Exporter) then
      with Exporter do
      begin
        Title := 'Source file exported to file';
        Highlighter := SynEdit.Highlighter;
        ExportAsText := True;
        ExportAll(SynEdit.Lines);
        SaveToFile(FileName);
      end;
  end;
end;

procedure TFrmEditor.ExporttoRTF1Click(Sender: TObject);
var
  FileName: string;
  Exporter: TSynCustomExporter;
begin
  dlgFileSaveAs.Filter := SynExporterRTF1.DefaultFilter;
  if dlgFileSaveAs.Execute then
  begin
    FileName := dlgFileSaveAs.FileName;
    if ExtractFileExt(FileName) = '' then
      FileName := FileName + '.rtf';
    Exporter := SynExporterRTF1;
    if Assigned(Exporter) then
      with Exporter do
      begin
        Title := 'Source file exported to file';
        Highlighter := SynEdit.Highlighter;
        ExportAsText := True;
        ExportAll(SynEdit.Lines);
        SaveToFile(FileName);
      end;
  end;
end;

procedure TFrmEditor.FormCreate(Sender: TObject);
begin
  FSettings := TSettings.Create;

  Application.OnException := AppException;
  TLogPreview.Add('TFrmEditor.FormCreate');
  SynEdit.Font.Size := FSettings.FontSize;
  SynEdit.Font.Name := FSettings.FontName;
end;

procedure TFrmEditor.FormDestroy(Sender: TObject);
begin
  TLogPreview.Add('TFrmEditor.FormDestroy');
  inherited;
end;

procedure TFrmEditor.Lines1Click(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  SynEdit.SelectionMode := smLine;
end;

procedure TFrmEditor.LoadFromFile(const FileName: string);
begin
  TLogPreview.Add('TFrmEditor.LoadFromFile Init');
  FFileName := FileName;
  SynEdit.Lines.LoadFromFile(FFileName);
  TLogPreview.Add('TFrmEditor.LoadFromFile Done');
end;

procedure TFrmEditor.Normal1Click(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  SynEdit.SelectionMode := smNormal;
end;

procedure TFrmEditor.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not (csLoading in componentstate) and (AWidth <> 320) and (AHeight <> 240)
    and (AWidth <> 0) and (AHeight <> 0) then
  begin
    TLogPreview.Add('TFrmEditor.SetBounds'+
      ' CurrentPPI: '+Self.CurrentPPI.ToString+
      ' AWidth: '+AWidth.ToString+
      ' AHeight: '+AHeight.ToString);
  end;
  inherited;
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

procedure TFrmEditor.ToolButtonSearchClick(Sender: TObject);
begin
  ShowSearchDialog();
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
