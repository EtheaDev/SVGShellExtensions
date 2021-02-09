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

unit SVGEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SynEdit, pngimage,
  System.Generics.Collections,
  SynEditHighlighter,
  ComCtrls, ToolWin, ImgList, SynHighlighterXML,
  Vcl.Menus, SynEditExport,
  SynExportHTML, SynExportRTF, SynEditRegexSearch, SynEditMiscClasses,
  SynEditSearch, uSVGSettings, System.ImageList, SynEditCodeFolding,
  SVGIconImageList, SVGIconImageListBase, SVGIconImage;

type
  TFrmEditor = class(TForm)
    SynEdit: TSynEdit;
    PanelTop: TPanel;
    PanelEditor: TPanel;
    SynXMLSyn: TSynXMLSyn;
    StatusBar: TStatusBar;
    SynEditSearch: TSynEditSearch;
    SynEditRegexSearch: TSynEditRegexSearch;
    SVGIconImageList: TSVGIconImageList;
    ToolButtonZoomIn: TToolButton;
    ToolButtonZommOut: TToolButton;
    ToolButtonSearch: TToolButton;
    ToolBar: TToolBar;
    ToolButtonFont: TToolButton;
    ToolButtonAbout: TToolButton;
    SeparatorEditor: TToolButton;
    ToolButtonShowText: TToolButton;
    ToolButtonReformat: TToolButton;
    FontDialog: TFontDialog;
    SynXMLSynDark: TSynXMLSyn;
    ImagePanel: TPanel;
    SVGIconImage: TSVGIconImage;
    Splitter: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure ToolButtonZoomInClick(Sender: TObject);
    procedure ToolButtonZommOutClick(Sender: TObject);
    procedure ToolButtonFontClick(Sender: TObject);
    procedure ToolButtonAboutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ToolButtonSearchClick(Sender: TObject);
    procedure ToolButtonSelectModeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ToolButtonShowTextClick(Sender: TObject);
    procedure ToolButtonReformatClick(Sender: TObject);
    procedure ToolButtonMouseEnter(Sender: TObject);
    procedure ToolButtonMouseLeave(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
    FFontSize: Integer;
    FSimpleText: string;
    FFileName: string;
    fSearchFromCaret: boolean;
    FSettings: TSettings;

    class var FExtensions: TDictionary<TSynCustomHighlighterClass, TStrings>;
    class var FAParent: TWinControl;

    procedure AppException(Sender: TObject; E: Exception);
    procedure ShowSearchDialog;
    procedure DoSearchText(ABackwards: boolean);
    procedure UpdateGUI;
    procedure UpdateFromSettings;
    procedure SaveSettings;
    function GetEditorFontSize: Integer;
    procedure SetEditorFontSize(const Value: Integer);
    procedure UpdateHighlighter;
  protected
  public
    procedure ScaleControls(const ANewPPI: Integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class property Extensions: TDictionary<TSynCustomHighlighterClass, TStrings> read FExtensions write FExtensions;
    class property AParent: TWinControl read FAParent write FAParent;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(const AStream: TStream);
    property EditorFontSize: Integer read GetEditorFontSize write SetEditorFontSize;
  end;


implementation

uses
  SynEditTypes,
  Vcl.Clipbrd,
  Vcl.Themes,
  uLogExcept,
  System.Types,
  Registry,
  uMisc,
  IOUtils,
  ShellAPI,
  ComObj,
  IniFiles,
  GraphUtil,
  uAbout,
  Xml.XMLDoc,
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

constructor TFrmEditor.Create(AOwner: TComponent);
begin
  FSettings := TSettings.Create;
  inherited;
end;

destructor TFrmEditor.Destroy;
begin
  FreeAndNil(FSettings);
  inherited;
end;

procedure TFrmEditor.DoSearchText(ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
  StatusBar.SimpleText := '';
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
    SynEdit.SearchEngine := SynEditRegexSearch
  else
    SynEdit.SearchEngine := SynEditSearch;

  if SynEdit.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    StatusBar.SimpleText := STextNotFound;
    if ssoBackwards in Options then
      SynEdit.BlockEnd := SynEdit.BlockBegin
    else
      SynEdit.BlockBegin := SynEdit.BlockEnd;
    SynEdit.CaretXY := SynEdit.BlockBegin;
  end;
end;

procedure TFrmEditor.UpdateGUI;
begin
  if PanelEditor.Visible then
  begin
    Splitter.Top := PanelEditor.Top + PanelEditor.Height;
    Splitter.Visible := True;
    ToolButtonShowText.Caption := 'Hide Text';
    ToolButtonShowText.Hint := 'Hide content of SVG file';
    ToolButtonShowText.ImageName := 'hide-text';
  end
  else
  begin
    Splitter.Visible := False;
    ToolButtonShowText.Caption := 'Show Text';
    ToolButtonShowText.Hint := 'Show content of SVG file';
    ToolButtonShowText.ImageName := 'show-text';
  end;
  ToolButtonShowText.Visible := True;
  ToolButtonAbout.Visible := True;
  ToolButtonFont.Visible := PanelEditor.Visible;
  ToolButtonReformat.Visible := PanelEditor.Visible;
  ToolButtonSearch.Visible := PanelEditor.Visible;
  ToolButtonZoomIn.Visible := PanelEditor.Visible;
  ToolButtonZommOut.Visible := PanelEditor.Visible;
end;

procedure TFrmEditor.UpdateHighlighter;
var
  LBackgroundColor: TColor;
begin
  if FSettings.UseDarkStyle then
  begin
    SynEdit.Highlighter := SynXMLSynDark;
  end
  else
  begin
    SynEdit. Highlighter := SynXMLSyn;
  end;
  SynEdit.Gutter.Font.Color := StyleServices.GetSystemColor(clWindowText);
  SynEdit.Gutter.Color := StyleServices.GetSystemColor(clBtnFace);
  LBackgroundColor := StyleServices.GetSystemColor(clWindow);

  SynXMLSynDark.ElementAttri.Background := LBackgroundColor;
  SynXMLSynDark.AttributeAttri.Background := LBackgroundColor;
  SynXMLSynDark.NamespaceAttributeAttri.Background := LBackgroundColor;
  SynXMLSynDark.AttributeValueAttri.Background := LBackgroundColor;
  SynXMLSynDark.NamespaceAttributeValueAttri.Background := LBackgroundColor;
  SynXMLSynDark.TextAttri.Background := LBackgroundColor;
  SynXMLSynDark.CDATAAttri.Background := LBackgroundColor;
  SynXMLSynDark.EntityRefAttri.Background := LBackgroundColor;
  SynXMLSynDark.ProcessingInstructionAttri.Background := LBackgroundColor;
  SynXMLSynDark.CommentAttri.Background := LBackgroundColor;
  SynXMLSynDark.DocTypeAttri.Background := LBackgroundColor;
  SynXMLSynDark.SpaceAttri.Background := LBackgroundColor;
  SynXMLSynDark.SymbolAttri.Background := LBackgroundColor;
end;

procedure TFrmEditor.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  TLogPreview.Add('TFrmEditor.FormAfterMonitorDpiChanged: '+
  '- Old: '+OldDPI.ToString+' - New: '+NewDPI.ToString);
end;

procedure TFrmEditor.FormCreate(Sender: TObject);
begin
  TLogPreview.Add('TFrmEditor.FormCreate');
  Application.OnException := AppException;
  FSimpleText := StatusBar.SimpleText;
  UpdateFromSettings;
  UpdateHighlighter;
end;

procedure TFrmEditor.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  TLogPreview.Add('TFrmEditor.FormDestroy');
  inherited;
end;

procedure TFrmEditor.FormResize(Sender: TObject);
begin
  PanelEditor.Height := Round(Self.Height * (FSettings.SplitterPos / 100));
  Splitter.Top := PanelEditor.Height;
  if Self.Width < (550 * Self.ScaleFactor) then
    ToolBar.ShowCaptions := False
  else
    Toolbar.ShowCaptions := True;
  UpdateGUI;
end;

function TFrmEditor.GetEditorFontSize: Integer;
begin
  Result := FFontSize;
end;

procedure TFrmEditor.LoadFromFile(const AFileName: string);
begin
  TLogPreview.Add('TFrmEditor.LoadFromFile Init');
  FFileName := AFileName;
  SynEdit.Lines.LoadFromFile(FFileName);
  SVGIconImage.SVGText := SynEdit.Lines.Text;
  TLogPreview.Add('TFrmEditor.LoadFromFile Done');
end;

procedure TFrmEditor.LoadFromStream(const AStream: TStream);
var
  LStringStream: TStringStream;
begin
  TLogPreview.Add('TFrmEditor.LoadFromStream Init');
  AStream.Position := 0;
  LStringStream := TStringStream.Create;
  try
    LStringStream.LoadFromStream(AStream);
    SynEdit.Lines.Text := LStringStream.DataString;
    SVGIconImage.SVGText := LStringStream.DataString;
  finally
    LStringStream.Free;
  end;
  TLogPreview.Add('TFrmEditor.LoadFromStream Done');
end;

procedure TFrmEditor.SaveSettings;
begin
  if Assigned(FSettings) then
  begin
    FSettings.UpdateSettings(SynEdit.Font.Name,
      EditorFontSize,
      PanelEditor.Visible);
    FSettings.WriteSettings;
  end;
end;

procedure TFrmEditor.ScaleControls(const ANewPPI: Integer);
var
  LCurrentPPI: Integer;
  LScaleFactor: Integer;
begin
  LCurrentPPI := FCurrentPPI;
  if ANewPPI <> LCurrentPPI then
    SVGIconImageList.Size := MulDiv(SVGIconImageList.Size, ANewPPI, LCurrentPPI);
end;

procedure TFrmEditor.SetEditorFontSize(const Value: Integer);
var
  LScaleFactor: Single;
begin
  if (Value >= MinfontSize) and (Value <= MaxfontSize) then
  begin
    TLogPreview.Add('TFrmEditor.SetEditorFontSize'+
      ' CurrentPPI: '+Self.CurrentPPI.ToString+
      ' ScaleFactor: '+ScaleFactor.ToString+
      ' Value: '+Value.ToString);
    if FFontSize <> 0 then
      LScaleFactor := SynEdit.Font.Size / FFontSize
    else
      LScaleFactor := 1;
    FFontSize := Value;
    SynEdit.Font.Size := Round(FFontSize * LScaleFactor);
    SynEdit.Gutter.Font.Size := SynEdit.Font.Size;
  end;
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

  StatusBar.SimpleText := '';
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

procedure TFrmEditor.SplitterMoved(Sender: TObject);
begin
  FSettings.SplitterPos := splitter.Top * 100 div
    (Self.Height - Toolbar.Height);
  SaveSettings;
end;

procedure TFrmEditor.ToolButtonShowTextClick(Sender: TObject);
begin
  PanelEditor.Visible := not PanelEditor.Visible;
  UpdateGUI;
  SaveSettings;
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

procedure TFrmEditor.ToolButtonMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := (Sender as TToolButton).Hint;
end;

procedure TFrmEditor.ToolButtonMouseLeave(Sender: TObject);
begin
  StatusBar.SimpleText := FSimpleText;
end;

procedure TFrmEditor.ToolButtonReformatClick(Sender: TObject);
begin
  SynEdit.Lines.Text := Xml.XMLDoc.FormatXMLData(SynEdit.Lines.Text);
end;

procedure TFrmEditor.UpdateFromSettings;
begin
  FSettings.ReadSettings;
  if FSettings.FontSize >= MinfontSize then
    EditorFontSize := FSettings.FontSize
  else
    EditorFontSize := MinfontSize;
  SynEdit.Font.Name := FSettings.FontName;
  PanelEditor.Visible := FSettings.ShowEditor;
  UpdateGUI;
end;

procedure TFrmEditor.ToolButtonFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(SynEdit.Font);
  FontDialog.Font.Size := EditorFontSize;
  if FontDialog.Execute then
  begin
    FSettings.FontName := FontDialog.Font.Name;
    FSettings.FontSize := FontDialog.Font.Size;
    FSettings.WriteSettings;
    UpdateFromSettings;
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
  EditorFontSize := EditorFontSize - 1;
  SaveSettings;
end;

procedure TFrmEditor.ToolButtonZoomInClick(Sender: TObject);
begin
  EditorFontSize := EditorFontSize + 1;
  SaveSettings;
end;

end.
