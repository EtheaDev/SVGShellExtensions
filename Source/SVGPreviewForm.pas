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

unit SVGPreviewForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SynEdit,
  System.Generics.Collections,
  SynEditHighlighter,
  ComCtrls, ToolWin, ImgList, SynHighlighterXML,
  Vcl.Menus, SynEditExport,
  SynExportHTML, SynExportRTF, SynEditMiscClasses,
  uSVGSettings, System.ImageList, SynEditCodeFolding,
  SVGIconImageList, SVGIconImageListBase, SVGIconImage, Vcl.VirtualImageList;

type
  TFrmPreview = class(TForm)
    SynEdit: TSynEdit;
    PanelTop: TPanel;
    PanelEditor: TPanel;
    StatusBar: TStatusBar;
    SVGIconImageList: TVirtualImageList;
    ToolButtonZoomIn: TToolButton;
    ToolButtonZommOut: TToolButton;
    ToolBar: TToolBar;
    ToolButtonSettings: TToolButton;
    ToolButtonAbout: TToolButton;
    SeparatorEditor: TToolButton;
    ToolButtonShowText: TToolButton;
    ToolButtonReformat: TToolButton;
    ImagePanel: TPanel;
    SVGIconImage: TSVGIconImage;
    Splitter: TSplitter;
    panelPreview: TPanel;
    BackgroundGrayScaleLabel: TLabel;
    BackgroundTrackBar: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure ToolButtonZoomInClick(Sender: TObject);
    procedure ToolButtonZommOutClick(Sender: TObject);
    procedure ToolButtonSettingsClick(Sender: TObject);
    procedure ToolButtonAboutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ToolButtonSelectModeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ToolButtonShowTextClick(Sender: TObject);
    procedure ToolButtonReformatClick(Sender: TObject);
    procedure ToolButtonMouseEnter(Sender: TObject);
    procedure ToolButtonMouseLeave(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure BackgroundTrackBarChange(Sender: TObject);
  private
    FFontSize: Integer;
    FSimpleText: string;
    FFileName: string;
    FPreviewSettings: TPreviewSettings;

    class var FExtensions: TDictionary<TSynCustomHighlighterClass, TStrings>;
    class var FAParent: TWinControl;

    function DialogPosRect: TRect;
    procedure AppException(Sender: TObject; E: Exception);
    procedure UpdateGUI;
    procedure UpdateFromSettings;
    procedure SaveSettings;
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
    property EditorFontSize: Integer read FFontSize write SetEditorFontSize;
  end;


implementation

uses
  SynEditTypes
  , Vcl.Clipbrd
{$IFNDEF DISABLE_STYLES}
  , Vcl.Themes
{$ENDIF}
  , uLogExcept
  , System.Types
  , Registry
  , uMisc
  , IOUtils
  , ShellAPI
  , ComObj
  , IniFiles
  , GraphUtil
  , uAbout
  , Xml.XMLDoc
  , SVGSettings
  , DResources
  ;

{$R *.dfm}
  { TFrmEditor }

procedure TFrmPreview.AppException(Sender: TObject; E: Exception);
begin
  // log unhandled exceptions (TSynEdit, etc)
  TLogPreview.Add('AppException');
  TLogPreview.Add(E);
end;

procedure TFrmPreview.BackgroundTrackBarChange(Sender: TObject);
var
  LValue: byte;
begin
  LValue := BackgroundTrackBar.Position;
  BackgroundGrayScaleLabel.Caption := Format(
    Background_Grayscale_Caption,
    [LValue * 100 div 255]);
  ImagePanel.Color := RGB(LValue, LValue, LValue);
  FPreviewSettings.LightBackground := BackgroundTrackBar.Position;
end;

constructor TFrmPreview.Create(AOwner: TComponent);
begin
  inherited;
  FPreviewSettings := TPreviewSettings.CreateSettings(SynEdit.Highlighter);
  dmResources := TdmResources.Create(nil);
end;

destructor TFrmPreview.Destroy;
begin
  FreeAndNil(FPreviewSettings);
  FreeAndNil(dmResources);
  inherited;
end;

function TFrmPreview.DialogPosRect: TRect;
begin
  if Self.Parent <> nil then
    GetWindowRect(Self.Parent.ParentWindow, Result)
  else
    Result := TRect.Create(0,0,0,0);
end;

procedure TFrmPreview.UpdateGUI;
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
  ToolButtonSettings.Visible := True;
  ToolButtonReformat.Visible := PanelEditor.Visible;
  ToolButtonZoomIn.Visible := PanelEditor.Visible;
  ToolButtonZommOut.Visible := PanelEditor.Visible;
end;

procedure TFrmPreview.UpdateHighlighter;
var
  LBackgroundColor: TColor;
begin
{$IFNDEF DISABLE_STYLES}
  LBackgroundColor := StyleServices.GetSystemColor(clWindow);
{$ELSE}
  LBackgroundColor := clWindow;
{$ENDIF}
  SynEdit.Highlighter := dmResources.GetSynHighlighter(
    FPreviewSettings.UseDarkStyle, LBackgroundColor);
  //Assegna i colori "custom" all'Highlighter
  FPreviewSettings.ReadSettings(SynEdit.Highlighter, nil);
  SynEdit.Gutter.Font.Name := SynEdit.Font.Name;
{$IFNDEF DISABLE_STYLES}
  SynEdit.Gutter.Font.Color := StyleServices.GetSystemColor(clWindowText);
  SynEdit.Gutter.Color := StyleServices.GetSystemColor(clBtnFace);
{$ELSE}
  SynEdit.Gutter.Font.Color := clWindowText;
  SynEdit.Gutter.Color := clBtnFace;
{$ENDIF}
end;

procedure TFrmPreview.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  TLogPreview.Add('TFrmEditor.FormAfterMonitorDpiChanged: '+
  '- Old: '+OldDPI.ToString+' - New: '+NewDPI.ToString);
end;

procedure TFrmPreview.FormCreate(Sender: TObject);
begin
  TLogPreview.Add('TFrmEditor.FormCreate');
  Application.OnException := AppException;
  FSimpleText := StatusBar.SimpleText;
  UpdateFromSettings;
end;

procedure TFrmPreview.FormDestroy(Sender: TObject);
begin
  HideAboutForm;
  SaveSettings;
  TLogPreview.Add('TFrmEditor.FormDestroy');
  inherited;
end;

procedure TFrmPreview.FormResize(Sender: TObject);
begin
  PanelEditor.Height := Round(Self.Height * (FPreviewSettings.SplitterPos / 100));
  Splitter.Top := PanelEditor.Height;
  if Self.Width < (550 * Self.ScaleFactor) then
    ToolBar.ShowCaptions := False
  else
    Toolbar.ShowCaptions := True;
  UpdateGUI;
end;

procedure TFrmPreview.LoadFromFile(const AFileName: string);
begin
  TLogPreview.Add('TFrmEditor.LoadFromFile Init');
  FFileName := AFileName;
  SynEdit.Lines.LoadFromFile(FFileName);
  SVGIconImage.SVGText := SynEdit.Lines.Text;
  TLogPreview.Add('TFrmEditor.LoadFromFile Done');
end;

procedure TFrmPreview.LoadFromStream(const AStream: TStream);
var
  LStringStream: TStringStream;
begin
  TLogPreview.Add('TFrmEditor.LoadFromStream Init');
  AStream.Position := 0;
  LStringStream := TStringStream.Create('',TEncoding.UTF8);
  try
    LStringStream.LoadFromStream(AStream);
    SynEdit.Lines.Text := LStringStream.DataString;
    SVGIconImage.SVGText := LStringStream.DataString;
  finally
    LStringStream.Free;
  end;
  TLogPreview.Add('TFrmEditor.LoadFromStream Done');
end;

procedure TFrmPreview.SaveSettings;
begin
  if Assigned(FPreviewSettings) then
  begin
    FPreviewSettings.UpdateSettings(SynEdit.Font.Name,
      EditorFontSize,
      PanelEditor.Visible);
    FPreviewSettings.WriteSettings(SynEdit.Highlighter, nil);
  end;
end;

procedure TFrmPreview.ScaleControls(const ANewPPI: Integer);
var
  LCurrentPPI: Integer;
  LNewSize: Integer;
begin
  LCurrentPPI := FCurrentPPI;
  if ANewPPI <> LCurrentPPI then
  begin
    LNewSize := MulDiv(SVGIconImageList.Width, ANewPPI, LCurrentPPI);
    SVGIconImageList.SetSize(LNewSize, LNewSize);
  end;
end;

procedure TFrmPreview.SetEditorFontSize(const Value: Integer);
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

procedure TFrmPreview.SplitterMoved(Sender: TObject);
begin
  FPreviewSettings.SplitterPos := splitter.Top * 100 div
    (Self.Height - Toolbar.Height);
  SaveSettings;
end;

procedure TFrmPreview.ToolButtonShowTextClick(Sender: TObject);
begin
  PanelEditor.Visible := not PanelEditor.Visible;
  UpdateGUI;
  SaveSettings;
end;

procedure TFrmPreview.ToolButtonAboutClick(Sender: TObject);
begin
  ShowAboutForm(DialogPosRect, Title_SVGPreview);
end;

procedure TFrmPreview.ToolButtonMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := (Sender as TToolButton).Hint;
end;

procedure TFrmPreview.ToolButtonMouseLeave(Sender: TObject);
begin
  StatusBar.SimpleText := FSimpleText;
end;

procedure TFrmPreview.ToolButtonReformatClick(Sender: TObject);
begin
  SynEdit.Lines.Text := Xml.XMLDoc.FormatXMLData(SynEdit.Lines.Text);
end;

procedure TFrmPreview.UpdateFromSettings;
begin
  FPreviewSettings.ReadSettings(SynEdit.Highlighter, nil);
  if FPreviewSettings.FontSize >= MinfontSize then
    EditorFontSize := FPreviewSettings.FontSize
  else
    EditorFontSize := MinfontSize;
  SynEdit.Font.Name := FPreviewSettings.FontName;
  PanelEditor.Visible := FPreviewSettings.ShowEditor;
{$IFNDEF DISABLE_STYLES}
  TStyleManager.TrySetStyle(FPreviewSettings.StyleName, False);
{$ENDIF}
  BackgroundTrackBar.Position := FPreviewSettings.LightBackground;
  UpdateHighlighter;
  UpdateGUI;
end;

procedure TFrmPreview.ToolButtonSettingsClick(Sender: TObject);
begin
  if ShowSettings(DialogPosRect, Title_SVGPreview, SynEdit, FPreviewSettings, True) then
  begin
    FPreviewSettings.WriteSettings(SynEdit.Highlighter, nil);
    UpdateFromSettings;
  end;
end;

procedure TFrmPreview.ToolButtonSelectModeClick(Sender: TObject);
begin
  TToolButton(Sender).CheckMenuDropdown;
end;

procedure TFrmPreview.ToolButtonZommOutClick(Sender: TObject);
begin
  EditorFontSize := EditorFontSize - 1;
  SaveSettings;
end;

procedure TFrmPreview.ToolButtonZoomInClick(Sender: TObject);
begin
  EditorFontSize := EditorFontSize + 1;
  SaveSettings;
end;

end.
