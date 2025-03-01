{******************************************************************************}
{                                                                              }
{       SVG Shell Extensions: Shell extensions for SVG files                   }
{       (Preview Panel, Thumbnail Icon, SVG Editor)                            }
{                                                                              }
{       Copyright (c) 2021-2025 (Ethea S.r.l.)                                 }
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
unit SettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, ColorGrd, StdCtrls, CheckLst, SynEdit,
  ActnList, SynEditHighlighter, SynUnicode, System.ImageList, Vcl.ImgList,
  SVGIconImageListBase, SVGIconImageList, uSettings, Vcl.ButtonGroup,
  Vcl.ToolWin, DResources, Vcl.VirtualImageList, uAbout, Vcl.WinXCtrls,
  Vcl.ButtonStylesAttributes, Vcl.StyledButtonGroup, Vcl.StyledButton;

type
  TUserSettingsForm = class(TForm)
    pc: TPageControl;
    tsColors: TTabSheet;
    paLeft: TPanel;
    paElements: TPanel;
    BoxElements: TListBox;
    paElemTitle: TPanel;
    VertSplitter: TSplitter;
    paAttributesContainer: TPanel;
    paAttributes: TPanel;
    StatusBar: TStatusBar;
    SynEdit: TSynEdit;
    cbTextAttrib: TGroupBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbUnderline: TCheckBox;
    cbStrikeOut: TCheckBox;
    gbWhiteSpace: TGroupBox;
    cbForeground: TCheckBox;
    cbBackground: TCheckBox;
    OpenDialog: TOpenDialog;
    tsFont: TTabSheet;
    SettingsImageList: TSVGIconImageList;
    stTheme: TTabSheet;
    FontLabel: TLabel;
    CbFont: TComboBox;
    SizeLabel: TLabel;
    EditFontSize: TEdit;
    FontSizeUpDown: TUpDown;
    ElementColorGroupBox: TGroupBox;
    ForegroundColorBox: TColorBox;
    ForegroundColorLabel: TLabel;
    BackgroundColorLabel: TLabel;
    BackgroundColorBox: TColorBox;
    MenuButtonGroup: TStyledButtonGroup;
    TitlePanel: TPanel;
    ThemeLeftPanel: TPanel;
    ThemesRadioGroup: TRadioGroup;
    SelectThemeRadioGroup: TRadioGroup;
    ThemeClientPanel: TPanel;
    ResetPanel: TPanel;
    ResetButton: TStyledButton;
    stGeneral: TTabSheet;
    PanelTopTheme: TPanel;
    PanelTopFont: TPanel;
    PanelTopEditor: TPanel;
    PanelTopPreviewSettings: TPanel;
    EngineRadioGroup: TRadioGroup;
    RoundedButtonsGroupBox: TGroupBox;
    ToolbarRoundedCheckBox: TCheckBox;
    ButtonsRoundedCheckBox: TCheckBox;
    MenuRoundedCheckBox: TCheckBox;
    FixedColorGroupBox: TGroupBox;
    ApplyToRootCheckBox: TCheckBox;
    GrayScaleCheckBox: TCheckBox;
    FixedColorColorBox: TColorBox;
    ActiveLineColorGroupBox: TGroupBox;
    DarkActiveLineColorColorBox: TColorBox;
    LightActiveLineColorColorBox: TColorBox;
    procedure BoxElementsClick(Sender: TObject);
    procedure cbForegroundClick(Sender: TObject);
    procedure cbBackgroundClick(Sender: TObject);
    procedure cbFontStyleClick(Sender: TObject);
    procedure GetActiveAttribute;
    procedure SynEditClick(Sender: TObject);
    procedure SynEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ColorGridClick(Sender: TObject);
    procedure ExitFromSettings(Sender: TObject);
    procedure ColorBoxSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuButtonGroupButtonClicked(Sender: TObject; Index: Integer);
    procedure SelectThemeRadioGroupClick(Sender: TObject);
    procedure ThemesRadioGroupClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure CbFontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
  private
    FHighlighter: TSynCustomHighlighter;
    FSourceSynEdit: TSynEdit;
    FFileName: string;
    FAboutForm: TFrmAbout;
    FTitle: string;
    procedure PopulateAvailThemes;
    procedure AssignSettings(ASettings: TSettings);
    procedure UpdateSettings(ASettings: TSettings);
    function GetCurrentElement: TSynHighlighterAttributes;
    procedure RefreshColorBoxes;
    procedure RefreshDefaultCheckBox;
    procedure RefreshTextAttributes;
    procedure ColorBoxChanged;
    function GetBackGroundColor: TColor;
    function GetForeGroundColor: TColor;
    procedure AddElements;
    procedure RefreshMap;
    procedure CloneSynEdit(Source, Dest : TSynEdit );
    procedure ChangeAllDefaultColors(const OldForeground, NewForeground,
      OldBackGround, NewBackGround: TColor);
    function GetCurrentIsWhiteSpace: Boolean;
    procedure SetTitle(const Value: string);
    procedure ChangePage(AIndex: Integer);
    procedure CreateAboutForm;
    function SelectedStyleName: string;
    function SelectedStyleIsDark: Boolean;
    property CurrentElement: TSynHighlighterAttributes read GetCurrentElement;
    property CurrentIsWhiteSpace: Boolean read GetCurrentIsWhiteSpace;
    property ForeGroundColor: TColor read GetForeGroundColor;
    property BackGroundColor: TColor read GetBackGroundColor;
    property Title: string read FTitle write SetTitle;
  public
  end;

function ShowSettings(const AParentRect: TRect;
  const ATitle: string;
  const ASourceSynEdit: TSynEdit;
  const ASettings: TSettings;
  AFromPreview: Boolean): Boolean;

implementation

uses
  System.UITypes,
{$IFNDEF DISABLE_STYLES}
  Vcl.Themes,
{$ENDIF}
  D2DSVGFactory,
  uRegistry;

{$R *.dfm}

function ShowSettings(const AParentRect: TRect;
  const ATitle: string;
  const ASourceSynEdit: TSynEdit;
  const ASettings: TSettings; AFromPreview: Boolean): Boolean;
type
  TSynCustomHighlighterClass = class of TSynCustomHighlighter;
var
  HighLightSettingsClass: TSynCustomHighlighterClass;
  LSettingsForm: TUserSettingsForm;
  I: integer;
begin
  Result := False;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I].ClassType = TUserSettingsForm then
    begin
      Screen.Forms[I].BringToFront;
      exit;
    end;

  LSettingsForm := TUserSettingsForm.Create(nil);
  with LSettingsForm do
  Try
    Title := ATitle;
    AssignSettings(ASettings);
    if (AparentRect.Left <> 0) and (AparentRect.Right <> 0) then
    begin
      LSettingsForm.Left := (AParentRect.Left + AParentRect.Right - LSettingsForm.Width) div 2;
      LSettingsForm.Top := (AParentRect.Top + AParentRect.Bottom - LSettingsForm.Height) div 2;
    end;
    StatusBar.SimpleText := FFileName;

    FSourceSynEdit := ASourceSynEdit;
    if Assigned(FSourceSynEdit) then
    begin
      SynEdit.Color := FSourceSynEdit.Color;
      SynEdit.Font.Assign(FSourceSynEdit.Font);
      SynEdit.Gutter.Assign(FSourceSynEdit.Gutter);
      SynEdit.ActiveLineColor := FSourceSynEdit.ActiveLineColor;
      HighLightSettingsClass := TSynCustomHighlighterClass(
        FSourceSynEdit.Highlighter.ClassType);
      FHighlighter := HighLightSettingsClass.Create(nil);
      SynEdit.Highlighter := FHighlighter;
      CloneSynEdit(ASourceSynEdit,SynEdit);
      SynEdit.Text := ASourceSynEdit.Text;
      AddElements;
    end;
    try
      Result := ShowModal = mrOk;
      if Result then
        UpdateSettings(ASettings);
    Finally
      if Assigned(FHighlighter) then
        FHighlighter.Free;
    End;

  Finally
    LSettingsForm.Free;
  End;
end;

{ TUserSettingsForm }

procedure TUserSettingsForm.AddElements;
var
  i : integer;
begin
  //Add Elements as Highlighters attributes
  For i := 0 to FHighlighter.AttrCount -1 do
  begin
    BoxElements.AddItem(FHighlighter.Attribute[I].Name, FHighlighter.Attribute[I]);
    //Chage WiteSpace Element position to 0
    if FHighlighter.Attribute[I] = FHighlighter.WhitespaceAttribute then
      BoxElements.Items.Move(BoxElements.Items.Count-1, 0);
  end;
  BoxElements.ItemIndex := 0;
  RefreshMap;
end;

procedure TUserSettingsForm.BoxElementsClick(Sender: TObject);
begin
  RefreshMap;
end;

procedure TUserSettingsForm.RefreshColorBoxes;
begin
  if (CurrentElement.ForeGround <> ForeGroundColor) or
    CurrentIsWhiteSpace then
  begin
    ForegroundColorBox.Enabled := True;
    ForegroundColorBox.Selected := CurrentElement.ForeGround;
  end
  else
  begin
    ForegroundColorBox.Enabled := False;
    ForegroundColorBox.Selected := ForeGroundColor;
  end;
  if (CurrentElement.Background <> BackGroundColor) or
    CurrentIsWhiteSpace then
  begin
    BackgroundColorBox.Enabled := True;
    BackgroundColorBox.Selected := CurrentElement.BackGround;
  end
  else
  begin
    BackgroundColorBox.Enabled := False;
    BackgroundColorBox.Selected := BackGroundColor;
  end;
end;

procedure TUserSettingsForm.RefreshDefaultCheckBox;
begin
  cbForeground.OnClick := nil;
  cbBackground.OnClick := nil;
  Try
    cbForeground.Checked := CurrentElement.ForeGround = ForegroundColor;
    cbBackground.Checked := CurrentElement.Background = BackgroundColor;
  Finally
    cbForeground.OnClick := cbForegroundClick;
    cbBackground.OnClick := cbBackgroundClick;
  End;
end;

procedure TUserSettingsForm.RefreshTextAttributes;
begin
  with CurrentElement do
  begin
    //Text Attributes
    cbBold.Checked := fsBold in Style;
    cbItalic.Checked := fsItalic in Style;
    cbUnderline.Checked := fsUnderline in Style;
    cbStrikeOut.Checked := fsStrikeOut in Style;
  end;
end;

procedure TUserSettingsForm.ResetButtonClick(Sender: TObject);
var
  LBackGroundColor: TColor;
begin
{$IFNDEF DISABLE_STYLES}
  LBackGroundColor := TStyleManager.Style[SelectedStyleName].GetSystemColor(clWindow);
{$ELSE}
  LBackGroundColor := clWindow;
{$ENDIF}
  SynEdit.Highlighter.Assign(dmResources.GetSynHighlighter(
    SelectedStyleIsDark, LBackGroundColor));
  DarkActiveLineColorColorBox.Selected := default_darkactivelinecolor;
  LightActiveLineColorColorBox.Selected := default_lightactivelinecolor;
end;

procedure TUserSettingsForm.RefreshMap;
begin
  //imposta la mappa sulla base delle impostazioni della lista
  with CurrentElement do
  begin
    RefreshColorBoxes;
    RefreshDefaultCheckBox;
    RefreshTextAttributes;
    gbWhiteSpace.Visible := CurrentElement <> FHighlighter.WhitespaceAttribute;
  end;
end;

function TUserSettingsForm.GetCurrentElement: TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes(BoxElements.Items.Objects[BoxElements.ItemIndex]);
end;

function TUserSettingsForm.GetCurrentIsWhiteSpace: Boolean;
begin
  Result := CurrentElement.Name = 'Whitespace';
end;

procedure TUserSettingsForm.cbForegroundClick(Sender: TObject);
begin
  if cbForeground.Checked then
    CurrentElement.Foreground := ForeGroundColor
  else
    CurrentElement.Foreground := clDefault;
  RefreshColorBoxes;
  RefreshDefaultCheckBox;
end;

procedure TUserSettingsForm.ChangeAllDefaultColors(const OldForeground,
  NewForeground, OldBackGround, NewBackGround: TColor);
var
  I: Integer;
  LAttribute: TSynHighlighterAttributes;
begin
  for i := 0 to FHighlighter.AttrCount -1 do
  begin
    LAttribute := FHighlighter.Attribute[I];
    if LAttribute.Name <> 'Whitespace' then
    begin
      if LAttribute.Foreground = OldForeground then
        LAttribute.Foreground := NewForeground;
      if LAttribute.Background = OldBackGround then
        LAttribute.Background := NewBackGround;
    end;
  end;
end;

procedure TUserSettingsForm.cbBackgroundClick(Sender: TObject);
begin
  if cbBackground.Checked then
    CurrentElement.Background := BackGroundColor
  else
    CurrentElement.Background := clDefault;
  RefreshColorBoxes;
  RefreshDefaultCheckBox;
end;

procedure TUserSettingsForm.CbFontDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with CbFont do
  begin
    Canvas.fillrect(rect);
    Canvas.Font.Name  := Items[Index];
    Canvas.textout(rect.Left, rect.Top, Items[Index]);
  end;
end;

procedure TUserSettingsForm.ColorBoxChanged;
begin
  cbForeground.OnClick := nil;
  cbBackground.OnClick := nil;
  try
    if CurrentIsWhiteSpace then
    begin
      ChangeAllDefaultColors(CurrentElement.Foreground, ForegroundColorBox.Selected,
        CurrentElement.Background, BackGroundColorBox.Selected);
      CurrentElement.Foreground := ForegroundColorBox.Selected;
      CurrentElement.Background := BackgroundColorBox.Selected;
    end
    else
    begin
      if CurrentElement.ForeGround <> ForeGroundColor then
        CurrentElement.Foreground := ForegroundColorBox.Selected
      else
        CurrentElement.Foreground := ForeGroundColor;

      if CurrentElement.Background <> BackGroundColor then
        CurrentElement.Background := BackgroundColorBox.Selected
      else
        CurrentElement.Background := BackGroundColor;
    end;
    RefreshDefaultCheckBox;
  finally
    cbForeground.OnClick := cbForegroundClick;
    cbBackground.OnClick := cbBackgroundClick;
  end;
end;

procedure TUserSettingsForm.cbFontStyleClick(Sender: TObject);
var
  FontStyle : TFontStyle;
begin
  if Sender = cbBold then
    FontStyle := fsBold
  else if Sender = cbItalic then
    FontStyle := fsItalic
  else if Sender = cbUnderline then
    FontStyle := fsUnderline
  else if Sender = cbStrikeOut then
    FontStyle := fsStrikeOut
  else
    Exit;

  with (Sender as Tcheckbox) do
  begin
    if Checked then
      CurrentElement.Style := CurrentElement.Style + [fontStyle]
    else
      CurrentElement.Style := CurrentElement.Style - [fontStyle];
  end;
  RefreshMap;
end;

procedure TUserSettingsForm.GetActiveAttribute;
var
  Token : UnicodeString;
  Attr : TSynHighlighterAttributes;
begin
  //Recupera l'attributo attivo a partire dall'editor
  Token := '';
  SynEdit.GetHighlighterAttriAtRowCol(SynEdit.CaretXY,Token,Attr);
  if Attr <> nil then
  begin
    BoxElements.ItemIndex := BoxElements.Items.IndexOf(Attr.Name);
    RefreshMap;
  end
  else
    BoxElements.ItemIndex := 0; //goto WiteSpace Element
end;

procedure TUserSettingsForm.SelectThemeRadioGroupClick(Sender: TObject);
begin
  ThemeClientPanel.StyleName := SelectedStyleName;
  CreateAboutForm;
end;

procedure TUserSettingsForm.SetTitle(const Value: string);
begin
  FTitle := Value;
  TitlePanel.Caption := '  '+FTitle+' - '+TitlePanel.Caption;
  Caption := TitlePanel.Caption;
end;

procedure TUserSettingsForm.SynEditClick(Sender: TObject);
begin
  GetActiveAttribute;
end;

procedure TUserSettingsForm.SynEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GetActiveAttribute;
end;

procedure TUserSettingsForm.ThemesRadioGroupClick(Sender: TObject);
begin
  PopulateAvailThemes;
end;

procedure TUserSettingsForm.ColorGridClick(Sender: TObject);
begin
  ColorBoxChanged;
end;

procedure TUserSettingsForm.FormCreate(Sender: TObject);
begin
  tsColors.TabVisible := False;
  tsFont.TabVisible := False;
  stTheme.TabVisible := False;
  stGeneral.TabVisible := False;
  CbFont.Items.Assign(Screen.Fonts);
  TitlePanel.Font.Height := Round(TitlePanel.Font.Height * 1.5);
  MenuButtonGroup.Font.Height := Round(MenuButtonGroup.Font.Height * 1.2);
  PanelTopPreviewSettings.Font.Style := PanelTopPreviewSettings.Font.Style + [fsBold];
  PanelTopTheme.Font.Style := PanelTopTheme.Font.Style + [fsBold];
  PanelTopFont.Font.Style := PanelTopFont.Font.Style + [fsBold];
  PanelTopEditor.Font.Style := PanelTopEditor.Font.Style + [fsBold];
end;

procedure TUserSettingsForm.FormDestroy(Sender: TObject);
begin
  FAboutForm.Free;
end;

procedure TUserSettingsForm.ColorBoxSelect(Sender: TObject);
begin
  ColorBoxChanged;
end;

function TUserSettingsForm.GetBackGroundColor: TColor;
begin
  Result := FHighlighter.WhitespaceAttribute.Background;
end;

function TUserSettingsForm.GetForeGroundColor: TColor;
begin
  Result := FHighlighter.WhitespaceAttribute.Foreground;
end;

procedure TUserSettingsForm.ChangePage(AIndex: Integer);
begin
  pc.ActivePageIndex := AIndex;
end;

procedure TUserSettingsForm.AssignSettings(ASettings: TSettings);
begin
  ChangePage(ASettings.ActivePageIndex);
  MenuButtonGroup.ItemIndex := pc.ActivePageIndex +1;
  SettingsImageList.FixedColor := ASettings.ButtonTextColor;
  FFileName := ASettings.SettingsFileName;
  ThemesRadioGroup.ItemIndex := Ord(ASettings.ThemeSelection);
  CbFont.ItemIndex := CbFont.Items.IndexOf(ASettings.FontName);
  FontSizeUpDown.Position := ASettings.FontSize;
  EngineRadioGroup.Enabled := WinSvgSupported;
  EngineRadioGroup.ItemIndex := Ord(ASettings.SVGEngine);
  ToolbarRoundedCheckBox.Checked := ASettings.ToolbarDrawRounded;
  ButtonsRoundedCheckBox.Checked := ASettings.ButtonDrawRounded;
  MenuRoundedCheckBox.Checked := ASettings.MenuDrawRounded;
  FixedColorColorBox.Selected := ASettings.FixedColor;
  ApplyToRootCheckBox.Checked := ASettings.ApplyToRootOnly;
  GrayScaleCheckBox.Checked := ASettings.GrayScale;
  DarkActiveLineColorColorBox.Selected := TEditorSettings(ASettings).DarkActiveLineColor;
  LightActiveLineColorColorBox.Selected := TEditorSettings(ASettings).LightActiveLineColor;

  PopulateAvailThemes;
end;

function TUserSettingsForm.SelectedStyleIsDark: Boolean;
var
  LThemeAttributes: TThemeAttribute;
begin
  TThemeAttribute.GetStyleAttributes(SelectedStyleName, LThemeAttributes);
  if not Assigned(LThemeAttributes) then
    Result := not IsWindowsAppThemeLight
  else
    Result := LThemeAttributes.ThemeType = ttDark;
end;

function TUserSettingsForm.SelectedStyleName: string;
begin
  if SelectThemeRadioGroup.ItemIndex <> -1 then
    Result := SelectThemeRadioGroup.Items[SelectThemeRadioGroup.ItemIndex]
  else
    Result := DefaultStyleName;
end;

procedure TUserSettingsForm.UpdateSettings(ASettings: TSettings);
begin
  ASettings.ActivePageIndex := pc.ActivePageIndex;
  ASettings.ThemeSelection := TThemeSelection(ThemesRadioGroup.ItemIndex);
  ASettings.FontName := CbFont.Text;
  ASettings.FontSize := FontSizeUpDown.Position;
  ASettings.StyleName := SelectedStyleName;
  ASettings.SVGEngine := TSVGEngine(EngineRadioGroup.ItemIndex);
  ASettings.ToolbarDrawRounded := ToolbarRoundedCheckBox.Checked;
  ASettings.ButtonDrawRounded := ButtonsRoundedCheckBox.Checked;
  ASettings.MenuDrawRounded := MenuRoundedCheckBox.Checked;
  ASettings.FixedColor := FixedColorColorBox.Selected;
  ASettings.ApplyToRootOnly := ApplyToRootCheckBox.Checked;
  ASettings.GrayScale := GrayScaleCheckBox.Checked;

  if ASettings is TEditorSettings then
  begin
    TEditorSettings(ASettings).DarkActiveLineColor := DarkActiveLineColorColorBox.Selected;
    TEditorSettings(ASettings).LightActiveLineColor := LightActiveLineColorColorBox.Selected;
  end;
end;

procedure TUserSettingsForm.MenuButtonGroupButtonClicked(Sender: TObject;
  Index: Integer);
begin
  if Sender is TButtonGroup then
  begin
    case Index of
      0: ExitFromSettings(nil);
      1,2,3,4: ChangePage(Index -1);
    else
      Beep;
    end;
  end;
end;

procedure TUserSettingsForm.CreateAboutForm;
begin
  FAboutForm.Free;
  FAboutForm := TFrmAbout.Create(Self);
  FAboutForm.BorderIcons := [];
  FAboutForm.Title := FTitle;
  FAboutForm.Parent := ThemeClientPanel;
  FAboutForm.Align := alClient;
  FAboutForm.DisableButtons;
  FAboutForm.btnOK.OnClick := ExitFromSettings;
  FAboutForm.Visible := True;
end;

procedure TUserSettingsForm.PopulateAvailThemes;
var
  I: Integer;
  IsLight: Boolean;
  LStyleName: string;
  LThemeAttributes: TThemeAttribute;
begin
  if TThemeSelection(ThemesRadioGroup.ItemIndex) = tsAsWindows then
    IsLight := IsWindowsAppThemeLight
  else
    IsLight := TThemeSelection(ThemesRadioGroup.ItemIndex) = tsLightTheme;

  SelectThemeRadioGroup.Items.Clear;
{$IFNDEF DISABLE_STYLES}
  for I := 0 to High(TStyleManager.StyleNames) do
  begin
    LStyleName := TStyleManager.StyleNames[I];
    TThemeAttribute.GetStyleAttributes(LStyleName, LThemeAttributes);
    if not Assigned(LThemeAttributes) then
      Continue;
    if IsLight and (LThemeAttributes.ThemeType = ttLight) or
      (not IsLight and (LThemeAttributes.ThemeType = ttDark)) then
      SelectThemeRadioGroup.Items.Add(LStyleName);
  end;
{$ELSE}
    LStyleName := 'Windows';
    TThemeAttribute.GetStyleAttributes(LStyleName, LThemeAttributes);
{$ENDIF}
  SelectThemeRadioGroup.OnClick := nil;
  try
    TStringList(SelectThemeRadioGroup.Items).Sort;
{$IFNDEF DISABLE_STYLES}
    SelectThemeRadioGroup.ItemIndex :=
      SelectThemeRadioGroup.Items.IndexOf(TStyleManager.ActiveStyle.Name);
{$ELSE}
    SelectThemeRadioGroup.ItemIndex := 0;
{$ENDIF}
    if SelectThemeRadioGroup.ItemIndex = -1 then
      SelectThemeRadioGroup.ItemIndex := 0;
  finally
    SelectThemeRadioGroup.OnClick := SelectThemeRadioGroupClick;
    SelectThemeRadioGroupClick(SelectThemeRadioGroup);
  end;
  DarkActiveLineColorColorBox.Visible := not IsLight;
  LightActiveLineColorColorBox.Visible := IsLight;
end;

procedure TUserSettingsForm.ExitFromSettings(Sender: TObject);
begin
  //Salva i parametri su file
  if Assigned(FSourceSynEdit) and Assigned(SynEdit) then
    CloneSynEdit(SynEdit, FSourceSynEdit);
  ModalResult := mrOk;
end;

procedure TUserSettingsForm.CloneSynEdit(Source, Dest: TSynEdit);
begin
  Dest.Highlighter.Assign(Source.Highlighter);
  Dest.Font.Assign(Source.Font);
  Dest.Color := Source.Color;
end;

end.
