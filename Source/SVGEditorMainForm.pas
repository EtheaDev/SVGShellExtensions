{******************************************************************************}
{                                                                              }
{       SVG Text Editor: Shell extensions for SVG files                        }
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
unit SVGEditorMainForm;

interface

uses
  Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Variants
  , System.Classes
  , System.ImageList
  , System.Actions
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.ExtCtrls
  , Vcl.WinXCtrls
  , Vcl.StdCtrls
  , Vcl.CategoryButtons
  , Vcl.Buttons
  , Vcl.ImgList
  , Vcl.Imaging.PngImage
  , Vcl.ComCtrls
  , Vcl.ActnList
  , Vcl.Menus
  , Vcl.Mask
  , Vcl.WinXCalendars
  , Vcl.CheckLst
  , Vcl.DBCtrls
  , Vcl.ColorGrd
  , Vcl.Samples.Spin
  , Vcl.Grids
  , Vcl.DBGrids
  , Vcl.ToolWin
  , Vcl.GraphUtil
  , SVGIconImageList //uses SVGIconImageList - download free at: https://github.com/EtheaDev/SVGIconImageList
  , FVCLThemeSelector
  , SVGIconImageListBase
  , SVGIconImage, Vcl.StdActns, Vcl.VirtualImageList
{$IFDEF VCLSTYLEUTILS}
  //VCLStyles support:
  //to compile you need to donwload VCLStyleUtils+DDetours from:
  //https://github.com/RRUZ/vcl-styles-utils
  //https://github.com/MahdiSafsafi/DDetours
  , Vcl.PlatformVclStylesActnCtrls
  , Vcl.Styles.ColorTabs
  , Vcl.Styles.ControlColor
  , Vcl.Styles.DbGrid
  , Vcl.Styles.DPIAware
  , Vcl.Styles.Fixes
  , Vcl.Styles.Ext
  , Vcl.Styles.FontAwesome
  , Vcl.Styles.FormStyleHooks
  , Vcl.Styles.NC
  , Vcl.Styles.OwnerDrawFix
  , Vcl.Styles.Utils.ScreenTips
  , Vcl.Styles.Utils.SysStyleHook
  , Vcl.Styles.WebBrowser
  , Vcl.Styles.Utils
  , Vcl.Styles.Utils.Menus
  , Vcl.Styles.Utils.Misc
  , Vcl.Styles.Utils.SystemMenu
  , Vcl.Styles.Utils.Graphics
  , Vcl.Styles.Utils.SysControls
  , Vcl.Styles.UxTheme
  , Vcl.Styles.Hooks
  , Vcl.Styles.Utils.Forms
  , Vcl.Styles.Utils.ComCtrls
  , Vcl.Styles.Utils.StdCtrls
{$ENDIF}
  ;

type
  TFormMain = class(TForm)
    panlTop: TPanel;
    SV: TSplitView;
    catMenuItems: TCategoryButtons;
    catSettings: TCategoryButtons;
    catMenuSettings: TCategoryButtons;
    catPanelSettings: TCategoryButtons;
    lblTitle: TLabel;
    splSplit: TSplitter;
    svSettings: TSplitView;
    splSettings: TSplitter;
    pnlSettings: TPanel;
    pcSettings: TPageControl;
    tsStyle: TTabSheet;
    grpDisplayMode: TRadioGroup;
    grpCloseStyle: TRadioGroup;
    grpPlacement: TRadioGroup;
    tsAnimation: TTabSheet;
    tsLog: TTabSheet;
    lstLog: TListBox;
    actMenu: TAction;
    actBack: TAction;
    actAnimate: TAction;
    PageControl: TPageControl;
    ToolBar: TToolBar;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    tsIconFonts: TTabSheet;
    Label3: TLabel;
    IconFontsSizeLabel: TLabel;
    IconFontsTrackBar: TTrackBar;
    FileOpenDialog: TFileOpenDialog;
    IconsToggleSwitch: TToggleSwitch;
    tswAnimation: TToggleSwitch;
    lblAnimationDelay: TLabel;
    trkAnimationDelay: TTrackBar;
    lblAnimationStep: TLabel;
    trkAnimationStep: TTrackBar;
    tsvDisplayMode: TToggleSwitch;
    ttsCloseStyle: TToggleSwitch;
    ttsCloseSplitView: TToggleSwitch;
    ActionList: TActionList;
    acStyleSelection: TAction;
    acViewStatusBar: TAction;
    acNewFile: TAction;
    acOpenFile: TAction;
    acEditCut: TEditCut;
    acSearch: TAction;
    acSearchAgain: TAction;
    acClose: TAction;
    acEditCopy: TEditCopy;
    acEditPaste: TEditPaste;
    acEditSelectAll: TEditSelectAll;
    acEditUndo: TEditUndo;
    acSave: TAction;
    acReplace: TAction;
    acQuit: TAction;
    acAbout: TAction;
    acCloseAll: TAction;
    acSaveAll: TAction;
    actnPrint: TAction;
    actnPrinterSetup: TAction;
    actnPrintPreview: TAction;
    actnPageSetup: TAction;
    actnEditOptions: TAction;
    actnEnlargeFont: TAction;
    actnReduceFont: TAction;
    actnSaveAs: TAction;
    actnColorSettings: TAction;
    actnFormatXML: TAction;
    SVGIconImageList: TVirtualImageList;
    MenuButtonToolbar: TToolBar;
    ToolButton1: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure grpDisplayModeClick(Sender: TObject);
    procedure grpPlacementClick(Sender: TObject);
    procedure grpCloseStyleClick(Sender: TObject);
    procedure SVClosed(Sender: TObject);
    procedure SVOpened(Sender: TObject);
    procedure SVOpening(Sender: TObject);
    procedure trkAnimationDelayChange(Sender: TObject);
    procedure trkAnimationStepChange(Sender: TObject);
    procedure actMenuExecute(Sender: TObject);
    procedure CatPreventCollapase(Sender: TObject;
      const Category: TButtonCategory);
    procedure SVResize(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure svSettingsClosed(Sender: TObject);
    procedure svSettingsOpened(Sender: TObject);
    procedure SVClosing(Sender: TObject);
    procedure tswAnimationClick(Sender: TObject);
    procedure actBackExecute(Sender: TObject);
    procedure actViewOptionsExecute(Sender: TObject);
    procedure actAnimateExecute(Sender: TObject);
    procedure svSettingsClosing(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tsvDisplayModeClick(Sender: TObject);
    procedure ttsCloseStyleClick(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
    procedure FontComboBoxSelect(Sender: TObject);
    procedure IconFontsTrackBarChange(Sender: TObject);
    procedure acIconFontsExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure IconsToggleSwitchClick(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure FormBeforeMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure acStyleSelectionExecute(Sender: TObject);
  private
    FActiveFont: TFont;
    FActiveStyleName: string;
    procedure UpdateButtons;
    procedure Log(const Msg: string);
    procedure AfterMenuClick;
    procedure ShowSettingPage(TabSheet: TTabSheet; AutoOpen: Boolean = False);
    procedure IconFontsTrackBarUpdate;
    procedure SetActiveStyleName(const Value: string);
    procedure AdjustCatSettings;
    procedure UpdateDefaultAndSystemFonts;
    procedure UpdateIconsToGUI;
  protected
    procedure Loaded; override;
  public
    destructor Destroy; override;
    property ActiveStyleName: string read FActiveStyleName write SetActiveStyleName;
  end;

var
  FrmMain: TFormMain;

implementation

uses
  Vcl.Themes
  , DResources
  , System.UITypes;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.FormShow(Sender: TObject);
begin
  IconFontsTrackBarUpdate;
  UpdateButtons;
end;

procedure TFormMain.FontComboBoxSelect(Sender: TObject);
begin
  Font.Name := 'Segoe UI';
  UpdateDefaultAndSystemFonts;
end;

procedure TFormMain.IconFontsTrackBarChange(Sender: TObject);
begin
  SVGIconImageList.SetSize(IconFontsTrackBar.Position,
    IconFontsTrackBar.Position);
  IconFontsTrackBarUpdate;
end;

procedure TFormMain.IconFontsTrackBarUpdate;
var
  LContainerSize: Integer;

  procedure UpdateCategoryButtonSize(ACatButton: TCategoryButtons);
  begin
    ACatButton.ButtonHeight := LContainerSize;
    ACatButton.ButtonWidth := LContainerSize;
  end;

begin
  IconFontsTrackBar.Position := SVGIconImageList.Width;
  IconFontsSizeLabel.Caption := IntToStr(IconFontsTrackBar.Position);

  LContainerSize := IconFontsTrackBar.Position + 10;
  panlTop.Height := LContainerSize + 10;
  SV.Top := panlTop.Top+panlTop.Height;

  svSettings.Top := SV.Top;
  ToolBar.ButtonHeight := LContainerSize;
  MenuButtonToolbar.ButtonHeight := LContainerSize;
  UpdateCategoryButtonSize(catMenuItems);
  UpdateCategoryButtonSize(catMenuSettings);
  UpdateCategoryButtonSize(catSettings);
  UpdateCategoryButtonSize(catPanelSettings);
end;

procedure TFormMain.IconsToggleSwitchClick(Sender: TObject);
begin
  ; //TODO
end;

procedure TFormMain.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  LockWindowUpdate(0);

  IconFontsTrackBarUpdate;

  UpdateDefaultAndSystemFonts;
end;

procedure TFormMain.FormBeforeMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  LockWindowUpdate(Handle);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Caption := Application.Title;

  //Hide Tabs
  for I := 0 to pcSettings.PageCount-1 do
    pcSettings.Pages[I].TabVisible := False;

  //Assign Windows10 Style to Settings Menu
  ActiveStyleName := 'Windows10';
  svSettings.StyleName := 'Windows10';

  //Title label
  lblTitle.Caption := Application.Title;
  lblTitle.Font.Color := clHighlightText;
  lblTitle.Font.Height := lblTitle.Font.Height - 4;
  lblTitle.Font.Style := lblTitle.Font.Style + [fsBold];
end;

procedure TFormMain.CatPreventCollapase(Sender: TObject;
  const Category: TButtonCategory);
begin
  // Prevent the catMenuButton Category group from being collapsed
  (Sender as TCategoryButtons).Categories[0].Collapsed := False;
end;

destructor TFormMain.Destroy;
begin
  inherited;
  FActiveFont.Free;
end;

procedure TFormMain.grpDisplayModeClick(Sender: TObject);
begin
  SV.DisplayMode := TSplitViewDisplayMode(grpDisplayMode.ItemIndex);
end;

procedure TFormMain.grpCloseStyleClick(Sender: TObject);
begin
  SV.CloseStyle := TSplitViewCloseStyle(grpCloseStyle.ItemIndex);
end;

procedure TFormMain.grpPlacementClick(Sender: TObject);
begin
  SV.Placement := TSplitViewPlacement(grpPlacement.ItemIndex);
end;

procedure TFormMain.SetActiveStyleName(const Value: string);
var
  I: Integer;
  LFontColor, LColoredColor: TColor;
  LMaskColor: TColor;
begin
  if Value <> '' then
  begin
    TStyleManager.SetStyle(Value);
    //WriteAppStyleToReg(COMPANY_NAME, ExtractFileName(Application.ExeName), Value);
    FActiveStyleName := Value;
    if FActiveStyleName = 'Windows10' then
    begin
      //For "Windows10" style: use "Windows 10 blue" color for the icons
      LFontColor := RGB(0, 120, 215);
      LColoredColor := LFontColor;
      LMaskColor := clBtnFace;
    end
    else
    begin
      //Request color from Style
      LFontColor := TStyleManager.ActiveStyle.GetStyleFontColor(sfButtonTextNormal);
      LMaskColor := TStyleManager.ActiveStyle.GetStyleFontColor(sfButtonTextDisabled);
      LColoredColor := clBlack;
    end;
    catMenuItems.Font.Color := LFontColor;
    catMenuItems.BackgroundGradientDirection := gdVertical;
    catMenuItems.RegularButtonColor := clNone;
    catMenuItems.SelectedButtonColor := clNone;
  end;
end;

procedure TFormMain.UpdateIconsToGUI;
begin
  MenuButtonToolbar.Images := SVGIconImageList;
  ToolBar.Images := SVGIconImageList;
  catMenuItems.Images := SVGIconImageList;
  catSettings.Images := SVGIconImageList;
  catMenuSettings.Images := SVGIconImageList;
  ActionList.Images := SVGIconImageList;
  UpdateButtons;
end;

procedure TFormMain.ShowSettingPage(TabSheet: TTabSheet;
  AutoOpen: Boolean = False);
begin
  if Assigned(TabSheet) then
  begin
    pcSettings.ActivePage := TabSheet;
    pnlSettings.Visible := True;
    catMenuSettings.Visible := False;
    actBack.Caption := Tabsheet.Caption;
    if AutoOpen then
      svSettings.Open;
  end
  else
  begin
    pnlSettings.Visible := False;
    catMenuSettings.Visible := True;
    actBack.Caption := 'Back';
  end;
end;

procedure TFormMain.SVClosed(Sender: TObject);
begin
  // When TSplitView is closed, adjust ButtonOptions and Width
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions - [boShowCaptions];
  actMenu.Hint := 'Expand';
  splSplit.Visible := False;
end;

procedure TFormMain.SVClosing(Sender: TObject);
begin
  SV.OpenedWidth := SV.Width;
end;

procedure TFormMain.SVOpened(Sender: TObject);
begin
  // When not animating, change size of catMenuItems when TSplitView is opened
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions + [boShowCaptions];
  actMenu.Hint := 'Collapse';
  splSplit.Visible := True;
  splSplit.Left := SV.Left + SV.Width;
end;

procedure TFormMain.SVOpening(Sender: TObject);
begin
  // When animating, change size of catMenuItems at the beginning of open
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions + [boShowCaptions];
end;

procedure TFormMain.AdjustCatSettings;
var
  LButtonWidth, LNumButtons, LButtonsFit: Integer;
  LCaptionOffset: Integer;
begin
  catSettings.Realign;
  LButtonWidth := catSettings.ButtonWidth;
  LNumButtons := catSettings.Categories[0].Items.Count;
  LButtonsFit := (SV.Width) div (LButtonWidth+2);
  LCaptionOffset := Round(-Font.Height+14);
  if (LButtonsFit <> 0) and (LButtonsFit < LNumButtons) then
    catSettings.Height := (((LNumButtons div LButtonsFit)+1) * catSettings.ButtonHeight) + LCaptionOffset
  else
  begin
    catSettings.Height := catSettings.ButtonHeight + LCaptionOffset;
  end;
end;

procedure TFormMain.SVResize(Sender: TObject);
begin
  AdjustCatSettings;
end;

procedure TFormMain.svSettingsClosed(Sender: TObject);
begin
  splSettings.Visible := False;
  ShowSettingPage(nil);
end;

procedure TFormMain.svSettingsClosing(Sender: TObject);
begin
  if svSettings.Width <> 0 then
    svSettings.OpenedWidth := svSettings.Width;
end;

procedure TFormMain.svSettingsOpened(Sender: TObject);
begin
  splSettings.Visible := True;
  splSettings.Left := svSettings.Left -1;
end;

procedure TFormMain.trkAnimationDelayChange(Sender: TObject);
begin
  SV.AnimationDelay := trkAnimationDelay.Position * 5;
  lblAnimationDelay.Caption := Format('Animation Delay (%d)', [SV.AnimationDelay]);
end;

procedure TFormMain.trkAnimationStepChange(Sender: TObject);
begin
  SV.AnimationStep := trkAnimationStep.Position * 5;
  lblAnimationStep.Caption := Format('Animation Step (%d)', [SV.AnimationStep]);
end;

procedure TFormMain.ttsCloseStyleClick(Sender: TObject);
begin
  if ttsCloseStyle.State = tssOff then
    SV.CloseStyle := svcCompact
  else
    SV.CloseStyle := svcCollapse;
end;

procedure TFormMain.tsvDisplayModeClick(Sender: TObject);
begin
  if tsvDisplayMode.State = tssOff then
    SV.DisplayMode := svmDocked
  else
    SV.DisplayMode := svmOverlay;
end;

procedure TFormMain.tswAnimationClick(Sender: TObject);
begin
  SV.UseAnimation := tswAnimation.State = tssOn;
  lblAnimationDelay.Enabled := SV.UseAnimation;
  trkAnimationDelay.Enabled := SV.UseAnimation;
  lblAnimationStep.Enabled := SV.UseAnimation;
  trkAnimationStep.Enabled := SV.UseAnimation;
end;

procedure TFormMain.acAboutExecute(Sender: TObject);
begin
  TaskMessageDlg('About this Application',
    Application.Title, mtInformation, [mbOK], 2000);
end;

procedure TFormMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.acStyleSelectionExecute(Sender: TObject);
var
  LStyleName: string;
begin
  LStyleName := TStyleManager.ActiveStyle.Name;

  if ShowVCLThemeSelector(LStyleName, True) then
  TStyleManager.TrySetStyle(LStyleName);
end;

procedure TFormMain.acIconFontsExecute(Sender: TObject);
begin
  ShowSettingPage(tsIconFonts);
end;

procedure TFormMain.actAnimateExecute(Sender: TObject);
begin
  ShowSettingPage(tsAnimation);
end;

procedure TFormMain.actBackExecute(Sender: TObject);
begin
  ShowSettingPage(nil);
end;

procedure TFormMain.actMenuExecute(Sender: TObject);
begin
  if SV.Opened then
    SV.Close
  else
    SV.Open;
end;

procedure TFormMain.actSettingsExecute(Sender: TObject);
begin
  if svSettings.Opened then
    svSettings.Close
  else
    svSettings.Open;
end;

procedure TFormMain.actViewOptionsExecute(Sender: TObject);
begin
  ShowSettingPage(tsStyle);
end;

procedure TFormMain.AfterMenuClick;
begin
  if SV.Opened and (ttsCloseSplitView.State = tssOn) then
    SV.Close;
  if svSettings.Opened and (ttsCloseSplitView.State = tssOn) then
    svSettings.Close;
end;

procedure TFormMain.UpdateButtons;
begin
  //Buttons with Actions must be reassigned to refresh Icon
end;

procedure TFormMain.UpdateDefaultAndSystemFonts;
var
  LHeight: Integer;
begin
  //Update Application.DefaultFont for Childforms with ParentFont = True
  Application.DefaultFont.Assign(Font);
  //Update system fonts as user preferences
  LHeight := Muldiv(Font.Height, Screen.PixelsPerInch, Monitor.PixelsPerInch);
  Screen.IconFont.Name := Font.Name;
  Screen.IconFont.Height := LHeight;
  Screen.MenuFont.Name := Font.Name;
  Screen.MenuFont.Height := LHeight;
  Screen.MessageFont.Name := Font.Name;
  Screen.MessageFont.Height := LHeight;
  Screen.HintFont.Name := Font.Name;
  Screen.HintFont.Height := LHeight;
  Screen.CaptionFont.Name := Font.Name;
  Screen.CaptionFont.Height := LHeight;

  catMenuItems.Font.Assign(Font);
end;

procedure TFormMain.Loaded;
begin
  FActiveFont := TFont.Create;

  //Acquire system font and size (eg. for windows 10 Segoe UI and 14 at 96 DPI)
  //but without using Assign!
  Font.Name := Screen.IconFont.Name;
  //If you want to use system font Height:
  Font.Height := Muldiv(Screen.IconFont.Height, 96, Screen.IconFont.PixelsPerInch);

  //Check for Font stored into Registry (user preferences)
  (*
  ReadAppStyleAndFontFromReg(COMPANY_NAME,
    ExtractFileName(Application.ExeName), FActiveStyleName, FActiveFont);
  *)
  Font.Assign(FActiveFont);

  inherited;

  //For ParentFont on Child Forms
  UpdateDefaultAndSystemFonts;

  //GUI default
  ActiveStyleName := FActiveStyleName;
  svSettings.Opened := False;
  PageControl.ActivePageIndex := 0;
end;

procedure TFormMain.Log(const Msg: string);
var
  Idx: Integer;
begin
  Idx := lstLog.Items.Add(Msg);
  lstLog.TopIndex := Idx;
  ShowSettingPage(tsLog, True);
end;

end.
