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
{******************************************************************************}
unit ChildForm;

interface

uses Winapi.Windows, System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Controls,
  Vcl.StdCtrls, Vcl.ImgList, Vcl.FormTabsBar, Winapi.Messages;

type
  TSpecialFormTabsBar = class(TCustomFormTabsBar)
  private
    FMainForm: TForm;
  protected
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function AcceptForm(AForm: TForm): Boolean; override;
    procedure DrawTab(ACanvas: TCanvas; AIndex: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor CreateForMainForm(AMainForm: TForm;
      AParent: TWinControl);
  end;

  TMDIChildForm = class(TForm)
  private
    FImageIndex: Integer;
  public
    EditingFile: TObject;
    procedure SetIconByName(const AImageList: TCustomImageList;
      AIconName: string);
  end;

implementation

{$R *.dfm}

uses
  System.SysUtils
  , System.Types
  , Vcl.Themes
  ;

{ TMDIChildForm }

procedure TMDIChildForm.SetIconByName(const AImageList: TCustomImageList;
  AIconName: string);
var
  LImageIndex: Integer;
  LIcon: TIcon;
begin
  LImageIndex := AImageList.GetIndexByName(AIconName);
  if (LImageIndex <> FImageIndex) and (LImageIndex >= 0) then
  begin
    LIcon := TIcon.Create;
    try
      AImageList.GetIcon(LImageIndex, LIcon);
      Icon := LIcon;
    finally
      LIcon.Free;
    end;
    FImageIndex := LImageIndex;
  end;
end;

{ TSpecialFormTabsBar }

function TSpecialFormTabsBar.AcceptForm(AForm: TForm): Boolean;
begin
  Result := inherited AcceptForm(AForm);
  if AForm = Owner then
    Result := False;
end;

procedure TSpecialFormTabsBar.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  Cursor := crHandPoint;
end;

procedure TSpecialFormTabsBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  Cursor := crDefault;
end;

constructor TSpecialFormTabsBar.CreateForMainForm(AMainForm: TForm;
  AParent: TWinControl);
begin
  inherited Create(AMainForm);
  FMainForm := AMainForm;
  FMainForm.VisualManager := Self;
  Parent := AParent;
  Align := alClient;
  TabOptions.ShowFormIcon := True;
  TabOptions.ShowCloseButton := True;
  TabMinWidth := 100;
  TabMaxWidth := 250;
  ShowTabsMenuButton := True;
end;

procedure TSpecialFormTabsBar.DrawTab(ACanvas: TCanvas; AIndex: Integer);
begin
  var LTab := Tabs[AIndex];
  var LDrawRect := LTab.BoundsRect;
  var LIconBorder := Round(2*ScaleFactor);
  inherited;
  var R := GetFormIconRect(LTab, LDrawRect);
  //Clear the icon painted by default
  ACanvas.Brush.Style := bsSolid;
  //if LTab.State <> ftdsSelected then
  ACanvas.Brush.Color := clBtnFace;
  //Paint an Icon Bigger than LIconBorder
  InflateRect(R, LIconBorder, LIconBorder);
  ACanvas.FillRect(R);
  var LIcon := LTab.Form.Icon;
  DrawIconEx(ACanvas.Handle, R.Left, R.Top, LIcon.Handle,
    R.Width, R.Height, 0, 0, DI_NORMAL);
end;

procedure TSpecialFormTabsBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMainForm.Hint := '';
  var LIndex := TabIndexFromPoint(TPoint.Create(X, Y));
  if LIndex >= 0 then
  begin
    var LTab := Tabs[LIndex];
    FMainForm.Hint := LTab.Form.Hint;
  end;
end;

end.
