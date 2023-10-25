{******************************************************************************}
{                                                                              }
{       SVG Shell Extensions: Shell extensions for SVG files                   }
{       (Preview Panel, Thumbnail Icon, SVG Editor)                            }
{                                                                              }
{       Copyright (c) 2021-2023 (Ethea S.r.l.)                                 }
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
unit FTestPrintPreview;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, ToolWin, ActnList, ImgList, Dialogs,
  SynEditPrintPreview, Menus, AppEvnts, Printers, System.Actions,
  System.ImageList, Vcl.VirtualImageList, Vcl.ButtonGroup, Vcl.CategoryButtons;

resourcestring
  PRINT_CMD_HINT = 'Print ( %s )|Print the document on %s';

type
  TTestPrintPreviewDlg = class(TForm)
    ActionList: TActionList;
    PrevCmd: TAction;
    NextCmd: TAction;
    ZoomCmd: TAction;
    PrintCmd: TAction;
    CloseCmd: TAction;
    StatusBar: TStatusBar;
    PopupMenu: TPopupMenu;
    Fitto1: TMenuItem;
    Pagewidth1: TMenuItem;
    N1: TMenuItem;
    N251: TMenuItem;
    N501: TMenuItem;
    N1001: TMenuItem;
    N2001: TMenuItem;
    N4001: TMenuItem;
    ApplicationEvents: TApplicationEvents;
    SynEditPrintPreview: TSynEditPrintPreview;
    VirtualImageList: TVirtualImageList;
    TitlePanel: TPanel;
    catMenuItems: TCategoryButtons;
    WholePageAction: TAction;
    PageWidthAction: TAction;
    Zoom25Action: TAction;
    procedure PrevCmdExecute(Sender: TObject);
    procedure NextCmdExecute(Sender: TObject);
    procedure PrintCmdExecute(Sender: TObject);
    procedure CloseCmdExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ApplicationEventsHint(Sender: TObject);
    procedure SynEditPrintPreviewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SynEditPrintPreviewPreviewPage(Sender: TObject;
      PageNumber: Integer);
    procedure FormCreate(Sender: TObject);
    procedure WholePageActionExecute(Sender: TObject);
    procedure PageWidthActionExecute(Sender: TObject);
    procedure Zoom25ActionExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TestPrintPreviewDlg: TTestPrintPreviewDlg;

implementation

{$R *.DFM}

uses DResources;

procedure TTestPrintPreviewDlg.FormShow(Sender: TObject);
begin
  if Printer.PrinterIndex >= 0 then
    PrintCmd.Hint := Format(PRINT_CMD_HINT,
      [Printer.Printers[Printer.PrinterIndex],
      Printer.Printers[Printer.PrinterIndex]]);
  SynEditPrintPreview.SynEditPrint.Font.Name := 'Consolas';
  SynEditPrintPreview.SynEditPrint.Font.Size := 8;
  SynEditPrintPreview.UpdatePreview;
  SynEditPrintPreview.FirstPage;
end;

procedure TTestPrintPreviewDlg.PageWidthActionExecute(Sender: TObject);
begin
  SynEditPrintPreview.ScaleMode := pscPageWidth;
end;

procedure TTestPrintPreviewDlg.PrevCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.PreviousPage;
end;

procedure TTestPrintPreviewDlg.NextCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.NextPage;
end;

procedure TTestPrintPreviewDlg.Zoom25ActionExecute(Sender: TObject);
begin
  SynEditPrintPreview.ScalePercent := 25;
end;

procedure TTestPrintPreviewDlg.PrintCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.Print;
end;

procedure TTestPrintPreviewDlg.CloseCmdExecute(Sender: TObject);
begin
  Close;
end;

procedure TTestPrintPreviewDlg.ApplicationEventsHint(Sender: TObject);
begin
  StatusBar.Panels[0].Text := '  ' + Application.Hint;
end;

procedure TTestPrintPreviewDlg.SynEditPrintPreviewMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  FScale: Integer;
begin
  FScale := SynEditPrintPreview.ScalePercent;
  if Button = mbLeft then begin
    if SynEditPrintPreview.ScaleMode = pscWholePage then
      SynEditPrintPreview.ScalePercent := 100
    else begin
      FScale := FScale * 2;
      if FScale > 400 then
        FScale := 400;
      SynEditPrintPreview.ScalePercent := FScale;
    end;
  end
  else begin
    FScale := FScale div 2;
    if FScale < 25 then
      FScale := 25;
    SynEditPrintPreview.ScalePercent := FScale;
  end;
end;

procedure TTestPrintPreviewDlg.SynEditPrintPreviewPreviewPage(
  Sender: TObject; PageNumber: Integer);
begin
  StatusBar.Panels[1].Text := ' Page: ' + IntToStr(SynEditPrintPreview.PageNumber);
end;

procedure TTestPrintPreviewDlg.WholePageActionExecute(Sender: TObject);
begin
  SynEditPrintPreview.ScaleMode := pscWholePage;
end;

procedure TTestPrintPreviewDlg.FormCreate(Sender: TObject);
begin
  //Aggancia l'imagelist che si sgancia
  ActionList.Images := VirtualImageList;
  PopupMenu.Images := VirtualImageList;
  TitlePanel.Font.Size := TitlePanel.Font.Size * 2;
end;

end.

