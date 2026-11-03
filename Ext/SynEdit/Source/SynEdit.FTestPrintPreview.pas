{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FTestPrintPreview.pas, written by Ethea S.r.l.
Renamed to SynEdit.FTestPrintPreview.pas by Ethea S.r.l. (Ethea SynEdit, fork of pyscripter/SynEdit).
Copyright (c) 2005-2026 Ethea S.r.l. All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
-------------------------------------------------------------------------------}
unit SynEdit.FTestPrintPreview;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ActnList, Vcl.ImgList, Vcl.Dialogs,
  SynEdit.PrintPreview, Vcl.Menus, Vcl.AppEvnts, Vcl.Printers, System.Actions;

resourcestring
  PRINT_CMD_HINT = 'Print ( %s )|Print the document on %s';

type
  TTestPrintPreviewDlg = class(TForm)
    ActionList: TActionList;
    FirstCmd: TAction;
    PrevCmd: TAction;
    NextCmd: TAction;
    LastCmd: TAction;
    ZoomCmd: TAction;
    PrintCmd: TAction;
    CloseCmd: TAction;
    ToolBar: TToolBar;
    FirstBtn: TToolButton;
    PrevBtn: TToolButton;
    NextBtn: TToolButton;
    LastBtn: TToolButton;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    PrintBtn: TToolButton;
    ToolButton4: TToolButton;
    CloseBtn: TToolButton;
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

    procedure FirstCmdExecute(Sender: TObject);
    procedure PrevCmdExecute(Sender: TObject);
    procedure NextCmdExecute(Sender: TObject);
    procedure LastCmdExecute(Sender: TObject);
    procedure ZoomCmdExecute(Sender: TObject);
    procedure PrintCmdExecute(Sender: TObject);
    procedure CloseCmdExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Fitto1Click(Sender: TObject);
    procedure ApplicationEventsHint(Sender: TObject);
    procedure SynEditPrintPreviewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SynEditPrintPreviewPreviewPage(Sender: TObject;
      PageNumber: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TestPrintPreviewDlg: TTestPrintPreviewDlg;

implementation

uses DFileSupported;

{$R *.DFM}

procedure TTestPrintPreviewDlg.FormShow(Sender: TObject);
begin
  if Printer.PrinterIndex >= 0 then
    PrintCmd.Hint := Format(PRINT_CMD_HINT,
      [Printer.Printers[Printer.PrinterIndex],
      Printer.Printers[Printer.PrinterIndex]]);
  SetBounds(
    Self.Monitor.Left,
    Self.Monitor.Top,
    Self.Monitor.Width,
    Self.Monitor.Height);
  SynEditPrintPreview.SynEditPrint.Font.Name := 'Consolas';
  SynEditPrintPreview.SynEditPrint.Font.Size := 8;
  SynEditPrintPreview.UpdatePreview;
  SynEditPrintPreview.FirstPage;
end;

procedure TTestPrintPreviewDlg.FirstCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.FirstPage;
end;

procedure TTestPrintPreviewDlg.PrevCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.PreviousPage;
end;

procedure TTestPrintPreviewDlg.NextCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.NextPage;
end;

procedure TTestPrintPreviewDlg.LastCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.LastPage;
end;

procedure TTestPrintPreviewDlg.ZoomCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.ScaleMode := pscWholePage;
end;

procedure TTestPrintPreviewDlg.PrintCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.Print;
end;

procedure TTestPrintPreviewDlg.CloseCmdExecute(Sender: TObject);
begin
  Close;
end;

procedure TTestPrintPreviewDlg.Fitto1Click(Sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    -1: SynEditPrintPreview.ScaleMode := pscWholePage;
    -2: SynEditPrintPreview.ScaleMode := pscPageWidth;
  else
    SynEditPrintPreview.ScalePercent := (Sender as TMenuItem).Tag;
  end;
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

procedure TTestPrintPreviewDlg.FormCreate(Sender: TObject);
begin
  //Aggancia l'imagelist che si sgancia
  ActionList.Images := dmFileSupported.imgActions;
  ToolBar.Images := dmFileSupported.imgActions;
  PopupMenu.Images := dmFileSupported.imgActions;
end;

end.

