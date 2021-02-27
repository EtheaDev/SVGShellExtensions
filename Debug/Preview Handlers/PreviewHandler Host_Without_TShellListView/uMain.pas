unit uMain;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.OleCtnrs,
  Vcl.ExtCtrls, Vcl.ComCtrls;
type
  TFrmMain = class(TForm)
    Panel3: TPanel;
    EditFileName: TEdit;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    RadioGroup: TRadioGroup;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RadioGroupClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadPreview(const FileName: string);
  public
    { Public declarations }
  end;
var
  FrmMain: TFrmMain;

implementation

uses
  Winapi.ActiveX;
{$R *.dfm}


procedure TFrmMain.Button1Click(Sender: TObject);
begin
 if OpenDialog1.Execute() then
 begin
   EditFileName.Text:= OpenDialog1.FileName;
   //LoadPreview(EditFileName.Text);
 end;
end;
procedure TFrmMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TFrmMain.LoadPreview(const FileName: string);
begin
end;
procedure TFrmMain.RadioGroupClick(Sender: TObject);
begin
(*
  case RadioGroup.ItemIndex of
    0: Print(32);
    1: Print(64);
    2: Print(128);
    3: Print(256);
  end;
*)
end;

end.
