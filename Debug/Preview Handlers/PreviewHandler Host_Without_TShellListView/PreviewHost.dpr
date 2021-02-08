program PreviewHost;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FrmMain},
  uHostPreview in 'uHostPreview.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
