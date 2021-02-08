program ThumbnailHost;

uses
  Vcl.Forms,
  uMain in '..\PreviewHandler Host_Without_TShellListView\uMain.pas' {FrmMain},
  uHostThumbnail in 'uHostThumbnail.pas',
  uSVGThumbnailHandler in '..\..\..\Source\uSVGThumbnailHandler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
