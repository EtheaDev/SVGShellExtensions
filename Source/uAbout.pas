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
{  The Original Code is uAbout.pas:                                            }
{  Delphi Preview Handler  https://github.com/RRUZ/delphi-preview-handler      }
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright 2011-2021 Rodrigo Ruz V.   }
{  All Rights Reserved.                                                        }
{******************************************************************************}

unit uAbout;

interface

uses
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Controls, System.Classes, Vcl.ImgList, System.ImageList,
  Vcl.Imaging.GIFImg, SVGIconImage, Vcl.ButtonStylesAttributes, Vcl.StyledButton,
  System.SysUtils, System.Types, Vcl.Forms, Vcl.ComCtrls;

resourcestring
  Title_SVGTextEditor = 'SVG Text Editor';
  Title_SVGPreview = 'SVG Preview';

  NewVersionAvailable = 'A new installer is available to download:'+sLineBreak+
    'Current version: %s - Available version: %s'+sLineBreak+
    'Do you want to dowload and install the new version?';
  NewVersionNotAvailable = 'No updates available.'+sLineBreak+
    'You have already the latest version: %s';
  Stop_Download_Msg = 'Stop download installer!';
  CheckForUpdates_Msg = 'Check for updates';
  Download_Stopped_Err = 'Download of installer stopped!';
  Downloading_Err = 'Error downloading!';
  Error_Checking_New_Version = 'Error checking if a new installer is available: %s';

const
  HELP_URL = 'https://github.com/EtheaDev/SVGShellExtensions';
  GIT_HUB_URL = 'https://github.com/EtheaDev/SVGShellExtensions';
  SETUP_FILENAME = 'SVGShellExtensionsSetup.exe';

type
  TFrmAbout = class(TForm)
    BottomPanel: TPanel;
    btnOK: TStyledButton;
    TitleLabel: TLabel;
    LabelVersion: TLabel;
    MemoCopyRights: TMemo;
    btnIssues: TStyledButton;
    btnCheckUpdates: TStyledButton;
    SVGLinkLabel: TLinkLabel;
    SVGIconImage1: TSVGIconImage;
    SVGIconImage2: TSVGIconImage;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnIssuesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCheckUpdatesClick(Sender: TObject);
    procedure SVGLinkLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FTitle: string;
    FStopDownload: Boolean;
    FExpectedSize: Int64;
    FProgressBar: TProgressBar;
    procedure SetTitle(const Value: string);
    procedure DownloadAndInstallSetup;
    procedure UpdateGUI(const ADownloadInProgress: Boolean);
  public
    procedure HttpClientReceiveData(const Sender: TObject;
      AContentLength, AReadCount: Int64; var Abort: Boolean);
    procedure DisableButtons;
    property Title: string read FTitle write SetTitle;
  end;

procedure ShowAboutForm(const AParentRect: TRect;
  const ATitle: string; const DownloadNewSetup: Boolean);
procedure HideAboutForm;

function AcceptNewSetup(const AShowMsg: Boolean): Boolean;
function GetCurrentVersion(const AApplicationExeName: TFileName): string;

implementation

uses
  Winapi.ShellApi
  , Winapi.Windows
  , Vcl.Dialogs
  , Vcl.Graphics
  , uMisc
  , StyledTaskDialog
  , GitHubAPI
  ;

{$R *.dfm}

var
 _HttpClient: TGitHubHttpClient;

function GetCurrentVersion(const AApplicationExeName: TFileName): string;
var
  LMajorVersion, LMinorVersion, LRelease: integer;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  LMajorVersion := 0;
  LMinorVersion := 0;
  LRelease := 0;

  InfoSize := GetFileVersionInfoSize(PChar(AApplicationExeName), Wnd);
  if InfoSize > 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(AApplicationExeName), Wnd, InfoSize, VerBuf) then
      begin
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          LMajorVersion := HIWORD(FI.dwFileVersionMS);
          LMinorVersion := LOWORD(FI.dwFileVersionMS);
          LRelease := HIWORD(FI.dwFileVersionLS);
        end;
      end;
    finally
      FreeMem(VerBuf);
    end;
  end;
  Result := Format('v%d.%d.%d', [LMajorVersion,LMinorVersion,LRelease]);
end;

function AcceptNewSetup(const AShowMsg: Boolean): Boolean;
var
  LCurrentVersion, LNewVersion: string;
begin
  while True do
  begin
    Screen.Cursor := crHourGlass;
    try try
      LCurrentVersion := GetCurrentVersion(Application.ExeName);
      if not Assigned(_HttpClient) then
        _HttpClient := TGitHubHttpClient.Create(nil);
      Result := _HttpClient.IsNewVersionAvailable(
        LCurrentVersion, GIT_HUB_URL, LNewVersion);
      Break;  
    except
      on E: ECheckNewVersionException do
      begin
        if StyledTaskMessageDlg(CheckForUpdates_Msg,
          Format(Error_Checking_New_Version, [E.Message]),
          TMsgDlgType.mtWarning,
          [mbAbort, mbRetry],0, mbAbort) = mrRetry then
            Continue
          else
            Exit(False);  
      end
      else
        raise;
    end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  if Result then
  begin
    Result := StyledMessageDlg(Format(NewVersionAvailable,
      [LCurrentVersion, LNewVersion]),
      TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel],
      0) = mrYes;
  end
  else if AShowMsg then
    StyledMessageDlg(Format(NewVersionNotAvailable,
      [LCurrentVersion]), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

function GetAboutForm: TFrmAbout;
var
  I: integer;
begin
  Result := Nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I].ClassType = TFrmAbout then
    begin
      Result := Screen.Forms[I] as TFrmAbout;
      exit;
    end;
end;

procedure HideAboutForm;
var
  LFrm: TFrmAbout;
begin
  LFrm := GetAboutForm;
  if LFrm <> nil then
    LFrm.Close;
end;

procedure ShowAboutForm(const AParentRect: TRect; const ATitle: string;
  const DownloadNewSetup: Boolean);
var
  LFrm: TFrmAbout;
begin
  LFrm := GetAboutForm;
  if LFrm <> nil then
  begin
    LFrm.BringToFront;
    exit;
  end;

  LFrm := TFrmAbout.Create(nil);
  try
    if (AparentRect.Left <> 0) and (AparentRect.Right <> 0) then
    begin
      LFrm.Left := (AParentRect.Left + AParentRect.Right - LFrm.Width) div 2;
      LFrm.Top := (AParentRect.Top + AParentRect.Bottom - LFrm.Height) div 2;
    end;
    LFrm.Title := ATitle;
    LFrm.FStopDownload := not DownloadNewSetup;
    LFrm.ShowModal;
  finally
    LFrm.Free;
  end;
end;

procedure TFrmAbout.btnCheckUpdatesClick(Sender: TObject);
begin
  if not FStopDownload then
    FStopDownload := True
  else if AcceptNewSetup(True) then
    DownloadAndInstallSetup;
end;


procedure TFrmAbout.btnOKClick(Sender: TObject);
begin
  Close();
end;

procedure TFrmAbout.btnIssuesClick(Sender: TObject);
begin
   ShellExecute(Handle, 'open',
    PChar(GIT_HUB_URL+'/issues'),
    nil, nil, SW_SHOW);
end;

procedure TFrmAbout.DisableButtons;
begin
  btnOK.OnClick := nil;
end;

procedure TFrmAbout.UpdateGUI(const ADownloadInProgress: Boolean);
begin
  btnIssues.Enabled := not ADownloadInProgress;
  btnOK.Enabled := not ADownloadInProgress;
  FProgressBar.Visible := ADownloadInProgress;
  if ADownloadInProgress then
  begin
    FStopDownload := False;
    btnCheckUpdates.Caption := Stop_Download_Msg;
  end
  else
  begin
    btnCheckUpdates.Caption := CheckForUpdates_Msg;
  end;
end;

procedure TFrmAbout.DownloadAndInstallSetup;
var
  AOutFileName: TFileName;
begin
  UpdateGUI(True);
  try
    //Start Download
    var LSize := _HttpClient.DownloadLatestSetup(
      SETUP_FILENAME,
      HttpClientReceiveData,
      AOutFileName);
    if LSize <> FExpectedSize then
    begin
      if FStopDownload then
        raise Exception.Create(Download_Stopped_Err)
      else
        raise Exception.Create(Downloading_Err);
    end;
  finally
    UpdateGUI(False);
  end;
  ShellExecute(0, 'open', PChar(AOutFileName), nil, nil, SW_SHOWNORMAL);
  Application.Terminate;
end;

procedure TFrmAbout.FormActivate(Sender: TObject);
begin
  if not FStopDownload then
    DownloadAndInstallSetup;
end;

procedure TFrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FStopDownload := True;
  Action := caFree;
end;

procedure TFrmAbout.FormCreate(Sender: TObject);
var
  FileVersionStr: string;
begin
  TitleLabel.Font.Height := Round(TitleLabel.Font.Height * 1.6);

  FileVersionStr := GetCurrentVersion(GetModuleLocation());
  {$IFDEF WIN32}
  LabelVersion.Caption := Format('%s (32bit)', [FileVersionStr]);
  {$ELSE}
  LabelVersion.Caption := Format('%s (64bit)', [FileVersionStr]);
  {$ENDIF}
  //ProgressBar for Download
  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Visible := False;
  FProgressBar.Align := alBottom;
  FProgressBar.Height := Round(6 * ScaleFactor);
  FProgressBar.Parent := BottomPanel;
  FProgressBar.MarqueeInterval := 1;
  FProgressBar.Max := 100;
  btnCheckUpdates.Visible := True;
  FStopDownload := True;

  MemoCopyRights.Lines.Add('Author: Carlo Barazzetta - Ethea S.r.l.');
  MemoCopyRights.Lines.Add('Custom icons: Ariel Montes - Ethea S.r.l.');
  MemoCopyRights.Lines.Add('https://github.com/EtheaDev/SVGShellExtensions');
  MemoCopyRights.Lines.Add('Copyright © 2021-2025 all rights reserved.');
  MemoCopyRights.Lines.Add('');
  MemoCopyRights.Lines.Add('Third Party libraries and tools used from Ethea:');
  MemoCopyRights.Lines.Add('SVGIconImageList https://github.com/EtheaDev/SVGIconImageList/');
  MemoCopyRights.Lines.Add('Copyright © 2019-2025 all rights reserved.');
  MemoCopyRights.Lines.Add('');
  MemoCopyRights.Lines.Add('The Initial Developer of the Original Code is Rodrigo Ruz V.');
  MemoCopyRights.Lines.Add('Portions created by Rodrigo Ruz V. are Copyright © 2011-2022 Rodrigo Ruz V.');
  MemoCopyRights.Lines.Add('https://github.com/RRUZ/delphi-preview-handler');
  MemoCopyRights.Lines.Add('');
  MemoCopyRights.Lines.Add('Third Party libraries and tools used');
  MemoCopyRights.Lines.Add('SynEdit forked by Pyscripter https://github.com/pyscripter/SynEdit/');
  MemoCopyRights.Lines.Add('');
  MemoCopyRights.Lines.Add('Image32 Library - http://www.angusj.com/delphi/image32/Docs/_Body.htm');
  MemoCopyRights.Lines.Add('Copyright ©2019-2025 Angus Johnson.');
  MemoCopyRights.Lines.Add('');
end;

procedure TFrmAbout.FormShow(Sender: TObject);
begin
  if btnOK.CanFocus then
    btnOK.SetFocus;
end;

procedure TFrmAbout.HttpClientReceiveData(const Sender: TObject; AContentLength,
  AReadCount: Int64; var Abort: Boolean);
begin
  if (AContentLength > 0) then
  begin
    if FExpectedSize = 0 then
      FExpectedSize := AContentLength;
    var LNewPos := Round(AReadCount * 100 / AContentLength)+10;
    if LNewPos <= 100 then
      FProgressBar.Position := LNewPos;
  end;
  Application.ProcessMessages;
  //Abort Download if Stop Download Button was pressed
  Abort := FStopDownload;
end;

procedure TFrmAbout.SVGLinkLabelLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(Handle, 'open', PChar(Link), nil, nil, SW_SHOW);
end;

procedure TFrmAbout.SetTitle(const Value: string);
begin
  FTitle := Value;
  Caption := FTitle;
  TitleLabel.Caption := Value;
end;

initialization
  _HttpClient := nil;

finalization
  FreeAndNil(_HttpClient);

end.
