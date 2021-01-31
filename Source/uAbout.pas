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

unit uAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, pngimage, Vcl.ImgList, System.ImageList,
  Vcl.Imaging.GIFImg, SVGIconImage;

type
  TFrmAbout = class(TForm)
    Panel1:    TPanel;
    Button1:   TButton;
    LogoImage: TImage;
    Label1:    TLabel;
    LabelVersion: TLabel;
    MemoCopyRights: TMemo;
    Button3: TButton;
    btnCheckUpdates: TButton;
    LinkLabel1: TLinkLabel;
    SVGIconImage1: TSVGIconImage;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LinkLabel1Click(Sender: TObject);
    procedure btnCheckUpdatesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;



implementation

uses
  ShellApi, uMisc;

{$R *.dfm}

procedure TFrmAbout.btnCheckUpdatesClick(Sender: TObject);
var
  LBinaryPath, LUpdaterPath: string;
begin
  LBinaryPath := GetModuleLocation();
  LUpdaterPath := ExtractFilePath(LBinaryPath)+'Updater.exe';
  ShellExecute(0, 'open', PChar(LUpdaterPath), PChar(Format('"%s"', [LBinaryPath])), '', SW_SHOWNORMAL);
end;


procedure TFrmAbout.Button1Click(Sender: TObject);
begin
  Close();
end;

procedure TFrmAbout.Button3Click(Sender: TObject);
begin
   ShellExecute(Handle, 'open',
    PChar('https://github.com/EtheaDev/SVGShellExtensions/issues'),
    nil, nil, SW_SHOW);
end;

procedure TFrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action:=caFree;
end;

procedure TFrmAbout.FormCreate(Sender: TObject);
var
  FileVersionStr: string;
begin
  Label1.Font.Size := Label1.Font.Size * 2;

  FileVersionStr:=uMisc.GetFileVersion(GetModuleLocation());
  LabelVersion.Caption := Format('Version %s', [FileVersionStr]);
  MemoCopyRights.Lines.Add(
    'Author: Carlo Barazzetta - Ethea S.r.l.');
  MemoCopyRights.Lines.Add('https://github.com/EtheaDev/SVGShellExtensions');
  MemoCopyRights.Lines.Add('Copyright © 2021 all rights reserved.');
  MemoCopyRights.Lines.Add('');
  MemoCopyRights.Lines.Add('Third Party libraries and tools used from Ethea:');
  MemoCopyRights.Lines.Add('SVGIconImageList https://github.com/EtheaDev/SVGIconImageList/');
  MemoCopyRights.Lines.Add('');
  MemoCopyRights.Lines.Add('The Initial Developer of the Original Code is Rodrigo Ruz V.');
  MemoCopyRights.Lines.Add('Portions created by Rodrigo Ruz V. are Copyright © 2011-2021 Rodrigo Ruz V.');
  MemoCopyRights.Lines.Add('https://github.com/RRUZ/delphi-preview-handler');
  MemoCopyRights.Lines.Add('');
  MemoCopyRights.Lines.Add('Third Party libraries and tools used');
  MemoCopyRights.Lines.Add('SynEdit http://synedit.svn.sourceforge.net/viewvc/synedit/ all rights reserved.');
  MemoCopyRights.Lines.Add('');
  MemoCopyRights.Lines.Add('TSVG Library - http://www.mwcs.de');
  MemoCopyRights.Lines.Add('Original version © 2005, 2008 Martin Walter.');
  MemoCopyRights.Lines.Add('');
end;

procedure TFrmAbout.LinkLabel1Click(Sender: TObject);
begin
   ShellExecute(Handle, 'open',
    PChar('https://github.com/EtheaDev/SVGShellExtensions'), nil, nil, SW_SHOW);
end;

end.
