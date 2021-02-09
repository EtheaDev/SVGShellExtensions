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
unit uSVGSettings;

interface

uses
  System.SysUtils;

type
  TSettings = class
  private
    FSplitterPos: Integer;
    FFontSize: Integer;
    FStyleName: string;
    FUseDarkStyle: boolean;
    FFontName: string;
    FShowEditor: Boolean;
    FPreferD2D: Boolean;
    class function GetSettingsPath: string; static;
    class function GetSettingsFileName: string; static;
    function GetUseDarkStyle: Boolean;
    procedure SetPreferD2D(const Value: Boolean);
  public
    constructor Create;
    class property SettingsFileName: string read GetSettingsFileName;
    class property SettingsPath: string read GetSettingsPath;

    procedure UpdateSettings(const AFontName: string;
      AFontSize: Integer; AEditorVisible: Boolean);
    procedure ReadSettings;
    procedure WriteSettings;

    property UseDarkStyle: Boolean read GetUseDarkStyle;
    property FontSize: Integer read FFontSize write FFontSize;
    property FontName: string read FFontName write FFontName;
    property StyleName: string read FStyleName write FStyleName;
    property ShowEditor: Boolean read FShowEditor write FShowEditor;
    property SplitterPos: Integer read FSplitterPos write FSplitterPos;
    property PreferD2D: Boolean read FPreferD2D write SetPreferD2D;
  end;

implementation

uses
  IniFiles,
  //SVGIconImageList
  SVGInterfaces,
  PasSVGFactory,
  D2DSVGFactory,
  System.Types,
  System.TypInfo,
  System.Rtti,
  System.StrUtils,
  System.IOUtils,
  Winapi.ShlObj,
  Winapi.Windows,
  Vcl.Themes,
  uLogExcept,
  uRegistry,
  uMisc
  ;

{ TSettings }

constructor TSettings.Create;
begin
  inherited;
  ReadSettings;
end;

class function TSettings.GetSettingsFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(GetSettingsPath) + 'Settings.ini';
end;

class function TSettings.GetSettingsPath: string;
begin
  Result := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_APPDATA)) + 'SVGPreviewHandler\';
  System.SysUtils.ForceDirectories(Result);
end;

function TSettings.GetUseDarkStyle: Boolean;
begin
  Result := FUseDarkStyle;
end;

procedure TSettings.ReadSettings;
var
  Settings: TIniFile;
  LIsLightTheme: Boolean;
begin
  try
    TLogPreview.Add('ReadSettings '+SettingsFileName);
    Settings := TIniFile.Create(SettingsFileName);
    try
      FFontSize := Settings.ReadInteger('Global', 'FontSize', 10);
      FFontName := Settings.ReadString('Global', 'FontName', 'Consolas');
      FShowEditor := Settings.ReadInteger('Global', 'ShowEditor', 1) = 1;
      FSplitterPos := Settings.ReadInteger('Global', 'SplitterPos', 33);
      FPreferD2D := Boolean(Settings.ReadInteger('Global', 'PreferD2D', 0));
      //Select Style based on Actual Windows Theme
      LIsLightTheme := IsWindowsAppThemeLight;
      if LIsLightTheme then
      begin
        FUseDarkStyle := False;
        FStyleName := 'Windows10';
        //FStyleName := 'Windows';
      end
      else
      begin
        FUseDarkStyle := True;
        FStyleName := 'Glow';
        //FStyleName := 'Windows10 SlateGray';
        //FStyleName := 'Windows10 Dark';
        //FStyleName := 'Windows';
      end;
    finally
      Settings.Free;
    end;
  except
    on E: Exception do
      TLogPreview.Add(Format('Error in TSettings.ReadSettings - Message: %s: Trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TSettings.SetPreferD2D(const Value: Boolean);
begin
  FPreferD2D := Value;
  if FPreferD2D then
    SetGlobalSvgFactory(GetPasSVGFactory)
  else
    SetGlobalSvgFactory(GetD2DSVGFactory);
end;

procedure TSettings.UpdateSettings(const AFontName: string;
  AFontSize: Integer; AEditorVisible: Boolean);
begin
  FontSize := AFontSize;
  FontName := AFontName;
  StyleName := TStyleManager.ActiveStyle.Name;
  ShowEditor := AEditorVisible;
end;

procedure TSettings.WriteSettings;
var
  Settings: TIniFile;
begin
  try
    TLogPreview.Add('WriteSettings '+SettingsFileName);
    Settings := TIniFile.Create(SettingsFileName);
    try
      Settings.WriteInteger('Global', 'FontSize', FFontSize);
      Settings.WriteString('Global', 'FontName', FFontName);
      Settings.WriteString('Global', 'StyleName', FStyleName);
      Settings.WriteInteger('Global', 'ShowEditor', Ord(FShowEditor));
      Settings.WriteInteger('Global', 'SplitterPos', FSplitterPos);
      Settings.WriteInteger('Global', 'PreferD2D', Ord(FPreferD2D));
    finally
      Settings.Free;
    end;
  except
    on E: Exception do
      TLogPreview.Add(Format('Error in TSettings.WriteSettings - Message: %s: Trace %s', [E.Message, E.StackTrace]));
  end;
end;

end.
