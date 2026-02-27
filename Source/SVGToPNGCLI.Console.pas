{******************************************************************************}
{                                                                              }
{   SVG To PNG Converter Command Line                                          }
{                                                                              }
{   Copyright (c) 2026 (Ethea S.r.l.)                                          }
{   Author: Carlo Barazzetta                                                   }
{                                                                              }
{   https://github.com/EtheaDev/SVGIconImageList                               }
{                                                                              }
{   Based on: Delphi Day 2020: Una CLI che i tuoi utenti ameranno              }
{             by Marco Breveglieri                                             }
{   https://www.breveglieri.it/eventi/2020-06-11-delphiday-creare-client-cli/  }
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
unit SVGToPNGCLI.Console;

interface

uses
  Winapi.Windows;

type

  TConsoleForeColor = (
      Black = 0,
      Maroon = FOREGROUND_RED,
      Green = FOREGROUND_GREEN,
      Navy = FOREGROUND_BLUE,
      Teal = FOREGROUND_GREEN or FOREGROUND_BLUE,
      Olive = FOREGROUND_RED or FOREGROUND_GREEN,
      Purple = FOREGROUND_RED or FOREGROUND_BLUE,
      Gray = FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE,
      Silver = FOREGROUND_INTENSITY,
      Red = FOREGROUND_INTENSITY or FOREGROUND_RED,
      Lime = FOREGROUND_INTENSITY or FOREGROUND_GREEN,
      Blue = FOREGROUND_INTENSITY or FOREGROUND_BLUE,
      Aqua = FOREGROUND_INTENSITY or FOREGROUND_GREEN or FOREGROUND_BLUE,
      Yellow = FOREGROUND_INTENSITY or FOREGROUND_RED or FOREGROUND_GREEN,
      Fuchsia = FOREGROUND_INTENSITY or FOREGROUND_RED or FOREGROUND_BLUE,
      White = FOREGROUND_INTENSITY
        or FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE
    );

  TConsoleBackColor = (
    BlackBack   = 0,
    MaroonBack  = BACKGROUND_RED,
    GreenBack   = BACKGROUND_GREEN,
    NavyBack    = BACKGROUND_BLUE,
    TealBack    = BACKGROUND_GREEN or BACKGROUND_BLUE,
    OliveBack   = BACKGROUND_RED or BACKGROUND_GREEN,
    PurpleBack  = BACKGROUND_RED or BACKGROUND_BLUE,
    GrayBack    = BACKGROUND_RED or BACKGROUND_GREEN or BACKGROUND_BLUE,
    SilverBack  = BACKGROUND_INTENSITY,
    RedBack     = BACKGROUND_INTENSITY or BACKGROUND_RED,
    LimeBack    = BACKGROUND_INTENSITY or BACKGROUND_GREEN,
    BlueBack    = BACKGROUND_INTENSITY or BACKGROUND_BLUE,
    AquaBack    = BACKGROUND_INTENSITY or BACKGROUND_GREEN or BACKGROUND_BLUE,
    YellowBack  = BACKGROUND_INTENSITY or BACKGROUND_RED or BACKGROUND_GREEN,
    FuchsiaBack = BACKGROUND_INTENSITY or BACKGROUND_RED or BACKGROUND_BLUE,
    WhiteBack   = BACKGROUND_INTENSITY
      or BACKGROUND_RED or BACKGROUND_GREEN or BACKGROUND_BLUE
  );

  TConsoleLevel = (Default, Info, Success, Warning, Error);

  TConsole = class
  private
    class var FHandle: Cardinal;
    class var FTitle: string;
  public
    class constructor Create;
    class procedure SetBackgroundColor(AColor: TConsoleBackColor);
    class procedure SetForegroundColor(AColor: TConsoleForeColor);
    class procedure SetTitle(const ATitle: string);
    class procedure Log(const AMessage: string; ALevel: TConsoleLevel);
    class procedure LogInfoSuccess(const AInfo, ASuccess: string;
      const AAddTimeStamp: Boolean = False);
    class procedure EmptyLine;
    class function GetTitle: string;
  end;

implementation

uses
  System.SysUtils;

class constructor TConsole.Create;
begin
  inherited;
  FHandle := GetStdHandle(STD_OUTPUT_HANDLE);
end;

class procedure TConsole.EmptyLine;
begin
  Writeln('');
end;

class function TConsole.GetTitle: string;
begin
  Result := FTitle;
end;

class procedure TConsole.LogInfoSuccess(const AInfo, ASuccess: string;
  const AAddTimeStamp: Boolean = False);
var
  LTimeStamp, LFormat: string;
begin
  if AAddTimeStamp then
  begin
    LFormat := '%s - %s';
    LTimeStamp := FormatDateTime('hh:nn:ss.zzz', Now);
  end
  else
  begin
    LFormat := '%s%s';
    LTimeStamp := '';
  end;
  TConsole.Log(Format(LFormat, [LTimeStamp, AInfo]), TConsoleLevel.Info);
  if ASuccess <> '' then
    TConsole.Log(Format('"%s"', [ASuccess]), TConsoleLevel.Success);
  TConsole.EmptyLine;
end;

class procedure TConsole.Log(const AMessage: string; ALevel: TConsoleLevel);
begin
  case ALevel of
    Default:
      SetForegroundColor(TConsoleForeColor.Gray);
    Info:
      SetForegroundColor(TConsoleForeColor.Aqua);
    Success:
      SetForegroundColor(TConsoleForeColor.Green);
    Warning:
      SetForegroundColor(TConsoleForeColor.Yellow);
    Error:
      SetForegroundColor(TConsoleForeColor.Red);
  end;
  Writeln(AMessage);
  SetForegroundColor(TConsoleForeColor.Gray);
end;

class procedure TConsole.SetBackgroundColor(AColor: TConsoleBackColor);
begin
  SetConsoleTextAttribute(FHandle, Word(AColor));
end;

class procedure TConsole.SetForegroundColor(AColor: TConsoleForeColor);
begin
  SetConsoleTextAttribute(FHandle, Word(AColor));
end;

class procedure TConsole.SetTitle(const ATitle: string);
begin
  FTitle := ATitle;
  SetConsoleTitle(PChar(ATitle));
end;

end.
