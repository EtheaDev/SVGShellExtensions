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
unit SVGToPNGCLI.Bootstrap;

interface

procedure BootstrapCli;

implementation

uses
  System.SysUtils,
  VSoft.CommandLine.Options,
  SVGToPNGCLI.Commands,
  SVGToPNGCLI.Options,
  SVGToPNGCLI.ResourceStrings;

procedure BootstrapCli;
var
  LCommand: TCommandDefinition;
  LOption: IOptionDefinition;
begin

  //
  // PREFERENCES
  //

  TOptionsRegistry.DescriptionTab := 30;
  TOptionsRegistry.NameValueSeparator := ':';

  //
  // OPERATION: Help
  //

  // Command: "help"
  LCommand := TOptionsRegistry.RegisterCommand('help', '?',
    StrCmdHelpDescription, StrCmdHelpInfo, 'help <command>');

  // Option: "command"
  LOption := LCommand.RegisterUnNamedOption<string>(
    StrCmdHelpCommandInfo, 'command',
    procedure (const AValue: string)
    begin
      if Length(AValue) > 0 then
        TOptionsForHelp.CommandName := AValue
      else
        TOptionsForHelp.CommandName := 'help';
    end);
  LOption.Required := False;

  //
  // OPERATION: convert
  //

  // Command: "convert"
  LCommand := TOptionsRegistry.RegisterCommand(TOptionsForConvert.CONVERT_COMMAND, 'conv',
    StrCmdConvertDescription, StrCmdConvertInfo,
    TOptionsForConvert.CONVERT_COMMAND+' -in:<InputPattern> -size:<Sizes> <options>');

  LCommand.Examples.Add(TOptionsForConvert.CONVERT_COMMAND+' -in:"C:\icons\*.svg" -size:32');
  LCommand.Examples.Add(TOptionsForConvert.CONVERT_COMMAND+' -in:"C:\icons\home.svg" -size:16,32,48,64 -out:"C:\output"');
  LCommand.Examples.Add(TOptionsForConvert.CONVERT_COMMAND+' -in:"*.svg" -size:256 -grayscale');

  // convert Option: "in"
  LOption := LCommand.RegisterOption<TFileName>('in', 'i',
    Format('%s (%s)', [StrOptInputPattern, StrRequired]),
    procedure(const AValue: TFileName)
    begin
      TOptionsForInput.InputPattern := AValue;
    end);
  LOption.Required := True;

  // convert Option: "out"
  LOption := LCommand.RegisterOption<string>('out', 'o',
    Format('%s (%s)', [StrOptOutputFolder, StrOptional]),
    procedure(const AValue: string)
    begin
      TOptionsForOutput.OutputFolder := AValue;
    end);
  LOption.Required := False;

  // convert Option: "size"
  LOption := LCommand.RegisterOption<string>('size', 's',
    Format('%s (%s)', [StrOptSizes, StrRequired]),
    procedure(const AValue: string)
    begin
      TOptionsForConvert.Sizes := AValue;
    end);
  LOption.Required := True;

  // convert Option: "color"
  LOption := LCommand.RegisterOption<string>('color', 'c',
    Format('%s (%s)', [StrOptFixedColor, StrOptional]),
    procedure(const AValue: string)
    begin
      TOptionsForConvert.FixedColor := AValue;
    end);
  LOption.Required := False;

  // convert Option: "grayscale"
  LOption := LCommand.RegisterOption<Boolean>('grayscale', 'g',
    Format('%s (%s)', [StrOptGrayScale, StrOptional]),
    procedure(const AValue: Boolean)
    begin
      TOptionsForConvert.GrayScale := AValue;
    end);
  LOption.Required := False;

  // convert Option: "opacity"
  LOption := LCommand.RegisterOption<Single>('opacity', 'a',
    Format('%s (%s)', [StrOptOpacity, StrOptional]),
    procedure(const AValue: Single)
    begin
      TOptionsForConvert.Opacity := AValue;
    end);
  LOption.Required := False;

  // Handler
  TCommandBroker.RegisterCommand(TOptionsForConvert.CONVERT_COMMAND,
    procedure ()
    begin
      TOptionsForConvert.Execute;
    end);

end;

end.
