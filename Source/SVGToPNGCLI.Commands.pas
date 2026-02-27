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
unit SVGToPNGCLI.Commands;

interface

uses
  System.Generics.Collections,
  System.SysUtils;

type

  TCommandBroker = class
  private
    class var FCommandHandlers: TDictionary<string, TProc>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure ExecuteCommand(const ACommandName: string);
    class procedure RegisterCommand(const ACommandName: string; AProc: TProc);
  end;

implementation

uses
  SVGToPNGCLI.ResourceStrings;

class constructor TCommandBroker.Create;
begin
  inherited;
  FCommandHandlers := TDictionary<string, TProc>.Create;
end;

class destructor TCommandBroker.Destroy;
begin
  inherited;
  FreeAndNil(FCommandHandlers);
end;

class procedure TCommandBroker.ExecuteCommand(const ACommandName: string);
var
  LProc: TProc;
begin
  if not FCommandHandlers.TryGetValue(ACommandName, LProc) then
    raise Exception.Create(Format(StrErrCommandUnknown, [ACommandName]));
  LProc();
end;

class procedure TCommandBroker.RegisterCommand(const ACommandName: string;
  AProc: TProc);
begin
  FCommandHandlers.Add(ACommandName, AProc);
end;

end.
