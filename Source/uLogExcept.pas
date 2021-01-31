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
unit uLogExcept;

interface

Uses
  System.SysUtils,
  System.Classes;

type
  TLogPreview = class
  private
    FLogStream: TStream;
  public
    property LogStream: TStream read FLogStream write FLogStream;
    class procedure Add(const AMessage: string); overload;
    class procedure Add(const AException: Exception); overload;
  end;

implementation

uses
  uMisc,
  IOUtils;

var
  sLogFile: string;

{.$DEFINE ENABLELOG}

procedure AppendAllText(const FileName, Contents: string);
{$IFDEF ENABLELOG}
var
  LFileStream: TFileStream;
  LBuffer: TBytes;
{$ENDIF}
begin
{$IFDEF ENABLELOG}
  if (TFile.Exists(FileName)) then
    LFileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone)
  else
    LFileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);

  try
    LFileStream.Seek(0, soFromEnd);
    LBuffer := TEncoding.ANSI.GetBytes(Contents);
    LFileStream.WriteBuffer(LBuffer, Length(LBuffer));
  finally
    LFileStream.Free;
  end;
{$ENDIF}
end;

{ TLogException }
class procedure TLogPreview.Add(const AMessage: string);
begin
  try
    AppendAllText(sLogFile, FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AMessage + sLineBreak);
  except
    on e: EFOpenError do;
  end;
end;

class procedure TLogPreview.Add(const AException: Exception);
begin
  try
    AppendAllText(sLogFile, Format('%s %s StackTrace %s %s', [FormatDateTime('hh:nn:ss.zzz', Now), AException.Message,
      AException.StackTrace, sLineBreak]));
  except
    on e: EFOpenError do;
  end;
end;

initialization

sLogFile := IncludeTrailingPathDelimiter(GetTempDirectory) + 'SVGshellExtensions.log';

end.
