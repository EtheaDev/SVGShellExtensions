// **************************************************************************************************
//
// Unit uLogExcept
// unit for the Delphi Preview Handler  https://github.com/RRUZ/delphi-preview-handler
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uLogExcept.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2021 Rodrigo Ruz V.
// All Rights Reserved.
//
// *************************************************************************************************

unit uLogExcept;

interface

Uses
  System.SysUtils,
  System.Classes;

type
  TLogPreview = class
  private
    FLogStream: TStream;
    class var FLogFile: string;
    class procedure InitLogFile; static;
  public
    property LogStream: TStream read FLogStream write FLogStream;
    class procedure Add(const AMessage: string); overload;
    class procedure Add(const AException: Exception); overload;
  end;

implementation

uses
  uMisc,
  IOUtils;

{$IFDEF DEBUG}
{$DEFINE ENABLELOG}
{$ENDIF}

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

class procedure TLogPreview.InitLogFile;
begin
  if FLogFile = '' then
    FLogFile := IncludeTrailingPathDelimiter(GetTempDirectory) + 'SvgShellExtensions.log';
end;

class procedure TLogPreview.Add(const AMessage: string);
begin
{$IFDEF ENABLELOG}
  try
    InitLogFile;
    //if (Copy(AMessage,1,4) = 'GDI+') then
    AppendAllText(FLogFile, FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AMessage + sLineBreak);
  except
    on e: EFOpenError do;
  end;
{$ENDIF}
end;

class procedure TLogPreview.Add(const AException: Exception);
begin
  try
    InitLogFile;
    AppendAllText(FLogFile, Format('%s %s StackTrace %s %s', [FormatDateTime('hh:nn:ss.zzz', Now), AException.Message,
      AException.StackTrace, sLineBreak]));
  except
    on e: EFOpenError do;
  end;
end;

initialization

end.
