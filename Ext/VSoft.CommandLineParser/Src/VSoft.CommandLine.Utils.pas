unit VSoft.CommandLine.Utils;

interface


function GetConsoleWidth : integer;

type
  TStringUtils = class
    class function Split(const theString : string; const separator : string): TArray<string>;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
  System.SysUtils,
  System.StrUtils;

function IndexOf(const theString : string; const value : string; const startIndex : integer) : integer;
begin
  Result := PosEx(Value, theString, StartIndex + 1) - 1;
end;

class function TStringUtils.Split(const theString : string; const separator : string): TArray<string>;
const
  DeltaGrow = 32;
var
  NextSeparator, LastIndex: Integer;
  Total: Integer;
  CurrentLength: Integer;
  S: string;
begin
 Total := 0;
  LastIndex := 0;
  CurrentLength := 0;
  NextSeparator := IndexOf(theString, Separator, LastIndex);
  while (NextSeparator > 0)  do
  begin
    S := Copy(theString, LastIndex + 1, NextSeparator - LastIndex);
    if (S <> '') then
    begin
      Inc(Total);
      if CurrentLength < Total then
      begin
        CurrentLength := Total + DeltaGrow;
        SetLength(Result, CurrentLength);
      end;
      Result[Total - 1] := S;
    end;
    LastIndex := NextSeparator + Length(Separator);
    NextSeparator := IndexOf(theString, Separator, LastIndex);
  end;

  if (LastIndex < Length(theString)) then
  begin
    Inc(Total);
    SetLength(Result, Total);
    Result[Total - 1] := Copy(theString, LastIndex + 1, Length(theString) - LastIndex);
  end
  else
    SetLength(Result, Total);
end;

{$IFDEF MSWINDOWS}
function GetConsoleWidth : integer;
var
  info : CONSOLE_SCREEN_BUFFER_INFO;
  hStdOut : THandle;
begin
  Result := High(Integer); // Default is unlimited width
  hStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if GetConsoleScreenBufferInfo(hStdOut, info) then
    Result := info.dwSize.X;
end;
{$ENDIF}
{$IFDEF MACOS}
function GetConsoleWidth : integer;
begin
  result := 80;
  //TODO : Find a way to get the console width on osx
end;
{$ENDIF}
{$IFDEF LINUX64}
function GetConsoleWidth : integer;
begin
  result := 80;
  //TODO : Find a way to get the console width on linux
end;
{$ENDIF}
end.
