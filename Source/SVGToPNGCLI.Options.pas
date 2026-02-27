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
unit SVGToPNGCLI.Options;

interface

uses
  System.SysUtils;

const
  EXITCODE_OK = 0;
  EXITCODE_UNDEFINED_ERROR = -1;
  EXITCODE_PARSE_ERROR = 1;

type
  TOptionsForHelp = class
  public
    class var CommandName: string;
  end;

  TOptionsForInput = class
  public
  class var
    InputPattern: TFileName;
  end;

  TOptionsForOutput = class
  public
  class var
    OutputFolder: string;
  end;

  TOptionsForConvert = class
  public
  const CONVERT_COMMAND = 'convert';
  class var
    Sizes: string;
    FixedColor: string;
    GrayScale: Boolean;
    Opacity: Single;
  class constructor Create;
  class procedure Execute;
  end;

implementation

uses
  System.IOUtils,
  System.Types,
  System.UITypes,
  System.StrUtils,
  Vcl.Graphics,
  SVGInterfaces,
  SVGIconUtils,
  SVGToPNGCLI.ResourceStrings,
  SVGToPNGCLI.Console;

function ParseColorString(const AColorStr: string): TColor;
var
  LColor: string;
begin
  if AColorStr = '' then
    Exit(SVG_INHERIT_COLOR);

  LColor := Trim(AColorStr);

  // Handle #RRGGBB hex format -> convert to Delphi $00BBGGRR
  if (Length(LColor) = 7) and (LColor[1] = '#') then
  begin
    Result := TColor(
      StrToInt('$' + Copy(LColor, 6, 2) + Copy(LColor, 4, 2) + Copy(LColor, 2, 2)));
    Exit;
  end;

  // Handle Delphi color names (clBlack, clRed, etc.)
  Result := StringToColor(LColor);
end;

function ParseSizes(const ASizes: string): TArray<Integer>;
var
  LParts: TStringDynArray;
  I, LSize: Integer;
  LResult: TArray<Integer>;
begin
  LParts := SplitString(ASizes, ',');
  SetLength(LResult, Length(LParts));
  for I := 0 to High(LParts) do
  begin
    if not TryStrToInt(Trim(LParts[I]), LSize) then
      raise Exception.CreateFmt(StrErrInvalidSize, [Trim(LParts[I])]);
    LResult[I] := LSize;
  end;
  Result := LResult;
end;

{ TOptionsForConvert }

class constructor TOptionsForConvert.Create;
begin
  Sizes := '32';
  FixedColor := '';
  GrayScale := False;
  Opacity := 1.0;
end;

class procedure TOptionsForConvert.Execute;
var
  LSVG: ISVG;
  LFiles: TStringDynArray;
  LSizes: TArray<Integer>;
  LInputDir, LFilePattern, LOutputDir: string;
  LBaseName, LOutFileName: string;
  LColor: TColor;
  I, J: Integer;
begin
  TConsole.LogInfoSuccess(StrStartCommand, CONVERT_COMMAND, True);

  // Parse sizes
  LSizes := ParseSizes(Sizes);
  TConsole.LogInfoSuccess(StrSizes, Sizes);

  // Parse color
  LColor := ParseColorString(FixedColor);

  // Resolve input pattern
  LInputDir := ExtractFilePath(TOptionsForInput.InputPattern);
  LFilePattern := ExtractFileName(TOptionsForInput.InputPattern);

  if LInputDir = '' then
    LInputDir := GetCurrentDir;
  LInputDir := TPath.GetFullPath(LInputDir);

  // Resolve output folder
  if TOptionsForOutput.OutputFolder <> '' then
    LOutputDir := TPath.GetFullPath(TOptionsForOutput.OutputFolder)
  else
    LOutputDir := LInputDir;

  // Create output folder if it does not exist
  if not TDirectory.Exists(LOutputDir) then
    TDirectory.CreateDirectory(LOutputDir);

  TConsole.LogInfoSuccess(StrOutputFolder, LOutputDir);

  // Find files matching pattern
  LFiles := TDirectory.GetFiles(LInputDir, LFilePattern);

  if Length(LFiles) = 0 then
    raise Exception.CreateFmt(StrErrNoFilesFound, [TOptionsForInput.InputPattern]);

  TConsole.LogInfoSuccess(StrFilesFound, IntToStr(Length(LFiles)));

  // Process each file
  for I := 0 to High(LFiles) do
  begin
    TConsole.LogInfoSuccess(StrProcessingFile, ExtractFileName(LFiles[I]));

    LSVG := GlobalSVGFactory.NewSvg;
    LSVG.LoadFromFile(LFiles[I]);

    // Apply optional properties
    if LColor <> SVG_INHERIT_COLOR then
      LSVG.FixedColor := LColor;
    if GrayScale then
      LSVG.GrayScale := True;
    if Opacity <> 1.0 then
      LSVG.Opacity := Opacity;

    LBaseName := TPath.GetFileNameWithoutExtension(LFiles[I]);

    for J := 0 to High(LSizes) do
    begin
      // Build output file name
      if Length(LSizes) = 1 then
        LOutFileName := LBaseName
      else
        LOutFileName := LBaseName + '_' + IntToStr(LSizes[J]);

      SVGExportToPng(LSizes[J], LSizes[J], LSVG, LOutputDir, LOutFileName);

      TConsole.LogInfoSuccess(StrExportedPng,
        LOutFileName + '.png' + ' (' + IntToStr(LSizes[J]) + 'x' + IntToStr(LSizes[J]) + ')');
    end;
  end;

  TConsole.LogInfoSuccess(StrDoneCommand, CONVERT_COMMAND, True);
end;

end.
