{******************************************************************************}
{                                                                              }
{       GitHub API                                                             }
{       (Check and Download new Setup from GitHub Project)                     }
{                                                                              }
{       Copyright (c) 2025 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
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
{******************************************************************************}
unit GitHubAPI;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Net.HttpClient;

resourcestring
  ERR_VERSION_FORMAT_NOT_VALID = 'Format of version "%s" is not valid: expected a string "vN.N.N"';
  ERR_GET_REQUEST_FAILED = 'GET requested from "%s" failed, web server could not be reached!';
  ERR_GET_REQUEST_FROM_FAILED = 'GET requested from "%s" failed: "%s"';

type
  ECheckNewVersionException = Exception;

  TGitHubHttpClient = Class(TComponent)
  private
    FHTTPClient: THTTPClient;
    FGitHubProjectURL: string;
    FSetupFileName: TFileName;
    FCustomHeaders: TStringList;
    procedure SetCustomHeaders(const Value: TStringList);
    function CombineUrl(const ABaseUrl, APath: string): string;
    function InvokeGETAsString(const APath: string): string;
    procedure DecodeVersion(const AVersionTag: string; out AMajor, AMinor,
      ARelease: Integer);
    procedure DestroyClient;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetLatestVersionTag(const AGitHubProjectURL: string = ''): string;
    function GetLatestVersionAsJSonString(const AGitHubProjectURL: string = ''): string;
    function IsNewVersionAvailable(const ACurrentVersion: string;
  const AGitHubProjectURL: string;
  out ANewVersion: string): Boolean;
    function DownloadLatestSetup(const ASetupFileName: TFileName;
      const AReceiveDataEvent: TReceiveDataEvent;
      out ADownloadedFileName: TFileName): Int64;
    function CompareVersions(const ACurrentVersion, ANewVersion: string): Integer;
  published
    property GitHubProjectURL: string read FGitHubProjectURL write FGitHubProjectURL;
    property CustomHeaders: TStringList read FCustomHeaders write SetCustomHeaders;
    property SetupFileName: TFileName read FSetupFileName write FSetupFileName;
  end;

implementation

uses
  System.Net.URLClient,
  System.RegularExpressions,
  System.IOUtils,
  System.JSON,
  System.NetConsts
  ;

{ TGitHubHttpClient }

function TGitHubHttpClient.CombineUrl(const ABaseUrl, APath: string): string;
begin
  var CleanPath := APath;
  while (Length(CleanPath) > 0) and (CleanPath[1] = '/') do
    Delete(CleanPath, 1, 1);

  if ABaseUrl.EndsWith('/') then
    Result := ABaseUrl + CleanPath
  else
    Result := ABaseUrl + '/' + CleanPath;
end;

procedure TGitHubHttpClient.DecodeVersion(const AVersionTag: string;
  out AMajor, AMinor, ARelease: Integer);
var
  Match: TMatch;
  Regex: TRegEx;
begin
  AMajor := 0;
  AMinor := 0;
  ARelease := 0;
  Regex := TRegEx.Create('^v(\d+)\.(\d+)\.(\d+)$', [roIgnoreCase]);
  Match := Regex.Match(AVersionTag);
  if Match.Success then
  begin
    AMajor := StrToInt(Match.Groups[1].Value);
    AMinor := StrToInt(Match.Groups[2].Value);
    ARelease := StrToInt(Match.Groups[3].Value);
  end
  else
  begin
    DestroyClient;
    raise ECheckNewVersionException.CreateFmt(
      ERR_VERSION_FORMAT_NOT_VALID,
      [AVersionTag]);
  end;
end;

function TGitHubHttpClient.CompareVersions(const ACurrentVersion,
  ANewVersion: string): Integer;
var
  ACurrentMajor, ACurrentMinor, ACurrentRelease: Integer;
  ANewMajor, ANewMinor, ANewRelease: Integer;
begin
  //Returns 1 if ANewVersion > ACurrentVersion
  //Returns 0 if ANewVersion = ACurrentVersion
  //Returns -1 if ANewVersion < ACurrentVersion
  DecodeVersion(ACurrentVersion, ACurrentMajor, ACurrentMinor, ACurrentRelease);
  DecodeVersion(ANewVersion, ANewMajor, ANewMinor, ANewRelease);
  if ANewMajor > ACurrentMajor then
    Exit(1)
  else if ANewMajor < ACurrentMajor then
    Exit(-1);
  if ANewMinor > ACurrentMinor then
    Exit(1)
  else if ANewMinor < ACurrentMinor then
    Exit(-1);
  if ANewRelease > ACurrentRelease then
    Exit(1)
  else if ANewRelease < ACurrentRelease then
    Exit(-1);
  Result := 0; // Same versions
end;

constructor TGitHubHttpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TGitHubHttpClient.Destroy;
begin
  DestroyClient;
  inherited;
end;

procedure TGitHubHttpClient.DestroyClient;
begin
  if Assigned(FHTTPClient) then
  begin
    FreeAndNil(FHTTPClient);
    FreeAndNil(FCustomHeaders);
  end;
end;

function TGitHubHttpClient.DownloadLatestSetup(
  const ASetupFileName: TFileName;
  const AReceiveDataEvent: TReceiveDataEvent;
  out ADownloadedFileName: TFileName
  ): Int64;
begin
  if ASetupFileName <> '' then
    FSetupFileName := ASetupFileName;
  Assert(FSetupFileName <> '');
  var LFileName := ExtractFileName(FSetupFileName);
  //Build URL Project + 'releases/latest/download/' + FSetupFileName
  var LURL := CombineUrl(FGitHubProjectURL, 'releases/latest/download/');
  LURL := CombineUrl(LURL, LFileName);
  ADownloadedFileName := TPath.Combine(TPath.GetDownloadsPath, LFileName);
  if FileExists(ADownloadedFileName) then
    DeleteFile(ADownloadedFileName);
  var LFileStream := TFileStream.Create(ADownloadedFileName, fmCreate);
  try
    FHTTPClient.Accept := '';
    FHTTPClient.ContentType := 'application/octet-stream';
    FHTTPClient.OnReceiveData := AReceiveDataEvent;
    var LResponse := FHTTPClient.Get(LURL, LFileStream);
    if LResponse.StatusCode <> 200 then
    begin
      DestroyClient;
      raise ECheckNewVersionException.Create(LResponse.StatusText)
    end
    else
      Result := LFileStream.Size;
  finally
    LFileStream.Free;
  end;
end;

function TGitHubHttpClient.GetLatestVersionTag(
  const AGitHubProjectURL: string = ''): string;
var
  LJSONValue: TJSONValue;
begin
  LJSONValue :=TJSONObject.ParseJSONValue(
    GetLatestVersionAsJSonString(AGitHubProjectURL));
  if LJSONValue is TJSONObject then
  begin
    try
      Result := TJSONObject(LJSONValue).GetValue('tag_name').Value;
    finally
      LJSONValue.Free;
    end;
  end
  else
    Result := 'v0.0.0';
end;

function TGitHubHttpClient.GetLatestVersionAsJSonString(
  const AGitHubProjectURL: string = ''): string;
begin
  if AGitHubProjectURL <> '' then
    FGitHubProjectURL := AGitHubProjectURL;
  Assert(FGitHubProjectURL <> '');
  Result := InvokeGETAsString('releases/latest');
end;

function TGitHubHttpClient.InvokeGETAsString(const APath: string): string;
Var
  LHeaders: TArray<TNameValuePair>;
Begin
  Assert((FGitHubProjectURL <> '') and (APath <> ''));
  if not Assigned(FHTTPClient) then
  begin
    FHTTPClient := THTTPClient.Create;
    FCustomHeaders := TStringList.Create;
  end;
  try
    //Add a custom header to Request
    if FCustomHeaders.Count > 0 then
    begin
      SetLength(LHeaders, FCustomHeaders.Count);
      for var I := 0 to FCustomHeaders.Count-1 do
      begin
        LHeaders[I].Name := FCustomHeaders.Names[I];
        LHeaders[I].Value := FCustomHeaders.ValueFromIndex[I];
      end;
    end
    else
    begin
      FHTTPClient.Accept := 'application/json';
      FHTTPClient.ContentType := 'application/json';
    end;
    //Build URL Project + Path
    var LURL := CombineUrl(FGitHubProjectURL, APath);
    var LOutStream := TStringStream.Create('', TEncoding.UTF8);
    try
      var LResponse := FHTTPClient.Get(LURL, LOutStream, LHeaders);

      if Not Assigned(LResponse) Then
      begin
        DestroyClient;
        Raise ECheckNewVersionException.CreateFmt(
          ERR_GET_REQUEST_FAILED, [LURL]);
      end;

      if LResponse.StatusCode <> 200 Then
      begin
        DestroyClient;
        Raise ECheckNewVersionException.CreateFmt(
         ERR_GET_REQUEST_FROM_FAILED,
          [LURL, LResponse.ContentAsString(TEncoding.UTF8)]);
      end;

      Result := LOutStream.DataString;
    finally
      LOutStream.Free;
    end;
  except
    DestroyClient;
    raise;
  end;
end;

function TGitHubHttpClient.IsNewVersionAvailable(
  const ACurrentVersion: string;
  const AGitHubProjectURL: string;
  out ANewVersion: string): Boolean;
begin
  ANewVersion := GetLatestVersionTag(AGitHubProjectURL);
  Result := CompareVersions(ACurrentVersion, ANewVersion) > 0;
end;

procedure TGitHubHttpClient.SetCustomHeaders(const Value: TStringList);
begin
  FCustomHeaders.Assign(Value);
end;

end.
