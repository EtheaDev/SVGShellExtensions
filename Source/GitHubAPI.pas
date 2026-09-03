{******************************************************************************}
{                                                                              }
{       GitHub API                                                             }
{       (Check and Download new Setup from GitHub Project)                     }
{                                                                              }
{       Copyright (c) 2026 (Ethea S.r.l.)                                      }
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
  ERR_SETUP_FILENAME_MISSING = 'Setup file name not set: cannot download the new version';
  ERR_PROJECT_URL_MISSING = 'GitHub project URL not set: cannot reach the project';

type
  //NB: must be a real class, not an alias of Exception: as an alias any
  //"on E: ECheckNewVersionException" would swallow every exception, Access
  //Violations included.
  ECheckNewVersionException = class(Exception);

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
    //Creates the HTTP client on first use (and again after DestroyClient),
    //applying the timeouts. Every method needing FHTTPClient must call it.
    procedure EnsureClient;
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

const
  //Timeouts (ms) of the version check: it must never hang the caller
  CHECK_CONNECTION_TIMEOUT = 5000;
  CHECK_RESPONSE_TIMEOUT = 15000;
  //The setup download can legitimately take long on a slow line
  DOWNLOAD_RESPONSE_TIMEOUT = 600000;

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
  // Optional 'v'/'V' prefix and an ignored trailing suffix (e.g. a pre-release
  // tag like 'beta' in 'V4.0.18beta'): only the leading N.N.N is significant.
  Regex := TRegEx.Create('^v?(\d+)\.(\d+)\.(\d+)', [roIgnoreCase]);
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
  //Created here and not on first use: CustomHeaders is a published property,
  //so it can be written (also by the streaming system) before any request.
  FCustomHeaders := TStringList.Create;
end;

destructor TGitHubHttpClient.Destroy;
begin
  DestroyClient;
  FreeAndNil(FCustomHeaders);
  inherited;
end;

procedure TGitHubHttpClient.EnsureClient;
begin
  if not Assigned(FHTTPClient) then
  begin
    FHTTPClient := THTTPClient.Create;
    //Without an explicit timeout a check on an unreachable host can hang the
    //caller for a very long time.
    FHTTPClient.ConnectionTimeout := CHECK_CONNECTION_TIMEOUT;
    FHTTPClient.ResponseTimeout := CHECK_RESPONSE_TIMEOUT;
  end;
end;

procedure TGitHubHttpClient.DestroyClient;
begin
  //NB: only the HTTP client is released. The custom headers belong to the
  //component lifetime: destroying them here would silently discard the
  //caller's configuration on the first failed request.
  FreeAndNil(FHTTPClient);
end;

function TGitHubHttpClient.DownloadLatestSetup(
  const ASetupFileName: TFileName;
  const AReceiveDataEvent: TReceiveDataEvent;
  out ADownloadedFileName: TFileName
  ): Int64;
begin
  if ASetupFileName <> '' then
    FSetupFileName := ASetupFileName;
  //NB: real checks and not Assert. Assertions can be compiled out, and an
  //empty project URL did not fail here at all: it silently produced the
  //malformed URL '/releases/latest/download/<setup>'.
  if FSetupFileName = '' then
    raise ECheckNewVersionException.Create(ERR_SETUP_FILENAME_MISSING);
  if FGitHubProjectURL = '' then
    raise ECheckNewVersionException.Create(ERR_PROJECT_URL_MISSING);
  var LFileName := ExtractFileName(FSetupFileName);
  //Build URL Project + 'releases/latest/download/' + FSetupFileName
  var LURL := CombineUrl(FGitHubProjectURL, 'releases/latest/download/');
  LURL := CombineUrl(LURL, LFileName);
  ADownloadedFileName := TPath.Combine(TPath.GetDownloadsPath, LFileName);
  if FileExists(ADownloadedFileName) then
    DeleteFile(ADownloadedFileName);
  EnsureClient;
  var LFileStream := TFileStream.Create(ADownloadedFileName, fmCreate);
  try
    FHTTPClient.Accept := '';
    FHTTPClient.ContentType := 'application/octet-stream';
    //A whole setup takes much longer than a version check
    FHTTPClient.ResponseTimeout := DOWNLOAD_RESPONSE_TIMEOUT;
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
  LTagValue: TJSONValue;
begin
  //'v0.0.0' means "no release found": it compares as older than any version,
  //so the caller simply reports that no update is available.
  Result := 'v0.0.0';
  LJSONValue := TJSONObject.ParseJSONValue(
    GetLatestVersionAsJSonString(AGitHubProjectURL));
  if LJSONValue = nil then
    Exit;
  try
    if LJSONValue is TJSONObject then
    begin
      //The field can be missing on a malformed/unexpected answer
      LTagValue := TJSONObject(LJSONValue).GetValue('tag_name');
      if Assigned(LTagValue) then
        Result := LTagValue.Value;
    end;
  finally
    //Freed on every path: previously a non-object answer leaked it
    LJSONValue.Free;
  end;
end;

function TGitHubHttpClient.GetLatestVersionAsJSonString(
  const AGitHubProjectURL: string = ''): string;
begin
  if AGitHubProjectURL <> '' then
    FGitHubProjectURL := AGitHubProjectURL;
  if FGitHubProjectURL = '' then
    raise ECheckNewVersionException.Create(ERR_PROJECT_URL_MISSING);
  Result := InvokeGETAsString('releases/latest');
end;

function TGitHubHttpClient.InvokeGETAsString(const APath: string): string;
Var
  LHeaders: TArray<TNameValuePair>;
Begin
  Assert((FGitHubProjectURL <> '') and (APath <> ''));
  EnsureClient;
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
    on E: ECheckNewVersionException do
    begin
      DestroyClient;
      raise;
    end;
    on E: Exception do
    begin
      //Transport failures (no network, proxy, timeout, TLS...) are reported to
      //the caller as a version-check error, so that it can offer a retry
      //instead of letting a raw socket exception reach the user.
      DestroyClient;
      raise ECheckNewVersionException.CreateFmt(ERR_GET_REQUEST_FROM_FAILED,
        [CombineUrl(FGitHubProjectURL, APath), E.Message]);
    end;
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
