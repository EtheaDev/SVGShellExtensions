{******************************************************************************}
{                                                                              }
{       SVG Shell Extensions: Shell extensions for SVG files                   }
{       (Preview Panel, Thumbnail Icon, SVG Editor)                            }
{                                                                              }
{       Copyright (c) 2021-2024 (Ethea S.r.l.)                                 }
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
unit uPreviewHandlerRegister;

interface

uses
  ComObj,
  Classes,
  Windows,
  uPreviewHandler;

type
  TPreviewHandlerRegister = class(TComObjectFactory)
  private
    FPreviewHandlerClass: TPreviewHandlerClass;
    class procedure DeleteRegValue(const Key, ValueName: string; RootKey: HKEY);
  protected
  public
    constructor Create(APreviewHandlerClass: TPreviewHandlerClass;
      const APreviewClassID: TGUID; const AName, ADescription: string);
    destructor Destroy; override;
    function CreateComObject(const Controller: IUnknown): TComObject; override;
    procedure UpdateRegistry(ARegister: Boolean); override;
    property PreviewHandlerClass: TPreviewHandlerClass read FPreviewHandlerClass;
  end;

implementation

uses
  Math,
  StrUtils,
  SysUtils,
  ShlObj,
  System.Win.ComConst,
  ComServ;

constructor TPreviewHandlerRegister.Create(APreviewHandlerClass: TPreviewHandlerClass;
  const APreviewClassID: TGUID;  const AName, ADescription: string);
begin
  inherited Create(ComServ.ComServer, APreviewHandlerClass.GetComClass,
    APreviewClassID, AName, ADescription, ciMultiInstance, tmApartment);
  FPreviewHandlerClass := APreviewHandlerClass;
end;

function TPreviewHandlerRegister.CreateComObject(const Controller: IUnknown): TComObject;
begin
  result := inherited CreateComObject(Controller);
  TComPreviewHandler(result).PreviewHandlerClass := PreviewHandlerClass;
end;

class procedure TPreviewHandlerRegister.DeleteRegValue(const Key, ValueName: string; RootKey: HKEY);
var
  RegKey: HKEY;
begin
  if RegOpenKeyEx(RootKey, PChar(Key), 0, KEY_ALL_ACCESS, regKey) = ERROR_SUCCESS then
  begin
    try
      RegDeleteValue(regKey, PChar(ValueName));
    finally
      RegCloseKey(regKey)
    end;
  end;
end;

destructor TPreviewHandlerRegister.Destroy;
begin
  inherited;
end;

//How to Register a Preview Handler
//http://msdn.microsoft.com/en-us/library/cc144144%28v=vs.85%29.aspx
procedure TPreviewHandlerRegister.UpdateRegistry(ARegister: Boolean);

    function IsWow64Process: Boolean;
    type
      TIsWow64Process = function( hProcess: Windows.THandle; var Wow64Process: Windows.BOOL): Windows.BOOL; stdcall;
    var
      IsWow64Process: TIsWow64Process;
      Wow64Process: Windows.BOOL;
    begin
      Result := False;
      IsWow64Process := GetProcAddress(GetModuleHandle(Windows.kernel32), 'IsWow64Process');
      if Assigned(IsWow64Process) then
      begin
        if not IsWow64Process(GetCurrentProcess, Wow64Process) then
         Raise Exception.Create('Invalid handle');
        Result := Wow64Process;
      end;
    end;


    procedure CreateRegKeyDWORD(const Key, ValueName: string;Value: DWORD; RootKey: HKEY);
    var
      Handle: HKey;
      Status, Disposition: Integer;
    begin
      Status := RegCreateKeyEx(RootKey, PChar(Key), 0, '',
        REG_OPTION_NON_VOLATILE, KEY_READ or KEY_WRITE, nil, Handle,
        @Disposition);
      if Status = 0 then
      begin
        {
        Status := RegSetValueEx(Handle, PChar(ValueName), 0, REG_SZ,
          PChar(Value), (Length(Value) + 1)* sizeof(char));
        }
        Status := RegSetValueEx(Handle, PChar(ValueName), 0, REG_DWORD,
          @Value, sizeof(Value));
        RegCloseKey(Handle);
      end;
      if Status <> 0 then raise EOleRegistrationError.CreateRes(@SCreateRegKeyError);
    end;

  procedure CreateRegKeyREG_SZ(const Key, ValueName: string;Value: string; RootKey: HKEY);
  var
    Handle: HKey;
    Status, Disposition: Integer;
  begin
    Status := RegCreateKeyEx(RootKey, PChar(Key), 0, '',
      REG_OPTION_NON_VOLATILE, KEY_READ or KEY_WRITE, nil, Handle,
      @Disposition);
    if Status = 0 then
    begin
      Status := RegSetValueEx(Handle, PChar(ValueName), 0, REG_SZ,
        PChar(Value), (Length(Value) + 1)* sizeof(char));
      RegCloseKey(Handle);
    end;
    if Status <> 0 then raise EOleRegistrationError.CreateRes(@SCreateRegKeyError);
  end;

const
  Prevhost_32 = '{534A1E02-D58F-44f0-B58B-36CBED287C7C}';
  Prevhost_64 = '{6d2b5079-2f0b-48dd-ab7f-97cec514d30b}';

var
  RootKey: HKEY;
  RootUserReg: HKEY;
  RootPrefix: string;
  sComServerKey: string;
  ProgID: string;
  sAppID: string;
  sClassID: string;
  LRegKey: string;
begin

  if Instancing = ciInternal then
    Exit;

  ComServer.GetRegRootAndPrefix(RootKey, RootPrefix);
  RootUserReg := IfThen(ComServer.PerUserRegistration, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE);
  sClassID := SysUtils.GUIDToString(ClassID);
  ProgID := GetProgID;
  sComServerKey := Format('%sCLSID\%s\%s',[RootPrefix,sClassID,ComServer.ServerKey]);
  sAppID := IfThen(IsWow64Process, Prevhost_32, Prevhost_64);
  if ARegister then
  begin
    inherited UpdateRegistry(True);

    LRegKey := Format('%sCLSID\%s',[RootPrefix, sClassID]);
    CreateRegKey(LRegKey, 'AppID', sAppID, RootKey);
    CreateRegKey(LRegKey, 'DisplayName', 'Delphi Svg Preview Handler', RootKey);
    CreateRegKeyDWORD(LRegKey, 'DisableLowILProcessIsolation', 1, RootKey);
    CreateRegKeyREG_SZ(LRegKey, 'DllSurrogate', '%SystemRoot%\system32\prevhost.exe', RootKey);

    if ProgID <> '' then
    begin
      CreateRegKey(sComServerKey, 'ProgID', ProgID, RootKey);

      //Add extension for .svg files
      CreateRegKey(RootPrefix + '.svg' + '\shellex\' + SID_IPreviewHandler, '', sClassID, RootKey);

      CreateRegKey(sComServerKey, 'VersionIndependentProgID', ProgID, RootKey);
      CreateRegKey(RootPrefix + ProgID + '\shellex\' + SID_IPreviewHandler, '', sClassID, RootKey);
      CreateRegKey('SOFTWARE\Microsoft\Windows\CurrentVersion\PreviewHandlers', sClassID, Description, RootUserReg);
    end;
  end
  else
  begin
    if ProgID <> '' then
    begin
      DeleteRegValue('SOFTWARE\Microsoft\Windows\CurrentVersion\PreviewHandlers', sClassID, RootUserReg);
      DeleteRegKey(RootPrefix + ProgID + '\shellex', RootKey);
      DeleteRegValue(Format('%sCLSID\%s',[RootPrefix, sClassID]), 'DllSurrogate', RootKey);
      DeleteRegValue(Format('%sCLSID\%s',[RootPrefix, sClassID]), 'DisableLowILProcessIsolation', RootKey);
      //Delete extension for svg
      DeleteRegKey(RootPrefix + '.svg' + '\shellex\' + SID_IPreviewHandler, RootKey);
    end;
    inherited UpdateRegistry(False);
  end;
end;

end.
