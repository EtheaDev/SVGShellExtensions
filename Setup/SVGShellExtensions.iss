; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#define MyAppName 'SVG Shell Extensions and SVG Text Editor'
#define MyAppVersion '3.2.2'

[Setup]
AppName={#MyAppName}
AppPublisher=Ethea S.r.l.
AppVerName={#MyAppName} {#MyAppVersion}
VersionInfoVersion={#MyAppVersion}
AppPublisherURL=https://www.ethea.it/
AppSupportURL=https://github.com/EtheaDev/SVGShellExtensions/issues
DefaultDirName={commonpf}\Ethea\SVGShellExtensions
OutputBaseFileName=SVGShellExtensionsSetup
DisableDirPage=false
DefaultGroupName=SVG Shell Extensions
Compression=lzma
SolidCompression=true
UsePreviousAppDir=false
AppendDefaultDirName=false
PrivilegesRequired=admin
WindowVisible=false
WizardImageFile=WizEtheaImage.bmp
WizardSmallImageFile=WizEtheaSmallImage.bmp
AppContact=info@ethea.it
SetupIconFile=..\Icons\setup.ico
AppID=SVGShellExtensions
UsePreviousSetupType=false
UsePreviousTasks=false
AlwaysShowDirOnReadyPage=true
AlwaysShowGroupOnReadyPage=true
ShowTasksTreeLines=true
DisableWelcomePage=False
AppCopyright=Copyright � 2021-2024 Ethea S.r.l.
ArchitecturesInstallIn64BitMode=x64
MinVersion=0,6.1sp1
CloseApplications=force
UninstallDisplayIcon={app}\SVGTextEditor.exe

[Languages]
Name: eng; MessagesFile: compiler:Default.isl; LicenseFile: .\License_ENG.rtf
Name: ita; MessagesFile: compiler:Languages\Italian.isl; LicenseFile: .\Licenza_ITA.rtf


[CustomMessages]
UninstallMessage=Undefined
eng.UninstallMessage=An older version of "SVG Shell Extensions and SVG Text Editor"  was detected. The uninstaller will be executed...
ita.UninstallMessage=� stata rilevata una versione precedente di "SVG  Shell Extensions and SVG Text Editor". Il programma di disinstallazione verr� eseguito...
[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked; Components: Editor

[Files]
; 32 Bit files
Source: "..\Bin32\sk4d.dll"; DestDir: "{sys}"; Flags : sharedfile 32bit; Components: ShellExtensions
Source: "..\Bin32\SVGTextEditor.exe"; DestDir: "{app}"; Flags: ignoreversion 32bit; Components: Editor
Source: "..\Bin32\SVGShellExtensions32.dll"; DestDir: {app}; Flags : regserver sharedfile; Components: ShellExtensions

; 64 Bit files
Source: "..\Bin64\sk4d.dll"; DestDir: "{sys}"; Flags : sharedfile 64bit; Components: ShellExtensions
Source: "..\Bin64\SVGTextEditor.exe"; DestDir: "{app}"; Flags: ignoreversion 64bit; Components: Editor
Source: "..\Bin64\SVGShellExtensions.dll"; DestDir: {app}; Flags : 64bit regserver sharedfile uninsrestartdelete; Components: ShellExtensions

[Icons]
Name: "{group}\SVGTextEditor"; Filename: "{app}\SVGTextEditor.exe"; WorkingDir: "{app}"; IconFilename: "{app}\SVGTextEditor.exe"; Components: Editor
Name: "{commondesktop}\SVGTextEditor"; Filename: "{app}\SVGTextEditor.exe"; Tasks: desktopicon; Components: Editor

[Run]
Filename: {app}\SVGTextEditor.exe; Description: {cm:LaunchProgram,'SVG Text Editor'}; Flags: nowait postinstall skipifsilent; Components: Editor

[Components]
Name: Editor; Description: SVG Text Editor; Flags: fixed; Types: custom full
Name: ShellExtensions; Description: Shell Extensions (Preview and Thumbnails); Types: custom compact full

[Registry]
Root: "HKCR"; Subkey: ".svg"; ValueType: string; ValueData: "Open"; Flags: uninsdeletekey
Root: "HKCR"; Subkey: "Applications\SVGTextEditor.exe"; ValueType: string; ValueData: "SVG Text Editor"; Flags: uninsdeletekey
Root: "HKCR"; Subkey: "Applications\SVGTextEditor.exe\Shell\Open\Command"; ValueType: string; ValueData: """{app}\SVGTextEditor.exe"" ""%1"""; Flags: uninsdeletevalue
Root: "HKCR"; Subkey: "Applications\SVGTextEditor.exe\DefaultIcon"; ValueType: string; ValueData: "{app}\SVGTextEditor.exe,0"; Flags: uninsdeletevalue

[Code]

function IsSilentMode(): Boolean;
var
  j: Cardinal;
begin
  Result := false;   
  begin
  for j := 0 to ParamCount do
  begin
    Result := (ParamStr(j)='/verysilent') or (ParamStr(j)='/silent') or
     (ParamStr(j)='/VERYSILENT') or (ParamStr(j)='/SILENT');
    if Result then
      break;
    end; 
  end;
end;

function InitializeSetup(): Boolean;
begin
   Result:=True;
end;

function GetUninstallString(): String;
var
  sUnInstPath: String;
  sUnInstallString: String;
begin
  sUnInstPath := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\{#emit SetupSetting("AppId")}_is1');
  sUnInstallString := '';
  if not RegQueryStringValue(HKLM, sUnInstPath, 'UninstallString', sUnInstallString) then
    RegQueryStringValue(HKCU, sUnInstPath, 'UninstallString', sUnInstallString);
  Result := sUnInstallString;
end;

function IsUpgrade(): Boolean;
var
  s : string;
begin
  s := GetUninstallString();
  //MsgBox('GetUninstallString '+s, mbInformation, MB_OK);
  Result := (s <> '');
end;

function UnInstallOldVersion(): Integer;
var
  sUnInstallString: String;
  iResultCode: Integer;
begin
// Return Values:
// 1 - uninstall string is empty
// 2 - error executing the UnInstallString
// 3 - successfully executed the UnInstallString

  // default return value
  Result := 0;

  // get the uninstall string of the old app
  sUnInstallString := GetUninstallString();
  if sUnInstallString <> '' then 
  begin
    sUnInstallString := RemoveQuotes(sUnInstallString);
    if Exec(sUnInstallString, '/SILENT /NORESTART /SUPPRESSMSGBOXES', '', SW_HIDE, ewWaitUntilTerminated, iResultCode) then
      Result := 3
    else
      Result := 2;
  end else
    Result := 1;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if (CurStep=ssInstall) then
  begin
    if (IsUpgrade()) then
    begin
      if not IsSilentMode() then
        MsgBox(ExpandConstant('{cm:UninstallMessage}'), mbInformation, MB_OK);
      UnInstallOldVersion();
    end;
  end;
end;
