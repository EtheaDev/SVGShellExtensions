{******************************************************************************}
{                                                                              }
{       SVG Shell Extensions: Shell extensions for SVG files                   }
{       (Preview Panel, Thumbnail Icon, SVG Editor)                            }
{                                                                              }
{       Copyright (c) 2021-2023 (Ethea S.r.l.)                                 }
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
{ The Original Code is:                                                        }
{ Writing a Windows Shell Extension (by Marco Cantu)                           }
{ https://blog.marcocantu.com/blog/2016-03-writing-windows-shell-extension.html}
{                                                                              }
{******************************************************************************}
unit uSVGContextMenuHandler;

interface

uses
  Winapi.Windows
  , ActiveX
  , ComObj
  , ShlObj
  , ShellApi
  , uSettings
  , SVGInterfaces
  , SVGIconUtils;

const
  MENU_ITEM_OPEN_WITH_EDITOR = 0;
  MENU_ITEM_EXPORT_TO_PNG = 1;
  MENU_ITEM_COUNT = 2;

type
  TSVGContextMenu = class(TComObject, IUnknown,
    IContextMenu,
    //IContextMenu2, IContextMenu3,
    IShellExtInit)
  private
    FFileName: string;
    FOwnerDrawId: UINT;
    FSettings: TPreviewSettings;
  protected
    {Declare IContextMenu methods here}
    function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast,
      uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(idCmd: UINT_PTR; uFlags: UINT; pwReserved: PUINT;
      pszName: LPSTR; cchMax: UINT): HResult; stdcall;
    {Declare IShellExtInit methods here}
    function IShellExtInit.Initialize = InitShellExt;
    function InitShellExt(pidlFolder: PItemIDList; lpdobj: IDataObject;
      hKeyProgID: HKEY): HResult; stdcall;
    //IContextMenu2
(*
    function HandleMenuMsg(uMsg: UINT; WParam: WPARAM; LParam: LPARAM): HResult; stdcall;
    //IContextMenu3
    function HandleMenuMsg2(uMsg: UINT; wParam: WPARAM; lParam: LPARAM; var lpResult: LRESULT): HResult; stdcall;
    function MenuMessageHandler(uMsg: UINT; wParam: WPARAM; lParam: LPARAM; var lpResult: LRESULT): HResult; stdcall;
*)
  public
    destructor Destroy; override;
  end;

  TSVGContextMenuFactory = class (TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

const
  MyClass_SVGContextMenu_64: TGUID = '{B763EEFC-33D1-4F23-A451-C0D9A5EED592}';
  MyClass_SVGContextMenu_32: TGUID = '{2E933706-1E1D-48CA-9DB8-4706CA13B7F8}';

implementation

uses
  Vcl.Graphics
  , System.Types
  , ComServ
  , Messages
  , SysUtils
  , Registry
  , uLogExcept
  , System.Classes
{$IFNDEF DISABLE_STYLES}
  , Vcl.Themes
{$ENDIF}
  , dlgExportPNG
  , DResources;

// IShellExtInit method
function TSVGContextMenu.InitShellExt(pidlFolder: PItemIDList;
  lpdobj: IDataObject; hKeyProgID: HKEY): HResult; stdcall;
var
  medium: TStgMedium;
  fe: TFormatEtc;
  LFileExt: string;
  LCountFile: Integer;
begin
  TLogPreview.Add('TSVGContextMenu.InitShellExt');

  Result := E_FAIL;
  // check if the lpdobj pointer is nil
  if Assigned(lpdobj) then
  begin
    TLogPreview.Add('Assigned(lpdobj)');
    with fe do
    begin
      cfFormat := CF_HDROP;
      ptd := nil;
      dwAspect := DVASPECT_CONTENT;
      lindex := -1;
      tymed := TYMED_HGLOBAL;
    end;
    // transform the lpdobj data to a storage medium structure
    Result := lpdobj.GetData(fe, medium);
    if not Failed(Result) then
    begin
      LCountFile := DragQueryFile(medium.hGlobal, $FFFFFFFF, nil, 0);
      TLogPreview.Add('LCountFile: '+IntToStr(LCountFile));
      // check if only one file is selected
      if LCountFile = 1 then
      begin
        SetLength(FFileName, 1000);
        DragQueryFile(medium.hGlobal, 0, PChar (FFileName), 1000);
        // realign string
        FFileName := PChar(FFileName);
        FSettings := TPreviewSettings.CreateSettings(nil);
        TLogPreview.Add('FFileName: '+FFileName);
        LFileExt := ExtractFileExt(FFileName);
        // only for .svg files
        if SameText(LFileExt,'.svg') then
          Result := NOERROR
        else
          Result := E_FAIL;
      end
      else
        Result := E_FAIL;
    end;
    ReleaseStgMedium(medium);
  end;
end;

// context menu methods

function TSVGContextMenu.QueryContextMenu(Menu: HMENU;
  indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult;
var
  LMenuIndex: Integer;
begin
  FOwnerDrawId := idCmdFirst;
  // add a new item to context menu
  LMenuIndex := indexMenu;
  InsertMenu(Menu, LMenuIndex, MF_STRING or MF_BYPOSITION, idCmdFirst+MENU_ITEM_OPEN_WITH_EDITOR,
    'Open with "SVG Text Editor"...');
  Inc(LMenuIndex);
  InsertMenu(Menu, LMenuIndex, MF_STRING or MF_BYPOSITION, idCmdFirst+MENU_ITEM_EXPORT_TO_PNG,
    'Export to PNG files...');
  // Return number of menu items added
  Result := MENU_ITEM_COUNT;
end;

function TSVGContextMenu.InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult;
var
  LStringStream: TStringStream;
  LFileName: string;
  LSVGText: string;
  Reg: TRegistry;
  LCommand: string;

  procedure EditorNotInstalled;
  begin
    MessageBox(0, '"SVG Text Editor" not installed',
      'Error opening file', MB_OK);
  end;

  procedure EditorNotFound;
  begin
    MessageBox(0, '"SVG Text Editor" not found!',
      'Error opening file', MB_OK);
  end;

begin
  Result := NOERROR;
  // Make sure we are not being called by an application
  if HiWord(NativeInt(lpici.lpVerb)) <> 0 then
  begin
    Result := E_FAIL;
    Exit;
  end;
  // Make sure we aren't being passed an invalid argument number
  if LoWord(lpici.lpVerb) >= MENU_ITEM_COUNT then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;
  // execute the command specified by lpici.lpVerb.
  if LoWord(lpici.lpVerb) = MENU_ITEM_OPEN_WITH_EDITOR then
  begin
    TLogPreview.Add('TSVGContextMenu: Menu clicked');

    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      TLogPreview.Add('TSVGContextMenuHandler: Open Registry');
      if Reg.OpenKey('OpenSVGEditor\Shell\Open\Command', False) then
      begin
        LCommand := Reg.ReadString('');
        LCommand := StringReplace(LCommand,' "%1"','', []);
        LFileName := format('"%s"',[FFileName]);
        TLogPreview.Add(Format('TSVGContextMenuHandler: Command: %s FileName %s',
          [LCommand, LFileName]));
        if (FFileName <> '') and FileExists(FFileName) then
        begin
          TLogPreview.Add(Format('TSVGContextMenuHandler: ShellExecute: %s for file %s',
            [LCommand, LFileName]));
          ShellExecute(0, 'Open', PChar(LCommand), PChar(LFileName), nil, SW_SHOWNORMAL);
        end
        else
          EditorNotInstalled;
      end
      else
        EditorNotFound;
    finally
      Reg.Free;
    end;
  end
  else if LoWord(lpici.lpVerb) = MENU_ITEM_EXPORT_TO_PNG then
  begin
    LStringStream := TStringStream.Create('',TEncoding.UTF8);
    LStringStream.LoadFromFile(fFileName);
    try
      LFileName := ChangeFileExt(fFileName,'.png');
{$IFNDEF DISABLE_STYLES}
        if (Trim(FSettings.StyleName) <> '') and not SameText('Windows', FSettings.StyleName) then
          TStyleManager.TrySetStyle(FSettings.StyleName, False);
{$ENDIF}
      LSVGText := LStringStream.DataString;
      ExportToPNG(TRect.Create(0,0,0,0),
        LFileName,
        LSVGText,
        False,
        FSettings.PngExportCustomSize,
        FSettings.PngExportFormat,
        FSettings.PngExportSizes);
    finally
      LStringStream.Free;
    end;
  end;
end;

destructor TSVGContextMenu.Destroy;
begin
  FreeAndNil(FSettings);
  inherited;
end;

function TSVGContextMenu.GetCommandString(idCmd: UINT_PTR; uFlags: UINT; pwReserved: PUINT;
  pszName: LPSTR; cchMax: UINT): HResult; stdcall;
begin
  Result := E_INVALIDARG;
end;

(*
//IContextMenu2
function TSVGContextMenu.HandleMenuMsg(uMsg: UINT; WParam: WPARAM; LParam: LPARAM): HResult; stdcall;
var
 res: Winapi.Windows.LPARAM;
begin
 TLogPreview.Add('HandleMenuMsg: HandleMenuMsg');
 Result:=MenuMessageHandler ( uMsg, wParam, lParam, res);
end;

//IContextMenu3
function TSVGContextMenu.HandleMenuMsg2(uMsg: UINT; wParam: WPARAM; lParam: LPARAM; var lpResult: LRESULT): HResult; stdcall;
begin
  TLogPreview.Add('HandleMenuMsg: HandleMenuMsg2');
  Result:= MenuMessageHandler( uMsg, wParam, lParam, lpResult);
end;

function TSVGContextMenu.MenuMessageHandler(uMsg: UINT; wParam: WPARAM; lParam: LPARAM; var lpResult: LRESULT): HResult; stdcall;
const
  Dx = 20;
  Dy = 5;
  MinHeight = 16;
var
  LRect: TRect;
  i, Lx,Ly :Integer;
  LCanvas: TCanvas;
  SaveIndex: Integer;
  LIcon: TIcon;
  FSVG: ISVG;
  Found: Boolean;
begin
  TLogPreview.Add('HandleMenuMsg: MenuMessageHandler');
  try
    case uMsg of
      WM_DRAWITEM:
      begin
        if PDrawItemStruct(lParam)^.itemID<>FOwnerDrawId then
        with PDrawItemStruct(lParam)^ do
        begin
          FSVG := GlobalSVGFactory.NewSvg;
          FSVG.Source := GETSVGLogoText;
          LRect.Left := rcItem.Left-16;
          LRect.Top := rcItem.Top + (rcItem.Bottom - rcItem.Top - 16) div 2;
          LRect.Width := 16;
          LRect.Height := 16;
          FSVG.PaintTo(hDC,LRect,False);
        end;
      end;
    end;
    Result:=S_OK;

  except on  E: Exception do
    begin
     Result := E_FAIL;
    end;
  end;
end;
*)

{ TSVGContextMenuFactory methods }

procedure TSVGContextMenuFactory.UpdateRegistry(Register: Boolean);
var
  Reg: TRegistry;
begin
  inherited UpdateRegistry (Register);

  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CLASSES_ROOT;
  if not Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved', True) then
    Exit;
  try
    if Register then
    begin
      //New registration only for .svg files
      {$IFDEF WIN64}
      if Reg.OpenKey('\*\ShellEx\ContextMenuHandlers\SVGContextMenu', True) then
        Reg.WriteString('', GUIDToString(MyClass_SVGContextMenu_64))
      {$ELSE}
      if Reg.OpenKey('\*\ShellEx\ContextMenuHandlers\SVGContextMenu32', True) then
        Reg.WriteString('', GUIDToString(MyClass_SVGContextMenu_32))
      {$ENDIF}
    end
    else
    begin
      //Old registration
      if Reg.OpenKey('\*\ShellEx\ContextMenuHandlers\SVGContextMenu', False) then
        Reg.DeleteKey('\*\ShellEx\ContextMenuHandlers\SVGContextMenu');
      //New registration only for .svg files
      {$IFDEF WIN64}
      if Reg.OpenKey('\*\ShellEx\ContextMenuHandlers\SVGContextMenu', True) then
        Reg.DeleteKey('\*\ShellEx\ContextMenuHandlers\SVGContextMenu');
      {$ELSE}
      if Reg.OpenKey('\*\ShellEx\ContextMenuHandlers\SVGContextMenu32', False) then
        Reg.DeleteKey('\*\ShellEx\ContextMenuHandlers\SVGContextMenu32');
      {$ENDIF}
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

initialization
  {$IFDEF WIN64}
  TSVGContextMenuFactory.Create(
    ComServer, TSVGContextMenu, MyClass_SVGContextMenu_64,
    'SVGContextMenu', 'SVGContextMenu Shell Extension',
    ciMultiInstance, tmApartment);
  {$ELSE}
  TSVGContextMenuFactory.Create(
    ComServer, TSVGContextMenu, MyClass_SVGContextMenu_32,
    'SVGContextMenu32', 'SVGContextMenu Shell Extension',
    ciMultiInstance, tmApartment);
  {$ENDIF}

end.
