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
unit uPreviewHandler;


interface

uses
  Forms,
  Classes,
  Controls,
  ComObj,
  ShlObj,
  Windows,
  uPreviewContainer,
  SynEditHighlighter,
  System.Generics.Collections,
  ActiveX;


type
  TPreviewHandler = class abstract
  public
    class var FExtensions: TDictionary<TSynCustomHighlighterClass, TStrings>;
    constructor Create(AParent: TWinControl); virtual;
    class function GetComClass: TComClass; virtual; abstract;
    class procedure AddExtentions(ClassType: TSynCustomHighlighterClass; Extensions: array of string);
    class procedure RegisterPreview(const AClassID: TGUID; const AName, ADescription: string);
    procedure Unload; virtual;
  end;


  TPreviewHandlerClass = class of TPreviewHandler;

  TComPreviewHandler = class(TComObject, IOleWindow, IObjectWithSite, IPreviewHandler, IPreviewHandlerVisuals)
 // strict private
    function IPreviewHandler.DoPreview = IPreviewHandler_DoPreview;
    function ContextSensitiveHelp(fEnterMode: LongBool): HRESULT; stdcall;
    function GetSite(const riid: TGUID; out site: IInterface): HRESULT; stdcall;
    function GetWindow(out wnd: HWND): HRESULT; stdcall;
    function IPreviewHandler_DoPreview: HRESULT; stdcall;
    function QueryFocus(var phwnd: HWND): HRESULT; stdcall;
    function SetBackgroundColor(color: TColorRef): HRESULT; stdcall;
    function SetFocus: HRESULT; stdcall;
    function SetFont(const plf: TLogFont): HRESULT; stdcall;
    function SetRect(var prc: TRect): HRESULT; stdcall;
    function SetSite(const pUnkSite: IInterface): HRESULT; stdcall;
    function SetTextColor(color: TColorRef): HRESULT; stdcall;
    function SetWindow(hwnd: HWND; var prc: TRect): HRESULT; stdcall;
    function TranslateAccelerator(var pmsg: tagMSG): HRESULT; stdcall;
    function Unload: HRESULT; stdcall;
  private
    FBackgroundColor: TColorRef;
    FBounds: TRect;
    FContainer: TPreviewContainer;
    FLogFont: TLogFont;
    FParentWindow: HWND;
    FPreviewHandler: TPreviewHandler;
    FPreviewHandlerClass: TPreviewHandlerClass;
    FPreviewHandlerFrame: IPreviewHandlerFrame;
    FSite: IInterface;
    FTextColor: TColorRef;
  protected
    procedure CheckContainer;
    procedure CheckPreviewHandler;
    procedure InternalUnload; virtual; abstract;
    procedure InternalDoPreview; virtual; abstract;
    property  BackgroundColor: TColorRef read FBackgroundColor write FBackgroundColor;
    property  TextColor: TColorRef read FTextColor write FTextColor;
    property  Bounds: TRect read FBounds write FBounds;
    property  LogFont: TLogFont read FLogFont  write FLogFont;
    property  ParentWindow: HWND read FParentWindow write FParentWindow;
    property  PreviewHandler: TPreviewHandler read FPreviewHandler;
    property  PreviewHandlerFrame: IPreviewHandlerFrame read FPreviewHandlerFrame;
    property  Site: IInterface read FSite;
  public
    destructor Destroy; override;
    property  Container: TPreviewContainer read FContainer write FContainer;
    property PreviewHandlerClass: TPreviewHandlerClass read FPreviewHandlerClass write FPreviewHandlerClass;
  end;


implementation

uses
    ComServ,
    Types,
    SysUtils,
    Graphics,
    ExtCtrls,
    uMisc,
    uLogExcept,
    uPreviewHandlerRegister,
    uEditor;


destructor TComPreviewHandler.Destroy;
begin
  TLogPreview.Add('Destroy Init');
  FPreviewHandler.Free;
  if FContainer <> nil then
   FContainer.Free;
  inherited Destroy;
  TLogPreview.Add('Destroy Done');
end;

procedure TComPreviewHandler.CheckContainer;
begin
  TLogPreview.Add('CheckContainer Init');
  TLogPreview.Add('CheckContainer FContainer = nil '+BoolToStr(FContainer = nil, True));
  if (FContainer = nil) and IsWindow(FParentWindow) then
  begin
    TLogPreview.Add('ParentWindow '+IntToHex(ParentWindow, 8));
    FContainer := TPreviewContainer.Create(nil);
    FContainer.ParentWindow := FParentWindow;
    FContainer.BorderStyle := bsNone;
    FContainer.Visible:=True;
    FContainer.SetBoundsRect(FBounds);
    FContainer.Preview:=Self;

    TFrmEditor.AParent := FContainer;
  end;
  TLogPreview.Add('CheckContainer Done');
end;

procedure TComPreviewHandler.CheckPreviewHandler;
begin
  TLogPreview.Add('CheckPreviewHandler Init');
  if FContainer = nil then
    CheckContainer;

  if FPreviewHandler = nil then
    FPreviewHandler := PreviewHandlerClass.Create(Container);

  TLogPreview.Add('CheckPreviewHandler Done');
end;

function TComPreviewHandler.ContextSensitiveHelp(fEnterMode: LongBool): HRESULT;
begin
  result := E_NOTIMPL;
end;

function TComPreviewHandler.GetSite(const riid: TGUID; out site: IInterface): HRESULT;
begin
  TLogPreview.Add('GetSite Init');
  site := nil;
  if FSite = nil then
    result := E_FAIL
  else
  if Supports(FSite, riid, site) then
    result := S_OK
  else
    result := E_NOINTERFACE;
  TLogPreview.Add('GetSite Done');
end;

function TComPreviewHandler.GetWindow(out wnd: HWND): HRESULT;
begin
  TLogPreview.Add('GetWindow Init');
  if Container = nil then
  begin
    result := E_FAIL;
  end
  else
  begin
    wnd := Container.Handle;
    result := S_OK;
  end;
  TLogPreview.Add('GetWindow Done');
end;

function TComPreviewHandler.IPreviewHandler_DoPreview: HRESULT;
begin
  TLogPreview.Add('IPreviewHandler_DoPreview Init');
  try
    CheckPreviewHandler;
    InternalDoPreview;
  except
    on E: Exception do
      TLogPreview.Add(Format('Error in TComPreviewHandler.IPreviewHandler_DoPreview - Message: %s: Trace %s', [E.Message, E.StackTrace]));
  end;
  Result := S_OK;
  TLogPreview.Add('IPreviewHandler_DoPreview Done');
end;

function TComPreviewHandler.QueryFocus(var phwnd: HWND): HRESULT;
begin
  TLogPreview.Add('QueryFocus Init');
  phwnd := GetFocus;
  Result := S_OK;
  TLogPreview.Add('QueryFocus Done');
end;

function TComPreviewHandler.SetBackgroundColor(color: Cardinal): HRESULT;
begin
  TLogPreview.Add('SetBackgroundColor Init');
  FBackgroundColor := color;
  if Container <> nil then
    Container.SetBackgroundColor(FBackgroundColor);
  Result := S_OK;
  TLogPreview.Add('SetBackgroundColor Done');
end;

function TComPreviewHandler.SetFocus: HRESULT;
begin
  TLogPreview.Add('SetFocus Init');
  if Container <> nil then
  begin
    if GetKeyState(VK_SHIFT) < 0 then
      Container.SetFocusTabLast
    else
      Container.SetFocusTabFirst;
  end;
  Result := S_OK;
  TLogPreview.Add('SetFocus Done');
end;

function TComPreviewHandler.SetFont(const plf: TLogFont): HRESULT;
begin
  TLogPreview.Add('SetFont Init');
  FLogFont := plf;
  if Container <> nil then
    Container.SetTextFont(FLogFont);
  Result := S_OK;
  TLogPreview.Add('SetFont Done');
end;

function TComPreviewHandler.SetRect(var prc: TRect): HRESULT;
begin
  TLogPreview.Add('SetRect Init');
  FBounds := prc;
  if Container <> nil then
    Container.SetBoundsRect(FBounds);
  Result := S_OK;
  TLogPreview.Add('SetRect Done');
end;

function TComPreviewHandler.SetSite(const pUnkSite: IInterface): HRESULT;
begin
  TLogPreview.Add('SetSite Init');
  FSite := PUnkSite;
  FPreviewHandlerFrame := FSite as IPreviewHandlerFrame;
  result := S_OK;
  TLogPreview.Add('SetSite Done');
end;

function TComPreviewHandler.SetTextColor(color: Cardinal): HRESULT;
begin
  TLogPreview.Add('SetTextColor Init');
  FTextColor := color;
  if Container <> nil then
    Container.SetTextColor(FTextColor);
  Result := S_OK;
  TLogPreview.Add('SetTextColor Done');
end;

function TComPreviewHandler.SetWindow(hwnd: HWND; var prc: TRect): HRESULT;
begin
  TLogPreview.Add('SetWindow Init');
  FParentWindow := hwnd;
  FBounds := prc;
  if Container <> nil then
  begin
    Container.ParentWindow := FParentWindow;
    Container.SetBoundsRect(FBounds);
    TLogPreview.Add('SetWindow Ok');
  end;
  Result := S_OK;
  TLogPreview.Add('SetWindow Done');
end;

function TComPreviewHandler.TranslateAccelerator(var pmsg: tagMSG): HRESULT;
begin
  if FPreviewHandlerFrame = nil then
    result := S_FALSE
  else
    result := FPreviewHandlerFrame.TranslateAccelerator(pmsg);
end;

function TComPreviewHandler.Unload: HRESULT;
begin
  TLogPreview.Add('Unload Init');
  if PreviewHandler <> nil then
    PreviewHandler.Unload;
  InternalUnload;
  result := S_OK;
  TLogPreview.Add('Unload Done');
end;

class procedure TPreviewHandler.AddExtentions(ClassType: TSynCustomHighlighterClass; Extensions: array of string);
var
  i: integer;
begin
  if not FExtensions.ContainsKey(ClassType) then
    FExtensions.Add(ClassType, TStringList.Create);

   for i:=0 to Length(Extensions)-1 do
     FExtensions.Items[ClassType].Add(LowerCase(Extensions[i]));
end;

constructor TPreviewHandler.Create(AParent: TWinControl);
begin
  inherited Create;
end;

class procedure TPreviewHandler.RegisterPreview(const AClassID: TGUID;
  const AName, ADescription: string);
var
  Extensions: array of string;
  LItem: TPair<TSynCustomHighlighterClass, TStrings>;
  i, c, j: integer;
begin
  TLogPreview.Add('RegisterPreview Init ' + AName);
  c:=0;
  for LItem in FExtensions do
    Inc(c, LItem.Value.Count);

  TLogPreview.Add('RegisterPreview count ' + IntToStr(c));
  SetLength(Extensions, c);

  j:=0;
  for LItem in FExtensions do
   for i := 0 to   LItem.Value.Count-1 do
   begin
     Extensions[j] := LItem.Value[i];
     Inc(j);
   end;

  TPreviewHandlerRegister.Create(Self, AClassID, AName, ADescription, Extensions);

  TLogPreview.Add('RegisterPreview Done ' + AName);
end;

procedure TPreviewHandler.Unload;
begin
end;

end.


