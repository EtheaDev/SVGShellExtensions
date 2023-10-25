//**************************************************************************************************
//
// Unit uPreviewHandler
// unit for the Delphi Preview Handler https://github.com/RRUZ/delphi-preview-handler
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uPreviewHandler.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2021 Rodrigo Ruz V.
// All Rights Reserved.
//
//*************************************************************************************************


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
  System.Generics.Collections,
  ActiveX,
  PreviewForm;


type
  TPreviewHandler = class abstract
  public
    constructor Create(AParent: TWinControl); virtual;
    class function GetComClass: TComClass; virtual; abstract;
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
    FCurrentPPI: Integer;
    FBackgroundColor: TColorRef;
    FBounds: TRect;
    FContainer: TFrmPreview;
    FLogFont: TLogFont;
    FParentWindow: HWND;
    FPreviewHandler: TPreviewHandler;
    FPreviewHandlerClass: TPreviewHandlerClass;
    FPreviewHandlerFrame: IPreviewHandlerFrame;
    FSite: IInterface;
    FTextColor: TColorRef;
    procedure SetBounds(const Value: TRect);
    procedure UpdateContainerBoundsRect;
  protected
    function CheckContainer: Boolean;
    procedure CheckPreviewHandler;
    procedure InternalUnload; virtual; abstract;
    procedure InternalDoPreview; virtual; abstract;
    property  BackgroundColor: TColorRef read FBackgroundColor write FBackgroundColor;
    property  TextColor: TColorRef read FTextColor write FTextColor;
    property  Bounds: TRect read FBounds write SetBounds;
    property  LogFont: TLogFont read FLogFont  write FLogFont;
    property  ParentWindow: HWND read FParentWindow write FParentWindow;
    property  PreviewHandler: TPreviewHandler read FPreviewHandler;
    property  PreviewHandlerFrame: IPreviewHandlerFrame read FPreviewHandlerFrame;
    property  Site: IInterface read FSite;
  public
    destructor Destroy; override;
    property  Container: TFrmPreview read FContainer write FContainer;
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
    uPreviewHandlerRegister;


destructor TComPreviewHandler.Destroy;
begin
  TLogPreview.Add('Destroy Init');
  inherited Destroy;
  FreeAndNil(FContainer);
  FreeAndNil(FPreviewHandler);
  TLogPreview.Add('Destroy Done');
end;

procedure TComPreviewHandler.UpdateContainerBoundsRect;
var
  LNewPPI: Integer;
begin
  if (Container <> nil) then
  begin
    LNewPPI := GetDpiForWindow(FParentWindow);
    Container.SetBoundsRectAndPPI(
      FBounds,
      FCurrentPPI,
      LNewPPI
      );
  end;
end;

function TComPreviewHandler.CheckContainer: Boolean;
var
  LRect: TRect;
begin
  TLogPreview.Add('CheckContainer Init');
  if (FContainer = nil) and IsWindow(FParentWindow) then
  begin
    TLogPreview.Add('ParentWindow '+IntToHex(FParentWindow, 8));

    GetWindowRect(FParentWindow, LRect);
    TLogPreview.Add('CheckContainer'+GetRect(LRect,' - GetWindowRect'));

    FContainer := TFrmPreview.Create(nil);

    TLogPreview.Add('FContainer created:'+GetRect(FBounds,' FBounds'));
    FContainer.ParentWindow := FParentWindow;
    FContainer.BorderStyle := bsNone;
    UpdateContainerBoundsRect;
    FContainer.PreviewHandler := Self;
    TFrmPreview.AParent := FContainer;
  end;
  TLogPreview.Add('CheckContainer Done');
  Result := Assigned(FContainer);
end;

procedure TComPreviewHandler.CheckPreviewHandler;
begin
  TLogPreview.Add('CheckPreviewHandler Init');
  if CheckContainer then
  begin
    if FPreviewHandler = nil then
      FPreviewHandler := PreviewHandlerClass.Create(Container);
  end;
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
var
  LRect: TRect;
begin
  TLogPreview.Add('GetWindow Init');
  if (Container = nil) or (Container.Handle = 0) then
  begin
    wnd := 0;
    result := E_FAIL;
  end
  else
  begin
    wnd := Container.Handle;
    GetWindowRect(wnd, LRect);
    TLogPreview.Add('GetWindow'+GetRect(LRect,' - GetWindowRect'));

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
      TLogPreview.Add(Format('Error in TComPreviewHandler.IPreviewHandler_DoPreview - Message: %s: Trace %s',
        [E.Message, E.StackTrace]));
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

procedure TComPreviewHandler.SetBounds(const Value: TRect);
begin
  if Value = FBounds then
    Exit;
  TLogPreview.Add('TComPreviewHandler.SetBounds -'+
    ' Width: '+Value.Width.ToString+
    ' Height: '+Value.Height.ToString);
  if (Value.Width <> 0) and (Value.Height <> 0) then
  begin
    FBounds := Value;
    FBounds.Left := 0;
    FBounds.Top := 0;
    //CheckPreviewHandler;
    UpdateContainerBoundsRect;
  end;
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
var
  LNewPPI: Integer;
  LRect: TRect;
begin
  LNewPPI := GetDpiForWindow(FParentWindow);

  TLogPreview.Add('SetRect Init:'+
    ' prc.Width: '+prc.Width.ToString+
    ' prc.Height: '+prc.Height.ToString+
    ' GetDpiForWindow: '+LNewPPI.ToString);

  GetWindowRect(FParentWindow, LRect);
  TLogPreview.Add('SetRect'+GetRect(LRect,' - GetWindowRect'));

  //Bounds is calculated on Windows Rect
  TLogPreview.Add('SetRect: Imposto Bounds dalla Window'+GetRect(LRect, ' LRect'));
  Bounds := TRect.Create(0,0,LRect.Width,LRect.Height);
  FCurrentPPI := LNewPPI;
  TLogPreview.Add('FCurrentPPI := LNewPPI: '+LNewPPI.ToString);
  Result := S_OK;
  TLogPreview.Add('SetRect Done');
end;

function TComPreviewHandler.SetSite(const pUnkSite: IInterface): HRESULT;
begin
  TLogPreview.Add('SetSite Init');
  FSite := PUnkSite;
  FPreviewHandlerFrame := FSite as IPreviewHandlerFrame;
  FBounds := TRect.Create(0,0,0,0);
  result := S_OK;
  TLogPreview.Add('SetSite Done');
end;

function TComPreviewHandler.SetTextColor(color: Cardinal): HRESULT;
begin
  TLogPreview.Add('SetTextColor Init - Color:'+Color.ToString);
  FTextColor := color;
  if Container <> nil then
    Container.SetTextColor(FTextColor);
  Result := S_OK;
  TLogPreview.Add('SetTextColor Done');
end;

function TComPreviewHandler.SetWindow(hwnd: HWND; var prc: TRect): HRESULT;
var
  LMonitor: TMonitor;
  LRect: TRect;
begin
  TLogPreview.Add('SetWindow Init'+GetRect(prc, '-prc:'));
  FParentWindow := hwnd;
//  FCurrentPPI := 96;
  FCurrentPPI := GetDpiForWindow(hwnd);

  GetWindowRect(FParentWindow, LRect);
  TLogPreview.Add('SetWindow'+GetRect(LRect,' - GetWindowRect'));

  TLogPreview.Add('SetWindow: Window DPI: '+FCurrentPPI.ToString);

  LMonitor := Screen.MonitorFromWindow(hwnd);
//  FCurrentPPI := LMonitor.PixelsPerInch;
  TLogPreview.Add('SetWindow: Monitor - '+
  ' Width: '+LMonitor.Width.ToString+
  ' Height: '+LMonitor.Height.ToString+
  ' PPI: '+LMonitor.PixelsPerInch.ToString);

  TLogPreview.Add('SetWindow: Imposto Bounds'+GetRect(LRect, ' LRect'));
  Bounds := TRect.Create(0,0,LRect.Width,LRect.Height);

  Result := S_OK;
(*
  if (prc.Width <> 0) and (prc.Height <> 0) then
  begin
    Bounds := prc;
    Result := S_OK;
  end
  else
  begin
    FCurrentPPI := LMonitor.PixelsPerInch;
    LRect.Width := 320;
    LRect.Height := 240;
    Bounds := LRect;
    Result := S_OK;
  end;
*)
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
  TLogPreview.Add('TComPreviewHandler Unload Init');
  if PreviewHandler <> nil then
    PreviewHandler.Unload;
  InternalUnload;
  result := S_OK;
  TLogPreview.Add('TComPreviewHandler Unload Done');
end;

constructor TPreviewHandler.Create(AParent: TWinControl);
begin
  inherited Create;
end;

class procedure TPreviewHandler.RegisterPreview(const AClassID: TGUID;
  const AName, ADescription: string);
begin
  TLogPreview.Add('RegisterPreview Init ' + AName);
  TPreviewHandlerRegister.Create(Self, AClassID, AName, ADescription);
  TLogPreview.Add('RegisterPreview Done ' + AName);
end;

procedure TPreviewHandler.Unload;
begin
end;

end.


