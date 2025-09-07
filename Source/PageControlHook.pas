{******************************************************************************}
{                                                                              }
{       PageControlHook: Add Close Button to Tab Controls                      }
{                                                                              }
{       Copyright (c) 2025 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
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
unit PageControlHook;

interface

uses
  System.Classes
  , Winapi.Windows
  , Winapi.Messages
  , Winapi.CommCtrl
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.ComCtrls
  , Vcl.Themes
  ;

Type
  TCustomTabControlClass = Class(TCustomTabControl);

  TPageControlCustomDragObject = Class(TDragObjectEx)
  strict private
    FImageList: TImageList;
  protected
    function GetDragImages: TDragImageList; override;
  public
    constructor Create(ABitmap: TBitMap); ReIntroduce;
    destructor Destroy; override;
  end;

  TPageControlEx = Class(TPageControl)
  strict private
    FCloseButtons: Array Of TRect;
    FMouseOverIndex: Integer;
    FCloseIndex: Integer;
    FBeginDrag: TPoint;
    FOnCloseButtonClick: TNotifyEvent;
    FClosing: Boolean;
    procedure CMMouseLeave(var AOutMessage: TMessage); message CM_MOUSELEAVE;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure AngleTextOut(const ACanvas: TCanvas; const AAngle: Integer;
      const AX, AY: Integer; const AText: String);
    procedure DrawControlText(const ACanvas: TCanvas; const ADetails: TThemedElementDetails;
      const AText: String; var AOutRect: TRect; const AFlags: Cardinal);
    function UnthemedButtonState(const ATabIndex: Integer): Cardinal;
    function ThemedButtonState(const ATabIndex: Integer): Cardinal;
  private
    procedure DoDraw(const ADC: HDC; const ADrawTabs: Boolean);
  protected
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DrawTab(const ACanvas: TCanvas; const ATabIndex: Integer;
      const AOnlyCloseButton: Boolean); ReIntroduce;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure CloseTab(const ATabIndex: Integer; const ASetMouseClosing: Boolean = False);
    function TabRect(const AIndex: Integer): TRect;
  published
    Property OnCloseButtonClick: TNotifyEvent Read FOnCloseButtonClick Write FOnCloseButtonClick;
  end;

  TCloseButtonTabStyleHook = Class(TTabControlStyleHook)
  private
    FMouseOverIndex: Integer;
    function GetButtonCloseRect(const ATabIndex: Integer): TRect;
    procedure WMLButtonDown(var AOutMsg: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMMouseMove(var AOutMsg: TMessage); message WM_MOUSEMOVE;
  strict protected
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure DrawTab(Canvas: TCanvas; Index: Integer); override;
  public
    constructor Create(AOwner: TWinControl); override;
    procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
      const S: string; var R: TRect; Flags: Cardinal); {$IF CompilerVersion > 32}Override;{$IFEND}
  end;

implementation

uses
  System.Types
  , System.Math
  , System.SysUtils
  , Vcl.Styles
  , Winapi.UxTheme
  ;

{ TPageControlCustomDragObject }

function TPageControlCustomDragObject.GetDragImages: TDragImageList;
begin
  Result := FImageList;
end;

constructor TPageControlCustomDragObject.Create(ABitmap: TBitMap);
begin
  inherited Create;
  FImageList := TImageList.Create(nil);
  try
    FImageList.Width := ABitmap.Width;
    FImageList.Height := ABitmap.Height;
    FImageList.Add(ABitmap, nil);
    FImageList.SetDragImage(0, 0, 0);
  except
    FImageList.Free;
    raise;
  end;
end;

destructor TPageControlCustomDragObject.Destroy;
begin
  FreeAndNil(FImageList);
  inherited;
end;

{ TPageControlEx }

procedure TPageControlEx.CMMouseLeave(var AOutMessage: TMessage);
begin
  inherited;
  FCloseIndex := -1;
  FMouseOverIndex := -1;
  AOutMessage.Result := 1;
end;

procedure TPageControlEx.WMContextMenu(var Message: TWMContextMenu);
var
  mpos: TPoint;
begin
  mpos := Self.ScreenToClient(Mouse.CursorPos);

  for var I := 0 To Self.PageCount - 1 do
    if Self.Pages[I].TabVisible And PtInRect(Self.TabRect(I), mpos) then
    begin
      Self.ActivePageIndex := I;
      inherited;
      Break;
    end;
end;

procedure TPageControlEx.AngleTextOut(const ACanvas: TCanvas; const AAngle: Integer;
  const AX, AY: Integer; const AText: String);
var
  LLogFont: TLogFont;
begin
  GetObject(ACanvas.Font.Handle, SizeOf(LLogFont), Addr(LLogFont));
  LLogFont.lfEscapement := AAngle * 10;
  LLogFont.lfOrientation := LLogFont.lfEscapement;
  var LNewFont := CreateFontIndirect(LLogFont);
  var LOldFont := SelectObject(ACanvas.Handle, LNewFont);
  SetBkMode(ACanvas.Handle, TRANSPARENT);
  ACanvas.TextOut(AX, AY, AText);
  LNewFont := SelectObject(ACanvas.Handle, LOldFont);
  DeleteObject(LNewFont);
end;

function TPageControlEx.UnthemedButtonState(const ATabIndex: Integer): Cardinal;
begin
  if not Self.Enabled then
    Result := DFCS_INACTIVE
  else if ATabIndex = FMouseOverIndex then
    if FMouseOverIndex = FCloseIndex then
      Result := DFCS_PUSHED
    else
      Result := DFCS_HOT
  else
    Result := 0;
end;

function TPageControlEx.ThemedButtonState(const ATabIndex: Integer): Cardinal;
begin
  if not Self.Enabled then
    Result := CBS_DISABLED
  else if ATabIndex = FMouseOverIndex then
    if FMouseOverIndex = FCloseIndex then
      Result := CBS_PUSHED
    else
      Result := CBS_HOT
  else
    Result := CBS_NORMAL;
end;

procedure TPageControlEx.DoDraw(const ADC: HDC; const ADrawTabs: Boolean);
var
  LRect: TRect;
  LCurrentTab: Integer;
begin
  SetLength(FCloseButtons, Self.PageCount);
  for var I := Low(FCloseButtons) To High(FCloseButtons) do
    FCloseButtons[I] := Rect(0, 0, 0, 0);
  LCurrentTab := Self.TabIndex;
  Try
    Self.Canvas.Handle := ADC;
    if ADrawTabs then
      for var I := 0 To Self.Tabs.Count - 1 do
        if I <> LCurrentTab then
          DrawTab(Self.Canvas, I, False);
    if LCurrentTab < 0 then
      LRect := Rect(0, 0, Self.Width, Self.Height)
    else
    begin
      LRect := TabRect(LCurrentTab);
      LRect.Left := 0;
      LRect.Top := LRect.Bottom;
      LRect.Right := Width;
      LRect.Bottom := Height;
    end;
    StyleServices.DrawElement(ADC, StyleServices.GetElementDetails(ttPane),
      LRect, LRect, CurrentPPI);
    if (LCurrentTab >= 0) And ADrawTabs then
      DrawTab(Self.Canvas, LCurrentTab, False);
  Finally
    Self.Canvas.Handle := 0;
  end;
end;

procedure TPageControlEx.DoStartDrag(var DragObject: TDragObject);
var
  LTab: TRect;
  LBitmap, LTabBitmap: TBitMap;
begin
  inherited;
  if DragObject <> nil then
    Exit;
  // Create a bitmap of LTab button
  LTab := Self.TabRect(Self.ActivePage.TabIndex);
  LBitmap := TBitMap.Create;
  LBitmap.Canvas.Lock;
  LTabBitmap := TBitMap.Create;
  Try
    LBitmap.Height := Self.Height;
    LBitmap.Width := Self.Width;
    LTabBitmap.Height := LTab.Height;
    LTabBitmap.Width := LTab.Width;
    Self.PaintTo(LBitmap.Canvas.Handle, 0, 0);
    LTabBitmap.Canvas.CopyRect(LTabBitmap.Canvas.ClipRect, LBitmap.Canvas, LTab);
    DragObject := TPageControlCustomDragObject.Create(LTabBitmap);
  Finally
    LBitmap.Canvas.Unlock;
    FreeAndNil(LTabBitmap);
    FreeAndNil(LBitmap);
  end;
end;

procedure TPageControlEx.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  inherited;
  Accept := Source is TPageControlCustomDragObject;
end;

procedure TPageControlEx.DrawTab(const ACanvas: TCanvas; const ATabIndex: Integer;
  const AOnlyCloseButton: Boolean);
var
  LIconRect, LRect, LLayoutRect: TRect;
  LImageIndex, LImageWidth, LImageHeight: Integer;
  LOffset, LTextHor, LTextVert: Integer;
  LThemedTab: TThemedTab;
  LHTheme: HTheme;
  LDetails: TThemedElementDetails;
begin
  if not AOnlyCloseButton then
  begin
    if (Self.Images <> nil) And (ATabIndex < Self.Images.Count) then
    begin
      LImageWidth := Images.Width;
      LImageHeight := Images.Height;
      LOffset := 3;
    end
    else
    begin
      LImageWidth := 0;
      LImageHeight := 0;
      LOffset := 0;
    end;
    LRect := TabRect(ATabIndex);
    if LRect.Left < 0 then
      Exit;
    if TabPosition In [tpTop, tpBottom] then
    begin
      if ATabIndex = TabIndex then
        InflateRect(LRect, 0, 2);
    end
    else if ATabIndex = Self.TabIndex then
      Dec(LRect.Left, 2)
    else
      Dec(LRect.Right, 2);
    ACanvas.Font.Assign(Font);
    LLayoutRect := LRect;
    LThemedTab := ttTabDontCare;
    Case Self.TabPosition Of
      tpTop:
        if ATabIndex = Self.TabIndex then
          LThemedTab := ttTabItemSelected
        else
          LThemedTab := ttTabItemNormal;
      tpLeft:
        if ATabIndex = Self.TabIndex then
          LThemedTab := ttTabItemLeftEdgeSelected
        else
          LThemedTab := ttTabItemLeftEdgeNormal;
      tpBottom:
        if ATabIndex = Self.TabIndex then
          LThemedTab := ttTabItemBothEdgeSelected
        else
          LThemedTab := ttTabItemBothEdgeNormal;
      tpRight:
        if ATabIndex = Self.TabIndex then
          LThemedTab := ttTabItemRightEdgeSelected
        else
          LThemedTab := ttTabItemRightEdgeNormal;
    end;
    if StyleServices.Available then
    begin
      LDetails := StyleServices.GetElementDetails(LThemedTab);
      StyleServices.DrawElement(ACanvas.Handle, LDetails, LRect, LRect, CurrentPPI);
    end;
    if Self is TCustomTabControl then
      LImageIndex := TCustomTabControlClass(Self).GetImageIndex(ATabIndex)
    else
      LImageIndex := ATabIndex;
    if (Images <> nil) And (LImageIndex >= 0) And (LImageIndex < Images.Count) then
    begin
      LIconRect := LLayoutRect;

      Case Self.TabPosition Of
        tpTop, tpBottom:
          begin
            LIconRect.Left := LIconRect.Left + LOffset;
            LIconRect.Right := LIconRect.Left + LImageWidth;
            LLayoutRect.Left := LIconRect.Right;
            LIconRect.Top := LIconRect.Top + (LIconRect.Bottom - LIconRect.Top) Div 2 - LImageHeight Div 2;

            if (Self.TabPosition = tpTop) And (ATabIndex = Self.TabIndex) then
              OffsetRect(LIconRect, 0, -1)
            else if (Self.TabPosition = tpBottom) And (ATabIndex = Self.TabIndex) then
              OffsetRect(LIconRect, 0, 1);
          end;
        tpLeft:
          begin
            LIconRect.Bottom := LIconRect.Bottom - LOffset;
            LIconRect.Top := LIconRect.Bottom - LImageHeight;
            LLayoutRect.Bottom := LIconRect.Top;
            LIconRect.Left := LIconRect.Left + (LIconRect.Right - LIconRect.Left) Div 2 - LImageWidth div 2;
          end;
        tpRight:
          begin
            LIconRect.Top := LIconRect.Top + LOffset;
            LIconRect.Bottom := LIconRect.Top + LImageHeight;
            LLayoutRect.Top := LIconRect.Bottom;
            LIconRect.Left := LIconRect.Left + (LIconRect.Right - LIconRect.Left) Div 2 - LImageWidth div 2;
          end;
      end;
      LIconRect.Height := Images.Height;
      LIconRect.Width := Images.Width;
      if StyleServices.Available then
        StyleServices.DrawIcon(ACanvas.Handle, LDetails, LIconRect, Self.Images.Handle, LImageIndex);
    end;
    if StyleServices.Available then
    begin
      Case Self.TabPosition Of
        tpTop, tpBottom:
          begin
            LLayoutRect.Left := LLayoutRect.Left + 5;
            LLayoutRect.Right := LLayoutRect.Right - 20;
          end;
      end;
      Case Self.TabPosition Of
        tpLeft:
          begin
            LTextHor := LLayoutRect.Left + (LLayoutRect.Right - LLayoutRect.Left) Div 2 - ACanvas.TextHeight(Self.Tabs[ATabIndex]) Div 2;
            LTextVert := LLayoutRect.Top + (LLayoutRect.Bottom - LLayoutRect.Top) Div 2 + ACanvas.TextWidth(Self.Tabs[ATabIndex]) Div 2;
            AngleTextOut(ACanvas, 90, LTextHor, LTextVert, Self.Tabs[ATabIndex]);
          end;
        tpRight:
          begin
            LTextHor := LLayoutRect.Left + (LLayoutRect.Right - LLayoutRect.Left) Div 2 + ACanvas.TextHeight(Self.Tabs[ATabIndex]) Div 2;
            LTextVert := LLayoutRect.Top + (LLayoutRect.Bottom - LLayoutRect.Top) Div 2 - ACanvas.TextWidth(Self.Tabs[ATabIndex]) Div 2;
            AngleTextOut(ACanvas, -90, LTextHor, LTextVert, Self.Tabs[ATabIndex]);
          end;
      else
        DrawControlText(ACanvas, LDetails, Self.Tabs[ATabIndex], LLayoutRect, DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
      end;
    end;
    Case Self.TabPosition Of
      tpTop, tpBottom:
        FCloseButtons[ATabIndex].Top := LRect.Top + (LRect.Bottom - LRect.Top) Div 2 - 7;
      tpLeft:
        FCloseButtons[ATabIndex].Top := LRect.Top + 7;
      tpRight:
        FCloseButtons[ATabIndex].Top := LRect.Bottom - 17;
    end;
    FCloseButtons[ATabIndex].Bottom := FCloseButtons[ATabIndex].Top + 14;
    FCloseButtons[ATabIndex].Right := LRect.Right - 4;
    FCloseButtons[ATabIndex].Left := FCloseButtons[ATabIndex].Right - 14;
  end;
  if UseThemes then
  begin
    LHTheme := OpenThemeData(Handle, 'WINDOW');
    if LHTheme <> 0 then
      Try
        DrawThemeBackground(LHTheme, ACanvas.Handle, WP_CLOSEBUTTON, ThemedButtonState(ATabIndex), FCloseButtons[ATabIndex], nil);
      Finally
        CloseThemeData(LHTheme);
      end;
  end
  else
    DrawFrameControl(ACanvas.Handle, FCloseButtons[ATabIndex], DFC_CAPTION, DFCS_CAPTIONCLOSE or UnthemedButtonState(ATabIndex));
end;

procedure TPageControlEx.Loaded;
begin
  inherited;
  Self.ControlStyle := Self.ControlStyle + [csDisplayDragImage];
end;

procedure TPageControlEx.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  if FClosing then
  begin
    FClosing := False;
    Exit;
  end;
  if Button = mbMiddle then
  begin
    I := Self.IndexOfTabAt(X, Y);
    if I > -1 then
    begin
      CloseTab(I);
      FCloseIndex := -1;
      FMouseOverIndex := -1;
      Self.Repaint;
    end;
  end
  else if Button = mbLeft then
  begin
    for I := Low(FCloseButtons) To High(FCloseButtons) do
      if PtInRect(FCloseButtons[I], Point(X, Y)) then
      begin
        FCloseIndex := I;
        Self.DrawTab(Self.Canvas, FCloseIndex, True);
        Break;
      end;
    if FCloseIndex = -1 then
      FBeginDrag := ScreenToClient(Mouse.CursorPos);
  end;
end;

procedure TPageControlEx.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LPos: TPoint;
  LHoverIndex: Integer;
begin
  inherited;
  if (ssLeft In Shift) and (FBeginDrag <> TPoint.Zero) then
  begin
    LPos := ScreenToClient(Mouse.CursorPos);
    if (Abs(LPos.X - FBeginDrag.X) >= Mouse.DragThreshold) or
      (Abs(LPos.Y - FBeginDrag.Y) >= Mouse.DragThreshold) then
    begin
      BeginDrag(True);
      FBeginDrag := TPoint.Zero;
    end;
    Exit;
  end;
  LHoverIndex := -1;
  Try
    if TStyleManager.ActiveStyle <> TStyleManager.SystemStyle then
      Exit;
    if FCloseIndex = -1 then
    begin
      LHoverIndex := FMouseOverIndex;
      FMouseOverIndex := -1;
      for var I := Low(FCloseButtons) To High(FCloseButtons) do
        if PtInRect(FCloseButtons[I], Point(X, Y)) then
        begin
          FMouseOverIndex := I;
          Break;
        end;
    end;
    if not (ssLeft In Shift) or (FCloseIndex = -1) then
      Exit;
    if not PtInRect(FCloseButtons[FCloseIndex], Point(X, Y)) then
      FCloseIndex := -1;
  Finally
    if FMouseOverIndex > -1 then
      Self.DrawTab(Self.Canvas, FMouseOverIndex, True);

    if (LHoverIndex > -1) And (LHoverIndex <> FMouseOverIndex) then
      Self.DrawTab(Self.Canvas, LHoverIndex, True);
  end;
end;

procedure TPageControlEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FBeginDrag := TPoint.Zero;
  if (TStyleManager.ActiveStyle <> TStyleManager.SystemStyle) or (Button <> mbLeft) or (FCloseIndex = -1) then
    Exit;
  if PtInRect(FCloseButtons[FCloseIndex], Point(X, Y)) then
  begin
    CloseTab(FCloseIndex);
    FCloseIndex := -1;
    FMouseOverIndex := -1;
  end;
end;

procedure TPageControlEx.PaintWindow(DC: HDC);
begin
  DoDraw(DC, True);
end;

constructor TPageControlEx.Create(AOwner: TComponent);
begin
  inherited;
  FClosing := False;
  FBeginDrag := TPoint.Zero;
  FOnCloseButtonClick := nil;
end;

procedure TPageControlEx.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited;
  for var I := 0 To PageCount - 1 do
  begin
    if not Self.Pages[I].TabVisible then
      Continue;
    var LRect := TabRect(Self.Pages[I].TabIndex);

    if PtInRect(LRect, Point(X, Y)) then
    begin
      if I <> Self.ActivePage.PageIndex then
        Self.ActivePage.PageIndex := I;
      Break;
    end;
  end;
end;

procedure TPageControlEx.CloseTab(const ATabIndex: Integer;
  const ASetMouseClosing: Boolean = False);
begin
  if ASetMouseClosing then
    FClosing := True;
  for var I := 0 To Self.PageCount - 1 do
    if Self.Pages[I].TabIndex = ATabIndex then
    begin
      if Assigned(FOnCloseButtonClick) then
        FOnCloseButtonClick(Self.Pages[I])
      else
        Self.Pages[I].Free;
      Break;
    end;
end;

function TPageControlEx.TabRect(const AIndex: Integer): TRect;
begin
  TabCtrl_GetItemRect(Handle, AIndex, Result);
end;

procedure TPageControlEx.DrawControlText(const ACanvas: TCanvas; const ADetails: TThemedElementDetails;
  const AText: String; var AOutRect: TRect; const AFlags: Cardinal);
begin
  ACanvas.Font := Self.Font;
  var LTextFormat := TTextFormatFlags(AFlags);
  StyleServices.DrawText(ACanvas.Handle, ADetails, AText, AOutRect, LTextFormat, ACanvas.Font.Color);
end;

{ TCloseButtonTabStyleHook }

function TCloseButtonTabStyleHook.GetButtonCloseRect(const ATabIndex: Integer): TRect;
var
  LRect: TRect;
  LMargin: Integer;
begin
  LRect := TabRect[ATabIndex];
  if LRect.Left >= 0 then
  begin
    LMargin := Round(10 * Control.ScaleFactor);
    if Self.TabPosition In [tpTop, tpBottom] then
    begin
      if ATabIndex = TabIndex then
        InflateRect(LRect, 0, LMargin);
    end
    else if ATabIndex = Self.TabIndex then
      Dec(LRect.Left, LMargin)
    else
      Dec(LRect.Right, LMargin);

    Result := LRect;

    if not StyleServices.GetElementContentRect(
      0, StyleServices.GetElementDetails(twSmallCloseButtonNormal), Result, LRect) then
      LRect := Rect(0, 0, 0, 0);
    Result.Left := Result.Right - (LRect.Width) - LMargin;
  end
  else
    Result := Rect(0,0,0,0);
end;

procedure TCloseButtonTabStyleHook.WMLButtonDown(var AOutMsg: TWMMouse);
begin
  inherited;
  if not (Control is TPageControlEx) then
    Exit;
  for var I := 0 To Self.TabCount - 1 do
  begin
    if PtInRect(GetButtonCloseRect(I), AOutMsg.Pos) then
    begin
      (Control As TPageControlEx).CloseTab(I, True);
      AOutMsg.Result := 1;
      Break;
    end;
  end;
end;

procedure TCloseButtonTabStyleHook.WMMouseMove(var AOutMsg: TMessage);
begin
  inherited;
  if not (Control is TPageControlEx) then
    Exit;
  var LMouseOverIndex := -1;
  for var I := 0 To Self.TabCount - 1 do
  begin
    if PtInRect(GetButtonCloseRect(I), TWMMouseMove(AOutMsg).Pos) then
    begin
      LMouseOverIndex := I;
      Break;
    end;
  end;
  if FMouseOverIndex <> LMouseOverIndex then
  begin
    FMouseOverIndex := LMouseOverIndex;
    Self.Invalidate;
  end;
  if FMouseOverIndex <> -1 then
    Control.Cursor := crHandPoint
  else
    Control.Cursor := crDefault;
end;

procedure TCloseButtonTabStyleHook.MouseEnter;
begin
  inherited;
  FMouseOverIndex := -1;
end;

procedure TCloseButtonTabStyleHook.MouseLeave;
begin
  inherited;
  Control.Cursor := crDefault;
  if FMouseOverIndex >= 0 then
  begin
    FMouseOverIndex := -1;
    Self.Invalidate;
  end;
end;

procedure TCloseButtonTabStyleHook.DrawTab(Canvas: TCanvas; Index: Integer);
var
  LDetails: TThemedElementDetails;
  LRect: TRect;
begin
  inherited;
  if not (Control is TPageControlEx) then
    Exit;
  if (FMouseOverIndex >= 0) And (Index = FMouseOverIndex) then
    LDetails := StyleServices.GetElementDetails(twSmallCloseButtonHot)
  else if Index = TabIndex then
    LDetails := StyleServices.GetElementDetails(twSmallCloseButtonNormal)
  else
    LDetails := StyleServices.GetElementDetails(twSmallCloseButtonDisabled);
  LRect := GetButtonCloseRect(Index);
  if LRect.Bottom - LRect.Top > 0 then
    StyleServices.DrawElement(Canvas.Handle, LDetails, LRect, LRect, Control.CurrentPPI);
end;

constructor TCloseButtonTabStyleHook.Create(AOwner: TWinControl);
begin
  inherited;
  FMouseOverIndex := -1;
end;

procedure TCloseButtonTabStyleHook.DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails; const S: String; var R: TRect; Flags: Cardinal);
var
  LNewFlags: Cardinal;
begin
  LNewFlags := Flags;
  if Control is TPageControlEx then
  begin
    if R.Left = 0 then
      Exit;
    if Self.TabPosition In [tpTop, tpBottom] then
      R.Right := R.Right - GetButtonCloseRect(0).Width;
    {$IF CompilerVersion > 35}
    // Restore the old functionality because Delphi 12.2 centers the text by default
    if LNewFlags And DT_CENTER <> 0 then
      LNewFlags := LNewFlags - DT_CENTER;
    {$ENDIF}
    if LNewFlags And DT_WORDBREAK <> 0 then
      LNewFlags := LNewFlags - DT_WORDBREAK;
  end;
  inherited DrawControlText(Canvas, Details, S, R, LNewFlags);
end;

Initialization
  TStyleManager.Engine.RegisterStyleHook(TTabControl, TCloseButtonTabStyleHook);
  TStyleManager.Engine.RegisterStyleHook(TCustomTabControl, TCloseButtonTabStyleHook);

end.
