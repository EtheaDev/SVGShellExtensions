{******************************************************************************}
{                                                                              }
{       SVG Shell Extensions: Shell extensions for SVG files                   }
{       (Preview Panel, Thumbnail Icon, SVG Editor)                            }
{                                                                              }
{       Copyright (c) 2021-2026 (Ethea S.r.l.)                                 }
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
{******************************************************************************}
unit uSVGPaletteBar;

//Owner-draw bar showing the colors of an SVG document as swatches, in two
//sections: the colors used by the document (with the number of occurrences)
//and the custom colors added by the user (followed by a "+" button).
//The control only shows the items it is given and reports the user actions:
//it knows nothing about SVG nor about the editor. It is created at runtime,
//so it does not need to be installed in the IDE.

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms;

type
  TSVGPaletteSection = (psDocument, psCustom);

  TSVGPaletteItem = record
    Color: TAlphaColor;
    Count: Integer;     //drawn inside the swatch when > 0
    Hint: string;
  end;
  TSVGPaletteItems = TArray<TSVGPaletteItem>;

  //What is under the mouse: a swatch, the "+" button or nothing
  TSVGPaletteHit = record
    Section: TSVGPaletteSection;
    Index: Integer;     //-1 = no swatch
    IsAddButton: Boolean;
    function IsSwatch: Boolean;
    function SameAs(const AOther: TSVGPaletteHit): Boolean;
    class function None: TSVGPaletteHit; static;
  end;

  TSVGSwatchEvent = procedure(Sender: TObject;
    ASection: TSVGPaletteSection; AIndex: Integer) of object;

  TSVGPaletteBar = class(TCustomControl)
  private
    FItems: array[TSVGPaletteSection] of TSVGPaletteItems;
    FRects: array[TSVGPaletteSection] of TArray<TRect>;
    FCaptionRects: array[TSVGPaletteSection] of TRect;
    FAddRect: TRect;
    FContentHeight: Integer;
    FScrollPos: Integer;
    FHot: TSVGPaletteHit;
    FSelected: TSVGPaletteHit;
    FCaretIndex: Integer;
    FMouseDownHit: TSVGPaletteHit;
    FSuppressClick: Boolean;
    FShowCustomSection: Boolean;
    FDocumentCaption: string;
    FCustomCaption: string;
    FEmptyText: string;
    FAddHint: string;
    FOnSwatchClick: TSVGSwatchEvent;
    FOnSwatchDblClick: TSVGSwatchEvent;
    FOnAddClick: TNotifyEvent;
    function SwatchSize: Integer;
    function Gap: Integer;
    function CornerRadius: Integer;
    procedure UpdateLayout;
    procedure UpdateScrollBar;
    procedure SetScrollPos(AValue: Integer);
    procedure InvalidateHit(const AHit: TSVGPaletteHit);
    function HitRect(const AHit: TSVGPaletteHit): TRect;
    procedure SetShowCustomSection(const AValue: Boolean);
    procedure SetCaretIndex(const AValue: Integer);
    procedure SetCaption(var AField: string; const AValue: string);
    procedure SetDocumentCaption(const AValue: string);
    procedure SetCustomCaption(const AValue: string);
    procedure SetEmptyText(const AValue: string);
    procedure DrawSwatch(const ARect: TRect; const AItem: TSVGPaletteItem;
      const AHot, ASelected, AIsCaret: Boolean);
    procedure DrawAddButton(const ARect: TRect; const AHot: Boolean);
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetItems(const ASection: TSVGPaletteSection;
      const AItems: TSVGPaletteItems);
    function Items(const ASection: TSVGPaletteSection): TSVGPaletteItems;
    function HitTest(const X, Y: Integer): TSVGPaletteHit;
    //Selects a swatch (AIndex = -1 clears the selection)
    procedure Select(const ASection: TSVGPaletteSection; const AIndex: Integer);
    procedure ScrollIntoView(const ASection: TSVGPaletteSection; const AIndex: Integer);
    property Selected: TSVGPaletteHit read FSelected;
    //Document swatch of the color under the editor caret (-1 = none)
    property CaretIndex: Integer read FCaretIndex write SetCaretIndex;
    property ShowCustomSection: Boolean read FShowCustomSection write SetShowCustomSection;
    property DocumentCaption: string read FDocumentCaption write SetDocumentCaption;
    property CustomCaption: string read FCustomCaption write SetCustomCaption;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property AddHint: string read FAddHint write FAddHint;
    property OnSwatchClick: TSVGSwatchEvent read FOnSwatchClick write FOnSwatchClick;
    property OnSwatchDblClick: TSVGSwatchEvent read FOnSwatchDblClick write FOnSwatchDblClick;
    property OnAddClick: TNotifyEvent read FOnAddClick write FOnAddClick;
    property Align;
    property Font;
    property ParentFont;
    property PopupMenu;
    property OnContextPopup;
  end;

implementation

uses
  System.Math,
  Vcl.Themes;

{ TSVGPaletteHit }

function TSVGPaletteHit.IsSwatch: Boolean;
begin
  Result := (Index >= 0) and not IsAddButton;
end;

function TSVGPaletteHit.SameAs(const AOther: TSVGPaletteHit): Boolean;
begin
  Result := (IsAddButton = AOther.IsAddButton) and (Index = AOther.Index) and
    ((Index < 0) or (Section = AOther.Section));
end;

class function TSVGPaletteHit.None: TSVGPaletteHit;
begin
  Result.Section := psDocument;
  Result.Index := -1;
  Result.IsAddButton := False;
end;

{ TSVGPaletteBar }

constructor TSVGPaletteBar.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks] - [csAcceptsControls];
  DoubleBuffered := True;
  ShowHint := True;
  TabStop := False;
  FHot := TSVGPaletteHit.None;
  FSelected := TSVGPaletteHit.None;
  FMouseDownHit := TSVGPaletteHit.None;
  FCaretIndex := -1;
  FShowCustomSection := True;
  FDocumentCaption := 'Document';
  FCustomCaption := 'Custom';
  FEmptyText := 'No colors';
  FAddHint := 'Add color...';
  Width := 200;
  Height := 60;
end;

procedure TSVGPaletteBar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_VSCROLL;
end;

function TSVGPaletteBar.SwatchSize: Integer;
begin
  Result := ScaleValue(24);
end;

function TSVGPaletteBar.Gap: Integer;
begin
  Result := ScaleValue(6);
end;

function TSVGPaletteBar.CornerRadius: Integer;
begin
  Result := ScaleValue(4);
end;

procedure TSVGPaletteBar.SetItems(const ASection: TSVGPaletteSection;
  const AItems: TSVGPaletteItems);
begin
  FItems[ASection] := Copy(AItems);
  if (FSelected.Section = ASection) and (FSelected.Index > High(AItems)) then
    FSelected := TSVGPaletteHit.None;
  if (ASection = psDocument) and (FCaretIndex > High(AItems)) then
    FCaretIndex := -1;
  FHot := TSVGPaletteHit.None;
  UpdateLayout;
  Invalidate;
end;

function TSVGPaletteBar.Items(const ASection: TSVGPaletteSection): TSVGPaletteItems;
begin
  Result := FItems[ASection];
end;

procedure TSVGPaletteBar.UpdateLayout;
var
  LMargin, LSize, LGap, LCaptionWidth, LLeft, LRight, X, Y: Integer;
  LSection: TSVGPaletteSection;
  I: Integer;

  procedure NextPosition;
  begin
    Inc(X, LSize + LGap);
    if X + LSize > LRight then
    begin
      X := LLeft;
      Inc(Y, LSize + LGap);
    end;
  end;

begin
  if not HandleAllocated then
    Exit;
  LMargin := ScaleValue(4);
  LSize := SwatchSize;
  LGap := Gap;
  Canvas.Font := Font;
  LCaptionWidth := Canvas.TextWidth(FDocumentCaption);
  if FShowCustomSection then
    LCaptionWidth := Max(LCaptionWidth, Canvas.TextWidth(FCustomCaption));
  LLeft := LMargin + LCaptionWidth + LGap * 2;
  LRight := Max(ClientWidth - LMargin, LLeft + LSize);
  Y := LMargin;
  for LSection := Low(TSVGPaletteSection) to High(TSVGPaletteSection) do
  begin
    FCaptionRects[LSection] := TRect.Empty;
    SetLength(FRects[LSection], Length(FItems[LSection]));
    if (LSection = psCustom) and not FShowCustomSection then
    begin
      FAddRect := TRect.Empty;
      Continue;
    end;
    FCaptionRects[LSection] := Rect(LMargin, Y, LLeft - LGap, Y + LSize);
    X := LLeft;
    for I := 0 to High(FItems[LSection]) do
    begin
      FRects[LSection][I] := Rect(X, Y, X + LSize, Y + LSize);
      NextPosition;
    end;
    if LSection = psCustom then
    begin
      FAddRect := Rect(X, Y, X + LSize, Y + LSize);
      NextPosition;
    end
    else if Length(FItems[LSection]) = 0 then
      NextPosition; //the row holds the "no colors" text
    //Every section starts on a new row
    if X <> LLeft then
      Inc(Y, LSize + LGap);
  end;
  //The last gap is kept: it holds the caret marker of the last row
  FContentHeight := Y + LMargin;
  UpdateScrollBar;
end;

procedure TSVGPaletteBar.UpdateScrollBar;
var
  LInfo: TScrollInfo;
begin
  if not HandleAllocated then
    Exit;
  FScrollPos := EnsureRange(FScrollPos, 0, Max(0, FContentHeight - ClientHeight));
  LInfo.cbSize := SizeOf(LInfo);
  LInfo.fMask := SIF_ALL;
  LInfo.nMin := 0;
  LInfo.nMax := FContentHeight - 1;
  LInfo.nPage := ClientHeight;
  LInfo.nPos := FScrollPos;
  LInfo.nTrackPos := 0;
  SetScrollInfo(Handle, SB_VERT, LInfo, True);
end;

procedure TSVGPaletteBar.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, Max(0, FContentHeight - ClientHeight));
  if AValue <> FScrollPos then
  begin
    FScrollPos := AValue;
    UpdateScrollBar;
    Invalidate;
  end;
end;

procedure TSVGPaletteBar.WMVScroll(var Message: TWMVScroll);
var
  LInfo: TScrollInfo;
  LLine: Integer;
begin
  LLine := SwatchSize + Gap;
  case Message.ScrollCode of
    SB_LINEUP: SetScrollPos(FScrollPos - LLine);
    SB_LINEDOWN: SetScrollPos(FScrollPos + LLine);
    SB_PAGEUP: SetScrollPos(FScrollPos - ClientHeight);
    SB_PAGEDOWN: SetScrollPos(FScrollPos + ClientHeight);
    SB_TOP: SetScrollPos(0);
    SB_BOTTOM: SetScrollPos(FContentHeight);
    SB_THUMBTRACK, SB_THUMBPOSITION:
    begin
      //Message.Pos is limited to 16 bits: read the real position
      LInfo.cbSize := SizeOf(LInfo);
      LInfo.fMask := SIF_TRACKPOS;
      if GetScrollInfo(Handle, SB_VERT, LInfo) then
        SetScrollPos(LInfo.nTrackPos);
    end;
  end;
  Message.Result := 0;
end;

function TSVGPaletteBar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result and (FContentHeight > ClientHeight) then
  begin
    SetScrollPos(FScrollPos - Sign(WheelDelta) * (SwatchSize + Gap));
    Result := True;
  end;
end;

procedure TSVGPaletteBar.ScrollIntoView(const ASection: TSVGPaletteSection;
  const AIndex: Integer);
var
  LRect: TRect;
begin
  if (AIndex < 0) or (AIndex > High(FRects[ASection])) then
    Exit;
  LRect := FRects[ASection][AIndex];
  if LRect.Top - Gap < FScrollPos then
    SetScrollPos(LRect.Top - Gap)
  else if LRect.Bottom + Gap > FScrollPos + ClientHeight then
    SetScrollPos(LRect.Bottom + Gap - ClientHeight);
end;

function TSVGPaletteBar.HitTest(const X, Y: Integer): TSVGPaletteHit;
var
  LPoint: TPoint;
  LSection: TSVGPaletteSection;
  I: Integer;
begin
  Result := TSVGPaletteHit.None;
  LPoint := Point(X, Y + FScrollPos);
  for LSection := Low(TSVGPaletteSection) to High(TSVGPaletteSection) do
    for I := 0 to High(FRects[LSection]) do
      if FRects[LSection][I].Contains(LPoint) then
      begin
        Result.Section := LSection;
        Result.Index := I;
        Exit;
      end;
  if FShowCustomSection and FAddRect.Contains(LPoint) then
  begin
    Result.Section := psCustom;
    Result.IsAddButton := True;
  end;
end;

function TSVGPaletteBar.HitRect(const AHit: TSVGPaletteHit): TRect;
begin
  if AHit.IsAddButton then
    Result := FAddRect
  else if (AHit.Index >= 0) and (AHit.Index <= High(FRects[AHit.Section])) then
    Result := FRects[AHit.Section][AHit.Index]
  else
    Result := TRect.Empty;
  if not Result.IsEmpty then
    Result.Offset(0, -FScrollPos);
end;

procedure TSVGPaletteBar.InvalidateHit(const AHit: TSVGPaletteHit);
var
  LRect: TRect;
begin
  LRect := HitRect(AHit);
  if HandleAllocated and not LRect.IsEmpty then
  begin
    //Include the frame and the caret marker drawn outside the swatch
    LRect.Inflate(Gap, Gap);
    InvalidateRect(Handle, LRect, False);
  end;
end;

procedure TSVGPaletteBar.Select(const ASection: TSVGPaletteSection;
  const AIndex: Integer);
var
  LNew: TSVGPaletteHit;
begin
  LNew := TSVGPaletteHit.None;
  if (AIndex >= 0) and (AIndex <= High(FItems[ASection])) then
  begin
    LNew.Section := ASection;
    LNew.Index := AIndex;
  end;
  if not LNew.SameAs(FSelected) then
  begin
    InvalidateHit(FSelected);
    FSelected := LNew;
    InvalidateHit(FSelected);
  end;
end;

procedure TSVGPaletteBar.SetCaretIndex(const AValue: Integer);
var
  LHit: TSVGPaletteHit;
begin
  if AValue = FCaretIndex then
    Exit;
  LHit := TSVGPaletteHit.None;
  LHit.Index := FCaretIndex;
  InvalidateHit(LHit);
  FCaretIndex := AValue;
  LHit.Index := FCaretIndex;
  InvalidateHit(LHit);
end;

procedure TSVGPaletteBar.SetShowCustomSection(const AValue: Boolean);
begin
  if FShowCustomSection <> AValue then
  begin
    FShowCustomSection := AValue;
    UpdateLayout;
    Invalidate;
  end;
end;

procedure TSVGPaletteBar.SetCaption(var AField: string; const AValue: string);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    UpdateLayout;
    Invalidate;
  end;
end;

procedure TSVGPaletteBar.SetDocumentCaption(const AValue: string);
begin
  SetCaption(FDocumentCaption, AValue);
end;

procedure TSVGPaletteBar.SetCustomCaption(const AValue: string);
begin
  SetCaption(FCustomCaption, AValue);
end;

procedure TSVGPaletteBar.SetEmptyText(const AValue: string);
begin
  SetCaption(FEmptyText, AValue);
end;

procedure TSVGPaletteBar.Resize;
begin
  inherited;
  UpdateLayout;
end;

procedure TSVGPaletteBar.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  UpdateLayout;
end;

procedure TSVGPaletteBar.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateLayout;
  Invalidate;
end;

procedure TSVGPaletteBar.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TSVGPaletteBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  //Everything is painted in Paint: avoids flickering
  Message.Result := 1;
end;

procedure TSVGPaletteBar.DrawSwatch(const ARect: TRect;
  const AItem: TSVGPaletteItem; const AHot, ASelected, AIsCaret: Boolean);
var
  LRect: TRect;
  LColor: TColor;
  LText: string;
  LLuminance: Integer;
  LMarker: array[0..2] of TPoint;
  LMarkerSize, LFrame, LFontSize: Integer;
begin
  LColor := TColor(((AItem.Color and $FF) shl 16) or (AItem.Color and $FF00) or
    ((AItem.Color shr 16) and $FF));
  if ASelected then
  begin
    //Thick frame around the selected swatch, rounded like the swatch
    LFrame := ScaleValue(3);
    LRect := ARect;
    LRect.Inflate(LFrame, LFrame);
    Canvas.Pen.Color := StyleServices.GetSystemColor(clHighlight);
    Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
    Canvas.RoundRect(LRect, (CornerRadius + LFrame) * 2, (CornerRadius + LFrame) * 2);
  end;
  LRect := ARect;
  Canvas.Pen.Width := 1;
  if AHot then
    Canvas.Pen.Color := StyleServices.GetSystemColor(clWindowText)
  else
    Canvas.Pen.Color := StyleServices.GetSystemColor(clBtnShadow);
  Canvas.Brush.Color := LColor;
  Canvas.RoundRect(LRect, CornerRadius * 2, CornerRadius * 2);
  if AItem.Count > 0 then
  begin
    if AItem.Count > 99 then
      LText := '99+'
    else
      LText := IntToStr(AItem.Count);
    //Black or white text, depending on the luminance of the swatch
    LLuminance := (299 * GetRValue(LColor) + 587 * GetGValue(LColor) +
      114 * GetBValue(LColor)) div 1000;
    //As large as the font of the bar, reduced only when it does not fit
    Canvas.Font := Font;
    LFontSize := Font.Size;
    Canvas.Font.Size := LFontSize;
    while (LFontSize > 6) and (Canvas.TextWidth(LText) > LRect.Width - ScaleValue(4)) do
    begin
      Dec(LFontSize);
      Canvas.Font.Size := LFontSize;
    end;
    if LLuminance > 140 then
      Canvas.Font.Color := clBlack
    else
      Canvas.Font.Color := clWhite;
    Canvas.Brush.Style := bsClear;
    DrawText(Canvas.Handle, PChar(LText), Length(LText), LRect,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    Canvas.Brush.Style := bsSolid;
  end;
  if AIsCaret then
  begin
    //Small triangle under the swatch: color under the editor caret
    LMarkerSize := ScaleValue(4);
    LMarker[0] := Point(ARect.CenterPoint.X - LMarkerSize, ARect.Bottom + Gap - 1);
    LMarker[1] := Point(ARect.CenterPoint.X + LMarkerSize, ARect.Bottom + Gap - 1);
    LMarker[2] := Point(ARect.CenterPoint.X, ARect.Bottom + Gap - 1 - LMarkerSize);
    Canvas.Pen.Color := StyleServices.GetSystemColor(clWindowText);
    Canvas.Brush.Color := StyleServices.GetSystemColor(clWindowText);
    Canvas.Polygon(LMarker);
  end;
end;

procedure TSVGPaletteBar.DrawAddButton(const ARect: TRect; const AHot: Boolean);
var
  LRect: TRect;
  LCenter: TPoint;
  LArm: Integer;
begin
  LRect := ARect;
  if AHot then
    Canvas.Pen.Color := StyleServices.GetSystemColor(clWindowText)
  else
    Canvas.Pen.Color := StyleServices.GetSystemColor(clBtnShadow);
  Canvas.Pen.Style := psDot;
  Canvas.Brush.Style := bsClear;
  Canvas.RoundRect(LRect, CornerRadius * 2, CornerRadius * 2);
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := ScaleValue(2);
  LCenter := LRect.CenterPoint;
  LArm := LRect.Width div 4;
  Canvas.MoveTo(LCenter.X - LArm, LCenter.Y);
  Canvas.LineTo(LCenter.X + LArm + 1, LCenter.Y);
  Canvas.MoveTo(LCenter.X, LCenter.Y - LArm);
  Canvas.LineTo(LCenter.X, LCenter.Y + LArm + 1);
  Canvas.Pen.Width := 1;
end;

procedure TSVGPaletteBar.Paint;
var
  LSection: TSVGPaletteSection;
  LRect: TRect;
  LHit: TSVGPaletteHit;
  LCaption: string;
  I: Integer;
begin
  Canvas.Brush.Color := StyleServices.GetSystemColor(clWindow);
  Canvas.FillRect(ClientRect);
  for LSection := Low(TSVGPaletteSection) to High(TSVGPaletteSection) do
  begin
    if FCaptionRects[LSection].IsEmpty then
      Continue;
    //Reset the font for every caption: DrawSwatch leaves the canvas with the
    //size and the black/white color of the last counter drawn
    Canvas.Font := Font;
    Canvas.Font.Color := StyleServices.GetSystemColor(clWindowText);
    if LSection = psDocument then
      LCaption := FDocumentCaption
    else
      LCaption := FCustomCaption;
    LRect := FCaptionRects[LSection];
    LRect.Offset(0, -FScrollPos);
    Canvas.Brush.Style := bsClear;
    DrawText(Canvas.Handle, PChar(LCaption), Length(LCaption), LRect,
      DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    if (LSection = psDocument) and (Length(FItems[psDocument]) = 0) then
    begin
      //No colors in the document
      LRect.Left := LRect.Right + Gap;
      LRect.Right := ClientWidth;
      Canvas.Font.Color := StyleServices.GetSystemColor(clGrayText);
      DrawText(Canvas.Handle, PChar(FEmptyText), Length(FEmptyText), LRect,
        DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
      Canvas.Font.Color := StyleServices.GetSystemColor(clWindowText);
    end;
    Canvas.Brush.Style := bsSolid;
    for I := 0 to High(FItems[LSection]) do
    begin
      LHit.Section := LSection;
      LHit.Index := I;
      LHit.IsAddButton := False;
      DrawSwatch(HitRect(LHit), FItems[LSection][I], LHit.SameAs(FHot),
        LHit.SameAs(FSelected), (LSection = psDocument) and (I = FCaretIndex));
    end;
  end;
  if FShowCustomSection and not FAddRect.IsEmpty then
  begin
    LHit := TSVGPaletteHit.None;
    LHit.IsAddButton := True;
    LHit.Section := psCustom;
    DrawAddButton(HitRect(LHit), FHot.IsAddButton);
  end;
end;

procedure TSVGPaletteBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FMouseDownHit := HitTest(X, Y);
  //The second press of a double click must not raise a further click
  FSuppressClick := ssDouble in Shift;
end;

procedure TSVGPaletteBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LHit: TSVGPaletteHit;
begin
  inherited;
  LHit := HitTest(X, Y);
  if not LHit.SameAs(FHot) then
  begin
    InvalidateHit(FHot);
    FHot := LHit;
    InvalidateHit(FHot);
    if LHit.IsSwatch or LHit.IsAddButton then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
  end;
end;

procedure TSVGPaletteBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  InvalidateHit(FHot);
  FHot := TSVGPaletteHit.None;
end;

procedure TSVGPaletteBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  LHit: TSVGPaletteHit;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  if FSuppressClick then
  begin
    FSuppressClick := False;
    Exit;
  end;
  LHit := HitTest(X, Y);
  //Act only when the button is released on the swatch where it was pressed
  if not LHit.SameAs(FMouseDownHit) then
    Exit;
  if LHit.IsAddButton then
  begin
    if Assigned(FOnAddClick) then
      FOnAddClick(Self);
  end
  else if LHit.IsSwatch then
  begin
    Select(LHit.Section, LHit.Index);
    if Assigned(FOnSwatchClick) then
      FOnSwatchClick(Self, LHit.Section, LHit.Index);
  end;
end;

procedure TSVGPaletteBar.DblClick;
begin
  inherited;
  if FMouseDownHit.IsSwatch and Assigned(FOnSwatchDblClick) then
    FOnSwatchDblClick(Self, FMouseDownHit.Section, FMouseDownHit.Index);
end;

procedure TSVGPaletteBar.CMHintShow(var Message: TCMHintShow);
var
  LHit: TSVGPaletteHit;
  LRect: TRect;
begin
  //One hint for each swatch: the CursorRect makes the hint change when the
  //mouse moves to another swatch
  LHit := HitTest(Message.HintInfo.CursorPos.X, Message.HintInfo.CursorPos.Y);
  LRect := HitRect(LHit);
  if LHit.IsAddButton then
    Message.HintInfo.HintStr := FAddHint
  else if LHit.IsSwatch then
    Message.HintInfo.HintStr := FItems[LHit.Section][LHit.Index].Hint
  else
    Message.HintInfo.HintStr := '';
  //Outside the swatches no hint is shown (empty HintStr), but a tiny
  //CursorRect is still given so the hint is re-evaluated as soon as the mouse
  //moves onto a swatch
  if LRect.IsEmpty then
    LRect := Rect(Message.HintInfo.CursorPos.X, Message.HintInfo.CursorPos.Y,
      Message.HintInfo.CursorPos.X + 1, Message.HintInfo.CursorPos.Y + 1);
  Message.HintInfo.CursorRect := LRect;
  Message.Result := 0;
end;

initialization
  //The bar has its own vertical scrollbar: with a VCL style active it must be
  //drawn by the style, like the one of the editors. Registered here, as
  //SynEdit does, so it does not depend on the VCLStyleUtils units (excluded
  //by NO_VCL_STYLES)
  TCustomStyleEngine.RegisterStyleHook(TSVGPaletteBar, TScrollingStyleHook);

finalization
  TCustomStyleEngine.UnRegisterStyleHook(TSVGPaletteBar, TScrollingStyleHook);

end.
