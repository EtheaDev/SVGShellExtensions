{******************************************************************************}
{                                                                              }
{       SVG Image in TPicture: useful to show a Scalable Vector Graphic        }
{                                                                              }
{       Copyright (c) 2019-2021 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconImageList                           }
{                                                                              }
{******************************************************************************}
{       Original version (c) 2005, 2008 Martin Walter with license:            }
{       Use of this file is permitted for commercial and non-commercial        }
{       use, as long as the author is credited.                                }
{       home page: http://www.mwcs.de                                          }
{       email    : martin.walter@mwcs.de                                       }
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
unit SVGIconImage;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Types
  , System.Classes
  , System.UITypes
  , Vcl.Controls
  , Vcl.Graphics
  , Vcl.ImgList
  , SVGIconItems
  , SVGInterfaces;

type
  TSVGIconImage = class(TGraphicControl)
  strict private
    FSVG: ISVG;
    FCenter: Boolean;
    FProportional: Boolean;
    FStretch: Boolean;
    FAutoSize: Boolean;
    FOpacity: Byte;
    FFileName: TFileName;
    FImageList: TCustomImageList;
    FImageIndex: System.UITypes.TImageIndex;
    FImageChangeLink: TChangeLink;
    FFixedColor: TColor;
    FGrayScale: Boolean;
    FApplyFixedColorToRootOnly: Boolean;
    procedure SetCenter(Value: Boolean);
    procedure SetProportional(Value: Boolean);
    procedure SetOpacity(Value: Byte);
    procedure SetFileName(const Value: TFileName);
    procedure SetImageIndex(const Value: System.UITypes.TImageIndex);
    procedure SetStretch(const Value: Boolean);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetAutoSizeImage(const Value: Boolean);
    procedure ImageListChange(Sender: TObject);
    procedure SetFixedColor(const Value: TColor);
    procedure SetGrayScale(const Value: Boolean);
    procedure SetApplyFixedColorToRootOnly(const Value: Boolean);
    function GetInheritedFixedColor: TColor;
    function GetInheritedApplyToRootOnly: Boolean;
    procedure ReadDummyBool(Reader: TReader);
    procedure ReadDummyFloat(Reader: TReader);
    procedure WriteDummy(Writer: TWriter);
  private
    function GetSVGText: string;
    procedure SetSVGText(const AValue: string);
    function UsingSVGText: Boolean;
    function GetSVG: ISVG;
  protected
    procedure DefineProperties(Filer: TFiler); override;

    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CheckAutoSize;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateSVGFactory;
    destructor Destroy; override;
    procedure Clear;
    function SVGIconItem: TSVGIconItem;
    function Empty: Boolean;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure Assign(Source: TPersistent); override;
    property SVG: ISVG read GetSVG;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSizeImage;
    property Center: Boolean read FCenter write SetCenter default True;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property ImageIndex: System.UITypes.TImageIndex read FImageIndex write SetImageIndex default -1;
    property FileName: TFileName read FFileName write SetFileName;
    property SVGText: string read GetSVGText write SetSVGText stored UsingSVGText;
    property FixedColor: TColor read FFixedColor write SetFixedColor default SVG_INHERIT_COLOR;
    property ApplyFixedColorToRootOnly: Boolean read FApplyFixedColorToRootOnly write SetApplyFixedColorToRootOnly default false;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property Proportional: Boolean read FProportional write SetProportional default True;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default True;
    property Touch;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TSVGGraphic = class(TGraphic)
  strict private
    FSVG: ISVG;
    FOpacity: Byte;
    FFileName: TFileName;

    procedure SetOpacity(Value: Byte);
    procedure SetFileName(const Value: TFileName);
  protected
    procedure DefineProperties(Filer: TFiler); override;

    function GetEmpty: Boolean; override;
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;

    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  public
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

    constructor Create; override;
    procedure Clear;

    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure AssignSVG(SVG: ISVG);

    procedure LoadFromFile(const Filename: String); override;
    procedure LoadFromStream(Stream: TStream); override;

    procedure SaveToStream(Stream: TStream); override;

    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;

    property Opacity: Byte read FOpacity write SetOpacity;
  published
    property FileName: TFileName read FFileName write SetFileName;
  end;

implementation

uses
  SVGIconImageListBase
  {$IFDEF D10_3+}
  , Vcl.VirtualImageList
  {$ENDIF}
  , SVGIconImageCollection;

procedure TSVGIconImage.UpdateSVGFactory;
var
  LOldSVGText: string;
begin
  LOldSVGText := fsvg.Source;
  FSVG := GlobalSVGFactory.NewSvg;
  FSVG.Source := LOldSVGText;
  Invalidate;
end;

constructor TSVGIconImage.Create(AOwner: TComponent);
begin
  inherited;
  FSVG := GlobalSVGFactory.NewSvg;
  FProportional := True;
  FCenter := True;
  FStretch := True;
  FOpacity := 255;
  FImageIndex := -1;
  FFixedColor := SVG_INHERIT_COLOR;
  FGrayScale := False;
  //ParentBackground := True;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

procedure TSVGIconImage.ReadDummyBool(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TSVGIconImage.ReadDummyFloat(Reader: TReader);
begin
  Reader.ReadFloat;
end;

procedure TSVGIconImage.WriteDummy(Writer: TWriter);
begin
  ; //do nothing
end;

procedure TSVGIconImage.DefineProperties(Filer: TFiler);
begin
  //For prevent error loading dfm with old properties removed from this component
  Filer.DefineProperty('Scale', ReadDummyFloat, WriteDummy, False);
  Filer.DefineProperty('DoubleBuffered', ReadDummyBool, WriteDummy, False);
  Filer.DefineProperty('ParentDoubleBuffered', ReadDummyBool, WriteDummy, False);
  Filer.DefineProperty('ParentBackground', ReadDummyBool, WriteDummy, False);
end;

destructor TSVGIconImage.Destroy;
begin
  FImageChangeLink.Free;
  inherited;
end;

procedure TSVGIconImage.CheckAutoSize;
begin
  if FAutoSize and (FSVG.Width > 0) and (FSVG.Height > 0) then
  begin
    SetBounds(Left, Top,  Round(FSVG.Width), Round(FSVG.Height));
  end;
end;

procedure TSVGIconImage.Clear;
begin
  FSVG.Clear;
  FFileName := '';
  Repaint;
end;

function TSVGIconImage.Empty: Boolean;
begin
  Empty := FSVG.IsEmpty;
end;

function TSVGIconImage.GetInheritedApplyToRootOnly: Boolean;
begin
  Result := False;
  if not Assigned(FImageList) then
    Exit;
  if FImageList is TSVGIconImageListBase then
  begin
    if FImageIndex >= 0 then
    begin
      if FImageIndex < FImageList.Count then
        Result := SVGIconItem.ApplyFixedColorToRootOnly;
    end;
    Result := Result or TSVGIconImageListBase(FImageList).ApplyFixedColorToRootOnly;
  end;
end;

function TSVGIconImage.GetInheritedFixedColor: TColor;
begin
  Result := SVG_INHERIT_COLOR;
  if not Assigned(FImageList) then
    Exit;
  if FImageList is TSVGIconImageListBase then
  begin
    if FImageIndex >= 0 then
    begin
      if FImageIndex < FImageList.Count then
        Result := SVGIconItem.FixedColor;
      if Result <> SVG_INHERIT_COLOR then
        exit;
    end;
    Result := TSVGIconImageListBase(FImageList).FixedColor;
  end;
end;

function TSVGIconImage.GetSVG: ISVG;
begin
  if not UsingSVGText then
    Result := SVGIconItem.SVG
  else
    Result := FSVG;
end;

function TSVGIconImage.GetSVGText: string;
begin
  Result := SVG.Source;
end;

procedure TSVGIconImage.ImageListChange(Sender: TObject);
begin
  if Sender = FImageList then
    Invalidate;
end;

function TSVGIconImage.SVGIconItem: TSVGIconItem;
var
  {$IFDEF D10_3+}
  LVirtualImageList: TVirtualImageList;
  LItem: TVirtualImageListItem;
  {$ENDIF}
  LSVGIconItems: TSVGIconItems;
begin
  Result := nil;
  if FImageList is TSVGIconImageListBase then
  begin
    LSVGIconItems := TSVGIconImageListBase(FImageList).SVGIconItems;
    Result := LSVGIconItems[FImageIndex];
  end;
  {$IFDEF D10_3+}
  if (FImageList is TVirtualImageList) then
  begin
    LVirtualImageList := TVirtualImageList(FImageList);
    if LVirtualImageList.ImageCollection is TSVGIconImageCollection then
    begin
      LSVGIconItems := TSVGIconImageCollection(LVirtualImageList.ImageCollection).SVGIconItems;
      LItem := LVirtualImageList.Images[FImageIndex];
      if Assigned(LItem) then
        Result := LSVGIconItems[LItem.Collectionindex];
    end;
  end;
  {$ENDIF}
end;

function TSVGIconImage.UsingSVGText: Boolean;
begin
  Result := not (Assigned(FImageList) and (FImageIndex >= 0) and
    (FImageIndex < FImageList.Count));
end;

procedure TSVGIconImage.Paint;
var
  LSVG: ISVG;
  LWidth, LHeight: Integer;
  LOrigin: TPointF;
begin
  if not UsingSVGText then
    LSVG := SVGIconItem.SVG

  else
    LSVG := FSVG;

  if not LSVG.IsEmpty then
  begin
    LSVG.Opacity := FOpacity / 255;
    if FFixedColor <> SVG_INHERIT_COLOR then
    begin
      LSVG.FixedColor := FFixedColor;
      LSVG.ApplyFixedColorToRootOnly := FApplyFixedColorToRootOnly;
    end
    else
    begin
      LSVG.FixedColor := GetInheritedFixedColor;
      LSVG.ApplyFixedColorToRootOnly := GetInheritedApplyToRootOnly;
    end;
    LSVG.GrayScale := FGrayScale;
    if FStretch or not Assigned(FImageList) then
    begin
      LWidth := Width;
      LHeight := Height;
      LOrigin := TPointF.Create(0, 0);
    end
    else
    begin
      LWidth := FImageList.Width;
      LHeight := FImageList.Height;
      LOrigin := TPointF.Create(
        (Width - LWidth) div 2, ((Height - LHeight) div 2));
    end;
    LSVG.PaintTo(Canvas.Handle,
      TRectF.Create(LOrigin, LWidth, LHeight), FProportional);
    LSVG.Opacity := 1;
  end;

  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDash;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

procedure TSVGIconImage.LoadFromFile(const FileName: string);
begin
  if csLoading in ComponentState then
    Exit;
  try
    FSVG.LoadFromFile(FileName);
    FFileName := FileName;
  except
    Clear;
    raise;
  end;
  CheckAutoSize;
  Repaint;
end;

procedure TSVGIconImage.LoadFromStream(Stream: TStream);
begin
  try
    FFileName := '';
    FSVG.LoadFromStream(Stream);
  except
    FSVG.Clear;
    raise;
  end;
  CheckAutoSize;
  Repaint;
end;

procedure TSVGIconImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;
end;

procedure TSVGIconImage.Assign(Source: TPersistent);
begin
  if (Source is TSVGIconImage) then
  begin
    FSVG := (Source as TSVGIconImage).FSVG;
    FImageIndex := -1;
    CheckAutoSize;
  end;

  Repaint;
end;

procedure TSVGIconImage.SaveToFile(const FileName: string);
begin
  FSVG.SaveToFile(FileName);
end;

procedure TSVGIconImage.SetAutoSizeImage(const Value: Boolean);
begin
  if (Value = FAutoSize) then
    Exit;
  FAutoSize := Value;

  CheckAutoSize;
end;

procedure TSVGIconImage.SetCenter(Value: Boolean);
begin
  if Value = FCenter then
    Exit;

  FCenter := Value;
  Repaint;
end;

procedure TSVGIconImage.SetProportional(Value: Boolean);
begin
  if Value = FProportional then
    Exit;

  FProportional := Value;
  Repaint;
end;

procedure TSVGIconImage.SetStretch(const Value: Boolean);
begin
  if Value <> FStretch then
  begin
    FStretch := Value;
    Repaint;
  end;
end;

procedure TSVGIconImage.SetSVGText(const AValue: string);
begin
  FSVG.Source := AValue;
  Repaint;
end;

procedure TSVGIconImage.SetOpacity(Value: Byte);
begin
  if Value = FOpacity then
    Exit;

  FOpacity := Value;
  Repaint;
end;

procedure TSVGIconImage.SetFileName(const Value: TFileName);
begin
  if Value = FFileName then
    Exit;
  LoadFromFile(Value);
end;

procedure TSVGIconImage.SetFixedColor(const Value: TColor);
begin
  if Value <> FFixedColor then
  begin
    FFixedColor := Value;
    if FFixedColor <> SVG_INHERIT_COLOR then
      FGrayScale := False;
    Repaint;
  end;
end;

procedure TSVGIconImage.SetApplyFixedColorToRootOnly(const Value: Boolean);
begin
  if FApplyFixedColorToRootOnly <> Value then
  begin
    FApplyFixedColorToRootOnly := Value;
    Repaint;
  end;
end;

procedure TSVGIconImage.SetGrayScale(const Value: Boolean);
begin
  if Value <> FGrayScale then
  begin
    FGrayScale := Value;
    if FGrayScale then
      FixedColor := SVG_INHERIT_COLOR;
    Repaint;
  end;
end;

procedure TSVGIconImage.SetImageIndex(const Value: System.UITypes.TImageIndex);
begin
  if FImageIndex = Value then
    Exit;
  FImageIndex := Value;
  CheckAutoSize;
  Repaint;
end;

procedure TSVGIconImage.SetImageList(const Value: TCustomImageList);
begin
  if FImageList <> nil then FImageList.UnRegisterChanges(FImageChangeLink);
  FImageList := Value;
  if FImageList <> nil then
  begin
    FImageList.RegisterChanges(FImageChangeLink);
    FImageList.FreeNotification(Self);
    SVGText := '';
  end;
end;

constructor TSVGGraphic.Create;
begin
  inherited;
  FSVG := GlobalSVGFactory.NewSvg;
  FOpacity := 255;
end;

procedure TSVGGraphic.Clear;
begin
  FSVG.Clear;
  FFileName := '';
  Changed(Self);
end;

procedure TSVGGraphic.Assign(Source: TPersistent);
begin
  if (Source is TSVGGraphic) then
  begin
    FSVG := TSVGGraphic(Source).FSVG;
    Changed(Self);
  end;
end;

procedure TSVGGraphic.AssignSVG(SVG: ISVG);
begin
  FSVG := SVG;
  Changed(Self);
end;

procedure TSVGGraphic.AssignTo(Dest: TPersistent);
begin
  if Dest is TSVGGraphic then
    TSVGGraphic(Dest).Assign(Self);
end;

procedure TSVGGraphic.SetOpacity(Value: Byte);
begin
  if Value = FOpacity then
    Exit;

  FOpacity := Value;
  Changed(Self);
end;

procedure TSVGGraphic.SetWidth(Value: Integer);
begin
  inherited;
end;

procedure TSVGGraphic.SetFileName(const Value: TFileName);
begin
  if Value = FFileName then
    Exit;

  LoadFromFile(Value);
end;

procedure TSVGGraphic.SetHeight(Value: Integer);
begin
  inherited;
end;

procedure TSVGGraphic.ReadData(Stream: TStream);
var
  Size: LongInt;
  MemStream: TMemoryStream;
begin
  Stream.Read(Size, SizeOf(Size));
  MemStream := TMemoryStream.Create;
  try
    MemStream.CopyFrom(Stream, Size);
    MemStream.Position := 0;
    FSVG.LoadFromStream(MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TSVGGraphic.WriteData(Stream: TStream);
var
  Size: LongInt;
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    FSVG.SaveToStream(MemStream);
    Size := MemStream.Size;
    Stream.Write(Size, SizeOf(Size));
    MemStream.Position := 0;
    MemStream.SaveToStream(Stream);
  finally
    MemStream.Free;
  end;
end;

procedure TSVGGraphic.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, True);
end;

procedure TSVGGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if Empty then
    Exit;

  FSVG.Opacity := FOpacity / 255;
  FSVG.PaintTo(ACanvas.Handle, TRectF.Create(Rect));
end;


function TSVGGraphic.GetEmpty: Boolean;
begin
  Result := FSVG.IsEmpty;
end;

function TSVGGraphic.GetWidth: Integer;
begin
  Result := Round(FSVG.Width);
end;

function TSVGGraphic.GetHeight: Integer;
begin
  Result := Round(FSVG.Height);
end;

procedure TSVGGraphic.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  inherited;

end;

procedure TSVGGraphic.LoadFromFile(const Filename: String);
begin
  FSVG.LoadFromFile(Filename);
  Changed(Self);
end;

procedure TSVGGraphic.LoadFromStream(Stream: TStream);
begin
  FSVG.LoadFromStream(Stream);
  Changed(Self);
end;

procedure TSVGGraphic.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  inherited;

end;

procedure TSVGGraphic.SaveToStream(Stream: TStream);
begin
  FSVG.SaveToStream(Stream);
end;


initialization
  TPicture.RegisterFileFormat('SVG', 'Scalable Vector Graphics', TSVGGraphic);

finalization
  TPicture.UnregisterGraphicClass(TSVGGraphic);
end.
