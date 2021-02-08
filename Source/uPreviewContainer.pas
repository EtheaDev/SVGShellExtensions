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
unit uPreviewContainer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TPreviewContainer = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPreviewHandler: TObject;
  public
    //procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetFocusTabFirst;
    procedure SetFocusTabLast;
    procedure SetBackgroundColor(color: TColorRef);
    //procedure SetBoundsRect(const ARect: TRect);
    procedure SetBoundsRectAndPPI(const ARect: TRect;
      const AOldPPI, ANewPPI: Integer); virtual;
    procedure SetTextColor(color: TColorRef);
    procedure SetTextFont(const plf: TLogFont);
    property  PreviewHandler: TObject read FPreviewHandler write FPreviewHandler;
  end;

implementation

uses
  SynEdit,
  System.Math,
  Vcl.Styles.Ext,
  Vcl.Styles,
  Vcl.Themes,
  uLogExcept,
  uSVGSettings;

{$R *.dfm}

procedure TPreviewContainer.SetFocusTabFirst;
begin
  SelectNext(nil, True, True);
end;

procedure TPreviewContainer.SetFocusTabLast;
begin
  SelectNext(nil, False, True);
end;

procedure TPreviewContainer.FormCreate(Sender: TObject);
var
  LSettings: TSettings;
begin
  TLogPreview.Add('TPreviewContainer.FormCreate'+
    'ScaleFactor: '+Self.ScaleFactor.ToString+
    'CurrentPPI '+Self.CurrentPPI.ToString);
  LSettings := TSettings.Create;
  try
    if not IsStyleHookRegistered(TCustomSynEdit, TScrollingStyleHook) then
      TStyleManager.Engine.RegisterStyleHook(TCustomSynEdit, TScrollingStyleHook);

    if (Trim(LSettings.StyleName) <> '') and not SameText('Windows', LSettings.StyleName) then
      TStyleManager.TrySetStyle(LSettings.StyleName, False);
  finally
    LSettings.Free;
  end;
  TLogPreview.Add('TPreviewContainer.FormCreate Done');
end;

procedure TPreviewContainer.FormDestroy(Sender: TObject);
begin
  TLogPreview.Add('TPreviewContainer.FormDestroy');
end;

procedure TPreviewContainer.SetBackgroundColor(color: TColorRef);
begin
end;

(*
procedure TPreviewContainer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not (csLoading in componentstate) and (AWidth <> 320) and (AHeight <> 240)
    and (AWidth <> 0) and (AHeight <> 0) then
  begin
    TLogPreview.Add('TPreviewContainer.SetBounds'+
      ' ScaleFactor: '+Self.ScaleFactor.ToString+
      ' CurrentPPI: '+Self.CurrentPPI.ToString+
//      ' Scaled: '+Self.Scaled.ToString+
      ' Width: '+Width.ToString+
      ' Height: '+Height.ToString+
      ' AWidth: '+AWidth.ToString+
      ' AHeight: '+AHeight.ToString);
  end;
  inherited;
  if not (csLoading in componentstate) and (AWidth <> 320) and (AHeight <> 240)
    and (AWidth <> 0) and (AHeight <> 0) then
  begin
    TLogPreview.Add('TPreviewContainer.SetBounds'+
      ' ScaleFactor: '+Self.ScaleFactor.ToString+
      ' CurrentPPI: '+Self.CurrentPPI.ToString+
//      ' Scaled: '+Self.Scaled.ToString+
      ' Width: '+Width.ToString+
      ' Height: '+Height.ToString);
  end;
end;
*)
procedure TPreviewContainer.SetBoundsRectAndPPI(const ARect: TRect;
  const AOldPPI, ANewPPI: Integer);
begin
  if (ARect.Width <> 0) and (ARect.Height <> 0) then
  begin
    TLogPreview.Add('TPreviewContainer.SetBoundsRect:'+
    ' Visible: '+Self.Visible.Tostring+
      ' CurrentPPI:'+Self.CurrentPPI.ToString+
      ' AOldPPI:'+AOldPPI.ToString+
      ' ANewPPI:'+ANewPPI.ToString+
      ' Scaled:'+Self.Scaled.ToString+
      ' ARect.Width: '+ARect.Width.ToString+
      ' ARect.Height: '+ARect.Height.ToString);

      if ANewPPI > AOldPPI then
      begin
        SetBounds(
          ARect.Left,
          ARect.Top,
          MulDiv(ARect.Width, ANewPPI, AOldPPI),
          MulDiv(ARect.Height, ANewPPI, AOldPPI));
      end
      else
      begin
        SetBounds(
          ARect.Left,
          ARect.Top,
          ARect.Width,
          ARect.Height);
        //ClientPanel.ScaleForPPI(ANewPPI);
      end;

    FCurrentPPI := ANewPPI;
  end;
end;

(*
procedure TPreviewContainer.SetBoundsRect(const ARect: TRect);
begin
  SetBounds(ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
end;
*)

procedure TPreviewContainer.SetTextColor(color: TColorRef);
begin
end;

procedure TPreviewContainer.SetTextFont(const plf: TLogFont);
begin
end;

end.
