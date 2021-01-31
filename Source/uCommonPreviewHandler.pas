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
unit uCommonPreviewHandler;

interface

{ .$DEFINE USE_TStreamPreviewHandler }

uses
//  uStackTrace,
  Classes,
  Controls,
  StdCtrls,
  SysUtils,
  uEditor,
{$IFDEF USE_TStreamPreviewHandler}
  uStreamPreviewHandler,
{$ELSE}
  uFilePreviewHandler,
{$ENDIF}
  uPreviewHandler;

type
{$IFDEF USE_TStreamPreviewHandler}
  TBasePreviewHandler = class(TStreamPreviewHandler)
{$ELSE}
  TBasePreviewHandler = class(TFilePreviewHandler)
{$ENDIF}
  private
    FEditor: TFrmEditor;
  public
    constructor Create(AParent: TWinControl); override;
    procedure Unload; override;
{$IFDEF USE_TStreamPreviewHandler}
    procedure DoPreview(Stream: TIStreamAdapter); override;
{$ELSE}
    procedure DoPreview(const FilePath: string); override;
{$ENDIF}
    property Editor: TFrmEditor read FEditor write FEditor;
  end;

implementation

Uses
  uLogExcept,
  SynEdit,
  Windows,
  Forms,
  WinAPI.GDIPObj, WinAPI.GDIPApi,
  uMisc, uPreviewContainer;

constructor TBasePreviewHandler.Create(AParent: TWinControl);
begin
  inherited;
  FEditor := nil;
end;

{$IFDEF USE_TStreamPreviewHandler}

procedure TBasePreviewHandler.DoPreview(Stream: TIStreamAdapter);
begin
  try
    TLogPreview.Add('DoPreview ' + Self.ClassName);
    if (Editor <> nil) and IsWindow(Editor.Handle) then
    begin
      TLogPreview.Add('DoPreview Visible');
      Editor.Visible := True;
      TLogPreview.Add('DoPreview LoadFromStream');
      Editor.SynEdit1.Lines.LoadFromStream(Stream);
    end;
  except
    on E: Exception do
      TLogPreview.Add(Format('Error in TBasePreviewHandler.DoPreview(Stream) - Message: %s: Trace %s', [E.Message, E.StackTrace]));
  end;
end;
{$ELSE}

procedure TBasePreviewHandler.DoPreview(const FilePath: string);
begin
  try
    TLogPreview.Add('DoPreview ' + Self.ClassName);
    //if (Editor <> nil) and IsWindow(Editor.Handle) then
    begin
      TLogPreview.Add('TGlobalPreviewHandler TFrmEditor.Create');

      //Initialize GDI+
      StartupInput.DebugEventCallback := nil;
      StartupInput.SuppressBackgroundThread := False;
      StartupInput.SuppressExternalCodecs := False;
      StartupInput.GdiplusVersion := 1;
      GdiplusStartup(gdiplusToken, @StartupInput, nil);

      Editor := TFrmEditor.Create(nil);
      Editor.Parent := TFrmEditor.AParent;
      Editor.Align := alClient;
      Editor.BorderStyle := bsNone;
      Editor.Extensions := FExtensions;

      TLogPreview.Add('DoPreview Visible');
      Editor.Visible := True;
      TLogPreview.Add('DoPreview LoadFile');
      Editor.LoadFile(FilePath);
    end;
  except
    on E: Exception do
      TLogPreview.Add(Format('Error in TBasePreviewHandler.DoPreview(FilePath) - Message: %s: Trace %s', [E.Message, E.StackTrace]));
  end;
end;
{$ENDIF}
{
  http://msdn.microsoft.com/en-us/library/bb776865%28v=vs.85%29.aspx
  IPreviewHandler::Unload
  When this method is called, stop any rendering, release any resources allocated by reading data from the stream, and release the IStream itself.
  Once this method is called, the handler must be reinitialized before any attempt to call IPreviewHandler::DoPreview again.
}

type
  TWinControlClass = class(TWinControl);

procedure TBasePreviewHandler.Unload;
begin
  try
    GdiplusShutdown(gdiplusToken);

    TLogPreview.Add('Unload  Init ' + Self.ClassName);

     if Editor<>nil then
     begin
       Editor.Free;
       Editor:=nil;
     end;

     if (TFrmEditor.AParent<>nil) then
     begin
       if TPreviewContainer(TFrmEditor.AParent).Preview<>nil then
        TComPreviewHandler(TPreviewContainer(TFrmEditor.AParent).Preview).Container:=nil;

       TFrmEditor.AParent.Free;
       TFrmEditor.AParent:=nil;
     end;


    inherited;
    TLogPreview.Add('Unload  Done ' + Self.ClassName);
  except
    on E: Exception do
      TLogPreview.Add(Format('Error in TBasePreviewHandler.Unload - Message: %s: Trace %s', [E.Message, E.StackTrace]));
  end;
end;

end.
