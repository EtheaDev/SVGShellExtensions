// **************************************************************************************************
//
// Unit uCommonPreviewHandler
// unit for the Delphi Preview Handler  https://github.com/RRUZ/delphi-preview-handler
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uCommonPreviewHandler.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2021 Rodrigo Ruz V.
// All Rights Reserved.
//
// *************************************************************************************************
unit uCommonPreviewHandler;

interface

{$DEFINE USE_TStreamPreviewHandler}

uses
  Classes,
  Controls,
  StdCtrls,
  SysUtils,
  SVGEditor,
{$IFDEF USE_TStreamPreviewHandler}
  uStreamAdapter,
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
    //if (Editor <> nil) and IsWindow(Editor.Handle) then
    begin
      Initialize_GDI;
      TLogPreview.Add('TGlobalPreviewHandler TFrmEditor.Create');
      Editor := TFrmEditor.Create(nil);
      Editor.Parent := TPreviewContainer(TFrmEditor.AParent);
      Editor.Align := alClient;
      Editor.BorderStyle := bsNone;

      TLogPreview.Add('DoPreview Visible');
      Editor.Visible := True;
      TLogPreview.Add('DoPreview LoadFromStream');
      Editor.LoadFromStream(Stream);
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
      Initialize_GDI;
      TLogPreview.Add('TGlobalPreviewHandler TFrmEditor.Create');
      Editor := TFrmEditor.Create(nil);
      Editor.Align := alClient;
      Editor.Parent := TPreviewContainer(TFrmEditor.AParent).ClientPanel;
      Editor.BorderStyle := bsNone;

      TLogPreview.Add('DoPreview Visible');
      Editor.Visible := True;
      TLogPreview.Add('DoPreview LoadFile');
      Editor.LoadFromFile(FilePath);
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
    TLogPreview.Add('Unload  Init ' + Self.ClassName);
    // if IsWindow(TWinControlClass(Editor).WindowHandle) then
    // begin
    // Editor.Visible:=False;
    // Editor.SynEdit1.Lines.Clear;
    // end;

     if Editor<>nil then
     begin
       Editor.Free;
       Editor:=nil;
     end;

     if (TFrmEditor.AParent<>nil) then
     begin
       if TPreviewContainer(TFrmEditor.AParent).PreviewHandler <> nil then
        TComPreviewHandler(TPreviewContainer(TFrmEditor.AParent).PreviewHandler).Container := nil;

       TFrmEditor.AParent.Free;
       TFrmEditor.AParent:=nil;
     end;


    inherited;
    Finalize_GDI;
    TLogPreview.Add('Unload  Done ' + Self.ClassName);
  except
    on E: Exception do
      TLogPreview.Add(Format('Error in TBasePreviewHandler.Unload - Message: %s: Trace %s', [E.Message, E.StackTrace]));
  end;
end;

end.
