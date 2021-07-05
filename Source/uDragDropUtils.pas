{******************************************************************************}
{                                                                              }
{       uDragDropUtils                                                         }
{                                                                              }
{       CopyRight - David Heffernan                                            }
{                                                                              }
{******************************************************************************}
unit uDragDropUtils;

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  Winapi.ShellAPI,
  System.StrUtils,
  Vcl.Forms;

type
  IDragDrop = interface
    function DropAllowed(const FileNames: array of string): Boolean;
    procedure Drop(const FileNames: array of string);
  end;

  TDropTarget = class(TObject, IInterface, IDropTarget)
  private
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private
    // IDropTarget
    FHandle: HWND;
    FDragDrop: IDragDrop;
    FDropAllowed: Boolean;
    procedure GetFileNames(const dataObj: IDataObject; var FileNames: TArray<string>);
    procedure SetEffect(var dwEffect: Integer);
    function DragEnter(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
  public
    constructor Create(AHandle: HWND; const ADragDrop: IDragDrop);
    destructor Destroy; override;
  end;

implementation

{ TDropTarget }

constructor TDropTarget.Create(AHandle: HWND; const ADragDrop: IDragDrop);
begin
  inherited Create;
  FHandle := AHandle;
  FDragDrop := ADragDrop;
  RegisterDragDrop(FHandle, Self)
end;

destructor TDropTarget.Destroy;
begin
  RevokeDragDrop(FHandle);
  inherited;
end;

function TDropTarget.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then begin
    Result := S_OK;
  end else begin
    Result := E_NOINTERFACE;
  end;
end;

function TDropTarget._AddRef: Integer;
begin
  Result := -1;
end;

function TDropTarget._Release: Integer;
begin
  Result := -1;
end;

procedure TDropTarget.GetFileNames(const dataObj: IDataObject; var FileNames: TArray<string>);
var
  i: Integer;
  formatetcIn: TFormatEtc;
  medium: TStgMedium;
  dropHandle: HDROP;
begin
  FileNames := nil;
  formatetcIn.cfFormat := CF_HDROP;
  formatetcIn.ptd := nil;
  formatetcIn.dwAspect := DVASPECT_CONTENT;
  formatetcIn.lindex := -1;
  formatetcIn.tymed := TYMED_HGLOBAL;
  if dataObj.GetData(formatetcIn, medium)=S_OK then begin
    (* This cast needed because HDROP is incorrectly declared as Longint in ShellAPI.pas.  It should be declared as THandle
       which is an unsigned integer.  Without this fix the routine fails in top-down memory allocation scenarios. *)
    dropHandle := HDROP(medium.hGlobal);
    SetLength(FileNames, DragQueryFile(dropHandle, $FFFFFFFF, nil, 0));
    for i := 0 to high(FileNames) do begin
      SetLength(FileNames[i], DragQueryFile(dropHandle, i, nil, 0));
      DragQueryFile(dropHandle, i, @FileNames[i][1], Length(FileNames[i])+1);
    end;
  end;
end;

procedure TDropTarget.SetEffect(var dwEffect: Integer);
begin
  if FDropAllowed then begin
    dwEffect := DROPEFFECT_COPY;
  end else begin
    dwEffect := DROPEFFECT_NONE;
  end;
end;

function TDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  FileNames: TArray<string>;
begin
  Result := S_OK;
  Try
    GetFileNames(dataObj, FileNames);
    FDropAllowed := (Length(FileNames)>0) and FDragDrop.DropAllowed(FileNames);
    SetEffect(dwEffect);
  Except
    Result := E_UNEXPECTED;
  End;
end;

function TDropTarget.DragLeave: HResult;
begin
  Result := S_OK;
end;

function TDropTarget.DragOver(grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin
  Result := S_OK;
  Try
    SetEffect(dwEffect);
  Except
    Result := E_UNEXPECTED;
  End;
end;

function TDropTarget.Drop(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  FileNames: TArray<string>;
begin
  Result := S_OK;
  Try
    GetFileNames(dataObj, FileNames);
    if Length(FileNames)>0 then begin
      FDragDrop.Drop(FileNames);
    end;
  Except
    Application.HandleException(Self);
  End;
end;

end.
