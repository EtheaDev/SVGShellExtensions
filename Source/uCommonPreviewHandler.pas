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
  PreviewForm,
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
  end;

implementation

end.
