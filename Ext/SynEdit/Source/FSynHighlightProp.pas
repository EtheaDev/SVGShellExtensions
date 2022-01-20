{*******************************************************************}
{                                                                   }
{   FSynHighlightProp                                               }
{                                                                   }
{   Copyright (c) 2005-2022 Ethea S.r.l.                            }
{   ALL RIGHTS RESERVED / TUTTI I DIRITTI RISERVATI                 }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   The entire contents of this file is protected by                }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM ETHEA S.R.L.                                }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   Il contenuto di questo file è protetto dalle leggi              }
{   internazionali sul Copyright. Sono vietate la riproduzione, il  }
{   reverse-engineering e la distribuzione non autorizzate di tutto }
{   o parte del codice contenuto in questo file. Ogni infrazione    }
{   sarà perseguita civilmente e penalmente a termini di legge.     }
{                                                                   }
{   RESTRIZIONI                                                     }
{                                                                   }
{   SONO VIETATE, SENZA IL CONSENSO SCRITTO DA PARTE DI             }
{   ETHEA S.R.L., LA COPIA, LA VENDITA, LA DISTRIBUZIONE E IL       }
{   TRASFERIMENTO A TERZI, A QUALUNQUE TITOLO, DEL CODICE SORGENTE  }
{   CONTENUTO IN QUESTO FILE E ALTRI FILE AD ESSO COLLEGATI.        }
{                                                                   }
{   SI FACCIA RIFERIMENTO ALLA LICENZA D'USO PER INFORMAZIONI SU    }
{   EVENTUALI RESTRIZIONI ULTERIORI.                                }
{                                                                   }
{*******************************************************************} 
unit FSynHighlightProp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, ColorGrd, StdCtrls, CheckLst, SynEdit,
  ActnList, SynEditHighlighter, SynUnicode;

type
  TfmHighLightSettings = class(TForm)
    pc: TPageControl;
    tsColors: TTabSheet;
    paLeft: TPanel;
    paColor: TPanel;
    ColorGrid: TColorGrid;
    paElements: TPanel;
    BoxElements: TListBox;
    paElemTitle: TPanel;
    Splitter1: TSplitter;
    paAttributesContainer: TPanel;
    paAttributes: TPanel;
    StatusBar: TStatusBar;
    SynEdit: TSynEdit;
    lbColors: TLabel;
    cbTextAttrib: TGroupBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbUnderline: TCheckBox;
    cbStrikeOut: TCheckBox;
    gbWhiteSpace: TGroupBox;
    cbForeground: TCheckBox;
    cbBackground: TCheckBox;
    paBottom: TPanel;
    paButtons: TPanel;
    btCancel: TButton;
    btOK: TButton;
    LoadButton: TButton;
    OpenDialog: TOpenDialog;
    procedure BoxElementsClick(Sender: TObject);
    procedure cbForegroundClick(Sender: TObject);
    procedure cbBackgroundClick(Sender: TObject);
    procedure cbFontStyleClick(Sender: TObject);
    procedure GetActiveAttribute;
    procedure SynEditClick(Sender: TObject);
    procedure SynEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ColorGridClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
  private
    FHighlighter : TSynCustomHighlighter;
    FSourceSynEdit : TSynEdit;
    FFileName : string;
    function GetCurrentElement: TSynHighlighterAttributes;
    procedure RefreshColorGrid;
    procedure RefreshDefaultCheckBox;
    procedure RefreshTextAttributes;
    procedure ColorGridChange;
    function GetBackGroundColor: TColor;
    function GetForeGroundColor: TColor;
    procedure AddElements;
    procedure RefreshMap;
    procedure CloneSynEdit(Source, Dest : TSynEdit );
    property CurrentElement : TSynHighlighterAttributes read GetCurrentElement;
    property ForeGroundColor : TColor read GetForeGroundColor;
    property BackGroundColor : TColor read GetBackGroundColor;
  public
  end;

procedure LoadSettings(Editor : TSynEdit; const SettingsFileName : string);
procedure ShowHighLighterSettings(SourceSynEdit : TSynEdit; const SettingsFileName : string);

implementation

{$R *.dfm}

procedure RefreshEditorColor(Editor : TSynEdit);
begin
  //Editor.Font.Color := Editor.Highlighter.WhitespaceAttribute.Foreground;
  //Editor.Color := Editor.Highlighter.WhitespaceAttribute.Background;
end;

procedure LoadSettings(Editor : TSynEdit; const SettingsFileName : string);
begin
  //carica i parametri da file
  Editor.Color := clWindowText;
  Editor.Font.Color := clWindowText;
  if Editor.Highlighter.LoadFromFile(SettingsFileName) then
  begin
    RefreshEditorColor(Editor);
  end;
end;

procedure ShowHighLighterSettings(SourceSynEdit : TSynEdit;
  const SettingsFileName : string);
type
  TSynCustomHighlighterClass = class of TSynCustomHighlighter;
var
  HighLightSettingsClass : TSynCustomHighlighterClass;
  fmHighLightSettings : TfmHighLightSettings;
begin
  if SourceSynEdit.Highlighter = nil then exit;

  fmHighLightSettings := TfmHighLightSettings.Create(nil);
  with fmHighLightSettings do
  Try
    FSourceSynEdit := SourceSynEdit;
    SynEdit.Color := FSourceSynEdit.Color;
    SynEdit.Font.Assign(FSourceSynEdit.Font);
    SynEdit.ActiveLineColor := FSourceSynEdit.ActiveLineColor;
    FFileName := SettingsFileName;
    StatusBar.SimpleText := FFileName;
    HighLightSettingsClass := TSynCustomHighlighterClass(SourceSynEdit.Highlighter.ClassType);
    FHighlighter := HighLightSettingsClass.Create(nil);
    Try
      SynEdit.Highlighter := FHighlighter;
      CloneSynEdit(SourceSynEdit,SynEdit);
      SynEdit.Text := SourceSynEdit.Text;
      AddElements;
      ShowModal;
    Finally
      FHighlighter.Free;
    End;

  Finally
    fmHighLightSettings.Free;
  End;
end;

{ TfmHighLightSettings }

procedure TfmHighLightSettings.AddElements;
var
  i : integer;
begin
  //Add Elements as Highlighters attributes
  For i := 0 to FHighlighter.AttrCount -1 do
  begin
    BoxElements.AddItem(FHighlighter.Attribute[I].Name, FHighlighter.Attribute[I]);
    //Chage WiteSpace Element position to 0
    if FHighlighter.Attribute[I] = FHighlighter.WhitespaceAttribute then
      BoxElements.Items.Move(BoxElements.Items.Count-1, 0);
  end;
  BoxElements.ItemIndex := 0;
  RefreshMap;
end;

procedure TfmHighLightSettings.BoxElementsClick(Sender: TObject);
begin
  RefreshMap;
end;

procedure TfmHighLightSettings.RefreshColorGrid;
begin
  with CurrentElement do
  begin
    if ForeGround <> clNone then
    begin
      ColorGrid.ForegroundEnabled := True;
      ColorGrid.ForegroundIndex := ColorGrid.ColorToIndex(ForeGround);
    end
    else
    begin
      ColorGrid.ForegroundEnabled := False;
      ColorGrid.ForegroundIndex := -1;
    end;
    if Background <> clNone then
    begin
      ColorGrid.BackgroundEnabled := True;
      ColorGrid.BackgroundIndex := ColorGrid.ColorToIndex(BackGround);
    end
    else
    begin
      ColorGrid.BackgroundEnabled := False;
      ColorGrid.BackgroundIndex := -1;
    end;
  end;
  RefreshEditorColor(SynEdit);
end;

procedure TfmHighLightSettings.RefreshDefaultCheckBox;
begin
  with CurrentElement do
  begin
    cbForeground.OnClick := nil;
    cbBackground.OnClick := nil;
    Try
      cbForeground.Checked := ForeGround = clNone;
      cbForeground.Enabled := not cbForeground.Checked;
      cbBackground.Checked := Background = clNone;
      cbBackground.Enabled := not cbBackground.Checked;
    Finally
      cbForeground.OnClick := cbForegroundClick;
      cbBackground.OnClick := cbBackgroundClick;
    End;
  end;
end;

procedure TfmHighLightSettings.RefreshTextAttributes;
begin
  with CurrentElement do
  begin
    //Text Attributes
    cbBold.Checked := fsBold in Style;
    cbItalic.Checked := fsItalic in Style;
    cbUnderline.Checked := fsUnderline in Style;
    cbStrikeOut.Checked := fsStrikeOut in Style;
  end;
end;

procedure TfmHighLightSettings.RefreshMap;
begin
  //imposta la mappa sulla base delle impostazioni della lista
  with CurrentElement do
  begin
    RefreshColorGrid;
    RefreshDefaultCheckBox;
    RefreshTextAttributes;
    gbWhiteSpace.Visible := CurrentElement <> FHighlighter.WhitespaceAttribute;
  end;
end;

function TfmHighLightSettings.GetCurrentElement: TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes(BoxElements.Items.Objects[BoxElements.ItemIndex]);
end;

procedure TfmHighLightSettings.cbForegroundClick(Sender: TObject);
begin
  if cbForeground.Checked then
    CurrentElement.Foreground := clNone
  else
  begin
    ColorGrid.ForegroundIndex := ColorGrid.ColorToIndex(ForeGroundColor);
    CurrentElement.Foreground := ForeGroundColor;
  end;
  RefreshDefaultCheckBox;
  RefreshColorGrid;
end;

procedure TfmHighLightSettings.cbBackgroundClick(Sender: TObject);
begin
  if cbBackground.Checked then
    CurrentElement.Background := clNone
  else
  begin
    ColorGrid.BackgroundIndex := ColorGrid.ColorToIndex(BackGroundColor);
    CurrentElement.Background := BackGroundColor;
  end;
  RefreshDefaultCheckBox;
  RefreshColorGrid;
end;

procedure TfmHighLightSettings.ColorGridChange;
begin
  Try
    cbForeground.OnClick := nil;
    cbBackground.OnClick := nil;
    if ColorGrid.ForegroundEnabled then
    begin
      CurrentElement.Foreground := ColorGrid.ForegroundColor;
    end
    else
      CurrentElement.Foreground := clNone;

    if ColorGrid.BackgroundEnabled then
    begin
      CurrentElement.Background := ColorGrid.BackgroundColor;
    end
    else
      CurrentElement.Background := clNone;
    RefreshDefaultCheckBox;
  Finally
    cbForeground.OnClick := cbForegroundClick;
    cbBackground.OnClick := cbBackgroundClick;
  End;
end;

procedure TfmHighLightSettings.cbFontStyleClick(Sender: TObject);
var
  FontStyle : TFontStyle;
begin
  if Sender = cbBold then
    FontStyle := fsBold
  else if Sender = cbItalic then
    FontStyle := fsItalic
  else if Sender = cbUnderline then
    FontStyle := fsUnderline
  else if Sender = cbStrikeOut then
    FontStyle := fsStrikeOut
  else
    Exit;

  with (Sender as Tcheckbox) do
  begin
    if Checked then
      CurrentElement.Style := CurrentElement.Style + [fontStyle]
    else
      CurrentElement.Style := CurrentElement.Style - [fontStyle];
  end;
  RefreshMap;
end;

procedure TfmHighLightSettings.GetActiveAttribute;
var
  Token : UnicodeString;
  Attr : TSynHighlighterAttributes;
begin
  //Recupera l'attributo attivo a partire dall'editor
  Token := '';
  SynEdit.GetHighlighterAttriAtRowCol(SynEdit.CaretXY,Token,Attr);
  if Attr <> nil then
  begin
    BoxElements.ItemIndex := BoxElements.Items.IndexOf(Attr.Name);
    RefreshMap;
  end
  else
    BoxElements.ItemIndex := 0; //goto WiteSpace Element
end;

procedure TfmHighLightSettings.SynEditClick(Sender: TObject);
begin
  GetActiveAttribute;
end;

procedure TfmHighLightSettings.SynEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GetActiveAttribute;
end;

procedure TfmHighLightSettings.ColorGridClick(Sender: TObject);
begin
  ColorGridChange;
  RefreshEditorColor(SynEdit);
end;

function TfmHighLightSettings.GetBackGroundColor: TColor;
begin
  Result := FHighlighter.WhitespaceAttribute.Background;
end;

function TfmHighLightSettings.GetForeGroundColor: TColor;
begin
  Result := FHighlighter.WhitespaceAttribute.Foreground;
end;

procedure TfmHighLightSettings.LoadButtonClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Editor color files|*.ini';
  OpenDialog.DefaultExt := '.ini';
  OpenDialog.InitialDir :=  ExtractFilePath(FFileName);
  if OpenDialog.Execute then
    FHighlighter.LoadFromFile(OpenDialog.FileName);
end;

procedure TfmHighLightSettings.btOKClick(Sender: TObject);
begin
  //Salva i parametri su file
  FHighlighter.SaveToFile(FFileName);
  CloneSynEdit(SynEdit, FSourceSynEdit);
end;

procedure TfmHighLightSettings.CloneSynEdit(Source, Dest: TSynEdit);
begin
  Dest.Highlighter.Assign(Source.Highlighter);
  Dest.Font.Assign(Source.Font);
  Dest.Color := Source.Color;
end;

end.
