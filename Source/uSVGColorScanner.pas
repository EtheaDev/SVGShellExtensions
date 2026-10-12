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
unit uSVGColorScanner;

//Finds every color value written in the text of an SVG document, together with
//its position in the text, and groups them into a palette.
//The scan works on the text and not on a parsed SVG tree on purpose: the SVG
//parser does not keep the position of the nodes, and the editor needs the
//exact line/char of every value to highlight it and to replace it.
//The unit has no VCL dependencies so it can be tested on its own.

interface

uses
  System.SysUtils,
  System.UITypes;

type
  TSVGColorFormat = (cfHex3, cfHex4, cfHex6, cfHex8, cfRGB, cfRGBA,
    cfHSL, cfHSLA, cfNamed);

  TSVGColorOccurrence = record
    Offset: Integer;        //1-based position in the scanned text
    Line: Integer;          //1-based line (TBufferCoord.Line)
    CharStart: Integer;     //1-based char in the line (TBufferCoord.Char)
    CharEnd: Integer;       //1-based char after the last char of the value
    RawText: string;        //the value as written: '#F00', 'rgb(255,0,0)', 'red'
    Color: TAlphaColor;     //$AARRGGBB
    Format: TSVGColorFormat;
    PropertyName: string;   //'fill', 'stroke', 'stop-color', ...
    function Length: Integer;
  end;
  TSVGColorOccurrences = TArray<TSVGColorOccurrence>;

  //One entry of the palette: the occurrences are grouped by RGB, ignoring
  //the alpha, so '#F00', '#ff0000', 'red' and '#FF000080' are the same entry.
  TSVGPaletteEntry = record
    Color: TAlphaColor;              //opaque RGB of the group
    Occurrences: TArray<Integer>;    //indexes in the occurrences array
    PropertyNames: TArray<string>;   //distinct properties using the color
    Alphas: TArray<Byte>;            //distinct alpha values used
    function Count: Integer;
  end;
  TSVGPalette = TArray<TSVGPaletteEntry>;

  TSVGColorScanner = class
  public
    //Scans the whole text of an SVG document. The occurrences are returned in
    //text order.
    class function Scan(const AText: string): TSVGColorOccurrences; static;
    //Groups the occurrences by RGB, in order of first appearance.
    class function BuildPalette(
      const AOccurrences: TSVGColorOccurrences): TSVGPalette; static;
    //Index of the palette entry with the given color (alpha ignored), or -1.
    class function IndexOfColor(const APalette: TSVGPalette;
      const AColor: TAlphaColor): Integer; static;
    //Index of the occurrence containing the given text position, or -1.
    class function OccurrenceAt(const AOccurrences: TSVGColorOccurrences;
      const ALine, AChar: Integer): Integer; static;
    //Parses a single color value (hex, rgb(), rgba(), hsl(), hsla(), name).
    class function TryParseColor(const AValue: string; out AColor: TAlphaColor;
      out AFormat: TSVGColorFormat): Boolean; static;
    //Writes a color using the same notation of an existing occurrence, so a
    //replacement keeps the style of the document: hex stays hex (with the same
    //case and, when possible, the same length), rgb() stays rgb(), a name
    //stays a name when the new color has one.
    class function FormatColorLike(const AColor: TAlphaColor;
      const AOriginal: TSVGColorOccurrence): string; static;
    //'#RRGGBB', or '#RRGGBBAA' when the color is not opaque.
    class function ColorToHex(const AColor: TAlphaColor): string; static;
    //Name of the color (e.g. 'red'), or '' when the color has no name.
    class function ColorToName(const AColor: TAlphaColor): string; static;
  end;

function AlphaColorToColor(const AColor: TAlphaColor): TColor;
function ColorToAlphaColor(const AColor: TColor): TAlphaColor;
function OpaqueColor(const AColor: TAlphaColor): TAlphaColor; inline;
function AlphaOf(const AColor: TAlphaColor): Byte; inline;

implementation

uses
  System.Math,
  System.Classes,
  System.RegularExpressions,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  //Same layout of the record used by Image32, so its table of the named
  //colors can be included as it is instead of duplicating it here.
  TColorConst = record
    ColorName: string;
    ColorValue: Cardinal;
  end;

{$I '..\Ext\SVGIconImageList\Image32\source\Img32.SVG.HtmlColorConsts.inc'}

const
  //Properties holding a color, both as presentation attributes and as CSS
  COLOR_PROPERTIES = 'fill|stroke|stop-color|flood-color|lighting-color|color|solid-color';

  //fill="..." / stroke='...': the lookbehind avoids data-fill, my-color, ...
  ATTRIBUTE_PATTERN = '(?<=\s)(' + COLOR_PROPERTIES + ')\s*=\s*(?:"([^"]*)"|''([^'']*)'')';
  //style="..." attribute
  STYLE_ATTR_PATTERN = '(?<=\s)style\s*=\s*(?:"([^"]*)"|''([^'']*)'')';
  //<style> element
  STYLE_ELEMENT_PATTERN = '<style\b[^>]*>(.*?)</style\s*>';
  //CSS declaration (inside a style attribute or a <style> element)
  DECLARATION_PATTERN = '(?<![\w.#-])(' + COLOR_PROPERTIES + ')\s*:\s*([^;{}"'']*)';
  //Tokens of a property value. url(...) is matched only to be skipped, so
  //that the id of a gradient (e.g. url(#red)) is not taken as a color.
  VALUE_TOKEN_PATTERN = 'url\s*\([^)]*\)|#[0-9a-f]+\b|(?:rgba?|hsla?)\s*\([^)]*\)|(?<![\w-])[a-z]+(?![\w-])';
  //Ranges that cannot contain colors: replaced with blanks before the scan
  XML_COMMENT_PATTERN = '<!--.*?-->';
  CSS_COMMENT_PATTERN = '/\*.*?\*/';

var
  NamedColors: TDictionary<string, TAlphaColor>;
  AttributeRegEx, StyleAttrRegEx, StyleElementRegEx,
  DeclarationRegEx, ValueTokenRegEx,
  XmlCommentRegEx, CssCommentRegEx: TRegEx;

function AlphaColorToColor(const AColor: TAlphaColor): TColor;
begin
  //$AARRGGBB -> $00BBGGRR
  Result := TColor(((AColor and $FF) shl 16) or (AColor and $FF00) or
    ((AColor shr 16) and $FF));
end;

function ColorToAlphaColor(const AColor: TColor): TAlphaColor;
var
  LRGB: Cardinal;
begin
  LRGB := Cardinal(AColor) and $FFFFFF;
  Result := $FF000000 or ((LRGB and $FF) shl 16) or (LRGB and $FF00) or
    ((LRGB shr 16) and $FF);
end;

function OpaqueColor(const AColor: TAlphaColor): TAlphaColor;
begin
  Result := AColor or $FF000000;
end;

function AlphaOf(const AColor: TAlphaColor): Byte;
begin
  Result := AColor shr 24;
end;

function MakeColor(const R, G, B, A: Integer): TAlphaColor;
begin
  Result := (Cardinal(EnsureRange(A, 0, 255)) shl 24) or
    (Cardinal(EnsureRange(R, 0, 255)) shl 16) or
    (Cardinal(EnsureRange(G, 0, 255)) shl 8) or
    Cardinal(EnsureRange(B, 0, 255));
end;

//Replaces every char of the matches with a blank, keeping line breaks, so
//the offsets of the remaining text do not change.
function BlankMatches(const AText: string; const ARegEx: TRegEx): string;
var
  LMatch: TMatch;
  I: Integer;
begin
  Result := AText;
  for LMatch in ARegEx.Matches(AText) do
    for I := LMatch.Index to LMatch.Index + LMatch.Length - 1 do
      if not CharInSet(Result[I], [#10, #13]) then
        Result[I] := ' ';
end;

{ TSVGColorOccurrence }

function TSVGColorOccurrence.Length: Integer;
begin
  Result := CharEnd - CharStart;
end;

{ TSVGPaletteEntry }

function TSVGPaletteEntry.Count: Integer;
begin
  Result := System.Length(Occurrences);
end;

{ Color parsing }

function TryParseNumber(const AValue: string; out ANumber: Double;
  out AIsPercent: Boolean): Boolean;
var
  LValue: string;
begin
  LValue := Trim(AValue);
  AIsPercent := LValue.EndsWith('%');
  if AIsPercent then
    LValue := Copy(LValue, 1, System.Length(LValue) - 1)
  else if LValue.EndsWith('deg', True) then
    LValue := Copy(LValue, 1, System.Length(LValue) - 3);
  Result := TryStrToFloat(LValue, ANumber, TFormatSettings.Invariant);
end;

//Splits the arguments of rgb(...)/hsl(...): both the legacy syntax with commas
//and the modern one with blanks and a slash before the alpha are accepted.
function SplitArguments(const AValue: string; out AArgs: TArray<string>): Boolean;
var
  LOpen, LClose: Integer;
  LInner: string;
begin
  LOpen := Pos('(', AValue);
  LClose := AValue.LastIndexOf(')') + 1;
  Result := (LOpen > 0) and (LClose > LOpen);
  if not Result then
    Exit;
  LInner := Copy(AValue, LOpen + 1, LClose - LOpen - 1);
  AArgs := TRegEx.Split(Trim(LInner), '\s*[,/]\s*|\s+');
  Result := System.Length(AArgs) in [3, 4];
end;

function AlphaFromArgument(const AArg: string; out AAlpha: Integer): Boolean;
var
  LNumber: Double;
  LIsPercent: Boolean;
begin
  Result := TryParseNumber(AArg, LNumber, LIsPercent);
  if Result then
  begin
    if LIsPercent then
      LNumber := LNumber / 100;
    AAlpha := Round(EnsureRange(LNumber, 0, 1) * 255);
  end;
end;

function HueToRGB(const P, Q: Double; T: Double): Double;
begin
  if T < 0 then T := T + 1;
  if T > 1 then T := T - 1;
  if T < 1/6 then
    Result := P + (Q - P) * 6 * T
  else if T < 1/2 then
    Result := Q
  else if T < 2/3 then
    Result := P + (Q - P) * (2/3 - T) * 6
  else
    Result := P;
end;

procedure HSLToRGB(const H, S, L: Double; out R, G, B: Integer);
var
  LHue, P, Q: Double;
begin
  if S = 0 then
  begin
    R := Round(L * 255);
    G := R;
    B := R;
  end
  else
  begin
    LHue := H / 360;
    LHue := LHue - Floor(LHue);
    if L < 0.5 then
      Q := L * (1 + S)
    else
      Q := L + S - L * S;
    P := 2 * L - Q;
    R := Round(HueToRGB(P, Q, LHue + 1/3) * 255);
    G := Round(HueToRGB(P, Q, LHue) * 255);
    B := Round(HueToRGB(P, Q, LHue - 1/3) * 255);
  end;
end;

procedure RGBToHSL(const AColor: TAlphaColor; out H, S, L: Double);
var
  R, G, B, LMax, LMin, LDelta: Double;
begin
  R := ((AColor shr 16) and $FF) / 255;
  G := ((AColor shr 8) and $FF) / 255;
  B := (AColor and $FF) / 255;
  LMax := Max(R, Max(G, B));
  LMin := Min(R, Min(G, B));
  L := (LMax + LMin) / 2;
  LDelta := LMax - LMin;
  if LDelta = 0 then
  begin
    H := 0;
    S := 0;
  end
  else
  begin
    if L > 0.5 then
      S := LDelta / (2 - LMax - LMin)
    else
      S := LDelta / (LMax + LMin);
    if LMax = R then
      H := (G - B) / LDelta + IfThen(G < B, 6, 0)
    else if LMax = G then
      H := (B - R) / LDelta + 2
    else
      H := (R - G) / LDelta + 4;
    H := H * 60;
  end;
end;

function TryParseHex(const AValue: string; out AColor: TAlphaColor;
  out AFormat: TSVGColorFormat): Boolean;
var
  LHex: string;
  LValue: Cardinal;
  R, G, B, A: Integer;
begin
  LHex := Copy(AValue, 2, MaxInt);
  Result := (System.Length(LHex) in [3, 4, 6, 8]) and
    TryStrToUInt('$' + LHex, LValue);
  if not Result then
    Exit;
  case System.Length(LHex) of
    3, 4:
    begin
      //#RGB(A): every digit is doubled
      if System.Length(LHex) = 4 then
      begin
        AFormat := cfHex4;
        A := (LValue and $F) * $11;
        LValue := LValue shr 4;
      end
      else
      begin
        AFormat := cfHex3;
        A := 255;
      end;
      R := ((LValue shr 8) and $F) * $11;
      G := ((LValue shr 4) and $F) * $11;
      B := (LValue and $F) * $11;
    end;
    6:
    begin
      AFormat := cfHex6;
      A := 255;
      R := (LValue shr 16) and $FF;
      G := (LValue shr 8) and $FF;
      B := LValue and $FF;
    end;
  else
    //#RRGGBBAA
    AFormat := cfHex8;
    A := LValue and $FF;
    R := (LValue shr 24) and $FF;
    G := (LValue shr 16) and $FF;
    B := (LValue shr 8) and $FF;
  end;
  AColor := MakeColor(R, G, B, A);
end;

function TryParseRGB(const AValue: string; out AColor: TAlphaColor;
  out AFormat: TSVGColorFormat): Boolean;
var
  LArgs: TArray<string>;
  LRGB: array[0..2] of Integer;
  LNumber: Double;
  LIsPercent: Boolean;
  I, A: Integer;
begin
  Result := SplitArguments(AValue, LArgs);
  if not Result then
    Exit;
  for I := 0 to 2 do
  begin
    Result := TryParseNumber(LArgs[I], LNumber, LIsPercent);
    if not Result then
      Exit;
    if LIsPercent then
      LNumber := LNumber * 255 / 100;
    LRGB[I] := Round(EnsureRange(LNumber, 0, 255));
  end;
  A := 255;
  if System.Length(LArgs) = 4 then
  begin
    Result := AlphaFromArgument(LArgs[3], A);
    if not Result then
      Exit;
  end;
  if AValue.StartsWith('rgba', True) then
    AFormat := cfRGBA
  else
    AFormat := cfRGB;
  AColor := MakeColor(LRGB[0], LRGB[1], LRGB[2], A);
end;

function TryParseHSL(const AValue: string; out AColor: TAlphaColor;
  out AFormat: TSVGColorFormat): Boolean;
var
  LArgs: TArray<string>;
  H, S, L: Double;
  LIsPercent: Boolean;
  R, G, B, A: Integer;
begin
  Result := SplitArguments(AValue, LArgs) and
    TryParseNumber(LArgs[0], H, LIsPercent) and
    TryParseNumber(LArgs[1], S, LIsPercent) and
    TryParseNumber(LArgs[2], L, LIsPercent);
  if not Result then
    Exit;
  A := 255;
  if System.Length(LArgs) = 4 then
  begin
    Result := AlphaFromArgument(LArgs[3], A);
    if not Result then
      Exit;
  end;
  HSLToRGB(H, EnsureRange(S / 100, 0, 1), EnsureRange(L / 100, 0, 1), R, G, B);
  if AValue.StartsWith('hsla', True) then
    AFormat := cfHSLA
  else
    AFormat := cfHSL;
  AColor := MakeColor(R, G, B, A);
end;

{ TSVGColorScanner }

class function TSVGColorScanner.TryParseColor(const AValue: string;
  out AColor: TAlphaColor; out AFormat: TSVGColorFormat): Boolean;
var
  LValue: string;
begin
  LValue := Trim(AValue);
  if LValue = '' then
    Exit(False);
  if LValue[1] = '#' then
    Result := TryParseHex(LValue, AColor, AFormat)
  else if LValue.StartsWith('rgb', True) then
    Result := TryParseRGB(LValue, AColor, AFormat)
  else if LValue.StartsWith('hsl', True) then
    Result := TryParseHSL(LValue, AColor, AFormat)
  else
  begin
    //'none', 'transparent' and 'currentColor' are not in the dictionary
    Result := NamedColors.TryGetValue(LowerCase(LValue), AColor);
    AFormat := cfNamed;
  end;
end;

class function TSVGColorScanner.Scan(const AText: string): TSVGColorOccurrences;
var
  LMasked: string;
  LLineStarts: TList<Integer>;
  LList: TList<TSVGColorOccurrence>;

  procedure BuildLineStarts;
  var
    I: Integer;
  begin
    LLineStarts.Add(1);
    I := 1;
    while I <= System.Length(AText) do
    begin
      if AText[I] = #13 then
      begin
        if (I < System.Length(AText)) and (AText[I + 1] = #10) then
          Inc(I);
        LLineStarts.Add(I + 1);
      end
      else if AText[I] = #10 then
        LLineStarts.Add(I + 1);
      Inc(I);
    end;
  end;

  function LineOfOffset(const AOffset: Integer): Integer;
  var
    LLow, LHigh, LMid: Integer;
  begin
    //Binary search of the last line starting at or before AOffset (0-based)
    LLow := 0;
    LHigh := LLineStarts.Count - 1;
    while LLow < LHigh do
    begin
      LMid := (LLow + LHigh + 1) div 2;
      if LLineStarts[LMid] <= AOffset then
        LLow := LMid
      else
        LHigh := LMid - 1;
    end;
    Result := LLow;
  end;

  procedure ScanValue(const AProperty: string; const AValueOffset: Integer;
    const AValue: string);
  var
    LToken: TMatch;
    LOccurrence: TSVGColorOccurrence;
    LLineIndex: Integer;
  begin
    for LToken in ValueTokenRegEx.Matches(AValue) do
    begin
      if LToken.Value.StartsWith('url', True) and (Pos('(', LToken.Value) > 0) then
        Continue;
      if not TryParseColor(LToken.Value, LOccurrence.Color, LOccurrence.Format) then
        Continue;
      LOccurrence.Offset := AValueOffset + LToken.Index - 1;
      LOccurrence.RawText := LToken.Value;
      LOccurrence.PropertyName := LowerCase(AProperty);
      LLineIndex := LineOfOffset(LOccurrence.Offset);
      LOccurrence.Line := LLineIndex + 1;
      LOccurrence.CharStart := LOccurrence.Offset - LLineStarts[LLineIndex] + 1;
      LOccurrence.CharEnd := LOccurrence.CharStart + LToken.Length;
      LList.Add(LOccurrence);
    end;
  end;

  //Scans the CSS declarations of a style attribute or of a <style> element
  procedure ScanDeclarations(const AOffset: Integer; const ACss: string);
  var
    LDeclaration: TMatch;
  begin
    for LDeclaration in DeclarationRegEx.Matches(ACss) do
      ScanValue(LDeclaration.Groups[1].Value,
        AOffset + LDeclaration.Groups[2].Index - 1,
        LDeclaration.Groups[2].Value);
  end;

  //Value group of an attribute matched as "..." (AFirst) or '...' (AFirst+1)
  function AttributeValue(const AMatch: TMatch; const AFirst: Integer;
    out AOffset: Integer): string;
  var
    LGroup: Integer;
  begin
    for LGroup := AFirst to AFirst + 1 do
      if (LGroup < AMatch.Groups.Count) and AMatch.Groups[LGroup].Success and
        (AMatch.Groups[LGroup].Length > 0) then
      begin
        AOffset := AMatch.Groups[LGroup].Index;
        Exit(AMatch.Groups[LGroup].Value);
      end;
    AOffset := 0;
    Result := '';
  end;

var
  LMatch: TMatch;
  LValue: string;
  LValueOffset: Integer;
begin
  Result := nil;
  if AText = '' then
    Exit;
  LLineStarts := TList<Integer>.Create;
  LList := TList<TSVGColorOccurrence>.Create;
  try
    BuildLineStarts;
    LMasked := BlankMatches(AText, XmlCommentRegEx);

    //Presentation attributes: fill="red"
    for LMatch in AttributeRegEx.Matches(LMasked) do
    begin
      LValue := AttributeValue(LMatch, 2, LValueOffset);
      if LValue <> '' then
        ScanValue(LMatch.Groups[1].Value, LValueOffset, LValue);
    end;

    //Style attributes: style="fill:red;stroke:blue"
    for LMatch in StyleAttrRegEx.Matches(LMasked) do
    begin
      LValue := AttributeValue(LMatch, 1, LValueOffset);
      if LValue <> '' then
        ScanDeclarations(LValueOffset, LValue);
    end;

    //<style> elements: .cls{fill:red}
    for LMatch in StyleElementRegEx.Matches(LMasked) do
      if LMatch.Groups.Count > 1 then
        ScanDeclarations(LMatch.Groups[1].Index,
          BlankMatches(LMatch.Groups[1].Value, CssCommentRegEx));

    LList.Sort(TComparer<TSVGColorOccurrence>.Construct(
      function(const A, B: TSVGColorOccurrence): Integer
      begin
        Result := A.Offset - B.Offset;
      end));
    Result := LList.ToArray;
  finally
    LList.Free;
    LLineStarts.Free;
  end;
end;

class function TSVGColorScanner.BuildPalette(
  const AOccurrences: TSVGColorOccurrences): TSVGPalette;
var
  LIndexes: TDictionary<TAlphaColor, Integer>;
  LEntry: Integer;
  I: Integer;
  LKey: TAlphaColor;
  LAlpha: Byte;
begin
  Result := nil;
  LIndexes := TDictionary<TAlphaColor, Integer>.Create;
  try
    for I := 0 to High(AOccurrences) do
    begin
      LKey := OpaqueColor(AOccurrences[I].Color);
      if not LIndexes.TryGetValue(LKey, LEntry) then
      begin
        LEntry := System.Length(Result);
        SetLength(Result, LEntry + 1);
        Result[LEntry].Color := LKey;
        LIndexes.Add(LKey, LEntry);
      end;
      Result[LEntry].Occurrences := Result[LEntry].Occurrences + [I];
      if TArray.IndexOf<string>(Result[LEntry].PropertyNames,
        AOccurrences[I].PropertyName) < 0 then
        Result[LEntry].PropertyNames := Result[LEntry].PropertyNames +
          [AOccurrences[I].PropertyName];
      LAlpha := AlphaOf(AOccurrences[I].Color);
      if TArray.IndexOf<Byte>(Result[LEntry].Alphas, LAlpha) < 0 then
        Result[LEntry].Alphas := Result[LEntry].Alphas + [LAlpha];
    end;
  finally
    LIndexes.Free;
  end;
end;

class function TSVGColorScanner.IndexOfColor(const APalette: TSVGPalette;
  const AColor: TAlphaColor): Integer;
var
  I: Integer;
begin
  for I := 0 to High(APalette) do
    if APalette[I].Color = OpaqueColor(AColor) then
      Exit(I);
  Result := -1;
end;

class function TSVGColorScanner.OccurrenceAt(
  const AOccurrences: TSVGColorOccurrences;
  const ALine, AChar: Integer): Integer;
var
  I: Integer;
begin
  //The caret is "on" a value also when it is right after its last char
  for I := 0 to High(AOccurrences) do
  begin
    if AOccurrences[I].Line > ALine then
      Break;
    if (AOccurrences[I].Line = ALine) and
      (AChar >= AOccurrences[I].CharStart) and
      (AChar <= AOccurrences[I].CharEnd) then
      Exit(I);
  end;
  Result := -1;
end;

class function TSVGColorScanner.ColorToHex(const AColor: TAlphaColor): string;
begin
  Result := '#' + IntToHex(AColor and $FFFFFF, 6);
  if AlphaOf(AColor) <> 255 then
    Result := Result + IntToHex(AlphaOf(AColor), 2);
end;

class function TSVGColorScanner.ColorToName(const AColor: TAlphaColor): string;
var
  I: Integer;
begin
  Result := '';
  if AlphaOf(AColor) <> 255 then
    Exit;
  for I := Low(ColorConsts) to High(ColorConsts) do
    if (ColorConsts[I].ColorValue = AColor) and
      NamedColors.ContainsKey(ColorConsts[I].ColorName) then
      Exit(ColorConsts[I].ColorName);
end;

class function TSVGColorScanner.FormatColorLike(const AColor: TAlphaColor;
  const AOriginal: TSVGColorOccurrence): string;
var
  R, G, B, A: Byte;
  LUpperCase, LSpaced: Boolean;
  LSeparator: string;
  H, S, L: Double;
  LParsed: TAlphaColor;
  LFormat: TSVGColorFormat;

  function IsShortHex: Boolean;
  begin
    //Every channel made of two equal digits: $11, $22, ... $FF
    Result := (R mod $11 = 0) and (G mod $11 = 0) and (B mod $11 = 0) and
      (A mod $11 = 0);
  end;

  function AlphaText: string;
  begin
    Result := FormatFloat('0.##', A / 255, TFormatSettings.Invariant);
  end;

begin
  R := (AColor shr 16) and $FF;
  G := (AColor shr 8) and $FF;
  B := AColor and $FF;
  A := AlphaOf(AColor);
  //Keep the case of the original hex digits; digits only -> upper case
  LUpperCase := AOriginal.RawText = UpperCase(AOriginal.RawText);
  LSpaced := Pos(', ', AOriginal.RawText) > 0;
  if LSpaced then
    LSeparator := ', '
  else
    LSeparator := ',';
  case AOriginal.Format of
    cfHex3, cfHex4:
      if IsShortHex then
      begin
        Result := '#' + IntToHex(R div $11, 1) + IntToHex(G div $11, 1) +
          IntToHex(B div $11, 1);
        if A <> 255 then
          Result := Result + IntToHex(A div $11, 1);
      end
      else
        Result := ColorToHex(AColor);
    cfHex6, cfHex8:
      Result := ColorToHex(AColor);
    cfRGB, cfRGBA:
      if A = 255 then
        Result := Format('rgb(%d%s%d%s%d)', [R, LSeparator, G, LSeparator, B])
      else
        Result := Format('rgba(%d%s%d%s%d%s%s)',
          [R, LSeparator, G, LSeparator, B, LSeparator, AlphaText]);
    cfHSL, cfHSLA:
    begin
      RGBToHSL(AColor, H, S, L);
      if A = 255 then
        Result := Format('hsl(%d%s%d%%%s%d%%)',
          [Round(H), LSeparator, Round(S * 100), LSeparator, Round(L * 100)])
      else
        Result := Format('hsla(%d%s%d%%%s%d%%%s%s)',
          [Round(H), LSeparator, Round(S * 100), LSeparator, Round(L * 100),
          LSeparator, AlphaText]);
      //hsl() with integer values cannot express every RGB color: fall back to
      //hex rather than writing a slightly different color
      if not TryParseHSL(Result, LParsed, LFormat) or
        (OpaqueColor(LParsed) <> OpaqueColor(AColor)) then
        Result := ColorToHex(AColor);
    end;
  else
    //cfNamed
    Result := ColorToName(AColor);
    if Result = '' then
      Result := ColorToHex(AColor);
  end;
  if Result.StartsWith('#') then
  begin
    if LUpperCase or not (AOriginal.Format in [cfHex3, cfHex4, cfHex6, cfHex8]) then
      Result := UpperCase(Result)
    else
      Result := LowerCase(Result);
  end;
end;

procedure InitializeScanner;
var
  I: Integer;
  LName: string;
begin
  NamedColors := TDictionary<string, TAlphaColor>.Create;
  for I := Low(ColorConsts) to High(ColorConsts) do
  begin
    LName := ColorConsts[I].ColorName;
    //Keywords that are not real colors: they are not shown in the palette
    if (LName = 'none') or (LName = 'currentcolor') or (LName = 'transparent') then
      Continue;
    NamedColors.AddOrSetValue(LName, ColorConsts[I].ColorValue);
  end;
  AttributeRegEx := TRegEx.Create(ATTRIBUTE_PATTERN, [roIgnoreCase]);
  StyleAttrRegEx := TRegEx.Create(STYLE_ATTR_PATTERN, [roIgnoreCase]);
  StyleElementRegEx := TRegEx.Create(STYLE_ELEMENT_PATTERN, [roIgnoreCase, roSingleLine]);
  DeclarationRegEx := TRegEx.Create(DECLARATION_PATTERN, [roIgnoreCase]);
  ValueTokenRegEx := TRegEx.Create(VALUE_TOKEN_PATTERN, [roIgnoreCase]);
  XmlCommentRegEx := TRegEx.Create(XML_COMMENT_PATTERN, [roSingleLine]);
  CssCommentRegEx := TRegEx.Create(CSS_COMMENT_PATTERN, [roSingleLine]);
end;

initialization
  InitializeScanner;

finalization
  FreeAndNil(NamedColors);

end.
