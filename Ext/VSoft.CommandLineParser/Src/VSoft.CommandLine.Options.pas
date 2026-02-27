{***************************************************************************}
{                                                                           }
{           VSoft.CommandLine                                               }
{                                                                           }
{           Copyright (C) 2014 Vincent Parrett                              }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}
unit VSoft.CommandLine.Options;
interface
uses
  Generics.Collections,
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  System.Classes;
type
  TConstProc<T> = reference to procedure (const Arg1: T);
  TConstProc<T,T2> = reference to procedure (const Arg1: T; const Arg2 : T2);
  TConstProc<T,T2, T3> = reference to procedure (const Arg1: T; const Arg2 : T2; const Arg3 : T3);
  ICommandLineParseResult = interface
    ['{1715B9FF-8A34-47C9-843E-619C5AEA3F32}']
    function GetHasErrors : boolean;
    function GetErrorText : string;
    function GetCommandName : string;
    property HasErrors : boolean read GetHasErrors;
    property ErrorText : string read GetErrorText;
    property Command : string read GetCommandName;
  end;
  ICommandLineParser = interface
    ['{6F970026-D1EE-4A3E-8A99-300AD3EE9C33}']
    //parses the command line
    function Parse : ICommandLineParseResult;overload;
    //parses the passed in string - makes testing easier.
    function Parse(const values : TStrings) : ICommandLineParseResult;overload;
  end;

  IOptionDefinition = interface
  ['{1EAA06BA-8FBF-43F8-86D7-9F5DE26C4E86}']
    function GetLongName : string;
    function GetShortName : string;
    function GetHasValue : boolean;
    procedure SetHasValue(const value : boolean);
    function GetHelpText : string;
    procedure SetHelpText(const value : string);
    function GetRequired : boolean;
    procedure SetRequired(const value : boolean);
    function GetValueRequired : boolean;
    procedure SetValueRequired(const value : boolean);
    function GetAllowMultiple : boolean;
    procedure SetAllowMultiple(const value : boolean);
    function GetIsOptionFile : boolean;
    procedure SetIsOptionFile(const value : boolean);
    function GetIsHidden : boolean;
    procedure SetIsHidden(const value : boolean);
    function GetIsUnnamed : boolean;
    property LongName       : string read GetLongName;
    property ShortName      : string read GetShortName;
    property HasValue       : boolean read GetHasValue write SetHasValue;
    property HelpText       : string read GetHelpText write SetHelpText;
    property Required       : boolean read GetRequired write SetRequired;
    property ValueRequired  : boolean read GetValueRequired write SetValueRequired;
    property AllowMultiple  : boolean read GetAllowMultiple write SetAllowMultiple;
    property IsOptionFile   : boolean read GetIsOptionFile write SetIsOptionFile;
    property IsUnnamed      : boolean read GetIsUnnamed;
    property Hidden         : boolean read GetIsHidden write SetIsHidden;
  end;

  ICommandDefinition = interface
  ['{58199FE2-19DF-4F9B-894F-BD1C5B62E0CB}']
    function GetRegisteredOptions : TList<IOptionDefinition>;
    function GetUnNamedOptions  : TList<IOptionDefinition>;
    procedure GetAllRegisteredOptions(const list : TList<IOptionDefinition>);
    function GetName : string;
    function GetAlias : string;
    function GetDescription : string;
    function GetHelpText : string;
    function GetUsage : string;
    function GetVisible : boolean;
    function GetIsDefault : boolean;
    function GetExamples : TList<string>;
    procedure AddOption(const value : IOptionDefinition);
    function TryGetOption(const name : string; var option : IOptionDefinition) : boolean;
    function HasOption(const name : string) : boolean;
    procedure Clear;
    procedure EnumerateCommandOptions(const proc : TConstProc<string,string, string>);overload;
    procedure EnumerateCommandOptions(const proc : TConstProc<IOptionDefinition>);overload;
    property Name : string read GetName;
    property Alias : string read GetAlias;
    property Description : string read GetDescription;
    property HelpText : string read GetHelpText;
    property IsDefault : boolean read GetIsDefault;
    property Usage : string read GetUsage;
    property RegisteredOptions : TList<IOptionDefinition> read GetRegisteredOptions;
    property RegisteredUnamedOptions : TList<IOptionDefinition> read GetUnNamedOptions;
    property Visible : boolean read GetVisible;
    property Examples : TList<string> read GetExamples;
  end;
  //Using a record here because non generic interfaces cannot have generic methods!! Still!
  //The actual command implementation is elsewhere.
  TCommandDefinition = record
  private
    FCommandDef : ICommandDefinition;
    function GetName: string;
    function GetDescription : string;
    function GetUsage : string;
    function GetAlias : string;
    function GetExamples : TList<string>;
  public
    function RegisterOption<T>(const longName: string; const Action : TConstProc<T>) : IOptionDefinition;overload;
    function RegisterOption<T>(const longName: string; const shortName : string; const Action : TConstProc<T>) : IOptionDefinition;overload;
    function RegisterOption<T>(const longName: string; const shortName : string; const helpText : string; const Action : TConstProc<T>) : IOptionDefinition;overload;
    function RegisterUnNamedOption<T>(const helpText : string; const Action : TConstProc<T>) : IOptionDefinition;overload;
    function RegisterUnNamedOption<T>(const helpText : string; const valueDescription : string; const Action : TConstProc<T>) : IOptionDefinition;overload;
    function HasOption(const value : string) : boolean;
    constructor Create(const commandDef : ICommandDefinition);
    property Name : string read GetName;
    property Alias : string read GetAlias;
    property Description : string read GetDescription;
    property Usage : string read GetUsage;
    property Examples : TList<string> read GetExamples;
    property Command : ICommandDefinition read FCommandDef;
  end;

  TOptionsRegistry = class
  private
    class var
      FNameValueSeparator: string;
      FDefaultCommand : TCommandDefinition;
      FCommandDefs : TDictionary<string,ICommandDefinition>;
      FDescriptionTab : integer;
      FConsoleWidth : integer;
  protected
    class function GetDefaultCommand: ICommandDefinition; static;
  protected
    class constructor Create;
    class destructor Destroy;
  public
    class procedure Clear; //use for testing only;
    class function RegisterOption<T>(const longName: string; const Action : TConstProc<T>) : IOptionDefinition;overload;
    class function RegisterOption<T>(const longName: string; const shortName : string; const Action : TConstProc<T>) : IOptionDefinition;overload;
    class function RegisterOption<T>(const longName: string; const shortName : string; const helpText : string; const Action : TConstProc<T>) : IOptionDefinition;overload;
    class function RegisterUnNamedOption<T>(const helpText : string; const Action : TConstProc<T>) : IOptionDefinition;overload;
    class function RegisterUnNamedOption<T>(const helpText : string; const valueDescription : string; const Action : TConstProc<T>) : IOptionDefinition;overload;
    class function RegisterCommand(const name : string; const alias : string; const description : string; const helpString : string; const usage : string; const visible : boolean = true) : TCommandDefinition;
    class function Parse: ICommandLineParseResult;overload;
    class function Parse(const values : TStrings) : ICommandLineParseResult;overload;
    class procedure PrintUsage(const proc : TConstProc<string>; const printDefaultUsage : boolean = true);overload;
    class function PrintUsage(const commandName : string; const proc : TConstProc<string>): Boolean; overload;
    class procedure PrintUsage(const command : ICommandDefinition; const proc : TConstProc<string>);overload;
    class procedure EnumerateCommands(const proc : TConstProc<string,string>);overload;
    class procedure EnumerateCommands(const proc : TConstProc<ICommandDefinition>);overload;
    class procedure EmumerateCommandOptions(const commandName : string; const proc : TConstProc<string,string, string>);overload;
    class procedure EmumerateCommandOptions(const commandName : string; const proc : TConstProc<IOptionDefinition>);overload;
    class function GetCommandByName(const name : string) : ICommandDefinition;
    class property NameValueSeparator: string read FNameValueSeparator write FNameValueSeparator;
    class property DescriptionTab : integer read FDescriptionTab write FDescriptionTab;
    class property DefaultCommand : ICommandDefinition read GetDefaultCommand;
    class property RegisteredCommands : TDictionary<string,ICommandDefinition> read FCommandDefs;
  end;
implementation
uses
  Generics.Defaults,
  System.StrUtils,
  VSoft.CommandLine.Utils,
  VSoft.CommandLine.Parser,
  VSoft.Commandline.OptionDef,
  VSoft.CommandLine.CommandDef;
{ TOptionsRegistry }
class function TOptionsRegistry.RegisterOption<T>(const longName, shortName: string; const Action: TConstProc<T>): IOptionDefinition;
begin
  result := FDefaultCommand.RegisterOption<T>(longName,shortName,Action);
end;
class function TOptionsRegistry.Parse: ICommandLineParseResult;
var
  parser : ICommandLineParser;
begin
  parser := TCommandLineParser.Create(NameValueSeparator);
  result := parser.Parse;
end;
class function TOptionsRegistry.RegisterCommand(const name: string; const alias : string; const description : string; const helpString : string; const usage : string; const visible : boolean): TCommandDefinition;
var
  cmdDef : ICommandDefinition;
begin
  cmdDef := TCommandDefImpl.Create(name,alias, usage, description, helpString,visible);
  result := TCommandDefinition.Create(cmdDef);
  FCommandDefs.Add(LowerCase(name),cmdDef);
end;

class procedure TOptionsRegistry.Clear;
begin
  FDefaultCommand.FCommandDef.Clear;
  FCommandDefs.Clear;
end;
class constructor TOptionsRegistry.Create;
var
  cmdDef : ICommandDefinition;
begin
  cmdDef := TCommandDefImpl.Create('','','','','',false,true);
  FDefaultCommand := TCommandDefinition.Create(cmdDef);
  FCommandDefs := TDictionary<string,ICommandDefinition>.Create;
  FNameValueSeparator := ':';
  FDescriptionTab := 35;
  FConsoleWidth := GetConsoleWidth;
end;
class destructor TOptionsRegistry.Destroy;
begin
  FCommandDefs.Free;
end;
class function TOptionsRegistry.GetCommandByName(const name: string): ICommandDefinition;
begin
  result := nil;
  FCommandDefs.TryGetValue(LowerCase(name), Result);
end;
class function TOptionsRegistry.GetDefaultCommand: ICommandDefinition;
begin
  result := TOptionsRegistry.FDefaultCommand.FCommandDef;
end;
class function TOptionsRegistry.Parse(const values: TStrings): ICommandLineParseResult;
var
  parser : ICommandLineParser;
begin
  parser := TCommandLineParser.Create(NameValueSeparator);
  result := parser.Parse(values);
end;
class procedure TOptionsRegistry.EmumerateCommandOptions(const commandName: string; const proc: TConstProc<string, string, string>);
var
  cmd : ICommandDefinition;
begin
  if not FCommandDefs.TryGetValue(LowerCase(commandName), cmd) then
    raise Exception.Create('Unknown command : ' + commandName);
  cmd.EnumerateCommandOptions(proc);
end;

class procedure TOptionsRegistry.EnumerateCommands(const proc: TConstProc<string,string>);
var
  cmd : ICommandDefinition;
  cmdList : TList<ICommandDefinition>;
begin
  //The commandDefs are stored in a dictionary, so we need to sort them ourselves.
  cmdList := TList<ICommandDefinition>.Create;
  try
    for cmd in FCommandDefs.Values do
    begin
      if cmd.Visible then
        cmdList.Add(cmd);
    end;
    cmdList.Sort(TComparer<ICommandDefinition>.Construct(
    function (const L, R: ICommandDefinition): integer
    begin
      Result := CompareText(L.Name,R.Name);
    end));
    for cmd in cmdList do
      proc(cmd.Name,cmd.Description);
  finally
    cmdList.Free;
  end;
end;
function PadRight(const s: string; TotalWidth: Integer; PaddingChar: Char = ' '): string;
begin
  TotalWidth := TotalWidth - Length(s);
  if TotalWidth > 0 then
    Result := s + StringOfChar(PaddingChar, TotalWidth)
  else
    Result := s;
end;

function PadLeft(const s: string; TotalWidth: Integer; PaddingChar: Char = ' '): string;
begin
   Result := StringOfChar(PaddingChar, TotalWidth) + s
end;

class procedure TOptionsRegistry.PrintUsage(const command: ICommandDefinition; const proc: TConstProc<string>);
var
  maxDescW : integer;
  exeName : string;
  i: Integer;
  printOption : TConstProc<IOptionDefinition>;
begin
  exeName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  if not command.IsDefault then
  begin
    proc('Usage: ' + exeName + ' ' + command.Usage);
    proc(command.Description);
    if command.HelpText <> '' then
    begin
      proc(command.HelpText);
    end;
    proc('Options:');
  end
  else
  begin
    proc('Options:');
  end;
  if FConsoleWidth < High(Integer) then
    maxDescW := FConsoleWidth
  else
    maxDescW := High(Integer);
  maxDescW := maxDescW - FDescriptionTab;
  printOption :=  procedure(const opt : IOptionDefinition)
                  var
                     descStrings : TArray<string>;
                     i : integer;
                     numDescStrings : integer;
                     s  : string;
                  begin
                    s := WrapText(opt.HelpText, sLineBreak, [' ', '-', #9, ','],  maxDescW -1);
                    descStrings := TStringUtils.Split(s, sLineBreak);
                    for i := 0 to length(descStrings) -1 do
                      descStrings[i] := Trim(descStrings[i]);
                    if opt.IsUnnamed then
                      s := ' <' + opt.ShortName + '>'
                    else
                    begin
                      s := ' -' + opt.LongName;
                      if opt.ShortName <> '' then
                        s := s + '|-' + opt.ShortName ;
                    end;
                    if opt.HasValue then
                      s := s + FNameValueSeparator + '<' + opt.LongName + '>';
                    if opt.AllowMultiple then
                      s := s + ' +'
                    else
                      s := s + '  ';
                    s := PadRight(s, FDescriptionTab);
                    numDescStrings := Length(descStrings);
                    if numDescStrings > 0 then
                      s := s + descStrings[0];
                    proc(s);
                    if numDescStrings > 1 then
                    begin
                      for i := 1 to numDescStrings -1 do
                      begin
                        s := PadLeft(descStrings[i], FDescriptionTab);
                        proc(s);
                      end;
                    end;
                  end;
  command.EnumerateCommandOptions(printOption);
  if not command.IsDefault then
    FDefaultCommand.command.EnumerateCommandOptions(printOption);

  if command.Examples.Count > 0 then
  begin
    proc('Examples:');
    for i := 0 to command.Examples.Count - 1 do
    begin
      proc('  ' + exeName + ' ' + command.Examples.Items[i]);
    end;
  end;
end;

class function TOptionsRegistry.PrintUsage(const commandName: string; const proc: TConstProc<string>): Boolean;
var
  cmd : ICommandDefinition;
begin
  Result := True;
  if commandName = '' then
  begin
    PrintUsage(proc);
    exit;
  end;
  if not FCommandDefs.TryGetValue(LowerCase(commandName),cmd) then
  begin
    proc('Unknown command: ' + commandName);
    Result := False;
    exit;
  end;
  PrintUsage(cmd,proc);
end;
function compareKey(const L, R: String): Integer;
begin
  Result := CompareText(L, R);
end;
class procedure TOptionsRegistry.PrintUsage(const proc: TConstProc<string>; const printDefaultUsage : boolean);
var
  cmd : ICommandDefinition;
  descStrings : TArray<string>;
  i : integer;
  numDescStrings : integer;
  maxDescW : integer;
  s : string;
  keyArray: TArray<String>;
  key : string;
  exeName : string;
begin
  exeName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  //if we have more than 1 command then we are using command mode
  if FCommandDefs.Count > 0 then
  begin
    proc('Usage: ' + exeName + ' [command] [options]');
    proc('Commands:');

    if FConsoleWidth < High(Integer) then
      maxDescW := FConsoleWidth
    else
      maxDescW := High(Integer);
     maxDescW := maxDescW - FDescriptionTab - 2;
    keyArray:= FCommandDefs.Keys.ToArray;
    TArray.Sort<String>(keyArray, TComparer<String>.Construct(compareKey));
    for key in keyArray do
    begin
      cmd := FCommandDefs[key];
      if not cmd.Visible then
        continue;
      s := WrapText(cmd.Description,maxDescW);
      descStrings := TStringUtils.Split(s, sLineBreak);
      proc(' ' + PadRight(cmd.Name, descriptionTab -1) + descStrings[0]);
      numDescStrings := Length(descStrings);
      if numDescStrings > 1 then
      begin
        for i := 1 to numDescStrings -1 do
          proc(PadRight('', descriptionTab) + descStrings[i]);
      end;
    end;
    PrintUsage(FDefaultCommand.FCommandDef,proc);
  end
  else //non command mode
  begin
    if printDefaultUsage then
    begin
      proc('Usage: ' + exeName + ' [options]');
    end;
    PrintUsage(FDefaultCommand.FCommandDef,proc);
  end;
end;
class function TOptionsRegistry.RegisterOption<T>(const longName, shortName, helpText: string; const Action: TConstProc<T>): IOptionDefinition;
begin
  result := RegisterOption<T>(longName,shortName,Action);
  result.HelpText := helpText;
end;
class function TOptionsRegistry.RegisterUnNamedOption<T>(const helpText, valueDescription: string; const Action: TConstProc<T>): IOptionDefinition;
begin
  result := FDefaultCommand.RegisterUnNamedOption<T>(helpText,valueDescription, Action);
end;

class function TOptionsRegistry.RegisterOption<T>(const longName: string; const Action: TConstProc<T>): IOptionDefinition;
begin
  result := RegisterOption<T>(longName,'',Action);
end;
class function TOptionsRegistry.RegisterUnNamedOption<T>(const helpText: string; const Action: TConstProc<T>): IOptionDefinition;
begin
  result := FDefaultCommand.RegisterUnNamedOption<T>(helpText,Action);
end;
{ TCommandDef }
constructor TCommandDefinition.Create(const commandDef : ICommandDefinition);
begin
  FCommandDef := commandDef;
end;

function TCommandDefinition.RegisterOption<T>(const longName: string; const Action: TConstProc<T>): IOptionDefinition;
begin
  result := RegisterOption<T>(longName,'',Action);
end;
function TCommandDefinition.RegisterOption<T>(const longName, shortName: string; const Action: TConstProc<T>): IOptionDefinition;
begin
  if longName = '' then
    raise Exception.Create('Name required - use RegisterUnamed to register unamed options');
  if FCommandDef.HasOption(LowerCase(longName)) then
    raise Exception.Create('Option [' + longName + '] already registered on command [' + FCommandDef.Name + ']');
  if FCommandDef.HasOption(LowerCase(shortName)) then
    raise Exception.Create('Option [' + shortName + '] already registered on command [' + FCommandDef.Name + ']');
  result := TOptionDefinition<T>.Create(longName,shortName,Action);
  FCommandDef.AddOption(Result);
end;
function TCommandDefinition.GetAlias: string;
begin
  result := FCommandDef.Alias;
end;
function TCommandDefinition.GetDescription: string;
begin
  result := FCommandDef.Description;
end;
function TCommandDefinition.GetExamples: TList<string>;
begin
  result := FCommandDef.Examples;
end;

function TCommandDefinition.GetName: string;
begin
  result := FCommandDef.Name;
end;
function TCommandDefinition.GetUsage: string;
begin
  result := FCommandDef.Usage;
end;
function TCommandDefinition.HasOption(const value: string): boolean;
begin
  result := FCommandDef.HasOption(value);
end;
function TCommandDefinition.RegisterOption<T>(const longName, shortName, helpText: string; const Action: TConstProc<T>): IOptionDefinition;
begin
  result := RegisterOption<T>(longName,shortName,Action);
  result.HelpText := helpText;
end;
function TCommandDefinition.RegisterUnNamedOption<T>(const helpText, valueDescription: string; const Action: TConstProc<T>): IOptionDefinition;
begin
  result := TOptionDefinition<T>.Create('',valueDescription,helptext,Action);
  result.HasValue := false;
  FCommandDef.AddOption(result);

end;

function TCommandDefinition.RegisterUnNamedOption<T>(const helpText: string; const Action: TConstProc<T>): IOptionDefinition;
begin
  result := TOptionDefinition<T>.Create('','',helptext,Action);
  result.HasValue := false;
  FCommandDef.AddOption(result);
end;

class procedure TOptionsRegistry.EmumerateCommandOptions(const commandName: string; const proc: TConstProc<IOptionDefinition>);
var
  cmd : ICommandDefinition;
begin
  if not FCommandDefs.TryGetValue(commandName,cmd) then
    raise Exception.Create('Unknown command: ' + commandName);
  cmd.EnumerateCommandOptions(proc);
end;
class procedure TOptionsRegistry.EnumerateCommands(const proc: TConstProc<ICommandDefinition>);
var
  cmd : ICommandDefinition;
  cmdList : TList<ICommandDefinition>;
begin
  //The commandDefs are stored in a dictionary, so we need to sort them ourselves.
  cmdList := TList<ICommandDefinition>.Create;
  try
    for cmd in FCommandDefs.Values do
    begin
      if cmd.Visible then
        cmdList.Add(cmd);
    end;
    cmdList.Sort(TComparer<ICommandDefinition>.Construct(
    function (const L, R: ICommandDefinition): integer
    begin
      Result := CompareText(L.Name,R.Name);
    end));
    for cmd in cmdList do
      proc(cmd);
  finally
    cmdList.Free;
  end;
end;
end.
