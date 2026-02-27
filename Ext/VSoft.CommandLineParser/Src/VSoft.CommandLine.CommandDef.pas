unit VSoft.CommandLine.CommandDef;

interface

uses
  Generics.Collections,
  VSoft.CommandLine.Options;

type
  TCommandDefImpl = class(TInterfacedObject, ICommandDefinition)
  private
    FName         : string;
    FAlias        : string;
    FDescription  : string;
    FHelpText     : string;
    FUsage        : string;
    FVisible      : boolean;
    FIsDefault    : boolean;
    FOptionsLookup      : TDictionary<string,IOptionDefinition>;
    //can't put unnamed options in dictionary, so we keep a list
    FUnnamedOptions     : TList<IOptionDefinition>;
    //all registered options.
    FRegisteredOptions  : TList<IOptionDefinition>;
    FExamples : TList<string>;
  protected
    procedure AddOption(const value: IOptionDefinition);
    function HasOption(const name : string) : boolean;
    function GetRegisteredOptions : TList<IOptionDefinition>;
    function GetUnNamedOptions  : TList<IOptionDefinition>;
    function GetName : string;
    function GetAlias : string;
    function GetDescription : string;
    function GetHelpText : string;
    function GetIsDefault : boolean;
    function GetUsage : string;
    function GetVisible : boolean;
    function GetExamples: TList<string>;
    function TryGetOption(const name : string; var option : IOptionDefinition) : boolean;
    procedure Clear;
    procedure GetAllRegisteredOptions(const list : TList<IOptionDefinition>);
    procedure EnumerateCommandOptions(const proc : TConstProc<string,string, string>);overload;
    procedure EnumerateCommandOptions(const proc : TConstProc<IOptionDefinition>);overload;

  public
    constructor Create(const name : string; const alias : string; const usage : string; const description : string; const helpText : string; const visible : boolean; const isDefault : boolean = false);
    destructor Destroy;override;
  end;

implementation

uses
  Generics.Defaults,
  System.SysUtils;

{ TCommandDef }

procedure TCommandDefImpl.AddOption(const value: IOptionDefinition);
begin
  if value.IsUnnamed then
    FUnNamedOptions.Add(value)
  else
  begin
    FRegisteredOptions.Add(value);
    FOptionsLookup.AddOrSetValue(LowerCase(value.LongName),value);
    if value.ShortName <> '' then
      FOptionsLookup.AddOrSetValue(LowerCase(value.ShortName),value);
  end;

end;

procedure TCommandDefImpl.Clear;
begin
  FOptionsLookup.Clear;
  FRegisteredOptions.Clear;
  FUnnamedOptions.Clear;
end;

constructor TCommandDefImpl.Create(const name: string; const alias : string;  const usage : string; const description : string; const helpText : string; const visible : boolean; const isDefault : boolean = false);
begin
  FName               := name;
  FUsage              := usage;
  FDescription        := description;
  FHelpText           := helpText;
  FAlias              := alias;
  FVisible            := visible;
  FIsDefault          := isDefault;

  FOptionsLookup      := TDictionary<string,IOptionDefinition>.Create;
  FUnnamedOptions     := TList<IOptionDefinition>.Create;
  FRegisteredOptions  := TList<IOptionDefinition>.Create;
  FExamples           := TList<string>.Create;
end;

destructor TCommandDefImpl.Destroy;
begin
  FExamples.Free;
  FOptionsLookup.Free;
  FUnnamedOptions.Free;
  FRegisteredOptions.Free;
  inherited;
end;

procedure TCommandDefImpl.EnumerateCommandOptions(const proc: TConstProc<IOptionDefinition>);
var
  optionList : TList<IOptionDefinition>;
  opt : IOptionDefinition;
begin
  optionList := TList<IOptionDefinition>.Create;
  try
    optionList.AddRange(FUnnamedOptions);
    optionList.AddRange(FRegisteredOptions);

    optionList.Sort(TComparer<IOptionDefinition>.Construct(
      function (const L, R: IOptionDefinition): integer
      begin
        Result := CompareText(L.LongName,R.LongName);
      end));

    for opt in optionList do
    begin
      if not opt.Hidden then
        proc(opt);
    end;
  finally
    optionList.Free;
  end;
end;

procedure TCommandDefImpl.EnumerateCommandOptions(const proc: TConstProc<string, string, string>);
var
  optionList : TList<IOptionDefinition>;
  opt : IOptionDefinition;
begin
  optionList := TList<IOptionDefinition>.Create;
  try
    optionList.AddRange(FUnnamedOptions);
    optionList.AddRange(FRegisteredOptions);

    optionList.Sort(TComparer<IOptionDefinition>.Construct(
      function (const L, R: IOptionDefinition): integer
      begin
        Result := CompareText(L.LongName,R.LongName);
      end));

    for opt in optionList do
    begin
      if not opt.Hidden then
        proc(opt.LongName,opt.ShortName, opt.HelpText);
    end;
  finally
    optionList.Free;
  end;

end;

function TCommandDefImpl.GetAlias: string;
begin
  result := FAlias;
end;

procedure TCommandDefImpl.GetAllRegisteredOptions(const list: TList<IOptionDefinition>);
begin
  list.AddRange(FUnnamedOptions);
  list.AddRange(FRegisteredOptions);
end;

function TCommandDefImpl.GetDescription: string;
begin
  result := FDescription;
end;

function TCommandDefImpl.GetExamples: TList<string>;
begin
  result := FExamples;
end;

function TCommandDefImpl.GetHelpText: string;
begin
  result := FHelpText;
end;

function TCommandDefImpl.GetIsDefault: boolean;
begin
  result := FIsDefault;
end;

function TCommandDefImpl.GetName: string;
begin
  result := FName;
end;

function TCommandDefImpl.GetRegisteredOptions: TList<IOptionDefinition>;
begin
  result := FRegisteredOptions;
end;

function TCommandDefImpl.GetUnNamedOptions: TList<IOptionDefinition>;
begin
  result := FUnNamedOptions;
end;

function TCommandDefImpl.GetUsage: string;
begin
  result := FUsage;
end;

function TCommandDefImpl.GetVisible: boolean;
begin
  result := FVisible;
end;

function TCommandDefImpl.HasOption(const name: string): boolean;
begin
  result := FOptionsLookup.ContainsKey(LowerCase(name));
end;

function TCommandDefImpl.TryGetOption(const name: string; var option: IOptionDefinition): boolean;
begin
  result := FOptionsLookup.TryGetValue(LowerCase(name),option);
end;

end.
