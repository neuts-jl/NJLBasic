unit uhelp;
interface
uses
  SysUtils, Classes, fgl, StrUtils;

type
  TCommand = class
    Category: string;
    Command: string;
    Description: string;
    Example: string;

    constructor Create(const ACategory, ACommand, ADescription, AExample: string);
  end;

  TCommandList = specialize TFPGList<TCommand>;

  THelp = class
  private
    CommandList: TCommandList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    function GetCategories: string;
    function GetCategory(const Category: string): string;
    function GetHelp(const Command: string): string;
  end;

implementation

constructor TCommand.Create(const ACategory, ACommand, ADescription, AExample: string);
begin
  Category := ACategory;
  Command := ACommand;
  Description := ADescription;
  Example := AExample;
end;

constructor THelp.Create;
begin
  CommandList := TCommandList.Create;
end;

destructor THelp.Destroy;
begin
  CommandList.Free;
  inherited;
end;

procedure THelp.LoadFromFile(const FileName: string);
var
  F: TStringList;
  i: Integer;
  Ligne, Category, Command, Description, Example: string;
  Cmd: TCommand;
begin
  F := TStringList.Create;
  try
    F.LoadFromFile(FileName);
    for i := 0 to F.Count - 1 do
    begin
      Ligne := F[i];
      Category := ExtractDelimited(1, Ligne, ['|']);
      Command := ExtractDelimited(2, Ligne, ['|']);
      Description := ExtractDelimited(3, Ligne, ['|']);
      Example := ExtractDelimited(4, Ligne, ['|']);
      Cmd := TCommand.Create(Category, Command, Description, Example);
      CommandList.Add(Cmd);
    end;
  finally
    F.Free;
  end;
end;

function THelp.GetCategories: string;
var
  i: Integer;
  Categories: TStringList;
  Cmd: TCommand;
  ResultStr: string;
begin
  Categories := TStringList.Create;
  try
    for i := 0 to CommandList.Count - 1 do
    begin
      Cmd := CommandList[i];
      if Categories.IndexOf(Cmd.Category) = -1 then
        Categories.Add(Cmd.Category);
    end;

    ResultStr := '';
    for i := 0 to Categories.Count - 1 do
      if Trim(Categories[i])<>'' then
        ResultStr := ResultStr + Categories[i] + sLineBreak;

    Result := ResultStr;
  finally
    Categories.Free;
  end;
end;

function GetCommand(Const Command:string):string;
begin
  Result := Command;
  while pos('  ',Result)>0 do
    Result:=StringReplace(Result,'  ',' ',[rfReplaceAll]);
  if Pos('(', Result) > 0 then
    Result := Trim(Copy(Result, 1, Pos('(', Result)));
  if Pos(' ', Result) > 0 then
    Result := Trim(Copy(Result, 1, Pos(' ', Result) - 1));
  Result:=Trim(StringReplace(Result,'_',' ',[rfReplaceAll]));
end;

function THelp.GetCategory(const Category: string): string;
var
  i: Integer;
  Cmd: TCommand;
  ResultStr, UCategory: string;
begin
  ResultStr := '';
  UCategory:=UpperCase(Category);
  for i := 0 to CommandList.Count - 1 do
  begin
    Cmd := CommandList[i];
    if UpperCase(Cmd.Category) = UCategory then
      ResultStr := ResultStr + GetCommand(Cmd.Command) + sLineBreak;
  end;
  Result := ResultStr;
end;

function THelp.GetHelp(const Command: string): string;
var
  i: Integer;
  Cmd, SearchedCmd: string;
  CmdObj: TCommand;
begin
  SearchedCmd := UpperCase(GetCommand(StringReplace(Command,' ','_',[rfReplaceAll])));

  for i := 0 to CommandList.Count - 1 do
  begin
    CmdObj := CommandList[i];
    Cmd := UpperCase(GetCommand(CmdObj.Command));
    if SameText(Cmd, SearchedCmd) then
    begin
      Result:='   Category : ' + CmdObj.Category +  sLineBreak;
      if Pos('(',CmdObj.Command)>0 then
        Result:=Result+'   Function : '
      else
        Result:=Result+'    Command : ';
      Result:=Result+StringReplace(CmdObj.Command,'_',' ',[rfReplaceAll]) +  sLineBreak ;
      Result:=Result+'Description : ' + CmdObj.Description + sLineBreak ;
      if CmdObj.Example<>'' then
         Result:=Result+'    Example : ' + CmdObj.Example;
      exit;
    end;
  end;
  Result := 'Topic, command or function not found.';
end;

end.

