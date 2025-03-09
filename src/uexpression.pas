unit uExpression;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, uTokenizer, fgl;

type
  TExpressionParser = class;

  TValueType = (vtString, vtDecimal, vtInteger, vtBoolean);

  TValue = record
    ValueType: TValueType;
    StrValue: string;
    NumValue: double;
    BoolValue: boolean;
  end;

  TVariables = specialize TFPGMap<string, TValue>;

  TArraySize = record
    x, y, z: integer;
  end;

  TVariableArray = array of array of array of TValue;
  TVariablesArray = specialize TFPGMap<string, TVariableArray>;

  TFunction = function(Parser: TExpressionParser): TValue;
  TFunctions = specialize TFPGMap<string, TFunction>;


  TExpressionParser = class
  private
    FCaseSensitive: boolean;
    FTokenizer: TTokenizer;
    FVariables: TVariables;
    FVariablesArray: TVariablesArray;
    FFunctions: TFunctions;
    FWorkSpace: string;
    function ParseTerm: TValue;
    function ParseFactor: TValue;
    function ParseFunctionOrVariable(Name: string): TValue;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseExpression: TValue;
    function Evaluate(const Expr: string): TValue;
    function GetVariables: string;
    function GetVarFullName(const Name: string): string;
    procedure SetVariable(const Name: string; Value: TValue);
    procedure SetVariable(const Name: string; Value: string); overload;
    procedure SetVariable(const Name: string; Value: double); overload;
    procedure SetVariable(const Name: string; Value: integer); overload;
    procedure SetVariable(const Name: string; Value: boolean); overload;
    function GetVariable(const Name:string):TValue;
    function GetIntVariable(const Name:string):Integer;
    function GetArraySize(Arr: TVariableArray): TArraySize;
    function GetArraySize(Name:string): TArraySize;
    procedure SetDimVariableArray(const Name: string; SizeX, SizeY, SizeZ: integer);
    procedure SetVariableArray(const Name: string; Value: TValue; x, y, z: integer);
    procedure SetVariableArray(const Name: string; Value: string;
      x, y, z: integer); overload;
    procedure SetVariableArray(const Name: string; Value: double;x, y, z: integer); overload;
    procedure SetVariableArray(const Name: string; Value: integer;x, y, z: integer); overload;
    procedure SetVariableArray(const Name: string; Value: boolean;x, y, z: integer); overload;
    function ParseVariablesArrayValue(const Name: string): TValue;
    procedure ClearVariables;
    procedure DeleteVariable(Name:string);
    procedure RegisterFunction(const Name: string; Func: TFunction);
    procedure WaitComma;
    function GetDecValue: double;
    function GetIntValue: integer;
    function GetStrValue: string;
    function GetBoolValue: boolean;
    property Tokenizer: TTokenizer read FTokenizer;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property WorkSpace: string read FWorkSpace write FWorkSpace;
    property Variables:TVariables read FVariables;
  end;

implementation

uses
  ubasicvarfunc;

constructor TExpressionParser.Create;
begin
  FCaseSensitive := False;
  FTokenizer := TTokenizer.Create;
  FVariables := TVariables.Create;
  FVariablesArray := TVariablesArray.Create;
  FFunctions := TFunctions.Create;
  RegisterFunc(Self);
  ClearVariables;
end;

destructor TExpressionParser.Destroy;
begin
  FTokenizer.Free;
  FVariablesArray.Free;
  FVariables.Free;
  FFunctions.Free;
  inherited;
end;

function TExpressionParser.GetVariables: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to FVariables.Count - 1 do
    Result := Result + FVariables.Keys[i] + #10;
  Result := Trim(Result);
end;

function TExpressionParser.GetVarFullName(const Name: string): string;
var
  AName: string;
begin
  if not FCaseSensitive then
    AName := LowerCase(Name)
  else
    AName := Name;
  if FWorkSpace <> '' then
    AName := FWorkSpace + '.' + AName;
  Result := AName;
end;

procedure TExpressionParser.SetVariable(const Name: string; Value: TValue);
begin
  FVariables[GetVarFullName(Name)] := Value;
end;

procedure TExpressionParser.SetVariable(const Name: string; Value: string);
var
  FValue: TValue;
begin
  FValue.StrValue := Value;
  FValue.ValueType := vtString;
  SetVariable(Name, FValue);
end;

procedure TExpressionParser.SetVariable(const Name: string; Value: double);
var
  FValue: TValue;
begin
  FValue.NumValue := Value;
  FValue.ValueType := vtDecimal;
  SetVariable(Name, FValue);
end;

procedure TExpressionParser.SetVariable(const Name: string; Value: integer);
var
  FValue: TValue;
begin
  FValue.NumValue := Value;
  FValue.ValueType := vtInteger;
  SetVariable(Name, FValue);
end;

procedure TExpressionParser.SetVariable(const Name: string; Value: boolean);
var
  FValue: TValue;
begin
  FValue.BoolValue := Value;
  FValue.ValueType := vtBoolean;
  SetVariable(Name, FValue);
end;

function TExpressionParser.GetVariable(const Name:string):TValue;
begin
  if FVariables.IndexOf(GetVarFullName(Name))=-1 then
    Raise Exception.Create('Variable '+Name+' not found');
  Result:=FVariables[GetVarFullName(Name)];
end;

function TExpressionParser.GetIntVariable(const Name:string):Integer;
var
  Value:TValue;
begin
  Value:=GetVariable(Name);
  if Value.ValueType<>vtInteger then
    Raise Exception.Create('Variable '+Name+' is not integer');
  Result:=Round(Value.NumValue)
end;

function TExpressionParser.GetArraySize(Arr: TVariableArray): TArraySize;
begin
  Result.X := High(Arr) - Low(Arr) + 1;
  if Result.X > 0 then
    Result.Y := High(Arr[0]) - Low(Arr[0]) + 1;
  if Result.Y > 0 then
    Result.Z := High(Arr[0][0]) - Low(Arr[0][0]) + 1;
end;

function TExpressionParser.GetArraySize(Name:string): TArraySize;
var
  Arr: TVariableArray;
begin
  if FVariablesArray.indexOf(GetVarFullName(Name))=-1 then
    Raise Exception.Create(Name+' array not dimensionned');
  Arr:=FVariablesArray[GetVarFullName(Name)];
  Result:=GetArraySize(Arr);
end;

procedure TExpressionParser.SetDimVariableArray(const Name: string;
  SizeX, SizeY, SizeZ: integer);
var
  Arr: TVariableArray;
begin
  SetLength(Arr, SizeX, SizeY, SizeZ);
  FVariablesArray[GetVarFullName(Name)] := Arr;
end;

procedure TExpressionParser.SetVariableArray(const Name: string;
  Value: TValue; x, y, z: integer);
var
  Size:TArraySize;
begin
  Size:=GetArraySize(Name);
  if (y>1) and (Size.y = 1) then
    raise Exception.Create('The array has only one dimension');
  if (z>1) and (Size.z = 1) then
    raise Exception.Create('The array has only two dimension');
  if (x >= Size.x) or (x < 0) then
    raise Exception.Create('Out of range on first index');
  if (y >= Size.y) or (y < 0) then
    raise Exception.Create('Out of range on second index');
  if (z >= Size.z) or (z < 0) then
    raise Exception.Create('Out of range on the third index');
  FVariablesArray[GetVarFullName(Name)][x, y, z] := Value;
end;

procedure TExpressionParser.SetVariableArray(const Name: string;
  Value: string; x, y, z: integer);
var
  FValue: TValue;
begin
  FValue.ValueType := vtString;
  FValue.StrValue := Value;
  SetVariableArray(Name, FValue, x, y, z);
end;

procedure TExpressionParser.SetVariableArray(const Name: string;
  Value: double; x, y, z: integer);
var
  FValue: TValue;
begin
  FValue.ValueType := vtDecimal;
  FValue.NumValue := Value;
  SetVariableArray(Name, FValue, x, y, z);
end;

procedure TExpressionParser.SetVariableArray(const Name: string;
  Value: integer; x, y, z: integer);
var
  FValue: TValue;
begin
  FValue.ValueType := vtInteger;
  FValue.NumValue := Value;
  SetVariableArray(Name, FValue, x, y, z);
end;

procedure TExpressionParser.SetVariableArray(const Name: string;
  Value: Boolean; x, y, z: integer);
var
  FValue: TValue;
begin
  FValue.ValueType := vtBoolean;
  FValue.BoolValue := Value;
  SetVariableArray(Name, FValue, x, y, z);
end;

function TExpressionParser.ParseVariablesArrayValue(const Name: string): TValue;
var
  x, y, z: integer;
  Token: TToken;
  Size: TArraySize;
begin
  if FTokenizer.GetNextToken.Value <> '(' then
    raise Exception.Create('Expected "(" after function name: ' + Name);
  x := GetIntValue;
  y := 0;
  z := 0;
  Size := GetArraySize(FVariablesArray[Name]);
  Token := Tokenizer.GetNextToken;
  if Token.Value = ',' then
  begin
    y := GetIntValue;
    if Size.y = 1 then
      raise Exception.Create('The array has only one dimension');
    Token := Tokenizer.GetNextToken;
    if Token.Value = ',' then
    begin
      z := GetIntValue;
      if Size.z = 1 then
        raise Exception.Create('The array has only two dimensions');
    end
    else
      Tokenizer.UnreadToken;
  end
  else
    Tokenizer.UnreadToken;
  if (x >= Size.x) or (x < 0) then
    raise Exception.Create('Out of range on first index');
  if (y >= Size.y) or (y < 0) then
    raise Exception.Create('Out of range on second index');
  if (z >= Size.z) or (z < 0) then
    raise Exception.Create('Out of range on the third index');
  Result := FVariablesArray[Name][x, y, z];
  if FTokenizer.GetNextToken.Value <> ')' then
    raise Exception.Create('Mismatched parentheses in function: ' + Name);
end;

procedure TExpressionParser.ClearVariables;
var
  i: integer;
begin
  if FWorkSpace <> '' then
  begin
    i := 0;
    while i < FVariables.Count do
    begin
      if Copy(FVariables.Keys[i], 1, Length(FWorkSpace) + 1) = FWorkSpace + '.' then
        FVariables.Delete(i)
      else
        Inc(i);
    end;
    i := 0;
    while i < FVariablesArray.Count do
    begin
      if Copy(FVariablesArray.Keys[i], 1, Length(FWorkSpace) + 1) = FWorkSpace + '.' then
        FVariablesArray.Delete(i)
      else
        Inc(i);
    end;
  end
  else
  begin
    FVariables.Clear;
    FVariablesArray.Clear;
  end;
  RegisterVar(Self);
end;

procedure TExpressionParser.DeleteVariable(Name:string);
begin
  if FVariables.IndexOf(GetVarFullName(Name))=-1 then
    Raise Exception.Create('Variable '+Name+' not found');
  FVariables.Delete(FVariables.IndexOf(Name));
end;

procedure TExpressionParser.WaitComma;
var
  LValue: string;
begin
  LValue := Tokenizer.Token.Value;
  if Tokenizer.GetNextToken.Value <> ',' then
    raise Exception.Create('Expected "," after ' + LValue);
end;

function TExpressionParser.GetDecValue: double;
var
  Value: TValue;
begin
  Value := ParseExpression;
  if not (Value.ValueType in [vtInteger, vtDecimal]) then
    raise(Exception.Create('Mismatch value'));
  Result := Value.NumValue;
end;

function TExpressionParser.GetIntValue: integer;
var
  Value: TValue;
begin
  Value := ParseExpression;
  if Value.ValueType <> vtInteger then
    raise(Exception.Create('Mismatch value'));
  Result := Round(Value.NumValue);
end;

function TExpressionParser.GetBoolValue: boolean;
var
  Value: TValue;
begin
  Value := ParseExpression;
  if Value.ValueType <> vtBoolean then
    raise(Exception.Create('Mismatch value'));
  Result := Value.BoolValue;
end;

function TExpressionParser.GetStrValue: string;
var
  Value: TValue;
  Token: TToken;
begin
  Value := ParseExpression;
  if Value.ValueType <> vtString then
    raise(Exception.Create('Mismatch value'));
  Result := Value.StrValue;
end;

procedure TExpressionParser.RegisterFunction(const Name: string; Func: TFunction);
begin
  if not FCaseSensitive then
    FFunctions[LowerCase(Name)] := Func
  else
    FFunctions[Name] := Func;
end;

function TExpressionParser.Evaluate(const Expr: string): TValue;
begin
  if Trim(Expr)='' then
    Raise Exception.Create('Empty expression');
  FTokenizer.Source := Expr;
  Result := ParseExpression;
end;

function TExpressionParser.ParseExpression: TValue;
var
  TermValue: TValue;
  Token: TToken;
  procedure LogicalOpToSep;
  begin
    if (LowerCase(Token.Value) = 'or') or (LowerCase(Token.Value) = 'and') then
    begin
      Token.Value := LowerCase(Token.Value);
      Token.TokenType := ttSeparator;
    end;
  end;
begin
  Result := ParseTerm;
  Token := FTokenizer.GetNextToken;
  LogicalOpToSep;
  while Token.TokenType = ttSeparator do
  begin
    case Token.Value of
      '+':
      begin
        TermValue := ParseTerm;
        if (Result.ValueType = vtString) and (TermValue.ValueType = vtString) then
          Result.StrValue := Result.StrValue + TermValue.StrValue
        else if (Result.ValueType in [vtDecimal, vtInteger]) and
          (TermValue.ValueType in [vtDecimal, vtInteger]) then
          Result.NumValue := Result.NumValue + TermValue.NumValue
        else
          raise Exception.Create('Type mismatch: cannot add incompatible types');
      end;
      '-':
      begin
        TermValue := ParseTerm;
        if (Result.ValueType in [vtDecimal, vtInteger]) and
          (TermValue.ValueType in [vtDecimal, vtInteger]) then
          Result.NumValue := Result.NumValue - TermValue.NumValue
        else
          raise Exception.Create('Type mismatch: cannot subtract non-numeric values');
      end;
      '==', '=':
      begin
        TermValue := ParseTerm;
        if Result.ValueType = TermValue.ValueType then
        begin
          case Result.ValueType of
            vtString: Result.BoolValue := Result.StrValue = TermValue.StrValue;
            vtDecimal, vtInteger: Result.BoolValue :=
                Result.NumValue = TermValue.NumValue;
            vtBoolean: Result.BoolValue := Result.BoolValue = TermValue.BoolValue;
          end;
          Result.ValueType := vtBoolean;
        end
        else
          raise Exception.Create('Type mismatch in comparison');
      end;
      '!=', '<>':
      begin
        TermValue := ParseTerm;
        if Result.ValueType = TermValue.ValueType then
        begin
          case Result.ValueType of
            vtString: Result.BoolValue := Result.StrValue <> TermValue.StrValue;
            vtDecimal, vtInteger: Result.BoolValue :=
                Result.NumValue <> TermValue.NumValue;
            vtBoolean: Result.BoolValue := Result.BoolValue <> TermValue.BoolValue;
          end;
          Result.ValueType := vtBoolean;
        end
        else
          raise Exception.Create('Type mismatch in comparison');
      end;
      '<':
      begin
        TermValue := ParseTerm;
        if (Result.ValueType in [vtDecimal, vtInteger]) and
          (TermValue.ValueType in [vtDecimal, vtInteger]) then
        begin
          Result.BoolValue := Result.NumValue < TermValue.NumValue;
          Result.ValueType := vtBoolean;
        end
        else
          raise Exception.Create('Type mismatch: cannot compare non-numeric values');
      end;
      '>':
      begin
        TermValue := ParseTerm;
        if (Result.ValueType in [vtDecimal, vtInteger]) and
          (TermValue.ValueType in [vtDecimal, vtInteger]) then
        begin
          Result.BoolValue := Result.NumValue > TermValue.NumValue;
          Result.ValueType := vtBoolean;
        end
        else
          raise Exception.Create('Type mismatch: cannot compare non-numeric values');
      end;
      '<=':
      begin
        TermValue := ParseTerm;
        if (Result.ValueType in [vtDecimal, vtInteger]) and
          (TermValue.ValueType in [vtDecimal, vtInteger]) then
        begin
          Result.BoolValue := Result.NumValue <= TermValue.NumValue;
          Result.ValueType := vtBoolean;
        end
        else
          raise Exception.Create('Type mismatch: cannot compare non-numeric values');
      end;
      '>=':
      begin
        TermValue := ParseTerm;
        if (Result.ValueType in [vtDecimal, vtInteger]) and
          (TermValue.ValueType in [vtDecimal, vtInteger]) then
        begin
          Result.BoolValue := Result.NumValue >= TermValue.NumValue;
          Result.ValueType := vtBoolean;
        end
        else
          raise Exception.Create('Type mismatch: cannot compare non-numeric values');
      end;
      '&&', 'and':
      begin
        TermValue := ParseTerm;
        if (Result.ValueType = vtBoolean) and (TermValue.ValueType = vtBoolean) then
          Result.BoolValue := Result.BoolValue and TermValue.BoolValue
        else
          raise Exception.Create('Type mismatch: "and" requires boolean values');
      end;
      '||', 'or':
      begin
        TermValue := ParseTerm;
        if (Result.ValueType = vtBoolean) and (TermValue.ValueType = vtBoolean) then
          Result.BoolValue := Result.BoolValue or TermValue.BoolValue
        else
          raise Exception.Create('Type mismatch: "or" requires boolean values');
      end;
      else
        Break;
    end;
    Token := FTokenizer.GetNextToken;
    LogicalOpToSep;
  end;
  if (Token.TokenType in [ttString, ttInteger, ttDecimal, ttIdentifier]) then
    raise Exception.Create('Syntax error: missing operator before "' +
      Token.Value + '"');
  FTokenizer.UnreadToken;
end;

function TExpressionParser.ParseTerm: TValue;
var
  FactorValue: TValue;
  Token: TToken;
begin
  Result := ParseFactor;
  Token := FTokenizer.GetNextToken;
  while Token.TokenType = ttSeparator do
  begin
    case Token.Value of
      '*':
      begin
        FactorValue := ParseFactor;
        if (Result.ValueType in [vtDecimal, vtInteger]) and
          (FactorValue.ValueType in [vtDecimal, vtInteger]) then
          Result.NumValue := Result.NumValue * FactorValue.NumValue
        else
          raise Exception.Create('Type mismatch: cannot multiply non-numeric values');
      end;
      '/':
      begin
        FactorValue := ParseFactor;
        if (FactorValue.ValueType in [vtDecimal, vtInteger]) then
        begin
          if FactorValue.NumValue = 0 then
            raise Exception.Create('Division by zero');
          Result.NumValue := Result.NumValue / FactorValue.NumValue;
        end
        else
          raise Exception.Create('Type mismatch: cannot divide non-numeric values');
      end;
      else
        Break;
    end;
    Token := FTokenizer.GetNextToken;
  end;
  FTokenizer.UnreadToken;
end;

function TExpressionParser.ParseFactor: TValue;
var
  Token: TToken;
  Sign: integer;
  FS: TFormatSettings;
  DoNot: boolean;
begin
  Sign := 1;
  DoNot:=False;
  Token := FTokenizer.GetNextToken;
  if Token.TokenType <> ttString then
  begin
    if Token.Value = '-' then
    begin
      Sign := -1;
      Token := FTokenizer.GetNextToken;
    end;
    DoNot := LowerCase(Token.Value) = 'not';
    if DoNot then
      Token := FTokenizer.GetNextToken;
  end;
  case Token.TokenType of
    ttInteger:
    begin
      Result.ValueType := vtInteger;
      Result.NumValue := StrToInt(Token.Value);
    end;
    ttDecimal:
    begin
      FS := DefaultFormatSettings;
      FS.DecimalSeparator := '.';
      Result.ValueType := vtDecimal;
      Result.NumValue := StrToFloat(Token.Value, FS);
    end;
    ttString:
    begin
      Result.ValueType := vtString;
      Result.StrValue := Token.Value;
    end;
    ttUnterminatedString:
      raise Exception.Create('Unterminated string');
    ttIdentifier:
      Result := ParseFunctionOrVariable(Token.Value);
    ttSeparator:
    begin
      if Token.Value = '(' then
      begin
        Result := ParseExpression;
        Token := FTokenizer.GetNextToken;
        if Token.Value <> ')' then
          raise Exception.Create('Mismatched parentheses');
      end
      else
        raise Exception.Create('Unexpected separator: ' + Token.Value);
    end;
    else
      raise Exception.Create('Invalid factor: ' + Token.Value);
  end;
  if DoNot then
  begin
    if Result.ValueType = vtBoolean then
      Result.BoolValue := not Result.BoolValue
    else
      raise Exception.Create('Boolean value excepted');
  end
  else if Result.ValueType in [vtInteger, vtDecimal] then
    Result.NumValue := Sign * Result.NumValue;
end;

function TExpressionParser.ParseFunctionOrVariable(Name: string): TValue;
var
  Func: TFunction;
begin
  Func := nil;
  if not FCaseSensitive then
    Name := LowerCase(Name);

  if (FWorkSpace <> '') and (FVariables.IndexOf(FWorkSpace + '.' + Name) <> -1) then
    Result := FVariables[FWorkSpace + '.' + Name]
  else if FVariables.IndexOf(Name) <> -1 then
    Result := FVariables[Name]
  else if (FWorkSpace <> '') and
    (FVariablesArray.IndexOf(FWorkSpace + '.' + Name) <> -1) then
    Result := ParseVariablesArrayValue(FWorkSpace + '.' + Name)
  else if FVariablesArray.IndexOf(Name) <> -1 then
    Result := ParseVariablesArrayValue(Name)
  else if FFunctions.IndexOf(Name) <> -1 then
  begin
    if FTokenizer.GetNextToken.Value <> '(' then
      raise Exception.Create('Expected "(" after function name: ' + Name);

    Func := FFunctions[Name];
    Result := Func(self);

    if FTokenizer.GetNextToken.Value <> ')' then
      raise Exception.Create('Mismatched parentheses in function: ' + Name);
  end
  else
    raise Exception.Create('Unknown variable or function: ' + Name);
end;



end.
