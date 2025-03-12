{
  *****************************************************************************
   Unit        : ubasicinterpreter
   Author      : NEUTS JL
   License     : GPL (GNU General Public License)
   Date        : 01/03/2025

   Description : Basic interpreter for NJLBasic


   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>.
  *****************************************************************************
}
unit ubasicinterpreter;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fileutil,
  fgl,
  Base64,
  LConvEncoding,
  process,
  ucustomconsole,
  utokenizer,
  uexpression,
  uresourceexe;

const
  KVersion = 'V1.0.0';

  Black = 0;
  Red = 1;
  Green = 2;
  Yellow = 3;
  Blue = 4;
  Magenta = 5;
  Cyan = 6;
  Gray = 7;
  White = 15;
  LightRed = 9;
  LightGreen = 10;
  LightYellow = 11;
  LightBlue = 12;
  LightMagenta = 13;
  LightCyan = 14;
  LightGray = 8;

type
  TBasicMode = (bmStopped, bmSystem, bmInteractive, bmRunning);

  TIfInfo = class
    EndIfPC: integer;
    ElsePC: integer;
  end;

  TTIfInfos = specialize TFPGList<TIfInfo>;
  TIfInfos = class(TTIfInfos);

  TForNextInfo = class
    VarName: string;
    PC: integer;
    StartValue: integer;
    EndValue: integer;
    StepValue: integer;
    CurrentValue: integer;
  end;

  TTForNextInfos = specialize TFPGList<TForNextInfo>;
  TForNextInfos = class(TTForNextInfos);

  TWhileInfo = class
    PC: integer;
    Condition: string;
  end;

  TTWhileInfos = specialize TFPGList<TWhileInfo>;
  TWhileInfos = class(TTWhileInfos);


  TLoopInfo = class
    Condition: string;
    PC: integer;
  end;

  TTLoopInfos = specialize TFPGList<TLoopInfo>;
  TLoopInfos = class(TTLoopInfos);

  TGosubInfo = class
    WorkSpace: string;
    PC: integer;
  end;

  TTGosubStack = specialize TFPGList<TGosubInfo>;
  TGosubStack = class(TTGosubStack);

  TSubProgram = class
  public
    Name: string;
    ParamNames: TStringList;
    StartLine: integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TSubPrograms = specialize TFPGMap<string, TSubProgram>;

  TFileMode = (fmRead, fmWrite, fmAppend);

  TFileInfo = class
    FileName: string;
    FileMode: TFileMode;
    FileHandle: TextFile;
  end;

  TTFileInfos = specialize TFPGList<TFileInfo>;
  TFileInfos = class(TTFileInfos);

  TBasicInterpreter = class
  private
    FConsole: TCustomConsole;
    FLineToEdit: string;
    FNLineToCont: integer;
    FOnError: string;
    FTrace: boolean;
    FTmpTokenizer: TTokenizer;
    FIfStack: TIfInfos;
    FForNextStack: TForNextInfos;
    FWhileStack: TWhileInfos;
    FLoopStack: TLoopInfos;
    FGosubStack: TGosubStack;
    FSubPrograms: TSubPrograms;
    FFileInfos: TFileInfos;
    procedure PrintFreeMem;
    procedure PrintCopyRight;
    procedure DoDim;
    procedure DoLet;
    procedure DoPrint;
    procedure DoInput;
    procedure DoGoto;
    procedure DoGosub;
    procedure DoReturn;
    procedure DoSub;
    procedure DoEndSub;
    procedure DoCall;
    procedure DoIf;
    procedure DoElse;
    procedure DoEndIf;
    procedure DoFor;
    procedure DoNext;
    procedure DoWhile;
    procedure DoWend;
    procedure DoDo;
    procedure DoLoop;
    procedure DoOn;
    procedure DoResume;
    procedure DoFileClose;
    procedure DoFileLineInput;
    procedure DoFileOpen;
    procedure DoFileWrite;
    procedure DoKill;
    procedure DoName;
    procedure DoChDir;
    procedure DoMkDir;
    procedure DoRmDir;
    procedure DoCopyFile;
    procedure DoShell;
    procedure DoDate;
    procedure DoTime;
    procedure DoClear;
    procedure DoRenum;
    procedure DoRun;
    procedure DoEnd;
    procedure DoStop;
    procedure DoCont;
    procedure DoList;
    procedure DoNew;
    procedure DoLoad;
    procedure DoSave;
    procedure DoFiles;
    procedure DoColor;
    procedure DoCursor;
    procedure DoLocate;
    procedure DoSleep;
    procedure DoEdit;
    function IsBase64(const S: string): boolean;
    function ObfuscateSource(const Source, Key: string): string;
    function ClarifySource(const Source, Key: string): string;
    procedure Interactive;
    procedure BuildExe;
  protected
    FMode: TBasicMode;
    FExpr: TExpressionParser;
    FProgTokenizer: TTokenizer;
    FProg: TStringList;
    FPC: integer;
    function PCToNLine(const PC: integer): integer;
    function NLineToPC(NLine: integer): integer;
    function LabelToPC(ALabel: string): integer;
    function GetValue(ForName: string; const sep: string = '';
      const Optional: boolean = False): TValue;
    function GetStrValue(ForName: string; const sep: string = '';
      const Optional: boolean = False): string;
    function GetDecValue(ForName: string; const sep: string = '';
      const Optional: boolean = False): double;
    function GetIntValue(ForName: string; const sep: string = '';
      const Optional: boolean = False): integer;
    function GetBoolValue(ForName: string; const sep: string = '';
      const Optional: boolean = False): boolean;
    function ExtraCommand(ACommand: string): boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExecFile(FileName: string);
    procedure Start;
    procedure Interpret(Line: string);
    property Mode: TBasicMode read FMode write FMode;
    property Console: TCustomConsole read FConsole write FConsole;
  end;


var
  Basic: TBasicInterpreter;

implementation

function DoFileEof(Parser: TExpressionParser): TValue;
var
  FileNumber: integer;
  FileInfo: TFileInfo;
begin
  with Basic do
  begin
    FileNumber := Parser.GetIntValue;
    if (FileNumber <= 0) or (FileNumber > FFileInfos.Count) then
      raise Exception.Create('Invalid file number');

    FileInfo := FFileInfos[FileNumber - 1];
    Result.ValueType := vtBoolean;
    Result.BoolValue := EOF(FileInfo.FileHandle);
  end;
end;

function DoInkey(Parser: TExpressionParser): TValue;
var
  Key: TKey;
begin
  Result.ValueType := vtString;
  Key := Basic.Console.Inkey;
  if Key.Car <> #0 then
    Result.StrValue := Key.Car
  else if Key.Code <> 0 then
    Result.StrValue := #0 + Chr(Key.Code)
  else
    Result.StrValue := '';
end;

function DoConsoleX(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := Basic.Console.Console.X;
end;

function DoConsoleY(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := Basic.Console.Console.Y;
end;

function O2I(O: TObject): integer;
begin
  Result := integer(Pointer(O));
end;

function I2O(n: integer): TObject;
begin
  Result := TObject(Pointer(n));
end;

function GetTmpFile(Ext: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir) +
    FormatDateTime('yyyymmddhhnnsszzz', Now) + Ext;
end;

function GetTmpDir: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir) +
    FormatDateTime('yyyymmddhhnnsszzz', Now) + '\';
  ForceDirectories(Result);
end;

constructor TSubProgram.Create;
begin
  inherited;
  ParamNames := TStringList.Create;
end;

destructor TSubProgram.Destroy;
begin
  ParamNames.Free;
  inherited;
end;

constructor TBasicInterpreter.Create;
begin
  inherited Create;
  FLineToEdit := '';
  FNLineToCont := 0;
  FOnError := '';
  FTrace := False;
  FMode := bmStopped;
  FExpr := TExpressionParser.Create;
  FExpr.Tokenizer.EolCar := '''';
  FProg := TStringList.Create;
  FProgTokenizer := TTokenizer.Create;
  FProgTokenizer.EolCar := '''';
  FTmpTokenizer := TTokenizer.Create;
  FTmpTokenizer.EolCar := '''';
  FIfStack := TIfInfos.Create;
  FForNextStack := TForNextInfos.Create;
  FWhileStack := TWhileInfos.Create;
  FLoopStack := TLoopInfos.Create;
  FGosubStack := TGosubStack.Create;
  FSubPrograms := TSubPrograms.Create;
  FFileInfos := TFileInfos.Create;
  FExpr.RegisterFunction('eof', @DoFileEof);
  FExpr.RegisterFunction('inkey', @DoInkey);
  FExpr.RegisterFunction('consolex', @DoConsoleX);
  FExpr.RegisterFunction('consoley', @DoConsoleY);
  FExpr.SetVariable('err', 0);
  FExpr.SetVariable('erl', 0);
  FExpr.SetVariable('error', '');
end;

destructor TBasicInterpreter.Destroy;
begin
  FFileInfos.Free;
  FSubPrograms.Free;
  FGosubStack.Free;
  FLoopStack.Free;
  FWhileStack.Free;
  FForNextStack.Free;
  FIfStack.Free;
  FTmpTokenizer.Free;
  FProgTokenizer.Free;
  FProg.Free;
  FExpr.Free;
  inherited Destroy;
end;

function TBasicInterpreter.PCToNLine(const PC: integer): integer;
begin
  Result := O2I(FProg.Objects[PC]);
end;

function TBasicInterpreter.NLineToPC(NLine: integer): integer;
var
  i: integer;
begin
  for i := 0 to FProg.Count - 1 do
    if PCToNLine(i) = NLine then
      exit(i);
  Result := -1;
end;

function TBasicInterpreter.LabelToPC(ALabel: string): integer;
var
  i: integer;
begin
  ALabel := UpperCase(ALabel) + ':';
  for i := 0 to FProg.Count - 1 do
    if Pos(ALabel, UpperCase(Trim(FProg[i]))) = 1 then
      exit(i);
  Result := -1;
end;

function TBasicInterpreter.GetValue(ForName: string; const Sep: string = '';
  const Optional: boolean = False): TValue;
var
  Token: TToken;
  ASep, Expression: string;
  AStart, AEnd, ALevel: integer;
begin
  Expression := '';
  ASep := UpperCase(Sep);
  Token := FProgTokenizer.GetNextToken;
  AStart := Token.Index;
  ALevel := 0;
  while ((Pos(UpperCase(Token.Value), ASep) = 0) and not
      (Token.TokenType in [ttEol, ttUnterminatedString])) or (ALevel <> 0) do
  begin
    if Token.Value = '(' then
      Inc(ALevel)
    else if Token.Value = ')' then
      Dec(ALevel);
    Token := FProgTokenizer.GetNextToken;
  end;
  AEnd := Token.Index;
  if Token.TokenType = ttUnterminatedString then
    raise Exception.Create('"' + ForName + '" : Unterminated string');
  Expression := FProgTokenizer.GetRemainToken(AStart, AEnd);
  if Expression = '' then
    raise Exception.Create('"' + ForName + '" excepted');
  if ASep <> '' then
  begin
    if (Pos(UpperCase(Token.Value), ASep) = 0) and not Optional then
      raise Exception.Create(ASep + ' expected after "' + ForName + '"');
  end;
  Result := FExpr.Evaluate(Trim(Expression));
end;

function TBasicInterpreter.GetStrValue(ForName: string; const Sep: string = '';
  const Optional: boolean = False): string;
var
  Value: TValue;
begin
  Value := GetValue(ForName, Sep, Optional);
  if Value.ValueType <> vtString then
    raise Exception.Create('"' + ForName + '" : string excepted');
  Result := Value.StrValue;
end;

function TBasicInterpreter.GetIntValue(ForName: string; const Sep: string = '';
  const Optional: boolean = False): integer;
var
  Value: TValue;
begin
  Value := GetValue(ForName, Sep, Optional);
  if Value.ValueType <> vtInteger then
    raise Exception.Create('"' + ForName + '" : integer excepted');
  Result := Round(Value.NumValue);
end;

function TBasicInterpreter.GetDecValue(ForName: string; const Sep: string = '';
  const Optional: boolean = False): double;
var
  Value: TValue;
begin
  Value := GetValue(ForName, Sep, Optional);
  if Value.ValueType <> vtDecimal then
    raise Exception.Create('"' + ForName + '" : decimal excepted');
  Result := Value.NumValue;
end;

function TBasicInterpreter.GetBoolValue(ForName: string; const Sep: string = '';
  const Optional: boolean = False): boolean;
var
  Value: TValue;
begin
  Value := GetValue(ForName, Sep, Optional);
  if Value.ValueType <> vtBoolean then
    raise Exception.Create('"' + ForName + '" : boolean excepted');
  Result := Value.BoolValue;
end;

procedure TBasicInterpreter.DoDim;
var
  VName: string;
  Size: TArraySize;
  Token: TToken;
begin
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Variable identifier excepted');
  VName := Token.Value;
  Token := FProgTokenizer.GetNextToken;
  if FProgTokenizer.Token.Value <> '(' then
    raise Exception.Create('( excepted after ' + VName);
  Size.x := GetIntValue('First size', ',)');
  Inc(Size.x);
  if Size.x < 2 then
    raise Exception.Create('Bad size for the first size');
  Size.y := 1;
  Size.z := 1;
  Token := FProgTokenizer.Token;
  if Token.Value = ',' then
  begin
    Size.y := GetIntValue('Second size', ',)');
    if Size.y < 2 then
      raise Exception.Create('Bad size for the second size');
    Inc(Size.y);
    if FProgTokenizer.Token.Value = ',' then
    begin
      Size.z := GetIntValue('Third size', ',)');
      Inc(Size.z);
      if Size.z < 2 then
        raise Exception.Create('Bad size for the third size');
    end;
    if FProgTokenizer.Token.Value <> ')' then
      raise Exception.Create(') excepted');
  end
  else if Token.Value <> ')' then
    raise Exception.Create(') excepted');
  FExpr.SetDimVariableArray(VName, Size.x, Size.y, Size.z);
end;

procedure TBasicInterpreter.DoLet;
// LET x = expression
// LET x(y) = expression
// x = expression
//....
var
  VarName: string;
  Token: TToken;
  Value: TValue;
  x, y, z: integer;
  IsArray: boolean;
begin
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Excepted identifier');
  VarName := Token.Value;
  Token := FProgTokenizer.GetNextToken;
  if Token.Value = '(' then
  begin
    IsArray := True;
    x := GetIntValue('index 1', ',)');
    y := 0;
    z := 0;
    if FProgTokenizer.Token.Value = ',' then
    begin
      y := GetIntValue('index 2', ',)');
      if FProgTokenizer.Token.Value = ',' then
        z := GetIntValue('index 3', ',)');
    end;
    if FProgTokenizer.Token.Value <> ')' then
      raise Exception.Create(') excepted');
    Token := FProgTokenizer.GetNextToken;
  end
  else
    IsArray := False;
  if Token.Value <> '=' then
    raise Exception.Create('Excepted =');
  FProgTokenizer.GetNextToken;
  Value := FExpr.Evaluate(FProgTokenizer.GetRemainToken);
  if not IsArray then
  begin
    case Value.ValueType of
      vtString:
        FExpr.SetVariable(VarName, Value.StrValue);
      vtDecimal:
        FExpr.SetVariable(VarName, Value.NumValue);
      vtInteger:
        FExpr.SetVariable(VarName, Round(Value.NumValue));
      vtBoolean:
        FExpr.SetVariable(VarName, Value.BoolValue);
    end;
  end
  else
  begin
    case Value.ValueType of
      vtString:
        FExpr.SetVariableArray(VarName, Value.StrValue, x, y, z);
      vtDecimal:
        FExpr.SetVariableArray(VarName, Value.NumValue, x, y, z);
      vtInteger:
        FExpr.SetVariableArray(VarName, Round(Value.NumValue), x, y, z);
      vtBoolean:
        FExpr.SetVariableArray(VarName, Value.BoolValue, x, y, z);
    end;
  end;
end;

procedure TBasicInterpreter.DoPrint;
//PRINT expression[;....]
var
  Value: TValue;
  Token: TToken;
  Expression: string;
  LF: boolean;
begin
  Token := FProgTokenizer.GetNextToken;
  if Token.Value = '' then
  begin
    Console.Println;
    Exit;
  end;
  repeat
    Expression := '';
    repeat
      LF := Token.Value <> ';';
      if Token.Value <> ';' then
      begin
        if Token.TokenType = ttString then
          Expression := Expression + '"' + Token.Value + '"'
        else
          Expression := Expression + Token.Value;
      end;
      Token := FProgTokenizer.GetNextToken;
    until (Token.Value = ';') or (Token.TokenType = ttEol);
    if Expression <> '' then
    begin
      Value := FExpr.Evaluate(Expression);
      case Value.ValueType of
        vtString:
          Console.Print(Value.StrValue);
        vtDecimal, vtInteger:
          Console.Print(FormatFloat('0.########', Value.NumValue));
        vtBoolean:
        begin
          if Value.BoolValue then
            Console.Print('TRUE')
          else
            Console.Print('FALSE');
        end;
      end;
    end;
  until Token.TokenType = ttEol;
  if LF then
    Console.Println;
end;

procedure TBasicInterpreter.DoInput;
//INPUT ["prompt"], variable
var
  Token: TToken;
  VarName, Prompt: string;
  InputValue: string;
  IntValue: integer;
  FloatValue: double;
begin
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType = ttString then
  begin
    Prompt := Token.Value;
    Token := FProgTokenizer.GetNextToken;
    if Token.Value <> ',' then
      raise Exception.Create('Comma expected after prompt');
  end
  else
  begin
    Prompt := '?';
    FProgTokenizer.UnreadToken;
  end;

  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Variable name expected');
  VarName := Token.Value;

  if Prompt <> '' then
    Console.Print(Prompt);
  Console.Input(InputValue);

  if TryStrToInt(InputValue, IntValue) then
    FExpr.SetVariable(VarName, IntValue)
  else if TryStrToFloat(InputValue, FloatValue) then
    FExpr.SetVariable(VarName, FloatValue)
  else
    FExpr.SetVariable(VarName, InputValue);
end;

procedure TBasicInterpreter.DoGoto;
//GOTO lineNumber
//GOTO Label
var
  Token: TToken;
  APC: integer;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType = ttInteger then
  begin
    APC := NLineToPC(StrToInt(Token.Value));
    if (APC < 0) or (APC > FProg.Count - 1) then
      raise Exception.Create('GOTO line out of range');
  end
  else if Token.TokenType = ttIdentifier then
  begin
    APC := LabelToPC(Token.Value);
    if APC = -1 then
      raise Exception.Create('GOTO label [' + Token.Value + '] not found');
  end
  else
    raise Exception.Create('Mismatch GOTO value');
  if APC = FPC then
    raise Exception.Create('GOTO infinite loop');
  FPC := APC - 1;
end;

procedure TBasicInterpreter.DoGosub;
//GOSUB lineNumber
//GOSUB Label
var
  GosubInfo: TGosubInfo;
  Token: TToken;
  APC: integer;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');

  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType = ttInteger then
  begin
    APC := NLineToPC(StrToInt(Token.Value));
    if (APC < 0) or (APC > FProg.Count - 1) then
      raise Exception.Create('GOSUB line out of range');
  end
  else if Token.TokenType = ttIdentifier then
  begin
    APC := LabelToPC(Token.Value);
    if APC = -1 then
      raise Exception.Create(': GOSUB label [' + Token.Value + '] not found');
  end
  else
    raise Exception.Create('Mismatch GOTO value');
  if APC = FPC then
    raise Exception.Create('GOSUB infinite loop');

  GosubInfo := TGosubInfo.Create;
  GosubInfo.WorkSpace := '';
  GosubInfo.PC := FPC;
  FGosubStack.Add(GosubInfo);

  FPC := APC - 1;
end;

procedure TBasicInterpreter.DoReturn;
//RETURN
var
  GosubInfo: TGosubInfo;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');

  if FGosubStack.Count = 0 then
    raise Exception.Create('RETURN without GOSUB');

  if FExpr.WorkSpace <> '' then
    FExpr.ClearVariables;
  GosubInfo := FGosubStack[FGosubStack.Count - 1];
  FPC := GosubInfo.PC;
  FExpr.WorkSpace := GosubInfo.WorkSpace;
  FGosubStack.Delete(FGosubStack.Count - 1);
end;

procedure TBasicInterpreter.DoSub;
// SUB  subname([...,...])
var
  Token: TToken;
  SubName: string;
  SubProgram: TSubProgram;
  ParamName: string;
  Found: boolean;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');

  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Subprogram name expected');
  SubName := Token.Value;

  SubProgram := TSubProgram.Create;
  SubProgram.Name := SubName;
  SubProgram.StartLine := FPC + 1;

  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> '(' then
    raise Exception.Create('( expected after subprogram name');

  repeat
    Token := FProgTokenizer.GetNextToken;
    if Token.TokenType = ttIdentifier then
    begin
      ParamName := Token.Value;
      SubProgram.ParamNames.Add(ParamName);
    end
    else if (Token.Value <> ',') and (Token.Value <> ')') then
      raise Exception.Create(', or ) expected');
  until Token.Value = ')';

  if FSubPrograms.IndexOf(SubName) > -1 then
    raise Exception.Create('Subprogram already defined');
  FSubPrograms.Add(SubName, SubProgram);
  Found := False;
  while FPC < FProg.Count - 1 do
  begin
    Inc(FPC);
    FTmpTokenizer.Source := FProg[FPC];
    Token := FTmpTokenizer.GetNextToken;
    if UpperCase(Token.Value) = 'END' then
    begin
      Token := FTmpTokenizer.GetNextToken;
      if UpperCase(Token.Value) = 'SUB' then
      begin
        Found := True;
        break;
      end;
    end;
  end;
  if not Found then
    raise Exception.Create('SUB without END SUB');
end;

procedure TBasicInterpreter.DoEndSub;
//END SUB
var
  GosubInfo: TGosubInfo;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');

  if FGosubStack.Count = 0 then
    raise Exception.Create('END SUB without CALL');

  if FExpr.WorkSpace <> '' then
    FExpr.ClearVariables;
  GosubInfo := FGosubStack[FGosubStack.Count - 1];
  FPC := GosubInfo.PC;
  FExpr.WorkSpace := GosubInfo.WorkSpace;
  FGosubStack.Delete(FGosubStack.Count - 1);
end;

procedure TBasicInterpreter.DoCall;
//CALL subname([...,...])
var
  Token: TToken;
  SubName: string;
  SubProgram: TSubProgram;
  ParamValues: array of TValue;
  i: integer;
  GosubInfo: TGosubInfo;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');

  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Subprogram name expected');
  SubName := Token.Value;

  if FSubPrograms.IndexOf(SubName) = -1 then
    raise Exception.Create('Subprogram not found');
  SubProgram := FSubprograms[SubName];

  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> '(' then
    raise Exception.Create('( expected after subprogram name');

  SetLength(ParamValues, SubProgram.ParamNames.Count);
  for i := 0 to SubProgram.ParamNames.Count - 1 do
  begin
    Token := FProgTokenizer.GetNextToken;
    if Token.TokenType = ttEol then
      raise Exception.Create('Not enough parameters');
    ParamValues[i] := FExpr.Evaluate(FProgTokenizer.GetRemainToken);
    if i < SubProgram.ParamNames.Count - 1 then
    begin
      Token := FProgTokenizer.GetNextToken;
      if Token.Value <> ',' then
        raise Exception.Create(', expected');
    end;
  end;

  GosubInfo := TGosubInfo.Create;
  GosubInfo.PC := FPC;
  GosubInfo.WorkSpace := FExpr.WorkSpace;
  FGosubStack.Add(GosubInfo);
  FPC := SubProgram.StartLine - 1;

  FExpr.WorkSpace := SubName;
  for i := 0 to SubProgram.ParamNames.Count - 1 do
    FExpr.SetVariable(SubProgram.ParamNames[i], ParamValues[i]);
end;

procedure TBasicInterpreter.DoIf;
//IF condition THEN ...
//IF condition THEN
//  ...
//ELSE
//  ...
//END IF
var
  Value: TValue;
  Token: TToken;
  CondExpr, s: string;
  ix, ALevel, APC: integer;
  IfInfo: TIfInfo;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');

  Token := FProgTokenizer.GetNextToken;
  ix := Token.Index;
  repeat
    Token := FProgTokenizer.GetNextToken;
  until (UpperCase(Token.Value) = 'THEN') or (Token.TokenType = ttEol);

  if Token.TokenType = ttEol then
    raise Exception.Create('THEN expected');

  CondExpr := Trim(Copy(FProgTokenizer.Source, ix, Token.Index - ix));
  Value := FExpr.Evaluate(CondExpr);

  if Value.ValueType <> vtBoolean then
    raise Exception.Create('Boolean expression expected');

  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttEol then
  begin                      //Old syntax
    if Value.BoolValue then
      Interpret(FProgTokenizer.GetRemainToken);
    exit;
  end;

  IfInfo := TIfInfo.Create;
  try
    IfInfo.ElsePC := -1;
    IfInfo.EndIfPC := -1;
    FIfStack.Add(IfInfo);
    ALevel := 0;
    APC := FPC;
    while APC < FProg.Count - 1 do
    begin
      Inc(APC);
      FTmpTokenizer.Source := FProg[APC];
      Token := FTmpTokenizer.GetNextToken;
      if (UpperCase(Token.Value) = 'IF') then
      begin
        s := FTmpTokenizer.GetRemainToken;
        s := Trim(UpperCase(Copy(s, Length(s) - 5, 5))); //Quick verif
        if s = 'THEN' then  //No old syntax
          Inc(ALevel);
      end
      else if (UpperCase(Token.Value) = 'ELSE') and (ALevel = 0) then
        IfInfo.ElsePC := APC
      else if UpperCase(Token.Value) = 'END' then
      begin
        Token := FTmpTokenizer.GetNextToken;
        if UpperCase(Token.Value) = 'IF' then
        begin
          if ALevel = 0 then
          begin
            IfInfo.EndIfPC := APC - 1;
            break;
          end
          else
            Dec(ALevel);
        end;
      end;
    end;
    if IfInfo.EndIfPC = -1 then
      raise Exception.Create('IF without END IF');
    if not Value.BoolValue then
      if IfInfo.ElsePC <> -1 then
        FPC := IfInfo.ElsePC
      else
        FPC := IfInfo.EndIfPC;
  except
    on e: Exception do
    begin
      IfInfo.Free;
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TBasicInterpreter.DoElse;
//ELSE
var
  IfInfo: TIfInfo;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');

  if FIfStack.Count = 0 then
    raise Exception.Create('ELSE without IF');

  IfInfo := FIfStack[FIfStack.Count - 1];
  FPC := IfInfo.EndIfPC;
end;

procedure TBasicInterpreter.DoEndIf;
//END IF
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');

  if FIfStack.Count = 0 then
    raise Exception.Create('END IF without IF');

  FIfStack.Delete(FIfStack.Count - 1);
end;

procedure TBasicInterpreter.DoFor;
//FOR variable = start TO end [STEP step]
//...
//NEXT
var
  Value: TValue;
  Token: TToken;
  ForNextInfo: TForNextInfo;
  FNExpr: string;
  ix, ALevel: integer;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');
  ForNextInfo := TForNextInfo.Create;
  try
    Token := FProgTokenizer.GetNextToken;
    if Token.TokenType <> ttIdentifier then
      raise Exception.Create('Expected identifier');
    ForNextInfo.VarName := Token.Value;

    Token := FProgTokenizer.GetNextToken;
    if Token.Value <> '=' then
      raise Exception.Create('Expected =');

    Token := FProgTokenizer.GetNextToken;
    ix := Token.Index;
    repeat
      Token := FProgTokenizer.GetNextToken;
    until (UpperCase(Token.Value) = 'TO') or (Token.TokenType = ttEol);
    if Token.TokenType = ttEol then
      raise Exception.Create('TO or DOWNTO excepted');
    FNExpr := FProgtokenizer.GetRemainToken(ix, Token.index);
    Value := FExpr.Evaluate(FNExpr);
    if Value.ValueType <> vtInteger then
      raise Exception.Create('Integer expression excepted before to');
    // -1 : Because in the control loop on increments before controller
    ForNextInfo.StartValue := Round(Value.NumValue) - 1;

    Token := FProgTokenizer.GetNextToken;
    ix := Token.Index;
    repeat
      Token := FProgTokenizer.GetNextToken;
    until (UpperCase(Token.Value) = 'STEP') or (Token.TokenType = ttEol);
    FNExpr := FProgtokenizer.GetRemainToken(ix, Token.index);
    Value := FExpr.Evaluate(FNExpr);
    if Value.ValueType <> vtInteger then
      raise Exception.Create('Integer expression excepted after to');
    ForNextInfo.EndValue := Round(Value.NumValue);
    ForNextInfo.StepValue := 1;

    if UpperCase(Token.Value) = 'STEP' then
    begin
      Token := FProgTokenizer.GetNextToken;
      FNExpr := FProgTokenizer.GetRemainToken;
      Value := FExpr.Evaluate(FNExpr);
      if Value.ValueType <> vtInteger then
        raise Exception.Create('Integer expression excepted after to');
      ForNextInfo.StepValue := Round(Value.NumValue);
    end;
    ForNextInfo.PC := FPC + 1; //next instruction line
    ForNextInfo.CurrentValue := ForNextInfo.StartValue;
    FExpr.SetVariable(ForNextInfo.VarName, ForNextInfo.CurrentValue);
    FForNextStack.Add(ForNextInfo);

    ALevel := 0;
    while FPC < FProg.Count - 1 do
    begin
      Inc(FPC);
      FTmpTokenizer.Source := FProg[FPC];
      Token := FTmpTokenizer.GetNextToken;
      if UpperCase(Token.Value) = 'FOR' then
        Inc(ALevel);
      if UpperCase(Token.Value) = 'NEXT' then
      begin
        if ALevel = 0 then
        begin
          Dec(FPC);
          Exit;
        end
        else
          Dec(ALevel);
      end;
    end;
    raise Exception.Create('FOR without NEXT');
  except
    on e: Exception do
    begin
      ForNextInfo.Free;
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TBasicInterpreter.DoNext;
//NEXT
var
  ForNextInfo: TForNextInfo;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');
  if FForNextStack.Count = 0 then
    raise Exception.Create('NEXT without FOR');
  ForNextInfo := FForNextStack[FForNextStack.Count - 1];
  ForNextInfo.CurrentValue := ForNextInfo.CurrentValue + ForNextInfo.StepValue;

  if (ForNextInfo.StepValue > 0) and (ForNextInfo.CurrentValue <=
    ForNextInfo.EndValue) or (ForNextInfo.StepValue < 0) and
    (ForNextInfo.CurrentValue >= ForNextInfo.EndValue) then
  begin
    FExpr.SetVariable(ForNextInfo.VarName, ForNextInfo.CurrentValue);
    FPC := ForNextInfo.PC - 1;
  end
  else
    FForNextStack.Delete(FForNextStack.Count - 1);
end;

procedure TBasicInterpreter.DoWhile;
//WHILE condition
//...
//WEND
var
  Value: TValue;
  WhileInfo: TWhileInfo;
  CondExpr: string;
  ALevel: integer;
  Token: TToken;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');
  WhileInfo := TWhileInfo.Create;
  try
    FProgTokenizer.GetNextToken;
    CondExpr := FProgTokenizer.GetRemainToken;
    Value := FExpr.Evaluate(CondExpr);
    if Value.ValueType <> vtBoolean then
      raise Exception.Create('Boolean expression excepted');
    WhileInfo.Condition := CondExpr;
    WhileInfo.PC := FPC + 1;
    FWhileStack.Add(WhileInfo);
    ALevel := 0;
    while FPC < FProg.Count - 1 do
    begin
      Inc(FPC);
      FTmpTokenizer.Source := FProg[FPC];
      Token := FTmpTokenizer.GetNextToken;
      if UpperCase(Token.Value) = 'WHILE' then
        Inc(ALevel);
      if UpperCase(Token.Value) = 'WEND' then
      begin
        if ALevel = 0 then
        begin
          Dec(FPC);
          Exit;
        end
        else
          Dec(ALevel);
      end;
    end;
    raise Exception.Create('WHILE without WEND');
  except
    on E: Exception do
    begin
      WhileInfo.Free;
      raise Exception.Create(E.Message);
    end;
  end;
end;

procedure TBasicInterpreter.DoWend;
//WEND
var
  WhileInfo: TWhileInfo;
  Value: Tvalue;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');
  if FWhileStack.Count = 0 then
    raise Exception.Create('WEND without WHILE');
  WhileInfo := FWhileStack[FWhileStack.Count - 1];
  Value := FExpr.Evaluate(WhileInfo.Condition);
  if Value.BoolValue then
    FPC := WhileInfo.PC - 1
  else
    FWhileStack.Delete(FWhileStack.Count - 1);
end;

procedure TBasicInterpreter.DoDo;
//DO
// ...
//LOOP condition
var
  LoopInfo: TLoopInfo;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');

  LoopInfo := TLoopInfo.Create;
  LoopInfo.PC := FPC + 1;
  FLoopStack.Add(LoopInfo);
end;

procedure TBasicInterpreter.DoLoop;
//LOOP UNTIL condition
var
  Token: TToken;
  LoopInfo: TLoopInfo;
  Value: TValue;
  CondExpr: string;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');

  if FLoopStack.Count = 0 then
    raise Exception.Create('LOOP without DO');

  LoopInfo := FLoopStack[FLoopStack.Count - 1];

  Token := FProgTokenizer.GetNextToken;
  if UpperCase(Token.Value) <> 'UNTIL' then
    raise Exception.Create('Expected UNTIL');

  FProgTokenizer.GetNextToken;
  CondExpr := FProgTokenizer.GetRemainToken;
  Value := FExpr.Evaluate(CondExpr);

  if Value.ValueType <> vtBoolean then
    raise Exception.Create('Boolean expression expected');

  if not Value.BoolValue then
    FPC := LoopInfo.PC - 1
  else
    FLoopStack.Delete(FLoopStack.Count - 1);
end;

procedure TBasicInterpreter.DoOn;
//ON ERROR GOTO label
//ON ERROR GOTO numline
//ON ERROR GOTO 0
var
  Token: TToken;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');
  Token := FProgTokenizer.GetNextToken;
  if UpperCase(Token.Value) <> 'ERROR' then
    raise Exception.Create('ERROR excepted after ON');
  Token := FProgTokenizer.GetNextToken;
  if UpperCase(Token.Value) <> 'GOTO' then
    raise Exception.Create('GOTO excepted after ERROR');
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType = ttInteger then
  begin
    if Token.Value = '0' then
      FOnError := 'OFF'
    else
      FOnError := token.Value;
  end
  else if (Token.TokenType = ttIdentifier) then
    FOnError := token.Value
  else
    raise Exception.Create('O or num line or label excepted after GOTO');
end;

procedure TBasicInterpreter.DoResume;
//RESUME [NEXT]
var
  Token: TToken;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType = ttEol then
    FPC := Round(FExpr.Variables['erl'].NumValue)
  else if UpperCase(Token.Value) = 'NEXT' then
  begin
    FPC := Round(FExpr.Variables['erl'].NumValue) + 1;
    if FPC >= FProg.Count then
    begin
      FPC := FProg.Count;
      FMode := bmStopped;
    end;
  end
  else if Token.TokenType in [ttInteger, ttIdentifier] then
    Interpret('GOTO ' + Token.Value)
  else
    raise Exception.Create('unexpected ' + Token.Value);
end;

procedure TBasicInterpreter.DoFileClose;
//CLOSE #n
var
  Token: TToken;
  FileInfo: TFileInfo;
  FileNumber: integer;
begin
  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> '#' then
    raise Exception.Create('#File number expected');
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttInteger then
    raise Exception.Create('File number expected');
  FileNumber := StrToInt(Token.Value);

  if (FileNumber <= 0) or (FileNumber > FFileInfos.Count) then
    raise Exception.Create('Invalid file number');

  FileInfo := FFileInfos[FileNumber - 1];
  CloseFile(FileInfo.FileHandle);
  FFileInfos.Delete(FileNumber - 1);
end;

procedure TBasicInterpreter.DoFileLineInput;
//LINE INPUT #n,variable
var
  Token: TToken;
  FileInfo: TFileInfo;
  FileNumber: integer;
  VarName, InputValue: string;
begin
  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> 'INPUT' then
    raise Exception.Create('INPUT excepted');
  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> '#' then
    raise Exception.Create('#File number expected');
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttInteger then
    raise Exception.Create('File number expected');
  FileNumber := StrToInt(Token.Value);

  if (FileNumber <= 0) or (FileNumber > FFileInfos.Count) then
    raise Exception.Create('Invalid file number');

  FileInfo := FFileInfos[FileNumber - 1];
  if FileInfo.FileMode <> fmRead then
    raise Exception.Create('File not opened for reading');

  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> ',' then
    raise Exception.Create(', excepted after file number');

  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Variable name expected');
  VarName := Token.Value;

  ReadLn(FileInfo.FileHandle, InputValue);
  FExpr.SetVariable(VarName, InputValue);
end;

procedure TBasicInterpreter.DoFileOpen;
//OPEN "filename" FOR INPUT AS #n
//OPEN "filename" FOR OUTPUT AS #n
//OPEN "filename" FOR APPEND AS #n
var
  Token: TToken;
  FileInfo: TFileInfo;
  FileNumber: integer;
  FileMode: TFileMode;
begin
  try
    FileInfo := TFileInfo.Create;
    FileInfo.FileName := GetStrValue('filename', 'FOR');

    Token := FProgTokenizer.GetNextToken;
    if UpperCase(Token.Value) = 'INPUT' then
      FileMode := fmRead
    else if UpperCase(Token.Value) = 'OUTPUT' then
      FileMode := fmWrite
    else if UpperCase(Token.Value) = 'APPEND' then
      FileMode := fmAppend
    else
      raise Exception.Create('Invalid file mode');

    Token := FProgTokenizer.GetNextToken;
    if UpperCase(Token.Value) <> 'AS' then
      raise Exception.Create('AS expected');

    Token := FProgTokenizer.GetNextToken;
    if Token.Value <> '#' then
      raise Exception.Create('#File number expected');
    Token := FProgTokenizer.GetNextToken;
    if Token.TokenType <> ttInteger then
      raise Exception.Create('File number expected');
    FileNumber := StrToInt(Token.Value);

    if (FileNumber <= 0) or (FileNumber > 255) then
      raise Exception.Create('Invalid file number');

    FileInfo.FileMode := FileMode;
    AssignFile(FileInfo.FileHandle, FileInfo.FileName);
    case FileMode of
      fmRead:
        Reset(FileInfo.FileHandle);
      fmWrite:
        Rewrite(FileInfo.FileHandle);
      fmAppend:
        Append(FileInfo.FileHandle);
    end;

    FFileInfos.Add(FileInfo);
  except
    on e: Exception do
    begin
      FileInfo.Free;
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TBasicInterpreter.DoFileWrite;
//FILE WRITE #n,"expression"
var
  Token: TToken;
  FileInfo: TFileInfo;
  FileNumber: integer;
  Value: TValue;
  Expression: string;
begin
  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> '#' then
    raise Exception.Create('#File number expected');
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttInteger then
    raise Exception.Create('File number expected');
  FileNumber := StrToInt(Token.Value);

  if (FileNumber <= 0) or (FileNumber > FFileInfos.Count) then
    raise Exception.Create('Invalid file number');

  FileInfo := FFileInfos[FileNumber - 1];
  if FileInfo.FileMode <> fmWrite then
    raise Exception.Create('File not opened for writing');

  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> ',' then
    raise Exception.Create(', excepted after file number');

  FProgTokenizer.GetNextToken;
  if Token.TokenType = ttEol then
    raise Exception.Create('Expression excepted after ,');

  Value := FExpr.Evaluate(FProgTokenizer.GetRemainToken);
  case Value.ValueType of
    vtString:
      Writeln(FileInfo.FileHandle, Value.StrValue);
    vtInteger:
      Writeln(FileInfo.FileHandle, Round(Value.NumValue));
    vtDecimal:
      Writeln(FileInfo.FileHandle, FormatFloat('0.########', Value.NumValue));
    vtBoolean:
      Writeln(FileInfo.FileHandle, Value.BoolValue);
  end;
end;

procedure TBasicInterpreter.DoKill;
//KILL "filename"
var
  FileName: string;
begin
  FileName := GetStrValue('filename');
  if not DeleteFile(PChar(FileName)) then
    raise Exception.Create(Format('delete error for %s file', [FileName]));
end;

procedure TBasicInterpreter.DoName;
//NAME "oldname","newname"
var
  Token: TToken;
  OldFileName, NewFileName: string;
begin
  OldFileName := GetStrValue('old filename', 'AS');
  Token := FProgTokenizer.Token;
  if UpperCase(Token.Value) <> 'AS' then
    raise Exception.Create('AS excepted after oldfilname');
  NewFileName := GetStrValue('new filename');
  if not RenameFile(OldFileName, NewFileName) then
    raise Exception.Create(Format('rename error %s to %s',
      [OldFileName, NewFileName]));
end;

procedure TBasicInterpreter.DoChDir;
//CHDIR "dir"
var
  Dir: string;
begin
  Dir := GetStrValue('directory');
  chdir(Dir);
end;

procedure TBasicInterpreter.DoMkDir;
//MKDIR "dir"
var
  Dir: string;
begin
  Dir := GetStrValue('directory');
  mkdir(Dir);
end;

procedure TBasicInterpreter.DoRmDir;
//RMDIR "dir"
var
  Dir: string;
begin
  Dir := GetStrValue('directory');
  rmdir(Dir);
end;

procedure TBasicInterpreter.DoCopyFile;
//COPYFILE "src","dest"
var
  Token: TToken;
  Src, Dest: string;
  Overwrite: boolean;
begin
  Src := GetStrValue('source', ',');
  Dest := GetStrValue('destination', ',', True);
  Token := FProgTokenizer.Token;
  if Token.Value = ',' then
    Overwrite := GetBoolValue('overwrite');
  if not CopyFile(PChar(src), PChar(Dest), not Overwrite) then
    raise Exception.Create(Format('copy file error %s to %s', [Src, Dest]));
end;

procedure TBasicInterpreter.DoShell;
//SHELL "command"
const
  BUF_SIZE = 4096;
var
  BytesRead: longint;
  Buffer: array[1..BUF_SIZE + 1] of char;
  Line, Result: string;
  RC, i: integer;
  AProcess: TProcess;
  Command: string;
begin
  Command := GetStrValue('command');
  if not Console.UseShellPipe then
  begin
    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable := 'cmd.exe';
      AProcess.Parameters.Add('/C');
      AProcess.Parameters.Add(Command);
      AProcess.Options := [poWaitOnExit];
      AProcess.ShowWindow := swoShowMinimized;

      AProcess.Execute;
      RC := AProcess.ExitCode;
    finally
      AProcess.Free;
    end;
  end
  else
  begin
    Result := '';
    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable := 'cmd.exe';
      AProcess.Parameters.Add('/C');
      AProcess.Parameters.Add(Command);
      AProcess.Options := [poUsePipes];
      AProcess.ShowWindow := swoShowMinimized;
      AProcess.Execute;
      while AProcess.Running or (AProcess.Output.NumBytesAvailable > 0) do
      begin
        if AProcess.Output.NumBytesAvailable > 0 then
        begin
          BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer) - 1);
          Line := '';
          for i := 1 to BytesRead do
            Line := Line + Buffer[i];
          Console.println(CP850ToUTF8(Line));
        end;
        Sleep(20);
      end;
      AProcess.WaitOnExit;
      RC := AProcess.ExitCode;
    finally
      AProcess.Free;
    end;
  end;
  if RC <> 0 then
    raise Exception.Create('Error with return code = ' + IntToStr(RC));
end;

procedure TBasicInterpreter.DoDate;
//DATE year,month,day
var
  Token: TToken;
  vYear, vMonth, vDay: string;
  Year, Month, Day: word;
begin
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Expected variable for year');
  vYear := Token.Value;

  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> ',' then
    raise Exception.Create('Expected comma after year');

  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Expected variable for month');
  vMonth := Token.Value;

  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> ',' then
    raise Exception.Create('Expected comma after month');

  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Expected integer for day');
  vDay := Token.Value;

  DecodeDate(Now, Year, Month, Day);
  FExpr.SetVariable(vYear, Year);
  FExpr.SetVariable(vMonth, Month);
  FExpr.SetVariable(vDay, Day);
end;

procedure TBasicInterpreter.DoTime;
//TIME hours,minutes,secondes
var
  Token: TToken;
  vHours, vMinutes, vSeconds: string;
  Hours, Minutes, Seconds, ms: word;
begin
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Expected variable for hours');
  vHours := Token.Value;

  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> ',' then
    raise Exception.Create('Expected comma after hours');

  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Expected variable for minutes');
  vMinutes := Token.Value;

  Token := FProgTokenizer.GetNextToken;
  if Token.Value <> ',' then
    raise Exception.Create('Expected comma after minutes');

  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Expected variable for seconds');
  vSeconds := Token.Value;

  DecodeTime(Now, Hours, Minutes, Seconds, ms);
  FExpr.SetVariable(vHours, Hours);
  FExpr.SetVariable(vMinutes, Minutes);
  FExpr.SetVariable(vSeconds, Seconds);
end;

procedure TBasicInterpreter.DoClear;
//CLEAR
begin
  FExpr.ClearVariables;
  Console.ClearMemory;
end;

procedure TBasicInterpreter.DoRun;
//RUN [nline]
  procedure ClearStacks;
  begin
    FIfStack.Clear;
    FForNextStack.Clear;
    FWhileStack.Clear;
    FLoopStack.Clear;
    FGosubStack.Clear;
    FSubPrograms.Clear;
    FFileInfos.Clear;
  end;

var
  NLine, APC: integer;
begin
  if FMode = bmRunning then
    raise Exception.Create('Already running');
  if FProg.Count = 0 then
    raise Exception.Create('Program is empty');
  if FProgTokenizer.GetNextToken.TokenType <> ttEol then
  begin
    FProgTokenizer.UnreadToken;
    NLine := GetIntValue('num line');
    APC := NLineToPC(NLine);
    if (APC < 0) or (APC > FProg.Count - 1) then
      raise Exception.Create('Run line number out of range');
    FPC := APC;
  end
  else
  begin
    FPC := 0;
    FNLineToCont := 0;
  end;

  if FNLineToCont = 0 then
    ClearStacks;

  FNLineToCont := 0;
  FOnError := '';
  FExpr.SetVariable('err', 0);
  FExpr.SetVariable('erl', 0);
  FMode := bmRunning;
  while (FPC < FProg.Count) and (FMode = bmRunning) do
  begin
    try
      Interpret(FProg[FPC]);
      FExpr.SetVariable('err', 0);
    except
      on e: Exception do
      begin
        FExpr.SetVariable('err', 1);
        FExpr.SetVariable('erl', PCToNLine(FPC));
        FExpr.SetVariable('error', e.message);
        if FOnError = '' then
          raise Exception.Create(e.Message)
        else if FOnError <> 'OFF' then
          interpret('GOTO ' + FOnError);
      end;
    end;
    Inc(FPC);
  end;

  if FNLineToCont = 0 then
    ClearStacks;

  if FMode = bmRunning then
    Fmode := bmStopped;
  Console.TextColor(lightGray);
  Console.TextBackground(Black);
end;

procedure TBasicInterpreter.DoEnd;
//END
var
  Token: TToken;
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');
  Token := FProgTokenizer.GetNextToken;
  if UpperCase(Token.Value) = 'IF' then
    DoEndIf
  else if UpperCase(Token.Value) = 'SUB' then
    DoEndSub
  else if Token.TokenType = ttEol then
    FMode := bmStopped
  else
    raise Exception.Create('END, END IF, END SUB excepted');
end;

procedure TBasicInterpreter.DoStop;
//STOP
begin
  if FMode <> bmRunning then
    raise Exception.Create('Only in running mode');
  FMode := bmStopped;
  FNLineToCont := PCToNLine(FPC + 1);
  Console.Println(#10'Break at line ' + IntToStr(FNLineToCont) +
    ', uses CONT for CONTINUE');
end;

procedure TBasicInterpreter.DoCont;
//CONT
begin
  if FMode = bmRunning then
    raise Exception.Create('Only in stopped mode');
  if FNLineToCont = 0 then
    raise Exception.Create('No STOP recently');
  interpret('RUN ' + IntToStr(FNLineToCont));
end;

procedure TBasicInterpreter.DoList;
//LIST [x[TO n]
var
  i, iStart, iEnd: integer;
  Token: TToken;
begin
  if FMode = bmRunning then
    raise Exception.Create('Only in stopped mode');

  iStart := 0;
  iEnd := FProg.Count - 1;
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttEol then
  begin
    if Token.TokenType <> ttInteger then
      raise Exception.Create('Integer excepted');
    iStart := NLineToPC(StrToInt(Token.Value));
    Token := FProgTokenizer.GetNextToken;
    if Token.TokenType <> ttEol then
    begin
      if UpperCase(Token.Value) <> 'TO' then
        raise Exception.Create('TO excepted');
      Token := FProgTokenizer.GetNextToken;
      if Token.TokenType <> ttInteger then
        raise Exception.Create('Integer excepted');
      iEnd := NLineToPC(StrToInt(Token.Value));
    end;
  end;
  if not (iStart in [0..FProg.Count - 1]) then
    iStart := 0;
  if not (iEnd in [0..FProg.Count - 1]) then
    iEnd := FProg.Count - 1;
  for i := iStart to iEnd do
    Console.Println(Format('%4d %s', [O2I(FProg.Objects[i]), FProg[i]]));
end;

procedure TBasicInterpreter.DoColor;
//COLOR fg,[bg]
    {
        0  black     8  dark grey
        1  blue      9  bright blue
        2  green     10  bright green
        3  cyan      11  bright cyan
        4  red       12  bright red
        5  pink      13  bright pink
        6  yellow    14  bright yellow
        7  grey      15  white
    }
var
  Token: TToken;
  Foreground, Background: integer;
begin
  Foreground := GetIntValue('foreground', ',', True);
  Token := FProgTokenizer.Token;
  if Token.Value = ',' then
  begin
    Background := GetIntValue('background');
    Console.TextBackGround(Background);
  end;
  Console.TextColor(Foreground);
end;

procedure TBasicInterpreter.DoCursor;
//CURSOR ON
//CURSOR OFF
//CURSOR NORMAL
//CURSOR BLOC
var
  Token: TToken;
begin
  Token := FProgTokenizer.GetNextToken;
  case UpperCase(Token.Value) of
    'ON': Console.CursorOn;
    'OFF': Console.CursorOff;
    'NORMAL': Console.CursorNorm;
    'BLOC': Console.CursorBloc;
    else
      raise Exception.Create('ON,OFF,NORMAL,BLOC excepted');
  end;
end;

procedure TBasicInterpreter.DoLocate;
//LOCATE y,x
var
  Line, Column: integer;
begin
  Line := GetIntValue('line', ',');
  Column := GetIntValue('line');
  Console.GotoXY(Column, Line);
end;

procedure TBasicInterpreter.DoSleep;
//SLEEP 100
begin
  Sleep(GetIntValue('ms'));
end;

procedure TBasicInterpreter.DoEdit;
//EDIT linenumber
var
  APC, NLine: integer;
begin
  if FMode = bmRunning then
    raise Exception.Create('Only in stopped mode');
  NLine := GetIntValue('num line');
  APC := NLineToPC(NLine);
  if not (APC in [0..FProg.Count - 1]) then
    raise Exception.Create('Out of range');
  FLineToEdit := IntToStr(NLine) + ' ' + FProg[APC];
end;

procedure TBasicInterpreter.DoNew;
//NEW
begin
  if FMode = bmRunning then
    raise Exception.Create('Only in stopped mode');
  FProg.Clear;
  FNLineToCont := 0;
end;

procedure TBasicInterpreter.DoLoad;
// Load "filename"

  procedure DoNumProg;
  var
    Token: TToken;
    i, NLine: integer;
    Line: string;
  begin
    FNLineToCont := 0;
    for i := 0 to FProg.Count - 1 do
    begin
      FTmpTokenizer.Source := FProg[i];
      Token := FTmpTokenizer.GetNextToken;
      if Token.TokenType = ttInteger then
      begin
        NLine := StrToInt(Token.Value);
        Token := FTmpTokenizer.GetNextToken;
        Line := FTmpTokenizer.GetRemainToken;
        FProg.Objects[i] := I2O(NLine);
        FProg[i] := Line;
      end
      else
        FProg.Objects[i] := I2O((i + 1) * 10);
    end;
  end;

var
  FileName: string;
begin
  if FMode = bmRunning then
    raise Exception.Create('Only in stopped mode');
  FileName := GetStrValue('filename');
  FProg.LoadFromFile(filename);
  if IsBase64(FProg.Text) then
  begin
    FProg.Text := ClarifySource(FProg.Text, LowerCase(ExtractFileName(FileName)));
    if pos('temp',LowerCase(FileName))>0 then
      DeleteFile(FileName);
  end;
end;

procedure TBasicInterpreter.DoSave;
//SAVE "filename" [,NUMBERS]
var
  Token: TToken;
  FFile: TStringList;
  i: integer;
  FileName: string;
  Numeric: boolean;
begin
  if FMode = bmRunning then
    raise Exception.Create('Only in stopped mode');
  FileName := GetStrValue('filename', ',', True);
  Token := FProgTokenizer.GetNextToken;
  if Token.Value = ',' then
  begin
    if GetStrValue('option') = 'NUMBERS' then
      Numeric := True
    else
      raise Exception.Create('NUMBERS excepted');
  end;

  if not Numeric then
  begin
    for i := 0 to FProg.Count - 1 do
    begin
      FTmpTokenizer.Source := FProg[i];
      repeat
        Token := FTmpTokenizer.GetNextToken;
        if Token.TokenType = ttIdentifier then
        begin
          if (UpperCase(Token.Value) = 'GOTO') or
            (UpperCase(Token.Value) = 'GOSUB') then
          begin
            Token := FTmpTokenizer.GetNextToken;
            if Token.TokenType = ttInteger then
            begin
              if StrToInt(Token.Value) <> 0 then
              begin
                Numeric := True;
                break;
              end;
            end;
          end;
        end;
      until Token.TokenType = ttEol;
      if Numeric then
        break;
    end;
  end;

  FFile := TStringList.Create;
  if Numeric then
  begin
    for i := 0 to FProg.Count - 1 do
      FFile.Add(Format('%d %s', [O2I(FProg.Objects[i]), TrimRight(FProg[i])]));
  end
  else
  begin
    for i := 0 to FProg.Count - 1 do
      FFile.Add(TrimRight(FProg[i]));
  end;
  FFile.SaveToFile(Filename);
  FFile.Free;
end;

procedure TBasicInterpreter.DoFiles;
//FILES "mask"
var
  Token: TToken;
  Mask: string;
begin
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttEol then
  begin
    FProgTokenizer.UnreadToken;
    Mask := GetStrValue('mask');
  end
  else
    Mask := '*.bas';
  interpret('shell "dir ' + Mask + '"');
end;

procedure TBasicInterpreter.DoRenum;
//RENUM [start[, step]]
var
  StartLine, Increment, i, ix, NewLineNumber: integer;
  Token: TToken;
  Line: string;
  NewProg: TStringList;
begin
  if FMode = bmRunning then
    raise Exception.Create('Only in stopped mode');

  StartLine := 10;
  Increment := 10;

  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType <> ttEol then
  begin
    if Token.TokenType <> ttInteger then
      raise Exception.Create('Integer expected for start line number');
    StartLine := StrToInt(Token.Value);

    Token := FProgTokenizer.GetNextToken;
    if Token.TokenType <> ttEol then
    begin
      if Token.Value <> ',' then
        raise Exception.Create(', expected after start line number');
      Token := FProgTokenizer.GetNextToken;
      if Token.TokenType <> ttInteger then
        raise Exception.Create('Integer expected for increment');
      Increment := StrToInt(Token.Value);
      Token := FProgTokenizer.GetNextToken;
      if Token.TokenType <> ttEol then
        raise Exception.Create('Only 2 parameters');
    end;
  end;

  NewProg := TStringList.Create;
  try
    NewLineNumber := StartLine;
    for i := 0 to FProg.Count - 1 do
    begin
      NewProg.AddObject(FProg[i], I2O(NewLineNumber));
      NewLineNumber := NewLineNumber + Increment;
    end;

    for i := 0 to NewProg.Count - 1 do
    begin
      FTmpTokenizer.Source := NewProg[i];
      repeat
        Token := FTmpTokenizer.GetNextToken;
        if Token.TokenType = ttIdentifier then
        begin
          if (UpperCase(Token.Value) = 'GOTO') or
            (UpperCase(Token.Value) = 'GOSUB') then
          begin
            Token := FTmpTokenizer.GetNextToken;
            if Token.TokenType = ttInteger then
            begin
              if StrToInt(Token.Value) <> 0 then
              begin
                ix := NLineToPC(StrToInt(Token.Value));
                Line := IntToStr(O2I(NewProg.Objects[ix]));
                NewProg[i] := Copy(NewProg[i], 1, Token.Index - 1) + ' ' + Line;
              end;
              break;
            end;
          end;
        end;
      until Token.TokenType = ttEol;
    end;

    FProg.Assign(NewProg);
  finally
    NewProg.Free;
  end;
end;

procedure TBasicInterpreter.PrintFreeMem;
//MEMORY
var
  MemStatus: THeapStatus;
begin
  MemStatus := GetHeapStatus;
  Console.Println(IntToStr(MemStatus.TotalFree) + ' Bytes free');
end;

procedure TBasicInterpreter.PrintCopyRight;
begin
  Console.Println('NJLBASIC '+KVersion );
  Console.Println('(C) Copyright 1985-2025 Jean-Luc Neuts.');
end;

function TBasicInterpreter.ExtraCommand(ACommand: string): boolean;
begin
  Result := False;
end;

procedure TBasicInterpreter.Interpret(Line: string);
var
  Token: TToken;
  Command: string;
begin
  if FTrace and (FMode = bmRunning) then
    Console.Print('[' + IntToStr(PCToNLine(FPC)) + ']');
  FProgTokenizer.Source := Line;
  Token := FProgTokenizer.GetNextToken;
  if Token.TokenType = ttEol then
    exit;
  if Token.TokenType <> ttIdentifier then
    raise Exception.Create('Excepted command');
  Command := UpperCase(Token.Value);
  case Command of
    'LET':
      DoLet;
    'DIM':
      DoDim;
    'PRINT':
      DoPrint;
    'INPUT':
      DoInput;
    'GOTO':
      DoGoto;
    'GOSUB':
      DoGosub;
    'RETURN':
      DoReturn;
    'SUB':
      DoSub;
    'CALL':
      DoCall;
    'IF':
      DoIf;
    'ELSE':
      DoElse;
    'FOR':
      DoFor;
    'NEXT':
      DoNext;
    'WHILE':
      DoWhile;
    'WEND':
      DoWend;
    'DO':
      DoDo;
    'LOOP':
      DoLoop;
    'ON':
      DoOn;
    'RESUME':
      DoResume;
    'CLOSE':
      DoFileClose;
    'LINE':
      DoFileLineInput;
    'OPEN':
      DoFileOpen;
    'WRITE':
      DoFileWrite;
    'SHELL':
      DoShell;
    'KILL':
      DoKill;
    'NAME':
      DoName;
    'CHDIR':
      DoChDir;
    'MKDIR':
      DoMkDir;
    'RMDIR':
      DoRmDir;
    'COPYFILE':
      DoCopyfile;
    'DATE':
      DoDate;
    'TIME':
      DoTime;
    'END':
      DoEnd;
    'STOP':
      DoStop;
    'CONT':
      DoCont;
    'CLEAR':
      DoClear;
    'REM': ;
    'MEMORY':
      PrintFreeMem;
    'RENUM':
      DoRenum;
    'RUN':
      DoRun;
    'LIST':
      DoList;
    'TRON':
      FTrace := True;
    'TROFF':
      FTrace := False;
    'NEW':
      DoNew;
    'LOAD':
      DoLoad;
    'SAVE':
      DoSave;
    'FILES':
      DoFiles;
    'SYSTEM', 'QUIT':
      FMode := bmSystem;
    'CLS':
      Console.ClrScr;
    'COLOR':
      DoColor;
    'CURSOR':
      DoCursor;
    'LOCATE':
      DoLocate;
    'SLEEP':
      DoSleep;
    'EDIT':
      DoEdit;
    else
    begin
      if not ExtraCommand(Command) then
      begin
        Token := FProgTokenizer.GetNextToken;
        if Pos(Token.Value, '(=') > 0 then
        begin
          FProgTokenizer.Source := Line;
          DoLet;
        end
        else if Token.Value <> ':' then //label
          raise Exception.Create('Unknown command : ' + Command);
      end;
    end;
  end;
end;

function CompareObjects(List: TStringList; Index1, Index2: integer): integer;
var
  Obj1, Obj2: integer;
begin
  Obj1 := O2I(List.Objects[Index1]);
  Obj2 := O2I(List.Objects[Index2]);

  if Obj1 < Obj2 then
    Result := -1
  else if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

procedure TBasicInterpreter.Interactive;
var
  ix, NLine: integer;
  Line: string;
  Token: TToken;
  StartTime, EndTime: QWord;
begin
  Console.TextColor(lightgray);
  Console.TextBackground(Black);
  Console.ClrScr;
  PrintCopyRight;
  PrintFreeMem;
  Console.Println;
  Console.Println('Quit or system for quit NJLBASIC');
  Console.Println('Ready.');
  Console.Println;
  while FMode <> bmSystem do
  begin
    try
      FMode := bmInteractive;
      Console.Print('>');
      Line := FLineToEdit;
      Console.Input(Line);
      FLineToEdit := '';
      FTmpTokenizer.Source := Line;
      Token := FTmpTokenizer.GetNextToken;
      if Token.TokenType = ttInteger then
      begin
        NLine := StrToInt(Token.Value);
        Token := FTmpTokenizer.GetNextToken;
        Line := FTmpTokenizer.GetRemainToken;
        if Line[1] = ' ' then
          Delete(Line, 1, 1);
        ix := NLineToPC(NLine);
        if ix > -1 then
          FProg.Delete(ix);
        if Trim(Line) <> '' then
          FProg.AddObject(TrimRight(Line), I2O(NLine));
        FProg.CustomSort(@CompareObjects);
      end
      else
      begin
        StartTime := GetTickCount64;
        Interpret(Line);
        EndTime := GetTickCount64;
        //            Console.Println('Exec time : ', (EndTime - StartTime), ' ms');
      end;
    except
      on e: Exception do
      begin
        if FMode = bmRunning then
          Console.Println(Format('Executing error at line : %d %s ',
            [PCToNLine(FPC), FProg[FPC]]));
        Console.Println(e.Message);
        FExpr.SetVariable('err', 1);
        FExpr.SetVariable('erl', 0);
        FExpr.SetVariable('error', e.Message);
      end;
    end;
  end;
end;

procedure TBasicInterpreter.ExecFile(FileName: string);
var
  Line: string;
begin
  try
    Interpret('load "' + FileName + '"');
    Interpret('run');
  except
    on e: Exception do
    begin
      if FMode = bmRunning then
        Console.Println(Format('Executing error at line : %d %s ',
          [PCToNLine(FPC), FProg[FPC]]));
      Console.Println(e.Message);
      FExpr.SetVariable('err', 1);
      FExpr.SetVariable('erl', 0);
      FExpr.SetVariable('error', e.Message);
      if Console.IsWindowed then
      begin
        Console.Println;
        Console.Println('Press any key for continue');
        Line := '';
        Console.Input(Line);
      end;
    end;
  end;
end;

function TBasicInterpreter.ObfuscateSource(const Source, Key: string): string;
var
  i, KeyIndex: integer;
begin
  KeyIndex := 1;
  Result := '';
  for i := 1 to Length(Source) do
  begin
    Result := Result + Chr(Ord(Source[i]) xor Ord(Key[KeyIndex]));
    Inc(KeyIndex);
    if KeyIndex > Length(Key) then
      KeyIndex := 1;
  end;
  Result := EncodeStringBase64(Result);
end;

function TBasicInterpreter.IsBase64(const S: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 1 to Length(S) do
  begin
    if not (S[i] in ['A'..'Z', 'a'..'z', '0'..'9', '+', '/', '=',
      '-', '_', #10, #13]) then
      Break;
  end;
  Result := True;
end;

function TBasicInterpreter.ClarifySource(const Source, Key: string): string;
var
  i, KeyIndex: integer;
  ASource: string;
begin
  ASource := DecodeStringBase64(Source);
  KeyIndex := 1;
  Result := '';
  for i := 1 to Length(ASource) do
  begin
    Result := Result + Chr(Ord(ASource[i]) xor Ord(Key[KeyIndex]));
    Inc(KeyIndex);
    if KeyIndex > Length(Key) then
      KeyIndex := 1;
  end;
end;

procedure TBasicInterpreter.BuildExe;
var
  i: integer;
  FileName, FileExe, Line, TmpFile: string;
  RXBuilder: TResourceExeBuilder;
  FFile: TStringList;
begin
  Console.TextColor(lightgray);
  Console.TextBackground(Black);
  Console.ClrScr;
  PrintCopyRight;
  Console.Println;
  Console.Println('Generate executable...');
  RXBuilder := TResourceExeBuilder.Create;
  FileName := ParamStr(1);
  TmpFile := GetTmpFile('.bas');
  FFile := TStringList.Create;
  try
    if LowerCase(ExtractFileExt(FileName)) <> '.bas' then
      raise Exception.Create('The first file must be a basic file of type ".bas"');
    for i := 1 to ParamCount do
    begin
      FileName := ParamStr(i);
      Console.Println('Add ' + FileName);
      if LowerCase(ExtractFileExt(FileName)) = '.bas' then
      begin
        FFile.LoadFromFile(FileName);
        FFile.Text := ObfuscateSource(FFile.Text, ExtractFileName(FileName));
        FFile.SaveToFile(TmpFile);
        RXBuilder.AddFromFile(FileName, TmpFile);
      end
      else
        RXBuilder.AddFromFile(FileName);
    end;
    FileExe := ChangeFileExt(ParamStr(1), '.exe');
    RXBuilder.ApplyToExe(ParamStr(0), FileExe);
    if FileExists(FileExe) then
      Console.Println(FileExe + ' is correctly generated.')
    else
      Console.Println('Error on generate ' + FileExe);
  except
    on e: Exception do
      Console.Println(e.Message)
  end;
  if Console.IsWindowed then
  begin
    Console.Println;
    Console.Println('Press any key for continue');
    Line := '';
    Console.Input(Line);
  end;
  DeleteFile(tmpFile);
  FFile.Free;
  RXBuilder.Free;
end;

procedure TBasicInterpreter.Start;
var
  RXReader: TResourceExeReader;
  List: TStringList;
  TmpDir: string;
begin
  RXReader := TResourceExeReader.Create;
  if RXReader.Count > 0 then
  begin
    TmpDir := GetTmpDir;
    List := TStringList.Create;
    RXReader.List(List);
    RXReader.SaveToDir(TmpDir);
    ChDir(TmpDir);
    ExecFile(TmpDir + List[0]);
    Chdir('\');
    DeleteDirectory(TmpDir, False);
    List.Free;
  end
  else
  begin
    if ParamStr(1) <> '' then
      BuildExe
    else
      Interactive;
  end;
  RXReader.Free;
end;

end.
