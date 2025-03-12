{
  *****************************************************************************
   Unit        : ubasicvarfunc
   Author      : NEUTS JL
   License     : GPL (GNU General Public License)
   Date        : 01/03/2025

   Description : Functions library and vairables for NJLBasic


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
unit ubasicvarfunc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uExpression, utokenizer, Math;

procedure RegisterVar(ExpressionParser: TExpressionParser);
procedure RegisterFunc(ExpressionParser: TExpressionParser);

implementation

function DoAbs(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtDecimal;
  Result.NumValue := Abs(Parser.GetDecValue);
end;

function DoAsc(Parser: TExpressionParser): TValue;
var
  s: string;
begin
  Result.ValueType := vtInteger;
  s := Parser.GetStrValue;
  if s = '' then
    raise Exception.Create('String is empty');
  if Length(s) > 1 then
    raise Exception.Create('Only one char');
  Result.NumValue := Ord(s[1]);
end;

function DoAverage(Parser: TExpressionParser): TValue;
var
  Sum: float;
  n: integer;
begin
  Sum := 0;
  n := 0;
  repeat
    Sum := Sum + Parser.GetDecValue;
    Inc(n);
  until Parser.Tokenizer.GetNextToken.Value <> ',';
  Parser.Tokenizer.UnreadToken;
  Result.ValueType := vtDecimal;
  Result.NumValue := sum / n;
end;

function DoChr(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := Chr(Parser.GetIntValue);
end;

function DoCInt(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtDecimal;
  Result.NumValue := Round(Parser.GetDecValue);
end;

function GetCommand: string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to ParamCount do
    Result := Result + ParamStr(i) + ' ';
end;

function DoCos(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtDecimal;
  Result.NumValue := Cos(Parser.GetDecValue);
end;

function DoCurdir(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := GetCurrentDir;
end;

var
  Files: TSearchRec;

function DoDir(Parser: TExpressionParser): TValue;
var
  Mask: string;
  Token: TToken;
begin
  Result.ValueType := vtString;
  Result.StrValue := '';
  Token := Parser.Tokenizer.GetNextToken;
  Parser.Tokenizer.UnreadToken;
  if Token.Value = ')' then
  begin
    if FindNext(Files) = 0 then
      Result.StrValue := Files.Name;
  end
  else
  begin
    Mask := Parser.GetStrValue;
    if FindFirst(Mask, faAnyFile, Files) = 0 then
      Result.StrValue := Files.Name;
  end;
end;

function DoDirExists(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtBoolean;
  Result.BoolValue := DirectoryExists(Parser.GetStrValue);
end;

function DoDate(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := DateToStr(Now);
end;

function DoEnviron(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := GetEnvironmentVariable(Parser.GetStrValue);
end;

function DoExp(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtDecimal;
  Result.NumValue := Exp(Parser.GetDecValue);
end;

function DoFileExists(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtBoolean;
  Result.BoolValue := FileExists(Parser.GetStrValue);
end;

function DoFileLen(Parser: TExpressionParser): TValue;
var
  F: file of byte;
begin
  Result.ValueType := vtInteger;
  AssignFile(F, Parser.GetStrValue);
  Reset(F);
  Result.NumValue := Round(FileSize(F));
  CloseFile(F);
end;

function DoInstr(Parser: TExpressionParser): TValue;
var
  str, substr: string;
  start, R: integer;
  Value: TValue;
begin
  Value := Parser.ParseExpression;
  if Value.ValueType = vtInteger then
  begin
    start := Round(Value.NumValue);
    Parser.WaitComma;
    str := Parser.GetStrValue;
  end
  else
  begin
    start := 1;
    str := Value.StrValue;
  end;
  Parser.WaitComma;
  substr := Parser.GetStrValue;
  R := Pos(substr, Copy(str, start, Length(str) - start + 1));
  if R > 0 then
    R := R + start - 1
  else
    R := 0;
  Result.ValueType := vtInteger;
  Result.NumValue := R;
end;

function DoInt(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := Floor(Parser.GetDecValue);
end;

function DoLCase(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := LowerCase(Parser.GetStrValue);
end;

function DoLeft(Parser: TExpressionParser): TValue;
var
  s: string;
  n: integer;
begin
  s := Parser.GetStrValue;
  Parser.WaitComma;
  n := Parser.GetIntValue;
  Result.ValueType := vtString;
  Result.StrValue := Copy(s, 1, n);
end;

function DoLen(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := Length(Parser.GetStrValue);
end;

function DoLTrim(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := TrimLeft(Parser.GetStrValue);
end;

function DoLog(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtDecimal;
  Result.NumValue := Ln(Parser.GetDecValue);
end;

function DoMax(Parser: TExpressionParser): TValue;
var
  v1, v2: double;
begin
  v1 := Parser.GetDecValue;
  Parser.WaitComma;
  v2 := Parser.GetDecValue;
  Result.ValueType := vtDecimal;
  Result.NumValue := Max(v1, v2);
end;

function DoMid(Parser: TExpressionParser): TValue;
var
  s: string;
  i, j: integer;
begin
  s := Parser.GetStrValue;
  Parser.WaitComma;
  i := Parser.GetIntValue;
  Parser.WaitComma;
  j := Parser.GetIntValue;
  Result.ValueType := vtString;
  Result.StrValue := Copy(s, i, j);
end;

function DoMin(Parser: TExpressionParser): TValue;
var
  v1, v2: double;
begin
  v1 := Parser.GetDecValue;
  Parser.WaitComma;
  v2 := Parser.GetDecValue;
  Result.ValueType := vtDecimal;
  Result.NumValue := Min(v1, v2);
end;

function DoPower(Parser: TExpressionParser): TValue;
var
  Base, Exponent: double;
begin
  Base := Parser.GetDecValue;
  Parser.WaitComma;
  Exponent := Parser.GetDecValue;
  Result.ValueType := vtDecimal;
  Result.NumValue := Power(Base, Exponent);
end;

function DoReplace(Parser: TExpressionParser): TValue;
var
  Text, ToFind, ReplaceWith: string;
  n: integer;
begin
  Text := Parser.GetStrValue;
  Parser.WaitComma;
  ToFind := Parser.GetStrValue;
  Parser.WaitComma;
  ReplaceWith := Parser.GetStrValue;
  Result.ValueType := vtString;
  Result.StrValue := StringReplace(Text, ToFind, ReplaceWith, [rfReplaceAll]);
end;

function DoRight(Parser: TExpressionParser): TValue;
var
  s: string;
  n: integer;
begin
  s := Parser.GetStrValue;
  Parser.WaitComma;
  n := Parser.GetIntValue;
  if n < Length(s) then
    s := Copy(s, Length(s) - n + 1, n);
  Result.ValueType := vtString;
  Result.StrValue := s;
end;

function DoRnd(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtDecimal;
  Result.NumValue := Random;
end;

function DoRTrim(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := TrimRight(Parser.GetStrValue);
end;

function DoSgn(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtDecimal;
  Result.NumValue := Sign(Parser.GetDecValue);
end;

function DoSin(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtDecimal;
  Result.NumValue := Sin(Parser.GetDecValue);
end;

function DoSpace(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := StringOfChar(' ', Parser.GetIntValue);
end;

function DoString(Parser: TExpressionParser): TValue;
var
  n: integer;
  s: string;
begin
  n := Parser.GetIntValue;
  Parser.WaitComma;
  s := Parser.GetStrValue;
  if s = '' then
    raise Exception.Create('String is empty');
  if Length(s) > 1 then
    raise Exception.Create('Only one char');
  Result.ValueType := vtString;
  Result.StrValue := StringOfChar(s[1], n);
end;

function DoSqr(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtDecimal;
  Result.NumValue := Sqrt(Parser.GetDecValue);
end;

function DoStr(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := IntToStr(Parser.GetIntValue);
end;

function DoTan(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtDecimal;
  Result.NumValue := Tan(Parser.GetDecValue);
end;

function DoTime(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := TimeToStr(Now);
end;

function DoTimer(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := Round(GetTickCount / 1000);
end;

function DoTrim(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := Trim(Parser.GetStrValue);
end;

function DoUCase(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtString;
  Result.StrValue := UpperCase(Parser.GetStrValue);
end;

function DoVal(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := StrToInt(Parser.GetStrValue);
end;

procedure RegisterVar(ExpressionParser: TExpressionParser);
begin
  with ExpressionParser do
  begin
    SetVariable('pi', pi);
    SetVariable('e', Exp(1));
    SetVariable('true', True);
    SetVariable('false', False);
    SetVariable('err', 0);
    SetVariable('command', GetCommand);
  end;
end;

procedure RegisterFunc(ExpressionParser: TExpressionParser);
begin
  with ExpressionParser do
  begin
    RegisterFunction('abs', @DoAbs);
    RegisterFunction('asc', @DoAsc);
    RegisterFunction('avg', @DoAverage);
    RegisterFunction('chr', @DoChr);
    RegisterFunction('cint', @DoCInt);
    RegisterFunction('cos', @DoCos);
    RegisterFunction('curdir', @DoCurDir);
    RegisterFunction('date', @DoDate);
    RegisterFunction('dir', @DoDir);
    RegisterFunction('dir$', @DoDir);
    RegisterFunction('direxists', @DoDirExists);
    RegisterFunction('environ', @DoEnviron);
    RegisterFunction('exp', @DoExp);
    RegisterFunction('fileexists', @DoFileExists);
    RegisterFunction('filelen', @DoFileLen);
    RegisterFunction('instr', @DoInstr);
    RegisterFunction('int', @DoInt);
    RegisterFunction('lcase', @DoLCase);
    RegisterFunction('lcase$', @DoLCase);
    RegisterFunction('left', @DoLeft);
    RegisterFunction('left$', @DoLeft);
    RegisterFunction('len', @DoLen);
    RegisterFunction('ltrim', @DoLTrim);
    RegisterFunction('log', @DoLog);
    RegisterFunction('max', @DoMax);
    RegisterFunction('mid', @DoMid);
    RegisterFunction('mid$', @DoMid);
    RegisterFunction('min', @DoMin);
    RegisterFunction('power', @DoPower);
    RegisterFunction('replace', @DoReplace);
    RegisterFunction('right', @DoRight);
    RegisterFunction('right$', @DoRight);
    RegisterFunction('rnd', @DoRnd);
    RegisterFunction('rtrim', @DoRTrim);
    RegisterFunction('sgn', @DoSgn);
    RegisterFunction('sin', @DoSin);
    RegisterFunction('space', @DoSpace);
    RegisterFunction('space$', @DoSpace);
    RegisterFunction('string', @DoString);
    RegisterFunction('string$', @DoString);
    RegisterFunction('sqr', @DoSqr);
    RegisterFunction('str', @DoStr);
    RegisterFunction('str$', @DoStr);
    RegisterFunction('tan', @DoTan);
    RegisterFunction('time', @DoTime);
    RegisterFunction('timer', @DoTimer);
    RegisterFunction('trim', @DoTrim);
    RegisterFunction('ucase', @DoUCase);
    RegisterFunction('val', @DoVal);
  end;
end;

begin
  Randomize;
end.
