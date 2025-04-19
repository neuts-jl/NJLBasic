{
  *****************************************************************************
   Unit        : utokenizer
   Author      : NEUTS JL
   License     : GPL (GNU General Public License)
   Date        : 01/03/2025

   Description : tokenizer


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
unit uTokenizer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

const
  //Please note that the first character of the double separator must appear
  //in the separators
  DefSeparators = '",",.,+,-,/,*,(,),{,},;,=,<,>,!,&,|,<>,<=,>=,!=,&&,||';

type
  TTokenType = (ttIdentifier, ttString, ttUnterminatedString, ttInteger,
    ttDecimal, ttSeparator, ttEol, ttUnknown);

  TToken = record
    TokenType: TTokenType;
    Value: string;
    Index: integer;
  end;

  TTokenizer = class
  private
    FToken: TToken;
    FSingleSeparators: string;
    FDoubleSeparators: TStringList;
    FSource: string;
    FIndex: integer;
    FEolCar:Char;
    procedure SetSeparators(Value: string);
    function GetSeparators: string;
    procedure SetSource(Value: string);
    procedure UnreadChar;
    function GetNextChar: char;
    function SkipSpaces: char;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UnreadToken;
    function GetNextToken: TToken;
    function GetRemainToken(const AStart:integer=0;AEnd:integer=0):string;
    property Source: string read FSource write SetSource;
    property Index: integer read FIndex write FIndex;
    property Separators: string read GetSeparators write SetSeparators;
    property Token: TToken read FToken;
    property EolCar:Char read FEolCar write FEolCar;
  end;


implementation

function IsLetter(C: char): boolean;
begin
  Result := (C in ['A'..'Z', 'a'..'z']);
end;

function IsDigit(C: char): boolean;
begin
  Result := (C in ['0'..'9']);
end;

function IsLetterOrDigit(C: char): boolean;
begin
  Result := IsLetter(C) or IsDigit(C);
end;

function IsForIdentFirst(C: char): boolean;
begin
  Result := IsLetter(C) or (C = '_');
end;

function IsForIdent(C: char): boolean;
begin
  Result := IsLetterOrDigit(C) or (C = '_') or (C = '$');
end;

constructor TTokenizer.Create;
begin
  FSingleSeparators := '';
  FDoubleSeparators := TStringList.Create;
  Separators := DefSeparators;
  FSource := '';
  FIndex := 1;
  FEolCar:='''';
end;

destructor TTokenizer.Destroy;
begin
  FDoubleSeparators.Free;
end;

procedure TTokenizer.SetSeparators(Value: string);
var
  FSeparators: TStringList;
  i: integer;
begin
  FSeparators := TStringList.Create;
  try
    FSeparators.CommaText := Value;
    FSingleSeparators := '';
    FDoubleSeparators.Clear;
    for i := 0 to FSeparators.Count - 1 do
      if Length(FSeparators[i]) = 1 then
        FSingleSeparators := FSingleSeparators + FSeparators[i]
      else
        FDoubleSeparators.Add(FSeparators[i]);
  finally
    FSeparators.Free;
  end;
end;

function TTokenizer.GetSeparators: string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(FSingleSeparators) do
    Result := Result + FSingleSeparators[i] + ',';
  for i := 0 to FDoubleSeparators.Count - 1 do
    Result := Result + FDoubleSeparators[i] + ',';
end;

procedure TTokenizer.SetSource(Value: string);
begin
  FSource := Value + #0;
  FIndex := 1;
  FToken.Index := 1;
  FToken.Value := '';
  FToken.TokenType := ttEol;
end;

function TTokenizer.GetNextChar: char;
begin
  if FIndex <= Length(FSource) then
  begin
    Result := FSource[FIndex];
    Inc(FIndex);
  end
  else
    Result := #0;
end;

procedure TTokenizer.UnreadChar;
begin
  if FIndex > 1 then
    Dec(FIndex);
end;

procedure TTokenizer.UnreadToken;
begin
  FIndex := FToken.Index;
end;

function TTokenizer.SkipSpaces: char;
begin
  repeat
    Result := GetNextChar;
  until not (Result in [#9, #10, #13, #32]);
end;

function TTokenizer.GetRemainToken(const AStart:integer=0;AEnd:integer=0):string;
begin
  if AEnd=0 then
    AEnd:=Length(Source);
  if AStart<>0 then
    Result:=Trim(Copy(Source,AStart,AEnd-AStart))
  else
    Result:=Trim(Copy(Source,FToken.Index,AEnd));
end;

function TTokenizer.GetNextToken: TToken;
var
  Ch, NextCh: char;
  TokenValue: string;
begin
  Result.Index := FIndex;
  Ch := SkipSpaces;
  if (Ch=#0) or (Ch = EolCar) then
  begin
    Result.TokenType := ttEol;
    Result.Value := '';
    FToken := Result;
    Exit;
  end;

  if Pos(ch, FSingleSeparators) > 0 then
  begin
    NextCh := GetNextChar;

    if FDoubleSeparators.IndexOf(Ch + NextCh) > -1 then
    begin
      Result.TokenType := ttSeparator;
      Result.Value := Ch + NextCh;
      FToken := Result;
      Exit;
    end;

    UnreadChar;
    Result.TokenType := ttSeparator;
    Result.Value := Ch;
    FToken := Result;
    Exit;
  end;

  if Ch = '"' then
  begin
    TokenValue := '';
    while True do
    begin
      Ch := GetNextChar;
      if Ch in [#0, #10, #13] then
      begin
        Result.TokenType := ttUnterminatedString;
        Result.Value := TokenValue;
        FToken := Result;
        Exit;
      end;
      if Ch = '\' then
      begin
        Ch := GetNextChar;
        if Ch = '"' then
          TokenValue := TokenValue + '"'
        else
          TokenValue := TokenValue + '\' + Ch;
      end
      else if Ch = '"' then
        Break
      else
        TokenValue := TokenValue + Ch;
    end;

    Result.TokenType := ttString;
    Result.Value := TokenValue;
    FToken := Result;
    Exit;
  end;

  if IsDigit(Ch) then
  begin
    TokenValue := Ch;
    while True do
    begin
      Ch := GetNextChar;
      if IsDigit(Ch) then
        TokenValue := TokenValue + Ch
      else if Ch = '.' then
      begin
        TokenValue := TokenValue + Ch;
        Ch := GetNextChar;
        if IsDigit(Ch) then
          TokenValue := TokenValue + Ch
        else
        begin
          UnreadChar;
          Break;
        end;
        while True do
        begin
          Ch := GetNextChar;
          if not IsDigit(Ch) then
          begin
            UnreadChar;
            Break;
          end;
          TokenValue := TokenValue + Ch;
        end;
        Result.TokenType := ttDecimal;
        Result.Value := TokenValue;
        FToken := Result;
        Exit;
      end
      else
      begin
        UnreadChar;
        Break;
      end;
    end;
    Result.TokenType := ttInteger;
    Result.Value := TokenValue;
    FToken := Result;
    exit;
  end;

  if IsForIdentFirst(Ch) then
  begin
    TokenValue := Ch;
    while True do
    begin
      Ch := GetNextChar;
      if not IsForIdent(Ch) then
      begin
        UnreadChar;
        Break;
      end;
      TokenValue := TokenValue + Ch;
    end;
    Result.TokenType := ttIdentifier;
    Result.Value := TokenValue;
    FToken := Result;
    Exit;
  end;

  Result.TokenType := ttUnknown;
  Result.Value := Ch;
  FToken := Result;
end;

end.
