{
  *****************************************************************************
   Unit        : uansiconsole
   Author      : NEUTS JL
   License     : GPL (GNU General Public License)
   Date        : 01/03/2025

   Description : Ansi console for NJLBasic


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
unit uansiconsole;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  uttyansi,
  uttyconsole,
  uttyinputedit,
  ucustomconsole;

type
  TAnsiConsole = class(TCustomConsole)
  private
    function BasicColorToColor(BasicColor: Integer): Integer;
  public
    Constructor Create;
    procedure ClrScr;override;
    procedure GotoXY(X, Y: Integer);override;
    procedure CursorOff;override;
    procedure CursorOn;override;
    procedure CursorBloc;override;
    procedure CursorNorm;override;
    procedure TextBackground(Color: Integer);override;
    procedure TextColor(Color: Integer);override;
    procedure PrintLn(const AText: string = '');override;
    procedure Print(const AText: string);override;
    procedure Input(var Line: string);override;
    function  Console:TPoint;override;
  end;

implementation


function TAnsiConsole.BasicColorToColor(BasicColor: Integer): Integer;
begin
  case BasicColor of
    0: Result := 0;   { black -> Black }
    1: Result := 4;   { blue -> Blue }
    2: Result := 2;   { green -> Green }
    3: Result := 6;   { cyan -> Cyan }
    4: Result := 1;   { red -> Red }
    5: Result := 5;   { pink -> Magenta }
    6: Result := 3;   { yellow -> Yellow }
    7: Result := 8;   { grey -> Gray }
    8: Result := 7;   { dark grey -> LightGray }
    9: Result := 12;  { bright blue -> LightBlue }
    10: Result := 10; { bright green -> LightGreen }
    11: Result := 14; { bright cyan -> LightCyan }
    12: Result := 9;  { bright red -> LightRed }
    13: Result := 13; { bright pink -> LightMagenta }
    14: Result := 11; { bright yellow -> LightYellow }
    15: Result := 15; { white -> White }
    else
      Result := 15;
  end;
end;

Constructor TAnsiConsole.Create;
begin
  FIsWindowed:=False;
  FUseShellPipe:=False;
end;

procedure TAnsiConsole.ClrScr;
begin
  uttyansi.ClrScr;
end;

procedure TAnsiConsole.GotoXY(X, Y: Integer);
begin
  uttyansi.GotoXY(X, Y);
end;

procedure TAnsiConsole.CursorOff;
begin
  uttyansi.CursorOff;
end;

procedure TAnsiConsole.CursorOn;
begin
  uttyansi.CursorOn;
end;

procedure TAnsiConsole.CursorBloc;
begin
  uttyansi.CursorBloc;
end;

procedure TAnsiConsole.CursorNorm;
begin
  uttyansi.CursorNorm;
end;

procedure TAnsiConsole.TextBackground(Color: Integer);
begin
  uttyansi.TextBackground(BasicColorToColor(Color));
end;

procedure TAnsiConsole.TextColor(Color: Integer);
begin
  uttyansi.TextColor(BasicColorToColor(Color));
end;

procedure TAnsiConsole.PrintLn(const AText: string = '');
begin
  WriteLn(AText);
end;

procedure TAnsiConsole.Print(const AText: string);
begin
  Write(AText);
end;

procedure TAnsiConsole.Input(var Line: string);
begin
  InputEdit(Line, 2, GetConsoleSize.X - 2);
  WriteLn;
end;

function TAnsiConsole.Console:TPoint;
begin
  Result:=GetConsoleSize;
end;

end.
