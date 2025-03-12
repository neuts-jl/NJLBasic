{
  *****************************************************************************
   Unit        : ucustomconsole
   Author      : NEUTS JL
   License     : GPL (GNU General Public License)
   Date        : 01/03/2025

   Description : Generic console for NJLBasic


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
unit ucustomconsole;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TKey = record
    Car: char;
    Code: word;
  end;

  TCustomConsole = class
  protected
    FIsWindowed: boolean;
    FUseShellPipe: boolean;
  public
    procedure ClearMemory; virtual;

    procedure CursorOff; virtual;
    procedure CursorOn; virtual;
    procedure CursorBloc; virtual;
    procedure CursorNorm; virtual;

    procedure ClrScr; virtual;
    procedure TextColor(Color: integer); virtual;
    procedure TextBackground(Color: integer); virtual;
    procedure Gotoxy(X, Y: integer); virtual;
    procedure Print(const AText: string); virtual;
    procedure Println(const AText: string = ''); virtual;
    procedure Input(var Line: string); virtual;
    function Inkey: TKey; virtual;
    function Console: TPoint; virtual;

    property IsWindowed: boolean read FIsWindowed;
    property UseShellPipe: boolean read FUseShellPipe;

  end;


implementation

procedure NotImplemented;
begin
  raise Exception.Create('Not implemented in this console');
end;

procedure TCustomConsole.ClearMemory;
begin
end;

procedure TCustomConsole.CursorOff;
begin
  NotImplemented;
end;

procedure TCustomConsole.CursorOn;
begin
  NotImplemented;
end;

procedure TCustomConsole.CursorBloc;
begin
  NotImplemented;
end;

procedure TCustomConsole.CursorNorm;
begin
  NotImplemented;
end;

procedure TCustomConsole.ClrScr;
begin
  NotImplemented;
end;

procedure TCustomConsole.TextColor(Color: integer);
begin
  NotImplemented;
end;

procedure TCustomConsole.TextBackground(Color: integer);
begin
  NotImplemented;
end;

procedure TCustomConsole.Print(const AText: string);
begin
  NotImplemented;
end;

procedure TCustomConsole.Println(const AText: string = '');
begin
  NotImplemented;
end;

procedure TCustomConsole.Gotoxy(X, Y: integer);
begin
  NotImplemented;
end;

procedure TCustomConsole.Input(var Line: string);
begin
  NotImplemented;
end;

function TCustomConsole.Inkey: TKey;
begin
  NotImplemented;
end;

function TCustomConsole.Console: TPoint;
begin
  NotImplemented;
end;

end.
