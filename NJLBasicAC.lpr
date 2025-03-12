{
  *****************************************************************************
   Unit        : NJLBasicAC
   Author      : NEUTS JL
   License     : GPL (GNU General Public License)
   Date        : 01/03/2025

   Description : NJLBasic for Ansi console


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
program NJLBasicAC;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  Windows,
  ubasicinterpreter,
  uansiconsole, FileUtil;

var
  Console:TAnsiConsole;

  function ConsoleCtrlHandler(CtrlType: DWORD): longbool; stdcall;
  begin
    if CtrlType = CTRL_C_EVENT then
    begin
      if Basic.Mode = bmRunning then
      begin
        Console.Println;
        Console.Println;
        Console.Println('User break by Ctrl-C');
        Basic.Mode := bmStopped;
      end;
      Result := True;
    end
    else
      Result := False;
  end;


begin
  {$IFDEF UNIX}
     fpSignal(SIGINT, @SignalCtrlHandler);
  {$ENDIF}
  {$IFDEF WINDOWS}
    SetConsoleCtrlHandler(@ConsoleCtrlHandler, True);
  {$ENDIF}
  Console:=TAnsiConsole.Create;
  Basic := TBasicInterpreter.Create;
  Basic.Console:=Console;
  Basic.Start;
  Basic.Free;
  Console.Free;
end.


