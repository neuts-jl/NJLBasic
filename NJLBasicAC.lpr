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


