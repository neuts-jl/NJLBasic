unit uconsole;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Process,
  ucustomconsole;

type
  TConsole = class(TCustomConsole)
  public
    Constructor Create;
    procedure TextColor(Color: integer);override;
    procedure TextBackground(Color: integer);override;
    procedure ClrScr;override;
    procedure PrintLn(const AText: string = '');override;
    procedure Print(const AText: string);override;
    procedure Input(var Line: string);override;
  end;

implementation

Constructor TConsole.Create;
begin
  FIsWindowed:=False;
  FUseShellPipe:=False;
end;

procedure TConsole.TextColor(Color: integer);
begin
end;

procedure TConsole.TextBackground(Color: integer);
begin
end;

procedure TConsole.ClrScr;
var
  AProcess: TProcess;
begin
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'cmd.exe';
    AProcess.Parameters.Add('/C');
    AProcess.Parameters.Add('cls');
    AProcess.Options := [poWaitOnExit];
    AProcess.Execute;
  finally
    AProcess.Free;
  end;
end;
procedure TConsole.PrintLn(const AText: string = '');
begin
  WriteLn(AText);
end;

procedure TConsole.Print(const AText: string);
begin
  Write(AText);
end;

procedure TConsole.Input(var Line: string);
begin
  Readln(Line);
end;

end.
