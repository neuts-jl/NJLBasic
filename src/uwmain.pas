unit uwmain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ubasicinterpreter, ubasicgraphic, uwconsole;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Edit1: TEdit;
    PaintBox: TPaintBox;
    TimerInit: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerInitTimer(Sender: TObject);
  public
    Console:TWConsole;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption:=ChangeFileExt(ExtractFileName(Application.ExeName),'');
  Edit1.Hide;
  Console:=TWConsole.Create(Edit1,PaintBox);
  Basic := TBasicGraphic.Create(Console);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Basic.Mode:=bmSystem;
  Console.Stop:=True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Basic.Free;
  Console.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  TimerInit.Enabled:=True;
end;

procedure TfrmMain.TimerInitTimer(Sender: TObject);
begin
  TimerInit.Enabled:=False;
  Edit1.Width:=1;
  Edit1.Top:=0;
  Edit1.left:=0;
  Edit1.Show;
  Basic.Start;
  Close;
end;

end.


