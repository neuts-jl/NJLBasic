unit ubasicgraphic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  ubasicinterpreter,
  uwconsole,
  utokenizer,
  uexpression,
  MMSystem;

type
  TMediaThread = class(TThread)
  private
    FAlias: string;
    FFileName: string;
    FCommand: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const Command, Alias, FileName: string);
  end;

  TGraphAttributes = record
    FontSize, FontStyle: integer;
    FontName: string;
    Width, PenStyle, Fill: integer;
  end;

  TBasicGraphic = class(TBasicInterpreter)
  private
    GA: TGraphAttributes;
    FExtra: TWConsole;
    procedure DoImage;
    procedure DoDrawText;
    procedure DoDrawLine;
    procedure DoDrawRectangle;
    procedure DoDrawCircle;
    procedure DoDrawEllipse;
    procedure DoDrawWait;
    procedure DoDraw;
    procedure DoSprite;
    procedure DoMedia;
  protected
    function ExtraCommand(ACommand: string): boolean; override;
  public
    constructor Create(AExtra: TWConsole);
  end;

implementation

uses
  uwmain;

function DoMouseX(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := frmMain.Console.Mouse.X;
end;

function DoMouseY(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := frmMain.Console.Mouse.Y;
end;

function DoMouseButton(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := frmMain.Console.MouseButton;
end;

function DoScreenX(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := frmMain.Console.Screen.X;
end;

function DoScreenY(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := frmMain.Console.Screen.Y;
end;

function DoImageX(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := frmMain.Console.GetImageSize(Parser.GetStrValue).X;
end;

function DoImageY(Parser: TExpressionParser): TValue;
begin
  Result.ValueType := vtInteger;
  Result.NumValue := frmMain.Console.GetImageSize(Parser.GetStrValue).Y;
end;

function DoGetSprite(Parser: TExpressionParser): TValue;
var
  Info, SpriteName, AnimationName: string;
begin
  Info := UpperCase(Parser.GetStrValue);
  Parser.WaitComma;
  SpriteName := Parser.GetStrValue;
  case Info of
    'X':
    begin
      Result.ValueType := vtInteger;
      Result.NumValue := Round(frmMain.Console.GetSpritePos(SpriteName).X);
    end;
    'Y':
    begin
      Result.ValueType := vtInteger;
      Result.NumValue := Round(frmMain.Console.GetSpritePos(SpriteName).Y);
    end;
    'WIDTH':
    begin
      Result.ValueType := vtInteger;
      Result.NumValue := Round(frmMain.Console.GetSpriteSize(SpriteName).X);
    end;
    'HEIGHT':
    begin
      Result.ValueType := vtInteger;
      Result.NumValue := Round(frmMain.Console.GetSpriteSize(SpriteName).Y);
    end;
    'VISIBILITY':
    begin
      Result.ValueType := vtBoolean;
      Result.BoolValue := frmMain.Console.GetSpriteVisibility(SpriteName);
    end;
    'EDGE_HIT':
    begin
      Result.ValueType := vtString;
      Result.StrValue := frmMain.Console.GetSpriteEdgeHit(SpriteName);
    end;
    'COLLIDE':
    begin
      Result.ValueType := vtString;
      Result.StrValue := frmMain.Console.GetSpriteCollide(SpriteName);
    end;
    'ANIM_END':
    begin
      Parser.WaitComma;
      AnimationName := Parser.GetStrValue;
      Result.ValueType := vtBoolean;
      Result.BoolValue := frmMain.Console.GetSpriteAnimEnd(SpriteName, AnimationName);
    end;
    'MOUSE_BUTTON':
    begin
      Result.ValueType := vtInteger;
      Result.NumValue := Round(frmMain.Console.GetSpriteMouse(SpriteName));
    end
    else
      raise Exception.Create(
        '"X", "Y", "WIDTH", "HEIGHT", "VISIBILITY", "EDGE_HIT", "COLLIDE", "ANIM_END" excepted');
  end;
end;

constructor TBasicGraphic.Create(AExtra: TWConsole);
begin
  inherited Create;
  FExtra := AExtra;
  Console := AExtra;
  GA.FontSize := 11;
  GA.FontStyle := 0;
  GA.fontname := 'Courier new';
  GA.Fill := 1;
  GA.Width := 1;
  GA.PenStyle := 0;
  FExpr.RegisterFunction('mousex', @DoMouseX);
  FExpr.RegisterFunction('mousey', @DoMouseY);
  FExpr.RegisterFunction('mousebutton', @DoMouseButton);
  FExpr.RegisterFunction('screenx', @DoScreenX);
  FExpr.RegisterFunction('screeny', @DoScreenY);
  FExpr.RegisterFunction('imagex', @DoImageX);
  FExpr.RegisterFunction('imagey', @DoImageY);
  FExpr.RegisterFunction('sprite', @DoGetSprite);
end;

procedure TBasicGraphic.DoImage;
//IMAGE LOAD "im                agename" "image1.png"
//IMAGE DRAW "imagename", x, y, [w, h]
//IMAGE CLEAR ["imagename"]
var
  Ins, FileName, ImageName: string;
  x, y, w, h, img: integer;
  Token: TToken;
begin
  Ins := UpperCase(FProgTokenizer.GetNextToken.Value);
  case Ins of
    'LOAD':
    begin
      ImageName := GetStrValue('image name', ',');
      FileName := GetStrValue('filename');
      FExtra.LoadImage(ImageName, FileName);
    end;
    'DRAW':
    begin
      ImageName := GetStrValue('image name', ',');
      x := GetIntValue('x', ',');
      y := GetIntValue('y', ',', True);
      if FProgTokenizer.Token.Value = ',' then
      begin
        w := GetIntValue('w', ',');
        h := GetIntValue('h');
      end;
      FExtra.DrawImage(ImageName, x, y, w, h);
    end;
    'CLEAR':
    begin
      Token := FProgTokenizer.GetNextToken;
      if (Token.TokenType = ttEol) then
        FExtra.ClearImages
      else
      begin
        FProgTokenizer.UnreadToken;
        ImageName := GetStrValue('image name');
        FExtra.ClearImage(ImageName);
      end;
    end
    else
      raise Exception.Create('LOAD, DRAW, CLEAR excepted');
  end;
end;

procedure TBasicGraphic.DoDrawText;
//DRAW TEXT x, y, "text" [, "fontname", fonsize, fontstyle]
var
  x, y: integer;
  Text: string;
begin
  x := GetIntValue('x', ',');
  y := GetIntValue('y', ',');
  Text := GetStrValue('text', ',', True);
  if FProgTokenizer.Token.Value = ',' then
  begin
    GA.FontName := GetStrValue('font name', ',');
    GA.FontSize := GetIntValue('font size', ',');
    GA.FontStyle := GetIntValue('font style');
  end;
  FExtra.DrawText(x, y, Text, GA.FontName, GA.FontSize, GA.FontStyle);
end;

procedure TBasicGraphic.DoDrawLine;
//DRAW LINE x1, y1, x2, y2[, width, penstyle]
var
  x1, y1, x2, y2: integer;
begin
  x1 := GetIntValue('x1', ',');
  y1 := GetIntValue('y1', ',');
  x2 := GetIntValue('x2', ',');
  y2 := GetIntValue('y2', ',', True);
  if FProgTokenizer.Token.Value = ',' then
  begin
    GA.Width := GetIntValue('width', ',');
    GA.PenStyle := GetIntValue('pen style');
  end;
  FExtra.DrawLine(x1, y1, x2, y2, GA.Width, GA.PenStyle);
end;


procedure TBasicGraphic.DoDrawRectangle;
//DRAW RECTANGLE x1, y1, x2, y2[, width, penstyle, fill]
var
  x1, y1, x2, y2: integer;
begin
  x1 := GetIntValue('x1', ',');
  y1 := GetIntValue('y1', ',');
  x2 := GetIntValue('x2', ',');
  y2 := GetIntValue('y2', ',', True);
  if FProgTokenizer.Token.Value = ',' then
  begin
    GA.Width := GetIntValue('width', ',');
    GA.PenStyle := GetIntValue('pen style', ',');
    GA.Fill := GetIntValue('fill');
  end;
  FExtra.DrawRectangle(x1, y1, x2, y2, GA.Width, GA.PenStyle, GA.Fill);
end;

procedure TBasicGraphic.DoDrawCircle;
//DRAW CIRCLE x, y, r[, width, penstyle, fill]
var
  x, y, r: integer;
begin
  x := GetIntValue('x', ',');
  y := GetIntValue('y', ',');
  r := GetIntValue('r', ',', True);
  if FProgTokenizer.Token.Value = ',' then
  begin
    GA.Width := GetIntValue('width', ',');
    GA.PenStyle := GetIntValue('pen style', ',');
    GA.Fill := GetIntValue('fill');
  end;
  FExtra.DrawCircle(x, y, r, GA.Width, GA.PenStyle, GA.Fill);
end;

procedure TBasicGraphic.DoDrawEllipse;
//DRAW ELLIPSE x1, y1, x2, y2[, width, penstyle, fill]
var
  x1, y1, x2, y2: integer;
begin
  x1 := GetIntValue('x1', ',');
  y1 := GetIntValue('y1', ',');
  x2 := GetIntValue('x2', ',');
  y2 := GetIntValue('y2', ',', True);
  if FProgTokenizer.Token.Value = ',' then
  begin
    GA.Width := GetIntValue('width', ',');
    GA.PenStyle := GetIntValue('pen style', ',');
    GA.Fill := GetIntValue('fill');
  end;
  FExtra.DrawEllipse(x1, y1, x2, y2, GA.Width, GA.PenStyle, GA.Fill);
end;

procedure TBasicGraphic.DoDrawWait;
//DRAW WAIT FRAME 10
var
  Token: TToken;
begin
  Token := FProgTokenizer.GetNextToken;
  if UpperCase(Token.Value) <> 'FRAME' then
    raise Exception.Create('FRAME excepted');
  FExtra.WaitAndUpdate(GetIntValue('ms'));
end;

procedure TBasicGraphic.DoDraw;
// DRAW TEXT...
// DRAW LINE...
// DRAW RECTANGLE...
// DRAW CIRCLE...
// DRAW ELLIPSE...
// DRAW WAIT...
var
  Token: TToken;
begin
  Token := FProgTokenizer.GetNextToken;
  case UpperCase(Token.Value) of
    'TEXT':
      DoDrawText;
    'LINE':
      DoDrawLine;
    'RECTANGLE':
      DoDrawRectangle;
    'CIRCLE':
      DoDrawCircle;
    'ELLIPSE':
      DoDrawEllipse;
    'WAIT':
      DoDrawWait;
    else
      raise Exception.Create('TEXT, LINE, RECTANGLE, CIRCLE, ELLIPSE, WAIT excepted');
  end;
end;

procedure TBasicGraphic.DoSprite;
//SPRITE CREATE "sprite", "imagefilename", framecount, animationcount
//SPRITE CLEAR ["sprite"]
//SPRITE POSITION "sprite", x,y
//SPRITE SHOW "sprite"
//SPRITE HIDE "sprite"
//SPRITE ANIMATION ADD "sprite", "animation",  animationindex, startframe, endframe, speed, loop
//SPRITE ANIMATION START "sprite", "animation"
var
  Ins, SpriteName, ImageName, AnimationName: string;
  FrameCount, AnimationCount, x, y, sx, sy: integer;
  P: TPoint;
  AnimationIndex, StartFrame, EndFrame: integer;
  Speed: double;
  Loop: boolean;
  Token: TToken;
begin
  Ins := UpperCase(FProgTokenizer.GetNextToken.Value);
  if pos(Ins, 'CREATE POSITION MOVE SHOW ANIMATION HIDE SHOW CLEAR') = 0 then
    raise Exception.Create(
      'CREATE, POSITION, MOVE, SHOW, CLEAR, ANIMATION, HIDE, SHOW excepted');
  if Ins = 'ANIMATION' then
  begin
    Ins := UpperCase(FProgTokenizer.GetNextToken.Value);
    if pos(Ins, 'ADD START') = 0 then
      raise Exception.Create('ADD, START excepted');
  end;
  case Ins of
    'CREATE':
    begin
      SpriteName := GetStrValue('sprite name', ',');
      ImageName := GetStrValue('image name', ',');
      AnimationCount := GetIntValue('animation count', ',');
      FrameCount := GetIntValue('frame count');
      FExtra.CreateSprite(SpriteName, ImageName, FrameCount, AnimationCount);
    end;
    'POSITION':
    begin
      SpriteName := GetStrValue('sprite name', ',');
      x := GetIntValue('x', ',');
      y := GetIntValue('y');
      FExtra.SetSpritePos(SpriteName, x, y);
    end;
    'MOVE':
    begin
      SpriteName := GetStrValue('sprite name', ',');
      P := FExtra.GetSpritePos(SpriteName);
      sx := GetIntValue('SW', ',');
      sy := GetIntValue('SY');
      FExtra.SetSpritePos(SpriteName, P.X + sx, P.Y + sy);
    end;
    'SHOW':
    begin
      SpriteName := GetStrValue('sprite name');
      FExtra.SetSpriteVisibility(SpriteName, True);
    end;
    'HIDE':
    begin
      SpriteName := GetStrValue('sprite name');
      FExtra.SetSpriteVisibility(SpriteName, False);
    end;
    'ADD':
    begin
      SpriteName := GetStrValue('sprite name', ',');
      AnimationName := GetStrValue('animation name', ',');
      AnimationIndex := GetIntValue('animation index', ',');
      StartFrame := GetIntValue('start frame', ',');
      EndFrame := GetIntValue('end frame', ',');
      Speed := GetDecValue('speed', ',');
      Loop := GetBoolValue('loop');
      FExtra.AddSpriteAnimation(SpriteName, AnimationName, AnimationIndex,
        StartFrame, EndFrame, Speed, Loop);
    end;
    'START':
    begin
      SpriteName := GetStrValue('sprite name', ',');
      AnimationName := GetStrValue('animation name');
      FExtra.StartSpriteAnimation(SpriteName, AnimationName);
    end;
    'CLEAR':
    begin
      Token := FProgTokenizer.GetNextToken;
      if (Token.TokenType = ttEol) then
        FExtra.ClearSprites
      else
      begin
        FProgTokenizer.UnreadToken;
        SpriteName := GetStrValue('sprite name');
        FExtra.ClearSprite(SpriteName);
      end;
    end;
  end;
end;

constructor TMediaThread.Create(const Command, Alias, FileName: string);
begin
  inherited Create(True);
  FCommand := Command;
  FAlias := Alias;
  FFileName := FileName;
  FreeOnTerminate := True;
  Start;
end;

procedure TMediaThread.Execute;
begin
  case FCommand of
    'PLAY':
    begin
      mciSendString(PChar('open "' + FFileName + '" type mpegvideo alias ' + FAlias),
        nil, 0, 0);
      mciSendString(PChar('play ' + FAlias), nil, 0, 0);
    end;
    'PAUSE':
      mciSendString(PChar('pause ' + FAlias), nil, 0, 0);
    'RESUME':
      mciSendString(PChar('resume ' + FAlias), nil, 0, 0);
    'STOP':
    begin
      mciSendString(PChar('stop ' + FAlias), nil, 0, 0);
      mciSendString(PChar('close ' + FAlias), nil, 0, 0);
    end;
  end;
end;

procedure TBasicGraphic.DoMedia;
// MEDIA PLAY "alias","filename"
// MEDIA PAUSE "alias"
// MEDIA RESUME "alias"
// MEDIA REWIND "alias"
// MEDIA STOP "alias"
var
  Ins, FileName, Alias: string;
begin
  Ins := UpperCase(FProgTokenizer.GetNextToken.Value);
  case Ins of
    'PLAY':
    begin
      Alias := GetStrValue('alias', ',');
      FileName := GetStrValue('filename');
      TMediaThread.Create(Ins, Alias, FileName);
    end;
    'PAUSE',
    'RESUME',
    'STOP':
    begin
      Alias := GetStrValue('alias');
      TMediaThread.Create(INS, Alias, '');
    end;
    else
      raise Exception.Create('PLAY, PAUSE, RESUME, STOP excepted');
  end;
end;

function TBasicGraphic.ExtraCommand(ACommand: string): boolean;
begin
  Result := True;
  case ACommand of
    'IMAGE':
      DoImage;
    'DRAW':
      DoDraw;
    'SPRITE':
      DoSprite;
    'MEDIA':
      DoMedia;
    else
      Result := False;
  end;
end;

end.
