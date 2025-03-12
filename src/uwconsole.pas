{
  *****************************************************************************
   Unit        : uwconsole
   Author      : NEUTS JL
   License     : GPL (GNU General Public License)
   Date        : 01/03/2025

   Description : Windowed console for NJLBasic


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
unit uwconsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, LCLType, IntfGraphics,
  FPImage, types, Math, Forms, fgl, FPCanvas, ucustomconsole;

type
  TAnimationSprite = class
    FAnimationIndex: integer;
    FStartFrame: integer;
    FEndFrame: integer;
    FSpeed: double;
    FLoop: boolean;
  end;

  TAnimationsSprite = specialize TFPGMap<string, TAnimationSprite>;

  TSprite = class
    FImageFileName:string;
    FFrameWidth: integer;
    FFrameHeight: integer;
    FFrameCount: integer;
    FAnimationCount: integer;
    FAnimations: TAnimationsSprite;
    FSpeedCount: integer;
    FAnimationName: string;
    FFrameIndex: integer;
    FVisible: boolean;
    FPos: TPoint;
  end;

  TSpritesBMP = specialize TFPGMap<string, TBitmap>;
  TSprites = specialize TFPGMap<string, TSprite>;

  TBImage = class(TPicture);

  TImages = specialize TFPGMap<string, TBImage>;

  TWConsole = class(TCustomConsole)
  private
    FCurrentMousePos: TPoint;
    FCurrentMouseButton:integer;
    FKey: TKey;
    FHistory: TStringList;
    FHistoryIndex: integer;
    FStartTime: integer;
    FEndEdit: boolean;
    FPaintBox: TPaintBox;
    FEdit: TEdit;
    FBuffer: TBitmap;
    FTextColor: TColor;
    FBackgroundColor: TColor;
    FCursorX, FCursorY: integer;
    FSpritesBMP:TSpritesBMP;
    FSprites: TSprites;
    FImages: TImages;
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure OnKeyPress(Sender: TObject; var Key: char);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
                Shift: TShiftState; X, Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
                Shift: TShiftState; X, Y: Integer);
    procedure Scroll;
    procedure LF;

    procedure OnPaint(Sender: TObject);
    procedure OnResize(Sender: TObject);
    function GetCharWidth: integer;
    function GetCharHeight: integer;
    procedure ResizeBuffer(NewWidth, NewHeight: integer);
    function BasicColorToTColor(BasicColor: integer): TColor;
  public
    Stop: boolean;
    constructor Create(AEdit: TEdit; APaintBox: TPaintBox);
    destructor Destroy; override;
    procedure ClearMemory; override;
    procedure TextColor(Color: integer); override;
    procedure TextBackground(Color: integer); override;
    procedure Clrscr; override;
    procedure Gotoxy(X, Y: integer); override;
    procedure Print(const AText: string); override;
    procedure Println(const AText: string = ''); override;
    procedure Input(var Line: string); override;
    function Inkey: TKey; override;
    function Console: TPoint; override;

    //Graphics
    function Screen: TPoint;
    function Mouse: TPoint;
    function MouseButton: integer;
    procedure DrawText(x, y: integer; AText, FontName: string;
      FontSize, FontStyle: integer);
    procedure DrawLine(x1, y1, x2, y2, Width, PenStyle: integer);
    procedure DrawRectangle(x1, y1, x2, y2, Width, PenStyle, Fill: integer);
    procedure DrawCircle(x, y, r, Width, PenStyle, Fill: integer);
    procedure DrawEllipse(x1, y1, x2, y2, Width, PenStyle, Fill: integer);

    procedure LoadImage(const ImageName, FileName: string);
    function GetImageSize(const ImageName: string): TPoint;
    procedure DrawImage(const ImageName: string; x, y, w, h: integer);
    procedure ClearImage(const ImageName: string);
    procedure ClearImages;

    //Sprites
    procedure CreateSprite(const SpriteName, ImageFileName: string;
      FrameCount, AnimationCount: integer);
    procedure AddSpriteAnimation(const SpriteName, AnimationName: string;
      AnimationIndex, StartFrame, EndFrame: integer; Speed: double; Loop: boolean);
    procedure StartSpriteAnimation(const SpriteName, AnimationName: string);
    procedure UpdateSprites;
    function GetSpriteEdgeHit(const SpriteName: string): string;
    function GetSpriteCollide(const SpriteName: string): string;
    function GetSpriteAnimEnd(const SpriteName, AnimationName: string): boolean;
    function GetSpriteMouse(const SpriteName: string):integer;
    function GetSpriteSize(const SpriteName: string): TPoint;
    function GetSpritePos(const SpriteName: string): TPoint;
    function GetSpriteVisibility(const SpriteName: string): boolean;
    procedure SetSpritePos(const SpriteName: string; x, y: integer);
    procedure SetSpriteVisibility(const SpriteName: string; Visible: boolean);
    procedure ClearSprite(const SpriteName: string);
    procedure ClearSprites;

    procedure WaitAndUpdate(MS: integer);
  end;

implementation

constructor TWConsole.Create(AEdit: TEdit; APaintBox: TPaintBox);
begin
  FSprites := TSprites.Create;
  FSpritesBMP := TSpritesBMP.Create;
  FImages := TImages.Create;
  FHistory := TStringList.Create;
  FHistoryIndex := 0;
  FIsWindowed := True;
  FUseShellPipe := True;
  FIsWindowed := True;
  FStartTime := 0;

  FEdit := AEdit;
  FEdit.Font.Name := 'Courier New';
  FEdit.Font.Size := 11;
  FEdit.Font.Style := [fsBold];
  FEdit.BorderStyle := bsNone;
  FEdit.OnkeyDown := @OnKeyDown;
  FEdit.OnkeyPress := @OnKeyPress;

  FPaintBox := APaintBox;
  FPaintBox.OnPaint := @OnPaint;
  FPaintBox.OnResize := @OnResize;
  FPaintBox.OnMouseMove := @OnMouseMove;
  FPaintBox.OnMouseDown := @OnMouseDown;
  FPaintBox.OnMouseUp := @OnMouseUp;
  FBuffer := TBitmap.Create;

  FCursorX := 0;
  FCursorY := 0;
  TextColor(15);
  TextBackground(0);
  ClrScr;
end;

destructor TWConsole.Destroy;
begin
  ClearMemory;
  FImages.Free;
  FSprites.Free;
  FSpritesBMP.Free;
  FHistory.Free;
  FBuffer.Free;
  inherited Destroy;
end;

procedure TWConsole.ClearMemory;
begin
  ClearSprites;
  ClearImages;
end;

function TWConsole.BasicColorToTColor(BasicColor: integer): TColor;
begin
  case BasicColor of
    0: Result := clBlack;
    1: Result := clBlue;
    2: Result := clGreen;
    3: Result := clAqua;  // Cyan
    4: Result := clRed;
    5: Result := clFuchsia; // Pink
    6: Result := clOlive; // Yellow (approximation)
    7: Result := clGray;
    8: Result := clSilver;
    9: Result := clSkyBlue; // Bright Blue
    10: Result := clLime; // Bright Green
    11: Result := clSkyBlue; // Bright Cyan (approximation)
    12: Result := clMaroon; // Bright Red (approximation)
    13: Result := clPurple; // Bright Pink
    14: Result := clYellow;
    15: Result := clWhite;
    else
      Result := clBlack;
  end;
end;

procedure TWConsole.TextColor(Color: integer);
begin
  FTextColor := BasicColorToTColor(Color);
  FEdit.Font.Color := FTextColor;
  FBuffer.Canvas.Pen.Color := FTextColor;
  FBuffer.Canvas.Font.Color := FTextColor;
  FBuffer.Canvas.Brush.Color := FTextColor;
end;

procedure TWConsole.TextBackground(Color: integer);
begin
  FBackgroundColor := BasicColorToTColor(Color);
  FEdit.Color := FBackgroundColor;
end;

procedure TWConsole.ClrScr;
begin
  FBuffer.SetSize(FPaintBox.Width, FPaintBox.Height);
  FBuffer.Canvas.Brush.Color := FBackgroundColor;
  FBuffer.Canvas.FillRect(Rect(0, 0, FBuffer.Width, FBuffer.Height));
  FCursorX := 0;
  FCursorY := 0;
end;

procedure TWConsole.Scroll;
begin
  FBuffer.Canvas.CopyRect(
    Rect(0, 0, FBuffer.Width, FBuffer.Height - GetCharHeight),
    FBuffer.Canvas,
    Rect(0, GetCharHeight, FBuffer.Width, FBuffer.Height)
    );
  FBuffer.Canvas.FillRect(Rect(0, FBuffer.Height - GetCharHeight,
    FBuffer.Width, FBuffer.Height));
  FCursorY := FBuffer.Height - GetCharHeight;
  FPaintBox.Invalidate;
  Application.ProcessMessages;
end;

procedure TWConsole.OnPaint(Sender: TObject);
begin
  FPaintBox.Canvas.Draw(0, 0, FBuffer);
end;

procedure TWConsole.OnResize(Sender: TObject);
begin
  ResizeBuffer(Max(FPaintBox.Width, FBuffer.Width),
    Max(FPaintBox.Height, FBuffer.Height));
end;

procedure TWConsole.ResizeBuffer(NewWidth, NewHeight: integer);
var
  TempBuffer: TBitmap;
begin
  TempBuffer := TBitmap.Create;
  try
    TempBuffer.SetSize(FBuffer.Width, FBuffer.Height);
    TempBuffer.Canvas.Draw(0, 0, FBuffer);

    FBuffer.SetSize(NewWidth, NewHeight);

    FBuffer.Canvas.Brush.Color := FBackgroundColor;
    FBuffer.Canvas.FillRect(Rect(0, 0, NewWidth, NewHeight));

    FBuffer.Canvas.Draw(0, 0, TempBuffer);
  finally
    TempBuffer.Free;
  end;
  FPaintBox.Invalidate;
  Application.ProcessMessages;
end;

procedure TWConsole.Print(const AText: string);
var
  RemainingText, LineText: string;
  MaxWidth: integer;
  LineBreakPos: integer;
begin
  FBuffer.Canvas.Font.Assign(FEdit.Font);
  FBuffer.Canvas.Brush.Color := FBackgroundColor;
  MaxWidth := FBuffer.Width div GetCharWidth;
  RemainingText := AText;

  while RemainingText <> '' do
  begin
    LineBreakPos := Pos(#10, RemainingText);
    if (LineBreakPos = 0) or (LineBreakPos > MaxWidth) then
      LineBreakPos := MaxWidth;

    LineText := Copy(RemainingText, 1, LineBreakPos);
    Delete(RemainingText, 1, LineBreakPos);

    if (Length(RemainingText) > 0) and (RemainingText[1] = #13) then
      Delete(RemainingText, 1, 1);

    FBuffer.Canvas.TextOut(FCursorX, FCursorY, LineText);
    FCursorX := FCursorX + FBuffer.Canvas.TextWidth(LineText);

    if (RemainingText <> '') or (LineBreakPos < MaxWidth) then
      LF;
  end;

  FPaintBox.Invalidate;
  Application.ProcessMessages;
end;

procedure TWConsole.Println(const AText: string = '');
begin
  Print(AText);
  LF;
end;

procedure TWConsole.LF;
begin
  FCursorX := 0;
  FCursorY := FCursorY + GetCharHeight;
  if FCursorY >= FBuffer.Height - GetCharHeight then
  begin
    Scroll;
    Dec(FCursorY, GetCharHeight div 2);
  end;
end;

procedure TWConsole.OnKeyPress(Sender: TObject; var Key: char);
begin
  FKey.Car := Key;
end;

procedure TWConsole.OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if FEdit.Width = 1 then
  begin
    FKey.Code := Key;
    exit;
  end;
  case Key of
    VK_RETURN:
    begin
      FEndEdit := True;
      if Trim(FEdit.Text) <> '' then
        FHistory.Add(FEdit.Text);
      Key := 0;
    end;
    VK_UP:
    begin
      if FHistoryIndex > 0 then
      begin
        Dec(FHistoryIndex);
        FEdit.Text := FHistory[FHistoryIndex];
        FEdit.SelStart := Length(FEdit.Text);
      end;
      Key := 0;
    end;
    VK_DOWN:
    begin
      if FHistoryIndex < FHistory.Count - 1 then
      begin
        Inc(FHistoryIndex);
        FEdit.Text := FHistory[FHistoryIndex];
        FEdit.SelStart := Length(FEdit.Text);
      end;
      Key := 0;
    end;
  end;
end;

procedure TWConsole.Input(var Line: string);
begin
  FEndEdit := False;
  FEdit.Left := FCursorX;
  FEdit.Top := FCursorY;
  FEdit.Width := FEdit.Parent.ClientWidth - FEdit.Left;
  FEdit.Text := Line;
  FEdit.Visible := True;
  FEdit.SetFocus;
  FEdit.SelStart := Length(Line);
  FHistoryIndex := FHistory.Count;
  while not FEndEdit and not Stop do
  begin
    Application.ProcessMessages;
    Sleep(5);
  end;
  Line := FEdit.Text;
  Println(Line);
  FEdit.Width := 1;
  FEdit.Top := FEdit.Parent.Height;
  FEdit.Left := 0;
end;

function TWConsole.Inkey: TKey;
begin
  FEdit.SetFocus;
  Result := FKey;
  FKey.Car := #0;
  FKey.Code := 0;
  Application.ProcessMessages;
end;

function TWConsole.Screen: TPoint;
begin
  Result.x := FPaintBox.Width;
  Result.y := FPaintBox.Height;
end;

function TWConsole.Console: TPoint;
begin
  Result.x := FBuffer.Width div GetCharWidth;
  Result.y := FBuffer.Height div GetCharHeight;
end;

procedure TWConsole.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  FCurrentMousePos.X := X;
  FCurrentMousePos.Y := Y;
end;

procedure TWConsole.OnMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
begin
  FCurrentMouseButton:=Integer(Button)+1;
end;

procedure TWConsole.OnMouseUp(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
begin
  FCurrentMouseButton:=0;
end;

function TWConsole.Mouse: TPoint;
begin
  Result := FCurrentMousePos;
end;

function TWConsole.MouseButton: integer;
begin
  Result := FCurrentMouseButton;
end;

procedure TWConsole.Gotoxy(X, Y: integer);
begin
  FCursorX := X * GetCharWidth;
  FCursorY := Y * GetCharHeight;
end;

function TWConsole.GetCharWidth: integer;
begin
  Result := FBuffer.Canvas.TextWidth('W');
end;

function TWConsole.GetCharHeight: integer;
begin
  Result := FBuffer.Canvas.TextHeight('Hg');
end;

procedure TWConsole.DrawText(x, y: integer; AText, FontName: string;
  FontSize, FontStyle: integer);
begin
  FBuffer.Canvas.Font.Name := FontName;
  FBuffer.Canvas.Font.Size := FontSize;
  FBuffer.Canvas.Font.Style := TFontStyles(FontStyle);
  FBuffer.Canvas.Font.Color := FTextColor;
    FBuffer.Canvas.Brush.Style := bsClear;
  FBuffer.Canvas.TextOut(x, y, AText);
end;

procedure TWConsole.DrawLine(x1, y1, x2, y2, Width, PenStyle: integer);
begin
  FBuffer.Canvas.Pen.Width := Width;
  FBuffer.Canvas.Pen.Style := TFPPenStyle(PenStyle);
  FBuffer.Canvas.Line(x1, y1, x2, y2);
end;

procedure TWConsole.DrawRectangle(x1, y1, x2, y2, Width, PenStyle, Fill: integer);
begin
  FBuffer.Canvas.Brush.Color := FTextColor;
  FBuffer.Canvas.Brush.Style := TFPBrushStyle(Fill);
  FBuffer.Canvas.Pen.Style := TFPPenStyle(PenStyle);
  FBuffer.Canvas.Pen.Width := Width;
  FBuffer.Canvas.Rectangle(x1, y1, x2, y2);
end;

procedure TWConsole.DrawCircle(x, y, r, Width, PenStyle, Fill: integer);
begin
  FBuffer.Canvas.Brush.Color := FTextColor;
  FBuffer.Canvas.Brush.Style := TFPBrushStyle(Fill);
  FBuffer.Canvas.Pen.Style := TFPPenStyle(PenStyle);
  FBuffer.Canvas.Pen.Width := Width;
  FBuffer.Canvas.Ellipse(x - r, y - r, x + r, y + r);
end;

procedure TWConsole.DrawEllipse(x1, y1, x2, y2, Width, PenStyle, Fill: integer);
begin
  FBuffer.Canvas.Brush.Color := FTextColor;
  FBuffer.Canvas.Brush.Style := TFPBrushStyle(Fill);
  FBuffer.Canvas.Pen.Style := TFPPenStyle(PenStyle);
  FBuffer.Canvas.Pen.Width := Width;
  FBuffer.Canvas.Ellipse(x1, y1, x2, y2);
end;

procedure TWConsole.LoadImage(const ImageName, FileName: string);
var
  Image: TBImage;
begin
  if FImages.IndexOf(ImageName) <> -1 then
    raise Exception.Create(ImageName + ' already loaded');
  Image := TBImage.Create;
  Image.loadFromFile(FileName);
  FImages[ImageName] := Image;
end;

function TWConsole.GetImageSize(const ImageName: string): TPoint;
var
  AImage: TBImage;
begin
  if FImages.IndexOf(ImageName) = -1 then
    raise Exception.Create(ImageName + ': Image not found');
  AImage := FImages[ImageName];
  Result.X := AImage.Width;
  Result.Y := AImage.Height;
end;

procedure TWConsole.DrawImage(const ImageName: string; x, y, w, h: integer);
var
  Image: TBImage;
begin
  if FImages.IndexOf(ImageName) = -1 then
    raise Exception.Create(ImageName + ' not found');
  Image := FImages[ImageName];
  if (w > 0) and (h > 0) then
    FBuffer.Canvas.StretchDraw(Rect(x, y, x + w, y + h), Image.Graphic)
  else
    FBuffer.Canvas.Draw(x, y, Image.Graphic);
end;


procedure TWConsole.ClearImage(const ImageName: string);
var
  AImage: TImage;
  ix: integer;
begin
  ix := FImages.IndexOf(ImageName);
  if ix = -1 then
    raise Exception.Create(ImageName + ': Image not found');
  FImages.Delete(ix);
end;

procedure TWConsole.ClearImages;
var
  i: integer;
begin
  i := 0;
  while i < FImages.Count do
    ClearImage(FImages.Keys[i]);
  FImages.Clear;
end;

procedure TWConsole.CreateSprite(const SpriteName, ImageFileName: string;
  FrameCount, AnimationCount: integer);
var
  ASprite: TSprite;
  ABmp: TBitmap;
  AImage: TBImage;
begin
  if FSprites.IndexOf(SpriteName) > -1 then
    raise Exception.Create(SpriteName + ' : already created');
  if FrameCount = 0 then
    raise Exception.Create('Invalid frame count');
  if AnimationCount = 0 then
    raise Exception.Create('Invalid animation count');
  AImage := TBImage.Create;
  try
    AImage.loadFromFile(ImageFileName);
    ABmp := TBitmap.Create;
    ABmp.PixelFormat := pf32bit;
    ABmp.SetSize(AImage.Width, AImage.Height);
    ABmp.Canvas.Draw(0, 0, AImage.Graphic);
    ABmp.Transparent := True;
    //    ABmp.TransparentColor:=clBlack;
    FSpritesBMP[ImageFileName]:=ABMP;

    ASprite := TSprite.Create;
    ASprite.FImageFileName:=ImageFileName;
    ASprite.FFrameWidth := ABmp.Width div FrameCount;
    ASprite.FFrameHeight := ABmp.Height div AnimationCount;
    ASprite.FFrameCount := FrameCount;
    ASprite.FAnimationCount := AnimationCount;
    ASprite.FVisible := True;
    ASprite.FPos.X := 0;
    ASprite.FPos.Y := 0;

    ASprite.FAnimations := TAnimationsSprite.Create;
    FSprites.Add(SpriteName, ASprite);
  finally
    AImage.Free;
  end;
end;

procedure TWConsole.AddSpriteAnimation(const SpriteName, AnimationName: string;
  AnimationIndex, StartFrame, EndFrame: integer; Speed: double; Loop: boolean);
var
  ASprite: TSprite;
  Animation: TAnimationSprite;
begin
  if FSprites.IndexOf(SpriteName) = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites[SpriteName];
  if (AnimationIndex < 0) or (AnimationIndex >= ASprite.FAnimationCount) then
    raise Exception.Create('incorrect animation index');
  if (StartFrame < 0) or (StartFrame >= ASprite.FFrameCount) or
    (EndFrame < StartFrame) then
    raise Exception.Create('incorrect start frame/end frame');
  if Speed < 0 then
    raise Exception.Create('invalid speed');
  Animation := TAnimationSprite.Create;
  Animation.FAnimationIndex := AnimationIndex;
  Animation.FStartFrame := StartFrame;
  Animation.FEndFrame := EndFrame;
  Animation.FSpeed := Speed;

  Animation.FLoop := Loop;
  ASprite.FAnimations.Add(AnimationName, Animation);
end;

procedure TWConsole.StartSpriteAnimation(const SpriteName, AnimationName: string);
var
  ASprite: TSprite;
  Animation: TAnimationSprite;
begin
  if FSprites.IndexOf(SpriteName) = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites[SpriteName];
  if ASprite.FAnimations.IndexOf(AnimationName) = -1 then
    raise Exception.Create(AnimationName + ': animation not found for ' +
      SpriteName + ' sprite');
  Animation := ASprite.FAnimations[AnimationName];
  ASprite.FAnimationName := AnimationName;
  ASprite.FFrameIndex := Animation.FStartFrame;
  ASprite.FSpeedCount := Round(Animation.FSpeed * 10);
end;

procedure TWConsole.UpdateSprites;

  procedure UpdateSprite(SpriteName: string);
  var
    ASprite: TSprite;
    Animation: TAnimationSprite;
    ABmp:TBitmap;
  begin
    if FSprites.IndexOf(SpriteName) = -1 then
      raise Exception.Create(SpriteName + ': sprite not found');
    ASprite := FSprites[SpriteName];
    if not ASprite.FVisible then
      exit;
    with ASprite do
    begin
      ABmp:=FSpritesBMP[FImageFileName];
      Animation := FAnimations[ASprite.FAnimationName];

      FBuffer.Canvas.CopyRect(
        Rect(FPos.X, FPos.Y, FPos.X + FFrameWidth, FPos.Y + FFrameHeight),
        ABmp.Canvas,
        Rect(FFrameIndex * FFrameWidth, Animation.FAnimationIndex *
        FFrameHeight, (FFrameIndex + 1) * FFrameWidth,
        (Animation.FAnimationIndex + 1) * FFrameHeight)
        );
      if FSpeedCount > 0 then
        Dec(FSpeedCount)
      else
      begin
        if FFrameIndex < Animation.FEndFrame then
          Inc(FFrameIndex)
        else if Animation.FLoop then
          FFrameIndex := Animation.FStartFrame;
        FSpeedCount := Round(Animation.FSpeed * 10);
      end;
    end;
  end;

var
  i: integer;
begin
  for i := 0 to FSprites.Count - 1 do
    UpdateSprite(FSprites.Keys[i]);
end;

function TWConsole.GetSpriteAnimEnd(const SpriteName, AnimationName: string): boolean;
var
  ASprite: TSprite;
  Animation: TAnimationSprite;
begin
  if FSprites.IndexOf(SpriteName) = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites[SpriteName];
  if ASprite.FAnimations.indexOf(AnimationName)=-1 then
    raise Exception.Create(AnimationName + ': animation not found for ' +
      SpriteName + ' sprite');
  Animation := ASprite.FAnimations[AnimationName];
  Result := not Animation.FLoop and (ASprite.FFrameIndex >= Animation.FEndFrame - 1);
end;

function TWConsole.GetSpriteMouse(const SpriteName: string):integer;
function PointInRect( const P: TPoint;const R: TRect): Boolean;
begin
  // Vérifie si le point est à l'intérieur du rectangle
  Result := (P.X >= R.Left) and (P.X <= R.Right) and
            (P.Y >= R.Top) and (P.Y <= R.Bottom);
end;
var
  ASprite: TSprite;
  R:TRect;
begin
  if FSprites.IndexOf(SpriteName) = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites[SpriteName];
  R.Left:=ASprite.FPos.X;
  R.Right:=R.Left+ASprite.FFrameWidth;
  R.Top:=ASprite.FPos.Y;
  R.Bottom:=R.Top+ASprite.FFrameHeight;
  if ASprite.FVisible and PointInRect(FCurrentMousePos,R) then
    Result:=FCurrentMouseButton
  else
    Result:=0;
end;

function TWConsole.GetSpriteEdgeHit(const SpriteName: string): string;
var
  ASprite: TSprite;
begin
  if FSprites.IndexOf(SpriteName) = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites[SpriteName];
  if ASprite.FPos.X + ASprite.FFrameWidth >= FBuffer.Width then
    exit('RIGHT')
  else if ASprite.FPos.X <= 0 then
    exit('LEFT')
  else if ASprite.FPos.Y + ASprite.FFrameHeight >= FBuffer.Height then
    exit('BOT')
  else if ASprite.FPos.Y <= 0 then
    exit('TOP');
end;

function TWConsole.GetSpriteCollide(const SpriteName: string): string;

  function SpriteCollide(Sprite1, Sprite2: TSprite): boolean;
  begin
    Result := not ((Sprite1.FPos.X + Sprite1.FFrameWidth <= Sprite2.FPos.X) or
      (Sprite1.FPos.X >= Sprite2.FPos.X + Sprite2.FFrameWidth) or
      (Sprite1.FPos.Y + Sprite1.FFrameHeight <= Sprite2.FPos.Y) or
      (Sprite1.FPos.Y >= Sprite2.FPos.Y + Sprite2.FFrameHeight));
  end;

var
  ASprite, OtherSprite: TSprite;
  i: integer;
begin
  if FSprites.IndexOf(SpriteName) = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites[SpriteName];
  for i := 0 to FSprites.Count - 1 do
  begin
    OtherSprite := FSprites.Data[i];
    if ASprite.FVisible and OtherSprite.FVisible and (OtherSprite <> ASprite) and
      SpriteCollide(ASprite, OtherSprite) then
      Exit(FSprites.Keys[i]);
  end;
  Result:='';
end;

function TWConsole.GetSpritePos(const SpriteName: string): TPoint;
var
  ASprite: TSprite;
begin
  if FSprites.IndexOf(SpriteName) = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites[SpriteName];
  Result := ASprite.FPos;
end;

function TWConsole.GetSpriteSize(const SpriteName: string): TPoint;
var
  ASprite: TSprite;
begin
  if FSprites.IndexOf(SpriteName) = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites[SpriteName];
  Result.X := ASprite.FFrameWidth;
  Result.Y := ASprite.FFrameHeight;
end;

function TWConsole.GetSpriteVisibility(const SpriteName: string): boolean;
var
  ASprite: TSprite;
begin
  if FSprites.IndexOf(SpriteName) = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites[SpriteName];
  Result := ASprite.FVisible;
end;

procedure TWConsole.SetSpritePos(const SpriteName: string; x, y: integer);
var
  ASprite: TSprite;
begin
  if FSprites.IndexOf(SpriteName) = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites[SpriteName];
  ASprite.FPos.X := x;
  ASprite.FPos.Y := y;
end;

procedure TWConsole.SetSpriteVisibility(const SpriteName: string; Visible: boolean);
var
  ASprite: TSprite;
begin
  if FSprites.IndexOf(SpriteName) = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites[SpriteName];
  ASprite.FVisible := Visible;
end;

procedure TWConsole.ClearSprite(const SpriteName: string);
var
  ASprite: TSprite;
  ix,ib: integer;
begin
  ix := FSprites.IndexOf(SpriteName);
  if ix = -1 then
    raise Exception.Create(SpriteName + ': sprite not found');
  ASprite := FSprites.Data[ix];
  ib:=FSpritesBMP.IndexOf(ASprite.FImageFileName);
  if ib>-1 then
    FSpritesBMP.Delete(ib);
  ASprite.FAnimations.Clear;
  FSprites.Delete(ix);
end;

procedure TWConsole.ClearSprites;
var
  i: integer;
begin
  i := 0;
  while i < FSprites.Count do
    ClearSprite(FSprites.Keys[i]);
  FSprites.Clear;
end;

procedure TWConsole.WaitAndUpdate(MS: integer);
var
  ElapsedTime: int64;
begin
  UpdateSprites;
  ElapsedTime := GetTickCount64 - FStartTime;
  if ElapsedTime < MS then
    Sleep(MS - ElapsedTime);
  FPaintBox.Invalidate;
  Application.ProcessMessages;
  FStartTime := GetTickCount64;
end;

end.
