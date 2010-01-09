unit uplaygroundclasses;

interface

uses
  SysUtils, Classes, Graphics, Types,

  AcTypes,
  Au3DAudioRenderer;

type
  TPlayground = class;
  
  TObj = class
    private
      FX, FY, FWidth, FHeight: Single;
      FParent: TPlayground;
      FSelected: Boolean;
      procedure SetX(AValue: Single);
      procedure SetY(AValue: Single);
    protected
      procedure DoDraw(ACanvas: TCanvas);virtual;
      procedure DoMove;virtual;
    public
      constructor Create(AParent: TPlayground);
      destructor Destroy;override;
      
      function BoundsRect: TRect;

      procedure Draw(ACanvas: TCanvas);

      property X: Single read FX write SetX;
      property Y: Single read FY write SetY;
      property Selected: Boolean read FSelected write FSelected;
  end;

  TObjList = class(TList)
    private
      function GetItem(AIndex: integer): TObj;
    protected
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      property Items[Index: integer]: TObj read GetItem;default;
  end;

  TPlayground = class
    private
      FObjs: TObjList;
      FCanvas: TCanvas;
      FScale: Single;
      FMouseDownItem: TObj;
      FSelected: TObj;
      FDX, FDY: integer;
    public
      constructor Create;
      destructor Destroy;override;

      procedure Draw(ACanvas: TCanvas);

      procedure ToScreen(var AX, AY: Single);

      function ObjectAt(AX, AY: integer): TObj;
      
      procedure MouseDown(AX, AY: integer);
      procedure MouseMove(AX, AY: integer; Shift: TShiftState);
      procedure MouseUp;

      procedure SelectNothing;

      property Objects: TObjList read FObjs;
      property Scale: Single read FScale write FScale;
      property Selected: TObj read FSelected;
  end;

  TListener = class(TObj)
    private
      FListener: TAu3DListener;
      procedure ListenerMove(AListener: TAu3DListener; ATimeGap: Double);
    protected
      procedure DoDraw(ACanvas: TCanvas);override;
    public
      constructor Create(AParent: TPlayground; AListener: TAu3DListener);
      destructor Destroy;override;

      property Listener: TAu3DListener read FListener;
  end;

  TSource = class(TObj)
    private
      FEmitter: TAu3DStaticEmitter;
      FCaption: string;
      procedure EmitterMove(AEmitter: TAu3DCustomEmitter; ATimeGap: Double);
    protected
      procedure DoDraw(ACanvas: TCanvas);override;
    public
      constructor Create(AParent: TPlayground; AEmitter: TAu3DStaticEmitter);
      destructor Destroy;override;

      property Caption: string read FCaption write FCaption;
      property Emitter: TAu3DStaticEmitter read FEmitter;
  end;
  


implementation

{ TObj }

function TObj.BoundsRect: TRect;
var
  ax, ay: Single;
begin
  ax := FX; ay := FY;
  FParent.ToScreen(ax, ay);
  result := Bounds(round(ax), round(ay), round(FWidth), round(FHeight));
end;

constructor TObj.Create(AParent: TPlayground);
begin
  inherited Create;

  FWidth := 16;
  FHeight := 16;

  FParent := AParent;
  FParent.Objects.Add(self);
end;

destructor TObj.Destroy;
begin
  FParent.Objects.Remove(self);
  inherited;
end;

procedure TObj.DoDraw(ACanvas: TCanvas);
begin
  //
end;

procedure TObj.DoMove;
begin
  //
end;

procedure TObj.Draw(ACanvas: TCanvas);
begin
  if Selected then
  begin
    ACanvas.Pen.Style := psDot;
    ACanvas.Pen.Color := clGray;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(
      BoundsRect.Left - 5,
      BoundsRect.Top - 5,
      BoundsRect.Right + 5,
      BoundsRect.Bottom + 5);
  end;
  DoDraw(ACanvas);
end;

procedure TObj.SetX(AValue: Single);
begin
  if AValue <> FX then
  begin
    FX := AValue;
    DoMove;
  end;
end;

procedure TObj.SetY(AValue: Single);
begin
  if AValue <> FY then
  begin
    FY := AValue;
    DoMove;
  end;
end;

{ TObjList }

function TObjList.GetItem(AIndex: integer): TObj;
begin
  result := inherited Items[AIndex];
end;

procedure TObjList.Notify(ptr: Pointer; action: TListNotification);
begin
  if action = lnDeleted then
    TObj(ptr).Free;
end;


{ TPlayground }

constructor TPlayground.Create;
begin
  inherited Create;

  FObjs := TObjList.Create;
  FScale := 10;
end;

destructor TPlayground.Destroy;
begin
  FObjs.Free;
end;

function RGB(r, g, b: Byte): Integer;
begin
  result := r or (g shl 8) or (b shl 16);
end;

procedure TPlayground.Draw(ACanvas: TCanvas);
var
  i: integer;
  x, y: integer;
begin
  FCanvas := ACanvas;
  ACanvas.Brush.Color := RGB(250, 250, 250);
  ACanvas.FillRect(ACanvas.ClipRect);

  for x := 0 to ACanvas.ClipRect.Right div 10 do
    for y := 0 to ACanvas.ClipRect.Bottom div 10 do
      ACanvas.Pixels[x * 10, y * 10] := clWebLightGrey;

  for i := 0 to FObjs.Count - 1 do
    FObjs[i].Draw(ACanvas);
end;

procedure TPlayground.MouseDown(AX, AY: integer);
var
  i: integer;
begin
  for i := 0 to FObjs.Count - 1 do
    FObjs[i].Selected := false;

  FSelected := nil;

  FMouseDownItem := ObjectAt(AX, AY);
  if FMouseDownItem <> nil then
    FMouseDownItem.Selected := true;
  FSelected := FMouseDownItem;
  FDX := AX;
  FDY := AY;
end;

procedure TPlayground.MouseMove(AX, AY: integer; Shift: TShiftState);
begin
  if FMouseDownItem <> nil then
  begin
    if ssLeft in Shift then
    begin
      FMouseDownItem.X := FMouseDownItem.X + (AX - FDX) / FScale;
      FMouseDownItem.Y := FMouseDownItem.Y + (AY - FDY) / FScale;
    end;
    FDX := AX; FDY := AY;
  end;
end;

procedure TPlayground.MouseUp;
begin
  FMouseDownItem := nil;
end;

function PointInRect(x, y: integer; r: TRect): boolean;
begin
  result :=
    (x >= r.Left) and (y >= r.Top) and (x <= r.Right) and (y <= r.Bottom);
end;

function TPlayground.ObjectAt(AX, AY: integer): TObj;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FObjs.Count - 1 do
  begin
    if PointInRect(ax, ay, FObjs[i].BoundsRect)  then
    begin
      result := FObjs[i];
      exit;
    end;    
  end;
end;

procedure TPlayground.SelectNothing;
begin
  FSelected := nil;
  FMouseDownItem := nil;
end;

procedure TPlayground.ToScreen(var AX, AY: Single);
begin
  AX := AX * FScale + FCanvas.ClipRect.Right / 2;
  AY := AY * FScale + FCanvas.ClipRect.Bottom / 2;
end;

{ TListener }

constructor TListener.Create(AParent: TPlayground; AListener: TAu3DListener);
begin
  inherited Create(AParent);

  FWidth := 16;
  FHeight := 16;

  FListener := AListener;
  FListener.OnMove := ListenerMove;
end;

destructor TListener.Destroy;
begin

  inherited;
end;

procedure TListener.DoDraw(ACanvas: TCanvas);
var
  caption: string;
begin
  with ACanvas do
  begin
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWebDarkOrange;
    Ellipse(BoundsRect);

    caption := 'Listener';

    Brush.Style := bsClear;
    Font.Color := clGray;
    TextOut(
      round(
        BoundsRect.Left +
        ((BoundsRect.Right - BoundsRect.Left) - TextWidth(caption)) * 0.5),
      BoundsRect.Bottom, caption);
  end;
end;

procedure TListener.ListenerMove(AListener: TAu3DListener; ATimeGap: Double);
begin
  AListener.Setup3DScene(AcVector3(FX, FY, 0), AcVector3(FX, FY, -1), AcVector3(0, 1, 0));
end;

{ TSource }

constructor TSource.Create(AParent: TPlayground; AEmitter: TAu3DStaticEmitter);
begin
  inherited Create(AParent);

  FWidth := 16;
  FHeight := 16;

  FEmitter := AEmitter;
  FEmitter.OnMove := EmitterMove;
end;

destructor TSource.Destroy;
begin
  inherited;
end;

procedure TSource.DoDraw(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWebDarkBlue;
    Ellipse(BoundsRect);

    Brush.Style := bsClear;
    Font.Color := clGray;
    TextOut(
      round(
        BoundsRect.Left +
        ((BoundsRect.Right - BoundsRect.Left) - TextWidth(FCaption)) * 0.5),
      BoundsRect.Bottom, FCaption);
  end;
end;

procedure TSource.EmitterMove(AEmitter: TAu3DCustomEmitter; ATimeGap: Double);
begin
  AEmitter.Position := AcVector3(FX, FY, 0);
end;


end.
