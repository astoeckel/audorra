unit umain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, ToolWin, StdCtrls, ImgList, XPMan,

  uplaygroundclasses,

  AcTypes,

  AuWASAPI, AuDirectSound, AuAcinerella,
  AuUtils, AuAudio, Au3DAudio, Au3DAudioRenderer;

type
  Tfrmmain = class(TForm)
    PaintBox1: TPaintBox;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    TabControl1: TTabControl;
    lstStaticSounds: TListView;
    ToolBar2: TToolBar;
    btnAddStaticSound: TToolButton;
    btnDeleteStaticSound: TToolButton;
    btnStaticSoundCreateInstance: TToolButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    trbGain: TTrackBar;
    lblName: TLabel;
    lblClass: TLabel;
    lblGain: TLabel;
    btnPlay: TButton;
    btnPause: TButton;
    btnStop: TButton;
    Label4: TLabel;
    lblState: TLabel;
    GroupBox2: TGroupBox;
    btnPlayAll: TButton;
    btnStopAll: TButton;
    btnPauseAll: TButton;
    ImageList1: TImageList;
    Splitter1: TSplitter;
    XPManifest1: TXPManifest;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnAddStaticSoundClick(Sender: TObject);
    procedure btnDeleteStaticSoundClick(Sender: TObject);
    procedure btnStaticSoundCreateInstanceClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure trbGainChange(Sender: TObject);
    procedure btnPlayAllClick(Sender: TObject);
    procedure btnPauseAllClick(Sender: TObject);
    procedure btnStopAllClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    listener: TListener;
    playground: TPlayground;
    audio: TAuAudio;
    audio3d: TAu3DAudio;
    soundlist: TAuSoundList;
    bmp: TBitmap;
    procedure ListSounds;
    procedure DisplaySoundInfo;
  end;

var
  frmmain: Tfrmmain;

implementation

{$R *.dfm}

{ Tfrmmain }

procedure Tfrmmain.FormCreate(Sender: TObject);
begin
  audio := TAuAudio.Create;
  if audio.Initialize then
  begin
    audio3d := TAu3DAudio.Create(audio, au3dss51, 48000, 16);
    audio3d.Initialize;
    audio3d.Renderer.Environment.Scale := 0.1;
  end;

  soundlist := TAuSoundList.Create(audio3d);

  bmp := TBitmap.Create;
  playground := TPlayground.Create;
  listener := TListener.Create(playground, audio3d.Listener);
  DoubleBuffered := true;
end;

procedure Tfrmmain.FormDestroy(Sender: TObject);
begin
  soundlist.Free;
  audio3d.Free;
  audio.Free;
  playground.Free;
  bmp.Free;
end;

procedure Tfrmmain.FormResize(Sender: TObject);
begin
  bmp.Width := PaintBox1.Width;
  bmp.Height := PaintBox1.Height;
end;

procedure Tfrmmain.ListSounds;
var
  i: integer;
begin
  lstStaticSounds.Items.BeginUpdate;
  try
    lstStaticSounds.Clear;
    for i := 0 to soundlist.Count - 1 do
    begin
      with lstStaticSounds.Items.Add do
      begin
        Caption := soundlist[i].Name;
        ImageIndex := -1;
        case soundlist[i].Format.Channels of
          1: ImageIndex := 6;
          2: ImageIndex := 7;
          6: ImageIndex := 8;
          8: ImageIndex := 9;          
        end;
        SubItems.Add(FormatFloat('0.00', soundlist[i].Len / 1000) + 's'); 
      end;
    end;
  finally
    lstStaticSounds.Items.EndUpdate;
  end;
end;

procedure Tfrmmain.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  playground.MouseDown(x, y);
  DisplaySoundInfo;
end;

procedure Tfrmmain.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  playground.MouseMove(x, y, Shift);
  PaintBox1.Repaint;
end;

procedure Tfrmmain.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  playground.MouseUp;
end;

procedure Tfrmmain.PaintBox1Paint(Sender: TObject);
begin
  playground.Draw(bmp.Canvas);
  Paintbox1.Canvas.Draw(0, 0, bmp);
end;

procedure Tfrmmain.trbGainChange(Sender: TObject);
begin
  if (playground.Selected <> nil) then
  begin
    lblGain.Caption := FormatFloat('0.0', trbGain.Position / 10)+'dB';
    audio3d.Lock;
    try
      if playground.Selected is TSource then      
        TSource(playground.Selected).Emitter.Gain := AuFromDezibel(trbGain.Position / 10)
      else if playground.Selected is TListener then
        TListener(playground.Selected).Listener.Gain := AuFromDezibel(trbGain.Position / 10)
    finally
      audio3d.Unlock;
    end;
  end;
end;

procedure Tfrmmain.btnStaticSoundCreateInstanceClick(Sender: TObject);
var
  em: TAu3DStaticEmitter;
  obj: TSource;
  i: integer;
begin
  for i := 0 to lstStaticSounds.Items.Count - 1 do
  begin
    if lstStaticSounds.Items[i].Selected then
    begin
      em := TAu3DStaticEmitter.Create(soundlist.Items[i].Sound);
      obj := TSource.Create(playground, em);
      obj.Caption := soundlist.Items[i].Name;
    end;
  end;

  PaintBox1.Repaint;
end;

procedure Tfrmmain.btnStopAllClick(Sender: TObject);
var
  i, j: integer;
begin
  for i := 0 to soundlist.Count - 1 do
  begin
    for j := 0 to soundlist[i].Sound.Emitters.Count - 1 do
    begin
      audio3d.Lock;
      try
        soundlist[i].Sound.Emitters[j].Active := false;
        TAu3DStaticEmitter(soundlist[i].Sound.Emitters[j]).SeekToSample(0);
      finally
        audio3d.Unlock;
      end;
    end;
  end;
end;

procedure Tfrmmain.btnStopClick(Sender: TObject);
begin
  if (playground.Selected <> nil) and (playground.Selected is TSource) then
  begin
    audio3d.Lock;
    try
      TSource(playground.Selected).Emitter.Active := false;
      TSource(playground.Selected).Emitter.SeekToSample(0);
    finally
      audio3d.Unlock;
    end;
  end;

  DisplaySoundInfo;
end;

procedure Tfrmmain.DisplaySoundInfo;
begin
  if playground.Selected <> nil then
  begin
    GroupBox1.Visible := true;

    if playground.Selected is TSource then
    begin
      lblName.Caption := TSource(playground.Selected).Caption;
      trbGain.Position := Round(AuToDezibel(TSource(playground.Selected).Emitter.Gain) * 10);

      if TSource(playground.Selected).Emitter.Active then
        lblState.Caption := 'Playing'
      else
        lblState.Caption := 'Paused';

      btnPlay.Enabled := true;
      btnPause.Enabled := true;
      btnStop.Enabled := true;
    end else
    if playground.Selected is TListener then
    begin
      lblName.Caption := 'Listener';
      lblState.Caption := 'n.A.';
      trbGain.Position := Round(AuToDezibel(TListener(playground.Selected).Listener.Gain) * 10);

      btnPlay.Enabled := false;
      btnPause.Enabled := false;
      btnStop.Enabled := false;
    end;

    trbGainChange(nil);

    lblClass.Caption := playground.Selected.ClassName;
  end else
    GroupBox1.Visible := false;
end;

procedure Tfrmmain.btnAddStaticSoundClick(Sender: TObject);
var
  i: integer;
  snd: TAuStaticSound;
begin
  if OpenDialog1.Execute then
  begin
    for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      snd := soundlist.AddNew(ExtractFileName(OpenDialog1.Files[i]));
      snd.LoadFromFile(OpenDialog1.Files[i]);

      //If the sound can not be opened, remove it from the list. It will automatically
      //be freed
      if not snd.Open then
        soundlist.Remove(snd)
      else begin
        snd.Loop := true;
      end;
    end;

    ListSounds;
  end;
end;

procedure Tfrmmain.btnDeleteStaticSoundClick(Sender: TObject);
var
  i, j: integer;
  snd: TAu3DCustomSound;
begin
  for i := lstStaticSounds.Items.Count - 1 downto 0 do
  begin
    if lstStaticSounds.Items[i].Selected then
    begin
      snd := soundlist[i].Sound;
      soundlist.Delete(i);

      //Remove all objects connected to the sound
      for j := playground.Objects.Count - 1 downto 0 do
      begin
        if (playground.Objects[j] is TSource) and
           (TSource(playground.Objects[j]).Emitter.Sound = snd) then
        begin
          if playground.Objects[j].Selected then
            playground.SelectNothing;

          playground.Objects.Delete(j);
        end;
      end;
    end;
  end;
  ListSounds;
  DisplaySoundInfo;

  PaintBox1.Repaint;
end;

procedure Tfrmmain.btnPauseAllClick(Sender: TObject);
var
  i, j: integer;
begin
  for i := 0 to soundlist.Count - 1 do
  begin
    for j := 0 to soundlist[i].Sound.Emitters.Count - 1 do
      soundlist[i].Sound.Emitters[j].Active := false;
  end;
end;

procedure Tfrmmain.btnPauseClick(Sender: TObject);
begin
  if (playground.Selected <> nil) and (playground.Selected is TSource) then
    TSource(playground.Selected).Emitter.Active := false;

  DisplaySoundInfo;
end;

procedure Tfrmmain.btnPlayAllClick(Sender: TObject);
var
  i, j: integer;
begin
  for i := 0 to soundlist.Count - 1 do
  begin
    for j := 0 to soundlist[i].Sound.Emitters.Count - 1 do
      soundlist[i].Sound.Emitters[j].Active := true;
  end;
end;

procedure Tfrmmain.btnPlayClick(Sender: TObject);
begin
  if (playground.Selected <> nil) and (playground.Selected is TSource) then
    TSource(playground.Selected).Emitter.Active := true;

  DisplaySoundInfo;
end;

end.
