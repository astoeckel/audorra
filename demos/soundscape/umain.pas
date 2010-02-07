unit umain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, ToolWin, StdCtrls, ImgList, XPMan,

  uplaygroundclasses,

  AcTypes, AcSysUtils, 

  AuWASAPI, AuDirectSound, AuAcinerella,
  AuUtils, AuAudio, Au3DAudio, Au3DAudioRenderer;

type
  Tstreamedsoundlist = class(TList)
    private
      FParent: TAu3DAudio;
      function GetItem(AIndex: integer): TAuStreamedSound;
    protected
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      constructor Create(AParent: TAu3DAudio);
      function Add(AName: String): TAuStreamedSound;

      property Items[AIndex: integer]: TAuStreamedSound read GetItem; default;
  end;
  
  Tfrmmain = class(TForm)
    PaintBox1: TPaintBox;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
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
    Label5: TLabel;
    lblPitch: TLabel;
    trbPitch: TTrackBar;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    ToolBar2: TToolBar;
    btnAddStaticSound: TToolButton;
    btnDeleteStaticSound: TToolButton;
    btnStaticSoundCreateInstance: TToolButton;
    lstStaticSounds: TListView;
    TabSheet2: TTabSheet;
    ToolBar1: TToolBar;
    btnAddStreamedSound: TToolButton;
    btnDeleteStreamedSound: TToolButton;
    btnStreamedSoundCreateInstance: TToolButton;
    lstStreamedSounds: TListView;
    ToolBar3: TToolBar;
    btnStreamedSoundPlay: TToolButton;
    btnStreamedSoundStop: TToolButton;
    Label6: TLabel;
    btnStreamedSoundPause: TToolButton;
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
    procedure trbPitchChange(Sender: TObject);
    procedure btnAddStreamedSoundClick(Sender: TObject);
    procedure btnStreamedSoundCreateInstanceClick(Sender: TObject);
    procedure btnDeleteStreamedSoundClick(Sender: TObject);
    procedure btnStreamedSoundPlayClick(Sender: TObject);
    procedure btnStreamedSoundStopClick(Sender: TObject);
    procedure btnStreamedSoundPauseClick(Sender: TObject);
    procedure lstStreamedSoundsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lstStaticSoundsDblClick(Sender: TObject);
    procedure lstStreamedSoundsDblClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    listener: TListener;
    playground: TPlayground;
    audio: TAuAudio;
    audio3d: TAu3DAudio;
    soundlist: TAuSoundList;
    streamedlist: Tstreamedsoundlist;
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
    if not audio3d.Initialize then
    begin
      ShowMessage('5.1 Sound is not available on the default output device. Falling back to stereo.');
      FreeAndNil(audio3d);
      audio3d := TAu3DAudio.Create(audio, au3dssStereo, 48000, 16);
      if not audio3d.Initialize then
      begin
        FreeAndNil(audio3d);
        ShowMessage('Output device couldn'' be opened.');
      end;
    end;

    if audio3d <> nil then
    begin
      audio3d.Renderer.Environment.Scale := 0.1;

      soundlist := TAuSoundList.Create(audio3d);
      streamedlist := Tstreamedsoundlist.Create(audio3d);

      bmp := TBitmap.Create;
      playground := TPlayground.Create;
      listener := TListener.Create(playground, audio3d.Listener);
      DoubleBuffered := true;

      exit;
    end;
  end else
    ShowMessage('The audio system couldn''t be initialized.');

  FreeAndNil(audio3d);
  FreeAndNil(audio);
end;

procedure Tfrmmain.FormDestroy(Sender: TObject);
begin
  streamedlist.Free;
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
  //List all static sounds
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

  //List all streamed sounds
  lstStreamedSounds.Items.BeginUpdate;
  try
    lstStreamedSounds.Clear;
    for i := 0 to streamedlist.Count - 1 do
    begin
      with lstStreamedSounds.Items.Add do
      begin
        Caption := streamedlist[i].Name;
        ImageIndex := -1;
        case streamedlist[i].Sound.Parameters.Channels of
          1: ImageIndex := 6;
          2: ImageIndex := 7;
          6: ImageIndex := 8;
          8: ImageIndex := 9;          
        end;
        SubItems.Add(FormatFloat('0.00', streamedlist[i].Len / 1000) + 's'); 
      end;
    end;
  finally
    lstStreamedSounds.Items.EndUpdate;
  end;
end;

procedure Tfrmmain.lstStaticSoundsDblClick(Sender: TObject);
begin
  if lstStaticSounds.SelCount <= 0 then
    btnAddStaticSoundClick(nil);
end;

procedure Tfrmmain.lstStreamedSoundsDblClick(Sender: TObject);
begin
  if lstStreamedSounds.SelCount <= 0 then
    btnAddStreamedSoundClick(nil);
end;

procedure Tfrmmain.lstStreamedSoundsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  btnStreamedSoundPlay.Enabled := lstStreamedSounds.SelCount > 0;
  btnStreamedSoundPause.Enabled := lstStreamedSounds.SelCount > 0;
  btnStreamedSoundStop.Enabled := lstStreamedSounds.SelCount > 0;
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

procedure Tfrmmain.trbPitchChange(Sender: TObject);
begin
  if (playground.Selected <> nil) then
  begin
    lblPitch.Caption := FormatFloat('0.00', trbPitch.Position / 100);
    audio3d.Lock;
    try
      if playground.Selected is TSource then
        TSource(playground.Selected).Emitter.Pitch := trbPitch.Position / 100
      else if playground.Selected is TListener then
        audio3d.Renderer.Environment.Pitch := trbPitch.Position / 100;
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

  for i := 0 to streamedlist.Count - 1 do
  begin
    streamedlist[i].Stop;

    for j := 0 to streamedlist[i].Sound.Emitters.Count - 1 do
      streamedlist[i].Sound.Emitters[j].Active := false;
  end;
end;

procedure Tfrmmain.btnStopClick(Sender: TObject);
begin
  if (playground.Selected <> nil) and (playground.Selected is TSource) then
  begin
    audio3d.Lock;
    try
      TSource(playground.Selected).Emitter.Active := false;
      if TSource(playground.Selected).Emitter is TAu3DStaticEmitter then      
        TAu3DStaticEmitter(TSource(playground.Selected).Emitter).SeekToSample(0);
    finally
      audio3d.Unlock;
    end;
  end;

  DisplaySoundInfo;
end;

procedure Tfrmmain.btnStreamedSoundCreateInstanceClick(Sender: TObject);
var
  em: TAu3DStreamedEmitter;
  obj: TSource;
  i: integer;
begin
  for i := 0 to lstStreamedSounds.Items.Count - 1 do
  begin
    if lstStreamedSounds.Items[i].Selected then
    begin
      em := TAu3DStreamedEmitter.Create(streamedlist.Items[i].Sound);
      streamedlist.Items[i].Play;
      obj := TSource.Create(playground, em);
      obj.Caption := streamedlist.Items[i].Name;
      obj.Color := clWebDarkGreen;
    end;
  end;

  PaintBox1.Repaint;
end;

procedure Tfrmmain.btnStreamedSoundPauseClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to lstStreamedSounds.Items.Count - 1 do
    if lstStreamedSounds.Items[i].Selected then
      streamedlist[i].Pause;
end;

procedure Tfrmmain.btnStreamedSoundPlayClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to lstStreamedSounds.Items.Count - 1 do
    if lstStreamedSounds.Items[i].Selected then
      streamedlist[i].Play;
end;

procedure Tfrmmain.btnStreamedSoundStopClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to lstStreamedSounds.Items.Count - 1 do
    if lstStreamedSounds.Items[i].Selected then
      streamedlist[i].Stop;
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
      trbPitch.Position := Round(TSource(playground.Selected).Emitter.Pitch * 100);

      if TSource(playground.Selected).Emitter.Active then
        lblState.Caption := 'Playing'
      else
        lblState.Caption := 'Paused';

      btnPlay.Enabled := true;
      btnPause.Enabled := true;
      btnStop.Enabled := TSource(playground.Selected).Emitter is TAu3DStaticEmitter;
    end else
    if playground.Selected is TListener then
    begin
      lblName.Caption := 'Listener';
      lblState.Caption := 'n.A.';
      trbGain.Position := Round(AuToDezibel(TListener(playground.Selected).Listener.Gain) * 10);
      trbPitch.Position := Round(audio3d.Renderer.Environment.Pitch * 100);

      btnPlay.Enabled := false;
      btnPause.Enabled := false;
      btnStop.Enabled := false;
    end;

    trbGainChange(nil);
    trbPitchChange(nil);

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

procedure Tfrmmain.btnAddStreamedSoundClick(Sender: TObject);
var
  i: integer;
  snd: TAuStreamedSound;
begin
  if OpenDialog1.Execute then
  begin
    for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      snd := streamedlist.Add(ExtractFileName(OpenDialog1.Files[i]));
      snd.LoadFromFile(OpenDialog1.Files[i]);

      //If the sound can not be opened, remove it from the list. It will automatically
      //be freed
      if not snd.Open then
        soundlist.Remove(snd);
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

procedure Tfrmmain.btnDeleteStreamedSoundClick(Sender: TObject);
var
  i, j: integer;
  snd: TAu3DCustomSound;
begin
  for i := lstStreamedSounds.Items.Count - 1 downto 0 do
  begin
    if lstStreamedSounds.Items[i].Selected then
    begin
      snd := streamedlist[i].Sound;
      streamedlist.Delete(i);

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

  for i := 0 to streamedlist.Count - 1 do
  begin
    streamedlist[i].Pause;

    for j := 0 to streamedlist[i].Sound.Emitters.Count - 1 do
      streamedlist[i].Sound.Emitters[j].Active := false;
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
  //Static sounds
  for i := 0 to soundlist.Count - 1 do
  begin
    for j := 0 to soundlist[i].Sound.Emitters.Count - 1 do
      soundlist[i].Sound.Emitters[j].Active := true;
  end;

  //Streamed sounds
  for i := 0 to streamedlist.Count - 1 do
  begin
    streamedlist[i].Play;
    for j := 0 to streamedlist[i].Sound.Emitters.Count - 1 do
      streamedlist[i].Sound.Emitters[j].Active := true;
  end;
end;

procedure Tfrmmain.btnPlayClick(Sender: TObject);
begin
  if (playground.Selected <> nil) and (playground.Selected is TSource) then
    TSource(playground.Selected).Emitter.Active := true;

  DisplaySoundInfo;
end;

{ Tstreamedsoundlist }

constructor Tstreamedsoundlist.Create(AParent: TAu3DAudio);
begin
  inherited Create;
  FParent := AParent;
end;

function Tstreamedsoundlist.Add(AName: String): TAuStreamedSound;
var
  tmp: TAuStreamedSound;
begin
  tmp := TAuStreamedSound.Create(FParent);
  tmp.Name := AName;
  inherited Add(tmp);

  result := tmp;
end;

function Tstreamedsoundlist.GetItem(AIndex: integer): TAuStreamedSound;
begin
  result := inherited Items[AIndex];
end;

procedure Tstreamedsoundlist.Notify(ptr: Pointer; action: TListNotification);
begin
  if action = lnDeleted then
    TAuStreamedSound(ptr).Free;
end;

end.
