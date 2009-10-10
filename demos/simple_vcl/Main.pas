unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, ToolWin, StdCtrls, ExtCtrls, Menus, Math, XPMan, FileCtrl,
  wnd_OpenURL,

  AcPersistent, AcDLLExplorer, AcStrUtils,
  AuAcinerella, AuOpenAL, AuUtils, AuTypes, AuAudio, AuAnalyzers, AuComplex;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    btnPlay: TToolButton;
    btnPause: TToolButton;
    btnStop: TToolButton;
    ToolButton4: TToolButton;
    btnOpen: TToolButton;
    ToolButton6: TToolButton;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    XPManifest1: TXPManifest;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    ToolButton2: TToolButton;
    PopupMenu1: TPopupMenu;
    Panel3: TPanel;
    ToolBar2: TToolBar;
    ImageList2: TImageList;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ListView1: TListView;
    ImageList3: TImageList;
    ToolButton10: TToolButton;
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    CoolBar1: TCoolBar;
    PaintBox1: TPaintBox;
    Panel4: TPanel;
    PopupMenu2: TPopupMenu;
    mitmCtrlMV: TMenuItem;
    N1: TMenuItem;
    mitmResetVol: TMenuItem;
    PopupMenu3: TPopupMenu;
    OpenFile1: TMenuItem;
    OpenURL1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure ProgressBar1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ProgressBar1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1DblClick(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PaintBox1Click(Sender: TObject);
    procedure mitmResetVolClick(Sender: TObject);
    procedure volitemClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnOpenURL(Sender: TObject);
  private
  public
    AuAudio: TAuAudio;
    AuPlayer: TAuPlayer;
    AuPeakMeter: TAuPeakmeter;
    AuOscilloscope: TAuOscilloscope;
    AuFFT: TAuFFT;
//    AuHTTPProt: TAuHTTPProtocol;

    vol_chn: integer;

    vis_bmp: TBitmap;
    mask_bmp: TBitmap;
    changed: boolean;
    prefix: string;
    suffix: string;
    playlist_ind: integer;
    oszmem: PByte;
    vis_indx: integer;
    procedure DeviceClick(Sender: TObject);
    procedure FinishedSong(Sender: TObject);
    procedure ActivateButtons;
    procedure DeactivateButtons;
    procedure DrawPeakMeter;
    procedure DrawOscilloscope;
    procedure DrawOscibars;
    procedure DrawFFT;
    procedure PlayerStateChange(Sender: TObject);
    procedure PlayPlyLstSong;
    procedure LastPlyLstSong;
    procedure NextPlyLstSong;
    procedure RemoveSongs;
    function AddSongs(AClear: boolean = false): boolean;
    procedure OpenSongs;
    procedure OpenURL;
    procedure AddDirectory(ADir: string);
    procedure CreateVolPopups;
  end;

var
  Form1: TForm1;

const
  vis_count = 4;

implementation

{$R *.dfm}

procedure TForm1.FinishedSong(Sender: TObject);
begin
  NextPlyLstSong;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  itm: TMenuItem;
begin
  AcRegSrv.DbgDump;

  ReportMemoryLeaksOnShutdown := true;

  AuAudio := TAuAudio.Create;

  //Initialize the form
  DeactivateButtons;

  vis_bmp := TBitmap.Create;
  mask_bmp := TBitmap.Create;
  ToolBar1.DoubleBuffered := true;
    
  if AuAudio.Initialize then
  begin
    for i := 0 to AuAudio.Devices.Count - 1 do
    begin
      itm := TMenuItem.Create(PopupMenu1);
      itm.Caption := AuAudio.Devices[i].Name;
      itm.OnClick := DeviceClick;
      itm.GroupIndex := 1;
      itm.RadioItem := true;
      itm.Tag := AuAudio.Devices[i].ID;

      if AuAudio.StandardDeviceID = AuAudio.Devices[i].ID then
        itm.Checked := true;
                                 
      PopupMenu1.Items.Add(itm);
    end;
      
    AuPlayer := TAuPlayer.Create(AuAudio);
    AuPlayer.OnStateChange := PlayerStateChange;
    AuPlayer.OnSongFinishes := FinishedSong;

    AuPeakMeter := TAuPeakMeter.Create;
    AuPlayer.AddAnalzyer(AuPeakMeter);

    AuOscilloscope := TAuOscilloscope.Create(0.1);
    AuPlayer.AddAnalzyer(AuOscilloscope);

    AuFFT := TAuFFT.Create(1024);
    AuPlayer.AddAnalzyer(AuFFT);

    Timer1.Enabled := true;
  end else
  begin
    ShowMessage(AuAudio.GetLastError);
    FreeAndNil(AuAudio);
    halt;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AuPlayer.Free;
  AuPeakMeter.Free;
  AuOscilloscope.Free;
  AuFFT.Free;
  AuAudio.Free;
  vis_bmp.Free;
  mask_bmp.Free;

  if oszmem <> nil then
    FreeMem(oszmem);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Paintbox1.Width := Toolbar1.ClientWidth - PaintBox1.Left;
  vis_bmp.Width := PaintBox1.ClientWidth;
  vis_bmp.Height := PaintBox1.ClientHeight;
  mask_bmp.Width := PaintBox1.ClientWidth;
  mask_bmp.Height := PaintBox1.ClientHeight;
end;

procedure TForm1.Image1DblClick(Sender: TObject);
begin
  TrackBar1.Position := 0;
end;

procedure TForm1.LastPlyLstSong;
begin
  playlist_ind := playlist_ind - 1;
  if playlist_ind < 0 then
    playlist_ind := 0
  else begin
    ListView1.ItemIndex := playlist_ind;
    PlayPlyLstSong;
  end;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  if ListView1.ItemIndex <> -1 then
  begin
    playlist_ind := ListView1.ItemIndex;
    PlayPlyLstSong;
  end else
    AddSongs;
end;

procedure TForm1.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) or (Key = VK_BACK) then
    RemoveSongs;
end;

procedure TForm1.mitmResetVolClick(Sender: TObject);
begin
  AuPlayer.VolumeFilter.Reset;
  AuPlayer.MasterVolume := 1;
  vol_chn := -1;
  volitemClick(mitmCtrlMV);
end;

procedure TForm1.NextPlyLstSong;
begin
  playlist_ind := playlist_ind + 1;
  if playlist_ind >= ListView1.Items.Count then
    playlist_ind := ListView1.Items.Count - 1
  else begin
    ListView1.ItemIndex := playlist_ind;
    PlayPlyLstSong;
  end;
end;

procedure TForm1.PaintBox1Click(Sender: TObject);
begin
  vis_indx := (vis_indx + 1) mod (vis_count);
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, vis_bmp);
end;

procedure TForm1.PlayerStateChange(Sender: TObject);
begin
  prefix := '';
  case AuPlayer.State of
    aupsOpened: prefix := '[OPENED] ';
    aupsPlaying: prefix := '[PLAYING] ';
    aupsPaused: prefix := '[PAUSED] ';
  end;

  Caption := prefix + suffix;
end;

procedure TForm1.PlayPlyLstSong;
var
  i: integer;
begin
  DeactivateButtons;
  AuPlayer.LoadFromURL(ListView1.Items[playlist_ind].Caption);
  if AuPlayer.Open then
  begin
    AuPlayer.Play;
    CreateVolPopups;
    ActivateButtons;
  end;

  for i := 0 to ListView1.Items.Count - 1 do
    ListView1.Items[i].ImageIndex := 0;

  ListView1.Items[playlist_ind].ImageIndex := 1;

  suffix := ExtractFileName(ListView1.Items[playlist_ind].Caption);

  Caption := prefix + suffix;
  if Caption = '' then
    Caption := 'Simple Player Demo';
end;

procedure TForm1.ProgressBar1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
  begin
    ProgressBar1.Position := round(ProgressBar1.Max / ProgressBar1.ClientWidth * X);
    changed := true;
  end;
end;

procedure TForm1.ProgressBar1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//  if changed then
  begin
    changed := false;
    ProgressBar1.Position := round(ProgressBar1.Max / ProgressBar1.ClientWidth * X);
    AuPlayer.Position := ProgressBar1.Position;
  end;
end;

procedure TForm1.RemoveSongs;
var
  i: integer;
begin
  ListView1.Items.BeginUpdate;
  for i := ListView1.Items.Count - 1 downto 0 do
  begin
    if ListView1.Items[i].Selected then
    begin
      ListView1.Items.Delete(i);
    end;
  end;
  ListView1.Items.EndUpdate;
end;

function MSToTime(AV: Integer): string;
begin
  result :=
    FormatFloat('00', AV div (3600 * 1000)) + ':' +
    FormatFloat('00', (AV div 60000) mod 60) + ':' +
    FormatFloat('00', (AV div 1000) mod 60) + ',' +
    FormatFloat('000', AV mod 1000);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  pos: integer;
begin
  case vis_indx of
    3: DrawPeakMeter;
    1: DrawOscilloscope;
    2: DrawOscibars;
    0: DrawFFT;
  end;

  if AuPlayer <> nil then
  begin
    if AuPlayer.Seekable then
    begin
      ProgressBar1.Visible := true;
      Panel2.Caption :=
        MSToTime(AuPlayer.Position) + ' / ' +
        MSToTime(AuPlayer.Len - AuPlayer.Position)
    end
    else
    begin
      ProgressBar1.Visible := false;
      Panel2.Caption :=
        MSToTime(AuPlayer.Position);
      exit;
    end;

    if changed then
      exit;
      
    pos := AuPlayer.Len;
    if pos >= 0 then
      ProgressBar1.Max := pos;
    
    pos := AuPlayer.Position;
    if pos >= 0 then
      ProgressBar1.Position := pos;
  end;
end;

procedure TForm1.ToolButton10Click(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select a folder', '', dir) then
    AddDirectory(dir);
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  LastPlyLstSong;
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
begin
  OpenSongs;
end;

procedure TForm1.ToolButton5Click(Sender: TObject);
begin
  NextPlyLstSong;
end;

procedure TForm1.ToolButton8Click(Sender: TObject);
begin
  AddSongs;
end;

procedure TForm1.ToolButton9Click(Sender: TObject);
begin
  RemoveSongs;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  if mitmCtrlMV.Checked then  
    AuPlayer.MasterVolume := AuFromDezibel(TrackBar1.Position)
  else
    AuPlayer.VolumeFilter.Volume[vol_chn] := AuFromDezibel(TrackBar1.Position);

  if TrackBar1.Position = 0 then
  begin
    Label1.Caption := 'Volume: ' + IntToStr(TrackBar1.Position) + 'dB';
    Label1.Font.Color := clGreen;
  end;
  if TrackBar1.Position < 0 then
  begin
    Label1.Caption := 'Volume: ' + IntToStr(TrackBar1.Position) + 'dB';
    Label1.Font.Color := clBlue;
  end;
  if TrackBar1.Position > 0 then
  begin
    Label1.Caption := 'Volume: +' + IntToStr(TrackBar1.Position) + 'dB';
    Label1.Font.Color := clRed;
  end;
end;

procedure TForm1.volitemClick(Sender: TObject);
begin
  vol_chn := TMenuItem(Sender).Tag;
  TMenuItem(Sender).Checked := true;

  if vol_chn >= 0 then
    TrackBar1.Position := Round(AuToDezibel(AuPlayer.VolumeFilter.Volume[vol_chn]))
  else
    TrackBar1.Position := Round(AuToDezibel(AuPlayer.MasterVolume));
end;

procedure TForm1.AddDirectory(ADir: string);
var
  sr: TSearchRec;
  ext: string;
begin
  ListView1.Items.BeginUpdate;
  
  ADir := IncludeTrailingPathDelimiter(ADir);
  FindFirst(ADir + '*', faAnyFile or faDirectory, sr);
  while FindNext(sr) = 0 do
  begin
    if (sr.Name <> '.') and (sr.Name <> '..') then
    begin
      if (sr.Attr and faDirectory) = faDirectory then
        AddDirectory(ADir + sr.Name + '\')
      else begin
        ext := LowerCase(ExtractFileExt(sr.Name));
        if (ext = '.mp3') or (ext = '.ogg') or (ext = '.flac') or (ext = '.mpc') or
           (ext = '.ac3') or (ext = '.wma') or (ext = '.wmv') or (ext = '.vob') or
           (ext = '.avi') or (ext = '.flv') or (ext = '.mpg') or (ext = '.aif') or
           (ext = '.wav') then
        begin
          with ListView1.Items.Add do
          begin
            Caption := ADir + sr.Name;
            ImageIndex := 0;
          end;
        end;
      end;
    end;
  end;
  FindClose(sr);

  ListView1.Items.EndUpdate; 
end;

function TForm1.AddSongs(AClear: boolean): boolean;
var
  i: integer;
begin
  result := false;
  suffix := '';
  if OpenDialog1.Execute then
  begin
    if AClear then
      ListView1.Clear;
      
    for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      with ListView1.Items.Add do
      begin
        Caption := OpenDialog1.Files[i];
        ImageIndex := 0;
      end;
    end;

    result := true;
  end; 
end;

procedure TForm1.OpenSongs;
begin
  if AddSongs(true) then
  begin
    playlist_ind := 0;
    PlayPlyLstSong;
  end;
end;

procedure TForm1.OpenURL;
var
  add: boolean;
  url: string;
begin
  if OpenURLWnd.GetURL(url, add) then
  begin
    if not add then
      ListView1.Clear;

    with ListView1.Items.Add do
    begin
      Caption := url;
      ImageIndex := 0;
    end;

    if not add then
    begin
      playlist_ind := 0;
      PlayPlyLstSong;
    end;
  end;
end;

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  OpenSongs;
end;

procedure TForm1.btnOpenURL(Sender: TObject);
begin
  OpenURL;
end;

procedure TForm1.btnPauseClick(Sender: TObject);
begin
  AuPlayer.Pause;
end;

procedure TForm1.btnPlayClick(Sender: TObject);
begin
  AuPlayer.Play;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  AuPlayer.Stop;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  with ListView1.Items.Add do
  begin
    Caption := InputBox('URL','Enter an URL.', '');
    ImageIndex := 0;
  end;
end;

procedure TForm1.CreateVolPopups;
var
  i: integer;
  itm: TMenuItem;
begin
  for i := PopupMenu2.Items.Count - 1 downto 0 do
    if PopupMenu2.Items[i].Tag >= 0 then    
      PopupMenu2.Items.Delete(i);

  for i := AuPlayer.VolumeFilter.Parameters.Channels - 1 downto 0 do
  begin
    itm := TMenuItem.Create(nil);
    itm.Caption := 'Control channel ' + IntToStr(i);
    itm.Tag := i;
    itm.GroupIndex := 1;
    itm.RadioItem := true;
    itm.OnClick := volitemClick;  
    PopupMenu2.Items.Insert(3, itm);
  end;                                 
end;

procedure TForm1.ActivateButtons;
begin
  btnPlay.Enabled := true;
  btnPause.Enabled := true;
  btnStop.Enabled := true;
end;

procedure TForm1.DeactivateButtons;
begin
  btnPlay.Enabled := false;
  btnPause.Enabled := false;
  btnStop.Enabled := false;
end;

procedure TForm1.DeviceClick(Sender: TObject);
begin
  with TMenuItem(Sender) do
  begin
    AuAudio.StandardDeviceID := Tag;
    Checked := true;
  end;
end;

function ColorBetween(c1, c2: TColor; v: Single): TColor;
begin
  result :=
    RGB(
      round(GetRValue(c1) * v + GetRValue(c2) * (1 - v)),
      round(GetGValue(c1) * v + GetGValue(c2) * (1 - v)),
      round(GetBValue(c1) * v + GetBValue(c2) * (1 - v)));
end;

procedure TForm1.DrawFFT;

const
  maxval = 330.0;
  barwidth = 3;
  bargap = 1;

var
  fftdata: TAuComplexArray;
  i, c: integer;
  val: Extended;
  posx: integer;
  posy: integer;
  barcount: integer;
  fpb: Single;
  bar: integer;
  bars: array of Single; 
begin
  if vis_bmp.Width <= 0 then
    exit;
    
  SetLength(fftdata, AuFFT.GetDataSize);
  AuFFT.GetChannelData(0, fftdata);

  barcount := vis_bmp.Width div (barwidth + bargap);
  c := Round(Length(fftdata) * 0.3);
  fpb := c / barcount;
  if fpb < 1 then
    fpb := 1;

  SetLength(bars, barcount);
  for i := 0 to High(bars) do
    bars[i] := 0;

  for i := 0 to c - 1 do
  begin
    bar := Round(i / fpb);
    if bar >= barcount then
      break;
    
    //Calculate the volume value
    val := Sqrt(Sqr(fftdata[i].Re)+Sqr(fftdata[i].Im));

    if val > bars[bar] then
      bars[bar] := val;
  end;

  with mask_bmp.Canvas do
  begin
    Brush.Color := clBlack;
    FillRect(Rect(0, 0, vis_bmp.Width, vis_bmp.Height));

    Brush.Color := clWhite;
    Pen.Color := clWhite;
    for i := 0 to barcount-1 do
    begin
      //Calculate the x position of the bar
      posx := i * (barwidth + bargap);

      //Calculate the height of the bar
      posy := Round((AuToDezibel(bars[i] / maxval)+30) * 0.033333 * vis_bmp.Height);
      if posy > 0 then      
        Rectangle(posx, vis_bmp.Height, posx+barwidth, vis_bmp.Height - posy);
    end;
  end;

  with vis_bmp.Canvas do
  begin
    for i := 0 to vis_bmp.Height - 1 do
    begin
      Pen.Color := ColorBetween(clWebOrangeRed, clWhite, i / vis_bmp.Height);
      MoveTo(0, i);
      LineTo(vis_bmp.Width, i);
    end;
  end;

  BitBlt(
    vis_bmp.Canvas.Handle, 0, 0, vis_bmp.Width, vis_bmp.Height,
    mask_bmp.Canvas.Handle, 0, 0, SRCAND);

  vis_bmp.TransparentColor := clBlack;
  vis_bmp.Transparent := true;

  with mask_bmp.Canvas do
  begin
    for i := 0 to mask_bmp.Height - 1 do
    begin
      Pen.Color := ColorBetween(clBlack, RGB(50,50,50), i / vis_bmp.Height);
      MoveTo(0, i);
      LineTo(vis_bmp.Width, i);
    end;
    Draw(0, 0, vis_bmp);
  end;

  PaintBox1.Canvas.Draw(0, 0, mask_bmp); 
end;

procedure TForm1.DrawOscibars;
var
  ds: integer;
  i, j, h: integer;
  ps: PSingle;
  val, lv, iv: Single;
  cc, sc: integer;
  x, y, lx, s: integer;

begin
  x := 0;
  lv := 0;
  with vis_bmp.Canvas do
  begin
    ds := AuOscilloscope.GetDataSize;
    ReallocMem(oszmem, ds);
    if AuOscilloscope.GetData(oszmem) then
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, vis_bmp.Width, vis_bmp.Height));

      ps := PSingle(oszmem);
      cc := AuOscilloscope.Parameters.Channels;
      sc := ds div Integer(AuBytesPerSample(AuOscilloscope.Parameters));
      for i := 0 to sc - 1 do
      begin
        val := 0;
        for j := 0 to cc - 1 do
        begin
          val := val + ps^;
          inc(ps);                     
        end;
        val := val / cc;

        Pen.Color := ColorBetween(clWebCornFlowerBlue, clNavy, abs(val));
        lx := x;
        x := round(i / sc * vis_bmp.Width);
        s := x - (lx + 1);
        for h := lx + 1 to x do
        begin
          if s > 0 then
          begin
            iv := (h - (lx + 1)) * (1 / s);
            iv := lv * (1 - iv) + val * iv;
          end else
            iv := val;
            
          y := round((vis_bmp.Height / 2) * (abs(iv) + 1));
          MoveTo(h, y);
          y := round((vis_bmp.Height / 2) * (-abs(iv) + 1));
          LineTo(h, y);
        end;
        lv := val;
      end;
    end;
  end;

  PaintBox1.Canvas.Draw(0, 0, vis_bmp);
end;

procedure TForm1.DrawOscilloscope;
var
  ds: integer;
  i, j: integer;
  ps: PSingle;
  val: Single;
  cc, sc: integer;
  x, y: integer;

begin
  with vis_bmp.Canvas do
  begin
    ds := AuOscilloscope.GetDataSize;
    ReallocMem(oszmem, ds);
    if AuOscilloscope.GetData(oszmem) then
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, vis_bmp.Width, vis_bmp.Height));

      ps := PSingle(oszmem);
      cc := AuOscilloscope.Parameters.Channels;
      sc := ds div Integer(AuBytesPerSample(AuOscilloscope.Parameters));
      for i := 0 to sc - 1 do
      begin
        val := 0;
        for j := 0 to cc - 1 do
        begin
          val := val + ps^;
          inc(ps);
        end;
        val := val / cc;

        Pen.Color := ColorBetween(clWebCornFlowerBlue, clNavy, abs(val));
        x := round(i / sc * vis_bmp.Width);
        y := round((vis_bmp.Height / 2) * (val + 1));
        if i = 0 then
          MoveTo(x, y)
        else
          LineTo(x, y);
      end;
    end;
  end;

  PaintBox1.Canvas.Draw(0, 0, vis_bmp);
end;

procedure TForm1.DrawPeakMeter;
var
  i: integer;
  h: integer;
  w: integer;
  peaks: TAuPeaks;

  procedure DrawGradientRect(canvas: TCanvas; r: TRect; c1, c2: TColor; w: Integer);
  var
    i: integer;
  begin
    for i := r.Left to r.Right do
    begin
      Canvas.Pen.Color := ColorBetween(c2, c1, i / w);
      Canvas.MoveTo(i, r.Top);
      Canvas.LineTo(i, r.Bottom);
    end;
  end;
  
begin
  with vis_bmp.Canvas do
  begin
    if vis_bmp.Width <= 0 then
      exit;
      
    if AuPeakMeter.GetPeaks(peaks) then
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, vis_bmp.Width, vis_bmp.Height));

      h := vis_bmp.Height div Length(peaks.ChannelPeaks);
      for i := 0 to High(peaks.ChannelPeaks) do
      begin
        w := Round((AuToDezibel(peaks.ChannelPeaks[i]) + 30) * 0.0333 * vis_bmp.Width);
        if w < 0 then
          w := 0;
        DrawGradientRect(
          vis_bmp.Canvas,
          Rect(0, i * h + 1, w, (i + 1) * h -1),
          clNavy, clWebCornFlowerBlue,
          vis_bmp.Width);
      end;
    end;
  end;

  PaintBox1.Canvas.Draw(0, 0, vis_bmp);
end;

end.
