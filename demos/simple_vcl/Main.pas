unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, ToolWin, StdCtrls, ExtCtrls, Menus, Math, XPMan, FileCtrl,
  wnd_OpenURL,

  AcPersistent, AcDLLExplorer, AcStrUtils,
  AuHTTP, AuVorbis, AuCDAudio, AuWASAPI, AuDirectSound, AuUtils, AuTypes, AuAudio, AuAnalyzers, AuComplex,
  AuVisualisations, Tabs, Buttons;

type
  TfrmPlayer = class(TForm)
    pnlInfo: TPanel;
    lstPlaylist: TListView;
    TrackBar1: TTrackBar;
    XPManifest1: TXPManifest;
    trbVol: TTrackBar;
    lblVol: TLabel;
    pnlTop: TPanel;
    lblPos: TLabel;
    imglMenu: TImageList;
    pmenOpen: TPopupMenu;
    Openfiles1: TMenuItem;
    OpenURL1: TMenuItem;
    Opendirectory1: TMenuItem;
    N1: TMenuItem;
    Timer1: TTimer;
    imglState: TImageList;
    OpenDialog1: TOpenDialog;
    pnlInfoTop: TPanel;
    barMenu: TToolBar;
    btnPlay: TToolButton;
    btnPause: TToolButton;
    btnStop: TToolButton;
    ToolButton4: TToolButton;
    btnOpen: TToolButton;
    lblLength: TLabel;
    btnMute: TSpeedButton;
    btnVolNor: TSpeedButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    pnlVis: TPanel;
    pntbVis: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure pnlVisResize(Sender: TObject);
    procedure trbVolChange(Sender: TObject);
    procedure lstPlaylistDblClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnOpenDirClick(Sender: TObject);
    procedure lstPlaylistKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure volitemClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnOpenURL(Sender: TObject);
    procedure pntbVisMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnMuteClick(Sender: TObject);
    procedure btnVolNorClick(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
  public
    AuAudio: TAuAudio;
    AuPlayer: TAuPlayer;

    AuVisualisations: array of TAuVisualisation;
    AuCurrentVisualisation: integer;

    vol_chn: integer;

    changed: boolean;
    prefix: string;
    suffix: string;
    playlist_ind: integer;
    trbchange_time: Cardinal;
    seek: boolean;
    procedure FinishedSong(Sender: TObject);
    procedure ActivateButtons;
    procedure DeactivateButtons;
    procedure PlayerStateChange(Sender: TObject);
    procedure PlayPlyLstSong;
    procedure LastPlyLstSong;
    procedure NextPlyLstSong;
    procedure RemoveSongs;
    function AddSongs(AClear: boolean = false): boolean;
    procedure OpenSongs;
    procedure OpenURL;
    procedure AddDirectory(ADir: string);
  end;

var
  frmPlayer: TfrmPlayer;

const
  vis_count = 4;

implementation

{$R *.dfm}

procedure TfrmPlayer.File1Click(Sender: TObject);
begin
  lstPlaylist.Visible := not lstPlaylist.Visible;
  if not lstPlaylist.Visible then
  begin
    ClientHeight := pnlTop.Height + 16;
    Constraints.MaxHeight := Height;
    Constraints.MinHeight := Height;
  end else
  begin
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    ClientHeight := pnlTop.Height + 200;
  end;
end;

procedure TfrmPlayer.FinishedSong(Sender: TObject);
begin
  NextPlyLstSong;
end;

procedure TfrmPlayer.FormCreate(Sender: TObject);
var
  i: integer;
begin
  AcRegSrv.DbgDump;

  ReportMemoryLeaksOnShutdown := true;

  AuAudio := TAuAudio.Create;

  //Initialize the form
  DeactivateButtons;   

  if AuAudio.Initialize then
  begin
    AuPlayer := TAuPlayer.Create(AuAudio);
    AuPlayer.OnStateChange := PlayerStateChange;
    AuPlayer.OnSongFinishes := FinishedSong;

    SetLength(AuVisualisations, 3);
    AuVisualisations[0] := TAuFFTVisualisation.Create(512);
    AuVisualisations[1] := TAuPeakMeterVisualisation.Create;
    AuVisualisations[2] := TAuOsciloscopeVisualisation.Create(0.1);
    for i := 0 to High(AuVisualisations) do
      AuPlayer.AddAnalzyer(AuVisualisations[i].Analyzer);

    AuCurrentVisualisation := 0;
    AuVisualisations[AuCurrentVisualisation].Activate;

    Timer1.Enabled := true;
  end else
  begin
    ShowMessage(AuAudio.GetLastError);
    FreeAndNil(AuAudio);
    halt;
  end;
end;

procedure TfrmPlayer.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  AuPlayer.Free;
  for i := 0 to Length(AuVisualisations) - 1 do
    AuVisualisations[i].Free;
  AuAudio.Free;
end;

procedure TfrmPlayer.pnlVisResize(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(AuVisualisations) do
    AuVisualisations[i].Resize(pntbVis.Width, pntbVis.Height);
end;

procedure TfrmPlayer.LastPlyLstSong;
begin
  playlist_ind := playlist_ind - 1;
  if playlist_ind < 0 then
    playlist_ind := 0
  else begin
    lstPlaylist.ItemIndex := playlist_ind;
    PlayPlyLstSong;
  end;
end;

procedure TfrmPlayer.lstPlaylistDblClick(Sender: TObject);
begin
  if lstPlaylist.ItemIndex <> -1 then
  begin
    playlist_ind := lstPlaylist.ItemIndex;
    PlayPlyLstSong;
  end else
    AddSongs;
end;

procedure TfrmPlayer.lstPlaylistKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) or (Key = VK_BACK) then
    RemoveSongs;
end;

procedure TfrmPlayer.NextPlyLstSong;
begin
  playlist_ind := playlist_ind + 1;
  if playlist_ind >= lstPlaylist.Items.Count then
    playlist_ind := lstPlaylist.Items.Count - 1
  else begin
    lstPlaylist.ItemIndex := playlist_ind;
    PlayPlyLstSong;
  end;
end;

procedure TfrmPlayer.pntbVisMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //Deaktivate the current visualisation
  AuVisualisations[AuCurrentVisualisation].Deactivate;

  //Calculate the next visualisation index
  AuCurrentVisualisation := (AuCurrentVisualisation + 1) mod
    (Length(AuVisualisations));

  //Activate the next visualisation
  AuVisualisations[AuCurrentVisualisation].Activate;
end;

procedure TfrmPlayer.PlayerStateChange(Sender: TObject);
begin
  prefix := '';
  case AuPlayer.State of
    aupsOpened: prefix := '[OPENED] ';
    aupsPlaying: prefix := '[PLAYING] ';
    aupsPaused: prefix := '[PAUSED] ';
  end;

  Caption := prefix + suffix;
end;

procedure TfrmPlayer.PlayPlyLstSong;
var
  i: integer;
begin
  DeactivateButtons;
  AuPlayer.LoadFromURL(lstPlaylist.Items[playlist_ind].Caption);
  if AuPlayer.Open then
  begin
    AuPlayer.Play;
    ActivateButtons;
  end;

  for i := 0 to lstPlaylist.Items.Count - 1 do
    lstPlaylist.Items[i].ImageIndex := 0;

  lstPlaylist.Items[playlist_ind].ImageIndex := 1;

  suffix := ExtractFileName(lstPlaylist.Items[playlist_ind].Caption);

  Caption := prefix + suffix;
  if Caption = '' then
    Caption := 'Simple Player Demo';
end;

procedure TfrmPlayer.RemoveSongs;
var
  i: integer;
begin
  lstPlaylist.Items.BeginUpdate;
  for i := lstPlaylist.Items.Count - 1 downto 0 do
    if lstPlaylist.Items[i].Selected then
      lstPlaylist.Items.Delete(i);
  lstPlaylist.Items.EndUpdate;
end;

procedure TfrmPlayer.btnVolNorClick(Sender: TObject);
begin
  trbVol.Position := 0;
end;

function MSToTime(AV: Integer): string;
begin
  result :=
    FormatFloat('00', AV div (3600 * 1000)) + ':' +
    FormatFloat('00', (AV div 60000) mod 60) + ':' +
    FormatFloat('00', (AV div 1000) mod 60);
{    FormatFloat('000', AV mod 1000);}
end;

procedure TfrmPlayer.Timer1Timer(Sender: TObject);
var
  pos: integer;
begin
  if AuVisualisations[AuCurrentVisualisation].Update then;
    AuVisualisations[AuCurrentVisualisation].Draw(pntbVis.Canvas);

  if AuPlayer <> nil then
  begin
    if AuPlayer.Len > -1 then
    begin
      TrackBar1.Enabled := true;
      lblPos.Caption :=
        MSToTime(AuPlayer.Position);
      lblLength.Caption := MSToTime(AuPlayer.Len);
    end
    else
    begin
      TrackBar1.Enabled := false;
      lblLength.Caption := '00:00:00';
      lblPos.Caption :=
        MSToTime(AuPlayer.Position);
      exit;
    end;

    if GetTickCount - trbchange_time > 250 then
    begin
      changed := true;

      if seek then
      begin
        seek := false;
        AuPlayer.Position := TrackBar1.Position;
      end;
        
      pos := AuPlayer.Len;
      if pos >= 0 then
        TrackBar1.Max := pos;

      pos := AuPlayer.Position;
      if pos >= 0 then
        TrackBar1.Position := pos;
    end;
  end;
end;

procedure TfrmPlayer.TrackBar1Change(Sender: TObject);
begin
  if not changed then
  begin
    seek := true;
    trbchange_time := GetTickCount;
  end;
  changed := false;
end;

procedure TfrmPlayer.btnOpenDirClick(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select a folder', '', dir) then
    AddDirectory(dir);
end;

procedure TfrmPlayer.trbVolChange(Sender: TObject);
begin
  AuPlayer.MasterVolume := AuFromDezibel(trbVol.Position / 10);
  
  if trbVol.Position <= 0 then
    lblVol.Caption := FormatFloat('0.0', trbVol.Position / 10) + 'dB';
  if trbVol.Position > 0 then
    lblVol.Caption := '+' + FormatFloat('0.0', trbVol.Position / 10) + 'dB';

  if trbVol.Position = trbVol.Min then
  begin
    AuPlayer.MasterVolume := 0;
    lblVol.Caption := '-INF dB';
  end;
end;

procedure TfrmPlayer.volitemClick(Sender: TObject);
begin
  vol_chn := TMenuItem(Sender).Tag;
  TMenuItem(Sender).Checked := true;

  if vol_chn >= 0 then
    TrackBar1.Position := Round(AuToDezibel(AuPlayer.VolumeFilter.Volume[vol_chn]))
  else
    TrackBar1.Position := Round(AuToDezibel(AuPlayer.MasterVolume));
end;

procedure TfrmPlayer.AddDirectory(ADir: string);
var
  sr: TSearchRec;
  ext: string;
begin
  lstPlaylist.Items.BeginUpdate;
  
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
          with lstPlaylist.Items.Add do
          begin
            Caption := ADir + sr.Name;
            ImageIndex := 0;
          end;
        end;
      end;
    end;
  end;
  FindClose(sr);

  lstPlaylist.Items.EndUpdate;
end;

function TfrmPlayer.AddSongs(AClear: boolean): boolean;
var
  i: integer;
begin
  result := false;
  suffix := '';
  if OpenDialog1.Execute then
  begin
    if AClear then
      lstPlaylist.Clear;
      
    for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      with lstPlaylist.Items.Add do
      begin
        Caption := OpenDialog1.Files[i];
        ImageIndex := 0;
      end;
    end;

    result := true;
  end; 
end;

procedure TfrmPlayer.OpenSongs;
begin
  if AddSongs(true) then
  begin
    playlist_ind := 0;
    PlayPlyLstSong;
  end;
end;

procedure TfrmPlayer.OpenURL;
var
  add: boolean;
  url: string;
begin
  if OpenURLWnd.GetURL(url, add) then
  begin
    if not add then
      lstPlaylist.Clear;

    with lstPlaylist.Items.Add do
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

procedure TfrmPlayer.btnMuteClick(Sender: TObject);
begin
  trbVol.Position := trbVol.Min;
end;

procedure TfrmPlayer.btnOpenClick(Sender: TObject);
begin
  OpenSongs;
end;

procedure TfrmPlayer.btnOpenURL(Sender: TObject);
begin
  OpenURL;
end;

procedure TfrmPlayer.btnPauseClick(Sender: TObject);
begin
  AuPlayer.Pause;
end;

procedure TfrmPlayer.btnPlayClick(Sender: TObject);
begin
  AuPlayer.Play;
end;

procedure TfrmPlayer.btnStopClick(Sender: TObject);
begin
  AuPlayer.Stop;
end;

procedure TfrmPlayer.Button1Click(Sender: TObject);
begin
  with lstPlaylist.Items.Add do
  begin
    Caption := InputBox('URL','Enter an URL.', '');
    ImageIndex := 0;
  end;
end;

procedure TfrmPlayer.ActivateButtons;
begin
  btnPlay.Enabled := true;
  btnPause.Enabled := true;
  btnStop.Enabled := true;
end;

procedure TfrmPlayer.DeactivateButtons;
begin
  btnPlay.Enabled := false;
  btnPause.Enabled := false;
  btnStop.Enabled := false;
end;

end.
