unit main_laz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Menus, ActnList, ExtCtrls, StdCtrls, Buttons,
  LCLIntf, LCLType,

  AuAudio, AuTypes, AuUtils, AuWAV, AuAcinerella, AuVisualisations,
  {$IFDEF WIN32}
  AuDirectSound;
  {$ELSE}
  AuALSA;
  {$ENDIF}

type

  { TfrmMain }

  TfrmMain = class(TForm)
    act_mute: TAction;
    act_prev: TAction;
    act_next: TAction;
    act_stop: TAction;
    act_pause: TAction;
    act_play: TAction;
    act_ply_removefiles: TAction;
    act_ply_addfiles: TAction;
    act_quit: TAction;
    act_open_url: TAction;
    act_open: TAction;
    ActionList1: TActionList;
    imgl_state: TImageList;
    imgl_16: TImageList;
    imgl_24: TImageList;
    lstPlaylist: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    dlgOpenMedia: TOpenDialog;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlTime: TPanel;
    PopupMenu1: TPopupMenu;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    trbPosition: TTrackBar;
    procedure act_muteExecute(Sender: TObject);
    procedure act_nextExecute(Sender: TObject);
    procedure act_openExecute(Sender: TObject);
    procedure act_pauseExecute(Sender: TObject);
    procedure act_playExecute(Sender: TObject);
    procedure act_ply_addfilesExecute(Sender: TObject);
    procedure act_ply_removefilesExecute(Sender: TObject);
    procedure act_prevExecute(Sender: TObject);
    procedure act_quitExecute(Sender: TObject);
    procedure act_stopExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstPlaylistDblClick(Sender: TObject);
    procedure lstPlaylistKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure trbPositionChange(Sender: TObject);
    procedure trbPositionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure trbPositionMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure ShowHint(Sender: TObject);
    procedure PlayerStop(Sender: TObject);
    procedure PlayerStateChange(Sender: TObject);
  public
    AuAudio: TAuAudio;
    AuPlayer: TAuPlayer;
    AuPeaks: TAuVisualisation;

    plyback: integer;

    peakmeter_bg: TBitmap;
    peakmeter: TBitmap;

    upd_trb: boolean;
    trackbar_drag: boolean;
    seeked: Boolean;

    function AddFiles(AClear: boolean = false): boolean;
    procedure DeleteFiles;
    procedure PlayCurrent;
    procedure Adv(ACount: integer);
  end;

var
  frmMain: TfrmMain;

implementation

{ TfrmMain }

procedure TfrmMain.act_quitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.act_stopExecute(Sender: TObject);
begin
  AuPlayer.Stop;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  AuPeaks.Resize(PaintBox1.ClientWidth, PaintBox1.ClientHeight);
end;

procedure TfrmMain.act_openExecute(Sender: TObject);
begin
  if AddFiles(true) then
  begin
    plyback := 0;
    PlayCurrent;
  end;
end;

procedure TfrmMain.act_muteExecute(Sender: TObject);
begin
  act_mute.Checked := not act_mute.Checked;
  if act_mute.Checked then
  begin
    AuPlayer.MasterVolume := 0;
    act_mute.ImageIndex := 9;
  end else
  begin
    AuPlayer.MasterVolume := 1;
    act_mute.ImageIndex := 8;
  end;
end;

procedure TfrmMain.act_nextExecute(Sender: TObject);
begin
  Adv(1);
end;

procedure TfrmMain.act_pauseExecute(Sender: TObject);
begin
  AuPlayer.Pause;
end;

procedure TfrmMain.act_playExecute(Sender: TObject);
begin
  AuPlayer.Play;
end;

procedure TfrmMain.act_ply_addfilesExecute(Sender: TObject);
begin
  AddFiles;
end;

procedure TfrmMain.act_ply_removefilesExecute(Sender: TObject);
begin
  DeleteFiles;
end;

procedure TfrmMain.act_prevExecute(Sender: TObject);
begin
  Adv(-1);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.OnHint := @ShowHint;

  AuAudio := TAuAudio.Create;
  if AuAudio.Initialize then
  begin
    AuPlayer := TAuPlayer.Create(AuAudio);
    AuPlayer.OnStateChange := @PlayerStateChange;
    AuPlayer.OnSongFinishes := @PlayerStop;

    AuPeaks := TAuFFTVisualisation.Create(512);
    AuPeaks.Analyzer.Active := true;
    AuPlayer.AddAnalyzer(AuPeaks.Analyzer);
  end;

  peakmeter_bg := TBitmap.Create;
  peakmeter := TBitmap.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  AuPlayer.Free;
  AuPeaks.Free;
  AuAudio.Free;
end;

procedure TfrmMain.lstPlaylistDblClick(Sender: TObject);
begin
  if lstPlaylist.Selected <> nil then
  begin
    plyback := lstPlaylist.Selected.Index;
    PlayCurrent;
  end else
    AddFiles(false);
end;

procedure TfrmMain.lstPlaylistKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_BACK) or (Key = VK_DELETE) then
    DeleteFiles;
end;

procedure TfrmMain.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, peakmeter);
end;

procedure TfrmMain.PaintBox1Resize(Sender: TObject);
begin
  AuPeaks.Resize(PaintBox1.ClientWidth, PaintBox1.ClientHeight);
end;

const
  day = 24 * 60 * 60 * 1000;

procedure TfrmMain.Timer1Timer(Sender: TObject);
var
  i: integer;
  h: integer;
  w: integer;
  peaks: TAuPeaks;
begin
  if AuPeaks.Update then
    AuPeaks.Draw(PaintBox1.Canvas);

  if (AuPlayer <> nil) then
  begin
    upd_trb := true;
    if AuPlayer.Position > -1 then
    begin
      Panel3.Visible := (AuPlayer.Len >= 0);

      if (not trackbar_drag) then
      begin
        trbPosition.Position := AuPlayer.Position div 10;
      end;
      pnlTime.Caption := FormatDateTime('hh:nn:ss.zzz', AuPlayer.Position / day);
    end;
  end;
end;

procedure TfrmMain.trbPositionChange(Sender: TObject);
begin
  if (not upd_trb) and (AuPlayer.State >= aupsOpened) then
  begin
    seeked := true;
  end;

  upd_trb := false;
end;

procedure TfrmMain.trbPositionMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  trackbar_drag := true;
end;

procedure TfrmMain.trbPositionMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if seeked then
  begin
    AuPlayer.Position := trbPosition.Position * 10;
    AuPlayer.Play;
    seeked := false;
  end;
  trackbar_drag := false;
end;

procedure TfrmMain.ShowHint(Sender: TObject);
begin
  StatusBar1.SimpleText := Application.Hint;
end;

procedure TfrmMain.PlayerStop(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to lstPlaylist.Items.Count - 1 do
    lstPlaylist.Items[i].ImageIndex := -1;

  Adv(1);
end;

procedure TfrmMain.PlayerStateChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to lstPlaylist.Items.Count - 1 do
    lstPlaylist.Items[i].ImageIndex := -1;

  if (plyback >= 0) and (plyback < lstPlaylist.Items.Count) then
  begin
    if (AuPlayer.State = aupsPlaying) then
      lstPlaylist.Items[plyback].ImageIndex := 1
    else if (AuPlayer.State = aupsPaused) then
      lstPlaylist.Items[plyback].ImageIndex := 2
    else if (AuPlayer.State = aupsOpened) then
      lstPlaylist.Items[plyback].ImageIndex := 0;
  end;
end;

function TfrmMain.AddFiles(AClear: boolean): boolean;
var
  i: integer;
begin
  result := false;
  if dlgOpenMedia.Execute then
  begin
    lstPlaylist.BeginUpdate;
    if AClear then
      lstPlaylist.Clear;

    for i := 0 to dlgOpenMedia.Files.Count - 1 do
      with lstPlaylist.Items.Add do
      begin
        Caption := dlgOpenMedia.Files[i];
        ImageIndex := -1;
      end;
    lstPlaylist.EndUpdate;

    result := true;
  end;
end;

procedure TfrmMain.DeleteFiles;
var
  i: integer;
begin
  lstPlaylist.BeginUpdate;
  for i := lstPlaylist.Items.Count - 1 downto 0 do
    if lstPlaylist.Items[i].Selected then
      lstPlaylist.Items.Delete(i);

  lstPlaylist.EndUpdate;
end;

procedure TfrmMain.PlayCurrent;
var
  i: integer;
begin
  for i := 0 to lstPlaylist.Items.Count - 1 do
    lstPlaylist.Items[i].ImageIndex := 0;

  if (plyback >= 0) and (plyback < lstPlaylist.Items.Count) then
  begin
    AuPlayer.LoadFromFile(lstPlaylist.Items[plyback].Caption);
    if AuPlayer.Open then
      AuPlayer.Play;

    upd_trb := true;
    trbPosition.Max := AuPlayer.Len div 10;
    Panel3.Visible := ((AuPlayer.Position > -1) and (AuPlayer.Len > -1));

    lstPlaylist.ItemFocused := lstPlaylist.Items[plyback];
    lstPlaylist.ItemFocused.ImageIndex := 1;
  end;
end;

procedure TfrmMain.Adv(ACount: integer);
begin
  plyback := plyback + ACount;
  if plyback < 0 then
    plyback := 0
  else if plyback >= lstPlaylist.Items.Count then
    plyback := lstPlaylist.Items.Count - 1
  else
    PlayCurrent;
end;

initialization
  {$I main_laz.lrs}

end.

