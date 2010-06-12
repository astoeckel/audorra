unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XPMan, ImgList, ComCtrls,

  AcNotify,
  AuDirectSound, AuWAV, AuAcinerella, AuAudio, Au3DAudio, Au3DAudioRenderer;
  
type
  TForm1 = class(TForm)
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button3: TButton;
    ListView1: TListView;
    ImageList1: TImageList;
    XPManifest1: TXPManifest;
    Button4: TButton;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AuAudio: TAuAudio;
    Au3DAudio: TAu3DAudio;
    AuSoundList: TAuSoundList;
    procedure DisplayItems;
    procedure OnNotify(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  AuSoundList.SaveToFile('list.asl');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i: integer;
begin
  if OpenDialog1.Execute then
  begin
    for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      with AuSoundList.AddNew(ExtractFileName(OpenDialog1.Files[i])) do
      begin
        LoadFromFile(OpenDialog1.Files[i]);
        Open;
      end;
    end;

    DisplayItems;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  AuSoundList.LoadFromFile('list.asl');
  DisplayItems;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  AuSoundList.Clear;
  DisplayItems;
end;

procedure TForm1.DisplayItems;
var
  i: integer;
begin
  ListView1.Clear;
  for i := 0 to AuSoundList.Count - 1 do
  begin
    with ListView1.Items.Add do
    begin
      Caption := AuSoundList[i].Name;
      ImageIndex := 0;
      SubItems.Add(FormatFloat('#0.00', AuSoundList[i].Len / 1000)+'s');
      SubItems.Add(IntToStr(AuSoundList[i].Sound.Emitters.Count));
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;

  AuAudio := TAuAudio.Create;
  if AuAudio.Initialize then
  begin
    Au3DAudio := TAu3DAudio.Create(AuAudio, au3dss51, 44100, 16);
    if Au3DAudio.Initialize then
      AuSoundList := TAuSoundList.Create(Au3DAudio);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AuSoundList.Free;
  Au3DAudio.Free;
  AuAudio.Free;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
var
  sel: TAuStaticSound;
  em: TAu3DStaticEmitter;
begin
  if ListView1.ItemIndex >= 0 then
  begin
    sel :=  AuSoundList.Items[ListView1.ItemIndex];
    if sel <> nil then
    begin
      Au3DAudio.Lock;
      try
        em := TAu3DStaticEmitter.Create(sel.Sound);
        em.OnStop := OnNotify;
        sel.Sound.Loop := CheckBox1.Checked;
      finally
        Au3DAudio.Unlock;
      end;

      ListView1.Items[ListView1.ItemIndex].SubItems[1] := IntToStr(AuSoundList[ListView1.ItemIndex].Sound.Emitters.Count);
    end;
  end;
end;

procedure TForm1.OnNotify(Sender: TObject);
var
  i: integer;
begin
  Au3DAudio.Lock;
  try
    if (not CheckBox1.Checked) or (not TAu3DStaticSound(TAu3DStaticEmitter(Sender).Sound).Loop) then
      TAu3DStaticEmitter(Sender).Free;

    for i := 0 to AuSoundList.Count - 1 do
    begin
      if AuSoundList[i].Sound = TAu3DStaticEmitter(Sender).Sound then
        ListView1.Items[i].SubItems[1] := IntToStr(AuSoundList[i].Sound.Emitters.Count);
    end;
  finally
    Au3DAudio.Unlock;
  end;
end;

end.
