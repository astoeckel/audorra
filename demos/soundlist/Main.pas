unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XPMan, ImgList, ComCtrls,

  AuOpenAL, {AuWaveOut32Driver,} AuWAV, AuAcinerella, AuAudio,

  AdTypes;

type
  TForm1 = class(TForm)
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button3: TButton;
    Timer1: TTimer;
    ListView1: TListView;
    ImageList1: TImageList;
    XPManifest1: TXPManifest;
    Timer2: TTimer;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AuAudio: TAuAudio;
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
      SubItems.Add(IntToStr(AuSoundList[i].Instances.Count));
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;

  AuAudio := TAuAudio.Create;
  if AuAudio.Initialize then
    AuSoundList := TAuSoundList.Create(AuAudio);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AuSoundList.Free;
  AuAudio.Free;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
var
  sel, newsound: TAuStaticSound;
begin
  if ListView1.ItemIndex >= 0 then
  begin
    sel :=  AuSoundList.Items[ListView1.ItemIndex];
    if sel <> nil then
    begin
      newsound := TAuStaticSound.Create(AuAudio);
      newsound.AutoFreeOnStop := true;
      newsound.Assign(sel);
      newsound.Play;
      newsound.OnSoundFinishes := OnNotify;

      ListView1.Items[ListView1.ItemIndex].SubItems[1] := IntToStr(AuSoundList[ListView1.ItemIndex].Instances.Count);
    end;
  end;
end;

procedure TForm1.OnNotify(Sender: TObject);
var
  ind: integer;
begin
  ind := AuSoundList.IndexOf(TAuStaticSound(Sender).ParentSound);
  if ind >= 0 then
  begin
    ListView1.Items[ind].SubItems[1] := IntToStr(AuSoundList[ind].Instances.Count - 1);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  AuSoundList.Move(1);
end;

end.
