unit wnd_OpenURL;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TOpenURLWnd = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    function GetURL(var AUrl: string; var AAdd: boolean): boolean;
  end;

var
  OpenURLWnd: TOpenURLWnd;

implementation

{$R *.dfm}

procedure TOpenURLWnd.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Edit1.Text := OpenDialog1.FileName;
  end;
end;

function TOpenURLWnd.GetURL(var AUrl: string; var AAdd: boolean): boolean;
begin
  result := ShowModal = mrOk;
  AUrl := Edit1.Text;
  AAdd := CheckBox1.Checked;
end;

end.
