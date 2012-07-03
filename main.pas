unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, HTTPSender, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    mmo: TMemo;
    btn1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.btn1Click(Sender: TObject);
var
  HTTP: THttpSender;
  r: ansistring;
  a: TStringStream;
begin
  HTTP := THttpSender.Create;
  HTTP.Get('http://zhyk.ru/');
  //showmessage(HTTP.response.RawHeaders);
  //showmessage(HTTP.response.Location);
 mmo.Text:=http.responsetext;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  // http.CookieManager.CookieCollection
end;

end.
