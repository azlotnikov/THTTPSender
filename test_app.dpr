program test_app;

uses
  Vcl.Forms,
  main in 'main.pas' {Form3},
  HTTPSender in 'HTTPSender.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
