program SeloDigital;

uses
  Vcl.Forms,
  SeloForm in 'SeloForm.pas' {FormSelo},
  MensagemForm in 'MensagemForm.pas' {FormMensagem};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSelo, FormSelo);
  Application.Run;
end.
