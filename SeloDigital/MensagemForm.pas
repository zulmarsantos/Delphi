unit MensagemForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TFormMensagem = class(TForm)
    panel: TPanel;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FTipoAto: string;
    FProtocolo: string;

    procedure SetProtocolo(const Value: string);
    procedure SetTipoAto(const Value: string);

  public
    property Protocolo: string read FProtocolo write SetProtocolo;
    property TipoAto: string read FTipoAto write SetTipoAto;
  end;

var
  FormMensagem: TFormMensagem;

implementation

{$R *.dfm}

{ TFormMensagem }

procedure TFormMensagem.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  panel.Caption := EmptyStr;
end;

procedure TFormMensagem.FormShow(Sender: TObject);
begin
  panel.Caption := Format('Aguarde! Processando protocolo %s com tipo de ato %s...', [Protocolo, TipoAto]);
end;

procedure TFormMensagem.SetProtocolo(const Value: string);
begin
  FProtocolo := Value;
end;

procedure TFormMensagem.SetTipoAto(const Value: string);
begin
  FTipoAto := Value;
end;

end.
