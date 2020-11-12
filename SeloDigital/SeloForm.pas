unit SeloForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB, Data.Win.ADODB,
  Data.SqlExpr, IniFiles, ZAbstractConnection, ZConnection, ZAbstractRODataset,
  ZAbstractDataset, ZDataset, System.JSON, Vcl.ExtCtrls, IdCoderMIME, Jpeg,
  System.StrUtils, MensagemForm;

type
  TFormSelo = class(TForm)
    Master: TZConnection;
    Query: TZQuery;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    _formMensagem: TFormMensagem;

    _jsonRetorno: TJSONValue;

    _tipoAto: string;
    _idap: string;
    _registro: string;

    _status: Integer;
    _mensagem: string;

    procedure gerar;
    procedure conectar;
    procedure atualizar;

    function converter(numeroSelo: string; base64: string): string;
    function extrair(const texto: string; const param1, param2: string): string;

  end;

var
  FormSelo: TFormSelo;

implementation

{$R *.dfm}

procedure TFormSelo.atualizar;
var
  imagem : TJpegImage;
  numeroSelo: string;
  arquivo: string;
begin
  Master.StartTransaction;
  try
    if _status = 0 then begin
      numeroSelo := _jsonRetorno.GetValue<string>('NumeroSelo');
      arquivo := converter(numeroSelo, _jsonRetorno.GetValue<string>('Imagem'));

      Query.ParamByName('p_selo').AsString   := numeroSelo;
      Query.ParamByName('p_qrcode').AsString := _jsonRetorno.GetValue<string>('QRCode');

      imagem := TJpegImage.Create;
      try
        imagem.LoadFromFile(arquivo);
        Query.ParamByName('p_img').Assign(imagem);
      finally
        imagem.Free;
      end;
    end
    else begin
      Query.ParamByName('p_qrcode').AsString := Concat(FormatFloat('00', _status), ' ', _mensagem);
    end;

    Query.ParamByName('p_numtipato').AsString := _tipoAto;
    Query.ParamByName('p_idap').AsString      := _idap;
    Query.ParamByName('p_registro').AsString  := _registro;
    Query.ExecSQL;

    Master.Commit;
  except
    on ex: Exception do
    begin
      Master.Rollback;
      raise Exception.Create(Concat('Erro ao atualizar banco dedados', #10#13, ex.Message));
    end;
  end;
end;

procedure TFormSelo.conectar;
var
  ini : TIniFile;
begin
  ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'selodigital.ini');
  try
    if not ini.SectionExists('SPTABEL') then begin
      raise Exception.Create('Arquivo de configuração inválido');
    end;

    Master.Connected := False;
    Master.HostName  := ini.ReadString('SPTABEL', 'HostName', '');
    Master.Port      := StrToInt(ini.ReadString('SPTABEL', 'Port', '3306'));
    Master.Database  := ini.ReadString('SPTABEL', 'Database', '');
    Master.User      := ini.ReadString('SPTABEL', 'User', '');
    Master.Password  := 'k15720';
    Master.Connected := True;
  finally
    ini.Free;
  end;
end;

function TFormSelo.converter(numeroSelo: string; base64: string): string;
var
  stream: TMemoryStream;
  decoder: TIdDecoderMIME;
  arquivo: string;
begin
  stream := TMemoryStream.Create;
  decoder := TIdDecoderMIME.Create;
  try
    arquivo := Concat('..\tmp\', numeroSelo, '.jpg');

    decoder.DecodeStream(base64, stream);
    stream.SaveToFile(arquivo);

    Result := arquivo;
  finally
    stream.Free;
    decoder.Free;
  end;
end;

function TFormSelo.extrair(const texto, param1, param2: string): string;
var
  pos1, pos2: integer;
begin
  Result := EmptyStr;

  pos1 := Pos(param1, texto) + Length(param1);
  if pos1 > 0 then begin
    pos2 := PosEx(param2, texto, pos1+1);

    if pos2 > 0 then begin
      Result := Copy(texto, pos1, pos2 - pos1 - 1);
      Result := ReplaceText(Result, '"', '');
    end;
  end;
end;

procedure TFormSelo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Application.Terminate;
end;

procedure TFormSelo.FormCreate(Sender: TObject);
begin
  Application.ShowMainForm := False; 
  try
    _formMensagem := TFormMensagem.Create(nil);
    try
      conectar;
      gerar;
    finally
      FreeAndNil(_formMensagem);
    end;
  except
    on ex: Exception do
    begin
      Application.MessageBox(pchar(ex.Message), 'Erro', MB_OK + MB_ICONERROR);
    end;
  end;
  Master.Connected := False;

  Close;  
end;

procedure TFormSelo.gerar;
var
  xHandle: THandle;
  GerarSeloDigital: function(parJSON, parPath: PAnsiChar): PAnsiChar; stdcall;
  retorno : PAnsiChar;
  parametro : string;
  posicao : Integer;
  i : Integer;
begin
  if ParamCount = 0 then
  begin
    raise Exception.Create('Parâmetro não informado na chamada');
  end;
  
  {$WARN SYMBOL_PLATFORM OFF}
  posicao := Pos('{', CmdLine)-1;
  for i := posicao to Length(CmdLine)-2 do begin
    parametro := parametro + CmdLine[i];
  end;
  {$WARN SYMBOL_PLATFORM ON}
  
  _tipoAto  := extrair(UpperCase(parametro), '"NUMERO_TIPOATO":', '"IDAP":');
  _idap     := extrair(UpperCase(parametro), '"IDAP":', '"DATAATO":');
  _registro := extrair(UpperCase(parametro), '"PROTOCOLO":', '"NUMPEDIDO":');

  _status   := 0;
  _mensagem := EmptyStr;

  retorno   := nil;

  xHandle := LoadLibrary('C:\Lyli\Egiston\Egiston.PR.dll');
  if xHandle = 0 then begin
    raise Exception.Create('Egiston.PR.dll não localizada');
  end
  else begin
    _formMensagem.Protocolo := _registro;
    _formMensagem.TipoAto   := _tipoAto;
    _formMensagem.Show;

    Application.ProcessMessages;
    try
      GerarSeloDigital := GetProcAddress(xHandle, PAnsiChar('GerarSeloDigital')); //nome do seu método no C#
      if @GerarSeloDigital = nil then begin
        raise Exception.Create('Erro ao carregar Egiston.PR.dll');
      end
      else begin
        try
          retorno := GerarSeloDigital(PAnsiChar(AnsiString(parametro)), PansiChar(AnsiString('C:\Lyli\Egiston'))); //chama o método
        except
          _status   := 98;
          _mensagem := 'Erro no processamento de retorno do selo digital';
        end;

        if _status = 0 then begin
          try
            _jsonRetorno := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(string(retorno)), 0);

            _status   := _jsonRetorno.GetValue<Integer>('Status');
            _mensagem := _jsonRetorno.GetValue<string>('Mensagem');
          except
            _status   := 99;
            _mensagem := 'Erro na conversão do retorno para JSON';
          end;
        end;

        atualizar;
      end;
    finally
      FreeLibrary(xHandle);

      _formMensagem.Hide;
    end;
  end;
end;

end.
