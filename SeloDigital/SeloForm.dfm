object FormSelo: TFormSelo
  Left = 0
  Top = 0
  Caption = 'Selo'
  ClientHeight = 361
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Master: TZConnection
    ControlsCodePage = cGET_ACP
    AutoEncodeStrings = True
    Catalog = ''
    Properties.Strings = (
      'controls_cp=GET_ACP')
    HostName = ''
    Port = 0
    Database = ''
    User = ''
    Password = ''
    Protocol = 'MariaDB-10'
  end
  object Query: TZQuery
    Connection = Master
    SQL.Strings = (
      'update selados'
      '   set selo = :p_selo,'
      '       qrcode = :p_qrcode,'
      '       img = :p_img'
      ' where numtipato = :p_numtipato'
      '   and idap = :p_idap'
      '   and registro = :p_registro'
      '')
    Params = <
      item
        DataType = ftString
        Name = 'p_selo'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'p_qrcode'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'p_img'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'p_numtipato'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'p_idap'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'p_registro'
        ParamType = ptInput
      end>
    Top = 32
    ParamData = <
      item
        DataType = ftString
        Name = 'p_selo'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'p_qrcode'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'p_img'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'p_numtipato'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'p_idap'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'p_registro'
        ParamType = ptInput
      end>
  end
end
