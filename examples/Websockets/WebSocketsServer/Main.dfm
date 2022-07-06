object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Websocket server '
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 15
  object btnStart: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = btnStopClick
  end
  object edtMessage: TEdit
    Left = 8
    Top = 39
    Width = 545
    Height = 23
    TabOrder = 2
  end
  object btnSend: TButton
    Left = 559
    Top = 38
    Width = 57
    Height = 25
    Caption = 'Send'
    TabOrder = 3
    OnClick = btnSendClick
  end
  object mmMessages: TMemo
    Left = 8
    Top = 68
    Width = 608
    Height = 365
    ReadOnly = True
    TabOrder = 4
  end
end
