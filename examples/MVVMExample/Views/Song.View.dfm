object SongView: TSongView
  Left = 0
  Top = 0
  Caption = 'Song'
  ClientHeight = 123
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  PixelsPerInch = 96
  DesignSize = (
    623
    123)
  TextHeight = 13
  object edTitle: TLabeledEdit
    Left = 24
    Top = 40
    Width = 569
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 20
    EditLabel.Height = 13
    EditLabel.Caption = 'Title'
    TabOrder = 0
    Text = ''
  end
  object btnSave: TBitBtn
    Left = 518
    Top = 88
    Width = 75
    Height = 25
    Action = actClose
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
  end
  object btnCancel: TBitBtn
    Left = 432
    Top = 88
    Width = 75
    Height = 25
    Action = actClose
    Caption = 'Close'
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 2
  end
  object ActionList: TActionList
    Left = 320
    Top = 16
    object actClose: TAction
      Caption = 'Close'
      OnExecute = actCloseExecute
    end
  end
end
