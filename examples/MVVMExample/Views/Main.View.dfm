object MainView: TMainView
  Left = 0
  Top = 0
  Caption = 'MainView'
  ClientHeight = 269
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  PixelsPerInch = 96
  DesignSize = (
    623
    269)
  TextHeight = 13
  object ShowButton: TButton
    Left = 0
    Top = 240
    Width = 105
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Open song'
    TabOrder = 0
  end
  object SongsGrid: TDBGrid
    Left = 8
    Top = 8
    Width = 607
    Height = 226
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = SongsDataSource
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnDblClick = SongsGridDblClick
  end
  object NewButton: TButton
    Left = 112
    Top = 240
    Width = 105
    Height = 25
    Action = actNewSong
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object DeleteButton: TButton
    Left = 224
    Top = 240
    Width = 105
    Height = 25
    Action = actDeleteSong
    Anchors = [akLeft, akBottom]
    TabOrder = 3
  end
  object SongsDataSource: TDataSource
    Left = 568
    Top = 208
  end
  object ActionList: TActionList
    Left = 304
    Top = 144
    object actNewSong: TAction
      Caption = 'New Song'
    end
    object actDeleteSong: TAction
      Caption = 'Delete song'
      OnExecute = actDeleteSongExecute
    end
  end
end
