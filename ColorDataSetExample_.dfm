object Form1: TForm1
  Left = 923
  Top = 419
  Width = 587
  Height = 503
  Caption = 'Form1'
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 580
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 571
    Height = 324
    Align = alClient
    DataSource = DataSource1
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnDrawColumnCell = DBGrid1DrawColumnCell
  end
  object Panel1: TPanel
    Left = 0
    Top = 324
    Width = 571
    Height = 140
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      571
      140)
    object Memo1: TMemo
      Left = 1
      Top = 81
      Width = 569
      Height = 58
      Align = alClient
      Lines.Strings = (
        'Display of multi-dimension conditions reflected in DBGrid.'
        'Each color represents independent parameter.'
        'Observe color mix on row change')
      TabOrder = 0
    end
    object TrackBar1: TTrackBar
      Left = 1
      Top = 1
      Width = 569
      Height = 80
      Align = alTop
      Max = 50
      Min = 2
      Position = 20
      TabOrder = 1
      OnChange = TrackBar1Change
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 32
      Width = 553
      Height = 43
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Filter by color'
      TabOrder = 2
      object cbRed: TCheckBox
        Left = 32
        Top = 21
        Width = 97
        Height = 17
        Caption = 'Red'
        TabOrder = 0
        OnClick = cbClick
      end
      object cbGreen: TCheckBox
        Left = 121
        Top = 21
        Width = 97
        Height = 17
        Caption = 'Green'
        TabOrder = 1
        OnClick = cbClick
      end
      object cbYellow: TCheckBox
        Left = 219
        Top = 21
        Width = 97
        Height = 17
        Caption = 'Yellow'
        Color = clBtnFace
        ParentColor = False
        TabOrder = 2
        OnClick = cbClick
      end
    end
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    FetchOnDemand = False
    Params = <>
    OnFilterRecord = ClientDataSet1FilterRecord
    Left = 104
    Top = 48
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 152
    Top = 48
  end
end
