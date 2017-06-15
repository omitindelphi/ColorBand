object Form1: TForm1
  Left = 522
  Top = 159
  Width = 378
  Height = 600
  Caption = 'Form1'
  Color = clBtnFace
  Constraints.MaxHeight = 1200
  Constraints.MaxWidth = 1400
  Constraints.MinHeight = 100
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 362
    Height = 361
    Align = alClient
  end
  object Panel1: TPanel
    Left = 0
    Top = 361
    Width = 362
    Height = 200
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      362
      200)
    object Label1: TLabel
      Left = 168
      Top = 175
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object SpeedButton1: TSpeedButton
      Left = 8
      Top = 168
      Width = 23
      Height = 22
      OnClick = SpeedButton1Click
    end
    object Label2: TLabel
      Left = 112
      Top = 112
      Width = 31
      Height = 13
      Caption = 'Label2'
    end
    object BitBtn1: TBitBtn
      Left = 128
      Top = 133
      Width = 121
      Height = 41
      TabOrder = 0
      OnClick = BitBtn1Click
      Kind = bkOK
    end
    object TrackBar1: TTrackBar
      Left = 16
      Top = 16
      Width = 336
      Height = 45
      Anchors = [akLeft, akTop, akRight]
      Max = 200
      Frequency = 10
      Position = 36
      TabOrder = 1
      OnChange = TrackBar1Change
    end
    object TrackBar2: TTrackBar
      Left = 16
      Top = 56
      Width = 336
      Height = 45
      Anchors = [akLeft, akTop, akRight]
      Max = 200
      Min = -200
      Frequency = 10
      TabOrder = 2
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 110
      Width = 87
      Height = 17
      Caption = 'Neighborhood'
      TabOrder = 3
      OnClick = CheckBox1Click
    end
  end
end
