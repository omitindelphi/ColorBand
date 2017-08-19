object DisplayCellForm: TDisplayCellForm
  Left = 0
  Top = 0
  Caption = 'DisplayCellForm'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object KillerTimer: TTimer
    Enabled = False
    OnTimer = KillerTimerTimer
    Left = 320
    Top = 88
  end
end
