object FrmStartUpInfo: TFrmStartUpInfo
  Left = 184
  Top = 189
  Width = 1011
  Height = 480
  Caption = 'Read me ...'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1003
    Height = 412
    Align = alClient
    TabOrder = 0
    object mmoInfoText: TMemo
      Left = 1
      Top = 1
      Width = 1001
      Height = 410
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'mmoInfoText')
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 412
    Width = 1003
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnOk: TBitBtn
      Left = 896
      Top = 8
      Width = 85
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object cbxDontShow: TCheckBox
      Left = 8
      Top = 8
      Width = 137
      Height = 17
      Caption = 'Don'#39't show again'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
end
