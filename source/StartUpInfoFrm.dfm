object FrmStartUpInfo: TFrmStartUpInfo
  Left = 184
  Top = 165
  BorderStyle = bsDialog
  Caption = 'Read me ...'
  ClientHeight = 453
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 412
    Align = alClient
    TabOrder = 0
    object mmoInfoText: TMemo
      Left = 1
      Top = 1
      Width = 686
      Height = 410
      Align = alClient
      Lines.Strings = (
        'mmoInfoText')
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 412
    Width = 688
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnOk: TBitBtn
      Left = 592
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
      TabOrder = 1
    end
    object btnHistory: TBitBtn
      Left = 472
      Top = 8
      Width = 75
      Height = 25
      Caption = 'History'
      TabOrder = 2
      OnClick = btnHistoryClick
    end
  end
end
