object FrmVersion: TFrmVersion
  Left = 506
  Top = 245
  Width = 410
  Height = 177
  Caption = 'Set Version'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlClient: TPanel
    Left = 0
    Top = 0
    Width = 402
    Height = 109
    Align = alClient
    TabOrder = 0
    object lblMajor: TLabel
      Left = 32
      Top = 24
      Width = 26
      Height = 13
      Caption = 'Major'
    end
    object lblMinor: TLabel
      Left = 120
      Top = 24
      Width = 26
      Height = 13
      Caption = 'Minor'
    end
    object lblRelease: TLabel
      Left = 208
      Top = 24
      Width = 39
      Height = 13
      Caption = 'Release'
    end
    object lblBuild: TLabel
      Left = 288
      Top = 24
      Width = 23
      Height = 13
      Caption = 'Build'
    end
    object edtMajor: TSpinEdit
      Left = 32
      Top = 40
      Width = 73
      Height = 22
      MaxValue = 999
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object edtMinor: TSpinEdit
      Left = 120
      Top = 40
      Width = 73
      Height = 22
      MaxValue = 999
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object edtRelease: TSpinEdit
      Left = 208
      Top = 40
      Width = 73
      Height = 22
      MaxValue = 999
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
    object edtBuild: TSpinEdit
      Left = 288
      Top = 40
      Width = 73
      Height = 22
      MaxValue = 999
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
    object cbxVersionForAll: TCheckBox
      Left = 32
      Top = 80
      Width = 313
      Height = 17
      Caption = 'Don'#39't ask again'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 109
    Width = 402
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnOk: TBitBtn
      Left = 104
      Top = 8
      Width = 89
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 232
      Top = 8
      Width = 89
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
end
