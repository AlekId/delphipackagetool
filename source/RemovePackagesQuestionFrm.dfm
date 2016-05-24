object FrmRemovePackage: TFrmRemovePackage
  Left = 241
  Top = 323
  Width = 774
  Height = 195
  Caption = 'Remove Packages Dialog'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 127
    Width = 766
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      766
      41)
    object btnOk: TBitBtn
      Left = 541
      Top = 8
      Width = 85
      Height = 25
      Anchors = [akRight, akBottom]
      TabOrder = 0
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 664
      Top = 8
      Width = 85
      Height = 25
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 1
      Kind = bkCancel
    end
    object cbxDeleteBPLAndDCPFiles: TCheckBox
      Left = 32
      Top = 8
      Width = 249
      Height = 17
      Caption = 'Delete the .bpl and .dcp files.'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object pnlClient: TPanel
    Left = 0
    Top = 0
    Width = 766
    Height = 127
    Align = alClient
    TabOrder = 1
    object lblInfo: TLabel
      Left = 32
      Top = 104
      Width = 335
      Height = 13
      Caption = 
        '(Note: This deletes only packages which are referenced in  the r' +
        'egistry)'
    end
    object rbtAllThirdParty: TRadioButton
      Left = 32
      Top = 32
      Width = 513
      Height = 17
      Caption = 
        'Delete all Packages except the Borland Packages in ($DELPHI)\bin' +
        ' or ($BDS)\bin'
      TabOrder = 0
    end
    object rbtAllProjectsBPL: TRadioButton
      Left = 32
      Top = 72
      Width = 721
      Height = 17
      Caption = 'Delete all Packages located in ($DELPHI)\Projects\bpl'
      TabOrder = 1
    end
  end
end
