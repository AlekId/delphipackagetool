object frmAbout: TfrmAbout
  Left = 200
  Top = 212
  BorderStyle = bsDialog
  Caption = 'frmBaseAbout'
  ClientHeight = 366
  ClientWidth = 598
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 598
    Height = 329
    Align = alClient
    TabOrder = 0
    DesignSize = (
      598
      329)
    object lblApplicationName: TLabel
      Left = 32
      Top = 16
      Width = 104
      Height = 13
      Caption = 'Application Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblVersion: TLabel
      Left = 32
      Top = 40
      Width = 47
      Height = 13
      Caption = 'Version:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblCompany: TLabel
      Left = 32
      Top = 88
      Width = 56
      Height = 13
      Caption = 'Company:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblVersionValue: TLabel
      Left = 140
      Top = 40
      Width = 3
      Height = 13
      Caption = '-'
    end
    object lblApplicationValue: TLabel
      Left = 140
      Top = 16
      Width = 3
      Height = 13
      Caption = '-'
    end
    object lblCompanyValue: TLabel
      Left = 140
      Top = 88
      Width = 3
      Height = 13
      Caption = '-'
    end
    object lblBuildDate: TLabel
      Left = 32
      Top = 64
      Width = 60
      Height = 13
      Caption = 'BuildDate:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblBuildValue: TLabel
      Left = 140
      Top = 64
      Width = 3
      Height = 13
      Caption = '-'
    end
    object Image1: TImage
      Left = 480
      Top = 16
      Width = 65
      Height = 57
      Anchors = [akTop, akRight]
      Picture.Data = {
        055449636F6E0000010001002020100000000000E80200001600000028000000
        2000000040000000010004000000000000020000000000000000000000000000
        0000000000000000000080000080000000808000800000008000800080800000
        80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
        FFFFFF0000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000FF0000000000000000000000000000FFFF000
        00000000000000000000000FFFFFFF00000000000000000000000FFFFFFFFF00
        0000000000000000000FFFFFFFFFFFF000000000000000000FFFFFFFFFFCCFF0
        000000000000000FFFFFFFFFFCCFFFFF0000000000000FFFFFFFFFFCCFFFCCFF
        0000000000000FFFFFFFFCCFFFCCFFFFF0000000000000FFFFFCCFFFCCFFFCCF
        F0000000000000FFFCCFFFCCFFFCCFFFFF0000000000F80FFFFFCCFFFCCFFFCC
        FF000000000FF80FFFCCFFFCCFFFCCFFFFF0000000FFFF80FFFFFCCFFFCCFFFC
        CFF0000000FFFF80FFFCCFFFCCFFFCCFFFFF0000000FFFF80FFFFFCCFFFCCFFF
        CCFF00000000FFF80FFFCCFFFFFFFFCCFFFFF00000080FFF80FFFFFF5FFFCCFF
        FCCFF00000FF80FF80FFFFFF5FFFFFFCCFFFFF0000FFF80FF80FFFFFF55FFCCF
        FFFFFF000FFFFF80F80FF5555F5FFFFFFFFFFFF00FFFFFF80F80FFFF55F5FFFF
        FFFFFFF0000FFFFF8080FFF5FF55FFFFFFFFF00000000FFFF8080FFFFF8888FF
        FFF000000000000FFF800FFFF0000888F0000000000000000FF800FF08888000
        0800000000000000000F8000FFFFFF078000000000000000000000FFFFFFFFF0
        0000000000000000000000000000000000000000000000000000000000000000
        00000000FFFFFFFFFFFFE7FFFFFF83FFFFFE03FFFFF801FFFFE001FFFF8000FF
        FE0000FFF800007FF000007FF000003FF800003FF000001FE000001FC000000F
        8000000F80000007C0000007C0000003C0000003800000018000000100000000
        0000000080000001E0000007F800000FFE00001FFF80003FFFE0007FFFF800FF
        FFFFFFFF}
    end
    object lblHomepage: TLabel
      Left = 32
      Top = 112
      Width = 65
      Height = 13
      Caption = 'Homepage:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblHomepageValue: TLabel
      Left = 140
      Top = 112
      Width = 5
      Height = 13
      Cursor = crHandPoint
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      OnClick = lblHomepageValueClick
    end
    object lblFileLocation: TLabel
      Left = 32
      Top = 136
      Width = 27
      Height = 13
      Caption = 'App:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblAppLocationValue: TLabel
      Left = 140
      Top = 136
      Width = 3
      Height = 13
      Caption = '-'
    end
    object mmoCredits: TMemo
      Left = 8
      Top = 168
      Width = 579
      Height = 154
      Cursor = crArrow
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        'The source code is available at sourceforge.net.'
        ''
        'Thanks to:'
        'It contains some portions of JCL code.')
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 329
    Width = 598
    Height = 37
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      598
      37)
    object btnClose: TBitBtn
      Left = 260
      Top = 6
      Width = 75
      Height = 25
      Anchors = []
      TabOrder = 0
      Kind = bkOK
    end
  end
end
