object FrmSelectFiles: TFrmSelectFiles
  Left = 192
  Top = 159
  Width = 1305
  Height = 612
  Caption = 'FrmSelectFiles'
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
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1297
    Height = 41
    Align = alTop
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 545
    Width = 1297
    Height = 40
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      1297
      40)
    object btnOk: TBitBtn
      Left = 1081
      Top = 8
      Width = 90
      Height = 26
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 1193
      Top = 8
      Width = 90
      Height = 26
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333000033338833333333333333333F333333333333
        0000333911833333983333333388F333333F3333000033391118333911833333
        38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
        911118111118333338F3338F833338F3000033333911111111833333338F3338
        3333F8330000333333911111183333333338F333333F83330000333333311111
        8333333333338F3333383333000033333339111183333333333338F333833333
        00003333339111118333333333333833338F3333000033333911181118333333
        33338333338F333300003333911183911183333333383338F338F33300003333
        9118333911183333338F33838F338F33000033333913333391113333338FF833
        38F338F300003333333333333919333333388333338FFF830000333333333333
        3333333333333333333888330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
  end
  object lbxFiles: TCheckListBox
    Left = 0
    Top = 41
    Width = 1297
    Height = 504
    Align = alClient
    ItemHeight = 13
    Sorted = True
    TabOrder = 2
  end
end
