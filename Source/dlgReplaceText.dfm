inherited TextReplaceDialog: TTextReplaceDialog
  Caption = 'Replace text'
  ClientHeight = 206
  OldCreateOrder = True
  ExplicitHeight = 235
  PixelsPerInch = 96
  TextHeight = 13
  object ReplaceWidthLabel: TLabel [0]
    Left = 8
    Top = 41
    Width = 82
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Replace with:'
  end
  inherited cbSearchText: TComboBox
    TabOrder = 5
  end
  inherited FSearchOptions: TGroupBox
    Top = 70
    TabOrder = 2
    ExplicitTop = 70
  end
  inherited FSearchDirection: TRadioGroup
    Top = 70
    TabOrder = 3
    ExplicitTop = 70
  end
  inherited btnOK: TButton
    Top = 174
    TabOrder = 4
    ExplicitTop = 174
  end
  object cbReplaceText: TComboBox [6]
    Left = 96
    Top = 37
    Width = 228
    Height = 21
    TabOrder = 1
  end
  inherited btnCancel: TButton
    Top = 174
    TabOrder = 5
    ExplicitTop = 174
  end
end
