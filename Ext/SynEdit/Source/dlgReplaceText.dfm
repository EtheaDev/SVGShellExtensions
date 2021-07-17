inherited TextReplaceDialog: TTextReplaceDialog
  Caption = 'Replace text'
  ClientHeight = 184
  OldCreateOrder = True
  ExplicitHeight = 213
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
  inherited SearchForLabel: TLabel
    Left = 38
    Width = 52
    ExplicitLeft = 38
    ExplicitWidth = 52
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
    Top = 155
    TabOrder = 4
    ExplicitTop = 155
  end
  inherited btnCancel: TButton
    Top = 155
    TabOrder = 5
    ExplicitTop = 155
  end
  object cbReplaceText: TComboBox
    Left = 96
    Top = 37
    Width = 228
    Height = 21
    TabOrder = 1
  end
end
