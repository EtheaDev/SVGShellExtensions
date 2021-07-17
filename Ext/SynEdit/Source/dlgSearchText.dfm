object TextSearchDialog: TTextSearchDialog
  Left = 161
  Top = 217
  BorderStyle = bsDialog
  Caption = 'Search Text'
  ClientHeight = 154
  ClientWidth = 332
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object SearchForLabel: TLabel
    Left = 8
    Top = 12
    Width = 82
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Search for:'
  end
  object FSearchText: TComboBox
    Left = 96
    Top = 8
    Width = 228
    Height = 21
    TabOrder = 0
  end
  object FSearchOptions: TGroupBox
    Left = 8
    Top = 40
    Width = 154
    Height = 109
    Caption = 'Options'
    TabOrder = 1
    object FSearchCaseSensitive: TCheckBox
      Left = 8
      Top = 17
      Width = 140
      Height = 17
      Caption = 'C&ase sensitivity'
      TabOrder = 0
    end
    object FSearchWholeWords: TCheckBox
      Left = 8
      Top = 39
      Width = 140
      Height = 17
      Caption = '&Whole words only'
      TabOrder = 1
    end
    object FSearchFromCursor: TCheckBox
      Left = 8
      Top = 61
      Width = 140
      Height = 17
      Caption = 'Search from &caret'
      TabOrder = 2
    end
    object FSearchSelectedOnly: TCheckBox
      Left = 8
      Top = 83
      Width = 140
      Height = 17
      Caption = '&Selected text only'
      TabOrder = 3
    end
  end
  object FSearchDirection: TRadioGroup
    Left = 170
    Top = 40
    Width = 154
    Height = 65
    Caption = 'Direction'
    ItemIndex = 0
    Items.Strings = (
      '&Forward'
      '&Backward')
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 170
    Top = 125
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 249
    Top = 125
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
