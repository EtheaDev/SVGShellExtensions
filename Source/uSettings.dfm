object FrmSettings: TFrmSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Editor Settings'
  ClientHeight = 172
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Font'
  end
  object Label2: TLabel
    Left = 239
    Top = 8
    Width = 19
    Height = 13
    Caption = 'Size'
  end
  object CbFont: TComboBox
    Left = 8
    Top = 27
    Width = 225
    Height = 21
    Style = csDropDownList
    Sorted = True
    TabOrder = 0
  end
  object EditFontSize: TEdit
    Left = 239
    Top = 27
    Width = 34
    Height = 21
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 1
    Text = '10'
  end
  object UpDown1: TUpDown
    Left = 273
    Top = 27
    Width = 16
    Height = 21
    Associate = EditFontSize
    Min = 8
    Max = 30
    Position = 10
    TabOrder = 2
  end
  object ButtonSave: TButton
    Left = 212
    Top = 139
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 3
    OnClick = ButtonSaveClick
  end
  object Button1: TButton
    Left = 293
    Top = 139
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = Button1Click
  end
  object cbShowEditor: TCheckBox
    Left = 8
    Top = 56
    Width = 225
    Height = 17
    Caption = 'Show editor at startup'
    TabOrder = 5
  end
end
