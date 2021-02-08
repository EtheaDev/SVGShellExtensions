object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Preview Handler Host Demo'
  ClientHeight = 480
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 432
    Height = 49
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 455
    DesignSize = (
      432
      49)
    object EditFileName: TEdit
      Left = 16
      Top = 16
      Width = 322
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 0
      ExplicitWidth = 345
    end
    object Button1: TButton
      Left = 344
      Top = 13
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Select File'
      TabOrder = 1
      OnClick = Button1Click
      ExplicitLeft = 367
    end
  end
  object RadioGroup: TRadioGroup
    Left = 8
    Top = 64
    Width = 137
    Height = 193
    Caption = 'Thumbnail size'
    Items.Strings = (
      'Small'
      'Medium'
      'Large'
      'Huge')
    TabOrder = 1
    OnClick = RadioGroupClick
  end
  object Panel1: TPanel
    Left = 224
    Top = 64
    Width = 185
    Height = 177
    TabOrder = 2
  end
  object OpenDialog1: TOpenDialog
    Left = 360
    Top = 424
  end
end
