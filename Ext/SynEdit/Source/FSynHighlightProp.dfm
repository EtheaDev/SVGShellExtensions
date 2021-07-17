object fmHighLightSettings: TfmHighLightSettings
  Left = 259
  Top = 148
  Caption = 'Highlighting properties'
  ClientHeight = 442
  ClientWidth = 531
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 480
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object TLabel
    Left = 4
    Top = 32
    Width = 3
    Height = 13
  end
  object pc: TPageControl
    Left = 0
    Top = 0
    Width = 531
    Height = 423
    ActivePage = tsColors
    Align = alClient
    TabOrder = 0
    object tsColors: TTabSheet
      Caption = 'Colors'
      ImageIndex = 1
      object Splitter1: TSplitter
        Left = 193
        Top = 0
        Width = 4
        Height = 362
        ExplicitLeft = 143
        ExplicitHeight = 366
      end
      object paLeft: TPanel
        Left = 0
        Top = 0
        Width = 193
        Height = 362
        Align = alLeft
        TabOrder = 0
        object paColor: TPanel
          Left = 1
          Top = 232
          Width = 191
          Height = 129
          Align = alBottom
          BevelOuter = bvLowered
          TabOrder = 1
          DesignSize = (
            191
            129)
          object lbColors: TLabel
            Left = 7
            Top = 4
            Width = 68
            Height = 13
            Caption = 'Element &Color:'
          end
          object ColorGrid: TColorGrid
            Left = 5
            Top = 19
            Width = 180
            Height = 108
            Hint = 
              'Left click to select foreground color. Right click to select bac' +
              'kground color'
            Anchors = [akLeft, akTop, akRight]
            ClickEnablesColor = True
            Ctl3D = True
            ForegroundEnabled = False
            BackgroundEnabled = False
            ParentCtl3D = False
            TabOrder = 0
            OnClick = ColorGridClick
          end
        end
        object paElements: TPanel
          Left = 1
          Top = 1
          Width = 191
          Height = 231
          Align = alClient
          BevelOuter = bvLowered
          TabOrder = 0
          object BoxElements: TListBox
            Left = 1
            Top = 17
            Width = 189
            Height = 213
            Align = alClient
            ItemHeight = 13
            TabOrder = 0
            OnClick = BoxElementsClick
          end
          object paElemTitle: TPanel
            Left = 1
            Top = 1
            Width = 189
            Height = 16
            Align = alTop
            Caption = 'Elements'
            TabOrder = 1
          end
        end
      end
      object paAttributesContainer: TPanel
        Left = 197
        Top = 0
        Width = 326
        Height = 362
        Align = alClient
        TabOrder = 1
        object paAttributes: TPanel
          Left = 1
          Top = 1
          Width = 324
          Height = 65
          Align = alTop
          BevelOuter = bvLowered
          TabOrder = 0
          object TLabel
            Left = 8
            Top = 4
            Width = 3
            Height = 13
          end
          object cbTextAttrib: TGroupBox
            Left = 5
            Top = 4
            Width = 150
            Height = 58
            Caption = 'Text Attributes'
            TabOrder = 0
            object cbBold: TCheckBox
              Left = 5
              Top = 16
              Width = 50
              Height = 17
              Caption = '&Bold'
              TabOrder = 0
              OnClick = cbFontStyleClick
            end
            object cbItalic: TCheckBox
              Left = 5
              Top = 36
              Width = 50
              Height = 17
              Caption = '&Italic'
              TabOrder = 1
              OnClick = cbFontStyleClick
            end
            object cbUnderline: TCheckBox
              Left = 62
              Top = 16
              Width = 72
              Height = 17
              Caption = '&Underline'
              TabOrder = 2
              OnClick = cbFontStyleClick
            end
            object cbStrikeOut: TCheckBox
              Left = 62
              Top = 36
              Width = 72
              Height = 17
              Caption = 'Stri&keOut'
              TabOrder = 3
              OnClick = cbFontStyleClick
            end
          end
          object gbWhiteSpace: TGroupBox
            Left = 160
            Top = 4
            Width = 150
            Height = 58
            Caption = 'Use WhiteSpace color for'
            TabOrder = 1
            object cbForeground: TCheckBox
              Left = 8
              Top = 16
              Width = 106
              Height = 17
              Caption = '&Foreground color'
              TabOrder = 0
              OnClick = cbForegroundClick
            end
            object cbBackground: TCheckBox
              Left = 8
              Top = 35
              Width = 106
              Height = 17
              Caption = '&Background color'
              TabOrder = 1
              OnClick = cbBackgroundClick
            end
          end
        end
        object SynEdit: TSynEdit
          Left = 1
          Top = 66
          Width = 324
          Height = 295
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 1
          OnClick = SynEditClick
          OnKeyUp = SynEditKeyUp
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Terminal'
          Gutter.Font.Style = []
          FontSmoothing = fsmNone
        end
      end
      object paBottom: TPanel
        Left = 0
        Top = 362
        Width = 523
        Height = 33
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object paButtons: TPanel
          Left = 362
          Top = 0
          Width = 161
          Height = 33
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object btCancel: TButton
            Left = 83
            Top = 4
            Width = 75
            Height = 25
            Cancel = True
            Caption = 'Cancel'
            ModalResult = 2
            TabOrder = 1
          end
          object btOK: TButton
            Left = 4
            Top = 4
            Width = 75
            Height = 25
            Caption = 'OK'
            Default = True
            ModalResult = 1
            TabOrder = 0
            OnClick = btOKClick
          end
        end
        object LoadButton: TButton
          Left = 2
          Top = 4
          Width = 95
          Height = 25
          Caption = 'Load from file...'
          TabOrder = 1
          OnClick = LoadButtonClick
        end
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 423
    Width = 531
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object OpenDialog: TOpenDialog
    Left = 256
    Top = 224
  end
end
