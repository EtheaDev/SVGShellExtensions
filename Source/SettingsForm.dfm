object UserSettingsForm: TUserSettingsForm
  Left = 259
  Top = 148
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 511
  ClientWidth = 815
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 600
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDefault
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object TLabel
    Left = 4
    Top = 32
    Width = 3
    Height = 15
  end
  object pc: TPageControl
    Left = 153
    Top = 41
    Width = 662
    Height = 451
    ActivePage = stGeneral
    Align = alClient
    Images = SettingsImageList
    TabOrder = 0
    object tsColors: TTabSheet
      Caption = 'Text color'
      ImageName = 'palette'
      object VertSplitter: TSplitter
        Left = 143
        Top = 0
        Width = 4
        Height = 377
        MinSize = 366
      end
      object paLeft: TPanel
        Left = 0
        Top = 25
        Width = 193
        Height = 377
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object paElements: TPanel
          Left = 0
          Top = 0
          Width = 193
          Height = 219
          Align = alClient
          BevelOuter = bvLowered
          TabOrder = 0
          object BoxElements: TListBox
            Left = 1
            Top = 17
            Width = 191
            Height = 201
            Align = alClient
            BevelOuter = bvNone
            ItemHeight = 15
            TabOrder = 0
            OnClick = BoxElementsClick
          end
          object paElemTitle: TPanel
            Left = 1
            Top = 1
            Width = 191
            Height = 16
            Align = alTop
            BevelOuter = bvNone
            Caption = 'Elements'
            TabOrder = 1
          end
        end
        object ElementColorGroupBox: TGroupBox
          Left = 0
          Top = 219
          Width = 193
          Height = 116
          Align = alBottom
          Caption = 'Element colors'
          TabOrder = 1
          DesignSize = (
            193
            116)
          object ForegroundColorLabel: TLabel
            Left = 9
            Top = 20
            Width = 94
            Height = 15
            Caption = 'Foreground Color'
          end
          object BackgroundColorLabel: TLabel
            Left = 9
            Top = 65
            Width = 96
            Height = 15
            Caption = 'Background Color'
          end
          object ForegroundColorBox: TColorBox
            Left = 9
            Top = 36
            Width = 174
            Height = 22
            NoneColorColor = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 20
            TabOrder = 0
            OnSelect = ColorBoxSelect
          end
          object BackgroundColorBox: TColorBox
            Left = 9
            Top = 81
            Width = 174
            Height = 22
            NoneColorColor = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 20
            TabOrder = 1
            OnSelect = ColorBoxSelect
          end
        end
        object ResetPanel: TPanel
          Left = 0
          Top = 335
          Width = 193
          Height = 42
          Align = alBottom
          TabOrder = 2
          DesignSize = (
            193
            42)
          object ResetButton: TStyledButton
            Left = 9
            Top = 6
            Width = 174
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Reset to default colors'
            TabOrder = 0
            OnClick = ResetButtonClick
          end
        end
      end
      object paAttributesContainer: TPanel
        Left = 197
        Top = 25
        Width = 457
        Height = 377
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object paAttributes: TPanel
          Left = 0
          Top = 0
          Width = 457
          Height = 65
          Align = alTop
          BevelOuter = bvLowered
          TabOrder = 0
          object TLabel
            Left = 8
            Top = 4
            Width = 3
            Height = 15
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
              Width = 59
              Height = 17
              Caption = '&Bold'
              TabOrder = 0
              OnClick = cbFontStyleClick
            end
            object cbItalic: TCheckBox
              Left = 5
              Top = 36
              Width = 59
              Height = 17
              Caption = '&Italic'
              TabOrder = 1
              OnClick = cbFontStyleClick
            end
            object cbUnderline: TCheckBox
              Left = 67
              Top = 16
              Width = 79
              Height = 17
              Caption = '&Underline'
              TabOrder = 2
              OnClick = cbFontStyleClick
            end
            object cbStrikeOut: TCheckBox
              Left = 67
              Top = 36
              Width = 79
              Height = 17
              Caption = 'Stri&keOut'
              TabOrder = 3
              OnClick = cbFontStyleClick
            end
          end
          object gbWhiteSpace: TGroupBox
            Left = 160
            Top = 4
            Width = 158
            Height = 58
            Caption = 'Use WhiteSpace color for'
            TabOrder = 1
            object cbForeground: TCheckBox
              Left = 8
              Top = 16
              Width = 128
              Height = 17
              Caption = '&Foreground color'
              TabOrder = 0
              OnClick = cbForegroundClick
            end
            object cbBackground: TCheckBox
              Left = 8
              Top = 36
              Width = 128
              Height = 17
              Caption = '&Background color'
              TabOrder = 1
              OnClick = cbBackgroundClick
            end
          end
        end
        object SynEdit: TSynEdit
          Left = 0
          Top = 65
          Width = 457
          Height = 289
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          TabOrder = 1
          OnClick = SynEditClick
          OnKeyUp = SynEditKeyUp
          CodeFolding.GutterShapeSize = 11
          CodeFolding.IndentGuidesColor = clGray
          CodeFolding.IndentGuides = True
          UseCodeFolding = False
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Consolas'
          Gutter.Font.Style = []
          Gutter.Font.Quality = fqClearTypeNatural
          Gutter.Width = 0
          Gutter.Bands = <>
          ScrollbarAnnotations = <>
          FontSmoothing = fsmNone
        end
        object ActiveLineColorGroupBox: TGroupBox
          Left = 0
          Top = 354
          Width = 453
          Height = 47
          Align = alBottom
          Caption = 'Active Line Color of Editor'
          TabOrder = 2
          object DarkActiveLineColorColorBox: TColorBox
            Left = 2
            Top = 17
            Width = 174
            Height = 22
            Hint = 'Active Line Color for Dark Theme'
            Align = alLeft
            NoneColorColor = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
            DropDownCount = 20
            TabOrder = 0
            OnSelect = ColorBoxSelect
          end
          object LightActiveLineColorColorBox: TColorBox
            Left = 176
            Top = 17
            Width = 174
            Height = 22
            Hint = 'Active Line Color for Light Theme'
            Align = alLeft
            NoneColorColor = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
            DropDownCount = 20
            TabOrder = 1
            OnSelect = ColorBoxSelect
          end
        end
      end
      object PanelTopEditor: TPanel
        Left = 0
        Top = 0
        Width = 654
        Height = 25
        Align = alTop
        Caption = 'EDITOR COLOR SETTINGS'
        TabOrder = 2
      end
    end
    object tsFont: TTabSheet
      Caption = 'Font'
      ImageIndex = 1
      ImageName = 'alphabetical-variant'
      object FontLabel: TLabel
        Left = 8
        Top = 33
        Width = 57
        Height = 15
        Caption = 'Font name'
      end
      object SizeLabel: TLabel
        Left = 8
        Top = 79
        Width = 20
        Height = 15
        Caption = 'Size'
      end
      object CbFont: TComboBox
        Left = 8
        Top = 50
        Width = 225
        Height = 22
        Style = csOwnerDrawFixed
        Sorted = True
        TabOrder = 0
        OnDrawItem = CbFontDrawItem
      end
      object EditFontSize: TEdit
        Left = 8
        Top = 96
        Width = 34
        Height = 23
        Alignment = taRightJustify
        NumbersOnly = True
        TabOrder = 1
        Text = '12'
      end
      object FontSizeUpDown: TUpDown
        Left = 42
        Top = 96
        Width = 16
        Height = 23
        Associate = EditFontSize
        Min = 8
        Max = 30
        Position = 12
        TabOrder = 2
      end
      object PanelTopFont: TPanel
        Left = 0
        Top = 0
        Width = 654
        Height = 25
        Align = alTop
        Caption = 'FONT SELECTION AND SIZE'
        TabOrder = 3
      end
    end
    object stTheme: TTabSheet
      Caption = 'Theme'
      ImageIndex = 2
      ImageName = 'theme-light-dark'
      object ThemeLeftPanel: TPanel
        Left = 0
        Top = 25
        Width = 185
        Height = 377
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object ThemesRadioGroup: TRadioGroup
          Left = 0
          Top = 0
          Width = 185
          Height = 118
          Align = alTop
          Caption = 'Theme'
          ItemIndex = 0
          Items.Strings = (
            'Same as Windows'
            'Force Dark'
            'Force Light')
          TabOrder = 0
          OnClick = ThemesRadioGroupClick
        end
        object SelectThemeRadioGroup: TRadioGroup
          Left = 0
          Top = 118
          Width = 185
          Height = 259
          Align = alClient
          Caption = 'Selected Theme'
          TabOrder = 1
          OnClick = SelectThemeRadioGroupClick
        end
      end
      object ThemeClientPanel: TPanel
        Left = 185
        Top = 25
        Width = 469
        Height = 377
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        StyleElements = []
      end
      object PanelTopTheme: TPanel
        Left = 0
        Top = 0
        Width = 654
        Height = 25
        Align = alTop
        Caption = 'THEME SELECTION'
        TabOrder = 2
      end
    end
    object stGeneral: TTabSheet
      Caption = 'Preview settings'
      ImageIndex = 3
      ImageName = 'arrow-left'
      object PanelTopPreviewSettings: TPanel
        Left = 0
        Top = 0
        Width = 654
        Height = 25
        Align = alTop
        Caption = 'RENDERING OPTIONS'
        TabOrder = 0
      end
      object EngineRadioGroup: TRadioGroup
        Left = 13
        Top = 37
        Width = 284
        Height = 102
        Caption = 'SVG rendering options'
        ItemIndex = 0
        Items.Strings = (
          'Delphi Image32'
          'Windows Direct 2D')
        TabOrder = 1
      end
      object RoundedButtonsGroupBox: TGroupBox
        AlignWithMargins = True
        Left = 13
        Top = 145
        Width = 284
        Height = 97
        Caption = 'Rounded Buttons'
        TabOrder = 2
        object ToolbarRoundedCheckBox: TCheckBox
          Left = 15
          Top = 24
          Width = 185
          Height = 17
          Caption = 'Apply to Toolbars'
          TabOrder = 0
        end
        object ButtonsRoundedCheckBox: TCheckBox
          Left = 15
          Top = 47
          Width = 185
          Height = 17
          Caption = 'Apply to Buttons'
          TabOrder = 1
        end
        object MenuRoundedCheckBox: TCheckBox
          Left = 15
          Top = 70
          Width = 185
          Height = 17
          Caption = 'Apply to Menu Buttons'
          TabOrder = 2
        end
      end
      object FixedColorGroupBox: TGroupBox
        AlignWithMargins = True
        Left = 13
        Top = 248
        Width = 284
        Height = 73
        Caption = 'Apply Fixed Color to SVG'
        TabOrder = 3
        object ApplyToRootCheckBox: TCheckBox
          Left = 15
          Top = 47
          Width = 185
          Height = 17
          Caption = 'Applied to Root Only'
          TabOrder = 0
        end
        object FixedColorColorBox: TColorBox
          Left = 15
          Top = 19
          Width = 145
          Height = 22
          TabOrder = 1
        end
      end
      object GrayScaleCheckBox: TCheckBox
        Left = 28
        Top = 327
        Width = 185
        Height = 17
        Caption = 'Apply GrayScale'
        TabOrder = 4
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 492
    Width = 815
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object MenuButtonGroup: TStyledButtonGroup
    Left = 0
    Top = 41
    Width = 153
    Height = 451
    Align = alLeft
    BevelOuter = bvNone
    BorderStyle = bsNone
    ButtonHeight = 48
    ButtonOptions = [gboFullSize, gboGroupStyle, gboShowCaptions]
    Images = SettingsImageList
    Items = <
      item
        Caption = '  Back'
        ImageIndex = 3
        ImageName = 'arrow-left'
      end
      item
        Caption = '  Text colors'
        ImageIndex = 0
        ImageName = 'palette'
      end
      item
        Caption = '  Font'
        ImageIndex = 1
        ImageName = 'alphabetical-variant'
      end
      item
        Caption = '  Theme'
        ImageIndex = 2
        ImageName = 'theme-light-dark'
      end
      item
        Caption = '  Preview'
        ImageIndex = 4
        ImageName = 'eye-settings'
      end>
    TabOrder = 2
    OnButtonClicked = MenuButtonGroupButtonClicked
  end
  object TitlePanel: TPanel
    Left = 0
    Top = 0
    Width = 815
    Height = 41
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Settings'
    TabOrder = 3
  end
  object OpenDialog: TOpenDialog
    Left = 584
    Top = 312
  end
  object SettingsImageList: TSVGIconImageList
    Size = 36
    SVGIconItems = <
      item
        IconName = 'palette'
        SVGText = 
          '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" id="mdi-pa' +
          'lette" width="24" height="24" viewBox="0 0 24 24">'#13#10'<path d="M17' +
          '.5,12A1.5,1.5 0 0,1 16,10.5A1.5,1.5 0 0,1 17.5,9A1.5,1.5 0 0,1 1' +
          '9,10.5A1.5,1.5 0 0,1 17.5,12M14.5,8A1.5,1.5 0 0,1 13,6.5A1.5,1.5' +
          ' 0 0,1 14.5,5A1.5,1.5 0 0,1 16,6.5A1.5,1.5 0 0,1 14.5,8M9.5,8A1.' +
          '5,1.5 0 0,1 8,6.5A1.5,1.5 0 0,1 9.5,5A1.5,1.5 0 0,1 11,6.5A1.5,1' +
          '.5 0 0,1 9.5,8M6.5,12A1.5,1.5 0 0,1 5,10.5A1.5,1.5 0 0,1 6.5,9A1' +
          '.5,1.5 0 0,1 8,10.5A1.5,1.5 0 0,1 6.5,12M12,3A9,9 0 0,0 3,12A9,9' +
          ' 0 0,0 12,21A1.5,1.5 0 0,0 13.5,19.5C13.5,19.11 13.35,18.76 13.1' +
          '1,18.5C12.88,18.23 12.73,17.88 12.73,17.5A1.5,1.5 0 0,1 14.23,16' +
          'H16A5,5 0 0,0 21,11C21,6.58 16.97,3 12,3Z" />'#13#10'</svg>'
      end
      item
        IconName = 'alphabetical-variant'
        SVGText = 
          '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" id="mdi-al' +
          'phabetical-variant" width="24" height="24" viewBox="0 0 24 24">'#13 +
          #10'<path d="M3 7A2 2 0 0 0 1 9V17H3V13H5V17H7V9A2 2 0 0 0 5 7H3M3 ' +
          '9H5V11H3M15 10.5V9A2 2 0 0 0 13 7H9V17H13A2 2 0 0 0 15 15V13.5A1' +
          '.54 1.54 0 0 0 13.5 12A1.54 1.54 0 0 0 15 10.5M13 15H11V13H13V15' +
          'M13 11H11V9H13M19 7A2 2 0 0 0 17 9V15A2 2 0 0 0 19 17H21A2 2 0 0' +
          ' 0 23 15V14H21V15H19V9H21V10H23V9A2 2 0 0 0 21 7Z" />'#13#10'</svg>'
      end
      item
        IconName = 'theme-light-dark'
        SVGText = 
          '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" id="mdi-th' +
          'eme-light-dark" width="24" height="24" viewBox="0 0 24 24">'#13#10'<pa' +
          'th d="M7.5,2C5.71,3.15 4.5,5.18 4.5,7.5C4.5,9.82 5.71,11.85 7.53' +
          ',13C4.46,13 2,10.54 2,7.5A5.5,5.5 0 0,1 7.5,2M19.07,3.5L20.5,4.9' +
          '3L4.93,20.5L3.5,19.07L19.07,3.5M12.89,5.93L11.41,5L9.97,6L10.39,' +
          '4.3L9,3.24L10.75,3.12L11.33,1.47L12,3.1L13.73,3.13L12.38,4.26L12' +
          '.89,5.93M9.59,9.54L8.43,8.81L7.31,9.59L7.65,8.27L6.56,7.44L7.92,' +
          '7.35L8.37,6.06L8.88,7.33L10.24,7.36L9.19,8.23L9.59,9.54M19,13.5A' +
          '5.5,5.5 0 0,1 13.5,19C12.28,19 11.15,18.6 10.24,17.93L17.93,10.2' +
          '4C18.6,11.15 19,12.28 19,13.5M14.6,20.08L17.37,18.93L17.13,22.28' +
          'L14.6,20.08M18.93,17.38L20.08,14.61L22.28,17.15L18.93,17.38M20.0' +
          '8,12.42L18.94,9.64L22.28,9.88L20.08,12.42M9.63,18.93L12.4,20.08L' +
          '9.87,22.27L9.63,18.93Z" />'#13#10'</svg>'
      end
      item
        IconName = 'arrow-left'
        SVGText = 
          '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" id="mdi-ar' +
          'row-left" width="24" height="24" viewBox="0 0 24 24">'#13#10'<path d="' +
          'M20,11V13H8L13.5,18.5L12.08,19.92L4.16,12L12.08,4.08L13.5,5.5L8,' +
          '11H20Z" />'#13#10'</svg>'
      end
      item
        IconName = 'eye-settings'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-eye-settings" w' +
          'idth="24" height="24" viewBox="0 0 24 24"><path d="M12,9A3,3 0 0' +
          ',0 9,12A3,3 0 0,0 12,15A3,3 0 0,0 15,12A3,3 0 0,0 12,9M12,17A5,5' +
          ' 0 0,1 7,12A5,5 0 0,1 12,7A5,5 0 0,1 17,12A5,5 0 0,1 12,17M12,4.' +
          '5C7.14,4.5 2.78,7.5 1,12C3.39,18.08 10.25,21.06 16.33,18.67C19.3' +
          '8,17.47 21.8,15.06 23,12C21.22,7.5 16.86,4.5 12,4.5M7,22H9V24H7V' +
          '22M11,22H13V24H11V22M15,22H17V24H15V22Z" /></svg>'
      end>
    Scaled = True
    Left = 500
    Top = 192
  end
end
