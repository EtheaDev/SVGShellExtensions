inherited FrmPreview: TFrmPreview
  Left = 522
  Top = 286
  ClientHeight = 617
  ClientWidth = 617
  DoubleBuffered = True
  Font.Name = 'Segoe UI'
  StyleElements = [seFont, seClient, seBorder]
  OnResize = FormResize
  ExplicitWidth = 633
  ExplicitHeight = 656
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 362
    Width = 617
    Height = 6
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    MinSize = 100
    OnMoved = SplitterMoved
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 617
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object StyledToolbar: TStyledToolbar
      Left = 0
      Top = 0
      Width = 617
      Height = 35
      Align = alClient
      AutoSize = True
      ButtonHeight = 30
      ButtonWidth = 35
      Images = SVGIconImageList
      List = True
      TabOrder = 0
      object ToolButtonShowText: TStyledToolButton
        Left = 0
        Top = 0
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
        OnClick = ToolButtonShowTextClick
        Visible = False
        Caption = 'Hide text'
        ImageIndex = 1
        ImageName = 'Hide-Text'
        AutoSize = True
      end
      object ToolButtonSettings: TStyledToolButton
        Left = 35
        Top = 0
        Hint = 'Preview settings...'
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
        OnClick = ToolButtonSettingsClick
        Visible = False
        Caption = 'Settings...'
        ImageIndex = 11
        ImageName = 'preferences-desktop'
        AutoSize = True
      end
      object ToolButtonAbout: TStyledToolButton
        Left = 70
        Top = 0
        Hint = 'Show about...'
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
        OnClick = ToolButtonAboutClick
        Visible = False
        Caption = 'About...'
        ImageIndex = 2
        ImageName = 'about'
        AutoSize = True
      end
      object ToolButtonReformat: TStyledToolButton
        Left = 105
        Top = 0
        Hint = 'Reformat XML text'
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
        OnClick = ToolButtonReformatClick
        Caption = 'Format'
        ImageIndex = 10
        ImageName = 'Reformat'
        AutoSize = True
      end
      object SeparatorEditor: TStyledToolButton
        Left = 140
        Top = 0
        ImageName = 'settings'
        Style = tbsSeparator
      end
      object ToolButtonZoomIn: TStyledToolButton
        Left = 146
        Top = 0
        Hint = 'Zoom in (increase font size)'
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
        OnClick = ToolButtonZoomInClick
        Visible = False
        Caption = 'Zoom In'
        ImageIndex = 6
        ImageName = 'plus'
        AutoSize = True
      end
      object ToolButtonZoomOut: TStyledToolButton
        Left = 181
        Top = 0
        Hint = 'Zoom out (decrease font size)'
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
        OnClick = ToolButtonZoomOutClick
        Visible = False
        Caption = 'Zoom Out'
        ImageIndex = 7
        ImageName = 'minus'
        AutoSize = True
      end
    end
  end
  object PanelEditor: TPanel
    Left = 0
    Top = 35
    Width = 617
    Height = 206
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object SynEdit: TSynEdit
      Left = 0
      Top = 0
      Width = 617
      Height = 233
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Pitch = fpFixed
      Font.Style = []
      TabOrder = 0
      CodeFolding.GutterShapeSize = 11
      CodeFolding.CollapsedLineColor = clGrayText
      CodeFolding.FolderBarLinesColor = clGrayText
      CodeFolding.IndentGuidesColor = clGray
      CodeFolding.IndentGuides = True
      CodeFolding.ShowCollapsedLine = False
      CodeFolding.ShowHintMark = True
      UseCodeFolding = False
      BorderStyle = bsNone
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.ShowLineNumbers = True
      ReadOnly = True
      FontSmoothing = fsmNone
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 598
    Width = 617
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = 
      ' SVG Preview - Ver.%s (%dbit)- Copyright '#169' 2021-2024 Ethea S.r.l' +
      '. - Author: Carlo Barazzetta'
    UseSystemFont = False
  end
  object ImagePanel: TPanel
    Left = 0
    Top = 368
    Width = 617
    Height = 230
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 3
    StyleElements = []
    object SVGIconImage: TSVGIconImage
      Left = 0
      Top = 40
      Width = 617
      Height = 281
      AutoSize = False
      Align = alClient
    end
    object panelPreview: TPanel
      Left = 0
      Top = 0
      Width = 617
      Height = 40
      Align = alTop
      ParentBackground = False
      TabOrder = 0
      object BackgroundGrayScaleLabel: TLabel
        Left = 10
        Top = 6
        Width = 56
        Height = 29
        AutoSize = False
        Caption = 'Backlight %:'
        WordWrap = True
      end
      object BackgroundTrackBar: TTrackBar
        AlignWithMargins = True
        Left = 81
        Top = 4
        Width = 532
        Height = 32
        Cursor = crHandPoint
        Margins.Left = 80
        Align = alClient
        Max = 255
        Frequency = 10
        Position = 117
        TabOrder = 0
        TabStop = False
        OnChange = BackgroundTrackBarChange
      end
    end
  end
  object ColorPanel: TPanel
    Left = 0
    Top = 335
    Width = 617
    Height = 30
    Align = alTop
    ParentBackground = False
    TabOrder = 2
    object GrayscaleCheckBox: TCheckBox
      AlignWithMargins = True
      Left = 4
      Top = 3
      Width = 76
      Height = 26
      Margins.Top = 2
      Align = alLeft
      Caption = 'Grayscale'
      TabOrder = 0
      OnClick = GrayscaleCheckBoxClick
    end
    object ApplyToRootCheckBox: TCheckBox
      AlignWithMargins = True
      Left = 325
      Top = 3
      Width = 121
      Height = 26
      Margins.Top = 2
      Align = alLeft
      Caption = 'Apply to root only'
      TabOrder = 3
      OnClick = ApplyToRootCheckBoxClick
    end
    object FixedColorBox: TColorBox
      AlignWithMargins = True
      Left = 174
      Top = 6
      Width = 145
      Height = 22
      Align = alLeft
      DefaultColorColor = clDefault
      NoneColorColor = clNone
      Selected = clNone
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
      TabOrder = 2
      OnSelect = FixedColorBoxSelect
    end
    object FixedColorCheckBox: TCheckBox
      AlignWithMargins = True
      Left = 86
      Top = 3
      Width = 82
      Height = 26
      Margins.Top = 2
      Align = alLeft
      Caption = 'Fixed Color'
      TabOrder = 1
      OnClick = FixedColorCheckBoxClick
    end
  end
  object SVGIconImageList: TVirtualImageList
    Images = <
      item
        CollectionIndex = 42
        CollectionName = 'Show-Text'
        Name = 'Show-Text'
      end
      item
        CollectionIndex = 43
        CollectionName = 'Hide-Text'
        Name = 'Hide-Text'
      end
      item
        CollectionIndex = 23
        CollectionName = 'about'
        Name = 'about'
      end
      item
        CollectionIndex = 41
        CollectionName = 'Support'
        Name = 'Support'
      end
      item
        CollectionIndex = 0
        CollectionName = 'Style'
        Name = 'Style'
      end
      item
        CollectionIndex = 45
        CollectionName = 'Services'
        Name = 'Services'
      end
      item
        CollectionIndex = 26
        CollectionName = 'plus'
        Name = 'plus'
      end
      item
        CollectionIndex = 25
        CollectionName = 'Minus'
        Name = 'Minus'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Search'
        Name = 'Search'
      end
      item
        CollectionIndex = 38
        CollectionName = 'export'
        Name = 'export'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Reformat'
        Name = 'Reformat'
      end
      item
        CollectionIndex = 28
        CollectionName = 'preferences-desktop'
        Name = 'preferences-desktop'
      end>
    ImageCollection = dmResources.SVGIconImageCollection
    Width = 24
    Height = 24
    Left = 384
    Top = 208
  end
end
