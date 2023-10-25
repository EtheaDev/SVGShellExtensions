inherited FrmPreview: TFrmPreview
  Left = 522
  Top = 286
  ClientHeight = 617
  ClientWidth = 617
  DoubleBuffered = True
  Font.Name = 'Segoe UI'
  StyleElements = [seFont, seClient, seBorder]
  OnResize = FormResize
  ExplicitWidth = 629
  ExplicitHeight = 655
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 329
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
    object ToolBar: TToolBar
      Left = 0
      Top = 0
      Width = 617
      Height = 35
      Align = alClient
      AutoSize = True
      ButtonHeight = 30
      ButtonWidth = 35
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = SVGIconImageList
      List = True
      TabOrder = 0
      object ToolButtonShowText: TToolButton
        Left = 0
        Top = 0
        Cursor = crHandPoint
        AutoSize = True
        Caption = 'Hide text'
        ImageIndex = 1
        ImageName = 'Hide-Text'
        Visible = False
        OnClick = ToolButtonShowTextClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonSettings: TToolButton
        Left = 35
        Top = 0
        Cursor = crHandPoint
        Hint = 'Preview settings...'
        AutoSize = True
        Caption = 'Settings...'
        ImageIndex = 11
        ImageName = 'preferences-desktop'
        Visible = False
        OnClick = ToolButtonSettingsClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonAbout: TToolButton
        Left = 70
        Top = 0
        Cursor = crHandPoint
        Hint = 'Show about...'
        AutoSize = True
        Caption = 'About...'
        ImageIndex = 2
        ImageName = 'about'
        Visible = False
        OnClick = ToolButtonAboutClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonReformat: TToolButton
        Left = 105
        Top = 0
        Cursor = crHandPoint
        Hint = 'Reformat XML text'
        AutoSize = True
        Caption = 'Format'
        ImageIndex = 10
        ImageName = 'Reformat'
        OnClick = ToolButtonReformatClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object SeparatorEditor: TToolButton
        Left = 140
        Top = 0
        Width = 8
        Caption = 'SeparatorEditor'
        ImageName = 'settings'
        Style = tbsSeparator
      end
      object ToolButtonZoomIn: TToolButton
        Left = 148
        Top = 0
        Cursor = crHandPoint
        Hint = 'Zoom in (increase font size)'
        AutoSize = True
        Caption = 'Zoom In'
        ImageIndex = 6
        ImageName = 'plus'
        Visible = False
        OnClick = ToolButtonZoomInClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonZoomOut: TToolButton
        Left = 183
        Top = 0
        Cursor = crHandPoint
        Hint = 'Zoom out (decrease font size)'
        AutoSize = True
        Caption = 'Zoom Out'
        ImageIndex = 7
        ImageName = 'minus'
        Visible = False
        OnClick = ToolButtonZoomOutClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
    end
  end
  object PanelEditor: TPanel
    Left = 0
    Top = 35
    Width = 617
    Height = 294
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object SynEdit: TSynEdit
      Left = 0
      Top = 0
      Width = 617
      Height = 294
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
      ' SVG Preview - Ver.%s (%dbit)- Copyright '#169' 2021-2023 Ethea S.r.l' +
      '. - Author: Carlo Barazzetta'
    UseSystemFont = False
  end
  object ImagePanel: TPanel
    Left = 0
    Top = 335
    Width = 617
    Height = 263
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 3
    StyleElements = []
    object SVGIconImage: TSVGIconImage
      Left = 0
      Top = 40
      Width = 617
      Height = 223
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
