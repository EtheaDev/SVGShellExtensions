object FrmEditor: TFrmEditor
  Left = 522
  Top = 286
  ClientHeight = 543
  ClientWidth = 888
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 329
    Width = 888
    Height = 8
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    MinSize = 100
    OnMoved = SplitterMoved
    ExplicitLeft = 2
    ExplicitTop = 256
    ExplicitWidth = 886
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 888
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object ToolBar: TToolBar
      Left = 0
      Top = 0
      Width = 888
      Height = 35
      Align = alClient
      AutoSize = True
      ButtonHeight = 30
      ButtonWidth = 87
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = SVGIconImageList
      List = True
      ShowCaptions = True
      TabOrder = 0
      object ToolButtonShowText: TToolButton
        Left = 0
        Top = 0
        Cursor = crHandPoint
        AutoSize = True
        Caption = 'Hide text'
        ImageIndex = 10
        ImageName = 'hide-text'
        Visible = False
        OnClick = ToolButtonShowTextClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonAbout: TToolButton
        Left = 85
        Top = 0
        Cursor = crHandPoint
        Hint = 'Show about...'
        AutoSize = True
        Caption = 'About...'
        ImageIndex = 4
        ImageName = 'about'
        Visible = False
        OnClick = ToolButtonAboutClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object SeparatorEditor: TToolButton
        Left = 165
        Top = 0
        Width = 8
        Caption = 'SeparatorEditor'
        ImageIndex = 3
        ImageName = 'settings'
        Style = tbsSeparator
      end
      object ToolButtonFont: TToolButton
        Left = 173
        Top = 0
        Cursor = crHandPoint
        Hint = 'Preview settings...'
        AutoSize = True
        Caption = 'Settings...'
        ImageIndex = 8
        ImageName = 'support'
        Visible = False
        OnClick = ToolButtonFontClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonReformat: TToolButton
        Left = 263
        Top = 0
        Hint = 'Reformat XML text'
        AutoSize = True
        Caption = 'Format'
        ImageIndex = 11
        ImageName = 'services'
        OnClick = ToolButtonReformatClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonZoomIn: TToolButton
        Left = 338
        Top = 0
        Cursor = crHandPoint
        Hint = 'Zoom in (increase font size)'
        AutoSize = True
        Caption = 'Zoom In'
        ImageIndex = 12
        ImageName = 'plus'
        Visible = False
        OnClick = ToolButtonZoomInClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonZommOut: TToolButton
        Left = 419
        Top = 0
        Cursor = crHandPoint
        Hint = 'Zoom out (decrease font size)'
        AutoSize = True
        Caption = 'Zoom Out'
        ImageIndex = 13
        ImageName = 'minus'
        Visible = False
        OnClick = ToolButtonZommOutClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonSearch: TToolButton
        Left = 510
        Top = 0
        Cursor = crHandPoint
        Hint = 'Search into text...'
        AutoSize = True
        Caption = 'Search...'
        ImageIndex = 2
        ImageName = 'fine_print'
        Visible = False
        OnClick = ToolButtonSearchClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
    end
  end
  object PanelEditor: TPanel
    Left = 0
    Top = 35
    Width = 888
    Height = 294
    Align = alTop
    BevelOuter = bvLowered
    Caption = 'PanelEditor'
    TabOrder = 1
    object SynEdit: TSynEdit
      Left = 1
      Top = 1
      Width = 886
      Height = 292
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
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.ShowLineNumbers = True
      Highlighter = SynXMLSyn
      ReadOnly = True
      FontSmoothing = fsmNone
      ExplicitLeft = 0
      ExplicitTop = -3
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 524
    Width = 888
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = 
      ' SVG Preview - Copyright '#169' 2021 - Ethea S.r.l. - Author: Carlo B' +
      'arazzetta'
    SizeGrip = False
    UseSystemFont = False
  end
  object ImagePanel: TPanel
    Left = 0
    Top = 337
    Width = 888
    Height = 187
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 3
    ExplicitLeft = 1
    ExplicitTop = 264
    ExplicitWidth = 886
    ExplicitHeight = 224
    object SVGIconImage: TSVGIconImage
      Left = 1
      Top = 1
      Width = 886
      Height = 185
      AutoSize = False
      Proportional = True
      Align = alClient
      ExplicitWidth = 884
      ExplicitHeight = 222
    end
  end
  object SynXMLSyn: TSynXMLSyn
    DefaultFilter = 'XML Files (*.svg)|*.svg'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    WantBracesParsed = False
    Left = 88
    Top = 96
  end
  object SynEditSearch: TSynEditSearch
    Left = 672
    Top = 312
  end
  object SynEditRegexSearch: TSynEditRegexSearch
    Left = 664
    Top = 376
  end
  object SVGIconImageList: TSVGIconImageList
    Size = 24
    SVGIconItems = <
      item
        IconName = 'add_image'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#8CB' +
          'CD6" d="M40,41H8c-2.2,0-4-1.8-4-4V11c0-2.2,1.8-4,4-4h32c2.2,0,4,' +
          '1.8,4,4v26C44,39.2,42.2,41,40,41z"/>'#13#10'    <circle fill="#B3DDF5"' +
          ' cx="35" cy="16" r="3"/>'#13#10'    <polygon fill="#9AC9E3" points="20' +
          ',16 9,32 31,32"/>'#13#10'    <polygon fill="#B3DDF5" points="31,22 23,' +
          '32 39,32"/>'#13#10'    <circle fill="#43A047" cx="38" cy="38" r="10"/>' +
          #13#10'    <g fill="#fff">'#13#10'        <rect x="36" y="32" width="4" hei' +
          'ght="12"/>'#13#10'        <rect x="32" y="36" width="12" height="4"/>'#13 +
          #10'    </g>'#13#10'</svg>'#13#10
      end
      item
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#8CB' +
          'CD6" d="M40,41H8c-2.2,0-4-1.8-4-4V11c0-2.2,1.8-4,4-4h32c2.2,0,4,' +
          '1.8,4,4v26C44,39.2,42.2,41,40,41z"/>'#13#10'    <circle fill="#B3DDF5"' +
          ' cx="35" cy="16" r="3"/>'#13#10'    <polygon fill="#9AC9E3" points="20' +
          ',16 9,32 31,32"/>'#13#10'    <polygon fill="#B3DDF5" points="31,22 23,' +
          '32 39,32"/>'#13#10'    <circle fill="#F44336" cx="38" cy="38" r="10"/>' +
          #13#10'    <rect x="32" y="36" fill="#fff" width="12" height="4"/>'#13#10'<' +
          '/svg>'#13#10
      end
      item
        IconName = 'fine_print'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '90CAF9" points="33,42 5,42 5,4 24,4 33,13"/>'#13#10'    <polygon fill=' +
          '"#E1F5FE" points="31.5,14 23,14 23,5.5"/>'#13#10'    <rect x="38.3" y=' +
          '"34.8" transform="matrix(.707 -.707 .707 .707 -17.177 40.055)" f' +
          'ill="#616161" width="2.8" height="12"/>'#13#10'    <circle fill="#6161' +
          '61" cx="28" cy="29" r="11"/>'#13#10'    <circle fill="#90CAF9" cx="28"' +
          ' cy="29" r="9"/>'#13#10'    <rect x="39.5" y="37.6" transform="matrix(' +
          '.707 -.707 .707 .707 -17.661 41.223)" fill="#37474F" width="2.8"' +
          ' height="8.7"/>'#13#10'    <g fill="#1976D2">'#13#10'        <path d="M30,31' +
          'h-9.7c0.4,1.6,1.3,3,2.5,4H30V31z"/>'#13#10'        <path d="M20.3,27H3' +
          '0v-4h-7.3C21.5,24,20.7,25.4,20.3,27z"/>'#13#10'        <path d="M20.1,' +
          '20H11v2h7.3C18.8,21.3,19.4,20.6,20.1,20z"/>'#13#10'        <path d="M1' +
          '7.1,24H11v2h5.4C16.6,25.3,16.8,24.6,17.1,24z"/>'#13#10'        <path d' +
          '="M16,29c0-0.3,0-0.7,0.1-1H11v2h5.1C16,29.7,16,29.3,16,29z"/>'#13#10' ' +
          '       <path d="M16.4,32H11v2h6.1C16.8,33.4,16.6,32.7,16.4,32z"/' +
          '>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'settings'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#607' +
          'D8B" d="M39.6,27.2c0.1-0.7,0.2-1.4,0.2-2.2s-0.1-1.5-0.2-2.2l4.5-' +
          '3.2c0.4-0.3,0.6-0.9,0.3-1.4L40,10.8 c-0.3-0.5-0.8-0.7-1.3-0.4l-5' +
          ',2.3c-1.2-0.9-2.4-1.6-3.8-2.2l-0.5-5.5c-0.1-0.5-0.5-0.9-1-0.9h-8' +
          '.6c-0.5,0-1,0.4-1,0.9l-0.5,5.5 c-1.4,0.6-2.7,1.3-3.8,2.2l-5-2.3c' +
          '-0.5-0.2-1.1,0-1.3,0.4l-4.3,7.4c-0.3,0.5-0.1,1.1,0.3,1.4l4.5,3.2' +
          'c-0.1,0.7-0.2,1.4-0.2,2.2 s0.1,1.5,0.2,2.2L4,30.4c-0.4,0.3-0.6,0' +
          '.9-0.3,1.4L8,39.2c0.3,0.5,0.8,0.7,1.3,0.4l5-2.3c1.2,0.9,2.4,1.6,' +
          '3.8,2.2l0.5,5.5 c0.1,0.5,0.5,0.9,1,0.9h8.6c0.5,0,1-0.4,1-0.9l0.5' +
          '-5.5c1.4-0.6,2.7-1.3,3.8-2.2l5,2.3c0.5,0.2,1.1,0,1.3-0.4l4.3-7.4' +
          ' c0.3-0.5,0.1-1.1-0.3-1.4L39.6,27.2z M24,35c-5.5,0-10-4.5-10-10c' +
          '0-5.5,4.5-10,10-10c5.5,0,10,4.5,10,10C34,30.5,29.5,35,24,35z"/>'#13 +
          #10'    <path fill="#455A64" d="M24,13c-6.6,0-12,5.4-12,12c0,6.6,5.' +
          '4,12,12,12s12-5.4,12-12C36,18.4,30.6,13,24,13z M24,30 c-2.8,0-5-' +
          '2.2-5-5c0-2.8,2.2-5,5-5s5,2.2,5,5C29,27.8,26.8,30,24,30z"/>'#13#10'</s' +
          'vg>'#13#10
      end
      item
        IconName = 'about'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#219' +
          '6F3" d="M37,40H11l-6,6V12c0-3.3,2.7-6,6-6h26c3.3,0,6,2.7,6,6v22C' +
          '43,37.3,40.3,40,37,40z"/>'#13#10'    <g fill="#fff">'#13#10'        <rect x=' +
          '"22" y="20" width="4" height="11"/>'#13#10'        <circle cx="24" cy=' +
          '"15" r="2"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'edit_image'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#8CB' +
          'CD6" d="M31,41H8c-2.2,0-4-1.8-4-4V11c0-2.2,1.8-4,4-4h32c2.2,0,4,' +
          '1.8,4,4v17C44,35.2,38.2,41,31,41z"/>'#13#10'    <circle fill="#B3DDF5"' +
          ' cx="35" cy="16" r="3"/>'#13#10'    <polygon fill="#9AC9E3" points="20' +
          ',16 9,32 31,32"/>'#13#10'    <polygon fill="#B3DDF5" points="31,22 23,' +
          '32 39,32"/>'#13#10'    <path fill="#E57373" d="M47.7,29.1l-2.8-2.8c-0.' +
          '4-0.4-1.1-0.4-1.6,0L42,27.6l4.4,4.4l1.3-1.3C48.1,30.3,48.1,29.6,' +
          '47.7,29.1z"/>'#13#10'    <rect x="27.1" y="35.1" transform="matrix(.70' +
          '7 -.707 .707 .707 -16.508 36.511)" fill="#FF9800" width="17.4" h' +
          'eight="6.2"/>'#13#10'    <rect x="41.5" y="27.8" transform="matrix(-.7' +
          '07 .707 -.707 -.707 95.395 22.352)" fill="#B0BEC5" width="3.1" h' +
          'eight="6.2"/>'#13#10'    <polygon fill="#FFC107" points="27.5,42.2 26,' +
          '48 31.8,46.5"/>'#13#10'    <polygon fill="#37474F" points="26.7,45 26,' +
          '48 29,47.3"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'document-hide'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '90CAF9" points="40,45 8,45 8,3 30,3 40,13"/>'#13#10'    <polygon fill=' +
          '"#E1F5FE" points="38.5,14 29,14 29,4.5"/>'#13#10'    <g fill="#1976D2"' +
          '>'#13#10'        <rect x="16" y="21" width="17" height="2"/>'#13#10'        ' +
          '<rect x="16" y="25" width="13" height="2"/>'#13#10'        <rect x="16' +
          '" y="29" width="17" height="2"/>'#13#10'        <rect x="16" y="33" wi' +
          'dth="13" height="2"/>'#13#10'    </g>'#13#10'    <circle fill="#F44336" cx="' +
          '38" cy="38" r="10"/>'#13#10'    <g fill="#fff">'#13#10'        <rect x="36.5' +
          '" y="32" transform="matrix(-.707 .707 -.707 -.707 91.74 38)" wid' +
          'th="3" height="12"/>'#13#10'        <rect x="36.5" y="32" transform="m' +
          'atrix(-.707 -.707 .707 -.707 38 91.74)" width="3" height="12"/>'#13 +
          #10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'sms'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#009' +
          '688" d="M37,39H11l-6,6V11c0-3.3,2.7-6,6-6h26c3.3,0,6,2.7,6,6v22C' +
          '43,36.3,40.3,39,37,39z"/>'#13#10'    <g fill="#fff">'#13#10'        <circle ' +
          'cx="24" cy="22" r="3"/>'#13#10'        <circle cx="34" cy="22" r="3"/>' +
          #13#10'        <circle cx="14" cy="22" r="3"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'support'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#607' +
          'D8B" d="M44.7,11L36,19.6c0,0-2.6,0-5.2-2.6s-2.6-5.2-2.6-5.2l8.7-' +
          '8.7c-4.9-1.2-10.8,0.4-14.4,4 c-5.4,5.4-0.6,12.3-2,13.7C12.9,28.7' +
          ',5.1,34.7,4.9,35c-2.3,2.3-2.4,6-0.2,8.2c2.2,2.2,5.9,2.1,8.2-0.2c' +
          '0.3-0.3,6.7-8.4,14.2-15.9 c1.4-1.4,8,3.7,13.6-1.8C44.2,21.7,45.9' +
          ',15.9,44.7,11z M9.4,41.1c-1.4,0-2.5-1.1-2.5-2.5C6.9,37.1,8,36,9.' +
          '4,36 c1.4,0,2.5,1.1,2.5,2.5C11.9,39.9,10.8,41.1,9.4,41.1z"/>'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'show-text'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '90CAF9" points="40,45 8,45 8,3 30,3 40,13"/>'#13#10'    <polygon fill=' +
          '"#E1F5FE" points="38.5,14 29,14 29,4.5"/>'#13#10'    <g fill="#1976D2"' +
          '>'#13#10'        <rect x="16" y="21" width="17" height="2"/>'#13#10'        ' +
          '<rect x="16" y="25" width="13" height="2"/>'#13#10'        <rect x="16' +
          '" y="29" width="17" height="2"/>'#13#10'        <rect x="16" y="33" wi' +
          'dth="13" height="2"/>'#13#10'    </g>'#13#10'    <circle fill="#43A047" cx="' +
          '38" cy="38" r="10"/>'#13#10'    <g fill="#fff">'#13#10'        <rect x="36" ' +
          'y="32" width="4" height="12"/>'#13#10'        <rect x="32" y="36" widt' +
          'h="12" height="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'hide-text'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '90CAF9" points="40,45 8,45 8,3 30,3 40,13"/>'#13#10'    <polygon fill=' +
          '"#E1F5FE" points="38.5,14 29,14 29,4.5"/>'#13#10'    <g fill="#1976D2"' +
          '>'#13#10'        <rect x="16" y="21" width="17" height="2"/>'#13#10'        ' +
          '<rect x="16" y="25" width="13" height="2"/>'#13#10'        <rect x="16' +
          '" y="29" width="17" height="2"/>'#13#10'        <rect x="16" y="33" wi' +
          'dth="13" height="2"/>'#13#10'    </g>'#13#10'    <circle fill="#F44336" cx="' +
          '38" cy="38" r="10"/>'#13#10'    <rect x="32" y="36" fill="#fff" width=' +
          '"12" height="4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'services'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#E65' +
          '100" d="M25.6,34.4c0.1-0.4,0.1-0.9,0.1-1.4s0-0.9-0.1-1.4l2.8-2c0' +
          '.3-0.2,0.4-0.6,0.2-0.9l-2.7-4.6 c-0.2-0.3-0.5-0.4-0.8-0.3L22,25.' +
          '3c-0.7-0.6-1.5-1-2.4-1.4l-0.3-3.4c0-0.3-0.3-0.6-0.6-0.6h-5.3c-0.' +
          '3,0-0.6,0.3-0.6,0.6L12.4,24 c-0.9,0.3-1.6,0.8-2.4,1.4l-3.1-1.4c-' +
          '0.3-0.1-0.7,0-0.8,0.3l-2.7,4.6c-0.2,0.3-0.1,0.7,0.2,0.9l2.8,2c-0' +
          '.1,0.4-0.1,0.9-0.1,1.4 s0,0.9,0.1,1.4l-2.8,2c-0.3,0.2-0.4,0.6-0.' +
          '2,0.9l2.7,4.6c0.2,0.3,0.5,0.4,0.8,0.3l3.1-1.4c0.7,0.6,1.5,1,2.4,' +
          '1.4l0.3,3.4 c0,0.3,0.3,0.6,0.6,0.6h5.3c0.3,0,0.6-0.3,0.6-0.6l0.3' +
          '-3.4c0.9-0.3,1.6-0.8,2.4-1.4l3.1,1.4c0.3,0.1,0.7,0,0.8-0.3l2.7-4' +
          '.6 c0.2-0.3,0.1-0.7-0.2-0.9L25.6,34.4z M16,38c-2.8,0-5-2.2-5-5c0' +
          '-2.8,2.2-5,5-5c2.8,0,5,2.2,5,5C21,35.8,18.8,38,16,38z"/>'#13#10'    <p' +
          'ath fill="#FFA000" d="M41.9,15.3C42,14.8,42,14.4,42,14s0-0.8-0.1' +
          '-1.3l2.5-1.8c0.3-0.2,0.3-0.5,0.2-0.8l-2.5-4.3 c-0.2-0.3-0.5-0.4-' +
          '0.8-0.2l-2.9,1.3c-0.7-0.5-1.4-0.9-2.2-1.3l-0.3-3.1C36,2.2,35.8,2' +
          ',35.5,2h-4.9c-0.3,0-0.6,0.2-0.6,0.5l-0.3,3.1 c-0.8,0.3-1.5,0.7-2' +
          '.2,1.3l-2.9-1.3c-0.3-0.1-0.6,0-0.8,0.2l-2.5,4.3c-0.2,0.3-0.1,0.6' +
          ',0.2,0.8l2.5,1.8C24,13.2,24,13.6,24,14 s0,0.8,0.1,1.3l-2.5,1.8c-' +
          '0.3,0.2-0.3,0.5-0.2,0.8l2.5,4.3c0.2,0.3,0.5,0.4,0.8,0.2l2.9-1.3c' +
          '0.7,0.5,1.4,0.9,2.2,1.3l0.3,3.1 c0,0.3,0.3,0.5,0.6,0.5h4.9c0.3,0' +
          ',0.6-0.2,0.6-0.5l0.3-3.1c0.8-0.3,1.5-0.7,2.2-1.3l2.9,1.3c0.3,0.1' +
          ',0.6,0,0.8-0.2l2.5-4.3 c0.2-0.3,0.1-0.6-0.2-0.8L41.9,15.3z M33,1' +
          '9c-2.8,0-5-2.2-5-5c0-2.8,2.2-5,5-5c2.8,0,5,2.2,5,5C38,16.8,35.8,' +
          '19,33,19z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'plus'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#4' +
          'CAF50" cx="24" cy="24" r="21"/>'#13#10'    <g fill="#fff">'#13#10'        <r' +
          'ect x="21" y="14" width="6" height="20"/>'#13#10'        <rect x="14" ' +
          'y="21" width="20" height="6"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'minus'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#F' +
          '44336" cx="24" cy="24" r="21"/>'#13#10'    <g fill="#fff">'#13#10'        <r' +
          'ect x="14" y="21" width="20" height="6"/>'#13#10'    </g>'#13#10'</svg>'
      end
      item
        IconName = 'idea'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#F' +
          'FF59D" cx="24" cy="22" r="20"/>'#13#10'    <path fill="#FBC02D" d="M37' +
          ',22c0-7.7-6.6-13.8-14.5-12.9c-6,0.7-10.8,5.5-11.4,11.5c-0.5,4.6,' +
          '1.4,8.7,4.6,11.3 c1.4,1.2,2.3,2.9,2.3,4.8V37h12v-0.1c0-1.8,0.8-3' +
          '.6,2.2-4.8C35.1,29.7,37,26.1,37,22z"/>'#13#10'    <path fill="#FFF59D"' +
          ' d="M30.6,20.2l-3-2c-0.3-0.2-0.8-0.2-1.1,0L24,19.8l-2.4-1.6c-0.3' +
          '-0.2-0.8-0.2-1.1,0l-3,2 c-0.2,0.2-0.4,0.4-0.4,0.7s0,0.6,0.2,0.8l' +
          '3.8,4.7V37h2V26c0-0.2-0.1-0.4-0.2-0.6l-3.3-4.1l1.5-1l2.4,1.6c0.3' +
          ',0.2,0.8,0.2,1.1,0 l2.4-1.6l1.5,1l-3.3,4.1C25.1,25.6,25,25.8,25,' +
          '26v11h2V26.4l3.8-4.7c0.2-0.2,0.3-0.5,0.2-0.8S30.8,20.3,30.6,20.2' +
          'z"/>'#13#10'    <circle fill="#5C6BC0" cx="24" cy="44" r="3"/>'#13#10'    <p' +
          'ath fill="#9FA8DA" d="M26,45h-4c-2.2,0-4-1.8-4-4v-5h12v5C30,43.2' +
          ',28.2,45,26,45z"/>'#13#10'    <g fill="#5C6BC0">'#13#10'        <path d="M30' +
          ',41l-11.6,1.6c0.3,0.7,0.9,1.4,1.6,1.8l9.4-1.3C29.8,42.5,30,41.8,' +
          '30,41z"/>'#13#10'        <polygon points="18,38.7 18,40.7 30,39 30,37"' +
          '/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end>
    Scaled = True
    Left = 336
    Top = 211
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MinFontSize = 8
    MaxFontSize = 30
    Options = [fdTrueTypeOnly, fdEffects, fdFixedPitchOnly, fdScalableOnly]
    Left = 440
    Top = 280
  end
  object SynXMLSynDark: TSynXMLSyn
    DefaultFilter = 'XML Files (*.svg)|*.svg'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    ElementAttri.Background = clWindow
    ElementAttri.Foreground = 14198787
    AttributeAttri.Background = clWindow
    AttributeAttri.Foreground = 14198787
    NamespaceAttributeAttri.Background = clWindow
    NamespaceAttributeAttri.Foreground = 8454143
    AttributeValueAttri.Background = clWindow
    AttributeValueAttri.Foreground = 14408667
    NamespaceAttributeValueAttri.Background = clWindow
    NamespaceAttributeValueAttri.Foreground = 8454143
    TextAttri.Background = clWindow
    TextAttri.Foreground = 15263976
    CDATAAttri.Background = clWindow
    CDATAAttri.Foreground = clSkyBlue
    EntityRefAttri.Background = clWindow
    EntityRefAttri.Foreground = 16744576
    ProcessingInstructionAttri.Background = clWindow
    ProcessingInstructionAttri.Foreground = clSilver
    CommentAttri.Background = clWindow
    CommentAttri.Foreground = 5373864
    DocTypeAttri.Background = clWindow
    DocTypeAttri.Foreground = 9750039
    SymbolAttri.Background = clWindow
    SymbolAttri.Foreground = 6008319
    WantBracesParsed = False
    Left = 168
    Top = 96
  end
end
