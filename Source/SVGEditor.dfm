object FrmEditor: TFrmEditor
  Left = 522
  Top = 286
  Caption = 'FrmEditor'
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 888
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object PanelToolBar: TPanel
      Left = 0
      Top = 0
      Width = 888
      Height = 35
      Align = alClient
      BevelOuter = bvNone
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object ToolBar1: TToolBar
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
        ParentShowHint = False
        ShowCaptions = True
        ShowHint = True
        TabOrder = 0
        object ToolButtonZoomIn: TToolButton
          Left = 0
          Top = 0
          Hint = 'Zoom In'
          AutoSize = True
          Caption = 'Zoom In'
          ImageIndex = 0
          ImageName = 'add_image'
          OnClick = ToolButtonZoomInClick
        end
        object ToolButtonZommOut: TToolButton
          Left = 81
          Top = 0
          Hint = 'Zoom Out'
          AutoSize = True
          Caption = 'Zoom Out'
          ImageIndex = 1
          OnClick = ToolButtonZommOutClick
        end
        object ToolButtonEdit: TToolButton
          Left = 172
          Top = 0
          Caption = 'Edit...'
          ImageIndex = 5
          ImageName = 'edit_image'
          OnClick = ToolButtonEditClick
        end
        object ToolButtonSearch: TToolButton
          Left = 259
          Top = 0
          Hint = 'Search'
          AutoSize = True
          Caption = 'Search...'
          ImageIndex = 2
          ImageName = 'search'
          OnClick = ToolButtonSearchClick
        end
        object ToolButtonSave: TToolButton
          Left = 341
          Top = 0
          Hint = 'Save Settings'
          AutoSize = True
          Caption = 'Settings...'
          ImageIndex = 3
          ImageName = 'settings'
          OnClick = ToolButtonSaveClick
        end
        object ToolButton1: TToolButton
          Left = 431
          Top = 0
          Width = 8
          Caption = 'ToolButton1'
          ImageIndex = 4
          ImageName = 'about'
          Style = tbsSeparator
        end
        object ToolButtonAbout: TToolButton
          Left = 439
          Top = 0
          Hint = 'Report bugs'
          AutoSize = True
          Caption = 'About...'
          ImageIndex = 4
          ImageName = 'about'
          OnClick = ToolButtonAboutClick
        end
      end
    end
  end
  object PanelEditor: TPanel
    Left = 0
    Top = 35
    Width = 888
    Height = 489
    Align = alClient
    BevelOuter = bvNone
    Caption = 'PanelEditor'
    TabOrder = 1
    object SynEdit: TSynEdit
      Left = 0
      Top = 0
      Width = 888
      Height = 489
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
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Gutter.ShowLineNumbers = True
      Highlighter = SynXMLSyn
      ReadOnly = True
      FontSmoothing = fsmNone
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 524
    Width = 888
    Height = 19
    Panels = <>
    SimplePanel = True
    SizeGrip = False
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
  object dlgFileSaveAs: TSaveDialog
    Title = 'Export file as'
    Left = 476
    Top = 364
  end
  object SynEditSearch1: TSynEditSearch
    Left = 672
    Top = 312
  end
  object SynEditRegexSearch1: TSynEditRegexSearch
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
        IconName = 'search'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#616161' +
          '">'#13#10'        <rect x="34.6" y="28.1" transform="matrix(.707 -.707' +
          ' .707 .707 -15.154 36.586)" width="4" height="17"/>'#13#10'        <ci' +
          'rcle cx="20" cy="20" r="16"/>'#13#10'    </g>'#13#10'    <rect x="36.2" y="3' +
          '2.1" transform="matrix(.707 -.707 .707 .707 -15.839 38.239)" fil' +
          'l="#37474F" width="4" height="12.3"/>'#13#10'    <circle fill="#64B5F6' +
          '" cx="20" cy="20" r="13"/>'#13#10'    <path fill="#BBDEFB" d="M26.9,14' +
          '.2c-1.7-2-4.2-3.2-6.9-3.2s-5.2,1.2-6.9,3.2c-0.4,0.4-0.3,1.1,0.1,' +
          '1.4c0.4,0.4,1.1,0.3,1.4-0.1 C16,13.9,17.9,13,20,13s4,0.9,5.4,2.5' +
          'c0.2,0.2,0.5,0.4,0.8,0.4c0.2,0,0.5-0.1,0.6-0.2C27.2,15.3,27.2,14' +
          '.6,26.9,14.2z"/>'#13#10'</svg>'#13#10
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
      end>
    Scaled = True
    Left = 352
    Top = 195
  end
end
