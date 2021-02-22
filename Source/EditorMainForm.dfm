object frmMain: TfrmMain
  Left = 250
  Top = 217
  Caption = 'SVG Text Editor'
  ClientHeight = 590
  ClientWidth = 907
  Color = clWindow
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnBeforeMonitorDpiChanged = FormBeforeMonitorDpiChanged
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    907
    590)
  PixelsPerInch = 96
  TextHeight = 15
  object RightSplitter: TSplitter
    Left = 651
    Top = 36
    Width = 6
    Height = 535
    Align = alRight
    AutoSnap = False
    MinSize = 240
    ExplicitLeft = 741
    ExplicitTop = 61
    ExplicitHeight = 500
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 571
    Width = 907
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 80
      end
      item
        Alignment = taCenter
        Width = 40
      end
      item
        Alignment = taCenter
        Width = 80
      end
      item
        Width = 100
      end
      item
        Width = 80
      end
      item
        Width = 80
      end>
  end
  object PageControl: TPageControl
    Left = 44
    Top = 36
    Width = 607
    Height = 535
    Align = alClient
    Images = VirtualImageList
    PopupMenu = popEditor
    TabOrder = 1
    OnChange = PageControlChange
  end
  object ImagePanel: TPanel
    Left = 657
    Top = 36
    Width = 250
    Height = 535
    Align = alRight
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    StyleElements = []
    object StatusSplitter: TSplitter
      Left = 0
      Top = 497
      Width = 250
      Height = 4
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      ExplicitLeft = 1
      ExplicitTop = 487
      ExplicitWidth = 248
    end
    object SVGIconImage: TSVGIconImage
      Left = 0
      Top = 172
      Width = 250
      Height = 325
      AutoSize = False
      ParentDoubleBuffered = False
      DoubleBuffered = True
      Proportional = True
      Align = alClient
      OnMouseMove = SVGIconImageMouseMove
      ExplicitLeft = 1
      ExplicitTop = 173
      ExplicitWidth = 248
      ExplicitHeight = 323
    end
    object panelPreview: TPanel
      Left = 0
      Top = 24
      Width = 250
      Height = 40
      Align = alTop
      ParentBackground = False
      ShowCaption = False
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitTop = 25
      ExplicitWidth = 248
      object BackgroundGrayScaleLabel: TLabel
        Left = 10
        Top = 6
        Width = 63
        Height = 39
        AutoSize = False
        Caption = 'Backlight %:'
        WordWrap = True
      end
      object BackgroundTrackBar: TTrackBar
        AlignWithMargins = True
        Left = 81
        Top = 4
        Width = 165
        Height = 32
        Margins.Left = 80
        Align = alClient
        Max = 255
        Frequency = 10
        Position = 117
        TabOrder = 0
        TabStop = False
        OnChange = BackgroundTrackBarChange
        ExplicitWidth = 163
      end
    end
    object FlowPanel: TFlowPanel
      AlignWithMargins = True
      Left = 3
      Top = 67
      Width = 244
      Height = 102
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 2
      ExplicitLeft = 4
      ExplicitTop = 68
      ExplicitWidth = 242
      object SVGIconImage16: TSVGIconImage
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 24
        Height = 24
        Hint = '16x16'
        AutoSize = False
        ParentDoubleBuffered = False
        DoubleBuffered = True
      end
      object SVGIconImage32: TSVGIconImage
        AlignWithMargins = True
        Left = 33
        Top = 3
        Width = 36
        Height = 36
        Hint = '32x32'
        AutoSize = False
        ParentDoubleBuffered = False
        DoubleBuffered = True
      end
      object SVGIconImage48: TSVGIconImage
        AlignWithMargins = True
        Left = 75
        Top = 3
        Width = 48
        Height = 48
        Hint = '48x48'
        AutoSize = False
        ParentDoubleBuffered = False
        DoubleBuffered = True
      end
      object SVGIconImage96: TSVGIconImage
        AlignWithMargins = True
        Left = 129
        Top = 3
        Width = 96
        Height = 96
        Hint = '96x96'
        AutoSize = False
        ParentDoubleBuffered = False
        DoubleBuffered = True
      end
    end
    object ImagePreviewPanel: TPanel
      Left = 0
      Top = 0
      Width = 250
      Height = 24
      Align = alTop
      Caption = 'IMAGE PREVIEW'
      ParentBackground = False
      TabOrder = 3
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 248
    end
    object StatusPanel: TPanel
      Left = 0
      Top = 501
      Width = 250
      Height = 34
      Align = alBottom
      ParentBackground = False
      TabOrder = 4
      ExplicitLeft = 1
      ExplicitTop = 500
      ExplicitWidth = 248
      object StatusImage: TSVGIconImage
        Left = 1
        Top = 1
        Width = 32
        Height = 32
        AutoSize = False
        Proportional = True
        ImageList = VirtualImageList
        ImageIndex = 39
        Align = alLeft
      end
      object StatusStaticText: TStaticText
        AlignWithMargins = True
        Left = 36
        Top = 4
        Width = 210
        Height = 26
        Align = alClient
        Alignment = taCenter
        TabOrder = 1
        ExplicitWidth = 208
      end
    end
  end
  object SV: TSplitView
    Left = 0
    Top = 36
    Width = 160
    Height = 535
    CloseStyle = svcCompact
    Color = clHighlight
    CompactWidth = 44
    DisplayMode = svmOverlay
    OpenedWidth = 160
    ParentDoubleBuffered = True
    Placement = svpLeft
    TabOrder = 3
    OnClosed = SVClosed
    OnClosing = SVClosing
    OnOpened = SVOpened
    OnOpening = SVOpening
    object catMenuItems: TCategoryButtons
      Left = 0
      Top = 0
      Width = 160
      Height = 535
      Align = alClient
      BackgroundGradientDirection = gdVertical
      BorderStyle = bsNone
      ButtonFlow = cbfVertical
      ButtonHeight = 36
      ButtonWidth = 36
      ButtonOptions = [boFullSize, boShowCaptions, boBoldCaptions, boCaptionOnlyBorder]
      Categories = <
        item
          Caption = 'File'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = acNewFile
            end
            item
              Action = acOpenFile
            end
            item
              Action = OpenRecentAction
            end
            item
              Action = acClose
            end
            item
              Action = acCloseAll
            end
            item
              Action = acSave
            end
            item
              Action = acSaveAll
            end
            item
              Action = actnSaveAs
            end
            item
              Action = ExportToPNGAction
            end
            item
              Action = acQuit
            end>
        end
        item
          Caption = 'Text'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = actnFormatXML
            end
            item
              Action = acEditSelectAll
            end
            item
              Action = acEditUndo
            end
            item
              Action = acEditCut
            end
            item
              Action = acEditCopy
            end
            item
              Action = acEditPaste
            end
            item
              Action = acSearch
            end
            item
              Action = acSearchAgain
            end
            item
              Action = acReplace
            end>
        end
        item
          Caption = 'Print'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = actnPrint
            end
            item
              Action = actnPrintPreview
            end>
        end>
      Color = clBtnFace
      DoubleBuffered = True
      HotButtonColor = 12500670
      Images = VirtualImageList
      ParentDoubleBuffered = False
      RegularButtonColor = clNone
      SelectedButtonColor = clNone
      TabOrder = 0
      OnGetHint = catMenuItemsGetHint
      OnMouseLeave = catMenuItemsMouseLeave
      OnMouseMove = catMenuItemsMouseMove
    end
  end
  object panlTop: TPanel
    Left = 0
    Top = 0
    Width = 907
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 4
    object lblTitle: TLabel
      AlignWithMargins = True
      Left = 40
      Top = 3
      Width = 79
      Height = 30
      Align = alLeft
      Caption = 'SVG Text Editor'
      Layout = tlCenter
      ExplicitHeight = 15
    end
    object SettingsToolBar: TToolBar
      AlignWithMargins = True
      Left = 704
      Top = 3
      Width = 200
      Height = 30
      Align = alRight
      AutoSize = True
      ButtonHeight = 32
      ButtonWidth = 32
      Color = clYellow
      DrawingStyle = dsGradient
      GradientEndColor = clBtnFace
      GradientStartColor = clBtnFace
      Images = VirtualImageList
      ParentColor = False
      TabOrder = 0
      Transparent = True
      object ColorSettingsToolButton: TToolButton
        Left = 0
        Top = 0
        Action = actnColorSettings
      end
      object EditOptionsToolButton: TToolButton
        Left = 32
        Top = 0
        Action = actnEditOptions
      end
      object PageSetupToolButton: TToolButton
        Left = 64
        Top = 0
        Action = actnPageSetup
      end
      object PrinterSetupToolButton: TToolButton
        Left = 96
        Top = 0
        Action = actnPrinterSetup
      end
      object ToolButton9: TToolButton
        Left = 128
        Top = 0
        Width = 8
        ImageIndex = 23
        ImageName = 'about'
        Style = tbsSeparator
      end
      object AboutToolButton: TToolButton
        Left = 136
        Top = 0
        Action = acAbout
      end
      object QuitToolButton: TToolButton
        Left = 168
        Top = 0
        Action = acQuit
        ImageName = 'Exit'
      end
    end
    object MenuButtonToolbar: TToolBar
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 31
      Height = 30
      Align = alLeft
      AutoSize = True
      ButtonHeight = 32
      ButtonWidth = 32
      Color = clYellow
      DrawingStyle = dsGradient
      GradientEndColor = clBtnFace
      GradientStartColor = clBtnFace
      Images = VirtualImageList
      ParentColor = False
      TabOrder = 1
      Transparent = True
      object ToolButton1: TToolButton
        AlignWithMargins = True
        Left = 0
        Top = 0
        Action = actMenu
        AutoSize = True
      end
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'SVG Image files (.svg)|*.svg'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 300
    Top = 196
  end
  object ActionList: TActionList
    Images = VirtualImageList
    OnExecute = ActionListExecute
    OnUpdate = ActionListUpdate
    Left = 308
    Top = 108
    object acNewFile: TAction
      Category = 'File'
      Caption = 'New ...'
      Hint = 'New SVG File'
      ImageIndex = 2
      ImageName = 'New'
      OnExecute = acNewFileExecute
    end
    object acOpenFile: TAction
      Category = 'File'
      Caption = 'Open ...'
      Hint = 'Open SVG File...'
      ImageIndex = 1
      ImageName = 'Open'
      OnExecute = acOpenFileExecute
    end
    object acEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Enabled = False
      Hint = 'Cuts the selection and puts it on the Clipboard'
      ImageIndex = 10
      ImageName = 'Cut'
      ShortCut = 16472
      OnExecute = acEditCutExecute
    end
    object acSearch: TAction
      Category = 'Edit'
      Caption = 'Search ...'
      Hint = 'Search into text...'
      ImageIndex = 6
      ImageName = 'Search'
      ShortCut = 16454
      OnExecute = acSearchExecute
      OnUpdate = acSearchUpdate
    end
    object acSearchAgain: TAction
      Category = 'Edit'
      Caption = 'Repeat search'
      Hint = 'Repeat last search'
      ImageIndex = 7
      ImageName = 'Search-repeat'
      ShortCut = 114
      OnExecute = acSearchAgainExecute
      OnUpdate = acSearchAgainUpdate
    end
    object acClose: TAction
      Category = 'File'
      Caption = 'Close'
      Hint = 'Close File'
      ImageIndex = 3
      ImageName = 'Close'
      ShortCut = 16499
      OnExecute = acCloseExecute
      OnUpdate = actnEditingUpdate
    end
    object acEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Enabled = False
      Hint = 'Copies the selection and puts it on the Clipboard'
      ImageIndex = 8
      ImageName = 'Copy'
      ShortCut = 16451
      OnExecute = acEditCopyExecute
      OnUpdate = actionForFileUpdate
    end
    object acEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Enabled = False
      Hint = 'Inserts Clipboard contents'
      ImageIndex = 9
      ImageName = 'Paste'
      ShortCut = 16470
      OnExecute = acEditPasteExecute
      OnUpdate = actionForFileUpdate
    end
    object acEditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Selects the entire document'
      ImageIndex = 18
      ImageName = 'Select-all'
      ShortCut = 16449
      OnExecute = acEditSelectAllExecute
      OnUpdate = actionForFileUpdate
    end
    object acEditUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Reverts the last action'
      ImageIndex = 11
      ImageName = 'Undo'
      ShortCut = 16474
      OnExecute = acEditUndoExecute
      OnUpdate = acEditUndoUpdate
    end
    object acSave: TAction
      Category = 'File'
      Caption = 'Save'
      Hint = 'Save File'
      ImageIndex = 12
      ImageName = 'Save'
      ShortCut = 16467
      OnExecute = acSaveExecute
      OnUpdate = acSaveUpdate
    end
    object acReplace: TAction
      Category = 'Edit'
      Caption = 'Replace ...'
      Hint = 'Search and Replace'
      ImageIndex = 20
      ImageName = 'replace'
      ShortCut = 16466
      OnExecute = acReplaceExecute
      OnUpdate = acReplaceUpdate
    end
    object acQuit: TAction
      Category = 'File'
      Caption = 'Quit'
      Hint = 'Close application'
      ImageIndex = 22
      ImageName = 'exit'
      ShortCut = 16472
      OnExecute = acQuitExecute
    end
    object acAbout: TAction
      Category = 'Help'
      Caption = 'About ...'
      Hint = 'About SVG Text Editor'
      ImageIndex = 23
      ImageName = 'about'
      OnExecute = acAboutExecute
    end
    object acCloseAll: TAction
      Category = 'File'
      Caption = 'Close All'
      Hint = 'Close all files'
      ImageIndex = 4
      ImageName = 'Close-all'
      OnExecute = acCloseAllExecute
      OnUpdate = acCloseAllUpdate
    end
    object acSaveAll: TAction
      Category = 'File'
      Caption = 'Save All'
      Hint = 'Save all changes'
      ImageIndex = 13
      ImageName = 'Save-all'
      OnExecute = acSaveAllExecute
      OnUpdate = acSaveAllUpdate
    end
    object actnPrint: TAction
      Category = 'File'
      Caption = 'Print file ...'
      Hint = 'Print file'
      ImageIndex = 15
      ImageName = 'Print'
      OnExecute = actnPrintExecute
      OnUpdate = actnEditingUpdate
    end
    object actnPrinterSetup: TAction
      Category = 'Settings'
      Caption = 'Printer Setup ...'
      Hint = 'Printer Setup'
      ImageIndex = 17
      ImageName = 'Print-settings'
      OnExecute = actnPrinterSetupExecute
    end
    object actnPrintPreview: TAction
      Category = 'File'
      Caption = 'Print Preview'
      Hint = 'Print Preview'
      ImageIndex = 16
      ImageName = 'print-preview'
      OnExecute = actnPrintPreviewExecute
      OnUpdate = actnEditingUpdate
    end
    object actnPageSetup: TAction
      Category = 'Settings'
      Caption = 'Page Setup ...'
      Hint = 'Printer Page Setup'
      ImageIndex = 30
      ImageName = 'view_details'
      OnExecute = actnPageSetupExecute
      OnUpdate = actnEditingUpdate
    end
    object actnEditOptions: TAction
      Category = 'Settings'
      Caption = 'Editor Options ...'
      Hint = 'Editor Options'
      ImageIndex = 28
      ImageName = 'preferences-desktop'
      OnExecute = actnEditOptionsExecute
    end
    object actnEnlargeFont: TAction
      Category = 'Settings'
      Caption = 'Font +'
      Hint = 'Enlarge Font'
      ImageIndex = 26
      ImageName = 'plus'
      OnExecute = actnFontExecute
    end
    object actnReduceFont: TAction
      Category = 'Settings'
      Caption = 'Font -'
      Hint = 'Reduce Font'
      ImageIndex = 25
      ImageName = 'Minus'
      OnExecute = actnFontExecute
    end
    object actnSaveAs: TAction
      Category = 'File'
      Caption = 'Save As ...'
      Hint = 'Save As ...'
      ImageIndex = 14
      ImageName = 'Save-as'
      OnExecute = actnSaveAsExecute
      OnUpdate = actnEditingUpdate
    end
    object actnColorSettings: TAction
      Category = 'Settings'
      Caption = 'Theme settings'
      Hint = 'Theme settings (colors, font)'
      ImageIndex = 0
      ImageName = 'Style'
      OnExecute = actnColorSettingsExecute
      OnUpdate = actnColorSettingsUpdate
    end
    object actnFormatXML: TAction
      Category = 'Edit'
      Caption = 'Reformat text'
      Hint = 'Reformat text'
      ImageIndex = 19
      ImageName = 'Reformat'
      ShortCut = 16468
      OnExecute = actnFormatXMLExecute
      OnUpdate = actionForFileUpdate
    end
    object actMenu: TAction
      Caption = 'Collapse'
      Hint = 'Collapse'
      ImageIndex = 24
      ImageName = 'menu'
      OnExecute = actMenuExecute
    end
    object OpenRecentAction: TAction
      Category = 'File'
      Caption = 'Open recent...'
      Hint = 'Open recent SVG File...'
      ImageIndex = 5
      ImageName = 'Close-all-folder'
      OnExecute = OpenRecentActionExecute
    end
    object ExportToPNGAction: TAction
      Category = 'File'
      Caption = 'Export to png...'
      Hint = 'Export to multiple png files...'
      ImageIndex = 38
      ImageName = 'export'
      ShortCut = 16453
      OnExecute = ExportToPNGActionExecute
    end
  end
  object SaveDialog: TSaveDialog
    Filter = 'SVG Image files (.svg)|*.svg'
    Left = 300
    Top = 264
  end
  object popEditor: TPopupMenu
    Images = VirtualImageList
    Left = 456
    Top = 152
    object CloseMenuItem: TMenuItem
      Action = acClose
    end
    object SaveMenuItem: TMenuItem
      Action = acSave
    end
    object CloseAll1: TMenuItem
      Action = acCloseAll
    end
    object Sep1MenuItem: TMenuItem
      Caption = '-'
    end
    object Reformattext1: TMenuItem
      Action = actnFormatXML
    end
    object SelectAllMenuItem: TMenuItem
      Action = acEditSelectAll
    end
    object CopyMenuItem: TMenuItem
      Action = acEditCopy
    end
    object CutMenuItem: TMenuItem
      Action = acEditCut
    end
    object PasteMenuItem: TMenuItem
      Action = acEditPaste
    end
    object Sep2MenuItem: TMenuItem
      Caption = '-'
    end
    object SearchMenuItem: TMenuItem
      Action = acSearch
    end
    object ReplaceMenuItem: TMenuItem
      Action = acReplace
      ImageName = 'Replace'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ExporttoPNGMenuItem: TMenuItem
      Action = ExportToPNGAction
    end
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 657
    Top = 261
  end
  object PrintDialog: TPrintDialog
    Left = 656
    Top = 313
  end
  object SynEditPrint: TSynEditPrint
    Copies = 1
    Header.FrameTypes = [ftBox, ftShaded]
    Header.DefaultFont.Charset = DEFAULT_CHARSET
    Header.DefaultFont.Color = clBlack
    Header.DefaultFont.Height = -13
    Header.DefaultFont.Name = 'Arial'
    Header.DefaultFont.Style = []
    Footer.FrameTypes = [ftBox, ftShaded]
    Footer.DefaultFont.Charset = DEFAULT_CHARSET
    Footer.DefaultFont.Color = clBlack
    Footer.DefaultFont.Height = -13
    Footer.DefaultFont.Name = 'Arial'
    Footer.DefaultFont.Style = []
    Margins.UnitSystem = usCM
    Margins.Left = 1.500000000000000000
    Margins.Right = 1.500000000000000000
    Margins.Top = 2.000000000000000000
    Margins.Bottom = 2.000000000000000000
    Margins.Header = 1.500000000000000000
    Margins.Footer = 1.500000000000000000
    Margins.LeftHFTextIndent = 0.200000000000000000
    Margins.RightHFTextIndent = 0.200000000000000000
    Margins.HFInternalMargin = 0.050000000000000000
    Margins.MirrorMargins = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Colors = True
    TabWidth = 8
    Color = clWhite
    Left = 656
    Top = 208
  end
  object SynEditSearch: TSynEditSearch
    Left = 204
    Top = 192
  end
  object VirtualImageList: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Style'
        Disabled = False
        Name = 'Style'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Open'
        Disabled = False
        Name = 'Open'
      end
      item
        CollectionIndex = 2
        CollectionName = 'New'
        Disabled = False
        Name = 'New'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Close'
        Disabled = False
        Name = 'Close'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Close-all'
        Disabled = False
        Name = 'Close-all'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Close-all-folder'
        Disabled = False
        Name = 'Close-all-folder'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Search'
        Disabled = False
        Name = 'Search'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Search-repeat'
        Disabled = False
        Name = 'Search-repeat'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Copy'
        Disabled = False
        Name = 'Copy'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Paste'
        Disabled = False
        Name = 'Paste'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Cut'
        Disabled = False
        Name = 'Cut'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Undo'
        Disabled = False
        Name = 'Undo'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Save'
        Disabled = False
        Name = 'Save'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Save-all'
        Disabled = False
        Name = 'Save-all'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Save-as'
        Disabled = False
        Name = 'Save-as'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Print'
        Disabled = False
        Name = 'Print'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Print-preview'
        Disabled = False
        Name = 'Print-preview'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Print-settings'
        Disabled = False
        Name = 'Print-settings'
      end
      item
        CollectionIndex = 18
        CollectionName = 'Select-all'
        Disabled = False
        Name = 'Select-all'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Reformat'
        Disabled = False
        Name = 'Reformat'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Replace'
        Disabled = False
        Name = 'Replace'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Settings'
        Disabled = False
        Name = 'Settings'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Exit'
        Disabled = False
        Name = 'Exit'
      end
      item
        CollectionIndex = 23
        CollectionName = 'about'
        Disabled = False
        Name = 'about'
      end
      item
        CollectionIndex = 24
        CollectionName = 'menu'
        Disabled = False
        Name = 'menu'
      end
      item
        CollectionIndex = 25
        CollectionName = 'Minus'
        Disabled = False
        Name = 'Minus'
      end
      item
        CollectionIndex = 26
        CollectionName = 'plus'
        Disabled = False
        Name = 'plus'
      end
      item
        CollectionIndex = 27
        CollectionName = 'back'
        Disabled = False
        Name = 'back'
      end
      item
        CollectionIndex = 28
        CollectionName = 'preferences-desktop'
        Disabled = False
        Name = 'preferences-desktop'
      end
      item
        CollectionIndex = 29
        CollectionName = 'preferences-desktop-color'
        Disabled = False
        Name = 'preferences-desktop-color'
      end
      item
        CollectionIndex = 30
        CollectionName = 'view_details'
        Disabled = False
        Name = 'view_details'
      end
      item
        CollectionIndex = 31
        CollectionName = 'left'
        Disabled = False
        Name = 'left'
      end
      item
        CollectionIndex = 32
        CollectionName = 'right'
        Disabled = False
        Name = 'right'
      end
      item
        CollectionIndex = 33
        CollectionName = 'binoculars'
        Disabled = False
        Name = 'binoculars'
      end
      item
        CollectionIndex = 34
        CollectionName = 'whole-page'
        Disabled = False
        Name = 'whole-page'
      end
      item
        CollectionIndex = 35
        CollectionName = 'page-width'
        Disabled = False
        Name = 'page-width'
      end
      item
        CollectionIndex = 36
        CollectionName = 'svg-logo'
        Disabled = False
        Name = 'svg-logo'
      end
      item
        CollectionIndex = 37
        CollectionName = 'svg-logo-gray'
        Disabled = False
        Name = 'svg-logo-gray'
      end
      item
        CollectionIndex = 38
        CollectionName = 'export'
        Disabled = False
        Name = 'export'
      end
      item
        CollectionIndex = 46
        CollectionName = 'error'
        Disabled = False
        Name = 'error'
      end
      item
        CollectionIndex = 47
        CollectionName = 'info'
        Disabled = False
        Name = 'info'
      end>
    ImageCollection = dmResources.SVGIconImageCollection
    Width = 24
    Height = 24
    Left = 448
    Top = 304
  end
  object RecentPopupMenu: TPopupMenu
    OnPopup = RecentPopupMenuPopup
    Left = 456
    Top = 216
  end
end
