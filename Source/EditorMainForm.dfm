object frmMain: TfrmMain
  Left = 250
  Top = 217
  Caption = 'SVG Text Editor'
  ClientHeight = 588
  ClientWidth = 899
  Color = clAppWorkSpace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsMDIForm
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnBeforeMonitorDpiChanged = FormBeforeMonitorDpiChanged
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    899
    588)
  TextHeight = 15
  object RightSplitter: TSplitter
    Left = 643
    Top = 69
    Width = 6
    Height = 500
    Align = alRight
    AutoSnap = False
    MinSize = 240
    ExplicitTop = 38
    ExplicitHeight = 531
  end
  object ClientPanel: TPanel
    AlignWithMargins = True
    Left = 42
    Top = 38
    Width = 857
    Height = 31
    Margins.Left = 42
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    ParentColor = True
    TabOrder = 4
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 569
    Width = 899
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
  object ImagePanel: TPanel
    Left = 649
    Top = 69
    Width = 250
    Height = 500
    Align = alRight
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    StyleElements = []
    object StatusSplitter: TSplitter
      Left = 0
      Top = 462
      Width = 250
      Height = 4
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      ExplicitTop = 493
    end
    object SVGIconImage: TSVGIconImage
      Left = 0
      Top = 172
      Width = 250
      Height = 290
      AutoSize = False
      Align = alClient
      OnMouseMove = SVGIconImageMouseMove
    end
    object panelPreview: TPanel
      Left = 0
      Top = 24
      Width = 250
      Height = 40
      Align = alTop
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
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
      TabOrder = 1
      object SVGIconImage16: TSVGIconImage
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 24
        Height = 24
        Hint = '16x16'
        AutoSize = False
      end
      object SVGIconImage32: TSVGIconImage
        AlignWithMargins = True
        Left = 33
        Top = 3
        Width = 36
        Height = 36
        Hint = '32x32'
        AutoSize = False
      end
      object SVGIconImage48: TSVGIconImage
        AlignWithMargins = True
        Left = 75
        Top = 3
        Width = 48
        Height = 48
        Hint = '48x48'
        AutoSize = False
      end
      object SVGIconImage96: TSVGIconImage
        AlignWithMargins = True
        Left = 129
        Top = 3
        Width = 96
        Height = 96
        Hint = '96x96'
        AutoSize = False
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
    end
    object StatusPanel: TPanel
      Left = 0
      Top = 466
      Width = 250
      Height = 34
      Align = alBottom
      ParentBackground = False
      TabOrder = 2
      object StatusImage: TSVGIconImage
        Left = 1
        Top = 1
        Width = 32
        Height = 32
        AutoSize = False
        ImageList = VirtualImageList
        ImageIndex = 39
        ImageName = 'error'
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
        TabOrder = 0
      end
    end
  end
  object SV: TSplitView
    Left = 0
    Top = 36
    Width = 44
    Height = 533
    CloseStyle = svcCompact
    Color = clHighlight
    CompactWidth = 44
    DisplayMode = svmOverlay
    Opened = False
    OpenedWidth = 160
    ParentDoubleBuffered = True
    Placement = svpLeft
    TabOrder = 1
    OnClosed = SVClosed
    OnClosing = SVClosing
    OnOpened = SVOpened
    OnOpening = SVOpening
    object catMenuItems: TStyledCategoryButtons
      Left = 0
      Top = 0
      Width = 44
      Height = 533
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
              ImageName = 'Exit'
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
              ImageName = 'Replace'
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
              ImageName = 'Print-preview'
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
    Width = 899
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 2
    object lblTitle: TLabel
      AlignWithMargins = True
      Left = 40
      Top = 3
      Width = 79
      Height = 32
      Align = alLeft
      Caption = 'SVG Text Editor'
      Layout = tlCenter
      ExplicitHeight = 15
    end
    object SettingsToolBar: TStyledToolbar
      AlignWithMargins = True
      Left = 696
      Top = 3
      Width = 200
      Height = 32
      Align = alRight
      ButtonHeight = 32
      ButtonWidth = 32
      Images = VirtualImageList
      Indent = 3
      TabOrder = 0
      object ColorSettingsToolButton: TStyledToolButton
        Left = 0
        Top = 0
        Action = actnColorSettings
      end
      object EditOptionsToolButton: TStyledToolButton
        Left = 32
        Top = 0
        Action = actnEditOptions
      end
      object PageSetupToolButton: TStyledToolButton
        Left = 64
        Top = 0
        Action = actnPageSetup
      end
      object PrinterSetupToolButton: TStyledToolButton
        Left = 96
        Top = 0
        Action = actnPrinterSetup
      end
      object SepToolButton: TStyledToolButton
        Left = 128
        Top = 0
        Style = tbsSeparator
      end
      object AboutToolButton: TStyledToolButton
        Left = 134
        Top = 0
        Action = acAbout
      end
      object QuitToolButton: TStyledToolButton
        Left = 166
        Top = 0
        Action = acQuit
        ImageName = 'Exit'
      end
    end
    object MenuButtonToolbar: TStyledToolbar
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 31
      Height = 32
      Align = alLeft
      ButtonHeight = 32
      ButtonWidth = 32
      Images = VirtualImageList
      Indent = 3
      TabOrder = 1
      object MenuToolButton: TStyledToolButton
        Left = 0
        Top = 0
        Action = actMenu
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
      OnUpdate = actionForFileUpdate
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
      OnUpdate = acEditCopyUpdate
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
      ShortCut = 16465
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
      ImageIndex = 41
      ImageName = 'Support'
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
      Hint = 'Save File As ...'
      ImageIndex = 14
      ImageName = 'Save-as'
      OnExecute = actnSaveAsExecute
      OnUpdate = actnEditingUpdate
    end
    object actnColorSettings: TAction
      Category = 'Settings'
      Caption = 'Theme settings'
      Hint = 'Theme settings (colors, font, themes)'
      ImageIndex = 28
      ImageName = 'preferences-desktop'
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
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
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
    object CloseAllMenuItem: TMenuItem
      Action = acCloseAll
    end
    object Sep1MenuItem: TMenuItem
      Caption = '-'
    end
    object ReformatTextMenuItem: TMenuItem
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
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Style'
        Name = 'Style'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Open'
        Name = 'Open'
      end
      item
        CollectionIndex = 2
        CollectionName = 'New'
        Name = 'New'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Close-all'
        Name = 'Close-all'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Close-all-folder'
        Name = 'Close-all-folder'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Search'
        Name = 'Search'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Search-repeat'
        Name = 'Search-repeat'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Paste'
        Name = 'Paste'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Cut'
        Name = 'Cut'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Undo'
        Name = 'Undo'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Save'
        Name = 'Save'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Save-all'
        Name = 'Save-all'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Save-as'
        Name = 'Save-as'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Print'
        Name = 'Print'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Print-preview'
        Name = 'Print-preview'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Print-settings'
        Name = 'Print-settings'
      end
      item
        CollectionIndex = 18
        CollectionName = 'Select-all'
        Name = 'Select-all'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Reformat'
        Name = 'Reformat'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Replace'
        Name = 'Replace'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Settings'
        Name = 'Settings'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Exit'
        Name = 'Exit'
      end
      item
        CollectionIndex = 23
        CollectionName = 'about'
        Name = 'about'
      end
      item
        CollectionIndex = 24
        CollectionName = 'menu'
        Name = 'menu'
      end
      item
        CollectionIndex = 25
        CollectionName = 'Minus'
        Name = 'Minus'
      end
      item
        CollectionIndex = 26
        CollectionName = 'plus'
        Name = 'plus'
      end
      item
        CollectionIndex = 27
        CollectionName = 'back'
        Name = 'back'
      end
      item
        CollectionIndex = 28
        CollectionName = 'preferences-desktop'
        Name = 'preferences-desktop'
      end
      item
        CollectionIndex = 29
        CollectionName = 'preferences-desktop-color'
        Name = 'preferences-desktop-color'
      end
      item
        CollectionIndex = 30
        CollectionName = 'view_details'
        Name = 'view_details'
      end
      item
        CollectionIndex = 31
        CollectionName = 'left'
        Name = 'left'
      end
      item
        CollectionIndex = 32
        CollectionName = 'right'
        Name = 'right'
      end
      item
        CollectionIndex = 33
        CollectionName = 'binoculars'
        Name = 'binoculars'
      end
      item
        CollectionIndex = 34
        CollectionName = 'whole-page'
        Name = 'whole-page'
      end
      item
        CollectionIndex = 35
        CollectionName = 'page-width'
        Name = 'page-width'
      end
      item
        CollectionIndex = 36
        CollectionName = 'svg-logo'
        Name = 'svg-logo'
      end
      item
        CollectionIndex = 37
        CollectionName = 'svg-logo-gray'
        Name = 'svg-logo-gray'
      end
      item
        CollectionIndex = 38
        CollectionName = 'export'
        Name = 'export'
      end
      item
        CollectionIndex = 46
        CollectionName = 'error'
        Name = 'error'
      end
      item
        CollectionIndex = 47
        CollectionName = 'info'
        Name = 'info'
      end
      item
        CollectionIndex = 41
        CollectionName = 'Support'
        Name = 'Support'
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
  object VirtualImageList20: TVirtualImageList
    Images = <
      item
        CollectionIndex = 48
        CollectionName = 'close-circle-outline'
        Name = 'close-circle-outline'
      end
      item
        CollectionIndex = 36
        CollectionName = 'svg-logo'
        Name = 'svg-logo'
      end
      item
        CollectionIndex = 37
        CollectionName = 'svg-logo-gray'
        Name = 'svg-logo-gray'
      end>
    ImageCollection = dmResources.SVGIconImageCollection
    Width = 20
    Height = 20
    Left = 448
    Top = 368
  end
  object LoadTimer: TTimer
    Enabled = False
    OnTimer = LoadTimerTimer
    Left = 272
    Top = 352
  end
  object CheckFileChangedTimer: TTimer
    Interval = 3000
    OnTimer = CheckFileChangedTimerTimer
    Left = 144
    Top = 352
  end
end
