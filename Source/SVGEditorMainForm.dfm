object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'TSplitView'
  ClientHeight = 669
  ClientWidth = 1034
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ScreenSnap = True
  ShowHint = True
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnBeforeMonitorDpiChanged = FormBeforeMonitorDpiChanged
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splSettings: TSplitter
    Left = 1031
    Top = 42
    Height = 627
    Align = alRight
    AutoSnap = False
    Beveled = True
    MinSize = 45
    ExplicitLeft = 208
    ExplicitTop = 57
    ExplicitHeight = 682
  end
  object splSplit: TSplitter
    Left = 200
    Top = 42
    Height = 627
    AutoSnap = False
    Beveled = True
    MinSize = 45
    ExplicitLeft = 344
    ExplicitTop = 296
    ExplicitHeight = 100
  end
  object PageControl: TPageControl
    Left = 203
    Top = 42
    Width = 528
    Height = 627
    Align = alClient
    TabOrder = 3
  end
  object panlTop: TPanel
    Left = 0
    Top = 0
    Width = 1034
    Height = 42
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 121
      Height = 36
      Align = alLeft
      Caption = 'TSplitView Demonstration'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object ToolBar: TToolBar
      AlignWithMargins = True
      Left = 923
      Top = 3
      Width = 108
      Height = 36
      Align = alRight
      AutoSize = True
      ButtonHeight = 36
      ButtonWidth = 36
      Color = clYellow
      DrawingStyle = dsGradient
      GradientEndColor = clBtnFace
      GradientStartColor = clBtnFace
      Images = SVGIconImageList
      ParentColor = False
      TabOrder = 0
      Transparent = True
      object ToolButton2: TToolButton
        Left = 0
        Top = 0
        Hint = 'Home action...'
        Caption = 'Home'
        ImageName = 'home'
      end
      object ToolButton3: TToolButton
        Left = 36
        Top = 0
        Hint = 'Change theme using Theme Selector...'
        Caption = 'Theme selector'
        ImageName = 'palette'
      end
      object ToolButton4: TToolButton
        Left = 72
        Top = 0
        Caption = 'Settings'
        ImageIndex = 21
        ImageName = 'settings'
        OnClick = actSettingsExecute
      end
    end
  end
  object SV: TSplitView
    Left = 0
    Top = 42
    Width = 200
    Height = 627
    CloseStyle = svcCompact
    CompactWidth = 48
    OpenedWidth = 200
    ParentBackground = True
    ParentColor = True
    ParentDoubleBuffered = True
    Placement = svpLeft
    TabOrder = 1
    OnClosed = SVClosed
    OnClosing = SVClosing
    OnOpened = SVOpened
    OnOpening = SVOpening
    OnResize = SVResize
    object catMenuItems: TCategoryButtons
      Left = 0
      Top = 42
      Width = 200
      Height = 512
      Align = alClient
      BackgroundGradientDirection = gdVertical
      BorderStyle = bsNone
      ButtonFlow = cbfVertical
      ButtonHeight = 36
      ButtonWidth = 36
      ButtonOptions = [boFullSize, boShowCaptions, boCaptionOnlyBorder]
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
              Action = acQuit
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
            end
            item
              Action = actnPrinterSetup
            end>
        end
        item
          Caption = 'Edit'
          Color = clNone
          Collapsed = False
          Items = <
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
            end
            item
              Action = actnFormatXML
            end>
        end
        item
          Caption = 'Help'
          Color = 16771818
          Collapsed = False
          Items = <
            item
              Action = acStyleSelection
            end
            item
              Action = acAbout
            end>
        end>
      DoubleBuffered = True
      HotButtonColor = 12500670
      Images = SVGIconImageList
      ParentDoubleBuffered = False
      RegularButtonColor = clNone
      SelectedButtonColor = clNone
      TabOrder = 0
    end
    object catSettings: TCategoryButtons
      Left = 0
      Top = 554
      Width = 200
      Height = 73
      Align = alBottom
      BorderStyle = bsNone
      ButtonFlow = cbfVertical
      ButtonHeight = 36
      ButtonWidth = 36
      ButtonOptions = [boCaptionOnlyBorder]
      Categories = <
        item
          Caption = 'Settings'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Caption = 'Home'
              Hint = 'Home action...'
              ImageName = 'home'
            end
            item
              Caption = 'Change Style'
              Hint = 'Cambia stile'
              ImageIndex = 0
              ImageName = 'Style'
            end
            item
              Caption = 'Settings'
              ImageName = 'cog'
              OnClick = actSettingsExecute
            end>
        end>
      HotButtonColor = 15974029
      Images = SVGIconImageList
      RegularButtonColor = clNone
      SelectedButtonColor = clNone
      TabOrder = 1
      OnCategoryCollapase = CatPreventCollapase
    end
    object MenuButtonToolbar: TToolBar
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 194
      Height = 36
      AutoSize = True
      ButtonHeight = 36
      ButtonWidth = 36
      Color = clYellow
      DrawingStyle = dsGradient
      GradientEndColor = clBtnFace
      GradientStartColor = clBtnFace
      Images = SVGIconImageList
      ParentColor = False
      TabOrder = 2
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
  object svSettings: TSplitView
    Left = 731
    Top = 42
    Width = 300
    Height = 627
    AnimationDelay = 10
    AnimationStep = 30
    Color = 15000804
    CompactWidth = 44
    OpenedWidth = 300
    ParentDoubleBuffered = True
    Placement = svpRight
    TabOrder = 2
    OnClosed = svSettingsClosed
    OnClosing = svSettingsClosing
    OnOpened = svSettingsOpened
    object catMenuSettings: TCategoryButtons
      Left = 0
      Top = 0
      Width = 300
      Height = 249
      Align = alTop
      BorderStyle = bsNone
      ButtonFlow = cbfVertical
      ButtonHeight = 36
      ButtonWidth = 36
      ButtonOptions = [boFullSize, boShowCaptions, boCaptionOnlyBorder]
      Categories = <
        item
          Color = clNone
          Collapsed = False
          Items = <
            item
              Caption = 'Menu'
              Hint = 'Menu settings'
              ImageName = 'view-dashboard'
              OnClick = actViewOptionsExecute
            end
            item
              Caption = 'Icons'
              ImageName = 'account-box-multiple-outline'
              OnClick = acIconFontsExecute
            end
            item
              Caption = 'Animate'
              ImageName = 'play-box-outline'
              OnClick = actAnimateExecute
            end
            item
              Caption = 'Log'
              ImageName = 'text-box-search-outline'
            end>
        end>
      Color = clBtnFace
      HotButtonColor = 12500670
      RegularButtonColor = clNone
      SelectedButtonColor = clNone
      TabOrder = 0
      OnCategoryCollapase = CatPreventCollapase
    end
    object pnlSettings: TPanel
      Left = 0
      Top = 249
      Width = 300
      Height = 378
      Align = alClient
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 1
      Visible = False
      object pcSettings: TPageControl
        Left = 0
        Top = 67
        Width = 300
        Height = 311
        Margins.Top = 60
        ActivePage = tsStyle
        Align = alClient
        TabOrder = 0
        object tsStyle: TTabSheet
          Caption = 'Menu'
          DesignSize = (
            292
            283)
          object grpDisplayMode: TRadioGroup
            Left = 2
            Top = 6
            Width = 286
            Height = 42
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Display Mode'
            Columns = 2
            ItemIndex = 0
            Items.Strings = (
              'Docked'
              'Overlay')
            TabOrder = 0
            OnClick = grpDisplayModeClick
          end
          object grpCloseStyle: TRadioGroup
            Left = 3
            Top = 65
            Width = 286
            Height = 42
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Close Style'
            Columns = 2
            ItemIndex = 1
            Items.Strings = (
              'Collapse'
              'Compact')
            TabOrder = 1
            OnClick = grpCloseStyleClick
          end
          object grpPlacement: TRadioGroup
            Left = 2
            Top = 124
            Width = 287
            Height = 46
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Placement'
            Columns = 2
            ItemIndex = 0
            Items.Strings = (
              'Left'
              'Right')
            TabOrder = 2
            OnClick = grpPlacementClick
          end
        end
        object tsIconFonts: TTabSheet
          Caption = 'Icons'
          ImageIndex = 4
          DesignSize = (
            292
            283)
          object Label3: TLabel
            Left = 26
            Top = 24
            Width = 68
            Height = 13
            Caption = 'Icon Font Size'
          end
          object IconFontsSizeLabel: TLabel
            Left = 128
            Top = 24
            Width = 12
            Height = 13
            Caption = '24'
          end
          object IconFontsTrackBar: TTrackBar
            Left = 10
            Top = 64
            Width = 282
            Height = 36
            Anchors = [akLeft, akTop, akRight]
            Max = 64
            Min = 16
            Position = 24
            TabOrder = 0
            OnChange = IconFontsTrackBarChange
          end
          object IconsToggleSwitch: TToggleSwitch
            Left = 14
            Top = 106
            Width = 132
            Height = 20
            StateCaptions.CaptionOn = 'Colored Icons'
            StateCaptions.CaptionOff = 'Greyscale Icons'
            TabOrder = 1
            OnClick = IconsToggleSwitchClick
          end
        end
        object tsAnimation: TTabSheet
          Caption = 'Animation'
          ImageIndex = 1
          DesignSize = (
            292
            283)
          object lblAnimationDelay: TLabel
            Left = 11
            Top = 41
            Width = 100
            Height = 13
            Caption = 'Animation Delay (15)'
          end
          object lblAnimationStep: TLabel
            Left = 11
            Top = 108
            Width = 95
            Height = 13
            Caption = 'Animation Step (20)'
          end
          object tswAnimation: TToggleSwitch
            Left = 9
            Top = 9
            Width = 72
            Height = 20
            State = tssOn
            TabOrder = 0
            OnClick = tswAnimationClick
          end
          object trkAnimationDelay: TTrackBar
            Left = 3
            Top = 62
            Width = 282
            Height = 36
            Anchors = [akLeft, akTop, akRight]
            Max = 15
            Min = 1
            Position = 3
            TabOrder = 1
            OnChange = trkAnimationDelayChange
          end
          object trkAnimationStep: TTrackBar
            Left = 3
            Top = 129
            Width = 282
            Height = 33
            Anchors = [akLeft, akTop, akRight]
            Max = 15
            Min = 1
            Position = 4
            TabOrder = 2
            OnChange = trkAnimationStepChange
          end
          object tsvDisplayMode: TToggleSwitch
            Left = 9
            Top = 177
            Width = 94
            Height = 20
            StateCaptions.CaptionOn = 'Overlay'
            StateCaptions.CaptionOff = 'Docked'
            TabOrder = 3
            OnClick = tsvDisplayModeClick
          end
          object ttsCloseStyle: TToggleSwitch
            Left = 9
            Top = 209
            Width = 98
            Height = 20
            StateCaptions.CaptionOn = 'Collapse'
            StateCaptions.CaptionOff = 'Compact'
            TabOrder = 4
            OnClick = ttsCloseStyleClick
          end
          object ttsCloseSplitView: TToggleSwitch
            Left = 9
            Top = 241
            Width = 153
            Height = 20
            State = tssOn
            StateCaptions.CaptionOn = 'Auto close menu'
            StateCaptions.CaptionOff = 'Leave menu opened'
            TabOrder = 5
            OnClick = ttsCloseStyleClick
          end
        end
        object tsLog: TTabSheet
          Caption = 'Log'
          ImageIndex = 2
          object lstLog: TListBox
            Left = 0
            Top = 0
            Width = 292
            Height = 283
            Align = alClient
            ItemHeight = 13
            ParentColor = True
            TabOrder = 0
          end
        end
      end
      object catPanelSettings: TCategoryButtons
        Left = 0
        Top = 0
        Width = 300
        Height = 67
        Align = alTop
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ButtonFlow = cbfVertical
        ButtonHeight = 36
        ButtonWidth = 36
        ButtonOptions = [boFullSize, boShowCaptions, boCaptionOnlyBorder]
        Categories = <
          item
            Caption = 'Settings'
            Color = clNone
            Collapsed = False
            Items = <
              item
                Caption = 'Back'
                ImageIndex = 8
                ImageName = 'arrow-left'
                OnClick = actBackExecute
              end>
          end>
        GradientDirection = gdVertical
        HotButtonColor = 15974029
        RegularButtonColor = clWhite
        SelectedButtonColor = 15132390
        TabOrder = 1
        OnCategoryCollapase = CatPreventCollapase
      end
    end
  end
  object ActionList: TActionList
    Images = SVGIconImageList
    Left = 396
    Top = 188
    object actMenu: TAction
      Hint = 'Collapse'
      ImageIndex = 24
      ImageName = 'menu'
      OnExecute = actMenuExecute
    end
    object actSettings: TAction
      Caption = 'Settings'
      ImageIndex = 21
      ImageName = 'settings'
      OnExecute = actSettingsExecute
    end
    object actViewOptions: TAction
      Caption = 'Menu'
      Hint = 'Menu settings'
      ImageName = 'dashboard'
      OnExecute = actViewOptionsExecute
    end
    object actBack: TAction
      Caption = 'Back'
      ImageIndex = 27
      ImageName = 'left'
      OnExecute = actBackExecute
    end
    object actAnimate: TAction
      Caption = 'Animate'
      ImageName = 'play'
      OnExecute = actAnimateExecute
    end
    object acFont: TAction
      Caption = 'Font'
      ImageName = 'font'
    end
    object acApplyFont: TAction
      Caption = 'Save'
      Hint = 'Save application font...'
      ImageName = 'check'
    end
    object acStyleSelection: TAction
      Category = 'Help'
      Caption = 'Change Style'
      Hint = 'Cambia stile'
      ImageIndex = 0
      ImageName = 'Style'
      OnExecute = acStyleSelectionExecute
    end
    object acViewStatusBar: TAction
      Category = 'View'
      Caption = 'Status Bar'
    end
    object acNewFile: TAction
      Category = 'File'
      Caption = 'New ...'
      Hint = 'New SVG File'
      ImageIndex = 2
      ImageName = 'New'
    end
    object acOpenFile: TAction
      Category = 'File'
      Caption = 'Open ...'
      Hint = 'Open SVG File...'
      ImageIndex = 1
      ImageName = 'Open'
    end
    object acEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Enabled = False
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 10
      ImageName = 'Cut'
      ShortCut = 16472
    end
    object acSearch: TAction
      Category = 'Edit'
      Caption = 'Search ...'
      Hint = 'Search into text...'
      ImageIndex = 6
      ImageName = 'Search'
      ShortCut = 16454
    end
    object acSearchAgain: TAction
      Category = 'Edit'
      Caption = 'Repeat search'
      Hint = 'Repeat last search'
      ImageIndex = 7
      ImageName = 'Search-repeat'
      ShortCut = 114
    end
    object acClose: TAction
      Category = 'File'
      Caption = 'Close'
      Hint = 'Close File'
      ImageIndex = 3
      ImageName = 'Close'
      ShortCut = 16499
    end
    object acEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Enabled = False
      Hint = 'Copies the selection and puts it on the Clipboard'
      ImageIndex = 8
      ImageName = 'Copy'
      ShortCut = 16451
    end
    object acEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Enabled = False
      Hint = 'Inserts Clipboard contents'
      ImageIndex = 9
      ImageName = 'Paste'
      ShortCut = 16470
    end
    object acEditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Selects the entire document'
      ImageIndex = 18
      ImageName = 'Select-all'
      ShortCut = 16449
    end
    object acEditUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Reverts the last action'
      ImageIndex = 11
      ImageName = 'Undo'
      ShortCut = 16474
    end
    object acSave: TAction
      Category = 'File'
      Caption = 'Save'
      Hint = 'Save File'
      ImageIndex = 12
      ImageName = 'Save'
      ShortCut = 16467
    end
    object acReplace: TAction
      Category = 'Edit'
      Caption = 'Replace ...'
      Hint = 'Search and Replace'
      ImageIndex = 20
      ImageName = 'replace'
      ShortCut = 16466
    end
    object acQuit: TAction
      Category = 'File'
      Caption = 'Quit'
      Hint = 'Close application'
      ImageIndex = 22
      ImageName = 'exit'
      ShortCut = 16472
    end
    object acAbout: TAction
      Category = 'Help'
      Caption = 'About ...'
      Hint = 'About XMLEditor'
      OnExecute = acAboutExecute
    end
    object acCloseAll: TAction
      Category = 'File'
      Caption = 'Close All'
      Hint = 'Close all files'
      ImageIndex = 4
      ImageName = 'Close-all'
    end
    object acSaveAll: TAction
      Category = 'File'
      Caption = 'Save All'
      Hint = 'Save all changes'
      ImageIndex = 13
      ImageName = 'Save-all'
    end
    object actnPrint: TAction
      Category = 'File'
      Caption = 'Print file ...'
      Hint = 'Print file'
      ImageIndex = 15
      ImageName = 'Print'
    end
    object actnPrinterSetup: TAction
      Category = 'Settings'
      Caption = 'Printer Setup ...'
      Hint = 'Printer Setup'
      ImageIndex = 17
      ImageName = 'Print-settings'
    end
    object actnPrintPreview: TAction
      Category = 'File'
      Caption = 'Print Preview'
      Hint = 'Print Preview'
      ImageIndex = 16
      ImageName = 'print-preview'
    end
    object actnPageSetup: TAction
      Category = 'Settings'
      Caption = 'Page Setup ...'
      Hint = 'Printer Page Setup'
      ImageIndex = 25
      ImageName = 'Minus'
    end
    object actnEditOptions: TAction
      Category = 'Settings'
      Caption = 'Editor Options ...'
      Hint = 'Editor Options'
      ImageIndex = 49
    end
    object actnEnlargeFont: TAction
      Category = 'Settings'
      Caption = 'Enlarge Font'
      Hint = 'Enlarge Font'
      ImageIndex = 63
    end
    object actnReduceFont: TAction
      Category = 'Settings'
      Caption = 'Reduce Font'
      Hint = 'Reduce Font'
      ImageIndex = 64
    end
    object actnSaveAs: TAction
      Category = 'File'
      Caption = 'Save As ...'
      Hint = 'Save As ...'
      ImageIndex = 14
      ImageName = 'Save-as'
    end
    object actnColorSettings: TAction
      Category = 'Settings'
      Caption = 'Color settings'
      Hint = 'Color settings (syntax highligthning)'
      ImageIndex = 55
    end
    object actnFormatXML: TAction
      Category = 'Edit'
      Caption = 'Reformat text'
      Hint = 'Reformat text'
      ImageIndex = 19
      ImageName = 'Reformat'
      ShortCut = 16467
    end
  end
  object FileOpenDialog: TFileOpenDialog
    DefaultExtension = '*.gif'
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 632
    Top = 424
  end
  object SVGIconImageList: TVirtualImageList
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
        CollectionIndex = 31
        CollectionName = 'left'
        Disabled = False
        Name = 'left'
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
      end>
    ImageCollection = dmResources.SVGIconImageCollection
    Width = 24
    Height = 24
    Left = 512
    Top = 344
  end
end
