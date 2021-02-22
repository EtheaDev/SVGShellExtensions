object TestPrintPreviewDlg: TTestPrintPreviewDlg
  Left = 297
  Top = 170
  Caption = 'Print Preview'
  ClientHeight = 596
  ClientWidth = 1018
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 577
    Width = 1018
    Height = 19
    Panels = <
      item
        Width = 400
      end
      item
        Width = 100
      end>
  end
  object SynEditPrintPreview: TSynEditPrintPreview
    Left = 145
    Top = 41
    Width = 873
    Height = 536
    ScaleMode = pscPageWidth
    OnMouseDown = SynEditPrintPreviewMouseDown
    OnPreviewPage = SynEditPrintPreviewPreviewPage
  end
  object TitlePanel: TPanel
    Left = 0
    Top = 0
    Width = 1018
    Height = 41
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = '  SVG Text Print preview'
    TabOrder = 2
  end
  object catMenuItems: TCategoryButtons
    Left = 0
    Top = 41
    Width = 145
    Height = 536
    Align = alLeft
    BackgroundGradientDirection = gdVertical
    BorderStyle = bsNone
    ButtonFlow = cbfVertical
    ButtonHeight = 36
    ButtonWidth = 36
    ButtonOptions = [boFullSize, boShowCaptions, boCaptionOnlyBorder]
    Categories = <
      item
        Caption = 'Navigate'
        Color = clNone
        Collapsed = False
        Items = <
          item
            Action = PrevCmd
          end
          item
            Action = NextCmd
          end>
      end
      item
        Caption = 'Page View'
        Color = clNone
        Collapsed = False
        Items = <
          item
            Action = WholePageAction
          end
          item
            Action = PageWidthAction
          end>
      end
      item
        Caption = 'Print'
        Color = clNone
        Collapsed = False
        Items = <
          item
            Action = PrintCmd
          end
          item
            Action = CloseCmd
          end>
      end>
    DoubleBuffered = True
    HotButtonColor = 12500670
    Images = VirtualImageList
    ParentDoubleBuffered = False
    RegularButtonColor = clNone
    SelectedButtonColor = clNone
    TabOrder = 3
  end
  object ActionList: TActionList
    Images = VirtualImageList
    Left = 336
    Top = 159
    object PrevCmd: TAction
      Caption = 'Previous page'
      Hint = 'Go to previous page'
      ImageIndex = 0
      ImageName = 'left'
      ShortCut = 32848
      OnExecute = PrevCmdExecute
    end
    object NextCmd: TAction
      Caption = 'Next page'
      Hint = 'Go to next page'
      ImageIndex = 1
      ImageName = 'right'
      ShortCut = 32846
      OnExecute = NextCmdExecute
    end
    object ZoomCmd: TAction
      Caption = 'Zoom %'
      Hint = 'Zoom In/Out'
      ImageIndex = 2
      ImageName = 'binoculars'
      ShortCut = 32858
    end
    object PrintCmd: TAction
      Caption = 'Print...'
      Hint = 'Print the document'
      ImageIndex = 3
      ImageName = 'Print'
      ShortCut = 16464
      OnExecute = PrintCmdExecute
    end
    object CloseCmd: TAction
      Caption = 'Exit'
      Hint = 'Close Print Preview'
      ImageIndex = 4
      ImageName = 'Exit'
      ShortCut = 32837
      OnExecute = CloseCmdExecute
    end
    object WholePageAction: TAction
      Caption = 'Whole page'
      ImageIndex = 5
      ImageName = 'whole-page'
      OnExecute = WholePageActionExecute
    end
    object PageWidthAction: TAction
      Caption = 'Page width'
      ImageIndex = 6
      ImageName = 'page-width'
      OnExecute = PageWidthActionExecute
    end
    object Zoom25Action: TAction
      Caption = 'Zoom 25%'
      ImageIndex = 6
      ImageName = 'page-width'
      OnExecute = Zoom25ActionExecute
    end
  end
  object PopupMenu: TPopupMenu
    Left = 276
    Top = 121
    object Fitto1: TMenuItem
      Tag = -1
      Caption = 'Whole page'
    end
    object Pagewidth1: TMenuItem
      Tag = -2
      Caption = 'Page width'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object N251: TMenuItem
      Tag = 25
      Caption = '25%'
    end
    object N501: TMenuItem
      Tag = 50
      Caption = '50%'
    end
    object N1001: TMenuItem
      Tag = 100
      Caption = '100%'
    end
    object N2001: TMenuItem
      Tag = 200
      Caption = '200%'
    end
    object N4001: TMenuItem
      Tag = 400
      Caption = '400%'
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnHint = ApplicationEventsHint
    Left = 345
    Top = 65
  end
  object VirtualImageList: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
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
        CollectionIndex = 15
        CollectionName = 'Print'
        Disabled = False
        Name = 'Print'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Exit'
        Disabled = False
        Name = 'Exit'
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
      end>
    ImageCollection = dmResources.SVGIconImageCollection
    Width = 32
    Height = 32
    Left = 248
    Top = 200
  end
end
