object TestPrintPreviewDlg: TTestPrintPreviewDlg
  Left = 297
  Top = 170
  Caption = 'Print Preview'
  ClientHeight = 381
  ClientWidth = 500
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 500
    Height = 23
    AutoSize = True
    ButtonHeight = 23
    ButtonWidth = 24
    Caption = 'ToolBar'
    Images = dmFileSupported.imgActions
    Indent = 5
    TabOrder = 0
    object FirstBtn: TToolButton
      Left = 5
      Top = 0
      Action = FirstCmd
      ParentShowHint = False
      ShowHint = True
    end
    object PrevBtn: TToolButton
      Left = 29
      Top = 0
      Action = PrevCmd
      ParentShowHint = False
      ShowHint = True
    end
    object NextBtn: TToolButton
      Left = 53
      Top = 0
      Action = NextCmd
      ParentShowHint = False
      ShowHint = True
    end
    object LastBtn: TToolButton
      Left = 77
      Top = 0
      Action = LastCmd
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton1: TToolButton
      Left = 101
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 109
      Top = 0
      Action = ZoomCmd
      DropdownMenu = PopupMenu
      ParentShowHint = False
      ShowHint = True
      Style = tbsDropDown
    end
    object ToolButton5: TToolButton
      Left = 148
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object PrintBtn: TToolButton
      Left = 156
      Top = 0
      Action = PrintCmd
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton4: TToolButton
      Left = 180
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object CloseBtn: TToolButton
      Left = 188
      Top = 0
      Action = CloseCmd
      ParentShowHint = False
      ShowHint = True
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 362
    Width = 500
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
    Left = 0
    Top = 23
    Width = 500
    Height = 339
    ScaleMode = pscPageWidth
    OnMouseDown = SynEditPrintPreviewMouseDown
    OnPreviewPage = SynEditPrintPreviewPreviewPage
  end
  object ActionList: TActionList
    Images = dmFileSupported.imgActions
    Left = 264
    Top = 63
    object FirstCmd: TAction
      Caption = 'FirstCmd'
      Hint = '|Go to first page'
      ImageIndex = 68
      ShortCut = 32838
      OnExecute = FirstCmdExecute
    end
    object PrevCmd: TAction
      Caption = 'PrevCmd'
      Hint = 'Go to previous page'
      ImageIndex = 0
      ShortCut = 32848
      OnExecute = PrevCmdExecute
    end
    object NextCmd: TAction
      Caption = 'NextCmd'
      Hint = 'Go to next page'
      ImageIndex = 1
      ShortCut = 32846
      OnExecute = NextCmdExecute
    end
    object LastCmd: TAction
      Caption = 'LastCmd'
      Hint = 'Go to last page'
      ImageIndex = 69
      ShortCut = 32844
      OnExecute = LastCmdExecute
    end
    object ZoomCmd: TAction
      Caption = 'FitCmd'
      Hint = 'Zoom In/Out'
      ImageIndex = 53
      ShortCut = 32858
      OnExecute = ZoomCmdExecute
    end
    object PrintCmd: TAction
      Caption = 'PrintCmd'
      Hint = 'Print the document'
      ImageIndex = 19
      ShortCut = 16464
      OnExecute = PrintCmdExecute
    end
    object CloseCmd: TAction
      Caption = 'CloseCmd'
      Hint = 'Close Print Preview'
      ImageIndex = 61
      ShortCut = 32835
      OnExecute = CloseCmdExecute
    end
  end
  object PopupMenu: TPopupMenu
    Images = dmFileSupported.imgActions
    Left = 180
    Top = 65
    object Fitto1: TMenuItem
      Tag = -1
      Caption = 'Whole page'
      OnClick = Fitto1Click
    end
    object Pagewidth1: TMenuItem
      Tag = -2
      Caption = 'Page width'
      OnClick = Fitto1Click
    end
    object N1: TMenuItem
      Caption = '-'
      OnClick = Fitto1Click
    end
    object N251: TMenuItem
      Tag = 25
      Caption = '25%'
      OnClick = Fitto1Click
    end
    object N501: TMenuItem
      Tag = 50
      Caption = '50%'
      OnClick = Fitto1Click
    end
    object N1001: TMenuItem
      Tag = 100
      Caption = '100%'
      OnClick = Fitto1Click
    end
    object N2001: TMenuItem
      Tag = 200
      Caption = '200%'
      OnClick = Fitto1Click
    end
    object N4001: TMenuItem
      Tag = 400
      Caption = '400%'
      OnClick = Fitto1Click
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnHint = ApplicationEventsHint
    Left = 345
    Top = 65
  end
end
