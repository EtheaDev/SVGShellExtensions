object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Preview Handler Host Demo'
  ClientHeight = 559
  ClientWidth = 960
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 417
    Top = 0
    Width = 4
    Height = 559
    AutoSnap = False
    ExplicitLeft = 217
    ExplicitHeight = 338
  end
  object Panel1: TPanel
    Left = 421
    Top = 0
    Width = 539
    Height = 559
    Align = alClient
    TabOrder = 0
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 417
    Height = 559
    Align = alLeft
    TabOrder = 1
    object ShellListView1: TShellListView
      Left = 1
      Top = 1
      Width = 415
      Height = 557
      ObjectTypes = [otFolders, otNonFolders]
      Root = 'D:\ETHEA\SVGIconImageList\Demo\NewSydneyVectors'
      Sorted = True
      Align = alClient
      ReadOnly = False
      HideSelection = False
      OnChange = ShellListView1Change
      TabOrder = 0
    end
  end
end
