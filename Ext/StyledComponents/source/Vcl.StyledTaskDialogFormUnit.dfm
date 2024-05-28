object StyledTaskDialogForm: TStyledTaskDialogForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 244
  ClientWidth = 992
  Color = clWindow
  Constraints.MinHeight = 200
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 15
  object BottomBevel: TBevel
    Left = 0
    Top = 206
    Width = 992
    Height = 3
    Align = alBottom
    Shape = bsBottomLine
  end
  object FooterPanel: TPanel
    Left = 0
    Top = 209
    Width = 992
    Height = 35
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkFlat
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object FooterIconPanel: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 25
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
    end
    object FooterTextLabel: TLinkLabel
      AlignWithMargins = True
      Left = 37
      Top = 8
      Width = 951
      Height = 17
      Margins.Left = 4
      Margins.Top = 8
      Margins.Right = 4
      Margins.Bottom = 8
      Align = alClient
      Caption = 'FooterTextLabel'
      TabOrder = 1
      OnLinkClick = TextLabelLinkClick
    end
  end
  object CenterPanel: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 984
    Height = 158
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 1
    object ImagePanel: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 64
      Height = 150
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object IconContainer: TPanel
        Left = 0
        Top = 0
        Width = 64
        Height = 64
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object MessageScrollBox: TScrollBox
      Left = 72
      Top = 0
      Width = 912
      Height = 158
      VertScrollBar.Visible = False
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 1
      object TitleLabel: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 46
        Height = 15
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        Caption = 'Title Text'
        WordWrap = True
      end
      object AutoSizeLabel: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 27
        Width = 904
        Height = 13
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        AutoSize = False
        Caption = 'AutoSizeLabel'
        WordWrap = True
      end
      object TextLabel: TLinkLabel
        AlignWithMargins = True
        Left = 4
        Top = 46
        Width = 840
        Height = 15
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        BevelInner = bvNone
        BevelOuter = bvNone
        Caption = 'Text Label'
        TabOrder = 0
        OnLinkClick = TextLabelLinkClick
      end
    end
  end
  object ButtonsPanel: TPanel
    Left = 0
    Top = 166
    Width = 992
    Height = 40
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    DoubleBuffered = False
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 2
    object YesButton: TStyledButton
      AlignWithMargins = True
      Left = 35
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'Yes'
      ModalResult = 6
      TabOrder = 0
      OnClick = ButtonClick
    end
    object NoButton: TStyledButton
      AlignWithMargins = True
      Left = 115
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'No'
      ModalResult = 7
      TabOrder = 1
      OnClick = ButtonClick
    end
    object OKButton: TStyledButton
      AlignWithMargins = True
      Left = 195
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 2
      OnClick = ButtonClick
    end
    object CancelButton: TStyledButton
      AlignWithMargins = True
      Left = 275
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
      OnClick = ButtonClick
    end
    object AbortButton: TStyledButton
      AlignWithMargins = True
      Left = 355
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'Abort'
      ModalResult = 3
      TabOrder = 4
      OnClick = ButtonClick
    end
    object RetryButton: TStyledButton
      AlignWithMargins = True
      Left = 435
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'Retry'
      ModalResult = 4
      TabOrder = 5
      OnClick = ButtonClick
    end
    object IgnoreButton: TStyledButton
      AlignWithMargins = True
      Left = 515
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'Ignore'
      ModalResult = 5
      TabOrder = 6
      OnClick = ButtonClick
    end
    object AllButton: TStyledButton
      AlignWithMargins = True
      Left = 595
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'All'
      ModalResult = 12
      TabOrder = 7
      OnClick = ButtonClick
    end
    object NoToAllButton: TStyledButton
      AlignWithMargins = True
      Left = 675
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'No to All'
      ModalResult = 13
      TabOrder = 8
      OnClick = ButtonClick
    end
    object YesToAllButton: TStyledButton
      AlignWithMargins = True
      Left = 755
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'Yes to All'
      ModalResult = 14
      TabOrder = 9
      OnClick = ButtonClick
    end
    object HelpButton: TStyledButton
      AlignWithMargins = True
      Left = 835
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'Help'
      ModalResult = 9
      TabOrder = 10
      OnClick = HelpButtonClick
    end
    object CloseButton: TStyledButton
      AlignWithMargins = True
      Left = 915
      Top = 4
      Width = 74
      Height = 32
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'Close'
      ModalResult = 8
      TabOrder = 11
      OnClick = ButtonClick
    end
  end
end
