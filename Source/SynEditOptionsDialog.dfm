object fmEditorOptionsDialog: TfmEditorOptionsDialog
  Left = 580
  Top = 154
  BorderStyle = bsDialog
  Caption = 'Editor Options'
  ClientHeight = 393
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 369
    Height = 359
    ActivePage = Display
    Align = alClient
    TabOrder = 0
    object Display: TTabSheet
      Caption = 'Display'
      object gbRightEdge: TGroupBox
        Left = 8
        Top = 136
        Width = 159
        Height = 88
        Caption = 'Right Edge'
        TabOrder = 1
        object lblEdgeColumn: TLabel
          Left = 4
          Top = 26
          Width = 76
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Edge Column:'
        end
        object eRightEdge: TEdit
          Left = 84
          Top = 23
          Width = 62
          Height = 21
          TabOrder = 0
          Text = '0'
        end
      end
      object gbGutter: TGroupBox
        Left = 8
        Top = 8
        Width = 343
        Height = 121
        Caption = 'Gutter'
        TabOrder = 0
        object ckGutterAutosize: TCheckBox
          Left = 9
          Top = 37
          Width = 134
          Height = 17
          Caption = 'Autosize'
          TabOrder = 1
        end
        object ckGutterShowLineNumbers: TCheckBox
          Left = 9
          Top = 56
          Width = 134
          Height = 17
          Caption = 'Show line numbers'
          TabOrder = 2
        end
        object ckGutterShowLeaderZeros: TCheckBox
          Left = 9
          Top = 94
          Width = 134
          Height = 17
          Caption = 'Show leading zeros'
          TabOrder = 4
        end
        object ckGutterStartAtZero: TCheckBox
          Left = 9
          Top = 75
          Width = 134
          Height = 17
          Caption = 'Start at zero'
          TabOrder = 3
        end
        object ckGutterVisible: TCheckBox
          Left = 9
          Top = 18
          Width = 134
          Height = 17
          Caption = 'Visible'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
      end
      object gbBookmarks: TGroupBox
        Left = 8
        Top = 232
        Width = 159
        Height = 79
        Caption = 'Bookmarks'
        TabOrder = 3
        object ckBookmarkKeys: TCheckBox
          Left = 9
          Top = 24
          Width = 130
          Height = 17
          Caption = 'Bookmark keys'
          TabOrder = 0
        end
        object ckBookmarkVisible: TCheckBox
          Left = 9
          Top = 48
          Width = 130
          Height = 17
          Caption = 'Bookmarks visible'
          TabOrder = 1
        end
      end
      object gbEditorFont: TGroupBox
        Left = 180
        Top = 230
        Width = 172
        Height = 79
        Caption = 'Editor Font'
        TabOrder = 4
        object btnFont: TButton
          Left = 72
          Top = 49
          Width = 84
          Height = 25
          Caption = 'Font'
          TabOrder = 1
          OnClick = btnFontClick
        end
        object pnlEditorFont: TPanel
          Left = 8
          Top = 19
          Width = 143
          Height = 30
          BevelOuter = bvNone
          TabOrder = 0
          object lblFont: TLabel
            Left = 2
            Top = 1
            Width = 91
            Height = 15
            Caption = 'Consolas 10pt'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
          end
        end
      end
      object gbLineSpacing: TGroupBox
        Left = 180
        Top = 136
        Width = 172
        Height = 88
        Caption = 'Line spacing / Tab spacing'
        TabOrder = 2
        object lblExtraLines: TLabel
          Left = 14
          Top = 27
          Width = 76
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Extra Lines:'
        end
        object lblTabWidth: TLabel
          Left = 14
          Top = 56
          Width = 76
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Tab Width:'
        end
        object eLineSpacing: TEdit
          Left = 94
          Top = 23
          Width = 62
          Height = 21
          TabOrder = 0
          Text = '0'
        end
        object eTabWidth: TEdit
          Left = 94
          Top = 53
          Width = 62
          Height = 21
          TabOrder = 1
          Text = '8'
        end
      end
    end
    object Options: TTabSheet
      Caption = 'Options'
      object gbOptions: TGroupBox
        Left = 8
        Top = 0
        Width = 346
        Height = 247
        Caption = 'Options'
        TabOrder = 0
        object ckAutoIndent: TCheckBox
          Left = 9
          Top = 15
          Width = 160
          Height = 17
          Hint = 
            'Will indent the caret on new lines with the same amount of leadi' +
            'ng white space as the preceding line'
          Caption = 'Auto indent'
          TabOrder = 0
        end
        object ckDragAndDropEditing: TCheckBox
          Left = 9
          Top = 53
          Width = 160
          Height = 17
          Hint = 
            'Allows you to select a block of text and drag it within the docu' +
            'ment to another location'
          Caption = 'Drag and drop editing'
          TabOrder = 2
        end
        object ckAutoSizeMaxWidth: TCheckBox
          Left = 9
          Top = 34
          Width = 160
          Height = 17
          Hint = 'Allows the editor accept OLE file drops'
          Caption = 'Auto size scroll width'
          TabOrder = 1
        end
        object ckHalfPageScroll: TCheckBox
          Left = 176
          Top = 15
          Width = 160
          Height = 17
          Hint = 
            'When scrolling with page-up and page-down commands, only scroll ' +
            'a half page at a time'
          Caption = 'Half page scroll'
          TabOrder = 12
        end
        object ckEnhanceEndKey: TCheckBox
          Left = 9
          Top = 186
          Width = 160
          Height = 17
          Hint = 'Makes it so the caret is never visible'
          Caption = 'Enhance End Key'
          TabOrder = 9
        end
        object ckScrollByOneLess: TCheckBox
          Left = 176
          Top = 34
          Width = 160
          Height = 17
          Hint = 'Forces scrolling to be one less'
          Caption = 'Scroll by one less'
          TabOrder = 13
        end
        object ckScrollPastEOF: TCheckBox
          Left = 176
          Top = 53
          Width = 160
          Height = 17
          Hint = 'Allows the cursor to go past the end of file marker'
          Caption = 'Scroll past end of file'
          TabOrder = 14
        end
        object ckScrollPastEOL: TCheckBox
          Left = 176
          Top = 72
          Width = 160
          Height = 17
          Hint = 
            'Allows the cursor to go past the last character into the white s' +
            'pace at the end of a line'
          Caption = 'Scroll past end of line'
          TabOrder = 15
        end
        object ckShowScrollHint: TCheckBox
          Left = 176
          Top = 91
          Width = 160
          Height = 17
          Hint = 
            'Shows a hint of the visible line numbers when scrolling vertical' +
            'ly'
          Caption = 'Show scroll hint'
          TabOrder = 16
        end
        object ckSmartTabs: TCheckBox
          Left = 9
          Top = 129
          Width = 160
          Height = 17
          Hint = 
            'When tabbing, the cursor will go to the next non-white space cha' +
            'racter of the previous line'
          Caption = 'Smart tabs'
          TabOrder = 6
        end
        object ckTabsToSpaces: TCheckBox
          Left = 176
          Top = 148
          Width = 160
          Height = 17
          Hint = 'Converts a tab character to the number of spaces in Tab Width'
          Caption = 'Tabs to spaces'
          TabOrder = 19
        end
        object ckTrimTrailingSpaces: TCheckBox
          Left = 176
          Top = 167
          Width = 160
          Height = 17
          Hint = 'Spaces at the end of lines will be trimmed and not saved'
          Caption = 'Trim trailing spaces'
          TabOrder = 20
        end
        object ckWantTabs: TCheckBox
          Left = 9
          Top = 110
          Width = 160
          Height = 17
          Hint = 
            'Let the editor accept tab characters instead of going to the nex' +
            't control'
          Caption = 'Want tabs'
          TabOrder = 5
        end
        object ckAltSetsColumnMode: TCheckBox
          Left = 9
          Top = 72
          Width = 160
          Height = 17
          Hint = 
            'Holding down the Alt Key will put the selection mode into column' +
            'ar format'
          Caption = 'Alt sets column mode'
          TabOrder = 3
        end
        object ckKeepCaretX: TCheckBox
          Left = 9
          Top = 91
          Width = 160
          Height = 17
          Hint = 
            'When moving through lines the X position will always stay the sa' +
            'me'
          Caption = 'Maintain caret column'
          TabOrder = 4
        end
        object ckScrollHintFollows: TCheckBox
          Left = 176
          Top = 110
          Width = 160
          Height = 17
          Hint = 'The scroll hint follows the mouse when scrolling vertically'
          Caption = 'Scroll hint follows mouse'
          TabOrder = 17
        end
        object ckGroupUndo: TCheckBox
          Left = 176
          Top = 186
          Width = 160
          Height = 17
          Hint = 
            'When undoing/redoing actions, handle all continous changes of th' +
            'e same kind in one call instead undoing/redoing each command sep' +
            'arately'
          Caption = 'Group undo'
          TabOrder = 21
        end
        object ckSmartTabDelete: TCheckBox
          Left = 9
          Top = 148
          Width = 160
          Height = 17
          Hint = 'similar to Smart Tabs, but when you delete characters'
          Caption = 'Smart tab delete'
          TabOrder = 7
        end
        object ckRightMouseMoves: TCheckBox
          Left = 176
          Top = 205
          Width = 160
          Height = 17
          Hint = 
            'When clicking with the right mouse for a popup menu, move the cu' +
            'rsor to that location'
          Caption = 'Right mouse moves cursor'
          TabOrder = 22
        end
        object ckEnhanceHomeKey: TCheckBox
          Left = 9
          Top = 167
          Width = 160
          Height = 17
          Hint = 'enhances home key positioning, similar to visual studio'
          Caption = 'Enhance Home Key'
          TabOrder = 8
        end
        object ckHideShowScrollbars: TCheckBox
          Left = 9
          Top = 205
          Width = 160
          Height = 17
          Hint = 
            'if enabled, then the scrollbars will only show when necessary.  ' +
            'If you have ScrollPastEOL, then it the horizontal bar will alway' +
            's be there (it uses MaxLength instead)'
          Caption = 'Hide scrollbars as necessary'
          TabOrder = 10
        end
        object ckDisableScrollArrows: TCheckBox
          Left = 9
          Top = 224
          Width = 160
          Height = 17
          Hint = 
            'Disables the scroll bar arrow buttons when you can'#39't scroll in t' +
            'hat direction any more'
          Caption = 'Disable scroll arrows'
          TabOrder = 11
        end
        object ckShowSpecialChars: TCheckBox
          Left = 176
          Top = 224
          Width = 160
          Height = 17
          Hint = 'Shows linebreaks, spaces and tabs using special symbols'
          Caption = 'Show special chars'
          TabOrder = 23
        end
        object ckTabIndent: TCheckBox
          Left = 176
          Top = 129
          Width = 160
          Height = 17
          Hint = 'Use tab for indention'
          Caption = 'Tab indent'
          TabOrder = 18
        end
      end
    end
    object Keystrokes: TTabSheet
      Caption = 'Keystrokes'
      object btnAddKey: TButton
        Left = 96
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Add'
        TabOrder = 2
        OnClick = btnAddKeyClick
      end
      object btnRemKey: TButton
        Left = 176
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Remove'
        TabOrder = 3
        OnClick = btnRemKeyClick
      end
      object gbKeyStrokes: TGroupBox
        Left = 8
        Top = 192
        Width = 346
        Height = 119
        Caption = 'Keystroke Options'
        TabOrder = 4
        object lblCommand: TLabel
          Left = 16
          Top = 28
          Width = 90
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Command:'
        end
        object lblKeystroke2: TLabel
          Left = 16
          Top = 91
          Width = 90
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Keystroke:'
        end
        object lblKeystroke: TLabel
          Left = 16
          Top = 59
          Width = 90
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Keystroke:'
        end
        object cKeyCommand: TComboBox
          Left = 120
          Top = 23
          Width = 186
          Height = 21
          TabOrder = 0
          OnExit = cKeyCommandExit
          OnKeyPress = cKeyCommandKeyPress
          OnKeyUp = cKeyCommandKeyUp
        end
      end
      object btnUpdateKey: TButton
        Left = 16
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Update'
        TabOrder = 1
        OnClick = btnUpdateKeyClick
      end
      object pnlCommands: TPanel
        Left = 8
        Top = 13
        Width = 346
        Height = 132
        BevelInner = bvRaised
        BevelOuter = bvLowered
        Caption = 'pnlCommands'
        TabOrder = 0
        object KeyList: TListView
          Left = 2
          Top = 2
          Width = 342
          Height = 128
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Command'
              Width = 160
            end
            item
              Caption = 'Keystroke'
              Width = 160
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnChanging = KeyListChanging
        end
      end
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 359
    Width = 369
    Height = 34
    Align = alBottom
    TabOrder = 1
    object btnOk: TButton
      Left = 203
      Top = 4
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 283
      Top = 4
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ColorDialog: TColorDialog
    Left = 24
    Top = 346
  end
  object ColorPopup: TPopupMenu
    Left = 64
    Top = 346
    object mnuNone: TMenuItem
      Tag = -1
      Caption = 'None'
    end
    object mnuScrollBar: TMenuItem
      Caption = 'Scrollbar'
    end
    object mnuBackground: TMenuItem
      Tag = 1
      Caption = 'Background'
    end
    object mnuActiveCaption: TMenuItem
      Tag = 2
      Caption = 'Active Caption'
    end
    object mnuInactiveCaption: TMenuItem
      Tag = 3
      Caption = 'Inactive Caption'
    end
    object mnuMenu: TMenuItem
      Tag = 4
      Caption = 'Menu'
    end
    object mnuWindow: TMenuItem
      Tag = 5
      Caption = 'Window'
    end
    object mnuWindowFrame: TMenuItem
      Tag = 6
      Caption = 'Window Frame'
    end
    object Menu2: TMenuItem
      Tag = 7
      Caption = 'Menu Text'
    end
    object mnuWindowText: TMenuItem
      Tag = 8
      Caption = 'Window Text'
    end
    object mnuCaptionText: TMenuItem
      Tag = 9
      Caption = 'Caption Text'
    end
    object mnuActiveBorder: TMenuItem
      Tag = 10
      Caption = 'Active Border'
    end
    object mnuInactiveBorder: TMenuItem
      Tag = 11
      Caption = 'Inactive Border'
    end
    object mnuApplicationWorkspace: TMenuItem
      Tag = 12
      Caption = 'Application Workspace'
    end
    object mnuHighlight: TMenuItem
      Tag = 13
      Caption = 'Highlight'
    end
    object mnuHighlightText: TMenuItem
      Tag = 14
      Caption = 'Highlight Text'
    end
    object mnuButtonFace: TMenuItem
      Tag = 15
      Caption = 'Button Face'
    end
    object mnuButtonShadow: TMenuItem
      Tag = 16
      Caption = 'Button Shadow'
    end
    object mnuGrayText: TMenuItem
      Tag = 17
      Caption = 'Gray Text'
    end
    object mnuButtonText: TMenuItem
      Tag = 18
      Caption = 'Button Text'
    end
    object mnuInactiveCaptionText: TMenuItem
      Tag = 19
      Caption = 'Inactive Caption Text'
    end
    object mnuHighlight2: TMenuItem
      Tag = 20
      Caption = 'Highlight'
    end
    object mnu3dDarkShadow: TMenuItem
      Tag = 21
      Caption = '3D Dark Shadow'
    end
    object mnu3DLight: TMenuItem
      Tag = 22
      Caption = '3D Light'
    end
    object mnuInfoTipText: TMenuItem
      Tag = 23
      Caption = 'Info Tip Text'
    end
    object mnuInfoTipBackground: TMenuItem
      Tag = 24
      Caption = 'Info Tip Background'
    end
  end
  object ImageList: TImageList
    Left = 104
    Top = 346
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdEffects, fdFixedPitchOnly]
    Left = 136
    Top = 346
  end
end
