object FrmAbout: TFrmAbout
  Left = 651
  Top = 323
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'About SVG Preview'
  ClientHeight = 316
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TitleLabel: TLabel
    Left = 190
    Top = 8
    Width = 116
    Height = 13
    Caption = 'SVG Preview - Freeware'
  end
  object LabelVersion: TLabel
    Left = 190
    Top = 34
    Width = 35
    Height = 13
    Caption = 'Version'
  end
  object Panel1: TPanel
    Left = 0
    Top = 265
    Width = 446
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object Button1: TButton
      Left = 360
      Top = 16
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button3: TButton
      Left = 8
      Top = 16
      Width = 125
      Height = 25
      Caption = 'Submit issue...'
      ImageIndex = 0
      TabOrder = 1
      OnClick = Button3Click
    end
    object btnCheckUpdates: TButton
      Left = 139
      Top = 16
      Width = 125
      Height = 25
      Caption = 'Check for updates'
      ImageIndex = 3
      TabOrder = 2
      Visible = False
      OnClick = btnCheckUpdatesClick
    end
  end
  object MemoCopyRights: TMemo
    Left = 8
    Top = 83
    Width = 427
    Height = 166
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object LinkLabel1: TLinkLabel
    Left = 8
    Top = 62
    Width = 267
    Height = 19
    Caption = 
      '<a href="https://github.com/EtheaDev/SVGShellExtensions">https:/' +
      '/github.com/EtheaDev/SVGShellExtensions</a>'
    TabOrder = 2
    UseVisualStyle = True
    OnClick = LinkLabel1Click
  end
  object SVGIconImage1: TSVGIconImage
    Left = 383
    Top = 30
    Width = 52
    Height = 49
    AutoSize = False
    SVGText = 
      '<svg xmlns="http://www.w3.org/2000/svg"'#13#10'     xmlns:xlink="http:' +
      '//www.w3.org/1999/xlink"'#13#10'     width="100%" '#13#10'     height="100%"' +
      #13#10'     viewBox="0 0 300 300">'#13#10#13#10'  <title>SVG Logo</title>'#13#10'  <d' +
      'esc>Designed for the SVG Logo Contest in 2006 by Harvey Rayner, ' +
      'and adopted by W3C in 2009. It is available under the Creative C' +
      'ommons license for those who have an SVG product or who are usin' +
      'g SVG on their site.</desc>'#13#10' '#13#10'   <metadata id="license">'#13#10'    ' +
      ' <rdf:RDF '#13#10'       xmlns:rdf="http://www.w3.org/1999/02/22-rdf-s' +
      'yntax-ns#"'#13#10'       xmlns:dc="http://purl.org/dc/elements/1.1/"'#13#10 +
      '       xmlns:cc="http://web.resource.org/cc/">'#13#10'       <cc:Work ' +
      'rdf:about="">'#13#10'         <dc:title>SVG Logo</dc:title>'#13#10'         ' +
      '<dc:date>14-08-2009</dc:date>'#13#10'         <dc:creator>'#13#10'          ' +
      ' <cc:Agent><dc:title>W3C</dc:title></cc:Agent>'#13#10'           <cc:A' +
      'gent><dc:title>Harvey Rayner, designer</dc:title></cc:Agent>'#13#10'  ' +
      '       </dc:creator>'#13#10'         <dc:description>See document desc' +
      'ription</dc:description>'#13#10'         <cc:license rdf:resource="htt' +
      'p://creativecommons.org/licenses/by-nc-sa/2.5/" />'#13#10'         <dc' +
      ':format>image/svg+xml</dc:format>'#13#10'         <dc:type rdf:resourc' +
      'e="http://purl.org/dc/dcmitype/StillImage" />'#13#10'       </cc:Work>' +
      #13#10'       <cc:License rdf:about="http://creativecommons.org/licen' +
      'ses/by-nc-sa/2.5/">'#13#10'         <cc:permits rdf:resource="http://w' +
      'eb.resource.org/cc/Reproduction" />'#13#10'         <cc:permits rdf:re' +
      'source="http://web.resource.org/cc/Distribution" />'#13#10'         <c' +
      'c:requires rdf:resource="http://web.resource.org/cc/Notice" />'#13#10 +
      '         <cc:requires rdf:resource="http://web.resource.org/cc/A' +
      'ttribution" />'#13#10'         <cc:prohibits rdf:resource="http://web.' +
      'resource.org/cc/CommercialUse" />'#13#10'         <cc:permits rdf:reso' +
      'urce="http://web.resource.org/cc/DerivativeWorks" />'#13#10'         <' +
      'cc:requires rdf:resource="http://web.resource.org/cc/ShareAlike"' +
      ' />'#13#10'       </cc:License>'#13#10'     </rdf:RDF>'#13#10'   </metadata>'#13#10'   '#13 +
      #10#13#10'   <defs>'#13#10'     <g id="SVG" fill="#ffffff" transform="scale(2' +
      ') translate(20,79)">'#13#10'        <path id="S" d="M 5.482,31.319 C2.' +
      '163,28.001 0.109,23.419 0.109,18.358 C0.109,8.232 8.322,0.024 18' +
      '.443,0.024 C28.569,0.024 36.782,8.232 36.782,18.358 L26.042,18.3' +
      '58 C26.042,14.164 22.638,10.765 18.443,10.765 C14.249,10.765 10.' +
      '850,14.164 10.850,18.358 C10.850,20.453 11.701,22.351 13.070,23.' +
      '721 L13.075,23.721 C14.450,25.101 15.595,25.500 18.443,25.952 L1' +
      '8.443,25.952 C23.509,26.479 28.091,28.006 31.409,31.324 L31.409,' +
      '31.324 C34.728,34.643 36.782,39.225 36.782,44.286 C36.782,54.412' +
      ' 28.569,62.625 18.443,62.625 C8.322,62.625 0.109,54.412 0.109,44' +
      '.286 L10.850,44.286 C10.850,48.480 14.249,51.884 18.443,51.884 C' +
      '22.638,51.884 26.042,48.480 26.042,44.286 C26.042,42.191 25.191,' +
      '40.298 23.821,38.923 L23.816,38.923 C22.441,37.548 20.468,37.074' +
      ' 18.443,36.697 L18.443,36.692 C13.533,35.939 8.800,34.638 5.482,' +
      '31.319 L5.482,31.319 L5.482,31.319 Z"/>'#13#10#13#10'        <path id="V" ' +
      'd="M 73.452,0.024 L60.482,62.625 L49.742,62.625 L36.782,0.024 L4' +
      '7.522,0.024 L55.122,36.687 L62.712,0.024 L73.452,0.024 Z"/>'#13#10#13#10' ' +
      '       <path id="G" d="M 91.792,25.952 L110.126,25.952 L110.126,' +
      '44.286 L110.131,44.286 C110.131,54.413 101.918,62.626 91.792,62.' +
      '626 C81.665,62.626 73.458,54.413 73.458,44.286 L73.458,44.286 L7' +
      '3.458,18.359 L73.453,18.359 C73.453,8.233 81.665,0.025 91.792,0.' +
      '025 C101.913,0.025 110.126,8.233 110.126,18.359 L99.385,18.359 C' +
      '99.385,14.169 95.981,10.765 91.792,10.765 C87.597,10.765 84.198,' +
      '14.169 84.198,18.359 L84.198,44.286 L84.198,44.286 C84.198,48.48' +
      '1 87.597,51.880 91.792,51.880 C95.981,51.880 99.380,48.481 99.38' +
      '5,44.291 L99.385,44.286 L99.385,36.698 L91.792,36.698 L91.792,25' +
      '.952 L91.792,25.952 Z"/>'#13#10'      </g>'#13#10'   </defs>'#13#10#13#10'   <path id=' +
      '"base" fill="#000" d="M8.5,150 H291.5 V250 C291.5,273.5 273.5,29' +
      '1.5 250,291.5 H50 C26.5,291.5 8.5,273.5 8.5,250 Z"/>'#13#10'   <g stro' +
      'ke-width="38.0086" stroke="#000">'#13#10'     <g id="svgstar" transfor' +
      'm="translate(150, 150)">'#13#10'       <path id="svgbar" fill="#ffb13b' +
      '" '#13#10'         d="M-84.1487,-15.8513 a22.4171,22.4171 0 1 0 0,31.7' +
      '026 h168.2974 a22.4171,22.4171 0 1 0 0,-31.7026 Z"/>'#13#10'       <us' +
      'e xlink:href="#svgbar" transform="rotate(45)"/>'#13#10'       <use xli' +
      'nk:href="#svgbar" transform="rotate(90)"/>'#13#10'       <use xlink:hr' +
      'ef="#svgbar" transform="rotate(135)"/>'#13#10'     </g>'#13#10'   </g>'#13#10'   <' +
      'use xlink:href="#svgstar"/>'#13#10'   <use xlink:href="#base" opacity=' +
      '"0.85"/>'#13#10'   <use xlink:href="#SVG"/>'#13#10#13#10'</svg>'#13#10
  end
  object SVGIconImage2: TSVGIconImage
    Left = 9
    Top = 3
    Width = 175
    Height = 53
    AutoSize = False
    Proportional = True
    SVGText = 
      '<svg xmlns="http://www.w3.org/2000/svg" width="755.906" height="' +
      '226.772" viewBox="0 0 200 60">'#13#10' <defs>'#13#10'  <filter id="A">'#13#10'   <' +
      'feGaussianBlur in="SourceAlpha" stdDeviation="2"/>'#13#10'   <feOffset' +
      ' dx="2" dy="2" result="A"/>'#13#10'   <feFlood flood-color="#fff" floo' +
      'd-opacity=".6"/>'#13#10'   <feComposite in2="A" operator="in" result="' +
      'A"/>'#13#10'   <feMerge>'#13#10'    <feMergeNode in="A"/>'#13#10'    <feMergeNode ' +
      'in="SourceGraphic"/>'#13#10'   </feMerge>'#13#10'  </filter>'#13#10' </defs>'#13#10' <pa' +
      'th fill="#005e98" filter="url(#A)" transform="matrix(.8 0 0 .8 1' +
      '8 6)" d="M0 60l4.557-11.2H37.6V60zm52.8-48.8h-9.2V0h29.6v11.2H64' +
      'V60H52.8zM79.2 0h11.2v60H79.2zm30.4 0h11.2v60h-11.2zm17.2 48.8h2' +
      '9.6V60h-29.6zm52.075-13.2H162.4V24.4h11.92l-5.368-13.2H162.4V0h1' +
      '3.2L200 60h-11.2zM94.4 24.4h11.2v11.2H94.4zm32.4 0h29.598v11.2H1' +
      '26.8zm-112.325 0H37.6v11.2H9.924zm5.368-13.2L24.4 0h13.2v11.2zM1' +
      '26.8 0h29.6v11.2h-29.6z"/>'#13#10'</svg>'
  end
end
