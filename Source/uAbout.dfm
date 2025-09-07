object FrmAbout: TFrmAbout
  Left = 651
  Top = 323
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 315
  ClientWidth = 442
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    442
    315)
  TextHeight = 13
  object TitleLabel: TLabel
    Left = 190
    Top = 8
    Width = 187
    Height = 41
    AutoSize = False
    Caption = 'SVG Editor and Preview'
    WordWrap = True
  end
  object LabelVersion: TLabel
    Left = 397
    Top = 64
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Version'
  end
  object SVGIconImage1: TSVGIconImage
    Left = 383
    Top = 8
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
    SVGText = 
      '<?xml version="1.0" encoding="UTF-8"?>'#13#10'<svg version="1.1" viewB' +
      'ox="0 0 1500 500" xmlns="http://www.w3.org/2000/svg">'#13#10' <path d=' +
      '"m 29.959524,376.13727c-0.76451-0.65379-6.0819-10.147-9.7722-17.' +
      '448-12.307-24.347-18.221-44.831-19.118-66.237-0.86096-20.522 6.3' +
      '28-41.535 20.128-58.833 16.357-20.505 41.513-34.639 70.082-39.37' +
      '6 7.1343-1.183 11.248-1.5176 20.132-1.637 4.6266-0.0622 8.4572-0' +
      '.15828 8.5124-0.21351 0.0553-0.0552-0.1747-1.3857-0.51098-2.9567' +
      '-0.53126-2.4817-0.61063-3.5816-0.60524-8.387 7e-3 -6.2771 0.3420' +
      '2-8.2278 2.2667-13.199 1.2078-3.1196 1.6207-3.5926 3.0344-3.4764' +
      'l1.0134 0.0833 3.0432 5.9258c5.5025 10.714 10.216 17.563 17.574 ' +
      '25.534 1.6238 1.7589 1.9119 1.962 2.7833 1.962 0.64759 0 1.1636-' +
      '0.19159 1.5461-0.57405l0.57407-0.57406-0.70016-2.8365c-3.6152-14' +
      '.647-2.2014-26.711 4.2573-36.329 2.2951-3.4176 3.1823-4.3009 4.3' +
      '2-4.3009 1.0179 0 1.6943 0.50473 1.8776 1.4011 0.0644 0.31581 0.' +
      '43375 2.2334 0.82046 4.2614 1.8272 9.5824 5.785 20.44 10.921 29.' +
      '961 4.0554 7.5178 7.9975 13.592 16.45 25.346 7.444 10.352 8.7704' +
      ' 12.251 10.854 15.539 7.0094 11.059 9.6667 18.641 9.6451 27.522-' +
      '0.0113 4.6803-0.5214 7.6596-2.0764 12.127-1.0191 2.9278-3.5089 8' +
      '.1077-4.1546 8.6438-0.22033 0.18285-0.85331 0.29518-1.4066 0.249' +
      '57l-1.006-0.083-5.5398-11.061c-6.3211-12.621-9.0304-17.596-10.14' +
      '8-18.633-0.59271-0.54997-1.0452-0.72432-1.8808-0.72432-0.98261 0' +
      '-1.2426 0.14785-2.432 1.3827-2.3867 2.4779-3.8608 6.6791-3.3356 ' +
      '9.5062 0.36987 1.991 0.51832 2.2984 3.5294 7.309 10.68 17.771 14' +
      '.324 32.016 11.222 43.858-1.5491 5.9143-5.8604 11.878-11.474 15.' +
      '87-2.5304 1.7998-3.3379 1.9794-4.2996 0.95558l-0.61494-0.65449 0' +
      '.50117-3.2302c0.65217-4.2032 0.66424-10.016 0.0271-13.035-2.4378' +
      '-11.55-11.258-23.668-24.147-33.174-8.0866-5.9645-17.017-10.226-2' +
      '5.81-12.315-7.9227-1.8827-19.198-2.0312-28.97-0.38191-19.669 3.3' +
      '199-36.586 14.363-48.172 31.447-7.7223 11.386-13.611 25.896-16.1' +
      '91 39.9-1.2558 6.8129-1.5873 10.664-1.5873 18.436 0 7.276 0.3376' +
      '8 11.622 1.3195 16.984 0.78891 4.3082 0.78674 4.7164-0.02831 5.3' +
      '574-0.87842 0.69101-1.7289 0.72922-2.4526 0.11033z" fill="#9c9b9' +
      'b" stroke="#7a7a7a" stroke-width="2"/>'#13#10' <path d="m 209.67,498.7' +
      '6727c-49.73-2.4409-94.499-24.132-120.6-58.433-10.493-13.787-18.0' +
      '28-29.405-21.983-45.562-3.3018-13.493-4.5368-30.212-3.0544-41.34' +
      '9 1.7015-12.783 6.6481-26.898 12.468-35.579 8.5726-12.788 21.628' +
      '-21.916 34.982-24.461 4.1317-0.78725 7.7046-1.0841 8.5095-0.7069' +
      '2 1.0166 0.47634 1.258 1.5346 0.68668 3.0122-1.5375 3.9774-2.547' +
      ' 7.1288-3.1404 9.8039-4.627 20.86 4.2672 41.937 23.959 56.779 22' +
      '.585 17.022 55.759 22.871 85.69 15.111 27.034-7.0098 47.752-24.1' +
      '84 54.636-45.288 2.0096-6.1608 2.3807-8.6319 2.3739-15.805-6e-3 ' +
      '-5.7157-0.0603-6.4431-0.71715-9.5114-0.3912-1.8272-1.3184-5.205-' +
      '2.0606-7.506-5.4327-16.845-15.016-33.609-30.696-53.696-19.119-24' +
      '.493-33.999-44.335-46.613-62.154-41.496-58.622-60.928-99.326-60.' +
      '937-127.64-7e-3 -22.885 12.471-38.088 38.327-46.7 21.684-7.2219 ' +
      '54.628-9.6274 97.438-7.1147 17.919 1.0517 47.601 3.8904 48.503 4' +
      '.6385 1.0235 0.84942 0.94317 2.8249-0.13282 3.266-0.24024 0.0984' +
      '29-2.57 0.58531-5.1774 1.0818-19.711 3.7538-32.714 9.5741-41.244' +
      ' 18.462-4.7938 4.9944-7.7248 10.472-9.1962 17.186-3.8243 17.452 ' +
      '4.0794 41.758 24.368 74.935 8.5349 13.957 18.359 28.39 37.103 54' +
      '.509 4.7815 6.6632 11.436 15.967 14.786 20.675 3.3509 4.7077 7.3' +
      '44 10.159 8.8736 12.115 8.5163 10.888 17.588 24.64 23.713 35.95 ' +
      '17.538 32.378 24.604 63.991 21.134 94.548-0.21386 1.8831-0.5746 ' +
      '5.1422-0.80164 7.2427-2.2681 20.983-9.8142 43.451-21.318 63.471-' +
      '14.952 26.022-36.383 47.977-61.367 62.865-30.6 18.235-70.426 27.' +
      '724-108.51 25.855z" fill="#3ba08b" stroke="#227b69" stroke-width' +
      '="2"/>'#13#10' <path d="m 565.03665,411.47 21.235-52.192h153.98v52.192' +
      'zm246.04-227.41h-42.872v-52.192h137.93v52.192h-42.872v227.41h-52' +
      '.192zm123.03-52.192h52.192v279.6h-52.192zm141.66 0h52.192v279.6h' +
      '-52.192zm80.152 227.41h137.93v52.192h-137.93zm242.67-61.512h-76.' +
      '773v-52.192h55.547l-25.015-61.512h-30.532v-52.192h61.512l113.7 2' +
      '79.6h-52.192zm-393.66-52.192h52.192v52.192h-52.192zm150.99 0h137' +
      '.93v52.192h-137.93zm-523.44 0h107.76v52.192h-128.97zm25.015-61.5' +
      '12 21.235-52.192h61.512v52.192zm498.42-52.192h137.93v52.192h-137' +
      '.93z" fill="#3ba08b" stroke="#227b69" stroke-width="4"/>'#13#10'</svg>' +
      #13#10
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 264
    Width = 442
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object btnOK: TStyledButton
      Left = 351
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'CLOSE'
      Default = True
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnIssues: TStyledButton
      Left = 8
      Top = 8
      Width = 125
      Height = 25
      Caption = 'Submit issue...'
      ImageIndex = 0
      TabOrder = 0
      OnClick = btnIssuesClick
    end
    object btnCheckUpdates: TStyledButton
      Left = 139
      Top = 8
      Width = 200
      Height = 25
      Caption = 'Check for updates'
      ImageIndex = 3
      TabOrder = 1
      Visible = False
      OnClick = btnCheckUpdatesClick
    end
  end
  object MemoCopyRights: TMemo
    Left = 8
    Top = 88
    Width = 427
    Height = 166
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object SVGLinkLabel: TLinkLabel
    Left = 8
    Top = 62
    Width = 266
    Height = 19
    Caption = 
      '<a href="https://github.com/EtheaDev/SVGShellExtensions">https:/' +
      '/github.com/EtheaDev/SVGShellExtensions</a>'
    TabOrder = 2
    UseVisualStyle = True
    OnLinkClick = SVGLinkLabelLinkClick
  end
end
