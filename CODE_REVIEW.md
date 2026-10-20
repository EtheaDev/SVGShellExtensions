# Code Review — SVG Shell Extensions

**Data:** 2 settembre 2026
**Ambito di questa sezione:** l'applicazione editor (`SVGTextEditor`), in particolare `Source/EditorMainForm.pas` (2.224 righe) e le unit specifiche dell'editor.
**Già coperto in precedenza:** thumbnail handler, preview handler, `DResources`, `GitHubAPI`, `uAbout`, `uSettings`, `uRegistry`, `uLogExcept` (correzioni già applicate, versione 3.4.2).
**Escluso:** dipendenze vendored in `Ext/`.

> **Nota sui numeri di riga:** si riferiscono allo stato attuale del codice (dopo le correzioni già applicate e prima di quelle proposte qui).

---

## Sintesi

L'editor SVG e l'editor Markdown (`MDTextEditor`) condividono lo stesso scheletro: `TEditingFile` + `TObjectList` + un `TTabSheet` con un `TSynEdit` per file, `PageControl.Tag` usato come puntatore all'oggetto, un timer per il controllo delle modifiche su disco e un timer di rendering. La comparazione riga per riga fra i due è il metodo più produttivo per questa review, perché fa emergere i punti in cui **uno dei due è stato corretto e l'altro no**.

Tre risultati principali:

1. **Il debounce del rendering nell'editor SVG è inerte.** L'infrastruttura c'è (timer + flag) ma il rendering è rimasto sincrono nell'`OnChange`. `MDTextEditor` ha completato lo spostamento, l'editor SVG no. Ogni battuta di tasto ri-parsifica l'SVG fino a **5 volte**.
2. **Tre bug di gestione della memoria** nel percorso di apertura file, tutti sullo stesso schema: l'oggetto viene inserito in una lista *owner* prima del `try`, e il gestore di eccezioni lo libera una seconda volta. Uno dei tre è presente **anche** in `MDTextEditor`.
3. **I file con estensione maiuscola non si aprono.** `.SVG` viene rifiutato. `MDTextEditor` usa l'helper corretto, l'editor SVG usa `pos()`.

Da notare in positivo: `uSingleInstance.pas` e `WMCopyData` sono scritti bene (firma applicativa, `cbData` validato, WideChar), e la gestione dell'encoding è corretta — verificata empiricamente, vedi § 4.1.

---

## 1. Bug funzionali

### 1.1 I file con estensione maiuscola vengono rifiutati — `EditorMainForm.pas:560`

```pascal
function TfrmMain.CanAcceptFileName(const AFileName: string): Boolean;
begin
  Result := pos(ExtractFileExt(AFileName), AcceptedExtensions) <> 0;  // '.svg;.xml'
end;
```

`Pos` in Delphi è **case-sensitive**, ed `ExtractFileExt` preserva le maiuscole. Quindi:

| file | `ExtractFileExt` | `pos(...)` | esito |
|---|---|---|---|
| `logo.svg` | `.svg` | 1 | aperto |
| `logo.SVG` | `.SVG` | 0 | **rifiutato** |
| `logo.Svg` | `.Svg` | 0 | **rifiutato** |
| `dati.xm` | `.xm` | 6 | **accettato per errore** |
| `archivio.s` | `.s` | 1 | **accettato per errore** |

Il messaggio che l'utente vede è `Cannot open file with extensions different from ".svg;.xml"` su un file che *ha* l'estensione giusta. Il percorso è comune a doppio clic, riga di comando, drag&drop e `WM_COPYDATA`, quindi il caso si presenta con qualunque file salvato in maiuscolo da un tool esterno.

`MDTextEditor` fa la cosa giusta:

```pascal
Result := IsFileNameWithExt(AFileName, AMarkDownFileExt);   // usa SameText
```

**Fix:** portare `IsFileNameWithExt`/`GetFileMasks` da `MDShellEx.Misc.pas` in `uMisc.pas` (dove oggi non ci sono) e usarle, oppure in alternativa un confronto locale con `SameText` su un array di estensioni.

### 1.2 Doppia liberazione nel gestore di eccezioni di `AddEditingFile` — `EditorMainForm.pas:1170`

```pascal
LEditor := TSynEdit.Create(LTabSheet);     // riga 1151: owner = LTabSheet
...
Except
  LTabSheet.Free;                          // distrugge anche LEditor (è suo)
  LEditor.Free;                            // <-- puntatore dangling -> AV
  raise;
End;
```

`LEditor` ha `LTabSheet` come owner, quindi `LTabSheet.Free` lo distrugge già. Il `LEditor.Free` successivo opera su memoria liberata. Se l'eccezione arriva prima della creazione dell'editor `LEditor` è `nil` e il `Free` è innocuo — il bug si manifesta solo se qualcosa fallisce *dopo* la riga 1151, tipicamente in `UpdateFromSettings` o `UpdateHighlighter`.

**Presente anche in `MDTextEditor`**, dove peggiora: libera pure `LFEViewer`, anch'esso figlio del TabSheet.

**Fix:** liberare solo `LTabSheet` e azzerare `EditingFile.SynEditor`.

### 1.3 L'oggetto è già nella lista owner quando il chiamante lo libera — `EditorMainForm.pas:1135`

`AddEditingFile` inserisce nella lista **come prima cosa**, fuori dal `try`:

```pascal
Result := EditFileList.Add(EditingFile);   // riga 1135
Try
  ...
Except
  ... raise;
End;
```

e `EditFileList` è creata con `TObjectList.Create(True)` — *owns objects* (riga 984). I due chiamanti assumono il contrario:

**`OpenFile` (riga 612-624):**

```pascal
I := -1;                                   // riga 595
...
I := AddEditingFile(EditingFile);          // se solleva, I resta -1
...
Except
  if I >= 0 then
    RemoveEditingFile(EditingFile)
  else
    EditingFile.Free;                      // <-- ma è già in EditFileList
  raise;
End;
```

Se `AddEditingFile` solleva, `I` è ancora `-1`, quindi si esegue `EditingFile.Free` mentre la lista ne detiene ancora il puntatore *e* la proprietà. La seconda liberazione arriva alla chiusura dell'applicazione o al primo `acCloseAll`.

**`acNewFileExecute` (riga 1042-1048):** identico schema, senza nemmeno la variabile indice.

**Fix:** spostare `EditFileList.Add` dentro il `try` di `AddEditingFile`, e rimuoverlo dalla lista nel gestore di eccezioni. Così i chiamanti possono liberare l'oggetto senza ambiguità.

### 1.4 `ConfirmChanges` dereferenzia `SynEditor` senza verificarlo — `EditorMainForm.pas:1417`

```pascal
if EditingFile.SynEditor.Modified then
```

`SynEditor` viene assegnato solo alla riga 1162, in mezzo al `try` di `AddEditingFile`. Il percorso `OpenFile` → `except` → `RemoveEditingFile` → `ConfirmChanges` è quindi raggiungibile con `SynEditor = nil`, e produce un access violation *dentro* la gestione di un'altra eccezione.

**Fix:** `if Assigned(EditingFile.SynEditor) and EditingFile.SynEditor.Modified then`.

### 1.5 `LoadOpenedFiles` attiva la scheda sbagliata — `EditorMainForm.pas:948`

```pascal
for I := 0 to FEditorSettings.OpenedFileList.Count-1 do
begin
  LFileName := FEditorSettings.OpenedFileList.Strings[I];
  if OpenFile(LFileName, False) and SameText(LFileName, LCurrentFileName) then
    LIndex := I;                           // indice nelle *impostazioni*
end;
if LIndex <> -1 then
  PageControl.ActivePageIndex := LIndex;   // indice fra le *pagine*
```

`I` indicizza la lista salvata; le pagine vengono create solo per i file aperti **con successo**. Basta che un file ricordato sia stato cancellato o rinominato perché i due indici divergano e al riavvio venga selezionata la scheda sbagliata.

**Fix:** ricavare l'indice da `PageControl.PageCount` dopo l'apertura riuscita, o attivare la pagina cercando il `TEditingFile` per nome file.

### 1.6 `RemoveEditingFile` presume che gli indici delle due collezioni coincidano — `EditorMainForm.pas:1450`

```pascal
EditFileList.Delete(pos);
PageControl.Pages[pos].Free;
```

`pos` è l'indice in `EditFileList`, usato per indicizzare `PageControl.Pages`. Oggi funziona perché le due collezioni crescono insieme, ma nulla lo garantisce e non c'è un'asserzione che lo verifichi. È il tipo di accoppiamento che si rompe in silenzio alla prima modifica (riordino delle schede, apertura non in coda).

**Fix a basso costo:** ricavare la pagina da `EditingFile.TabSheet` invece che per indice — il campo esiste già ed è assegnato alla riga 1147.

### 1.7 `acSaveAllExecute` si interrompe al primo errore — `EditorMainForm.pas:1521`

```pascal
for i := 0 to EditFileList.Count -1 do
  with TEditingFile(EditFileList.items[i]) do
    if SynEditor.Modified then
      SaveToFile;
```

Nessuna gestione per file. Il primo `SaveToFile` che fallisce (file in sola lettura, disco pieno, percorso non più valido) propaga l'eccezione e i file modificati successivi **non vengono salvati**, senza che l'utente lo sappia: il dialogo di errore parla di un file solo.

**Fix:** `try/except` per iterazione, accumulando i nomi dei file non salvati e mostrandoli in un unico messaggio finale.

---

## 2. Rendering: il debounce non è mai stato completato

### 2.1 Il rendering è sincrono nell'`OnChange` — `EditorMainForm.pas:1113`

```pascal
procedure TfrmMain.SynEditChange(Sender: TObject);
begin
  if (Sender = CurrentEditor) and not FProcessingFiles then
  begin
    FEditingInProgress := True;
    LoadTimer.Enabled := False;
    UpdateTabsheetImage(pageControl.ActivePage, CurrentEditor.Modified);
    AssignSVGToImage;                       // <-- rendering completo, qui
  end;
end;

procedure TfrmMain.LoadTimerTimer(Sender: TObject);
begin
  if FEditingInProgress then
  begin
    FEditingInProgress := False;            // <-- e non fa nient'altro
  end;
end;
```

`FEditingInProgress` viene scritto in due punti e letto in uno solo, per rimettersi a `False`: **nessuno lo usa**. `LoadTimer` esiste solo per azzerare un flag che non ha lettori. È l'ossatura di un debounce mai portato a termine.

Il confronto con `MDTextEditor` lo dimostra — lì lo spostamento è stato fatto:

```pascal
procedure TfrmMain.SynEditChange(Sender: TObject);         // MDTextEditor
begin
  if (Sender = CurrentEditor) and not FProcessingFiles then
  begin
    FEditingInProgress := True;
    LoadTimer.Enabled := False;
    UpdateTabsheetImage(...);
    dmResources.StopLoadingImages(True);
    //UpdateMDViewer(False);                 // <-- rendering sincrono rimosso
  end;
end;

procedure TfrmMain.LoadTimerTimer(Sender: TObject);        // MDTextEditor
begin
  if FEditingInProgress then
  begin
    LoadTimer.Enabled := False;
    if FEditorSettings.AutoRefreshWhenEditing then         // <-- e reso opzionale
    begin
      FEditingInProgress := False;
      dmResources.StopLoadingImages(False);
      UpdateMDViewer(True);                                // <-- rendering qui
    end;
  end;
end;
```

### 2.2 Il costo effettivo per battuta di tasto — `EditorMainForm.pas:1183`

```pascal
SVGIconImage.SVGText := LSVGText;
if CurrentEditor.Lines.Count < 1000 then
begin
  SVGIconImage16.SVGText := LSVGText;
  SVGIconImage32.SVGText := LSVGText;
  SVGIconImage48.SVGText := LSVGText;
  SVGIconImage96.SVGText := LSVGText;
end
```

Ogni assegnazione a `SVGText` fa un parse completo del documento. Con la chiamata sincrona di § 2.1, **ogni carattere digitato** costa: `CurrentEditor.Lines.Text` (concatenazione di tutte le righe in una stringa) + **5 parse** dell'SVG, per qualunque file sotto le 1000 righe. Sopra le 1000 righe restano `Lines.Text` + 1 parse, più il costo di azzerare le quattro icone.

Il limite a 1000 righe suggerisce che il problema era noto, ma la soglia mitiga solo il caso peggiore: un'icona tipica da 100-300 righe paga il conto pieno.

**Fix:** spostare `AssignSVGToImage` in `LoadTimerTimer` e riabilitare il timer in `SynEditChange` (`LoadTimer.Enabled := True` invece di `False`), impostando un `Interval` esplicito nel DFM — oggi non c'è, quindi vale il default di 1000 ms, probabilmente troppo lento: 300-500 ms è la scelta abituale. Vale la pena aggiungere anche l'equivalente di `AutoRefreshWhenEditing` nelle impostazioni.

### 2.3 `LoadTimer` è armato da un gestore che non lo riguarda — `EditorMainForm.pas:1989`

`LoadTimer.Enabled := True` è l'ultima istruzione di `ActionListUpdate`, il gestore `OnUpdate` dell'action list, che con `Application.ActionUpdateDelay := 50` viene eseguito in pratica a ripetizione.

> **Correzione (2 settembre 2026).** La prima stesura di questo paragrafo affermava che le riabilitazioni ripetute azzerano il conteggio del timer, e che quindi spostare il rendering nel timer avrebbe prodotto un timer che non scade mai. **È sbagliato**, verificato sul sorgente VCL:
>
> ```pascal
> procedure TTimer.SetEnabled(Value: Boolean);
> begin
>   if Value <> FEnabled then      // <-- esce subito se il valore non cambia
>   begin
>     FEnabled := Value;
>     UpdateTimer;                 // solo qui si chiama KillTimer/SetTimer
>   end;
> end;
> ```
>
> Assegnare `True` a un timer già abilitato è un no-op: `UpdateTimer` non viene richiamato e il conteggio non si azzera. Le riabilitazioni ripetute sono quindi innocue, ed è proprio così che funziona il debounce di `MDTextEditor`: `SynEditChange` ferma il timer, il successivo `ActionListUpdate` lo riarma con un conteggio nuovo, e l'`OnTimer` scatta `Interval` ms dopo l'ultima battuta.

Resta un problema di leggibilità, non di correttezza: il debounce dipende da un gestore che non ha nulla a che vedere con l'editing, e il meccanismo non è comprensibile leggendo `SynEditChange` e `LoadTimerTimer` da soli.

**Fix:** armare il timer in `SynEditChange` con la coppia idiomatica `Enabled := False; Enabled := True;` e togliere la riga da `ActionListUpdate`, così il meccanismo è tutto in due metodi adiacenti.

### 2.4 Cursore a clessidra a ogni tasto — `EditorMainForm.pas:1203`

`Screen.Cursor := crHourGlass` in `AssignSVGToImage`: con la chiamata sincrona il cursore lampeggia a ogni battuta. Si risolve da sé spostando il rendering nel timer, ma vale la pena notare che la gestione del cursore è sparsa — `CloseSplitViewMenu` (riga 1326) lo rimette a `crDefault` senza sapere chi l'aveva cambiato.

---

## 3. Robustezza e dettagli

### 3.1 `WMCopyData` non limita `cbData` — `EditorMainForm.pas:1376`

```pascal
LChars := (Msg.CopyDataStruct.cbData div SizeOf(Char));
if LChars > 0 then Dec(LChars);
SetString(LFileName, PChar(Msg.CopyDataStruct.lpData), LChars);
```

La validazione a monte è buona (firma `dwData`, `cbData <> 0`, `lpData <> nil`), ma `cbData` è controllato dal mittente e non ha un limite superiore: un mittente difettoso o ostile che dichiara un `cbData` più grande del buffer reale fa leggere `SetString` oltre la memoria valida. È la stessa correzione già applicata a `MarkDownHelpViewer`.

**Fix:** rifiutare `cbData > (MAX_PATH + 1) * SizeOf(Char)` e troncare la stringa al primo `#0` incontrato.

### 3.2 Il pulsante Help nel dialogo di errore non fa nulla — `EditorMainForm.pas:2203`

```pascal
StyledTaskMessageDlg(STR_ERROR, ..., [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbHelp], 0);
```

`SVGTextEditor.dpr:81` imposta `Application.HelpFile := ''`. Il pulsante Help compare e non produce alcun effetto.

**Fix:** rimuovere `mbHelp`.

### 3.3 Ramo `mrCancel` irraggiungibile in `ConfirmChanges` — `EditorMainForm.pas:1423`

```pascal
LConfirm := StyledMessageDlg(..., [mbYes, mbNo], 0);   // niente mbCancel
if LConfirm = mrYes then
  EditingFile.SaveToFile
else if LConfirm = mrCancel then                        // mai vero
  Abort;
```

L'insieme dei pulsanti non contiene `mbCancel`, quindi `Abort` non viene mai eseguito. Di conseguenza **l'utente non ha modo di annullare la chiusura** dell'applicazione con file modificati: può solo scegliere se salvare ciascun file, uno per uno. L'`Abort` indica che l'intenzione era diversa.

Da segnalare che `Abort` sarebbe stato gestito correttamente: `TApplication.HandleException` esclude `EAbort` dal richiamo di `OnException`, quindi non avrebbe prodotto un dialogo di errore.

**Fix:** decidere fra le due semantiche. Se si vuole permettere l'annullamento, aggiungere `mbCancel` all'insieme dei pulsanti e in `FormClose` intercettare `EAbort` per impostare `Action := caNone`.

### 3.4 Dettagli minori

| Punto | Riga | Nota |
|---|---|---|
| `TEditingFile.Destroy` | 508 | override vuoto (solo `inherited`), da rimuovere |
| `FFileAge` | 1303, 1314 | `TfrmMain` accede a un campo `private` di `TEditingFile` (legale nella stessa unit, ma meglio una proprietà) |
| `LTabSheet.Tag := NativeInt(EditingFile)` | 1143 | puntatore nel `Tag`; funziona, un campo tipizzato sarebbe più sicuro |
| `SynEditor.OnChange(SynEditor)` | 528, 537 | chiamata diretta di un event handler; in `SaveToFile` forza un re-render completo a ogni salvataggio |
| `LCurrentFileName` | 946 | riletto dalle impostazioni a ogni iterazione del ciclo |
| `FormCreate` | 1006 | `WindowState := wsMaximized` incondizionato: la posizione/stato della finestra non viene ripristinata |

---

## 4. Verifiche empiriche

### 4.1 La gestione dell'encoding è corretta

`TEditingFile.LoadFromFile` (riga 513) e `TFrmPreview.LoadFromFile` (`PreviewForm.pas:352`) usano lo schema:

```pascal
try
  SynEditor.Lines.LoadFromFile(AFileName, TEncoding.UTF8);
except
  on E: EEncodingError do
    SynEditor.Lines.LoadFromFile(AFileName, TEncoding.ANSI);
end;
```

`SynEdit.Lines` è un `TSynEditStringList`, che fa **override** di `LoadFromStream` e non chiama la logica di `TStrings`. Restava quindi da stabilire se `EEncodingError` venisse ancora sollevato. Verificato compilando (`dcc64` 37.0):

| verifica | risultato |
|---|---|
| `TEncoding.UTF8.GetString` su byte ANSI non validi (`$E0`, `$E9`) | **solleva `EEncodingError`** — "No mapping for the Unicode character exists in the target multi-byte code page" |
| `TEncoding.Default = TEncoding.ANSI` | **True** (stessa istanza) |
| `GetBufferEncoding(buf, UTF8, Default)` su buffer senza BOM | ritorna 0, `AEncoding` resta UTF8 |
| `GetBufferEncoding(buf, nil, Default)` su buffer senza BOM | `AEncoding` diventa ANSI |

Il fallback **funziona**: `TSynEditStringList.LoadFromStream` arriva a `Encoding.GetString(...)`, che solleva, e l'`except` recupera con ANSI.

Nota utile per il futuro: la terza riga spiega perché forzare `TEncoding.UTF8` **disattiva** il rilevamento di SynEdit (`GetBufferEncoding` con `AEncoding` non nil controlla solo quel preambolo), e la seconda spiega perché passando `nil` il rilevamento funziona: `DefaultEncoding` è ANSI, quindi il test `if (Encoding = TEncoding.ANSI) and DetectUTF8 and IsUTF8(Buffer, Size)` di `SynEditTextBuffer.pas:610` è vero e promuove a UTF-8 un file UTF-8 senza BOM.

**Semplificazione possibile (non un bug):** `SynEditor.Lines.LoadFromFile(AFileName)` senza encoding esplicito produce lo stesso risultato usando la politica già implementata in SynEdit — BOM se c'è, altrimenti sniffing UTF-8, altrimenti ANSI — senza passare da un'eccezione nel caso normale. Sette righe diventano una. L'encoding rilevato viene conservato in `FEncoding` e riusato da `SaveToFile`, e `WriteBOM` viene impostato di conseguenza, quindi il round-trip resta corretto in entrambe le varianti.

### 4.2 Nessuna perdita di handle GDI nel thumbnail provider

Test su cartella di 337 SVG, cache delle miniature svuotata, provider in-process in `explorer.exe`:

| | baseline | dopo 337 miniature |
|---|---|---|
| oggetti GDI | 6485 | 6686 (+201, con le 337 icone a schermo) |
| picco | 6708 | 6871 |

Log: 390 cicli `GetThumbnail`, 393 `PaintTo start` e 393 `PaintTo end`, **zero eccezioni** in 4.259 righe. Con una perdita di un `TBitmap` per chiamata l'incremento sarebbe stato di 700-1000 oggetti. La correzione `hBitmap := LBitmap.ReleaseHandle` + `LBitmap.Free` è confermata.

---

## 5. Priorità proposte

**Gruppo A — bug con effetto visibile o crash**

1. § 1.1 estensione maiuscola rifiutata (l'unico che l'utente incontra nell'uso normale)
2. § 1.3 doppia liberazione in `OpenFile` e `acNewFileExecute`
3. § 1.2 doppia liberazione in `AddEditingFile` (**anche in `MDTextEditor`**)
4. § 1.4 `SynEditor` nil in `ConfirmChanges`
5. § 1.5 scheda sbagliata al ripristino della sessione
6. § 1.7 `acSaveAll` che si ferma al primo errore

**Gruppo B — rendering**

7. § 2.1 + § 2.3 spostare il rendering nel timer, con `Interval` esplicito e l'armamento in `SynEditChange`.
8. § 2.4 cursore (si risolve col precedente)

**Gruppo C — robustezza e pulizia**

9. § 3.1 limite su `cbData`
10. § 3.3 decidere la semantica di annullamento chiusura
11. § 3.2 rimuovere `mbHelp`
12. § 3.4 dettagli minori
13. § 4.1 semplificazione del caricamento (opzionale)

---

## Stato

*(da aggiornare a mano a mano che gli interventi vengono applicati)*

| § | Intervento | Stato |
|---|---|---|
| 1.1 | `CanAcceptFileName` con `SameText` | **fatto** |
| 1.2 | `AddEditingFile` except-block | **fatto** (anche in `MDTextEditor`) |
| 1.3 | `EditFileList.Add` dopo il `try` | **fatto** (anche in `MDTextEditor`) |
| 1.4 | `Assigned(SynEditor)` in `ConfirmChanges` | **fatto** |
| 1.5 | indice pagina in `LoadOpenedFiles` | **fatto** |
| 1.6 | `EditingFile.TabSheet` invece dell'indice | **fatto** |
| 1.7 | `acSaveAll` per-file | **fatto** |
| 2.1-2.4 | debounce del rendering | **fatto** |
| 3.1 | limite `cbData` | **fatto** |
| 3.2 | `mbHelp` | **fatto** |
| 3.3 | annullamento chiusura | **fatto** (aggiunto `mbCancel`) |
| 3.4 | dettagli minori | **fatto** |
| 4.1 | semplificazione encoding | opzionale |
| — | thumbnail: `ReleaseHandle` + `Free` | **fatto e verificato** (§ 4.2) |
| — | `dmResources` singleton | **fatto e verificato** |
| — | check nuova versione asincrono | **fatto** |
| — | fallback ANSI in `PreviewForm` | **fatto e verificato** (§ 4.1) |
| — | versione 3.4.2 in tutti i punti | **fatto** |

---

## Gruppo A applicato — 2 settembre 2026

### Interventi

| § | File | Modifica |
|---|---|---|
| 1.1 | `uMisc.pas` | Aggiunti `ASVGFileExt` (`'.svg'`, `'.xml'`, unica fonte di verità), `IsFileNameWithExt` (confronto con `SameText` sull'estensione intera) e `GetFileExtensionsList` |
| 1.1 | `EditorMainForm.pas` | `CanAcceptFileName` usa `IsFileNameWithExt`; `AcceptedExtensions` deriva la stringa dall'array |
| 1.2 | `EditorMainForm.pas` | `AddEditingFile`: nell'`except` resta solo `LTabSheet.Free`, e `EditingFile.SynEditor`/`.TabSheet` vengono azzerati |
| 1.3 | `EditorMainForm.pas` | `EditFileList.Add` spostata **dopo** il `try`: la proprietà passa alla lista solo a successo avvenuto |
| 1.3 | `EditorMainForm.pas` | `OpenFile`: introdotta `LCreated`; `acNewFileExecute`: `SetFocus` spostata fuori dal `try` |
| 1.4 | `EditorMainForm.pas` | `ConfirmChanges` verifica `Assigned(EditingFile.SynEditor)`; anche `ModifiedCount` |
| 1.5 | `EditorMainForm.pas` | `LoadOpenedFiles` usa `PageControl.ActivePage` invece dell'indice delle impostazioni |
| 1.7 | `EditorMainForm.pas` | `acSaveAllExecute` con `try/except` per file e messaggio riepilogativo (`FILES_NOT_SAVED`) |

### Verifica sperimentale della proprietà dei controlli VCL

Il § 1.2 dipendeva da una domanda a cui non volevo rispondere a memoria: **distruggere il parent distrugge i figli, o solo l'owner lo fa?** Nell'editor SVG `TSynEdit.Create(LTabSheet)` ha il TabSheet come owner, ma in `MDTextEditor` sia `TSynEdit` sia `THtmlViewer` sono creati con `Create(nil)` e hanno solo il `Parent` impostato.

Verificato compilando (`dcc64` 37.0) un programma che sottoclassa `TEdit` per registrare la propria distruzione:

| figlio del pannello | distrutto da `LPanel.Free` |
|---|---|
| owner = pannello, parent = pannello | **sì** |
| owner = **nil**, parent = pannello | **sì** |

`TWinControl.Destroy` distrugge i controlli figli **indipendentemente dalla proprietà**. Quindi in `MDTextEditor` `LTabSheet.Free` distruggeva già `LEditor`, `LFEViewer` e `LSplitter`, e le due `Free` successive operavano su puntatori pendenti: il difetto era reale anche lì, e più esteso.

### Difetto dei `.dproj` trovato durante la verifica

I build in configurazione **Debug** di `SVGTextEditor.dproj` e `SVGToPNG.dproj` fallivano *prima* di queste modifiche:

```
error F1026: File not found: 'true.dpr'
```

Causa: nel `PropertyGroup` di `Cfg_2` (Debug) la proprietà valeva `<DCC_DebugInformation>true</DCC_DebugInformation>`, mentre i valori ammessi sono numerici (`0` = nessuna, `2` = completa). Il target passa il valore così com'è a `dcc64`, che lo interpreta come nome del file sorgente. Si vede nella riga di comando generata:

```
dcc64.exe -$O- -$W+ -$R+ -$Q+ true --no-config -B -Q ...
```

Corretto a `2` in entrambi i progetti. Lo stesso valore errato è presente in `MDTextEditor.dproj`, dove però era mascherato: le configurazioni `Cfg_2_Win32` e `Cfg_2_Win64` lo sovrascrivono con `2`. Corretto anche là per coerenza.

### Compilazione

| progetto | Debug | Release |
|---|---|---|
| `SVGTextEditor.dproj` Win64 | OK | OK |
| `MDTextEditor.dproj` Win64 | OK | OK |

Nessun warning o hint introdotto (i tre hint `H2077` comparsi sulle inizializzazioni a `nil` diventate inutili sono stati risolti rimuovendole).

---

## Gruppo B applicato — 2 settembre 2026

### Il debounce, completato

| File | Modifica |
|---|---|
| `EditorMainForm.pas` | `SynEditChange` non renderizza più: alza `FEditingInProgress`, riavvia il timer con `Enabled := False; Enabled := True;` e aggiorna solo l'icona della scheda |
| `EditorMainForm.pas` | `LoadTimerTimer` disabilita il timer e, se c'è editing in corso, chiama `AssignSVGToImage`. È qui che ora avviene l'unica parsificazione |
| `EditorMainForm.pas` | Rimosso `LoadTimer.Enabled := True` da `ActionListUpdate`: il timer è armato solo da `SynEditChange` |
| `EditorMainForm.dfm` | `LoadTimer.Interval = 400` (prima assente, quindi il default di 1000 ms) |

L'anteprima si aggiorna 400 ms dopo l'ultima battuta invece che a ogni carattere. Il valore è un compromesso: `MDTextEditor` usa di fatto 1000 ms (default), che per l'SVG risulta lento; 400 ms mantiene la sensazione di immediatezza durante la digitazione normale eliminando la raffica per carattere. È un solo valore nel DFM, facile da tarare.

**Nota sul cambio di comportamento:** prima l'anteprima seguiva la digitazione carattere per carattere. Ora c'è una pausa di 400 ms. È il prezzo del fix, ed è la stessa scelta già fatta nell'editor markdown.

### Rendering saltato quando il contenuto non è cambiato

Aggiunto il campo `FRenderedSVGText` e un confronto in testa ad `AssignSVGToImage`. Il metodo è raggiunto da nove punti diversi — cambio pagina, drag&drop, `WM_COPYDATA`, apertura file, timer del file modificato, chiusura di tutti i file, e ora il timer del debounce — e capitava spesso che lo stesso testo venisse parsificato due volte di fila (per esempio all'apertura: `AddEditingFile` chiama `PageControl.OnChange`, e poi `acOpenFileExecute` richiama `AssignSVGToImage`).

La cache è corretta perché il risultato dipende **solo** dal testo: `FixedColor`, `GrayScale` e `ApplyFixedColorToRootOnly` sono applicati in `TSVGIconImage.Paint`, non alla parsificazione.

### Cursore (§ 2.4)

`AssignSVGToImage` salva e ripristina `Screen.Cursor` invece di forzare `crDefault`. Il `crDefault` incondizionato cancellava anche la clessidra impostata da un'operazione esterna: `OpenFile` la tiene per tutto il caricamento e passa da qui attraverso il cambio pagina.

### § 2.2: le cinque parsificazioni, e perché ne resta una sola *per rendering*

Il § 2.2 osservava che un rendering parsifica lo stesso documento cinque volte (anteprima principale più le miniature 16/32/48/96). Con il debounce questo accade una volta per pausa di digitazione invece che per carattere, il che è la parte grossa del guadagno.

Le quattro parsificazioni residue **si potrebbero** eliminare: `TSVGIconImage.Assign` non ri-parsifica, condivide l'interfaccia già costruita:

```pascal
procedure TSVGIconImage.Assign(Source: TPersistent);
begin
  if (Source is TSVGIconImage) then
  begin
    FSVG := (Source as TSVGIconImage).FSVG;   // stessa istanza, nessun parse
```

**Non l'ho applicato**, e la ragione è precisa. Condividere l'`ISVG` significa che le cinque immagini puntano al medesimo documento, e `ClearIcons` azzera le quattro miniature con `SVGText := ''`, che si traduce in `FSVG.Source := ''`: **azzererebbe il documento condiviso, svuotando anche l'anteprima principale.** Lo scenario è concreto — si apre un SVG breve (condivisione stabilita), si incolla un file da 2000 righe, il rendering successivo prende il ramo `else` e chiama `ClearIcons`. L'unico modo di rompere la condivisione attraverso l'API pubblica è `UpdateSVGFactory`, che ricrea `FSVG`, ma farne un de-aliasing implicito è una dipendenza troppo sottile da lasciare in eredità.

C'è anche un secondo effetto: `Opacity`, `FixedColor` e `GrayScale` diventerebbero stato comune. Oggi l'applicazione li imposta sempre uguali su tutte e cinque (`SetFixedColor`, `SetGrayScale`, `SetApplyToRootOnly`), quindi non si vedrebbe nulla — ma una futura miniatura con resa diversa si romperebbe in silenzio.

Il guadagno residuo (5 parse → 1 per pausa di digitazione) non vale un difetto di anteprima vuota. Se lo si vuole fare, la strada pulita è aggiungere al componente un setter che *cloni* l'`ISVG` invece di condividerla, cosa che va fatta in `SVGIconImageList` a monte.

### Non applicato: l'interruttore nelle impostazioni

`MDTextEditor` gestisce il debounce sotto l'opzione `AutoRefreshWhenEditing` (`MDShellEx.Settings.pas`, casella nella form delle impostazioni). L'equivalente qui richiede di toccare `uSettings.pas`, `SettingsForm.pas` e il relativo DFM per posizionare un controllo nuovo — è una funzionalità, non la correzione, e non posso verificarne la resa visiva. Lasciata come passo successivo se la si vuole.

### Compilazione

`SVGTextEditor.dproj` Win64, Debug e Release: OK. Nessun warning né hint nuovo.

---

## Regressione del check versione asincrono — 2 settembre 2026

**Sintomo:** Access Violation in `System._UStrAsg` aprendo la form About dal
controllo automatico di nuova versione.

```
System._UStrAsg(???,???)
GitHubAPI.TGitHubHttpClient.DownloadLatestSetup('...Setup.exe',(...HttpClientReceiveData,$38766A0),'')
uAbout.TFrmAbout.DownloadAndInstallSetup
uAbout.TFrmAbout.FormActivate($38766A0)
...
Vcl.Forms.TCustomForm.ShowModal
uAbout.ShowAboutForm(..., True)
EditorMainForm.TfrmMain.ActionListUpdate$ActRec.$0$Body('v3.4.0','v3.4.1')
uAbout.CheckNewSetupAsync$ActRec.$0$Body$ActRec.$0$0$Body
System.Classes.CheckSynchronize(???)
```

**Causa.** La variabile globale `_HttpClient` veniva creata **solo** dentro
`AcceptNewSetup`, cioè il percorso del controllo **manuale**:

```pascal
if not Assigned(_HttpClient) then
  _HttpClient := TGitHubHttpClient.Create(nil);
Result := _HttpClient.IsNewVersionAvailable(...);
```

mentre `DownloadAndInstallSetup` la usava senza verificarla. Finché il controllo
era sincrono la sequenza era sempre "controlla poi scarica", quindi il client
esisteva per effetto collaterale. Spostando il controllo automatico in un task
di background — che per non condividere stato fra thread usa un client locale
suo — nessuno crea più `_HttpClient`: la form About aperta direttamente dal
callback arriva a `DownloadLatestSetup` con `Self = nil`, e la prima
assegnazione di campo (`FSetupFileName := ASetupFileName`) scrive a un offset
da `nil`. Da qui il frame `System._UStrAsg`.

Il terzo argomento mostrato come `''` nel trace è il parametro `out
ADownloadedFileName`, non la causa: è vuoto perché la funzione non era ancora
arrivata ad assegnarlo.

**Secondo difetto sullo stesso percorso.** Anche creando il client, un'istanza
appena costruita ha `FGitHubProjectURL` vuoto, e `DownloadLatestSetup` lo
verificava solo con un `Assert` **sul nome del file**, non sull'URL. Il
risultato non sarebbe stato un errore ma la URL malformata
`/releases/latest/download/<setup>`.

**Terzo difetto: il download fallito si ritentava in loop.** `FormActivate`
avvia il download quando `FStopDownload` è `False`, e `OnActivate` scatta a
ogni riattivazione della form. `UpdateGUI(False)`, chiamata nel `finally`,
rimetteva la didascalia del pulsante ma **non** il flag: chiuso il dialogo
d'errore, la form si riattivava e il download ripartiva.

### Correzioni

| Dove | Modifica |
|---|---|
| form About (3 progetti) | Nuova `function GitHubClient: TGitHubHttpClient` che crea il client alla prima chiamata **e** ne imposta sempre `GitHubProjectURL`. Usata sia da `AcceptNewSetup` sia da `DownloadAndInstallSetup` |
| form About (3 progetti) | `UpdateGUI(False)` rimette `FStopDownload := True`, coerentemente con la didascalia che torna a "check for updates" |
| `GitHubAPI.pas` (3 copie) | I due `Assert` sui parametri obbligatori diventano controlli veri con `ECheckNewVersionException` e messaggi propri (`ERR_SETUP_FILENAME_MISSING`, `ERR_PROJECT_URL_MISSING`): un `Assert` può essere escluso dalla compilazione, e sull'URL vuoto non c'era alcun controllo |

La regressione era presente in tutti e tre i progetti che hanno ricevuto
`CheckNewSetupAsync` — MarkDownHelpViewer, MarkDownShellExtensions e
SVGShellExtensions — e si manifesta solo quando una versione più recente esiste
davvero, per questo non era emersa prima.

### Osservazioni non modificate

- Il download parte da `FormActivate` e gira quindi dentro la closure accodata
  da `TThread.Queue`, con una form modale annidata. Funziona, ma un pulsante
  esplicito o un `PostMessage` alla form sarebbe più lineare.
- `DownloadAndInstallSetup` confronta la dimensione scaricata con
  `FExpectedSize`, che viene valorizzato dal primo callback `OnReceiveData` con
  `AContentLength`. Se il server rispondesse senza `Content-Length` il
  confronto fallirebbe sempre e un download riuscito sarebbe segnalato come
  errore. GitHub lo invia, quindi in pratica non si verifica.

### Regex delle versioni allineata a kittoX

`DecodeVersion` usava `'^v(\d+)\.(\d+)\.(\d+)$'`, che impone il prefisso `v`
e vieta qualunque suffisso. La copia in `kittoX/Kide/Source/GitHubAPI.pas` era
gia' stata resa piu' tollerante, e ora le quattro copie sono identiche:

```pascal
// Optional 'v'/'V' prefix and an ignored trailing suffix (e.g. a pre-release
// tag like 'beta' in 'V4.0.18beta'): only the leading N.N.N is significant.
Regex := TRegEx.Create('^v?(\d+)\.(\d+)\.(\d+)', [roIgnoreCase]);
```

Con la versione precedente un tag come `3.4.2`, `V4.0.18beta` o `v3.4.2-rc1`
faceva sollevare `ECheckNewVersionException`: nel controllo automatico
l'eccezione viene inghiottita in silenzio e l'aggiornamento non viene **mai**
proposto, nel controllo manuale l'utente vede il dialogo di errore con
"retry". Il rischio riguarda solo il tag che arriva da GitHub, perche'
`GetCurrentVersion` produce sempre `vN.N.N`.

Comportamento verificato compilando un confronto fra le due regex
(`dcc64` 37.0):

| tag | vecchia | nuova |
|---|---|---|
| `v3.4.2`, `V3.4.2` | 3.4.2 | 3.4.2 |
| `v3.4.10` | 3.4.10 | 3.4.10 |
| `v10.20.30` | 10.20.30 | 10.20.30 |
| `3.4.2` | eccezione | **3.4.2** |
| `V4.0.18beta` | eccezione | **4.0.18** |
| `v3.4.2-rc1` | eccezione | **3.4.2** |
| `v2.5` | eccezione | eccezione |
| `release-3.4.2` | eccezione | eccezione |
| `''`, `vX.Y.Z` | eccezione | eccezione |

Nessun tag che prima veniva accettato cambia risultato, e togliere l'ancora
`$` non intacca la greediness di `\d+`: `v3.4.10` resta `3.4.10` e non
`3.4.1`. L'ancora `^` continua a respingere i prefissi non numerici come
`release-3.4.2`.

Effetto collaterale da tenere presente: un tag di pre-release viene ora
interpretato come la versione finale corrispondente, quindi `v3.4.3-rc1`
sarebbe proposto come `3.4.3`. In pratica non capita perche' l'endpoint
`releases/latest` di GitHub esclude le pre-release.

---

## Gruppo C applicato — 2 settembre 2026

Passaggio unico sui due editor: gli stessi interventi su `EditorMainForm.pas`
(SVG) e `MDTextEditor.EditorMainForm.pas` (markdown), che erano disallineati
**in entrambe le direzioni**.

### Che cosa mancava a chi

| § | difetto | editor SVG | editor markdown |
|---|---|---|---|
| 1.1 | estensione maiuscola rifiutata | corretto nel Gruppo A | era già corretto |
| 1.2 / 1.3 | doppie liberazioni | corretto nel Gruppo A | portato nel Gruppo A |
| 1.4 | `ConfirmChanges` senza `Assigned(SynEditor)` | corretto nel Gruppo A | **corretto ora** |
| 1.5 | `LoadOpenedFiles` attiva la scheda sbagliata | corretto nel Gruppo A | era già corretto |
| 1.6 | indici delle due collezioni accoppiati | **corretto ora** | era già corretto |
| 1.7 | `acSaveAll` si ferma al primo errore | corretto nel Gruppo A | **corretto ora** |
| 3.1 | `cbData` senza limite | **corretto ora** | **corretto ora** |
| 3.2 | `mbHelp` inerte | **corretto ora** | **corretto ora** |
| 3.3 | chiusura non annullabile | **corretto ora** | **corretto ora** |
| 3.4 | dettagli minori | **corretto ora** | non applicabili |

Su § 1.5 e § 1.6 l'editor markdown era **avanti**, e con una soluzione migliore
di quella scritta per l'SVG: itera `EditFileList` invece di indicizzarlo e
ricava la pagina da `LEditFile.TabSheet`. È quella che ho portato sull'SVG, non
l'inversa.

### § 3.3 — la chiusura diventa annullabile

L'insieme dei pulsanti di `ConfirmChanges` era `[mbYes, mbNo]` mentre il codice
conteneva un ramo `else if LConfirm = mrCancel then Abort;` **irraggiungibile**:
chi chiudeva con file modificati poteva solo decidere, file per file, se
salvare, senza poter tornare indietro. Aggiunto `mbCancel`, e `FormClose`
intercetta `EAbort`:

```pascal
try
  ConfirmChanges(LEditingFile);
except
  on EAbort do
  begin
    Action := caNone;
    Exit;
  end;
end;
```

`Action := caNone` impedisce a `TCustomForm.Close` di arrivare ad
`Application.Terminate`, e uscire prima di `UpdateOpenedFiles`/`WriteSettings`
lascia le impostazioni intatte: la sessione non è finita.

Verificato che il percorso regga davvero, leggendo `Vcl.StyledTaskDialog.pas`
invece di darlo per scontato:

- senza command link, ogni pulsante diventa un `TTaskDialogButtonItem` e riceve
  `ModalResult := LModalResults[DlgBtn]` (riga 662), dove `LModalResults`
  (riga 562) associa `mbCancel` a `mrCancel`;
- con i command link attivi, `mbCancel` insieme a `mbNo` diventa il common
  button `tcbCancel` (righe 609-613), che restituisce anch'esso `mrCancel`.

Effetto collaterale gradito: adesso che esiste un pulsante Cancel, il tasto Esc
sul dialogo annulla la chiusura, mentre prima era inerte.

Negli altri percorsi non serviva alcuna modifica: `RemoveEditingFile` (usato da
`acClose`, dal pulsante di chiusura della scheda e da `acCloseAll`) propaga
`EAbort` e la scheda resta aperta, e `EAbort` è silenzioso per costruzione —
`TApplication.HandleException` non lo instrada mai a `OnException`.

### § 3.1 — `cbData`, con la descrizione corretta

La prima stesura di questo paragrafo parlava di lettura oltre il buffer. È
imprecisa: per un `WM_COPYDATA` **fra processi** il kernel copia `cbData` byte
nello spazio del destinatario, quindi `lpData` punta a un blocco valido di
quella dimensione. Restano due problemi reali:

- un valore assurdo produceva un'allocazione assurda;
- `SetString` prende esattamente la lunghezza dichiarata, quindi un payload non
  realmente terminato lasciava spazzatura in coda al nome del file.

Aggiunti quindi un limite e il troncamento al primo `#0`:

```pascal
const
  //A Windows path with the \\?\ prefix can reach 32767 characters.
  MAX_COPYDATA_BYTES = (32767 + 1) * SizeOf(Char);
...
  if Msg.CopyDataStruct.cbData > MAX_COPYDATA_BYTES then
    Exit;
  ...
  LChars := Pos(#0, LFileName);
  if LChars > 0 then
    SetLength(LFileName, LChars - 1);
```

Il limite è 32767 caratteri e non `MAX_PATH`, perché il mittente
(`uSingleInstance.SendFileToExistingInstance`) invia `ExpandFileName`, che su un
sistema con i percorsi lunghi abilitati può superare i 260 caratteri.

### § 3.4 — dettagli minori (solo editor SVG)

| Modifica |
|---|
| Rimosso `TEditingFile.Destroy`, che era un override vuoto (`inherited;` e nulla più) |
| `FFileAge` esposto come `property FileTimestamp`: `TfrmMain` raggiungeva il campo privato direttamente. Chiamarla `FileAge` avrebbe fatto ombra alla funzione omonima di `System.SysUtils` usata dentro la classe |

Non toccati, con motivo: `LTabSheet.Tag := NativeInt(EditingFile)` funziona e la
sua sostituzione con un campo tipizzato è un refactoring più ampio; la chiamata
diretta `SynEditor.OnChange(SynEditor)` in `SaveToFile` è **portante**, perché è
ciò che aggiorna l'icona della scheda dopo il salvataggio, e con il debounce del
Gruppo B il rendering che ne consegue è ormai gratuito (la cache del testo lo
salta).

### § 1.6 — la pagina si ricava dall'oggetto

`RemoveEditingFile` indicizzava `PageControl.Pages` con l'indice di
`EditFileList`. Ora prende `EditingFile.TabSheet`, legge `PageIndex` prima di
distruggerla e attiva la pagina che ne prende il posto (o l'ultima, se si è
chiusa quella finale) — comportamento più prevedibile del precedente, che in
caso di chiusura della prima scheda non attivava nulla.

L'ordine di distruzione è diverso dai due editor e va rispettato: nell'editor
SVG il `TSynEdit` ha il TabSheet come owner e `TEditingFile` non possiede
controlli, quindi `EditFileList.Delete` libera solo l'oggetto e il successivo
`LTabSheet.Free` distrugge l'editor. Nell'editor markdown `TEditingFile.Destroy`
fa `FreeAndNil(HTMLViewer); FreeAndNil(SynEditor);`, per questo là l'ordine è
documentato con un commento esplicito.

### Compilazione

`SVGTextEditor.dproj` e `MDTextEditor.dproj`, Win64, Debug e Release: tutti OK,
nessun warning né hint nuovo.

### Resta aperto

- § 4.1 semplificazione del caricamento encoding (opzionale, nessun bug)
- l'interruttore `AutoRefreshWhenEditing` per l'editor SVG (funzionalità)
- ~~l'elemento nell'interfaccia della form About che apra `HELP_URL`~~ **fatto**: la `LinkLabel` sotto il logo Ethea prende la didascalia da `HELP_URL` in `FormCreate`, invece di averla scritta nel DFM
- ~~l'URL di documentazione di SVGShellExtensions~~ **fatto**: `https://ethea.it/componenti/svgshellextensions/`

---

## Il gutter di SynEdit non seguiva lo stile VCL — 2 settembre 2026

**Sintomo:** passando al tema scuro il gutter resta grigio chiaro con i numeri
di riga neri, mentre l'area di testo diventa correttamente scura.

### Causa: SynEdit non e' affatto style-aware

`SynEdit.pas` **non consulta mai `StyleServices`**. In tutta la libreria solo
tre unit lo fanno — `SynCompletionProposal.pas`, `SynEditScrollBars.pas` e
`SynEditMiscClasses.pas`, quest'ultima per le sole annotazioni della barra di
scorrimento. Il gutter non e' coinvolto.

`TSynGutter.Create` inizializza i colori con costanti di sistema **grezze**:

```pascal
FColor := clBtnFace;
FBorderColor := clWindow;
FGradientStartColor := clWindow;
FGradientEndColor := clBtnFace;
```

Senza passare da `StyleServices.GetSystemColor`, `clBtnFace` resta il grigio
di Windows (`#F0F0F0`) e il font del gutter il nero di sistema, qualunque sia
lo stile VCL attivo.

**Perche' allora l'area di testo funziona?** Perche' non e' SynEdit a farlo, e'
l'applicazione: `UpdateHighlighter` calcola
`StyleServices.GetSystemColor(clWindow)` e lo stampa sul `Background` di
**ogni** attributo dell'highlighter. Il gutter, che non e' coperto da nessun
attributo, non veniva toccato da nessuno. E `uSettings.pas` non legge ne'
scrive **alcun** colore del gutter, quindi non c'era nemmeno una preferenza
utente a coprire il caso.

### I quattro colori che il gutter disegna davvero

Prima di scegliere cosa impostare ho cercato dove ciascun colore viene letto in
fase di disegno:

| colore | letto in |
|---|---|
| `Gutter.Color` | `SynEdit.pas:2468` — `TSynDWrite.SolidBrush(fGutter.Color)`, fondo piatto |
| `Gutter.GradientStartColor` / `GradientEndColor` | `SynEdit.pas:2462-2463`, quando `Gradient` e' attivo |
| `Gutter.BorderColor` | `SynEditMiscClasses.pas:2386` |
| `Gutter.Font.Color` | `SynEditMiscClasses.pas:2297`, **solo se `UseFontStyle` e' True** |

L'ultima riga e' il dettaglio che conta. `PaintLineNumbers` fa:

```pascal
if Gutter.UseFontStyle then
begin
  TextFormat := Gutter.FTextFormat;
  FontColor := Gutter.Font.Color;
end
else
begin
  TextFormat := SynEdit.TextFormat;
  FontColor := SynEdit.Font.Color;       // <-- ripiego sul font dell'editor
end;
```

Quindi impostare solo `Gutter.Font.Color` avrebbe lasciato il difetto in piedi
per chi disattiva `UseFontStyle` nelle opzioni dell'editor. Per questo viene
impostato anche il colore del font dell'editor.

### Correzione

Nuovo metodo `ApplyStyleToGutter(AGutter: TSynGutter)` in entrambi gli editor,
che risolve i cinque colori attraverso `StyleServices.GetSystemColor`, cioe' la
stessa API che l'applicazione usa gia' per l'area di testo.

Chiamato in **due** punti, perche' le due rotte del cambio tema sono distinte:

| punto | copre |
|---|---|
| `InitEditorOptions`, su `FEditorOptions.Gutter` | tutti gli editor, perche' `AssignTo` fa `Gutter.Assign` e copia anche `Font` e `Color` |
| `UpdateHighlighter`, sul gutter dell'editor passato | `UpdateHighlighters`, che rinfresca ogni editor aperto **senza** passare dal contenitore delle opzioni |

In `InitEditorOptions` vengono impostati anche `Font.Color` e `Color` del
contenitore. Il secondo chiude un caso vicino nell'editor markdown: quando
`HighlightMarkdownText` e' disattivato l'highlighter viene staccato
(`Highlighter := nil`) e il fondo del testo arriva da `TCustomSynEdit.Color`,
che con i colori grezzi sarebbe rimasto chiaro anche in tema scuro.

L'ordine in `UpdateFromSettings` e' quello giusto senza modifiche:
`UpdateApplicationStyle` cambia lo stile **prima** di `InitEditorOptions`,
quindi `GetSystemColor` restituisce gia' i colori del nuovo stile.

### Verifica

Compilato un programma che carica gli 81 file `.vsf` disponibili (redist di RAD
Studio piu' CatalogRepository) e per ogni stile registrato in `uSettings.pas`
stampa esattamente i colori che `ApplyStyleToGutter` assegna, con il rapporto
di contrasto WCAG fra fondo del gutter e numeri:

| stile | fondo gutter | numeri | bordo | contrasto |
|---|---|---|---|---|
| Glow | `#242930` | `#FFFFFF` | `#191D22` | 14,64:1 |
| Windows10 Dark | `#000000` | `#FFFFFF` | `#000000` | 21,00:1 |
| Windows10 SlateGray | `#37474F` | `#FFFFFF` | `#4B5A62` | 9,65:1 |
| Windows11 Impressive Dark | `#16253D` | `#FFFFFF` | `#121F35` | 15,37:1 |
| Windows11 Modern Dark | `#2B2B2B` | `#FFFFFF` | `#1F1F1F` | 14,16:1 |
| Windows11 Polar Dark | `#343648` | `#FFFFFF` | `#2A2C3C` | 11,88:1 |
| Windows10 BlackPearl | `#212B35` | `#FFFFFF` | `#444E59` | 14,37:1 |
| Windows10 Blue Whale | `#22334C` | `#E1E1E1` | `#202F46` | 9,76:1 |
| Windows | `#F0F0F0` | `#000000` | `#FFFFFF` | 18,43:1 |
| Windows10 | `#F3F3F3` | `#000000` | `#FFFFFF` | 18,93:1 |
| Windows11 Modern Light | `#F3F3F3` | `#000000` | `#FFFFFF` | 18,93:1 |
| Flat UI Light | `#FFFFFF` | `#000000` | `#FFFFFF` | 21,00:1 |
| **colori grezzi (comportamento precedente)** | **`#F0F0F0`** | **`#000000`** | `#FFFFFF` | 18,43:1 |

Tutti e otto gli stili scuri danno un gutter scuro con numeri chiari, con
contrasto fra 9,65:1 e 21:1 — sempre oltre la soglia AA di 4,5:1. I quattro
stili chiari restituiscono gli stessi valori di prima, quindi non c'e'
regressione sul tema chiaro. L'ultima riga e' il difetto: i colori grezzi danno
`#F0F0F0` su `#000000` **qualunque** sia lo stile attivo.

### Nota

La correzione e' nell'applicazione, non in SynEdit. Renderlo style-aware a
monte sarebbe piu' elegante — il gutter potrebbe risolvere da se' i colori
lasciati al valore di default — ma vorrebbe dire modificare una dipendenza
vendored e distinguere fra "colore di default" e "colore scelto dall'utente",
distinzione che oggi non esiste perche' `TColor` non ha un valore "non
impostato" per questi campi.
