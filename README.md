# EsercizioTTE

Questo repository contiene un esempio di script per la conduzione di una Target Trial Emulation basata su dati amministrativi italiani. Lo scopo di questo codice è didattico, e i dati contenuti nel repository sono simulati.

Lo script implementa lo studio simulato il cui protocollo si trova a [questo link](https://docs.google.com/document/d/1I-wd7GWlaKxPp6ZlQHGuis6ksHDSb8MfDjzSG0-qWzU/edit?tab=t.0).

Il codice è ispirato massicciamente al codice dei Case Studies 3, 4 e 5 del progetto europeo [TARGET-EU](https://github.com/target-roc19), finanziato dalla European Medicines Agency, condotto dal network [EU-PE&PV](https://www.uu.nl/en/research/eu-pepv-research-network) e programmato da istituzioni dell'associazione europea [VAC4EU](https://vac4eu.org/).

La struttura del codice specifica di questo studio è illustrata da [questo grafico](https://comfy-sherbet-4e44c6.netlify.app/), il cui formato è descritto [più sotto](#formato-del-grafico-e-struttura-del-codice-di-questo-specifico-studio).

## Indice

- [Struttura concettuale del codice](#struttura-concettuale-del-codice)
- [Contenuto del repository](#contenuto-del-repository)
  - [Dati simulati](#dati-simulati)
  - [Formato del grafico e struttura del codice di questo specifico studio](#formato-del-grafico-e-struttura-del-codice-di-questo-specifico-studio)
- [Come usare questo repository](#come-usare-questo-repository)
  - [Scaricare il codice](#scaricare-il-codice)
  - [Usare il codice per generare risultati (modalità TEST <- F)](#usare-il-codice-per-generare-risultati-modalità-test---f)
  - [Usare il codice per testare il processo di un singolo step (modalità TEST <- T)](#usare-il-codice-per-testare-il-processo-di-un-singolo-step-modalità-test---t)

## Struttura concettuale del codice

La struttura del programma segue un flusso introdotto nel 2016 da [Gini e colleghi](https://pubmed.ncbi.nlm.nih.gov/27014709/), sviluppato nel progetto europeo [IMI-ConcePTION](https://github.com/IMI-ConcePTION/ConceptionTools/wiki) e attualmente mantenuto dall'associazione europea [VAC4EU](https://vac4eu.org/).

La pipeline concettuale prevede 6 tipi di dataset (D1, ..., D6) e 5 tipi di trasformazioni intermedie tra loro (T1, ..., T5). Questa è la rappresentazione grafica della pipeline:

<img src="https://github.com/user-attachments/assets/72f025cb-c98f-4e29-99d7-f546cf6f7aa9" width="1000"  alt="Graphical representation of the ConcePTION pipeline">

I tipi di dataset e di trasformazioni sono i seguenti:

- <b>D1 - Dati originali</b>: questa è la data source originale, raccolta per ragioni che normalmente non includono la ricerca o il quesito specifico
- <b>T1 - Extract Transform and Load (ETL)</b>: questo è il processo di estrarre da D1 una istanza ristretta, sufficiente per condurre lo studio, e di convertirla in un Common Data Model; in questo esercizio utilizziamo una espansione del CDM di TheShinISS (più dettagli qui sotto)
- <b>D2 - Istanza del Common Data Model</b>: questo è il risultato della conversione
- <b>T2 - Crea variabili di studio</b>: questo è il processo con cui vengono costruite variabili sulla base dei dati contenuti nell'istanza
- <b>D3 - Variabili di studio</b>: sono dataset che contengono i soggetti dello studio o altre unità di osservazione intermedie di interesse (gravidanze, episodi di trattamento con un farmaco, ...) e le loro variabili, necessarie per implementare il protocollo di studio.
- <b>T3 - Applica il disegno di studio</b>: questo è il processo con cui le variabili di studio vengono utilizzate per applicare il disegno di studio, come previsto dal protocollo, e quindi selezionare, stimare propensity scores, appaiare, censurare, ...
- <b>D4 - Dataset analitici</b>: dataset in cui sono custodite le variabili che hanno una funzione nell'analisi, come esposizione, t0, tempo alla fine del follow-up...
- <b>T4 - Conduci l'analisi</b>: questo è il processo con cui si conducono analisi descrittive o si stimano modelli statistici
- <b>D5 - Risultati</b>: dataset in cui vengono depositati i risultati dell'analisi (conteggi, medie, mediane, ..., parametri di modelli, ...)
- <b>T5 - Visualizza</b>: creazione di figure e tabelle
- <b>D6 - Figure e tabelle</b> 

La pipeline si adatta naturalmente a studi multicentrici. In questo caso, nella pipeline si prevede che tutti i dataset D1, D2, D3 e D4 siano custoditi presso i soggetti che hanno accesso ai dati. T1 è programmato dai soggetti che hanno accesso ai dati, mentre T2, T3 e T4 sono inclusi in un programma centrale. I dataset D5 sono dataset di numeri, e possono essere condivisi con tutto il gruppo di ricerca. 

In questo repository T1 è omesso. Vi si trovano un D2 sufficiente a condurre lo studio, e tutte le trasformazioni T2, ..., T5. Qui sotto si spiega come è possibile utilizzare il repository per generare D3, D4, D5 e, soprattutto, D6, ovvero le tabelle e figure dello studio.

[↑ Torna all'indice](#indice)

## Contenuto del repository

### Dati simulati

I dati simulati D2 utilizzati in questo repository seguono una espansione del CDM di [TheShinISS](https://pmc.ncbi.nlm.nih.gov/articles/PMC8507511/), che è stato disegnato dall'Istituto Superiore di Sanità e da altri colleghi italiani per condurre studi multicentrici su dati amministrativi italiani. 

Tutte le tabelle sono collegate dall'identificativo pseudonimizzato della persona, `id`. Le date sono registrate come testo nel formato `AAAAMMGG` (ad esempio `20190318`); un valore vuoto indica che la data manca o non è applicabile.

<img width="1584" height="492" alt="er_it" src="https://github.com/user-attachments/assets/97560586-c3e4-4ccd-bf87-a3c2181f3f5d" />


| Tabella | Contenuto | Un record è | Chiave |
|---|---|---|---|
| `ANAGRAFE_ASSISTITI` | Anagrafe regionale degli assistiti dal servizio sanitario regionale | un periodo di assistenza di una persona | `id`, `data_inizioass` |
| `SDO` | Schede di Dimissione Ospedaliera | una diagnosi registrata in un ricovero | nessuna: `id`, `data_a`, `dia`, `dia_col` possono ripetersi |
| `PS` | Accessi al Pronto Soccorso | una diagnosi registrata in un accesso al pronto soccorso | nessuna |
| `EXE` | Esenzioni dalla partecipazione alla spesa per patologia | un'esenzione di una persona | nessuna |
| `SPF` | Farmaci erogati dalle farmacie territoriali | un'erogazione | nessuna: sono possibili record identici |
| `FED` | Farmaci a erogazione diretta da parte del servizio sanitario | un'erogazione | nessuna: sono possibili record identici |

Una persona può avere più di un record in `ANAGRAFE_ASSISTITI`, uno per ciascun periodo di assistenza. Tutte le altre tabelle hanno zero, uno o più record per persona.

#### ANAGRAFE_ASSISTITI

<details>
<summary>Mostra le variabili di ANAGRAFE_ASSISTITI</summary>

| Variabile | Tipo | Valori | Descrizione |
|---|---|---|---|
| `id` | carattere | es. `P0000001` | Identificativo pseudonimizzato della persona |
| `sesso` | intero | `1` = maschio, `2` = femmina | Sesso |
| `data_inizioass` | data | `AAAAMMGG` | Inizio del periodo di assistenza presso il servizio sanitario regionale |
| `data_fineass` | data | `AAAAMMGG` | Fine del periodo di assistenza; la data di estrazione se la persona è ancora assistita |
| `datadec` | data | `AAAAMMGG`, vuota se in vita | Data di decesso |
| `datanas` | data | `AAAAMMGG` | Data di nascita |

</details>

#### SDO – Schede di Dimissione Ospedaliera

<details>
<summary>Mostra le variabili di SDO</summary>

| Variabile | Tipo | Valori | Descrizione |
|---|---|---|---|
| `id` | carattere | | Identificativo della persona, collegato a `ANAGRAFE_ASSISTITI` |
| `data_a` | data | `AAAAMMGG` | Data di ricovero |
| `data_d` | data | `AAAAMMGG` | Data di dimissione |
| `dia` | carattere | codice ICD-9-CM con o senza punto, es. `410`, `2720`, `272.0` | Diagnosi |
| `dia_col` | intero | `1` = diagnosi principale, `2`, `3`, … = diagnosi secondarie | Posizione della diagnosi nella scheda di dimissione |

</details>

#### PS – Accessi al Pronto Soccorso

<details>
<summary>Mostra le variabili di PS</summary>

| Variabile | Tipo | Valori | Descrizione |
|---|---|---|---|
| `id` | carattere | | Identificativo della persona, collegato a `ANAGRAFE_ASSISTITI` |
| `data_a` | data | `AAAAMMGG` | Data di accesso al pronto soccorso |
| `dia` | carattere | codice ICD-9-CM con o senza punto | Diagnosi |

</details>

#### EXE – Esenzioni per patologia

<details>
<summary>Mostra le variabili di EXE</summary>

| Variabile | Tipo | Valori | Descrizione |
|---|---|---|---|
| `id` | carattere | | Identificativo della persona, collegato a `ANAGRAFE_ASSISTITI` |
| `datai` | data | `AAAAMMGG` | Data di inizio dell'esenzione |
| `dataf` | data | `AAAAMMGG`, vuota se ancora valida | Data di fine dell'esenzione |
| `dia` | carattere | codice ICD-9-CM con o senza punto | Diagnosi che dà diritto all'esenzione |

</details>

#### SPF e FED – Farmaci erogati

Le due tabelle hanno la stessa struttura: `SPF` contiene i farmaci erogati dalle farmacie territoriali, `FED` i farmaci erogati direttamente dal servizio sanitario (farmacie ospedaliere e distribuzione per conto).


<details>
<summary>Mostra le variabili di SPF e FED</summary>

| Variabile | Tipo | Valori | Descrizione |
|---|---|---|---|
| `id` | carattere | | Identificativo della persona, collegato a `ANAGRAFE_ASSISTITI` |
| `datasped` | data | `AAAAMMGG` | Data di erogazione |
| `pezzi` | intero | ≥ 1 | Numero di confezioni erogate |
| `aic` | carattere | 9 cifre | Codice di autorizzazione all'immissione in commercio (AIC) del prodotto |
| `atc` | carattere | codice ATC, a qualsiasi livello (es. `A10`, `C01DA`) | Classificazione Anatomica Terapeutica Chimica del farmaco |

</details>


### Formato del grafico e struttura del codice di questo specifico studio

La struttura del codice specifica di questo studio è illustrata da [questo grafico](https://comfy-sherbet-4e44c6.netlify.app/). Il formato del grafico è stato introdotto da VAC4EU e utilizzato in diversi progetti condotti per la European Medicines Agency, per esempio il [CS3 di TARGET-EU](https://target-roc19.github.io/case-study-03-dapagliflozin-public/diagram.html).

Nel grafico:

- ogni cerchio rappresenta una trasformazione T* e premendolo si viene trasportati al codice corrispondente nel repository
- ogni rettangolo rappresenta un dataset D* e premendolo si viene trasportati al codebook di tale dataset

[↑ Torna all'indice](#indice)

## Come usare questo repository

### Scaricare il codice

Per usare il repository, bisogna installare RStudio, scaricare il codice da [Code > Download ZIP](https://github.com/ARS-toscana/EsercizioTTE/archive/refs/heads/main.zip), scompattare la cartella e aprire il file `.Rproj` in RStudio.

Si può anche clonare il repository sul proprio computer. Le istruzioni per fare questo sono disponibili [qui](https://docs.posit.co/ide/user/ide/guide/tools/version-control.html).

### Usare il codice per generare risultati (modalità TEST <- F)

- Aprire il file `to_run.R`
- Lanciarlo: si può lanciare tutto insieme o un passo alla volta.
- Nella cartella del progetto si troveranno:
   - una cartella `g_intermediate`, che contiene i dataset D3 e D4;
   - una cartella `g_export`, che contiene i dataset D5 e D6.

### Usare il codice per testare il processo di un singolo step (modalità TEST <- T)

Molti step hanno una modalità di test, pensata per capire che cosa fa uno step e per verificarne il funzionamento su pochi record costruiti a mano, di cui si conosce in anticipo il risultato atteso.

Ogni step che prevede questa modalità ha una propria sottocartella in `i_test`: per esempio, lo step `p_steps/03_T3_10_crea_D4_esp_comp_pesi.R` si testa con i dati in `i_test/test_D4_esp_comp_pesi`. La sottocartella contiene:

- i dataset di input dello step, con pochi record (file `.rds`, `.RData` o `.csv`);
- in alcuni casi, le stesse tabelle in formato `.xlsx`, più facili da leggere e modificare, e uno script `load.R` che le converte nel formato letto dallo step;
- in alcuni casi, uno script `simulate_input.R` che genera i dati di input;
- in alcuni casi, una cartella `ground_truth` con l'output atteso;
- una cartella `g_output`, in cui lo step salva i propri risultati quando viene lanciato in modalità test.

Per testare uno step:

1. In `to_run.R`, porre `TEST <- T` e lanciare solo il preambolo, cioè le righe fino al caricamento dei parametri (le chiamate a `source()` dei file in `p_parameters`), senza lanciare gli step.
2. Aprire lo step di interesse in `p_steps`. All'inizio dello step c'è un blocco come questo:

   ```r
   if (TEST){
     testname <- "test_D4_esp_comp_pesi"
     thisdirinput <- file.path(dirtest,testname)
     thisdiroutput <- file.path(thisdirinput,"g_output")
     dir.create(thisdiroutput, showWarnings = F)
   }else{
     thisdirinput <- dirtemp
     thisdiroutput <- dirtemp
   }
   ```

   In modalità test lo step legge i dati dalla sottocartella di test e scrive i risultati nella sua cartella `g_output`, invece che in `g_intermediate` e `g_export`: i risultati dell'esecuzione completa non vengono toccati.
3. Ispezionare i dati di input. Per modificarli, si possono cambiare i file `.xlsx` e lanciare `load.R`, oppure modificare e lanciare `simulate_input.R`. Attenzione: `load.R` svuota l'ambiente di lavoro di R, quindi dopo averlo lanciato bisogna rilanciare il preambolo di `to_run.R`.
4. Lanciare lo step e ispezionare l'output in `g_output`. Se c'è una cartella `ground_truth`, confrontare l'output con il risultato atteso.
5. Per tornare all'uso normale, rimettere `TEST <- F` in `to_run.R` e rilanciare il preambolo.

[↑ Torna all'indice](#indice)
