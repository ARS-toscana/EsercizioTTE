# EsercizioTTE

Questo repository contiene un esempio di script per la conduzione di una Target Trial Emulation basata su dati amministrativi italiani. Lo scopo di questo codice è didattico, e i dati contenuti nel repository sono simulati.

Lo script implementa lo studio simulato il cui protocollo si trova a [questo link](https://docs.google.com/document/d/1I-wd7GWlaKxPp6ZlQHGuis6ksHDSb8MfDjzSG0-qWzU/edit?tab=t.0)

## Struttura del codice

Il codice è ispirato massicciamente dal codice dei Case Studies 3, 4 e 5 del progetto europeo [TARGET-EU](https://github.com/target-roc19), finanziato dalla European Medicines Agency e condotto dal network [EU-PE&PV](https://www.uu.nl/en/research/eu-pepv-research-network). La struttura del programma segue un flusso introdotto nel 2016 da [Gini e colleghi](https://pubmed.ncbi.nlm.nih.gov/27014709/), sviluppato nel progetto europeo [IMI-ConcePTION](https://github.com/IMI-ConcePTION/ConceptionTools/wiki) e attualmente mantenuto dall'associazione europea [VAC4EU](https://vac4eu.org/) 

La pipeline concettuale prevede 6 tipi di dataset (D1, ..., D6) e 5 tipi di trasformazioni intermedie tra loro (T1, ..., T5). Questa è la rappresentazione grafica della pipeline

<img src="https://github.com/user-attachments/assets/72f025cb-c98f-4e29-99d7-f546cf6f7aa9" width="1000"  alt="Graphical representation of the ConcePTION pipeline">

I tipi di dataset e trasformazioni sono le seguenti

- D1 - Dati originali: questa è la data source originale, raccolta per ragioni che normalmente non includono la ricerca o il quesito specifico
- T1 - Extract Transform and Load (ETL): questo è il processo di estratte da D1 una istanza ristretta, sufficiente per condurre uno studio, e di convertirla in un Common Data Model; in questo esercizio utilizziamo una espansione del CDM di The ShinISS (pù dettagli qui sotto)
- D2 - Common Data Model: questo è il risultato della conversione: è una <i>istanza del CDM</i>
- T2 - Crea variabili di studio: questo è il processo con cui vengono costruite variabili sulla base dei dati contenuti nell'istanza
- D3 - Variabili di studio: sono dataset che contengono i soggetti dello studio o altre unità di osservazioni intermedie di interesse (gravidanze, episoddi di trattamento con un farmaco, ...) e le loro variabili, necessarie per implementare il protocollo di studio.
- T3 - Applica il disegno di studio: questo è il processo con cui le variabili di studio vengono utilizzate per applicare il disegno di studio, come previsto dal protocollo, e quindi selezionare, stimare propensity scores, appaiare, censurare, ...
- D4 - Dataset analitici: dataset in cui sono custodite le variabili che hanno una funzione nell'analisi, come esposizione, t0, tempo alla fine del follow up...
- T4 - Conduci l'analisi: questo è il processo con cui si conducono analisi descrittive o si stimano modelli statistici
- D5 - Risultati: dataset in cui vengono depositati i risultati dell'analisi (conteggi, medie, mediane, ..., parametri di modelli, ...)
- T5 - Visualizza: creazione di figure e tabelle
- D6 - Figure e tabelle 

La pipeline si adatta naturalmente a studi multicentrici. In questo caso, nella pipeline si prevede che tutti i dataset D1, D2, D3 e D4 siano custoditi presso i soggetti che hanno accesso ai dati. T1 è programmato dai soggetti che hanno accesso ai dati, mentre T2, T3 e T4 sono inclusi in un programma centrale. I dataset D5 sono dataset di numeri, e possono essere condivisi con tutto il gruppo di ricerca. 

## Dati simulati

I dati simulati D2 utilizzati in questo repository seguono una espansione del CDM di [TheShinISS](https://pmc.ncbi.nlm.nih.gov/articles/PMC8507511/), che è stato disegnato dall'Istituto Superiore di Sanità e altri colleghi italiani per condurre studi multicentrici su dati amministrativi italiani. Specificamente il modello di dati è il seguente

## Come usare questo repository
