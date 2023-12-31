       program-id.     Zoom-GT.
       REMARKS. NUMERO MASSIMO DI CAMPI IN GRID 10.

      *************************
       working-storage section.
      *************************
       copy "acugui.def".
       copy "acucobol.def".
       copy "crtvars.def".

       78 78-walt              value 1.
       78 78-win-box           value 2.
       78 78-about             value 3.
       78 78-walt-file         value 4.
       78 78-walt-delim        value 5.
       78 78-quit              value 27.

           copy "xzoom-common.def".
           copy "xzoom3.def".
           copy "externals.def".

       77  param-size pic 9(5).
       77  nome-file  pic X(256).

       77  stato-zoom signed-long.

       77  idx pic 9(3).

       77  filler  pic 9 value 0.
         88 TuttiIRec value 1, false 0.

       77  filler  pic 9 value 0.
         88 destinif value 1, false 0.

       LINKAGE SECTION.
       77  como-file   pic x(20).
       77  como-record pic x(32000).

      ************************************************************************
       procedure division using como-file, como-record.
      ************************************************************************
       MAIN-LOGIC.

           set environment "XZOOM_HIDE_CLOCK"     to 1.

           set environment "XZOOM_WINDOW_DELAYED" to 1.

      * PER LO ZOOM DEGLI ARTICOLI PER NON VEDERE LA SCRITTA NULL NEI SOTTOGRUPPI
           set environment "XZOOM_NULL_TEXT_NUMERIC" to "#Blank#".
           set environment "XZOOM_NULL_TEXT_ALPHA"   to "#Blank#".

           set environment "XZOOM_FONT" to "Calibri,12".

           call "C$PARAMSIZE" 
                using 2, 
                giving param-size.

           set environment "XZOOM_LAYOUT" to "Grid".


           evaluate como-file
           when "clienti"
                perform PREPARA-CLIENTI
           when "clienti-alfa"
                perform PREPARA-CLIENTI-ALFA
           when "articoli"
                perform PREPARA-ARTICOLI
           when "articoli-alfa"
                perform PREPARA-ARTICOLI-ALFA
           when "codiva"
                perform PREPARA-CODIVA       
           when "codiva-esente"
                perform PREPARA-CODIVA-ESENTE
           when "tipocli-alfa"
                perform PREPARA-TIPOCLI-ALFA
           when "tipocli"
                perform PREPARA-TIPOCLI
           when "tlistini"
                perform PREPARA-TLISTINI
           when "tschede"
                perform PREPARA-TSCHEDE
           when "macrogroups"
                perform PREPARA-MACROGROUPS
           when "groups"
                perform PREPARA-GROUPS    
           when "intexe"
                perform PREPARA-INTEXE
           when "duration"
                perform PREPARA-DURATION
           when "wodmap"
                perform PREPARA-WODMAP
           when "wodmap-alfa"
                perform PREPARA-WODMAP-ALFA 
           when "zoom-exe-mcg"             
                perform PREPARA-ZOOM-EXE-MCG
           when "zoom-wodbook"
                perform PREPARA-ZOOM-WODBOOK
           when "tmp-grp-exe"             
                perform PREPARA-TMP-GRP-EXE
           when "exercises"
                perform PREPARA-EXERCISES
           when "wodbook"
                perform PREPARA-WODBOOK

           when other
                display message box "guarda che non � ancora stato fatto
      -                             "IL PARAGRAFO DI PREPARAZIONE PER QU
      -                             "ESTO FILE"
                exit program
           end-evaluate.


           call "XZOOM" using xzoom-linkage 
                                 COMO-RECORD(1:PARAM-SIZE)
                                 giving stato-zoom.

           cancel "XZOOM".          

           goback stato-zoom.

      ***---
       PREPARA-CLIENTI-ALFA.
           perform PREPARA-CLIENTI.    
           move  1   to xzoom-file-key.

      ***---
       PREPARA-CLIENTI.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  16                      to xzoom-lw.
           move  130                     to xzoom-sw.
           move "clienti"                to xzoom-file-name(1).

      * CAMPO 1
           add 1 to idx
           move  6                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)       to true.
      
      * CAMPO 2
           add 1 to idx.
           move  50                      to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  25                      to xzoom-field-column(idx).
           move "Ragione Sociale/Cognome"to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx.
           move  50                      to xzoom-field-length(idx).
           move  56                      to xzoom-field-offset(idx).
           move  14                      to xzoom-field-column(idx).
           move "Nome"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
                                       
      * CAMPO 4
           add 1 to idx.
           move  50                      to xzoom-field-length(idx).
           move  106                     to xzoom-field-offset(idx).
           move  19                      to xzoom-field-column(idx).
           move "Indirizzo"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 5
           add 1 to idx.
           move  50                      to xzoom-field-length(idx).
           move  156                     to xzoom-field-offset(idx).
           move  14                      to xzoom-field-column(idx).
           move "Citt�"                  to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 6
           add 1 to idx.
           move  2                       to xzoom-field-length(idx).
           move  206                     to xzoom-field-offset(idx).
           move  4                       to xzoom-field-column(idx).
           move "Prov"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 7
           add 1 to idx.
           move  50                      to xzoom-field-length(idx).
           move  276                     to xzoom-field-offset(idx).
           move  18                      to xzoom-field-column(idx).
           move "E-mail"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 8
           add 1 to idx.
           move  20                      to xzoom-field-length(idx).
           move  326                     to xzoom-field-offset(idx).
           move  9                       to xzoom-field-column(idx).
           move "Telefono"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 9
           add 1 to idx.
           move  20                      to xzoom-field-length(idx).
           move  346                     to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Mobile"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
                                                               
           move  idx                    to xzoom-fields.
           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-ARTICOLI-ALFA.
           perform PREPARA-ARTICOLI.
           move  1   to xzoom-file-key.

      ***---
       PREPARA-ARTICOLI.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  16                      to xzoom-lw.
           move  81                      to xzoom-sw.
           move "articoli"               to xzoom-file-name(1).

      * CAMPO 1
           add 1 to idx
           move  6                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)       to true.
      
      * CAMPO 2
           add 1 to idx.
           move  50                      to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  50                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
                                                               
           move  idx                    to xzoom-fields.
           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-CODIVA-ESENTE.
           perform PREPARA-CODIVA.   

           set xzoom-when-true(1)    to true.
           set xzoom-begin-with(1)   to true.
           set xzoom-ignore-case(1)  to true.
                                 
           move como-record(54:5)    to xzoom-wild-value(1).
           move 5                    to xzoom-wild-value-length(1).
           move 5                    to xzoom-wild-length(1).
           move 53                   to xzoom-wild-offset(1).


      ***---
       PREPARA-CODIVA.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  64                      to xzoom-sw.
           move "codiva"                 to xzoom-file-name(1).
           move  3                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  3                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)       to true.
      
      * CAMPO 2
           add 1 to idx.
           move  50                      to xzoom-field-length(idx).
           move   3                      to xzoom-field-offset(idx).
           move  34                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx.
           move   5                      to xzoom-field-length(idx).
           move  53                      to xzoom-field-offset(idx).
           move   7                      to xzoom-field-column(idx).
           move "Aliquota"               to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move 2                        to xzoom-field-dec(idx).
           move "##0,00"                 to xzoom-field-fmt(idx).

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-TIPOCLI-ALFA.
           perform PREPARA-TIPOCLI.
           move  1   to xzoom-file-key.


      ***---
       PREPARA-TIPOCLI.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  65                      to xzoom-sw.
           move "tipocli"                to xzoom-file-name(1).
           move  3                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  6                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  7                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-ft-alpha(idx)       to true.
      
      * CAMPO 2
           add 1 to idx.
           move  50                      to xzoom-field-length(idx).
           move   6                      to xzoom-field-offset(idx).
           move  35                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx.
           move   5                      to xzoom-field-length(idx).
           move  56                      to xzoom-field-offset(idx).
           move   5                      to xzoom-field-column(idx).
           move "Sconto"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move 2                        to xzoom-field-dec(idx).
           move "##0,00"                 to xzoom-field-fmt(idx).

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

      ***---
       PREPARA-TLISTINI.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  94                      to xzoom-sw.
           move "tlistini"               to xzoom-file-name(1).
           move  5                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 5                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "####0"                  to xzoom-field-fmt(idx).
      
      * CAMPO 2
           add 1 to idx.
           move  50                      to xzoom-field-length(idx).
           move   5                      to xzoom-field-offset(idx).
           move  40                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx.
           move   1                      to xzoom-field-length(idx).
           move  55                      to xzoom-field-offset(idx).
           move   4                      to xzoom-field-column(idx).
           move "Tipo"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 4
           add 1 to idx.
           move   6                      to xzoom-field-length(idx).
           move  56                      to xzoom-field-offset(idx).
           move   8                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 5
           add 1 to idx.
           move   8                      to xzoom-field-length(idx).
           move  62                      to xzoom-field-offset(idx).
           move  11                     to xzoom-field-column(idx).
           move "Inizio Validit�"        to xzoom-field-name(idx).  
           set xzoom-field-unsigned(Idx) to true.
           set xzoom-ft-display(Idx)     to true.
           move 8                        to xzoom-field-digits(Idx).
           move   0                      to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA (GGG)"    to xzoom-field-fmt(Idx).

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

      ***---
       PREPARA-TSCHEDE.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  100                     to xzoom-sw.
           move "tschede"                to xzoom-file-name(1).
           move "clienti"                to xzoom-file-name(2).
           move  4                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  6                       to xzoom-field-length(idx).
           move  6                       to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Cliente"                to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 2
           add 1 to idx.
           move  2                       to xzoom-field-file(Idx).
           move  1                       to xzoom-field-rel(Idx).    
           move  50                      to xzoom-field-length(idx).
           move   6                      to xzoom-field-offset(idx).
           move  35                      to xzoom-field-column(idx).
           move "Ragione Sociale/Cognome"to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx.
           move  2                       to xzoom-field-file(Idx).
           move  1                       to xzoom-field-rel(Idx).    
           move  50                      to xzoom-field-length(idx).
           move  56                      to xzoom-field-offset(idx).
           move  20                      to xzoom-field-column(idx).
           move "Nome"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 4
           add 1 to idx.
           move   1                      to xzoom-field-file(Idx).
           move   0                      to xzoom-field-rel(Idx).    
           move   8                      to xzoom-field-length(idx).
           move  12                      to xzoom-field-offset(idx).
           move  12                      to xzoom-field-column(idx).
           move "Data Creazione"         to xzoom-field-name(idx).  
           set xzoom-field-unsigned(Idx) to true.
           set xzoom-ft-display(Idx)     to true.
           move 8                        to xzoom-field-digits(Idx).
           move 0                        to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA (GGG)"    to xzoom-field-fmt(Idx).

      *    1
      *    File Reference Settings - Relazione schede > clienti
           move   1                to Idx.
           move   1                to xzoom-ref-m-file (Idx).
           move   2                to xzoom-ref-s-file (Idx).
           move   0                to xzoom-ref-s-key  (Idx).
           move   1                to xzoom-ref-fields (Idx).
           set xzoom-ref-join-outer(Idx) to true.
           
      *            Master File Settings
           move   6  to xzoom-ref-m-length(Idx, 1).
           move   6  to xzoom-ref-m-offset (Idx, 1).
           set xzoom-ref-m-unsigned (Idx, 1) to true.
           
      *            Slave File Settings
           move 6 to xzoom-ref-s-length(Idx, 1).
           move 0 to xzoom-ref-s-offset (Idx, 1).
           set xzoom-ref-s-unsigned (Idx, 1) to true.

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

      ***---
       PREPARA-MACROGROUPS.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  1   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  50                      to xzoom-sw.
           move "macrogroups"            to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  5                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 2
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  40                      to xzoom-field-length(idx).
           move   5                      to xzoom-field-offset(idx).
           move  100                     to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.     

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

      ***---
       PREPARA-GROUPS.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  1   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  50                      to xzoom-sw.
           move "groups"                 to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  5                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 2
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move   5                      to xzoom-field-offset(idx).
           move  100                     to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.     

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

      ***---
       PREPARA-DURATION.     
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  80                      to xzoom-sw.
           move "duration"               to xzoom-file-name(1).

      * CAMPO 1
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  2                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.             
      
      * CAMPO 2
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move   2                      to xzoom-field-offset(idx).
           move  22                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.     
      
      * CAMPO 3
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  2                       to xzoom-field-length(idx).
           move  102                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Esercizi"               to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 2                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "#0"                     to xzoom-field-fmt(idx). 
      
      * CAMPO 4
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  1                       to xzoom-field-length(idx).
           move  104                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Serie 1"                to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 1                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "0"                      to xzoom-field-fmt(idx).
      
      * CAMPO 5
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  1                       to xzoom-field-length(idx).
           move  105                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Serie 2"                to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 1                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "0"                      to xzoom-field-fmt(idx).
      
      * CAMPO 6
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  1                       to xzoom-field-length(idx).
           move  106                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Serie 3"                to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 1                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "0"                      to xzoom-field-fmt(idx).
      
      * CAMPO 7
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  1                       to xzoom-field-length(idx).
           move  107                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Serie 4"                to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 1                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "0"                      to xzoom-field-fmt(idx).
      
      * CAMPO 8
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  1                       to xzoom-field-length(idx).
           move  108                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Serie 5"                to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 1                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "0"                      to xzoom-field-fmt(idx).
                                                               
           move  idx                     to xzoom-fields.
           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.
                        
      ***---
       PREPARA-INTEXE.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  75                      to xzoom-sw.
           move "intexe"                 to xzoom-file-name(1).
           move  6                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  2                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  6                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 2
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  40                      to xzoom-field-length(idx).
           move   2                      to xzoom-field-offset(idx).
           move  20                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  2                       to xzoom-field-length(idx).
           move  110                     to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Min reps"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.     
      
      * CAMPO 4
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  2                       to xzoom-field-length(idx).
           move  112                     to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Max reps"               to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.     
      
      * CAMPO 5
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  1                       to xzoom-field-length(idx).
           move  116                     to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Rest/Pause"             to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.     
      
      * CAMPO 6
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  1                       to xzoom-field-length(idx).
           move  117                     to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "A tempo"                to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.     

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

      ***---
       PREPARA-WODMAP.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  60                      to xzoom-sw.
           move "wodmap"                 to xzoom-file-name(1).
           move  2                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  3                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 2
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move   3                      to xzoom-field-offset(idx).
           move  100                     to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.     

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

      ***---
       PREPARA-WODMAP-ALFA.
           perform PREPARA-WODMAP.
           move  1   to xzoom-file-key.

      ***---
       PREPARA-ZOOM-WODBOOK.  
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  4   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  130                     to xzoom-sw. 
           move ext-file                 to xzoom-file-name(1).
           move  10                      to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  18                      to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 2
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move  18                      to xzoom-field-offset(idx).
           move  20                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  8                       to xzoom-field-length(idx).
           move  118                     to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Creato il"              to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx) to true.
           set xzoom-ft-display(Idx)     to true.
           move 8                        to xzoom-field-digits(Idx).
           move   0                      to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA (GGG)"    to xzoom-field-fmt(Idx).
      
      * CAMPO 4
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  8                       to xzoom-field-length(idx).
           move  126                     to xzoom-field-offset(idx).
           move  8                       to xzoom-field-column(idx).
           move "Iniziato il"            to xzoom-field-name(idx).
           set xzoom-field-unsigned(Idx) to true.
           set xzoom-ft-display(Idx)     to true.
           move 8                        to xzoom-field-digits(Idx).
           move   0                      to xzoom-field-dec(Idx).
           move "AAAAMMGG-GG/MM/AAAA (GGG)"    to xzoom-field-fmt(Idx).
      
      * CAMPO 5
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  3                       to xzoom-field-length(idx).
           move  134                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Giorni"                 to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 3                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "##0"                    to xzoom-field-fmt(idx).
      
      * CAMPO 6
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  3                       to xzoom-field-length(idx).
           move  137                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Esercizi"               to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 3                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "##0"                    to xzoom-field-fmt(idx).
      
      * CAMPO 7
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move  140                     to xzoom-field-offset(idx).
           move  22                      to xzoom-field-column(idx).
           move "Mappatura"              to xzoom-field-name(idx).    
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 8
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move  240                     to xzoom-field-offset(idx).
           move  12                      to xzoom-field-column(idx).
           move "Durata"                 to xzoom-field-name(idx).    
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 9
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move  340                     to xzoom-field-offset(idx).
           move  12                      to xzoom-field-column(idx).
           move "Effort"                 to xzoom-field-name(idx).    
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.
                           
      ***---
       PREPARA-ZOOM-EXE-MCG.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  1   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  82                      to xzoom-sw. 
           move ext-file                 to xzoom-file-name(1).
           move  4                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  5                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  8                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 2
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  22                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move  105                     to xzoom-field-offset(idx).
           move  16                      to xzoom-field-column(idx).
           move "Intensit�"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 4
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move  205                     to xzoom-field-offset(idx).
           move  16                      to xzoom-field-column(idx).
           move "Gruppo"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.     

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

      ***---
       PREPARA-TMP-GRP-EXE.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  1   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  77                      to xzoom-sw. 
           move ext-file                 to xzoom-file-name(1).
           move  5                       to xzoom-fields.

      * CAMPO 1   
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  95                      to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  14                      to xzoom-field-column(idx).
           move "Gruppo muscolare"       to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 2
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  1                       to xzoom-field-length(idx).
           move  95                      to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Giorno"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 3
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  95                      to xzoom-field-length(idx).
           move  96                      to xzoom-field-offset(idx).
           move  22                      to xzoom-field-column(idx).
           move "Esercizio"              to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true.                   
      
      * CAMPO 4
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  3                       to xzoom-field-length(idx).
           move  191                     to xzoom-field-offset(idx).
           move  5                       to xzoom-field-column(idx).
           move "Series"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 5
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  10                      to xzoom-field-length(idx).
           move  194                     to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Reps"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

      ***---
       PREPARA-EXERCISES.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  61                      to xzoom-sw. 
           move "exercises"              to xzoom-file-name(1).
           move  5                       to xzoom-fields.

      * CAMPO 1   
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  5                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 
      
      * CAMPO 2
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  35                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.

      ***---
       PREPARA-WODBOOK.
           initialize xzoom-linkage xzoom-ext-info(1).
      
           move  0   to xzoom-file-key.
      *
           move  0                       to idx.
           move  0                       to xzoom-row.
           move  0                       to xzoom-cln.
           move  10                      to xzoom-lw.
           move  78                      to xzoom-sw. 
           move "wodbook"                to xzoom-file-name(1).
           move  4                       to xzoom-fields.

      * CAMPO 1   
           add 1 to idx
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  18                      to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Codice"                 to xzoom-field-name(idx).   
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 18                       to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "#################0"     to xzoom-field-fmt(idx).

      * CAMPO 2
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  100                     to xzoom-field-length(idx).
           move  35                      to xzoom-field-offset(idx).
           move  26                      to xzoom-field-column(idx).
           move "Descrizione"            to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  1                       to xzoom-field-length(idx).
           move  31                      to xzoom-field-offset(idx).
           move  12                      to xzoom-field-column(idx).
           move "Gg. di allenamento"     to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 4
           add 1 to idx.
           move  1                       to xzoom-field-file(Idx).
           move  0                       to xzoom-field-rel(Idx).    
           move  3                       to xzoom-field-length(idx).
           move  32                      to xzoom-field-offset(idx).
           move  10                      to xzoom-field-column(idx).
           move "Tot. esercizi"          to xzoom-field-name(idx).  
           set xzoom-al-right(idx)       to true.
           set xzoom-field-unsigned(idx) to true.
           set xzoom-ft-display(idx)     to true.
           move 3                        to xzoom-field-digits(idx).
           move 0                        to xzoom-field-dec(idx).
           move "##0"                    to xzoom-field-fmt(idx).   

           move  -1                      to xzoom-delimiter-offset.
           move  5                       to xzoom-delimiter-length.
           move "000"                    to xzoom-from-value.
           move "000"                    to xzoom-to-value.  

           set xzoom-when-true(1)    to true.
           set xzoom-begin-with(1)   to true.
           set xzoom-ignore-case(1)  to true.
                                 
           move "00"                 to xzoom-wild-value(1).
           move 2                    to xzoom-wild-value-length(1).
           move 2                    to xzoom-wild-length(1).
           move 26                   to xzoom-wild-offset(1).
