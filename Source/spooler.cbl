       program-id.     spooler.
       remarks. Questa versione di spooler è un pò particolare in quanto
                utilizza la proprietà TRANSPARENT per la jpg la quale
                però, basandosi sul settaggio delle colonne, mi varia la
                logica del programma che mi permetterà di stampare solo
                righe intere e lineari.
                'IMPORTANTE!!!' Il nome del job di stampa passato in 
                linkage nella variabile "spl-nome-job" può contenere
                al massimo 29 caratteri (non uno di più) altrimenti in
                fase di apertura con WINPRINT-SET-PRINTER va in crash 
                (Questo si verifica con Run-Time 6.0).

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       file-control.
           COPY "STAMPA.SL".
       file section.
           COPY "STAMPA.FD".
      *************************
       working-storage section.
      *************************
           copy "acugui.def".
           copy "acucobol.def".
           copy "crtvars.def".
           copy "winprint.def".

       01  extend-stat.
           03 primary-error    pic x(2).
           03 secondary-error  pic x(10).
       01  text-message        pic x(40).

       01  stato-stampa pic 9.
           88 stampa-annullata value zero.
           88 stampa-ok        value 1.

       01  status-stampa pic x(2).

       01  result signed-short.

       01  como-altezza   pic 9(3)v99.
       01  como-larghezza pic 9(3)v99.
       01  dim-crt        pic 9(3)v99.
       01  nr-crt         pic 9(3).
       01  cont           pic 9(5).
       01  ind            pic 9(5).
                                  
       78  78-col-s       value 4,0.
       78  78-col-reps    value 4,5. 
       78  78-col-r       value 6,4.

       78  78-col-rep1    value 7,08.
       78  78-col-kg1     value 7,90.
       78  78-col-buf1    value 9,50.  

       78  78-col-rep2    value 10,3.
       78  78-col-kg2     value 11,12.
       78  78-col-buf2    value 12,70.     

       78  78-col-rep3    value 13,53. |0.8
       78  78-col-kg3     value 14,34. |0.82
       78  78-col-buf3    value 15,90. |1.6

       78  78-col-rep4    value 16,74. |0.8
       78  78-col-kg4     value 17,56. |0.82
       78  78-col-buf4    value 19,15. |1.6

       78  78-col-rep5    value 19,96. |0.8
       78  78-col-kg5     value 20,78. |0.82
       78  78-col-buf5    value 22,35. |1.6

       78  78-col-note    value 23,2.
           
       01  como-area      pic x(1000).

       01  passaggio      pic 9.
           88 primo-passaggio value 0.
           88 altro-passaggio value 1.

       77  calling-program          pic x(20).
       77  como-data                pic 9(8).
       77  mese                     pic 99.

       78  std-margine-inf          value 2.
       78  std-margine-destro       value 1.
       78  std-margine-sinistro     value 0,6.

       01  controlli         pic xx.
           88 errori         value "ER".
           88 tutto-ok       value "OK".

       LINKAGE SECTION.
           COPY "SPOOLER.DEF".
         
      ************************************************************************
       procedure division using SPOOLER-LINK.
      ************************************************************************
       DECLARATIVES.
       STAMPA-ERR SECTION.
           use after error procedure on STAMPA.
           set tutto-ok  to true.
           evaluate status-stampa
           when "30" |Permanent Error!!!
                call "C$RERR" using extend-stat, text-message
                display message "Trasmission Error on Windows Spooler."
                      x"0d0a""Riavviare il sistema GESLUX e riprovare."
                      x"0d0a""Codice d'errore: " extend-stat
                          title "spooler"
                           icon 2
                set spl-sta-annu  to true
                set errori to true
           end-evaluate.       
       END DECLARATIVES.
       
       MAIN-LOGIC.
           set tutto-ok to true.
           evaluate true
           when spl-apertura
           when spl-apertura-anteprima   perform APERTURA
           when spl-stringa              perform STAMPA-STRINGA
           when spl-oggetto              perform STAMPA-OGGETTO
           when spl-bitmap               perform STAMPA-BITMAP
           when spl-salto-pagina         perform SALTO-PAGINA
           when spl-chiusura             close   stampa
           end-evaluate.

           goback.

      ***---
       APERTURA.
           call "C$CALLEDBY"  using calling-program.
      *    SELEZIONO LA STAMPANTE
           if spl-apertura
              if spl-nome-stampante = space
                 call "WIN$PRINTER" using winprint-setup-old,
                                          winprint-selection
                                   giving result

                 if result = 1
                    call "WIN$PRINTER" using winprint-get-current-info, 
                                             winprint-selection
                                      giving result

                    perform ORIENTAMENTO-FOGLIO
                 end-if

              else

                 initialize winprint-selection            
                 move spl-nome-stampante to winprint-name
                 if SPL-NUM-COPIE = zero
                    move 1   to SPL-NUM-COPIE
                 end-if
                 move SPL-NUM-COPIE   to WINPRINT-COPIES
                 move SPL-NUM-COPIE   to winprint-curr-copies
                 call "WIN$PRINTER" using winprint-set-printer,
                                          winprint-selection
                                   giving result

                 if result = 1
                    call "WIN$PRINTER" using winprint-get-current-info, 
                                             winprint-selection
                                      giving result

                    perform ORIENTAMENTO-FOGLIO
                 end-if

              end-if

              if result not = 1
                 if spl-titolo-msgbox = spaces
                    move "spooler" to spl-titolo-msgbox
                 end-if
                 if spl-nome-stampante not = space
                    display message "Stampante non disponibile!"
                              title spl-titolo-msgbox 
                               icon mb-warning-icon
                 end-if
                 set spl-sta-annu to true
                 exit paragraph
              end-if

           else 
              accept spl-nome-stampante 
                          from environment "STAMPANTE_ANTEPRIMA"

              initialize winprint-selection            
              move spl-nome-stampante to winprint-name
              call "WIN$PRINTER" using winprint-set-printer,
                                       winprint-selection
                                giving result

              if result = 1
                 call "WIN$PRINTER" using winprint-get-current-info,
                                          winprint-selection
                                   giving result
                 perform ORIENTAMENTO-FOGLIO
              end-if
                                         
              if result not = 1
                 if spl-titolo-msgbox = spaces
                    move "spooler" to spl-titolo-msgbox
                 end-if
                 display message "Anteprima non disponibile"
                           title spl-titolo-msgbox 
                            icon mb-warning-icon
                 set spl-sta-annu to true
                 exit paragraph
              end-if
           end-if.

      *    recupero le dimensioni del foglio
           perform CALCOLA-DIMENSIONI.
      *    setto il nome del job di stampa
           move spl-nome-job to winprint-job-title.
           call "WIN$PRINTER" using winprint-set-printer-ex,  
                                    winprint-selection
                             giving result.

           open output stampa.
           if tutto-ok
      *       setto la posizione in modo da avere sempre il posizionamento in centimetri
              initialize wprtdata-draw
              move 1                        to wprtdata-draw-start-y
              move 1                        to wprtdata-draw-start-x
              move zero                     to wprtdata-draw-stop-x
              move zero                     to wprtdata-draw-stop-y
              move wprtunits-centimeters    to wprtdata-draw-units
              move zero                     to wprtdata-draw-shape
              call "WIN$PRINTER" using winprint-set-cursor, 
                                       winprint-data
                                giving result
           end-if.

      ***---
       ORIENTAMENTO-FOGLIO.
           if spl-horizontal
              move wprtsel-orient-landscape  
                to winprint-curr-orientation  
           else              
              move wprtsel-orient-portrait
                to winprint-curr-orientation  
           end-if.
           call "WIN$PRINTER" using winprint-set-printer,
                                    winprint-selection
                             giving result.

      ***---
       STAMPA-STRINGA.
           evaluate calling-program 
           when "st-wod"
                perform SETTA-COLONNE-WOD
           end-evaluate.

      *    SETTO IL FONT
           initialize winprint-data.
           move spl-hfont        to wprtdata-font.
           call "WIN$PRINTER" using winprint-set-font, 
                                    winprint-data
                             giving result.
      *
           perform CALCOLA-CRT-RIGA.

      *    SETTO LA POSIZIONE 
           initialize wprtdata-draw.
           move spl-colonna              to wprtdata-draw-start-x.
           move spl-riga                 to wprtdata-draw-start-y.
           move zero                     to wprtdata-draw-stop-x.
           move zero                     to wprtdata-draw-stop-y.
           move wprtunits-centimeters to wprtdata-draw-units.
           move zero                     to wprtdata-draw-shape.

           call "WIN$PRINTER" using winprint-set-cursor, 
                                    winprint-data
                             giving result.

      *    setto il colore
      *    Formula per usare un colore particolare:
      *    compute colorref            =
      *            (rgb-red)           +
      *            (rgb-green * 256)   +
      *            (rgb-blue  * 65536).

           move spl-pen-color   to WPRTDATA-TEXT-COLOR

           CALL "WIN$PRINTER" USING  WINPRINT-SET-TEXT-COLOR, 
                                     WINPRINT-DATA
                              GIVING RESULT

      *     INSPECT SPL-RIGA-STAMPA REPLACING TRAILING SPACE BY LOW-VALUE.
      *
      *     MOVE ZERO TO CONT.
      *     INSPECT SPL-RIGA-STAMPA TALLYING CONT FOR CHARACTERS
      *          BEFORE LOW-VALUE.
      *
      *     INSPECT SPL-RIGA-STAMPA REPLACING TRAILING LOW-VALUE BY SPACE.
      *
      **    SCRIVO LA STRINGA
      *     SET PRIMO-PASSAGGIO TO TRUE.
      *     IF CONT < NR-CRT
              write stampa-rigo from spl-riga-stampa after 0
      *     ELSE 
      *        MOVE 1 TO IND
      *        PERFORM UNTIL IND > CONT
      *           IF PRIMO-PASSAGGIO
      *              SET ALTRO-PASSAGGIO TO TRUE               
      *           ELSE 
      *              ADD SPL-PASSO       TO SPL-RIGA     
      *              initialize wprtdata-draw
      *              MOVE SPL-COLONNA    TO WPRTDATA-DRAW-START-X
      *              MOVE SPL-RIGA       TO WPRTDATA-DRAW-START-Y
      *              MOVE ZERO           TO WPRTDATA-DRAW-STOP-X
      *              MOVE ZERO           TO WPRTDATA-DRAW-STOP-Y
      *              MOVE WPRTUNITS-CENTIMETERS
      *                                  TO WPRTDATA-DRAW-UNITS
      *              MOVE ZERO           TO WPRTDATA-DRAW-SHAPE
      *
      *              CALL "WIN$PRINTER" USING  WINPRINT-SET-CURSOR, 
      *                                        WINPRINT-DATA
      *                                 GIVING RESULT
      *           END-IF
      *           INITIALIZE COMO-AREA
      *           MOVE SPL-RIGA-STAMPA(IND:NR-CRT)   TO COMO-AREA
      *           WRITE STAMPA-RIGO 
      *                       FROM COMO-AREA(1:NR-CRT) AFTER 0
      *           ADD NR-CRT     TO IND
      *        END-PERFORM
      *     END-IF.
           call "WIN$PRINTER" using winprint-clear-page-columns.
           call "WIN$PRINTER" using winprint-clear-data-columns.

      ***---
       STAMPA-OGGETTO.
           move spl-pen        to wprtdata-pen-style.
           move spl-pen-width  to wprtdata-pen-width.
           move spl-pen-color  to wprtdata-pen-color.

           call "WIN$PRINTER" using winprint-graph-pen, 
                                    winprint-data
                             giving result.

      *    SETTO LO STILE DELL'OGGETTO
           move spl-stile to wprtdata-brush-style.

           move spl-pen-color  to WPRTDATA-BRUSH-COLOR


           call "WIN$PRINTER" using winprint-graph-brush, 
                                    winprint-data
                             giving result.



      *    STAMPO L'OGGETTO.
           initialize wprtdata-draw.
           move spl-colonna           to wprtdata-draw-start-x.
           move spl-riga              to wprtdata-draw-start-y.
           move spl-colonna-fine      to wprtdata-draw-stop-x.
           move spl-riga-fine         to wprtdata-draw-stop-y.
           move wprtunits-centimeters to wprtdata-draw-units.
           move spl-tipo-oggetto      to wprtdata-draw-shape.

           call "WIN$PRINTER" using winprint-graph-draw, 
                                    winprint-data
                             giving result.

      ***---
       STAMPA-BITMAP.
           initialize     wprtdata-print-bitmap.
      *    setto la bitmap
           move spl-hbitmap                   to wprtdata-bitmap.
           move spl-riga                      to wprtdata-bitmap-row.
           move spl-colonna                   to wprtdata-bitmap-col.
           move spl-bitmap-height             to wprtdata-bitmap-height.
           move spl-bitmap-width              to wprtdata-bitmap-width.
           move wprtbitmap-scale-centimeters  to wprtdata-bitmap-flags.
      *    stampo la bitmap
           call "WIN$PRINTER" using winprint-print-bitmap, 
                                    winprint-data
                             giving result.

      ***---
       SALTO-PAGINA.
           write stampa-rigo from spaces after page.

      ***---
       CALCOLA-DIMENSIONI.
           if spl-margine-inf = zero
              move std-margine-inf     to spl-margine-inf
           end-if.
           if spl-margine-destro = zero
              move std-margine-destro  to spl-margine-destro
           end-if.
           if spl-margine-sinistro = zero
              move std-margine-sinistro     to spl-margine-sinistro
           end-if.

           call "WIN$PRINTER" using winprint-get-current-info-ex, 
                                    winprint-selection
                             giving result.

           evaluate winprint-curr-papersize
           when 8 |formato a3
                if winprint-curr-orientation = 1|verticale
                   move 42   to como-altezza
                   move 29,7 to como-larghezza
                else 
                   move 29,7 to como-altezza
                   move 42   to como-larghezza
                end-if
           when 9 |formato a4
           when other
                if winprint-curr-orientation = 1|verticale
                   move 29,7 to como-altezza
                   move 21   to como-larghezza
                else
                   move 21   to como-altezza
                   move 29,7 to como-larghezza
                end-if
           end-evaluate.

           subtract spl-margine-inf from como-altezza.
           compute como-larghezza = como-larghezza - spl-margine-destro 
                                    - spl-margine-sinistro

           move como-larghezza to spl-larghezza.
           move como-altezza   to spl-altezza.

      ***---
       CALCOLA-CRT-RIGA.
           call "WIN$PRINTER" using winprint-get-page-layout, 
                                    winprint-data,
                             giving result.
           |WPRTDATA-COLUMNS-PER-PAGE

           compute dim-crt = como-larghezza / wprtdata-columns-per-page.

           compute nr-crt = spl-colonna / dim-crt.

           compute nr-crt = wprtdata-columns-per-page - nr-crt.

      ***--- 
       PULISCI-COLONNE.
           call "WIN$PRINTER" using winprint-clear-data-columns
                             giving result.

           call "WIN$PRINTER" using winprint-clear-page-columns,
                             giving result.
            
      ***---
       SETTA-COLONNE-WOD.
           perform PULISCI-COLONNE.
           initialize winprint-column.
           move spl-hfont             to winprint-col-font.
           move wprtunits-centimeters to winprint-col-units.
           move wprtalign-none        to winprint-col-alignment.
           set  winprint-transparent  to true.
           evaluate spl-tipo-colonna                 
           when 1                         
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          9, 10, 14, 18, 21, 23, 26, 29, 
                                         31, 34, 37, 39, 42, 45, 47, 50, 
                                         53, 55, 58, 62
                                    giving return-code
                |EXERCISE
                move 0,6               to winprint-col-start
                move wprtalign-left  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code  
                |S
                move 78-col-s    to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |REPS
                move 78-col-reps       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code     
                |REST
                move 78-col-r          to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |REP                                         
                move 78-col-rep1       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |KG
                move 78-col-kg1        to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |BUF
                move 78-col-buf1       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |REP                                         
                move 78-col-rep2       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |KG                                          
                move 78-col-kg2        to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |BUF
                move 78-col-buf2       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |REP                                         
                move 78-col-rep3       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |KG                                          
                move 78-col-kg3        to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |BUF
                move 78-col-buf3       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code                                    
                |REP                                         
                move 78-col-rep4       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |KG                                          
                move 78-col-kg4        to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |BUF
                move 78-col-buf4       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |REP                                         
                move 78-col-rep5       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |KG                                          
                move 78-col-kg5        to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |BUF
                move 78-col-buf5       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |NOTE
                move 78-col-note       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           when 2                                   
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           21, 23, 43, 46,
      *                                     39, 42, 45,    
      *                                     48, 51, 54,
      *                                     57, 60, 63,
      *                                     66, 69, 72,
      *                                     75, 78, 81
                                           56,  66,  76,
                                           86,  96, 106,
                                          116, 126, 136,
                                          146, 156, 166,
                                          176, 186, 196,
                                    giving return-code

                move 0,6               to winprint-col-start
                move wprtalign-left  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

                move 78-col-s          to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code    

                move 78-col-reps       to winprint-col-start  
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code         

                move 78-col-r          to winprint-col-start  
                add 0,05               to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code   
                             
                |REP                                         
                move 78-col-rep1       to winprint-col-start  
                add 0,05               to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |KG                                                                        
                move 78-col-kg1        to winprint-col-start  
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |BUF                                         
                move 78-col-buf1       to winprint-col-start 
                add 0,1                to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |REP                                                                      
                move 78-col-rep2       to winprint-col-start 
                add 0,05               to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |KG
                move 78-col-kg2       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |BUF
                move 78-col-buf2       to winprint-col-start 
                add  0,1               to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code          
                |REP                                                                      
                move 78-col-rep3       to winprint-col-start 
                add 0,05               to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |KG
                move 78-col-kg3       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |BUF
                move 78-col-buf3       to winprint-col-start 
                add  0,1               to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code          
                |REP                                                                      
                move 78-col-rep4       to winprint-col-start 
                add 0,05               to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |KG
                move 78-col-kg4       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |BUF
                move 78-col-buf4       to winprint-col-start 
                add  0,1               to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code                
                |REP                                                                      
                move 78-col-rep5       to winprint-col-start 
                add 0,05               to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |KG
                move 78-col-kg5       to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |BUF
                move 78-col-buf5       to winprint-col-start 
                add  0,1               to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
           when 2,1
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           21
                                    giving return-code
                move 78-col-reps       to winprint-col-start
                subtract 0,05 from winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           when 2,5
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           111
                                    giving return-code
                move 78-col-note       to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           when 3
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           21, 31
                                    giving return-code
                move 0,6               to winprint-col-start
                move wprtalign-left  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 4,1               to winprint-col-start 
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           end-evaluate.
