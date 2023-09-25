       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-wod.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.     
       copy "tmp-exe.sl".
       copy "exercises.sl".                

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.                       
       copy "tmp-exe.fd".
       copy "exercises.fd".                   

       WORKING-STORAGE SECTION.
      * COPY   
       copy "acugui.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
                           
       77  path-tmp-exe          pic x(256).      
       77  status-tmp-exe        pic xx.
       77  status-exercises      pic xx.     

      * COSTANTI
       78  titolo                value "Stampa WOD".
       78  78-max-righe          value 37.
       78  78-passo              value 0,5.
       78  78-margine-basso      value 19,2.
       78  78-pen-light          value 5.
       78  78-pen-heavy          value 15.

      * RIGHE PER LA STAMPA
       01  r-titolo              pic x(100).

       01  r-intesta.                 
           05 pic x(8) value "EXERCISE".
           05 pic x(4) value "REPS".
           05 pic x(3) value "REP".
           05 pic x(2) value "KG".
           05 pic x(3) value "BUF".
           05 pic x(3) value "REP".
           05 pic x(2) value "KG".
           05 pic x(3) value "BUF".
           05 pic x(3) value "REP".
           05 pic x(2) value "KG".
           05 pic x(3) value "BUF".
           05 pic x(3) value "REP".
           05 pic x(2) value "KG".
           05 pic x(3) value "BUF".
           05 pic x(3) value "REP".
           05 pic x(2) value "KG".    
           05 pic x(3) value "BUF".
           05 pic x(4) value "NOTE".

       01  r-riga.
           05 r-exe-desc         pic x(20).
           05 r-reps             pic x(10).

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.
       77  filler                pic 9.
           88 record-ok          value 1 false 0.
                    
      * VARIABILI   
       77  sw-gray               pic s9.                  
       01  tab-exe.
         05 el-num-exe           pic 99 occurs 10 value 0.
       77  como-day              pic 99.
       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.
                                 
       77  ArialNarrow11         handle of font.           
       77  ArialNarrow11B        handle of font.
       77  ArialNarrow8          handle of font.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       77  num-righe             pic 9(3).
       77  pagina                pic 9(3) value 0.
       77  tot-pagine            pic 9(3). 
       77  resto                 pic 9(3). 
       
       77  pagina-z              pic z(3).
       77  tot-pagine-z          pic z(3). 

       77  prg-xx                pic xxx.

       LINKAGE SECTION.
       77  link-path             pic x(256).
       77  link-stampante        pic x(200).

      ******************************************************************
       PROCEDURE DIVISION using link-path link-stampante.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set environment "PRINTER" to "-P SPOOLER".
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.

      ***---
       OPEN-FILES.                          
           move link-path to path-tmp-exe.
           open input tmp-exe exercises.

      ***---
       ELABORAZIONE.                  
           move low-value to tex-rec
           start tmp-exe key >= tex-key
                 invalid continue
             not invalid                           
                 perform until 1 = 2
                    read tmp-exe next at end exit perform end-read
                    add 1 to num-righe
                    add 1 to el-num-exe(tex-day)
                 end-perform
           end-start.           
           if num-righe > 0
              if num-righe <= 78-max-righe
                 move 1 to tot-pagine
              else
                 move 0 to resto
                 divide num-righe by 78-max-righe 
                           giving tot-pagine
                        remainder resto
                 if resto > 0
                    add 1 to tot-pagine
                 end-if
              end-if
              perform STAMPA
           end-if.

      ***---
       STAMPA.
           move 0 to como-day.
           if link-stampante = spaces
              initialize spooler-link
              call   "selprint" using selprint-linkage
              cancel "selprint"
           else
              move link-stampante to selprint-stampante
           end-if. 

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE

              move titolo to spl-nome-job
              set spl-apertura   to true
              set spl-horizontal to true
              set WFDEVICE-WIN-PRINTER    to true
              call "spooler" using spooler-link
              if spl-sta-annu
                 set errori to true
              else
                 move spl-altezza to save-altezza-pagina
                 perform CARICA-FONT
              end-if
           else
              set spl-sta-annu to true
              set errori to true
           end-if.

           if tutto-ok
              move 0 to como-day 
              perform INTESTAZIONE
              move low-value to tex-rec
              start tmp-exe key >= tex-key
                    invalid continue
                not invalid                           
                    perform until 1 = 2
                       read tmp-exe next at end exit perform end-read
                       add 1 to num-righe
                       if num-righe > 78-max-righe
                          perform SALTO-PAGINA
                          perform INTESTAZIONE
                       end-if         
                       move 78-pen-light to spl-pen-width
                       if como-day = 0
                          move tex-day to como-day
                       end-if
                       if tex-day not = como-day           
                          compute sw-gray = sw-gray * -1
                          move 78-pen-heavy to spl-pen-width
                          perform STAMPA-LINEA-ORIZZONTALE
                          move tex-day to como-day
                       end-if          

                       move tex-exe-code to exe-code
                       read exercises               

                       initialize r-exe-desc
                       move tex-split to prg-xx
                       inspect prg-xx replacing leading x"30" by x"20"
                       call "C$JUSTIFY" using prg-xx, "L"
                       string prg-xx delimited space
                              " - "  delimited size
                              exe-desc-stampa 
                         into r-exe-desc
                       end-string
                       move tex-reps        to r-reps 
                       
                       perform STAMPA-FRAME-RIGA
                                               
                       move r-riga          to spl-riga-stampa
                       move 2               to spl-tipo-colonna
                       perform SCRIVI  
                                            
                    end-perform  
                                             
                    move 78-pen-heavy to spl-pen-width
                    perform STAMPA-LINEA-ORIZZONTALE  
      
                    set spl-chiusura to true
                    call   "spooler" using spooler-link

              end-start
           end-if.

      ***---
       INTESTAZIONE.           
            move 1 to sw-gray.
           |FRAME
           move 78-pen-heavy to spl-pen-width.
           move 0,2      to spl-riga.   
           move 78-margine-basso  to spl-riga-fine.

           move 0,5   to spl-colonna
           move 28,7  to spl-colonna-fine

           set  spl-oggetto       to true
           set  spl-rettangolo    to true
           set  spl-brush-null    to true
           set  spl-nero          to true
           call "spooler"         using spooler-link.
                                 
           move 0,2 to spl-riga
           perform STAMPA-FRAME-RIGA.                
      
           perform STAMPA-PIE-DI-PAGINA.
                   
           move 0,25 to spl-riga.
           move ArialNarrow11B to spl-hfont.
           move r-intesta to spl-riga-stampa.
           move 1         to spl-tipo-colonna.
           perform SCRIVI.    
           move ArialNarrow11 to spl-hfont

           move 0 to num-righe.  
           move 0,7 to spl-riga.

           move -1 to sw-gray.

      ***---
       STAMPA-FRAME-RIGA.
           perform STAMPA-QUADRATO-GRAY.
           perform LINEE-VERTICALI.

      ***---     
       STAMPA-QUADRATO-GRAY.                                  
           compute spl-riga-fine = 78-passo + spl-riga.
           move 0,55  to spl-colonna.
           move 28,65 to spl-colonna-fine.

           move 78-pen-light to spl-pen-width.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           if sw-gray = 1
              set spl-brush-ltgray to true
           else
              set spl-brush-null   to true
           end-if.
           call "spooler"         using spooler-link.

      ***---
       LINEE-VERTICALI.
           add 78-passo to spl-riga giving spl-riga-fine.
           move 78-pen-heavy to spl-pen-width.
           move 4,1 to spl-colonna spl-colonna-fine.
           perform STAMPA-LINEA-VERTICALE.
                       
           move 78-pen-heavy to spl-pen-width.
           move 5,4 to spl-colonna spl-colonna-fine.
           perform STAMPA-LINEA-VERTICALE.
                       
           move 0 to resto.                            
           perform 15 times
              add 1 to resto                    
              move 78-pen-light to spl-pen-width
              if resto = 3 or 6 or 9 or 12 or 15
                 move 78-pen-heavy to spl-pen-width
              end-if
              add 0,95 to spl-colonna spl-colonna-fine
              perform STAMPA-LINEA-VERTICALE
           end-perform.                         
           move 78-pen-light to spl-pen-width.
                                  
                         
      ***---
       STAMPA-LINEA-VERTICALE.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-nero           to true.
           call "spooler"       using spooler-link.

      ***---
       STAMPA-LINEA-ORIZZONTALE.
           move 0,5   to spl-colonna.
           move 28,7  to spl-colonna-fine.
           move spl-riga to spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-nero           to true.
           call "spooler"       using spooler-link.

      ***---
       STAMPA-PIE-DI-PAGINA.
           add 0,1 to 78-margine-basso giving spl-riga.
           add 1 to pagina.
           move ArialNarrow8 to spl-hfont.
           accept como-data from century-date.

           initialize spl-riga-stampa.
           move 0 to spl-tipo-colonna.
                      
           move 27,3 to spl-colonna
           move pagina     to pagina-z.
           move tot-pagine to tot-pagine-z.
           initialize spl-riga-stampa.
           string "Pag. "      delimited size
                  pagina-z     delimited size
                  "  di "      delimited size
                  tot-pagine-z delimited size
                  into spl-riga-stampa
           end-string.
           perform SCRIVI.

      ***---
       SCRIVI.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.
           add  78-passo      to spl-riga.

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.

      ***---
       CARICA-FONT.
                    
      * Arial Narrow 8
           initialize wfont-data ArialNarrow8.
           move 8 to wfont-size.
           move "Arial Narrow"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, ArialNarrow8, 
                               wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Arial Narrow 11
           initialize wfont-data ArialNarrow11.
           move 11 to wfont-size.
           move "Arial Narrow"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, ArialNarrow11, 
                               wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

           initialize wfont-data ArialNarrow11B.
           move 11 to wfont-size.
           move "Arial Narrow"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, ArialNarrow11B, 
                               wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect wfont-name replacing trailing space by low-value.
           move wfont-size    to font-size-dply.

           string  "Font: "         delimited size
                   WFONT-NAME       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   FONT-SIZE-DPLY,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio.

           inspect messaggio replacing trailing space by low-value.

           display message messaggio.

      ***---
       CLOSE-FILES.                            
           close tmp-exe exercises.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy ArialNarrow11.
           destroy ArialNarrow8.

           cancel "spooler".
           initialize spooler-link.
           goback.
