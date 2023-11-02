       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-wod.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.     
       copy "tmp-exe.sl".
       copy "exercises.sl".                
       copy "intexe.sl".                

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.                       
       copy "tmp-exe.fd".
       copy "exercises.fd".                   
       copy "intexe.fd".

       WORKING-STORAGE SECTION.
      * COPY   
       copy "acugui.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
                           
       77  path-tmp-exe          pic x(256).      
       77  status-tmp-exe        pic xx.
       77  status-exercises      pic xx.
       77  status-intexe         pic xx.     

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
           05 pic x(1) value "S".
           05 pic x(4) value "REPS".
           05 pic x(4) value "R ''".
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
           05 r-series           pic xx.        
           05 r-reps             pic x(20).
           05 r-rest             pic xxx.  

           05 r-dati-modwod.
              10 r-rep-kg-buf    occurs 5.
                 20 r-rod-rep    PIC  x(10).
                 20 r-rod-kg     PIC  x(10).
                 20 r-rod-buf    PIC  x(10).
              10 r-rod-note      pic x(100).

       77  r-reps-only           pic x(20).

       01  riga-note.
           05 r-note             pic x(110).

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
       77  como-ss               pic 99.  
       77  como-prg              pic 99 value 0.
       77  sw-gray               pic s9.                  
       77  como-day              pic 99.
       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.
                                 
       77  ArialNarrow11         handle of font.                    
       77  ArialNarrow8          handle of font.                    
       77  ArialNarrow9          handle of font.           
       77  ArialNarrow7          handle of font.           
       77  ArialNarrow11B        handle of font.

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  len                   pic 99.

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
           open input tmp-exe exercises intexe.

      ***---
       ELABORAZIONE.                  
           move low-value to tex-rec
           start tmp-exe key >= tex-key
                 invalid continue
             not invalid                           
                 perform until 1 = 2
                    read tmp-exe next at end exit perform end-read
                    add 1 to num-righe 
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
                          add 1 to num-righe
                       end-if
                       move 78-pen-light to spl-pen-width
                       if como-day = 0
                          move tex-day to como-day
                       end-if
                       if tex-day not = como-day
                          move 0 to como-prg
                          compute sw-gray = sw-gray * -1
                          move 78-pen-heavy to spl-pen-width
                          perform STAMPA-LINEA-ORIZZONTALE
                          move tex-day to como-day
                       end-if

                       move tex-exe-code to exe-code
                       read exercises 
                            invalid move "NON TROVATA" to exe-desc
                       end-read
                       if tex-reps = spaces
                          move 0 to int-rest
                       else
                          move exe-int-code to int-code
                          read intexe 
                               invalid move 0 to int-rest 
                          end-read
                       end-if

                       initialize r-exe-desc
                       if tex-date = 0
                          if tex-ss = 0        
                             add 1 to como-prg
                             move como-prg to prg-xx
                             inspect prg-xx 
                                     replacing leading x"30" by x"20"
                             call "C$JUSTIFY" using prg-xx, "L"
                             string prg-xx delimited space                              
                                    "-"    delimited size
                                    exe-desc-stampa 
                               into r-exe-desc
                             end-string
                          else
                             if como-ss not = tex-ss
                                add 1 to como-prg
                                move como-prg to prg-xx
                                inspect prg-xx 
                                        replacing leading x"30" by x"20"
                                call "C$JUSTIFY" using prg-xx, "L"
                                string prg-xx delimited space                              
                                       "-"    delimited size
                                       exe-desc-stampa 
                                  into r-exe-desc
                                end-string
                                move tex-ss to como-ss
                             else                   
                                string "+ "  delimited size
                                       exe-desc-stampa 
                                  into r-exe-desc
                                end-string
                             end-if
                          end-if  
                          move 0 to len                       
                          inspect tex-reps replacing 
                                  trailing spaces by low-value
                          inspect tex-reps tallying len 
                                  for characters before low-value
                          inspect tex-reps replacing 
                                  trailing low-value by spaces
                          if len > 11
                             move tex-reps to r-reps-only
                             move spaces   to r-reps
                          else
                             move tex-reps to r-reps     
                             move spaces   to r-reps-only
                          end-if

                          move tex-series to r-series
                          inspect r-series 
                                  replacing leading x"30" by x"20"
                          call "C$JUSTIFY" using r-series, "L"
                          
                          move int-rest to r-rest
                          inspect r-rest 
                                  replacing leading x"30" by x"20"
                          call "C$JUSTIFY" using r-rest, "L"
                       else                        
                          initialize r-reps 
                          string tex-date(7:2) delimited size
                                 "/"           delimited size
                                 tex-date(5:2) delimited size
                                 "/"           delimited size
                                 tex-date(3:2) delimited size
                            into r-reps
                          end-string
                          move spaces to r-series r-rest r-exe-desc
                       end-if          

                       perform STAMPA-FRAME-RIGA
                                                                    
                       move tex-rod-rep-kg-buf(1) to r-rep-kg-buf(1)
                       move tex-rod-rep-kg-buf(2) to r-rep-kg-buf(2)
                       move tex-rod-rep-kg-buf(3) to r-rep-kg-buf(3)
                       move tex-rod-rep-kg-buf(4) to r-rep-kg-buf(4)
                       move tex-rod-rep-kg-buf(5) to r-rep-kg-buf(5)

                       if tex-int-cedimento > 0
                          move "KG:"  to r-rod-buf(4)
                       end-if
                       move ArialNarrow11 to spl-hfont     
                       move r-riga        to spl-riga-stampa
                       move 2             to spl-tipo-colonna
                       perform SCRIVI  
                       if r-reps-only not = spaces
                          subtract 78-passo from spl-riga
                          move 2,1 to spl-tipo-colonna
                          move r-reps-only to spl-riga-stampa
                          move ArialNarrow9 to spl-hfont     
                          perform SCRIVI
                       end-if

                       move ArialNarrow7 to spl-hfont     
                       subtract 78-passo from spl-riga
                       if tex-rod-note not = spaces
                          move tex-rod-note to r-note
                       else
                          move exe-note     to r-note         
                       end-if
                       move riga-note       to spl-riga-stampa
                       move 2,5             to spl-tipo-colonna
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

           move 0,1   to spl-colonna
           move 28,8  to spl-colonna-fine

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
           move 0,15  to spl-colonna.
           move 28,75 to spl-colonna-fine.

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
           move 3,6 to spl-colonna spl-colonna-fine.
           perform STAMPA-LINEA-VERTICALE.
                       
           move 78-pen-heavy to spl-pen-width.
           move 4,1 to spl-colonna spl-colonna-fine.
           perform STAMPA-LINEA-VERTICALE.
                       
           move 78-pen-heavy to spl-pen-width.
           move 6,0 to spl-colonna spl-colonna-fine.
           perform STAMPA-LINEA-VERTICALE.
                       
           move 78-pen-heavy to spl-pen-width.
           move 6,7 to spl-colonna spl-colonna-fine.
           perform STAMPA-LINEA-VERTICALE.
                       
           move 0 to resto.                            
           perform 15 times
              add 1 to resto                    
              move 78-pen-light to spl-pen-width
              if resto = 3 or 6 or 9 or 12 or 15
                 move 78-pen-heavy to spl-pen-width
              end-if
              evaluate resto
              when  1
              when  4
              when  7
              when 10
              when 13                                      
                   add 0,82 to spl-colonna spl-colonna-fine

              when  2
              when  5
              when  8
              when 11
              when 14
                   add 1,6 to spl-colonna spl-colonna-fine

              when  3     
              when  6
              when  9
              when 12
              when 15
                   add 0,8 to spl-colonna spl-colonna-fine

              end-evaluate
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
           move 0,1   to spl-colonna.
           move 28,8  to spl-colonna-fine.
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
                      
           move 27,4 to spl-colonna
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
                    
      * Arial Narrow 7
           initialize wfont-data ArialNarrow7.
           move 7 to wfont-size.
           move "Arial Narrow"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, ArialNarrow7, 
                               wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 
                    
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
  
      * Arial Narrow 9
           initialize wfont-data ArialNarrow9.
           move 9 to wfont-size.
           move "Arial Narrow"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, ArialNarrow9, 
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
           close tmp-exe exercises intexe.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".
                                 
           destroy ArialNarrow11.
           destroy ArialNarrow8.
           destroy ArialNarrow9.
           destroy ArialNarrow7.
           destroy ArialNarrow11B.

           cancel "spooler".
           initialize spooler-link.
           goback.
