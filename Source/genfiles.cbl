       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      genfiles.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "exercises.sl".
           copy "groups.sl".
           copy "macrogroups.sl". 
           copy "twodbook.sl".    
           copy "rwodbook.sl".     
           copy "intexe.sl".   
           copy "duration.sl".    
           copy "wodmap.sl".     
           copy "tmp-grp-exe.sl".
           copy "tmp-wod-exe.sl".
           copy "tmp-exe.sl".     
           copy "zoom-wodbook.sl".
      *
       SELECT FBLOCK
           ASSIGN       TO DISK "FBLOCK"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS AUTOMATIC WITH LOCK ON RECORD 
           FILE STATUS  IS STATO-IO
           RECORD KEY   IS FB-PRI-KEY.
      *

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "exercises.fd".
           copy "groups.fd".
           copy "macrogroups.fd".
           copy "twodbook.fd".   
           copy "rwodbook.fd".      
           copy "intexe.fd".    
           copy "duration.fd".    
           copy "wodmap.fd".     
           copy "tmp-grp-exe.fd". 
           copy "tmp-wod-exe.fd".
           copy "tmp-exe.fd".
           copy "zoom-wodbook.fd".
      *                               
       FD  FBLOCK
           LABEL RECORD IS STANDARD.
       01  REC-FBLOCK.
           05 FB-PRI-KEY.
              10 FB-PROG-ID    PIC  X(15).
              10 FB-DATA       PIC  9(8).
              10 FB-ORA        PIC  9(8).
           05 FB-HND-WIN       PIC S9(9).
       
       WORKING-STORAGE SECTION.
           COPY "acucobol.def".

       77  status-exercises      pic xx.
       77  status-groups         pic xx.
       77  status-macrogroups    pic xx.
       77  status-twodbook       pic xx.
       77  status-rwodbook       pic xx. 
       77  status-intexe         pic xx.
       77  status-duration       pic xx.
       77  status-wodmap         pic xx.
       77  status-tmp-grp-exe    pic xx.
       77  path-tmp-grp-exe      pic x. 
       77  status-tmp-exe        pic xx.
       77  path-tmp-exe          pic x.
       77  status-tmp-wod-exe    pic xx.
       77  path-tmp-wod-exe      pic x.
       77  status-zoom-wodbook   pic xx.
       77  path-zoom-wodbook     pic x.
       77  stato-io              pic xx.  

       78  titolo            value "Generazione files".

       LINKAGE SECTION.
       77  link-status       signed-short.

      ******************************************************************
       PROCEDURE DIVISION USING link-status.
       DECLARATIVES.
      ***---
       EXERCISES-ERR SECTION.
           use after error procedure on exercises.
           evaluate status-exercises
           when "35" continue
           when "39"
                display message "File [exercises] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[exercises] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.       

      ***---
       GROUPS-ERR SECTION.
           use after error procedure on GROUPS.
           evaluate status-GROUPS
           when "35" continue
           when "39"
                display message "File [GROUPS] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[GROUPS] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.      

      ***---
       MACROGROUPS-ERR SECTION.
           use after error procedure on MACROGROUPS.
           evaluate status-MACROGROUPS
           when "35" continue
           when "39"
                display message "File [MACROGROUPS] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[MACROGROUPS] Indexed file corrupt!"
                           title titolo
                            icon 3
                                       
           end-evaluate.               

      ***---
       TWODBOOK-ERR SECTION.
           use after error procedure on twodbook.
           evaluate status-twodbook
           when "35" continue
           when "39"
                display message "File [TWODBOOK] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TWODBOOK] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.               

      ***---
       RWODBOOK-ERR SECTION.
           use after error procedure on rwodbook.
           evaluate status-rwodbook
           when "35" continue
           when "39"
                display message "File [RWODBOOK] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RWODBOOK] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.               

      ***---
       INTEXE-ERR SECTION.
           use after error procedure on intexe.
           evaluate status-intexe
           when "35" continue
           when "39"
                display message "File [INTEXE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[INTEXE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.    

      ***---
       DURATION-ERR SECTION.
           use after error procedure on DURATION.
           evaluate status-DURATION
           when "35" continue
           when "39"
                display message "File [DURATION] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[DURATION] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.    

      ***---
       WODMAP-ERR SECTION.
           use after error procedure on wodmap.
           evaluate status-wodmap
           when "35" continue
           when "39"
                display message "File [WODMAP] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[WODMAP] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.    

       END DECLARATIVES.

       MAIN-PRG.
           accept SYSTEM-INFORMATION from system-info.
           move 0 to link-status.
                           
           open input exercises.
           if status-exercises = "35"
              open output exercises
              if status-exercises not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close exercises.

           open input groups.
           if status-groups = "35"
              open output groups
              if status-groups not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close groups.

           open input macrogroups.
           if status-macrogroups = "35"
              open output macrogroups
              if status-macrogroups not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close macrogroups.           

           open input twodbook.
           if status-twodbook = "35"
              open output twodbook
              if status-twodbook not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close twodbook.           

           open input rwodbook.
           if status-rwodbook = "35"
              open output rwodbook
              if status-rwodbook not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close rwodbook.

           open input intexe.
           if status-intexe = "35"
              open output intexe
              if status-intexe not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close intexe.

           open input duration.
           if status-duration = "35"
              open output duration
              if status-duration not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close duration.

           open input wodmap.
           if status-wodmap = "35"
              open output wodmap
              if status-wodmap not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close wodmap.

           delete file fblock.
           open output fblock.
           close       fblock.

           goback.
