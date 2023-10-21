       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-wodmap.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "wodmap.sl".     
       SELECT old-wodmap
           ASSIGN       TO  "old-wodmap"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-wodmap
           RECORD KEY   IS old-wom-key
           ALTERNATE RECORD KEY IS old-wom-k-desc = old-wom-desc
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "wodmap.fd".     
       FD  old-wodmap.
       01 old-wom-rec.
           05 old-wom-key.
               10 old-wom-code         PIC  9(3).
           05 old-wom-data.
               10 old-wom-desc         PIC  x(100).
               10 old-wom-days         PIC  9.
               10 old-wom-macrogroups  PIC  99.
               10 old-wom-effort       PIC  99.
               10 old-wom-split-tab.
                   15 old-wom-split-el-days
                              OCCURS 7 TIMES.
                       20 old-wom-split-el-days-split
                                  OCCURS 20 TIMES.
                           25 old-wom-split-el-split-sigla     PIC  x.
                           25 old-wom-split-el-split-int-code  PIC  9.
                           25 old-wom-split-el-split-ss        PIC  9.
                           25 old-wom-split-el-split-primary   PIC  9.
                           25 FILLER           PIC  x(99).
                           25 FILLER           PIC  9(18).
               10 old-wom-dur-code     PIC  99.
               10 old-wom-mcg-default-tab.
                   12 old-wom-el-mcg-default
                              OCCURS 15 TIMES.
                       15 old-wom-sigla-default            PIC  x.
                       15 old-wom-mcg-code-default         PIC  x(5).
               10 old-wom-filler       PIC  x(910).
               10 old-wom-filler-n1    PIC  9(18).
               10 old-wom-filler-n2    PIC  9(18).
               10 old-wom-filler-n3    PIC  9(18).
       
       WORKING-STORAGE SECTION.
       77  status-wodmap         pic xx.
       77  idx  pic 99.
       77  idx2 pic 99.

      ******************************************************************
       PROCEDURE DIVISION.
      ***---
       MAIN-PRG.
           open output wodmap.
           open input  old-wodmap.
           move low-value to old-wom-key.
           start old-wodmap key >= old-wom-key
           perform until 1 = 2
              read old-wodmap next at end exit perform end-read
              move old-wom-code            to wom-code       
              move old-wom-desc            to wom-desc       
              move old-wom-days            to wom-days       
              move old-wom-macrogroups     to wom-macrogroups
              move old-wom-effort          to wom-effort     
              perform varying idx from 1 by 1
                        until idx > 7
                 perform varying idx2 from 1 by 1 
                           until idx2 > 20
                    move old-wom-split-el-split-sigla(idx, idx2)
                      to wom-split-el-split-sigla(idx, idx2)
                    move old-wom-split-el-split-int-code(idx, idx2)
                      to wom-split-el-split-int-code(idx, idx2)
                    move old-wom-split-el-split-ss(idx, idx2)      
                      to wom-split-el-split-ss(idx, idx2)
                    move old-wom-split-el-split-primary(idx, idx2) 
                      to wom-split-el-split-primary(idx, idx2)
                 end-perform
              end-perform
              move old-wom-dur-code        to wom-dur-code       
              move old-wom-mcg-default-tab to wom-mcg-default-tab
              move old-wom-filler          to wom-filler         
              move old-wom-filler-n1       to wom-filler-n1      
              move old-wom-filler-n2       to wom-filler-n2      
              move old-wom-filler-n3       to wom-filler-n3      
       
              write wom-rec
           end-perform
           close       wodmap old-wodmap.
