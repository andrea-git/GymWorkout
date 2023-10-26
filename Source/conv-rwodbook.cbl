       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-rwodbook.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rwodbook.sl".     
       SELECT old-rwodbook
           ASSIGN       TO  "old-rwodbook"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-rwodbook
           RECORD KEY   IS old-rod-key
           ALTERNATE RECORD KEY IS old-rod-k-day = old-rod-day, 
           old-rod-split, old-rod-code
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-rod-k-prg = 
                                   old-rod-key, old-rod-prg-day
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-rod-k-mcg = 
                                   old-rod-mcg-code, old-rod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-rod-k-exe = 
                                   old-rod-exe-code, old-rod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-rod-k-multi = old-rod-exe-isMulti, 
           old-rod-mcg-code, old-rod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-rod-k-rp = old-rod-int-restpause, 
           old-rod-mcg-code, old-rod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-rod-k-ss = old-rod-ss, 
           old-rod-mcg-code, old-rod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-rod-k-confronto = 
                                   old-rod-exe-code, old-rod-int-code, 
                                   old-rod-day
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "rwodbook.fd".            
           
       FD old-rwodbook.
       01 old-rod-rec.
           05 old-rod-key.
               10 old-rod-code         PIC  9(18).
               10 old-rod-day          PIC  9(8).
               10 old-rod-split        PIC  99.
           05 old-rod-data.
               10 old-rod-prg-day      PIC  9.
               10 old-rod-mcg-code     PIC  x(5).
               10 old-rod-exe-code     PIC  x(5).
               10 old-rod-int-code     PIC  99.
               10 old-rod-exe-isMulti  PIC  9.
               10 old-rod-reps         PIC  x(10).
               10 old-rod-series       PIC  99.
               10 old-rod-int-restpause            PIC  9.
               10 old-rod-ss           PIC  9.
               10 old-rod-desc-univoca PIC  x(100).
               10 old-rod-dati-modwod.
                   15 old-rod-rep-kg-buf
                              OCCURS 10 TIMES.
                       20 old-rod-rep          PIC  x(3).
                       20 old-rod-kg           PIC  x(3).
                       20 old-rod-buf          PIC  x(3).
                   15 old-rod-note         PIC  x(100).
               10 FILLER           PIC  x(2000).
       
       WORKING-STORAGE SECTION.
       77  status-rwodbook         pic xx.
       77  idx  pic 99.

      ******************************************************************
       PROCEDURE DIVISION.
      ***---
       MAIN-PRG.
           open output rwodbook.
           open input  old-rwodbook.
           move low-value to old-rod-key.
           start old-rwodbook key >= old-rod-key
           perform until 1 = 2
              read old-rwodbook next at end exit perform end-read
              move old-rod-key           to rod-key
              move old-rod-prg-day       to rod-prg-day      
              move old-rod-mcg-code      to rod-mcg-code     
              move old-rod-exe-code      to rod-exe-code     
              move old-rod-int-code      to rod-int-code     
              move old-rod-exe-isMulti   to rod-exe-isMulti  
              move old-rod-reps          to rod-reps         
              move old-rod-series        to rod-series       
              move old-rod-int-restpause to rod-int-restpause
              move old-rod-ss            to rod-ss           
              move old-rod-desc-univoca  to rod-desc-univoca 
              perform varying idx from 1 by 1 
                        until idx > 10
                 move old-rod-rep(idx)   to rod-rep(idx)
                 move old-rod-kg(idx)    to rod-kg(idx) 
                 move old-rod-buf(idx)   to rod-buf(idx)
              end-perform
              move old-rod-note          to rod-note
              write rod-rec
           end-perform
           close       rwodbook old-rwodbook.
