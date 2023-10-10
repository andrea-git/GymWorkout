       SELECT wodbook
           ASSIGN       TO  "wodbook"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-wodbook
           RECORD KEY   IS wod-key
           ALTERNATE RECORD KEY IS wod-k-desc = wod-desc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS wod-k-prg = wod-code, wod-prg-day, 
           wod-split
           WITH DUPLICATES .
