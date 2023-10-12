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
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS wod-k-data = wod-day, wod-desc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS wod-k-creazione = 
           wod-data-creazione, wod-day, wod-desc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS wod-k-wom = wod-wom-code, 
           wod-data-creazione
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS wod-k-mcg = wod-mcg-code, wod-day
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS wod-k-exe = wod-exe-code, wod-day
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS wod-k-mcg-multi = wod-mcg-code, 
           wod-exe-isMulti, wod-day
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS wod-k-mcg-rp = wod-mcg-code, 
           wod-int-restpause, wod-day
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS wod-k-mcg-ss = wod-mcg-code, wod-ss, 
           wod-day
           WITH DUPLICATES .
