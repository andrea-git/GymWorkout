       SELECT rwodbook
           ASSIGN       TO  "rwodbook"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-rwodbook
           RECORD KEY   IS rod-key
           ALTERNATE RECORD KEY IS rod-k-day = rod-day, rod-split, 
           rod-code
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rod-k-prg = rod-key, rod-prg-day
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rod-k-mcg = rod-mcg-code, rod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rod-k-exe = rod-exe-code, rod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rod-k-multi = rod-exe-isMulti, 
           rod-mcg-code, rod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rod-k-rp = rod-int-restpause, 
           rod-mcg-code, rod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rod-k-ss = rod-ss, rod-mcg-code, 
           rod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rod-k-confronto = rod-exe-code, 
           rod-int-code, rod-day
           WITH DUPLICATES .
