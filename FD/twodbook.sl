       SELECT twodbook
           ASSIGN       TO  "twodbook"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-twodbook
           RECORD KEY   IS tod-key
           ALTERNATE RECORD KEY IS tod-k-creazione = 
           tod-data-creazione, tod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tod-k-wom = tod-wom-code, tod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tod-k-desc = tod-desc, tod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tod-k-ini = tod-day-ini, tod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tod-k-eff = tod-wom-effort, tod-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tod-k-dur = tod-dur-code, tod-key
           WITH DUPLICATES .
