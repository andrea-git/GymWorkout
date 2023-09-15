      *Contiene gli esercizi validi per gruppo/intensità (e multiarticolare)
       SELECT tmp-wod-exe
           ASSIGN       TO  path-tmp-wod-exe
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-wod-exe
           RECORD KEY   IS twe-key
           ALTERNATE RECORD KEY IS twe-k-effort = twe-exe-isMulti, 
           twe-mcg-code
           WITH DUPLICATES .
