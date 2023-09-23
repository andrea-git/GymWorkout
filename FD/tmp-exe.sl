      *Contiene gli esercizi da fare
       SELECT tmp-exe
           ASSIGN       TO  path-tmp-exe
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-exe
           RECORD KEY   IS tex-key
           ALTERNATE RECORD KEY IS tex-k-dupl = tex-day, tex-nome-dupl
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tex-k-mcg = tex-mcg-code, tex-key
           WITH DUPLICATES .
