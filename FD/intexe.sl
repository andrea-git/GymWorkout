       SELECT intexe
           ASSIGN       TO  "intexe"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-intexe
           RECORD KEY   IS int-key
           ALTERNATE RECORD KEY IS int-k-desc = int-desc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k-effort = int-effort
           WITH DUPLICATES .
