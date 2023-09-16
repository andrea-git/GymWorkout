       SELECT int-exe
           ASSIGN       TO  "int-exe"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-intensity
           RECORD KEY   IS int-key
           ALTERNATE RECORD KEY IS int-k-desc = int-desc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-k-effort = int-effort
           WITH DUPLICATES .
