       SELECT intensity
           ASSIGN       TO  "intensity"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-intensity
           RECORD KEY   IS int-key
           ALTERNATE RECORD KEY IS int-k-desc = int-desc, int-code
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS int-kj-effort = int-effort, int-key
           WITH DUPLICATES .
