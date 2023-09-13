       SELECT wodmap
           ASSIGN       TO  "wodmap"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-wodmap
           RECORD KEY   IS wom-key
           ALTERNATE RECORD KEY IS wom-k-desc = wom-desc
           WITH DUPLICATES .
