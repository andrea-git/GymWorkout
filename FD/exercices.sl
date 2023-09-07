       SELECT exercices
           ASSIGN       TO  "exercices"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-exercices
           RECORD KEY   IS exe-key
           ALTERNATE RECORD KEY IS exe-k-desc = exe-desc, exe-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS exe-k-group = exe-group, exe-key
           WITH DUPLICATES .
