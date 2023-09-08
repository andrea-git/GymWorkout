       SELECT exercises
           ASSIGN       TO  "exercises"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-exercises
           RECORD KEY   IS exe-key
           ALTERNATE RECORD KEY IS exe-k-desc = exe-desc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS exe-k-group = exe-grp-code, exe-key
           WITH DUPLICATES .
