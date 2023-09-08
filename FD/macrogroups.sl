       SELECT macrogroups
           ASSIGN       TO  "macrogroups"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-macrogroups
           RECORD KEY   IS mcg-key
           ALTERNATE RECORD KEY IS mcg-k-desc = mcg-desc, mcg-code
           WITH DUPLICATES .
