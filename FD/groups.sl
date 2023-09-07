       SELECT groups
           ASSIGN       TO  "groups"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-groups
           RECORD KEY   IS grp-key
           ALTERNATE RECORD KEY IS grp-k-desc = grp-desc, grp-key
           WITH DUPLICATES .
