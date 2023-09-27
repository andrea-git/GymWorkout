       SELECT tmp-grp-exe
           ASSIGN       TO  path-tmp-grp-exe
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-grp-exe
           RECORD KEY   IS tge-key
           ALTERNATE RECORD KEY IS tge-k-prg = tge-prg OF tge-data OF 
           tge-rec
           WITH DUPLICATES .
