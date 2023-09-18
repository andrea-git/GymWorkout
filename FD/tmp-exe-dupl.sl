       SELECT tmp-exe-dupl
           ASSIGN       TO  path-tmp-exe-dupl
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-exe-dupl
           RECORD KEY   IS ted-key
           ALTERNATE RECORD KEY IS ted-k-num = ted-num, ted-key
           WITH DUPLICATES .
