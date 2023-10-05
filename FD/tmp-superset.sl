       SELECT tmp-superset
           ASSIGN       TO  path-tmp-superset
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-superset
           RECORD KEY   IS tss-key.
