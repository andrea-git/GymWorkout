       SELECT zoom-exe-mcg
           ASSIGN       TO  path-zoom-exe-mcg
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-zoom-exe-mcg
           RECORD KEY   IS zem-key
           ALTERNATE RECORD KEY IS zem-k-int = zem-grp-desc, 
           zem-int-effort, zem-exe-desc
           WITH DUPLICATES .
