       SELECT zoom-wodbook
           ASSIGN       TO  path-zoom-wodbook
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-zoom-wodbook
           RECORD KEY   IS zwod-key
           ALTERNATE RECORD KEY IS key01 = zwod-desc, zwod-code
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS key02 = zwod-creation, zwod-desc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS key03 = zwod-ini, zwod-desc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS ksort = zwod-sort, zwod-desc
           WITH DUPLICATES .
