      *Per ordinamento
       SELECT tmp-exe-mcg
           ASSIGN       TO  path-tmp-exe-mcg
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-exe-mcg
           RECORD KEY   IS tem-key
           ALTERNATE RECORD KEY IS tem-k-multi = tem-exe-isMulti, 
           tem-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tem-k-disab = tem-exe-isDisable, 
           tem-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tem-k-rp = tem-exe-isRestPause, 
           tem-key
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tem-k-int = tem-int-code, 
           tem-exe-desc
           WITH DUPLICATES .
