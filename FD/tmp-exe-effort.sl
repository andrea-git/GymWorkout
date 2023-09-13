       SELECT tmp-exe-effort
           ASSIGN       TO  path-tmp-exe-effort
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-exe-effort
           RECORD KEY   IS tee-key
           ALTERNATE RECORD KEY IS tee-k-mcg-eff = tee-mcg-code, 
           tee-int-effort
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tee-k-exe-multi = tee-exe-isMulti, 
           tee-mcg-code
           WITH DUPLICATES .
