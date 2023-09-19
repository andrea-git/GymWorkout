       SELECT duration
           ASSIGN       TO  "duration"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-duration
           RECORD KEY   IS dur-key
           ALTERNATE RECORD KEY IS dur-k-desc = dur-desc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS dur-k-exercises = dur-exercises
           WITH DUPLICATES .
