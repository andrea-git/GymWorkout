      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-descr è l'ID del control ef-descr
           when 78-ID-ef-descr
                inquire ef-descr, value in ef-descr-buf

           |78-ID-ef-days è l'ID del control ef-days
           when 78-ID-ef-days
                inquire ef-days, value in ef-days-buf

           |10 è l'ID del control ef-macro
           when 10
                inquire ef-macro, value in ef-macro-buf

           end-evaluate.

