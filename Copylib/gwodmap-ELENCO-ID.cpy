      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-desc è l'ID del control ef-desc
           when 78-ID-ef-desc
                inquire ef-desc, value in ef-desc-buf

           end-evaluate.

