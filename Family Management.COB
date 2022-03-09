      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. T10.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *----
       77 NUM-FAMILIAS                PIC 9(02)         VALUES 0.
       77 ZNUM-FAMILIAS               PIC Z9.
       77 RESIDENCIA                  PIC 9             VALUES 0.
           88 CONTINENTE                                VALUES 1.
           88 ACORES                                    VALUES 2.
           88 MADEIRA                                   VALUES 3.
       77 IDADE-PAI                   PIC 9(02)         VALUES 0.
       77 IDADE-MAE                   PIC 9(02)         VALUES 0.
       77 NUM-FILHOS                  PIC 9(02)         VALUES 0.
       77 ZNUM-FILHOS                 PIC Z9.
       77 RENDIMENTO-FAMILIAR         PIC 9(05)         VALUES 0.
       77 ZRENDIMENTO-FAMILIAR        PIC ZZ,ZZ9.
       77 SUM-RENDIMENTO-FAMILIAR     PIC 9(07)         VALUES 0.
       77 ZSUM-RENDIMENTO-FAMILIAR    PIC Z,ZZZ,ZZZ.
      *----
       77 NUM-CONTINENTE              PIC 9(02)         VALUES 0.
       77 NUM-ACORES                  PIC 9(02)         VALUES 0.
       77 NUM-MADEIRA                 PIC 9(02)         VALUES 0.
      *----
       77 SUM-NUM-FILHOS              PIC 9(03)         VALUES 0.
       77 NUM-MEDIO-FILHOS            PIC 9(03)V99      VALUES 0.
       77 ZNUM-MEDIO-FILHOS           PIC ZZ9.99.
      *----
       77 NUM-FILHOS-2MENOS           PIC 9(02)         VALUES 0.
      *----
       77 NUM-FAMILIAS-MAIS2          PIC 9(02)         VALUES 0.
      *----
       77 SUM-IDADE-PAI               PIC 9(04)         VALUES 0.
       77 SUM-IDADE-MAE               PIC 9(04)         VALUES 0.
       77 IDADE-MEDIA-PAIS            PIC 9(03)V99      VALUES 0.
       77 ZIDADE-MEDIA-PAIS           PIC ZZ9.99.
      *----
       77 RESPOSTA                    PIC X VALUES "S".
       SCREEN SECTION.
       01 CLS BLANK SCREEN.

       PROCEDURE DIVISION.
       INICIO.

           PERFORM ENTRADA THRU SAIDA UNTIL FUNCTION
           UPPER-CASE(RESPOSTA) <> "S".
       FIM.
           STOP RUN.
       ENTRADA.
           MOVE 0 TO NUM-CONTINENTE, NUM-ACORES, NUM-MADEIRA,
           SUM-IDADE-PAI, SUM-IDADE-MAE,NUM-FAMILIAS-MAIS2,
           NUM-FILHOS-2MENOS, SUM-RENDIMENTO-FAMILIAR, SUM-NUM-FILHOS.
           DISPLAY CLS.
           DISPLAY "QUESTIONARIO A FAMILIAS" FOREGROUND-COLOR 2
           HIGHLIGHT AT 0101.
      * ----- N. DE FAMILIAS
           DISPLAY "INDIQUE NUMERO DE FAMILIAS:" AT 0301.
           ACCEPT NUM-FAMILIAS AT 0328 FOREGROUND-COLOR 3 HIGHLIGHT.
           MOVE NUM-FAMILIAS TO ZNUM-FAMILIAS.
           DISPLAY ZNUM-FAMILIAS AT 0328 FOREGROUND-COLOR 3 HIGHLIGHT.

           PERFORM NUM-FAMILIAS TIMES
               MOVE 0 TO RESIDENCIA
               DISPLAY "RESIDENCIA (1)CONTINENTE (2)ACORES (3)MADEIRA:"
               AT 0501

               PERFORM UNTIL CONTINENTE OR
                             ACORES     OR
                             MADEIRA

                   ACCEPT RESIDENCIA AT 0547 FOREGROUND-COLOR 3
                   HIGHLIGHT

                   IF (NOT CONTINENTE AND NOT ACORES AND NOT MADEIRA)
                       THEN
                       DISPLAY "RESIDENCIA NAO E VALIDA."
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0549
                   ELSE
                       DISPLAY " " ERASE EOL AT 0549
                       EVALUATE RESIDENCIA
                            WHEN 1
                                DISPLAY "CONTINENTE" AT 0549
                                FOREGROUND-COLOR 3 HIGHLIGHT
                            WHEN 2
                                DISPLAY "ACORES    " AT 0549
                                FOREGROUND-COLOR 3 HIGHLIGHT
                            WHEN 3
                                DISPLAY "MADEIRA   " AT 0549
                                FOREGROUND-COLOR 3 HIGHLIGHT
                       END-EVALUATE
                   END-IF
               END-PERFORM
               IF CONTINENTE THEN
                   ADD 1 TO NUM-CONTINENTE
               ELSE
                   IF ACORES THEN
                       ADD 1 TO NUM-ACORES
                   ELSE
                       ADD 1 TO NUM-MADEIRA
                   END-IF
               END-IF
      *---- IDADE DO PAI
               DISPLAY "IDADE DO PAI.................................:"
               AT 0701
               ACCEPT IDADE-PAI AT 0747 FOREGROUND-COLOR 3 HIGHLIGHT
               ADD IDADE-PAI TO SUM-IDADE-PAI
      *---- IDADE DA MAE
               DISPLAY "IDADE DA MAE.................................:"
               AT 0901
               ACCEPT IDADE-MAE AT 0947 FOREGROUND-COLOR 3 HIGHLIGHT
               ADD IDADE-MAE TO SUM-IDADE-MAE
      *---- NUMERO DE FILHOS
               DISPLAY "NUMERO DE FILHOS.............................:"
               AT 1101
               ACCEPT NUM-FILHOS AT 1147 FOREGROUND-COLOR 3 HIGHLIGHT
               MOVE NUM-FILHOS TO ZNUM-FILHOS
               ADD NUM-FILHOS TO SUM-NUM-FILHOS
               DISPLAY ZNUM-FILHOS AT 1147 FOREGROUND-COLOR 3 HIGHLIGHT
               IF (NUM-FILHOS > 2) THEN
                   ADD 1 TO NUM-FAMILIAS-MAIS2
               END-IF
      *---- RENDIMENTO FAMILIAR
               DISPLAY "RENDIMENTO FAMILIAR..........................:"
               AT 1301
               ACCEPT RENDIMENTO-FAMILIAR AT 1347 FOREGROUND-COLOR 3
               HIGHLIGHT
               MOVE RENDIMENTO-FAMILIAR TO ZRENDIMENTO-FAMILIAR
               DISPLAY ZRENDIMENTO-FAMILIAR AT 1347 FOREGROUND-COLOR 3
               HIGHLIGHT
               IF (NUM-FILHOS < 3) THEN
                   ADD 1 TO NUM-FILHOS-2MENOS
                   ADD RENDIMENTO-FAMILIAR TO SUM-RENDIMENTO-FAMILIAR
               END-IF
      *---- LIMPAR ACCEPTS
           DISPLAY " " ERASE EOL AT 0547
           DISPLAY " " ERASE EOL AT 0747
           DISPLAY " " ERASE EOL AT 0947
           DISPLAY " " ERASE EOL AT 1147
           DISPLAY " " ERASE EOL AT 1347

           END-PERFORM.
      *---- RESULTADOS
           DISPLAY "================================================="
           AT 1501.
           DISPLAY FUNCTION CONCATENATE("FAMILIAS NO CONTINENTE: ",
           NUM-CONTINENTE) AT 1701.
           DISPLAY FUNCTION CONCATENATE("FAMILIAS NOS ACORES: ",
           NUM-ACORES) AT 1801.
           DISPLAY FUNCTION CONCATENATE("FAMILIAS NA MADEIRA: ",
           NUM-MADEIRA) AT 1901.
      *---- NUMERO MEDIO DE FILHOS POR CASAL
           COMPUTE NUM-MEDIO-FILHOS = (SUM-NUM-FILHOS / NUM-FAMILIAS).
           MOVE NUM-MEDIO-FILHOS TO ZNUM-MEDIO-FILHOS.
           DISPLAY FUNCTION CONCATENATE("NUMERO MEDIO DE FILHOS POR ",
           "CASAL: ",ZNUM-MEDIO-FILHOS) AT 2001.
      *---- RENDIMENTO MEDIO CASAIS COM 2 FILHOS OU MENOS
           COMPUTE SUM-RENDIMENTO-FAMILIAR=(SUM-RENDIMENTO-FAMILIAR /
                                            NUM-FILHOS-2MENOS)
           MOVE SUM-RENDIMENTO-FAMILIAR TO ZSUM-RENDIMENTO-FAMILIAR
           DISPLAY FUNCTION CONCATENATE("RENDIMENTO MEDIO CASAIS ",
           "COM 2 FILHOS OU MENOS: ",ZSUM-RENDIMENTO-FAMILIAR) AT 2101.
      *----  FAMILIAS COM MAIS DE 2 FILHOS
           DISPLAY FUNCTION CONCATENATE("FAMILIAS COM MAIS DE 2 FILHOS:",
           NUM-FAMILIAS-MAIS2) AT 2201.
      *---- IDADE MEDIA DOS PAIS
           COMPUTE IDADE-MEDIA-PAIS = (SUM-IDADE-PAI + SUM-IDADE-MAE)
                                      / (NUM-FAMILIAS * 2).
           MOVE IDADE-MEDIA-PAIS TO ZIDADE-MEDIA-PAIS.
           DISPLAY FUNCTION CONCATENATE("IDADE MEDIA DOS PAIS:",
           ZIDADE-MEDIA-PAIS) AT 2301.
           DISPLAY "=============FIM DO QUESTIONARIO==================="
           AT 2401.
           DISPLAY "DIGITE (S) PARA CONTINUAR, OUTRA TECLA PARA SAIR:"
           AT 2501.
           ACCEPT RESPOSTA AT 2550.

       SAIDA.
           EXIT.
       END PROGRAM T10.
