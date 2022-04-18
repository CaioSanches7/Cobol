       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOB01.
      ******************************************************************
      *ÁREA DE COMENTÁRIOS.                                            *
      *AUTOR    = CAIO SANCHES - CAIOSS.                               *
      *OBJETIVO = APRENDER COBOL.                                      *
      *DATA     = 19/03/2022.                                          *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WRK-NM          PIC X(20)            VALUE SPACES.
       77  WRK-IDADE       PIC 9(02)            VALUE ZEROS.
       77  WRK-SLR         PIC 9(05)V99         VALUE ZEROS.
       77  WRK-MSK-SLR     PIC $ZZ.ZZ9,99       VALUE ZEROS.
       77  WRK-CPF         PIC 999.999.999/99   VALUE ZEROS.
       77  WRK-VL01        PIC S9(05)V99        VALUE ZEROS.
       77  WRK-VL02        PIC S9(05)V99        VALUE ZEROS.
       77  WRK-SD          PIC S9(05)V99        VALUE ZEROS.
       77  WRK-SD-SINAL    PIC -Z9.V99          VALUE ZEROS.
       77  WRK-RST         PIC  9(02)           VALUE ZEROS.
       77  WRK-PRODUTO     PIC  X(20)           VALUE SPACES.
       77  WRK-FRETE       PIC S9(04)V99        VALUE ZEROS.
       77  WRK-FRETE-MASK  PIC $ZZZ9,99         VALUE ZEROS.
       77  WRK-UF          PIC  X(02)           VALUE SPACES.
       77  WRK-CONT        PIC  9(02)           VALUE 01.
       77  WRK-VL-01       PIC  9(02)           VALUE ZEROS.
       77  WRK-SALDO       PIC  9(02)           VALUE ZEROS.
       77  WRK-NIVEL       PIC  9(02)           VALUE ZEROS.
           88 ADM                               VALUE 01.
           88 USER                              VALUE 02.
      *
       01  WRK-DT-ATU.
           02 WRK-DT-ANO   PIC 9(04)            VALUE ZEROS.
           02 WRK-DT-MES   PIC 9(02)            VALUE ZEROS.
           02 WRK-DT-DIA   PIC 9(02)            VALUE ZEROS.
      *
        PROCEDURE DIVISION.
      ******************************************************************
       1000-MAIN               SECTION.
      ******************************************************************
      *
           PERFORM 2000-PREENCHE-VALORES.
      *     PERFORM 3000-MOSTRA-DATA-NOME.
      *     PERFORM 4000-BUSCA-SALARIO.
      *     PERFORM 5000-BUSCA-CPF.
      *     PERFORM 6000-CALCULA-VALORES.
      *     PERFORM 7000-CALCULA-NEGATIVO.
      *     PERFORM 8000-DESAFIO-MEDIA.
      *     PERFORM 9000-UTILIZA-EVALUATE.
      *     PERFORM 10000-CALCULA-FRETE.
      *     PERFORM 11000-NIVEL-88.
           PERFORM 12000-CALCULA-TABUADA.
      *     PERFORM 99999-DISPLAY.
           STOP RUN.
      *
       1999-SAIDA.
      ******************************************************************
       2000-PREENCHE-VALORES         SECTION.
      ******************************************************************
      *
      *     ACCEPT WRK-NM.
      *     ACCEPT WRK-DT-ATU FROM DATE YYYYMMDD.
      *     ACCEPT WRK-SLR.
      *     ACCEPT WRK-CPF.
      *     ACCEPT WRK-PRODUTO.
      *     ACCEPT WRK-VL01.
      *     ACCEPT WRK-UF.
      *     ACCEPT WRK-VL02.
      *     ACCEPT WRK-NIVEL.
           ACCEPT WRK-VL-01.
      *
       2999-SAIDA.
      ******************************************************************
       3000-MOSTRA-DATA-NOME         SECTION.
      ******************************************************************
      *
           DISPLAY 'DATA: 'WRK-DT-DIA '/' WRK-DT-MES '/' WRK-DT-ANO.
           DISPLAY 'NOME: 'WRK-NM(1:4).
      *
       3999-SAIDA.
      ******************************************************************
       4000-BUSCA-SALARIO      SECTION.
      ******************************************************************
      *
           IF WRK-SLR GREATER ZEROS
              MOVE WRK-SLR           TO WRK-MSK-SLR
              DISPLAY 'Salario: ' WRK-MSK-SLR
           ELSE
               DISPLAY 'Salario invalido ou valor igual a zero!'
           END-IF.
      *
       4999-SAIDA.
      ******************************************************************
       5000-BUSCA-CPF          SECTION.
      ******************************************************************
      *
           DISPLAY 'CPF: 'WRK-CPF.
      *
       5999-SAIDA.
      ******************************************************************
       6000-CALCULA-VALORES       SECTION.
      ******************************************************************
      *
           ADD WRK-VL01 WRK-VL02 TO WRK-SD.
           DISPLAY 'SOMA'.
           DISPLAY 'VL1: ' WRK-VL01 ' VL2: ' WRK-VL02 ' SD: ' WRK-SD.
      *
           SUBTRACT WRK-VL01 FROM WRK-VL02 GIVING WRK-SD.
           DISPLAY 'SUBTRACAO'.
           DISPLAY 'VL1: ' WRK-VL01 ' VL2: ' WRK-VL02 ' SD: ' WRK-SD.
      *
           DIVIDE WRK-VL01 BY WRK-VL02 GIVING WRK-SD REMAINDER WRK-RST.
           DISPLAY 'DIVISAO'.
           DISPLAY 'VL1: ' WRK-VL01 ' VL2: ' WRK-VL02 ' SD: ' WRK-SD.
           DISPLAY 'RESTO: ' WRK-RST.
      *
           MULTIPLY WRK-VL01 BY WRK-VL02 GIVING WRK-SD.
           DISPLAY 'MULTIPLICACAO'.
           DISPLAY 'VL1: ' WRK-VL01 ' VL2: ' WRK-VL02 ' SD: ' WRK-SD.
      *
           COMPUTE WRK-SD = WRK-VL01 + WRK-VL02.
           DISPLAY 'COMPUTE.'.
           DISPLAY 'VL1: ' WRK-VL01 ' VL2: ' WRK-VL02 ' SD: ' WRK-SD.
      *
       6999-SAIDA.
      *
      ******************************************************************
       7000-CALCULA-NEGATIVO          SECTION.
      ******************************************************************
      *
           SUBTRACT WRK-VL01 FROM WRK-VL02 GIVING WRK-SD.
           MOVE WRK-SD                         TO WRK-SD-SINAL.
           DISPLAY 'SUBTRACAO'.
           DISPLAY 'VL1: ' WRK-VL01 ' VL2: ' WRK-VL02 ' SD: ' WRK-SD.
           DISPLAY 'MASCARADO: ' WRK-SD-SINAL.
      *
       7999-SAIDA.
      ******************************************************************
       8000-DESAFIO-MEDIA          SECTION.
      ******************************************************************
      *
           COMPUTE WRK-SD = (WRK-VL01 + WRK-VL02)/2.
           MOVE WRK-SD             TO WRK-SD-SINAL.
           IF WRK-SD >= 6
              DISPLAY 'ALUNO APROVADO! NOTA: ' WRK-SD-SINAL
           ELSE
              IF WRK-SD >= 4
                  DISPLAY 'ALUNO EM RECUPERACAO! NOTA: ' WRK-SD-SINAL
              ELSE
                  DISPLAY 'ALUNO REPROVADO! NOTA: 'WRK-SD-SINAL
              END-IF
           END-IF.
           DISPLAY 'MEDIA = ' WRK-SD-SINAL.
      *
       8999-SAIDA.
      ******************************************************************
       9000-UTILIZA-EVALUATE       SECTION.
      ******************************************************************
      *
           COMPUTE WRK-SD = (WRK-VL01 + WRK-VL02)/2.
           MOVE WRK-SD             TO WRK-SD-SINAL.

           EVALUATE WRK-SD
              WHEN 10
                 DISPLAY 'PARABENS! VOCE TIROU 10!'
              WHEN 6 THRU 9,9
                 DISPLAY 'APROVADO!'
              WHEN 4 THRU 5,9
                 DISPLAY 'RECUPERACAO.'
              WHEN 0 THRU 3,9
                 DISPLAY 'REPROVADO'
              WHEN OTHER
                 DISPLAY 'ERRO, VALORES INVALIDOS'
              END-EVALUATE.

       9999-SAIDA.
      ******************************************************************
       10000-CALCULA-FRETE      SECTION.
      ******************************************************************
      *
           EVALUATE WRK-UF
               WHEN 'SP'
                   COMPUTE WRK-FRETE = WRK-VL01 * 1,05
               WHEN 'RJ'
                   COMPUTE WRK-FRETE = WRK-VL01 * 1,50
               WHEN 'MG'
                   COMPUTE WRK-FRETE = WRK-VL01 * 1,10
               WHEN OTHER
                   DISPLAY 'NAO ENTREGAMOS NA SUA REGIAO.'
           END-EVALUATE.
      *
           DISPLAY '==================================================='
           DISPLAY WRK-UF.
           DISPLAY WRK-VL01.
           DISPLAY WRK-FRETE.
      *
           IF WRK-FRETE NOT EQUAL ZEROS
               MOVE WRK-FRETE          TO WRK-FRETE-MASK.
               DISPLAY '=============================================='.
               DISPLAY 'VALOR DO PRODUTO COM O FRETE: R' WRK-FRETE.

       10999-SAIDA.
      ******************************************************************
       11000-NIVEL-88          SECTION.
      ******************************************************************
           IF ADM
               DISPLAY 'NIVEL ADMINISTRADOR!'
           ELSE
               IF USER
                   DISPLAY 'NIVEL USUARIO!'
               ELSE
                   DISPLAY 'USUARIO INVALIDO!'
               END-IF
           END-IF.
      ******************************************************************
       12000-CALCULA-TABUADA   SECTION.
      ******************************************************************
           PERFORM 10 TIMES
               COMPUTE WRK-SALDO = WRK-VL-01 * WRK-CONT
               DISPLAY WRK-VL-01 ' X ' WRK-CONT ' = ' WRK-SALDO
               COMPUTE WRK-CONT = WRK-CONT + 1
           END-PERFORM.

      *POSSO EXECUTAR UM PARAGRAFO X VEZES
      * PERFORM 'PARAGRAFO' X TIMES
      * PERFORM 0100-TESTE 10 TIMES.
      ******************************************************************
       99999-DISPLAY           SECTION.
      ******************************************************************
      *
           DISPLAY 'NOME: ' WRK-NM.
           DISPLAY 'IDADE: ' WRK-IDADE.
           DISPLAY 'SALARIO: ' WRK-SLR .
           DISPLAY 'SALARIO MASCARA: ' WRK-MSK-SLR.
           DISPLAY 'CPF: ' WRK-CPF.
           DISPLAY 'VALOR 1: ' WRK-VL01.
           DISPLAY 'VALOR 2: ' WRK-VL02.
           DISPLAY 'SALDO: ' WRK-SD.
           DISPLAY 'SALDO SINAL: ' WRK-SD-SINAL.
           DISPLAY 'RESTO: ' WRK-RST.
           DISPLAY 'PRODUTO: ' WRK-PRODUTO.
           DISPLAY 'FRETE: ' WRK-FRETE.
           DISPLAY 'FRETE MASCARA: ' WRK-FRETE-MASK.
           DISPLAY 'UF: ' WRK-UF.
           DISPLAY 'ANO: ' WRK-DT-ANO.
           DISPLAY 'MES: ' WRK-DT-MES.
           DISPLAY 'DIA: ' WRK-DT-DIA.
           DISPLAY 'NIVEL USUARIO: ' WRK-NIVEL.

       99999-SAIDA.
      ******************************************************************
