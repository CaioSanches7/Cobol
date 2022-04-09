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
        77 WRK-NM          PIC X(20)           VALUE SPACES.
        77 WRK-IDADE       PIC 9(02)           VALUE ZEROS.
        77 WRK-SLR         PIC 9(05)V99        VALUE ZEROS.
        77 WRK-MSK-SLR     PIC $ZZ.ZZ9,99      VALUE ZEROS.
        77 WRK-CPF         PIC 999.999.999/99  VALUE ZEROS.
        77 WRK-VL01        PIC S9(02)V99          VALUE ZEROS.
        77 WRK-VL02        PIC S9(02)V99          VALUE ZEROS.
        77 WRK-SD          PIC S9(02)V99          VALUE ZEROS.
        77 WRK-SD-SINAL    PIC -Z9.V99          VALUE ZEROS.
        77 WRK-RST         PIC 9(02)           VALUE ZEROS.
      *
        01 WRK-DT-ATU.
           02 WRK-DT-ANO   PIC 9(04)           VALUE ZEROS.
           02 WRK-DT-MES   PIC 9(02)           VALUE ZEROS.
           02 WRK-DT-DIA   PIC 9(02)           VALUE ZEROS.
      *
        PROCEDURE DIVISION.
      ******************************************************************
       1000-MAIN               SECTION.
      ******************************************************************
      *
      *     PERFORM 2000-BUSCA-NOME.
      *     PERFORM 3000-BUSCA-DATA.
      *     PERFORM 4000-BUSCA-SALARIO.
      *     PERFORM 5000-BUSCA-CPF.
      *     PERFORM 6000-CALCULA-VALORES.
      *     PERFORM 7000-CALCULA-NEGATIVO.
           PERFORM 8000-DESAFIO-MEDIA.
           STOP RUN.
      *
       1999-SAIDA.
      ******************************************************************
       2000-BUSCA-NOME         SECTION.
      ******************************************************************
      *
           ACCEPT WRK-NM.
           DISPLAY 'NOME: 'WRK-NM(1:4).
      *
       2999-SAIDA.
      ******************************************************************
       3000-BUSCA-DATA         SECTION.
      ******************************************************************
      *
           ACCEPT WRK-DT-ATU FROM DATE YYYYMMDD.
           DISPLAY 'DATA: 'WRK-DT-DIA '/' WRK-DT-MES '/' WRK-DT-ANO.
      *
       3999-SAIDA.
      ******************************************************************
       4000-BUSCA-SALARIO      SECTION.
      ******************************************************************
      *
           ACCEPT WRK-SLR.
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
           ACCEPT WRK-CPF.
           DISPLAY 'CPF: 'WRK-CPF.
      *
       5999-SAIDA.
      ******************************************************************
       6000-CALCULA-VALORES       SECTION.
      ******************************************************************
      *
           ACCEPT WRK-VL01.
           ACCEPT WRK-VL02.
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
           ACCEPT WRK-VL01.
           ACCEPT WRK-VL02.
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
           ACCEPT WRK-VL01.
           ACCEPT WRK-VL02.
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
