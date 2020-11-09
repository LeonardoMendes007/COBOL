       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP001.
      *AUTHOR. lEONARDO MENDES GOMES DOS SANTOS.
      **************************************
      * MANUTENCAO DO CADASTRO DE MEDICO   *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT". 
       01 REGMED.
          03 CRM           PIC 9(06).
          03 NOME          PIC X(30).
          03 ESPEC         PIC 9(02).
          03 SEXO          PIC X(01).
          03 DATANASC.
            05 DIANASC        PIC 99.
            05 MESNASC        PIC 99.
            05 ANONASC        PIC 9(04).
          03 EMAIL         PIC X(30).
          03 TELEFONE.
            05 DDD         PIC 9(02).
            05 NUM         PIC 9(09).   
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 TXTTPAMIGO    PIC X(10) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.
       01 TEXSEXO       PIC X(12) VALUE SPACES.

       01 TABESP.
          03 COLESP    PIC X(30) OCCURS 6 TIMES.

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAMED.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                              CADASTRO M".
           05  LINE 02  COLUMN 41 
               VALUE  "EDICO".
           05  LINE 05  COLUMN 01 
               VALUE  "       CRM:".
           05  LINE 07  COLUMN 01 
               VALUE  "       NOME:".
           05  LINE 10  COLUMN 01 
               VALUE  "       ESPECIALIDADE:".
           05  LINE 12  COLUMN 01 
               VALUE  "       SEXO:".
           05  LINE 14  COLUMN 01 
               VALUE  "       DATA NASCIMENTO:  .  .  ".
           05  LINE 16  COLUMN 01 
               VALUE  "       EMAIL:".
           05  LINE 18  COLUMN 01 
               VALUE  "       TELEFONE:".

           05  TCRM
               LINE 05  COLUMN 12  PIC 999.999
               USING  CRM
               HIGHLIGHT.

           05  TNOME
               LINE 07 COLUMN 13   PIC X(30)
               USING NOME
               HIGHLIGHT. 

           05  TESPEC
               LINE 10 COLUMN 22   PIC 9(02)  
               USING ESPEC
               HIGHLIGHT. 

           05  TSEXO
               LINE 12 COLUMN 14   PIC X(01)
               USING SEXO
               HIGHLIGHT.

           05  TDIA
               LINE 14 COLUMN 24   PIC 9(02) 
               USING DIANASC
               HIGHLIGHT.

           05  TMES
               LINE 14 COLUMN 27   PIC 9(02)
               USING MESNASC
               HIGHLIGHT.

           05  TANO
               LINE 14 COLUMN 30   PIC 9(04)
               USING ANONASC
               HIGHLIGHT.

           05  TEMAIL
               LINE 16 COLUMN 14   PIC X(30) 
               USING EMAIL
               HIGHLIGHT. 

           05  TDDD
               LINE 18 COLUMN 17   PIC 9(02)
               USING DDD
               HIGHLIGHT. 
           
           05  TNUM
               LINE 18 COLUMN 20   PIC 9(09)
               USING NUM
               HIGHLIGHT. 

       01  TELAESP.
           05  LINE 14  COLUMN 55 VALUE  "1 - CLINICA MEDICA".
           05  LINE 15  COLUMN 55 VALUE  "2 - UROLOGIA".
           05  LINE 16  COLUMN 55 VALUE  "3 - GINECOLOGIA".
           05  LINE 17  COLUMN 55 VALUE  "4 - PEDIATRIA".
           05  LINE 18  COLUMN 55 VALUE  "5 - CARDIOLOGISTA".
           05  LINE 19  COLUMN 55 VALUE  "6 - OFTALMOLOGIA".

       01  CLEANESP.
           05  LINE 14  COLUMN 55 VALUE  "                      ".
           05  LINE 15  COLUMN 55 VALUE  "                      ".
           05  LINE 16  COLUMN 55 VALUE  "                      ".
           05  LINE 17  COLUMN 55 VALUE  "                      ".
           05  LINE 18  COLUMN 55 VALUE  "                      ".
           05  LINE 19  COLUMN 55 VALUE  "                      ".    
         
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

           MOVE "CLINICA MEDICA" TO COLESP(1)
           MOVE "UROLOGIA" TO COLESP(2)
           MOVE "GINECOLOGIA" TO COLESP(3)
           MOVE "PEDIATRIA" TO COLESP(4)
           MOVE "CARDIOLOGIA" TO COLESP(5)
           MOVE "OFTALMOLOGIA" TO COLESP(6).
       
           
           
       R0.
           OPEN I-O CADMED
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADMED
                 CLOSE CADMED
                 MOVE "*** ARQUIVO CADAMIGO FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMIGO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       
       R1.

           MOVE SPACES TO NOME SEXO EMAIL
           MOVE ZEROS TO ESPEC DDD NUM CRM 
           MOVE ZEROS TO DIANASC MESNASC ANONASC
           DISPLAY TELAMED.

       R2.
           
           MOVE ZEROS TO CRM
           
           ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO ROT-FIM.

           IF CRM = ZEROS
                MOVE "CRM INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R2.

       
       LER-CADMED.
           READ CADMED
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"  
                DISPLAY TELAMED
                PERFORM R4A
                PERFORM R5A
                PERFORM ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADAMIGO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
       
       R3.
           MOVE SPACES TO NOME
           
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
           GO TO R2.

           IF NOME = SPACES
                MOVE "NOME INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R3.
           
       R4.
          
           MOVE ZEROS TO ESPEC

           DISPLAY TELAESP.

           ACCEPT TESPEC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
           GO TO R3.
    
       R4A.

           IF ESPEC > 0 AND ESPEC < 7
              DISPLAY (10, 25) COLESP(ESPEC)
           ELSE
              MOVE "*VERIFIQUE A TABELA E DIGITE UM NUMERO VALIDO*" 
              TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

       
       R5.
           
           DISPLAY CLEANESP.

           MOVE SPACES TO SEXO

           ACCEPT TSEXO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
           GO TO R4.
       
       R5A. 

           IF SEXO = "F" 
              DISPLAY (12, 16) "FEMINIMO"
           ELSE
              IF SEXO = "M"
                 DISPLAY (12, 16) "MASCULINO"
              ELSE
                 IF SEXO = "I"
                    DISPLAY (12, 16) "INDEFINIDO"
                 ELSE
                    MOVE "*DIGITE M - MASCULINO E F - FEMININO*" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO R5.


       R6.
          
           MOVE ZEROS TO DIANASC

           ACCEPT TDIA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO R5.

           IF DIANASC = 0 OR DIANASC >  31
                MOVE "DIA INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R6.

       R6A.

           MOVE ZEROS TO MESNASC

           ACCEPT TMES
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO R6.

           IF MESNASC = 0 OR MESNASC >  12
                MOVE "MES INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R6A.


       R6B.
           
           MOVE ZEROS TO ANONASC

           ACCEPT TANO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO R6A.

           IF ANONASC < 1950 OR ANONASC >  2020
                MOVE "ANO INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R6B.
      
       R7.

           MOVE SPACES TO EMAIL
           
           ACCEPT TEMAIL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
           GO TO R6.

           IF EMAIL = SPACES
                MOVE "EMAIL INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R7.

       R8.

           MOVE ZEROS TO DDD
           MOVE ZEROS TO NUM 
       
           ACCEPT TDDD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO R7.

           IF DDD = ZEROS
                MOVE "DDD INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R8.

       R8A.
       
           ACCEPT TNUM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO R8.

           IF NUM = ZEROS
                MOVE "NUMERO INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R8A.

      * ------------- VERICAR SE E ALTERACAO -----------------
           
           IF W-SEL = 1 
                GO TO ALT-OPC.


       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R8.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGMED
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* AMIGO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

            
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3
                   ELSE
                      GO TO EXC-OPC.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.

       
       EXC-DL1.
                DELETE CADMED RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO AMIGO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO"  TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
                 
       ALT-RW1.
                REWRITE REGMED
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
       

       ROT-FIM.
           CLOSE CADMED.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERA��O REALIZADO COM SUCESSO
      *    22 = REGISTRO J?CADASTRADO
      *    23 = REGISTRO N? ENCONTRADO
      *    30 = ARQUIVO N? ENCONTRADO
      