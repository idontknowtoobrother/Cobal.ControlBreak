       IDENTIFICATION DIVISION. 
       PROGRAM-ID. CB1.
       AUTHOR. 62160246.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT INPUT1-FILE ASSIGN TO "input1.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION. 
       FILE SECTION. 
       FD  INPUT1-FILE.
       01 INPUT1-BUFFER.
          88 END-OF-INPUT1-FILE             VALUE HIGH-VALUES.
       05 COL-A                  PIC X(2).
          05 COL-COUNT           PIC 9(3).
       WORKING-STORAGE SECTION. 
       01 TOTAL                  PIC 9(4)   VALUE ZEROS.
       01 COL-A-TOTAL            PIC 9(4).
       01 COL-A-PROCESSING       PIC X(2).
       01 RPT-HEADER.
          05 FILLER              PIC X(4)   VALUE "  A ".
          05 FILLER              PIC X(4)   VALUE SPACES.
          05 FILLER              PIC X(7)   VALUE "  TOTAL".
       01 RPT-ROW.
          05 RPT-COL-A           PIC BBX(2).
          05 FILLER              PIC X(5)   VALUE SPACES.
          05 RPT-COL-A-TOTAL     PIC ZZZ9.
       01 RPT-FOOTER.
          05 FILLER              PIC X(9)   VALUE "   TOTAL:".
          05 RPT-TOTAL           PIC ZZZ9.
       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT INPUT1-FILE
           DISPLAY RPT-HEADER 
           PERFORM READ-LINE
           PERFORM PROCEDURE-COL-A UNTIL END-OF-INPUT1-FILE 
           MOVE TOTAL TO RPT-TOTAL 
           DISPLAY RPT-FOOTER 
           CLOSE INPUT1-FILE 
           GOBACK 
           .
       PROCEDURE-COL-A.
           MOVE COL-A TO COL-A-PROCESSING 
           MOVE ZEROS TO COL-A-TOTAL 
           PERFORM PROCEDURE-LINE UNTIL COL-A NOT = COL-A-PROCESSING
           MOVE COL-A-PROCESSING TO RPT-COL-A 
           MOVE COL-A-TOTAL TO RPT-COL-A-TOTAL
           DISPLAY RPT-ROW 
           .
       PROCEDURE-LINE. 
           ADD COL-COUNT TO TOTAL, COL-A-TOTAL 
           PERFORM READ-LINE 
           .
       READ-LINE.
           READ INPUT1-FILE 
           AT END
              SET END-OF-INPUT1-FILE TO TRUE
           END-READ
           .