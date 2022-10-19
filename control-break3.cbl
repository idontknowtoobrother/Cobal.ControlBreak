       IDENTIFICATION DIVISION. 
       PROGRAM-ID. CB3.
       AUTHOR. 62160246.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT INPUT1-FILE ASSIGN TO "input3.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION. 
       FILE SECTION. 
       FD  INPUT1-FILE.
       01 INPUT1-BUFFER.
          88 END-OF-INPUT1-FILE             VALUE HIGH-VALUES.
       05 COL-A                  PIC X(2).
          05 COL-B               PIC X(2).
          05 COL-C               PIC X(2).
          05 COL-COUNT           PIC 9(3).
       WORKING-STORAGE SECTION. 
       01 TOTAL                  PIC 9(4)   VALUE ZEROS.
       01 COL-A-TOTAL            PIC 9(4).
       01 COL-A-PROCESSING       PIC X(2).
       01 COL-B-TOTAL            PIC 9(4).
       01 COL-B-PROCESSING       PIC X(2).
       01 COL-C-TOTAL            PIC 9(4).
       01 COL-C-PROCESSING       PIC X(2).
       01 RPT-HEADER.
          05 FILLER              PIC X(4)   VALUE "  A ".
          05 FILLER              PIC X(4)   VALUE SPACES.
          05 FILLER              PIC X(4)   VALUE "   B".
          05 FILLER              PIC X(4)   VALUE SPACES.
          05 FILLER              PIC X(4)   VALUE "   C".
          05 FILLER              PIC X(4)   VALUE SPACES.
          05 FILLER              PIC X(7)   VALUE "  TOTAL".
       01 RPT-ROW.
          05 RPT-COL-A           PIC BBX(2).
          05 FILLER              PIC X(5)   VALUE SPACES.
          05 RPT-COL-B           PIC BBX(2).
          05 FILLER              PIC X(5)   VALUE SPACES.
          05 RPT-COL-C           PIC BBX(2).
          05 FILLER              PIC X(5)   VALUE SPACES.
          05 RPT-COL-TOTAL       PIC ZZZ9.
       01 RPT-A-TOTAL-ROW.
          05 FILLER              PIC X(7)   VALUE "       ".
          05 FILLER              PIC X(11)  VALUE "  A TOTAL: ".
          05 RPT-A-TOTAL         PIC ZZZ9.
       01 RPT-B-TOTAL-ROW.
          05 FILLER              PIC X(7)   VALUE "       ".
          05 FILLER              PIC X(11)  VALUE "  B TOTAL: ".
          05 RPT-B-TOTAL         PIC ZZZ9.

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
           MOVE COL-A-PROCESSING TO RPT-COL-A 
           MOVE ZEROS TO COL-A-TOTAL 
           PERFORM PROCEDURE-COL-B UNTIL COL-A NOT = COL-A-PROCESSING
           MOVE COL-A-TOTAL TO RPT-A-TOTAL
           DISPLAY RPT-A-TOTAL-ROW 
           .
       PROCEDURE-COL-B.
           MOVE COL-B TO COL-B-PROCESSING 
           MOVE COL-B-PROCESSING TO RPT-COL-B
           MOVE ZEROS TO COL-B-TOTAL 
           PERFORM PROCEDURE-COL-C UNTIL COL-B NOT = COL-B-PROCESSING
              OR COL-A NOT = COL-A-PROCESSING
           MOVE COL-B-TOTAL TO RPT-B-TOTAL
           DISPLAY RPT-B-TOTAL-ROW 
           .
       PROCEDURE-COL-C.
           MOVE COL-C TO COL-C-PROCESSING 
           MOVE COL-C-PROCESSING TO RPT-COL-C
           MOVE ZEROS TO COL-C-TOTAL 
           PERFORM PROCEDURE-LINE UNTIL COL-C NOT = COL-C-PROCESSING
              OR COL-B NOT = COL-B-PROCESSING
              OR COL-A NOT = COL-A-PROCESSING
           MOVE COL-C-TOTAL TO RPT-COL-TOTAL
           DISPLAY RPT-ROW 
           MOVE SPACES TO RPT-COL-A, RPT-COL-B, RPT-COL-C 
           .
       PROCEDURE-LINE. 
           ADD COL-COUNT TO TOTAL, COL-A-TOTAL, COL-B-TOTAL, COL-C-TOTAL
           PERFORM READ-LINE 
           .
       READ-LINE.
           READ INPUT1-FILE 
           AT END
              SET END-OF-INPUT1-FILE TO TRUE
           END-READ
           .