      *================================================================*
      * PROGRAM  : FEEWAIVE                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DECIDES WHETHER THE MONTHLY MAINTENANCE FEE IS      *
      *            WAIVED FOR THE STATEMENT CYCLE AND WHY. THE FIRST   *
      *            RULE THAT QUALIFIES SETS THE WAIVER CODE.           *
      *              ALL   FIRST CYCLE AFTER OPENING            NW     *
      *              ALL   PRIVATE WEALTH SEGMENT               PW     *
      *              DDAP  MIN DAILY BALANCE >= 1,500           MB     *
      *                    AVG DAILY BALANCE >= 5,000           AB     *
      *                    DIRECT DEPOSITS   >= 500 IN CYCLE    DD     *
      *                    EMPLOYEE                             EM     *
      *                    AGE 65 OR OVER                       SR     *
      *                    UNDER AGE 24                         ST     *
      *              DDPR  RELATIONSHIP BALANCE >= 50,000       RB     *
      *              MMKT  MIN DAILY BALANCE >= 2,500           MB     *
      *              SVST  BALANCE >= 300                       MB     *
      *                    CUSTOMER HAS PERSONAL CHECKING       LK     *
      * LINKAGE  : FWVPARM                                             *
      * CALLED BY: FEEASSES, ACCTINQ (CB03)                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-09-01 RJT  ORIGINAL - MINIMUM BALANCE WAIVER              *
      * 2002-04-08 BHS  AVERAGE BALANCE ALTERNATIVE FOR DDAP           *
      * 2006-02-13 BHS  DIRECT DEPOSIT WAIVER $500 - CHG0298           *
      * 2008-10-27 DPS  SVST LINKED CHECKING WAIVER                    *
      * 2012-05-07 DPS  SENIOR 65+ / STUDENT UNDER 24 - CHG0733        *
      * 2019-08-19 AKW  PRIVATE WEALTH / DDPR RELATIONSHIP - CHG1655   *
      * 2021-03-02 AKW  EMPLOYEE WAIVER - CHG1904                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FEEWAIVE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WAIVER-THRESHOLDS.
           05  WS-DDAP-MIN-BAL             PIC 9(09)V99 VALUE 1500.00.
           05  WS-DDAP-AVG-BAL             PIC 9(09)V99 VALUE 5000.00.
           05  WS-DDAP-DIRECT-DEP          PIC 9(09)V99 VALUE 500.00.
           05  WS-SENIOR-AGE               PIC 9(03)    VALUE 65.
           05  WS-STUDENT-AGE              PIC 9(03)    VALUE 24.
           05  WS-DDPR-REL-BAL             PIC 9(11)V99 VALUE 50000.00.
           05  WS-MMKT-MIN-BAL             PIC 9(09)V99 VALUE 2500.00.
           05  WS-SVST-MIN-BAL             PIC 9(09)V99 VALUE 300.00.

       01  WS-AGE-FIELDS.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-BUS-R REDEFINES WS-BUS-DATE.
               10  WS-BUS-CCYY             PIC 9(04).
               10  WS-BUS-MMDD             PIC 9(04).
           05  WS-DOB                      PIC 9(08).
           05  WS-DOB-R REDEFINES WS-DOB.
               10  WS-DOB-CCYY             PIC 9(04).
               10  WS-DOB-MMDD             PIC 9(04).

       LINKAGE SECTION.
       COPY FWVPARM.

       PROCEDURE DIVISION USING FEEWAIVE-PARMS.
       0000-MAIN.
           MOVE ZERO   TO FWV-CUST-AGE FWV-RETURN-CODE
           MOVE 'N'    TO FWV-WAIVE-FLAG
           MOVE SPACES TO FWV-WAIVER-CODE

           IF FWV-BUSINESS-DATE = ZERO
               MOVE 12 TO FWV-RETURN-CODE
               GOBACK
           END-IF
           PERFORM 1000-COMPUTE-AGE

           EVALUATE TRUE
               WHEN FWV-PRODUCT-CODE NOT = 'DDAP' AND 'DDPR'
                                       AND 'MMKT' AND 'SVST'
                   MOVE 'NF' TO FWV-WAIVER-CODE
               WHEN FWV-CYCLE-START-DATE = ZERO
               WHEN FWV-OPEN-DATE > FWV-CYCLE-START-DATE
                   MOVE 'NW' TO FWV-WAIVER-CODE
               WHEN FWV-CUST-SEGMENT = 'PW'
                   MOVE 'PW' TO FWV-WAIVER-CODE
               WHEN FWV-PRODUCT-CODE = 'DDAP'
                   PERFORM 2000-PERSONAL-CHECKING
               WHEN FWV-PRODUCT-CODE = 'DDPR'
                   IF FWV-REL-BALANCE >= WS-DDPR-REL-BAL
                       MOVE 'RB' TO FWV-WAIVER-CODE
                   END-IF
               WHEN FWV-PRODUCT-CODE = 'MMKT'
                   IF FWV-MIN-BAL-CYC >= WS-MMKT-MIN-BAL
                       MOVE 'MB' TO FWV-WAIVER-CODE
                   END-IF
               WHEN FWV-PRODUCT-CODE = 'SVST'
                   EVALUATE TRUE
                       WHEN FWV-CURRENT-BAL >= WS-SVST-MIN-BAL
                           MOVE 'MB' TO FWV-WAIVER-CODE
                       WHEN FWV-HAS-DDAP-FLAG = 'Y'
                           MOVE 'LK' TO FWV-WAIVER-CODE
                   END-EVALUATE
           END-EVALUATE

           IF FWV-WAIVER-CODE = SPACES
               MOVE 08 TO FWV-RETURN-CODE
           ELSE
               MOVE 'Y' TO FWV-WAIVE-FLAG
           END-IF
           GOBACK.

      *----------------------------------------------------------------*
      * AGE IN WHOLE YEARS ON THE BUSINESS DATE. NO BIRTH DATE ON FILE *
      * GIVES AGE ZERO, WHICH QUALIFIES FOR NEITHER AGE WAIVER.        *
      *----------------------------------------------------------------*
       1000-COMPUTE-AGE.
           IF FWV-CUST-BIRTH-DATE = ZERO
              OR FWV-CUST-BIRTH-DATE > FWV-BUSINESS-DATE
               EXIT PARAGRAPH
           END-IF
           MOVE FWV-BUSINESS-DATE   TO WS-BUS-DATE
           MOVE FWV-CUST-BIRTH-DATE TO WS-DOB
           COMPUTE FWV-CUST-AGE = WS-BUS-CCYY - WS-DOB-CCYY
           IF WS-BUS-MMDD < WS-DOB-MMDD
               SUBTRACT 1 FROM FWV-CUST-AGE
           END-IF.

      *----------------------------------------------------------------*
       2000-PERSONAL-CHECKING.
           EVALUATE TRUE
               WHEN FWV-MIN-BAL-CYC >= WS-DDAP-MIN-BAL
                   MOVE 'MB' TO FWV-WAIVER-CODE
               WHEN FWV-AVG-BAL-CYC >= WS-DDAP-AVG-BAL
                   MOVE 'AB' TO FWV-WAIVER-CODE
               WHEN FWV-DIRECT-DEP-AMT >= WS-DDAP-DIRECT-DEP
                   MOVE 'DD' TO FWV-WAIVER-CODE
               WHEN FWV-EMPLOYEE-FLAG = 'Y'
                   MOVE 'EM' TO FWV-WAIVER-CODE
               WHEN FWV-CUST-AGE >= WS-SENIOR-AGE
                   MOVE 'SR' TO FWV-WAIVER-CODE
               WHEN FWV-CUST-AGE > ZERO
                AND FWV-CUST-AGE < WS-STUDENT-AGE
                   MOVE 'ST' TO FWV-WAIVER-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
