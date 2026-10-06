      *================================================================*
      * PROGRAM  : UTAMLRUL                                            *
      * TYPE     : UNIT TEST DRIVER                                    *
      * PURPOSE  : UNIT TESTS FOR AMLRULES - AML SCENARIO ENGINE       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-06-01 DPS  ORIGINAL                                       *
      * 2018-05-11 AKW  AML7 CASES                                     *
      * 2020-11-30 AKW  INC39904 AML1 RE-ALERT CASE                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTAMLRUL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTPARM.
       COPY AMLPARM.
       01  WS-ADD-ITEM.
           05  WS-ADD-DATE                 PIC 9(08).
           05  WS-ADD-ACCT                 PIC X(12).
           05  WS-ADD-CODE                 PIC X(04).
           05  WS-ADD-DR-CR                PIC X(01).
           05  WS-ADD-AMOUNT               PIC S9(13)V99 COMP-3.
           05  WS-ADD-CHANNEL              PIC X(03).
           05  WS-ADD-CP-ACCT              PIC X(17).
           05  WS-ADD-CP-COUNTRY           PIC X(02).
       01  WS-FIND.
           05  WS-WANT-SCEN                PIC X(04).
           05  WS-FOUND-COUNT              PIC 9(02).
           05  WS-FOUND-SEV                PIC X(01).
           05  WS-FOUND-AMT                PIC S9(13)V99 COMP-3.
           05  WS-HX                       PIC 9(02).
           05  WS-NX                       PIC 9(02).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 'INIT' TO TST-FUNCTION
           MOVE 'AMLRULES' TO TST-SUITE-NAME
           CALL 'TESTLIB' USING TEST-PARMS

           PERFORM 1000-TEST-AML1-RAPID
           PERFORM 1100-TEST-AML2-GEO
           PERFORM 1200-TEST-AML3-PEP
           PERFORM 1300-TEST-AML4-MSB
           PERFORM 1400-TEST-AML5-DORMANT
           PERFORM 1500-TEST-AML6-ROUND-WIRE
           PERFORM 1600-TEST-AML7-PROFILE
           PERFORM 1700-TEST-INVALID

           MOVE 'SUMM' TO TST-FUNCTION
           CALL 'TESTLIB' USING TEST-PARMS
           STOP RUN.

      *----------------------------------------------------------------*
      * TAYLOR GREENE - WIRE IN 40,000 YESTERDAY, OUT 38,500 TODAY     *
      *----------------------------------------------------------------*
       1000-TEST-AML1-RAPID.
           PERFORM 8000-RESET
           PERFORM 8050-SET-RETAIL-LOW-ACTIVITY
           MOVE 20260929 TO WS-ADD-DATE
           MOVE 'DWIR' TO WS-ADD-CODE
           MOVE 'C' TO WS-ADD-DR-CR
           MOVE 40000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           MOVE 20260930 TO WS-ADD-DATE
           MOVE 'WWIR' TO WS-ADD-CODE
           MOVE 'D' TO WS-ADD-DR-CR
           MOVE 38500.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           MOVE 'AML1' TO WS-WANT-SCEN
           PERFORM 8200-FIND
           MOVE 'AML1 40,000 IN THEN 38,500 OUT (96%) RAISES HIT'
             TO TST-CASE-NAME
           MOVE 1 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ
           MOVE 'AML1 SEVERITY HIGH' TO TST-CASE-NAME
           MOVE 'H' TO TST-EXPECTED
           MOVE WS-FOUND-SEV TO TST-ACTUAL
           PERFORM 9100-AEQ

           PERFORM 8000-RESET
           PERFORM 8050-SET-RETAIL-LOW-ACTIVITY
           MOVE 20260929 TO WS-ADD-DATE
           MOVE 'DWIR' TO WS-ADD-CODE
           MOVE 'C' TO WS-ADD-DR-CR
           MOVE 40000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           MOVE 20260930 TO WS-ADD-DATE
           MOVE 'WWIR' TO WS-ADD-CODE
           MOVE 'D' TO WS-ADD-DR-CR
           MOVE 35000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML1 87.5% MOVED OUT - NO HIT' TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ

      *    PATTERN COMPLETED YESTERDAY - NOT RE-RAISED TODAY
           PERFORM 8000-RESET
           PERFORM 8050-SET-RETAIL-LOW-ACTIVITY
           MOVE 20260928 TO WS-ADD-DATE
           MOVE 'DWIR' TO WS-ADD-CODE
           MOVE 'C' TO WS-ADD-DR-CR
           MOVE 40000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           MOVE 20260929 TO WS-ADD-DATE
           MOVE 'WWIR' TO WS-ADD-CODE
           MOVE 'D' TO WS-ADD-DR-CR
           MOVE 39000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           MOVE 20260930 TO WS-ADD-DATE
           MOVE 'WPOS' TO WS-ADD-CODE
           MOVE 50.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML1 NOT RE-RAISED AFTER PATTERN COMPLETED'
             TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ

      *    SWEEPS ARE NOT MONITORED (INC22710)
           PERFORM 8000-RESET
           MOVE 'CB' TO AMP-SEGMENT
           MOVE 'DSWP' TO WS-ADD-CODE
           MOVE 'C' TO WS-ADD-DR-CR
           MOVE 500000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           MOVE 'WSWP' TO WS-ADD-CODE
           MOVE 'D' TO WS-ADD-DR-CR
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML1 IGNORES SWEEP IN / SWEEP OUT' TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ.

      *----------------------------------------------------------------*
       1100-TEST-AML2-GEO.
           PERFORM 8000-RESET
           MOVE 'CB' TO AMP-SEGMENT
           MOVE 'WWIR' TO WS-ADD-CODE
           MOVE 'D' TO WS-ADD-DR-CR
           MOVE 'WIR' TO WS-ADD-CHANNEL
           MOVE 15000.00 TO WS-ADD-AMOUNT
           MOVE 'HT' TO WS-ADD-CP-COUNTRY
           PERFORM 8100-ADD
           MOVE 2999.99 TO WS-ADD-AMOUNT
           MOVE 'NG' TO WS-ADD-CP-COUNTRY
           PERFORM 8100-ADD
           MOVE 50000.00 TO WS-ADD-AMOUNT
           MOVE 'GB' TO WS-ADD-CP-COUNTRY
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           MOVE 'AML2' TO WS-WANT-SCEN
           PERFORM 8200-FIND
           MOVE 'AML2 ONLY HT 15,000 HITS (NG < 3,000, GB NOT LISTED)'
             TO TST-CASE-NAME
           MOVE 1 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ
           MOVE 'AML2 GREY-LIST COUNTRY IS MEDIUM SEVERITY'
             TO TST-CASE-NAME
           MOVE 'M' TO TST-EXPECTED
           MOVE WS-FOUND-SEV TO TST-ACTUAL
           PERFORM 9100-AEQ

           PERFORM 8000-RESET
           MOVE 'DWIR' TO WS-ADD-CODE
           MOVE 'C' TO WS-ADD-DR-CR
           MOVE 'WIR' TO WS-ADD-CHANNEL
           MOVE 3000.00 TO WS-ADD-AMOUNT
           MOVE 'IR' TO WS-ADD-CP-COUNTRY
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML2 INCOMING 3,000.00 FROM IR - HIGH SEVERITY'
             TO TST-CASE-NAME
           MOVE 'H' TO TST-EXPECTED
           MOVE WS-FOUND-SEV TO TST-ACTUAL
           PERFORM 9100-AEQ.

      *----------------------------------------------------------------*
       1200-TEST-AML3-PEP.
           PERFORM 8000-RESET
           MOVE 'PW' TO AMP-SEGMENT
           MOVE 'Y'  TO AMP-PEP-FLAG
           MOVE 850000.00 TO AMP-ANNUAL-INCOME
           MOVE 'WWIR' TO WS-ADD-CODE
           MOVE 'D' TO WS-ADD-DR-CR
           MOVE 49999.99 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           MOVE 50000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           MOVE 'AML3' TO WS-WANT-SCEN
           PERFORM 8200-FIND
           MOVE 'AML3 PEP 50,000.00 HITS, 49,999.99 DOES NOT'
             TO TST-CASE-NAME
           MOVE 1 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ

           MOVE 'N' TO AMP-PEP-FLAG
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML3 NOT RAISED FOR NON-PEP' TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ.

      *----------------------------------------------------------------*
      * QUICKSEND: 10 PRIOR DAYS OF 20,000 = 200,000 / 21 BUS DAYS     *
      * = 9,523.81 AVG -> LIMIT 28,571.43                              *
      *----------------------------------------------------------------*
       1300-TEST-AML4-MSB.
           PERFORM 8000-RESET
           PERFORM 8060-SET-MSB-HISTORY
           MOVE 20260930 TO WS-ADD-DATE
           MOVE 'DCSH' TO WS-ADD-CODE
           MOVE 45000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           MOVE 'AML4' TO WS-WANT-SCEN
           PERFORM 8200-FIND
           MOVE 'AML4 MSB 45,000 TODAY > 3 X 30-DAY AVERAGE'
             TO TST-CASE-NAME
           MOVE 1 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ

           PERFORM 8000-RESET
           PERFORM 8060-SET-MSB-HISTORY
           MOVE 20260930 TO WS-ADD-DATE
           MOVE 'DCSH' TO WS-ADD-CODE
           MOVE 28000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML4 MSB 28,000 TODAY BELOW 28,571.43 - NO HIT'
             TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ

           PERFORM 8000-RESET
           PERFORM 8060-SET-MSB-HISTORY
           MOVE 'N' TO AMP-MSB-FLAG
           MOVE 20260930 TO WS-ADD-DATE
           MOVE 'DCSH' TO WS-ADD-CODE
           MOVE 45000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML4 NOT RAISED FOR NON-MSB CORPORATE'
             TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ.

      *----------------------------------------------------------------*
      * BAKER 100000000015 DORMANT SINCE 2024-06-01                    *
      *----------------------------------------------------------------*
       1400-TEST-AML5-DORMANT.
           PERFORM 8000-RESET
           PERFORM 8050-SET-RETAIL-LOW-ACTIVITY
           MOVE 24000.00 TO AMP-ANNUAL-INCOME
           MOVE 'D'      TO AMP-ACCT-STATUS(1)
           MOVE 20240601 TO AMP-ACCT-DORMANT-DATE(1)
           MOVE 'DWIR' TO WS-ADD-CODE
           MOVE 'C' TO WS-ADD-DR-CR
           MOVE 7500.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           MOVE 'AML5' TO WS-WANT-SCEN
           PERFORM 8200-FIND
           MOVE 'AML5 DORMANT ACCOUNT 7,500 WIRE IN RAISES HIT'
             TO TST-CASE-NAME
           MOVE 1 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ

           PERFORM 8000-RESET
           MOVE 20240601 TO AMP-ACCT-DORMANT-DATE(1)
           MOVE 'DWIR' TO WS-ADD-CODE
           MOVE 'C' TO WS-ADD-DR-CR
           MOVE 5000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML5 EXACTLY 5,000 IS NOT MORE THAN 5,000 - NO HIT'
             TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ

      *    REACTIVATED 45 DAYS AGO - OUTSIDE THE 30 DAY WINDOW
           PERFORM 8000-RESET
           MOVE 20240601 TO AMP-ACCT-DORMANT-DATE(1)
           MOVE 'DCHK' TO WS-ADD-CODE
           MOVE 'C' TO WS-ADD-DR-CR
           MOVE 100.00 TO WS-ADD-AMOUNT
           MOVE 20260816 TO WS-ADD-DATE
           PERFORM 8100-ADD
           MOVE 9000.00 TO WS-ADD-AMOUNT
           MOVE 20260930 TO WS-ADD-DATE
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML5 REACTIVATION OVER 30 DAYS AGO - NO HIT'
             TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ.

      *----------------------------------------------------------------*
      * VASQUEZ-ORTEGA 120,000 WIRE OUT TO NEW BENEFICIARY             *
      *----------------------------------------------------------------*
       1500-TEST-AML6-ROUND-WIRE.
           PERFORM 8000-RESET
           MOVE 'PW' TO AMP-SEGMENT
           MOVE 850000.00 TO AMP-ANNUAL-INCOME
           MOVE 'WWIR' TO WS-ADD-CODE
           MOVE 'D' TO WS-ADD-DR-CR
           MOVE 'WIR' TO WS-ADD-CHANNEL
           MOVE 'CH930076201162385' TO WS-ADD-CP-ACCT
           MOVE 120000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           MOVE 'AML6' TO WS-WANT-SCEN
           PERFORM 8200-FIND
           MOVE 'AML6 ROUND 120,000 WIRE TO NEW BENEFICIARY'
             TO TST-CASE-NAME
           MOVE 1 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ

      *    SAME BENEFICIARY PAID LAST WEEK - NOT NEW
           PERFORM 8000-RESET
           MOVE 'PW' TO AMP-SEGMENT
           MOVE 'WWIR' TO WS-ADD-CODE
           MOVE 'D' TO WS-ADD-DR-CR
           MOVE 'CH930076201162385' TO WS-ADD-CP-ACCT
           MOVE 20000.00 TO WS-ADD-AMOUNT
           MOVE 20260922 TO WS-ADD-DATE
           PERFORM 8100-ADD
           MOVE 120000.00 TO WS-ADD-AMOUNT
           MOVE 20260930 TO WS-ADD-DATE
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML6 KNOWN BENEFICIARY - NO HIT' TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ

           PERFORM 8000-RESET
           MOVE 'PW' TO AMP-SEGMENT
           MOVE 'WWIR' TO WS-ADD-CODE
           MOVE 'D' TO WS-ADD-DR-CR
           MOVE 'GB29NWBK601613319' TO WS-ADD-CP-ACCT
           MOVE 120500.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML6 120,500 IS NOT A WHOLE-THOUSAND AMOUNT'
             TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ.

      *----------------------------------------------------------------*
      * DELACROIX INCOME 62,000 -> MONTHLY 5,166.67 X 2 = 10,333.33    *
      *----------------------------------------------------------------*
       1600-TEST-AML7-PROFILE.
           PERFORM 8000-RESET
           MOVE 'RT' TO AMP-SEGMENT
           MOVE 62000.00 TO AMP-ANNUAL-INCOME
           MOVE 'DCSH' TO WS-ADD-CODE
           MOVE 'C' TO WS-ADD-DR-CR
           MOVE 20260922 TO WS-ADD-DATE
           MOVE 9500.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           MOVE 20260930 TO WS-ADD-DATE
           MOVE 1000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           MOVE 'AML7' TO WS-WANT-SCEN
           PERFORM 8200-FIND
           MOVE 'AML7 CREDITS 10,500 > 2 X MONTHLY INCOME 10,333.33'
             TO TST-CASE-NAME
           MOVE 1 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ

           MOVE 'CB' TO AMP-SEGMENT
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML7 NOT APPLICABLE TO CORPORATE SEGMENT'
             TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ

           PERFORM 8000-RESET
           MOVE 'RT' TO AMP-SEGMENT
           MOVE 62000.00 TO AMP-ANNUAL-INCOME
           MOVE 'DCSH' TO WS-ADD-CODE
           MOVE 'C' TO WS-ADD-DR-CR
           MOVE 20260929 TO WS-ADD-DATE
           MOVE 12000.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           MOVE 20260930 TO WS-ADD-DATE
           MOVE 'WPOS' TO WS-ADD-CODE
           MOVE 'D' TO WS-ADD-DR-CR
           MOVE 25.00 TO WS-ADD-AMOUNT
           PERFORM 8100-ADD
           CALL 'AMLRULES' USING AML-RULE-PARMS
           PERFORM 8200-FIND
           MOVE 'AML7 NOT RAISED WITHOUT A CREDIT TODAY'
             TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           MOVE WS-FOUND-COUNT TO TST-ACT-NUM
           PERFORM 9000-NEQ.

      *----------------------------------------------------------------*
       1700-TEST-INVALID.
           PERFORM 8000-RESET
           MOVE 20260230 TO AMP-BUSINESS-DATE
           CALL 'AMLRULES' USING AML-RULE-PARMS
           MOVE 'INVALID BUSINESS DATE RETURNS RC 12' TO TST-CASE-NAME
           MOVE 12 TO TST-EXP-NUM
           MOVE AMP-RETURN-CODE TO TST-ACT-NUM
           PERFORM 9000-NEQ

           PERFORM 8000-RESET
           CALL 'AMLRULES' USING AML-RULE-PARMS
           MOVE 'NO ACTIVITY RETURNS RC 00 AND NO HITS' TO TST-CASE-NAME
           MOVE 0 TO TST-EXP-NUM
           COMPUTE TST-ACT-NUM = AMP-RETURN-CODE + AMP-HIT-COUNT
           PERFORM 9000-NEQ.

      *----------------------------------------------------------------*
       8000-RESET.
           INITIALIZE AML-RULE-PARMS
           MOVE 20260930     TO AMP-BUSINESS-DATE
           MOVE 'C000000099' TO AMP-CUST-ID
           MOVE 'I'  TO AMP-CUST-TYPE
           MOVE 'RT' TO AMP-SEGMENT
           MOVE 'N'  TO AMP-PEP-FLAG AMP-MSB-FLAG
           MOVE 'L'  TO AMP-RISK-RATING
           MOVE 1 TO AMP-ACCT-COUNT
           MOVE '100000000025' TO AMP-ACCT-NUMBER(1)
           MOVE 'A' TO AMP-ACCT-STATUS(1)
           MOVE 20260915 TO AMP-ACCT-OPEN-DATE(1)
           MOVE 0 TO AMP-ACCT-DORMANT-DATE(1)
           MOVE 20260930     TO WS-ADD-DATE
           MOVE '100000000025' TO WS-ADD-ACCT
           MOVE 'BRN' TO WS-ADD-CHANNEL
           MOVE SPACES TO WS-ADD-CP-ACCT WS-ADD-CP-COUNTRY.

      *    LARGE INCOME SO AML7 DOES NOT INTERFERE
       8050-SET-RETAIL-LOW-ACTIVITY.
           MOVE 9000000.00 TO AMP-ANNUAL-INCOME.

       8060-SET-MSB-HISTORY.
           MOVE 'C' TO AMP-CUST-TYPE
           MOVE 'CB' TO AMP-SEGMENT
           MOVE 'Y' TO AMP-MSB-FLAG
           MOVE '200000000010' TO WS-ADD-ACCT AMP-ACCT-NUMBER(1)
           MOVE 'DCSH' TO WS-ADD-CODE
           MOVE 'C' TO WS-ADD-DR-CR
           MOVE 20000.00 TO WS-ADD-AMOUNT
           MOVE 20260914 TO WS-ADD-DATE
           PERFORM 10 TIMES
               PERFORM 8100-ADD
               ADD 1 TO WS-ADD-DATE
           END-PERFORM.

       8100-ADD.
           ADD 1 TO AMP-ITEM-COUNT
           MOVE WS-ADD-DATE    TO AMP-ITEM-DATE(AMP-ITEM-COUNT)
           MOVE WS-ADD-ACCT    TO AMP-ITEM-ACCT(AMP-ITEM-COUNT)
           MOVE WS-ADD-CODE    TO AMP-ITEM-CODE(AMP-ITEM-COUNT)
           MOVE WS-ADD-DR-CR   TO AMP-ITEM-DR-CR(AMP-ITEM-COUNT)
           MOVE WS-ADD-AMOUNT  TO AMP-ITEM-AMOUNT(AMP-ITEM-COUNT)
           MOVE WS-ADD-CHANNEL TO AMP-ITEM-CHANNEL(AMP-ITEM-COUNT)
           MOVE WS-ADD-CP-ACCT TO AMP-ITEM-CP-ACCT(AMP-ITEM-COUNT)
           MOVE WS-ADD-CP-COUNTRY
             TO AMP-ITEM-CP-COUNTRY(AMP-ITEM-COUNT).

       8200-FIND.
           MOVE 0 TO WS-FOUND-COUNT
           MOVE SPACE TO WS-FOUND-SEV
           PERFORM VARYING WS-HX FROM 1 BY 1
                   UNTIL WS-HX > AMP-HIT-COUNT
               IF AMP-HIT-SCENARIO(WS-HX) = WS-WANT-SCEN
                   ADD 1 TO WS-FOUND-COUNT
                   IF WS-FOUND-COUNT = 1
                       MOVE AMP-HIT-SEVERITY(WS-HX) TO WS-FOUND-SEV
                       MOVE AMP-HIT-AMOUNT(WS-HX) TO WS-FOUND-AMT
                   END-IF
               END-IF
           END-PERFORM.

       9000-NEQ.
           MOVE 'NEQ' TO TST-FUNCTION
           CALL 'TESTLIB' USING TEST-PARMS.

       9100-AEQ.
           MOVE 'AEQ' TO TST-FUNCTION
           CALL 'TESTLIB' USING TEST-PARMS
           MOVE SPACES TO TST-EXPECTED TST-ACTUAL.
