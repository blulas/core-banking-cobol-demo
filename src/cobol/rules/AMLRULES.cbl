      *================================================================*
      * PROGRAM  : AMLRULES                                            *
      * TYPE     : CALLED BUSINESS RULE MODULE                         *
      * PURPOSE  : AML TRANSACTION MONITORING SCENARIO ENGINE. RUNS    *
      *            EVERY ACTIVE SCENARIO IN TABLE AMLSCEN AGAINST ONE  *
      *            CUSTOMER'S POSTED ACTIVITY AND RETURNS ONE HIT PER  *
      *            SCENARIO OCCURRENCE (SCENARIO, SEVERITY, ACCOUNT,   *
      *            AMOUNT, DESCRIPTION).                               *
      *              AML1 RAPID MOVEMENT OF FUNDS                      *
      *              AML2 HIGH-RISK GEOGRAPHY WIRE                     *
      *              AML3 PEP LARGE TRANSACTION                        *
      *              AML4 MSB CUSTOMER VOLUME SPIKE                    *
      *              AML5 DORMANT ACCOUNT SUDDEN ACTIVITY              *
      *              AML6 ROUND-AMOUNT WIRE TO NEW BENEFICIARY         *
      *              AML7 ACTIVITY INCONSISTENT WITH STATED INCOME     *
      *            ONLY CUSTOMER-DRIVEN MONETARY CODES ARE MONITORED - *
      *            SWEEPS, INTEREST, FEES, ADJUSTMENTS AND CD          *
      *            MATURITIES ARE IGNORED.                             *
      * LINKAGE  : AMLPARM                                             *
      * TABLES   : AMLSCEN                                             *
      * CALLED BY: AMLMON (BATCH)                                      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2004-08-02 BHS  ORIGINAL - AML1 TO AML3                        *
      * 2008-02-25 BHS  AML2 USES FATF LIST IN AMLSCEN                 *
      * 2011-01-18 DPS  AML4 MSB SPIKE, AML5 DORMANT REACTIVATION      *
      * 2012-07-09 DPS  INC22710 SWEEPS RAISING AML1 ON ZBA ACCOUNTS - *
      *                 MONITORED CODE LIST ADDED                      *
      * 2015-05-27 DPS  AML6 ROUND WIRE NEW BENEFICIARY                *
      * 2018-05-11 AKW  CDD RULE - AML7 PROFILE MISMATCH               *
      * 2020-11-30 AKW  INC39904 AML1 RE-ALERTED DAILY - NOW RAISED    *
      *                 ONLY ON THE DAY THE PATTERN COMPLETES          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AMLRULES.
       AUTHOR. B H SANDERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY AMLSCEN.

      *----------------------------------------------------------------*
      * MONITORED TRANSACTION CODES (INC22710)                         *
      *----------------------------------------------------------------*
       01  WS-MONITORED-CODE-VALUES.
           05  FILLER PIC X(48) VALUE
               'DCSHDCHKDACHDWIRDXFRWCSHWATMWCHKWACHWWIRWXFRWPOS'.
       01  WS-MONITORED-CODE-TABLE REDEFINES WS-MONITORED-CODE-VALUES.
           05  WS-MON-CODE OCCURS 12 TIMES INDEXED BY MON-IDX
                                           PIC X(04).

       01  WS-SCENARIO-WORK.
           05  WS-SCEN-SUB                 PIC 9(02) COMP.
           05  WS-SCEN-WANTED              PIC X(04).
           05  WS-SCEN-APPLIES             PIC X(01).

       01  WS-WORK-FIELDS.
           05  WS-IX                       PIC 9(03) COMP.
           05  WS-JX                       PIC 9(03) COMP.
           05  WS-AX                       PIC 9(03) COMP.
           05  WS-KX                       PIC 9(02) COMP.
           05  WS-FOUND                    PIC X(01).
           05  WS-MONITORED                PIC X(01).
           05  WS-HAS-TODAY                PIC X(01).
           05  WS-WINDOW-START             PIC 9(08).
           05  WS-WINDOW-END               PIC 9(08).
           05  WS-REACT-DATE               PIC 9(08).
           05  WS-TOTAL-ALL                PIC S9(13)V99 COMP-3.
           05  WS-TOTAL-PRIOR              PIC S9(13)V99 COMP-3.
           05  WS-TOTAL-TODAY              PIC S9(13)V99 COMP-3.
           05  WS-LIMIT                    PIC S9(15)V99 COMP-3.
           05  WS-AVG-DAILY                PIC S9(13)V99 COMP-3.
           05  WS-BUS-DAYS                 PIC 9(03) COMP.
           05  WS-TEST-DATE                PIC 9(08).
           05  WS-CENTS                    PIC 9(15).
           05  WS-ROUND-CENTS              PIC 9(15).
           05  WS-AMT-ED                   PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-AMT-ED2                  PIC ZZZ,ZZZ,ZZ9.99.

       01  WS-NEW-HIT.
           05  WS-HIT-SCENARIO             PIC X(04).
           05  WS-HIT-RULE-ID              PIC X(08).
           05  WS-HIT-SEVERITY             PIC X(01).
           05  WS-HIT-ACCT                 PIC X(12).
           05  WS-HIT-AMOUNT               PIC S9(13)V99 COMP-3.
           05  WS-HIT-DESCRIPTION          PIC X(80).

       COPY DATEPARM.

       LINKAGE SECTION.
       COPY AMLPARM.

       PROCEDURE DIVISION USING AML-RULE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           IF AMP-RETURN-CODE = 00
               PERFORM 2100-AML1-RAPID-MOVEMENT
               PERFORM 2200-AML2-HIGH-RISK-GEO
               PERFORM 2300-AML3-PEP-LARGE
               PERFORM 2400-AML4-MSB-SPIKE
               PERFORM 2500-AML5-DORMANT-REACT
               PERFORM 2600-AML6-ROUND-WIRE
               PERFORM 2700-AML7-PROFILE
               IF AMP-HIT-COUNT > 0
                   MOVE 04 TO AMP-RETURN-CODE
               END-IF
           END-IF
           GOBACK.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           MOVE 00 TO AMP-RETURN-CODE
           MOVE 0  TO AMP-HIT-COUNT
           IF AMP-ITEM-COUNT > 250 OR AMP-ACCT-COUNT > 20
               MOVE 12 TO AMP-RETURN-CODE
           ELSE
               INITIALIZE DATE-PARMS
               SET DTU-VALIDATE TO TRUE
               MOVE AMP-BUSINESS-DATE TO DTU-DATE-1
               CALL 'DATEUTIL' USING DATE-PARMS
               IF NOT DTU-RESULT-YES
                   MOVE 12 TO AMP-RETURN-CODE
               END-IF
           END-IF.

      *================================================================*
      * AML1 - RAPID MOVEMENT. A CREDIT OF $25,000 OR MORE FOLLOWED,   *
      * WITHIN 3 CALENDAR DAYS, BY DEBITS FROM THE SAME ACCOUNT OF 90% *
      * OR MORE OF THE CREDIT. RAISED ON THE DAY THE 90% IS REACHED.   *
      *================================================================*
       2100-AML1-RAPID-MOVEMENT.
           MOVE 'AML1' TO WS-SCEN-WANTED
           PERFORM 8100-LOCATE-SCENARIO
           IF WS-SCEN-APPLIES = 'Y'
               PERFORM VARYING WS-IX FROM 1 BY 1
                       UNTIL WS-IX > AMP-ITEM-COUNT
                   MOVE WS-IX TO WS-AX
                   PERFORM 8300-CHECK-MONITORED
                   IF AMP-ITEM-DR-CR(WS-IX) = 'C'
                      AND WS-MONITORED = 'Y'
                      AND AMP-ITEM-AMOUNT(WS-IX)
                          >= SCEN-AMOUNT(WS-SCEN-SUB)
                       PERFORM 2110-AML1-CHECK-CREDIT
                   END-IF
               END-PERFORM
           END-IF.

       2110-AML1-CHECK-CREDIT.
           INITIALIZE DATE-PARMS
           SET DTU-ADD-DAYS TO TRUE
           MOVE AMP-ITEM-DATE(WS-IX) TO DTU-DATE-1
           MOVE SCEN-LOOKBACK-DAYS(WS-SCEN-SUB) TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-WINDOW-END
           IF WS-WINDOW-END < AMP-BUSINESS-DATE
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-TOTAL-ALL WS-TOTAL-PRIOR
           PERFORM VARYING WS-JX FROM 1 BY 1
                   UNTIL WS-JX > AMP-ITEM-COUNT
               MOVE WS-JX TO WS-AX
               PERFORM 8300-CHECK-MONITORED
               IF AMP-ITEM-DR-CR(WS-JX) = 'D'
                  AND WS-MONITORED = 'Y'
                  AND AMP-ITEM-ACCT(WS-JX) = AMP-ITEM-ACCT(WS-IX)
                  AND AMP-ITEM-DATE(WS-JX) >= AMP-ITEM-DATE(WS-IX)
                  AND AMP-ITEM-DATE(WS-JX) <= WS-WINDOW-END
                   ADD AMP-ITEM-AMOUNT(WS-JX) TO WS-TOTAL-ALL
                   IF AMP-ITEM-DATE(WS-JX) < AMP-BUSINESS-DATE
                       ADD AMP-ITEM-AMOUNT(WS-JX) TO WS-TOTAL-PRIOR
                   END-IF
               END-IF
           END-PERFORM
           COMPUTE WS-LIMIT = AMP-ITEM-AMOUNT(WS-IX)
                            * SCEN-PCT(WS-SCEN-SUB) / 100
           IF WS-TOTAL-ALL >= WS-LIMIT
              AND WS-TOTAL-PRIOR < WS-LIMIT
               PERFORM 8200-INIT-HIT
               MOVE AMP-ITEM-ACCT(WS-IX) TO WS-HIT-ACCT
               MOVE WS-TOTAL-ALL         TO WS-HIT-AMOUNT
               MOVE AMP-ITEM-AMOUNT(WS-IX) TO WS-AMT-ED
               MOVE WS-TOTAL-ALL           TO WS-AMT-ED2
               STRING 'RAPID MOVEMENT: CREDIT ' WS-AMT-ED
                      ' ON ' AMP-ITEM-DATE(WS-IX)
                      ' OUT ' WS-AMT-ED2
                      DELIMITED BY SIZE INTO WS-HIT-DESCRIPTION
               END-STRING
               PERFORM 8000-ADD-HIT
           END-IF.

      *================================================================*
      * AML2 - WIRE OF $3,000 OR MORE TO OR FROM A HIGH-RISK COUNTRY.  *
      * FATF CALL-FOR-ACTION / SANCTIONED = HIGH, GREY LIST = MEDIUM.  *
      *================================================================*
       2200-AML2-HIGH-RISK-GEO.
           MOVE 'AML2' TO WS-SCEN-WANTED
           PERFORM 8100-LOCATE-SCENARIO
           IF WS-SCEN-APPLIES = 'Y'
               PERFORM VARYING WS-IX FROM 1 BY 1
                       UNTIL WS-IX > AMP-ITEM-COUNT
                   IF AMP-ITEM-DATE(WS-IX) = AMP-BUSINESS-DATE
                      AND (AMP-ITEM-CHANNEL(WS-IX) = 'WIR'
                           OR AMP-ITEM-CODE(WS-IX) = 'DWIR'
                           OR AMP-ITEM-CODE(WS-IX) = 'WWIR')
                      AND AMP-ITEM-AMOUNT(WS-IX)
                          >= SCEN-AMOUNT(WS-SCEN-SUB)
                       SET HRC-IDX TO 1
                       SEARCH HRC-ENTRY
                           AT END
                               CONTINUE
                           WHEN HRC-COUNTRY(HRC-IDX)
                                = AMP-ITEM-CP-COUNTRY(WS-IX)
                               PERFORM 2210-AML2-RAISE
                       END-SEARCH
                   END-IF
               END-PERFORM
           END-IF.

       2210-AML2-RAISE.
           PERFORM 8200-INIT-HIT
           IF HRC-GREY-LIST(HRC-IDX)
               MOVE 'M' TO WS-HIT-SEVERITY
           END-IF
           MOVE AMP-ITEM-ACCT(WS-IX)   TO WS-HIT-ACCT
           MOVE AMP-ITEM-AMOUNT(WS-IX) TO WS-HIT-AMOUNT
           MOVE AMP-ITEM-AMOUNT(WS-IX) TO WS-AMT-ED
           STRING 'HIGH-RISK GEOGRAPHY WIRE ' AMP-ITEM-CODE(WS-IX)
                  ' COUNTRY ' AMP-ITEM-CP-COUNTRY(WS-IX)
                  ' LIST ' HRC-LEVEL(HRC-IDX)
                  ' AMT ' WS-AMT-ED
                  DELIMITED BY SIZE INTO WS-HIT-DESCRIPTION
           END-STRING
           PERFORM 8000-ADD-HIT.

      *================================================================*
      * AML3 - POLITICALLY EXPOSED PERSON, SINGLE ITEM OF $50,000 OR   *
      * MORE TODAY.                                                    *
      *================================================================*
       2300-AML3-PEP-LARGE.
           MOVE 'AML3' TO WS-SCEN-WANTED
           PERFORM 8100-LOCATE-SCENARIO
           IF WS-SCEN-APPLIES = 'Y' AND AMP-PEP-FLAG = 'Y'
               PERFORM VARYING WS-IX FROM 1 BY 1
                       UNTIL WS-IX > AMP-ITEM-COUNT
                   MOVE WS-IX TO WS-AX
                   PERFORM 8300-CHECK-MONITORED
                   IF AMP-ITEM-DATE(WS-IX) = AMP-BUSINESS-DATE
                      AND WS-MONITORED = 'Y'
                      AND AMP-ITEM-AMOUNT(WS-IX)
                          >= SCEN-AMOUNT(WS-SCEN-SUB)
                       PERFORM 8200-INIT-HIT
                       MOVE AMP-ITEM-ACCT(WS-IX)   TO WS-HIT-ACCT
                       MOVE AMP-ITEM-AMOUNT(WS-IX) TO WS-HIT-AMOUNT
                       MOVE AMP-ITEM-AMOUNT(WS-IX) TO WS-AMT-ED
                       STRING 'PEP LARGE TRANSACTION '
                              AMP-ITEM-CODE(WS-IX) ' '
                              AMP-ITEM-DR-CR(WS-IX)
                              ' AMT ' WS-AMT-ED
                              DELIMITED BY SIZE INTO WS-HIT-DESCRIPTION
                       END-STRING
                       PERFORM 8000-ADD-HIT
                   END-IF
               END-PERFORM
           END-IF.

      *================================================================*
      * AML4 - MSB VOLUME SPIKE. TODAY'S TOTAL VOLUME (IN + OUT) IS    *
      * MORE THAN 3 TIMES THE AVERAGE DAILY VOLUME OF THE PRIOR 30     *
      * CALENDAR DAYS (AVERAGED OVER BUSINESS DAYS IN THAT WINDOW).    *
      *================================================================*
       2400-AML4-MSB-SPIKE.
           MOVE 'AML4' TO WS-SCEN-WANTED
           PERFORM 8100-LOCATE-SCENARIO
           IF WS-SCEN-APPLIES = 'Y' AND AMP-MSB-FLAG = 'Y'
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE AMP-BUSINESS-DATE TO DTU-DATE-1
               COMPUTE DTU-NUMBER = 0 - SCEN-LOOKBACK-DAYS(WS-SCEN-SUB)
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-WINDOW-START
               MOVE ZERO TO WS-TOTAL-TODAY WS-TOTAL-PRIOR
               PERFORM VARYING WS-IX FROM 1 BY 1
                       UNTIL WS-IX > AMP-ITEM-COUNT
                   MOVE WS-IX TO WS-AX
                   PERFORM 8300-CHECK-MONITORED
                   IF WS-MONITORED = 'Y'
                       EVALUATE TRUE
                           WHEN AMP-ITEM-DATE(WS-IX) = AMP-BUSINESS-DATE
                               ADD AMP-ITEM-AMOUNT(WS-IX)
                                 TO WS-TOTAL-TODAY
                           WHEN AMP-ITEM-DATE(WS-IX) >= WS-WINDOW-START
                               ADD AMP-ITEM-AMOUNT(WS-IX)
                                 TO WS-TOTAL-PRIOR
                       END-EVALUATE
                   END-IF
               END-PERFORM
               PERFORM 2410-COUNT-BUSINESS-DAYS
               IF WS-BUS-DAYS > 0 AND WS-TOTAL-PRIOR > ZERO
                   COMPUTE WS-AVG-DAILY ROUNDED =
                           WS-TOTAL-PRIOR / WS-BUS-DAYS
                   COMPUTE WS-LIMIT = WS-AVG-DAILY
                                    * SCEN-PCT(WS-SCEN-SUB) / 100
                   IF WS-TOTAL-TODAY > WS-LIMIT
                       PERFORM 8200-INIT-HIT
                       MOVE AMP-ACCT-NUMBER(1) TO WS-HIT-ACCT
                       MOVE WS-TOTAL-TODAY     TO WS-HIT-AMOUNT
                       MOVE WS-TOTAL-TODAY     TO WS-AMT-ED
                       MOVE WS-AVG-DAILY       TO WS-AMT-ED2
                       STRING 'MSB VOLUME SPIKE TODAY ' WS-AMT-ED
                              ' VS 30D AVG ' WS-AMT-ED2
                              DELIMITED BY SIZE INTO WS-HIT-DESCRIPTION
                       END-STRING
                       PERFORM 8000-ADD-HIT
                   END-IF
               END-IF
           END-IF.

       2410-COUNT-BUSINESS-DAYS.
           MOVE 0 TO WS-BUS-DAYS
           MOVE WS-WINDOW-START TO WS-TEST-DATE
           PERFORM UNTIL WS-TEST-DATE >= AMP-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-IS-BUS-DAY TO TRUE
               MOVE WS-TEST-DATE TO DTU-DATE-1
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-RESULT-YES
                   ADD 1 TO WS-BUS-DAYS
               END-IF
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE WS-TEST-DATE TO DTU-DATE-1
               MOVE 1 TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-TEST-DATE
           END-PERFORM.

      *================================================================*
      * AML5 - DORMANT REACTIVATION. AN ACCOUNT THAT HAS BEEN DORMANT  *
      * (DORMANT DATE SET) WITH MORE THAN $5,000 OF ACTIVITY WITHIN 30 *
      * DAYS OF ITS FIRST CUSTOMER ACTIVITY AFTER DORMANCY. RAISED     *
      * ONLY WHEN THE ACCOUNT HAS ACTIVITY TODAY.                      *
      *================================================================*
       2500-AML5-DORMANT-REACT.
           MOVE 'AML5' TO WS-SCEN-WANTED
           PERFORM 8100-LOCATE-SCENARIO
           IF WS-SCEN-APPLIES = 'Y'
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE AMP-BUSINESS-DATE TO DTU-DATE-1
               COMPUTE DTU-NUMBER = 0 - SCEN-LOOKBACK-DAYS(WS-SCEN-SUB)
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-WINDOW-START
               PERFORM VARYING WS-KX FROM 1 BY 1
                       UNTIL WS-KX > AMP-ACCT-COUNT
                   IF AMP-ACCT-DORMANT-DATE(WS-KX) > 0
                       PERFORM 2510-AML5-CHECK-ACCOUNT
                   END-IF
               END-PERFORM
           END-IF.

       2510-AML5-CHECK-ACCOUNT.
           MOVE 99999999 TO WS-REACT-DATE
           MOVE ZERO     TO WS-TOTAL-ALL
           MOVE 'N'      TO WS-HAS-TODAY
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > AMP-ITEM-COUNT
               MOVE WS-IX TO WS-AX
               PERFORM 8300-CHECK-MONITORED
               IF AMP-ITEM-ACCT(WS-IX) = AMP-ACCT-NUMBER(WS-KX)
                  AND WS-MONITORED = 'Y'
                  AND AMP-ITEM-DATE(WS-IX)
                      > AMP-ACCT-DORMANT-DATE(WS-KX)
                   IF AMP-ITEM-DATE(WS-IX) < WS-REACT-DATE
                       MOVE AMP-ITEM-DATE(WS-IX) TO WS-REACT-DATE
                   END-IF
                   ADD AMP-ITEM-AMOUNT(WS-IX) TO WS-TOTAL-ALL
                   IF AMP-ITEM-DATE(WS-IX) = AMP-BUSINESS-DATE
                       MOVE 'Y' TO WS-HAS-TODAY
                   END-IF
               END-IF
           END-PERFORM
           IF WS-HAS-TODAY = 'Y'
              AND WS-REACT-DATE >= WS-WINDOW-START
              AND WS-TOTAL-ALL > SCEN-AMOUNT(WS-SCEN-SUB)
               PERFORM 8200-INIT-HIT
               MOVE AMP-ACCT-NUMBER(WS-KX) TO WS-HIT-ACCT
               MOVE WS-TOTAL-ALL           TO WS-HIT-AMOUNT
               MOVE WS-TOTAL-ALL           TO WS-AMT-ED
               STRING 'DORMANT SINCE ' AMP-ACCT-DORMANT-DATE(WS-KX)
                      ' REACTIVATED ' WS-REACT-DATE
                      ' ACTIVITY ' WS-AMT-ED
                      DELIMITED BY SIZE INTO WS-HIT-DESCRIPTION
               END-STRING
               PERFORM 8000-ADD-HIT
           END-IF.

      *================================================================*
      * AML6 - OUTGOING WIRE OF $100,000 OR MORE IN WHOLE THOUSANDS TO *
      * A BENEFICIARY ACCOUNT NOT SEEN FOR THIS CUSTOMER BEFORE.       *
      *================================================================*
       2600-AML6-ROUND-WIRE.
           MOVE 'AML6' TO WS-SCEN-WANTED
           PERFORM 8100-LOCATE-SCENARIO
           IF WS-SCEN-APPLIES = 'Y'
               COMPUTE WS-ROUND-CENTS = AML-ROUND-UNIT * 100
               PERFORM VARYING WS-IX FROM 1 BY 1
                       UNTIL WS-IX > AMP-ITEM-COUNT
                   IF AMP-ITEM-DATE(WS-IX) = AMP-BUSINESS-DATE
                      AND AMP-ITEM-CODE(WS-IX) = 'WWIR'
                      AND AMP-ITEM-AMOUNT(WS-IX)
                          >= SCEN-AMOUNT(WS-SCEN-SUB)
                       COMPUTE WS-CENTS = AMP-ITEM-AMOUNT(WS-IX) * 100
                       IF FUNCTION MOD(WS-CENTS, WS-ROUND-CENTS) = 0
                           PERFORM 2610-AML6-NEW-BENEFICIARY
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

       2610-AML6-NEW-BENEFICIARY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-JX FROM 1 BY 1
                   UNTIL WS-JX > AMP-ITEM-COUNT OR WS-FOUND = 'Y'
               IF WS-JX NOT = WS-IX
                  AND AMP-ITEM-CP-ACCT(WS-JX) = AMP-ITEM-CP-ACCT(WS-IX)
                  AND AMP-ITEM-DATE(WS-JX) < AMP-BUSINESS-DATE
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N'
               PERFORM 8200-INIT-HIT
               MOVE AMP-ITEM-ACCT(WS-IX)   TO WS-HIT-ACCT
               MOVE AMP-ITEM-AMOUNT(WS-IX) TO WS-HIT-AMOUNT
               MOVE AMP-ITEM-AMOUNT(WS-IX) TO WS-AMT-ED
               STRING 'ROUND WIRE ' WS-AMT-ED
                      ' TO NEW BENEFICIARY '
                      AMP-ITEM-CP-ACCT(WS-IX)
                      DELIMITED BY SIZE INTO WS-HIT-DESCRIPTION
               END-STRING
               PERFORM 8000-ADD-HIT
           END-IF.

      *================================================================*
      * AML7 - CREDITS IN THE LAST 30 DAYS (INCLUDING TODAY) MORE THAN *
      * 2 X STATED MONTHLY INCOME (ANNUAL INCOME / 12). INDIVIDUAL     *
      * SEGMENTS ONLY. RAISED WHEN THE CUSTOMER HAS A CREDIT TODAY.    *
      *================================================================*
       2700-AML7-PROFILE.
           MOVE 'AML7' TO WS-SCEN-WANTED
           PERFORM 8100-LOCATE-SCENARIO
           IF WS-SCEN-APPLIES = 'Y' AND AMP-ANNUAL-INCOME > ZERO
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE AMP-BUSINESS-DATE TO DTU-DATE-1
               COMPUTE DTU-NUMBER = 1 - SCEN-LOOKBACK-DAYS(WS-SCEN-SUB)
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-WINDOW-START
               MOVE ZERO TO WS-TOTAL-ALL
               MOVE 'N'  TO WS-HAS-TODAY
               PERFORM VARYING WS-IX FROM 1 BY 1
                       UNTIL WS-IX > AMP-ITEM-COUNT
                   MOVE WS-IX TO WS-AX
                   PERFORM 8300-CHECK-MONITORED
                   IF AMP-ITEM-DR-CR(WS-IX) = 'C'
                      AND WS-MONITORED = 'Y'
                      AND AMP-ITEM-DATE(WS-IX) >= WS-WINDOW-START
                       ADD AMP-ITEM-AMOUNT(WS-IX) TO WS-TOTAL-ALL
                       IF AMP-ITEM-DATE(WS-IX) = AMP-BUSINESS-DATE
                           MOVE 'Y' TO WS-HAS-TODAY
                       END-IF
                   END-IF
               END-PERFORM
               COMPUTE WS-LIMIT ROUNDED = AMP-ANNUAL-INCOME / 12
                                * SCEN-PCT(WS-SCEN-SUB) / 100
               IF WS-HAS-TODAY = 'Y' AND WS-TOTAL-ALL > WS-LIMIT
                   PERFORM 8200-INIT-HIT
                   MOVE AMP-ACCT-NUMBER(1) TO WS-HIT-ACCT
                   MOVE WS-TOTAL-ALL       TO WS-HIT-AMOUNT
                   MOVE WS-TOTAL-ALL       TO WS-AMT-ED
                   MOVE WS-LIMIT           TO WS-AMT-ED2
                   STRING 'CREDITS 30D ' WS-AMT-ED
                          ' EXCEED PROFILE LIMIT ' WS-AMT-ED2
                          DELIMITED BY SIZE INTO WS-HIT-DESCRIPTION
                   END-STRING
                   PERFORM 8000-ADD-HIT
               END-IF
           END-IF.

      *================================================================*
      * COMMON ROUTINES                                                *
      *================================================================*
       8000-ADD-HIT.
           IF AMP-HIT-COUNT < 20
               ADD 1 TO AMP-HIT-COUNT
               MOVE WS-NEW-HIT TO AMP-HIT(AMP-HIT-COUNT)
           END-IF.

      *----------------------------------------------------------------*
      * FIND WS-SCEN-WANTED IN AMLSCEN; WS-SCEN-APPLIES = 'Y' IF IT IS *
      * ACTIVE AND THE CUSTOMER SEGMENT IS IN ITS SEGMENT LIST.        *
      *----------------------------------------------------------------*
       8100-LOCATE-SCENARIO.
           MOVE 'N' TO WS-SCEN-APPLIES
           MOVE 0   TO WS-SCEN-SUB
           PERFORM VARYING WS-KX FROM 1 BY 1
                   UNTIL WS-KX > AML-SCENARIO-COUNT
               IF SCEN-ID(WS-KX) = WS-SCEN-WANTED
                   MOVE WS-KX TO WS-SCEN-SUB
               END-IF
           END-PERFORM
           IF WS-SCEN-SUB > 0
               IF SCEN-ACTIVE(WS-SCEN-SUB) = 'Y'
                   PERFORM VARYING WS-KX FROM 1 BY 1 UNTIL WS-KX > 5
                       IF SCEN-SEG(WS-SCEN-SUB, WS-KX) = '**'
                          OR SCEN-SEG(WS-SCEN-SUB, WS-KX) = AMP-SEGMENT
                           MOVE 'Y' TO WS-SCEN-APPLIES
                       END-IF
                   END-PERFORM
               END-IF
           END-IF.

       8200-INIT-HIT.
           MOVE SCEN-ID(WS-SCEN-SUB)       TO WS-HIT-SCENARIO
           MOVE SCEN-RULE-ID(WS-SCEN-SUB)  TO WS-HIT-RULE-ID
           MOVE SCEN-SEVERITY(WS-SCEN-SUB) TO WS-HIT-SEVERITY
           MOVE SPACES TO WS-HIT-ACCT WS-HIT-DESCRIPTION
           MOVE ZERO   TO WS-HIT-AMOUNT.

      *----------------------------------------------------------------*
      * WS-MONITORED = 'Y' IF ITEM WS-AX HAS A MONITORED TRAN CODE     *
      *----------------------------------------------------------------*
       8300-CHECK-MONITORED.
           MOVE 'N' TO WS-MONITORED
           SET MON-IDX TO 1
           SEARCH WS-MON-CODE
               AT END
                   CONTINUE
               WHEN WS-MON-CODE(MON-IDX) = AMP-ITEM-CODE(WS-AX)
                   MOVE 'Y' TO WS-MONITORED
           END-SEARCH.
