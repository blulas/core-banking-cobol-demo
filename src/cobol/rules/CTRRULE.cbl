      *================================================================*
      * PROGRAM  : CTRRULE                                             *
      * TYPE     : CALLED BUSINESS RULE MODULE                         *
      * PURPOSE  : CURRENCY TRANSACTION REPORT (CTR) DETERMINATION.    *
      *            31 CFR 1010.311 / 1010.313 - A CTR IS REQUIRED FOR  *
      *            EACH DEPOSIT, WITHDRAWAL, EXCHANGE OR OTHER         *
      *            TRANSFER OF CURRENCY OF MORE THAN $10,000. MULTIPLE *
      *            TRANSACTIONS BY OR ON BEHALF OF THE SAME PERSON ON  *
      *            THE SAME BUSINESS DAY ARE AGGREGATED ACROSS ALL     *
      *            ACCOUNTS AND BRANCHES. CASH IN AND CASH OUT ARE     *
      *            AGGREGATED SEPARATELY AND ARE NEVER NETTED.         *
      *            31 CFR 1020.315 - EXEMPT PERSONS (PHASE I: LISTED   *
      *            COMPANIES, GOVERNMENT ENTITIES). MONEY SERVICES     *
      *            BUSINESSES ARE INELIGIBLE FOR EXEMPTION.            *
      * LINKAGE  : CTRPARM                                             *
      * CALLED BY: CTRSCAN (BATCH), TELLER CASH TRANSACTIONS (ONLINE)  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-04-01 RJT  ORIGINAL - SPLIT FROM CTRSCAN                  *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-10-21 BHS  AGGREGATE ACROSS ALL ACCOUNTS OF THE PERSON    *
      *                 (USA PATRIOT ACT SECTION 365 REVIEW)           *
      * 2005-01-14 BHS  INC20331 CASH IN/OUT WERE BEING NETTED - FIXED *
      * 2009-01-05 DPS  REG-2008-E EXEMPTION RULE CHANGES - BIENNIAL   *
      *                 RENEWAL REMOVED, ANNUAL REVIEW NOTE ADDED      *
      * 2012-03-30 DPS  REG-2012-F FILING DUE 15 CALENDAR DAYS         *
      * 2019-10-07 AKW  CHG1802 MSB INELIGIBLE FOR EXEMPTION           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTRRULE.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * REGULATORY CONSTANTS                                           *
      *----------------------------------------------------------------*
       01  WS-CTR-CONSTANTS.
      *    REPORT WHEN TOTAL IS MORE THAN (NOT EQUAL TO) THIS AMOUNT
           05  WS-CTR-THRESHOLD            PIC S9(13)V99 COMP-3
                                           VALUE 10000.00.
      *    FILING DUE WITHIN 15 CALENDAR DAYS OF THE TRANSACTION
           05  WS-CTR-FILING-DAYS          PIC S9(05) VALUE 15.

       01  WS-WORK-FIELDS.
           05  WS-IX                       PIC 9(03) COMP.
           05  WS-JX                       PIC 9(03) COMP.
           05  WS-FOUND                    PIC X(01).
           05  WS-BRANCH-COUNT             PIC 9(03) COMP.
           05  WS-BRANCH-TBL OCCURS 100 TIMES
                                           PIC X(05).
           05  WS-ACCT-TBL OCCURS 100 TIMES
                                           PIC X(12).

       COPY DATEPARM.

       LINKAGE SECTION.
       COPY CTRPARM.

       PROCEDURE DIVISION USING CTR-RULE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           IF CTP-RETURN-CODE = 00
               PERFORM 2000-AGGREGATE-CASH
               PERFORM 3000-APPLY-THRESHOLD
               PERFORM 4000-APPLY-EXEMPTION
               PERFORM 5000-SET-FILING-DATE
           END-IF
           GOBACK.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           MOVE 00     TO CTP-RETURN-CODE
           MOVE ZERO   TO CTP-CASH-IN-TOTAL  CTP-CASH-OUT-TOTAL
                          CTP-CASH-IN-COUNT  CTP-CASH-OUT-COUNT
                          CTP-FILING-DUE-DATE CTP-ACCOUNT-COUNT
           MOVE 'N'    TO CTP-CTR-REQUIRED   CTP-CASH-IN-REPORTABLE
                          CTP-CASH-OUT-REPORTABLE CTP-OVER-THRESHOLD
                          CTP-AGGREGATED-FLAG CTP-MULTI-BRANCH-FLAG
                          CTP-EXEMPT-REVIEW-FLAG
           MOVE 'N'    TO CTP-EXEMPT-STATUS
           MOVE SPACES TO CTP-EXEMPT-REVIEW-NOTE CTP-DECISION-TEXT
           MOVE 0      TO WS-BRANCH-COUNT

           IF CTP-ITEM-COUNT = 0 OR CTP-ITEM-COUNT > 100
               MOVE 12 TO CTP-RETURN-CODE
               MOVE 'INVALID ITEM COUNT' TO CTP-DECISION-TEXT
           ELSE
               INITIALIZE DATE-PARMS
               SET DTU-VALIDATE TO TRUE
               MOVE CTP-BUSINESS-DATE TO DTU-DATE-1
               CALL 'DATEUTIL' USING DATE-PARMS
               IF NOT DTU-RESULT-YES
                   MOVE 12 TO CTP-RETURN-CODE
                   MOVE 'INVALID BUSINESS DATE' TO CTP-DECISION-TEXT
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * AGGREGATE CASH IN AND CASH OUT SEPARATELY ACROSS ALL ACCOUNTS  *
      * AND ALL BRANCHES. IN AND OUT ARE NEVER NETTED (INC20331).      *
      *----------------------------------------------------------------*
       2000-AGGREGATE-CASH.
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > CTP-ITEM-COUNT
               EVALUATE CTP-ITEM-DR-CR(WS-IX)
                   WHEN 'C'
                       ADD CTP-ITEM-AMOUNT(WS-IX) TO CTP-CASH-IN-TOTAL
                       ADD 1 TO CTP-CASH-IN-COUNT
                   WHEN 'D'
                       ADD CTP-ITEM-AMOUNT(WS-IX) TO CTP-CASH-OUT-TOTAL
                       ADD 1 TO CTP-CASH-OUT-COUNT
                   WHEN OTHER
                       MOVE 04 TO CTP-RETURN-CODE
               END-EVALUATE
               PERFORM 2100-COUNT-BRANCH
               PERFORM 2200-COUNT-ACCOUNT
           END-PERFORM
      *    2005-01-14 BHS INC20331 - OLD NETTING LOGIC REMOVED
      *    COMPUTE WS-NET-CASH = CTP-CASH-IN-TOTAL - CTP-CASH-OUT-TOTAL
           IF WS-BRANCH-COUNT > 1
               MOVE 'Y' TO CTP-MULTI-BRANCH-FLAG
           END-IF.

       2100-COUNT-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-JX FROM 1 BY 1
                   UNTIL WS-JX > WS-BRANCH-COUNT OR WS-FOUND = 'Y'
               IF WS-BRANCH-TBL(WS-JX) = CTP-ITEM-BRANCH(WS-IX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N'
               ADD 1 TO WS-BRANCH-COUNT
               MOVE CTP-ITEM-BRANCH(WS-IX)
                 TO WS-BRANCH-TBL(WS-BRANCH-COUNT)
           END-IF.

       2200-COUNT-ACCOUNT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-JX FROM 1 BY 1
                   UNTIL WS-JX > CTP-ACCOUNT-COUNT OR WS-FOUND = 'Y'
               IF WS-ACCT-TBL(WS-JX) = CTP-ITEM-ACCT(WS-IX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N'
               ADD 1 TO CTP-ACCOUNT-COUNT
               MOVE CTP-ITEM-ACCT(WS-IX)
                 TO WS-ACCT-TBL(CTP-ACCOUNT-COUNT)
           END-IF.

      *----------------------------------------------------------------*
      * THRESHOLD TEST - STRICTLY MORE THAN $10,000.00 PER DIRECTION.  *
      * EXACTLY $10,000.00 IS NOT REPORTABLE.                          *
      *----------------------------------------------------------------*
       3000-APPLY-THRESHOLD.
           IF CTP-CASH-IN-TOTAL > WS-CTR-THRESHOLD
               MOVE 'Y' TO CTP-CASH-IN-REPORTABLE
               IF CTP-CASH-IN-COUNT > 1
                   MOVE 'Y' TO CTP-AGGREGATED-FLAG
               END-IF
           END-IF
           IF CTP-CASH-OUT-TOTAL > WS-CTR-THRESHOLD
               MOVE 'Y' TO CTP-CASH-OUT-REPORTABLE
               IF CTP-CASH-OUT-COUNT > 1
                   MOVE 'Y' TO CTP-AGGREGATED-FLAG
               END-IF
           END-IF
           EVALUATE TRUE
               WHEN CTP-CASH-IN-REPORTABLE = 'Y'
                AND CTP-CASH-OUT-REPORTABLE = 'Y'
                   MOVE 'Y' TO CTP-OVER-THRESHOLD CTP-CTR-REQUIRED
                   MOVE 'CASH IN AND CASH OUT EACH EXCEED $10,000'
                     TO CTP-DECISION-TEXT
               WHEN CTP-CASH-IN-REPORTABLE = 'Y'
                   MOVE 'Y' TO CTP-OVER-THRESHOLD CTP-CTR-REQUIRED
                   MOVE 'CASH IN EXCEEDS $10,000'
                     TO CTP-DECISION-TEXT
               WHEN CTP-CASH-OUT-REPORTABLE = 'Y'
                   MOVE 'Y' TO CTP-OVER-THRESHOLD CTP-CTR-REQUIRED
                   MOVE 'CASH OUT EXCEEDS $10,000'
                     TO CTP-DECISION-TEXT
               WHEN OTHER
                   MOVE 'BELOW CTR THRESHOLD - NOT REPORTABLE'
                     TO CTP-DECISION-TEXT
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXEMPTION. A PHASE I EXEMPT PERSON (LISTED COMPANY, GOVERNMENT *
      * AGENCY) NEEDS NO CTR, BUT THE DESIGNATION MUST BE REVIEWED     *
      * ANNUALLY. AN MSB CAN NEVER BE EXEMPT EVEN IF FLAGGED.          *
      *----------------------------------------------------------------*
       4000-APPLY-EXEMPTION.
           EVALUATE TRUE
               WHEN CTP-CTR-EXEMPT-FLAG = 'Y' AND CTP-MSB-FLAG = 'Y'
                   MOVE 'I' TO CTP-EXEMPT-STATUS
                   IF CTP-OVER-THRESHOLD = 'Y'
                       MOVE 'MSB INELIGIBLE FOR EXEMPTION - CTR FILED'
                         TO CTP-DECISION-TEXT
                   END-IF
                   MOVE 'Y' TO CTP-EXEMPT-REVIEW-FLAG
                   MOVE 'REVOKE EXEMPTION - MSB IS INELIGIBLE'
                     TO CTP-EXEMPT-REVIEW-NOTE
               WHEN CTP-CTR-EXEMPT-FLAG = 'Y'
                   MOVE 'E' TO CTP-EXEMPT-STATUS
                   IF CTP-OVER-THRESHOLD = 'Y'
                       MOVE 'N' TO CTP-CTR-REQUIRED
                       MOVE 'Y' TO CTP-EXEMPT-REVIEW-FLAG
                       MOVE 'EXEMPT PERSON - NO CTR (31 CFR 1020.315)'
                         TO CTP-DECISION-TEXT
                       MOVE 'ANNUAL EXEMPTION REVIEW REQUIRED'
                         TO CTP-EXEMPT-REVIEW-NOTE
                   END-IF
               WHEN OTHER
                   MOVE 'N' TO CTP-EXEMPT-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
       5000-SET-FILING-DATE.
           IF CTP-CTR-IS-REQUIRED
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE CTP-BUSINESS-DATE  TO DTU-DATE-1
               MOVE WS-CTR-FILING-DAYS TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO CTP-FILING-DUE-DATE
           END-IF.
