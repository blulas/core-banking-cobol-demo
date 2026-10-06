      *================================================================*
      * PROGRAM  : PAYLIMIT                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : OUTBOUND PAYMENT LIMITS AND APPROVAL REQUIREMENTS.  *
      *            - PER-TRANSACTION AND DAILY AGGREGATE LIMITS BY     *
      *              PAYMENT RAIL AND CUSTOMER SEGMENT                 *
      *                RT  BASE TABLE (ACH 25,000/DAY, WIRE 100,000)   *
      *                MA  BASE X 2      PW  BASE X 10                 *
      *                CB / IN  ACCOUNT LIMITS (ACCT-DAILY-ACH-LIMIT,  *
      *                         ACCT-DAILY-WIRE-LIMIT)                 *
      *            - NEW ACCOUNTS (< 30 DAYS) ACH OUT <= 2,500 / DAY   *
      *            - WIRES >= 50,000 (RT/MA/PW) OR >= 1,000,000 (CB)   *
      *              REQUIRE DUAL APPROVAL                             *
      *            - INTERNATIONAL WIRES TO HIGH-RISK COUNTRIES ALWAYS *
      *              REQUIRE DUAL APPROVAL AND COMPLIANCE REVIEW       *
      * LINKAGE  : PLMPARM                                             *
      * CALLS    : DATEUTIL                                            *
      * CALLED BY: PAYINIT (ONLINE), PAYPROC (BATCH)                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-02-12 RJT  ORIGINAL - FLAT LIMITS                         *
      * 2004-08-09 BHS  SEGMENT MULTIPLIERS (PRIVATE CLIENT GROUP)     *
      * 2006-10-02 BHS  SOX 404 - DUAL APPROVAL ON WIRES >= 50,000     *
      * 2011-04-12 DPS  CORPORATE - USE ACCOUNT LIMITS FROM ACCTMST    *
      * 2012-05-14 DPS  INC11873 NEW ACCOUNT ACH FRAUD - 2,500/DAY     *
      * 2015-02-02 DPS  CB DUAL APPROVAL RAISED TO 1,000,000 CHG1302   *
      * 2018-08-06 AKW  HIGH-RISK COUNTRY TABLE (BSA/AML REVIEW)       *
      * 2021-07-12 AKW  FATF LIST UPDATE                               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYLIMIT.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * BASE (RETAIL) LIMIT TABLE                                      *
      *   TYPE(1) LIMIT-GROUP(1) PER-TXN 9(9)V99 DAILY 9(9)V99         *
      *   LIMIT GROUP A = ACH (ACH+SDACH SHARE), W = WIRE, B = BOOK    *
      *----------------------------------------------------------------*
       01  WS-BASE-LIMIT-VALUES.
           05  FILLER PIC X(24) VALUE 'AA0000100000000002500000'.
           05  FILLER PIC X(24) VALUE 'SA0000100000000002500000'.
           05  FILLER PIC X(24) VALUE 'WW0001000000000010000000'.
           05  FILLER PIC X(24) VALUE 'IW0001000000000010000000'.
           05  FILLER PIC X(24) VALUE 'BB0002500000000025000000'.
       01  WS-BASE-LIMIT-TABLE REDEFINES WS-BASE-LIMIT-VALUES.
           05  WS-LIM-ENTRY OCCURS 5 TIMES INDEXED BY LIM-IDX.
               10  WS-LIM-TYPE             PIC X(01).
               10  WS-LIM-GROUP            PIC X(01).
               10  WS-LIM-PER-TXN          PIC 9(09)V99.
               10  WS-LIM-DAILY            PIC 9(09)V99.

      *----------------------------------------------------------------*
      * SEGMENT MULTIPLIERS. CB AND IN USE ACCOUNT LIMITS (FLAG 'A').  *
      * UNKNOWN SEGMENTS DEFAULT TO RETAIL.                            *
      *----------------------------------------------------------------*
       01  WS-SEGMENT-VALUES.
           05  FILLER PIC X(06) VALUE 'RT001M'.
           05  FILLER PIC X(06) VALUE 'MA002M'.
           05  FILLER PIC X(06) VALUE 'PW010M'.
           05  FILLER PIC X(06) VALUE 'CB000A'.
           05  FILLER PIC X(06) VALUE 'IN000A'.
       01  WS-SEGMENT-TABLE REDEFINES WS-SEGMENT-VALUES.
           05  WS-SEG-ENTRY OCCURS 5 TIMES INDEXED BY SEG-IDX.
               10  WS-SEG-CODE             PIC X(02).
               10  WS-SEG-MULTIPLIER       PIC 9(03).
               10  WS-SEG-METHOD           PIC X(01).

      *----------------------------------------------------------------*
      * HIGH-RISK JURISDICTIONS - INTERNATIONAL WIRES GO TO COMPLIANCE *
      * (COMPREHENSIVELY EMBARGOED COUNTRIES ARE BLOCKED BY OFACSCRN)  *
      *----------------------------------------------------------------*
       01  WS-HIGH-RISK-VALUES.
           05  FILLER PIC X(02) VALUE 'AF'.
           05  FILLER PIC X(02) VALUE 'BY'.
           05  FILLER PIC X(02) VALUE 'MM'.
           05  FILLER PIC X(02) VALUE 'VE'.
           05  FILLER PIC X(02) VALUE 'YE'.
           05  FILLER PIC X(02) VALUE 'HT'.
           05  FILLER PIC X(02) VALUE 'SS'.
           05  FILLER PIC X(02) VALUE 'NI'.
      *    2021-07 FATF INCREASED MONITORING
           05  FILLER PIC X(02) VALUE 'PH'.
           05  FILLER PIC X(02) VALUE 'CU'.
           05  FILLER PIC X(02) VALUE 'IR'.
           05  FILLER PIC X(02) VALUE 'KP'.
           05  FILLER PIC X(02) VALUE 'SY'.
       01  WS-HIGH-RISK-TABLE REDEFINES WS-HIGH-RISK-VALUES.
           05  WS-HR-COUNTRY PIC X(02) OCCURS 13 TIMES
                             INDEXED BY HR-IDX.

       01  WS-CONSTANTS.
           05  WS-NEW-ACCT-DAYS            PIC 9(03) VALUE 30.
           05  WS-NEW-ACCT-ACH-LIMIT       PIC S9(13)V99 COMP-3
                                           VALUE 2500.00.
           05  WS-DUAL-THRESHOLD-RETAIL    PIC S9(13)V99 COMP-3
                                           VALUE 50000.00.
           05  WS-DUAL-THRESHOLD-CORP      PIC S9(13)V99 COMP-3
                                           VALUE 1000000.00.
      *    CB DEFAULTS WHEN THE ACCOUNT CARRIES NO LIMIT (CNV-0117)
           05  WS-CB-DFLT-ACH-LIMIT        PIC S9(13)V99 COMP-3
                                           VALUE 50000000.00.
           05  WS-CB-DFLT-WIRE-LIMIT       PIC S9(13)V99 COMP-3
                                           VALUE 250000000.00.
           05  WS-UNLIMITED                PIC S9(13)V99 COMP-3
                                           VALUE 9999999999999.99.

       01  WS-WORK.
           05  WS-MULTIPLIER               PIC 9(03).
           05  WS-METHOD                   PIC X(01).
           05  WS-GROUP                    PIC X(01).
           05  WS-USED-TODAY               PIC S9(13)V99 COMP-3.
           05  WS-PROJECTED                PIC S9(13)V99 COMP-3.
           05  WS-ACCT-AGE-DAYS            PIC S9(07).
           05  WS-HIGH-RISK-FLAG           PIC X(01).
           05  WS-CORP-FLAG                PIC X(01).
               88  WS-IS-CORPORATE                   VALUE 'Y'.

       COPY DATEPARM.

       LINKAGE SECTION.
       COPY PLMPARM.

       PROCEDURE DIVISION USING PAY-LIMIT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           IF PLM-RETURN-CODE NOT = ZERO
               GOBACK
           END-IF
           PERFORM 2000-SET-LIMITS
           PERFORM 3000-EVALUATE-DECLINES
           IF NOT PLM-DECLINE
               PERFORM 4000-EVALUATE-APPROVALS
           END-IF
           EVALUATE TRUE
               WHEN PLM-DECLINE
                   MOVE 08 TO PLM-RETURN-CODE
               WHEN PLM-NEEDS-APPROVAL
                   MOVE 04 TO PLM-RETURN-CODE
               WHEN OTHER
                   SET PLM-APPROVE TO TRUE
                   MOVE 'OKAY' TO PLM-REASON-CODE
                   MOVE 'WITHIN LIMITS' TO PLM-REASON-TEXT
                   MOVE 00 TO PLM-RETURN-CODE
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           MOVE ZERO   TO PLM-RETURN-CODE
                          PLM-TXN-LIMIT PLM-DAILY-LIMIT
                          PLM-DAILY-REMAINING PLM-DUAL-THRESHOLD
           MOVE SPACES TO PLM-DECISION PLM-REASON-CODE PLM-REASON-TEXT
           MOVE 'N'    TO PLM-DUAL-APPROVAL-FLAG PLM-COMPLIANCE-FLAG
                          PLM-NEW-ACCT-FLAG WS-HIGH-RISK-FLAG
                          WS-CORP-FLAG
           IF PLM-AMOUNT NOT > ZERO
               SET PLM-DECLINE TO TRUE
               MOVE 'AMTZ' TO PLM-REASON-CODE
               MOVE 'AMOUNT MUST BE GREATER THAN ZERO'
                 TO PLM-REASON-TEXT
               MOVE 08 TO PLM-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           SET LIM-IDX TO 1
           SEARCH WS-LIM-ENTRY
               AT END
                   SET PLM-DECLINE TO TRUE
                   MOVE 'TYPE' TO PLM-REASON-CODE
                   MOVE 'INVALID PAYMENT TYPE' TO PLM-REASON-TEXT
                   MOVE 12 TO PLM-RETURN-CODE
               WHEN WS-LIM-TYPE(LIM-IDX) = PLM-PAY-TYPE
                   MOVE WS-LIM-GROUP(LIM-IDX) TO WS-GROUP
           END-SEARCH.

      *----------------------------------------------------------------*
      * LIMIT DETERMINATION BY SEGMENT                                 *
      *----------------------------------------------------------------*
       2000-SET-LIMITS.
           MOVE 001 TO WS-MULTIPLIER
           MOVE 'M' TO WS-METHOD
           SET SEG-IDX TO 1
           SEARCH WS-SEG-ENTRY
               AT END
                   CONTINUE
               WHEN WS-SEG-CODE(SEG-IDX) = PLM-SEGMENT
                   MOVE WS-SEG-MULTIPLIER(SEG-IDX) TO WS-MULTIPLIER
                   MOVE WS-SEG-METHOD(SEG-IDX)     TO WS-METHOD
           END-SEARCH

           EVALUATE WS-GROUP
               WHEN 'A'
                   MOVE PLM-ACH-USED-TODAY  TO WS-USED-TODAY
               WHEN 'W'
                   MOVE PLM-WIRE-USED-TODAY TO WS-USED-TODAY
               WHEN OTHER
                   MOVE PLM-BOOK-USED-TODAY TO WS-USED-TODAY
           END-EVALUATE

           IF WS-METHOD = 'A'
               SET WS-IS-CORPORATE TO TRUE
               MOVE WS-DUAL-THRESHOLD-CORP TO PLM-DUAL-THRESHOLD
               EVALUATE WS-GROUP
                   WHEN 'A'
                       IF PLM-ACCT-ACH-LIMIT > ZERO
                           MOVE PLM-ACCT-ACH-LIMIT  TO PLM-DAILY-LIMIT
                       ELSE
                           MOVE WS-CB-DFLT-ACH-LIMIT TO PLM-DAILY-LIMIT
                       END-IF
                   WHEN 'W'
                       IF PLM-ACCT-WIRE-LIMIT > ZERO
                           MOVE PLM-ACCT-WIRE-LIMIT TO PLM-DAILY-LIMIT
                       ELSE
                           MOVE WS-CB-DFLT-WIRE-LIMIT
                             TO PLM-DAILY-LIMIT
                       END-IF
                   WHEN OTHER
                       MOVE WS-UNLIMITED TO PLM-DAILY-LIMIT
               END-EVALUATE
               MOVE PLM-DAILY-LIMIT TO PLM-TXN-LIMIT
           ELSE
               MOVE WS-DUAL-THRESHOLD-RETAIL TO PLM-DUAL-THRESHOLD
               COMPUTE PLM-TXN-LIMIT =
                   WS-LIM-PER-TXN(LIM-IDX) * WS-MULTIPLIER
               COMPUTE PLM-DAILY-LIMIT =
                   WS-LIM-DAILY(LIM-IDX) * WS-MULTIPLIER
           END-IF

      *    NEW ACCOUNT CHECK - ACCOUNT AGE IN CALENDAR DAYS
           IF PLM-ACCT-OPEN-DATE > ZERO
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PLM-ACCT-OPEN-DATE TO DTU-DATE-1
               MOVE PLM-BUSINESS-DATE  TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-ACCT-AGE-DAYS
               IF DTU-OK AND WS-ACCT-AGE-DAYS < WS-NEW-ACCT-DAYS
                   MOVE 'Y' TO PLM-NEW-ACCT-FLAG
               END-IF
           END-IF
           IF PLM-NEW-ACCT-FLAG = 'Y' AND WS-GROUP = 'A'
              AND NOT WS-IS-CORPORATE
               MOVE WS-NEW-ACCT-ACH-LIMIT TO PLM-DAILY-LIMIT
               IF PLM-TXN-LIMIT > WS-NEW-ACCT-ACH-LIMIT
                   MOVE WS-NEW-ACCT-ACH-LIMIT TO PLM-TXN-LIMIT
               END-IF
           END-IF

           COMPUTE WS-PROJECTED = WS-USED-TODAY + PLM-AMOUNT
           COMPUTE PLM-DAILY-REMAINING =
               PLM-DAILY-LIMIT - WS-USED-TODAY
           IF PLM-DAILY-REMAINING < ZERO
               MOVE ZERO TO PLM-DAILY-REMAINING
           END-IF.

      *----------------------------------------------------------------*
      * HARD LIMITS. CORPORATE OVER-LIMIT IS AN OVERRIDE (TREASURY     *
      * APPROVAL), NOT A DECLINE.                                      *
      *----------------------------------------------------------------*
       3000-EVALUATE-DECLINES.
           EVALUATE TRUE
               WHEN PLM-NEW-ACCT-FLAG = 'Y' AND WS-GROUP = 'A'
                AND NOT WS-IS-CORPORATE
                AND WS-PROJECTED > WS-NEW-ACCT-ACH-LIMIT
                   SET PLM-DECLINE TO TRUE
                   MOVE 'LNEW' TO PLM-REASON-CODE
                   MOVE 'NEW ACCOUNT ACH LIMIT 2,500 PER DAY EXCEEDED'
                     TO PLM-REASON-TEXT
               WHEN PLM-AMOUNT > PLM-TXN-LIMIT
                AND WS-IS-CORPORATE
                   SET PLM-NEEDS-APPROVAL TO TRUE
                   MOVE 'LOVR' TO PLM-REASON-CODE
                   MOVE 'ACCOUNT LIMIT EXCEEDED - TREASURY OVERRIDE'
                     TO PLM-REASON-TEXT
                   MOVE 'Y' TO PLM-DUAL-APPROVAL-FLAG
               WHEN PLM-AMOUNT > PLM-TXN-LIMIT
                   SET PLM-DECLINE TO TRUE
                   MOVE 'LTXN' TO PLM-REASON-CODE
                   MOVE 'PER-TRANSACTION LIMIT EXCEEDED'
                     TO PLM-REASON-TEXT
               WHEN WS-PROJECTED > PLM-DAILY-LIMIT
                AND WS-IS-CORPORATE
                   SET PLM-NEEDS-APPROVAL TO TRUE
                   MOVE 'LOVR' TO PLM-REASON-CODE
                   MOVE 'DAILY ACCOUNT LIMIT EXCEEDED - OVERRIDE'
                     TO PLM-REASON-TEXT
                   MOVE 'Y' TO PLM-DUAL-APPROVAL-FLAG
               WHEN WS-PROJECTED > PLM-DAILY-LIMIT
                   SET PLM-DECLINE TO TRUE
                   MOVE 'LDAY' TO PLM-REASON-CODE
                   MOVE 'DAILY AGGREGATE LIMIT EXCEEDED'
                     TO PLM-REASON-TEXT
           END-EVALUATE.

      *----------------------------------------------------------------*
      * APPROVAL REQUIREMENTS - FIRST REASON WINS, FLAGS ACCUMULATE    *
      *----------------------------------------------------------------*
       4000-EVALUATE-APPROVALS.
           IF PLM-INTL-WIRE
               SET HR-IDX TO 1
               SEARCH WS-HR-COUNTRY
                   AT END
                       CONTINUE
                   WHEN WS-HR-COUNTRY(HR-IDX) = PLM-BENE-COUNTRY
                       MOVE 'Y' TO WS-HIGH-RISK-FLAG
               END-SEARCH
           END-IF

           IF WS-HIGH-RISK-FLAG = 'Y'
               MOVE 'Y' TO PLM-DUAL-APPROVAL-FLAG
                           PLM-COMPLIANCE-FLAG
               IF NOT PLM-NEEDS-APPROVAL
                   SET PLM-NEEDS-APPROVAL TO TRUE
                   MOVE 'HRCY' TO PLM-REASON-CODE
                   MOVE 'HIGH-RISK COUNTRY - DUAL APPROVAL + COMPLIANCE'
                     TO PLM-REASON-TEXT
               END-IF
           END-IF

           IF PLM-ANY-WIRE AND PLM-AMOUNT NOT < PLM-DUAL-THRESHOLD
               MOVE 'Y' TO PLM-DUAL-APPROVAL-FLAG
               IF NOT PLM-NEEDS-APPROVAL
                   SET PLM-NEEDS-APPROVAL TO TRUE
                   MOVE 'DUAL' TO PLM-REASON-CODE
                   MOVE 'WIRE AT OR ABOVE DUAL APPROVAL THRESHOLD'
                     TO PLM-REASON-TEXT
               END-IF
           END-IF

      *    NEW ACCOUNT WIRES - CALLBACK / SECOND APPROVER (INC11873)
           IF PLM-ANY-WIRE AND PLM-NEW-ACCT-FLAG = 'Y'
              AND NOT WS-IS-CORPORATE
               MOVE 'Y' TO PLM-DUAL-APPROVAL-FLAG
               IF NOT PLM-NEEDS-APPROVAL
                   SET PLM-NEEDS-APPROVAL TO TRUE
                   MOVE 'NEWW' TO PLM-REASON-CODE
                   MOVE 'WIRE FROM ACCOUNT OPEN < 30 DAYS'
                     TO PLM-REASON-TEXT
               END-IF
           END-IF.
