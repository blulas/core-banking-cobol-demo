      *================================================================*
      * PROGRAM  : ACHRET                                              *
      * TYPE     : BATCH - EOD STEP 06                                 *
      * PURPOSE  : ACH RETURNS (RDFI). PICKS UP INBOUND ACH ENTRIES    *
      *            THAT TRANEDIT OR POSTTRAN COULD NOT POST (DD        *
      *            TRANREJ), TRANSLATES THE INTERNAL REJECT CODE INTO  *
      *            A NACHA RETURN REASON CODE AND STAGES A RETURN      *
      *            ENTRY (6) + RETURN ADDENDA (7 TYPE 99) ON DD ACHRTN *
      *            (EXTEND - ACHIN HAS ALREADY STAGED ITS OWN R02/R03  *
      *            RETURNS THERE). IT THEN FORMATS THE WHOLE STAGING   *
      *            FILE INTO THE NACHA RETURN FILE FOR THE FED (DD     *
      *            ACHRTNF): FILE HEADER, ONE BATCH PER ORIGINAL       *
      *            COMPANY / SEC / EFFECTIVE DATE, BATCH AND FILE      *
      *            CONTROLS WITH ENTRY HASH, 10-RECORD BLOCKING.       *
      *                                                                *
      *            SELECTION FROM TRANREJ:                             *
      *              - RECEIVED VIA ACHIN (TRAN-USER-ID OR SOURCE PGM  *
      *                = 'ACHIN') AND CODE DACH / WACH -> RETURN       *
      *              - ORIGINATED BY PAYPROC (SEQ 15000000-19999999)   *
      *                -> NOT RETURNABLE, LISTED FOR PAYMENT OPS       *
      *              - OTHER ACH / SDA CHANNEL ITEMS (NO TRACE / ODFI) *
      *                -> MANUAL RETURN QUEUE (REPORT ONLY)            *
      *              - EVERYTHING ELSE IS IGNORED                      *
      *            RETURN TIME FRAME: 2 BANKING DAYS AFTER THE         *
      *            SETTLEMENT DATE, 60 CALENDAR DAYS FOR UNAUTHORISED  *
      *            CONSUMER RETURN CODES. LATE ITEMS ARE NOT RETURNED. *
      * INPUT    : TRANREJ (TRANREC), BANKCTL, ACHRTN (STAGING),       *
      *            ACHINBND (OPTIONAL - ORIGINAL ENTRY NAME / ID)      *
      * OUTPUT   : ACHRTN (EXTEND), ACHRTNF (NACHA 94), ACHRRPT        *
      * RC       : 00 OK, 04 LATE / UNRETURNABLE ITEMS OR NO TRACE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2001-04-30 BHS  ORIGINAL - NSF RETURNS FROM POSTING            *
      * 2003-07-21 BHS  R16 FOR FROZEN / OFAC RESTRICTED ACCOUNTS      *
      * 2006-01-09 BHS  RETURN TIME FRAME CHECK (NACHA OR 2006)        *
      * 2011-08-15 DPS  FORMATTED RETURN FILE ACHRTNF - INC09920       *
      * 2015-03-02 DPS  R20 FOR NON-TRANSACTION ACCOUNTS (TIME DEP)    *
      * 2016-09-23 DPS  SAME-DAY ACH ITEMS (CHANNEL SDA)               *
      * 2018-09-17 AKW  NSF ITEMS NOW ARRIVE VIA TRANREJ 'NSF '        *
      * 2021-06-14 AKW  SELECT ON TRAN-USER-ID - POSTTRAN AND TRANEDIT *
      *                 OVERWRITE TRAN-SOURCE-PGM (INC22871)           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACHRET.
       AUTHOR. B H SANDOVAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL TRANREJ-FILE ASSIGN TO "TRANREJ"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-REJ-STATUS.
           SELECT OPTIONAL ACH-RTN-FILE ASSIGN TO "ACHRTN"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RTN-STATUS.
           SELECT OPTIONAL ACH-IN-FILE ASSIGN TO "ACHINBND"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT RETURN-FILE ASSIGN TO "ACHRTNF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RTNF-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ACHRRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANREJ-FILE.
       COPY TRANREC.
       FD  ACH-RTN-FILE.
       01  ACH-RTN-REC                     PIC X(94).
       FD  ACH-IN-FILE.
       01  ACH-IN-REC                      PIC X(94).
       FD  RETURN-FILE.
       01  RETURN-REC                      PIC X(94).
       FD  REPORT-FILE.
       01  REPORT-REC                      PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-REJ-STATUS               PIC X(02).
           05  WS-RTN-STATUS               PIC X(02).
           05  WS-IN-STATUS                PIC X(02).
           05  WS-RTNF-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-FLAGS.
           05  WS-REJ-EOF                  PIC X(01) VALUE 'N'.
               88  WS-REJ-END                        VALUE 'Y'.
           05  WS-RTN-EOF                  PIC X(01) VALUE 'N'.
               88  WS-RTN-END                        VALUE 'Y'.
           05  WS-IN-EOF                   PIC X(01) VALUE 'N'.
               88  WS-IN-END                         VALUE 'Y'.
           05  WS-ITEM-CLASS               PIC X(01).
               88  WS-CLS-RETURN                     VALUE 'R'.
               88  WS-CLS-ORIGINATED                 VALUE 'O'.
               88  WS-CLS-MANUAL                     VALUE 'M'.
               88  WS-CLS-IGNORE                     VALUE 'X'.
           05  WS-TIMELY-FLAG              PIC X(01).
               88  WS-TIMELY                         VALUE 'Y'.
               88  WS-LATE                           VALUE 'N'.

       01  WS-CONSTANTS.
           05  WS-OUR-ABA                  PIC X(09) VALUE '026000013'.
           05  WS-OUR-DFI                  PIC X(08) VALUE '02600001'.
           05  WS-OUR-NAME                 PIC X(23)
                                   VALUE 'GOLDMAN SACHS BANK USA'.
           05  WS-FED-ACH-ABA              PIC X(09) VALUE '076000005'.
           05  WS-FED-ACH-NAME             PIC X(23)
                                           VALUE 'FRB ACH OPERATOR'.
           05  WS-HASH-MODULUS             PIC 9(11) VALUE 10000000000.
      *    NACHA OPERATING RULES ARTICLE THREE SUBSECTION 3.8.3
           05  WS-RETURN-BANK-DAYS         PIC 9(02) VALUE 2.
           05  WS-UNAUTH-CAL-DAYS          PIC 9(03) VALUE 60.
           05  WS-PAYPROC-SEQ-LO           PIC 9(08) VALUE 15000000.
           05  WS-PAYPROC-SEQ-HI           PIC 9(08) VALUE 19999999.

      *--- INTERNAL REJECT CODE -> NACHA RETURN REASON ----------------*
      *    TYPE  S = STANDARD 2 BANKING DAY WINDOW                     *
      *          U = UNAUTHORISED (CONSUMER) 60 CALENDAR DAY WINDOW    *
       01  WS-RRC-VALUES.
           05  FILLER PIC X(36) VALUE
               'NSF R01SINSUFFICIENT FUNDS          '.
           05  FILLER PIC X(36) VALUE
               'ACLSR02SACCOUNT CLOSED              '.
           05  FILLER PIC X(36) VALUE
               'AESCR02SACCOUNT CLOSED - ESCHEATED  '.
           05  FILLER PIC X(36) VALUE
               'ACOFR02SACCOUNT CLOSED - CHARGED OFF'.
           05  FILLER PIC X(36) VALUE
               'ACNFR03SNO ACCOUNT / UNABLE TO LOCAT'.
           05  FILLER PIC X(36) VALUE
               'AFRZR16SACCOUNT FROZEN              '.
           05  FILLER PIC X(36) VALUE
               'ARSTR16SACCOUNT FROZEN - RESTRICTED '.
           05  FILLER PIC X(36) VALUE
               'ADRMR16SACCOUNT FROZEN - DORMANT    '.
           05  FILLER PIC X(36) VALUE
               'APNDR16SACCOUNT NOT YET OPEN        '.
           05  FILLER PIC X(36) VALUE
               'TDDRR20SNON-TRANSACTION ACCOUNT     '.
           05  FILLER PIC X(36) VALUE
               'TDCRR20SNON-TRANSACTION ACCOUNT     '.
           05  FILLER PIC X(36) VALUE
               'CHNLR20SNON-TRANSACTION ACCOUNT     '.
           05  FILLER PIC X(36) VALUE
               'NCHKR20SNON-TRANSACTION ACCOUNT     '.
           05  FILLER PIC X(36) VALUE
               'UNATR10UNOT AUTHORIZED BY CUSTOMER  '.
           05  FILLER PIC X(36) VALUE
               'STOPR08SPAYMENT STOPPED             '.
       01  WS-RRC-TABLE REDEFINES WS-RRC-VALUES.
           05  WS-RRC-ENTRY OCCURS 15 TIMES INDEXED BY RRC-IDX.
               10  WS-RRC-REJECT-CODE      PIC X(04).
               10  WS-RRC-RETURN-CODE      PIC X(03).
               10  WS-RRC-WINDOW-TYPE      PIC X(01).
               10  WS-RRC-DESC             PIC X(28).
      *    ANYTHING NOT IN THE TABLE IS RETURNED R17 (FILE RECORD EDIT)
       01  WS-RRC-DEFAULT-CODE             PIC X(03) VALUE 'R17'.

      *--- ORIGINAL ENTRIES FROM THE INBOUND FILE (BY TRACE) ----------*
       01  WS-ORIG-TABLE.
           05  WS-OT-COUNT                 PIC 9(04) VALUE 0.
           05  WS-OT-ENTRY OCCURS 2000 TIMES INDEXED BY OT-IDX.
               10  WS-OT-TRACE             PIC X(15).
               10  WS-OT-TRAN-CODE         PIC X(02).
               10  WS-OT-INDIV-ID          PIC X(15).
               10  WS-OT-INDIV-NAME        PIC X(22).
               10  WS-OT-SEC               PIC X(03).
               10  WS-OT-EFF-DATE          PIC X(06).
       01  WS-CUR-SEC                      PIC X(03).
       01  WS-CUR-EFF                      PIC X(06).

      *--- RETURN STAGING TABLE (ENTRY + ADDENDA PAIRS) ---------------*
       01  WS-RET-TABLE.
           05  WS-RT-COUNT                 PIC 9(04) VALUE 0.
           05  WS-RT-ENTRY OCCURS 1000 TIMES.
               10  WS-RT-KEY.
                   15  WS-RT-SEC           PIC X(03).
                   15  WS-RT-CO-ID         PIC X(10).
                   15  WS-RT-CO-NAME       PIC X(16).
                   15  WS-RT-EFF-DATE      PIC X(06).
               10  WS-RT-DETAIL            PIC X(94).
               10  WS-RT-ADDENDA           PIC X(94).
               10  WS-RT-WRITTEN           PIC X(01).
       01  WS-RT-SUB                       PIC 9(04).
       01  WS-RT-SUB2                      PIC 9(04).
       01  WS-PENDING-DETAIL               PIC X(94).
       01  WS-PENDING-FLAG                 PIC X(01) VALUE 'N'.

       COPY NACHAREC.

      *--- CURRENT ITEM -----------------------------------------------*
       01  WS-ITEM.
           05  WS-IT-TRACE                 PIC X(15).
           05  WS-IT-ODFI-ABA              PIC X(09).
           05  WS-IT-ORIG-CODE             PIC X(02).
           05  WS-IT-SEC                   PIC X(03).
           05  WS-IT-CO-NAME               PIC X(16).
           05  WS-IT-CO-ID                 PIC X(10).
           05  WS-IT-EFF-YYMMDD            PIC X(06).
           05  WS-IT-INDIV-ID              PIC X(15).
           05  WS-IT-INDIV-NAME            PIC X(22).
           05  WS-IT-RETURN-CODE           PIC X(03).
           05  WS-IT-WINDOW                PIC X(01).
           05  WS-IT-RETURN-DESC           PIC X(28).
           05  WS-IT-DEADLINE              PIC 9(08).
           05  WS-IT-DISPOSITION           PIC X(40).

       01  WS-COUNTERS.
           05  WS-REJ-READ                 PIC 9(07) VALUE 0.
           05  WS-ACH-SEEN                 PIC 9(07) VALUE 0.
           05  WS-RET-COUNT                PIC 9(07) VALUE 0.
           05  WS-LATE-COUNT               PIC 9(07) VALUE 0.
           05  WS-NOTRACE-COUNT            PIC 9(07) VALUE 0.
           05  WS-ORIG-COUNT               PIC 9(07) VALUE 0.
           05  WS-MANUAL-COUNT             PIC 9(07) VALUE 0.
           05  WS-STAGED-BEFORE            PIC 9(07) VALUE 0.
           05  WS-TRACE-SEQ                PIC 9(07) VALUE 0.
           05  WS-RET-DR-AMT               PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-RET-CR-AMT               PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-LATE-AMT                 PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-MANUAL-AMT               PIC S9(13)V99 COMP-3
                                           VALUE 0.
       01  WS-NACHA-TOTALS.
           05  WS-NB-BATCH-NO              PIC 9(07) VALUE 0.
           05  WS-NB-COUNT                 PIC 9(06).
           05  WS-NB-HASH                  PIC 9(15).
           05  WS-NB-DEBIT                 PIC 9(10)V99.
           05  WS-NB-CREDIT                PIC 9(10)V99.
           05  WS-NF-COUNT                 PIC 9(08) VALUE 0.
           05  WS-NF-HASH                  PIC 9(15) VALUE 0.
           05  WS-NF-DEBIT                 PIC 9(10)V99 VALUE 0.
           05  WS-NF-CREDIT                PIC 9(10)V99 VALUE 0.
           05  WS-NF-RECORDS               PIC 9(08) VALUE 0.
           05  WS-NF-BLOCKS                PIC 9(06) VALUE 0.
           05  WS-HASH-10                  PIC 9(10).
           05  WS-RDFI-NUM                 PIC 9(08).

       01  WS-RPT-LINE                     PIC X(132).
       01  WS-ED-AMT                       PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-ED-AMT2                      PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-ED-CNT                       PIC ZZZ,ZZ9.
       01  WS-TIME-NOW                     PIC 9(08).
       01  WS-YYMMDD                       PIC 9(06).

       COPY DATEPARM.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 1100-SCAN-STAGING
           PERFORM 1200-LOAD-ORIGINALS
           OPEN EXTEND ACH-RTN-FILE
           IF WS-RTN-STATUS NOT = '00' AND WS-RTN-STATUS NOT = '05'
               MOVE 'ACHRTN' TO ABND-FILE-NAME
               MOVE WS-RTN-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           PERFORM UNTIL WS-REJ-END
               READ TRANREJ-FILE
                   AT END SET WS-REJ-END TO TRUE
                   NOT AT END PERFORM 2000-PROCESS-REJECT
               END-READ
           END-PERFORM
           CLOSE TRANREJ-FILE ACH-RTN-FILE
           PERFORM 5000-BUILD-RETURN-FILE
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN INPUT  TRANREJ-FILE
           OPEN OUTPUT RETURN-FILE REPORT-FILE
           IF WS-REJ-STATUS NOT = '00' AND WS-REJ-STATUS NOT = '05'
               MOVE 'TRANREJ' TO ABND-FILE-NAME
               MOVE WS-REJ-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           IF WS-RTNF-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
               MOVE 'ACHRTNF' TO ABND-FILE-NAME
               MOVE WS-RTNF-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           MOVE SPACES TO WS-RPT-LINE
           STRING 'ACHRET   ACH RETURNS - RDFI ' WS-OUR-ABA
                  '   BUSINESS DATE ' CTL-BUSINESS-DATE
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE ALL '-' TO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           STRING 'ACCOUNT      CD          AMOUNT TRACE           '
                  'REJ  RTN DISPOSITION'
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT.

      *----------------------------------------------------------------*
      * WHAT ACHIN ALREADY STAGED - COUNT AND HIGHEST RETURN TRACE SEQ *
      *----------------------------------------------------------------*
       1100-SCAN-STAGING.
           OPEN INPUT ACH-RTN-FILE
           IF WS-RTN-STATUS NOT = '00' AND WS-RTN-STATUS NOT = '05'
               MOVE 'ACHRTN' TO ABND-FILE-NAME
               MOVE WS-RTN-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           PERFORM UNTIL WS-RTN-END
               READ ACH-RTN-FILE INTO NACHA-REC-DATA
                   AT END SET WS-RTN-END TO TRUE
                   NOT AT END
                       IF NACHA-ENTRY-DETAIL
                           ADD 1 TO WS-STAGED-BEFORE
                           IF NED-TRACE-ODFI = WS-OUR-DFI
                              AND NED-TRACE-SEQ > WS-TRACE-SEQ
                               MOVE NED-TRACE-SEQ TO WS-TRACE-SEQ
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACH-RTN-FILE
           MOVE 'N' TO WS-RTN-EOF.

      *----------------------------------------------------------------*
      * ORIGINAL ENTRY DETAIL (INDIVIDUAL NAME / ID, NACHA TRAN CODE)  *
      * FROM TODAY'S INBOUND FILE. OPTIONAL - IF THE DD IS MISSING THE *
      * RETURN IS BUILT FROM THE TRANREJ RECORD ALONE.                 *
      *----------------------------------------------------------------*
       1200-LOAD-ORIGINALS.
           OPEN INPUT ACH-IN-FILE
           IF WS-IN-STATUS NOT = '00'
               SET WS-IN-END TO TRUE
           END-IF
           PERFORM UNTIL WS-IN-END
               READ ACH-IN-FILE INTO NACHA-REC-DATA
                   AT END SET WS-IN-END TO TRUE
                   NOT AT END PERFORM 1210-LOAD-ORIGINAL
               END-READ
           END-PERFORM
           IF WS-IN-STATUS = '00' OR WS-IN-STATUS = '10'
               CLOSE ACH-IN-FILE
           END-IF.

       1210-LOAD-ORIGINAL.
           EVALUATE TRUE
               WHEN NACHA-REC-DATA(1:10) = '9999999999'
                   CONTINUE
               WHEN NACHA-BATCH-HEADER
                   MOVE NBH-SEC-CODE       TO WS-CUR-SEC
                   MOVE NBH-EFFECTIVE-DATE TO WS-CUR-EFF
               WHEN NACHA-ENTRY-DETAIL AND WS-OT-COUNT < 2000
                   ADD 1 TO WS-OT-COUNT
                   MOVE NED-TRACE-NUMBER    TO WS-OT-TRACE(WS-OT-COUNT)
                   MOVE NED-TRAN-CODE
                     TO WS-OT-TRAN-CODE(WS-OT-COUNT)
                   MOVE NED-INDIVIDUAL-ID
                     TO WS-OT-INDIV-ID(WS-OT-COUNT)
                   MOVE NED-INDIVIDUAL-NAME
                     TO WS-OT-INDIV-NAME(WS-OT-COUNT)
                   MOVE WS-CUR-SEC          TO WS-OT-SEC(WS-OT-COUNT)
                   MOVE WS-CUR-EFF
                     TO WS-OT-EFF-DATE(WS-OT-COUNT)
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-PROCESS-REJECT.
           ADD 1 TO WS-REJ-READ
           MOVE SPACES TO WS-ITEM
           PERFORM 2100-CLASSIFY-ITEM
           EVALUATE TRUE
               WHEN WS-CLS-IGNORE
                   CONTINUE
               WHEN WS-CLS-ORIGINATED
                   ADD 1 TO WS-ORIG-COUNT
                   MOVE 'OWN ORIGINATION - NOT RETURNABLE, PAY OPS'
                     TO WS-IT-DISPOSITION
                   PERFORM 8800-REPORT-ITEM
               WHEN WS-CLS-MANUAL
                   ADD 1 TO WS-MANUAL-COUNT
                   ADD TRAN-AMOUNT TO WS-MANUAL-AMT
                   MOVE 'NOT RECEIVED VIA ACHIN - MANUAL RETURN Q'
                     TO WS-IT-DISPOSITION
                   PERFORM 8800-REPORT-ITEM
               WHEN WS-CLS-RETURN
                   PERFORM 2200-MAP-RETURN-REASON
                   PERFORM 2300-CHECK-TIME-FRAME
                   PERFORM 2400-GET-ORIGINAL-DATA
                   EVALUATE TRUE
                       WHEN WS-IT-TRACE IS NOT NUMERIC
                         OR WS-IT-ODFI-ABA IS NOT NUMERIC
                           ADD 1 TO WS-NOTRACE-COUNT
                           ADD TRAN-AMOUNT TO WS-MANUAL-AMT
                           MOVE 'NO TRACE / ODFI - MANUAL RETURN QUEUE'
                             TO WS-IT-DISPOSITION
                       WHEN WS-LATE
                           ADD 1 TO WS-LATE-COUNT
                           ADD TRAN-AMOUNT TO WS-LATE-AMT
                           MOVE SPACES TO WS-IT-DISPOSITION
                           STRING 'LATE - DEADLINE ' WS-IT-DEADLINE
                                  ' - NOT RETURNED'
                                  DELIMITED BY SIZE
                                  INTO WS-IT-DISPOSITION
                       WHEN OTHER
                           PERFORM 2500-STAGE-RETURN
                   END-EVALUATE
                   PERFORM 8800-REPORT-ITEM
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANEDIT AND POSTTRAN STAMP THEIR OWN NAME IN TRAN-SOURCE-PGM  *
      * ON THE REJECT, SO AN ACHIN ITEM IS RECOGNISED BY ITS USER ID.  *
      *----------------------------------------------------------------*
       2100-CLASSIFY-ITEM.
           EVALUATE TRUE
               WHEN TRAN-CODE NOT = 'DACH' AND TRAN-CODE NOT = 'WACH'
                   SET WS-CLS-IGNORE TO TRUE
               WHEN TRAN-USER-ID = 'ACHIN'
                 OR TRAN-SOURCE-PGM = 'ACHIN'
                   SET WS-CLS-RETURN TO TRUE
                   ADD 1 TO WS-ACH-SEEN
               WHEN TRAN-ID-SEQ NOT < WS-PAYPROC-SEQ-LO
                AND TRAN-ID-SEQ NOT > WS-PAYPROC-SEQ-HI
                   SET WS-CLS-ORIGINATED TO TRUE
                   ADD 1 TO WS-ACH-SEEN
               WHEN TRAN-CH-ACH OR TRAN-CH-SAME-DAY-ACH
                   SET WS-CLS-MANUAL TO TRUE
                   ADD 1 TO WS-ACH-SEEN
               WHEN OTHER
                   SET WS-CLS-IGNORE TO TRUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REJECT CODE -> RETURN REASON. WHERE THE ORIGINAL WAS A CREDIT  *
      * TO A FROZEN OR RESTRICTED ACCOUNT IT IS STILL RETURNED R16.    *
      *----------------------------------------------------------------*
       2200-MAP-RETURN-REASON.
           MOVE WS-RRC-DEFAULT-CODE TO WS-IT-RETURN-CODE
           MOVE 'S'                 TO WS-IT-WINDOW
           MOVE 'FILE RECORD EDIT CRITERIA'  TO WS-IT-RETURN-DESC
           SET RRC-IDX TO 1
           SEARCH WS-RRC-ENTRY
               AT END CONTINUE
               WHEN WS-RRC-REJECT-CODE(RRC-IDX) = TRAN-REJECT-CODE
                   MOVE WS-RRC-RETURN-CODE(RRC-IDX)
                     TO WS-IT-RETURN-CODE
                   MOVE WS-RRC-WINDOW-TYPE(RRC-IDX) TO WS-IT-WINDOW
                   MOVE WS-RRC-DESC(RRC-IDX) TO WS-IT-RETURN-DESC
           END-SEARCH
      *    NSF ON A CREDIT CANNOT HAPPEN - TREAT AS AN EDIT FAILURE
           IF WS-IT-RETURN-CODE = 'R01' AND TRAN-IS-CREDIT
               MOVE WS-RRC-DEFAULT-CODE TO WS-IT-RETURN-CODE
           END-IF.

      *----------------------------------------------------------------*
      * RETURN TIME FRAME FROM THE SETTLEMENT (EFFECTIVE) DATE         *
      *   S: SETTLEMENT + 2 BANKING DAYS                               *
      *   U: SETTLEMENT + 60 CALENDAR DAYS (CONSUMER SEC ONLY)         *
      *----------------------------------------------------------------*
       2300-CHECK-TIME-FRAME.
           INITIALIZE DATE-PARMS
           MOVE TRAN-EFFECTIVE-DATE TO DTU-DATE-1
           IF TRAN-EFFECTIVE-DATE = ZERO
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           END-IF
           IF WS-IT-WINDOW = 'U'
               SET DTU-ADD-DAYS TO TRUE
               MOVE WS-UNAUTH-CAL-DAYS TO DTU-NUMBER
           ELSE
               SET DTU-ADD-BUS-DAYS TO TRUE
               MOVE WS-RETURN-BANK-DAYS TO DTU-NUMBER
           END-IF
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DTU-OK
               MOVE DTU-RESULT-DATE TO WS-IT-DEADLINE
           ELSE
               MOVE CTL-BUSINESS-DATE TO WS-IT-DEADLINE
           END-IF
           IF CTL-BUSINESS-DATE > WS-IT-DEADLINE
               SET WS-LATE TO TRUE
           ELSE
               SET WS-TIMELY TO TRUE
           END-IF.

      *----------------------------------------------------------------*
      * ACHIN LAYOUT: DESC 1-16 CO NAME, 29-31 SEC, 33-34 NACHA CODE,  *
      * REFERENCE = TRACE, COUNTER-ACCT = CO ID, ROUTING = ODFI ABA    *
      *----------------------------------------------------------------*
       2400-GET-ORIGINAL-DATA.
           MOVE TRAN-REFERENCE(1:15)       TO WS-IT-TRACE
           MOVE TRAN-COUNTERPARTY-ROUTING  TO WS-IT-ODFI-ABA
           MOVE TRAN-DESCRIPTION(1:16)     TO WS-IT-CO-NAME
           MOVE TRAN-DESCRIPTION(29:3)     TO WS-IT-SEC
           MOVE TRAN-DESCRIPTION(33:2)     TO WS-IT-ORIG-CODE
           MOVE TRAN-COUNTER-ACCT(1:10)    TO WS-IT-CO-ID
           MOVE TRAN-EFFECTIVE-DATE(3:6)   TO WS-IT-EFF-YYMMDD
           IF WS-IT-SEC = SPACES
               MOVE 'PPD' TO WS-IT-SEC
           END-IF
           IF WS-IT-ORIG-CODE IS NOT NUMERIC
               IF TRAN-IS-CREDIT
                   MOVE '22' TO WS-IT-ORIG-CODE
               ELSE
                   MOVE '27' TO WS-IT-ORIG-CODE
               END-IF
           END-IF
           SET OT-IDX TO 1
           SEARCH WS-OT-ENTRY
               AT END CONTINUE
               WHEN OT-IDX > WS-OT-COUNT
                   CONTINUE
               WHEN WS-OT-TRACE(OT-IDX) = WS-IT-TRACE
                   MOVE WS-OT-INDIV-ID(OT-IDX)   TO WS-IT-INDIV-ID
                   MOVE WS-OT-INDIV-NAME(OT-IDX) TO WS-IT-INDIV-NAME
                   MOVE WS-OT-TRAN-CODE(OT-IDX)  TO WS-IT-ORIG-CODE
                   MOVE WS-OT-EFF-DATE(OT-IDX)   TO WS-IT-EFF-YYMMDD
           END-SEARCH.

      *----------------------------------------------------------------*
      * RETURN ENTRY: TRAN CODE = ORIGINAL - 1, ROUTED BACK TO ODFI    *
      *----------------------------------------------------------------*
       2500-STAGE-RETURN.
           ADD 1 TO WS-RET-COUNT WS-TRACE-SEQ
           MOVE SPACES TO NACHA-REC-DATA
           MOVE '6' TO NED-REC-TYPE
           EVALUATE WS-IT-ORIG-CODE
               WHEN '22' MOVE '21' TO NED-TRAN-CODE
               WHEN '27' MOVE '26' TO NED-TRAN-CODE
               WHEN '32' MOVE '31' TO NED-TRAN-CODE
               WHEN '37' MOVE '36' TO NED-TRAN-CODE
               WHEN OTHER
                   IF TRAN-IS-CREDIT
                       MOVE '21' TO NED-TRAN-CODE
                   ELSE
                       MOVE '26' TO NED-TRAN-CODE
                   END-IF
           END-EVALUATE
           IF NED-TRAN-CODE = '21' OR NED-TRAN-CODE = '31'
               ADD TRAN-AMOUNT TO WS-RET-CR-AMT
           ELSE
               ADD TRAN-AMOUNT TO WS-RET-DR-AMT
           END-IF
           MOVE WS-IT-ODFI-ABA(1:8)  TO NED-RDFI-ID
           MOVE WS-IT-ODFI-ABA(9:1)  TO NED-CHECK-DIGIT
           MOVE TRAN-ACCT-NUMBER     TO NED-DFI-ACCOUNT
      *    THE RETURN IS FOR THE ORIGINAL ENTRY AMOUNT - THE NSF FEE
      *    CHARGED BY POSTTRAN STAYS ON THE CUSTOMER ACCOUNT
           MOVE TRAN-AMOUNT          TO NED-AMOUNT
           MOVE WS-IT-INDIV-ID       TO NED-INDIVIDUAL-ID
           MOVE WS-IT-INDIV-NAME     TO NED-INDIVIDUAL-NAME
           MOVE '1'                  TO NED-ADDENDA-IND
           MOVE WS-OUR-DFI           TO NED-TRACE-ODFI
           MOVE WS-TRACE-SEQ         TO NED-TRACE-SEQ
           WRITE ACH-RTN-REC FROM NACHA-REC-DATA
           PERFORM 8010-CHECK-RTN-WRITE

           MOVE SPACES TO NACHA-REC-DATA
           MOVE '7'                  TO NAR-REC-TYPE
           MOVE '99'                 TO NAR-ADDENDA-TYPE
           MOVE WS-IT-RETURN-CODE    TO NAR-RETURN-REASON
           MOVE WS-IT-TRACE          TO NAR-ORIG-TRACE
           MOVE WS-OUR-DFI           TO NAR-ORIG-RDFI
           MOVE WS-IT-SEC            TO NAR-INFO-SEC
           MOVE WS-IT-CO-ID          TO NAR-INFO-CO-ID
           MOVE WS-IT-CO-NAME        TO NAR-INFO-CO-NAME
           MOVE WS-IT-EFF-YYMMDD     TO NAR-INFO-EFF-DATE
           MOVE SPACES               TO NAR-TRACE-NUMBER
           STRING WS-OUR-DFI WS-TRACE-SEQ DELIMITED BY SIZE
             INTO NAR-TRACE-NUMBER
           WRITE ACH-RTN-REC FROM NACHA-REC-DATA
           PERFORM 8010-CHECK-RTN-WRITE
           MOVE SPACES TO WS-IT-DISPOSITION
           STRING 'RETURNED ' WS-IT-RETURN-CODE ' '
                  WS-IT-RETURN-DESC
                  DELIMITED BY SIZE INTO WS-IT-DISPOSITION.

      *----------------------------------------------------------------*
      * FORMAT THE NACHA RETURN FILE FROM THE COMPLETE STAGING FILE    *
      *----------------------------------------------------------------*
       5000-BUILD-RETURN-FILE.
           OPEN INPUT ACH-RTN-FILE
           MOVE 'N' TO WS-RTN-EOF WS-PENDING-FLAG
           PERFORM UNTIL WS-RTN-END
               READ ACH-RTN-FILE INTO NACHA-REC-DATA
                   AT END SET WS-RTN-END TO TRUE
                   NOT AT END PERFORM 5100-LOAD-STAGED
               END-READ
           END-PERFORM
           CLOSE ACH-RTN-FILE

           MOVE SPACES TO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           IF WS-RT-COUNT = ZERO
               MOVE 'NO RETURN ENTRIES - ACHRTNF EMPTY' TO WS-RPT-LINE
               PERFORM 8900-WRITE-REPORT
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO NACHA-REC-DATA
           MOVE '1'          TO NFH-REC-TYPE
           MOVE '01'         TO NFH-PRIORITY-CODE
           STRING ' ' WS-FED-ACH-ABA DELIMITED BY SIZE
             INTO NFH-IMMED-DEST
           STRING ' ' WS-OUR-ABA DELIMITED BY SIZE
             INTO NFH-IMMED-ORIGIN
           MOVE CTL-BUSINESS-DATE(3:6) TO WS-YYMMDD
           MOVE WS-YYMMDD    TO NFH-CREATION-DATE
           ACCEPT WS-TIME-NOW FROM TIME
           MOVE WS-TIME-NOW(1:4) TO NFH-CREATION-TIME
           MOVE 'R'          TO NFH-FILE-ID-MODIFIER
           MOVE '094'        TO NFH-RECORD-SIZE
           MOVE '10'         TO NFH-BLOCKING-FACTOR
           MOVE '1'          TO NFH-FORMAT-CODE
           MOVE WS-FED-ACH-NAME TO NFH-IMMED-DEST-NAME
           MOVE WS-OUR-NAME  TO NFH-IMMED-ORIGIN-NAME
           PERFORM 8750-WRITE-NACHA

           PERFORM VARYING WS-RT-SUB FROM 1 BY 1
                   UNTIL WS-RT-SUB > WS-RT-COUNT
               IF WS-RT-WRITTEN(WS-RT-SUB) = 'N'
                   PERFORM 5200-WRITE-BATCH
               END-IF
           END-PERFORM

           MOVE SPACES TO NACHA-REC-DATA
           ADD 1 TO WS-NF-RECORDS
           COMPUTE WS-NF-BLOCKS = (WS-NF-RECORDS + 9) / 10
           SUBTRACT 1 FROM WS-NF-RECORDS
           MOVE '9'              TO NFC-REC-TYPE
           MOVE WS-NB-BATCH-NO   TO NFC-BATCH-COUNT
           MOVE WS-NF-BLOCKS     TO NFC-BLOCK-COUNT
           MOVE WS-NF-COUNT      TO NFC-ENTRY-ADDENDA-CNT
           COMPUTE WS-HASH-10 =
               FUNCTION MOD(WS-NF-HASH, WS-HASH-MODULUS)
           MOVE WS-HASH-10       TO NFC-ENTRY-HASH
           MOVE WS-NF-DEBIT      TO NFC-TOTAL-DEBIT
           MOVE WS-NF-CREDIT     TO NFC-TOTAL-CREDIT
           PERFORM 8750-WRITE-NACHA
           PERFORM UNTIL FUNCTION MOD(WS-NF-RECORDS, 10) = 0
               MOVE ALL '9' TO NACHA-REC-DATA
               PERFORM 8750-WRITE-NACHA
           END-PERFORM

           MOVE WS-NB-BATCH-NO TO WS-ED-CNT
           MOVE WS-NF-DEBIT    TO WS-ED-AMT
           MOVE WS-NF-CREDIT   TO WS-ED-AMT2
           STRING 'ACHRTNF  BATCHES ' WS-ED-CNT
                  '  ENTRY HASH ' WS-HASH-10
                  '  DEBITS ' WS-ED-AMT '  CREDITS ' WS-ED-AMT2
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT.

      *    STAGING IS PAIRS: 6 RETURN ENTRY FOLLOWED BY ITS 7/99
       5100-LOAD-STAGED.
           EVALUATE TRUE
               WHEN NACHA-ENTRY-DETAIL
                   MOVE NACHA-REC-DATA TO WS-PENDING-DETAIL
                   MOVE 'Y' TO WS-PENDING-FLAG
               WHEN NACHA-ADDENDA AND NAR-ADDENDA-TYPE = '99'
                AND WS-PENDING-FLAG = 'Y'
                AND WS-RT-COUNT < 1000
                   ADD 1 TO WS-RT-COUNT
                   MOVE NAR-INFO-SEC     TO WS-RT-SEC(WS-RT-COUNT)
                   MOVE NAR-INFO-CO-ID   TO WS-RT-CO-ID(WS-RT-COUNT)
                   MOVE NAR-INFO-CO-NAME TO WS-RT-CO-NAME(WS-RT-COUNT)
                   MOVE NAR-INFO-EFF-DATE
                     TO WS-RT-EFF-DATE(WS-RT-COUNT)
                   MOVE WS-PENDING-DETAIL
                     TO WS-RT-DETAIL(WS-RT-COUNT)
      *            LOCAL-USE ADDENDA INFORMATION IS NOT SENT TO THE FED
                   MOVE SPACES TO NAR-ADDENDA-INFO
                   MOVE NACHA-REC-DATA TO WS-RT-ADDENDA(WS-RT-COUNT)
                   MOVE 'N' TO WS-RT-WRITTEN(WS-RT-COUNT)
                   MOVE 'N' TO WS-PENDING-FLAG
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       5200-WRITE-BATCH.
           ADD 1 TO WS-NB-BATCH-NO
           MOVE ZERO TO WS-NB-COUNT WS-NB-HASH WS-NB-DEBIT
                        WS-NB-CREDIT
           MOVE SPACES TO NACHA-REC-DATA
           MOVE '5'      TO NBH-REC-TYPE
           MOVE 200      TO NBH-SERVICE-CLASS
           MOVE WS-RT-CO-NAME(WS-RT-SUB)  TO NBH-COMPANY-NAME
           MOVE WS-RT-CO-ID(WS-RT-SUB)    TO NBH-COMPANY-ID
           MOVE WS-RT-SEC(WS-RT-SUB)      TO NBH-SEC-CODE
           MOVE 'RETURN'                  TO NBH-ENTRY-DESC
           MOVE WS-RT-EFF-DATE(WS-RT-SUB) TO NBH-EFFECTIVE-DATE
           MOVE '1'          TO NBH-ORIGINATOR-STATUS
           MOVE WS-OUR-DFI   TO NBH-ODFI-ID
           MOVE WS-NB-BATCH-NO TO NBH-BATCH-NUMBER
           PERFORM 8750-WRITE-NACHA

           PERFORM VARYING WS-RT-SUB2 FROM WS-RT-SUB BY 1
                   UNTIL WS-RT-SUB2 > WS-RT-COUNT
               IF WS-RT-WRITTEN(WS-RT-SUB2) = 'N'
                  AND WS-RT-KEY(WS-RT-SUB2) = WS-RT-KEY(WS-RT-SUB)
                   PERFORM 5300-WRITE-ENTRY
               END-IF
           END-PERFORM

           MOVE SPACES TO NACHA-REC-DATA
           MOVE '8'             TO NBC-REC-TYPE
           MOVE 200             TO NBC-SERVICE-CLASS
           MOVE WS-NB-COUNT     TO NBC-ENTRY-ADDENDA-CNT
           COMPUTE WS-HASH-10 =
               FUNCTION MOD(WS-NB-HASH, WS-HASH-MODULUS)
           MOVE WS-HASH-10      TO NBC-ENTRY-HASH
           MOVE WS-NB-DEBIT     TO NBC-TOTAL-DEBIT
           MOVE WS-NB-CREDIT    TO NBC-TOTAL-CREDIT
           MOVE WS-RT-CO-ID(WS-RT-SUB) TO NBC-COMPANY-ID
           MOVE WS-OUR-DFI      TO NBC-ODFI-ID
           MOVE WS-NB-BATCH-NO  TO NBC-BATCH-NUMBER
           PERFORM 8750-WRITE-NACHA
           ADD WS-NB-COUNT  TO WS-NF-COUNT
           ADD WS-NB-HASH   TO WS-NF-HASH
           ADD WS-NB-DEBIT  TO WS-NF-DEBIT
           ADD WS-NB-CREDIT TO WS-NF-CREDIT.

      *    21/31 ARE CREDIT-SIDE CODES, 26/36 DEBIT-SIDE CODES
       5300-WRITE-ENTRY.
           MOVE 'Y' TO WS-RT-WRITTEN(WS-RT-SUB2)
           MOVE WS-RT-DETAIL(WS-RT-SUB2) TO NACHA-REC-DATA
           PERFORM 8750-WRITE-NACHA
           ADD 1 TO WS-NB-COUNT
           MOVE NED-RDFI-ID TO WS-RDFI-NUM
           ADD WS-RDFI-NUM  TO WS-NB-HASH
           IF NED-TRAN-CODE = '21' OR NED-TRAN-CODE = '31'
               ADD NED-AMOUNT TO WS-NB-CREDIT
           ELSE
               ADD NED-AMOUNT TO WS-NB-DEBIT
           END-IF
           MOVE WS-RT-ADDENDA(WS-RT-SUB2) TO NACHA-REC-DATA
           PERFORM 8750-WRITE-NACHA
           ADD 1 TO WS-NB-COUNT.

      *----------------------------------------------------------------*
      * COMMON ROUTINES                                                *
      *----------------------------------------------------------------*
       8010-CHECK-RTN-WRITE.
           IF WS-RTN-STATUS NOT = '00'
               MOVE 'ACHRTN' TO ABND-FILE-NAME
               MOVE WS-RTN-STATUS TO ABND-FILE-STATUS
               MOVE '2500-STAGE-RETURN' TO ABND-PARAGRAPH
               MOVE 'WRITE FAILED ON ACHRTN' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF.

       8750-WRITE-NACHA.
           WRITE RETURN-REC FROM NACHA-REC-DATA
           ADD 1 TO WS-NF-RECORDS
           IF WS-RTNF-STATUS NOT = '00'
               MOVE 'ACHRTNF' TO ABND-FILE-NAME
               MOVE WS-RTNF-STATUS TO ABND-FILE-STATUS
               MOVE '8750-WRITE-NACHA' TO ABND-PARAGRAPH
               MOVE 'WRITE FAILED ON ACHRTNF' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF.

       8800-REPORT-ITEM.
           MOVE SPACES TO WS-RPT-LINE
           MOVE TRAN-AMOUNT TO WS-ED-AMT
           STRING TRAN-ACCT-NUMBER ' ' TRAN-CODE ' ' WS-ED-AMT ' '
                  TRAN-REFERENCE(1:15) ' ' TRAN-REJECT-CODE ' '
                  WS-IT-RETURN-CODE ' ' WS-IT-DISPOSITION
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT.

       8900-WRITE-REPORT.
           WRITE REPORT-REC FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE ALL '-' TO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-REJ-READ TO WS-ED-CNT
           STRING 'TRANREJ RECORDS READ         ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-ACH-SEEN TO WS-ED-CNT
           STRING '  OF WHICH ACH ITEMS         ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-STAGED-BEFORE TO WS-ED-CNT
           STRING 'RETURNS STAGED BY ACHIN      ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-RET-COUNT  TO WS-ED-CNT
           MOVE WS-RET-DR-AMT TO WS-ED-AMT
           MOVE WS-RET-CR-AMT TO WS-ED-AMT2
           STRING 'RETURNS STAGED BY ACHRET     ' WS-ED-CNT
                  '  DR ' WS-ED-AMT '  CR ' WS-ED-AMT2
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-RT-COUNT TO WS-ED-CNT
           STRING 'RETURN ENTRIES ON ACHRTNF    ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-LATE-COUNT TO WS-ED-CNT
           MOVE WS-LATE-AMT   TO WS-ED-AMT
           STRING 'LATE - NOT RETURNED          ' WS-ED-CNT
                  '     ' WS-ED-AMT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-NOTRACE-COUNT TO WS-ED-CNT
           STRING 'NO TRACE / ODFI - MANUAL     ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-MANUAL-COUNT TO WS-ED-CNT
           MOVE WS-MANUAL-AMT   TO WS-ED-AMT
           STRING 'NON-ACHIN ACH - MANUAL QUEUE ' WS-ED-CNT
                  '     ' WS-ED-AMT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-ORIG-COUNT TO WS-ED-CNT
           STRING 'OWN ORIGINATIONS REJECTED    ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT

           CLOSE RETURN-FILE REPORT-FILE
           DISPLAY 'ACHRET  REJECTS=' WS-REJ-READ
                   ' ACH=' WS-ACH-SEEN
                   ' RETURNED=' WS-RET-COUNT
                   ' ON-FILE=' WS-RT-COUNT
                   ' LATE=' WS-LATE-COUNT
                   ' MANUAL=' WS-MANUAL-COUNT
                   ' ORIGINATED=' WS-ORIG-COUNT
           IF WS-LATE-COUNT > 0 OR WS-NOTRACE-COUNT > 0
              OR WS-ORIG-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND-OPEN.
           MOVE '1000-INITIALISE' TO ABND-PARAGRAPH
           MOVE 'OPEN FAILED' TO ABND-MESSAGE
           PERFORM 9910-ABEND.

       9910-ABEND.
           MOVE 'ACHRET' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
