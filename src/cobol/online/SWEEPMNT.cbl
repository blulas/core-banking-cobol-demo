      *================================================================*
      * PROGRAM  : SWEEPMNT                                            *
      * TYPE     : CICS ONLINE - TRANSACTION CB51                      *
      * PURPOSE  : SWEEP / ZBA DEFINITION MAINTENANCE (SWEEPMST).      *
      *            ACTION I = INQUIRE, A = ADD, C = CHANGE.            *
      *            ENTER VALIDATES THE SCREEN; PF5 CONFIRMS AND WRITES *
      *            THE VALIDATED IMAGE HELD IN THE COMMAREA. ON ADD,   *
      *            THE SOURCE ACCOUNT IS LINKED TO THE SWEEP           *
      *            (ACCT-SWEEP-ID / ACCT-SWEEP-FLAG).                  *
      *   VALIDATION (SEE 3000-VALIDATE):                              *
      *     - TYPE Z/T/R, STATUS A/S/C, FREQUENCY D/W/M                *
      *     - SOURCE AND TARGET ACCOUNTS ON FILE, NOT CLOSED/FROZEN    *
      *     - Z: SOURCE MUST BE A CZBA SUB-ACCOUNT AND TARGET ITS      *
      *          PARENT (ACCT-PARENT-ACCT); PRIORITY 01-04             *
      *     - T/R: INVESTMENT ACCOUNT MUST BE CMMI, OWNED BY THE SAME  *
      *          CUSTOMER OR THE SAME CORPORATE FAMILY                 *
      *          (CUST-PARENT-CUST-ID); PRIORITY 05-99                 *
      *     - TARGET BALANCE >= 0; RANGE MIN < MAX                     *
      *     - INCREMENT IN WHOLE MULTIPLES OF $1,000                   *
      *     - MINIMUM SWEEP TRANSFER $5,000 (BLANK DEFAULTS TO $5,000) *
      * MAPSET   : SWPMNTS / SWPMNTM                                   *
      * FILES    : BANKCTL, SWEEPMST (UPDATE), ACCTMST (UPDATE),       *
      *            CUSTMST                                             *
      * COMMAREA : STATE, ACTION, VALIDATED SWEEP IMAGE (169 BYTES)    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2001-10-01 BHS  ORIGINAL - TXB CASH CONCENTRATION              *
      * 2002-03-18 BHS  INC02231 PARENT CHECK FOR ZBA                  *
      * 2004-05-17 BHS  RANGE MIN/MAX FIELDS                           *
      * 2008-11-03 RJT  CHG0977 MINIMUM SWEEP RAISED TO $5,000         *
      * 2011-04-12 DPS  CHG1544 INCREMENT, MIN TRANSFER, PRIORITY      *
      * 2012-01-09 DPS  CHG1690 INVESTMENT ACCOUNT MUST BE CMMI        *
      * 2016-07-22 AKW  INC24410 CORPORATE FAMILY (PARENT CUST) ALLOWED*
      * 2019-03-25 AKW  VIRTUAL ACCOUNTS MAY NOT CARRY A SWEEP         *
      * 2021-02-08 AKW  CHG2741 PF5 CONFIRMATION STEP                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SWEEPMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB51'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-RESP2                        PIC S9(08) COMP.
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-USERID                       PIC X(08).

      *--- SCREEN VALIDATION LIMITS ----------------------------------*
       01  WS-LIMITS.
      *    2008-11 CHG0977 - BELOW $5,000 THE TRANSFER COST EXCEEDS THE
      *    VALUE TO THE CLIENT. TREASURY SALES MAY NOT SET LOWER.
           05  WS-MIN-SWEEP-AMT            PIC S9(09)V99 COMP-3
                                           VALUE 5000.00.
           05  WS-INCREMENT-UNIT           PIC S9(09)V99 COMP-3
                                           VALUE 1000.00.
           05  WS-MAX-INCREMENT            PIC S9(09)V99 COMP-3
                                           VALUE 1000000.00.
           05  WS-ZBA-MAX-PRIORITY         PIC 9(02) VALUE 04.
           05  WS-ZBA-PRODUCT              PIC X(04) VALUE 'CZBA'.
           05  WS-INVEST-PRODUCT           PIC X(04) VALUE 'CMMI'.
      *    2004-05 BHS - BAND MUST BE AT LEAST $100,000 WIDE
      *    05  WS-MIN-BAND-WIDTH           PIC S9(13)V99 COMP-3
      *                                    VALUE 100000.00.

       01  WS-WORK.
           05  WS-ERROR-FLAG               PIC X(01).
               88  WS-ERROR                          VALUE 'Y'.
               88  WS-NO-ERROR                       VALUE 'N'.
           05  WS-NUM-WORK                 PIC S9(13)V99 COMP-3.
           05  WS-INCR-UNITS               PIC S9(09)V99 COMP-3.
           05  WS-INCR-REM                 PIC S9(09)V99 COMP-3.
           05  WS-TEXT-15                  PIC X(15).
           05  WS-SRC-CUST-ID              PIC X(10).
           05  WS-SRC-PARENT-CUST          PIC X(10).
           05  WS-TGT-CUST-ID              PIC X(10).
           05  WS-TGT-PARENT-CUST          PIC X(10).
           05  WS-SRC-PARENT-ACCT          PIC X(12).
           05  WS-SRC-SWEEP-ID             PIC X(08).
           05  WS-SRC-PRODUCT              PIC X(04).
           05  WS-TGT-PRODUCT              PIC X(04).
           05  WS-PRIORITY-9               PIC 9(02).
           05  WS-DATE-WORK                PIC 9(08).
           05  WS-HOLD-LAST-DATE           PIC 9(08).
           05  WS-HOLD-LAST-AMT            PIC S9(13)V99 COMP-3.
           05  WS-HOLD-LAST-DIR            PIC X(01).
           05  WS-HOLD-COUNT-YTD           PIC 9(05).
       01  WS-AMT-ED                       PIC -(11)9.99.
       01  WS-INC-ED                       PIC Z(7)9.99.
       01  WS-DATE-DISP.
           05  WS-DD-CCYY                  PIC 9(04).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-DD-MM                    PIC 9(02).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-DD-DD                    PIC 9(02).
       01  WS-DATE-9                       PIC 9(08).
       01  WS-DATE-X REDEFINES WS-DATE-9.
           05  WS-DX-CCYY                  PIC 9(04).
           05  WS-DX-MM                    PIC 9(02).
           05  WS-DX-DD                    PIC 9(02).
       01  WS-MESSAGE                      PIC X(79).

       01  WS-COMMAREA.
           05  CA-STATE                    PIC X(01).
               88  CA-NEW                            VALUE SPACE.
               88  CA-VALIDATED                      VALUE 'V'.
           05  CA-ACTION                   PIC X(01).
               88  CA-INQUIRE                        VALUE 'I'.
               88  CA-ADD                            VALUE 'A'.
               88  CA-CHANGE                         VALUE 'C'.
           05  CA-SWEEP-IMAGE              PIC X(167).

       COPY SWPMNTS.
       COPY BANKCTL.
       COPY SWEEPREC.
       COPY ACCTREC.
       COPY CUSTREC.
       COPY DATEPARM.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(169).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                     RIDFLD(WS-CTL-KEY) RESP(WS-RESP)
           END-EXEC
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               MOVE LOW-VALUES TO SWPMNTMO
               MOVE 'ENTER ACTION (A/C/I) AND SWEEP ID' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP-ERASE
               PERFORM 9000-RETURN
           END-IF
           MOVE DFHCOMMAREA TO WS-COMMAREA

           EVALUATE EIBAID
               WHEN DFHPF3
                   EXEC CICS XCTL PROGRAM('CBMENU')
                             RESP(WS-RESP)
                   END-EXEC
               WHEN DFHPF12
               WHEN DFHCLEAR
                   INITIALIZE WS-COMMAREA
                   MOVE LOW-VALUES TO SWPMNTMO
                   MOVE 'CANCELLED - NOTHING UPDATED' TO WS-MESSAGE
                   PERFORM 8000-SEND-MAP-ERASE
               WHEN DFHENTER
                   MOVE SPACE TO CA-STATE
                   PERFORM 1000-RECEIVE-MAP
                   PERFORM 2000-PROCESS-ACTION
               WHEN DFHPF5
                   IF CA-VALIDATED
                       PERFORM 5000-CONFIRM-UPDATE
                   ELSE
                       MOVE 'PRESS ENTER TO VALIDATE BEFORE PF5'
                         TO WS-MESSAGE
                       PERFORM 8100-SEND-DATAONLY
                   END-IF
               WHEN OTHER
                   MOVE 'INVALID KEY PRESSED' TO WS-MESSAGE
                   PERFORM 8100-SEND-DATAONLY
           END-EVALUATE
           PERFORM 9000-RETURN.

       1000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP('SWPMNTM') MAPSET('SWPMNTS')
                     INTO(SWPMNTMI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO SWPMNTMI
           END-IF
           INSPECT SWPMNTMI REPLACING ALL LOW-VALUES BY SPACES
           MOVE FUNCTION UPPER-CASE(ACTIONI) TO CA-ACTION.

      *----------------------------------------------------------------*
      * ACTION DISPATCH                                                *
      *----------------------------------------------------------------*
       2000-PROCESS-ACTION.
           IF SWPIDI = SPACES
               MOVE 'SWEEP ID IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO SWPIDL
               PERFORM 8100-SEND-DATAONLY
               PERFORM 9000-RETURN
           END-IF
           EVALUATE TRUE
               WHEN CA-INQUIRE
                   PERFORM 2100-INQUIRE
               WHEN CA-ADD
                   PERFORM 2200-ADD
               WHEN CA-CHANGE
                   PERFORM 2300-CHANGE
               WHEN OTHER
                   MOVE 'ACTION MUST BE A (ADD), C (CHANGE) OR I'
                     TO WS-MESSAGE
                   MOVE -1 TO ACTIONL
                   PERFORM 8100-SEND-DATAONLY
           END-EVALUATE.

       2100-INQUIRE.
           EXEC CICS READ FILE('SWEEPMST') INTO(SWEEP-RECORD)
                     RIDFLD(SWPIDI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NOTFND)
               MOVE 'SWEEP DEFINITION NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO SWPIDL
               PERFORM 8100-SEND-DATAONLY
           ELSE
               MOVE LOW-VALUES TO SWPMNTMO
               PERFORM 7000-RECORD-TO-SCREEN
               MOVE 'I' TO ACTIONO
               MOVE 'ENTER C TO CHANGE THIS DEFINITION' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP-ERASE
           END-IF.

       2200-ADD.
           EXEC CICS READ FILE('SWEEPMST') INTO(SWEEP-RECORD)
                     RIDFLD(SWPIDI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'SWEEP ID ALREADY EXISTS - USE ACTION C'
                 TO WS-MESSAGE
               MOVE -1 TO SWPIDL
               PERFORM 8100-SEND-DATAONLY
               PERFORM 9000-RETURN
           END-IF
           IF SWPIDI(1:3) NOT = 'SWP' OR SWPIDI(4:5) NOT NUMERIC
               MOVE 'SWEEP ID FORMAT IS SWPNNNNN' TO WS-MESSAGE
               MOVE -1 TO SWPIDL
               PERFORM 8100-SEND-DATAONLY
               PERFORM 9000-RETURN
           END-IF
           INITIALIZE SWEEP-RECORD
           MOVE SWPIDI   TO SWP-ID
           MOVE ZERO     TO SWP-LAST-SWEEP-DATE SWP-LAST-SWEEP-AMT
                            SWP-SWEEP-COUNT-YTD
           MOVE 'N'      TO SWP-LAST-SWEEP-DIR
           PERFORM 3000-VALIDATE
           PERFORM 4000-VALIDATION-RESULT.

       2300-CHANGE.
           EXEC CICS READ FILE('SWEEPMST') INTO(SWEEP-RECORD)
                     RIDFLD(SWPIDI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NOTFND)
               MOVE 'SWEEP DEFINITION NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO SWPIDL
               PERFORM 8100-SEND-DATAONLY
               PERFORM 9000-RETURN
           END-IF
      *    A CANCELLED SWEEP IS HISTORY - SET UP A NEW ID INSTEAD
           IF SWP-CANCELLED
               MOVE 'CANCELLED SWEEP CANNOT BE CHANGED - ADD A NEW ID'
                 TO WS-MESSAGE
               PERFORM 8100-SEND-DATAONLY
               PERFORM 9000-RETURN
           END-IF
           PERFORM 3000-VALIDATE
           PERFORM 4000-VALIDATION-RESULT.

      *----------------------------------------------------------------*
      * FIELD AND BUSINESS VALIDATION - SCREEN -> SWEEP-RECORD         *
      * FIRST ERROR FOUND IS REPORTED; CURSOR IS PLACED ON THE FIELD.  *
      *----------------------------------------------------------------*
       3000-VALIDATE.
           SET WS-NO-ERROR TO TRUE
           PERFORM 3100-CODES
           IF WS-NO-ERROR
               PERFORM 3200-SOURCE-ACCOUNT
           END-IF
           IF WS-NO-ERROR
               PERFORM 3300-TARGET-ACCOUNT
           END-IF
           IF WS-NO-ERROR
               PERFORM 3400-AMOUNTS
           END-IF
           IF WS-NO-ERROR
               PERFORM 3500-PRIORITY-DATES
           END-IF.

       3100-CODES.
           MOVE FUNCTION UPPER-CASE(SWPTYPI) TO SWP-TYPE
           IF NOT SWP-ZBA AND NOT SWP-TARGET AND NOT SWP-RANGE
               MOVE 'SWEEP TYPE MUST BE Z (ZBA), T (TARGET), R (RANGE)'
                 TO WS-MESSAGE
               MOVE -1 TO SWPTYPL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION UPPER-CASE(SWPSTSI) TO SWP-STATUS
           IF SWP-STATUS = SPACE
               MOVE 'A' TO SWP-STATUS
           END-IF
           IF NOT SWP-ACTIVE AND NOT SWP-SUSPENDED
              AND NOT SWP-CANCELLED
               MOVE 'STATUS MUST BE A, S OR C' TO WS-MESSAGE
               MOVE -1 TO SWPSTSL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           IF CA-ADD AND SWP-CANCELLED
               MOVE 'A NEW SWEEP CANNOT BE ADDED AS CANCELLED'
                 TO WS-MESSAGE
               MOVE -1 TO SWPSTSL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION UPPER-CASE(FREQI) TO SWP-FREQUENCY
           IF SWP-FREQUENCY = SPACE
               MOVE 'D' TO SWP-FREQUENCY
           END-IF
           IF NOT SWP-FREQ-DAILY AND NOT SWP-FREQ-WEEKLY
              AND NOT SWP-FREQ-MONTHLY
               MOVE 'FREQUENCY MUST BE D, W OR M' TO WS-MESSAGE
               MOVE -1 TO FREQL
               SET WS-ERROR TO TRUE
           END-IF.

       3200-SOURCE-ACCOUNT.
           MOVE SRCACTI TO SWP-SOURCE-ACCT
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                     RIDFLD(SWP-SOURCE-ACCT) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'SOURCE ACCOUNT NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO SRCACTL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           IF ACCT-CLOSED OR ACCT-FROZEN OR ACCT-CHARGED-OFF
              OR ACCT-ESCHEATED
               MOVE 'SOURCE ACCOUNT IS NOT OPEN FOR SWEEPS'
                 TO WS-MESSAGE
               MOVE -1 TO SRCACTL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
      *    2019-03 VIRTUAL ACCOUNTS CONCENTRATE AUTOMATICALLY (SWEEPRUN)
           IF ACCT-IS-VIRTUAL
               MOVE 'VIRTUAL ACCOUNTS CONCENTRATE TO HEADER - NO SWEEP'
                 TO WS-MESSAGE
               MOVE -1 TO SRCACTL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-PRODUCT-CODE    TO WS-SRC-PRODUCT SRCPRDO
           MOVE ACCT-PRIMARY-CUST-ID TO WS-SRC-CUST-ID
           MOVE ACCT-PARENT-ACCT     TO WS-SRC-PARENT-ACCT
           MOVE ACCT-SWEEP-ID        TO WS-SRC-SWEEP-ID
      *    ONE SWEEP PER SOURCE ACCOUNT
           IF WS-SRC-SWEEP-ID NOT = SPACES
              AND WS-SRC-SWEEP-ID NOT = SWP-ID
               STRING 'SOURCE ALREADY SWEPT BY ' DELIMITED BY SIZE
                      WS-SRC-SWEEP-ID            DELIMITED BY SIZE
                 INTO WS-MESSAGE
               MOVE -1 TO SRCACTL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE WS-SRC-CUST-ID TO SWP-CUST-ID
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(WS-SRC-CUST-ID) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL) OR NOT CUST-CORPORATE
               MOVE 'SWEEPS ARE FOR CORPORATE CUSTOMERS ONLY'
                 TO WS-MESSAGE
               MOVE -1 TO SRCACTL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE CUST-PARENT-CUST-ID TO WS-SRC-PARENT-CUST
           MOVE WS-SRC-CUST-ID      TO CUSTIDO
           MOVE CUST-LAST-NAME      TO CUSTNMO
           IF SWP-ZBA AND WS-SRC-PRODUCT NOT = WS-ZBA-PRODUCT
               MOVE 'ZBA SOURCE MUST BE A CZBA SUB-ACCOUNT'
                 TO WS-MESSAGE
               MOVE -1 TO SRCACTL
               SET WS-ERROR TO TRUE
           END-IF.

       3300-TARGET-ACCOUNT.
           MOVE TGTACTI TO SWP-TARGET-ACCT
           IF SWP-TARGET-ACCT = SWP-SOURCE-ACCT
               MOVE 'TARGET ACCOUNT CANNOT EQUAL SOURCE ACCOUNT'
                 TO WS-MESSAGE
               MOVE -1 TO TGTACTL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                     RIDFLD(SWP-TARGET-ACCT) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'TARGET ACCOUNT NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO TGTACTL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           IF NOT ACCT-ACTIVE
               MOVE 'TARGET ACCOUNT MUST BE ACTIVE' TO WS-MESSAGE
               MOVE -1 TO TGTACTL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-PRODUCT-CODE    TO WS-TGT-PRODUCT TGTPRDO
           MOVE ACCT-PRIMARY-CUST-ID TO WS-TGT-CUST-ID
           EVALUATE TRUE
      *        ZBA - TARGET IS THE SUB-ACCOUNT'S OWN PARENT
               WHEN SWP-ZBA
                   IF SWP-TARGET-ACCT NOT = WS-SRC-PARENT-ACCT
                       MOVE 'ZBA TARGET MUST BE THE PARENT ACCOUNT'
                         TO WS-MESSAGE
                       MOVE -1 TO TGTACTL
                       SET WS-ERROR TO TRUE
                   END-IF
      *        TARGET / RANGE - CHG1690 INVESTMENT VEHICLE IS CMMI ONLY
               WHEN OTHER
                   IF WS-TGT-PRODUCT NOT = WS-INVEST-PRODUCT
                       MOVE 'INVESTMENT ACCOUNT MUST BE CMMI'
                         TO WS-MESSAGE
                       MOVE -1 TO TGTACTL
                       SET WS-ERROR TO TRUE
                   ELSE
                       PERFORM 3310-SAME-FAMILY
                   END-IF
           END-EVALUATE.

      *    INC24410 - SAME CUSTOMER, OR PARENT/SUBSIDIARY, OR SISTER
      *    SUBSIDIARIES UNDER ONE PARENT (CUST-PARENT-CUST-ID)
       3310-SAME-FAMILY.
           IF WS-TGT-CUST-ID = WS-SRC-CUST-ID
               EXIT PARAGRAPH
           END-IF
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(WS-TGT-CUST-ID) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE SPACES TO WS-TGT-PARENT-CUST
           ELSE
               MOVE CUST-PARENT-CUST-ID TO WS-TGT-PARENT-CUST
           END-IF
           EVALUATE TRUE
               WHEN WS-SRC-PARENT-CUST = WS-TGT-CUST-ID
                   CONTINUE
               WHEN WS-TGT-PARENT-CUST = WS-SRC-CUST-ID
                   CONTINUE
               WHEN WS-SRC-PARENT-CUST NOT = SPACES
                AND WS-SRC-PARENT-CUST = WS-TGT-PARENT-CUST
                   CONTINUE
               WHEN OTHER
                   MOVE 'INVESTMENT ACCT NOT SAME CUSTOMER OR FAMILY'
                     TO WS-MESSAGE
                   MOVE -1 TO TGTACTL
                   SET WS-ERROR TO TRUE
           END-EVALUATE.

       3400-AMOUNTS.
           MOVE ZERO TO SWP-TARGET-BAL SWP-MIN-BAL SWP-MAX-BAL
                        SWP-INCREMENT SWP-MIN-TRANSFER
      *    ZBA - ALWAYS SWEPT TO EXACTLY ZERO, NO BAND OR INCREMENT
           IF SWP-ZBA
               EXIT PARAGRAPH
           END-IF
           MOVE TGTBALI TO WS-TEXT-15
           PERFORM 3900-NUMERIC-AMOUNT
           IF WS-ERROR
               MOVE 'TARGET BALANCE IS NOT A VALID AMOUNT' TO WS-MESSAGE
               MOVE -1 TO TGTBALL
               EXIT PARAGRAPH
           END-IF
           IF WS-NUM-WORK < 0
               MOVE 'TARGET BALANCE MUST BE ZERO OR GREATER'
                 TO WS-MESSAGE
               MOVE -1 TO TGTBALL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE WS-NUM-WORK TO SWP-TARGET-BAL

           IF SWP-RANGE
               MOVE MINBALI TO WS-TEXT-15
               PERFORM 3900-NUMERIC-AMOUNT
               IF WS-ERROR OR WS-NUM-WORK < 0
                   MOVE 'RANGE MINIMUM IS NOT A VALID AMOUNT'
                     TO WS-MESSAGE
                   MOVE -1 TO MINBALL
                   SET WS-ERROR TO TRUE
                   EXIT PARAGRAPH
               END-IF
               MOVE WS-NUM-WORK TO SWP-MIN-BAL
               MOVE MAXBALI TO WS-TEXT-15
               PERFORM 3900-NUMERIC-AMOUNT
               IF WS-ERROR
                   MOVE 'RANGE MAXIMUM IS NOT A VALID AMOUNT'
                     TO WS-MESSAGE
                   MOVE -1 TO MAXBALL
                   EXIT PARAGRAPH
               END-IF
               MOVE WS-NUM-WORK TO SWP-MAX-BAL
               IF SWP-MIN-BAL NOT < SWP-MAX-BAL
                   MOVE 'RANGE MINIMUM MUST BE LESS THAN MAXIMUM'
                     TO WS-MESSAGE
                   MOVE -1 TO MINBALL
                   SET WS-ERROR TO TRUE
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *    CHG1544 - INCREMENT IN WHOLE THOUSANDS, $1,000 TO $1,000,000
           MOVE INCRMTI TO WS-TEXT-15
           PERFORM 3900-NUMERIC-AMOUNT
           IF WS-ERROR
               MOVE 'INCREMENT IS NOT A VALID AMOUNT' TO WS-MESSAGE
               MOVE -1 TO INCRMTL
               EXIT PARAGRAPH
           END-IF
           IF WS-NUM-WORK = 0
               MOVE WS-INCREMENT-UNIT TO WS-NUM-WORK
           END-IF
           DIVIDE WS-NUM-WORK BY WS-INCREMENT-UNIT
               GIVING WS-INCR-UNITS REMAINDER WS-INCR-REM
           IF WS-INCR-REM NOT = 0
              OR WS-NUM-WORK < WS-INCREMENT-UNIT
              OR WS-NUM-WORK > WS-MAX-INCREMENT
               MOVE 'INCREMENT MUST BE A MULTIPLE OF $1,000 (MAX $1M)'
                 TO WS-MESSAGE
               MOVE -1 TO INCRMTL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE WS-NUM-WORK TO SWP-INCREMENT

      *    CHG0977 - MINIMUM SWEEP TRANSFER $5,000; BLANK = $5,000
           MOVE MINXFRI TO WS-TEXT-15
           PERFORM 3900-NUMERIC-AMOUNT
           IF WS-ERROR
               MOVE 'MIN TRANSFER IS NOT A VALID AMOUNT' TO WS-MESSAGE
               MOVE -1 TO MINXFRL
               EXIT PARAGRAPH
           END-IF
           IF WS-NUM-WORK = 0
               MOVE WS-MIN-SWEEP-AMT TO WS-NUM-WORK
           END-IF
           IF WS-NUM-WORK < WS-MIN-SWEEP-AMT
               MOVE 'MINIMUM SWEEP TRANSFER IS $5,000' TO WS-MESSAGE
               MOVE -1 TO MINXFRL
               SET WS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE WS-NUM-WORK TO SWP-MIN-TRANSFER.

       3500-PRIORITY-DATES.
      *    ZBA SUBS MUST SWEEP BEFORE THE PARENT'S TARGET SWEEP
           IF PRTYI = SPACES
               IF SWP-ZBA
                   MOVE 01 TO WS-PRIORITY-9
               ELSE
                   MOVE 05 TO WS-PRIORITY-9
               END-IF
           ELSE
               IF PRTYI NOT NUMERIC
                   MOVE 'PRIORITY MUST BE 01-99' TO WS-MESSAGE
                   MOVE -1 TO PRTYL
                   SET WS-ERROR TO TRUE
                   EXIT PARAGRAPH
               END-IF
               MOVE PRTYI TO WS-PRIORITY-9
           END-IF
           EVALUATE TRUE
               WHEN WS-PRIORITY-9 = 0
                   MOVE 'PRIORITY MUST BE 01-99' TO WS-MESSAGE
                   MOVE -1 TO PRTYL
                   SET WS-ERROR TO TRUE
               WHEN SWP-ZBA AND WS-PRIORITY-9 > WS-ZBA-MAX-PRIORITY
                   MOVE 'ZBA PRIORITY MUST BE 01-04' TO WS-MESSAGE
                   MOVE -1 TO PRTYL
                   SET WS-ERROR TO TRUE
               WHEN NOT SWP-ZBA
                AND WS-PRIORITY-9 NOT > WS-ZBA-MAX-PRIORITY
                   MOVE 'TARGET/RANGE PRIORITY MUST BE 05-99'
                     TO WS-MESSAGE
                   MOVE -1 TO PRTYL
                   SET WS-ERROR TO TRUE
           END-EVALUATE
           IF WS-ERROR
               EXIT PARAGRAPH
           END-IF
           MOVE WS-PRIORITY-9 TO SWP-PRIORITY

      *    EFFECTIVE DATE - BLANK = NEXT BUSINESS DAY; ADD NOT BACKDATED
           IF EFFDTI = SPACES
               MOVE CTL-NEXT-BUS-DATE TO SWP-EFF-DATE
           ELSE
               IF EFFDTI NOT NUMERIC
                   MOVE 'EFFECTIVE DATE MUST BE CCYYMMDD' TO WS-MESSAGE
                   MOVE -1 TO EFFDTL
                   SET WS-ERROR TO TRUE
                   EXIT PARAGRAPH
               END-IF
               MOVE EFFDTI TO WS-DATE-WORK
               PERFORM 3950-VALID-DATE
               IF WS-ERROR
                   MOVE 'EFFECTIVE DATE IS NOT A VALID DATE'
                     TO WS-MESSAGE
                   MOVE -1 TO EFFDTL
                   EXIT PARAGRAPH
               END-IF
               IF CA-ADD AND WS-DATE-WORK < CTL-BUSINESS-DATE
                   MOVE 'EFFECTIVE DATE CANNOT BE BEFORE BUSINESS DATE'
                     TO WS-MESSAGE
                   MOVE -1 TO EFFDTL
                   SET WS-ERROR TO TRUE
                   EXIT PARAGRAPH
               END-IF
               MOVE WS-DATE-WORK TO SWP-EFF-DATE
           END-IF
           IF EXPDTI = SPACES OR EXPDTI = '00000000'
               MOVE ZERO TO SWP-EXP-DATE
           ELSE
               IF EXPDTI NOT NUMERIC
                   MOVE 'EXPIRY DATE MUST BE CCYYMMDD' TO WS-MESSAGE
                   MOVE -1 TO EXPDTL
                   SET WS-ERROR TO TRUE
                   EXIT PARAGRAPH
               END-IF
               MOVE EXPDTI TO WS-DATE-WORK
               PERFORM 3950-VALID-DATE
               IF WS-ERROR OR WS-DATE-WORK NOT > SWP-EFF-DATE
                   MOVE 'EXPIRY DATE MUST BE AFTER EFFECTIVE DATE'
                     TO WS-MESSAGE
                   MOVE -1 TO EXPDTL
                   SET WS-ERROR TO TRUE
                   EXIT PARAGRAPH
               END-IF
               MOVE WS-DATE-WORK TO SWP-EXP-DATE
           END-IF.

       3900-NUMERIC-AMOUNT.
           MOVE ZERO TO WS-NUM-WORK
           IF WS-TEXT-15 = SPACES
               EXIT PARAGRAPH
           END-IF
           IF FUNCTION TEST-NUMVAL(WS-TEXT-15) NOT = 0
               SET WS-ERROR TO TRUE
           ELSE
               COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(WS-TEXT-15)
           END-IF.

       3950-VALID-DATE.
           INITIALIZE DATE-PARMS
           SET DTU-VALIDATE TO TRUE
           MOVE WS-DATE-WORK TO DTU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DTU-RESULT-YES
               SET WS-ERROR TO TRUE
           END-IF.

      *----------------------------------------------------------------*
      * VALIDATION OUTCOME - HOLD THE IMAGE FOR PF5                    *
      *----------------------------------------------------------------*
       4000-VALIDATION-RESULT.
           IF WS-ERROR
               MOVE SPACE TO CA-STATE
               PERFORM 8100-SEND-DATAONLY
           ELSE
               MOVE SWEEP-RECORD TO CA-SWEEP-IMAGE
               SET CA-VALIDATED TO TRUE
               MOVE LOW-VALUES TO SWPMNTMO
               PERFORM 7000-RECORD-TO-SCREEN
               MOVE CA-ACTION TO ACTIONO
               MOVE 'VALIDATED - PRESS PF5 TO CONFIRM, PF12 TO CANCEL'
                 TO WS-MESSAGE
               PERFORM 8000-SEND-MAP-ERASE
           END-IF.

      *----------------------------------------------------------------*
      * PF5 - WRITE / REWRITE SWEEPMST, LINK SOURCE ACCOUNT            *
      *----------------------------------------------------------------*
       5000-CONFIRM-UPDATE.
           MOVE CA-SWEEP-IMAGE    TO SWEEP-RECORD
           MOVE CTL-BUSINESS-DATE TO SWP-LAST-MAINT-DATE
           MOVE WS-USERID         TO SWP-LAST-MAINT-USER
           IF CA-ADD
               EXEC CICS WRITE FILE('SWEEPMST') FROM(SWEEP-RECORD)
                         RIDFLD(SWP-ID) RESP(WS-RESP)
               END-EXEC
               IF WS-RESP = DFHRESP(DUPREC)
                   MOVE 'SWEEP ID ADDED BY ANOTHER USER - NOT WRITTEN'
                     TO WS-MESSAGE
                   PERFORM 5900-RESET-AND-SEND
                   EXIT PARAGRAPH
               END-IF
           ELSE
               PERFORM 5100-REWRITE-SWEEP
               IF WS-RESP NOT = DFHRESP(NORMAL)
                   EXIT PARAGRAPH
               END-IF
           END-IF
           PERFORM 5200-LINK-SOURCE
           MOVE LOW-VALUES TO SWPMNTMO
           PERFORM 7000-RECORD-TO-SCREEN
           IF CA-ADD
               MOVE 'SWEEP DEFINITION ADDED - EFFECTIVE NEXT EOD'
                 TO WS-MESSAGE
           ELSE
               MOVE 'SWEEP DEFINITION CHANGED' TO WS-MESSAGE
           END-IF
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-MAP-ERASE.

      *    PRESERVE THE SWEEP HISTORY FIELDS MAINTAINED BY SWEEPRUN
       5100-REWRITE-SWEEP.
           EXEC CICS READ FILE('SWEEPMST') INTO(SWEEP-RECORD)
                     RIDFLD(CA-SWEEP-IMAGE(1:8)) UPDATE RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'SWEEP RECORD NO LONGER ON FILE' TO WS-MESSAGE
               PERFORM 5900-RESET-AND-SEND
               EXIT PARAGRAPH
           END-IF
           MOVE SWP-LAST-SWEEP-DATE TO WS-HOLD-LAST-DATE
           MOVE SWP-LAST-SWEEP-AMT  TO WS-HOLD-LAST-AMT
           MOVE SWP-LAST-SWEEP-DIR  TO WS-HOLD-LAST-DIR
           MOVE SWP-SWEEP-COUNT-YTD TO WS-HOLD-COUNT-YTD
           MOVE CA-SWEEP-IMAGE      TO SWEEP-RECORD
           MOVE WS-HOLD-LAST-DATE   TO SWP-LAST-SWEEP-DATE
           MOVE WS-HOLD-LAST-AMT    TO SWP-LAST-SWEEP-AMT
           MOVE WS-HOLD-LAST-DIR    TO SWP-LAST-SWEEP-DIR
           MOVE WS-HOLD-COUNT-YTD   TO SWP-SWEEP-COUNT-YTD
           MOVE CTL-BUSINESS-DATE   TO SWP-LAST-MAINT-DATE
           MOVE WS-USERID           TO SWP-LAST-MAINT-USER
           EXEC CICS REWRITE FILE('SWEEPMST') FROM(SWEEP-RECORD)
                     RESP(WS-RESP)
           END-EXEC.

       5200-LINK-SOURCE.
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                     RIDFLD(SWP-SOURCE-ACCT) UPDATE RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           IF SWP-CANCELLED
               MOVE SPACES TO ACCT-SWEEP-ID
               MOVE 'N'    TO ACCT-SWEEP-FLAG
           ELSE
               MOVE SWP-ID TO ACCT-SWEEP-ID
               MOVE 'Y'    TO ACCT-SWEEP-FLAG
           END-IF
           EXEC CICS REWRITE FILE('ACCTMST') FROM(ACCOUNT-RECORD)
                     RESP(WS-RESP)
           END-EXEC.

       5900-RESET-AND-SEND.
           INITIALIZE WS-COMMAREA
           PERFORM 8100-SEND-DATAONLY.

      *----------------------------------------------------------------*
      * SWEEP-RECORD -> SCREEN                                         *
      *----------------------------------------------------------------*
       7000-RECORD-TO-SCREEN.
           MOVE SWP-ID            TO SWPIDO
           MOVE SWP-TYPE          TO SWPTYPO
           MOVE SWP-STATUS        TO SWPSTSO
           MOVE SWP-CUST-ID       TO CUSTIDO
           MOVE SWP-SOURCE-ACCT   TO SRCACTO
           MOVE SWP-TARGET-ACCT   TO TGTACTO
           MOVE SWP-TARGET-BAL    TO WS-AMT-ED
           MOVE WS-AMT-ED         TO TGTBALO
           MOVE SWP-MIN-BAL       TO WS-AMT-ED
           MOVE WS-AMT-ED         TO MINBALO
           MOVE SWP-MAX-BAL       TO WS-AMT-ED
           MOVE WS-AMT-ED         TO MAXBALO
           MOVE SWP-INCREMENT     TO WS-INC-ED
           MOVE WS-INC-ED         TO INCRMTO
           MOVE SWP-MIN-TRANSFER  TO WS-INC-ED
           MOVE WS-INC-ED         TO MINXFRO
           MOVE SWP-FREQUENCY     TO FREQO
           MOVE SWP-PRIORITY      TO PRTYO
           MOVE SWP-EFF-DATE      TO EFFDTO
           MOVE SWP-EXP-DATE      TO EXPDTO
           IF SWP-LAST-SWEEP-DATE > 0
               MOVE SWP-LAST-SWEEP-DATE TO WS-DATE-9
               PERFORM 8300-FORMAT-DATE
               MOVE WS-DATE-DISP        TO LSTDTO
               MOVE SWP-LAST-SWEEP-AMT  TO WS-AMT-ED
               MOVE WS-AMT-ED           TO LSTAMTO
               MOVE SWP-LAST-SWEEP-DIR  TO LSTDIRO
           END-IF
           MOVE SWP-LAST-MAINT-USER TO MNTUSRO
           IF SWP-LAST-MAINT-DATE > 0
               MOVE SWP-LAST-MAINT-DATE TO WS-DATE-9
               PERFORM 8300-FORMAT-DATE
               MOVE WS-DATE-DISP        TO MNTDTO
           END-IF
           IF SWP-CUST-ID NOT = SPACES
               EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                         RIDFLD(SWP-CUST-ID) RESP(WS-RESP)
               END-EXEC
               IF WS-RESP = DFHRESP(NORMAL)
                   MOVE CUST-LAST-NAME TO CUSTNMO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       8000-SEND-MAP-ERASE.
           PERFORM 8200-COMMON-FIELDS
           EXEC CICS SEND MAP('SWPMNTM') MAPSET('SWPMNTS')
                     FROM(SWPMNTMO) ERASE CURSOR RESP(WS-RESP)
           END-EXEC.

       8100-SEND-DATAONLY.
           PERFORM 8200-COMMON-FIELDS
           EXEC CICS SEND MAP('SWPMNTM') MAPSET('SWPMNTS')
                     FROM(SWPMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.

       8200-COMMON-FIELDS.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-9) RESP(WS-RESP)
           END-EXEC
           PERFORM 8300-FORMAT-DATE
           MOVE WS-DATE-DISP TO CURDATEO
           MOVE WS-MESSAGE   TO MSGO.

       8300-FORMAT-DATE.
           MOVE WS-DX-CCYY TO WS-DD-CCYY
           MOVE WS-DX-MM   TO WS-DD-MM
           MOVE WS-DX-DD   TO WS-DD-DD.

       9000-RETURN.
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                     COMMAREA(WS-COMMAREA) LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
