      *================================================================*
      * PROGRAM  : PAYPROC                                             *
      * TYPE     : BATCH - EOD STEP 03                                 *
      * PURPOSE  : RELEASES DUE PAYMENT INSTRUCTIONS FROM PAYMST.      *
      *            DUE = STATUS APPROVED AND VALUE DATE <= BUSINESS    *
      *            DATE (ONE-TIME) OR NEXT DATE <= BUSINESS DATE       *
      *            (RECURRING). FOR EACH DUE INSTRUCTION:              *
      *              1. DEBIT ACCOUNT MUST BE ON FILE AND ACTIVE       *
      *              2. RE-SCREEN BENEFICIARY (OFACSCRN, DD SDNLIST)   *
      *                 HIT -> REJECTED + OFAC ALERT, POTENTIAL -> HELD*
      *              3. RE-CHECK LIMITS (PAYLIMIT) - DECLINE REJECTS,  *
      *                 DUAL APPROVAL WITHOUT A CHECKER HOLDS          *
      *              4. RE-CHECK CUTOFF (CUTOFF) - PAST CUTOFF ROLLS   *
      *                 THE VALUE DATE, SDACH MAY DOWNGRADE TO ACH     *
      *              5. GENERATE PAYTRAN DEBIT (WACH / WWIR / WXFR),   *
      *                 BOOK CREDIT SIDE (DXFR) AND WIRE FEE (WFWR)    *
      *              6. ACH: ENTRY ON OUTBOUND NACHA FILE (ACHOUTB)    *
      *                 WIRE: FEDWIRE MESSAGE (FEDWOUT)                *
      *              7. UPDATE PAYMST: STATUS / NEXT DATE (REWRITE)    *
      * INPUT    : PAYMST (I-O), ACCTMST, CUSTMST, SDNLIST, BANKCTL    *
      * OUTPUT   : PAYTRAN (TRANREC, TRANEDIT INPUT), ACHOUTB, FEDWOUT,*
      *            ALERTS (EXTEND), PAYRPT                             *
      * TRAN IDS : BUSINESS DATE + SEQ 15000001 UPWARDS                *
      * RC       : 00 OK, 04 ONE OR MORE OFAC HITS / REJECTIONS        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - BOOK AND WIRE ORDERS                *
      * 1996-02-12 RJT  ACH ORIGINATION FILE                           *
      * 2002-03-15 BHS  OFAC RE-SCREEN AT RELEASE                      *
      * 2004-11-15 BHS  FEDWIRE FAIM TAGS                              *
      * 2006-10-02 BHS  SOX 404 - CHECKER MUST DIFFER FROM MAKER       *
      * 2009-04-20 DPS  INTERNATIONAL WIRES VIA CORRESPONDENT          *
      * 2012-05-14 DPS  PAYLIMIT RE-CHECK AT RELEASE (INC11873)        *
      * 2016-09-23 DPS  SAME-DAY ACH BATCHES                           *
      * 2019-06-03 AKW  MULTI-ENTRY ACH BATCHES BY ORIGINATOR          *
      * 2021-02-08 AKW  COMPLIANCE HOLD FOR HIGH-RISK COUNTRIES        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYPROC.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYMENT-FILE ASSIGN TO "PAYMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PAY-ID
               ALTERNATE RECORD KEY IS PAY-DEBIT-ACCT
                   WITH DUPLICATES
               FILE STATUS IS WS-PAY-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           SELECT SDN-FILE ASSIGN TO "SDNLIST"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SDN-STATUS.
           SELECT PAY-TRAN-FILE ASSIGN TO "PAYTRAN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-STATUS.
           SELECT ACH-OUT-FILE ASSIGN TO "ACHOUTB"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACHO-STATUS.
           SELECT FEDWIRE-FILE ASSIGN TO "FEDWOUT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FEDW-STATUS.
           SELECT REPORT-FILE ASSIGN TO "PAYRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PAYMENT-FILE.
       COPY PAYREC.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  SDN-FILE.
       01  SDN-LINE                        PIC X(200).
       FD  PAY-TRAN-FILE.
       COPY TRANREC.
       FD  ACH-OUT-FILE.
       01  ACH-OUT-REC                     PIC X(94).
       FD  FEDWIRE-FILE.
       01  FEDWIRE-REC                     PIC X(100).
       FD  REPORT-FILE.
       01  REPORT-REC                      PIC X(132).
       FD  ALERT-FILE.
       COPY ALERTREC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-PAY-STATUS               PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-SDN-STATUS               PIC X(02).
           05  WS-TRAN-STATUS              PIC X(02).
           05  WS-ACHO-STATUS              PIC X(02).
           05  WS-FEDW-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
       01  WS-FLAGS.
           05  WS-PAY-EOF                  PIC X(01) VALUE 'N'.
               88  WS-PAY-END                        VALUE 'Y'.
           05  WS-SDN-EOF                  PIC X(01) VALUE 'N'.
               88  WS-SDN-END                        VALUE 'Y'.
           05  WS-OUTCOME                  PIC X(01).
               88  WS-OUT-RELEASE                    VALUE 'L'.
               88  WS-OUT-REJECT                     VALUE 'R'.
               88  WS-OUT-HOLD                       VALUE 'H'.
               88  WS-OUT-ROLL                       VALUE 'D'.

       01  WS-CONSTANTS.
           05  WS-OUR-ABA                  PIC X(09) VALUE '026000013'.
           05  WS-OUR-DFI                  PIC X(08) VALUE '02600001'.
           05  WS-OUR-SHORT-NAME           PIC X(18)
                                           VALUE 'GRANITE ST BK'.
           05  WS-OUR-NAME                 PIC X(23)
                                   VALUE 'GRANITE STREET BANK NA'.
           05  WS-FED-ACH-ABA              PIC X(09) VALUE '076000005'.
           05  WS-FED-ACH-NAME             PIC X(23)
                                           VALUE 'FRB ACH OPERATOR'.
      *    USD CORRESPONDENT FOR INTERNATIONAL WIRES
           05  WS-CORRESP-ABA              PIC X(09) VALUE '026009991'.
           05  WS-CORRESP-NAME             PIC X(18)
                                           VALUE 'FICT CORRESP NY'.
           05  WS-FEDWIRE-LTERM            PIC X(08) VALUE 'GSBKQ001'.
           05  WS-SEQ-BASE                 PIC 9(08) VALUE 15000000.
      *    OUTGOING WIRE FEES - SEE FEE SCHEDULE FS-2004-03
           05  WS-WIRE-FEE-DOMESTIC        PIC S9(07)V99 COMP-3
                                           VALUE 25.00.
           05  WS-WIRE-FEE-INTL            PIC S9(07)V99 COMP-3
                                           VALUE 45.00.
           05  WS-HASH-MODULUS             PIC 9(11) VALUE 10000000000.

      *--- COMPANY ENTRY DESCRIPTION BY PURPOSE CODE ------------------*
       01  WS-PURPOSE-VALUES.
           05  FILLER PIC X(14) VALUE 'SALAPAYROLL   '.
           05  FILLER PIC X(14) VALUE 'PENS401K      '.
           05  FILLER PIC X(14) VALUE 'TAXSTAX PMT   '.
           05  FILLER PIC X(14) VALUE 'SUPPVENDOR PMT'.
           05  FILLER PIC X(14) VALUE 'RENTRENT      '.
           05  FILLER PIC X(14) VALUE 'SUBSMEMBERSHIP'.
           05  FILLER PIC X(14) VALUE 'FAMITRANSFER  '.
       01  WS-PURPOSE-TABLE REDEFINES WS-PURPOSE-VALUES.
           05  WS-PURP-ENTRY OCCURS 7 TIMES INDEXED BY PURP-IDX.
               10  WS-PURP-CODE            PIC X(04).
               10  WS-PURP-DESC            PIC X(10).

      *--- DAILY USAGE BY DEBIT ACCOUNT (FOR PAYLIMIT) ----------------*
       01  WS-USAGE-TABLE.
           05  WS-USE-COUNT                PIC 9(03) VALUE 0.
           05  WS-USE-ENTRY OCCURS 200 TIMES INDEXED BY USE-IDX.
               10  WS-USE-ACCT             PIC X(12).
               10  WS-USE-ACH              PIC S9(13)V99 COMP-3.
               10  WS-USE-WIRE             PIC S9(13)V99 COMP-3.
               10  WS-USE-BOOK             PIC S9(13)V99 COMP-3.
       01  WS-USE-SUB                      PIC 9(03).

      *--- OUTBOUND ACH ENTRIES, WRITTEN AT END BY BATCH KEY ----------*
       01  WS-ACH-TABLE.
           05  WS-AE-COUNT                 PIC 9(04) VALUE 0.
           05  WS-AE-ENTRY OCCURS 500 TIMES.
               10  WS-AE-KEY.
                   15  WS-AE-ORIG-ACCT     PIC X(12).
                   15  WS-AE-SEC           PIC X(03).
                   15  WS-AE-SETTLE-DATE   PIC 9(08).
                   15  WS-AE-SD-FLAG       PIC X(01).
               10  WS-AE-CO-NAME           PIC X(16).
               10  WS-AE-CO-ID             PIC X(10).
               10  WS-AE-ENTRY-DESC        PIC X(10).
               10  WS-AE-TRAN-CODE         PIC X(02).
               10  WS-AE-ROUTING           PIC X(09).
               10  WS-AE-DFI-ACCT          PIC X(17).
               10  WS-AE-AMOUNT            PIC 9(08)V99.
               10  WS-AE-INDIV-ID          PIC X(15).
               10  WS-AE-NAME              PIC X(22).
               10  WS-AE-REMIT             PIC X(80).
               10  WS-AE-TRACE-SEQ         PIC 9(07).
               10  WS-AE-WRITTEN           PIC X(01).
       01  WS-AE-SUB                       PIC 9(04).
       01  WS-AE-SUB2                      PIC 9(04).
       01  WS-ACH-TRACE-SEQ                PIC 9(07) VALUE 0.
       01  WS-NACHA-TOTALS.
           05  WS-NB-COUNT                 PIC 9(06).
           05  WS-NB-HASH                  PIC 9(15).
           05  WS-NB-CREDIT                PIC 9(10)V99.
           05  WS-NB-BATCH-NO              PIC 9(07) VALUE 0.
           05  WS-NF-COUNT                 PIC 9(08) VALUE 0.
           05  WS-NF-HASH                  PIC 9(15) VALUE 0.
           05  WS-NF-CREDIT                PIC 9(10)V99 VALUE 0.
           05  WS-NF-RECORDS               PIC 9(08) VALUE 0.
           05  WS-NF-BLOCKS                PIC 9(06) VALUE 0.
           05  WS-HASH-10                  PIC 9(10).
           05  WS-RDFI-NUM                 PIC 9(08).
       COPY NACHAREC.
       COPY FEDWREC.

      *--- CURRENT PAYMENT WORK ---------------------------------------*
       01  WS-PAY-WORK.
           05  WS-DUE-DATE                 PIC 9(08).
           05  WS-REQ-HHMM                 PIC 9(04).
           05  WS-EFF-TYPE                 PIC X(01).
           05  WS-SETTLE-DATE              PIC 9(08).
           05  WS-ACTION                   PIC X(10).
           05  WS-DETAIL                   PIC X(70).
           05  WS-DETAIL-SAVE              PIC X(70).
           05  WS-ALRT-SEV                 PIC X(01).
           05  WS-ALRT-RULE                PIC X(08).
           05  WS-FEE                      PIC S9(07)V99 COMP-3.
           05  WS-DR-COST-CENTER           PIC X(05).
           05  WS-DR-ANALYSIS              PIC X(01).
           05  WS-DR-OPEN-DATE             PIC 9(08).
           05  WS-DR-ACH-LIMIT             PIC S9(13)V99 COMP-3.
           05  WS-DR-WIRE-LIMIT            PIC S9(13)V99 COMP-3.
           05  WS-CUST-NAME                PIC X(35).
           05  WS-CUST-FOUND               PIC X(01).
           05  WS-BENE-COUNTRY             PIC X(02).
           05  WS-APPROVER                 PIC X(08).
           05  WS-IMAD                     PIC X(22).
           05  WS-DEBIT-TRAN-ID            PIC X(16).

      *--- TRANSACTION BUILD AREA (8100-) -----------------------------*
       01  WS-TX.
           05  WS-TX-ACCT                  PIC X(12).
           05  WS-TX-CODE                  PIC X(04).
           05  WS-TX-AMOUNT                PIC S9(13)V99 COMP-3.
           05  WS-TX-CHANNEL               PIC X(03).
           05  WS-TX-DESC                  PIC X(40).
           05  WS-TX-COST-CENTER           PIC X(05).
           05  WS-TX-FEE                   PIC S9(07)V99 COMP-3.

       01  WS-COUNTERS.
           05  WS-TRAN-SEQ                 PIC 9(08).
           05  WS-IMAD-SEQ                 PIC 9(06) VALUE 0.
           05  WS-READ-COUNT               PIC 9(05) VALUE 0.
           05  WS-DUE-COUNT                PIC 9(05) VALUE 0.
           05  WS-RELEASE-COUNT            PIC 9(05) VALUE 0.
           05  WS-REJECT-COUNT             PIC 9(05) VALUE 0.
           05  WS-HOLD-COUNT               PIC 9(05) VALUE 0.
           05  WS-ROLL-COUNT               PIC 9(05) VALUE 0.
           05  WS-OFAC-HIT-COUNT           PIC 9(05) VALUE 0.
           05  WS-TRAN-COUNT               PIC 9(05) VALUE 0.
           05  WS-WIRE-COUNT               PIC 9(05) VALUE 0.
           05  WS-RELEASE-AMT              PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-FEE-AMT                  PIC S9(13)V99 COMP-3
                                           VALUE 0.

      *--- SDN LOAD ---------------------------------------------------*
       01  WS-SDN-FIELDS.
           05  WS-SF-ENTNUM                PIC X(10).
           05  WS-SF-TYPE                  PIC X(01).
           05  WS-SF-NAME                  PIC X(60).
           05  WS-SF-COUNTRY               PIC X(02).
           05  WS-SF-PROGRAM               PIC X(10).

      *--- CHECK DIGIT ------------------------------------------------*
       01  WS-CD-WORK.
           05  WS-CD-ROUTING               PIC 9(08).
           05  WS-CD-ROUTING-R REDEFINES WS-CD-ROUTING.
               10  WS-CD-DIGIT             PIC 9(01) OCCURS 8.
           05  WS-CD-SUM                   PIC 9(04).
           05  WS-CD-RESULT                PIC 9(01).
           05  WS-CD-I                     PIC 9(02).
       01  WS-CD-WEIGHTS-V                 PIC X(08) VALUE '37137137'.
       01  WS-CD-WEIGHTS REDEFINES WS-CD-WEIGHTS-V.
           05  WS-CD-WEIGHT                PIC 9(01) OCCURS 8.

       01  WS-RPT-LINE                     PIC X(132).
       01  WS-ED-AMT                       PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-ED-CNT                       PIC ZZ,ZZ9.
       01  WS-ED-SCORE                     PIC ZZ9.
       01  WS-TIME-NOW                     PIC 9(08).
       01  WS-YYMMDD                       PIC 9(06).

       COPY TRANCODE.
       COPY OFSPARM.
       COPY SDNTBL.
       COPY PLMPARM.
       COPY CUTPARM.
       COPY AVLPARM.
       COPY DATEPARM.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 1500-LOAD-SDN-TABLE
           MOVE LOW-VALUES TO PAY-ID
           START PAYMENT-FILE KEY NOT < PAY-ID
               INVALID KEY SET WS-PAY-END TO TRUE
           END-START
           PERFORM UNTIL WS-PAY-END
               READ PAYMENT-FILE NEXT RECORD
                   AT END SET WS-PAY-END TO TRUE
                   NOT AT END PERFORM 2000-PROCESS-PAYMENT
               END-READ
           END-PERFORM
           PERFORM 7000-WRITE-NACHA-FILE
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN I-O    PAYMENT-FILE
           OPEN INPUT  ACCOUNT-FILE CUSTOMER-FILE SDN-FILE
           OPEN OUTPUT PAY-TRAN-FILE ACH-OUT-FILE FEDWIRE-FILE
                       REPORT-FILE
           OPEN EXTEND ALERT-FILE
           IF WS-PAY-STATUS NOT = '00'
               MOVE 'PAYMST' TO ABND-FILE-NAME
               MOVE WS-PAY-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           IF WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           IF WS-SDN-STATUS NOT = '00'
               MOVE 'SDNLIST' TO ABND-FILE-NAME
               MOVE WS-SDN-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           IF WS-TRAN-STATUS NOT = '00' OR WS-ACHO-STATUS NOT = '00'
              OR WS-FEDW-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
               MOVE 'PAYTRAN' TO ABND-FILE-NAME
               MOVE WS-TRAN-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           IF WS-ALRT-STATUS NOT = '00' AND WS-ALRT-STATUS NOT = '05'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           MOVE WS-SEQ-BASE TO WS-TRAN-SEQ
           MOVE SPACES TO WS-RPT-LINE
           STRING 'PAYPROC  PAYMENT RELEASE   BUSINESS DATE '
                  CTL-BUSINESS-DATE
                  '   CUTOFFS ACH ' CTL-ACH-CUTOFF-TIME
                  ' SDACH ' CTL-SDACH-CUTOFF-TIME
                  ' WIRE ' CTL-WIRE-CUTOFF-TIME
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE ALL '-' TO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           STRING 'PAY-ID       T DEBIT ACCT           AMOUNT ACTION   '
                  '  DETAIL'
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT.

      *----------------------------------------------------------------*
      * SDN TABLE - LOADED ONCE, NORMALISED BY OFACSCRN ON FIRST CALL  *
      *----------------------------------------------------------------*
       1500-LOAD-SDN-TABLE.
           INITIALIZE SDN-TABLE
           MOVE 500 TO SDNT-MAX-ENTRIES
           MOVE 'N' TO SDNT-PREPARED-FLAG
           PERFORM UNTIL WS-SDN-END
               READ SDN-FILE
                   AT END SET WS-SDN-END TO TRUE
                   NOT AT END PERFORM 1510-LOAD-SDN-LINE
               END-READ
           END-PERFORM
           CLOSE SDN-FILE
           IF SDNT-COUNT = ZERO
               MOVE 'PAYPROC' TO ABND-PROGRAM
               MOVE '1500-LOAD-SDN-TABLE' TO ABND-PARAGRAPH
               MOVE 'SDNLIST' TO ABND-FILE-NAME
               MOVE 'SDN LIST EMPTY - PAYMENTS CANNOT BE SCREENED'
                 TO ABND-MESSAGE
               MOVE 16 TO ABND-CODE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF.

       1510-LOAD-SDN-LINE.
           IF SDN-LINE(1:1) = '#' OR SDN-LINE = SPACES
               EXIT PARAGRAPH
           END-IF
           IF SDNT-COUNT NOT < SDNT-MAX-ENTRIES
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-SDN-FIELDS
           UNSTRING SDN-LINE DELIMITED BY '|'
               INTO WS-SF-ENTNUM WS-SF-TYPE WS-SF-NAME
                    WS-SF-COUNTRY WS-SF-PROGRAM
           END-UNSTRING
           ADD 1 TO SDNT-COUNT
           COMPUTE SDNT-ENT-NUM(SDNT-COUNT) =
               FUNCTION NUMVAL(WS-SF-ENTNUM)
           MOVE WS-SF-TYPE    TO SDNT-TYPE(SDNT-COUNT)
           MOVE WS-SF-NAME    TO SDNT-NAME(SDNT-COUNT)
           MOVE WS-SF-COUNTRY TO SDNT-COUNTRY(SDNT-COUNT)
           MOVE WS-SF-PROGRAM TO SDNT-PROGRAM(SDNT-COUNT).

      *----------------------------------------------------------------*
       2000-PROCESS-PAYMENT.
           ADD 1 TO WS-READ-COUNT
           IF PAY-IS-RECURRING
               MOVE PAY-NEXT-DATE TO WS-DUE-DATE
           ELSE
               MOVE PAY-VALUE-DATE TO WS-DUE-DATE
               IF WS-DUE-DATE = ZERO
                   MOVE PAY-REQ-VALUE-DATE TO WS-DUE-DATE
               END-IF
           END-IF
           EVALUATE TRUE
               WHEN PAY-IS-TERMINAL
                   CONTINUE
               WHEN WS-DUE-DATE = ZERO
                   CONTINUE
               WHEN WS-DUE-DATE > CTL-BUSINESS-DATE
                   CONTINUE
               WHEN PAY-ENTERED OR PAY-PENDING-APPROVAL
                   MOVE 'NOT SENT' TO WS-ACTION
                   MOVE 'DUE BUT NOT APPROVED - AWAITING CHECKER'
                     TO WS-DETAIL
                   PERFORM 8800-REPORT-PAYMENT
               WHEN PAY-APPROVED
                   ADD 1 TO WS-DUE-COUNT
                   PERFORM 2100-RELEASE-PAYMENT
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-RELEASE-PAYMENT.
           MOVE SPACES TO WS-OUTCOME WS-ACTION WS-DETAIL WS-IMAD
           MOVE PAY-TYPE TO WS-EFF-TYPE
           MOVE PAY-BENE-COUNTRY TO WS-BENE-COUNTRY
           IF WS-BENE-COUNTRY = SPACES
               MOVE 'US' TO WS-BENE-COUNTRY
           END-IF
           MOVE ZERO TO WS-FEE

           PERFORM 2200-CHECK-DEBIT-ACCOUNT
           IF WS-OUTCOME = SPACE
               PERFORM 2300-OFAC-SCREEN
           END-IF
           IF WS-OUTCOME = SPACE
               PERFORM 2400-LIMIT-CHECK
           END-IF
           IF WS-OUTCOME = SPACE
               PERFORM 2500-CUTOFF-CHECK
           END-IF
           IF WS-OUTCOME = SPACE
               PERFORM 2600-VALIDATE-BENEFICIARY
           END-IF
           IF WS-OUTCOME = SPACE
               PERFORM 3000-GENERATE-PAYMENT
           END-IF
           PERFORM 4000-UPDATE-INSTRUCTION
           PERFORM 8800-REPORT-PAYMENT.

      *----------------------------------------------------------------*
       2200-CHECK-DEBIT-ACCOUNT.
           MOVE PAY-DEBIT-ACCT TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   SET WS-OUT-REJECT TO TRUE
                   MOVE 'NOAC' TO PAY-REJECT-REASON
                   MOVE 'DEBIT ACCOUNT NOT ON FILE' TO WS-DETAIL
           END-READ
           IF WS-OUTCOME NOT = SPACE
               EXIT PARAGRAPH
           END-IF
           IF NOT ACCT-ACTIVE
               SET WS-OUT-REJECT TO TRUE
               MOVE 'ACST' TO PAY-REJECT-REASON
               STRING 'DEBIT ACCOUNT STATUS ' ACCT-STATUS
                      ' ' ACCT-RESTRICT-REASON ' - NOT ACTIVE'
                      DELIMITED BY SIZE INTO WS-DETAIL
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-COST-CENTER      TO WS-DR-COST-CENTER
           MOVE ACCT-ANALYSIS-FLAG    TO WS-DR-ANALYSIS
           MOVE ACCT-OPEN-DATE        TO WS-DR-OPEN-DATE
           MOVE ACCT-DAILY-ACH-LIMIT  TO WS-DR-ACH-LIMIT
           MOVE ACCT-DAILY-WIRE-LIMIT TO WS-DR-WIRE-LIMIT
           MOVE ACCT-PRIMARY-CUST-ID  TO CUST-ID
           MOVE 'Y' TO WS-CUST-FOUND
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE 'N' TO WS-CUST-FOUND
                   INITIALIZE CUSTOMER-RECORD
                   MOVE 'RT' TO CUST-SEGMENT
           END-READ
           MOVE SPACES TO WS-CUST-NAME
           IF CUST-INDIVIDUAL
               STRING FUNCTION TRIM(CUST-FIRST-NAME) ' '
                      FUNCTION TRIM(CUST-LAST-NAME)
                      DELIMITED BY SIZE INTO WS-CUST-NAME
           ELSE
               MOVE CUST-LAST-NAME TO WS-CUST-NAME
           END-IF.

      *----------------------------------------------------------------*
      * OFAC RE-SCREEN OF THE BENEFICIARY (NOT FOR BOOK TRANSFERS -    *
      * BOTH PARTIES ARE CUSTOMERS SCREENED BY OFACBAT)                *
      *----------------------------------------------------------------*
       2300-OFAC-SCREEN.
           IF PAY-TYPE-BOOK
               EXIT PARAGRAPH
           END-IF
           INITIALIZE OFS-INPUT
           MOVE PAY-BENE-NAME   TO OFS-FULL-NAME
           MOVE PAY-BENE-COUNTRY TO OFS-COUNTRY
           CALL 'OFACSCRN' USING OFAC-SCREEN-PARMS SDN-TABLE
           MOVE OFS-RESULT       TO PAY-OFAC-RESULT
           MOVE OFS-SCORE        TO PAY-OFAC-SCORE
           MOVE CTL-BUSINESS-DATE TO PAY-OFAC-DATE
           MOVE OFS-SCORE        TO WS-ED-SCORE
           EVALUATE TRUE
               WHEN OFS-RC-ERROR
                   SET WS-OUT-HOLD TO TRUE
                   MOVE 'OFAC SCREENING ERROR - HELD' TO WS-DETAIL
               WHEN OFS-HIT
                   SET WS-OUT-REJECT TO TRUE
                   MOVE 'OFAC' TO PAY-REJECT-REASON
                   ADD 1 TO WS-OFAC-HIT-COUNT
                   STRING 'OFAC HIT ' OFS-MATCH-RULE ' SCORE '
                          WS-ED-SCORE ' SDN ' OFS-MATCH-ENT-NUM
                          ' - FUNDS NOT HELD, REFER TO COMPLIANCE'
                          DELIMITED BY SIZE INTO WS-DETAIL
                   MOVE 'C'        TO WS-ALRT-SEV
                   MOVE 'PAYOFAC1' TO WS-ALRT-RULE
                   PERFORM 8600-WRITE-OFAC-ALERT
               WHEN OFS-POTENTIAL-HIT AND NOT PAY-COMPL-CLEARED
                   SET WS-OUT-HOLD TO TRUE
                   SET PAY-COMPL-REVIEW-REQUIRED TO TRUE
                   STRING 'OFAC POTENTIAL ' OFS-MATCH-RULE ' SCORE '
                          WS-ED-SCORE ' SDN ' OFS-MATCH-ENT-NUM
                          ' - COMPLIANCE REVIEW'
                          DELIMITED BY SIZE INTO WS-DETAIL
                   MOVE 'H'        TO WS-ALRT-SEV
                   MOVE 'PAYOFAC2' TO WS-ALRT-RULE
                   PERFORM 8600-WRITE-OFAC-ALERT
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LIMITS / APPROVALS. A CHECKER EQUAL TO THE MAKER IS NOT A      *
      * SECOND APPROVAL (SOX 404).                                     *
      *----------------------------------------------------------------*
       2400-LIMIT-CHECK.
           PERFORM 8200-FIND-USAGE
           INITIALIZE PAY-LIMIT-PARMS
           MOVE PAY-TYPE           TO PLM-PAY-TYPE
           MOVE PAY-AMOUNT         TO PLM-AMOUNT
           MOVE CUST-SEGMENT       TO PLM-SEGMENT
           MOVE WS-DR-OPEN-DATE    TO PLM-ACCT-OPEN-DATE
           MOVE CTL-BUSINESS-DATE  TO PLM-BUSINESS-DATE
           MOVE WS-USE-ACH(WS-USE-SUB)  TO PLM-ACH-USED-TODAY
           MOVE WS-USE-WIRE(WS-USE-SUB) TO PLM-WIRE-USED-TODAY
           MOVE WS-USE-BOOK(WS-USE-SUB) TO PLM-BOOK-USED-TODAY
           MOVE WS-DR-ACH-LIMIT    TO PLM-ACCT-ACH-LIMIT
           MOVE WS-DR-WIRE-LIMIT   TO PLM-ACCT-WIRE-LIMIT
           MOVE WS-BENE-COUNTRY    TO PLM-BENE-COUNTRY
           CALL 'PAYLIMIT' USING PAY-LIMIT-PARMS

           IF PAY-CHECKER-USER = PAY-MAKER-USER
               MOVE SPACES TO WS-APPROVER
           ELSE
               MOVE PAY-CHECKER-USER TO WS-APPROVER
           END-IF

           EVALUATE TRUE
               WHEN PLM-DECLINE
                   SET WS-OUT-REJECT TO TRUE
                   MOVE PLM-REASON-CODE TO PAY-REJECT-REASON
                   STRING 'LIMIT ' PLM-REASON-CODE ' '
                          PLM-REASON-TEXT
                          DELIMITED BY SIZE INTO WS-DETAIL
               WHEN PLM-NEEDS-APPROVAL
                AND PLM-DUAL-REQUIRED AND WS-APPROVER = SPACES
                   SET WS-OUT-HOLD TO TRUE
                   STRING 'HELD ' PLM-REASON-CODE ' - SECOND APPROVAL'
                          ' REQUIRED' DELIMITED BY SIZE INTO WS-DETAIL
               WHEN PLM-NEEDS-APPROVAL
                AND PLM-COMPLIANCE-REQUIRED
                AND NOT PAY-COMPL-CLEARED
                   SET WS-OUT-HOLD TO TRUE
                   SET PAY-COMPL-REVIEW-REQUIRED TO TRUE
                   STRING 'HELD ' PLM-REASON-CODE
                          ' - COMPLIANCE REVIEW REQUIRED'
                          DELIMITED BY SIZE INTO WS-DETAIL
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CUTOFF - THE TIME OF DAY ONLY COUNTS FOR INSTRUCTIONS APPROVED *
      * TODAY. WAREHOUSED / RECURRING ITEMS RELEASE AT START OF DAY.   *
      *----------------------------------------------------------------*
       2500-CUTOFF-CHECK.
           MOVE ZERO TO WS-REQ-HHMM
           IF NOT PAY-IS-RECURRING
               EVALUATE TRUE
                   WHEN PAY-APPROVAL-DATE = CTL-BUSINESS-DATE
                       COMPUTE WS-REQ-HHMM = PAY-APPROVAL-TIME / 100
                   WHEN PAY-APPROVAL-DATE = ZERO
                    AND PAY-CREATED-DATE = CTL-BUSINESS-DATE
                       COMPUTE WS-REQ-HHMM = PAY-CREATED-TIME / 100
               END-EVALUATE
           END-IF
           INITIALIZE CUTOFF-PARMS
           MOVE PAY-TYPE              TO CUT-PAY-TYPE
           MOVE 'C'                   TO CUT-DR-CR
           MOVE PAY-AMOUNT            TO CUT-AMOUNT
           MOVE WS-DUE-DATE           TO CUT-REQUEST-DATE
           MOVE WS-REQ-HHMM           TO CUT-REQUEST-TIME
           MOVE CTL-BUSINESS-DATE     TO CUT-BUSINESS-DATE
           MOVE CTL-ACH-CUTOFF-TIME   TO CUT-ACH-CUTOFF
           MOVE CTL-SDACH-CUTOFF-TIME TO CUT-SDACH-CUTOFF
           MOVE CTL-WIRE-CUTOFF-TIME  TO CUT-WIRE-CUTOFF
           CALL 'CUTOFF' USING CUTOFF-PARMS
           IF CUT-RETURN-CODE = 12
               SET WS-OUT-HOLD TO TRUE
               STRING 'CUTOFF ERROR ' CUT-REASON-CODE
                      DELIMITED BY SIZE INTO WS-DETAIL
               EXIT PARAGRAPH
           END-IF
           MOVE CUT-EFFECTIVE-TYPE TO WS-EFF-TYPE
           MOVE CUT-SETTLE-DATE    TO WS-SETTLE-DATE
           IF CUT-PROCESS-DATE > CTL-BUSINESS-DATE
               SET WS-OUT-ROLL TO TRUE
               STRING 'ROLLED ' CUT-REASON-CODE ' TO '
                      CUT-PROCESS-DATE ' SETTLE ' CUT-SETTLE-DATE
                      ' (' CUT-REASON-TEXT ')'
                      DELIMITED BY SIZE INTO WS-DETAIL
           END-IF.

      *----------------------------------------------------------------*
       2600-VALIDATE-BENEFICIARY.
           EVALUATE TRUE
               WHEN PAY-TYPE-BOOK
                   MOVE PAY-BENE-ACCT(1:12) TO ACCT-NUMBER
                   READ ACCOUNT-FILE
                       INVALID KEY
                           SET WS-OUT-REJECT TO TRUE
                           MOVE 'BENE' TO PAY-REJECT-REASON
                           MOVE 'BOOK TRANSFER - CREDIT ACCOUNT NOT ON'
                             TO WS-DETAIL
                   END-READ
                   IF WS-OUTCOME = SPACE
                      AND (ACCT-CLOSED OR ACCT-ESCHEATED
                           OR ACCT-FROZEN)
                       SET WS-OUT-REJECT TO TRUE
                       MOVE 'BENE' TO PAY-REJECT-REASON
                       MOVE 'BOOK TRANSFER - CREDIT ACCOUNT CLOSED OR'
                         TO WS-DETAIL
                   END-IF
               WHEN PAY-TYPE-ANY-ACH
                   INITIALIZE ACH-VALID-PARMS
                   MOVE PAY-BENE-ROUTING TO AVL-ROUTING
                   IF PAY-BENE-SAVINGS
                       MOVE '32' TO AVL-TRAN-CODE
                   ELSE
                       MOVE '22' TO AVL-TRAN-CODE
                   END-IF
                   MOVE PAY-AMOUNT   TO AVL-AMOUNT
                   MOVE PAY-SEC-CODE TO AVL-SEC-CODE
      *            ORIGINATION: RECEIVER TYPE IMPLIED BY THE SEC CODE
                   IF PAY-SEC-CCD OR PAY-SEC-CTX
                       MOVE 'C' TO AVL-ACCT-TYPE
                   ELSE
                       MOVE 'I' TO AVL-ACCT-TYPE
                   END-IF
                   MOVE 'Y' TO AVL-AUTH-FLAG
                   CALL 'ACHVALID' USING ACH-VALID-PARMS
                   IF AVL-REJECT
                       SET WS-OUT-REJECT TO TRUE
                       MOVE 'ACHV' TO PAY-REJECT-REASON
                       STRING 'ACH EDIT ' AVL-REASON-CODE ' '
                              AVL-REASON-TEXT
                              DELIMITED BY SIZE INTO WS-DETAIL
                   END-IF
               WHEN PAY-TYPE-WIRE
                   IF PAY-BENE-ROUTING IS NOT NUMERIC
                       SET WS-OUT-REJECT TO TRUE
                   ELSE
                       MOVE PAY-BENE-ROUTING(1:8) TO WS-CD-ROUTING
                       PERFORM 8300-CHECK-DIGIT
                       IF WS-CD-RESULT NOT = PAY-BENE-ROUTING(9:1)
                           SET WS-OUT-REJECT TO TRUE
                       END-IF
                   END-IF
                   IF WS-OUT-REJECT
                       MOVE 'RTCK' TO PAY-REJECT-REASON
                       MOVE 'FEDWIRE - INVALID RECEIVER ABA'
                         TO WS-DETAIL
                   END-IF
               WHEN PAY-TYPE-INTL-WIRE
                   IF PAY-BENE-BIC = SPACES
                       SET WS-OUT-REJECT TO TRUE
                       MOVE 'BIC ' TO PAY-REJECT-REASON
                       MOVE 'INTL WIRE - BENEFICIARY BIC MISSING'
                         TO WS-DETAIL
                   END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GENERATE THE MONEY MOVEMENT                                    *
      *----------------------------------------------------------------*
       3000-GENERATE-PAYMENT.
           SET WS-OUT-RELEASE TO TRUE
           EVALUATE TRUE
               WHEN PAY-TYPE-BOOK
                   PERFORM 3100-BOOK-TRANSFER
               WHEN WS-EFF-TYPE = 'A' OR WS-EFF-TYPE = 'S'
                   PERFORM 3200-ACH-ENTRY
               WHEN OTHER
                   PERFORM 3300-WIRE-TRANSFER
           END-EVALUATE
           ADD 1 TO WS-RELEASE-COUNT
           ADD PAY-AMOUNT TO WS-RELEASE-AMT
           PERFORM 8250-ADD-USAGE.

       3100-BOOK-TRANSFER.
           MOVE PAY-DEBIT-ACCT   TO WS-TX-ACCT
           MOVE 'WXFR'           TO WS-TX-CODE
           MOVE PAY-AMOUNT       TO WS-TX-AMOUNT
           MOVE 'BAT'            TO WS-TX-CHANNEL
           MOVE SPACES           TO WS-TX-DESC
           STRING 'TRANSFER TO ' PAY-BENE-ACCT(1:12) ' ' PAY-ID
                  DELIMITED BY SIZE INTO WS-TX-DESC
           MOVE WS-DR-COST-CENTER TO WS-TX-COST-CENTER
           MOVE ZERO             TO WS-TX-FEE
           PERFORM 8100-WRITE-TRAN
           MOVE PAY-BENE-ACCT(1:12) TO WS-TX-ACCT
           MOVE 'DXFR'           TO WS-TX-CODE
           MOVE SPACES           TO WS-TX-DESC
           STRING 'TRANSFER FROM ' PAY-DEBIT-ACCT ' ' PAY-ID
                  DELIMITED BY SIZE INTO WS-TX-DESC
           MOVE ACCT-COST-CENTER TO WS-TX-COST-CENTER
           PERFORM 8100-WRITE-TRAN
           MOVE 'BOOKED' TO WS-ACTION
           STRING 'WXFR/DXFR TO ' PAY-BENE-ACCT(1:12)
                  ' VALUE ' CTL-BUSINESS-DATE
                  DELIMITED BY SIZE INTO WS-DETAIL.

       3200-ACH-ENTRY.
           ADD 1 TO WS-AE-COUNT WS-ACH-TRACE-SEQ
           MOVE WS-AE-COUNT TO WS-AE-SUB
           MOVE PAY-DEBIT-ACCT    TO WS-AE-ORIG-ACCT(WS-AE-SUB)
           MOVE PAY-SEC-CODE      TO WS-AE-SEC(WS-AE-SUB)
           MOVE WS-SETTLE-DATE    TO WS-AE-SETTLE-DATE(WS-AE-SUB)
           IF WS-EFF-TYPE = 'S'
               MOVE 'Y' TO WS-AE-SD-FLAG(WS-AE-SUB)
           ELSE
               MOVE 'N' TO WS-AE-SD-FLAG(WS-AE-SUB)
           END-IF
           MOVE WS-CUST-NAME      TO WS-AE-CO-NAME(WS-AE-SUB)
           MOVE SPACES            TO WS-AE-CO-ID(WS-AE-SUB)
           STRING '1' CUST-TAX-ID DELIMITED BY SIZE
             INTO WS-AE-CO-ID(WS-AE-SUB)
           MOVE 'PAYMENT'         TO WS-AE-ENTRY-DESC(WS-AE-SUB)
           SET PURP-IDX TO 1
           SEARCH WS-PURP-ENTRY
               AT END CONTINUE
               WHEN WS-PURP-CODE(PURP-IDX) = PAY-PURPOSE-CODE
                   MOVE WS-PURP-DESC(PURP-IDX)
                     TO WS-AE-ENTRY-DESC(WS-AE-SUB)
           END-SEARCH
           IF PAY-BENE-SAVINGS
               MOVE '32' TO WS-AE-TRAN-CODE(WS-AE-SUB)
           ELSE
               MOVE '22' TO WS-AE-TRAN-CODE(WS-AE-SUB)
           END-IF
           MOVE PAY-BENE-ROUTING  TO WS-AE-ROUTING(WS-AE-SUB)
           MOVE PAY-BENE-ACCT     TO WS-AE-DFI-ACCT(WS-AE-SUB)
           MOVE PAY-AMOUNT        TO WS-AE-AMOUNT(WS-AE-SUB)
           MOVE PAY-ID            TO WS-AE-INDIV-ID(WS-AE-SUB)
           MOVE PAY-BENE-NAME     TO WS-AE-NAME(WS-AE-SUB)
           MOVE PAY-REMIT-INFO    TO WS-AE-REMIT(WS-AE-SUB)
           MOVE WS-ACH-TRACE-SEQ  TO WS-AE-TRACE-SEQ(WS-AE-SUB)
           MOVE 'N'               TO WS-AE-WRITTEN(WS-AE-SUB)
           MOVE SPACES TO PAY-NETWORK-REF
           STRING WS-OUR-DFI WS-ACH-TRACE-SEQ DELIMITED BY SIZE
             INTO PAY-NETWORK-REF

           MOVE PAY-DEBIT-ACCT    TO WS-TX-ACCT
           MOVE 'WACH'            TO WS-TX-CODE
           MOVE PAY-AMOUNT        TO WS-TX-AMOUNT
           IF WS-EFF-TYPE = 'S'
               MOVE 'SDA' TO WS-TX-CHANNEL
           ELSE
               MOVE 'ACH' TO WS-TX-CHANNEL
           END-IF
           MOVE SPACES TO WS-TX-DESC
           STRING 'ACH ' PAY-SEC-CODE ' ' PAY-BENE-NAME
                  DELIMITED BY SIZE INTO WS-TX-DESC
           MOVE WS-DR-COST-CENTER TO WS-TX-COST-CENTER
           MOVE ZERO              TO WS-TX-FEE
           PERFORM 8100-WRITE-TRAN
           MOVE 'ACH SENT' TO WS-ACTION
           STRING 'WACH ' WS-TX-CHANNEL ' TRACE ' PAY-NETWORK-REF(1:15)
                  ' SETTLE ' WS-SETTLE-DATE
                  DELIMITED BY SIZE INTO WS-DETAIL
           IF WS-EFF-TYPE NOT = PAY-TYPE
               MOVE WS-DETAIL TO WS-DETAIL-SAVE
               MOVE SPACES TO WS-DETAIL
               STRING WS-DETAIL-SAVE(1:46) ' SDACH->ACH '
                      CUT-REASON-CODE
                      DELIMITED BY SIZE INTO WS-DETAIL
           END-IF.

      *----------------------------------------------------------------*
      * FEDWIRE: DEBIT, FEE, MESSAGE                                   *
      *----------------------------------------------------------------*
       3300-WIRE-TRANSFER.
           ADD 1 TO WS-IMAD-SEQ WS-WIRE-COUNT
           MOVE CTL-BUSINESS-DATE TO FW-1520-CYCLE-DATE
           MOVE WS-FEDWIRE-LTERM  TO FW-1520-LTERM
           MOVE WS-IMAD-SEQ       TO FW-1520-SEQ
           MOVE FW-1520-BODY      TO WS-IMAD PAY-NETWORK-REF

           MOVE PAY-DEBIT-ACCT    TO WS-TX-ACCT
           MOVE 'WWIR'            TO WS-TX-CODE
           MOVE PAY-AMOUNT        TO WS-TX-AMOUNT
           MOVE 'WIR'             TO WS-TX-CHANNEL
           MOVE SPACES TO WS-TX-DESC
           STRING 'WIRE TO ' PAY-BENE-NAME DELIMITED BY SIZE
             INTO WS-TX-DESC
           MOVE WS-DR-COST-CENTER TO WS-TX-COST-CENTER
           PERFORM 3310-WIRE-FEE
           MOVE WS-FEE            TO WS-TX-FEE
           PERFORM 8100-WRITE-TRAN
           IF WS-FEE > ZERO
               MOVE 'WFWR'        TO WS-TX-CODE
               MOVE WS-FEE        TO WS-TX-AMOUNT
               MOVE SPACES TO WS-TX-DESC
               IF PAY-TYPE-INTL-WIRE
                   STRING 'OUTGOING INTL WIRE FEE ' PAY-ID
                          DELIMITED BY SIZE INTO WS-TX-DESC
               ELSE
                   STRING 'OUTGOING DOMESTIC WIRE FEE ' PAY-ID
                          DELIMITED BY SIZE INTO WS-TX-DESC
               END-IF
               MOVE ZERO TO WS-TX-FEE
               PERFORM 8100-WRITE-TRAN
               ADD WS-FEE TO WS-FEE-AMT
           END-IF
           PERFORM 3400-WRITE-FEDWIRE
           MOVE 'WIRE SENT' TO WS-ACTION
           MOVE WS-FEE TO WS-ED-AMT
           STRING 'IMAD ' WS-IMAD ' SETTLE ' WS-SETTLE-DATE
                  ' FEE ' WS-ED-AMT
                  DELIMITED BY SIZE INTO WS-DETAIL.

      *    FEE SCHEDULE FS-2004-03 - WAIVED FOR EMPLOYEES, ANALYSED
      *    ACCOUNTS (BILLED VIA ANLYSFEE) AND PRIVATE WEALTH DOMESTIC
       3310-WIRE-FEE.
           EVALUATE TRUE
               WHEN CUST-IS-EMPLOYEE
                   MOVE ZERO TO WS-FEE
               WHEN WS-DR-ANALYSIS = 'Y'
                   MOVE ZERO TO WS-FEE
               WHEN PAY-TYPE-INTL-WIRE
                   MOVE WS-WIRE-FEE-INTL TO WS-FEE
               WHEN CUST-SEG-PRIVATE-WEALTH
                   MOVE ZERO TO WS-FEE
               WHEN OTHER
                   MOVE WS-WIRE-FEE-DOMESTIC TO WS-FEE
           END-EVALUATE
           MOVE WS-FEE TO PAY-FEE-AMOUNT.

       3400-WRITE-FEDWIRE.
           MOVE PAY-ID(5:8)       TO FW-1500-USER-REQ-CORR
           MOVE '{1500}'          TO FW-TAG
           MOVE FW-1500-BODY      TO FW-DATA
           PERFORM 8700-WRITE-FW-LINE
           MOVE '{1510}'          TO FW-TAG
           MOVE FW-1510-BODY      TO FW-DATA
           PERFORM 8700-WRITE-FW-LINE
           MOVE '{1520}'          TO FW-TAG
           MOVE FW-1520-BODY      TO FW-DATA
           PERFORM 8700-WRITE-FW-LINE
           MOVE PAY-AMOUNT        TO FW-2000-AMOUNT
           MOVE '{2000}'          TO FW-TAG
           MOVE FW-2000-BODY      TO FW-DATA
           PERFORM 8700-WRITE-FW-LINE
           MOVE WS-OUR-ABA        TO FW-DI-ROUTING
           MOVE WS-OUR-SHORT-NAME TO FW-DI-SHORT-NAME
           MOVE '{3100}'          TO FW-TAG
           MOVE FW-DI-BODY        TO FW-DATA
           PERFORM 8700-WRITE-FW-LINE
           IF PAY-TYPE-INTL-WIRE
               MOVE WS-CORRESP-ABA     TO FW-DI-ROUTING
               MOVE WS-CORRESP-NAME    TO FW-DI-SHORT-NAME
           ELSE
               MOVE PAY-BENE-ROUTING   TO FW-DI-ROUTING
               MOVE PAY-BENE-BANK-NAME TO FW-DI-SHORT-NAME
           END-IF
           MOVE '{3400}'          TO FW-TAG
           MOVE FW-DI-BODY        TO FW-DATA
           PERFORM 8700-WRITE-FW-LINE
           MOVE '{3600}'          TO FW-TAG
           MOVE 'CTR'             TO FW-DATA
           PERFORM 8700-WRITE-FW-LINE
           IF PAY-TYPE-INTL-WIRE
               MOVE '{4100}'      TO FW-TAG
               MOVE SPACES        TO FW-DATA
               STRING 'B' FUNCTION TRIM(PAY-BENE-BIC) '*'
                      FUNCTION TRIM(PAY-BENE-BANK-NAME) '*'
                      DELIMITED BY SIZE INTO FW-DATA
               PERFORM 8700-WRITE-FW-LINE
           END-IF
           MOVE '{4200}'          TO FW-TAG
           MOVE SPACES            TO FW-DATA
           STRING 'D' FUNCTION TRIM(PAY-BENE-ACCT) '*'
                  FUNCTION TRIM(PAY-BENE-NAME) '*'
                  FUNCTION TRIM(PAY-BENE-ADDR1) '*'
                  FUNCTION TRIM(PAY-BENE-ADDR2) '*'
                  DELIMITED BY SIZE INTO FW-DATA
           PERFORM 8700-WRITE-FW-LINE
           MOVE '{5000}'          TO FW-TAG
           MOVE SPACES            TO FW-DATA
           STRING 'D' PAY-DEBIT-ACCT '*'
                  FUNCTION TRIM(WS-CUST-NAME) '*'
                  FUNCTION TRIM(CUST-ADDR-LINE1) '*'
                  FUNCTION TRIM(CUST-CITY) ' ' CUST-STATE ' '
                  FUNCTION TRIM(CUST-ZIP) '*'
                  DELIMITED BY SIZE INTO FW-DATA
           PERFORM 8700-WRITE-FW-LINE
           MOVE '{6000}'          TO FW-TAG
           MOVE SPACES            TO FW-DATA
           STRING PAY-REMIT-INFO(1:35) '*' PAY-REMIT-INFO(36:35) '*'
                  'REF ' PAY-ID '*'
                  DELIMITED BY SIZE INTO FW-DATA
           PERFORM 8700-WRITE-FW-LINE.

      *----------------------------------------------------------------*
      * PAYMST UPDATE                                                  *
      *----------------------------------------------------------------*
       4000-UPDATE-INSTRUCTION.
           EVALUATE TRUE
               WHEN WS-OUT-REJECT
                   SET PAY-REJECTED TO TRUE
                   ADD 1 TO WS-REJECT-COUNT
                   IF WS-ACTION = SPACES
                       MOVE 'REJECTED' TO WS-ACTION
                   END-IF
               WHEN WS-OUT-HOLD
                   SET PAY-PENDING-APPROVAL TO TRUE
                   ADD 1 TO WS-HOLD-COUNT
                   MOVE 'HELD' TO WS-ACTION
               WHEN WS-OUT-ROLL
                   ADD 1 TO WS-ROLL-COUNT
                   MOVE 'ROLLED' TO WS-ACTION
                   IF NOT PAY-IS-RECURRING
                       MOVE CUT-PROCESS-DATE TO PAY-VALUE-DATE
                   END-IF
               WHEN WS-OUT-RELEASE
                   MOVE CTL-BUSINESS-DATE TO PAY-LAST-EXEC-DATE
                   ADD 1 TO PAY-EXEC-COUNT
                   MOVE WS-DEBIT-TRAN-ID TO PAY-LAST-TRAN-ID
                   IF PAY-IS-RECURRING
                       MOVE CTL-BUSINESS-DATE TO PAY-VALUE-DATE
                       PERFORM 4100-NEXT-OCCURRENCE
                   ELSE
                       IF PAY-TYPE-BOOK
                           SET PAY-COMPLETED TO TRUE
                       ELSE
                           SET PAY-RELEASED TO TRUE
                       END-IF
                   END-IF
           END-EVALUATE
           MOVE CTL-BUSINESS-DATE TO PAY-LAST-MAINT-DATE
           MOVE 'PAYPROC'         TO PAY-LAST-MAINT-USER
           REWRITE PAYMENT-RECORD
           IF WS-PAY-STATUS NOT = '00'
               MOVE 'PAYMST' TO ABND-FILE-NAME
               MOVE WS-PAY-STATUS TO ABND-FILE-STATUS
               MOVE '4000-UPDATE-INSTRUCTION' TO ABND-PARAGRAPH
               MOVE 'REWRITE FAILED ON PAYMST' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF.

      *    WEEKLY +7 DAYS, MONTHLY +1 MONTH FROM THE CURRENT NEXT DATE
       4100-NEXT-OCCURRENCE.
           INITIALIZE DATE-PARMS
           MOVE PAY-NEXT-DATE TO DTU-DATE-1
           IF PAY-FREQ-WEEKLY
               SET DTU-ADD-DAYS TO TRUE
               MOVE 7 TO DTU-NUMBER
           ELSE
               SET DTU-ADD-MONTHS TO TRUE
               MOVE 1 TO DTU-NUMBER
           END-IF
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO PAY-NEXT-DATE
           IF PAY-END-DATE > ZERO AND PAY-NEXT-DATE > PAY-END-DATE
               SET PAY-COMPLETED TO TRUE
               MOVE ZERO TO PAY-NEXT-DATE
           END-IF
           MOVE WS-DETAIL TO WS-DETAIL-SAVE
           MOVE SPACES TO WS-DETAIL
           STRING WS-DETAIL-SAVE(1:50) ' NEXT ' PAY-NEXT-DATE
                  DELIMITED BY SIZE INTO WS-DETAIL.

      *----------------------------------------------------------------*
      * NACHA ORIGINATION FILE - ONE BATCH PER ORIGINATOR ACCOUNT, SEC *
      * CODE, SETTLEMENT DATE AND SAME-DAY FLAG                        *
      *----------------------------------------------------------------*
       7000-WRITE-NACHA-FILE.
           IF WS-AE-COUNT = ZERO
               MOVE 'NO ACH ENTRIES ORIGINATED - ACHOUTB EMPTY'
                 TO WS-RPT-LINE
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
           MOVE 'A'          TO NFH-FILE-ID-MODIFIER
           MOVE '094'        TO NFH-RECORD-SIZE
           MOVE '10'         TO NFH-BLOCKING-FACTOR
           MOVE '1'          TO NFH-FORMAT-CODE
           MOVE WS-FED-ACH-NAME TO NFH-IMMED-DEST-NAME
           MOVE WS-OUR-NAME  TO NFH-IMMED-ORIGIN-NAME
           PERFORM 8750-WRITE-NACHA

           PERFORM VARYING WS-AE-SUB FROM 1 BY 1
                   UNTIL WS-AE-SUB > WS-AE-COUNT
               IF WS-AE-WRITTEN(WS-AE-SUB) = 'N'
                   PERFORM 7100-WRITE-BATCH
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
           MOVE ZERO             TO NFC-TOTAL-DEBIT
           MOVE WS-NF-CREDIT     TO NFC-TOTAL-CREDIT
           PERFORM 8750-WRITE-NACHA
      *    PAD THE LAST BLOCK WITH ALL-9 RECORDS
           PERFORM UNTIL FUNCTION MOD(WS-NF-RECORDS, 10) = 0
               MOVE ALL '9' TO NACHA-REC-DATA
               PERFORM 8750-WRITE-NACHA
           END-PERFORM.

       7100-WRITE-BATCH.
           ADD 1 TO WS-NB-BATCH-NO
           MOVE ZERO TO WS-NB-COUNT WS-NB-HASH WS-NB-CREDIT
           MOVE SPACES TO NACHA-REC-DATA
           MOVE '5'      TO NBH-REC-TYPE
           MOVE 220      TO NBH-SERVICE-CLASS
           MOVE WS-AE-CO-NAME(WS-AE-SUB)    TO NBH-COMPANY-NAME
           MOVE WS-AE-CO-ID(WS-AE-SUB)      TO NBH-COMPANY-ID
           MOVE WS-AE-SEC(WS-AE-SUB)        TO NBH-SEC-CODE
           MOVE WS-AE-ENTRY-DESC(WS-AE-SUB) TO NBH-ENTRY-DESC
           IF WS-AE-SD-FLAG(WS-AE-SUB) = 'Y'
               MOVE 'SD1300' TO NBH-DESCRIPTIVE-DATE
           END-IF
           MOVE WS-AE-SETTLE-DATE(WS-AE-SUB)(3:6)
             TO NBH-EFFECTIVE-DATE
           MOVE '1'          TO NBH-ORIGINATOR-STATUS
           MOVE WS-OUR-DFI   TO NBH-ODFI-ID
           MOVE WS-NB-BATCH-NO TO NBH-BATCH-NUMBER
           PERFORM 8750-WRITE-NACHA

           PERFORM VARYING WS-AE-SUB2 FROM WS-AE-SUB BY 1
                   UNTIL WS-AE-SUB2 > WS-AE-COUNT
               IF WS-AE-WRITTEN(WS-AE-SUB2) = 'N'
                  AND WS-AE-KEY(WS-AE-SUB2) = WS-AE-KEY(WS-AE-SUB)
                   PERFORM 7200-WRITE-ENTRY
               END-IF
           END-PERFORM

           MOVE SPACES TO NACHA-REC-DATA
           MOVE '8'             TO NBC-REC-TYPE
           MOVE 220             TO NBC-SERVICE-CLASS
           MOVE WS-NB-COUNT     TO NBC-ENTRY-ADDENDA-CNT
           COMPUTE WS-HASH-10 =
               FUNCTION MOD(WS-NB-HASH, WS-HASH-MODULUS)
           MOVE WS-HASH-10      TO NBC-ENTRY-HASH
           MOVE ZERO            TO NBC-TOTAL-DEBIT
           MOVE WS-NB-CREDIT    TO NBC-TOTAL-CREDIT
           MOVE WS-AE-CO-ID(WS-AE-SUB) TO NBC-COMPANY-ID
           MOVE WS-OUR-DFI      TO NBC-ODFI-ID
           MOVE WS-NB-BATCH-NO  TO NBC-BATCH-NUMBER
           PERFORM 8750-WRITE-NACHA
           ADD WS-NB-COUNT  TO WS-NF-COUNT
           ADD WS-NB-HASH   TO WS-NF-HASH
           ADD WS-NB-CREDIT TO WS-NF-CREDIT.

       7200-WRITE-ENTRY.
           MOVE 'Y' TO WS-AE-WRITTEN(WS-AE-SUB2)
           MOVE SPACES TO NACHA-REC-DATA
           MOVE '6'                          TO NED-REC-TYPE
           MOVE WS-AE-TRAN-CODE(WS-AE-SUB2)  TO NED-TRAN-CODE
           MOVE WS-AE-ROUTING(WS-AE-SUB2)(1:8) TO NED-RDFI-ID
           MOVE WS-AE-ROUTING(WS-AE-SUB2)(9:1) TO NED-CHECK-DIGIT
           MOVE WS-AE-DFI-ACCT(WS-AE-SUB2)   TO NED-DFI-ACCOUNT
           MOVE WS-AE-AMOUNT(WS-AE-SUB2)     TO NED-AMOUNT
           MOVE WS-AE-INDIV-ID(WS-AE-SUB2)   TO NED-INDIVIDUAL-ID
           MOVE WS-AE-NAME(WS-AE-SUB2)       TO NED-INDIVIDUAL-NAME
           MOVE WS-OUR-DFI                   TO NED-TRACE-ODFI
           MOVE WS-AE-TRACE-SEQ(WS-AE-SUB2)  TO NED-TRACE-SEQ
           IF (WS-AE-SEC(WS-AE-SUB2) = 'CCD' OR 'CTX')
              AND WS-AE-REMIT(WS-AE-SUB2) NOT = SPACES
               MOVE '1' TO NED-ADDENDA-IND
           ELSE
               MOVE '0' TO NED-ADDENDA-IND
           END-IF
           PERFORM 8750-WRITE-NACHA
           ADD 1 TO WS-NB-COUNT
           MOVE WS-AE-ROUTING(WS-AE-SUB2)(1:8) TO WS-RDFI-NUM
           ADD WS-RDFI-NUM TO WS-NB-HASH
           ADD WS-AE-AMOUNT(WS-AE-SUB2) TO WS-NB-CREDIT
           IF NED-ADDENDA-IND = '1'
               MOVE SPACES TO NACHA-REC-DATA
               MOVE '7'  TO NAD-REC-TYPE
               MOVE '05' TO NAD-ADDENDA-TYPE
               MOVE WS-AE-REMIT(WS-AE-SUB2) TO NAD-PAYMENT-INFO
               MOVE 1    TO NAD-ADDENDA-SEQ
               MOVE WS-AE-TRACE-SEQ(WS-AE-SUB2) TO NAD-ENTRY-SEQ
               PERFORM 8750-WRITE-NACHA
               ADD 1 TO WS-NB-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COMMON ROUTINES                                                *
      *----------------------------------------------------------------*
       8100-WRITE-TRAN.
           INITIALIZE TRANSACTION-RECORD
           ADD 1 TO WS-TRAN-SEQ WS-TRAN-COUNT
           MOVE CTL-BUSINESS-DATE   TO TRAN-ID-DATE
           MOVE WS-TRAN-SEQ         TO TRAN-ID-SEQ
           MOVE WS-TX-ACCT          TO TRAN-ACCT-NUMBER
           MOVE CTL-BUSINESS-DATE   TO TRAN-EFFECTIVE-DATE
           MOVE ZERO                TO TRAN-POST-DATE
           MOVE PAY-APPROVAL-TIME   TO TRAN-TIME
           MOVE WS-TX-CODE          TO TRAN-CODE
           SET TC-IDX TO 1
           SEARCH TC-ENTRY
               AT END
                   MOVE 'D' TO TRAN-DR-CR
               WHEN TC-CODE(TC-IDX) = TRAN-CODE
                   MOVE TC-DR-CR(TC-IDX)     TO TRAN-DR-CR
                   MOVE TC-CASH-FLAG(TC-IDX) TO TRAN-CASH-FLAG
                   MOVE TC-REGD-FLAG(TC-IDX) TO TRAN-REGD-FLAG
           END-SEARCH
           MOVE WS-TX-AMOUNT        TO TRAN-AMOUNT
           MOVE WS-TX-CHANNEL       TO TRAN-CHANNEL
           MOVE WS-TX-DESC          TO TRAN-DESCRIPTION
           MOVE PAY-ID              TO TRAN-REFERENCE
           MOVE PAY-BENE-ACCT       TO TRAN-COUNTER-ACCT
           MOVE PAY-BENE-NAME       TO TRAN-COUNTERPARTY-NAME
           MOVE PAY-BENE-ROUTING    TO TRAN-COUNTERPARTY-ROUTING
           MOVE WS-BENE-COUNTRY     TO TRAN-COUNTERPARTY-COUNTRY
           SET TRAN-PENDING         TO TRUE
           IF WS-APPROVER NOT = SPACES
               MOVE WS-APPROVER     TO TRAN-USER-ID
           ELSE
               MOVE PAY-MAKER-USER  TO TRAN-USER-ID
           END-IF
           MOVE WS-TX-COST-CENTER   TO TRAN-COST-CENTER
           MOVE WS-TX-FEE           TO TRAN-FEE-AMOUNT
           MOVE 'PAYPROC'           TO TRAN-SOURCE-PGM
           WRITE TRANSACTION-RECORD
           IF WS-TRAN-STATUS NOT = '00'
               MOVE 'PAYTRAN' TO ABND-FILE-NAME
               MOVE WS-TRAN-STATUS TO ABND-FILE-STATUS
               MOVE '8100-WRITE-TRAN' TO ABND-PARAGRAPH
               MOVE 'WRITE FAILED ON PAYTRAN' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF
           IF TRAN-CODE(1:1) = 'W' AND TRAN-CODE NOT = 'WFWR'
               MOVE TRAN-ID TO WS-DEBIT-TRAN-ID
           END-IF.

      *    LOCATE (OR ADD) THE USAGE SLOT FOR THE DEBIT ACCOUNT
       8200-FIND-USAGE.
           MOVE ZERO TO WS-USE-SUB
           PERFORM VARYING USE-IDX FROM 1 BY 1
                   UNTIL USE-IDX > WS-USE-COUNT OR WS-USE-SUB > 0
               IF WS-USE-ACCT(USE-IDX) = PAY-DEBIT-ACCT
                   SET WS-USE-SUB TO USE-IDX
               END-IF
           END-PERFORM
           IF WS-USE-SUB = 0 AND WS-USE-COUNT < 200
               ADD 1 TO WS-USE-COUNT
               MOVE WS-USE-COUNT   TO WS-USE-SUB
               MOVE PAY-DEBIT-ACCT TO WS-USE-ACCT(WS-USE-SUB)
               MOVE ZERO TO WS-USE-ACH(WS-USE-SUB)
                            WS-USE-WIRE(WS-USE-SUB)
                            WS-USE-BOOK(WS-USE-SUB)
           END-IF
           IF WS-USE-SUB = 0
               MOVE 200 TO WS-USE-SUB
           END-IF.

       8250-ADD-USAGE.
           EVALUATE TRUE
               WHEN PAY-TYPE-ANY-ACH
                   ADD PAY-AMOUNT TO WS-USE-ACH(WS-USE-SUB)
               WHEN PAY-TYPE-ANY-WIRE
                   ADD PAY-AMOUNT TO WS-USE-WIRE(WS-USE-SUB)
               WHEN OTHER
                   ADD PAY-AMOUNT TO WS-USE-BOOK(WS-USE-SUB)
           END-EVALUATE.

       8300-CHECK-DIGIT.
           MOVE ZERO TO WS-CD-SUM
           PERFORM VARYING WS-CD-I FROM 1 BY 1 UNTIL WS-CD-I > 8
               COMPUTE WS-CD-SUM = WS-CD-SUM
                   + WS-CD-DIGIT(WS-CD-I) * WS-CD-WEIGHT(WS-CD-I)
           END-PERFORM
           COMPUTE WS-CD-RESULT =
               FUNCTION MOD(10 - FUNCTION MOD(WS-CD-SUM, 10), 10).

       8600-WRITE-OFAC-ALERT.
           INITIALIZE ALERT-RECORD
           MOVE WS-ALRT-SEV       TO ALRT-SEVERITY
           MOVE WS-ALRT-RULE      TO ALRT-RULE-ID
           MOVE CTL-BUSINESS-DATE TO ALRT-DATE
           ACCEPT ALRT-TIME FROM TIME
           MOVE 'OFAC'            TO ALRT-TYPE
           MOVE PAY-CUST-ID       TO ALRT-CUST-ID
           MOVE PAY-DEBIT-ACCT    TO ALRT-ACCT-NUMBER
           MOVE PAY-AMOUNT        TO ALRT-AMOUNT
           MOVE SPACES            TO ALRT-DESCRIPTION
           STRING 'PMT ' PAY-ID ' BENE ' PAY-BENE-NAME(1:24)
                  ' SDN ' OFS-MATCH-ENT-NUM ' SCORE ' WS-ED-SCORE
                  DELIMITED BY SIZE INTO ALRT-DESCRIPTION
           SET ALRT-OPEN          TO TRUE
           MOVE 'PAYPROC'         TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD.

       8700-WRITE-FW-LINE.
           WRITE FEDWIRE-REC FROM FEDWIRE-LINE
           IF WS-FEDW-STATUS NOT = '00'
               MOVE 'FEDWOUT' TO ABND-FILE-NAME
               MOVE WS-FEDW-STATUS TO ABND-FILE-STATUS
               MOVE '8700-WRITE-FW-LINE' TO ABND-PARAGRAPH
               MOVE 'WRITE FAILED ON FEDWOUT' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF.

       8750-WRITE-NACHA.
           WRITE ACH-OUT-REC FROM NACHA-REC-DATA
           ADD 1 TO WS-NF-RECORDS
           IF WS-ACHO-STATUS NOT = '00'
               MOVE 'ACHOUTB' TO ABND-FILE-NAME
               MOVE WS-ACHO-STATUS TO ABND-FILE-STATUS
               MOVE '8750-WRITE-NACHA' TO ABND-PARAGRAPH
               MOVE 'WRITE FAILED ON ACHOUTB' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF.

       8800-REPORT-PAYMENT.
           MOVE SPACES TO WS-RPT-LINE
           MOVE PAY-AMOUNT TO WS-ED-AMT
           STRING PAY-ID ' ' PAY-TYPE ' ' PAY-DEBIT-ACCT ' '
                  WS-ED-AMT ' ' WS-ACTION ' ' WS-DETAIL
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT.

       8900-WRITE-REPORT.
           WRITE REPORT-REC FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE ALL '-' TO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-READ-COUNT TO WS-ED-CNT
           STRING 'INSTRUCTIONS READ    ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-DUE-COUNT TO WS-ED-CNT
           STRING 'APPROVED AND DUE     ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-RELEASE-COUNT TO WS-ED-CNT
           MOVE WS-RELEASE-AMT   TO WS-ED-AMT
           STRING 'RELEASED             ' WS-ED-CNT '  ' WS-ED-AMT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-ROLL-COUNT TO WS-ED-CNT
           STRING 'ROLLED (CUTOFF)      ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-HOLD-COUNT TO WS-ED-CNT
           STRING 'HELD FOR APPROVAL    ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-REJECT-COUNT TO WS-ED-CNT
           STRING 'REJECTED             ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-OFAC-HIT-COUNT TO WS-ED-CNT
           STRING '  OF WHICH OFAC HITS ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-AE-COUNT TO WS-ED-CNT
           STRING 'ACH ENTRIES ORIGINATED' WS-ED-CNT
                  '  BATCHES ' WS-NB-BATCH-NO
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-WIRE-COUNT TO WS-ED-CNT
           MOVE WS-FEE-AMT    TO WS-ED-AMT
           STRING 'FEDWIRE MESSAGES     ' WS-ED-CNT '  FEES ' WS-ED-AMT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-TRAN-COUNT TO WS-ED-CNT
           STRING 'PAYTRAN RECORDS      ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT

           CLOSE PAYMENT-FILE ACCOUNT-FILE CUSTOMER-FILE
                 PAY-TRAN-FILE ACH-OUT-FILE FEDWIRE-FILE
                 REPORT-FILE ALERT-FILE
           DISPLAY 'PAYPROC READ=' WS-READ-COUNT
                   ' DUE=' WS-DUE-COUNT
                   ' RELEASED=' WS-RELEASE-COUNT
                   ' ROLLED=' WS-ROLL-COUNT
                   ' HELD=' WS-HOLD-COUNT
                   ' REJECTED=' WS-REJECT-COUNT
                   ' OFAC=' WS-OFAC-HIT-COUNT
                   ' TRANS=' WS-TRAN-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND-OPEN.
           MOVE '1000-INITIALISE' TO ABND-PARAGRAPH
           MOVE 'OPEN FAILED' TO ABND-MESSAGE
           PERFORM 9910-ABEND.

       9910-ABEND.
           MOVE 'PAYPROC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
