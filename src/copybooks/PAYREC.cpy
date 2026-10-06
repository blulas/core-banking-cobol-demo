      *================================================================*
      * COPYBOOK : PAYREC                                              *
      * PURPOSE  : PAYMENT INSTRUCTION MASTER RECORD. ONE RECORD PER   *
      *            CUSTOMER PAYMENT ORDER (ONE-TIME OR RECURRING).     *
      * FILE     : PAYMST  (KSDS, KEY = PAY-ID,                        *
      *                     AIX  = PAY-DEBIT-ACCT WITH DUPLICATES)     *
      *            CICS FILE 'PAYMST', AIX PATH 'PAYMSTA'              *
      * WRITTEN  : PAYINIT (ONLINE ENTRY), LDPAYM (CONVERSION)         *
      * UPDATED  : WIREAPPR (APPROVAL), PAYPROC (RELEASE, NEXT DATE)   *
      *----------------------------------------------------------------*
      * STATUS LIFE CYCLE                                              *
      *   E ENTERED -> P PENDING APPROVAL -> A APPROVED -> L RELEASED  *
      *   -> C COMPLETED.   R REJECTED, X CANCELLED ARE TERMINAL.      *
      *   RECURRING ORDERS STAY 'A' UNTIL PAST END DATE, THEN 'C'.     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - BOOK AND WIRE TRANSFER ORDERS       *
      * 1996-02-12 RJT  ACH ORIGINATION (PPD/CCD)                      *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-03-15 BHS  OFAC RESULT / SCORE                            *
      * 2006-10-02 BHS  MAKER-CHECKER FIELDS (SOX 404)                 *
      * 2009-04-20 DPS  INTERNATIONAL WIRE - BIC, BENE COUNTRY         *
      * 2013-09-16 DPS  WEB / CTX SEC CODES, PURPOSE CODE              *
      * 2016-09-23 DPS  SAME-DAY ACH PAYMENT TYPE 'S'                  *
      * 2019-06-03 AKW  BATCH REFERENCE FOR MULTI-ENTRY ACH FILES      *
      * 2021-02-08 AKW  COMPLIANCE REVIEW AND RECONFIRM FLAGS INC20331 *
      *================================================================*
       01  PAYMENT-RECORD.
           05  PAY-ID                      PIC X(12).
           05  PAY-TYPE                    PIC X(01).
               88  PAY-TYPE-BOOK                     VALUE 'B'.
               88  PAY-TYPE-ACH                      VALUE 'A'.
               88  PAY-TYPE-SDACH                    VALUE 'S'.
               88  PAY-TYPE-WIRE                     VALUE 'W'.
               88  PAY-TYPE-INTL-WIRE                VALUE 'I'.
               88  PAY-TYPE-ANY-ACH                  VALUE 'A' 'S'.
               88  PAY-TYPE-ANY-WIRE                 VALUE 'W' 'I'.
               88  PAY-TYPE-VALID          VALUE 'B' 'A' 'S' 'W' 'I'.
           05  PAY-DEBIT-ACCT              PIC X(12).
           05  PAY-CUST-ID                 PIC X(10).
      *--- BENEFICIARY ------------------------------------------------*
           05  PAY-BENE-NAME               PIC X(35).
      *        DDA NUMBER, IBAN OR CLABE - LEFT JUSTIFIED
           05  PAY-BENE-ACCT               PIC X(34).
           05  PAY-BENE-ACCT-TYPE          PIC X(01).
               88  PAY-BENE-CHECKING                 VALUE 'C'.
               88  PAY-BENE-SAVINGS                  VALUE 'S'.
      *        ABA ROUTING (9). OUR OWN ABA = BOOK TRANSFER
           05  PAY-BENE-ROUTING            PIC X(09).
           05  PAY-BENE-BIC                PIC X(11).
           05  PAY-BENE-BANK-NAME          PIC X(35).
           05  PAY-BENE-COUNTRY            PIC X(02).
           05  PAY-BENE-ADDR1              PIC X(35).
           05  PAY-BENE-ADDR2              PIC X(35).
      *--- AMOUNT / DATES ---------------------------------------------*
           05  PAY-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  PAY-CURRENCY                PIC X(03).
           05  PAY-REQ-VALUE-DATE          PIC 9(08).
      *        VALUE DATE AFTER CUTOFF / BUSINESS DAY ADJUSTMENT
           05  PAY-VALUE-DATE              PIC 9(08).
           05  PAY-URGENT-FLAG             PIC X(01).
               88  PAY-IS-URGENT                     VALUE 'Y'.
      *--- RECURRING --------------------------------------------------*
           05  PAY-FREQUENCY               PIC X(01).
               88  PAY-FREQ-NONE                     VALUE 'N' ' '.
               88  PAY-FREQ-WEEKLY                   VALUE 'W'.
               88  PAY-FREQ-MONTHLY                  VALUE 'M'.
               88  PAY-IS-RECURRING                  VALUE 'W' 'M'.
           05  PAY-NEXT-DATE               PIC 9(08).
           05  PAY-END-DATE                PIC 9(08).
      *--- STATUS -----------------------------------------------------*
           05  PAY-STATUS                  PIC X(01).
               88  PAY-ENTERED                       VALUE 'E'.
               88  PAY-PENDING-APPROVAL              VALUE 'P'.
               88  PAY-APPROVED                      VALUE 'A'.
               88  PAY-RELEASED                      VALUE 'L'.
               88  PAY-REJECTED                      VALUE 'R'.
               88  PAY-CANCELLED                     VALUE 'X'.
               88  PAY-COMPLETED                     VALUE 'C'.
               88  PAY-IS-TERMINAL         VALUE 'L' 'R' 'X' 'C'.
           05  PAY-REJECT-REASON           PIC X(04).
      *--- MAKER / CHECKER --------------------------------------------*
           05  PAY-MAKER-USER              PIC X(08).
           05  PAY-CHECKER-USER            PIC X(08).
           05  PAY-APPROVAL-DATE           PIC 9(08).
           05  PAY-APPROVAL-TIME           PIC 9(06).
      *--- ACH ------------------------------------------------------- *
           05  PAY-SEC-CODE                PIC X(03).
               88  PAY-SEC-PPD                       VALUE 'PPD'.
               88  PAY-SEC-CCD                       VALUE 'CCD'.
               88  PAY-SEC-WEB                       VALUE 'WEB'.
               88  PAY-SEC-CTX                       VALUE 'CTX'.
      *        MULTI-ENTRY ACH: SAME BATCH REF = SAME NACHA BATCH
           05  PAY-BATCH-REF               PIC X(12).
      *--- COMPLIANCE -------------------------------------------------*
           05  PAY-OFAC-RESULT             PIC X(01).
               88  PAY-OFAC-NOT-SCREENED             VALUE ' '.
               88  PAY-OFAC-CLEAR                    VALUE 'C'.
               88  PAY-OFAC-POTENTIAL                VALUE 'P'.
               88  PAY-OFAC-HIT                      VALUE 'H'.
           05  PAY-OFAC-SCORE              PIC 9(03).
           05  PAY-OFAC-DATE               PIC 9(08).
      *        R = REVIEW REQUIRED, C = CLEARED BY COMPLIANCE
           05  PAY-COMPLIANCE-FLAG         PIC X(01).
               88  PAY-COMPL-NOT-REQUIRED            VALUE ' '.
               88  PAY-COMPL-REVIEW-REQUIRED         VALUE 'R'.
               88  PAY-COMPL-CLEARED                 VALUE 'C'.
           05  PAY-RECONFIRM-FLAG          PIC X(01).
               88  PAY-RECONFIRM-REQUIRED            VALUE 'Y'.
      *        ISO 20022 STYLE PURPOSE (RENT SALA SUPP TAXS TRAD ...)
           05  PAY-PURPOSE-CODE            PIC X(04).
      *        ORIGINATOR TO BENEFICIARY INFO / ACH ADDENDA
           05  PAY-REMIT-INFO              PIC X(80).
      *--- EXECUTION --------------------------------------------------*
           05  PAY-LAST-EXEC-DATE          PIC 9(08).
           05  PAY-EXEC-COUNT              PIC 9(05).
           05  PAY-LAST-TRAN-ID            PIC X(16).
           05  PAY-FEE-AMOUNT              PIC S9(07)V99 COMP-3.
      *        FEDWIRE IMAD OR ACH TRACE OF LAST RELEASE
           05  PAY-NETWORK-REF             PIC X(22).
      *--- AUDIT ------------------------------------------------------*
           05  PAY-CHANNEL                 PIC X(03).
           05  PAY-CREATED-DATE            PIC 9(08).
           05  PAY-CREATED-TIME            PIC 9(06).
           05  PAY-LAST-MAINT-DATE         PIC 9(08).
           05  PAY-LAST-MAINT-USER         PIC X(08).
           05  FILLER                      PIC X(30).
