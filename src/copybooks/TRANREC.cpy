      *================================================================*
      * COPYBOOK : TRANREC                                             *
      * PURPOSE  : MONETARY TRANSACTION RECORD. USED FOR CHANNEL INPUT *
      *            (TRANIN), EDITED (TRANEDT), POSTED (TRANPOST),      *
      *            REJECTS (TRANREJ) AND HISTORY (TRANHIST).           *
      * FILES    : SEQUENTIAL, FIXED LENGTH                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-09-10 BHS  CASH FLAG FOR CTR                              *
      * 2005-02-28 BHS  COUNTERPARTY FIELDS FOR OFAC                   *
      * 2016-09-23 DPS  SAME-DAY ACH INDICATOR VIA TRAN-CHANNEL 'SDA'  *
      *================================================================*
       01  TRANSACTION-RECORD.
      *    TRAN-ID = BUSINESS DATE (8) + SEQUENCE (8)
           05  TRAN-ID.
               10  TRAN-ID-DATE            PIC 9(08).
               10  TRAN-ID-SEQ             PIC 9(08).
           05  TRAN-ACCT-NUMBER            PIC X(12).
           05  TRAN-EFFECTIVE-DATE         PIC 9(08).
           05  TRAN-POST-DATE              PIC 9(08).
           05  TRAN-TIME                   PIC 9(06).
           05  TRAN-CODE                   PIC X(04).
           05  TRAN-DR-CR                  PIC X(01).
               88  TRAN-IS-DEBIT                     VALUE 'D'.
               88  TRAN-IS-CREDIT                    VALUE 'C'.
      *    AMOUNT IS ALWAYS POSITIVE - DIRECTION COMES FROM TRAN-DR-CR
           05  TRAN-AMOUNT                 PIC S9(13)V99 COMP-3.
           05  TRAN-CHANNEL                PIC X(03).
               88  TRAN-CH-BRANCH                    VALUE 'BRN'.
               88  TRAN-CH-ATM                       VALUE 'ATM'.
               88  TRAN-CH-ONLINE                    VALUE 'ONL'.
               88  TRAN-CH-MOBILE                    VALUE 'MOB'.
               88  TRAN-CH-ACH                       VALUE 'ACH'.
               88  TRAN-CH-SAME-DAY-ACH              VALUE 'SDA'.
               88  TRAN-CH-WIRE                      VALUE 'WIR'.
               88  TRAN-CH-BATCH                     VALUE 'BAT'.
               88  TRAN-CH-POS                       VALUE 'POS'.
               88  TRAN-CH-API                       VALUE 'API'.
           05  TRAN-DESCRIPTION            PIC X(40).
      *    CHECK NUMBER, ACH TRACE NUMBER, FEDWIRE IMAD, ETC.
           05  TRAN-REFERENCE              PIC X(20).
           05  TRAN-COUNTER-ACCT           PIC X(17).
           05  TRAN-COUNTERPARTY-NAME      PIC X(35).
           05  TRAN-COUNTERPARTY-ROUTING   PIC X(09).
           05  TRAN-COUNTERPARTY-COUNTRY   PIC X(02).
           05  TRAN-STATUS                 PIC X(01).
               88  TRAN-PENDING                      VALUE 'P'.
               88  TRAN-POSTED                       VALUE 'S'.
               88  TRAN-REJECTED                     VALUE 'R'.
               88  TRAN-REVERSED                     VALUE 'V'.
               88  TRAN-HELD                         VALUE 'H'.
           05  TRAN-REJECT-CODE            PIC X(04).
           05  TRAN-ORIG-TRAN-ID           PIC X(16).
           05  TRAN-USER-ID                PIC X(08).
           05  TRAN-COST-CENTER            PIC X(05).
           05  TRAN-CASH-FLAG              PIC X(01).
               88  TRAN-IS-CASH                      VALUE 'Y'.
           05  TRAN-HOLD-DAYS              PIC 9(02).
           05  TRAN-REGD-FLAG              PIC X(01).
               88  TRAN-COUNTS-REGD                  VALUE 'Y'.
           05  TRAN-OD-FLAG                PIC X(01).
               88  TRAN-POSTED-INTO-OD               VALUE 'Y'.
           05  TRAN-FEE-AMOUNT             PIC S9(07)V99 COMP-3.
           05  TRAN-GL-ACCOUNT             PIC X(10).
           05  TRAN-SOURCE-PGM             PIC X(08).
           05  FILLER                      PIC X(30).
