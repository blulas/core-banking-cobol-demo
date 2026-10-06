      *================================================================*
      * COPYBOOK : CUSTPND                                             *
      * PURPOSE  : CUSTOMER MAINTENANCE PENDING-APPROVAL RECORD        *
      *            (MAKER-CHECKER / DUAL CONTROL). WRITTEN BY CUSTMNT  *
      *            WHEN A CONTROLLED FIELD (LEGAL NAME, TAX ID) IS     *
      *            CHANGED; APPLIED TO CUSTMST ONLY WHEN A SECOND      *
      *            USER (THE CHECKER, NOT THE MAKER) APPROVES IT.      *
      * FILE     : CUSTPND  (CICS KSDS, KEY = PND-KEY)                 *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2007-01-15 BHS  ORIGINAL - SOX DUAL CONTROL (CHG0102)          *
      * 2013-08-19 DPS  REJECT REASON, 5 BUSINESS DAY EXPIRY           *
      *================================================================*
       01  CUST-PENDING-RECORD.
           05  PND-KEY.
               10  PND-CUST-ID             PIC X(10).
               10  PND-SEQ                 PIC 9(04).
           05  PND-FIELD-ID                PIC X(04).
               88  PND-FLD-LAST-NAME                 VALUE 'LNAM'.
               88  PND-FLD-FIRST-NAME                VALUE 'FNAM'.
               88  PND-FLD-MIDDLE-INIT               VALUE 'MINI'.
               88  PND-FLD-TAX-ID                    VALUE 'TXID'.
               88  PND-FLD-TAX-ID-TYPE               VALUE 'TXTP'.
           05  PND-OLD-VALUE               PIC X(30).
           05  PND-NEW-VALUE               PIC X(30).
           05  PND-STATUS                  PIC X(01).
               88  PND-AWAITING-CHECKER              VALUE 'P'.
               88  PND-APPROVED                      VALUE 'A'.
               88  PND-REJECTED                      VALUE 'R'.
               88  PND-EXPIRED                       VALUE 'E'.
           05  PND-MAKER-USER              PIC X(08).
           05  PND-MAKER-DATE              PIC 9(08).
           05  PND-MAKER-TIME              PIC 9(06).
           05  PND-CHECKER-USER            PIC X(08).
           05  PND-CHECKER-DATE            PIC 9(08).
      *    PENDING CHANGES NOT APPROVED IN 5 BUSINESS DAYS EXPIRE
           05  PND-EXPIRY-DATE             PIC 9(08).
           05  PND-REJECT-REASON           PIC X(30).
           05  FILLER                      PIC X(20).
