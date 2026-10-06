      *================================================================*
      * COPYBOOK : TRSARC2                                             *
      * PURPOSE  : PERSONAL TRUST PRIOR VERSION RECORD LAYOUT          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-11-20 MKL  ORIGINAL                                       *
      * 1993-09-16 MKL  TAX RERUN PROTECTION CHG2847                   *
      * 1994-05-26 LMB  TAX TABLE EXPANDED REQ7780                     *
      * 1997-04-21 CRP  PERFORMANCE - MARKET READ AHEAD REQ0457        *
      * 2003-06-18 SKN  TRUSTEE-FEE RERUN PROTECTION PRB9186           *
      * 2003-06-21 MKL  MARKET EXCEPTIONS TO OPS QUEUE REQ5862         *
      * 2004-09-01 PQR  NEW TRUSTEE-FEE REPORT COLUMNS PRB3393         *
      * 2004-11-09 JLM  TRUSTEE-FEE EXCEPTIONS TO OPS QUEUE PRB2547    *
      * 2004-08-07 LMB  TAX ROUNDING - HALF UP INC7099                 *
      *================================================================*
       01  TRS-H2-REC.
           05  TRSH2-KEY                   PIC X(12).
           05  TRSH2-NAME                  PIC X(30).
           05  TRSH2-CONTACT               PIC X(20).
           05  TRSH2-CUST-ID               PIC X(10).
           05  TRSH2-DESC                  PIC X(30).
           05  TRSH2-ADDR                  PIC X(30).
           05  TRSH2-REF-NO                PIC X(16).
           05  TRS-H2-AMOUNTS.
               10  TRSH2-DISTRIB-AMT       PIC S9(13)V99 COMP-3.
               10  TRSH2-MARKET-AMT        PIC S9(13)V99 COMP-3.
               10  TRSH2-INCOME-YTD        PIC S9(13)V99 COMP-3.
               10  TRSH2-TRUSTEE-FEE-YTD   PIC S9(13)V99 COMP-3.
               10  TRSH2-MARKET-YTD        PIC S9(13)V99 COMP-3.
               10  TRSH2-DISTRIB-YTD       PIC S9(13)V99 COMP-3.
               10  TRSH2-PRINCIPAL-YTD     PIC S9(13)V99 COMP-3.
               10  TRSH2-TAX-YTD           PIC S9(13)V99 COMP-3.
               10  TRSH2-TRUSTEE-FEE-AMT   PIC S9(13)V99 COMP-3.
               10  TRSH2-INCOME-AMT        PIC S9(13)V99 COMP-3.
               10  TRSH2-PRINCIPAL-AMT     PIC S9(13)V99 COMP-3.
               10  TRSH2-TAX-AMT           PIC S9(13)V99 COMP-3.
           05  TRS-H2-DATES.
               10  TRSH2-LAST-PAY-DATE     PIC 9(08).
               10  TRSH2-CLOSE-DATE        PIC 9(08).
               10  TRSH2-LAST-MAINT-DATE   PIC 9(08).
               10  TRSH2-LAST-STMT-DATE    PIC 9(08).
               10  TRSH2-EFFECTIVE-DATE    PIC 9(08).
               10  TRSH2-OPEN-DATE         PIC 9(08).
               10  TRSH2-MATURITY-DATE     PIC 9(08).
               10  TRSH2-EXPIRY-DATE       PIC 9(08).
               10  TRSH2-NOTICE-DATE       PIC 9(08).
           05  TRS-H2-CODES.
               10  TRSH2-REASON-CD         PIC X(01).
                   88  TRSH2-REASON-CD-H           VALUE 'H'.
                   88  TRSH2-REASON-CD-M           VALUE 'M'.
                   88  TRSH2-REASON-CD-T           VALUE 'T'.
               10  TRSH2-NOTICE-SENT-FLAG  PIC X(01).
                   88  TRSH2-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  TRSH2-STOP-FLAG         PIC X(01).
                   88  TRSH2-STOP-FLAG-YES         VALUE 'Y'.
               10  TRSH2-REGION-CD         PIC X(01).
                   88  TRSH2-REGION-CD-A           VALUE 'A'.
                   88  TRSH2-REGION-CD-D           VALUE 'D'.
                   88  TRSH2-REGION-CD-N           VALUE 'N'.
                   88  TRSH2-REGION-CD-T           VALUE 'T'.
               10  TRSH2-CYCLE-CD          PIC X(02).
               10  TRSH2-TRUST-TYPE-CD     PIC X(03).
                   88  TRSH2-TRUST-TYPE-CD-FLJ     VALUE 'FLJ'.
                   88  TRSH2-TRUST-TYPE-CD-MNG     VALUE 'MNG'.
                   88  TRSH2-TRUST-TYPE-CD-VFN     VALUE 'VFN'.
                   88  TRSH2-TRUST-TYPE-CD-XPL     VALUE 'XPL'.
               10  TRSH2-REVIEW-FLAG       PIC X(01).
                   88  TRSH2-REVIEW-FLAG-YES       VALUE 'Y'.
               10  TRSH2-DIST-FREQ-CD      PIC X(01).
                   88  TRSH2-DIST-FREQ-CD-E        VALUE 'E'.
                   88  TRSH2-DIST-FREQ-CD-K        VALUE 'K'.
                   88  TRSH2-DIST-FREQ-CD-P        VALUE 'P'.
                   88  TRSH2-DIST-FREQ-CD-W        VALUE 'W'.
                   88  TRSH2-DIST-FREQ-CD-Y        VALUE 'Y'.
                   88  TRSH2-DIST-FREQ-CD-Z        VALUE 'Z'.
               10  TRSH2-PREFERRED-FLAG    PIC X(01).
                   88  TRSH2-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  TRSH2-EXEMPT-FLAG       PIC X(01).
                   88  TRSH2-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  TRSH2-SUSPEND-FLAG      PIC X(01).
                   88  TRSH2-SUSPEND-FLAG-YES      VALUE 'Y'.
           05  TRS-H2-COUNTERS.
               10  TRSH2-RATE              PIC 9(03)V9(06).
               10  TRSH2-PAYMENT-CNT       PIC 9(07).
               10  TRSH2-RETRY-CNT         PIC 9(07).
               10  TRSH2-DISCOUNT          PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
