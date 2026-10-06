      *================================================================*
      * COPYBOOK : SDRISKP                                             *
      * PURPOSE  : LINKAGE FOR SAFE DEPOSIT BOXES RULE MODULE SDRISK   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-09-15 LMB  ORIGINAL                                       *
      * 1996-10-17 CRP  NEW DEPOSIT REPORT COLUMNS PRB5528             *
      * 1996-12-01 SKN  DEPOSIT LIMIT REVISED INC8309                  *
      * 1997-06-17 BHS  RENT ROUNDING - HALF UP INC9282                *
      * 2000-10-18 BHS  KEY-FEE STATUS CODES ADDED REQ7628             *
      * 2004-11-04 BHS  DEPOSIT NOTICE TEXT PER LEGAL CHG4774          *
      * 2005-11-08 CRP  RENT FEE SCHEDULE UPDATE CHG3108               *
      * 2006-03-07 SKN  DEPOSIT STATUS CODES ADDED CHG7175             *
      * 2008-11-13 PQR  LATE-FEE STATUS CODES ADDED PRB9332            *
      *================================================================*
       01  SDRISK-PARMS.
           05  SDRISK-LATE-FEE-YTD         PIC S9(13)V99 COMP-3.
           05  SDRISK-KEY-FEE-YTD          PIC S9(13)V99 COMP-3.
           05  SDRISK-DEPOSIT-AMT          PIC S9(13)V99 COMP-3.
           05  SDRISK-DEPOSIT-YTD          PIC S9(13)V99 COMP-3.
           05  SDRISK-LATE-FEE-AMT         PIC S9(13)V99 COMP-3.
           05  SDRISK-KEY-FEE-AMT          PIC S9(13)V99 COMP-3.
           05  SDRISK-NEXT-DUE-DATE        PIC 9(08).
           05  SDRISK-OPEN-DATE            PIC 9(08).
           05  SDRISK-NEXT-REVIEW-DATE     PIC 9(08).
           05  SDRISK-LAST-MAINT-DATE      PIC 9(08).
           05  SDRISK-REASON-CD            PIC X(01).
           05  SDRISK-CYCLE-CD             PIC X(02).
               88  SDRISK-CYCLE-CD-AP              VALUE 'AP'.
               88  SDRISK-CYCLE-CD-DA              VALUE 'DA'.
               88  SDRISK-CYCLE-CD-HV              VALUE 'HV'.
               88  SDRISK-CYCLE-CD-SE              VALUE 'SE'.
           05  SDRISK-BOX-SIZE-CD          PIC X(02).
               88  SDRISK-BOX-SIZE-CD-CU           VALUE 'CU'.
               88  SDRISK-BOX-SIZE-CD-MG           VALUE 'MG'.
               88  SDRISK-BOX-SIZE-CD-VG           VALUE 'VG'.
               88  SDRISK-BOX-SIZE-CD-VN           VALUE 'VN'.
               88  SDRISK-BOX-SIZE-CD-XJ           VALUE 'XJ'.
           05  SDRISK-VAULT-CD             PIC X(02).
               88  SDRISK-VAULT-CD-BU              VALUE 'BU'.
               88  SDRISK-VAULT-CD-ET              VALUE 'ET'.
               88  SDRISK-VAULT-CD-FF              VALUE 'FF'.
               88  SDRISK-VAULT-CD-NG              VALUE 'NG'.
               88  SDRISK-VAULT-CD-NH              VALUE 'NH'.
           05  SDRISK-AUTO-RENEW-FLAG      PIC X(01).
               88  SDRISK-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  SDRISK-OVERRIDE-FLAG        PIC X(01).
               88  SDRISK-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  SDRISK-PREFERRED-FLAG       PIC X(01).
               88  SDRISK-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  SDRISK-ITEM-CNT             PIC 9(07).
           05  SDRISK-PAYMENT-CNT          PIC 9(07).
           05  SDRISK-FEE-PCT              PIC 9(03)V9(06).
           05  SDRISK-PENALTY-PCT          PIC 9(03)V9(06).
           05  SDRISK-DESC                 PIC X(30).
           05  SDRISK-CUST-ID              PIC X(10).
           05  SDRISK-RETURN-CODE          PIC 9(02).
