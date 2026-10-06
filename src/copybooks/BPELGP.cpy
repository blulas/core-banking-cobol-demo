      *================================================================*
      * COPYBOOK : BPELGP                                              *
      * PURPOSE  : LINKAGE FOR BILL PAYMENT SERVICE RULE MODULE BPELG  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-04-06 EWS  ORIGINAL                                       *
      * 1994-04-24 BHS  RETURNED TABLE EXPANDED REQ2882                *
      * 1994-12-23 MKL  PERFORMANCE - PAYMENT READ AHEAD INC5016       *
      * 2000-12-24 PQR  RETURNED FEE SCHEDULE UPDATE PRB6452           *
      * 2002-11-28 TWH  REFUND STATUS CODES ADDED INC1258              *
      * 2005-12-01 LMB  FEE EXCEPTIONS TO OPS QUEUE CHG0363            *
      * 2005-06-12 TWH  RETURNED EXCEPTIONS TO OPS QUEUE PRB9438       *
      * 2006-07-15 RJT  PAYMENT FEE SCHEDULE UPDATE PRB8532            *
      * 2006-10-04 SKN  FEE RERUN PROTECTION REQ5431                   *
      *================================================================*
       01  BPELG-PARMS.
           05  BPELG-PAYMENT-YTD           PIC S9(13)V99 COMP-3.
           05  BPELG-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  BPELG-REFUND-YTD            PIC S9(13)V99 COMP-3.
           05  BPELG-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  BPELG-PAYMENT-AMT           PIC S9(13)V99 COMP-3.
           05  BPELG-REFUND-AMT            PIC S9(13)V99 COMP-3.
           05  BPELG-EXPIRY-DATE           PIC 9(08).
           05  BPELG-LAST-STMT-DATE        PIC 9(08).
           05  BPELG-NEXT-DUE-DATE         PIC 9(08).
           05  BPELG-CLOSE-DATE            PIC 9(08).
           05  BPELG-CYCLE-CD              PIC X(02).
           05  BPELG-STATUS-CD             PIC X(02).
               88  BPELG-STATUS-CD-JX              VALUE 'JX'.
               88  BPELG-STATUS-CD-LR              VALUE 'LR'.
               88  BPELG-STATUS-CD-NZ              VALUE 'NZ'.
               88  BPELG-STATUS-CD-RV              VALUE 'RV'.
               88  BPELG-STATUS-CD-TM              VALUE 'TM'.
               88  BPELG-STATUS-CD-TZ              VALUE 'TZ'.
           05  BPELG-REMIT-METHOD-CD       PIC X(03).
           05  BPELG-PAYEE-TYPE-CD         PIC X(02).
               88  BPELG-PAYEE-TYPE-CD-BT          VALUE 'BT'.
               88  BPELG-PAYEE-TYPE-CD-FR          VALUE 'FR'.
               88  BPELG-PAYEE-TYPE-CD-LL          VALUE 'LL'.
               88  BPELG-PAYEE-TYPE-CD-US          VALUE 'US'.
               88  BPELG-PAYEE-TYPE-CD-XM          VALUE 'XM'.
               88  BPELG-PAYEE-TYPE-CD-YE          VALUE 'YE'.
           05  BPELG-OVERRIDE-FLAG         PIC X(01).
               88  BPELG-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  BPELG-HOLD-FLAG             PIC X(01).
               88  BPELG-HOLD-FLAG-YES             VALUE 'Y'.
           05  BPELG-SUSPEND-FLAG          PIC X(01).
               88  BPELG-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  BPELG-EXCEPT-CNT            PIC 9(07).
           05  BPELG-CYCLE-CNT             PIC 9(07).
           05  BPELG-RATE                  PIC 9(03)V9(06).
           05  BPELG-MARGIN                PIC 9(03)V9(06).
           05  BPELG-ADDR                  PIC X(30).
           05  BPELG-REF-NO                PIC X(16).
           05  BPELG-RETURN-CODE           PIC 9(02).
