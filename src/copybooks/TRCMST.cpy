      *================================================================*
      * COPYBOOK : TRCMST                                              *
      * PURPOSE  : TRAVELERS CHEQUES MASTER RECORD (TRCMST KSDS)       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-01-11 CRP  ORIGINAL                                       *
      * 1988-11-27 TWH  REMIT ROUNDING - HALF UP INC2502               *
      * 1989-12-28 CRP  PERFORMANCE - COMMISSION READ AHEAD CHG0447    *
      * 1989-11-20 PQR  REFUND TABLE EXPANDED REQ2242                  *
      * 1990-09-09 LMB  REFUND CALCULATION CORRECTED CHG0604           *
      * 1997-05-18 SKN  SHORTAGE NOTICE TEXT PER LEGAL CHG5438         *
      * 2013-01-13 GAF  REFUND RERUN PROTECTION INC9818                *
      *================================================================*
       01  TRC-MASTER-REC.
           05  TRCM-KEY                    PIC X(12).
           05  TRCM-CUST-ID                PIC X(10).
           05  TRCM-NAME                   PIC X(30).
           05  TRCM-DESC                   PIC X(30).
           05  TRCM-REF-NO                 PIC X(16).
           05  TRCM-ADDR                   PIC X(30).
           05  TRCM-CONTACT                PIC X(20).
           05  TRC-MASTER-AMOUNTS.
               10  TRCM-FACE-AMT           PIC S9(13)V99 COMP-3.
               10  TRCM-FACE-YTD           PIC S9(13)V99 COMP-3.
               10  TRCM-COMMISSION-AMT     PIC S9(13)V99 COMP-3.
               10  TRCM-COMMISSION-YTD     PIC S9(13)V99 COMP-3.
               10  TRCM-CONSIGN-AMT        PIC S9(13)V99 COMP-3.
               10  TRCM-CONSIGN-YTD        PIC S9(13)V99 COMP-3.
               10  TRCM-REMIT-AMT          PIC S9(13)V99 COMP-3.
               10  TRCM-REMIT-YTD          PIC S9(13)V99 COMP-3.
               10  TRCM-REFUND-AMT         PIC S9(13)V99 COMP-3.
               10  TRCM-REFUND-YTD         PIC S9(13)V99 COMP-3.
               10  TRCM-SHORTAGE-AMT       PIC S9(13)V99 COMP-3.
               10  TRCM-SHORTAGE-YTD       PIC S9(13)V99 COMP-3.
           05  TRC-MASTER-DATES.
               10  TRCM-EFFECTIVE-DATE     PIC 9(08).
               10  TRCM-NEXT-DUE-DATE      PIC 9(08).
               10  TRCM-LAST-STMT-DATE     PIC 9(08).
               10  TRCM-NEXT-REVIEW-DATE   PIC 9(08).
               10  TRCM-CLOSE-DATE         PIC 9(08).
               10  TRCM-MATURITY-DATE      PIC 9(08).
               10  TRCM-LAST-ACT-DATE      PIC 9(08).
               10  TRCM-LAST-PAY-DATE      PIC 9(08).
               10  TRCM-EXPIRY-DATE        PIC 9(08).
           05  TRC-MASTER-CODES.
               10  TRCM-ISSUER-CD          PIC X(02).
                   88  TRCM-ISSUER-CD-AF           VALUE 'AF'.
                   88  TRCM-ISSUER-CD-CJ           VALUE 'CJ'.
                   88  TRCM-ISSUER-CD-DU           VALUE 'DU'.
                   88  TRCM-ISSUER-CD-JY           VALUE 'JY'.
                   88  TRCM-ISSUER-CD-RR           VALUE 'RR'.
                   88  TRCM-ISSUER-CD-TK           VALUE 'TK'.
               10  TRCM-DENOM-CD           PIC X(01).
                   88  TRCM-DENOM-CD-G             VALUE 'G'.
                   88  TRCM-DENOM-CD-M             VALUE 'M'.
                   88  TRCM-DENOM-CD-N             VALUE 'N'.
                   88  TRCM-DENOM-CD-P             VALUE 'P'.
                   88  TRCM-DENOM-CD-V             VALUE 'V'.
                   88  TRCM-DENOM-CD-Y             VALUE 'Y'.
               10  TRCM-BRANCH-CD          PIC X(02).
                   88  TRCM-BRANCH-CD-EG           VALUE 'EG'.
                   88  TRCM-BRANCH-CD-HZ           VALUE 'HZ'.
                   88  TRCM-BRANCH-CD-JF           VALUE 'JF'.
                   88  TRCM-BRANCH-CD-NZ           VALUE 'NZ'.
               10  TRCM-STATUS-CD          PIC X(02).
                   88  TRCM-STATUS-CD-HK           VALUE 'HK'.
                   88  TRCM-STATUS-CD-RU           VALUE 'RU'.
                   88  TRCM-STATUS-CD-WA           VALUE 'WA'.
               10  TRCM-SOURCE-CD          PIC X(02).
                   88  TRCM-SOURCE-CD-EG           VALUE 'EG'.
                   88  TRCM-SOURCE-CD-GH           VALUE 'GH'.
                   88  TRCM-SOURCE-CD-LC           VALUE 'LC'.
                   88  TRCM-SOURCE-CD-LU           VALUE 'LU'.
                   88  TRCM-SOURCE-CD-YS           VALUE 'YS'.
               10  TRCM-CYCLE-CD           PIC X(03).
               10  TRCM-OVERRIDE-FLAG      PIC X(01).
                   88  TRCM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  TRCM-NOTICE-SENT-FLAG   PIC X(01).
                   88  TRCM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  TRCM-EXEMPT-FLAG        PIC X(01).
                   88  TRCM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  TRCM-REVIEW-FLAG        PIC X(01).
                   88  TRCM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  TRCM-SUSPEND-FLAG       PIC X(01).
                   88  TRCM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  TRCM-STOP-FLAG          PIC X(01).
                   88  TRCM-STOP-FLAG-YES          VALUE 'Y'.
           05  TRC-MASTER-COUNTERS.
               10  TRCM-CYCLE-CNT          PIC 9(07).
               10  TRCM-EXCEPT-CNT         PIC 9(07).
               10  TRCM-ITEM-CNT           PIC 9(07).
               10  TRCM-RETRY-CNT          PIC 9(07).
               10  TRCM-RATE               PIC 9(03)V9(06).
               10  TRCM-FEE-PCT            PIC 9(03)V9(06).
               10  TRCM-MARGIN             PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
