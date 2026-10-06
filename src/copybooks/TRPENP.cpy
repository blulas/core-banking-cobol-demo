      *================================================================*
      * COPYBOOK : TRPENP                                              *
      * PURPOSE  : LINKAGE FOR PERSONAL TRUST RULE MODULE TRPEN        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-05-18 SKN  ORIGINAL                                       *
      * 1992-02-26 CRP  AUDIT FINDING - INCOME CONTROL TOTALS REQ7366  *
      * 1996-10-15 TWH  TAX CUTOFF MOVED REQ2737                       *
      * 2000-04-07 SKN  DISTRIB RERUN PROTECTION CHG0765               *
      * 2002-11-04 EWS  INCOME FEE SCHEDULE UPDATE PRB4420             *
      * 2006-09-07 EWS  TAX TABLE EXPANDED REQ1693                     *
      * 2007-04-14 RJT  DISTRIB TABLE EXPANDED CHG5549                 *
      *================================================================*
       01  TRPEN-PARMS.
           05  TRPEN-DISTRIB-YTD           PIC S9(13)V99 COMP-3.
           05  TRPEN-TRUSTEE-FEE-AMT       PIC S9(13)V99 COMP-3.
           05  TRPEN-INCOME-AMT            PIC S9(13)V99 COMP-3.
           05  TRPEN-INCOME-YTD            PIC S9(13)V99 COMP-3.
           05  TRPEN-TAX-AMT               PIC S9(13)V99 COMP-3.
           05  TRPEN-MARKET-YTD            PIC S9(13)V99 COMP-3.
           05  TRPEN-CLOSE-DATE            PIC 9(08).
           05  TRPEN-NOTICE-DATE           PIC 9(08).
           05  TRPEN-LAST-STMT-DATE        PIC 9(08).
           05  TRPEN-OPEN-DATE             PIC 9(08).
           05  TRPEN-REGION-CD             PIC X(01).
               88  TRPEN-REGION-CD-A               VALUE 'A'.
               88  TRPEN-REGION-CD-D               VALUE 'D'.
               88  TRPEN-REGION-CD-N               VALUE 'N'.
               88  TRPEN-REGION-CD-T               VALUE 'T'.
           05  TRPEN-CYCLE-CD              PIC X(02).
           05  TRPEN-STATUS-CD             PIC X(02).
               88  TRPEN-STATUS-CD-EH              VALUE 'EH'.
               88  TRPEN-STATUS-CD-GG              VALUE 'GG'.
               88  TRPEN-STATUS-CD-JG              VALUE 'JG'.
           05  TRPEN-TRUST-TYPE-CD         PIC X(03).
               88  TRPEN-TRUST-TYPE-CD-FLJ         VALUE 'FLJ'.
               88  TRPEN-TRUST-TYPE-CD-MNG         VALUE 'MNG'.
               88  TRPEN-TRUST-TYPE-CD-VFN         VALUE 'VFN'.
               88  TRPEN-TRUST-TYPE-CD-XPL         VALUE 'XPL'.
           05  TRPEN-SUSPEND-FLAG          PIC X(01).
               88  TRPEN-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  TRPEN-EXEMPT-FLAG           PIC X(01).
               88  TRPEN-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  TRPEN-PREFERRED-FLAG        PIC X(01).
               88  TRPEN-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  TRPEN-RETRY-CNT             PIC 9(07).
           05  TRPEN-PAYMENT-CNT           PIC 9(07).
           05  TRPEN-DISCOUNT              PIC 9(03)V9(06).
           05  TRPEN-RATE                  PIC 9(03)V9(06).
           05  TRPEN-ADDR                  PIC X(30).
           05  TRPEN-REF-NO                PIC X(16).
           05  TRPEN-RETURN-CODE           PIC 9(02).
