      *================================================================*
      * COPYBOOK : ARPMST                                              *
      * PURPOSE  : ACCOUNT RECONCILEMENT MASTER RECORD (ARPMST KSDS)   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-08 DPS  ORIGINAL                                       *
      * 1998-06-13 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-11-25 CRP  PAID EXCEPTIONS TO OPS QUEUE CHG7036           *
      * 2002-02-06 GAF  PAID CALCULATION CORRECTED PRB1123             *
      * 2008-04-23 RJT  PAID FEE SCHEDULE UPDATE CHG9413               *
      * 2008-08-26 TWH  NEW VOID REPORT COLUMNS PRB6038                *
      * 2011-12-14 BHS  PAID TABLE EXPANDED REQ3736                    *
      *================================================================*
       01  ARP-MASTER-REC.
           05  ARPM-KEY                    PIC X(12).
           05  ARPM-CUST-ID                PIC X(10).
           05  ARPM-NAME                   PIC X(30).
           05  ARPM-DESC                   PIC X(30).
           05  ARPM-REF-NO                 PIC X(16).
           05  ARPM-ADDR                   PIC X(30).
           05  ARPM-CONTACT                PIC X(20).
           05  ARP-MASTER-AMOUNTS.
               10  ARPM-ISSUED-AMT         PIC S9(13)V99 COMP-3.
               10  ARPM-ISSUED-YTD         PIC S9(13)V99 COMP-3.
               10  ARPM-PAID-AMT           PIC S9(13)V99 COMP-3.
               10  ARPM-PAID-YTD           PIC S9(13)V99 COMP-3.
               10  ARPM-OUTSTAND-AMT       PIC S9(13)V99 COMP-3.
               10  ARPM-OUTSTAND-YTD       PIC S9(13)V99 COMP-3.
               10  ARPM-STOP-AMT           PIC S9(13)V99 COMP-3.
               10  ARPM-STOP-YTD           PIC S9(13)V99 COMP-3.
               10  ARPM-VOID-AMT           PIC S9(13)V99 COMP-3.
               10  ARPM-VOID-YTD           PIC S9(13)V99 COMP-3.
               10  ARPM-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  ARPM-FEE-YTD            PIC S9(13)V99 COMP-3.
           05  ARP-MASTER-DATES.
               10  ARPM-LAST-STMT-DATE     PIC 9(08).
               10  ARPM-LAST-MAINT-DATE    PIC 9(08).
               10  ARPM-NEXT-DUE-DATE      PIC 9(08).
               10  ARPM-CLOSE-DATE         PIC 9(08).
               10  ARPM-LAST-ACT-DATE      PIC 9(08).
               10  ARPM-OPEN-DATE          PIC 9(08).
               10  ARPM-EFFECTIVE-DATE     PIC 9(08).
               10  ARPM-NEXT-REVIEW-DATE   PIC 9(08).
               10  ARPM-LAST-PAY-DATE      PIC 9(08).
           05  ARP-MASTER-CODES.
               10  ARPM-RECON-TYPE-CD      PIC X(03).
                   88  ARPM-RECON-TYPE-CD-EEH      VALUE 'EEH'.
                   88  ARPM-RECON-TYPE-CD-KBS      VALUE 'KBS'.
                   88  ARPM-RECON-TYPE-CD-PAC      VALUE 'PAC'.
                   88  ARPM-RECON-TYPE-CD-VKT      VALUE 'VKT'.
                   88  ARPM-RECON-TYPE-CD-YMX      VALUE 'YMX'.
                   88  ARPM-RECON-TYPE-CD-ZVT      VALUE 'ZVT'.
               10  ARPM-CYCLE-CD           PIC X(02).
                   88  ARPM-CYCLE-CD-CP            VALUE 'CP'.
                   88  ARPM-CYCLE-CD-MR            VALUE 'MR'.
                   88  ARPM-CYCLE-CD-PY            VALUE 'PY'.
                   88  ARPM-CYCLE-CD-SP            VALUE 'SP'.
                   88  ARPM-CYCLE-CD-TP            VALUE 'TP'.
               10  ARPM-REGION-CD          PIC X(02).
                   88  ARPM-REGION-CD-AD           VALUE 'AD'.
                   88  ARPM-REGION-CD-BU           VALUE 'BU'.
                   88  ARPM-REGION-CD-GM           VALUE 'GM'.
                   88  ARPM-REGION-CD-SW           VALUE 'SW'.
                   88  ARPM-REGION-CD-WD           VALUE 'WD'.
                   88  ARPM-REGION-CD-XV           VALUE 'XV'.
               10  ARPM-BRANCH-CD          PIC X(02).
                   88  ARPM-BRANCH-CD-FN           VALUE 'FN'.
                   88  ARPM-BRANCH-CD-GM           VALUE 'GM'.
                   88  ARPM-BRANCH-CD-GV           VALUE 'GV'.
                   88  ARPM-BRANCH-CD-HK           VALUE 'HK'.
                   88  ARPM-BRANCH-CD-VX           VALUE 'VX'.
               10  ARPM-CHANNEL-CD         PIC X(02).
                   88  ARPM-CHANNEL-CD-ME          VALUE 'ME'.
                   88  ARPM-CHANNEL-CD-SW          VALUE 'SW'.
                   88  ARPM-CHANNEL-CD-UA          VALUE 'UA'.
                   88  ARPM-CHANNEL-CD-WB          VALUE 'WB'.
               10  ARPM-SOURCE-CD          PIC X(02).
                   88  ARPM-SOURCE-CD-GW           VALUE 'GW'.
                   88  ARPM-SOURCE-CD-MB           VALUE 'MB'.
                   88  ARPM-SOURCE-CD-MH           VALUE 'MH'.
                   88  ARPM-SOURCE-CD-UG           VALUE 'UG'.
                   88  ARPM-SOURCE-CD-UR           VALUE 'UR'.
               10  ARPM-HOLD-FLAG          PIC X(01).
                   88  ARPM-HOLD-FLAG-YES          VALUE 'Y'.
               10  ARPM-OVERRIDE-FLAG      PIC X(01).
                   88  ARPM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  ARPM-NOTICE-SENT-FLAG   PIC X(01).
                   88  ARPM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  ARPM-EXEMPT-FLAG        PIC X(01).
                   88  ARPM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  ARPM-STOP-FLAG          PIC X(01).
                   88  ARPM-STOP-FLAG-YES          VALUE 'Y'.
               10  ARPM-PREFERRED-FLAG     PIC X(01).
                   88  ARPM-PREFERRED-FLAG-YES     VALUE 'Y'.
           05  ARP-MASTER-COUNTERS.
               10  ARPM-CYCLE-CNT          PIC 9(07).
               10  ARPM-PAYMENT-CNT        PIC 9(07).
               10  ARPM-EXCEPT-CNT         PIC 9(07).
               10  ARPM-ITEM-CNT           PIC 9(07).
               10  ARPM-FEE-PCT            PIC 9(03)V9(06).
               10  ARPM-PENALTY-PCT        PIC 9(03)V9(06).
               10  ARPM-DISCOUNT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
