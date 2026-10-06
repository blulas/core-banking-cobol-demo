      *================================================================*
      * COPYBOOK : ARPV1                                               *
      * PURPOSE  : ACCOUNT RECONCILEMENT ARCHIVE RECORD LAYOUT         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-18 LMB  ORIGINAL                                       *
      * 1998-11-06 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-08-04 DPS  OUTSTAND NOTICE TEXT PER LEGAL INC8007         *
      * 2005-09-25 CRP  AUDIT FINDING - FEE CONTROL TOTALS PRB7258     *
      * 2007-09-26 SKN  PAID RERUN PROTECTION PRB3800                  *
      * 2008-05-22 EWS  AUDIT FINDING - ISSUED CONTROL TOTALS PRB2968  *
      * 2010-12-12 JLM  VOID CALCULATION CORRECTED REQ3925             *
      * 2012-08-02 GAF  PAID ROUNDING - HALF UP REQ2180                *
      * 2013-06-02 PQR  AUDIT FINDING - OUTSTAND CONTROL TOTALS REQ4736*
      * 2013-06-08 EWS  PAID NOTICE TEXT PER LEGAL CHG8768             *
      *================================================================*
       01  ARP-H1-REC.
           05  ARPH1-NAME                  PIC X(30).
           05  ARPH1-CONTACT               PIC X(20).
           05  ARPH1-REF-NO                PIC X(16).
           05  ARPH1-CUST-ID               PIC X(10).
           05  ARPH1-DESC                  PIC X(30).
           05  ARPH1-ADDR                  PIC X(30).
           05  ARP-H1-AMOUNTS.
               10  ARPH1-PAID-YTD          PIC S9(13)V99 COMP-3.
               10  ARPH1-OUTSTAND-AMT      PIC S9(13)V99 COMP-3.
               10  ARPH1-OUTSTAND-YTD      PIC S9(13)V99 COMP-3.
               10  ARPH1-STOP-AMT          PIC S9(13)V99 COMP-3.
               10  ARPH1-ISSUED-YTD        PIC S9(13)V99 COMP-3.
               10  ARPH1-PAID-AMT          PIC S9(13)V99 COMP-3.
               10  ARPH1-ISSUED-AMT        PIC S9(13)V99 COMP-3.
               10  ARPH1-FEE-AMT           PIC S9(13)V99 COMP-3.
               10  ARPH1-VOID-AMT          PIC S9(13)V99 COMP-3.
               10  ARPH1-STOP-YTD          PIC S9(13)V99 COMP-3.
               10  ARPH1-VOID-YTD          PIC S9(13)V99 COMP-3.
               10  ARPH1-FEE-YTD           PIC S9(13)V99 COMP-3.
           05  ARP-H1-DATES.
               10  ARPH1-LAST-MAINT-DATE   PIC 9(08).
               10  ARPH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  ARPH1-CLOSE-DATE        PIC 9(08).
               10  ARPH1-LAST-STMT-DATE    PIC 9(08).
               10  ARPH1-LAST-PAY-DATE     PIC 9(08).
               10  ARPH1-LAST-ACT-DATE     PIC 9(08).
               10  ARPH1-OPEN-DATE         PIC 9(08).
           05  ARP-H1-CODES.
               10  ARPH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  ARPH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  ARPH1-CYCLE-CD          PIC X(02).
                   88  ARPH1-CYCLE-CD-CP           VALUE 'CP'.
                   88  ARPH1-CYCLE-CD-MR           VALUE 'MR'.
                   88  ARPH1-CYCLE-CD-PY           VALUE 'PY'.
                   88  ARPH1-CYCLE-CD-SP           VALUE 'SP'.
                   88  ARPH1-CYCLE-CD-TP           VALUE 'TP'.
               10  ARPH1-OVERRIDE-FLAG     PIC X(01).
                   88  ARPH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  ARPH1-RECON-TYPE-CD     PIC X(03).
                   88  ARPH1-RECON-TYPE-CD-EEH     VALUE 'EEH'.
                   88  ARPH1-RECON-TYPE-CD-KBS     VALUE 'KBS'.
                   88  ARPH1-RECON-TYPE-CD-PAC     VALUE 'PAC'.
                   88  ARPH1-RECON-TYPE-CD-VKT     VALUE 'VKT'.
                   88  ARPH1-RECON-TYPE-CD-YMX     VALUE 'YMX'.
                   88  ARPH1-RECON-TYPE-CD-ZVT     VALUE 'ZVT'.
               10  ARPH1-CHANNEL-CD        PIC X(02).
                   88  ARPH1-CHANNEL-CD-ME         VALUE 'ME'.
                   88  ARPH1-CHANNEL-CD-SW         VALUE 'SW'.
                   88  ARPH1-CHANNEL-CD-UA         VALUE 'UA'.
                   88  ARPH1-CHANNEL-CD-WB         VALUE 'WB'.
               10  ARPH1-REGION-CD         PIC X(02).
                   88  ARPH1-REGION-CD-AD          VALUE 'AD'.
                   88  ARPH1-REGION-CD-BU          VALUE 'BU'.
                   88  ARPH1-REGION-CD-GM          VALUE 'GM'.
                   88  ARPH1-REGION-CD-SW          VALUE 'SW'.
                   88  ARPH1-REGION-CD-WD          VALUE 'WD'.
                   88  ARPH1-REGION-CD-XV          VALUE 'XV'.
               10  ARPH1-SOURCE-CD         PIC X(02).
                   88  ARPH1-SOURCE-CD-GW          VALUE 'GW'.
                   88  ARPH1-SOURCE-CD-MB          VALUE 'MB'.
                   88  ARPH1-SOURCE-CD-MH          VALUE 'MH'.
                   88  ARPH1-SOURCE-CD-UG          VALUE 'UG'.
                   88  ARPH1-SOURCE-CD-UR          VALUE 'UR'.
               10  ARPH1-HOLD-FLAG         PIC X(01).
                   88  ARPH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  ARPH1-EXEMPT-FLAG       PIC X(01).
                   88  ARPH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  ARPH1-PREFERRED-FLAG    PIC X(01).
                   88  ARPH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  ARPH1-STOP-FLAG         PIC X(01).
                   88  ARPH1-STOP-FLAG-YES         VALUE 'Y'.
           05  ARP-H1-COUNTERS.
               10  ARPH1-ITEM-CNT          PIC 9(07).
               10  ARPH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  ARPH1-DISCOUNT          PIC 9(03)V9(06).
               10  ARPH1-EXCEPT-CNT        PIC 9(07).
               10  ARPH1-CYCLE-CNT         PIC 9(07).
               10  ARPH1-PAYMENT-CNT       PIC 9(07).
               10  ARPH1-FEE-PCT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
