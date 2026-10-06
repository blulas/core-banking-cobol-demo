      *================================================================*
      * COPYBOOK : ARDTEP                                              *
      * PURPOSE  : LINKAGE FOR ACCOUNT RECONCILEMENT RULE MODULE ARDTE *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-27 TWH  ORIGINAL                                       *
      * 2001-11-24 JLM  VOID LIMIT REVISED PRB2615                     *
      * 2005-08-06 CRP  ISSUED ROUNDING - HALF UP INC3426              *
      * 2005-10-02 PQR  PAID CUTOFF MOVED PRB0523                      *
      * 2005-06-02 GAF  STOP ROUNDING - HALF UP INC3956                *
      * 2006-06-14 RJT  PAID LIMIT REVISED CHG3400                     *
      * 2007-09-28 CRP  NEW OUTSTAND REPORT COLUMNS PRB6255            *
      * 2010-06-08 SKN  STOP CALCULATION CORRECTED CHG6577             *
      * 2012-09-17 EWS  OUTSTAND ROUNDING - HALF UP CHG1554            *
      *================================================================*
       01  ARDTE-PARMS.
           05  ARDTE-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  ARDTE-ISSUED-YTD            PIC S9(13)V99 COMP-3.
           05  ARDTE-PAID-YTD              PIC S9(13)V99 COMP-3.
           05  ARDTE-VOID-YTD              PIC S9(13)V99 COMP-3.
           05  ARDTE-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  ARDTE-VOID-AMT              PIC S9(13)V99 COMP-3.
           05  ARDTE-CLOSE-DATE            PIC 9(08).
           05  ARDTE-LAST-MAINT-DATE       PIC 9(08).
           05  ARDTE-LAST-STMT-DATE        PIC 9(08).
           05  ARDTE-LAST-PAY-DATE         PIC 9(08).
           05  ARDTE-SOURCE-CD             PIC X(02).
               88  ARDTE-SOURCE-CD-GW              VALUE 'GW'.
               88  ARDTE-SOURCE-CD-MB              VALUE 'MB'.
               88  ARDTE-SOURCE-CD-MH              VALUE 'MH'.
               88  ARDTE-SOURCE-CD-UG              VALUE 'UG'.
               88  ARDTE-SOURCE-CD-UR              VALUE 'UR'.
           05  ARDTE-RECON-TYPE-CD         PIC X(03).
               88  ARDTE-RECON-TYPE-CD-EEH         VALUE 'EEH'.
               88  ARDTE-RECON-TYPE-CD-KBS         VALUE 'KBS'.
               88  ARDTE-RECON-TYPE-CD-PAC         VALUE 'PAC'.
               88  ARDTE-RECON-TYPE-CD-VKT         VALUE 'VKT'.
               88  ARDTE-RECON-TYPE-CD-YMX         VALUE 'YMX'.
               88  ARDTE-RECON-TYPE-CD-ZVT         VALUE 'ZVT'.
           05  ARDTE-REGION-CD             PIC X(02).
               88  ARDTE-REGION-CD-AD              VALUE 'AD'.
               88  ARDTE-REGION-CD-BU              VALUE 'BU'.
               88  ARDTE-REGION-CD-GM              VALUE 'GM'.
               88  ARDTE-REGION-CD-SW              VALUE 'SW'.
               88  ARDTE-REGION-CD-WD              VALUE 'WD'.
               88  ARDTE-REGION-CD-XV              VALUE 'XV'.
           05  ARDTE-CYCLE-CD              PIC X(02).
               88  ARDTE-CYCLE-CD-CP               VALUE 'CP'.
               88  ARDTE-CYCLE-CD-MR               VALUE 'MR'.
               88  ARDTE-CYCLE-CD-PY               VALUE 'PY'.
               88  ARDTE-CYCLE-CD-SP               VALUE 'SP'.
               88  ARDTE-CYCLE-CD-TP               VALUE 'TP'.
           05  ARDTE-EXEMPT-FLAG           PIC X(01).
               88  ARDTE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  ARDTE-PREFERRED-FLAG        PIC X(01).
               88  ARDTE-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  ARDTE-STOP-FLAG             PIC X(01).
               88  ARDTE-STOP-FLAG-YES             VALUE 'Y'.
           05  ARDTE-ITEM-CNT              PIC 9(07).
           05  ARDTE-CYCLE-CNT             PIC 9(07).
           05  ARDTE-DISCOUNT              PIC 9(03)V9(06).
           05  ARDTE-PENALTY-PCT           PIC 9(03)V9(06).
           05  ARDTE-CUST-ID               PIC X(10).
           05  ARDTE-NAME                  PIC X(30).
           05  ARDTE-RETURN-CODE           PIC 9(02).
