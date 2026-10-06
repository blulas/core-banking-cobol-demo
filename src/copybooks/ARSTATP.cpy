      *================================================================*
      * COPYBOOK : ARSTATP                                             *
      * PURPOSE  : LINKAGE FOR ACCOUNT RECONCILEMENT RULE MODULE ARSTAT*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-06 MKL  ORIGINAL                                       *
      * 1996-07-27 CRP  OUTSTAND STATUS CODES ADDED CHG8555            *
      * 1997-12-21 TWH  ISSUED CALCULATION CORRECTED PRB5726           *
      * 1998-09-17 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-09-14 DPS  VOID TABLE EXPANDED PRB0687                    *
      * 2003-02-08 RJT  FEE CUTOFF MOVED REQ1921                       *
      * 2004-06-20 DPS  PAID STATUS CODES ADDED REQ2391                *
      * 2004-04-03 DPS  PERFORMANCE - OUTSTAND READ AHEAD REQ4325      *
      * 2006-05-17 GAF  OUTSTAND FEE SCHEDULE UPDATE INC7964           *
      * 2006-10-22 LMB  AUDIT FINDING - STOP CONTROL TOTALS CHG4022    *
      * 2007-06-24 BHS  VOID TABLE EXPANDED PRB3561                    *
      * 2009-09-16 PQR  FEE RERUN PROTECTION CHG1552                   *
      * 2010-01-02 DPS  PERFORMANCE - STOP READ AHEAD REQ9675          *
      *================================================================*
       01  ARSTAT-PARMS.
           05  ARSTAT-ISSUED-YTD           PIC S9(13)V99 COMP-3.
           05  ARSTAT-FEE-AMT              PIC S9(13)V99 COMP-3.
           05  ARSTAT-OUTSTAND-AMT         PIC S9(13)V99 COMP-3.
           05  ARSTAT-PAID-AMT             PIC S9(13)V99 COMP-3.
           05  ARSTAT-ISSUED-AMT           PIC S9(13)V99 COMP-3.
           05  ARSTAT-OUTSTAND-YTD         PIC S9(13)V99 COMP-3.
           05  ARSTAT-CLOSE-DATE           PIC 9(08).
           05  ARSTAT-LAST-STMT-DATE       PIC 9(08).
           05  ARSTAT-NEXT-REVIEW-DATE     PIC 9(08).
           05  ARSTAT-EFFECTIVE-DATE       PIC 9(08).
           05  ARSTAT-BRANCH-CD            PIC X(02).
               88  ARSTAT-BRANCH-CD-FN             VALUE 'FN'.
               88  ARSTAT-BRANCH-CD-GM             VALUE 'GM'.
               88  ARSTAT-BRANCH-CD-GV             VALUE 'GV'.
               88  ARSTAT-BRANCH-CD-HK             VALUE 'HK'.
               88  ARSTAT-BRANCH-CD-VX             VALUE 'VX'.
           05  ARSTAT-CHANNEL-CD           PIC X(02).
               88  ARSTAT-CHANNEL-CD-ME            VALUE 'ME'.
               88  ARSTAT-CHANNEL-CD-SW            VALUE 'SW'.
               88  ARSTAT-CHANNEL-CD-UA            VALUE 'UA'.
               88  ARSTAT-CHANNEL-CD-WB            VALUE 'WB'.
           05  ARSTAT-CYCLE-CD             PIC X(02).
               88  ARSTAT-CYCLE-CD-CP              VALUE 'CP'.
               88  ARSTAT-CYCLE-CD-MR              VALUE 'MR'.
               88  ARSTAT-CYCLE-CD-PY              VALUE 'PY'.
               88  ARSTAT-CYCLE-CD-SP              VALUE 'SP'.
               88  ARSTAT-CYCLE-CD-TP              VALUE 'TP'.
           05  ARSTAT-SOURCE-CD            PIC X(02).
               88  ARSTAT-SOURCE-CD-GW             VALUE 'GW'.
               88  ARSTAT-SOURCE-CD-MB             VALUE 'MB'.
               88  ARSTAT-SOURCE-CD-MH             VALUE 'MH'.
               88  ARSTAT-SOURCE-CD-UG             VALUE 'UG'.
               88  ARSTAT-SOURCE-CD-UR             VALUE 'UR'.
           05  ARSTAT-NOTICE-SENT-FLAG     PIC X(01).
               88  ARSTAT-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  ARSTAT-PREFERRED-FLAG       PIC X(01).
               88  ARSTAT-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  ARSTAT-OVERRIDE-FLAG        PIC X(01).
               88  ARSTAT-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  ARSTAT-ITEM-CNT             PIC 9(07).
           05  ARSTAT-CYCLE-CNT            PIC 9(07).
           05  ARSTAT-PENALTY-PCT          PIC 9(03)V9(06).
           05  ARSTAT-DISCOUNT             PIC 9(03)V9(06).
           05  ARSTAT-CUST-ID              PIC X(10).
           05  ARSTAT-REF-NO               PIC X(16).
           05  ARSTAT-RETURN-CODE          PIC 9(02).
