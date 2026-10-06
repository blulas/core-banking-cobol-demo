      *================================================================*
      * COPYBOOK : IRAMST                                              *
      * PURPOSE  : IRA CUSTODY MASTER RECORD (IRAMST KSDS)             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-02-23 JLM  ORIGINAL                                       *
      * 2000-03-05 PQR  EXCESS CALCULATION CORRECTED REQ5623           *
      * 2001-04-18 TWH  ROLLOVER LIMIT REVISED INC5644                 *
      * 2006-03-25 PQR  PERFORMANCE - ROLLOVER READ AHEAD PRB9982      *
      * 2006-11-02 RJT  NEW CONTRIB REPORT COLUMNS PRB1351             *
      * 2008-03-09 JLM  CONTRIB ROUNDING - HALF UP REQ7588             *
      * 2008-12-03 JLM  WITHHOLD CALCULATION CORRECTED INC4955         *
      *================================================================*
       01  IRA-MASTER-REC.
           05  IRAM-KEY                    PIC X(12).
           05  IRAM-CUST-ID                PIC X(10).
           05  IRAM-NAME                   PIC X(30).
           05  IRAM-DESC                   PIC X(30).
           05  IRAM-REF-NO                 PIC X(16).
           05  IRAM-ADDR                   PIC X(30).
           05  IRAM-CONTACT                PIC X(20).
           05  IRA-MASTER-AMOUNTS.
               10  IRAM-CONTRIB-AMT        PIC S9(13)V99 COMP-3.
               10  IRAM-CONTRIB-YTD        PIC S9(13)V99 COMP-3.
               10  IRAM-DISTRIB-AMT        PIC S9(13)V99 COMP-3.
               10  IRAM-DISTRIB-YTD        PIC S9(13)V99 COMP-3.
               10  IRAM-WITHHOLD-AMT       PIC S9(13)V99 COMP-3.
               10  IRAM-WITHHOLD-YTD       PIC S9(13)V99 COMP-3.
               10  IRAM-ROLLOVER-AMT       PIC S9(13)V99 COMP-3.
               10  IRAM-ROLLOVER-YTD       PIC S9(13)V99 COMP-3.
               10  IRAM-RMD-AMT            PIC S9(13)V99 COMP-3.
               10  IRAM-RMD-YTD            PIC S9(13)V99 COMP-3.
               10  IRAM-FMV-AMT            PIC S9(13)V99 COMP-3.
               10  IRAM-FMV-YTD            PIC S9(13)V99 COMP-3.
               10  IRAM-EXCESS-AMT         PIC S9(13)V99 COMP-3.
               10  IRAM-EXCESS-YTD         PIC S9(13)V99 COMP-3.
           05  IRA-MASTER-DATES.
               10  IRAM-LAST-STMT-DATE     PIC 9(08).
               10  IRAM-NEXT-DUE-DATE      PIC 9(08).
               10  IRAM-EFFECTIVE-DATE     PIC 9(08).
               10  IRAM-NOTICE-DATE        PIC 9(08).
               10  IRAM-OPEN-DATE          PIC 9(08).
               10  IRAM-MATURITY-DATE      PIC 9(08).
               10  IRAM-LAST-ACT-DATE      PIC 9(08).
               10  IRAM-EXPIRY-DATE        PIC 9(08).
               10  IRAM-NEXT-REVIEW-DATE   PIC 9(08).
           05  IRA-MASTER-CODES.
               10  IRAM-IRA-TYPE-CD        PIC X(01).
                   88  IRAM-IRA-TYPE-CD-F          VALUE 'F'.
                   88  IRAM-IRA-TYPE-CD-V          VALUE 'V'.
                   88  IRAM-IRA-TYPE-CD-W          VALUE 'W'.
                   88  IRAM-IRA-TYPE-CD-Y          VALUE 'Y'.
               10  IRAM-DIST-CODE-CD       PIC X(01).
                   88  IRAM-DIST-CODE-CD-C         VALUE 'C'.
                   88  IRAM-DIST-CODE-CD-J         VALUE 'J'.
                   88  IRAM-DIST-CODE-CD-U         VALUE 'U'.
                   88  IRAM-DIST-CODE-CD-X         VALUE 'X'.
               10  IRAM-CHANNEL-CD         PIC X(02).
                   88  IRAM-CHANNEL-CD-AS          VALUE 'AS'.
                   88  IRAM-CHANNEL-CD-AY          VALUE 'AY'.
                   88  IRAM-CHANNEL-CD-JR          VALUE 'JR'.
                   88  IRAM-CHANNEL-CD-TA          VALUE 'TA'.
                   88  IRAM-CHANNEL-CD-UM          VALUE 'UM'.
               10  IRAM-TYPE-CD            PIC X(02).
                   88  IRAM-TYPE-CD-EA             VALUE 'EA'.
                   88  IRAM-TYPE-CD-HR             VALUE 'HR'.
                   88  IRAM-TYPE-CD-XZ             VALUE 'XZ'.
               10  IRAM-REGION-CD          PIC X(02).
               10  IRAM-REASON-CD          PIC X(03).
                   88  IRAM-REASON-CD-CMR          VALUE 'CMR'.
                   88  IRAM-REASON-CD-LJS          VALUE 'LJS'.
                   88  IRAM-REASON-CD-NRT          VALUE 'NRT'.
                   88  IRAM-REASON-CD-UGA          VALUE 'UGA'.
                   88  IRAM-REASON-CD-WBS          VALUE 'WBS'.
                   88  IRAM-REASON-CD-ZYU          VALUE 'ZYU'.
               10  IRAM-SUSPEND-FLAG       PIC X(01).
                   88  IRAM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  IRAM-STOP-FLAG          PIC X(01).
                   88  IRAM-STOP-FLAG-YES          VALUE 'Y'.
               10  IRAM-OVERRIDE-FLAG      PIC X(01).
                   88  IRAM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  IRAM-AUTO-RENEW-FLAG    PIC X(01).
                   88  IRAM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  IRAM-REVIEW-FLAG        PIC X(01).
                   88  IRAM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  IRAM-HOLD-FLAG          PIC X(01).
                   88  IRAM-HOLD-FLAG-YES          VALUE 'Y'.
           05  IRA-MASTER-COUNTERS.
               10  IRAM-PAYMENT-CNT        PIC 9(07).
               10  IRAM-ITEM-CNT           PIC 9(07).
               10  IRAM-CYCLE-CNT          PIC 9(07).
               10  IRAM-RETRY-CNT          PIC 9(07).
               10  IRAM-RATE               PIC 9(03)V9(06).
               10  IRAM-MARGIN             PIC 9(03)V9(06).
               10  IRAM-FEE-PCT            PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
