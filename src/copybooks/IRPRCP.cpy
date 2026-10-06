      *================================================================*
      * COPYBOOK : IRPRCP                                              *
      * PURPOSE  : LINKAGE FOR IRA CUSTODY RULE MODULE IRPRC           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-06-06 SKN  ORIGINAL                                       *
      * 1997-05-02 CRP  NEW FMV REPORT COLUMNS PRB2568                 *
      * 1998-07-02 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-07-08 MKL  EXCESS FEE SCHEDULE UPDATE CHG5282             *
      * 2001-06-17 MKL  FMV TABLE EXPANDED CHG4749                     *
      * 2002-06-01 MKL  CONTRIB CUTOFF MOVED REQ9598                   *
      * 2004-04-25 BHS  WITHHOLD ROUNDING - HALF UP INC7893            *
      * 2011-11-19 TWH  DISTRIB STATUS CODES ADDED CHG5949             *
      *================================================================*
       01  IRPRC-PARMS.
           05  IRPRC-RMD-YTD               PIC S9(13)V99 COMP-3.
           05  IRPRC-CONTRIB-YTD           PIC S9(13)V99 COMP-3.
           05  IRPRC-EXCESS-YTD            PIC S9(13)V99 COMP-3.
           05  IRPRC-DISTRIB-YTD           PIC S9(13)V99 COMP-3.
           05  IRPRC-FMV-AMT               PIC S9(13)V99 COMP-3.
           05  IRPRC-ROLLOVER-AMT          PIC S9(13)V99 COMP-3.
           05  IRPRC-NEXT-REVIEW-DATE      PIC 9(08).
           05  IRPRC-LAST-STMT-DATE        PIC 9(08).
           05  IRPRC-LAST-ACT-DATE         PIC 9(08).
           05  IRPRC-EXPIRY-DATE           PIC 9(08).
           05  IRPRC-IRA-TYPE-CD           PIC X(01).
               88  IRPRC-IRA-TYPE-CD-F             VALUE 'F'.
               88  IRPRC-IRA-TYPE-CD-V             VALUE 'V'.
               88  IRPRC-IRA-TYPE-CD-W             VALUE 'W'.
               88  IRPRC-IRA-TYPE-CD-Y             VALUE 'Y'.
           05  IRPRC-TYPE-CD               PIC X(02).
               88  IRPRC-TYPE-CD-EA                VALUE 'EA'.
               88  IRPRC-TYPE-CD-HR                VALUE 'HR'.
               88  IRPRC-TYPE-CD-XZ                VALUE 'XZ'.
           05  IRPRC-CHANNEL-CD            PIC X(02).
               88  IRPRC-CHANNEL-CD-AS             VALUE 'AS'.
               88  IRPRC-CHANNEL-CD-AY             VALUE 'AY'.
               88  IRPRC-CHANNEL-CD-JR             VALUE 'JR'.
               88  IRPRC-CHANNEL-CD-TA             VALUE 'TA'.
               88  IRPRC-CHANNEL-CD-UM             VALUE 'UM'.
           05  IRPRC-REASON-CD             PIC X(03).
               88  IRPRC-REASON-CD-CMR             VALUE 'CMR'.
               88  IRPRC-REASON-CD-LJS             VALUE 'LJS'.
               88  IRPRC-REASON-CD-NRT             VALUE 'NRT'.
               88  IRPRC-REASON-CD-UGA             VALUE 'UGA'.
               88  IRPRC-REASON-CD-WBS             VALUE 'WBS'.
               88  IRPRC-REASON-CD-ZYU             VALUE 'ZYU'.
           05  IRPRC-OVERRIDE-FLAG         PIC X(01).
               88  IRPRC-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  IRPRC-SUSPEND-FLAG          PIC X(01).
               88  IRPRC-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  IRPRC-AUTO-RENEW-FLAG       PIC X(01).
               88  IRPRC-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  IRPRC-PAYMENT-CNT           PIC 9(07).
           05  IRPRC-RETRY-CNT             PIC 9(07).
           05  IRPRC-MARGIN                PIC 9(03)V9(06).
           05  IRPRC-FEE-PCT               PIC 9(03)V9(06).
           05  IRPRC-NAME                  PIC X(30).
           05  IRPRC-ADDR                  PIC X(30).
           05  IRPRC-RETURN-CODE           PIC 9(02).
