      *================================================================*
      * COPYBOOK : IRVALP                                              *
      * PURPOSE  : LINKAGE FOR IRA CUSTODY RULE MODULE IRVAL           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-19 MKL  ORIGINAL                                       *
      * 1998-02-20 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-12-20 EWS  ROLLOVER LIMIT REVISED CHG2426                 *
      * 2000-08-27 LMB  FMV EXCEPTIONS TO OPS QUEUE CHG9749            *
      * 2004-03-08 MKL  EXCESS LIMIT REVISED REQ8335                   *
      * 2006-01-17 SKN  PERFORMANCE - EXCESS READ AHEAD CHG0145        *
      * 2007-06-12 DPS  WITHHOLD EXCEPTIONS TO OPS QUEUE REQ2682       *
      * 2008-07-27 MKL  WITHHOLD RERUN PROTECTION PRB2483              *
      * 2009-04-10 GAF  AUDIT FINDING - DISTRIB CONTROL TOTALS CHG5103 *
      *================================================================*
       01  IRVAL-PARMS.
           05  IRVAL-WITHHOLD-YTD          PIC S9(13)V99 COMP-3.
           05  IRVAL-EXCESS-AMT            PIC S9(13)V99 COMP-3.
           05  IRVAL-ROLLOVER-AMT          PIC S9(13)V99 COMP-3.
           05  IRVAL-RMD-YTD               PIC S9(13)V99 COMP-3.
           05  IRVAL-FMV-YTD               PIC S9(13)V99 COMP-3.
           05  IRVAL-FMV-AMT               PIC S9(13)V99 COMP-3.
           05  IRVAL-NEXT-DUE-DATE         PIC 9(08).
           05  IRVAL-EFFECTIVE-DATE        PIC 9(08).
           05  IRVAL-MATURITY-DATE         PIC 9(08).
           05  IRVAL-LAST-STMT-DATE        PIC 9(08).
           05  IRVAL-TYPE-CD               PIC X(02).
               88  IRVAL-TYPE-CD-EA                VALUE 'EA'.
               88  IRVAL-TYPE-CD-HR                VALUE 'HR'.
               88  IRVAL-TYPE-CD-XZ                VALUE 'XZ'.
           05  IRVAL-DIST-CODE-CD          PIC X(01).
               88  IRVAL-DIST-CODE-CD-C            VALUE 'C'.
               88  IRVAL-DIST-CODE-CD-J            VALUE 'J'.
               88  IRVAL-DIST-CODE-CD-U            VALUE 'U'.
               88  IRVAL-DIST-CODE-CD-X            VALUE 'X'.
           05  IRVAL-CHANNEL-CD            PIC X(02).
               88  IRVAL-CHANNEL-CD-AS             VALUE 'AS'.
               88  IRVAL-CHANNEL-CD-AY             VALUE 'AY'.
               88  IRVAL-CHANNEL-CD-JR             VALUE 'JR'.
               88  IRVAL-CHANNEL-CD-TA             VALUE 'TA'.
               88  IRVAL-CHANNEL-CD-UM             VALUE 'UM'.
           05  IRVAL-REGION-CD             PIC X(02).
           05  IRVAL-HOLD-FLAG             PIC X(01).
               88  IRVAL-HOLD-FLAG-YES             VALUE 'Y'.
           05  IRVAL-OVERRIDE-FLAG         PIC X(01).
               88  IRVAL-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  IRVAL-STOP-FLAG             PIC X(01).
               88  IRVAL-STOP-FLAG-YES             VALUE 'Y'.
           05  IRVAL-ITEM-CNT              PIC 9(07).
           05  IRVAL-CYCLE-CNT             PIC 9(07).
           05  IRVAL-RATE                  PIC 9(03)V9(06).
           05  IRVAL-MARGIN                PIC 9(03)V9(06).
           05  IRVAL-CONTACT               PIC X(20).
           05  IRVAL-ADDR                  PIC X(30).
           05  IRVAL-RETURN-CODE           PIC 9(02).
