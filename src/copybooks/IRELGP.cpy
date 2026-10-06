      *================================================================*
      * COPYBOOK : IRELGP                                              *
      * PURPOSE  : LINKAGE FOR IRA CUSTODY RULE MODULE IRELG           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-09-05 JLM  ORIGINAL                                       *
      * 1996-05-18 SKN  FMV FEE SCHEDULE UPDATE INC6874                *
      * 1997-08-14 LMB  WITHHOLD FEE SCHEDULE UPDATE CHG8590           *
      * 1997-12-18 JLM  FMV FEE SCHEDULE UPDATE CHG9312                *
      * 2001-10-19 MKL  ROLLOVER FEE SCHEDULE UPDATE REQ0787           *
      * 2004-02-01 LMB  CONTRIB FEE SCHEDULE UPDATE CHG9046            *
      * 2004-09-07 RJT  RMD LIMIT REVISED CHG2145                      *
      * 2010-11-12 PQR  ROLLOVER EXCEPTIONS TO OPS QUEUE INC5823       *
      * 2011-04-26 PQR  ROLLOVER STATUS CODES ADDED CHG4604            *
      *================================================================*
       01  IRELG-PARMS.
           05  IRELG-FMV-AMT               PIC S9(13)V99 COMP-3.
           05  IRELG-EXCESS-AMT            PIC S9(13)V99 COMP-3.
           05  IRELG-ROLLOVER-YTD          PIC S9(13)V99 COMP-3.
           05  IRELG-RMD-AMT               PIC S9(13)V99 COMP-3.
           05  IRELG-ROLLOVER-AMT          PIC S9(13)V99 COMP-3.
           05  IRELG-FMV-YTD               PIC S9(13)V99 COMP-3.
           05  IRELG-LAST-ACT-DATE         PIC 9(08).
           05  IRELG-LAST-STMT-DATE        PIC 9(08).
           05  IRELG-EXPIRY-DATE           PIC 9(08).
           05  IRELG-NEXT-REVIEW-DATE      PIC 9(08).
           05  IRELG-CHANNEL-CD            PIC X(02).
               88  IRELG-CHANNEL-CD-AS             VALUE 'AS'.
               88  IRELG-CHANNEL-CD-AY             VALUE 'AY'.
               88  IRELG-CHANNEL-CD-JR             VALUE 'JR'.
               88  IRELG-CHANNEL-CD-TA             VALUE 'TA'.
               88  IRELG-CHANNEL-CD-UM             VALUE 'UM'.
           05  IRELG-REGION-CD             PIC X(02).
           05  IRELG-REASON-CD             PIC X(03).
               88  IRELG-REASON-CD-CMR             VALUE 'CMR'.
               88  IRELG-REASON-CD-LJS             VALUE 'LJS'.
               88  IRELG-REASON-CD-NRT             VALUE 'NRT'.
               88  IRELG-REASON-CD-UGA             VALUE 'UGA'.
               88  IRELG-REASON-CD-WBS             VALUE 'WBS'.
               88  IRELG-REASON-CD-ZYU             VALUE 'ZYU'.
           05  IRELG-TYPE-CD               PIC X(02).
               88  IRELG-TYPE-CD-EA                VALUE 'EA'.
               88  IRELG-TYPE-CD-HR                VALUE 'HR'.
               88  IRELG-TYPE-CD-XZ                VALUE 'XZ'.
           05  IRELG-STOP-FLAG             PIC X(01).
               88  IRELG-STOP-FLAG-YES             VALUE 'Y'.
           05  IRELG-REVIEW-FLAG           PIC X(01).
               88  IRELG-REVIEW-FLAG-YES           VALUE 'Y'.
           05  IRELG-AUTO-RENEW-FLAG       PIC X(01).
               88  IRELG-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  IRELG-CYCLE-CNT             PIC 9(07).
           05  IRELG-RETRY-CNT             PIC 9(07).
           05  IRELG-MARGIN                PIC 9(03)V9(06).
           05  IRELG-FEE-PCT               PIC 9(03)V9(06).
           05  IRELG-ADDR                  PIC X(30).
           05  IRELG-NAME                  PIC X(30).
           05  IRELG-RETURN-CODE           PIC 9(02).
