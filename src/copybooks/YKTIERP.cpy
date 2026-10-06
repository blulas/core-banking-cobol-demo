      *================================================================*
      * COPYBOOK : YKTIERP                                             *
      * PURPOSE  : LINKAGE FOR DATE WINDOW BRIDGE RULE MODULE YKTIER   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-02-08 LMB  ORIGINAL                                       *
      * 1993-04-11 DPS  PERFORMANCE - AMOUNT-2 READ AHEAD INC7937      *
      * 1994-06-04 LMB  AUDIT FINDING - CONTROL CONTROL TOTALS REQ3590 *
      * 1994-04-17 PQR  AMOUNT-1 TABLE EXPANDED INC4216                *
      * 1998-04-01 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-06-24 MKL  AMOUNT-1 STATUS CODES ADDED CHG3892            *
      * 2003-08-09 BHS  AMOUNT-1 CUTOFF MOVED INC2451                  *
      * 2004-02-18 RJT  AMOUNT-2 STATUS CODES ADDED PRB5301            *
      * 2005-02-03 LMB  CONTROL CALCULATION CORRECTED INC2674          *
      * 2006-03-25 CRP  AMOUNT-2 LIMIT REVISED INC4506                 *
      * 2006-10-13 MKL  AMOUNT-1 TABLE EXPANDED REQ0506                *
      * 2007-08-07 EWS  CONTROL CALCULATION CORRECTED INC8902          *
      * 2008-02-14 BHS  CONTROL LIMIT REVISED PRB7990                  *
      *================================================================*
       01  YKTIER-PARMS.
           05  YKTIER-AMOUNT-2-YTD         PIC S9(13)V99 COMP-3.
           05  YKTIER-CONTROL-YTD          PIC S9(13)V99 COMP-3.
           05  YKTIER-AMOUNT-1-YTD         PIC S9(13)V99 COMP-3.
           05  YKTIER-AMOUNT-2-AMT         PIC S9(13)V99 COMP-3.
           05  YKTIER-AMOUNT-1-AMT         PIC S9(13)V99 COMP-3.
           05  YKTIER-CONTROL-AMT          PIC S9(13)V99 COMP-3.
           05  YKTIER-LAST-ACT-DATE        PIC 9(08).
           05  YKTIER-MATURITY-DATE        PIC 9(08).
           05  YKTIER-LAST-MAINT-DATE      PIC 9(08).
           05  YKTIER-EXPIRY-DATE          PIC 9(08).
           05  YKTIER-CYCLE-CD             PIC X(02).
               88  YKTIER-CYCLE-CD-FJ              VALUE 'FJ'.
               88  YKTIER-CYCLE-CD-LA              VALUE 'LA'.
               88  YKTIER-CYCLE-CD-SP              VALUE 'SP'.
               88  YKTIER-CYCLE-CD-UC              VALUE 'UC'.
           05  YKTIER-SOURCE-CD            PIC X(03).
               88  YKTIER-SOURCE-CD-BNX            VALUE 'BNX'.
               88  YKTIER-SOURCE-CD-FTB            VALUE 'FTB'.
               88  YKTIER-SOURCE-CD-GAP            VALUE 'GAP'.
               88  YKTIER-SOURCE-CD-PAP            VALUE 'PAP'.
               88  YKTIER-SOURCE-CD-VGC            VALUE 'VGC'.
               88  YKTIER-SOURCE-CD-VJV            VALUE 'VJV'.
           05  YKTIER-REGION-CD            PIC X(01).
           05  YKTIER-PIVOT-CD             PIC X(03).
               88  YKTIER-PIVOT-CD-KGT             VALUE 'KGT'.
               88  YKTIER-PIVOT-CD-NNG             VALUE 'NNG'.
               88  YKTIER-PIVOT-CD-PTC             VALUE 'PTC'.
               88  YKTIER-PIVOT-CD-WZW             VALUE 'WZW'.
           05  YKTIER-PREFERRED-FLAG       PIC X(01).
               88  YKTIER-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  YKTIER-AUTO-RENEW-FLAG      PIC X(01).
               88  YKTIER-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  YKTIER-REVIEW-FLAG          PIC X(01).
               88  YKTIER-REVIEW-FLAG-YES          VALUE 'Y'.
           05  YKTIER-ITEM-CNT             PIC 9(07).
           05  YKTIER-RETRY-CNT            PIC 9(07).
           05  YKTIER-DISCOUNT             PIC 9(03)V9(06).
           05  YKTIER-PENALTY-PCT          PIC 9(03)V9(06).
           05  YKTIER-CUST-ID              PIC X(10).
           05  YKTIER-CONTACT              PIC X(20).
           05  YKTIER-RETURN-CODE          PIC 9(02).
