      *================================================================*
      * COPYBOOK : CCOMST                                              *
      * PURPOSE  : COIN AND CURRENCY MASTER RECORD (CCOMST KSDS)       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-05-15 DPS  ORIGINAL                                       *
      * 1988-01-24 DPS  CURRENCY FEE SCHEDULE UPDATE REQ8061           *
      * 1988-07-04 BHS  PERFORMANCE - SHIPMENT READ AHEAD CHG8363      *
      * 1989-12-07 LMB  VARIANCE LIMIT REVISED PRB5563                 *
      * 1990-07-06 MKL  VARIANCE ROUNDING - HALF UP CHG1267            *
      * 1991-12-21 MKL  PERFORMANCE - VARIANCE READ AHEAD INC6838      *
      * 1997-09-14 DPS  PERFORMANCE - VARIANCE READ AHEAD CHG5609      *
      * 1998-07-03 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-10-14 MKL  STRAP TABLE EXPANDED CHG1853                   *
      * 2002-02-08 CRP  SHIPMENT LIMIT REVISED PRB3445                 *
      * 2004-04-08 EWS  COIN FEE SCHEDULE UPDATE REQ6287               *
      * 2004-02-24 GAF  VARIANCE CUTOFF MOVED PRB4192                  *
      * 2004-04-16 CRP  COIN EXCEPTIONS TO OPS QUEUE CHG5725           *
      *================================================================*
       01  CCO-MASTER-REC.
           05  CCOM-KEY                    PIC X(12).
           05  CCOM-CUST-ID                PIC X(10).
           05  CCOM-NAME                   PIC X(30).
           05  CCOM-DESC                   PIC X(30).
           05  CCOM-REF-NO                 PIC X(16).
           05  CCOM-ADDR                   PIC X(30).
           05  CCOM-CONTACT                PIC X(20).
           05  CCO-MASTER-AMOUNTS.
               10  CCOM-COIN-AMT           PIC S9(13)V99 COMP-3.
               10  CCOM-COIN-YTD           PIC S9(13)V99 COMP-3.
               10  CCOM-CURRENCY-AMT       PIC S9(13)V99 COMP-3.
               10  CCOM-CURRENCY-YTD       PIC S9(13)V99 COMP-3.
               10  CCOM-STRAP-AMT          PIC S9(13)V99 COMP-3.
               10  CCOM-STRAP-YTD          PIC S9(13)V99 COMP-3.
               10  CCOM-SHIPMENT-AMT       PIC S9(13)V99 COMP-3.
               10  CCOM-SHIPMENT-YTD       PIC S9(13)V99 COMP-3.
               10  CCOM-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  CCOM-FEE-YTD            PIC S9(13)V99 COMP-3.
               10  CCOM-VARIANCE-AMT       PIC S9(13)V99 COMP-3.
               10  CCOM-VARIANCE-YTD       PIC S9(13)V99 COMP-3.
           05  CCO-MASTER-DATES.
               10  CCOM-LAST-ACT-DATE      PIC 9(08).
               10  CCOM-LAST-PAY-DATE      PIC 9(08).
               10  CCOM-OPEN-DATE          PIC 9(08).
               10  CCOM-NOTICE-DATE        PIC 9(08).
               10  CCOM-NEXT-DUE-DATE      PIC 9(08).
               10  CCOM-EFFECTIVE-DATE     PIC 9(08).
               10  CCOM-EXPIRY-DATE        PIC 9(08).
               10  CCOM-MATURITY-DATE      PIC 9(08).
               10  CCOM-NEXT-REVIEW-DATE   PIC 9(08).
           05  CCO-MASTER-CODES.
               10  CCOM-CARRIER-CD         PIC X(01).
                   88  CCOM-CARRIER-CD-J           VALUE 'J'.
                   88  CCOM-CARRIER-CD-L           VALUE 'L'.
                   88  CCOM-CARRIER-CD-M           VALUE 'M'.
                   88  CCOM-CARRIER-CD-X           VALUE 'X'.
               10  CCOM-DENOM-CD           PIC X(01).
                   88  CCOM-DENOM-CD-B             VALUE 'B'.
                   88  CCOM-DENOM-CD-D             VALUE 'D'.
                   88  CCOM-DENOM-CD-P             VALUE 'P'.
                   88  CCOM-DENOM-CD-S             VALUE 'S'.
                   88  CCOM-DENOM-CD-W             VALUE 'W'.
               10  CCOM-SOURCE-CD          PIC X(03).
               10  CCOM-REASON-CD          PIC X(02).
                   88  CCOM-REASON-CD-JL           VALUE 'JL'.
                   88  CCOM-REASON-CD-LW           VALUE 'LW'.
                   88  CCOM-REASON-CD-PS           VALUE 'PS'.
               10  CCOM-CHANNEL-CD         PIC X(02).
                   88  CCOM-CHANNEL-CD-AW          VALUE 'AW'.
                   88  CCOM-CHANNEL-CD-DW          VALUE 'DW'.
                   88  CCOM-CHANNEL-CD-GN          VALUE 'GN'.
                   88  CCOM-CHANNEL-CD-RS          VALUE 'RS'.
                   88  CCOM-CHANNEL-CD-UZ          VALUE 'UZ'.
                   88  CCOM-CHANNEL-CD-YR          VALUE 'YR'.
               10  CCOM-STATUS-CD          PIC X(03).
               10  CCOM-HOLD-FLAG          PIC X(01).
                   88  CCOM-HOLD-FLAG-YES          VALUE 'Y'.
               10  CCOM-EXEMPT-FLAG        PIC X(01).
                   88  CCOM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  CCOM-STOP-FLAG          PIC X(01).
                   88  CCOM-STOP-FLAG-YES          VALUE 'Y'.
               10  CCOM-AUTO-RENEW-FLAG    PIC X(01).
                   88  CCOM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  CCOM-OVERRIDE-FLAG      PIC X(01).
                   88  CCOM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  CCOM-NOTICE-SENT-FLAG   PIC X(01).
                   88  CCOM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
           05  CCO-MASTER-COUNTERS.
               10  CCOM-NOTICE-CNT         PIC 9(07).
               10  CCOM-CYCLE-CNT          PIC 9(07).
               10  CCOM-ITEM-CNT           PIC 9(07).
               10  CCOM-RETRY-CNT          PIC 9(07).
               10  CCOM-FEE-PCT            PIC 9(03)V9(06).
               10  CCOM-RATE               PIC 9(03)V9(06).
               10  CCOM-PENALTY-PCT        PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
