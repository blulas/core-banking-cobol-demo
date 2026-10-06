      *================================================================*
      * COPYBOOK : EURMST                                              *
      * PURPOSE  : EURO CONVERSION MASTER RECORD (EURMST KSDS)         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-04-10 CRP  ORIGINAL                                       *
      * 1999-05-25 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-06-18 TWH  ROUNDING FEE SCHEDULE UPDATE PRB5790           *
      * 2002-02-27 GAF  ROUNDING CUTOFF MOVED CHG2117                  *
      * 2003-08-23 LMB  ROUNDING EXCEPTIONS TO OPS QUEUE REQ5136       *
      * 2003-09-20 CRP  EURO CALCULATION CORRECTED PRB1963             *
      * 2006-10-24 EWS  PERFORMANCE - CONVERTED READ AHEAD REQ4547     *
      * 2008-03-10 SKN  TRIANGULAR ROUNDING - HALF UP INC4927          *
      * 2009-10-19 GAF  AUDIT FINDING - TRIANGULAR CONTROL TOTALS PRB03*
      *================================================================*
       01  EUR-MASTER-REC.
           05  EURM-KEY                    PIC X(12).
           05  EURM-CUST-ID                PIC X(10).
           05  EURM-NAME                   PIC X(30).
           05  EURM-DESC                   PIC X(30).
           05  EURM-REF-NO                 PIC X(16).
           05  EURM-ADDR                   PIC X(30).
           05  EURM-CONTACT                PIC X(20).
           05  EUR-MASTER-AMOUNTS.
               10  EURM-NATIONAL-AMT       PIC S9(13)V99 COMP-3.
               10  EURM-NATIONAL-YTD       PIC S9(13)V99 COMP-3.
               10  EURM-EURO-AMT           PIC S9(13)V99 COMP-3.
               10  EURM-EURO-YTD           PIC S9(13)V99 COMP-3.
               10  EURM-ROUNDING-AMT       PIC S9(13)V99 COMP-3.
               10  EURM-ROUNDING-YTD       PIC S9(13)V99 COMP-3.
               10  EURM-TRIANGULAR-AMT     PIC S9(13)V99 COMP-3.
               10  EURM-TRIANGULAR-YTD     PIC S9(13)V99 COMP-3.
               10  EURM-CONVERTED-AMT      PIC S9(13)V99 COMP-3.
               10  EURM-CONVERTED-YTD      PIC S9(13)V99 COMP-3.
               10  EURM-RESIDUAL-AMT       PIC S9(13)V99 COMP-3.
               10  EURM-RESIDUAL-YTD       PIC S9(13)V99 COMP-3.
               10  EURM-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  EURM-FEE-YTD            PIC S9(13)V99 COMP-3.
           05  EUR-MASTER-DATES.
               10  EURM-NEXT-REVIEW-DATE   PIC 9(08).
               10  EURM-NOTICE-DATE        PIC 9(08).
               10  EURM-OPEN-DATE          PIC 9(08).
               10  EURM-MATURITY-DATE      PIC 9(08).
               10  EURM-LAST-MAINT-DATE    PIC 9(08).
               10  EURM-EFFECTIVE-DATE     PIC 9(08).
               10  EURM-LAST-STMT-DATE     PIC 9(08).
               10  EURM-LAST-PAY-DATE      PIC 9(08).
               10  EURM-NEXT-DUE-DATE      PIC 9(08).
           05  EUR-MASTER-CODES.
               10  EURM-CURRENCY-CD        PIC X(02).
                   88  EURM-CURRENCY-CD-EW         VALUE 'EW'.
                   88  EURM-CURRENCY-CD-FJ         VALUE 'FJ'.
                   88  EURM-CURRENCY-CD-JC         VALUE 'JC'.
               10  EURM-CONV-METHOD-CD     PIC X(02).
                   88  EURM-CONV-METHOD-CD-GL      VALUE 'GL'.
                   88  EURM-CONV-METHOD-CD-JA      VALUE 'JA'.
                   88  EURM-CONV-METHOD-CD-LD      VALUE 'LD'.
                   88  EURM-CONV-METHOD-CD-MR      VALUE 'MR'.
                   88  EURM-CONV-METHOD-CD-VC      VALUE 'VC'.
               10  EURM-STATUS-CD          PIC X(01).
               10  EURM-TYPE-CD            PIC X(03).
                   88  EURM-TYPE-CD-AGL            VALUE 'AGL'.
                   88  EURM-TYPE-CD-SMF            VALUE 'SMF'.
                   88  EURM-TYPE-CD-UEC            VALUE 'UEC'.
                   88  EURM-TYPE-CD-VJA            VALUE 'VJA'.
                   88  EURM-TYPE-CD-WPH            VALUE 'WPH'.
               10  EURM-CHANNEL-CD         PIC X(03).
                   88  EURM-CHANNEL-CD-AEA         VALUE 'AEA'.
                   88  EURM-CHANNEL-CD-EBM         VALUE 'EBM'.
                   88  EURM-CHANNEL-CD-GAU         VALUE 'GAU'.
                   88  EURM-CHANNEL-CD-GSU         VALUE 'GSU'.
                   88  EURM-CHANNEL-CD-VWP         VALUE 'VWP'.
                   88  EURM-CHANNEL-CD-XDF         VALUE 'XDF'.
               10  EURM-REGION-CD          PIC X(03).
                   88  EURM-REGION-CD-DTL          VALUE 'DTL'.
                   88  EURM-REGION-CD-HYD          VALUE 'HYD'.
                   88  EURM-REGION-CD-JUL          VALUE 'JUL'.
                   88  EURM-REGION-CD-WSE          VALUE 'WSE'.
                   88  EURM-REGION-CD-YAU          VALUE 'YAU'.
                   88  EURM-REGION-CD-YJB          VALUE 'YJB'.
               10  EURM-REVIEW-FLAG        PIC X(01).
                   88  EURM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  EURM-AUTO-RENEW-FLAG    PIC X(01).
                   88  EURM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  EURM-EXEMPT-FLAG        PIC X(01).
                   88  EURM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  EURM-OVERRIDE-FLAG      PIC X(01).
                   88  EURM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  EURM-PREFERRED-FLAG     PIC X(01).
                   88  EURM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  EURM-STOP-FLAG          PIC X(01).
                   88  EURM-STOP-FLAG-YES          VALUE 'Y'.
           05  EUR-MASTER-COUNTERS.
               10  EURM-ITEM-CNT           PIC 9(07).
               10  EURM-EXCEPT-CNT         PIC 9(07).
               10  EURM-PAYMENT-CNT        PIC 9(07).
               10  EURM-CYCLE-CNT          PIC 9(07).
               10  EURM-PENALTY-PCT        PIC 9(03)V9(06).
               10  EURM-FEE-PCT            PIC 9(03)V9(06).
               10  EURM-RATE               PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
