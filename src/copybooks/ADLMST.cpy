      *================================================================*
      * COPYBOOK : ADLMST                                              *
      * PURPOSE  : DEALER AUTO FINANCE MASTER RECORD (ADLMST KSDS)     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-09-21 LMB  ORIGINAL                                       *
      * 1995-01-06 DPS  REBATE NOTICE TEXT PER LEGAL PRB7313           *
      * 1996-09-15 MKL  AMOUNT-FIN ROUNDING - HALF UP PRB5777          *
      * 1997-04-21 BHS  AMOUNT-FIN NOTICE TEXT PER LEGAL REQ2144       *
      * 1998-11-25 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-15 JLM  PERFORMANCE - GAP READ AHEAD CHG9939           *
      * 2001-01-27 DPS  REBATE FEE SCHEDULE UPDATE INC0545             *
      * 2004-12-18 RJT  PERFORMANCE - PAYMENT READ AHEAD REQ9468       *
      * 2005-03-02 DPS  RESERVE TABLE EXPANDED CHG2988                 *
      * 2007-01-06 RJT  AUDIT FINDING - AMOUNT-FIN CONTROL TOTALS REQ30*
      * 2007-08-24 BHS  PERFORMANCE - CHARGEBK READ AHEAD CHG5888      *
      *================================================================*
       01  ADL-MASTER-REC.
           05  ADLM-KEY                    PIC X(12).
           05  ADLM-CUST-ID                PIC X(10).
           05  ADLM-NAME                   PIC X(30).
           05  ADLM-DESC                   PIC X(30).
           05  ADLM-REF-NO                 PIC X(16).
           05  ADLM-ADDR                   PIC X(30).
           05  ADLM-CONTACT                PIC X(20).
           05  ADL-MASTER-AMOUNTS.
               10  ADLM-AMOUNT-FIN-AMT     PIC S9(13)V99 COMP-3.
               10  ADLM-AMOUNT-FIN-YTD     PIC S9(13)V99 COMP-3.
               10  ADLM-RESERVE-AMT        PIC S9(13)V99 COMP-3.
               10  ADLM-RESERVE-YTD        PIC S9(13)V99 COMP-3.
               10  ADLM-CHARGEBK-AMT       PIC S9(13)V99 COMP-3.
               10  ADLM-CHARGEBK-YTD       PIC S9(13)V99 COMP-3.
               10  ADLM-REBATE-AMT         PIC S9(13)V99 COMP-3.
               10  ADLM-REBATE-YTD         PIC S9(13)V99 COMP-3.
               10  ADLM-PAYMENT-AMT        PIC S9(13)V99 COMP-3.
               10  ADLM-PAYMENT-YTD        PIC S9(13)V99 COMP-3.
               10  ADLM-GAP-AMT            PIC S9(13)V99 COMP-3.
               10  ADLM-GAP-YTD            PIC S9(13)V99 COMP-3.
           05  ADL-MASTER-DATES.
               10  ADLM-LAST-STMT-DATE     PIC 9(08).
               10  ADLM-LAST-PAY-DATE      PIC 9(08).
               10  ADLM-NOTICE-DATE        PIC 9(08).
               10  ADLM-LAST-ACT-DATE      PIC 9(08).
               10  ADLM-NEXT-DUE-DATE      PIC 9(08).
               10  ADLM-EXPIRY-DATE        PIC 9(08).
               10  ADLM-EFFECTIVE-DATE     PIC 9(08).
               10  ADLM-NEXT-REVIEW-DATE   PIC 9(08).
               10  ADLM-LAST-MAINT-DATE    PIC 9(08).
           05  ADL-MASTER-CODES.
               10  ADLM-DEALER-CD          PIC X(03).
                   88  ADLM-DEALER-CD-DRU          VALUE 'DRU'.
                   88  ADLM-DEALER-CD-FNF          VALUE 'FNF'.
                   88  ADLM-DEALER-CD-GBX          VALUE 'GBX'.
                   88  ADLM-DEALER-CD-GTU          VALUE 'GTU'.
                   88  ADLM-DEALER-CD-JNE          VALUE 'JNE'.
                   88  ADLM-DEALER-CD-KTP          VALUE 'KTP'.
               10  ADLM-TIER-CD            PIC X(02).
                   88  ADLM-TIER-CD-EW             VALUE 'EW'.
                   88  ADLM-TIER-CD-JF             VALUE 'JF'.
                   88  ADLM-TIER-CD-KD             VALUE 'KD'.
                   88  ADLM-TIER-CD-UG             VALUE 'UG'.
                   88  ADLM-TIER-CD-WY             VALUE 'WY'.
               10  ADLM-STATUS-CD          PIC X(03).
                   88  ADLM-STATUS-CD-EUA          VALUE 'EUA'.
                   88  ADLM-STATUS-CD-HMW          VALUE 'HMW'.
                   88  ADLM-STATUS-CD-RCB          VALUE 'RCB'.
               10  ADLM-TYPE-CD            PIC X(01).
                   88  ADLM-TYPE-CD-B              VALUE 'B'.
                   88  ADLM-TYPE-CD-H              VALUE 'H'.
                   88  ADLM-TYPE-CD-S              VALUE 'S'.
                   88  ADLM-TYPE-CD-V              VALUE 'V'.
                   88  ADLM-TYPE-CD-W              VALUE 'W'.
                   88  ADLM-TYPE-CD-Z              VALUE 'Z'.
               10  ADLM-REASON-CD          PIC X(03).
                   88  ADLM-REASON-CD-FZV          VALUE 'FZV'.
                   88  ADLM-REASON-CD-KPP          VALUE 'KPP'.
                   88  ADLM-REASON-CD-MVN          VALUE 'MVN'.
                   88  ADLM-REASON-CD-UVY          VALUE 'UVY'.
                   88  ADLM-REASON-CD-ZWZ          VALUE 'ZWZ'.
               10  ADLM-CYCLE-CD           PIC X(01).
                   88  ADLM-CYCLE-CD-A             VALUE 'A'.
                   88  ADLM-CYCLE-CD-H             VALUE 'H'.
                   88  ADLM-CYCLE-CD-J             VALUE 'J'.
                   88  ADLM-CYCLE-CD-T             VALUE 'T'.
                   88  ADLM-CYCLE-CD-W             VALUE 'W'.
               10  ADLM-REVIEW-FLAG        PIC X(01).
                   88  ADLM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  ADLM-NOTICE-SENT-FLAG   PIC X(01).
                   88  ADLM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  ADLM-EXEMPT-FLAG        PIC X(01).
                   88  ADLM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  ADLM-STOP-FLAG          PIC X(01).
                   88  ADLM-STOP-FLAG-YES          VALUE 'Y'.
               10  ADLM-PREFERRED-FLAG     PIC X(01).
                   88  ADLM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  ADLM-OVERRIDE-FLAG      PIC X(01).
                   88  ADLM-OVERRIDE-FLAG-YES      VALUE 'Y'.
           05  ADL-MASTER-COUNTERS.
               10  ADLM-RETRY-CNT          PIC 9(07).
               10  ADLM-CYCLE-CNT          PIC 9(07).
               10  ADLM-PAYMENT-CNT        PIC 9(07).
               10  ADLM-EXCEPT-CNT         PIC 9(07).
               10  ADLM-RATE               PIC 9(03)V9(06).
               10  ADLM-MARGIN             PIC 9(03)V9(06).
               10  ADLM-FEE-PCT            PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
