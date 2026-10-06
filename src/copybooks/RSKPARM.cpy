      *================================================================*
      * COPYBOOK : RSKPARM                                             *
      * PURPOSE  : LINKAGE FOR RISKSCOR - CUSTOMER RISK SCORE 0-999    *
      *            AND RISK RATING L / M / H / P                       *
      *   CASH ACTIVITY INDICATOR (SUPPLIED BY CALLER):                *
      *     H = CASH IN+OUT >= 10,000.00 IN LOOKBACK WINDOW            *
      *     M = CASH IN+OUT >=  3,000.00 IN LOOKBACK WINDOW            *
      *     N = BELOW / NO CASH ACTIVITY                               *
      *   RC : 00 OK, 08 PROHIBITED, 12 INVALID INPUT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2002-03-15 BHS  ORIGINAL - USA PATRIOT ACT                     *
      * 2006-08-30 BHS  PEP AND OFAC FACTORS                           *
      * 2011-02-01 DPS  HIFCA GEOGRAPHY FACTOR (CHG0388)               *
      * 2019-10-07 AKW  MSB FACTOR, FACTOR BREAKDOWN OUTPUT            *
      *================================================================*
       01  RISKSCOR-PARMS.
           05  RSK-INPUT.
               10  RSK-CUST-TYPE           PIC X(01).
               10  RSK-SEGMENT             PIC X(02).
               10  RSK-PEP-FLAG            PIC X(01).
               10  RSK-MSB-FLAG            PIC X(01).
               10  RSK-INDUSTRY-CODE       PIC X(06).
               10  RSK-COUNTRY             PIC X(02).
               10  RSK-ZIP                 PIC X(10).
               10  RSK-TAX-ID-TYPE         PIC X(01).
               10  RSK-OFAC-STATUS         PIC X(01).
               10  RSK-ANNUAL-INCOME       PIC S9(13)V99 COMP-3.
               10  RSK-CUST-SINCE-DATE     PIC 9(08).
               10  RSK-AS-OF-DATE          PIC 9(08).
               10  RSK-PRODUCT-MIX.
                   15  RSK-CNT-DDA         PIC 9(03).
                   15  RSK-CNT-SAV         PIC 9(03).
                   15  RSK-CNT-MMA         PIC 9(03).
                   15  RSK-CNT-CD          PIC 9(03).
                   15  RSK-CNT-CORP-DDA    PIC 9(03).
               10  RSK-CASH-ACTIVITY       PIC X(01).
                   88  RSK-CASH-HEAVY                VALUE 'H'.
                   88  RSK-CASH-MODERATE             VALUE 'M'.
                   88  RSK-CASH-NONE                 VALUE 'N' ' '.
           05  RSK-OUTPUT.
               10  RSK-RESULT-SCORE        PIC 9(03).
               10  RSK-RESULT-RATING       PIC X(01).
                   88  RSK-RATED-LOW                 VALUE 'L'.
                   88  RSK-RATED-MEDIUM              VALUE 'M'.
                   88  RSK-RATED-HIGH                VALUE 'H'.
                   88  RSK-RATED-PROHIBITED          VALUE 'P'.
               10  RSK-PROHIBITED-FLAG     PIC X(01).
               10  RSK-FACTOR-SCORES.
                   15  RSK-F-ENTITY        PIC S9(03).
                   15  RSK-F-PEP           PIC S9(03).
                   15  RSK-F-MSB           PIC S9(03).
                   15  RSK-F-INDUSTRY      PIC S9(03).
                   15  RSK-F-GEOGRAPHY     PIC S9(03).
                   15  RSK-F-PRODUCT       PIC S9(03).
                   15  RSK-F-TENURE        PIC S9(03).
                   15  RSK-F-CASH          PIC S9(03).
                   15  RSK-F-OTHER         PIC S9(03).
           05  RSK-RETURN-CODE             PIC 9(02).
