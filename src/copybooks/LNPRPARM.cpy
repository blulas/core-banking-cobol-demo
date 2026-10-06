      *================================================================*
      * COPYBOOK : LNPRPARM                                            *
      * PURPOSE  : LINKAGE FOR LNPRICE - LOAN RATE PRICING             *
      *   CONSUMER (LPER)  : FIXED RATE FROM CREDIT SCORE GRID + TERM  *
      *                      AND RELATIONSHIP ADJUSTMENTS              *
      *   COMMERCIAL       : INDEX (PRIME / SOFR) + MARGIN BY RISK     *
      *                      GRADE, FLOOR AND CAP                      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-05-15 RJT  ORIGINAL - COMMERCIAL GRID                     *
      * 2016-04-25 AKW  LPER CONSUMER GRID                             *
      * 2022-01-10 AKW  SOFR INDEX                                     *
      *================================================================*
       01  LNPRICE-PARMS.
      *--- INPUT -----------------------------------------------------*
           05  LNP-PRODUCT-CODE            PIC X(04).
           05  LNP-CREDIT-SCORE            PIC 9(03).
           05  LNP-TERM-MONTHS             PIC 9(03).
           05  LNP-AUTOPAY-FLAG            PIC X(01).
           05  LNP-EMPLOYEE-FLAG           PIC X(01).
           05  LNP-RISK-GRADE              PIC 9(02).
           05  LNP-RATE-INDEX              PIC X(05).
      *    CALLER PASSES CTL-PRIME-RATE OR CTL-SOFR-RATE
           05  LNP-INDEX-RATE              PIC 9(03)V9(06).
      *    CONTRACTUAL FLOOR / CAP. ZERO = USE POLICY DEFAULT
           05  LNP-FLOOR-OVERRIDE          PIC 9(03)V9(06).
           05  LNP-CAP-OVERRIDE            PIC 9(03)V9(06).
      *    CUST-TOTAL-REL-BALANCE (DEPOSITS IN RELATIONSHIP)
           05  LNP-REL-DEPOSIT-BAL         PIC S9(13)V99 COMP-3.
      *--- OUTPUT ----------------------------------------------------*
           05  LNP-RATE-TYPE               PIC X(01).
           05  LNP-BASE-RATE               PIC 9(03)V9(06).
           05  LNP-MARGIN-RATE             PIC 9(03)V9(06).
           05  LNP-TOTAL-ADJ               PIC S9(03)V9(06).
           05  LNP-FLOOR-RATE              PIC 9(03)V9(06).
           05  LNP-CAP-RATE                PIC 9(03)V9(06).
           05  LNP-FINAL-RATE              PIC 9(03)V9(06).
           05  LNP-FLOOR-APPLIED           PIC X(01).
           05  LNP-CAP-APPLIED             PIC X(01).
           05  LNP-DECLINE-REASON          PIC X(03).
           05  LNP-RETURN-CODE             PIC 9(02).
               88  LNP-OK                            VALUE 00.
               88  LNP-DECLINED                      VALUE 08.
               88  LNP-INVALID                       VALUE 12.
