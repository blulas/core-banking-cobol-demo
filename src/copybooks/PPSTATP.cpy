      *================================================================*
      * COPYBOOK : PPSTATP                                             *
      * PURPOSE  : LINKAGE FOR POSITIVE PAY RULE MODULE PPSTAT         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-13 DPS  ORIGINAL                                       *
      * 1992-09-15 EWS  VOID STATUS CODES ADDED CHG2783                *
      * 1993-08-03 JLM  ISSUED CALCULATION CORRECTED CHG9960           *
      * 1993-12-27 PQR  ISSUED TABLE EXPANDED CHG3900                  *
      * 1994-12-16 GAF  ISSUED STATUS CODES ADDED PRB0231              *
      * 1995-06-13 EWS  VOID ROUNDING - HALF UP PRB2360                *
      * 1999-10-11 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-11-26 JLM  VOID CALCULATION CORRECTED REQ5779             *
      * 2002-10-19 EWS  VOID NOTICE TEXT PER LEGAL REQ8414             *
      * 2002-08-16 MKL  PAID RERUN PROTECTION REQ8145                  *
      * 2002-03-20 MKL  ISSUED ROUNDING - HALF UP INC9252              *
      * 2003-08-27 MKL  EXCEPT CUTOFF MOVED CHG5701                    *
      * 2003-02-05 PQR  VOID CUTOFF MOVED PRB4108                      *
      *================================================================*
       01  PPSTAT-PARMS.
           05  PPSTAT-EXCEPT-YTD           PIC S9(13)V99 COMP-3.
           05  PPSTAT-VOID-AMT             PIC S9(13)V99 COMP-3.
           05  PPSTAT-PAID-YTD             PIC S9(13)V99 COMP-3.
           05  PPSTAT-EXCEPT-AMT           PIC S9(13)V99 COMP-3.
           05  PPSTAT-PAID-AMT             PIC S9(13)V99 COMP-3.
           05  PPSTAT-STALE-YTD            PIC S9(13)V99 COMP-3.
           05  PPSTAT-CLOSE-DATE           PIC 9(08).
           05  PPSTAT-EFFECTIVE-DATE       PIC 9(08).
           05  PPSTAT-LAST-PAY-DATE        PIC 9(08).
           05  PPSTAT-EXPIRY-DATE          PIC 9(08).
           05  PPSTAT-REGION-CD            PIC X(01).
               88  PPSTAT-REGION-CD-A              VALUE 'A'.
               88  PPSTAT-REGION-CD-B              VALUE 'B'.
               88  PPSTAT-REGION-CD-D              VALUE 'D'.
               88  PPSTAT-REGION-CD-H              VALUE 'H'.
               88  PPSTAT-REGION-CD-N              VALUE 'N'.
               88  PPSTAT-REGION-CD-Y              VALUE 'Y'.
           05  PPSTAT-REASON-CD            PIC X(01).
               88  PPSTAT-REASON-CD-B              VALUE 'B'.
               88  PPSTAT-REASON-CD-M              VALUE 'M'.
               88  PPSTAT-REASON-CD-N              VALUE 'N'.
               88  PPSTAT-REASON-CD-U              VALUE 'U'.
               88  PPSTAT-REASON-CD-V              VALUE 'V'.
               88  PPSTAT-REASON-CD-Z              VALUE 'Z'.
           05  PPSTAT-MATCH-RULE-CD        PIC X(01).
               88  PPSTAT-MATCH-RULE-CD-A          VALUE 'A'.
               88  PPSTAT-MATCH-RULE-CD-B          VALUE 'B'.
               88  PPSTAT-MATCH-RULE-CD-F          VALUE 'F'.
               88  PPSTAT-MATCH-RULE-CD-M          VALUE 'M'.
               88  PPSTAT-MATCH-RULE-CD-S          VALUE 'S'.
               88  PPSTAT-MATCH-RULE-CD-V          VALUE 'V'.
           05  PPSTAT-CYCLE-CD             PIC X(01).
               88  PPSTAT-CYCLE-CD-A               VALUE 'A'.
               88  PPSTAT-CYCLE-CD-B               VALUE 'B'.
               88  PPSTAT-CYCLE-CD-C               VALUE 'C'.
               88  PPSTAT-CYCLE-CD-R               VALUE 'R'.
               88  PPSTAT-CYCLE-CD-T               VALUE 'T'.
               88  PPSTAT-CYCLE-CD-Y               VALUE 'Y'.
           05  PPSTAT-OVERRIDE-FLAG        PIC X(01).
               88  PPSTAT-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  PPSTAT-EXEMPT-FLAG          PIC X(01).
               88  PPSTAT-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  PPSTAT-HOLD-FLAG            PIC X(01).
               88  PPSTAT-HOLD-FLAG-YES            VALUE 'Y'.
           05  PPSTAT-NOTICE-CNT           PIC 9(07).
           05  PPSTAT-PAYMENT-CNT          PIC 9(07).
           05  PPSTAT-DISCOUNT             PIC 9(03)V9(06).
           05  PPSTAT-MARGIN               PIC 9(03)V9(06).
           05  PPSTAT-CONTACT              PIC X(20).
           05  PPSTAT-REF-NO               PIC X(16).
           05  PPSTAT-RETURN-CODE          PIC 9(02).
