      *================================================================*
      * COPYBOOK : AURISKP                                             *
      * PURPOSE  : LINKAGE FOR DEALER AUTO FINANCE RULE MODULE AURISK  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-11-15 PQR  ORIGINAL                                       *
      * 1997-08-14 RJT  AMOUNT-FIN CUTOFF MOVED REQ2522                *
      * 1998-12-07 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-10-03 MKL  RESERVE TABLE EXPANDED REQ3975                 *
      * 1999-06-13 PQR  CHARGEBK CUTOFF MOVED PRB5883                  *
      * 2000-09-04 TWH  RESERVE CALCULATION CORRECTED INC9221          *
      * 2005-06-12 EWS  REBATE TABLE EXPANDED INC0102                  *
      * 2007-12-06 SKN  GAP FEE SCHEDULE UPDATE INC4057                *
      *================================================================*
       01  AURISK-PARMS.
           05  AURISK-GAP-AMT              PIC S9(13)V99 COMP-3.
           05  AURISK-AMOUNT-FIN-AMT       PIC S9(13)V99 COMP-3.
           05  AURISK-CHARGEBK-AMT         PIC S9(13)V99 COMP-3.
           05  AURISK-REBATE-AMT           PIC S9(13)V99 COMP-3.
           05  AURISK-RESERVE-AMT          PIC S9(13)V99 COMP-3.
           05  AURISK-PAYMENT-AMT          PIC S9(13)V99 COMP-3.
           05  AURISK-NEXT-REVIEW-DATE     PIC 9(08).
           05  AURISK-EFFECTIVE-DATE       PIC 9(08).
           05  AURISK-LAST-STMT-DATE       PIC 9(08).
           05  AURISK-LAST-ACT-DATE        PIC 9(08).
           05  AURISK-STATUS-CD            PIC X(03).
               88  AURISK-STATUS-CD-EUA            VALUE 'EUA'.
               88  AURISK-STATUS-CD-HMW            VALUE 'HMW'.
               88  AURISK-STATUS-CD-RCB            VALUE 'RCB'.
           05  AURISK-TIER-CD              PIC X(02).
               88  AURISK-TIER-CD-EW               VALUE 'EW'.
               88  AURISK-TIER-CD-JF               VALUE 'JF'.
               88  AURISK-TIER-CD-KD               VALUE 'KD'.
               88  AURISK-TIER-CD-UG               VALUE 'UG'.
               88  AURISK-TIER-CD-WY               VALUE 'WY'.
           05  AURISK-CYCLE-CD             PIC X(01).
               88  AURISK-CYCLE-CD-A               VALUE 'A'.
               88  AURISK-CYCLE-CD-H               VALUE 'H'.
               88  AURISK-CYCLE-CD-J               VALUE 'J'.
               88  AURISK-CYCLE-CD-T               VALUE 'T'.
               88  AURISK-CYCLE-CD-W               VALUE 'W'.
           05  AURISK-DEALER-CD            PIC X(03).
               88  AURISK-DEALER-CD-DRU            VALUE 'DRU'.
               88  AURISK-DEALER-CD-FNF            VALUE 'FNF'.
               88  AURISK-DEALER-CD-GBX            VALUE 'GBX'.
               88  AURISK-DEALER-CD-GTU            VALUE 'GTU'.
               88  AURISK-DEALER-CD-JNE            VALUE 'JNE'.
               88  AURISK-DEALER-CD-KTP            VALUE 'KTP'.
           05  AURISK-REVIEW-FLAG          PIC X(01).
               88  AURISK-REVIEW-FLAG-YES          VALUE 'Y'.
           05  AURISK-EXEMPT-FLAG          PIC X(01).
               88  AURISK-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  AURISK-PREFERRED-FLAG       PIC X(01).
               88  AURISK-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  AURISK-PAYMENT-CNT          PIC 9(07).
           05  AURISK-RETRY-CNT            PIC 9(07).
           05  AURISK-RATE                 PIC 9(03)V9(06).
           05  AURISK-MARGIN               PIC 9(03)V9(06).
           05  AURISK-REF-NO               PIC X(16).
           05  AURISK-CONTACT              PIC X(20).
           05  AURISK-RETURN-CODE          PIC 9(02).
