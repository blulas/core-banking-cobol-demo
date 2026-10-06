      *================================================================*
      * COPYBOOK : EUPRCP                                              *
      * PURPOSE  : LINKAGE FOR EURO CONVERSION RULE MODULE EUPRC       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-06-22 RJT  ORIGINAL                                       *
      * 1999-11-06 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-12-24 DPS  TRIANGULAR ROUNDING - HALF UP PRB3605          *
      * 2004-03-17 RJT  CONVERTED LIMIT REVISED CHG7343                *
      * 2005-08-03 TWH  CONVERTED STATUS CODES ADDED CHG0397           *
      * 2008-08-27 MKL  RESIDUAL FEE SCHEDULE UPDATE INC8109           *
      *================================================================*
       01  EUPRC-PARMS.
           05  EUPRC-EURO-AMT              PIC S9(13)V99 COMP-3.
           05  EUPRC-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  EUPRC-ROUNDING-YTD          PIC S9(13)V99 COMP-3.
           05  EUPRC-EURO-YTD              PIC S9(13)V99 COMP-3.
           05  EUPRC-CONVERTED-YTD         PIC S9(13)V99 COMP-3.
           05  EUPRC-CONVERTED-AMT         PIC S9(13)V99 COMP-3.
           05  EUPRC-LAST-MAINT-DATE       PIC 9(08).
           05  EUPRC-NEXT-REVIEW-DATE      PIC 9(08).
           05  EUPRC-LAST-PAY-DATE         PIC 9(08).
           05  EUPRC-LAST-STMT-DATE        PIC 9(08).
           05  EUPRC-CURRENCY-CD           PIC X(02).
               88  EUPRC-CURRENCY-CD-EW            VALUE 'EW'.
               88  EUPRC-CURRENCY-CD-FJ            VALUE 'FJ'.
               88  EUPRC-CURRENCY-CD-JC            VALUE 'JC'.
           05  EUPRC-CONV-METHOD-CD        PIC X(02).
               88  EUPRC-CONV-METHOD-CD-GL         VALUE 'GL'.
               88  EUPRC-CONV-METHOD-CD-JA         VALUE 'JA'.
               88  EUPRC-CONV-METHOD-CD-LD         VALUE 'LD'.
               88  EUPRC-CONV-METHOD-CD-MR         VALUE 'MR'.
               88  EUPRC-CONV-METHOD-CD-VC         VALUE 'VC'.
           05  EUPRC-TYPE-CD               PIC X(03).
               88  EUPRC-TYPE-CD-AGL               VALUE 'AGL'.
               88  EUPRC-TYPE-CD-SMF               VALUE 'SMF'.
               88  EUPRC-TYPE-CD-UEC               VALUE 'UEC'.
               88  EUPRC-TYPE-CD-VJA               VALUE 'VJA'.
               88  EUPRC-TYPE-CD-WPH               VALUE 'WPH'.
           05  EUPRC-CHANNEL-CD            PIC X(03).
               88  EUPRC-CHANNEL-CD-AEA            VALUE 'AEA'.
               88  EUPRC-CHANNEL-CD-EBM            VALUE 'EBM'.
               88  EUPRC-CHANNEL-CD-GAU            VALUE 'GAU'.
               88  EUPRC-CHANNEL-CD-GSU            VALUE 'GSU'.
               88  EUPRC-CHANNEL-CD-VWP            VALUE 'VWP'.
               88  EUPRC-CHANNEL-CD-XDF            VALUE 'XDF'.
           05  EUPRC-REVIEW-FLAG           PIC X(01).
               88  EUPRC-REVIEW-FLAG-YES           VALUE 'Y'.
           05  EUPRC-AUTO-RENEW-FLAG       PIC X(01).
               88  EUPRC-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  EUPRC-OVERRIDE-FLAG         PIC X(01).
               88  EUPRC-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  EUPRC-CYCLE-CNT             PIC 9(07).
           05  EUPRC-EXCEPT-CNT            PIC 9(07).
           05  EUPRC-RATE                  PIC 9(03)V9(06).
           05  EUPRC-PENALTY-PCT           PIC 9(03)V9(06).
           05  EUPRC-DESC                  PIC X(30).
           05  EUPRC-CONTACT               PIC X(20).
           05  EUPRC-RETURN-CODE           PIC 9(02).
