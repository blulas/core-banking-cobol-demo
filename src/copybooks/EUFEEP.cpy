      *================================================================*
      * COPYBOOK : EUFEEP                                              *
      * PURPOSE  : LINKAGE FOR EURO CONVERSION RULE MODULE EUFEE       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-07-11 DPS  ORIGINAL                                       *
      * 2001-09-05 RJT  AUDIT FINDING - EURO CONTROL TOTALS INC4211    *
      * 2001-09-03 PQR  FEE CUTOFF MOVED INC7115                       *
      * 2002-12-09 BHS  FEE STATUS CODES ADDED PRB2965                 *
      * 2004-05-22 TWH  NEW TRIANGULAR REPORT COLUMNS INC0793          *
      * 2005-04-06 SKN  EURO TABLE EXPANDED CHG0491                    *
      * 2006-01-25 MKL  ROUNDING CALCULATION CORRECTED REQ9676         *
      *================================================================*
       01  EUFEE-PARMS.
           05  EUFEE-CONVERTED-YTD         PIC S9(13)V99 COMP-3.
           05  EUFEE-NATIONAL-YTD          PIC S9(13)V99 COMP-3.
           05  EUFEE-NATIONAL-AMT          PIC S9(13)V99 COMP-3.
           05  EUFEE-TRIANGULAR-YTD        PIC S9(13)V99 COMP-3.
           05  EUFEE-EURO-YTD              PIC S9(13)V99 COMP-3.
           05  EUFEE-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  EUFEE-LAST-MAINT-DATE       PIC 9(08).
           05  EUFEE-NOTICE-DATE           PIC 9(08).
           05  EUFEE-EFFECTIVE-DATE        PIC 9(08).
           05  EUFEE-LAST-PAY-DATE         PIC 9(08).
           05  EUFEE-CONV-METHOD-CD        PIC X(02).
               88  EUFEE-CONV-METHOD-CD-GL         VALUE 'GL'.
               88  EUFEE-CONV-METHOD-CD-JA         VALUE 'JA'.
               88  EUFEE-CONV-METHOD-CD-LD         VALUE 'LD'.
               88  EUFEE-CONV-METHOD-CD-MR         VALUE 'MR'.
               88  EUFEE-CONV-METHOD-CD-VC         VALUE 'VC'.
           05  EUFEE-REGION-CD             PIC X(03).
               88  EUFEE-REGION-CD-DTL             VALUE 'DTL'.
               88  EUFEE-REGION-CD-HYD             VALUE 'HYD'.
               88  EUFEE-REGION-CD-JUL             VALUE 'JUL'.
               88  EUFEE-REGION-CD-WSE             VALUE 'WSE'.
               88  EUFEE-REGION-CD-YAU             VALUE 'YAU'.
               88  EUFEE-REGION-CD-YJB             VALUE 'YJB'.
           05  EUFEE-TYPE-CD               PIC X(03).
               88  EUFEE-TYPE-CD-AGL               VALUE 'AGL'.
               88  EUFEE-TYPE-CD-SMF               VALUE 'SMF'.
               88  EUFEE-TYPE-CD-UEC               VALUE 'UEC'.
               88  EUFEE-TYPE-CD-VJA               VALUE 'VJA'.
               88  EUFEE-TYPE-CD-WPH               VALUE 'WPH'.
           05  EUFEE-CHANNEL-CD            PIC X(03).
               88  EUFEE-CHANNEL-CD-AEA            VALUE 'AEA'.
               88  EUFEE-CHANNEL-CD-EBM            VALUE 'EBM'.
               88  EUFEE-CHANNEL-CD-GAU            VALUE 'GAU'.
               88  EUFEE-CHANNEL-CD-GSU            VALUE 'GSU'.
               88  EUFEE-CHANNEL-CD-VWP            VALUE 'VWP'.
               88  EUFEE-CHANNEL-CD-XDF            VALUE 'XDF'.
           05  EUFEE-PREFERRED-FLAG        PIC X(01).
               88  EUFEE-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  EUFEE-OVERRIDE-FLAG         PIC X(01).
               88  EUFEE-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  EUFEE-REVIEW-FLAG           PIC X(01).
               88  EUFEE-REVIEW-FLAG-YES           VALUE 'Y'.
           05  EUFEE-CYCLE-CNT             PIC 9(07).
           05  EUFEE-PAYMENT-CNT           PIC 9(07).
           05  EUFEE-FEE-PCT               PIC 9(03)V9(06).
           05  EUFEE-RATE                  PIC 9(03)V9(06).
           05  EUFEE-REF-NO                PIC X(16).
           05  EUFEE-CUST-ID               PIC X(10).
           05  EUFEE-RETURN-CODE           PIC 9(02).
