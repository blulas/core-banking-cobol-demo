      *================================================================*
      * COPYBOOK : ESFEEP                                              *
      * PURPOSE  : LINKAGE FOR MORTGAGE ESCROW RULE MODULE ESFEE       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-01-04 MKL  ORIGINAL                                       *
      * 2000-05-21 CRP  SURPLUS STATUS CODES ADDED INC3885             *
      * 2001-11-17 EWS  TAX TABLE EXPANDED INC5627                     *
      * 2002-08-14 LMB  CUSHION EXCEPTIONS TO OPS QUEUE CHG1910        *
      * 2003-01-19 TWH  DISBURSE TABLE EXPANDED CHG5313                *
      * 2005-07-24 MKL  DISBURSE CUTOFF MOVED CHG1139                  *
      * 2006-12-18 GAF  INSURANCE CALCULATION CORRECTED REQ7419        *
      * 2007-05-23 BHS  SURPLUS FEE SCHEDULE UPDATE REQ0149            *
      * 2007-11-25 DPS  INSURANCE TABLE EXPANDED INC8674               *
      * 2010-05-23 CRP  PERFORMANCE - DISBURSE READ AHEAD REQ3199      *
      * 2012-03-15 LMB  TAX CUTOFF MOVED CHG7563                       *
      * 2012-12-03 TWH  INSURANCE RERUN PROTECTION INC9799             *
      * 2013-12-28 PQR  TAX CALCULATION CORRECTED REQ4377              *
      *================================================================*
       01  ESFEE-PARMS.
           05  ESFEE-INSURANCE-AMT         PIC S9(13)V99 COMP-3.
           05  ESFEE-TAX-AMT               PIC S9(13)V99 COMP-3.
           05  ESFEE-DISBURSE-AMT          PIC S9(13)V99 COMP-3.
           05  ESFEE-SURPLUS-YTD           PIC S9(13)V99 COMP-3.
           05  ESFEE-CUSHION-YTD           PIC S9(13)V99 COMP-3.
           05  ESFEE-TAX-YTD               PIC S9(13)V99 COMP-3.
           05  ESFEE-LAST-STMT-DATE        PIC 9(08).
           05  ESFEE-CLOSE-DATE            PIC 9(08).
           05  ESFEE-EFFECTIVE-DATE        PIC 9(08).
           05  ESFEE-NEXT-REVIEW-DATE      PIC 9(08).
           05  ESFEE-REGION-CD             PIC X(01).
               88  ESFEE-REGION-CD-B               VALUE 'B'.
               88  ESFEE-REGION-CD-G               VALUE 'G'.
               88  ESFEE-REGION-CD-Y               VALUE 'Y'.
           05  ESFEE-ANALYSIS-CD           PIC X(02).
               88  ESFEE-ANALYSIS-CD-GD            VALUE 'GD'.
               88  ESFEE-ANALYSIS-CD-LY            VALUE 'LY'.
               88  ESFEE-ANALYSIS-CD-XJ            VALUE 'XJ'.
           05  ESFEE-REASON-CD             PIC X(01).
               88  ESFEE-REASON-CD-C               VALUE 'C'.
               88  ESFEE-REASON-CD-F               VALUE 'F'.
               88  ESFEE-REASON-CD-K               VALUE 'K'.
               88  ESFEE-REASON-CD-L               VALUE 'L'.
               88  ESFEE-REASON-CD-S               VALUE 'S'.
               88  ESFEE-REASON-CD-X               VALUE 'X'.
           05  ESFEE-TYPE-CD               PIC X(03).
           05  ESFEE-PREFERRED-FLAG        PIC X(01).
               88  ESFEE-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  ESFEE-OVERRIDE-FLAG         PIC X(01).
               88  ESFEE-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  ESFEE-SUSPEND-FLAG          PIC X(01).
               88  ESFEE-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  ESFEE-NOTICE-CNT            PIC 9(07).
           05  ESFEE-EXCEPT-CNT            PIC 9(07).
           05  ESFEE-FEE-PCT               PIC 9(03)V9(06).
           05  ESFEE-DISCOUNT              PIC 9(03)V9(06).
           05  ESFEE-ADDR                  PIC X(30).
           05  ESFEE-CONTACT               PIC X(20).
           05  ESFEE-RETURN-CODE           PIC 9(02).
