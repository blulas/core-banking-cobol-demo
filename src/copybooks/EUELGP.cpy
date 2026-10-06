      *================================================================*
      * COPYBOOK : EUELGP                                              *
      * PURPOSE  : LINKAGE FOR EURO CONVERSION RULE MODULE EUELG       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-08-03 GAF  ORIGINAL                                       *
      * 1997-09-15 GAF  TRIANGULAR LIMIT REVISED PRB8747               *
      * 1999-05-22 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-02-12 CRP  FEE CALCULATION CORRECTED INC5560              *
      * 2001-12-22 GAF  NEW CONVERTED REPORT COLUMNS PRB4358           *
      * 2006-02-05 JLM  FEE EXCEPTIONS TO OPS QUEUE INC8006            *
      * 2008-12-02 CRP  EURO RERUN PROTECTION REQ6499                  *
      * 2008-10-03 CRP  EURO FEE SCHEDULE UPDATE PRB9271               *
      * 2009-05-26 CRP  EURO TABLE EXPANDED INC8155                    *
      *================================================================*
       01  EUELG-PARMS.
           05  EUELG-ROUNDING-AMT          PIC S9(13)V99 COMP-3.
           05  EUELG-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  EUELG-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  EUELG-CONVERTED-AMT         PIC S9(13)V99 COMP-3.
           05  EUELG-TRIANGULAR-AMT        PIC S9(13)V99 COMP-3.
           05  EUELG-NATIONAL-YTD          PIC S9(13)V99 COMP-3.
           05  EUELG-OPEN-DATE             PIC 9(08).
           05  EUELG-LAST-PAY-DATE         PIC 9(08).
           05  EUELG-NOTICE-DATE           PIC 9(08).
           05  EUELG-NEXT-REVIEW-DATE      PIC 9(08).
           05  EUELG-CHANNEL-CD            PIC X(03).
               88  EUELG-CHANNEL-CD-AEA            VALUE 'AEA'.
               88  EUELG-CHANNEL-CD-EBM            VALUE 'EBM'.
               88  EUELG-CHANNEL-CD-GAU            VALUE 'GAU'.
               88  EUELG-CHANNEL-CD-GSU            VALUE 'GSU'.
               88  EUELG-CHANNEL-CD-VWP            VALUE 'VWP'.
               88  EUELG-CHANNEL-CD-XDF            VALUE 'XDF'.
           05  EUELG-STATUS-CD             PIC X(01).
           05  EUELG-CURRENCY-CD           PIC X(02).
               88  EUELG-CURRENCY-CD-EW            VALUE 'EW'.
               88  EUELG-CURRENCY-CD-FJ            VALUE 'FJ'.
               88  EUELG-CURRENCY-CD-JC            VALUE 'JC'.
           05  EUELG-TYPE-CD               PIC X(03).
               88  EUELG-TYPE-CD-AGL               VALUE 'AGL'.
               88  EUELG-TYPE-CD-SMF               VALUE 'SMF'.
               88  EUELG-TYPE-CD-UEC               VALUE 'UEC'.
               88  EUELG-TYPE-CD-VJA               VALUE 'VJA'.
               88  EUELG-TYPE-CD-WPH               VALUE 'WPH'.
           05  EUELG-PREFERRED-FLAG        PIC X(01).
               88  EUELG-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  EUELG-REVIEW-FLAG           PIC X(01).
               88  EUELG-REVIEW-FLAG-YES           VALUE 'Y'.
           05  EUELG-EXEMPT-FLAG           PIC X(01).
               88  EUELG-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  EUELG-CYCLE-CNT             PIC 9(07).
           05  EUELG-ITEM-CNT              PIC 9(07).
           05  EUELG-RATE                  PIC 9(03)V9(06).
           05  EUELG-FEE-PCT               PIC 9(03)V9(06).
           05  EUELG-CUST-ID               PIC X(10).
           05  EUELG-ADDR                  PIC X(30).
           05  EUELG-RETURN-CODE           PIC 9(02).
