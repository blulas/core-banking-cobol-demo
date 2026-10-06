      *================================================================*
      * COPYBOOK : SDFEEP                                              *
      * PURPOSE  : LINKAGE FOR SAFE DEPOSIT BOXES RULE MODULE SDFEE    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-02-15 MKL  ORIGINAL                                       *
      * 1996-11-09 SKN  NEW DRILL REPORT COLUMNS CHG5961               *
      * 1996-06-03 LMB  NEW PREPAID REPORT COLUMNS PRB9259             *
      * 1998-05-27 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-03-19 MKL  PERFORMANCE - KEY-FEE READ AHEAD REQ4740       *
      * 2002-04-10 DPS  NEW DEPOSIT REPORT COLUMNS PRB4007             *
      * 2005-10-20 BHS  DEPOSIT EXCEPTIONS TO OPS QUEUE CHG2501        *
      * 2008-01-28 JLM  DRILL EXCEPTIONS TO OPS QUEUE INC4723          *
      * 2008-09-07 LMB  PERFORMANCE - DEPOSIT READ AHEAD PRB9001       *
      * 2008-01-17 GAF  KEY-FEE EXCEPTIONS TO OPS QUEUE REQ3753        *
      * 2009-07-05 DPS  RENT ROUNDING - HALF UP REQ2587                *
      *================================================================*
       01  SDFEE-PARMS.
           05  SDFEE-PREPAID-AMT           PIC S9(13)V99 COMP-3.
           05  SDFEE-PREPAID-YTD           PIC S9(13)V99 COMP-3.
           05  SDFEE-KEY-FEE-AMT           PIC S9(13)V99 COMP-3.
           05  SDFEE-DEPOSIT-YTD           PIC S9(13)V99 COMP-3.
           05  SDFEE-RENT-YTD              PIC S9(13)V99 COMP-3.
           05  SDFEE-DRILL-YTD             PIC S9(13)V99 COMP-3.
           05  SDFEE-LAST-MAINT-DATE       PIC 9(08).
           05  SDFEE-LAST-ACT-DATE         PIC 9(08).
           05  SDFEE-MATURITY-DATE         PIC 9(08).
           05  SDFEE-OPEN-DATE             PIC 9(08).
           05  SDFEE-SOURCE-CD             PIC X(02).
               88  SDFEE-SOURCE-CD-DD              VALUE 'DD'.
               88  SDFEE-SOURCE-CD-JZ              VALUE 'JZ'.
               88  SDFEE-SOURCE-CD-PS              VALUE 'PS'.
               88  SDFEE-SOURCE-CD-UM              VALUE 'UM'.
           05  SDFEE-BRANCH-CD             PIC X(03).
               88  SDFEE-BRANCH-CD-EXS             VALUE 'EXS'.
               88  SDFEE-BRANCH-CD-FJW             VALUE 'FJW'.
               88  SDFEE-BRANCH-CD-YFB             VALUE 'YFB'.
           05  SDFEE-BOX-SIZE-CD           PIC X(02).
               88  SDFEE-BOX-SIZE-CD-CU            VALUE 'CU'.
               88  SDFEE-BOX-SIZE-CD-MG            VALUE 'MG'.
               88  SDFEE-BOX-SIZE-CD-VG            VALUE 'VG'.
               88  SDFEE-BOX-SIZE-CD-VN            VALUE 'VN'.
               88  SDFEE-BOX-SIZE-CD-XJ            VALUE 'XJ'.
           05  SDFEE-VAULT-CD              PIC X(02).
               88  SDFEE-VAULT-CD-BU               VALUE 'BU'.
               88  SDFEE-VAULT-CD-ET               VALUE 'ET'.
               88  SDFEE-VAULT-CD-FF               VALUE 'FF'.
               88  SDFEE-VAULT-CD-NG               VALUE 'NG'.
               88  SDFEE-VAULT-CD-NH               VALUE 'NH'.
           05  SDFEE-SUSPEND-FLAG          PIC X(01).
               88  SDFEE-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  SDFEE-AUTO-RENEW-FLAG       PIC X(01).
               88  SDFEE-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  SDFEE-EXEMPT-FLAG           PIC X(01).
               88  SDFEE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  SDFEE-RETRY-CNT             PIC 9(07).
           05  SDFEE-ITEM-CNT              PIC 9(07).
           05  SDFEE-DISCOUNT              PIC 9(03)V9(06).
           05  SDFEE-PENALTY-PCT           PIC 9(03)V9(06).
           05  SDFEE-CONTACT               PIC X(20).
           05  SDFEE-CUST-ID               PIC X(10).
           05  SDFEE-RETURN-CODE           PIC 9(02).
