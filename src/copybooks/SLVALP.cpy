      *================================================================*
      * COPYBOOK : SLVALP                                              *
      * PURPOSE  : LINKAGE FOR STUDENT LOANS RULE MODULE SLVAL         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-02-12 JLM  ORIGINAL                                       *
      * 1991-03-25 BHS  PAYMENT RERUN PROTECTION CHG3052               *
      * 1995-08-08 RJT  AUDIT FINDING - PRINCIPAL CONTROL TOTALS PRB972*
      * 1995-07-11 JLM  SUBSIDY TABLE EXPANDED REQ1255                 *
      * 1996-08-21 JLM  DISBURSE EXCEPTIONS TO OPS QUEUE CHG1505       *
      * 1997-02-27 RJT  PAYMENT FEE SCHEDULE UPDATE PRB3032            *
      * 1997-07-01 MKL  PAYMENT RERUN PROTECTION INC8128               *
      * 1998-10-27 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-02-23 GAF  SUBSIDY LIMIT REVISED REQ1233                  *
      * 2001-10-13 DPS  DEFERINT EXCEPTIONS TO OPS QUEUE PRB8445       *
      * 2004-09-07 EWS  PERFORMANCE - PRINCIPAL READ AHEAD PRB3306     *
      * 2005-05-25 GAF  PAYMENT NOTICE TEXT PER LEGAL PRB6181          *
      * 2006-10-18 RJT  DISBURSE FEE SCHEDULE UPDATE PRB8196           *
      *================================================================*
       01  SLVAL-PARMS.
           05  SLVAL-DEFERINT-YTD          PIC S9(13)V99 COMP-3.
           05  SLVAL-SUBSIDY-YTD           PIC S9(13)V99 COMP-3.
           05  SLVAL-PAYMENT-YTD           PIC S9(13)V99 COMP-3.
           05  SLVAL-CAPINT-AMT            PIC S9(13)V99 COMP-3.
           05  SLVAL-DISBURSE-YTD          PIC S9(13)V99 COMP-3.
           05  SLVAL-CAPINT-YTD            PIC S9(13)V99 COMP-3.
           05  SLVAL-EXPIRY-DATE           PIC 9(08).
           05  SLVAL-LAST-MAINT-DATE       PIC 9(08).
           05  SLVAL-LAST-PAY-DATE         PIC 9(08).
           05  SLVAL-LAST-ACT-DATE         PIC 9(08).
           05  SLVAL-SOURCE-CD             PIC X(03).
               88  SLVAL-SOURCE-CD-AST             VALUE 'AST'.
               88  SLVAL-SOURCE-CD-BCK             VALUE 'BCK'.
               88  SLVAL-SOURCE-CD-CJG             VALUE 'CJG'.
               88  SLVAL-SOURCE-CD-DSJ             VALUE 'DSJ'.
               88  SLVAL-SOURCE-CD-JVY             VALUE 'JVY'.
               88  SLVAL-SOURCE-CD-VEF             VALUE 'VEF'.
           05  SLVAL-CYCLE-CD              PIC X(03).
               88  SLVAL-CYCLE-CD-CFJ              VALUE 'CFJ'.
               88  SLVAL-CYCLE-CD-KHC              VALUE 'KHC'.
               88  SLVAL-CYCLE-CD-SEU              VALUE 'SEU'.
               88  SLVAL-CYCLE-CD-XKA              VALUE 'XKA'.
           05  SLVAL-LOAN-TYPE-CD          PIC X(02).
               88  SLVAL-LOAN-TYPE-CD-CS           VALUE 'CS'.
               88  SLVAL-LOAN-TYPE-CD-DV           VALUE 'DV'.
               88  SLVAL-LOAN-TYPE-CD-JL           VALUE 'JL'.
               88  SLVAL-LOAN-TYPE-CD-NE           VALUE 'NE'.
               88  SLVAL-LOAN-TYPE-CD-SH           VALUE 'SH'.
           05  SLVAL-DEFER-CODE-CD         PIC X(02).
               88  SLVAL-DEFER-CODE-CD-AH          VALUE 'AH'.
               88  SLVAL-DEFER-CODE-CD-CZ          VALUE 'CZ'.
               88  SLVAL-DEFER-CODE-CD-GM          VALUE 'GM'.
               88  SLVAL-DEFER-CODE-CD-ND          VALUE 'ND'.
               88  SLVAL-DEFER-CODE-CD-UC          VALUE 'UC'.
           05  SLVAL-PREFERRED-FLAG        PIC X(01).
               88  SLVAL-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  SLVAL-SUSPEND-FLAG          PIC X(01).
               88  SLVAL-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  SLVAL-REVIEW-FLAG           PIC X(01).
               88  SLVAL-REVIEW-FLAG-YES           VALUE 'Y'.
           05  SLVAL-PAYMENT-CNT           PIC 9(07).
           05  SLVAL-ITEM-CNT              PIC 9(07).
           05  SLVAL-FEE-PCT               PIC 9(03)V9(06).
           05  SLVAL-DISCOUNT              PIC 9(03)V9(06).
           05  SLVAL-DESC                  PIC X(30).
           05  SLVAL-CUST-ID               PIC X(10).
           05  SLVAL-RETURN-CODE           PIC 9(02).
