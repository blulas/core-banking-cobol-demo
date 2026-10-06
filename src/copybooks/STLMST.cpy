      *================================================================*
      * COPYBOOK : STLMST                                              *
      * PURPOSE  : STUDENT LOANS MASTER RECORD (STLMST KSDS)           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-01-13 LMB  ORIGINAL                                       *
      * 1990-03-09 CRP  PRINCIPAL STATUS CODES ADDED CHG2845           *
      * 1992-07-22 EWS  DISBURSE NOTICE TEXT PER LEGAL REQ2401         *
      * 1993-01-24 BHS  DISBURSE STATUS CODES ADDED REQ4286            *
      * 1998-09-04 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-12-13 TWH  PERFORMANCE - SUBSIDY READ AHEAD CHG3279       *
      * 2002-10-04 TWH  DEFERINT LIMIT REVISED PRB6792                 *
      * 2003-01-10 CRP  DEFERINT LIMIT REVISED CHG5259                 *
      * 2004-11-04 BHS  PERFORMANCE - PAYMENT READ AHEAD INC8497       *
      * 2004-10-07 EWS  DEFERINT ROUNDING - HALF UP PRB7045            *
      * 2006-01-04 MKL  CAPINT CALCULATION CORRECTED REQ1993           *
      *================================================================*
       01  STL-MASTER-REC.
           05  STLM-KEY                    PIC X(12).
           05  STLM-CUST-ID                PIC X(10).
           05  STLM-NAME                   PIC X(30).
           05  STLM-DESC                   PIC X(30).
           05  STLM-REF-NO                 PIC X(16).
           05  STLM-ADDR                   PIC X(30).
           05  STLM-CONTACT                PIC X(20).
           05  STL-MASTER-AMOUNTS.
               10  STLM-PRINCIPAL-AMT      PIC S9(13)V99 COMP-3.
               10  STLM-PRINCIPAL-YTD      PIC S9(13)V99 COMP-3.
               10  STLM-DISBURSE-AMT       PIC S9(13)V99 COMP-3.
               10  STLM-DISBURSE-YTD       PIC S9(13)V99 COMP-3.
               10  STLM-CAPINT-AMT         PIC S9(13)V99 COMP-3.
               10  STLM-CAPINT-YTD         PIC S9(13)V99 COMP-3.
               10  STLM-SUBSIDY-AMT        PIC S9(13)V99 COMP-3.
               10  STLM-SUBSIDY-YTD        PIC S9(13)V99 COMP-3.
               10  STLM-PAYMENT-AMT        PIC S9(13)V99 COMP-3.
               10  STLM-PAYMENT-YTD        PIC S9(13)V99 COMP-3.
               10  STLM-DEFERINT-AMT       PIC S9(13)V99 COMP-3.
               10  STLM-DEFERINT-YTD       PIC S9(13)V99 COMP-3.
           05  STL-MASTER-DATES.
               10  STLM-OPEN-DATE          PIC 9(08).
               10  STLM-MATURITY-DATE      PIC 9(08).
               10  STLM-LAST-ACT-DATE      PIC 9(08).
               10  STLM-CLOSE-DATE         PIC 9(08).
               10  STLM-EXPIRY-DATE        PIC 9(08).
               10  STLM-NEXT-REVIEW-DATE   PIC 9(08).
               10  STLM-LAST-MAINT-DATE    PIC 9(08).
               10  STLM-LAST-STMT-DATE     PIC 9(08).
               10  STLM-LAST-PAY-DATE      PIC 9(08).
           05  STL-MASTER-CODES.
               10  STLM-LOAN-TYPE-CD       PIC X(02).
                   88  STLM-LOAN-TYPE-CD-CS        VALUE 'CS'.
                   88  STLM-LOAN-TYPE-CD-DV        VALUE 'DV'.
                   88  STLM-LOAN-TYPE-CD-JL        VALUE 'JL'.
                   88  STLM-LOAN-TYPE-CD-NE        VALUE 'NE'.
                   88  STLM-LOAN-TYPE-CD-SH        VALUE 'SH'.
               10  STLM-DEFER-CODE-CD      PIC X(02).
                   88  STLM-DEFER-CODE-CD-AH       VALUE 'AH'.
                   88  STLM-DEFER-CODE-CD-CZ       VALUE 'CZ'.
                   88  STLM-DEFER-CODE-CD-GM       VALUE 'GM'.
                   88  STLM-DEFER-CODE-CD-ND       VALUE 'ND'.
                   88  STLM-DEFER-CODE-CD-UC       VALUE 'UC'.
               10  STLM-SOURCE-CD          PIC X(03).
                   88  STLM-SOURCE-CD-AST          VALUE 'AST'.
                   88  STLM-SOURCE-CD-BCK          VALUE 'BCK'.
                   88  STLM-SOURCE-CD-CJG          VALUE 'CJG'.
                   88  STLM-SOURCE-CD-DSJ          VALUE 'DSJ'.
                   88  STLM-SOURCE-CD-JVY          VALUE 'JVY'.
                   88  STLM-SOURCE-CD-VEF          VALUE 'VEF'.
               10  STLM-STATUS-CD          PIC X(02).
                   88  STLM-STATUS-CD-HY           VALUE 'HY'.
                   88  STLM-STATUS-CD-JZ           VALUE 'JZ'.
                   88  STLM-STATUS-CD-RK           VALUE 'RK'.
               10  STLM-REGION-CD          PIC X(01).
                   88  STLM-REGION-CD-C            VALUE 'C'.
                   88  STLM-REGION-CD-D            VALUE 'D'.
                   88  STLM-REGION-CD-T            VALUE 'T'.
                   88  STLM-REGION-CD-U            VALUE 'U'.
                   88  STLM-REGION-CD-W            VALUE 'W'.
                   88  STLM-REGION-CD-Y            VALUE 'Y'.
               10  STLM-CYCLE-CD           PIC X(03).
                   88  STLM-CYCLE-CD-CFJ           VALUE 'CFJ'.
                   88  STLM-CYCLE-CD-KHC           VALUE 'KHC'.
                   88  STLM-CYCLE-CD-SEU           VALUE 'SEU'.
                   88  STLM-CYCLE-CD-XKA           VALUE 'XKA'.
               10  STLM-OVERRIDE-FLAG      PIC X(01).
                   88  STLM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  STLM-PREFERRED-FLAG     PIC X(01).
                   88  STLM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  STLM-HOLD-FLAG          PIC X(01).
                   88  STLM-HOLD-FLAG-YES          VALUE 'Y'.
               10  STLM-REVIEW-FLAG        PIC X(01).
                   88  STLM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  STLM-SUSPEND-FLAG       PIC X(01).
                   88  STLM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  STLM-NOTICE-SENT-FLAG   PIC X(01).
                   88  STLM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
           05  STL-MASTER-COUNTERS.
               10  STLM-ITEM-CNT           PIC 9(07).
               10  STLM-EXCEPT-CNT         PIC 9(07).
               10  STLM-PAYMENT-CNT        PIC 9(07).
               10  STLM-CYCLE-CNT          PIC 9(07).
               10  STLM-FEE-PCT            PIC 9(03)V9(06).
               10  STLM-DISCOUNT           PIC 9(03)V9(06).
               10  STLM-PENALTY-PCT        PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
