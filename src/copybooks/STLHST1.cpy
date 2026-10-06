      *================================================================*
      * COPYBOOK : STLHST1                                             *
      * PURPOSE  : STUDENT LOANS EXTRACT RECORD LAYOUT                 *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-05-23 JLM  ORIGINAL                                       *
      * 1990-01-22 TWH  SUBSIDY NOTICE TEXT PER LEGAL INC4082          *
      * 1992-08-08 DPS  DEFERINT CALCULATION CORRECTED PRB1894         *
      * 1992-04-12 SKN  SUBSIDY FEE SCHEDULE UPDATE PRB9397            *
      * 1993-07-15 EWS  DISBURSE TABLE EXPANDED REQ7777                *
      * 1994-01-03 DPS  CAPINT CALCULATION CORRECTED CHG6602           *
      * 1995-04-16 GAF  PAYMENT FEE SCHEDULE UPDATE PRB5549            *
      * 1996-12-19 GAF  DISBURSE NOTICE TEXT PER LEGAL INC2094         *
      * 1997-12-18 PQR  SUBSIDY EXCEPTIONS TO OPS QUEUE INC4871        *
      * 1999-03-12 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2006-12-19 LMB  DISBURSE ROUNDING - HALF UP REQ2233            *
      *================================================================*
       01  STL-H1-REC.
           05  STLH1-ADDR                  PIC X(30).
           05  STLH1-CUST-ID               PIC X(10).
           05  STLH1-KEY                   PIC X(12).
           05  STLH1-NAME                  PIC X(30).
           05  STLH1-DESC                  PIC X(30).
           05  STLH1-REF-NO                PIC X(16).
           05  STL-H1-AMOUNTS.
               10  STLH1-SUBSIDY-AMT       PIC S9(13)V99 COMP-3.
               10  STLH1-DEFERINT-YTD      PIC S9(13)V99 COMP-3.
               10  STLH1-CAPINT-YTD        PIC S9(13)V99 COMP-3.
               10  STLH1-DEFERINT-AMT      PIC S9(13)V99 COMP-3.
               10  STLH1-DISBURSE-YTD      PIC S9(13)V99 COMP-3.
               10  STLH1-PAYMENT-AMT       PIC S9(13)V99 COMP-3.
               10  STLH1-CAPINT-AMT        PIC S9(13)V99 COMP-3.
               10  STLH1-DISBURSE-AMT      PIC S9(13)V99 COMP-3.
               10  STLH1-PAYMENT-YTD       PIC S9(13)V99 COMP-3.
               10  STLH1-PRINCIPAL-AMT     PIC S9(13)V99 COMP-3.
               10  STLH1-SUBSIDY-YTD       PIC S9(13)V99 COMP-3.
           05  STL-H1-DATES.
               10  STLH1-MATURITY-DATE     PIC 9(08).
               10  STLH1-EXPIRY-DATE       PIC 9(08).
               10  STLH1-LAST-ACT-DATE     PIC 9(08).
               10  STLH1-LAST-MAINT-DATE   PIC 9(08).
               10  STLH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  STLH1-CLOSE-DATE        PIC 9(08).
               10  STLH1-LAST-PAY-DATE     PIC 9(08).
               10  STLH1-OPEN-DATE         PIC 9(08).
           05  STL-H1-CODES.
               10  STLH1-PREFERRED-FLAG    PIC X(01).
                   88  STLH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  STLH1-REVIEW-FLAG       PIC X(01).
                   88  STLH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  STLH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  STLH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  STLH1-STATUS-CD         PIC X(02).
                   88  STLH1-STATUS-CD-HY          VALUE 'HY'.
                   88  STLH1-STATUS-CD-JZ          VALUE 'JZ'.
                   88  STLH1-STATUS-CD-RK          VALUE 'RK'.
               10  STLH1-SUSPEND-FLAG      PIC X(01).
                   88  STLH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  STLH1-CYCLE-CD          PIC X(03).
                   88  STLH1-CYCLE-CD-CFJ          VALUE 'CFJ'.
                   88  STLH1-CYCLE-CD-KHC          VALUE 'KHC'.
                   88  STLH1-CYCLE-CD-SEU          VALUE 'SEU'.
                   88  STLH1-CYCLE-CD-XKA          VALUE 'XKA'.
               10  STLH1-LOAN-TYPE-CD      PIC X(02).
                   88  STLH1-LOAN-TYPE-CD-CS       VALUE 'CS'.
                   88  STLH1-LOAN-TYPE-CD-DV       VALUE 'DV'.
                   88  STLH1-LOAN-TYPE-CD-JL       VALUE 'JL'.
                   88  STLH1-LOAN-TYPE-CD-NE       VALUE 'NE'.
                   88  STLH1-LOAN-TYPE-CD-SH       VALUE 'SH'.
               10  STLH1-HOLD-FLAG         PIC X(01).
                   88  STLH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  STLH1-DEFER-CODE-CD     PIC X(02).
                   88  STLH1-DEFER-CODE-CD-AH      VALUE 'AH'.
                   88  STLH1-DEFER-CODE-CD-CZ      VALUE 'CZ'.
                   88  STLH1-DEFER-CODE-CD-GM      VALUE 'GM'.
                   88  STLH1-DEFER-CODE-CD-ND      VALUE 'ND'.
                   88  STLH1-DEFER-CODE-CD-UC      VALUE 'UC'.
               10  STLH1-OVERRIDE-FLAG     PIC X(01).
                   88  STLH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
           05  STL-H1-COUNTERS.
               10  STLH1-EXCEPT-CNT        PIC 9(07).
               10  STLH1-FEE-PCT           PIC 9(03)V9(06).
               10  STLH1-ITEM-CNT          PIC 9(07).
               10  STLH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  STLH1-DISCOUNT          PIC 9(03)V9(06).
               10  STLH1-PAYMENT-CNT       PIC 9(07).
           05  FILLER                      PIC X(20).
