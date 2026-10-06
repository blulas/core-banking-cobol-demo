      *================================================================*
      * COPYBOOK : COLMST                                              *
      * PURPOSE  : COLLECTIONS QUEUE MASTER RECORD (COLMST KSDS)       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-09-14 EWS  ORIGINAL                                       *
      * 1997-03-10 GAF  NEW PAST-DUE REPORT COLUMNS CHG9414            *
      * 1998-04-26 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-07-26 EWS  AUDIT FINDING - COLLECTED CONTROL TOTALS PRB981*
      * 2002-09-07 EWS  COLLECTED TABLE EXPANDED PRB0505               *
      * 2002-07-17 CRP  COLLECTED TABLE EXPANDED REQ0216               *
      * 2002-12-18 GAF  PERFORMANCE - WRITEOFF READ AHEAD INC2946      *
      * 2003-02-03 MKL  PROMISED STATUS CODES ADDED INC0492            *
      * 2004-01-02 TWH  COLLECTED RERUN PROTECTION INC3366             *
      * 2004-11-25 CRP  PAST-DUE STATUS CODES ADDED CHG0836            *
      * 2004-11-11 CRP  PAST-DUE LIMIT REVISED PRB8378                 *
      * 2004-04-09 LMB  WRITEOFF EXCEPTIONS TO OPS QUEUE INC5528       *
      * 2004-11-26 DPS  FEE CALCULATION CORRECTED REQ3819              *
      *================================================================*
       01  COL-MASTER-REC.
           05  COLM-KEY                    PIC X(12).
           05  COLM-CUST-ID                PIC X(10).
           05  COLM-NAME                   PIC X(30).
           05  COLM-DESC                   PIC X(30).
           05  COLM-REF-NO                 PIC X(16).
           05  COLM-ADDR                   PIC X(30).
           05  COLM-CONTACT                PIC X(20).
           05  COL-MASTER-AMOUNTS.
               10  COLM-PAST-DUE-AMT       PIC S9(13)V99 COMP-3.
               10  COLM-PAST-DUE-YTD       PIC S9(13)V99 COMP-3.
               10  COLM-PROMISED-AMT       PIC S9(13)V99 COMP-3.
               10  COLM-PROMISED-YTD       PIC S9(13)V99 COMP-3.
               10  COLM-COLLECTED-AMT      PIC S9(13)V99 COMP-3.
               10  COLM-COLLECTED-YTD      PIC S9(13)V99 COMP-3.
               10  COLM-WRITEOFF-AMT       PIC S9(13)V99 COMP-3.
               10  COLM-WRITEOFF-YTD       PIC S9(13)V99 COMP-3.
               10  COLM-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  COLM-FEE-YTD            PIC S9(13)V99 COMP-3.
           05  COL-MASTER-DATES.
               10  COLM-EFFECTIVE-DATE     PIC 9(08).
               10  COLM-NEXT-REVIEW-DATE   PIC 9(08).
               10  COLM-EXPIRY-DATE        PIC 9(08).
               10  COLM-NOTICE-DATE        PIC 9(08).
               10  COLM-CLOSE-DATE         PIC 9(08).
               10  COLM-OPEN-DATE          PIC 9(08).
               10  COLM-NEXT-DUE-DATE      PIC 9(08).
               10  COLM-LAST-MAINT-DATE    PIC 9(08).
               10  COLM-LAST-PAY-DATE      PIC 9(08).
           05  COL-MASTER-CODES.
               10  COLM-QUEUE-CD           PIC X(02).
               10  COLM-ACTION-CD          PIC X(02).
                   88  COLM-ACTION-CD-KJ           VALUE 'KJ'.
                   88  COLM-ACTION-CD-TM           VALUE 'TM'.
                   88  COLM-ACTION-CD-VL           VALUE 'VL'.
                   88  COLM-ACTION-CD-ZS           VALUE 'ZS'.
               10  COLM-CYCLE-CD           PIC X(02).
               10  COLM-STATUS-CD          PIC X(02).
                   88  COLM-STATUS-CD-PS           VALUE 'PS'.
                   88  COLM-STATUS-CD-UR           VALUE 'UR'.
                   88  COLM-STATUS-CD-XM           VALUE 'XM'.
                   88  COLM-STATUS-CD-YA           VALUE 'YA'.
               10  COLM-REASON-CD          PIC X(02).
                   88  COLM-REASON-CD-AT           VALUE 'AT'.
                   88  COLM-REASON-CD-BJ           VALUE 'BJ'.
                   88  COLM-REASON-CD-GG           VALUE 'GG'.
                   88  COLM-REASON-CD-ZR           VALUE 'ZR'.
               10  COLM-REGION-CD          PIC X(02).
                   88  COLM-REGION-CD-AT           VALUE 'AT'.
                   88  COLM-REGION-CD-AU           VALUE 'AU'.
                   88  COLM-REGION-CD-XR           VALUE 'XR'.
                   88  COLM-REGION-CD-XU           VALUE 'XU'.
               10  COLM-NOTICE-SENT-FLAG   PIC X(01).
                   88  COLM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  COLM-SUSPEND-FLAG       PIC X(01).
                   88  COLM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  COLM-REVIEW-FLAG        PIC X(01).
                   88  COLM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  COLM-OVERRIDE-FLAG      PIC X(01).
                   88  COLM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  COLM-EXEMPT-FLAG        PIC X(01).
                   88  COLM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  COLM-PREFERRED-FLAG     PIC X(01).
                   88  COLM-PREFERRED-FLAG-YES     VALUE 'Y'.
           05  COL-MASTER-COUNTERS.
               10  COLM-RETRY-CNT          PIC 9(07).
               10  COLM-ITEM-CNT           PIC 9(07).
               10  COLM-CYCLE-CNT          PIC 9(07).
               10  COLM-NOTICE-CNT         PIC 9(07).
               10  COLM-MARGIN             PIC 9(03)V9(06).
               10  COLM-FEE-PCT            PIC 9(03)V9(06).
               10  COLM-PENALTY-PCT        PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
