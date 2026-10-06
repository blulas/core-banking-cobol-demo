      *================================================================*
      * COPYBOOK : CIMEXT1                                             *
      * PURPOSE  : CHECK IMAGE ARCHIVE EXTRACT RECORD LAYOUT           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-04-23 PQR  ORIGINAL                                       *
      * 1997-03-02 DPS  ITEM ROUNDING - HALF UP PRB1838                *
      * 2001-10-05 GAF  ITEM FEE SCHEDULE UPDATE PRB3277               *
      * 2001-08-26 SKN  RETRIEVE-FEE RERUN PROTECTION REQ7034          *
      * 2003-01-24 PQR  NEW RETRIEVE-FEE REPORT COLUMNS CHG9812        *
      * 2006-09-01 LMB  IMAGE-FEE RERUN PROTECTION PRB7633             *
      * 2008-05-09 DPS  STORAGE EXCEPTIONS TO OPS QUEUE PRB0845        *
      * 2009-08-23 MKL  PERFORMANCE - ITEM READ AHEAD PRB5291          *
      *================================================================*
       01  CIM-H1-REC.
           05  CIMH1-CONTACT               PIC X(20).
           05  CIMH1-ADDR                  PIC X(30).
           05  CIMH1-REF-NO                PIC X(16).
           05  CIMH1-KEY                   PIC X(12).
           05  CIMH1-DESC                  PIC X(30).
           05  CIMH1-NAME                  PIC X(30).
           05  CIM-H1-AMOUNTS.
               10  CIMH1-ITEM-YTD          PIC S9(13)V99 COMP-3.
               10  CIMH1-RETRIEVE-FEE-AMT  PIC S9(13)V99 COMP-3.
               10  CIMH1-STORAGE-YTD       PIC S9(13)V99 COMP-3.
               10  CIMH1-IMAGE-FEE-AMT     PIC S9(13)V99 COMP-3.
               10  CIMH1-RETRIEVE-FEE-YTD  PIC S9(13)V99 COMP-3.
               10  CIMH1-ITEM-AMT          PIC S9(13)V99 COMP-3.
               10  CIMH1-STORAGE-AMT       PIC S9(13)V99 COMP-3.
           05  CIM-H1-DATES.
               10  CIMH1-OPEN-DATE         PIC 9(08).
               10  CIMH1-EFFECTIVE-DATE    PIC 9(08).
               10  CIMH1-NEXT-DUE-DATE     PIC 9(08).
               10  CIMH1-MATURITY-DATE     PIC 9(08).
               10  CIMH1-CLOSE-DATE        PIC 9(08).
               10  CIMH1-LAST-MAINT-DATE   PIC 9(08).
               10  CIMH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  CIMH1-LAST-STMT-DATE    PIC 9(08).
               10  CIMH1-NOTICE-DATE       PIC 9(08).
           05  CIM-H1-CODES.
               10  CIMH1-ITEM-TYPE-CD      PIC X(02).
                   88  CIMH1-ITEM-TYPE-CD-CW       VALUE 'CW'.
                   88  CIMH1-ITEM-TYPE-CD-FB       VALUE 'FB'.
                   88  CIMH1-ITEM-TYPE-CD-HN       VALUE 'HN'.
                   88  CIMH1-ITEM-TYPE-CD-KX       VALUE 'KX'.
               10  CIMH1-OVERRIDE-FLAG     PIC X(01).
                   88  CIMH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  CIMH1-PREFERRED-FLAG    PIC X(01).
                   88  CIMH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  CIMH1-HOLD-FLAG         PIC X(01).
                   88  CIMH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  CIMH1-SUSPEND-FLAG      PIC X(01).
                   88  CIMH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  CIMH1-STATUS-CD         PIC X(02).
                   88  CIMH1-STATUS-CD-AJ          VALUE 'AJ'.
                   88  CIMH1-STATUS-CD-JY          VALUE 'JY'.
                   88  CIMH1-STATUS-CD-RU          VALUE 'RU'.
                   88  CIMH1-STATUS-CD-UR          VALUE 'UR'.
                   88  CIMH1-STATUS-CD-YW          VALUE 'YW'.
               10  CIMH1-BRANCH-CD         PIC X(02).
                   88  CIMH1-BRANCH-CD-DJ          VALUE 'DJ'.
                   88  CIMH1-BRANCH-CD-FX          VALUE 'FX'.
                   88  CIMH1-BRANCH-CD-MT          VALUE 'MT'.
                   88  CIMH1-BRANCH-CD-UA          VALUE 'UA'.
                   88  CIMH1-BRANCH-CD-VL          VALUE 'VL'.
                   88  CIMH1-BRANCH-CD-XG          VALUE 'XG'.
               10  CIMH1-REVIEW-FLAG       PIC X(01).
                   88  CIMH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  CIMH1-CAPTURE-SITE-CD   PIC X(02).
                   88  CIMH1-CAPTURE-SITE-CD-BB    VALUE 'BB'.
                   88  CIMH1-CAPTURE-SITE-CD-CA    VALUE 'CA'.
                   88  CIMH1-CAPTURE-SITE-CD-PC    VALUE 'PC'.
           05  CIM-H1-COUNTERS.
               10  CIMH1-MARGIN            PIC 9(03)V9(06).
               10  CIMH1-EXCEPT-CNT        PIC 9(07).
               10  CIMH1-FEE-PCT           PIC 9(03)V9(06).
               10  CIMH1-PAYMENT-CNT       PIC 9(07).
           05  FILLER                      PIC X(20).
