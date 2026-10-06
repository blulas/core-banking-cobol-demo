      *================================================================*
      * COPYBOOK : CIMMST                                              *
      * PURPOSE  : CHECK IMAGE ARCHIVE MASTER RECORD (CIMMST KSDS)     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-08-16 GAF  ORIGINAL                                       *
      * 1991-06-16 RJT  STORAGE CUTOFF MOVED CHG1321                   *
      * 1996-08-11 GAF  NEW ITEM REPORT COLUMNS REQ5766                *
      * 1996-09-07 LMB  STORAGE TABLE EXPANDED CHG0555                 *
      * 1999-09-07 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-05-12 SKN  ITEM LIMIT REVISED PRB4380                     *
      * 2002-05-19 SKN  IMAGE-FEE STATUS CODES ADDED REQ0643           *
      * 2002-10-25 SKN  STORAGE RERUN PROTECTION PRB3407               *
      * 2003-09-17 MKL  IMAGE-FEE TABLE EXPANDED CHG5625               *
      *================================================================*
       01  CIM-MASTER-REC.
           05  CIMM-KEY                    PIC X(12).
           05  CIMM-CUST-ID                PIC X(10).
           05  CIMM-NAME                   PIC X(30).
           05  CIMM-DESC                   PIC X(30).
           05  CIMM-REF-NO                 PIC X(16).
           05  CIMM-ADDR                   PIC X(30).
           05  CIMM-CONTACT                PIC X(20).
           05  CIM-MASTER-AMOUNTS.
               10  CIMM-ITEM-AMT           PIC S9(13)V99 COMP-3.
               10  CIMM-ITEM-YTD           PIC S9(13)V99 COMP-3.
               10  CIMM-RETRIEVE-FEE-AMT   PIC S9(13)V99 COMP-3.
               10  CIMM-RETRIEVE-FEE-YTD   PIC S9(13)V99 COMP-3.
               10  CIMM-IMAGE-FEE-AMT      PIC S9(13)V99 COMP-3.
               10  CIMM-IMAGE-FEE-YTD      PIC S9(13)V99 COMP-3.
               10  CIMM-STORAGE-AMT        PIC S9(13)V99 COMP-3.
               10  CIMM-STORAGE-YTD        PIC S9(13)V99 COMP-3.
           05  CIM-MASTER-DATES.
               10  CIMM-MATURITY-DATE      PIC 9(08).
               10  CIMM-CLOSE-DATE         PIC 9(08).
               10  CIMM-NOTICE-DATE        PIC 9(08).
               10  CIMM-EFFECTIVE-DATE     PIC 9(08).
               10  CIMM-NEXT-DUE-DATE      PIC 9(08).
               10  CIMM-LAST-STMT-DATE     PIC 9(08).
               10  CIMM-OPEN-DATE          PIC 9(08).
               10  CIMM-LAST-MAINT-DATE    PIC 9(08).
               10  CIMM-NEXT-REVIEW-DATE   PIC 9(08).
           05  CIM-MASTER-CODES.
               10  CIMM-CAPTURE-SITE-CD    PIC X(02).
                   88  CIMM-CAPTURE-SITE-CD-BB     VALUE 'BB'.
                   88  CIMM-CAPTURE-SITE-CD-CA     VALUE 'CA'.
                   88  CIMM-CAPTURE-SITE-CD-PC     VALUE 'PC'.
               10  CIMM-ITEM-TYPE-CD       PIC X(02).
                   88  CIMM-ITEM-TYPE-CD-CW        VALUE 'CW'.
                   88  CIMM-ITEM-TYPE-CD-FB        VALUE 'FB'.
                   88  CIMM-ITEM-TYPE-CD-HN        VALUE 'HN'.
                   88  CIMM-ITEM-TYPE-CD-KX        VALUE 'KX'.
               10  CIMM-TYPE-CD            PIC X(02).
                   88  CIMM-TYPE-CD-CT             VALUE 'CT'.
                   88  CIMM-TYPE-CD-NS             VALUE 'NS'.
                   88  CIMM-TYPE-CD-TR             VALUE 'TR'.
               10  CIMM-BRANCH-CD          PIC X(02).
                   88  CIMM-BRANCH-CD-DJ           VALUE 'DJ'.
                   88  CIMM-BRANCH-CD-FX           VALUE 'FX'.
                   88  CIMM-BRANCH-CD-MT           VALUE 'MT'.
                   88  CIMM-BRANCH-CD-UA           VALUE 'UA'.
                   88  CIMM-BRANCH-CD-VL           VALUE 'VL'.
                   88  CIMM-BRANCH-CD-XG           VALUE 'XG'.
               10  CIMM-STATUS-CD          PIC X(02).
                   88  CIMM-STATUS-CD-AJ           VALUE 'AJ'.
                   88  CIMM-STATUS-CD-JY           VALUE 'JY'.
                   88  CIMM-STATUS-CD-RU           VALUE 'RU'.
                   88  CIMM-STATUS-CD-UR           VALUE 'UR'.
                   88  CIMM-STATUS-CD-YW           VALUE 'YW'.
               10  CIMM-CHANNEL-CD         PIC X(02).
                   88  CIMM-CHANNEL-CD-FW          VALUE 'FW'.
                   88  CIMM-CHANNEL-CD-KL          VALUE 'KL'.
                   88  CIMM-CHANNEL-CD-NB          VALUE 'NB'.
                   88  CIMM-CHANNEL-CD-TP          VALUE 'TP'.
                   88  CIMM-CHANNEL-CD-ZZ          VALUE 'ZZ'.
               10  CIMM-HOLD-FLAG          PIC X(01).
                   88  CIMM-HOLD-FLAG-YES          VALUE 'Y'.
               10  CIMM-REVIEW-FLAG        PIC X(01).
                   88  CIMM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  CIMM-SUSPEND-FLAG       PIC X(01).
                   88  CIMM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  CIMM-OVERRIDE-FLAG      PIC X(01).
                   88  CIMM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  CIMM-PREFERRED-FLAG     PIC X(01).
                   88  CIMM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  CIMM-EXEMPT-FLAG        PIC X(01).
                   88  CIMM-EXEMPT-FLAG-YES        VALUE 'Y'.
           05  CIM-MASTER-COUNTERS.
               10  CIMM-NOTICE-CNT         PIC 9(07).
               10  CIMM-RETRY-CNT          PIC 9(07).
               10  CIMM-PAYMENT-CNT        PIC 9(07).
               10  CIMM-EXCEPT-CNT         PIC 9(07).
               10  CIMM-FEE-PCT            PIC 9(03)V9(06).
               10  CIMM-MARGIN             PIC 9(03)V9(06).
               10  CIMM-PENALTY-PCT        PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
