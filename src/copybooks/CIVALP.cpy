      *================================================================*
      * COPYBOOK : CIVALP                                              *
      * PURPOSE  : LINKAGE FOR CHECK IMAGE ARCHIVE RULE MODULE CIVAL   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-08-04 BHS  ORIGINAL                                       *
      * 1992-07-14 GAF  ITEM RERUN PROTECTION PRB9980                  *
      * 1992-03-01 JLM  RETRIEVE-FEE EXCEPTIONS TO OPS QUEUE INC2788   *
      * 1997-07-21 DPS  IMAGE-FEE STATUS CODES ADDED INC3075           *
      * 1998-09-22 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-03-26 DPS  PERFORMANCE - STORAGE READ AHEAD CHG6716       *
      * 2002-08-04 CRP  IMAGE-FEE ROUNDING - HALF UP REQ3525           *
      * 2004-09-06 BHS  ITEM TABLE EXPANDED REQ8936                    *
      * 2004-03-14 DPS  AUDIT FINDING - IMAGE-FEE CONTROL TOTALS REQ907*
      * 2004-06-20 CRP  IMAGE-FEE RERUN PROTECTION REQ6678             *
      * 2004-10-28 TWH  STORAGE CUTOFF MOVED CHG5985                   *
      * 2006-05-02 SKN  STORAGE CUTOFF MOVED PRB8909                   *
      * 2007-05-21 TWH  IMAGE-FEE CUTOFF MOVED REQ5229                 *
      *================================================================*
       01  CIVAL-PARMS.
           05  CIVAL-IMAGE-FEE-AMT         PIC S9(13)V99 COMP-3.
           05  CIVAL-RETRIEVE-FEE-AMT      PIC S9(13)V99 COMP-3.
           05  CIVAL-STORAGE-AMT           PIC S9(13)V99 COMP-3.
           05  CIVAL-STORAGE-YTD           PIC S9(13)V99 COMP-3.
           05  CIVAL-ITEM-AMT              PIC S9(13)V99 COMP-3.
           05  CIVAL-ITEM-YTD              PIC S9(13)V99 COMP-3.
           05  CIVAL-OPEN-DATE             PIC 9(08).
           05  CIVAL-EFFECTIVE-DATE        PIC 9(08).
           05  CIVAL-LAST-STMT-DATE        PIC 9(08).
           05  CIVAL-LAST-MAINT-DATE       PIC 9(08).
           05  CIVAL-BRANCH-CD             PIC X(02).
               88  CIVAL-BRANCH-CD-DJ              VALUE 'DJ'.
               88  CIVAL-BRANCH-CD-FX              VALUE 'FX'.
               88  CIVAL-BRANCH-CD-MT              VALUE 'MT'.
               88  CIVAL-BRANCH-CD-UA              VALUE 'UA'.
               88  CIVAL-BRANCH-CD-VL              VALUE 'VL'.
               88  CIVAL-BRANCH-CD-XG              VALUE 'XG'.
           05  CIVAL-CHANNEL-CD            PIC X(02).
               88  CIVAL-CHANNEL-CD-FW             VALUE 'FW'.
               88  CIVAL-CHANNEL-CD-KL             VALUE 'KL'.
               88  CIVAL-CHANNEL-CD-NB             VALUE 'NB'.
               88  CIVAL-CHANNEL-CD-TP             VALUE 'TP'.
               88  CIVAL-CHANNEL-CD-ZZ             VALUE 'ZZ'.
           05  CIVAL-ITEM-TYPE-CD          PIC X(02).
               88  CIVAL-ITEM-TYPE-CD-CW           VALUE 'CW'.
               88  CIVAL-ITEM-TYPE-CD-FB           VALUE 'FB'.
               88  CIVAL-ITEM-TYPE-CD-HN           VALUE 'HN'.
               88  CIVAL-ITEM-TYPE-CD-KX           VALUE 'KX'.
           05  CIVAL-CAPTURE-SITE-CD       PIC X(02).
               88  CIVAL-CAPTURE-SITE-CD-BB        VALUE 'BB'.
               88  CIVAL-CAPTURE-SITE-CD-CA        VALUE 'CA'.
               88  CIVAL-CAPTURE-SITE-CD-PC        VALUE 'PC'.
           05  CIVAL-EXEMPT-FLAG           PIC X(01).
               88  CIVAL-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  CIVAL-HOLD-FLAG             PIC X(01).
               88  CIVAL-HOLD-FLAG-YES             VALUE 'Y'.
           05  CIVAL-OVERRIDE-FLAG         PIC X(01).
               88  CIVAL-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  CIVAL-PAYMENT-CNT           PIC 9(07).
           05  CIVAL-NOTICE-CNT            PIC 9(07).
           05  CIVAL-MARGIN                PIC 9(03)V9(06).
           05  CIVAL-PENALTY-PCT           PIC 9(03)V9(06).
           05  CIVAL-NAME                  PIC X(30).
           05  CIVAL-ADDR                  PIC X(30).
           05  CIVAL-RETURN-CODE           PIC 9(02).
