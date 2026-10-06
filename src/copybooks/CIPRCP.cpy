      *================================================================*
      * COPYBOOK : CIPRCP                                              *
      * PURPOSE  : LINKAGE FOR CHECK IMAGE ARCHIVE RULE MODULE CIPRC   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-05-23 PQR  ORIGINAL                                       *
      * 1989-11-04 EWS  NEW IMAGE-FEE REPORT COLUMNS REQ4154           *
      * 1989-06-28 EWS  IMAGE-FEE NOTICE TEXT PER LEGAL REQ5310        *
      * 1993-03-20 EWS  RETRIEVE-FEE LIMIT REVISED INC1116             *
      * 1996-04-07 TWH  ITEM LIMIT REVISED PRB9743                     *
      * 1998-07-02 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-05-05 MKL  IMAGE-FEE RERUN PROTECTION REQ0836             *
      * 1998-05-07 SKN  IMAGE-FEE EXCEPTIONS TO OPS QUEUE PRB5165      *
      * 2001-02-03 TWH  PERFORMANCE - STORAGE READ AHEAD INC3590       *
      * 2004-04-08 RJT  STORAGE CUTOFF MOVED CHG7413                   *
      * 2005-03-09 RJT  RETRIEVE-FEE RERUN PROTECTION REQ5801          *
      * 2006-02-13 SKN  STORAGE ROUNDING - HALF UP PRB9303             *
      * 2006-10-01 RJT  ITEM STATUS CODES ADDED CHG0751                *
      *================================================================*
       01  CIPRC-PARMS.
           05  CIPRC-IMAGE-FEE-AMT         PIC S9(13)V99 COMP-3.
           05  CIPRC-ITEM-AMT              PIC S9(13)V99 COMP-3.
           05  CIPRC-RETRIEVE-FEE-YTD      PIC S9(13)V99 COMP-3.
           05  CIPRC-RETRIEVE-FEE-AMT      PIC S9(13)V99 COMP-3.
           05  CIPRC-ITEM-YTD              PIC S9(13)V99 COMP-3.
           05  CIPRC-STORAGE-AMT           PIC S9(13)V99 COMP-3.
           05  CIPRC-NEXT-DUE-DATE         PIC 9(08).
           05  CIPRC-NOTICE-DATE           PIC 9(08).
           05  CIPRC-EFFECTIVE-DATE        PIC 9(08).
           05  CIPRC-MATURITY-DATE         PIC 9(08).
           05  CIPRC-STATUS-CD             PIC X(02).
               88  CIPRC-STATUS-CD-AJ              VALUE 'AJ'.
               88  CIPRC-STATUS-CD-JY              VALUE 'JY'.
               88  CIPRC-STATUS-CD-RU              VALUE 'RU'.
               88  CIPRC-STATUS-CD-UR              VALUE 'UR'.
               88  CIPRC-STATUS-CD-YW              VALUE 'YW'.
           05  CIPRC-CAPTURE-SITE-CD       PIC X(02).
               88  CIPRC-CAPTURE-SITE-CD-BB        VALUE 'BB'.
               88  CIPRC-CAPTURE-SITE-CD-CA        VALUE 'CA'.
               88  CIPRC-CAPTURE-SITE-CD-PC        VALUE 'PC'.
           05  CIPRC-CHANNEL-CD            PIC X(02).
               88  CIPRC-CHANNEL-CD-FW             VALUE 'FW'.
               88  CIPRC-CHANNEL-CD-KL             VALUE 'KL'.
               88  CIPRC-CHANNEL-CD-NB             VALUE 'NB'.
               88  CIPRC-CHANNEL-CD-TP             VALUE 'TP'.
               88  CIPRC-CHANNEL-CD-ZZ             VALUE 'ZZ'.
           05  CIPRC-ITEM-TYPE-CD          PIC X(02).
               88  CIPRC-ITEM-TYPE-CD-CW           VALUE 'CW'.
               88  CIPRC-ITEM-TYPE-CD-FB           VALUE 'FB'.
               88  CIPRC-ITEM-TYPE-CD-HN           VALUE 'HN'.
               88  CIPRC-ITEM-TYPE-CD-KX           VALUE 'KX'.
           05  CIPRC-EXEMPT-FLAG           PIC X(01).
               88  CIPRC-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  CIPRC-PREFERRED-FLAG        PIC X(01).
               88  CIPRC-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  CIPRC-SUSPEND-FLAG          PIC X(01).
               88  CIPRC-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  CIPRC-NOTICE-CNT            PIC 9(07).
           05  CIPRC-RETRY-CNT             PIC 9(07).
           05  CIPRC-PENALTY-PCT           PIC 9(03)V9(06).
           05  CIPRC-MARGIN                PIC 9(03)V9(06).
           05  CIPRC-REF-NO                PIC X(16).
           05  CIPRC-ADDR                  PIC X(30).
           05  CIPRC-RETURN-CODE           PIC 9(02).
