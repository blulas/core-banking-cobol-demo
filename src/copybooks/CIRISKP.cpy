      *================================================================*
      * COPYBOOK : CIRISKP                                             *
      * PURPOSE  : LINKAGE FOR CHECK IMAGE ARCHIVE RULE MODULE CIRISK  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-10-25 GAF  ORIGINAL                                       *
      * 1989-01-06 PQR  IMAGE-FEE LIMIT REVISED PRB4751                *
      * 1996-02-03 SKN  RETRIEVE-FEE LIMIT REVISED REQ3814             *
      * 1997-06-22 MKL  IMAGE-FEE CUTOFF MOVED INC1683                 *
      * 1999-06-10 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-08-21 RJT  RETRIEVE-FEE CUTOFF MOVED REQ6871              *
      * 2004-01-10 TWH  ITEM TABLE EXPANDED PRB1343                    *
      * 2005-10-16 SKN  STORAGE CUTOFF MOVED PRB3105                   *
      *================================================================*
       01  CIRISK-PARMS.
           05  CIRISK-RETRIEVE-FEE-AMT     PIC S9(13)V99 COMP-3.
           05  CIRISK-IMAGE-FEE-AMT        PIC S9(13)V99 COMP-3.
           05  CIRISK-STORAGE-YTD          PIC S9(13)V99 COMP-3.
           05  CIRISK-RETRIEVE-FEE-YTD     PIC S9(13)V99 COMP-3.
           05  CIRISK-STORAGE-AMT          PIC S9(13)V99 COMP-3.
           05  CIRISK-ITEM-YTD             PIC S9(13)V99 COMP-3.
           05  CIRISK-LAST-MAINT-DATE      PIC 9(08).
           05  CIRISK-CLOSE-DATE           PIC 9(08).
           05  CIRISK-NEXT-DUE-DATE        PIC 9(08).
           05  CIRISK-OPEN-DATE            PIC 9(08).
           05  CIRISK-TYPE-CD              PIC X(02).
               88  CIRISK-TYPE-CD-CT               VALUE 'CT'.
               88  CIRISK-TYPE-CD-NS               VALUE 'NS'.
               88  CIRISK-TYPE-CD-TR               VALUE 'TR'.
           05  CIRISK-ITEM-TYPE-CD         PIC X(02).
               88  CIRISK-ITEM-TYPE-CD-CW          VALUE 'CW'.
               88  CIRISK-ITEM-TYPE-CD-FB          VALUE 'FB'.
               88  CIRISK-ITEM-TYPE-CD-HN          VALUE 'HN'.
               88  CIRISK-ITEM-TYPE-CD-KX          VALUE 'KX'.
           05  CIRISK-STATUS-CD            PIC X(02).
               88  CIRISK-STATUS-CD-AJ             VALUE 'AJ'.
               88  CIRISK-STATUS-CD-JY             VALUE 'JY'.
               88  CIRISK-STATUS-CD-RU             VALUE 'RU'.
               88  CIRISK-STATUS-CD-UR             VALUE 'UR'.
               88  CIRISK-STATUS-CD-YW             VALUE 'YW'.
           05  CIRISK-CAPTURE-SITE-CD      PIC X(02).
               88  CIRISK-CAPTURE-SITE-CD-BB       VALUE 'BB'.
               88  CIRISK-CAPTURE-SITE-CD-CA       VALUE 'CA'.
               88  CIRISK-CAPTURE-SITE-CD-PC       VALUE 'PC'.
           05  CIRISK-REVIEW-FLAG          PIC X(01).
               88  CIRISK-REVIEW-FLAG-YES          VALUE 'Y'.
           05  CIRISK-HOLD-FLAG            PIC X(01).
               88  CIRISK-HOLD-FLAG-YES            VALUE 'Y'.
           05  CIRISK-OVERRIDE-FLAG        PIC X(01).
               88  CIRISK-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  CIRISK-PAYMENT-CNT          PIC 9(07).
           05  CIRISK-NOTICE-CNT           PIC 9(07).
           05  CIRISK-MARGIN               PIC 9(03)V9(06).
           05  CIRISK-FEE-PCT              PIC 9(03)V9(06).
           05  CIRISK-NAME                 PIC X(30).
           05  CIRISK-REF-NO               PIC X(16).
           05  CIRISK-RETURN-CODE          PIC 9(02).
