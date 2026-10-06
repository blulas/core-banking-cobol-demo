      *================================================================*
      * COPYBOOK : MFDTEP                                              *
      * PURPOSE  : LINKAGE FOR MICROFICHE STATEMENTS RULE MODULE MFDTE *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-01-11 JLM  ORIGINAL                                       *
      * 1996-02-22 RJT  RESEARCH CALCULATION CORRECTED INC3713         *
      * 1996-06-15 TWH  STORAGE STATUS CODES ADDED INC7215             *
      * 1996-04-11 TWH  RESEARCH STATUS CODES ADDED PRB5788            *
      * 1996-10-22 DPS  RESEARCH ROUNDING - HALF UP INC5657            *
      * 1996-12-09 PQR  STORAGE LIMIT REVISED CHG7775                  *
      * 1999-03-24 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-07-02 TWH  COPY-FEE CUTOFF MOVED CHG8280                  *
      * 2000-09-17 RJT  POSTAGE CUTOFF MOVED REQ2360                   *
      * 2000-02-08 JLM  PERFORMANCE - STORAGE READ AHEAD INC1215       *
      * 2007-10-01 RJT  AUDIT FINDING - STORAGE CONTROL TOTALS REQ2328 *
      * 2008-09-18 JLM  POSTAGE STATUS CODES ADDED REQ7591             *
      *================================================================*
       01  MFDTE-PARMS.
           05  MFDTE-RESEARCH-AMT          PIC S9(13)V99 COMP-3.
           05  MFDTE-STORAGE-YTD           PIC S9(13)V99 COMP-3.
           05  MFDTE-COPY-FEE-YTD          PIC S9(13)V99 COMP-3.
           05  MFDTE-RESEARCH-YTD          PIC S9(13)V99 COMP-3.
           05  MFDTE-POSTAGE-YTD           PIC S9(13)V99 COMP-3.
           05  MFDTE-POSTAGE-AMT           PIC S9(13)V99 COMP-3.
           05  MFDTE-LAST-PAY-DATE         PIC 9(08).
           05  MFDTE-LAST-MAINT-DATE       PIC 9(08).
           05  MFDTE-EXPIRY-DATE           PIC 9(08).
           05  MFDTE-CLOSE-DATE            PIC 9(08).
           05  MFDTE-TYPE-CD               PIC X(01).
               88  MFDTE-TYPE-CD-G                 VALUE 'G'.
               88  MFDTE-TYPE-CD-K                 VALUE 'K'.
               88  MFDTE-TYPE-CD-W                 VALUE 'W'.
               88  MFDTE-TYPE-CD-Y                 VALUE 'Y'.
           05  MFDTE-FICHE-TYPE-CD         PIC X(02).
               88  MFDTE-FICHE-TYPE-CD-AW          VALUE 'AW'.
               88  MFDTE-FICHE-TYPE-CD-JM          VALUE 'JM'.
               88  MFDTE-FICHE-TYPE-CD-KV          VALUE 'KV'.
               88  MFDTE-FICHE-TYPE-CD-MD          VALUE 'MD'.
               88  MFDTE-FICHE-TYPE-CD-NR          VALUE 'NR'.
           05  MFDTE-RETENTION-CD          PIC X(02).
               88  MFDTE-RETENTION-CD-KT           VALUE 'KT'.
               88  MFDTE-RETENTION-CD-SJ           VALUE 'SJ'.
               88  MFDTE-RETENTION-CD-WV           VALUE 'WV'.
           05  MFDTE-CHANNEL-CD            PIC X(02).
           05  MFDTE-EXEMPT-FLAG           PIC X(01).
               88  MFDTE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  MFDTE-AUTO-RENEW-FLAG       PIC X(01).
               88  MFDTE-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  MFDTE-STOP-FLAG             PIC X(01).
               88  MFDTE-STOP-FLAG-YES             VALUE 'Y'.
           05  MFDTE-NOTICE-CNT            PIC 9(07).
           05  MFDTE-CYCLE-CNT             PIC 9(07).
           05  MFDTE-DISCOUNT              PIC 9(03)V9(06).
           05  MFDTE-MARGIN                PIC 9(03)V9(06).
           05  MFDTE-ADDR                  PIC X(30).
           05  MFDTE-CUST-ID               PIC X(10).
           05  MFDTE-RETURN-CODE           PIC 9(02).
