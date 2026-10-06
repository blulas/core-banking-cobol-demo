      *================================================================*
      * COPYBOOK : SDNREC                                              *
      * PURPOSE  : SANCTIONS LIST ENTRY (OFAC SDN + BANK INTERNAL      *
      *            WATCH LIST), ONE ENTRY PER LISTED PARTY.            *
      * FILE     : SDNLIST (LINE SEQUENTIAL, PIPE DELIMITED)           *
      *              ENTNUM|TYPE|NAME|COUNTRY|PROGRAM|LISTDATE|REMARKS *
      *            '#' IN COLUMN 1 = COMMENT LINE                      *
      *            NAME FOR INDIVIDUALS IS 'LAST, FIRST MIDDLE'        *
      *            TYPE I = INDIVIDUAL, E = ENTITY, V = VESSEL         *
      *            ONLINE: VSAM KSDS 'SDNLIST' KEY SDN-ENT-NUM, AND    *
      *            CICS-RESIDENT TABLE PROGRAM 'SDNTBLD' (SEE SDNTBL). *
      * LOAD     : CALLERS UNSTRING EACH LINE INTO SDN-RECORD AND MOVE *
      *            TO THE NEXT SDNT-ENTRY OF SDN-TABLE (COPY SDNTBL).  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2002-03-15 BHS  ORIGINAL - OFAC SDN DOWNLOAD                   *
      * 2014-07-28 DPS  ADDED PROGRAM CODE AND LIST DATE               *
      *================================================================*
       01  SDN-RECORD.
           05  SDN-ENT-NUM                 PIC 9(06).
           05  SDN-TYPE                    PIC X(01).
               88  SDN-INDIVIDUAL                    VALUE 'I'.
               88  SDN-ENTITY                        VALUE 'E'.
               88  SDN-VESSEL                        VALUE 'V'.
           05  SDN-NAME                    PIC X(60).
           05  SDN-COUNTRY                 PIC X(02).
           05  SDN-PROGRAM                 PIC X(10).
           05  SDN-LIST-DATE               PIC 9(08).
           05  SDN-REMARKS                 PIC X(40).
