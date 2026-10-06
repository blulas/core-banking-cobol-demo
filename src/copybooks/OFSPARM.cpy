      *================================================================*
      * COPYBOOK : OFSPARM                                             *
      * PURPOSE  : LINKAGE FOR OFACSCRN - SANCTIONS NAME SCREENING     *
      *----------------------------------------------------------------*
      * CALLING SEQUENCE (STABLE - ALSO USED BY OFACBAT, PAYPROC,      *
      * PAYINIT AND OTHER SCREENING CALLERS):                          *
      *                                                                *
      *     COPY OFSPARM.        (WORKING-STORAGE)                     *
      *     COPY SDNTBL.         (WORKING-STORAGE, LOADED ONCE)        *
      *     INITIALIZE OFS-INPUT                                       *
      *     MOVE name            TO OFS-FULL-NAME                      *
      *     MOVE surname/given   TO OFS-LAST-NAME / OFS-FIRST-NAME     *
      *                             (OPTIONAL - INDIVIDUALS ONLY)      *
      *     MOVE 'I' / 'E' / ' ' TO OFS-PARTY-TYPE                     *
      *     MOVE iso country     TO OFS-COUNTRY  (OPTIONAL)            *
      *     CALL 'OFACSCRN' USING OFAC-SCREEN-PARMS SDN-TABLE          *
      *                                                                *
      * INPUT  OFS-FULL-NAME   FREE FORM ('IVANOV, PIOTR' OR 'PIOTR A  *
      *                        IVANOV' OR 'THE ZORVAK TRADING CO LTD') *
      *        OFS-LAST-NAME   IF BLANK, DERIVED: TEXT BEFORE A COMMA, *
      *        OFS-FIRST-NAME  ELSE LAST TOKEN = SURNAME, FIRST TOKEN  *
      *                        = GIVEN NAME                            *
      *        OFS-PARTY-TYPE  I INDIVIDUAL, E ENTITY, SPACE UNKNOWN   *
      *                        (E SKIPS THE SURNAME/GIVEN-NAME RULES)  *
      *        OFS-COUNTRY     ISO-3166 ALPHA-2 OF PARTY / BENE BANK   *
      * OUTPUT OFS-RESULT      C CLEAR (<75), P POTENTIAL (75-89),     *
      *                        H HIT (>=90 OR EMBARGOED COUNTRY)       *
      *        OFS-SCORE       0-100 BEST SCORE OVER ALL ENTRIES       *
      *        OFS-MATCH-*     BEST MATCHING LIST ENTRY                *
      *        OFS-MATCH-RULE  EXCT TSET SNFN SNFP SNFI SXFI SNAM DICE *
      *                        EMBG (SEE OFACSCRN 3000-SCORE-ENTRY)    *
      *        OFS-MATCH-COUNT ENTRIES SCORING >= 75                   *
      * RETURN 00 CLEAR  04 POTENTIAL  08 HIT  12 ERROR (BLANK NAME OR *
      *        EMPTY SDN TABLE)                                        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2002-03-15 BHS  ORIGINAL                                       *
      * 2011-01-24 DPS  SOUNDEX / TOKEN SCORING, MATCH COUNT           *
      * 2018-08-06 AKW  EMBARGO COUNTRY FLAG                           *
      *================================================================*
       01  OFAC-SCREEN-PARMS.
           05  OFS-INPUT.
               10  OFS-FULL-NAME           PIC X(60).
               10  OFS-LAST-NAME           PIC X(30).
               10  OFS-FIRST-NAME          PIC X(20).
               10  OFS-PARTY-TYPE          PIC X(01).
                   88  OFS-PARTY-INDIVIDUAL          VALUE 'I'.
                   88  OFS-PARTY-ENTITY              VALUE 'E'.
               10  OFS-COUNTRY             PIC X(02).
           05  OFS-OUTPUT.
               10  OFS-RESULT              PIC X(01).
                   88  OFS-CLEAR                     VALUE 'C'.
                   88  OFS-POTENTIAL-HIT             VALUE 'P'.
                   88  OFS-HIT                       VALUE 'H'.
               10  OFS-SCORE               PIC 9(03).
               10  OFS-MATCH-RULE          PIC X(04).
               10  OFS-MATCH-ENT-NUM       PIC 9(06).
               10  OFS-MATCH-NAME          PIC X(60).
               10  OFS-MATCH-PROGRAM       PIC X(10).
               10  OFS-MATCH-COUNTRY       PIC X(02).
               10  OFS-MATCH-COUNT         PIC 9(03).
               10  OFS-EMBARGO-FLAG        PIC X(01).
                   88  OFS-EMBARGOED                 VALUE 'Y'.
               10  OFS-NORM-NAME           PIC X(60).
           05  OFS-RETURN-CODE             PIC 9(02).
               88  OFS-RC-CLEAR                      VALUE 00.
               88  OFS-RC-POTENTIAL                  VALUE 04.
               88  OFS-RC-HIT                        VALUE 08.
               88  OFS-RC-ERROR                      VALUE 12.
