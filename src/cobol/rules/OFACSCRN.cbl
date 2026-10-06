      *================================================================*
      * PROGRAM  : OFACSCRN                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : SANCTIONS (OFAC SDN) NAME SCREENING OF A CUSTOMER,  *
      *            BENEFICIARY OR COUNTERPARTY AGAINST THE SDN TABLE   *
      *            PASSED BY THE CALLER.                               *
      *            1. NORMALISE: UPPER CASE, PUNCTUATION TO SPACES,    *
      *               DROP NOISE WORDS (INC LLC LTD CO THE MR MRS ...) *
      *            2. SCORE EVERY LIST ENTRY (3000-SCORE-ENTRY):       *
      *                 EXACT NORMALISED NAME            100  EXCT     *
      *                 SAME TOKEN SET, ANY ORDER         95  TSET     *
      *                 SURNAME + GIVEN NAME EXACT        90  SNFN     *
      *                 SURNAME + GIVEN NAME SOUNDEX      85  SNFP     *
      *                 SURNAME + FIRST INITIAL           80  SNFI     *
      *                 SURNAME SOUNDEX + FIRST INITIAL   75  SXFI     *
      *                 SURNAME ONLY                      50  SNAM     *
      *                 TOKEN OVERLAP (DICE X 100)       0-94 DICE     *
      *               COUNTRY MATCH ADDS 10 (MAX 100) TO SCORES >= 50  *
      *            3. BEST SCORE >= 90 HIT (BLOCK), 75-89 POTENTIAL    *
      *               (REVIEW), < 75 CLEAR                             *
      *            4. COMPREHENSIVELY EMBARGOED COUNTRY = HIT          *
      *               REGARDLESS OF NAME (31 CFR PARTS 510-560)        *
      * LINKAGE  : OFSPARM (PARMS), SDNTBL (TABLE, BY REFERENCE)       *
      * CALLED BY: PAYINIT (ONLINE), PAYPROC, OFACBAT (BATCH)          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2002-03-15 BHS  ORIGINAL - EXACT NAME MATCH ONLY               *
      * 2005-02-28 BHS  NOISE WORD REMOVAL                             *
      * 2011-01-24 DPS  SOUNDEX SURNAME, TOKEN OVERLAP, PRE-NORMALISED *
      *                 TABLE (CPU REDUCTION - INC14420)               *
      * 2014-07-28 DPS  COUNTRY BOOST                                  *
      * 2018-08-06 AKW  EMBARGO COUNTRY BLOCK                          *
      * 2019-11-04 AKW  THRESHOLDS 90/75 PER MODEL VALIDATION MV-0193  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OFACSCRN.
       AUTHOR. B H SANDOVAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-THRESHOLDS.
      *    MV-0193 - WAS 85 / 70 BEFORE 2019-11
           05  WS-HIT-THRESHOLD            PIC 9(03) VALUE 090.
           05  WS-POTENTIAL-THRESHOLD      PIC 9(03) VALUE 075.
           05  WS-COUNTRY-BOOST            PIC 9(03) VALUE 010.
           05  WS-BOOST-FLOOR              PIC 9(03) VALUE 050.

       01  WS-NOISE-VALUES.
           05  FILLER PIC X(08) VALUE 'INC     '.
           05  FILLER PIC X(08) VALUE 'LLC     '.
           05  FILLER PIC X(08) VALUE 'LTD     '.
           05  FILLER PIC X(08) VALUE 'CO      '.
           05  FILLER PIC X(08) VALUE 'THE     '.
           05  FILLER PIC X(08) VALUE 'MR      '.
           05  FILLER PIC X(08) VALUE 'MRS     '.
           05  FILLER PIC X(08) VALUE 'MS      '.
           05  FILLER PIC X(08) VALUE 'DR      '.
           05  FILLER PIC X(08) VALUE 'CORP    '.
           05  FILLER PIC X(08) VALUE 'LIMITED '.
           05  FILLER PIC X(08) VALUE 'COMPANY '.
       01  WS-NOISE-TABLE REDEFINES WS-NOISE-VALUES.
           05  WS-NOISE-WORD PIC X(08) OCCURS 12 TIMES
                             INDEXED BY NOISE-IDX.

      *    COMPREHENSIVE EMBARGO - CUBA, IRAN, NORTH KOREA, SYRIA
       01  WS-EMBARGO-VALUES.
           05  FILLER PIC X(08) VALUE 'CUIRKPSY'.
       01  WS-EMBARGO-TABLE REDEFINES WS-EMBARGO-VALUES.
           05  WS-EMBARGO-CTRY PIC X(02) OCCURS 4 TIMES
                               INDEXED BY EMB-IDX.

      *--- NORMALISATION WORK AREA (5000-) ---------------------------*
       01  WS-NM-INPUT.
           05  WS-NM-RAW                   PIC X(60).
           05  WS-NM-RAW-LAST              PIC X(30).
           05  WS-NM-RAW-FIRST             PIC X(20).
           05  WS-NM-TYPE                  PIC X(01).
       01  WS-NM-OUTPUT.
           05  WS-NM-NORM                  PIC X(60).
           05  WS-NM-LAST                  PIC X(30).
           05  WS-NM-FIRST                 PIC X(20).
           05  WS-NM-TOK-COUNT             PIC 9(02).
           05  WS-NM-TOK                   PIC X(20) OCCURS 10 TIMES.
       01  WS-NM-WORK.
           05  WS-NM-UPPER                 PIC X(60).
           05  WS-NM-CLEAN                 PIC X(60).
           05  WS-NM-PART-1                PIC X(60).
           05  WS-NM-PART-2                PIC X(60).
           05  WS-NM-PART-COUNT            PIC 9(02).
           05  WS-NM-SPLIT-TOK             PIC X(20) OCCURS 10 TIMES.
           05  WS-NM-PTR                   PIC 9(03).
           05  WS-NM-NOISE-FLAG            PIC X(01).
           05  WS-NM-SCRATCH               PIC X(60).

      *--- INPUT NAME AFTER NORMALISATION -----------------------------*
       01  WS-IN-NAME.
           05  WS-IN-NORM                  PIC X(60).
           05  WS-IN-LAST                  PIC X(30).
           05  WS-IN-FIRST                 PIC X(20).
           05  WS-IN-LAST-SX               PIC X(04).
           05  WS-IN-FIRST-SX              PIC X(04).
           05  WS-IN-TOK-COUNT             PIC 9(02).
           05  WS-IN-TOK                   PIC X(20) OCCURS 10 TIMES.

      *--- SOUNDEX (6000-) --------------------------------------------*
       01  WS-SX-WORK.
           05  WS-SX-IN                    PIC X(30).
           05  WS-SX-OUT                   PIC X(04).
           05  WS-SX-LEN                   PIC 9(01).
           05  WS-SX-CH                    PIC X(01).
           05  WS-SX-CODE                  PIC X(01).
           05  WS-SX-PREV                  PIC X(01).

      *--- SCORING ----------------------------------------------------*
       01  WS-SCORE-WORK.
           05  WS-ENTRY-SCORE              PIC 9(03).
           05  WS-ENTRY-RULE               PIC X(04).
           05  WS-CAND-SCORE               PIC 9(03).
           05  WS-CAND-RULE                PIC X(04).
           05  WS-COMMON                   PIC 9(02).
           05  WS-IN-SIG                   PIC 9(02).
           05  WS-SDN-SIG                  PIC 9(02).
           05  WS-DICE                     PIC 9(03).
           05  WS-FOUND-FLAG               PIC X(01).
           05  WS-BEST-SCORE               PIC 9(03).
           05  WS-BEST-IDX                 PIC 9(04).
           05  WS-BEST-RULE                PIC X(04).

       01  WS-SUBSCRIPTS.
           05  WS-I                        PIC 9(04).
           05  WS-J                        PIC 9(02).
           05  WS-K                        PIC 9(02).
           05  WS-E                        PIC 9(04).

       LINKAGE SECTION.
       COPY OFSPARM.
       COPY SDNTBL.

       PROCEDURE DIVISION USING OFAC-SCREEN-PARMS SDN-TABLE.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           IF OFS-RC-ERROR
               GOBACK
           END-IF
           IF NOT SDNT-PREPARED
               PERFORM 1100-PREPARE-TABLE
           END-IF
           PERFORM 2000-NORMALISE-INPUT

           MOVE ZERO   TO WS-BEST-SCORE WS-BEST-IDX
           MOVE SPACES TO WS-BEST-RULE
           PERFORM VARYING WS-E FROM 1 BY 1
                   UNTIL WS-E > SDNT-COUNT OR WS-E > 500
               PERFORM 3000-SCORE-ENTRY
               IF WS-ENTRY-SCORE NOT < WS-POTENTIAL-THRESHOLD
                   ADD 1 TO OFS-MATCH-COUNT
               END-IF
               IF WS-ENTRY-SCORE > WS-BEST-SCORE
                   MOVE WS-ENTRY-SCORE TO WS-BEST-SCORE
                   MOVE WS-ENTRY-RULE  TO WS-BEST-RULE
                   MOVE WS-E           TO WS-BEST-IDX
               END-IF
           END-PERFORM

           PERFORM 4000-SET-RESULT
           PERFORM 4100-EMBARGO-CHECK
           GOBACK.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           MOVE ZERO   TO OFS-SCORE OFS-MATCH-ENT-NUM OFS-MATCH-COUNT
                          OFS-RETURN-CODE
           MOVE SPACES TO OFS-MATCH-RULE OFS-MATCH-NAME
                          OFS-MATCH-PROGRAM OFS-MATCH-COUNTRY
                          OFS-NORM-NAME
           MOVE 'N'    TO OFS-EMBARGO-FLAG
           SET OFS-CLEAR TO TRUE
           IF OFS-FULL-NAME = SPACES
               IF OFS-LAST-NAME = SPACES
                   MOVE 12 TO OFS-RETURN-CODE
                   MOVE 'ERRN' TO OFS-MATCH-RULE
                   EXIT PARAGRAPH
               END-IF
               STRING FUNCTION TRIM(OFS-FIRST-NAME) DELIMITED BY SIZE
                      ' '                           DELIMITED BY SIZE
                      FUNCTION TRIM(OFS-LAST-NAME)  DELIMITED BY SIZE
                 INTO OFS-FULL-NAME
               END-STRING
           END-IF
           IF SDNT-COUNT = ZERO
               MOVE 12 TO OFS-RETURN-CODE
               MOVE 'ERRT' TO OFS-MATCH-RULE
           END-IF.

      *----------------------------------------------------------------*
      * ONE-TIME NORMALISATION OF THE LIST (INC14420)                  *
      *----------------------------------------------------------------*
       1100-PREPARE-TABLE.
           PERFORM VARYING WS-E FROM 1 BY 1
                   UNTIL WS-E > SDNT-COUNT OR WS-E > 500
               MOVE SDNT-NAME(WS-E)  TO WS-NM-RAW
               MOVE SPACES           TO WS-NM-RAW-LAST WS-NM-RAW-FIRST
               MOVE SDNT-TYPE(WS-E)  TO WS-NM-TYPE
               PERFORM 5000-NORMALISE-NAME
               MOVE WS-NM-NORM       TO SDNT-NORM-NAME(WS-E)
               MOVE WS-NM-LAST       TO SDNT-LAST-NAME(WS-E)
               MOVE WS-NM-FIRST      TO SDNT-FIRST-NAME(WS-E)
               MOVE WS-NM-TOK-COUNT  TO SDNT-TOKEN-COUNT(WS-E)
               IF SDNT-TOKEN-COUNT(WS-E) > 8
                   MOVE 8 TO SDNT-TOKEN-COUNT(WS-E)
               END-IF
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
                   MOVE WS-NM-TOK(WS-J) TO SDNT-TOKEN(WS-E WS-J)
               END-PERFORM
               MOVE WS-NM-LAST  TO WS-SX-IN
               PERFORM 6000-SOUNDEX
               MOVE WS-SX-OUT   TO SDNT-LAST-SOUNDEX(WS-E)
               MOVE WS-NM-FIRST TO WS-SX-IN
               PERFORM 6000-SOUNDEX
               MOVE WS-SX-OUT   TO SDNT-FIRST-SOUNDEX(WS-E)
           END-PERFORM
           SET SDNT-PREPARED TO TRUE.

      *----------------------------------------------------------------*
       2000-NORMALISE-INPUT.
           MOVE OFS-FULL-NAME   TO WS-NM-RAW
           MOVE OFS-LAST-NAME   TO WS-NM-RAW-LAST
           MOVE OFS-FIRST-NAME  TO WS-NM-RAW-FIRST
           MOVE OFS-PARTY-TYPE  TO WS-NM-TYPE
           PERFORM 5000-NORMALISE-NAME
           MOVE WS-NM-NORM      TO WS-IN-NORM OFS-NORM-NAME
           MOVE WS-NM-LAST      TO WS-IN-LAST
           MOVE WS-NM-FIRST     TO WS-IN-FIRST
           MOVE WS-NM-TOK-COUNT TO WS-IN-TOK-COUNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 10
               MOVE WS-NM-TOK(WS-J) TO WS-IN-TOK(WS-J)
           END-PERFORM
           MOVE WS-IN-LAST  TO WS-SX-IN
           PERFORM 6000-SOUNDEX
           MOVE WS-SX-OUT   TO WS-IN-LAST-SX
           MOVE WS-IN-FIRST TO WS-SX-IN
           PERFORM 6000-SOUNDEX
           MOVE WS-SX-OUT   TO WS-IN-FIRST-SX.

      *----------------------------------------------------------------*
      * SCORE ONE LIST ENTRY (WS-E) AGAINST THE NORMALISED INPUT.      *
      * THE HIGHEST APPLICABLE RULE WINS.                              *
      *----------------------------------------------------------------*
       3000-SCORE-ENTRY.
           MOVE ZERO   TO WS-ENTRY-SCORE
           MOVE SPACES TO WS-ENTRY-RULE
           IF WS-IN-NORM = SDNT-NORM-NAME(WS-E)
               MOVE 100    TO WS-ENTRY-SCORE
               MOVE 'EXCT' TO WS-ENTRY-RULE
           ELSE
               PERFORM 3100-TOKEN-OVERLAP
               IF SDNT-TYPE(WS-E) = 'I'
                  AND NOT OFS-PARTY-ENTITY
                  AND WS-IN-LAST NOT = SPACES
                  AND SDNT-LAST-NAME(WS-E) NOT = SPACES
                   PERFORM 3200-SURNAME-RULES
               END-IF
           END-IF

           IF WS-ENTRY-SCORE NOT < WS-BOOST-FLOOR
              AND OFS-COUNTRY NOT = SPACES
              AND OFS-COUNTRY = SDNT-COUNTRY(WS-E)
               ADD WS-COUNTRY-BOOST TO WS-ENTRY-SCORE
               IF WS-ENTRY-SCORE > 100
                   MOVE 100 TO WS-ENTRY-SCORE
               END-IF
           END-IF.

      *    TOKEN SET / DICE COEFFICIENT ON TOKENS OF 2+ CHARACTERS
       3100-TOKEN-OVERLAP.
           MOVE ZERO TO WS-COMMON WS-IN-SIG WS-SDN-SIG
           PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > SDNT-TOKEN-COUNT(WS-E)
               IF SDNT-TOKEN(WS-E WS-K)(2:1) NOT = SPACE
                   ADD 1 TO WS-SDN-SIG
               END-IF
           END-PERFORM
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-IN-TOK-COUNT
               IF WS-IN-TOK(WS-J)(2:1) NOT = SPACE
                   ADD 1 TO WS-IN-SIG
                   MOVE 'N' TO WS-FOUND-FLAG
                   PERFORM VARYING WS-K FROM 1 BY 1
                           UNTIL WS-K > SDNT-TOKEN-COUNT(WS-E)
                              OR WS-FOUND-FLAG = 'Y'
                       IF WS-IN-TOK(WS-J) = SDNT-TOKEN(WS-E WS-K)
                           MOVE 'Y' TO WS-FOUND-FLAG
                       END-IF
                   END-PERFORM
                   IF WS-FOUND-FLAG = 'Y'
                       ADD 1 TO WS-COMMON
                   END-IF
               END-IF
           END-PERFORM
           IF WS-COMMON > ZERO
               IF WS-COMMON = WS-IN-SIG AND WS-COMMON = WS-SDN-SIG
                   MOVE 95     TO WS-ENTRY-SCORE
                   MOVE 'TSET' TO WS-ENTRY-RULE
               ELSE
                   COMPUTE WS-DICE =
                       (200 * WS-COMMON) / (WS-IN-SIG + WS-SDN-SIG)
                   IF WS-DICE > 94
                       MOVE 94 TO WS-DICE
                   END-IF
                   MOVE WS-DICE TO WS-ENTRY-SCORE
                   MOVE 'DICE'  TO WS-ENTRY-RULE
               END-IF
           END-IF.

      *    INDIVIDUALS - SURNAME / GIVEN NAME / SOUNDEX RULES
       3200-SURNAME-RULES.
           MOVE ZERO   TO WS-CAND-SCORE
           MOVE SPACES TO WS-CAND-RULE
           EVALUATE TRUE
               WHEN WS-IN-LAST = SDNT-LAST-NAME(WS-E)
                AND WS-IN-FIRST NOT = SPACES
                AND WS-IN-FIRST = SDNT-FIRST-NAME(WS-E)
                   MOVE 90     TO WS-CAND-SCORE
                   MOVE 'SNFN' TO WS-CAND-RULE
               WHEN WS-IN-LAST = SDNT-LAST-NAME(WS-E)
                AND WS-IN-FIRST NOT = SPACES
                AND WS-IN-FIRST-SX = SDNT-FIRST-SOUNDEX(WS-E)
                   MOVE 85     TO WS-CAND-SCORE
                   MOVE 'SNFP' TO WS-CAND-RULE
               WHEN WS-IN-LAST = SDNT-LAST-NAME(WS-E)
                AND WS-IN-FIRST NOT = SPACES
                AND WS-IN-FIRST(1:1) = SDNT-FIRST-NAME(WS-E)(1:1)
                   MOVE 80     TO WS-CAND-SCORE
                   MOVE 'SNFI' TO WS-CAND-RULE
               WHEN WS-IN-LAST-SX = SDNT-LAST-SOUNDEX(WS-E)
                AND WS-IN-FIRST NOT = SPACES
                AND WS-IN-FIRST(1:1) = SDNT-FIRST-NAME(WS-E)(1:1)
                   MOVE 75     TO WS-CAND-SCORE
                   MOVE 'SXFI' TO WS-CAND-RULE
               WHEN WS-IN-LAST = SDNT-LAST-NAME(WS-E)
                   MOVE 50     TO WS-CAND-SCORE
                   MOVE 'SNAM' TO WS-CAND-RULE
           END-EVALUATE
           IF WS-CAND-SCORE > WS-ENTRY-SCORE
               MOVE WS-CAND-SCORE TO WS-ENTRY-SCORE
               MOVE WS-CAND-RULE  TO WS-ENTRY-RULE
           END-IF.

      *----------------------------------------------------------------*
       4000-SET-RESULT.
           MOVE WS-BEST-SCORE TO OFS-SCORE
           IF WS-BEST-IDX > ZERO
               MOVE WS-BEST-RULE              TO OFS-MATCH-RULE
               MOVE SDNT-ENT-NUM(WS-BEST-IDX) TO OFS-MATCH-ENT-NUM
               MOVE SDNT-NAME(WS-BEST-IDX)    TO OFS-MATCH-NAME
               MOVE SDNT-PROGRAM(WS-BEST-IDX) TO OFS-MATCH-PROGRAM
               MOVE SDNT-COUNTRY(WS-BEST-IDX) TO OFS-MATCH-COUNTRY
           END-IF
           EVALUATE TRUE
               WHEN OFS-SCORE NOT < WS-HIT-THRESHOLD
                   SET OFS-HIT TO TRUE
                   MOVE 08 TO OFS-RETURN-CODE
               WHEN OFS-SCORE NOT < WS-POTENTIAL-THRESHOLD
                   SET OFS-POTENTIAL-HIT TO TRUE
                   MOVE 04 TO OFS-RETURN-CODE
               WHEN OTHER
                   SET OFS-CLEAR TO TRUE
                   MOVE 00 TO OFS-RETURN-CODE
           END-EVALUATE.

       4100-EMBARGO-CHECK.
           IF OFS-COUNTRY = SPACES
               EXIT PARAGRAPH
           END-IF
           SET EMB-IDX TO 1
           SEARCH WS-EMBARGO-CTRY
               AT END
                   CONTINUE
               WHEN WS-EMBARGO-CTRY(EMB-IDX) = OFS-COUNTRY
                   MOVE 'Y'    TO OFS-EMBARGO-FLAG
                   MOVE 100    TO OFS-SCORE
                   MOVE 'EMBG' TO OFS-MATCH-RULE
                   SET OFS-HIT TO TRUE
                   MOVE 08 TO OFS-RETURN-CODE
           END-SEARCH.

      *----------------------------------------------------------------*
      * NORMALISE WS-NM-RAW -> WS-NM-NORM / TOKENS / SURNAME / GIVEN   *
      *----------------------------------------------------------------*
       5000-NORMALISE-NAME.
           MOVE SPACES TO WS-NM-OUTPUT WS-NM-PART-1 WS-NM-PART-2
           MOVE ZERO   TO WS-NM-TOK-COUNT WS-NM-PART-COUNT
           MOVE FUNCTION UPPER-CASE(WS-NM-RAW) TO WS-NM-UPPER
           UNSTRING WS-NM-UPPER DELIMITED BY ','
               INTO WS-NM-PART-1 WS-NM-PART-2
               TALLYING IN WS-NM-PART-COUNT
           END-UNSTRING

           MOVE WS-NM-UPPER TO WS-NM-SCRATCH
           PERFORM 5100-STRIP-PUNCTUATION
           MOVE WS-NM-SCRATCH TO WS-NM-CLEAN
           PERFORM 5200-TOKENISE

      *    SURNAME AND GIVEN NAME
           EVALUATE TRUE
               WHEN WS-NM-RAW-LAST NOT = SPACES
                   MOVE FUNCTION UPPER-CASE(WS-NM-RAW-LAST)
                     TO WS-NM-SCRATCH
                   PERFORM 5100-STRIP-PUNCTUATION
                   MOVE FUNCTION TRIM(WS-NM-SCRATCH) TO WS-NM-LAST
                   MOVE FUNCTION UPPER-CASE(WS-NM-RAW-FIRST)
                     TO WS-NM-SCRATCH
                   PERFORM 5100-STRIP-PUNCTUATION
                   MOVE FUNCTION TRIM(WS-NM-SCRATCH) TO WS-NM-SCRATCH
                   UNSTRING WS-NM-SCRATCH DELIMITED BY ALL SPACE
                       INTO WS-NM-FIRST
                   END-UNSTRING
               WHEN WS-NM-PART-COUNT > 1
                   MOVE WS-NM-PART-1 TO WS-NM-SCRATCH
                   PERFORM 5100-STRIP-PUNCTUATION
                   MOVE FUNCTION TRIM(WS-NM-SCRATCH) TO WS-NM-LAST
                   MOVE WS-NM-PART-2 TO WS-NM-SCRATCH
                   PERFORM 5100-STRIP-PUNCTUATION
                   MOVE FUNCTION TRIM(WS-NM-SCRATCH) TO WS-NM-SCRATCH
                   UNSTRING WS-NM-SCRATCH DELIMITED BY ALL SPACE
                       INTO WS-NM-FIRST
                   END-UNSTRING
               WHEN WS-NM-TOK-COUNT > 1
                   MOVE WS-NM-TOK(WS-NM-TOK-COUNT) TO WS-NM-LAST
                   MOVE WS-NM-TOK(1)               TO WS-NM-FIRST
               WHEN WS-NM-TOK-COUNT = 1
                   MOVE WS-NM-TOK(1) TO WS-NM-LAST
           END-EVALUATE
           IF WS-NM-TYPE = 'E'
               MOVE SPACES TO WS-NM-LAST WS-NM-FIRST
           END-IF.

       5100-STRIP-PUNCTUATION.
           INSPECT WS-NM-SCRATCH REPLACING ALL '.' BY SPACE
                                           ALL ',' BY SPACE
                                           ALL '-' BY SPACE
                                           ALL '&' BY SPACE
                                           ALL '/' BY SPACE
                                           ALL '(' BY SPACE
                                           ALL ')' BY SPACE
                                           ALL '"' BY SPACE
                                           ALL "'" BY SPACE
                                           ALL '#' BY SPACE
                                           ALL ';' BY SPACE
                                           ALL ':' BY SPACE.

      *    SPLIT ON SPACES, DROP NOISE WORDS, REBUILD SINGLE-SPACED
       5200-TOKENISE.
           MOVE SPACES TO WS-NM-SPLIT-TOK(1) WS-NM-SPLIT-TOK(2)
                          WS-NM-SPLIT-TOK(3) WS-NM-SPLIT-TOK(4)
                          WS-NM-SPLIT-TOK(5) WS-NM-SPLIT-TOK(6)
                          WS-NM-SPLIT-TOK(7) WS-NM-SPLIT-TOK(8)
                          WS-NM-SPLIT-TOK(9) WS-NM-SPLIT-TOK(10)
           MOVE FUNCTION TRIM(WS-NM-CLEAN) TO WS-NM-SCRATCH
           UNSTRING WS-NM-SCRATCH DELIMITED BY ALL SPACE
               INTO WS-NM-SPLIT-TOK(1) WS-NM-SPLIT-TOK(2)
                    WS-NM-SPLIT-TOK(3) WS-NM-SPLIT-TOK(4)
                    WS-NM-SPLIT-TOK(5) WS-NM-SPLIT-TOK(6)
                    WS-NM-SPLIT-TOK(7) WS-NM-SPLIT-TOK(8)
                    WS-NM-SPLIT-TOK(9) WS-NM-SPLIT-TOK(10)
           END-UNSTRING
           MOVE 1 TO WS-NM-PTR
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 10
               IF WS-NM-SPLIT-TOK(WS-J) NOT = SPACES
                   MOVE 'N' TO WS-NM-NOISE-FLAG
                   SET NOISE-IDX TO 1
                   SEARCH WS-NOISE-WORD
                       AT END
                           CONTINUE
                       WHEN WS-NOISE-WORD(NOISE-IDX) =
                            WS-NM-SPLIT-TOK(WS-J)
                           MOVE 'Y' TO WS-NM-NOISE-FLAG
                   END-SEARCH
                   IF WS-NM-NOISE-FLAG = 'N'
                       ADD 1 TO WS-NM-TOK-COUNT
                       MOVE WS-NM-SPLIT-TOK(WS-J)
                         TO WS-NM-TOK(WS-NM-TOK-COUNT)
                       IF WS-NM-PTR > 1
                           STRING ' ' DELIMITED BY SIZE
                             INTO WS-NM-NORM WITH POINTER WS-NM-PTR
                           END-STRING
                       END-IF
                       STRING WS-NM-SPLIT-TOK(WS-J) DELIMITED BY SPACE
                         INTO WS-NM-NORM WITH POINTER WS-NM-PTR
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * AMERICAN SOUNDEX OF THE FIRST WORD OF WS-SX-IN                 *
      * (H AND W DO NOT SEPARATE EQUAL CODES; VOWELS AND Y DO)         *
      *----------------------------------------------------------------*
       6000-SOUNDEX.
           MOVE SPACES TO WS-SX-OUT
           MOVE ZERO   TO WS-SX-LEN
           MOVE SPACE  TO WS-SX-PREV
           PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > 30 OR WS-SX-LEN = 4
               MOVE WS-SX-IN(WS-K:1) TO WS-SX-CH
               IF WS-SX-CH = SPACE AND WS-SX-LEN > 0
                   MOVE 31 TO WS-K
               ELSE
                   IF WS-SX-CH IS ALPHABETIC AND WS-SX-CH NOT = SPACE
                       PERFORM 6100-SOUNDEX-CODE
                       IF WS-SX-LEN = 0
                           MOVE WS-SX-CH   TO WS-SX-OUT(1:1)
                           MOVE 1          TO WS-SX-LEN
                           MOVE WS-SX-CODE TO WS-SX-PREV
                       ELSE
                           EVALUATE WS-SX-CODE
                               WHEN 'H'
                                   CONTINUE
                               WHEN '0'
                                   MOVE '0' TO WS-SX-PREV
                               WHEN OTHER
                                   IF WS-SX-CODE NOT = WS-SX-PREV
                                       ADD 1 TO WS-SX-LEN
                                       MOVE WS-SX-CODE
                                         TO WS-SX-OUT(WS-SX-LEN:1)
                                   END-IF
                                   MOVE WS-SX-CODE TO WS-SX-PREV
                           END-EVALUATE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF WS-SX-LEN > 0
               INSPECT WS-SX-OUT REPLACING ALL SPACE BY '0'
           END-IF.

       6100-SOUNDEX-CODE.
           EVALUATE WS-SX-CH
               WHEN 'B' WHEN 'F' WHEN 'P' WHEN 'V'
                   MOVE '1' TO WS-SX-CODE
               WHEN 'C' WHEN 'G' WHEN 'J' WHEN 'K'
               WHEN 'Q' WHEN 'S' WHEN 'X' WHEN 'Z'
                   MOVE '2' TO WS-SX-CODE
               WHEN 'D' WHEN 'T'
                   MOVE '3' TO WS-SX-CODE
               WHEN 'L'
                   MOVE '4' TO WS-SX-CODE
               WHEN 'M' WHEN 'N'
                   MOVE '5' TO WS-SX-CODE
               WHEN 'R'
                   MOVE '6' TO WS-SX-CODE
               WHEN 'H' WHEN 'W'
                   MOVE 'H' TO WS-SX-CODE
               WHEN OTHER
                   MOVE '0' TO WS-SX-CODE
           END-EVALUATE.
