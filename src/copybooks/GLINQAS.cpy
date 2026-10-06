      *================================================================*
      * COPYBOOK : GLINQAS                                             *
      * PURPOSE  : BMS SYMBOLIC MAP FOR MAPSET GLINQAS MAP GLINQAM     *
      *            TRANSACTION CB52 - SEE BMS SOURCE GLINQAS           *
      *================================================================*
       01  GLINQAMI.
           02  FILLER                      PIC X(12).
           02  TITLEL                      COMP PIC S9(4).
           02  TITLEF                      PIC X.
           02  FILLER REDEFINES TITLEF.
               03  TITLEA                  PIC X.
           02  TITLEI                      PIC X(040).
           02  CURDATEL                    COMP PIC S9(4).
           02  CURDATEF                    PIC X.
           02  FILLER REDEFINES CURDATEF.
               03  CURDATEA                PIC X.
           02  CURDATEI                    PIC X(010).
           02  GLACCTL                     COMP PIC S9(4).
           02  GLACCTF                     PIC X.
           02  FILLER REDEFINES GLACCTF.
               03  GLACCTA                 PIC X.
           02  GLACCTI                     PIC X(010).
           02  GLCCL                       COMP PIC S9(4).
           02  GLCCF                       PIC X.
           02  FILLER REDEFINES GLCCF.
               03  GLCCA                   PIC X.
           02  GLCCI                       PIC X(005).
           02  GLDESCL                     COMP PIC S9(4).
           02  GLDESCF                     PIC X.
           02  FILLER REDEFINES GLDESCF.
               03  GLDESCA                 PIC X.
           02  GLDESCI                     PIC X(030).
           02  GLTYPEL                     COMP PIC S9(4).
           02  GLTYPEF                     PIC X.
           02  FILLER REDEFINES GLTYPEF.
               03  GLTYPEA                 PIC X.
           02  GLTYPEI                     PIC X(001).
           02  GLNORML                     COMP PIC S9(4).
           02  GLNORMF                     PIC X.
           02  FILLER REDEFINES GLNORMF.
               03  GLNORMA                 PIC X.
           02  GLNORMI                     PIC X(001).
           02  PERSTL                      COMP PIC S9(4).
           02  PERSTF                      PIC X.
           02  FILLER REDEFINES PERSTF.
               03  PERSTA                  PIC X.
           02  PERSTI                      PIC X(010).
           02  OPENBLL                     COMP PIC S9(4).
           02  OPENBLF                     PIC X.
           02  FILLER REDEFINES OPENBLF.
               03  OPENBLA                 PIC X.
           02  OPENBLI                     PIC X(020).
           02  PERDRL                      COMP PIC S9(4).
           02  PERDRF                      PIC X.
           02  FILLER REDEFINES PERDRF.
               03  PERDRA                  PIC X.
           02  PERDRI                      PIC X(020).
           02  PERCRL                      COMP PIC S9(4).
           02  PERCRF                      PIC X.
           02  FILLER REDEFINES PERCRF.
               03  PERCRA                  PIC X.
           02  PERCRI                      PIC X(020).
           02  CLOSBLL                     COMP PIC S9(4).
           02  CLOSBLF                     PIC X.
           02  FILLER REDEFINES CLOSBLF.
               03  CLOSBLA                 PIC X.
           02  CLOSBLI                     PIC X(020).
           02  TDYDRL                      COMP PIC S9(4).
           02  TDYDRF                      PIC X.
           02  FILLER REDEFINES TDYDRF.
               03  TDYDRA                  PIC X.
           02  TDYDRI                      PIC X(020).
           02  TDYCRL                      COMP PIC S9(4).
           02  TDYCRF                      PIC X.
           02  FILLER REDEFINES TDYCRF.
               03  TDYCRA                  PIC X.
           02  TDYCRI                      PIC X(020).
           02  LSTPSTL                     COMP PIC S9(4).
           02  LSTPSTF                     PIC X.
           02  FILLER REDEFINES LSTPSTF.
               03  LSTPSTA                 PIC X.
           02  LSTPSTI                     PIC X(010).
           02  ALLCCL                      COMP PIC S9(4).
           02  ALLCCF                      PIC X.
           02  FILLER REDEFINES ALLCCF.
               03  ALLCCA                  PIC X.
           02  ALLCCI                      PIC X(020).
           02  CCCNTL                      COMP PIC S9(4).
           02  CCCNTF                      PIC X.
           02  FILLER REDEFINES CCCNTF.
               03  CCCNTA                  PIC X.
           02  CCCNTI                      PIC X(003).
           02  MSGL                        COMP PIC S9(4).
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  MSGI                        PIC X(079).
       01  GLINQAMO REDEFINES GLINQAMI.
           02  FILLER                      PIC X(12).
           02  FILLER                      PIC X(03).
           02  TITLEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  CURDATEO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  GLACCTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  GLCCO                       PIC X(005).
           02  FILLER                      PIC X(03).
           02  GLDESCO                     PIC X(030).
           02  FILLER                      PIC X(03).
           02  GLTYPEO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  GLNORMO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  PERSTO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  OPENBLO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  PERDRO                      PIC X(020).
           02  FILLER                      PIC X(03).
           02  PERCRO                      PIC X(020).
           02  FILLER                      PIC X(03).
           02  CLOSBLO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  TDYDRO                      PIC X(020).
           02  FILLER                      PIC X(03).
           02  TDYCRO                      PIC X(020).
           02  FILLER                      PIC X(03).
           02  LSTPSTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  ALLCCO                      PIC X(020).
           02  FILLER                      PIC X(03).
           02  CCCNTO                      PIC X(003).
           02  FILLER                      PIC X(03).
           02  MSGO                        PIC X(079).
