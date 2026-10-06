      *================================================================*
      * COPYBOOK : SWPMNTS                                             *
      * PURPOSE  : BMS SYMBOLIC MAP FOR MAPSET SWPMNTS MAP SWPMNTM     *
      *            TRANSACTION CB51 - SEE BMS SOURCE SWPMNTS           *
      *================================================================*
       01  SWPMNTMI.
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
           02  ACTIONL                     COMP PIC S9(4).
           02  ACTIONF                     PIC X.
           02  FILLER REDEFINES ACTIONF.
               03  ACTIONA                 PIC X.
           02  ACTIONI                     PIC X(001).
           02  SWPIDL                      COMP PIC S9(4).
           02  SWPIDF                      PIC X.
           02  FILLER REDEFINES SWPIDF.
               03  SWPIDA                  PIC X.
           02  SWPIDI                      PIC X(008).
           02  SWPTYPL                     COMP PIC S9(4).
           02  SWPTYPF                     PIC X.
           02  FILLER REDEFINES SWPTYPF.
               03  SWPTYPA                 PIC X.
           02  SWPTYPI                     PIC X(001).
           02  SWPSTSL                     COMP PIC S9(4).
           02  SWPSTSF                     PIC X.
           02  FILLER REDEFINES SWPSTSF.
               03  SWPSTSA                 PIC X.
           02  SWPSTSI                     PIC X(001).
           02  CUSTIDL                     COMP PIC S9(4).
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  CUSTIDI                     PIC X(010).
           02  CUSTNML                     COMP PIC S9(4).
           02  CUSTNMF                     PIC X.
           02  FILLER REDEFINES CUSTNMF.
               03  CUSTNMA                 PIC X.
           02  CUSTNMI                     PIC X(030).
           02  SRCACTL                     COMP PIC S9(4).
           02  SRCACTF                     PIC X.
           02  FILLER REDEFINES SRCACTF.
               03  SRCACTA                 PIC X.
           02  SRCACTI                     PIC X(012).
           02  SRCPRDL                     COMP PIC S9(4).
           02  SRCPRDF                     PIC X.
           02  FILLER REDEFINES SRCPRDF.
               03  SRCPRDA                 PIC X.
           02  SRCPRDI                     PIC X(004).
           02  TGTACTL                     COMP PIC S9(4).
           02  TGTACTF                     PIC X.
           02  FILLER REDEFINES TGTACTF.
               03  TGTACTA                 PIC X.
           02  TGTACTI                     PIC X(012).
           02  TGTPRDL                     COMP PIC S9(4).
           02  TGTPRDF                     PIC X.
           02  FILLER REDEFINES TGTPRDF.
               03  TGTPRDA                 PIC X.
           02  TGTPRDI                     PIC X(004).
           02  TGTBALL                     COMP PIC S9(4).
           02  TGTBALF                     PIC X.
           02  FILLER REDEFINES TGTBALF.
               03  TGTBALA                 PIC X.
           02  TGTBALI                     PIC X(015).
           02  MINBALL                     COMP PIC S9(4).
           02  MINBALF                     PIC X.
           02  FILLER REDEFINES MINBALF.
               03  MINBALA                 PIC X.
           02  MINBALI                     PIC X(015).
           02  MAXBALL                     COMP PIC S9(4).
           02  MAXBALF                     PIC X.
           02  FILLER REDEFINES MAXBALF.
               03  MAXBALA                 PIC X.
           02  MAXBALI                     PIC X(015).
           02  INCRMTL                     COMP PIC S9(4).
           02  INCRMTF                     PIC X.
           02  FILLER REDEFINES INCRMTF.
               03  INCRMTA                 PIC X.
           02  INCRMTI                     PIC X(011).
           02  MINXFRL                     COMP PIC S9(4).
           02  MINXFRF                     PIC X.
           02  FILLER REDEFINES MINXFRF.
               03  MINXFRA                 PIC X.
           02  MINXFRI                     PIC X(011).
           02  FREQL                       COMP PIC S9(4).
           02  FREQF                       PIC X.
           02  FILLER REDEFINES FREQF.
               03  FREQA                   PIC X.
           02  FREQI                       PIC X(001).
           02  PRTYL                       COMP PIC S9(4).
           02  PRTYF                       PIC X.
           02  FILLER REDEFINES PRTYF.
               03  PRTYA                   PIC X.
           02  PRTYI                       PIC X(002).
           02  EFFDTL                      COMP PIC S9(4).
           02  EFFDTF                      PIC X.
           02  FILLER REDEFINES EFFDTF.
               03  EFFDTA                  PIC X.
           02  EFFDTI                      PIC X(008).
           02  EXPDTL                      COMP PIC S9(4).
           02  EXPDTF                      PIC X.
           02  FILLER REDEFINES EXPDTF.
               03  EXPDTA                  PIC X.
           02  EXPDTI                      PIC X(008).
           02  LSTDTL                      COMP PIC S9(4).
           02  LSTDTF                      PIC X.
           02  FILLER REDEFINES LSTDTF.
               03  LSTDTA                  PIC X.
           02  LSTDTI                      PIC X(010).
           02  LSTAMTL                     COMP PIC S9(4).
           02  LSTAMTF                     PIC X.
           02  FILLER REDEFINES LSTAMTF.
               03  LSTAMTA                 PIC X.
           02  LSTAMTI                     PIC X(017).
           02  LSTDIRL                     COMP PIC S9(4).
           02  LSTDIRF                     PIC X.
           02  FILLER REDEFINES LSTDIRF.
               03  LSTDIRA                 PIC X.
           02  LSTDIRI                     PIC X(001).
           02  MNTUSRL                     COMP PIC S9(4).
           02  MNTUSRF                     PIC X.
           02  FILLER REDEFINES MNTUSRF.
               03  MNTUSRA                 PIC X.
           02  MNTUSRI                     PIC X(008).
           02  MNTDTL                      COMP PIC S9(4).
           02  MNTDTF                      PIC X.
           02  FILLER REDEFINES MNTDTF.
               03  MNTDTA                  PIC X.
           02  MNTDTI                      PIC X(010).
           02  MSGL                        COMP PIC S9(4).
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  MSGI                        PIC X(079).
       01  SWPMNTMO REDEFINES SWPMNTMI.
           02  FILLER                      PIC X(12).
           02  FILLER                      PIC X(03).
           02  TITLEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  CURDATEO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  ACTIONO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  SWPIDO                      PIC X(008).
           02  FILLER                      PIC X(03).
           02  SWPTYPO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  SWPSTSO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CUSTNMO                     PIC X(030).
           02  FILLER                      PIC X(03).
           02  SRCACTO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  SRCPRDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  TGTACTO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  TGTPRDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  TGTBALO                     PIC X(015).
           02  FILLER                      PIC X(03).
           02  MINBALO                     PIC X(015).
           02  FILLER                      PIC X(03).
           02  MAXBALO                     PIC X(015).
           02  FILLER                      PIC X(03).
           02  INCRMTO                     PIC X(011).
           02  FILLER                      PIC X(03).
           02  MINXFRO                     PIC X(011).
           02  FILLER                      PIC X(03).
           02  FREQO                       PIC X(001).
           02  FILLER                      PIC X(03).
           02  PRTYO                       PIC X(002).
           02  FILLER                      PIC X(03).
           02  EFFDTO                      PIC X(008).
           02  FILLER                      PIC X(03).
           02  EXPDTO                      PIC X(008).
           02  FILLER                      PIC X(03).
           02  LSTDTO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  LSTAMTO                     PIC X(017).
           02  FILLER                      PIC X(03).
           02  LSTDIRO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MNTUSRO                     PIC X(008).
           02  FILLER                      PIC X(03).
           02  MNTDTO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  MSGO                        PIC X(079).
