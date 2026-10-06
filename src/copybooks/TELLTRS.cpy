      *================================================================*
      * COPYBOOK : TELLTRS                                             *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET TELLTRS MAP TELLTRM       *
      *            TRANSACTION CB11 - TELLER CASH TRANSACTION          *
      *            GENERATED FROM BMS SOURCE TELLTRS - DO NOT EDIT     *
      *================================================================*
       01  TELLTRMI.
           02  FILLER                      PIC X(12).
           02  TRANIDL                     PIC S9(4) COMP.
           02  TRANIDF                     PIC X.
           02  FILLER REDEFINES TRANIDF.
               03  TRANIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TRANIDI                     PIC X(004).
           02  TITLEL                      PIC S9(4) COMP.
           02  TITLEF                      PIC X.
           02  FILLER REDEFINES TITLEF.
               03  TITLEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  TITLEI                      PIC X(040).
           02  CURDATEL                    PIC S9(4) COMP.
           02  CURDATEF                    PIC X.
           02  FILLER REDEFINES CURDATEF.
               03  CURDATEA                PIC X.
           02  FILLER                      PIC X(02).
           02  CURDATEI                    PIC X(010).
           02  TLRIDL                      PIC S9(4) COMP.
           02  TLRIDF                      PIC X.
           02  FILLER REDEFINES TLRIDF.
               03  TLRIDA                  PIC X.
           02  FILLER                      PIC X(02).
           02  TLRIDI                      PIC X(008).
           02  DRWOUTL                     PIC S9(4) COMP.
           02  DRWOUTF                     PIC X.
           02  FILLER REDEFINES DRWOUTF.
               03  DRWOUTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DRWOUTI                     PIC X(014).
           02  DRWLIML                     PIC S9(4) COMP.
           02  DRWLIMF                     PIC X.
           02  FILLER REDEFINES DRWLIMF.
               03  DRWLIMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DRWLIMI                     PIC X(014).
           02  FUNCL                       PIC S9(4) COMP.
           02  FUNCF                       PIC X.
           02  FILLER REDEFINES FUNCF.
               03  FUNCA                   PIC X.
           02  FILLER                      PIC X(02).
           02  FUNCI                       PIC X(001).
           02  ACCTNOL                     PIC S9(4) COMP.
           02  ACCTNOF                     PIC X.
           02  FILLER REDEFINES ACCTNOF.
               03  ACCTNOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ACCTNOI                     PIC X(012).
           02  ACCTNML                     PIC S9(4) COMP.
           02  ACCTNMF                     PIC X.
           02  FILLER REDEFINES ACCTNMF.
               03  ACCTNMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ACCTNMI                     PIC X(030).
           02  ASTATL                      PIC S9(4) COMP.
           02  ASTATF                      PIC X.
           02  FILLER REDEFINES ASTATF.
               03  ASTATA                  PIC X.
           02  FILLER                      PIC X(02).
           02  ASTATI                      PIC X(010).
           02  AVAILL                      PIC S9(4) COMP.
           02  AVAILF                      PIC X.
           02  FILLER REDEFINES AVAILF.
               03  AVAILA                  PIC X.
           02  FILLER                      PIC X(02).
           02  AVAILI                      PIC X(015).
           02  CSHINL                      PIC S9(4) COMP.
           02  CSHINF                      PIC X.
           02  FILLER REDEFINES CSHINF.
               03  CSHINA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CSHINI                      PIC X(014).
           02  CSHOUTL                     PIC S9(4) COMP.
           02  CSHOUTF                     PIC X.
           02  FILLER REDEFINES CSHOUTF.
               03  CSHOUTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CSHOUTI                     PIC X(013).
           02  AMOUNTL                     PIC S9(4) COMP.
           02  AMOUNTF                     PIC X.
           02  FILLER REDEFINES AMOUNTF.
               03  AMOUNTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  AMOUNTI                     PIC X(013).
           02  IDVERL                      PIC S9(4) COMP.
           02  IDVERF                      PIC X.
           02  FILLER REDEFINES IDVERF.
               03  IDVERA                  PIC X.
           02  FILLER                      PIC X(02).
           02  IDVERI                      PIC X(001).
           02  CNDNML                      PIC S9(4) COMP.
           02  CNDNMF                      PIC X.
           02  FILLER REDEFINES CNDNMF.
               03  CNDNMA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CNDNMI                      PIC X(030).
           02  CNDDOBL                     PIC S9(4) COMP.
           02  CNDDOBF                     PIC X.
           02  FILLER REDEFINES CNDDOBF.
               03  CNDDOBA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CNDDOBI                     PIC X(008).
           02  CNDADRL                     PIC S9(4) COMP.
           02  CNDADRF                     PIC X.
           02  FILLER REDEFINES CNDADRF.
               03  CNDADRA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CNDADRI                     PIC X(040).
           02  CNDIDTL                     PIC S9(4) COMP.
           02  CNDIDTF                     PIC X.
           02  FILLER REDEFINES CNDIDTF.
               03  CNDIDTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CNDIDTI                     PIC X(002).
           02  CNDIDNL                     PIC S9(4) COMP.
           02  CNDIDNF                     PIC X.
           02  FILLER REDEFINES CNDIDNF.
               03  CNDIDNA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CNDIDNI                     PIC X(020).
           02  CNDIDSL                     PIC S9(4) COMP.
           02  CNDIDSF                     PIC X.
           02  FILLER REDEFINES CNDIDSF.
               03  CNDIDSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CNDIDSI                     PIC X(002).
           02  CNDOCCL                     PIC S9(4) COMP.
           02  CNDOCCF                     PIC X.
           02  FILLER REDEFINES CNDOCCF.
               03  CNDOCCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CNDOCCI                     PIC X(020).
           02  CNDOBOL                     PIC S9(4) COMP.
           02  CNDOBOF                     PIC X.
           02  FILLER REDEFINES CNDOBOF.
               03  CNDOBOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CNDOBOI                     PIC X(001).
           02  SUPIDL                      PIC S9(4) COMP.
           02  SUPIDF                      PIC X.
           02  FILLER REDEFINES SUPIDF.
               03  SUPIDA                  PIC X.
           02  FILLER                      PIC X(02).
           02  SUPIDI                      PIC X(008).
           02  SUPPWDL                     PIC S9(4) COMP.
           02  SUPPWDF                     PIC X.
           02  FILLER REDEFINES SUPPWDF.
               03  SUPPWDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SUPPWDI                     PIC X(008).
           02  HLDTX1L                     PIC S9(4) COMP.
           02  HLDTX1F                     PIC X.
           02  FILLER REDEFINES HLDTX1F.
               03  HLDTX1A                 PIC X.
           02  FILLER                      PIC X(02).
           02  HLDTX1I                     PIC X(078).
           02  HLDTX2L                     PIC S9(4) COMP.
           02  HLDTX2F                     PIC X.
           02  FILLER REDEFINES HLDTX2F.
               03  HLDTX2A                 PIC X.
           02  FILLER                      PIC X(02).
           02  HLDTX2I                     PIC X(078).
           02  TRNREFL                     PIC S9(4) COMP.
           02  TRNREFF                     PIC X.
           02  FILLER REDEFINES TRNREFF.
               03  TRNREFA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TRNREFI                     PIC X(020).
           02  NEWAVLL                     PIC S9(4) COMP.
           02  NEWAVLF                     PIC X.
           02  FILLER REDEFINES NEWAVLF.
               03  NEWAVLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEWAVLI                     PIC X(015).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  TELLTRMO REDEFINES TELLTRMI.
           02  FILLER                      PIC X(12).
           02  FILLER                      PIC X(03).
           02  TRANIDC                     PIC X.
           02  TRANIDH                     PIC X.
           02  TRANIDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  TITLEC                      PIC X.
           02  TITLEH                      PIC X.
           02  TITLEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  CURDATEC                    PIC X.
           02  CURDATEH                    PIC X.
           02  CURDATEO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  TLRIDC                      PIC X.
           02  TLRIDH                      PIC X.
           02  TLRIDO                      PIC X(008).
           02  FILLER                      PIC X(03).
           02  DRWOUTC                     PIC X.
           02  DRWOUTH                     PIC X.
           02  DRWOUTO                     PIC X(014).
           02  FILLER                      PIC X(03).
           02  DRWLIMC                     PIC X.
           02  DRWLIMH                     PIC X.
           02  DRWLIMO                     PIC X(014).
           02  FILLER                      PIC X(03).
           02  FUNCC                       PIC X.
           02  FUNCH                       PIC X.
           02  FUNCO                       PIC X(001).
           02  FILLER                      PIC X(03).
           02  ACCTNOC                     PIC X.
           02  ACCTNOH                     PIC X.
           02  ACCTNOO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  ACCTNMC                     PIC X.
           02  ACCTNMH                     PIC X.
           02  ACCTNMO                     PIC X(030).
           02  FILLER                      PIC X(03).
           02  ASTATC                      PIC X.
           02  ASTATH                      PIC X.
           02  ASTATO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  AVAILC                      PIC X.
           02  AVAILH                      PIC X.
           02  AVAILO                      PIC X(015).
           02  FILLER                      PIC X(03).
           02  CSHINC                      PIC X.
           02  CSHINH                      PIC X.
           02  CSHINO                      PIC X(014).
           02  FILLER                      PIC X(03).
           02  CSHOUTC                     PIC X.
           02  CSHOUTH                     PIC X.
           02  CSHOUTO                     PIC X(013).
           02  FILLER                      PIC X(03).
           02  AMOUNTC                     PIC X.
           02  AMOUNTH                     PIC X.
           02  AMOUNTO                     PIC X(013).
           02  FILLER                      PIC X(03).
           02  IDVERC                      PIC X.
           02  IDVERH                      PIC X.
           02  IDVERO                      PIC X(001).
           02  FILLER                      PIC X(03).
           02  CNDNMC                      PIC X.
           02  CNDNMH                      PIC X.
           02  CNDNMO                      PIC X(030).
           02  FILLER                      PIC X(03).
           02  CNDDOBC                     PIC X.
           02  CNDDOBH                     PIC X.
           02  CNDDOBO                     PIC X(008).
           02  FILLER                      PIC X(03).
           02  CNDADRC                     PIC X.
           02  CNDADRH                     PIC X.
           02  CNDADRO                     PIC X(040).
           02  FILLER                      PIC X(03).
           02  CNDIDTC                     PIC X.
           02  CNDIDTH                     PIC X.
           02  CNDIDTO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  CNDIDNC                     PIC X.
           02  CNDIDNH                     PIC X.
           02  CNDIDNO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  CNDIDSC                     PIC X.
           02  CNDIDSH                     PIC X.
           02  CNDIDSO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  CNDOCCC                     PIC X.
           02  CNDOCCH                     PIC X.
           02  CNDOCCO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  CNDOBOC                     PIC X.
           02  CNDOBOH                     PIC X.
           02  CNDOBOO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  SUPIDC                      PIC X.
           02  SUPIDH                      PIC X.
           02  SUPIDO                      PIC X(008).
           02  FILLER                      PIC X(03).
           02  SUPPWDC                     PIC X.
           02  SUPPWDH                     PIC X.
           02  SUPPWDO                     PIC X(008).
           02  FILLER                      PIC X(03).
           02  HLDTX1C                     PIC X.
           02  HLDTX1H                     PIC X.
           02  HLDTX1O                     PIC X(078).
           02  FILLER                      PIC X(03).
           02  HLDTX2C                     PIC X.
           02  HLDTX2H                     PIC X.
           02  HLDTX2O                     PIC X(078).
           02  FILLER                      PIC X(03).
           02  TRNREFC                     PIC X.
           02  TRNREFH                     PIC X.
           02  TRNREFO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  NEWAVLC                     PIC X.
           02  NEWAVLH                     PIC X.
           02  NEWAVLO                     PIC X(015).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
