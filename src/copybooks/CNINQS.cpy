      *================================================================*
      * COPYBOOK : CNINQS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET CNINQS MAP CNINQM         *
      *            TRANSACTION CN44 - COIN AND CURRENCY                *
      *            GENERATED FROM BMS SOURCE CNINQS - DO NOT EDIT      *
      *================================================================*
       01  CNINQMI.
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
           02  KEYL                        PIC S9(4) COMP.
           02  KEYF                        PIC X.
           02  FILLER REDEFINES KEYF.
               03  KEYA                    PIC X.
           02  FILLER                      PIC X(02).
           02  KEYI                        PIC X(012).
           02  EXEMPTL                     PIC S9(4) COMP.
           02  EXEMPTF                     PIC X.
           02  FILLER REDEFINES EXEMPTF.
               03  EXEMPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXEMPTI                     PIC X(001).
           02  VARIANL                     PIC S9(4) COMP.
           02  VARIANF                     PIC X.
           02  FILLER REDEFINES VARIANF.
               03  VARIANA                 PIC X.
           02  FILLER                      PIC X(02).
           02  VARIANI                     PIC X(016).
           02  EFFECTL                     PIC S9(4) COMP.
           02  EFFECTF                     PIC X.
           02  FILLER REDEFINES EFFECTF.
               03  EFFECTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EFFECTI                     PIC X(010).
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(007).
           02  ADDRL                       PIC S9(4) COMP.
           02  ADDRF                       PIC X.
           02  FILLER REDEFINES ADDRF.
               03  ADDRA                   PIC X.
           02  FILLER                      PIC X(02).
           02  ADDRI                       PIC X(030).
           02  STATUSL                     PIC S9(4) COMP.
           02  STATUSF                     PIC X.
           02  FILLER REDEFINES STATUSF.
               03  STATUSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STATUSI                     PIC X(003).
           02  COINAML                     PIC S9(4) COMP.
           02  COINAMF                     PIC X.
           02  FILLER REDEFINES COINAMF.
               03  COINAMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  COINAMI                     PIC X(016).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(001).
           02  NOTICE1L                    PIC S9(4) COMP.
           02  NOTICE1F                    PIC X.
           02  FILLER REDEFINES NOTICE1F.
               03  NOTICE1A                PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICE1I                    PIC X(007).
           02  COINYTL                     PIC S9(4) COMP.
           02  COINYTF                     PIC X.
           02  FILLER REDEFINES COINYTF.
               03  COINYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  COINYTI                     PIC X(016).
           02  CARRIEL                     PIC S9(4) COMP.
           02  CARRIEF                     PIC X.
           02  FILLER REDEFINES CARRIEF.
               03  CARRIEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CARRIEI                     PIC X(001).
           02  REASONL                     PIC S9(4) COMP.
           02  REASONF                     PIC X.
           02  FILLER REDEFINES REASONF.
               03  REASONA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REASONI                     PIC X(002).
           02  CUSTIDL                     PIC S9(4) COMP.
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CUSTIDI                     PIC X(010).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  CNINQMO REDEFINES CNINQMI.
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
           02  KEYC                        PIC X.
           02  KEYH                        PIC X.
           02  KEYO                        PIC X(012).
           02  FILLER                      PIC X(03).
           02  EXEMPTC                     PIC X.
           02  EXEMPTH                     PIC X.
           02  EXEMPTO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  VARIANC                     PIC X.
           02  VARIANH                     PIC X.
           02  VARIANO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  EFFECTC                     PIC X.
           02  EFFECTH                     PIC X.
           02  EFFECTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  ADDRC                       PIC X.
           02  ADDRH                       PIC X.
           02  ADDRO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  STATUSC                     PIC X.
           02  STATUSH                     PIC X.
           02  STATUSO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  COINAMC                     PIC X.
           02  COINAMH                     PIC X.
           02  COINAMO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  NOTICE1C                    PIC X.
           02  NOTICE1H                    PIC X.
           02  NOTICE1O                    PIC X(007).
           02  FILLER                      PIC X(03).
           02  COINYTC                     PIC X.
           02  COINYTH                     PIC X.
           02  COINYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CARRIEC                     PIC X.
           02  CARRIEH                     PIC X.
           02  CARRIEO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  REASONC                     PIC X.
           02  REASONH                     PIC X.
           02  REASONO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  CUSTIDC                     PIC X.
           02  CUSTIDH                     PIC X.
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
