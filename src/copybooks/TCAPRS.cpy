      *================================================================*
      * COPYBOOK : TCAPRS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET TCAPRS MAP TCAPRM         *
      *            TRANSACTION TC65 - TRAVELERS CHEQUES                *
      *            GENERATED FROM BMS SOURCE TCAPRS - DO NOT EDIT      *
      *================================================================*
       01  TCAPRMI.
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
           02  COMMISL                     PIC S9(4) COMP.
           02  COMMISF                     PIC X.
           02  FILLER REDEFINES COMMISF.
               03  COMMISA                 PIC X.
           02  FILLER                      PIC X(02).
           02  COMMISI                     PIC X(016).
           02  CONTACL                     PIC S9(4) COMP.
           02  CONTACF                     PIC X.
           02  FILLER REDEFINES CONTACF.
               03  CONTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONTACI                     PIC X(020).
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(007).
           02  FACEYTL                     PIC S9(4) COMP.
           02  FACEYTF                     PIC X.
           02  FILLER REDEFINES FACEYTF.
               03  FACEYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FACEYTI                     PIC X(016).
           02  EXCEPTL                     PIC S9(4) COMP.
           02  EXCEPTF                     PIC X.
           02  FILLER REDEFINES EXCEPTF.
               03  EXCEPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPTI                     PIC X(007).
           02  NEXTREL                     PIC S9(4) COMP.
           02  NEXTREF                     PIC X.
           02  FILLER REDEFINES NEXTREF.
               03  NEXTREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTREI                     PIC X(010).
           02  LASTPAL                     PIC S9(4) COMP.
           02  LASTPAF                     PIC X.
           02  FILLER REDEFINES LASTPAF.
               03  LASTPAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTPAI                     PIC X(010).
           02  SUSPENL                     PIC S9(4) COMP.
           02  SUSPENF                     PIC X.
           02  FILLER REDEFINES SUSPENF.
               03  SUSPENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SUSPENI                     PIC X(001).
           02  REMITAL                     PIC S9(4) COMP.
           02  REMITAF                     PIC X.
           02  FILLER REDEFINES REMITAF.
               03  REMITAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REMITAI                     PIC X(016).
           02  ISSUERL                     PIC S9(4) COMP.
           02  ISSUERF                     PIC X.
           02  FILLER REDEFINES ISSUERF.
               03  ISSUERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ISSUERI                     PIC X(002).
           02  STOPFLL                     PIC S9(4) COMP.
           02  STOPFLF                     PIC X.
           02  FILLER REDEFINES STOPFLF.
               03  STOPFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STOPFLI                     PIC X(001).
           02  CONSIGL                     PIC S9(4) COMP.
           02  CONSIGF                     PIC X.
           02  FILLER REDEFINES CONSIGF.
               03  CONSIGA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONSIGI                     PIC X(016).
           02  REMITYL                     PIC S9(4) COMP.
           02  REMITYF                     PIC X.
           02  FILLER REDEFINES REMITYF.
               03  REMITYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REMITYI                     PIC X(016).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  TCAPRMO REDEFINES TCAPRMI.
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
           02  COMMISC                     PIC X.
           02  COMMISH                     PIC X.
           02  COMMISO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CONTACC                     PIC X.
           02  CONTACH                     PIC X.
           02  CONTACO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  FACEYTC                     PIC X.
           02  FACEYTH                     PIC X.
           02  FACEYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  EXCEPTC                     PIC X.
           02  EXCEPTH                     PIC X.
           02  EXCEPTO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  NEXTREC                     PIC X.
           02  NEXTREH                     PIC X.
           02  NEXTREO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  LASTPAC                     PIC X.
           02  LASTPAH                     PIC X.
           02  LASTPAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  SUSPENC                     PIC X.
           02  SUSPENH                     PIC X.
           02  SUSPENO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  REMITAC                     PIC X.
           02  REMITAH                     PIC X.
           02  REMITAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  ISSUERC                     PIC X.
           02  ISSUERH                     PIC X.
           02  ISSUERO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  STOPFLC                     PIC X.
           02  STOPFLH                     PIC X.
           02  STOPFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  CONSIGC                     PIC X.
           02  CONSIGH                     PIC X.
           02  CONSIGO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  REMITYC                     PIC X.
           02  REMITYH                     PIC X.
           02  REMITYO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
