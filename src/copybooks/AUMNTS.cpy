      *================================================================*
      * COPYBOOK : AUMNTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET AUMNTS MAP AUMNTM         *
      *            TRANSACTION AU88 - DEALER AUTO FINANCE              *
      *            GENERATED FROM BMS SOURCE AUMNTS - DO NOT EDIT      *
      *================================================================*
       01  AUMNTMI.
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
           02  EXCEPTL                     PIC S9(4) COMP.
           02  EXCEPTF                     PIC X.
           02  FILLER REDEFINES EXCEPTF.
               03  EXCEPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPTI                     PIC X(007).
           02  PAYMENL                     PIC S9(4) COMP.
           02  PAYMENF                     PIC X.
           02  FILLER REDEFINES PAYMENF.
               03  PAYMENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAYMENI                     PIC X(016).
           02  RATEL                       PIC S9(4) COMP.
           02  RATEF                       PIC X.
           02  FILLER REDEFINES RATEF.
               03  RATEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  RATEI                       PIC X(010).
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(001).
           02  ADDRL                       PIC S9(4) COMP.
           02  ADDRF                       PIC X.
           02  FILLER REDEFINES ADDRF.
               03  ADDRA                   PIC X.
           02  FILLER                      PIC X(02).
           02  ADDRI                       PIC X(030).
           02  EXEMPTL                     PIC S9(4) COMP.
           02  EXEMPTF                     PIC X.
           02  FILLER REDEFINES EXEMPTF.
               03  EXEMPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXEMPTI                     PIC X(001).
           02  STOPFLL                     PIC S9(4) COMP.
           02  STOPFLF                     PIC X.
           02  FILLER REDEFINES STOPFLF.
               03  STOPFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STOPFLI                     PIC X(001).
           02  LASTPAL                     PIC S9(4) COMP.
           02  LASTPAF                     PIC X.
           02  FILLER REDEFINES LASTPAF.
               03  LASTPAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTPAI                     PIC X(010).
           02  CHARGEL                     PIC S9(4) COMP.
           02  CHARGEF                     PIC X.
           02  FILLER REDEFINES CHARGEF.
               03  CHARGEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CHARGEI                     PIC X(016).
           02  GAPAMTL                     PIC S9(4) COMP.
           02  GAPAMTF                     PIC X.
           02  FILLER REDEFINES GAPAMTF.
               03  GAPAMTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  GAPAMTI                     PIC X(016).
           02  PAYMEN1L                    PIC S9(4) COMP.
           02  PAYMEN1F                    PIC X.
           02  FILLER REDEFINES PAYMEN1F.
               03  PAYMEN1A                PIC X.
           02  FILLER                      PIC X(02).
           02  PAYMEN1I                    PIC X(016).
           02  OVERRIL                     PIC S9(4) COMP.
           02  OVERRIF                     PIC X.
           02  FILLER REDEFINES OVERRIF.
               03  OVERRIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OVERRII                     PIC X(001).
           02  STATUSL                     PIC S9(4) COMP.
           02  STATUSF                     PIC X.
           02  FILLER REDEFINES STATUSF.
               03  STATUSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STATUSI                     PIC X(003).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  AUMNTMO REDEFINES AUMNTMI.
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
           02  EXCEPTC                     PIC X.
           02  EXCEPTH                     PIC X.
           02  EXCEPTO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  PAYMENC                     PIC X.
           02  PAYMENH                     PIC X.
           02  PAYMENO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  RATEC                       PIC X.
           02  RATEH                       PIC X.
           02  RATEO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  ADDRC                       PIC X.
           02  ADDRH                       PIC X.
           02  ADDRO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  EXEMPTC                     PIC X.
           02  EXEMPTH                     PIC X.
           02  EXEMPTO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  STOPFLC                     PIC X.
           02  STOPFLH                     PIC X.
           02  STOPFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  LASTPAC                     PIC X.
           02  LASTPAH                     PIC X.
           02  LASTPAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CHARGEC                     PIC X.
           02  CHARGEH                     PIC X.
           02  CHARGEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  GAPAMTC                     PIC X.
           02  GAPAMTH                     PIC X.
           02  GAPAMTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  PAYMEN1C                    PIC X.
           02  PAYMEN1H                    PIC X.
           02  PAYMEN1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  OVERRIC                     PIC X.
           02  OVERRIH                     PIC X.
           02  OVERRIO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  STATUSC                     PIC X.
           02  STATUSH                     PIC X.
           02  STATUSO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
