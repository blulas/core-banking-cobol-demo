      *================================================================*
      * COPYBOOK : CRINQS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET CRINQS MAP CRINQM         *
      *            TRANSACTION CR68 - PROPRIETARY CREDIT CARD          *
      *            GENERATED FROM BMS SOURCE CRINQS - DO NOT EDIT      *
      *================================================================*
       01  CRINQMI.
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
           02  LASTACL                     PIC S9(4) COMP.
           02  LASTACF                     PIC X.
           02  FILLER REDEFINES LASTACF.
               03  LASTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTACI                     PIC X(010).
           02  EFFECTL                     PIC S9(4) COMP.
           02  EFFECTF                     PIC X.
           02  FILLER REDEFINES EFFECTF.
               03  EFFECTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EFFECTI                     PIC X(010).
           02  FINANCL                     PIC S9(4) COMP.
           02  FINANCF                     PIC X.
           02  FILLER REDEFINES FINANCF.
               03  FINANCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FINANCI                     PIC X(016).
           02  PREFERL                     PIC S9(4) COMP.
           02  PREFERF                     PIC X.
           02  FILLER REDEFINES PREFERF.
               03  PREFERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PREFERI                     PIC X(001).
           02  DISCOUL                     PIC S9(4) COMP.
           02  DISCOUF                     PIC X.
           02  FILLER REDEFINES DISCOUF.
               03  DISCOUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DISCOUI                     PIC X(010).
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(007).
           02  FINANC1L                    PIC S9(4) COMP.
           02  FINANC1F                    PIC X.
           02  FILLER REDEFINES FINANC1F.
               03  FINANC1A                PIC X.
           02  FILLER                      PIC X(02).
           02  FINANC1I                    PIC X(016).
           02  PURCHAL                     PIC S9(4) COMP.
           02  PURCHAF                     PIC X.
           02  FILLER REDEFINES PURCHAF.
               03  PURCHAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PURCHAI                     PIC X(016).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(001).
           02  NEXTREL                     PIC S9(4) COMP.
           02  NEXTREF                     PIC X.
           02  FILLER REDEFINES NEXTREF.
               03  NEXTREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTREI                     PIC X(010).
           02  RATEL                       PIC S9(4) COMP.
           02  RATEF                       PIC X.
           02  FILLER REDEFINES RATEF.
               03  RATEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  RATEI                       PIC X(010).
           02  BILLCYL                     PIC S9(4) COMP.
           02  BILLCYF                     PIC X.
           02  FILLER REDEFINES BILLCYF.
               03  BILLCYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  BILLCYI                     PIC X(001).
           02  AUTOREL                     PIC S9(4) COMP.
           02  AUTOREF                     PIC X.
           02  FILLER REDEFINES AUTOREF.
               03  AUTOREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  AUTOREI                     PIC X(001).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  CRINQMO REDEFINES CRINQMI.
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
           02  LASTACC                     PIC X.
           02  LASTACH                     PIC X.
           02  LASTACO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  EFFECTC                     PIC X.
           02  EFFECTH                     PIC X.
           02  EFFECTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  FINANCC                     PIC X.
           02  FINANCH                     PIC X.
           02  FINANCO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  PREFERC                     PIC X.
           02  PREFERH                     PIC X.
           02  PREFERO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  DISCOUC                     PIC X.
           02  DISCOUH                     PIC X.
           02  DISCOUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  FINANC1C                    PIC X.
           02  FINANC1H                    PIC X.
           02  FINANC1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  PURCHAC                     PIC X.
           02  PURCHAH                     PIC X.
           02  PURCHAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  NEXTREC                     PIC X.
           02  NEXTREH                     PIC X.
           02  NEXTREO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  RATEC                       PIC X.
           02  RATEH                       PIC X.
           02  RATEO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  BILLCYC                     PIC X.
           02  BILLCYH                     PIC X.
           02  BILLCYO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  AUTOREC                     PIC X.
           02  AUTOREH                     PIC X.
           02  AUTOREO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
