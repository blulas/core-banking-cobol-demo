      *================================================================*
      * COPYBOOK : CRLSTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET CRLSTS MAP CRLSTM         *
      *            TRANSACTION CR91 - PROPRIETARY CREDIT CARD          *
      *            GENERATED FROM BMS SOURCE CRLSTS - DO NOT EDIT      *
      *================================================================*
       01  CRLSTMI.
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
           02  LATEFEL                     PIC S9(4) COMP.
           02  LATEFEF                     PIC X.
           02  FILLER REDEFINES LATEFEF.
               03  LATEFEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LATEFEI                     PIC X(016).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(010).
           02  BILLCYL                     PIC S9(4) COMP.
           02  BILLCYF                     PIC X.
           02  FILLER REDEFINES BILLCYF.
               03  BILLCYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  BILLCYI                     PIC X(001).
           02  FINANCL                     PIC S9(4) COMP.
           02  FINANCF                     PIC X.
           02  FILLER REDEFINES FINANCF.
               03  FINANCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FINANCI                     PIC X(016).
           02  NAMEL                       PIC S9(4) COMP.
           02  NAMEF                       PIC X.
           02  FILLER REDEFINES NAMEF.
               03  NAMEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  NAMEI                       PIC X(030).
           02  STOPFLL                     PIC S9(4) COMP.
           02  STOPFLF                     PIC X.
           02  FILLER REDEFINES STOPFLF.
               03  STOPFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STOPFLI                     PIC X(001).
           02  CONTACL                     PIC S9(4) COMP.
           02  CONTACF                     PIC X.
           02  FILLER REDEFINES CONTACF.
               03  CONTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONTACI                     PIC X(020).
           02  PENALTL                     PIC S9(4) COMP.
           02  PENALTF                     PIC X.
           02  FILLER REDEFINES PENALTF.
               03  PENALTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PENALTI                     PIC X(010).
           02  LATEFE1L                    PIC S9(4) COMP.
           02  LATEFE1F                    PIC X.
           02  FILLER REDEFINES LATEFE1F.
               03  LATEFE1A                PIC X.
           02  FILLER                      PIC X(02).
           02  LATEFE1I                    PIC X(016).
           02  TYPECDL                     PIC S9(4) COMP.
           02  TYPECDF                     PIC X.
           02  FILLER REDEFINES TYPECDF.
               03  TYPECDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TYPECDI                     PIC X(002).
           02  CARDTYL                     PIC S9(4) COMP.
           02  CARDTYF                     PIC X.
           02  FILLER REDEFINES CARDTYF.
               03  CARDTYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CARDTYI                     PIC X(003).
           02  AUTOREL                     PIC S9(4) COMP.
           02  AUTOREF                     PIC X.
           02  FILLER REDEFINES AUTOREF.
               03  AUTOREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  AUTOREI                     PIC X(001).
           02  RATEL                       PIC S9(4) COMP.
           02  RATEF                       PIC X.
           02  FILLER REDEFINES RATEF.
               03  RATEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  RATEI                       PIC X(010).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  CRLSTMO REDEFINES CRLSTMI.
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
           02  LATEFEC                     PIC X.
           02  LATEFEH                     PIC X.
           02  LATEFEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  BILLCYC                     PIC X.
           02  BILLCYH                     PIC X.
           02  BILLCYO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  FINANCC                     PIC X.
           02  FINANCH                     PIC X.
           02  FINANCO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NAMEC                       PIC X.
           02  NAMEH                       PIC X.
           02  NAMEO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  STOPFLC                     PIC X.
           02  STOPFLH                     PIC X.
           02  STOPFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  CONTACC                     PIC X.
           02  CONTACH                     PIC X.
           02  CONTACO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  PENALTC                     PIC X.
           02  PENALTH                     PIC X.
           02  PENALTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  LATEFE1C                    PIC X.
           02  LATEFE1H                    PIC X.
           02  LATEFE1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  TYPECDC                     PIC X.
           02  TYPECDH                     PIC X.
           02  TYPECDO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  CARDTYC                     PIC X.
           02  CARDTYH                     PIC X.
           02  CARDTYO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  AUTOREC                     PIC X.
           02  AUTOREH                     PIC X.
           02  AUTOREO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  RATEC                       PIC X.
           02  RATEH                       PIC X.
           02  RATEO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
