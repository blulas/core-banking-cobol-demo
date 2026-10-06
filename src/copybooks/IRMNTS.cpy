      *================================================================*
      * COPYBOOK : IRMNTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET IRMNTS MAP IRMNTM         *
      *            TRANSACTION IR52 - IRA CUSTODY                      *
      *            GENERATED FROM BMS SOURCE IRMNTS - DO NOT EDIT      *
      *================================================================*
       01  IRMNTMI.
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
           02  FMVYTDL                     PIC S9(4) COMP.
           02  FMVYTDF                     PIC X.
           02  FILLER REDEFINES FMVYTDF.
               03  FMVYTDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FMVYTDI                     PIC X(016).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(010).
           02  CHANNEL                     PIC S9(4) COMP.
           02  CHANNEF                     PIC X.
           02  FILLER REDEFINES CHANNEF.
               03  CHANNEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CHANNEI                     PIC X(002).
           02  ITEMCNL                     PIC S9(4) COMP.
           02  ITEMCNF                     PIC X.
           02  FILLER REDEFINES ITEMCNF.
               03  ITEMCNA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ITEMCNI                     PIC X(007).
           02  WITHHOL                     PIC S9(4) COMP.
           02  WITHHOF                     PIC X.
           02  FILLER REDEFINES WITHHOF.
               03  WITHHOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  WITHHOI                     PIC X(016).
           02  OPENDAL                     PIC S9(4) COMP.
           02  OPENDAF                     PIC X.
           02  FILLER REDEFINES OPENDAF.
               03  OPENDAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OPENDAI                     PIC X(010).
           02  DISTRIL                     PIC S9(4) COMP.
           02  DISTRIF                     PIC X.
           02  FILLER REDEFINES DISTRIF.
               03  DISTRIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DISTRII                     PIC X(016).
           02  PAYMENL                     PIC S9(4) COMP.
           02  PAYMENF                     PIC X.
           02  FILLER REDEFINES PAYMENF.
               03  PAYMENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAYMENI                     PIC X(007).
           02  DISTRI1L                    PIC S9(4) COMP.
           02  DISTRI1F                    PIC X.
           02  FILLER REDEFINES DISTRI1F.
               03  DISTRI1A                PIC X.
           02  FILLER                      PIC X(02).
           02  DISTRI1I                    PIC X(016).
           02  EFFECTL                     PIC S9(4) COMP.
           02  EFFECTF                     PIC X.
           02  FILLER REDEFINES EFFECTF.
               03  EFFECTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EFFECTI                     PIC X(010).
           02  FMVAMTL                     PIC S9(4) COMP.
           02  FMVAMTF                     PIC X.
           02  FILLER REDEFINES FMVAMTF.
               03  FMVAMTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FMVAMTI                     PIC X(016).
           02  EXCESSL                     PIC S9(4) COMP.
           02  EXCESSF                     PIC X.
           02  FILLER REDEFINES EXCESSF.
               03  EXCESSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCESSI                     PIC X(016).
           02  REFNOL                      PIC S9(4) COMP.
           02  REFNOF                      PIC X.
           02  FILLER REDEFINES REFNOF.
               03  REFNOA                  PIC X.
           02  FILLER                      PIC X(02).
           02  REFNOI                      PIC X(016).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  IRMNTMO REDEFINES IRMNTMI.
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
           02  FMVYTDC                     PIC X.
           02  FMVYTDH                     PIC X.
           02  FMVYTDO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CHANNEC                     PIC X.
           02  CHANNEH                     PIC X.
           02  CHANNEO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  ITEMCNC                     PIC X.
           02  ITEMCNH                     PIC X.
           02  ITEMCNO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  WITHHOC                     PIC X.
           02  WITHHOH                     PIC X.
           02  WITHHOO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  OPENDAC                     PIC X.
           02  OPENDAH                     PIC X.
           02  OPENDAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  DISTRIC                     PIC X.
           02  DISTRIH                     PIC X.
           02  DISTRIO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  PAYMENC                     PIC X.
           02  PAYMENH                     PIC X.
           02  PAYMENO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  DISTRI1C                    PIC X.
           02  DISTRI1H                    PIC X.
           02  DISTRI1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  EFFECTC                     PIC X.
           02  EFFECTH                     PIC X.
           02  EFFECTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  FMVAMTC                     PIC X.
           02  FMVAMTH                     PIC X.
           02  FMVAMTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  EXCESSC                     PIC X.
           02  EXCESSH                     PIC X.
           02  EXCESSO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  REFNOC                      PIC X.
           02  REFNOH                      PIC X.
           02  REFNOO                      PIC X(016).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
