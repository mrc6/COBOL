//CREAVSAM JOB 1,NOTIFY=&SYSUID
//***************************************************/
//CRIAVSAM EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSTERM  DD SYSOUT=*
//SYSOUT   DD *
//SYSIN    DD *
    DEFINE CLUSTER (                                   -
          NAME(Z87588.VSAM.STCTRL.KSDS)                -
           INDEXED                                     -
           RECORDSIZE(170 170)                         -
           TRACKS(5 2)                                 -
           VOLUMES(*VSAM*)                             -
           KEYS(10 0)                                  -
           CISZ(4096)                                  -
           REUSE                                       -
           FREESPACE(3 3) )                            -
       DATA (                                          -
           NAME(Z87588.VSAM.STCTRL.KSDS.DATA))         -
       INDEX (                                         -
           NAME(Z87588.VSAM.STCTRL.KSDS.INDEX))
