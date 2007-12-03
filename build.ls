Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  build.asm  Page 1



1                           COMMENT *
2      
3                          Compile this to build all files together.
4      
5                          Project:     SCUBA 2
6                          Author:      DAVID ATKINSON
7                          Target:      250MHz SDSU PCI card - DSP56301
8                          Controller:  For use with SCUBA 2 Multichannel Electronics
9      
10                         Modified:    MATTHEW HASSELFIELD
11     
12                         Assembler directives:
13                                 ROM=EEPROM => EEPROM CODE
14                                 ROM=ONCE => ONCE CODE
15     
16                                 *
17                                   PAGE    132                               ; Printronix page width - 132 columns
18                                   OPT     CEX                               ; print DC evaluations
19     
20                                   INCLUDE 'header.asm'
21                               COMMENT *
22     
23                         PCI code header file.
24     
25                         Project:     SCUBA 2
26                         Author:      DAVID ATKINSON
27                         Target:      250MHz SDSU PCI card - DSP56301
28                         Controller:  For use with SCUBA 2 Multichannel Electronics
29     
30                         Modified:    MATTHEW HASSELFIELD
31     
32                         Assembler directives:
33                                 ROM=0 => EEPROM CODE
34                                 ROM=1 => ROM CODE
35     
36                                 *
37                                   PAGE    132                               ; Printronix page width - 132 columns
38                                   OPT     CEX                               ; print DC evaluations
39     
**** 40 [header.asm 20]:  INCLUDE PCI_header.asm HERE  
40                                   MSG     ' INCLUDE PCI_header.asm HERE  '
41     
42                         ; Equates to define the X: memory tables
43        000000           VAR_TBL   EQU     0                                 ; Variables and constants table
44     
45                         APPLICATION
46        000800                     EQU     $800                              ; application memory start location in P memory
47                                                                             ; note applications should start with this address
48                                                                             ; and end with a JMP to PACKET_IN
49                                                                             ; if only want appl to run once
50                                                                             ; penultimate line of code should be
51                                                                             ; to clear bit APPLICATION_LOADED in STATUS
52                                                                             ; otherwise will run continusly until 'STP'
53                                                                             ; command is sent
54     
55        000200           APPL_PARAM EQU    $200                              ; application parameters in x memory start here.
56     
57     
58        000200           HF_FIFO   EQU     512                               ; number of 16 bit words in a half full FIFO
59        000020           SMALL_BLK EQU     32                                ; small block burst size for < 512 pixels
60                         IMAGE_BUFFER
61        000000                     EQU     0                                 ; location in y memory of image buffer....
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  header.asm  Page 2



62     
63     
64                         ;Status bits
65     
66                         APPLICATION_LOADED
67        000000                     EQU     0                                 ; set if PCI application to run
68                         SEND_TO_HOST
69        000001                     EQU     1                                 ; set in HST ISR when host ready for packet (stays se
t until after HST reply)
70                         FATAL_ERROR
71        000002                     EQU     2                                 ; PCI message to host error detected by driver....
72        000003           FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays set till p
acket processed
73     
74                         ;INTA_FLAG              EQU     4   ; used for interupt handshaking with host
75        000005           BYTE_SWAP EQU     5                                 ; flag to show byte swapping enabled
76                         PREAMBLE_ERROR
77        000006                     EQU     6                                 ; set if preamble error detected
78        000007           DATA_DLY  EQU     7                                 ; set in CON ISR if MCE command is 'GO'.  USed to add
 delay to first returned data packet
79     
80                         PACKET_CHOKE
81        000008                     EQU     8                                 ;  don't let any packets from MCE through to host....
82        000009           HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of packet (st
ays set until after HST reply)
83        00000A           SB_SPARE1 EQU     10
84        00000B           SB_SPARE2 EQU     11
85     
86     
87                         APPLICATION_RUNNING
88        00000C                     EQU     12                                ; can be set by an application to indicate its still 
running
89                                                                             ; e.g. set by diagnostic application
90                                                                             ; indicates in a 'self_test_mode'
91                                                                             ; subsequnet GO commands (for MCE) will be handelled 
internally.
92                                                                             ; disable with PCI STOP_APPLICATION command.
93     
94                         INTERNAL_GO
95        00000D                     EQU     13                                ; GO command received while diagnostic application st
ill running
96                                                                             ; tests DMA bursts as bus master
97     
99                         PCIDMA_RESTART
100       00000E                     EQU     14
101                        PCIDMA_RESUME
102       00000F                     EQU     15
103                        PCIDMA_RETRY
104       000010                     EQU     16
105    
106       000011           QT_ENABLED EQU    17
107       000012           QT_FLUSH  EQU     18
108                        QT_NEW_GRANT
109       000013                     EQU     19
110    
111                        ; HST timeout recovery....
112    
113       000200           MAX_DUMP  EQU     512                               ; if HST timeout.. max number that could be in FIFO i
s 511..
114       001000           DUMP_BUFF EQU     $1000                             ; store in Y memory above normal data buffer: in off-
chip RAM
115    
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  header.asm  Page 3



116    
117    
118                        ; Various addressing control registers
119       FFFFFB           BCR       EQU     $FFFFFB                           ; Bus Control Register
120       FFFFFA           DCR       EQU     $FFFFFA                           ; DRAM Control Register
121       FFFFF9           AAR0      EQU     $FFFFF9                           ; Address Attribute Register, channel 0
122       FFFFF8           AAR1      EQU     $FFFFF8                           ; Address Attribute Register, channel 1
123       FFFFF7           AAR2      EQU     $FFFFF7                           ; Address Attribute Register, channel 2
124       FFFFF6           AAR3      EQU     $FFFFF6                           ; Address Attribute Register, channel 3
125       FFFFFD           PCTL      EQU     $FFFFFD                           ; PLL control register
126       FFFFFE           IPRP      EQU     $FFFFFE                           ; Interrupt Priority register - Peripheral
127       FFFFFF           IPRC      EQU     $FFFFFF                           ; Interrupt Priority register - Core
128    
129                        ; PCI control register
130       FFFFCD           DTXS      EQU     $FFFFCD                           ; DSP Slave transmit data FIFO
131       FFFFCC           DTXM      EQU     $FFFFCC                           ; DSP Master transmit data FIFO
132       FFFFCB           DRXR      EQU     $FFFFCB                           ; DSP Receive data FIFO
133       FFFFCA           DPSR      EQU     $FFFFCA                           ; DSP PCI Status Register
134       FFFFC9           DSR       EQU     $FFFFC9                           ; DSP Status Register
135       FFFFC8           DPAR      EQU     $FFFFC8                           ; DSP PCI Address Register
136       FFFFC7           DPMC      EQU     $FFFFC7                           ; DSP PCI Master Control Register
137       FFFFC6           DPCR      EQU     $FFFFC6                           ; DSP PCI Control Register
138       FFFFC5           DCTR      EQU     $FFFFC5                           ; DSP Control Register
139    
140                        ; Port E is the Synchronous Communications Interface (SCI) port
141       FFFF9F           PCRE      EQU     $FFFF9F                           ; Port Control Register
142       FFFF9E           PRRE      EQU     $FFFF9E                           ; Port Direction Register
143       FFFF9D           PDRE      EQU     $FFFF9D                           ; Port Data Register
144    
145                        ; Various PCI register bit equates
146       000001           STRQ      EQU     1                                 ; Slave transmit data request (DSR)
147       000002           SRRQ      EQU     2                                 ; Slave receive data request (DSR)
148       000017           HACT      EQU     23                                ; Host active, low true (DSR)
149       000001           MTRQ      EQU     1                                 ; Set whem master transmitter is not full (DPSR)
150       000004           MARQ      EQU     4                                 ; Master address request (DPSR)
151       000002           MRRQ      EQU     2                                 ; Master Receive Request (DPSR)
152       00000A           TRTY      EQU     10                                ; PCI Target Retry (DPSR)
153    
154       000005           APER      EQU     5                                 ; Address parity error
155       000006           DPER      EQU     6                                 ; Data parity error
156       000007           MAB       EQU     7                                 ; Master Abort
157       000008           TAB       EQU     8                                 ; Target Abort
158       000009           TDIS      EQU     9                                 ; Target Disconnect
159       00000B           TO        EQU     11                                ; Timeout
160       00000E           MDT       EQU     14                                ; Master Data Transfer complete
161       000002           SCLK      EQU     2                                 ; SCLK = transmitter special code
162    
163                        ; bits in DPMC
164    
165       000017           FC1       EQU     23
166       000016           FC0       EQU     22
167    
168    
169                        ; DMA register definitions
170       FFFFEF           DSR0      EQU     $FFFFEF                           ; Source address register
171       FFFFEE           DDR0      EQU     $FFFFEE                           ; Destination address register
172       FFFFED           DCO0      EQU     $FFFFED                           ; Counter register
173       FFFFEC           DCR0      EQU     $FFFFEC                           ; Control register
174    
175                        ; The DCTR host flags are written by the DSP and read by PCI host
176       000003           DCTR_HF3  EQU     3                                 ; used as a semiphore for INTA handshaking
177       000004           DCTR_HF4  EQU     4                                 ;
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  header.asm  Page 4



178       000005           DCTR_HF5  EQU     5                                 ;
179       000006           INTA      EQU     6                                 ; Request PCI interrupt
180    
181                        ; The DSR host flags are written by the PCI host and read by the DSP
182       000004           DSR_BUF0  EQU     4                                 ; PCI host sets this when copying buffer 0
183       000005           DSR_BUF1  EQU     5                                 ; PCI host sets this when copying buffer 1
184    
185                        ; DPCR bit definitions
186       00000E           CLRT      EQU     14                                ; Clear transmitter
187       000012           MACE      EQU     18                                ; Master access counter enable
188       000015           IAE       EQU     21                                ; Insert Address Enable
189    
190                        ; Addresses of ESSI port
191       FFFFBC           TX00      EQU     $FFFFBC                           ; Transmit Data Register 0
192       FFFFB7           SSISR0    EQU     $FFFFB7                           ; Status Register
193       FFFFB6           CRB0      EQU     $FFFFB6                           ; Control Register B
194       FFFFB5           CRA0      EQU     $FFFFB5                           ; Control Register A
195    
196                        ; SSI Control Register A Bit Flags
197       000006           TDE       EQU     6                                 ; Set when transmitter data register is empty
198    
199                        ; Miscellaneous addresses
200       FFFFFF           RDFIFO    EQU     $FFFFFF                           ; Read the FIFO for incoming fiber optic data
201    
202                        ; Timer registers
203       FFFF8F           TCSR0     EQU     $FFFF8F                           ; Control and status register
204       FFFF8E           TLR0      EQU     $FFFF8E                           ; Load register
205       FFFF8D           TCPR0     EQU     $FFFF8D                           ; Compare register
206       FFFF8C           TCR0      EQU     $FFFF8C                           ; Count register
207       FFFF8B           TCSR1     EQU     $FFFF8B                           ; Control and status register
208       FFFF8A           TLR1      EQU     $FFFF8A                           ; Load register
209       FFFF89           TCPR1     EQU     $FFFF89                           ; Compare register
210       FFFF88           TCR1      EQU     $FFFF88                           ; Count register
211       FFFF87           TCSR2     EQU     $FFFF87                           ; Control and status register
212       FFFF86           TLR2      EQU     $FFFF86                           ; Load register
213       FFFF85           TCPR2     EQU     $FFFF85                           ; Compare register
214       FFFF84           TCR2      EQU     $FFFF84                           ; Count register
215    
216                        ;***************************************************************
217                        ; Phase Locked Loop initialization
218       050003           PLL_INIT  EQU     $050003                           ; PLL = 25 MHz x 4 = 100 MHz
219                        ;****************************************************************
220    
221                        ; Port C is Enhanced Synchronous Serial Port 0
222       FFFFBF           PCRC      EQU     $FFFFBF                           ; Port C Control Register
223       FFFFBE           PRRC      EQU     $FFFFBE                           ; Port C Data direction Register
224       FFFFBD           PDRC      EQU     $FFFFBD                           ; Port C GPIO Data Register
225    
226                        ; Port D is Enhanced Synchronous Serial Port 1
227       FFFFAF           PCRD      EQU     $FFFFAF                           ; Port D Control Register
228       FFFFAE           PRRD      EQU     $FFFFAE                           ; Port D Data direction Register
229       FFFFAD           PDRD      EQU     $FFFFAD                           ; Port D GPIO Data Register
230    
231                        ; Bit number definitions of GPIO pins on Port C
232       000002           ROM_FIFO  EQU     2                                 ; Select ROM or FIFO accesses for AA1
233       000004           AUX1      EQU     4                                 ; enable/disable byte swapping
234    
235                        ; Bit number definitions of GPIO pins on Port D
236       000000           EF        EQU     0                                 ; FIFO Empty flag, low true
237       000001           HF        EQU     1                                 ; FIFO half full flag, low true
238       000002           RS        EQU     2                                 ; FIFO reset signal, low true
239       000003           FSYNC     EQU     3                                 ; High during image transmission
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  header.asm  Page 5



240       000005           WRFIFO    EQU     5                                 ; Low true if FIFO is being written to
241    
242    
243                        ; Errors - self test application
244    
245       000000           Y_MEM_ER  EQU     0                                 ; y memory corrupted
246       000001           X_MEM_ER  EQU     1                                 ; x memory corrupted
247       000002           P_MEM_ER  EQU     2                                 ; p memory corrupted
248       000003           FO_EMPTY  EQU     3                                 ; no transmitted data in FIFO
249    
250       000004           FO_OVER   EQU     4                                 ; too much data received
251       000005           FO_UNDER  EQU     5                                 ; not enough data receiv
252       000006           FO_RX_ER  EQU     6                                 ; received data in FIFO incorrect.
253       000007           DEBUG     EQU     7                                 ; debug bit
254    
255    
257       000000           TE        EQU     0
258       000001           TOIE      EQU     1
259       000002           TCIE      EQU     2
260       000014           TOF       EQU     20
261       000015           TCF       EQU     21
262                                  INCLUDE 'init.asm'
263                              COMMENT *
264    
265                        This is the code which is executed first after power-up etc.
266                        It sets all the internal registers to their operating values,
267                        sets up the ISR vectors and inialises the hardware etc.
268    
269                        Project:     SCUBA 2
270                        Author:      DAVID ATKINSON
271                        Target:      250MHz SDSU PCI card - DSP56301
272                        Controller:  For use with SCUBA 2 Multichannel Electronics
273    
274                        Assembler directives:
275                                ROM=EEPROM => EEPROM CODE
276                                ROM=ONCE => ONCE CODE
277    
278                                *
279                                  PAGE    132                               ; Printronix page width - 132 columns
280                                  OPT     CEX                               ; print DC evaluations
281    
**** 282 [init.asm 20]:  INCLUDE PCI_initialisation.asm HERE  
282                                  MSG     ' INCLUDE PCI_initialisation.asm HERE  '
283    
284                        ; The EEPROM boot code expects first to read 3 bytes specifying the number of
285                        ; program words, then 3 bytes specifying the address to start loading the
286                        ; program words and then 3 bytes for each program word to be loaded.
287                        ; The program words will be condensed into 24 bit words and stored in contiguous
288                        ; PRAM memory starting at the specified starting address. Program execution
289                        ; starts from the same address where loading started.
290    
291                        ; Special address for two words for the DSP to bootstrap code from the EEPROM
292                                  IF      @SCP("ROM","ROM")                 ; Boot from ROM on power-on
293       P:000000 P:000000                   ORG     P:0,P:0
294  d    P:000000 P:000000 000810            DC      END_ADR-INIT-2                    ; Number of boot words
295  d    P:000001 P:000001 000000            DC      INIT                              ; Starting address
296       P:000000 P:000002                   ORG     P:0,P:2
297       P:000000 P:000002 0C0030  INIT      JMP     <INIT_PCI                         ; Configure PCI port
298       P:000001 P:000003 000000            NOP
299                                           ENDIF
300    
301    
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  init.asm  Page 6



302                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
303                                 ; command converter
304                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
308                                           ENDIF
309    
310                                 ; Vectored interrupt table, addresses at the beginning are reserved
311  d    P:000002 P:000004 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0       ; $02-$0f Reserved
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
312  d    P:000010 P:000012 000000            DC      0,0                               ; $10-$13 Reserved
     d                      000000
313    
314                                 ; FIFO HF* flag interrupt vector is here at $12 - this is connected to the
315                                 ; IRQB* interrupt line so its ISR vector must be here
316  d    P:000012 P:000014 000000            DC      0,0                               ; $was ld scatter routine ...HF*
     d                      000000
317    
318                                 ; a software reset button on the font panel of the card is connected to the IRQC*
319                                 ; line which if pressed causes the DSP to jump to an ISR which causes the program
320                                 ; counter to the beginning of the program INIT and sets the stack pointer to TOP.
321       P:000014 P:000016 0BF080            JSR     CLEAN_UP_PCI                      ; $14 - Software reset switch
                            0003C4
322    
323  d    P:000016 P:000018 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Reserved interrupts
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
324  d    P:000022 P:000024 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
325    
326                                 ; Now we're at P:$30, where some unused vector addresses are located
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  init.asm  Page 7



327                                 ; This is ROM only code that is only executed once on power-up when the
328                                 ; ROM code is downloaded. It is skipped over on OnCE downloads.
329    
330                                 ; A few seconds after power up on the Host, it interrogates the PCI bus to find
331                                 ; out what boards are installed and configures this PCI board. The EEPROM booting
332                                 ; procedure ends with program execution  starting at P:$0 where the EEPROM has
333                                 ; inserted a JMP INIT_PCI instruction. This routine sets the PLL paramter and
334                                 ; does a self configuration and software reset of the PCI controller in the DSP.
335                                 ; After configuring the PCI controller the DSP program overwrites the instruction
336                                 ; at P:$0 with a new JMP START to skip over the INIT_PCI routine. The program at
337                                 ; START address begins configuring the DSP and processing commands.
338                                 ; Similarly the ONCE option places a JMP START at P:$0 to skip over the
339                                 ; INIT_PCI routine. If this routine where executed after the host computer had booted
340                                 ; it would cause it to crash since the host computer would overwrite the
341                                 ; configuration space with its own values and doesn't tolerate foreign values.
342    
343                                 ; Initialize the PLL - phase locked loop
344                                 INIT_PCI
345       P:000030 P:000032 08F4BD            MOVEP             #PLL_INIT,X:PCTL        ; Initialize PLL
                            050003
346       P:000032 P:000034 000000            NOP
347    
348                                 ; Program the PCI self-configuration registers
349       P:000033 P:000035 240000            MOVE              #0,X0
350       P:000034 P:000036 08F485            MOVEP             #$500000,X:DCTR         ; Set self-configuration mode
                            500000
351       P:000036 P:000038 0604A0            REP     #4
352       P:000037 P:000039 08C408            MOVEP             X0,X:DPAR               ; Dummy writes to configuration space
353       P:000038 P:00003A 08F487            MOVEP             #>$0000,X:DPMC          ; Subsystem ID
                            000000
354       P:00003A P:00003C 08F488            MOVEP             #>$0000,X:DPAR          ; Subsystem Vendor ID
                            000000
355    
356                                 ; PCI Personal reset
357       P:00003C P:00003E 08C405            MOVEP             X0,X:DCTR               ; Personal software reset
358       P:00003D P:00003F 000000            NOP
359       P:00003E P:000040 000000            NOP
360       P:00003F P:000041 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00003F
361       P:000041 P:000043 07F084            MOVE              P:(*+3),X0              ; Trick to write "JMP <START" to P:0
                            000044
362       P:000043 P:000045 070004            MOVE              X0,P:(0)
363       P:000044 P:000046 0C0100            JMP     <START
364    
365  d    P:000045 P:000047 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
366  d    P:000051 P:000053 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  init.asm  Page 8



     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
367  d    P:00005D P:00005F 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; $60-$71 Reserved PCI
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
368    
369                                 ;**************************************************************************
370                                 ; Check for program space overwriting of ISR starting at P:$72
371                                           IF      @CVS(N,*)>$71
373                                           ENDIF
374    
375                                 ;       ORG     P:$72,P:$72
376       P:000072 P:000074                   ORG     P:$72,P:$74
377    
378                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
379                                 ; command converter
380                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
382                                           ENDIF
383    
384    
385                                 ;**************************************************************************
386    
387                                 ; Three non-maskable fast interrupt service routines for clearing PCI interrupts
388                                 ; The Host will use these to clear the INTA* after it has serviced the interrupt
389                                 ; which had been generated by the PCI board.
390    
391       P:000072 P:000074 0A8506            BCLR    #INTA,X:DCTR                      ; $72/3 - Clear PCI interrupt
392       P:000073 P:000075 000000            NOP
393    
394       P:000074 P:000076 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; clear interrupt flag
395       P:000075 P:000077 000000            NOP                                       ; needs to be fast addressing <
396    
397       P:000076 P:000078 0A0022            BSET    #FATAL_ERROR,X:<STATUS            ; $76/7 - driver PCI_MESSAGE_TO_HOST error
398       P:000077 P:000079 000000            NOP
399    
400                                 ; Interrupt locations for 7 available commands on PCI board
401                                 ; Each JSR takes up 2 locations in the table
402       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            0002FC
403       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            0002D1
404       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            00031D
405       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            000326
406                                 ; software reset is the same as cleaning up the PCI - use same routine
407                                 ; when HOST does a RESET then this routine is run
408       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            000427
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  init.asm  Page 9



409       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            0003D0
410       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            00041A
411       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            000331
412    
413                                 ; QT - set command
414       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            00034F
415       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            0003BA
416    
417                                 ; ***********************************************************************
418                                 ; For now have boot code starting from P:$100
419                                 ; just to make debugging tidier etc.
420    
421       P:000100 P:000102                   ORG     P:$100,P:$102
422    
423                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
424                                 ; command converter
425                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
427                                           ENDIF
428                                 ; ***********************************************************************
429    
430    
431    
432                                 ; ******************************************************************
433                                 ;
434                                 ;       AA0 = RDFIFO* of incoming fiber optic data
435                                 ;       AA1 = EEPROM access
436                                 ;       AA2 = DRAM access
437                                 ;       AA3 = output to parallel data connector, for a video pixel clock
438                                 ;       $FFxxxx = Write to fiber optic transmitter
439                                 ;
440                                 ; ******************************************************************
441    
442    
443       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
444       P:000102 P:000104 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
445       P:000103 P:000105 0A8515            BCLR    #21,X:DCTR
446       P:000104 P:000106 0A8516            BCLR    #22,X:DCTR
447       P:000105 P:000107 000000            NOP
448       P:000106 P:000108 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
449       P:000107 P:000109 000000            NOP
450       P:000108 P:00010A 000000            NOP                                       ; End of PCI programming
451    
452    
453                                 ; Set operation mode register OMR to normal expanded
454       P:000109 P:00010B 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
455       P:00010A P:00010C 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
456    
457                                 ; Program the serial port ESSI0 = Port C for serial transmission to
458                                 ;   the timing board
459       P:00010B P:00010D 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
460                                 ;**********************************************************************
461       P:00010D P:00010F 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
462                                                                                     ; DC0-CD4 = 0 for non-network operation
463                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  init.asm  Page 10



464                                                                                     ; SSC1 = 0 for SC1 not used
465                                 ;************************************************************************
466       P:00010F P:000111 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
                            010120
467                                                                                     ; SHFD = 0 for MSB shifted first
468                                                                                     ; CKP = 0 for rising clock edge transitions
469                                                                                     ; TE0 = 1 to enable transmitter #0
470                                                                                     ; MOD = 0 for normal, non-networked mode
471                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
472       P:000111 P:000113 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
473                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
474                                 ;********************************************************************************
475       P:000113 P:000115 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
476       P:000115 P:000117 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
                            000000
477                                 ;***********************************************************************************
478                                 ; 250MHz
479                                 ; Conversion from software bits to schematic labels for Port C and D
480                                 ;       PC0 = SC00 = AUX3               PD0 = SC10 = EF*
481                                 ;       PC1 = SC01 = A/B* = input       PD1 = SC11 = HF*
482                                 ;       PC2 = SC02 = No connect         PD2 = SC12 = RS*
483                                 ;       PC3 = SCK0 = No connect         PD3 = SCK1 = NWRFIFO*
484                                 ;       PC4 = SRD0 = AUX1               PD4 = SRD1 = No connect (** in 50Mhz this was MODE selec
t for 16 or 32 bit FO)
485                                 ;       PC5 = STD0 = No connect         PD5 = STD1 = WRFIFO*
486                                 ; ***********************************************************************************
487    
488    
489                                 ; ****************************************************************************
490                                 ; Program the serial port ESSI1 = Port D for general purpose I/O (GPIO)
491    
492       P:000117 P:000119 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
493       P:000119 P:00011B 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            00001C
494       P:00011B P:00011D 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
495       P:00011D P:00011F 060AA0            REP     #10
496       P:00011E P:000120 000000            NOP
497       P:00011F P:000121 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
498                                                                                     ; was %011100
499    
500                                 ; Program the SCI port to benign values
501       P:000121 P:000123 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
502       P:000123 P:000125 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
503       P:000125 P:000127 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
504                                 ;       PE0 = RXD
505                                 ;       PE1 = TXD
506                                 ;       PE2 = SCLK
507    
508                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
509       P:000127 P:000129 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
510       P:000129 P:00012B 07F40B            MOVEP             #$2800,X:TCSR1
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  init.asm  Page 11



                            002800
511       P:00012B P:00012D 07F407            MOVEP             #$2800,X:TCSR2
                            002800
512    
513    
514                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
515       P:00012D P:00012F 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
516       P:00012F P:000131 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
e
                            008929
517       P:000131 P:000133 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
                            000122
518    
519    
520                                 ; Program the DRAM memory access and addressing
521       P:000133 P:000135 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
                            020022
522       P:000135 P:000137 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
523    
524    
525                                 ; Clear all PCI error conditions
526       P:000137 P:000139 084E0A            MOVEP             X:DPSR,A
527       P:000138 P:00013A 0140C2            OR      #$1FE,A
                            0001FE
528       P:00013A P:00013C 000000            NOP
529       P:00013B P:00013D 08CE0A            MOVEP             A,X:DPSR
530    
531                                 ;--------------------------------------------------------------------
532                                 ; Enable one interrupt only: software reset switch
533       P:00013C P:00013E 08F4BF            MOVEP             #$0001C0,X:IPRC         ; IRQB priority = 1 (FIFO half full HF*)
                            0001C0
534                                                                                     ; IRQC priority = 2 (reset switch)
535       P:00013E P:000140 05F439            MOVE              #$200,SR                ; Mask set up for reset switch only
                            000200
536    
537    
538                                 ;--------------------------------------------------------------------------
539                                 ; Initialize the fiber optic serial transmitter to zero
540       P:000140 P:000142 01B786            JCLR    #TDE,X:SSISR0,*
                            000140
541       P:000142 P:000144 07F43C            MOVEP             #$000000,X:TX00
                            000000
542    
543                                 ;--------------------------------------------------------------------
544    
545                                 ; clear DTXM - PCI master transmitter
546       P:000144 P:000146 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
547       P:000145 P:000147 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000145
548    
549                                 ;----------------------------------------------------------------------
550                                 ; clear DRXR - PCI receiver
551    
552       P:000147 P:000149 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014C
553       P:000149 P:00014B 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
554       P:00014A P:00014C 000000            NOP
555       P:00014B P:00014D 0C0147            JMP     <CLR0
556                                 CLR1
557    
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  init.asm  Page 12



558                                 ;-----------------------------------------------------------------------------
559                                 ; copy parameter table from P memory into X memory
560    
561                                 ; but not word_count and num_dumped - don't want these reset by fatal error....
562                                 ; they will be reset by new packet or pci_reset ISR
563    
564    
565       P:00014C P:00014E 46F000            MOVE              X:WORD_COUNT,Y0         ; store packet word count
                            000006
566       P:00014E P:000150 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000007
567       P:000150 P:000152 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
                            000001
568    
569                                 ; Move the table of constants from P: space to X: space
570       P:000152 P:000154 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            000652
571       P:000154 P:000156 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
572       P:000155 P:000157 066780            DO      #VAR_TBL_LENGTH,X_WRITE
                            000158
573       P:000157 P:000159 07D984            MOVE              P:(R1)+,X0
574       P:000158 P:00015A 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
575                                 X_WRITE
576    
577    
578       P:000159 P:00015B 467000            MOVE              Y0,X:WORD_COUNT         ; restore packet word count
                            000006
579       P:00015B P:00015D 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
                            000007
580       P:00015D P:00015F 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000001
581    
582                                 ;-------------------------------------------------------------------------------
583                                 ; initialise some bits in STATUS
584    
585       P:00015F P:000161 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS     ; clear application loaded flag
586       P:000160 P:000162 0A000C            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appliaction running flag
587                                                                                     ; (e.g. not running diagnostic application
588                                                                                     ;      in self_test_mode)
589    
590       P:000161 P:000163 0A0002            BCLR    #FATAL_ERROR,X:<STATUS            ; initialise fatal error flag.
591       P:000162 P:000164 0A0028            BSET    #PACKET_CHOKE,X:<STATUS           ; enable MCE packet choke
592                                                                                     ; HOST not informed of anything from MCE unt
il
593                                                                                     ; comms are opened by host with first CON co
mmand
594    
595       P:000163 P:000165 0A0006            BCLR    #PREAMBLE_ERROR,X:<STATUS         ; flag to let host know premable error
596    
597                                 ;------------------------------------------------------------------------------
598                                 ; disable FIFO HF* intererupt...not used anymore.
599    
600       P:000164 P:000166 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable FIFO HF* interrupt
                            0001C0
601       P:000166 P:000168 05F439            MOVEC             #$200,SR                ; Mask level 1 interrupts
                            000200
602    
603                                 ;----------------------------------------------------------------------------
604                                 ; Disable Byte swapin - enabled after first command to MCE.
605                                 ; i.e after first 'CON'
606    
607       P:000168 P:00016A 0A0005            BCLR    #BYTE_SWAP,X:<STATUS              ; flag to let host know byte swapping off
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  init.asm  Page 13



608       P:000169 P:00016B 013D04            BCLR    #AUX1,X:PDRC                      ; enable disable
609    
611       P:00016A P:00016C 0BF080            JSR     CLEAR_FIFO
                            000517
612    
613                                 ;----------------------------------------------------------------------------
614                                 ; Initialize PCI controller again, after booting, to make sure it sticks
615       P:00016C P:00016E 0A8514            BCLR    #20,X:DCTR                        ; Terminate and reset mode
616       P:00016D P:00016F 000000            NOP
617       P:00016E P:000170 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00016E
618       P:000170 P:000172 000000            NOP
619       P:000171 P:000173 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
620       P:000172 P:000174 000000            NOP
621       P:000173 P:000175 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000173
622                                 ;-----------------------------------------------------------------------------
623                                 ; Here endth the initialisation code run after power up.
624                                 ; ----------------------------------------------------------------------------
625                                           INCLUDE 'main.asm'
626                                         COMMENT *
627    
628                                 This is the main section of the pci card code.
629    
630                                 Project:     SCUBA 2
631                                 Author:      DAVID ATKINSON
632                                 Target:      250MHz SDSU PCI card - DSP56301
633                                 Controller:  For use with SCUBA 2 Multichannel Electronics
634    
635                                 Modified:    MATTHEW HASSELFIELD
636    
637                                 Version:     Release Version U (1.4)
638    
639    
640                                 Assembler directives:
641                                         ROM=EEPROM => EEPROM CODE
642                                         ROM=ONCE => ONCE CODE
643    
644                                         *
645                                           PAGE    132                               ; Printronix page width - 132 columns
646                                           OPT     CEX                               ; print DC evaluations
647    
**** 648 [main.asm 23]:  INCLUDE PCI_main.asm HERE  
648                                           MSG     ' INCLUDE PCI_main.asm HERE  '
649    
650                                 ; --------------------------------------------------------------------------
651                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
652                                 ; --------------------------------------------------------------------------
653    
654                                 ; initialse buffer pointers
655                                 PACKET_IN
656    
657                                 ; R1 used as pointer for data written to y:memory            FO --> (Y)
658                                 ; R2 used as pointer for date in y mem to be writen to host  (Y) --> HOST
659    
660       P:000175 P:000177 310000            MOVE              #<IMAGE_BUFFER,R1       ; pointer for Fibre ---> Y mem
661       P:000176 P:000178 320000            MOVE              #<IMAGE_BUFFER,R2       ; pointer for Y mem ---> PCI BUS
662    
663                                 ; initialise some bits in status..
664       P:000177 P:000179 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
665       P:000178 P:00017A 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 14



666       P:000179 P:00017B 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS             ; clear Fiber Optic flag
667    
668    
670       P:00017A P:00017C 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START      ; fatal error?  Go to initialisation.
                            000100
671       P:00017C P:00017E 0A00A0            JSET    #APPLICATION_LOADED,X:<STATUS,APPLICATION ; application loaded?  Execute in ap
pl space.
                            000800
672    
674       P:00017E P:000180 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            0005F1
675       P:000180 P:000182 0B00B2            JSSET   #QT_FLUSH,X:STATUS,BUFFER_INFORM
                            00062A
676       P:000182 P:000184 000000            NOP                                       ; For expansion!
677       P:000183 P:000185 000000            NOP
678    
679                                 CHK_FIFO
680       P:000184 P:000186 0D0477            JSR     <GET_FO_WRD                       ; check for 16-bit word in fibre FIFO from M
CE
681       P:000185 P:000187 0A0083            JCLR    #FO_WRD_RCV,X:STATUS,PACKET_IN    ; loop
                            000175
682    
683    
685    
686                                 CHECK_WD
687       P:000187 P:000189 0A00A8            JSET    #PACKET_CHOKE,X:<STATUS,PACKET_IN ; IF MCE Packet choke on - just keep clearin
g FIFO.
                            000175
688       P:000189 P:00018B 441D00            MOVE              X0,X:<HEAD_W1_0         ;store received word
689       P:00018A P:00018C 56F000            MOVE              X:PREAMB1,A
                            000038
690       P:00018C P:00018E 200045            CMP     X0,A                              ; check it is correct
691       P:00018D P:00018F 0E21A1            JNE     <PRE_ERROR                        ; if not go to start
692    
693       P:00018E P:000190 0D047F            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
694       P:00018F P:000191 441C00            MOVE              X0,X:<HEAD_W1_1         ;store received word
695       P:000190 P:000192 56F000            MOVE              X:PREAMB1,A
                            000038
696       P:000192 P:000194 200045            CMP     X0,A                              ; check it is correct
697       P:000193 P:000195 0E21A1            JNE     <PRE_ERROR                        ; if not go to start
698    
699       P:000194 P:000196 0D047F            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
700       P:000195 P:000197 441F00            MOVE              X0,X:<HEAD_W2_0         ;store received word
701       P:000196 P:000198 56F000            MOVE              X:PREAMB2,A
                            000039
702       P:000198 P:00019A 200045            CMP     X0,A                              ; check it is correct
703       P:000199 P:00019B 0E21A1            JNE     <PRE_ERROR                        ; if not go to start
704    
705       P:00019A P:00019C 0D047F            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
706       P:00019B P:00019D 441E00            MOVE              X0,X:<HEAD_W2_1         ;store received word
707       P:00019C P:00019E 56F000            MOVE              X:PREAMB2,A
                            000039
708       P:00019E P:0001A0 200045            CMP     X0,A                              ; check it is correct
709       P:00019F P:0001A1 0E21A1            JNE     <PRE_ERROR                        ; if not go to start
710       P:0001A0 P:0001A2 0C01A6            JMP     <PACKET_INFO                      ; get packet info
711    
712                                 PRE_ERROR
713       P:0001A1 P:0001A3 0A0026            BSET    #PREAMBLE_ERROR,X:<STATUS         ; indicate a preamble error
714       P:0001A2 P:0001A4 440200            MOVE              X0,X:<PRE_CORRUPT       ; store corrupted word
715       P:0001A3 P:0001A5 0BF080            JSR     CLEAR_FIFO                        ; empty the fifo (2 ms!)
                            000517
716       P:0001A5 P:0001A7 0C0175            JMP     <PACKET_IN                        ; back to main loop
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 15



717    
718                                 PACKET_INFO                                         ; packet preamble valid
719    
720       P:0001A6 P:0001A8 0D047F            JSR     <WT_FIFO
721       P:0001A7 P:0001A9 442100            MOVE              X0,X:<HEAD_W3_0         ; RP or DA
722       P:0001A8 P:0001AA 0D047F            JSR     <WT_FIFO
723       P:0001A9 P:0001AB 442000            MOVE              X0,X:<HEAD_W3_1         ; $2020
724    
725       P:0001AA P:0001AC 0D047F            JSR     <WT_FIFO
726       P:0001AB P:0001AD 442300            MOVE              X0,X:<HEAD_W4_0         ; packet size lo
727       P:0001AC P:0001AE 0D047F            JSR     <WT_FIFO
728       P:0001AD P:0001AF 442200            MOVE              X0,X:<HEAD_W4_1         ; packet size hi
729    
731       P:0001AE P:0001B0 56F000            MOVE              X:HEAD_W3_0,A
                            000021
732    
733       P:0001B0 P:0001B2 0140C5            CMP     #>'RP',A
                            005250
734       P:0001B2 P:0001B4 0AF0AA            JEQ     BUFFER_PACKET_RP
                            0001BA
735    
736       P:0001B4 P:0001B6 0140C5            CMP     #>'DA',A
                            004441
737       P:0001B6 P:0001B8 0AF0AA            JEQ     BUFFER_PACKET_DA
                            0001BC
738    
739       P:0001B8 P:0001BA 0AF080            JMP     QT_PTYPE_ERROR
                            000284
740    
741    
742                                 BUFFER_PACKET_RP
743    
744       P:0001BA P:0001BC 0AF080            JMP     MCE_PACKET                        ; Process in the usual way
                            0001D1
745    
746                                 BUFFER_PACKET_DA
747    
748       
749       P:0001BC P:0001BE 0B0087            JSCLR   #DATA_DLY,X:STATUS,PACKET_DELAY
                            0001C8
750    
751       
752       P:0001BE P:0001C0 56F000            MOVE              X:FRAME_COUNT,A
                            000001
753       P:0001C0 P:0001C2 0140C0            ADD     #>1,A
                            000001
754       P:0001C2 P:0001C4 000000            NOP
755       P:0001C3 P:0001C5 560100            MOVE              A,X:<FRAME_COUNT
756    
757       
758       P:0001C4 P:0001C6 0A0091            JCLR    #QT_ENABLED,X:STATUS,MCE_PACKET
                            0001D1
759    
760       P:0001C6 P:0001C8 0AF080            JMP     QUIET_TRANSFER_NOW
                            000234
761    
762    
763                                 PACKET_DELAY
764       P:0001C8 P:0001CA 44F000            MOVE              X:DATA_DLY_VAL,X0
                            000040
765       P:0001CA P:0001CC 06C400            DO      X0,*+3                            ; 10ns x DATA_DLY_VAL
                            0001CC
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 16



766       P:0001CC P:0001CE 000000            NOP
767       P:0001CD P:0001CF 000000            NOP
768       P:0001CE P:0001D0 0A7007            BCLR    #DATA_DLY,X:STATUS                ; clear so delay isn't added next time.
                            000000
769       P:0001D0 P:0001D2 00000C            RTS
770    
771                                 ; -------------------------------------------------------------------------------------------
772                                 ; ----------------------------------- IT'S A PACKET FROM MCE --------------------------------
773                                 ; -------------------------------------------------------------------------------------------
774                                 ; prepare notify to inform host that a packet has arrived.
775    
776                                 MCE_PACKET
777       P:0001D1 P:0001D3 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
778       P:0001D3 P:0001D5 440C00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
779    
780       P:0001D4 P:0001D6 44A100            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
781       P:0001D5 P:0001D7 440D00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
782    
783       P:0001D6 P:0001D8 44A300            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
784       P:0001D7 P:0001D9 440E00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
785    
786       P:0001D8 P:0001DA 44A200            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
787       P:0001D9 P:0001DB 440F00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sasge
788    
789       P:0001DA P:0001DC 200013            CLR     A                                 ;
790       P:0001DB P:0001DD 340000            MOVE              #0,R4                   ; initialise word count
791       P:0001DC P:0001DE 560600            MOVE              A,X:<WORD_COUNT         ; initialise word count store (num of words 
written over bus/packet)
792       P:0001DD P:0001DF 560700            MOVE              A,X:<NUM_DUMPED         ; initialise number dumped from FIFO (after 
HST TO)
793    
794    
795                                 ; ----------------------------------------------------------------------------------------------
------------
796                                 ; Determine how to break up packet to write to host
797    
798                                 ; Note that this SR uses accumulator B
799                                 ; Therefore execute before we get the bus address from host (which is stored in B)
800                                 ; i.e before we issue notify message ('NFY')
801    
802       P:0001DE P:0001E0 0D0448            JSR     <CALC_NO_BUFFS                    ; subroutine which calculates the number of 
512 (16bit) buffers
803                                                                                     ; number of left over 32 (16bit) blocks
804                                                                                     ; and number of left overs (16bit) words
805    
806                                 ;  note that a 512 (16-bit) buffer is transfered to the host as 4 x 64 x 32bit DMA burst
807                                 ;            a 32  (16-bit) block is transfered to the host as a    16 x 32bit DMA burst
808                                 ;            left over 16bit words are transfered to the host in pairs as 32bit words
809                                 ; ----------------------------------------------------------------------------------------------
---
810    
811    
812                                 ; notify the host that there is a packet.....
813    
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 17



814       P:0001DF P:0001E1 0D048F            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
815       P:0001E0 P:0001E2 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
816    
817                                 ; initialise read/write buffers
818                                 ; AND IMMEDIATELY BEGIN TO BUFFER FIBRE DATA TO Y MEMORY.
819    
820       P:0001E1 P:0001E3 310000            MOVE              #<IMAGE_BUFFER,R1       ; FO ---> Y mem
821       P:0001E2 P:0001E4 320000            MOVE              #<IMAGE_BUFFER,R2       ; Y mem ----->  PCI BUS
822    
823    
824                                 ; ----------------------------------------------------------------------------------------------
-----------
825                                 ; Write TOTAL_BUFFS * 512 buffers to host
826                                 ; ----------------------------------------------------------------------------------------------
------
827       P:0001E3 P:0001E5 063C00            DO      X:<TOTAL_BUFFS,READ_BUFFS_END     ; note that if TOTAL_BUFFS = 0 we jump to AL
L_BUFFS_END
                            0001F0
828    
829       P:0001E5 P:0001E7 0A00A2  WAIT_BUFF JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO  ; if fatal error then dump fifo and reset (i
.e. if HST timeout)
                            000220
830       P:0001E7 P:0001E9 01ADA1            JSET    #HF,X:PDRD,WAIT_BUFF              ; Wait for FIFO to be half full + 1
                            0001E5
831       P:0001E9 P:0001EB 000000            NOP
832       P:0001EA P:0001EC 000000            NOP
833       P:0001EB P:0001ED 01ADA1            JSET    #HF,X:PDRD,WAIT_BUFF              ; Protection against metastability
                            0001E5
834    
835                                 ; Copy the image block as 512 x 16bit words to DSP Y: Memory using R1 as pointer
836       P:0001ED P:0001EF 060082            DO      #512,L_BUFFER
                            0001EF
837       P:0001EF P:0001F1 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
838                                 L_BUFFER
839       P:0001F0 P:0001F2 000000            NOP
840                                 READ_BUFFS_END                                      ; all buffers have been read (-->Y)
841    
842                                 ; ----------------------------------------------------------------------------------------------
-----------
843                                 ; Read NUM_LEFTOVER_BLOCKS * 32 blocks
844                                 ; ----------------------------------------------------------------------------------------------
------
845    
846                                 ; less than 512 pixels but if greater than 32 will then do bursts
847                                 ; of 16 x 32bit in length, if less than 32 then does single read writes
848    
849       P:0001F1 P:0001F3 063F00            DO      X:<NUM_LEFTOVER_BLOCKS,READ_BLOCKS ;note that if NUM_LEFOVERS_BLOCKS = 0 we ju
mp to LEFTOVER_BLOCKS
                            0001FE
850    
851       P:0001F3 P:0001F5 062080            DO      #32,S_BUFFER
                            0001FD
852       P:0001F5 P:0001F7 0A00A2  WAIT_1    JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO  ; check for fatal error (i.e. after HST time
out)
                            000220
853       P:0001F7 P:0001F9 01AD80            JCLR    #EF,X:PDRD,WAIT_1                 ; Wait for the pixel datum to be there
                            0001F5
854       P:0001F9 P:0001FB 000000            NOP                                       ; Settling time
855       P:0001FA P:0001FC 000000            NOP
856       P:0001FB P:0001FD 01AD80            JCLR    #EF,X:PDRD,WAIT_1                 ; Protection against metastability
                            0001F5
857       P:0001FD P:0001FF 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; save fibre word
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 18



858                                 S_BUFFER
859       P:0001FE P:000200 000000            NOP
860                                 READ_BLOCKS
861    
862                                 ; ----------------------------------------------------------------------------------------------
-------
863                                 ; Single write left over words to host
864                                 ; ----------------------------------------------------------------------------------------------
------
865    
866                                 LEFT_OVERS
867       P:0001FF P:000201 063D00            DO      X:<LEFT_TO_READ,LEFT_OVERS_READ   ; read in remaining words of data packet
                            000209
868                                                                                     ; if LEFT_TO_READ = 0 then will jump to LEFT
_OVERS_READ
869    
870       P:000201 P:000203 0A00A2  WAIT_2    JSET    #FATAL_ERROR,X:<STATUS,START      ; check for fatal error (i.e. after HST time
out)
                            000100
871       P:000203 P:000205 01AD80            JCLR    #EF,X:PDRD,WAIT_2                 ; Wait till something in FIFO flagged
                            000201
872       P:000205 P:000207 000000            NOP
873       P:000206 P:000208 000000            NOP
874       P:000207 P:000209 01AD80            JCLR    #EF,X:PDRD,WAIT_2                 ; protect against metastability.....
                            000201
875       P:000209 P:00020B 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; save fibre word
876                                 LEFT_OVERS_READ
877    
878                                 ;---------------------------------------------------------------------------------------
879                                 ; ENTIRE PACKET NOW IN Y MEMORY
880                                 ;----------------------------------------------------------------------------------------
881                                 ; CHECK THAT HST COMMAND WAS ISSUED DURING DATA COLLECTION...
882    
883    
884       P:00020A P:00020C 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; if fatal error - run initialisation code..
.
                            000100
885       P:00020C P:00020E 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; wait for host to reply - which it does wit
h 'send_packet_to_host' ISR
                            00020A
886    
888    
889       P:00020E P:000210 0BF080            JSR     PCI_BURST_NOW
                            000505
890       P:000210 P:000212 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
891    
892                                 ; ----------------------------------------------------------------------------------------------
------------
893                                 ; reply to host's send_packet_to_host command
894    
895                                  HST_ACK_REP
896       P:000212 P:000214 44F400            MOVE              #'REP',X0
                            524550
897       P:000214 P:000216 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
898       P:000215 P:000217 44F400            MOVE              #'HST',X0
                            485354
899       P:000217 P:000219 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
900       P:000218 P:00021A 44F400            MOVE              #'ACK',X0
                            41434B
901       P:00021A P:00021C 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
902       P:00021B P:00021D 44F400            MOVE              #'000',X0
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 19



                            303030
903       P:00021D P:00021F 440F00            MOVE              X0,X:<DTXS_WD4          ; no error
904       P:00021E P:000220 0D048F            JSR     <PCI_MESSAGE_TO_HOST
905       P:00021F P:000221 0C0175            JMP     <PACKET_IN
906    
907                                 ;-----------------------------------------------------------------------------------------------
----
908                                 ; clear out the fifo after an HST timeout...
909                                 ;----------------------------------------------------------
910    
911       P:000220 P:000222 61F400  DUMP_FIFO MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
912       P:000222 P:000224 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
913       P:000224 P:000226 200013            CLR     A
914       P:000225 P:000227 320000            MOVE              #0,R2                   ; use R2 as a dump count
915    
916       P:000226 P:000228 01AD80  NEXT_DUMP JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000231
917       P:000228 P:00022A 000000            NOP
918       P:000229 P:00022B 000000            NOP
919       P:00022A P:00022C 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000231
920    
921       P:00022C P:00022E 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
922       P:00022D P:00022F 205A00            MOVE              (R2)+                   ; inc dump count
923       P:00022E P:000230 224E00            MOVE              R2,A                    ;
924       P:00022F P:000231 200045            CMP     X0,A                              ; check we've not hit dump limit
925       P:000230 P:000232 0E2226            JNE     NEXT_DUMP                         ; not hit limit?
926    
927    
928       P:000231 P:000233 627000  FIFO_EMPTY MOVE             R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000007
929       P:000233 P:000235 0C0100            JMP     <START                            ; re-initialise
930    
931    
932    
933                                 ; ----------------------------------------------------------------------------------------------
--
934                                 ;                              END OF MAIN PACKET HANDLING CODE
935                                 ; ---------------------------------------------------------------------------------------------
936    
937    
938                                 ;----------------------------------------------;
939                                 ;  ALTERNATIVE PACKET HANDLING CODE            ;
940                                 ;----------------------------------------------;
941    
942    
943                                 QUIET_TRANSFER_NOW
944       
945    
946                                 ;       ;; This won't work until you re-order HEAD * 0,1
947                                 ;       MOVE    #HEAD_W4_1,R2           ; Note W4_0 is *after* W4_1 in memory
948                                 ;       JSR     LOAD_HILO_ADDRESS       ; Packet size, in word32
949                                 ;       ASL     #2,A,A                  ; Convert to bytes
950                                 ;       ADD     #0,B                    ; Clear carry
951                                 ;       ASL     #14,A,B                 ; B1 = size in bytes / 2^10
952                                 ;       MOVE    #0,X0
953                                 ;       INSERT  #$0E000A,X0,A           ; A0 = size in word32 % 2^10
954    
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 20



955       
956       P:000234 P:000236 200013            CLR     A
957       P:000235 P:000237 50F000            MOVE              X:HEAD_W4_0,A0
                            000023
958       P:000237 P:000239 44F000            MOVE              X:HEAD_W4_1,X0
                            000022
959       P:000239 P:00023B 0C1940            INSERT  #$010010,X0,A
                            010010
960    
961       P:00023B P:00023D 000000            NOP
962       P:00023C P:00023E 507000            MOVE              A0,X:TEMP_PSIZE
                            00005C
963    
964       P:00023E P:000240 014088            ADD     #0,B                              ; Clear carry
965       P:00023F P:000241 0C1D02            ASL     #1,A,A                            ;  * 2
966       P:000240 P:000242 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
967       P:000241 P:000243 240000            MOVE              #0,X0
968       P:000242 P:000244 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
969    
970       P:000244 P:000246 557000            MOVE              B1,X:TOTAL_BUFFS
                            00003C
971       P:000246 P:000248 507000            MOVE              A0,X:LEFT_TO_READ
                            00003D
972    
973                                 BUFFER_PACKET_HALFS
974       P:000248 P:00024A 310000            MOVE              #IMAGE_BUFFER,R1
975       P:000249 P:00024B 063C00            DO      X:TOTAL_BUFFS,BUFFER_PACKET_SINGLES
                            00024E
976       P:00024B P:00024D 0BF080            JSR     WAIT_FIFO_HALF
                            00026D
977       P:00024D P:00024F 0BF080            JSR     TRANSFER_FIFO_HALF
                            00027F
978    
979                                 BUFFER_PACKET_SINGLES
980       P:00024F P:000251 063D00            DO      X:LEFT_TO_READ,BUFFER_PACKET_SEND
                            000254
981       P:000251 P:000253 0BF080            JSR     WAIT_FIFO_SINGLE
                            000276
982       P:000253 P:000255 0BF080            JSR     TRANSFER_FIFO_SINGLE
                            000281
983    
984                                 BUFFER_PACKET_SEND
985       P:000255 P:000257 56F000            MOVE              X:QT_BUF_HEAD,A
                            000055
986       P:000257 P:000259 014180            ADD     #1,A
987       P:000258 P:00025A 57F000            MOVE              X:QT_BUF_MAX,B
                            000052
988       P:00025A P:00025C 20000D            CMP     A,B
989       P:00025B P:00025D 0AF0A1            JGE     BUFFER_PACKET_MATH
                            00025E
990       P:00025D P:00025F 2E0000            MOVE              #0,A
991                                 BUFFER_PACKET_MATH
992       P:00025E P:000260 57F000            MOVE              X:QT_BUF_TAIL,B
                            000056
993       P:000260 P:000262 20000D            CMP     A,B
994       P:000261 P:000263 0AF0AA            JEQ     BUFFER_PACKET_DROP                ; If yes, drop packet
                            000266
995    
996       P:000263 P:000265 0BF080            JSR     QT_DATA_PACKET                    ; and transfer
                            000285
997    
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 21



998                                 BUFFER_PACKET_DONE
999       P:000265 P:000267 0C0175            JMP     PACKET_IN
1000   
1001                                BUFFER_PACKET_DROP
1002      P:000266 P:000268 56F000            MOVE              X:QT_DROPS,A
                            00005A
1003      P:000268 P:00026A 014180            ADD     #1,A
1004      P:000269 P:00026B 000000            NOP
1005      P:00026A P:00026C 567000            MOVE              A,X:QT_DROPS
                            00005A
1006   
1007      P:00026C P:00026E 0C0265            JMP     BUFFER_PACKET_DONE
1008   
1009                                WAIT_FIFO_HALF
1010      P:00026D P:00026F 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            000283
1011      P:00026F P:000271 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
                            00026D
1012      P:000271 P:000273 000000            NOP
1013      P:000272 P:000274 000000            NOP
1014      P:000273 P:000275 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            00026D
1015      P:000275 P:000277 00000C            RTS
1016   
1017                                WAIT_FIFO_SINGLE
1018      P:000276 P:000278 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            000220
1019      P:000278 P:00027A 01AD80            JCLR    #EF,X:PDRD,WAIT_FIFO_SINGLE
                            000276
1020      P:00027A P:00027C 000000            NOP
1021      P:00027B P:00027D 000000            NOP
1022      P:00027C P:00027E 01AD80            JCLR    #EF,X:PDRD,WAIT_FIFO_SINGLE       ; Protect against metastability
                            000276
1023      P:00027E P:000280 00000C            RTS
1024   
1025                                TRANSFER_FIFO_HALF
1026      
1027      P:00027F P:000281 060082            DO      #512,TRANSFER_FIFO_DONE
                            000281
1028                                TRANSFER_FIFO_SINGLE
1029      P:000281 P:000283 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
1030                                TRANSFER_FIFO_DONE
1031      P:000282 P:000284 00000C            RTS
1032   
1033                                FATALITY_HANDLER
1034      P:000283 P:000285 0C0100            JMP     START                             ; What could possibly go wrong?
1035   
1036   
1037                                QT_PTYPE_ERROR
1038                                QT_FSIZE_ERROR
1039      
1040   
1041      P:000284 P:000286 00000C            RTS
1042   
1043   
1044                                QT_DATA_PACKET
1045      
1046      
1047   
1048      
1049                                ;       MOVE    #HEAD_W4_1,R2           ; Note W4_0 is *after* W4_1 in memory
1050                                ;       JSR     LOAD_HILO_ADDRESS       ; Size, in word32
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 22



1051      P:000285 P:000287 200013            CLR     A
1052      P:000286 P:000288 50F000            MOVE              X:TEMP_PSIZE,A0
                            00005C
1053   
1054      P:000288 P:00028A 014088            ADD     #0,B                              ; Clear carry
1055      P:000289 P:00028B 0C1D04            ASL     #2,A,A                            ; Size, in bytes
1056   
1057      
1058      P:00028A P:00028C 20001B            CLR     B
1059      P:00028B P:00028D 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            000053
1060      P:00028D P:00028F 20000D            CMP     A,B
1061      P:00028E P:000290 0E2284            JNE     QT_FSIZE_ERROR
1062   
1063      
1064      P:00028F P:000291 517000            MOVE              B0,X:BLOCK_SIZE
                            000042
1065      P:000291 P:000293 557000            MOVE              B1,X:BURST_SRC          ; Y:0
                            000046
1066   
1067      P:000293 P:000295 62F400            MOVE              #QT_DEST_LO,R2
                            000057
1068      P:000295 P:000297 0BF080            JSR     LOAD_HILO_ADDRESS
                            000641
1069      P:000297 P:000299 62F400            MOVE              #BURST_DEST_LO,R2
                            000044
1070      P:000299 P:00029B 0BF080            JSR     SAVE_HILO_ADDRESS
                            000649
1071   
1072      
1073      P:00029B P:00029D 0BF080            JSR     BLOCK_TRANSFER
                            00057B
1074   
1075      
1076      P:00029D P:00029F 0BF080            JSR     BUFFER_INCR
                            0005FF
1077   
1078      
1079      P:00029F P:0002A1 0BF080            JSR     BUFFER_INFORM_CHECK
                            00061D
1080   
1081   
1082      P:0002A1 P:0002A3 00000C            RTS
1083   
1084   
1085                                ; -------------------------------------------------------------------------------------
1086                                ;
1087                                ;                              INTERRUPT SERVICE ROUTINES
1088                                ;
1089                                ; ---------------------------------------------------------------------------------------
1090   
1091   
1093   
1094   
1095                                VCOM_PREPARE_REPLY
1096      
1097      
1098      P:0002A2 P:0002A4 50F400            MOVE              #'REP',A0
                            524550
1099      P:0002A4 P:0002A6 447000            MOVE              X0,X:DTXS_WD2           ; Command
                            00000D
1100      P:0002A6 P:0002A8 507000            MOVE              A0,X:DTXS_WD1
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 23



                            00000C
1101   
1102      P:0002A8 P:0002AA 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
                            41434B
1103      P:0002AA P:0002AC 000000            NOP
1104      P:0002AB P:0002AD 547000            MOVE              A1,X:DTXS_WD3           ; ACK
                            00000E
1105      P:0002AD P:0002AF 507000            MOVE              A0,X:DTXS_WD4           ; no comment
                            00000F
1106      P:0002AF P:0002B1 00000C            RTS
1107   
1108   
1109                                VCOM_CHECK
1110      
1111      
1112      
1113   
1114      P:0002B0 P:0002B2 208E00            MOVE              X0,A
1115      P:0002B1 P:0002B3 57F000            MOVE              X:DRXR_WD1,B
                            000008
1116      P:0002B3 P:0002B5 20000D            CMP     A,B
1117      P:0002B4 P:0002B6 0AF0AA            JEQ     VCOM_RTS
                            0002BE
1118   
1119      P:0002B6 P:0002B8 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
1120      P:0002B8 P:0002BA 50F400            MOVE              #'ERR',A0
                            455252
1121      P:0002BA P:0002BC 447000            MOVE              X0,X:DTXS_WD4
                            00000F
1122      P:0002BC P:0002BE 507000            MOVE              A0,X:DTXS_WD3
                            00000E
1123                                VCOM_RTS
1124      P:0002BE P:0002C0 00000C            RTS
1125   
1126   
1127                                VCOM_INTRO
1128      
1129      P:0002BF P:0002C1 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            0004AA
1130      P:0002C1 P:0002C3 20A400            MOVE              X1,X0
1131      P:0002C2 P:0002C4 0D02A2            JSR     VCOM_PREPARE_REPLY
1132      P:0002C3 P:0002C5 0D02B0            JSR     VCOM_CHECK
1133      P:0002C4 P:0002C6 00000C            RTS
1134   
1135                                VCOM_EXIT_ERROR_X0
1136      P:0002C5 P:0002C7 50F400            MOVE              #'ERR',A0
                            455252
1137      P:0002C7 P:0002C9 000000            NOP
1138      P:0002C8 P:0002CA 507000            MOVE              A0,X:DTXS_WD3
                            00000E
1139                                VCOM_EXIT_X0
1140      P:0002CA P:0002CC 447000            MOVE              X0,X:DTXS_WD4
                            00000F
1141                                VCOM_EXIT
1142      P:0002CC P:0002CE 0BF080            JSR     RESTORE_REGISTERS
                            0004D7
1143      P:0002CE P:0002D0 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00048F
1144      P:0002D0 P:0002D2 000004            RTI
1145   
1146   
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 24



1147   
1148   
1149                                ; ----------------------------------------------------------------------------
1150                                READ_MEMORY
1151                                ;-----------------------------------------------------------------------------
1152                                ;Read command:
1153                                ; word 1 = command = 'RDM'
1154                                ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
1155                                ; word 3 = address in memory
1156                                ; word 4 = not used
1157                                ;Version query:
1158                                ; word 1 = 'VER'
1159                                ; word 2-4 unused
1160   
1161      P:0002D1 P:0002D3 0BF080            JSR     SAVE_REGISTERS
                            0004E3
1162      P:0002D3 P:0002D5 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            0004AA
1163   
1164      P:0002D5 P:0002D7 44F400            MOVE              #'RDM',X0
                            52444D
1165      P:0002D7 P:0002D9 0D02A2            JSR     VCOM_PREPARE_REPLY
1166      P:0002D8 P:0002DA 0D02B0            JSR     VCOM_CHECK
1167      P:0002D9 P:0002DB 0AF0AA            JEQ     READ_MEMORY_XYP
                            0002E3
1168   
1169      
1170      P:0002DB P:0002DD 44F400            MOVE              #'VER',X0
                            564552
1171      P:0002DD P:0002DF 0D02A2            JSR     VCOM_PREPARE_REPLY
1172      P:0002DE P:0002E0 0D02B0            JSR     VCOM_CHECK
1173      P:0002DF P:0002E1 0E22CC            JNE     VCOM_EXIT
1174   
1175      P:0002E0 P:0002E2 44F000            MOVE              X:REV_NUMBER,X0
                            000003
1176      P:0002E2 P:0002E4 0C02CA            JMP     VCOM_EXIT_X0
1177   
1178                                READ_MEMORY_XYP
1179   
1180      
1181      P:0002E3 P:0002E5 56F000            MOVE              X:DRXR_WD2,A
                            000009
1182      P:0002E5 P:0002E7 60F000            MOVE              X:DRXR_WD3,R0
                            00000A
1183   
1184      P:0002E7 P:0002E9 0140C5            CMP     #'_X',A
                            005F58
1185      P:0002E9 P:0002EB 0AF0AA            JEQ     READ_MEMORY_X
                            0002F6
1186   
1187      P:0002EB P:0002ED 0140C5            CMP     #'_Y',A
                            005F59
1188      P:0002ED P:0002EF 0AF0AA            JEQ     READ_MEMORY_Y
                            0002F8
1189   
1190      P:0002EF P:0002F1 0140C5            CMP     #'_P',A
                            005F50
1191      P:0002F1 P:0002F3 0AF0AA            JEQ     READ_MEMORY_P
                            0002FA
1192   
1193      P:0002F3 P:0002F5 44F400            MOVE              #'MTE',X0
                            4D5445
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 25



1194      P:0002F5 P:0002F7 0C02C5            JMP     VCOM_EXIT_ERROR_X0
1195   
1196                                READ_MEMORY_X
1197      P:0002F6 P:0002F8 44E000            MOVE              X:(R0),X0
1198      P:0002F7 P:0002F9 0C02CA            JMP     VCOM_EXIT_X0
1199                                READ_MEMORY_Y
1200      P:0002F8 P:0002FA 4CE000            MOVE                          Y:(R0),X0
1201      P:0002F9 P:0002FB 0C02CA            JMP     VCOM_EXIT_X0
1202                                READ_MEMORY_P
1203      P:0002FA P:0002FC 07E084            MOVE              P:(R0),X0
1204      P:0002FB P:0002FD 0C02CA            JMP     VCOM_EXIT_X0
1205   
1206   
1207                                ;--------------------------------------------------------------
1208                                WRITE_MEMORY
1209                                ;---------------------------------------------------------------
1210                                ; word 1 = command = 'WRM'
1211                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1212                                ; word 3 = address in memory
1213                                ; word 4 = value
1214   
1215      P:0002FC P:0002FE 0BF080            JSR     SAVE_REGISTERS
                            0004E3
1216      P:0002FE P:000300 45F400            MOVE              #'WRM',X1
                            57524D
1217      P:000300 P:000302 0D02BF            JSR     VCOM_INTRO
1218      P:000301 P:000303 0E22CC            JNE     VCOM_EXIT
1219   
1220      
1221      P:000302 P:000304 56F000            MOVE              X:DRXR_WD2,A
                            000009
1222      P:000304 P:000306 60F000            MOVE              X:DRXR_WD3,R0
                            00000A
1223      P:000306 P:000308 44F000            MOVE              X:DRXR_WD4,X0
                            00000B
1224   
1225      P:000308 P:00030A 0140C5            CMP     #'_X',A
                            005F58
1226      P:00030A P:00030C 0AF0AA            JEQ     WRITE_MEMORY_X
                            000317
1227   
1228      P:00030C P:00030E 0140C5            CMP     #'_Y',A
                            005F59
1229      P:00030E P:000310 0AF0AA            JEQ     WRITE_MEMORY_Y
                            000319
1230   
1231      P:000310 P:000312 0140C5            CMP     #'_P',A
                            005F50
1232      P:000312 P:000314 0AF0AA            JEQ     WRITE_MEMORY_P
                            00031B
1233   
1234      P:000314 P:000316 44F400            MOVE              #'MTE',X0
                            4D5445
1235      P:000316 P:000318 0C02C5            JMP     VCOM_EXIT_ERROR_X0
1236   
1237                                WRITE_MEMORY_X
1238      P:000317 P:000319 446000            MOVE              X0,X:(R0)
1239      P:000318 P:00031A 0C02CA            JMP     VCOM_EXIT_X0
1240                                WRITE_MEMORY_Y
1241      P:000319 P:00031B 4C6000            MOVE                          X0,Y:(R0)
1242      P:00031A P:00031C 0C02CA            JMP     VCOM_EXIT_X0
1243                                WRITE_MEMORY_P
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 26



1244      P:00031B P:00031D 076084            MOVE              X0,P:(R0)
1245      P:00031C P:00031E 0C02CA            JMP     VCOM_EXIT_X0
1246   
1247   
1248                                ;-----------------------------------------------------------------------------
1249                                START_APPLICATION
1250                                ; an application should already have been downloaded to the PCI memory.
1251                                ; this command will execute it.
1252                                ; ----------------------------------------------------------------------
1253                                ; word 1 = command = 'GOA'
1254                                ; word 2-4 unused
1255   
1256      P:00031D P:00031F 0BF080            JSR     SAVE_REGISTERS
                            0004E3
1257      P:00031F P:000321 45F400            MOVE              #'GOA',X1
                            474F41
1258   
1259      P:000321 P:000323 0D02BF            JSR     VCOM_INTRO
1260      P:000322 P:000324 0E22CC            JNE     VCOM_EXIT
1261   
1262      P:000323 P:000325 0A7020            BSET    #APPLICATION_LOADED,X:STATUS
                            000000
1263      P:000325 P:000327 000004            RTI                                       ; Application will reply.
1264   
1265   
1266                                ; ---------------------------------------------------------
1267                                STOP_APPLICATION
1268                                ; this command stops an application that is currently running
1269                                ; used for applications that once started run contiunually
1270                                ;-----------------------------------------------------------
1271                                ; word 1 = command = ' STP'
1272                                ; word 2-4 unused
1273   
1274      P:000326 P:000328 0BF080            JSR     SAVE_REGISTERS
                            0004E3
1275      P:000328 P:00032A 45F400            MOVE              #'STP',X1
                            535450
1276   
1277      P:00032A P:00032C 0D02BF            JSR     VCOM_INTRO
1278      P:00032B P:00032D 0E22CC            JNE     VCOM_EXIT
1279   
1280      P:00032C P:00032E 0A7000            BCLR    #APPLICATION_LOADED,X:STATUS
                            000000
1281      P:00032E P:000330 0A700C            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1282      P:000330 P:000332 0C02CC            JMP     VCOM_EXIT
1283   
1284   
1285                                ;-----------------------------------------------------------------------------
1286                                RESET_CONTROLLER
1287                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1288                                ;-----------------------------------------------------------------------------
1289                                ; word 1 = command = 'RCO'
1290                                ; word 2-4 unused
1291   
1292      P:000331 P:000333 0BF080            JSR     SAVE_REGISTERS
                            0004E3
1293      P:000333 P:000335 45F400            MOVE              #'RCO',X1
                            52434F
1294      P:000335 P:000337 0D02BF            JSR     VCOM_INTRO
1295      P:000336 P:000338 0E22CC            JNE     VCOM_EXIT
1296   
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 27



1297      P:000337 P:000339 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1298      P:000338 P:00033A 000000            NOP
1299      P:000339 P:00033B 000000            NOP
1300      P:00033A P:00033C 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1301      P:00033C P:00033E 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1302      P:00033E P:000340 446000            MOVE              X0,X:(R0)
1303      P:00033F P:000341 0606A0            REP     #6                                ; Wait for transmission to complete
1304      P:000340 P:000342 000000            NOP
1305      P:000341 P:000343 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1306   
1307                                ; Wait for a bit for MCE to be reset.......
1308      P:000342 P:000344 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1309      P:000344 P:000346 06C400            DO      X0,L_DELAY
                            00034A
1310      P:000346 P:000348 06E883            DO      #1000,L_RDFIFO
                            000349
1311      P:000348 P:00034A 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1312      P:000349 P:00034B 000000            NOP                                       ;   receiver empty
1313                                L_RDFIFO
1314      P:00034A P:00034C 000000            NOP
1315                                L_DELAY
1316      P:00034B P:00034D 000000            NOP
1317   
1318      P:00034C P:00034E 44F400            MOVE              #'000',X0
                            303030
1319      P:00034E P:000350 0C02CA            JMP     VCOM_EXIT_X0
1320   
1321                                ;-----------------------------------------------------------------------------
1322                                QUIET_TRANSFER_SET
1323                                ;-----------------------------------------------------------------------------
1324                                ;Quiet transfer mode configuration
1325                                ; word 1 = command = 'QTS'
1326                                ; word 2 = parameter to set
1327                                ; word 3-4 = arguments
1328   
1329      P:00034F P:000351 0BF080            JSR     SAVE_REGISTERS                    ; standard opening
                            0004E3
1330      P:000351 P:000353 45F400            MOVE              #'QTS',X1
                            515453
1331      P:000353 P:000355 0D02BF            JSR     VCOM_INTRO
1332      P:000354 P:000356 0E22CC            JNE     VCOM_EXIT
1333   
1334      P:000355 P:000357 56F000            MOVE              X:DRXR_WD2,A            ; Parameter id
                            000009
1335      P:000357 P:000359 44F000            MOVE              X:DRXR_WD3,X0           ; First arg
                            00000A
1336      P:000359 P:00035B 45F000            MOVE              X:DRXR_WD4,X1           ; Second arg
                            00000B
1337   
1338      P:00035B P:00035D 0140C5            CMP     #'BAS',A
                            424153
1339      P:00035D P:00035F 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            0003B3
1340   
1341      P:00035F P:000361 0140C5            CMP     #'DEL',A
                            44454C
1342      P:000361 P:000363 62F400            MOVE              #QT_BUF_SIZE,R2
                            000051
1343      P:000363 P:000365 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 28



                            0003B1
1344   
1345      P:000365 P:000367 0140C5            CMP     #'NUM',A
                            4E554D
1346      P:000367 P:000369 62F400            MOVE              #QT_BUF_MAX,R2
                            000052
1347      P:000369 P:00036B 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0003B1
1348   
1349      P:00036B P:00036D 0140C5            CMP     #'INF',A
                            494E46
1350      P:00036D P:00036F 62F400            MOVE              #QT_INFORM,R2
                            000054
1351      P:00036F P:000371 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0003B1
1352   
1353      P:000371 P:000373 0140C5            CMP     #'SIZ',A
                            53495A
1354      P:000373 P:000375 62F400            MOVE              #QT_FRAME_SIZE,R2
                            000053
1355      P:000375 P:000377 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0003B1
1356   
1357      P:000377 P:000379 0140C5            CMP     #'TAI',A
                            544149
1358      P:000379 P:00037B 62F400            MOVE              #QT_BUF_TAIL,R2
                            000056
1359      P:00037B P:00037D 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0003B1
1360   
1361      P:00037D P:00037F 0140C5            CMP     #'HEA',A
                            484541
1362      P:00037F P:000381 62F400            MOVE              #QT_BUF_HEAD,R2
                            000055
1363      P:000381 P:000383 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0003B1
1364   
1365      P:000383 P:000385 0140C5            CMP     #'DRO',A
                            44524F
1366      P:000385 P:000387 62F400            MOVE              #QT_DROPS,R2
                            00005A
1367      P:000387 P:000389 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0003B1
1368   
1369      P:000389 P:00038B 0140C5            CMP     #'PER',A
                            504552
1370      P:00038B P:00038D 62F400            MOVE              #TCPR0,R2
                            FFFF8D
1371      P:00038D P:00038F 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0003B1
1372   
1373      P:00038F P:000391 0140C5            CMP     #'FLU',A
                            464C55
1374      P:000391 P:000393 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            00039A
1375   
1376      P:000393 P:000395 0140C5            CMP     #'SET',A
                            534554
1377      P:000395 P:000397 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            0003A2
1378   
1379      P:000397 P:000399 44F400            MOVE              #'MTE',X0
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 29



                            4D5445
1380      P:000399 P:00039B 0C02C5            JMP     VCOM_EXIT_ERROR_X0
1381   
1382                                QUIET_TRANSFER_SET_FLUSH
1383      P:00039A P:00039C 0A7012            BCLR    #QT_FLUSH,X:STATUS
                            000000
1384      P:00039C P:00039E 208E00            MOVE              X0,A
1385      P:00039D P:00039F 200003            TST     A
1386      P:00039E P:0003A0 0EA2CC            JEQ     VCOM_EXIT
1387      P:00039F P:0003A1 0A7032            BSET    #QT_FLUSH,X:STATUS
                            000000
1388      P:0003A1 P:0003A3 0C02CC            JMP     VCOM_EXIT
1389   
1390                                QUIET_TRANSFER_SET_ENABLED
1391      P:0003A2 P:0003A4 0A7011            BCLR    #QT_ENABLED,X:STATUS
                            000000
1392      P:0003A4 P:0003A6 0BF080            JSR     TIMER_DISABLE
                            0005EB
1393      P:0003A6 P:0003A8 208E00            MOVE              X0,A
1394      P:0003A7 P:0003A9 200003            TST     A
1395      P:0003A8 P:0003AA 0EA2CC            JEQ     VCOM_EXIT
1396      P:0003A9 P:0003AB 280000            MOVE              #0,A0
1397      P:0003AA P:0003AC 0A7031            BSET    #QT_ENABLED,X:STATUS
                            000000
1398      P:0003AC P:0003AE 507000            MOVE              A0,X:TLR0
                            FFFF8E
1399      P:0003AE P:0003B0 0BF080            JSR     TIMER_ENABLE
                            0005E5
1400      P:0003B0 P:0003B2 0C02CC            JMP     VCOM_EXIT
1401   
1402                                QUIET_TRANSFER_SET_R2
1403      P:0003B1 P:0003B3 446200            MOVE              X0,X:(R2)
1404      P:0003B2 P:0003B4 0C02CC            JMP     VCOM_EXIT
1405   
1406                                QUIET_TRANSFER_SET_BASE
1407      P:0003B3 P:0003B5 447000            MOVE              X0,X:QT_BASE_LO
                            00004F
1408      P:0003B5 P:0003B7 457000            MOVE              X1,X:QT_BASE_HI
                            000050
1409   
1410      P:0003B7 P:0003B9 0BF080            JSR     BUFFER_RESET
                            000611
1411   
1412      P:0003B9 P:0003BB 0C02CC            JMP     VCOM_EXIT
1413   
1414   
1415                                ;-----------------------------------------------------------------------------
1416                                SYSTEM_RESET
1417                                ;-----------------------------------------------------------------------------
1418                                ;Responseless system reset, including fifo empty
1419   
1420      
1421   
1422      P:0003BA P:0003BC 0BF080            JSR     CLEAR_FIFO
                            000517
1423   
1424      
1425   
1426                                ;       MOVEP   #$0001C0,X:IPRC         ; Disable HF* FIFO interrupt
1427                                ;       MOVE    #$200,SR                ; Mask set up for reset switch only.
1428   
1429      P:0003BC P:0003BE 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 30



1430      P:0003BD P:0003BF 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1431                                                                                    ; set to zero except for interrupts
1432      P:0003BF P:0003C1 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1433                                                                                    ; so first set to 0
1434      P:0003C0 P:0003C2 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1435                                                                                    ; therefore,return to initialization
1436      P:0003C2 P:0003C4 000000            NOP
1437      P:0003C3 P:0003C5 000004            RTI                                       ; return from ISR - to START
1438   
1439   
1440                                ;--------------------------------------------------------------------
1441                                CLEAN_UP_PCI
1442                                ;--------------------------------------------------------------------
1443                                ; Clean up the PCI board from wherever it was executing
1444   
1445      P:0003C4 P:0003C6 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable HF* FIFO interrupt
                            0001C0
1446      P:0003C6 P:0003C8 05F439            MOVE              #$200,SR                ; mask for reset interrupts only
                            000200
1447   
1448      P:0003C8 P:0003CA 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1449      P:0003C9 P:0003CB 05F43D            MOVEC             #$000200,SSL            ; SR = zero except for interrupts
                            000200
1450      P:0003CB P:0003CD 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1451      P:0003CC P:0003CE 05F43C            MOVEC             #START,SSH              ; Set PC to for full initialization
                            000100
1452      P:0003CE P:0003D0 000000            NOP
1453      P:0003CF P:0003D1 000004            RTI
1454   
1455                                ;----------------------------------------------------------------------
1456                                SEND_PACKET_TO_CONTROLLER
1457   
1458                                ; forward packet stuff to the MCE
1459                                ; gets address in HOST memory where packet is stored
1460                                ; read 3 consecutive locations starting at this address
1461                                ; then sends the data from these locations up to the MCE
1462                                ;----------------------------------------------------------------------
1463   
1464                                ; word 1 = command = 'CON'
1465                                ; word 2 = host high address
1466                                ; word 3 = host low address
1467                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1468                                ;        = '1' --> when MCE command is GO
1469   
1470                                ; all MCE commands are now 'block commands'
1471                                ; i.e. 64 words long.
1472   
1473      P:0003D0 P:0003D2 0D04E3            JSR     <SAVE_REGISTERS                   ; save working registers
1474   
1475      P:0003D1 P:0003D3 0D04AA            JSR     <RD_DRXR                          ; read words from host write to HTXR
1476                                                                                    ; reads as 4 x 24 bit words
1477   
1478      P:0003D2 P:0003D4 568800            MOVE              X:<DRXR_WD1,A           ; read command
1479      P:0003D3 P:0003D5 44F400            MOVE              #'CON',X0
                            434F4E
1480      P:0003D5 P:0003D7 200045            CMP     X0,A                              ; ensure command is 'CON'
1481      P:0003D6 P:0003D8 0E240B            JNE     <CON_ERROR                        ; error, command NOT HCVR address
1482   
1483                                ; convert 2 x 24 bit words ( only 16 LSBs are significant) from host into 32 bit address
1484      P:0003D7 P:0003D9 20001B            CLR     B
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 31



1485      P:0003D8 P:0003DA 448900            MOVE              X:<DRXR_WD2,X0          ; MS 16bits of address
1486      P:0003D9 P:0003DB 518A00            MOVE              X:<DRXR_WD3,B0          ; LS 16bits of address
1487      P:0003DA P:0003DC 0C1941            INSERT  #$010010,X0,B                     ; convert to 32 bits and put in B
                            010010
1488   
1489      P:0003DC P:0003DE 568B00            MOVE              X:<DRXR_WD4,A           ; read word 4 - GO command?
1490      P:0003DD P:0003DF 44F000            MOVE              X:ZERO,X0
                            000033
1491      P:0003DF P:0003E1 200045            CMP     X0,A
1492      P:0003E0 P:0003E2 0AF0AA            JEQ     BLOCK_CON
                            0003EE
1493   
1494   
1495      P:0003E2 P:0003E4 0A008C            JCLR    #APPLICATION_RUNNING,X:STATUS,SET_PACKET_DELAY ; not running diagnostic applic
ation?
                            0003EC
1496   
1497                                ; need to generate an internal go command to test master write on bus.....  Diagnostic test
1498      P:0003E4 P:0003E6 0A702D            BSET    #INTERNAL_GO,X:STATUS             ; set flag so that GO reply / data is genera
ted by PCI card...
                            000000
1499   
1500                                ; since INTERNAL_GO  - read command but don't send it to MCE...
1501   
1502                                CLR_CMD
1503      P:0003E6 P:0003E8 064080            DO      #64,END_CLR_CMD                   ; block size = 32bit x 64 (256 bytes)
                            0003E9
1504      P:0003E8 P:0003EA 0D04B7            JSR     <READ_FROM_PCI                    ; get next 32 bit word from HOST
1505      P:0003E9 P:0003EB 000000            NOP
1506                                END_CLR_CMD
1507      P:0003EA P:0003EC 0AF080            JMP     FINISH_CON                        ; don't send out on command on fibre
                            0003FC
1508   
1509   
1510                                SET_PACKET_DELAY
1511      P:0003EC P:0003EE 0A7027            BSET    #DATA_DLY,X:STATUS                ; set data delay so that next data packet af
ter go reply
                            000000
1512                                                                                    ; experiences a delay before host notify.
1513   
1514                                ; -----------------------------------------------------------------------
1515                                ; WARNING!!!
1516                                ; MCE requires IDLE characters between 32bit words sent FROM the PCI card
1517                                ; DO not change READ_FROM_PCI to DMA block transfer....
1518                                ; ------------------------------------------------------------------------
1519   
1520                                BLOCK_CON
1521      P:0003EE P:0003F0 66F000            MOVE              X:CONSTORE,R6
                            000041
1522   
1523      P:0003F0 P:0003F2 064080            DO      #64,END_BLOCK_CON                 ; block size = 32bit x 64 (256 bytes)
                            0003F8
1524      P:0003F2 P:0003F4 0D04B7            JSR     <READ_FROM_PCI                    ; get next 32 bit word from HOST
1525      P:0003F3 P:0003F5 208C00            MOVE              X0,A1                   ; prepare to send
1526      P:0003F4 P:0003F6 20A800            MOVE              X1,A0                   ; prepare to send
1527   
1528      P:0003F5 P:0003F7 4D5E00            MOVE                          X1,Y:(R6)+  ; b4, b3 (msb)
1529      P:0003F6 P:0003F8 4C5E00            MOVE                          X0,Y:(R6)+  ; b2, b1  (lsb)
1530   
1531      P:0003F7 P:0003F9 0D04EF            JSR     <XMT_WD_FIBRE                     ; off it goes
1532      P:0003F8 P:0003FA 000000            NOP
1533                                END_BLOCK_CON
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 32



1534   
1535      P:0003F9 P:0003FB 0A0008            BCLR    #PACKET_CHOKE,X:<STATUS           ; disable packet choke...
1536                                                                                    ; comms now open with MCE and packets will b
e processed.
1537                                ; Enable Byte swaping for correct comms protocol.
1538      P:0003FA P:0003FC 0A0025            BSET    #BYTE_SWAP,X:<STATUS              ; flag to let host know byte swapping on
1539      P:0003FB P:0003FD 013D24            BSET    #AUX1,X:PDRC                      ; enable hardware
1540   
1541   
1542                                ; -------------------------------------------------------------------------
1543                                ; when completed successfully then PCI needs to reply to Host with
1544                                ; word1 = reply/data = reply
1545                                FINISH_CON
1546      P:0003FC P:0003FE 44F400            MOVE              #'REP',X0
                            524550
1547      P:0003FE P:000400 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1548      P:0003FF P:000401 44F400            MOVE              #'CON',X0
                            434F4E
1549      P:000401 P:000403 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1550      P:000402 P:000404 44F400            MOVE              #'ACK',X0
                            41434B
1551      P:000404 P:000406 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1552      P:000405 P:000407 44F400            MOVE              #'000',X0
                            303030
1553      P:000407 P:000409 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1554      P:000408 P:00040A 0D04D7            JSR     <RESTORE_REGISTERS                ; restore working registers
1555      P:000409 P:00040B 0D048F            JSR     <PCI_MESSAGE_TO_HOST              ;  interrupt host with message (x0 restored 
here)
1556      P:00040A P:00040C 000004            RTI                                       ; return from ISR
1557   
1558                                ; when there is a failure in the host to PCI command then the PCI
1559                                ; needs still to reply to Host but with an error message
1560                                CON_ERROR
1561      P:00040B P:00040D 44F400            MOVE              #'REP',X0
                            524550
1562      P:00040D P:00040F 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1563      P:00040E P:000410 44F400            MOVE              #'CON',X0
                            434F4E
1564      P:000410 P:000412 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1565      P:000411 P:000413 44F400            MOVE              #'ERR',X0
                            455252
1566      P:000413 P:000415 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1567      P:000414 P:000416 44F400            MOVE              #'CNE',X0
                            434E45
1568      P:000416 P:000418 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1569      P:000417 P:000419 0D04D7            JSR     <RESTORE_REGISTERS                ; restore working registers
1570      P:000418 P:00041A 0D048F            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1571      P:000419 P:00041B 000004            RTI                                       ; return from ISR
1572   
1573                                ; ------------------------------------------------------------------------------------
1574                                SEND_PACKET_TO_HOST
1575                                ; this command is received from the Host and actions the PCI board to pick up an address
1576                                ; pointer from DRXR which the PCI board then uses to write packets from the
1577                                ; MCE to the host memory starting at the address given.
1578                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1579                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1580                                ; HST after packet sent (unless error).
1581                                ; --------------------------------------------------------------------------------------
1582                                ; word 1 = command = 'HST'
1583                                ; word 2 = host high address
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 33



1584                                ; word 3 = host low address
1585                                ; word 4 = not used but read
1586   
1587                                ; save some registers but not B
1588   
1589      P:00041A P:00041C 0D04E3            JSR     <SAVE_REGISTERS                   ; save working registers
1590      P:00041B P:00041D 45F400            MOVE              #'HST',X1
                            485354
1591      P:00041D P:00041F 0D02BF            JSR     VCOM_INTRO
1592      P:00041E P:000420 0E22CC            JNE     VCOM_EXIT
1593   
1594      
1595      P:00041F P:000421 448900            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
1596      P:000420 P:000422 518A00            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1597   
1598      P:000421 P:000423 447000            MOVE              X0,X:BURST_DEST_HI
                            000045
1599      P:000423 P:000425 517000            MOVE              B0,X:BURST_DEST_LO
                            000044
1600   
1601      P:000425 P:000427 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1602   
1603      P:000426 P:000428 000004            RTI                                       ; Main loop will reply after packet transfer
!
1604   
1605   
1606                                ; --------------------------------------------------------------------
1607                                SOFTWARE_RESET
1608                                ;----------------------------------------------------------------------
1609                                ; word 1 = command = 'RST'
1610                                ; word 2-4 unused
1611   
1612      P:000427 P:000429 0BF080            JSR     SAVE_REGISTERS
                            0004E3
1613      P:000429 P:00042B 45F400            MOVE              #'RST',X1
                            525354
1614      P:00042B P:00042D 0D02BF            JSR     VCOM_INTRO
1615      P:00042C P:00042E 0E22CC            JNE     VCOM_EXIT
1616   
1617                                ; RST command OK so reply to host
1618                                FINISH_RST
1619      P:00042D P:00042F 44F400            MOVE              #'000',X0
                            303030
1620      P:00042F P:000431 447000            MOVE              X0,X:DTXS_WD4
                            00000F
1621      P:000431 P:000433 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00048F
1622   
1623      P:000433 P:000435 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            000433
1624   
1625      P:000435 P:000437 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS     ; clear app flag
1626      P:000436 P:000438 0A0006            BCLR    #PREAMBLE_ERROR,X:<STATUS         ; clear preamble error
1627      P:000437 P:000439 0A000C            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1628   
1629                                ; initialise some parameter here - that we don't want to initialse under a fatal error reset.
1630   
1631      P:000438 P:00043A 200013            CLR     A
1632      P:000439 P:00043B 340000            MOVE              #0,R4                   ; initialise word count
1633      P:00043A P:00043C 560600            MOVE              A,X:<WORD_COUNT         ; initialise word count store (num of words 
written over bus/packet)
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 34



1634      P:00043B P:00043D 560700            MOVE              A,X:<NUM_DUMPED         ; initialise number dumped from FIFO (after 
HST TO)
1635   
1636   
1637                                ; remember we are in a ISR so can't just jump to start.
1638   
1639      P:00043C P:00043E 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable HF* FIFO interrupt
                            0001C0
1640      P:00043E P:000440 05F439            MOVE              #$200,SR                ; Mask set up for reset switch only.
                            000200
1641   
1642   
1643      P:000440 P:000442 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1644      P:000441 P:000443 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1645                                                                                    ; set to zero except for interrupts
1646      P:000443 P:000445 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1647                                                                                    ; so first set to 0
1648      P:000444 P:000446 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1649                                                                                    ; therefore,return to initialization
1650      P:000446 P:000448 000000            NOP
1651      P:000447 P:000449 000004            RTI                                       ; return from ISR - to START
1652   
1653   
1654                                ;---------------------------------------------------------------
1655                                ;
1656                                ;                          * END OF ISRs *
1657                                ;
1658                                ;--------------------------------------------------------------
1659   
1660   
1661   
1662                                ;----------------------------------------------------------------
1663                                ;
1664                                ;                     * Beginning of SUBROUTINES *
1665                                ;
1666                                ;-----------------------------------------------------------------
1667   
1668   
1669                                ; -------------------------------------------------------------
1670                                CALC_NO_BUFFS
1671                                ;----------------------------------------------------
1672                                ; number of 512 buffers in packet calculated (X:TOTAL_BUFFS)
1673                                ; and number of left over blocks (X:NUM_LEFTOVER_BLOCKS)
1674                                ; and left over words (X:LEFT_TO_READ)
1675   
1676      P:000448 P:00044A 20001B            CLR     B
1677      P:000449 P:00044B 51A300            MOVE              X:<HEAD_W4_0,B0         ; LS 16bits
1678      P:00044A P:00044C 44A200            MOVE              X:<HEAD_W4_1,X0         ; MS 16bits
1679   
1680      P:00044B P:00044D 0C1941            INSERT  #$010010,X0,B                     ; now size of packet B....giving # of 32bit 
words in packet
                            010010
1681      P:00044D P:00044F 000000            NOP
1682   
1683                                ; need to covert this to 16 bit since read from FIFO and saved in Y memory as 16bit words...
1684   
1685                                ; so double size of packet....
1686      P:00044E P:000450 20003A            ASL     B
1687   
1688                                ; now save
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 35



1689      P:00044F P:000451 212400            MOVE              B0,X0
1690      P:000450 P:000452 21A500            MOVE              B1,X1
1691      P:000451 P:000453 443600            MOVE              X0,X:<PACKET_SIZE_LOW   ; low 24 bits of packet size (in 16bit words
)
1692      P:000452 P:000454 453700            MOVE              X1,X:<PACKET_SIZE_HIH   ; high 8 bits of packet size (in 16bit words
)
1693   
1694      P:000453 P:000455 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1695      P:000454 P:000456 54B700            MOVE              X:<PACKET_SIZE_HIH,A1
1696      P:000455 P:000457 0C1C12            ASR     #9,A,A                            ; divide by 512...number of 16bit words in a
 buffer
1697      P:000456 P:000458 000000            NOP
1698      P:000457 P:000459 503C00            MOVE              A0,X:<TOTAL_BUFFS
1699   
1700      P:000458 P:00045A 210500            MOVE              A0,X1
1701      P:000459 P:00045B 47F400            MOVE              #HF_FIFO,Y1
                            000200
1702      P:00045B P:00045D 2000F0            MPY     X1,Y1,A
1703      P:00045C P:00045E 0C1C03            ASR     #1,A,B                            ; B holds number of 16bit words in all full 
buffers
1704      P:00045D P:00045F 000000            NOP
1705   
1706      P:00045E P:000460 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1707      P:00045F P:000461 54B700            MOVE              X:<PACKET_SIZE_HIH,A1   ; A holds total number of 16bit words
1708      P:000460 P:000462 200014            SUB     B,A                               ; now A holds number of left over 16bit word
s
1709      P:000461 P:000463 000000            NOP
1710      P:000462 P:000464 503D00            MOVE              A0,X:<LEFT_TO_READ      ; store number of left over 16bit words to r
ead
1711      P:000463 P:000465 0C1C0A            ASR     #5,A,A                            ; divide by 32... number of 16bit words in l
efover block
1712      P:000464 P:000466 000000            NOP
1713      P:000465 P:000467 503F00            MOVE              A0,X:<NUM_LEFTOVER_BLOCKS
1714      P:000466 P:000468 210500            MOVE              A0,X1
1715      P:000467 P:000469 47F400            MOVE              #>SMALL_BLK,Y1
                            000020
1716      P:000469 P:00046B 2000F0            MPY     X1,Y1,A
1717      P:00046A P:00046C 0C1C02            ASR     #1,A,A
1718      P:00046B P:00046D 000000            NOP
1719   
1720      P:00046C P:00046E 200018            ADD     A,B                               ; B holds words in all buffers
1721      P:00046D P:00046F 000000            NOP
1722      P:00046E P:000470 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1723      P:00046F P:000471 54B700            MOVE              X:<PACKET_SIZE_HIH,A1   ; A holds total number of words
1724      P:000470 P:000472 200014            SUB     B,A                               ; now A holds number of left over words
1725      P:000471 P:000473 000000            NOP
1726      P:000472 P:000474 503D00            MOVE              A0,X:<LEFT_TO_READ      ; store number of left over 16bit words to r
ead
1727   
1728      P:000473 P:000475 0C1C02            ASR     #1,A,A                            ; divide by two to get number of 32 bit word
s to write
1729      P:000474 P:000476 000000            NOP                                       ; for pipeline
1730      P:000475 P:000477 503E00            MOVE              A0,X:<LEFT_TO_WRITE     ; store number of left over 32 bit words (2 
x 16 bit) to write to host after small block transfer as well
1731   
1732      P:000476 P:000478 00000C            RTS
1733   
1734                                ;---------------------------------------------------------------
1735                                GET_FO_WRD
1736                                ;--------------------------------------------------------------
1737                                ; Anything in fibre receive FIFO?   If so store in X0
1738   
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 36



1739      P:000477 P:000479 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            00048D
1740      P:000479 P:00047B 000000            NOP
1741      P:00047A P:00047C 000000            NOP
1742      P:00047B P:00047D 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS             ; check twice for FO metastability.
                            00048D
1743      P:00047D P:00047F 0AF080            JMP     RD_FO_WD
                            000485
1744   
1745      P:00047F P:000481 01AD80  WT_FIFO   JCLR    #EF,X:PDRD,*                      ; Wait till something in FIFO flagged
                            00047F
1746      P:000481 P:000483 000000            NOP
1747      P:000482 P:000484 000000            NOP
1748      P:000483 P:000485 01AD80            JCLR    #EF,X:PDRD,WT_FIFO                ; check twice.....
                            00047F
1749   
1750                                ; Read one word from the fiber optics FIFO, check it and put it in A1
1751                                RD_FO_WD
1752      P:000485 P:000487 09443F            MOVEP             Y:RDFIFO,X0             ; then read to X0
1753      P:000486 P:000488 54F400            MOVE              #$00FFFF,A1             ; mask off top 2 bytes ($FC)
                            00FFFF
1754      P:000488 P:00048A 200046            AND     X0,A                              ; since receiving 16 bits in 24bit register
1755      P:000489 P:00048B 000000            NOP
1756      P:00048A P:00048C 218400            MOVE              A1,X0
1757      P:00048B P:00048D 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1758      P:00048C P:00048E 00000C            RTS
1759                                CLR_FO_RTS
1760      P:00048D P:00048F 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1761      P:00048E P:000490 00000C            RTS
1762   
1763   
1764                                ; ----------------------------------------------------------------------------
1765                                PCI_MESSAGE_TO_HOST
1766                                ;----------------------------------------------------------------------------
1767   
1768                                ; subroutine to send 4 words as a reply from PCI to the Host
1769                                ; using the DTXS-HRXS data path
1770                                ; PCI card writes here first then causes an interrupt INTA on
1771                                ; the PCI bus to alert the host to the reply message
1772   
1773      P:00048F P:000491 0A85A3            JSET    #DCTR_HF3,X:DCTR,*                ; make sure host ready to receive interrupt
                            00048F
1774                                                                                    ; cleared via fast interrupt if host out of 
its ISR
1775   
1776      P:000491 P:000493 0A8981            JCLR    #STRQ,X:DSR,*                     ; Wait for transmitter to be NOT FULL
                            000491
1777                                                                                    ; i.e. if CLR then FULL so wait
1778                                                                                    ; if not then it is clear to write
1779   
1780                                PCI_MESSAGE_TO_HOST_NOW                             ; non-blocking entry point...
1781   
1782      P:000493 P:000495 448C00            MOVE              X:<DTXS_WD1,X0
1783      P:000494 P:000496 447000            MOVE              X0,X:DTXS               ; Write 24 bit word1
                            FFFFCD
1784   
1785      P:000496 P:000498 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            000496
1786      P:000498 P:00049A 448D00            MOVE              X:<DTXS_WD2,X0
1787      P:000499 P:00049B 447000            MOVE              X0,X:DTXS               ; Write 24 bit word2
                            FFFFCD
1788   
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 37



1789      P:00049B P:00049D 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            00049B
1790      P:00049D P:00049F 448E00            MOVE              X:<DTXS_WD3,X0
1791      P:00049E P:0004A0 447000            MOVE              X0,X:DTXS               ; Write 24 bit word3
                            FFFFCD
1792   
1793      P:0004A0 P:0004A2 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            0004A0
1794      P:0004A2 P:0004A4 448F00            MOVE              X:<DTXS_WD4,X0
1795      P:0004A3 P:0004A5 447000            MOVE              X0,X:DTXS               ; Write 24 bit word4
                            FFFFCD
1796   
1797   
1798                                ; restore X0....
1799                                ; PCI_MESSAGE_TO_HOST is used by all command vector ISRs.
1800                                ; Working registers must be restored before RTI.
1801                                ; However, we want to restore before asserting INTA.
1802                                ; x0 is only one that can't be restored before PCI_MESSAGE_TO_HOST
1803                                ; (since it is used by this SR) hence we restore here.
1804                                ; this is redundant for a 'NFY' message (since sequential instruction)
1805                                ; but may be required for a PCI command reply 'REP' message.
1806                                ; (since interrupt driven)
1807   
1808      P:0004A5 P:0004A7 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            00002E
1809   
1810                                ; all the transmit words are in the FIFO, interrupt the Host
1811                                ; the Host should clear this interrupt once it is detected.
1812                                ; It does this by writing to HCVR to cause a fast interrupt.
1813   
1814   
1815      P:0004A7 P:0004A9 0A8523            BSET    #DCTR_HF3,X:DCTR                  ; set flag to handshake interrupt (INTA) wit
h host.
1816      P:0004A8 P:0004AA 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1817   
1818      P:0004A9 P:0004AB 00000C            RTS
1819   
1820                                ;---------------------------------------------------------------
1821                                RD_DRXR
1822                                ;--------------------------------------------------------------
1823                                ; routine is used to read from HTXR-DRXR data path
1824                                ; which is used by the Host to communicate with the PCI board
1825                                ; the host writes 4 words to this FIFO then interrupts the PCI
1826                                ; which reads the 4 words and acts on them accordingly.
1827   
1828   
1829      P:0004AA P:0004AC 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            0004AA
1830                                                                                    ; implies that host has written words
1831   
1832   
1833                                ; actually reading as slave here so this shouldn't be necessary......?
1834   
1835      P:0004AC P:0004AE 0A8717            BCLR    #FC1,X:DPMC                       ; 24 bit read FC1 = 0, FC1 = 0
1836      P:0004AD P:0004AF 0A8736            BSET    #FC0,X:DPMC
1837   
1838   
1839      P:0004AE P:0004B0 08440B            MOVEP             X:DRXR,X0               ; Get word1
1840      P:0004AF P:0004B1 440800            MOVE              X0,X:<DRXR_WD1
1841      P:0004B0 P:0004B2 08440B            MOVEP             X:DRXR,X0               ; Get word2
1842      P:0004B1 P:0004B3 440900            MOVE              X0,X:<DRXR_WD2
1843      P:0004B2 P:0004B4 08440B            MOVEP             X:DRXR,X0               ; Get word3
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 38



1844      P:0004B3 P:0004B5 440A00            MOVE              X0,X:<DRXR_WD3
1845      P:0004B4 P:0004B6 08440B            MOVEP             X:DRXR,X0               ; Get word4
1846      P:0004B5 P:0004B7 440B00            MOVE              X0,X:<DRXR_WD4
1847      P:0004B6 P:0004B8 00000C            RTS
1848   
1849                                ;---------------------------------------------------------------
1850                                READ_FROM_PCI
1851                                ;--------------------------------------------------------------
1852                                ; sub routine to read a 24 bit word in from PCI bus --> Y memory
1853                                ; 32bit host address in accumulator B.
1854   
1855                                ; read as master
1856   
1857      P:0004B7 P:0004B9 0C1890            EXTRACTU #$010010,B,A                     ; Get D31-16 bits only
                            010010
1858      P:0004B9 P:0004BB 000000            NOP
1859   
1860      P:0004BA P:0004BC 210C00            MOVE              A0,A1
1861      P:0004BB P:0004BD 000000            NOP
1862      P:0004BC P:0004BE 547000            MOVE              A1,X:DPMC               ; high 16bits of address in DSP master cntr 
reg.
                            FFFFC7
1863                                                                                    ; 32 bit read so FC1 = 0 and FC0 = 0
1864   
1865      P:0004BE P:0004C0 000000            NOP
1866      P:0004BF P:0004C1 0C1890            EXTRACTU #$010000,B,A
                            010000
1867      P:0004C1 P:0004C3 000000            NOP
1868      P:0004C2 P:0004C4 210C00            MOVE              A0,A1
1869      P:0004C3 P:0004C5 0140C2            OR      #$060000,A                        ; A1 gets written to DPAR register
                            060000
1870      P:0004C5 P:0004C7 000000            NOP                                       ; C3-C0 of DPAR=0110 for memory read
1871      P:0004C6 P:0004C8 08CC08  WRT_ADD   MOVEP             A1,X:DPAR               ; Write address to PCI bus - PCI READ action
1872      P:0004C7 P:0004C9 000000            NOP                                       ; Pipeline delay
1873      P:0004C8 P:0004CA 0A8AA2  RD_PCI    JSET    #MRRQ,X:DPSR,GET_DAT              ; If MTRQ = 1 go read the word from host via
 FIFO
                            0004D1
1874      P:0004CA P:0004CC 0A8A8A            JCLR    #TRTY,X:DPSR,RD_PCI               ; Bit is set if its a retry
                            0004C8
1875      P:0004CC P:0004CE 08F48A            MOVEP             #$0400,X:DPSR           ; Clear bit 10 = target retry bit
                            000400
1876      P:0004CE P:0004D0 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for PCI addressing to be complete
                            0004CE
1877      P:0004D0 P:0004D2 0C04C6            JMP     <WRT_ADD
1878   
1879      P:0004D1 P:0004D3 08440B  GET_DAT   MOVEP             X:DRXR,X0               ; Read 1st 16 bits of 32 bit word from host 
memory
1880      P:0004D2 P:0004D4 08450B            MOVEP             X:DRXR,X1               ; Read 2nd 16 bits of 32 bit word from host 
memory
1881   
1882                                ; note that we now have 4 bytes in X0 and X1.
1883                                ; The 32bit word was in host memory in little endian format
1884                                ; If form LSB --> MSB the bytes are b1, b2, b3, b4 in host memory
1885                                ; in progressing through the HTRX/DRXR FIFO the
1886                                ; bytes end up like this.....
1887                                ; then X0 = $00 b2 b1
1888                                ; and  X1 = $00 b4 b3
1889   
1890      P:0004D3 P:0004D5 0604A0            REP     #4                                ; increment PCI address by four bytes.
1891      P:0004D4 P:0004D6 000009            INC     B
1892      P:0004D5 P:0004D7 000000            NOP
1893      P:0004D6 P:0004D8 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 39



1894   
1895                                ;------------------------------------------------------------------------------------
1896                                RESTORE_REGISTERS
1897                                ;-------------------------------------------------------------------------------------
1898   
1899      P:0004D7 P:0004D9 05B239            MOVEC             X:<SV_SR,SR
1900   
1901      P:0004D8 P:0004DA 50A800            MOVE              X:<SV_A0,A0
1902      P:0004D9 P:0004DB 54A900            MOVE              X:<SV_A1,A1
1903      P:0004DA P:0004DC 52AA00            MOVE              X:<SV_A2,A2
1904   
1905      P:0004DB P:0004DD 51AB00            MOVE              X:<SV_B0,B0
1906      P:0004DC P:0004DE 55AC00            MOVE              X:<SV_B1,B1
1907      P:0004DD P:0004DF 53AD00            MOVE              X:<SV_B2,B2
1908   
1909      P:0004DE P:0004E0 44AE00            MOVE              X:<SV_X0,X0
1910      P:0004DF P:0004E1 45AF00            MOVE              X:<SV_X1,X1
1911   
1912      P:0004E0 P:0004E2 46B000            MOVE              X:<SV_Y0,Y0
1913      P:0004E1 P:0004E3 47B100            MOVE              X:<SV_Y1,Y1
1914   
1915      P:0004E2 P:0004E4 00000C            RTS
1916   
1917                                ;-------------------------------------------------------------------------------------
1918                                SAVE_REGISTERS
1919                                ;-------------------------------------------------------------------------------------
1920   
1921      P:0004E3 P:0004E5 053239            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1922   
1923      P:0004E4 P:0004E6 502800            MOVE              A0,X:<SV_A0
1924      P:0004E5 P:0004E7 542900            MOVE              A1,X:<SV_A1
1925      P:0004E6 P:0004E8 522A00            MOVE              A2,X:<SV_A2
1926   
1927      P:0004E7 P:0004E9 512B00            MOVE              B0,X:<SV_B0
1928      P:0004E8 P:0004EA 552C00            MOVE              B1,X:<SV_B1
1929      P:0004E9 P:0004EB 532D00            MOVE              B2,X:<SV_B2
1930   
1931      P:0004EA P:0004EC 442E00            MOVE              X0,X:<SV_X0
1932      P:0004EB P:0004ED 452F00            MOVE              X1,X:<SV_X1
1933   
1934      P:0004EC P:0004EE 463000            MOVE              Y0,X:<SV_Y0
1935      P:0004ED P:0004EF 473100            MOVE              Y1,X:<SV_Y1
1936   
1937      P:0004EE P:0004F0 00000C            RTS
1938   
1939                                ;-------------------------------------------------------
1940                                XMT_WD_FIBRE
1941                                ;-----------------------------------------------------
1942                                ; 250 MHz code - Transmit contents of Accumulator A1 to the MCE
1943                                ; we want to send 32bit word in little endian fomat to the host.
1944                                ; i.e. b4b3b2b1 goes b1, b2, b3, b4
1945                                ; currently the bytes are in this order:
1946                                ;  A1 = $00 b2 b1
1947                                ;  A0 = $00 b4 b3
1948                                ;  A = $00 00 b2 b1 00 b4 b3
1949   
1950                                ; This subroutine must take at least 160ns (4 bytes at 25Mbytes/s)
1951   
1952      P:0004EF P:0004F1 000000            NOP
1953      P:0004F0 P:0004F2 000000            NOP
1954   
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 40



1955                                ; split up 4 bytes b2, b1, b4, b3
1956   
1957      P:0004F1 P:0004F3 0C1D20            ASL     #16,A,A                           ; shift byte b2 into A2
1958      P:0004F2 P:0004F4 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1959   
1960      P:0004F4 P:0004F6 214700            MOVE              A2,Y1                   ; byte b2 in Y1
1961   
1962      P:0004F5 P:0004F7 0C1D10            ASL     #8,A,A                            ; shift byte b1 into A2
1963      P:0004F6 P:0004F8 000000            NOP
1964      P:0004F7 P:0004F9 214600            MOVE              A2,Y0                   ; byte b1 in Y0
1965   
1966      P:0004F8 P:0004FA 0C1D20            ASL     #16,A,A                           ; shift byte b4 into A2
1967      P:0004F9 P:0004FB 000000            NOP
1968      P:0004FA P:0004FC 214500            MOVE              A2,X1                   ; byte b4 in X1
1969   
1970   
1971      P:0004FB P:0004FD 0C1D10            ASL     #8,A,A                            ; shift byte b3 into A2
1972      P:0004FC P:0004FE 000000            NOP
1973      P:0004FD P:0004FF 214400            MOVE              A2,X0                   ; byte b3 in x0
1974   
1975                                ; transmit b1, b2, b3 ,b4
1976   
1977      P:0004FE P:000500 466000            MOVE              Y0,X:(R0)               ; byte b1 - off it goes
1978      P:0004FF P:000501 476000            MOVE              Y1,X:(R0)               ; byte b2 - off it goes
1979      P:000500 P:000502 446000            MOVE              X0,X:(R0)               ; byte b3 - off it goes
1980      P:000501 P:000503 456000            MOVE              X1,X:(R0)               ; byte b4 - off it goes
1981   
1982      P:000502 P:000504 000000            NOP
1983      P:000503 P:000505 000000            NOP
1984      P:000504 P:000506 00000C            RTS
1985   
1986   
1987   
1991   
1992                                PCI_BURST_NOW
1993   
1994      P:000505 P:000507 51A300            MOVE              X:<HEAD_W4_0,B0         ; LS 16bits
1995      P:000506 P:000508 0C1D91            ASL     #8,B,B
1996      P:000507 P:000509 45A200            MOVE              X:<HEAD_W4_1,X1         ; MS 16bits
1997      P:000508 P:00050A 0C1C8D            ASR     #6,B,B                            ; Size in BYTES
1998   
1999      P:000509 P:00050B 627000            MOVE              R2,X:BURST_SRC
                            000046
2000      P:00050B P:00050D 517000            MOVE              B0,X:BLOCK_SIZE
                            000042
2001   
2002      P:00050D P:00050F 0BF080            JSR     BLOCK_TRANSFER
                            00057B
2003      P:00050F P:000511 00000C            RTS
2004   
2005   
2006                                ;----------------------------------------------
2007                                FLUSH_PCI_FIFO
2008                                ;----------------------------------------------
2009      P:000510 P:000512 0A8A84            JCLR    #MARQ,X:DPSR,*
                            000510
2010      P:000512 P:000514 0A862E            BSET    #CLRT,X:DPCR
2011      P:000513 P:000515 000000            NOP
2012      P:000514 P:000516 0A86AE            JSET    #CLRT,X:DPCR,*
                            000514
2013      P:000516 P:000518 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 41



2014   
2015   
2016                                CLEAR_FIFO
2017      P:000517 P:000519 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
2018      P:000519 P:00051B 44F400            MOVE              #200000,X0
                            030D40
2019      P:00051B P:00051D 06C400            DO      X0,*+3
                            00051D
2020      P:00051D P:00051F 000000            NOP
2021      P:00051E P:000520 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
2022      P:000520 P:000522 00000C            RTS
2023   
2024   
2025                                ;-----------------------------------------------
2026                                PCI_ERROR_CLEAR
2027                                ;-----------------------------------------------
2028      
2029      
2030      
2031      
2032      
2033      
2034   
2035      P:000521 P:000523 50F000            MOVE              X:DMA_ERRORS,A0
                            000047
2036      P:000523 P:000525 000008            INC     A
2037      P:000524 P:000526 000000            NOP
2038      P:000525 P:000527 507000            MOVE              A0,X:DMA_ERRORS
                            000047
2039   
2040      P:000527 P:000529 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            000535
2041      P:000529 P:00052B 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
                            00053F
2042      P:00052B P:00052D 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            000549
2043      P:00052D P:00052F 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            000553
2044      P:00052F P:000531 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            00055D
2045      P:000531 P:000533 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            000567
2046      P:000533 P:000535 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            000571
2047   
2048                                ERROR_TRTY
2049      P:000535 P:000537 50F000            MOVE              X:EC_TRTY,A0
                            000048
2050      P:000537 P:000539 000008            INC     A
2051      P:000538 P:00053A 08F48A            MOVEP             #$0400,X:DPSR           ; Clear target retry error bit
                            000400
2052      P:00053A P:00053C 507000            MOVE              A0,X:EC_TRTY
                            000048
2053      P:00053C P:00053E 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2054      P:00053E P:000540 00000C            RTS
2055                                ERROR_TO
2056      P:00053F P:000541 50F000            MOVE              X:EC_TO,A0
                            000049
2057      P:000541 P:000543 000008            INC     A
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 42



2058      P:000542 P:000544 08F48A            MOVEP             #$0800,X:DPSR           ; Clear timeout error bit
                            000800
2059      P:000544 P:000546 507000            MOVE              A0,X:EC_TO
                            000049
2060      P:000546 P:000548 0A702F            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
2061      P:000548 P:00054A 00000C            RTS
2062                                ERROR_TDIS
2063      P:000549 P:00054B 50F000            MOVE              X:EC_TDIS,A0
                            00004A
2064      P:00054B P:00054D 000008            INC     A
2065      P:00054C P:00054E 08F48A            MOVEP             #$0200,X:DPSR           ; Clear target disconnect bit
                            000200
2066      P:00054E P:000550 507000            MOVE              A0,X:EC_TDIS
                            00004A
2067      P:000550 P:000552 0A702F            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
2068      P:000552 P:000554 00000C            RTS
2069                                ERROR_TAB
2070      P:000553 P:000555 50F000            MOVE              X:EC_TAB,A0
                            00004B
2071      P:000555 P:000557 000008            INC     A
2072      P:000556 P:000558 08F48A            MOVEP             #$0100,X:DPSR           ; Clear target abort error bit
                            000100
2073      P:000558 P:00055A 507000            MOVE              A0,X:EC_TAB
                            00004B
2074      P:00055A P:00055C 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2075      P:00055C P:00055E 00000C            RTS
2076                                ERROR_MAB
2077      P:00055D P:00055F 50F000            MOVE              X:EC_MAB,A0
                            00004C
2078      P:00055F P:000561 000008            INC     A
2079      P:000560 P:000562 08F48A            MOVEP             #$0080,X:DPSR           ; Clear master abort error bit
                            000080
2080      P:000562 P:000564 507000            MOVE              A0,X:EC_MAB
                            00004C
2081      P:000564 P:000566 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2082      P:000566 P:000568 00000C            RTS
2083                                ERROR_DPER
2084      P:000567 P:000569 50F000            MOVE              X:EC_DPER,A0
                            00004D
2085      P:000569 P:00056B 000008            INC     A
2086      P:00056A P:00056C 08F48A            MOVEP             #$0040,X:DPSR           ; Clear data parity error bit
                            000040
2087      P:00056C P:00056E 507000            MOVE              A0,X:EC_DPER
                            00004D
2088      P:00056E P:000570 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2089      P:000570 P:000572 00000C            RTS
2090                                ERROR_APER
2091      P:000571 P:000573 50F000            MOVE              X:EC_APER,A0
                            00004E
2092      P:000573 P:000575 000008            INC     A
2093      P:000574 P:000576 08F48A            MOVEP             #$0020,X:DPSR           ; Clear address parity error bit
                            000020
2094      P:000576 P:000578 507000            MOVE              A0,X:EC_APER
                            00004E
2095      P:000578 P:00057A 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2096      P:00057A P:00057C 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 43



2097   
2098   
2099                                ;----------------------------------------------
2100                                BLOCK_TRANSFER
2101                                ;----------------------------------------------
2102                                ;   In:
2103                                ;   - BLOCK_DEST_HI:BLOCK_DEST_LO is PC RAM address
2104                                ;   - BLOCK_SIZE is packet size, in bytes
2105                                ;   - BLOCK_SRC is start of data in Y memory
2106                                ;  Out:
2107                                ;   - BLOCK_SIZE will be decremented to zero.
2108                                ;   - BLOCK_DEST_HI:LO will be incremented by BLOCK_SIZE
2109                                ;   - BLOCK_SRC will be incremented by BLOCK_SIZE/2
2110                                ;  Trashes:
2111                                ;   - A and B
2112   
2113      
2114      P:00057B P:00057D 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            000042
2115   
2116      P:00057D P:00057F 014085            CMP     #0,A
2117      P:00057E P:000580 0AF0AA            JEQ     BLOCK_DONE
                            0005C2
2118   
2119      
2120   
2121      P:000580 P:000582 20001B            CLR     B
2122      P:000581 P:000583 55F000            MOVE              X:PCI_BURST_SIZE,B1
                            00005B
2123   
2124      P:000583 P:000585 200005            CMP     B,A                               ; A ? B
2125      P:000584 P:000586 0E1586            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
2126      P:000585 P:000587 21CF00            MOVE              A,B
2127                                BLOCK_TRANSFER1
2128      P:000586 P:000588 200014            SUB     B,A                               ; A -= B
2129      P:000587 P:000589 014088            ADD     #0,B                              ; Clear carry bit
2130      P:000588 P:00058A 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            000042
2131      P:00058A P:00058C 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            000043
2132      P:00058C P:00058E 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
2133   
2134      
2135      P:00058D P:00058F 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
2136      P:00058F P:000591 50F000            MOVE              X:BURST_SRC,A0
                            000046
2137      P:000591 P:000593 08C82F            MOVEP             A0,X:DSR0               ; DMA source
2138      P:000592 P:000594 200010            ADD     B,A
2139      P:000593 P:000595 00000B            DEC     B
2140      P:000594 P:000596 507000            MOVE              A0,X:BURST_SRC          ; BURST_SRC += BURST_SIZE/2
                            000046
2141   
2142      P:000596 P:000598 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
2143   
2144      
2145      P:000597 P:000599 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
2146   
2147                                BLOCK_PCI
2148      
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 44



2149      P:000599 P:00059B 200013            CLR     A
2150      P:00059A P:00059C 20001B            CLR     B
2151      P:00059B P:00059D 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            000043
2152      P:00059D P:00059F 00000B            DEC     B                                 ; n8 - 1
2153      P:00059E P:0005A0 014088            ADD     #0,B                              ; Clear carry
2154      P:00059F P:0005A1 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
2155      P:0005A0 P:0005A2 014088            ADD     #0,B                              ; Clear carry
2156      P:0005A1 P:0005A3 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
2157   
2158      P:0005A2 P:0005A4 50F000            MOVE              X:BURST_DEST_HI,A0
                            000045
2159   
2160      P:0005A4 P:0005A6 200010            ADD     B,A
2161      P:0005A5 P:0005A7 000000            NOP
2162      P:0005A6 P:0005A8 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
2163   
2164      P:0005A8 P:0005AA 280700            MOVE              #$07,A0
2165      P:0005A9 P:0005AB 014088            ADD     #0,B                              ; Clear carry
2166      P:0005AA P:0005AC 0C1D20            ASL     #16,A,A
2167      P:0005AB P:0005AD 51F000            MOVE              X:BURST_DEST_LO,B0
                            000044
2168      P:0005AD P:0005AF 200010            ADD     B,A
2169      P:0005AE P:0005B0 000000            NOP
2170   
2171      P:0005AF P:0005B1 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
2172   
2173                                BLOCK_CHECK
2174      P:0005B0 P:0005B2 000000            NOP
2175      P:0005B1 P:0005B3 000000            NOP
2176      P:0005B2 P:0005B4 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for burst termination
                            0005B2
2177   
2178      
2179      P:0005B4 P:0005B6 0A8AAE            JSET    #MDT,X:DPSR,BLOCK_OK
                            0005BD
2180   
2181      P:0005B6 P:0005B8 0D0521            JSR     PCI_ERROR_CLEAR
2182   
2183      P:0005B7 P:0005B9 0A700E            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
2184      P:0005B9 P:0005BB 0E85C3            JCS     <BLOCK_RESTART
2185   
2186      P:0005BA P:0005BC 0A700F            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
2187      P:0005BC P:0005BE 0E85C4            JCS     <BLOCK_RESUME
2188   
2189                                BLOCK_OK
2190      P:0005BD P:0005BF 50F000            MOVE              X:BURST_SIZE,A0         ; Pass # of words written to updater
                            000043
2191      P:0005BF P:0005C1 0BF080            JSR     BLOCK_UPDATE
                            0005D6
2192      P:0005C1 P:0005C3 0C057B            JMP     BLOCK_TRANSFER                    ; Finish the block
2193                                BLOCK_DONE
2194      P:0005C2 P:0005C4 00000C            RTS                                       ; Done
2195   
2196                                BLOCK_RESTART
2197      P:0005C3 P:0005C5 0C0599            JMP     BLOCK_PCI                         ; Recalculate pci and resend
2198   
2199                                BLOCK_RESUME
2200      P:0005C4 P:0005C6 200013            CLR     A
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 45



2201      P:0005C5 P:0005C7 20001B            CLR     B
2202      P:0005C6 P:0005C8 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
2203      P:0005C7 P:0005C9 0A8A8F            JCLR    #15,X:DPSR,BLOCK_RESUME1
                            0005CA
2204   
2205      P:0005C9 P:0005CB 000009            INC     B
2206   
2207                                BLOCK_RESUME1
2208   
2209      P:0005CA P:0005CC 000009            INC     B                                 ; We want N, not N-1.
2210      P:0005CB P:0005CD 014088            ADD     #0,B                              ; Clear carry
2211      P:0005CC P:0005CE 0C1C20            ASR     #16,A,A
2212      P:0005CD P:0005CF 200018            ADD     A,B                               ; B is words remaining
2213      P:0005CE P:0005D0 014088            ADD     #0,B                              ; Clear carry
2214      P:0005CF P:0005D1 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
2215      P:0005D0 P:0005D2 50F000            MOVE              X:BURST_SIZE,A0
                            000043
2216      P:0005D2 P:0005D4 200014            SUB     B,A                               ; A is words written
2217   
2218      P:0005D3 P:0005D5 0BF080            JSR     BLOCK_UPDATE
                            0005D6
2219      P:0005D5 P:0005D7 0C0599            JMP     BLOCK_PCI                         ; Recalculate pci and resend
2220   
2223                                BLOCK_UPDATE
2224      
2225      
2226   
2227      P:0005D6 P:0005D8 210500            MOVE              A0,X1                   ; Save A
2228      P:0005D7 P:0005D9 210900            MOVE              A0,B0                   ; Save A again...
2229      P:0005D8 P:0005DA 218D00            MOVE              A1,B1                   ; Save A again...
2230      P:0005D9 P:0005DB 000000            NOP
2231   
2232      P:0005DA P:0005DC 62F400            MOVE              #BURST_DEST_LO,R2
                            000044
2233      P:0005DC P:0005DE 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates BURST_DEST
                            000647
2234   
2235      P:0005DE P:0005E0 57F000            MOVE              X:BURST_SIZE,B
                            000043
2236      P:0005E0 P:0005E2 20006C            SUB     X1,B                              ; Zero flag must be preserved!
2237      P:0005E1 P:0005E3 000000            NOP
2238      P:0005E2 P:0005E4 557000            MOVE              B1,X:BURST_SIZE
                            000043
2239   
2240      P:0005E4 P:0005E6 00000C            RTS
2241   
2242   
2243                                ;----------------------------------------------;
2244                                ;  TIMER HANDLING                              ;
2245                                ;----------------------------------------------;
2246   
2247                                ; Start value is TLR, count is in TCR, int occurs at TCPR
2248                                ; Must set TCSR[TCIE] to enable int
2249                                ; Must set TCSR[T] for timer to restart
2250   
2251                                TIMER_ENABLE
2252      P:0005E5 P:0005E7 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2253      P:0005E7 P:0005E9 000000            NOP
2254      P:0005E8 P:0005EA 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2255      P:0005EA P:0005EC 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 46



2256   
2257                                TIMER_DISABLE
2258      P:0005EB P:0005ED 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
                            300200
2259      P:0005ED P:0005EF 000000            NOP
2260      P:0005EE P:0005F0 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2261      P:0005F0 P:0005F2 00000C            RTS
2262   
2264                                TIMER_ACTION
2265      P:0005F1 P:0005F3 56F000            MOVE              X:QT_INFORM_IDX,A
                            000059
2266      P:0005F3 P:0005F5 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2267      P:0005F5 P:0005F7 000000            NOP
2268      P:0005F6 P:0005F8 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2269      P:0005F8 P:0005FA 0140C5            CMP     #>0,A                             ; If inform_idx != 0
                            000000
2270      P:0005FA P:0005FC 0AF0AA            JEQ     TIMER_ACTION_OK
                            0005FE
2271      P:0005FC P:0005FE 0A7032            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2272                                TIMER_ACTION_OK
2273      P:0005FE P:000600 00000C            RTS
2274   
2275   
2276   
2277                                ;----------------------------------------------;
2278                                ;  CIRCULAR BUFFER HANDLING                    ;
2279                                ;----------------------------------------------;
2280   
2281                                BUFFER_INCR
2282   
2283      P:0005FF P:000601 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
                            000055
2284      P:000601 P:000603 014180            ADD     #1,A                              ;
2285      P:000602 P:000604 57F000            MOVE              X:QT_BUF_MAX,B          ;
                            000052
2286      P:000604 P:000606 20000D            CMP     A,B                               ;
2287      P:000605 P:000607 0AF0AF            JLE     BUFFER_RESET                      ;       head = 0
                            000611
2288                                                                                    ; else
2289      P:000607 P:000609 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            000055
2290   
2291      P:000609 P:00060B 20001B            CLR     B
2292      P:00060A P:00060C 51F000            MOVE              X:QT_BUF_SIZE,B0
                            000051
2293      P:00060C P:00060E 62F400            MOVE              #QT_DEST_LO,R2
                            000057
2294      P:00060E P:000610 0BF080            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
                            000647
2295   
2296      P:000610 P:000612 00000C            RTS
2297   
2298   
2299                                BUFFER_RESET
2300      P:000611 P:000613 62F400            MOVE              #QT_BASE_LO,R2
                            00004F
2301      P:000613 P:000615 0BF080            JSR     LOAD_HILO_ADDRESS
                            000641
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 47



2302      P:000615 P:000617 62F400            MOVE              #QT_DEST_LO,R2
                            000057
2303      P:000617 P:000619 0BF080            JSR     SAVE_HILO_ADDRESS                 ; QT_DEST_LO = QT_BASE_LO
                            000649
2304   
2305      P:000619 P:00061B 240000            MOVE              #0,X0
2306      P:00061A P:00061C 447000            MOVE              X0,X:QT_BUF_HEAD        ; HEAD = 0
                            000055
2307      P:00061C P:00061E 00000C            RTS
2308   
2309   
2310                                BUFFER_INFORM_CHECK
2311      P:00061D P:00061F 56F000            MOVE              X:QT_INFORM_IDX,A
                            000059
2312      P:00061F P:000621 014180            ADD     #1,A
2313      P:000620 P:000622 57F000            MOVE              X:QT_INFORM,B
                            000054
2314      P:000622 P:000624 20000D            CMP     A,B
2315      P:000623 P:000625 0AF0A7            JGT     BUFFER_INFORM_OK                  ; If inform_idx + 1 <= inform
                            000627
2316      P:000625 P:000627 0A7032            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2317   
2318                                BUFFER_INFORM_OK
2319      P:000627 P:000629 567000            MOVE              A,X:QT_INFORM_IDX       ; inform_idx = inform_idx + 1
                            000059
2320      P:000629 P:00062B 00000C            RTS
2321   
2322   
2323                                ;---------------------------------------------------------------
2324                                BUFFER_INFORM
2325                                ;---------------------------------------------------------------
2326                                ; Informs host of current buffer status
2327   
2328      P:00062A P:00062C 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2329      P:00062C P:00062E 440C00            MOVE              X0,X:<DTXS_WD1
2330   
2331      P:00062D P:00062F 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            000055
2332      P:00062F P:000631 440D00            MOVE              X0,X:<DTXS_WD2
2333   
2334      P:000630 P:000632 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000056
2335      P:000632 P:000634 440E00            MOVE              X0,X:<DTXS_WD3
2336   
2337      P:000633 P:000635 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
                            00005A
2338      P:000635 P:000637 440F00            MOVE              X0,X:<DTXS_WD4
2339   
2340   
2341      P:000636 P:000638 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            000640
2342      P:000638 P:00063A 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            000640
2343   
2344      P:00063A P:00063C 0D0493            JSR     PCI_MESSAGE_TO_HOST_NOW
2345   
2346      P:00063B P:00063D 0A7012            BCLR    #QT_FLUSH,X:STATUS
                            000000
2347      P:00063D P:00063F 240000            MOVE              #0,X0                   ; Reset inform index
2348      P:00063E P:000640 447000            MOVE              X0,X:QT_INFORM_IDX
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 48



                            000059
2349                                INFORM_EXIT
2350      P:000640 P:000642 00000C            RTS
2351   
2352   
2353   
2354                                ;----------------------------------------------;
2355                                ;  ADDRESS HANDLING                            ;
2356                                ;----------------------------------------------;
2357   
2361   
2362                                LOAD_HILO_ADDRESS
2363      
2364      
2365      P:000641 P:000643 200013            CLR     A
2366      P:000642 P:000644 50DA00            MOVE              X:(R2)+,A0
2367      P:000643 P:000645 44D200            MOVE              X:(R2)-,X0
2368      P:000644 P:000646 0C1940            INSERT  #$010010,X0,A
                            010010
2369      P:000646 P:000648 00000C            RTS
2370   
2371                                ADD_HILO_ADDRESS
2372      
2373      
2374   
2375      P:000647 P:000649 0D0641            JSR     LOAD_HILO_ADDRESS
2376      P:000648 P:00064A 200010            ADD     B,A
2377   
2378                                SAVE_HILO_ADDRESS
2379      
2380      
2381   
2382      P:000649 P:00064B 445A00            MOVE              X0,X:(R2)+              ; pre-increment
2383      P:00064A P:00064C 240000            MOVE              #0,X0
2384      P:00064B P:00064D 0C1D11            ASL     #8,A,B
2385      P:00064C P:00064E 0C1940            INSERT  #$008010,X0,A
                            008010
2386      P:00064E P:000650 555200            MOVE              B1,X:(R2)-              ; store hi16
2387      P:00064F P:000651 506200            MOVE              A0,X:(R2)
2388      P:000650 P:000652 0C1C90            ASR     #8,B,A
2389      P:000651 P:000653 00000C            RTS
2390   
2391   
2392                                ; TEST_ROUTINES                 ; Passed.
2393   
2394                                ;       MOVE    #$01,A1
2395                                ;       MOVE    #$020304,A0
2396                                ;       MOVE    #BDEBUG0,R2
2397   
2398                                ;       JSR     SAVE_HILO_ADDRESS
2399   
2400                                ;       JSR     LOAD_HILO_ADDRESS
2401                                ;       MOVE    #BDEBUG2,R2
2402                                ;       JSR     SAVE_HILO_ADDRESS
2403   
2404                                ;       MOVE    #$02,B1
2405                                ;       MOVE    #$000203,B0
2406                                ;       MOVE    #BDEBUG0,R2
2407                                ;       JSR     ADD_HILO_ADDRESS
2408   
2409                                ;       RTS
2410   
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 49



2411   
2412                                BOOTCODE_END
2413                                 BOOTEND_ADDR
2414      000652                              EQU     @CVI(BOOTCODE_END)
2415   
2416                                PROGRAM_END
2417      000652                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2418                                ;---------------------------------------------
2419   
2420   
2421                                ; --------------------------------------------------------------------
2422                                ; --------------- x memory parameter table ---------------------------
2423                                ; --------------------------------------------------------------------
2424   
2425      X:000000 P:000654                   ORG     X:VAR_TBL,P:
2426   
2427   
2428                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2429                                 VAR_TBL_START
2430      000652                              EQU     @LCV(L)-2
2431                                          ENDIF
2432   
2433                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2435                                          ENDIF
2436   
2437                                ; -----------------------------------------------
2438                                ; do not move these (X:0 --> x:3)
2439 d    X:000000 P:000654 000000  STATUS    DC      0
2440 d                               FRAME_COUNT
2441 d    X:000001 P:000655 000000            DC      0                                 ; used as a check....... increments for ever
y frame write.....must be cleared by host.
2442 d                               PRE_CORRUPT
2443 d    X:000002 P:000656 000000            DC      0
2444   
2445 d    X:000003 P:000657 550104  REV_NUMBER DC     $550104                           ; byte 0 = minor revision #
2446                                                                                    ; byte 1 = major revision #
2447                                                                                    ; byte 2 = release Version (ascii letter)
2448 d    X:000004 P:000658 000000  REV_DATA  DC      $000000                           ; data: day-month-year
2449 d    X:000005 P:000659 2EF490  P_CHECKSUM DC     $2EF490                           ;**** DO NOT CHANGE
2450                                ; -------------------------------------------------
2451 d    X:000006 P:00065A 000000  WORD_COUNT DC     0                                 ; word count.  Number of words successfully 
writen to host in last packet.
2452 d    X:000007 P:00065B 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2453                                ; ----------------------------------------------------------------------------------------------
----------------
2454   
2455 d    X:000008 P:00065C 000000  DRXR_WD1  DC      0
2456 d    X:000009 P:00065D 000000  DRXR_WD2  DC      0
2457 d    X:00000A P:00065E 000000  DRXR_WD3  DC      0
2458 d    X:00000B P:00065F 000000  DRXR_WD4  DC      0
2459 d    X:00000C P:000660 000000  DTXS_WD1  DC      0
2460 d    X:00000D P:000661 000000  DTXS_WD2  DC      0
2461 d    X:00000E P:000662 000000  DTXS_WD3  DC      0
2462 d    X:00000F P:000663 000000  DTXS_WD4  DC      0
2463   
2464 d    X:000010 P:000664 000000  PCI_WD1_1 DC      0
2465 d    X:000011 P:000665 000000  PCI_WD1_2 DC      0
2466 d    X:000012 P:000666 000000  PCI_WD2_1 DC      0
2467 d    X:000013 P:000667 000000  PCI_WD2_2 DC      0
2468 d    X:000014 P:000668 000000  PCI_WD3_1 DC      0
2469 d    X:000015 P:000669 000000  PCI_WD3_2 DC      0
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 50



2470 d    X:000016 P:00066A 000000  PCI_WD4_1 DC      0
2471 d    X:000017 P:00066B 000000  PCI_WD4_2 DC      0
2472 d    X:000018 P:00066C 000000  PCI_WD5_1 DC      0
2473 d    X:000019 P:00066D 000000  PCI_WD5_2 DC      0
2474 d    X:00001A P:00066E 000000  PCI_WD6_1 DC      0
2475 d    X:00001B P:00066F 000000  PCI_WD6_2 DC      0
2476   
2477   
2478 d    X:00001C P:000670 000000  HEAD_W1_1 DC      0
2479 d    X:00001D P:000671 000000  HEAD_W1_0 DC      0
2480 d    X:00001E P:000672 000000  HEAD_W2_1 DC      0
2481 d    X:00001F P:000673 000000  HEAD_W2_0 DC      0
2482 d    X:000020 P:000674 000000  HEAD_W3_1 DC      0
2483 d    X:000021 P:000675 000000  HEAD_W3_0 DC      0
2484 d    X:000022 P:000676 000000  HEAD_W4_1 DC      0
2485 d    X:000023 P:000677 000000  HEAD_W4_0 DC      0
2486   
2487   
2488 d    X:000024 P:000678 000000  REP_WD1   DC      0
2489 d    X:000025 P:000679 000000  REP_WD2   DC      0
2490 d    X:000026 P:00067A 000000  REP_WD3   DC      0
2491 d    X:000027 P:00067B 000000  REP_WD4   DC      0
2492   
2493 d    X:000028 P:00067C 000000  SV_A0     DC      0
2494 d    X:000029 P:00067D 000000  SV_A1     DC      0
2495 d    X:00002A P:00067E 000000  SV_A2     DC      0
2496 d    X:00002B P:00067F 000000  SV_B0     DC      0
2497 d    X:00002C P:000680 000000  SV_B1     DC      0
2498 d    X:00002D P:000681 000000  SV_B2     DC      0
2499 d    X:00002E P:000682 000000  SV_X0     DC      0
2500 d    X:00002F P:000683 000000  SV_X1     DC      0
2501 d    X:000030 P:000684 000000  SV_Y0     DC      0
2502 d    X:000031 P:000685 000000  SV_Y1     DC      0
2503   
2504 d    X:000032 P:000686 000000  SV_SR     DC      0                                 ; stauts register save.
2505   
2506 d    X:000033 P:000687 000000  ZERO      DC      0
2507 d    X:000034 P:000688 000001  ONE       DC      1
2508 d    X:000035 P:000689 000004  FOUR      DC      4
2509   
2510   
2511   
2512 d                               PACKET_SIZE_LOW
2513 d    X:000036 P:00068A 000000            DC      0
2514 d                               PACKET_SIZE_HIH
2515 d    X:000037 P:00068B 000000            DC      0
2516   
2517 d    X:000038 P:00068C 00A5A5  PREAMB1   DC      $A5A5                             ; pramble 16-bit word....2 of which make up 
first preamble 32bit word
2518 d    X:000039 P:00068D 005A5A  PREAMB2   DC      $5A5A                             ; preamble 16-bit word....2 of which make up
 second preamble 32bit word
2519 d    X:00003A P:00068E 004441  DATA_WD   DC      $4441                             ; "DA"
2520 d    X:00003B P:00068F 005250  REPLY_WD  DC      $5250                             ; "RP"
2521   
2522 d                               TOTAL_BUFFS
2523 d    X:00003C P:000690 000000            DC      0                                 ; total number of 512 buffers in packet
2524 d                               LEFT_TO_READ
2525 d    X:00003D P:000691 000000            DC      0                                 ; number of words (16 bit) left to read afte
r last 512 buffer
2526 d                               LEFT_TO_WRITE
2527 d    X:00003E P:000692 000000            DC      0                                 ; number of woreds (32 bit) to write to host
 i.e. half of those left over read
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 51



2528 d                               NUM_LEFTOVER_BLOCKS
2529 d    X:00003F P:000693 000000            DC      0                                 ; small block DMA burst transfer
2530   
2531 d                               DATA_DLY_VAL
2532 d    X:000040 P:000694 000000            DC      0                                 ; data delay value..  Delay added to first f
rame received after GO command
2533 d    X:000041 P:000695 000200  CONSTORE  DC      $200
2534   
2536   
2537 d    X:000042 P:000696 000000  BLOCK_SIZE DC     0
2538 d    X:000043 P:000697 000000  BURST_SIZE DC     0
2539 d                               BURST_DEST_LO
2540 d    X:000044 P:000698 000000            DC      0
2541 d                               BURST_DEST_HI
2542 d    X:000045 P:000699 000000            DC      0
2543 d    X:000046 P:00069A 000000  BURST_SRC DC      0
2544   
2545 d    X:000047 P:00069B 000000  DMA_ERRORS DC     0
2546 d    X:000048 P:00069C 000000  EC_TRTY   DC      0
2547 d    X:000049 P:00069D 000000  EC_TO     DC      0
2548 d    X:00004A P:00069E 000000  EC_TDIS   DC      0
2549 d    X:00004B P:00069F 000000  EC_TAB    DC      0
2550 d    X:00004C P:0006A0 000000  EC_MAB    DC      0
2551 d    X:00004D P:0006A1 000000  EC_DPER   DC      0
2552 d    X:00004E P:0006A2 000000  EC_APER   DC      0
2553   
2555   
2556 d    X:00004F P:0006A3 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2557 d    X:000050 P:0006A4 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2558 d                               QT_BUF_SIZE
2559 d    X:000051 P:0006A5 000000            DC      0                                 ; Separation of buffers, in bytes
2560 d    X:000052 P:0006A6 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2561 d                               QT_FRAME_SIZE
2562 d    X:000053 P:0006A7 000000            DC      0                                 ; Expected data packet size, in bytes
2563 d    X:000054 P:0006A8 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2564   
2565 d                               QT_BUF_HEAD
2566 d    X:000055 P:0006A9 000000            DC      0                                 ; Index of buf for next write
2567 d                               QT_BUF_TAIL
2568 d    X:000056 P:0006AA 000000            DC      0                                 ; Index at which we must not write
2569   
2570 d    X:000057 P:0006AB 000000  QT_DEST_LO DC     0                                 ; PC address for next write
2571 d    X:000058 P:0006AC 000000  QT_DEST_HI DC     0                                 ;
2572 d                               QT_INFORM_IDX
2573 d    X:000059 P:0006AD 000000            DC      0                                 ; Number of packets since last inform
2574 d    X:00005A P:0006AE 000000  QT_DROPS  DC      0                                 ; Dropped packets
2575   
2577 d                               PCI_BURST_SIZE
2578 d    X:00005B P:0006AF 000040            DC      $40                               ; Should be < 4*latency assigned by OS
2579   
2580 d    X:00005C P:0006B0 000000  TEMP_PSIZE DC     0
2581   
2582 d    X:00005D P:0006B1 000000  BDEBUG0   DC      0
2583 d    X:00005E P:0006B2 000000  BDEBUG1   DC      0
2584 d    X:00005F P:0006B3 000000  BDEBUG2   DC      0
2585 d    X:000060 P:0006B4 000000  BDEBUG3   DC      0
2586 d    X:000061 P:0006B5 000000  BDEBUG4   DC      0
2587 d    X:000062 P:0006B6 000000  BDEBUG5   DC      0
2588 d    X:000063 P:0006B7 000000  BDEBUG6   DC      0
2589 d    X:000064 P:0006B8 000000  BDEBUG7   DC      0
2590 d    X:000065 P:0006B9 000000  BDEBUG8   DC      0
2591 d    X:000066 P:0006BA 000000  BDEBUG9   DC      0
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  main.asm  Page 52



2592   
2593                                ;----------------------------------------------------------
2594   
2595   
2596   
2597                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2598                                 VAR_TBL_END
2599      0006B9                              EQU     @LCV(L)-2
2600                                          ENDIF
2601   
2602                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2604                                          ENDIF
2605   
2606                                 VAR_TBL_LENGTH
2607      000067                              EQU     VAR_TBL_END-VAR_TBL_START
2608   
2609   
2610                                          IF      @CVS(N,*)>=APPLICATION
2612                                          ENDIF
2613   
2614   
2615                                ;--------------------------------------------
2616                                ; APPLICATION AREA
2617                                ;---------------------------------------------
2618                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2619      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2620                                          ENDIF
2621   
2622                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2624                                          ENDIF
2625   
2626                                ; starts with no application loaded
2627                                ; so just reply with an error if we get a GOA command
2628      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2629      P:000802 P:000804 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
2630      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2631      P:000805 P:000807 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2632      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2633      P:000808 P:00080A 440E00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2634      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2635      P:00080B P:00080D 440F00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2636      P:00080C P:00080E 0D04D7            JSR     <RESTORE_REGISTERS
2637      P:00080D P:00080F 0D048F            JSR     <PCI_MESSAGE_TO_HOST
2638      P:00080E P:000810 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS
2639      P:00080F P:000811 0C0175            JMP     PACKET_IN
2640   
2641   
2643   
2644   
2645   
2646   
2647   
2648      000812                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM
2649   
2650   
2651   

0    Errors
Motorola DSP56300 Assembler  Version 6.3.4   07-12-03  14:43:05  build.asm  Page 53



0    Warnings


