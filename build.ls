Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  build.asm  Page 1



1                                  COMMENT *
2      
3                          Main source file.
4      
5                          Project:     UBC Multi-Channel Electronics
6                          Target:      250MHz SDSU PCI card - DSP56301
7      
8                          Original code by Bob Leach (Astro-cam) and David Atkinson (ATC).
9                          Versions U0101+ maintained by Matthew Hasselfield (UBC)
10     
11                         Version:     Release Version U (1.5)
12     
13     
14                         Assembler directives:
15                                 ROM=EEPROM => EEPROM CODE
16                                 ROM=ONCE => ONCE CODE
17     
18                                 *
19                                   PAGE    132                               ; Printronix page width - 132 columns
20                                   OPT     CEX                               ; print DC evaluations
21     
22                                   INCLUDE 'info.asm'
23                                 COMMENT *
24     
25                         Project:     SCUBA2 Multi-Channel Electronics
26                         Target:      250MHz SDSU PCI card - DSP56301
27                         Version:     U0105
28     
29                         Original code by Bob Leach (Astronomical Research Cameras)
30                         SCUBA2 series by David Atkinson (ATCUK)
31                         Versions U0101+ by Matthew Hasselfield (UBC)
32     
33                         Assembler directives:
34                                 ROM=EEPROM => EEPROM CODE
35                                 ROM=ONCE => ONCE CODE
36     
37                                 *
38                                   INCLUDE 'header.asm'
39                               COMMENT *
40     
41                         Defines for DSP register addresses.
42     
43                         See info.asm for versioning and authors.
44     
45                                 *
46                                   PAGE    132                               ; Printronix page width - 132 columns
47                                   OPT     CEX                               ; print DC evaluations
48     
49                         ; Equates to define the X: memory tables
50        000000           VAR_TBL   EQU     0                                 ; Variables and constants table
51     
52                         APPLICATION
53        000800                     EQU     $800                              ; application memory start location in P memory
54                                                                             ; note applications should start with this address
55                                                                             ; and end with a JMP to PACKET_IN
56                                                                             ; if only want appl to run once
57                                                                             ; penultimate line of code should be
58                                                                             ; to clear bit APPLICATION_LOADED in STATUS
59                                                                             ; otherwise will run continusly until 'STP'
60                                                                             ; command is sent
61     
62        000200           APPL_PARAM EQU    $200                              ; application parameters in x memory start here.
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  header.asm  Page 2



63     
64     
65        000200           HF_FIFO   EQU     512                               ; number of 16 bit words in a half full FIFO
66        000020           SMALL_BLK EQU     32                                ; small block burst size for < 512 pixels
67     
69                         IMAGE_BUFFER
70        000000                     EQU     0                                 ; Data frame buffer offset.
71                         REPLY_BUFFER
72        100000                     EQU     $100000                           ; Buffer MCE replies at 1M
73                         COMMAND_BUFFER
74        200000                     EQU     $200000                           ; Buffer MCE commands at 2M
75                         TIMER_BUFFER
76        201000                     EQU     $201000
77                         TIMER_BUFFER_END
78        202000                     EQU     $202000
79     
80     
81                         ; HST timeout recovery....
82     
83        000200           MAX_DUMP  EQU     512                               ; if HST timeout.. max number that could be in FIFO i
s 511..
84        001000           DUMP_BUFF EQU     $1000                             ; store in Y memory above normal data buffer: in off-
chip RAM
85     
86     
87     
88                         ; Various addressing control registers
89        FFFFFB           BCR       EQU     $FFFFFB                           ; Bus Control Register
90        FFFFFA           DCR       EQU     $FFFFFA                           ; DRAM Control Register
91        FFFFF9           AAR0      EQU     $FFFFF9                           ; Address Attribute Register, channel 0
92        FFFFF8           AAR1      EQU     $FFFFF8                           ; Address Attribute Register, channel 1
93        FFFFF7           AAR2      EQU     $FFFFF7                           ; Address Attribute Register, channel 2
94        FFFFF6           AAR3      EQU     $FFFFF6                           ; Address Attribute Register, channel 3
95        FFFFFD           PCTL      EQU     $FFFFFD                           ; PLL control register
96        FFFFFE           IPRP      EQU     $FFFFFE                           ; Interrupt Priority register - Peripheral
97        FFFFFF           IPRC      EQU     $FFFFFF                           ; Interrupt Priority register - Core
98     
99                         ; PCI control register
100       FFFFCD           DTXS      EQU     $FFFFCD                           ; DSP Slave transmit data FIFO
101       FFFFCC           DTXM      EQU     $FFFFCC                           ; DSP Master transmit data FIFO
102       FFFFCB           DRXR      EQU     $FFFFCB                           ; DSP Receive data FIFO
103       FFFFCA           DPSR      EQU     $FFFFCA                           ; DSP PCI Status Register
104       FFFFC9           DSR       EQU     $FFFFC9                           ; DSP Status Register
105       FFFFC8           DPAR      EQU     $FFFFC8                           ; DSP PCI Address Register
106       FFFFC7           DPMC      EQU     $FFFFC7                           ; DSP PCI Master Control Register
107       FFFFC6           DPCR      EQU     $FFFFC6                           ; DSP PCI Control Register
108       FFFFC5           DCTR      EQU     $FFFFC5                           ; DSP Control Register
109    
110                        ; Port E is the Synchronous Communications Interface (SCI) port
111       FFFF9F           PCRE      EQU     $FFFF9F                           ; Port Control Register
112       FFFF9E           PRRE      EQU     $FFFF9E                           ; Port Direction Register
113       FFFF9D           PDRE      EQU     $FFFF9D                           ; Port Data Register
114    
115                        ; Various PCI register bit equates
116       000001           STRQ      EQU     1                                 ; Slave transmit data request (DSR)
117       000002           SRRQ      EQU     2                                 ; Slave receive data request (DSR)
118       000017           HACT      EQU     23                                ; Host active, low true (DSR)
119       000001           MTRQ      EQU     1                                 ; Set whem master transmitter is not full (DPSR)
120       000004           MARQ      EQU     4                                 ; Master address request (DPSR)
121       000002           MRRQ      EQU     2                                 ; Master Receive Request (DPSR)
122       00000A           TRTY      EQU     10                                ; PCI Target Retry (DPSR)
123    
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  header.asm  Page 3



124       000005           APER      EQU     5                                 ; Address parity error
125       000006           DPER      EQU     6                                 ; Data parity error
126       000007           MAB       EQU     7                                 ; Master Abort
127       000008           TAB       EQU     8                                 ; Target Abort
128       000009           TDIS      EQU     9                                 ; Target Disconnect
129       00000B           TO        EQU     11                                ; Timeout
130       00000E           MDT       EQU     14                                ; Master Data Transfer complete
131       00000F           RDCQ      EQU     15                                ; Remaining Data Count Qualifier
132    
133       000002           SCLK      EQU     2                                 ; SCLK = transmitter special code
134    
135                        ; bits in DPMC
136    
137       000017           FC1       EQU     23
138       000016           FC0       EQU     22
139    
140    
141                        ; DMA register definitions
142       FFFFEF           DSR0      EQU     $FFFFEF                           ; Source address register
143       FFFFEE           DDR0      EQU     $FFFFEE                           ; Destination address register
144       FFFFED           DCO0      EQU     $FFFFED                           ; Counter register
145       FFFFEC           DCR0      EQU     $FFFFEC                           ; Control register
146    
147                        ; DCTR bits
148       000000           DCTR_HCIE EQU     0                                 ; Interrupt enable
149       000003           DCTR_HF3  EQU     3                                 ; used as a semaphore for INTA handshaking
150       000004           DCTR_HF4  EQU     4                                 ; indicates that DSP will hand-shake ints
151       000005           DCTR_HF5  EQU     5                                 ;
152       000006           INTA      EQU     6                                 ; Request PCI interrupt
153    
154                        ; The DSR host flags are written by the PCI host and read by the DSP
155       000003           DSR_HF0   EQU     3                                 ; PC side INTA hand-shaking
156       000004           DSR_HF1   EQU     4                                 ; PC side hand-shaking enabled
157       000005           DSR_HF2   EQU     5                                 ; PC side INTA disable (polling mode)
158    
159                        ; DPCR bit definitions
160       00000E           CLRT      EQU     14                                ; Clear transmitter
161       000012           MACE      EQU     18                                ; Master access counter enable
162       000015           IAE       EQU     21                                ; Insert Address Enable
163    
164    
165    
166                        ; Addresses of ESSI port
167       FFFFBC           TX00      EQU     $FFFFBC                           ; Transmit Data Register 0
168       FFFFB7           SSISR0    EQU     $FFFFB7                           ; Status Register
169       FFFFB6           CRB0      EQU     $FFFFB6                           ; Control Register B
170       FFFFB5           CRA0      EQU     $FFFFB5                           ; Control Register A
171    
172                        ; SSI Control Register A Bit Flags
173       000006           TDE       EQU     6                                 ; Set when transmitter data register is empty
174    
175                        ; Miscellaneous addresses
176       FFFFFF           RDFIFO    EQU     $FFFFFF                           ; Read the FIFO for incoming fiber optic data
177    
178                        ; Timer registers
179       FFFF8F           TCSR0     EQU     $FFFF8F                           ; Control and status register
180       FFFF8E           TLR0      EQU     $FFFF8E                           ; Load register
181       FFFF8D           TCPR0     EQU     $FFFF8D                           ; Compare register
182       FFFF8C           TCR0      EQU     $FFFF8C                           ; Count register
183       FFFF8B           TCSR1     EQU     $FFFF8B                           ; Control and status register
184       FFFF8A           TLR1      EQU     $FFFF8A                           ; Load register
185       FFFF89           TCPR1     EQU     $FFFF89                           ; Compare register
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  header.asm  Page 4



186       FFFF88           TCR1      EQU     $FFFF88                           ; Count register
187       FFFF87           TCSR2     EQU     $FFFF87                           ; Control and status register
188       FFFF86           TLR2      EQU     $FFFF86                           ; Load register
189       FFFF85           TCPR2     EQU     $FFFF85                           ; Compare register
190       FFFF84           TCR2      EQU     $FFFF84                           ; Count register
191    
192                        ;***************************************************************
193                        ; Phase Locked Loop initialization
194       050003           PLL_INIT  EQU     $050003                           ; PLL = 25 MHz x 4 = 100 MHz
195                        ;****************************************************************
196    
197                        ; Port C is Enhanced Synchronous Serial Port 0
198       FFFFBF           PCRC      EQU     $FFFFBF                           ; Port C Control Register
199       FFFFBE           PRRC      EQU     $FFFFBE                           ; Port C Data direction Register
200       FFFFBD           PDRC      EQU     $FFFFBD                           ; Port C GPIO Data Register
201    
202                        ; Port D is Enhanced Synchronous Serial Port 1
203       FFFFAF           PCRD      EQU     $FFFFAF                           ; Port D Control Register
204       FFFFAE           PRRD      EQU     $FFFFAE                           ; Port D Data direction Register
205       FFFFAD           PDRD      EQU     $FFFFAD                           ; Port D GPIO Data Register
206    
207                        ; Bit number definitions of GPIO pins on Port C
208       000002           ROM_FIFO  EQU     2                                 ; Select ROM or FIFO accesses for AA1
209       000004           AUX1      EQU     4                                 ; enable/disable byte swapping
210    
211                        ; Bit number definitions of GPIO pins on Port D
212       000000           EF        EQU     0                                 ; FIFO Empty flag, low true
213       000001           HF        EQU     1                                 ; FIFO half full flag, low true
214       000002           RS        EQU     2                                 ; FIFO reset signal, low true
215       000003           FSYNC     EQU     3                                 ; High during image transmission
216       000005           WRFIFO    EQU     5                                 ; Low true if FIFO is being written to
217    
218    
219                        ; Errors - self test application
220    
221       000000           Y_MEM_ER  EQU     0                                 ; y memory corrupted
222       000001           X_MEM_ER  EQU     1                                 ; x memory corrupted
223       000002           P_MEM_ER  EQU     2                                 ; p memory corrupted
224       000003           FO_EMPTY  EQU     3                                 ; no transmitted data in FIFO
225    
226       000004           FO_OVER   EQU     4                                 ; too much data received
227       000005           FO_UNDER  EQU     5                                 ; not enough data receiv
228       000006           FO_RX_ER  EQU     6                                 ; received data in FIFO incorrect.
229       000007           DEBUG     EQU     7                                 ; debug bit
230    
231    
233       000000           TE        EQU     0
234       000001           TOIE      EQU     1
235       000002           TCIE      EQU     2
236       000014           TOF       EQU     20
237       000015           TCF       EQU     21
238    
239    
241    
242       FFF000           FO_SEND   EQU     $FFF000
243    
244                        ;--------------------------------------------------------------------
245                        ; Interrupt configuration
246                        ;--------------------------------------------------------------------
247                        ;  IPRC determines core interrupt modes and levels.
248                        ;   - [5:3] IRQB mode|level - FIFO half full
249                        ;   - [8:6] IRQC mode|level - reset switch
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  header.asm  Page 5



250                        ;  So $1C0 is 111|000|000 = IRQC level-triggered priority 3 and IRQB disabled.
251                        ;
252                        ;  IPRP determines peripheral interrupt levels.
253                        ;   - [1:0] HI-32 level.  Must be higher than SR IPL mask
254                        ;  So set to 2, and ensure SR[I] > 1.
255                        ;
256                        ;  SR determines many things... but most importantly
257                        ;   - [9:8] Interrupt mask - must be smaller than HI-32 IPL
258                        ;  So set to $100
259    
260       0001C0           MY_IPRC   EQU     $0001C0
261       000002           MY_IPRP   EQU     $000002
262       000100           MY_SR     EQU     $000100
263    
264                                  INCLUDE 'init.asm'
265                                COMMENT *
266    
267                        Initial configuration, and ISR vector definitions.
268    
269                        See info.asm for versioning and authors.
270    
271                                *
272                                  PAGE    132                               ; Printronix page width - 132 columns
273                                  OPT     CEX                               ; print DC evaluations
274    
275                        ; The EEPROM boot code expects first to read 3 bytes specifying the number of
276                        ; program words, then 3 bytes specifying the address to start loading the
277                        ; program words and then 3 bytes for each program word to be loaded.
278                        ; The program words will be condensed into 24 bit words and stored in contiguous
279                        ; PRAM memory starting at the specified starting address. Program execution
280                        ; starts from the same address where loading started.
281    
282                        ; Special address for two words for the DSP to bootstrap code from the EEPROM
283                                  IF      @SCP("ROM","ROM")                 ; Boot from ROM on power-on
284       P:000000 P:000000                   ORG     P:0,P:0
285  d    P:000000 P:000000 000810            DC      END_ADR-INIT-2                    ; Number of boot words
286  d    P:000001 P:000001 000000            DC      INIT                              ; Starting address
287       P:000000 P:000002                   ORG     P:0,P:2
288       P:000000 P:000002 0C0030  INIT      JMP     <INIT_PCI                         ; Configure PCI port
289       P:000001 P:000003 000000            NOP
290                                           ENDIF
291    
292    
293                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
294                                 ; command converter
295                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
299                                           ENDIF
300    
301                                 ; Vectored interrupt table, addresses at the beginning are reserved
302  d    P:000002 P:000004 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0       ; $02-$0f Reserved
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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  init.asm  Page 6



     d                      000000
303  d    P:000010 P:000012 000000            DC      0,0                               ; $10-$13 Reserved
     d                      000000
304    
305                                 ; FIFO HF* flag interrupt vector is here at $12 - this is connected to the
306                                 ; IRQB* interrupt line so its ISR vector must be here
307  d    P:000012 P:000014 000000            DC      0,0                               ; $was ld scatter routine ...HF*
     d                      000000
308    
309                                 ; a software reset button on the font panel of the card is connected to the IRQC*
310                                 ; line which if pressed causes the DSP to jump to an ISR which causes the program
311                                 ; counter to the beginning of the program INIT and sets the stack pointer to TOP.
312       P:000014 P:000016 0BF080            JSR     CLEAN_UP_PCI                      ; $14 - Software reset switch
                            000432
313    
314  d    P:000016 P:000018 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Reserved interrupts
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
315  d    P:000022 P:000024 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0
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
316    
317                                 ; Now we're at P:$30, where some unused vector addresses are located
318                                 ; This is ROM only code that is only executed once on power-up when the
319                                 ; ROM code is downloaded. It is skipped over on OnCE downloads.
320    
321                                 ; A few seconds after power up on the Host, it interrogates the PCI bus to find
322                                 ; out what boards are installed and configures this PCI board. The EEPROM booting
323                                 ; procedure ends with program execution  starting at P:$0 where the EEPROM has
324                                 ; inserted a JMP INIT_PCI instruction. This routine sets the PLL paramter and
325                                 ; does a self configuration and software reset of the PCI controller in the DSP.
326                                 ; After configuring the PCI controller the DSP program overwrites the instruction
327                                 ; at P:$0 with a new JMP START to skip over the INIT_PCI routine. The program at
328                                 ; START address begins configuring the DSP and processing commands.
329                                 ; Similarly the ONCE option places a JMP START at P:$0 to skip over the
330                                 ; INIT_PCI routine. If this routine where executed after the host computer had booted
331                                 ; it would cause it to crash since the host computer would overwrite the
332                                 ; configuration space with its own values and doesn't tolerate foreign values.
333    
334                                 ; Initialize the PLL - phase locked loop
335                                 INIT_PCI
336       P:000030 P:000032 08F4BD            MOVEP             #PLL_INIT,X:PCTL        ; Initialize PLL
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  init.asm  Page 7



                            050003
337       P:000032 P:000034 000000            NOP
338    
339                                 ; Program the PCI self-configuration registers
340       P:000033 P:000035 240000            MOVE              #0,X0
341       P:000034 P:000036 08F485            MOVEP             #$500000,X:DCTR         ; Set self-configuration mode
                            500000
342       P:000036 P:000038 0604A0            REP     #4
343       P:000037 P:000039 08C408            MOVEP             X0,X:DPAR               ; Dummy writes to configuration space
344       P:000038 P:00003A 08F487            MOVEP             #>$0000,X:DPMC          ; Subsystem ID
                            000000
345       P:00003A P:00003C 08F488            MOVEP             #>$0000,X:DPAR          ; Subsystem Vendor ID
                            000000
346    
347                                 ; PCI Personal reset
348       P:00003C P:00003E 08C405            MOVEP             X0,X:DCTR               ; Personal software reset
349       P:00003D P:00003F 000000            NOP
350       P:00003E P:000040 000000            NOP
351       P:00003F P:000041 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00003F
352       P:000041 P:000043 07F084            MOVE              P:(*+3),X0              ; Trick to write "JMP <START" to P:0
                            000044
353       P:000043 P:000045 070004            MOVE              X0,P:(0)
354       P:000044 P:000046 0C0100            JMP     <START
355    
356  d    P:000045 P:000047 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
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
357  d    P:000051 P:000053 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
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
358  d    P:00005D P:00005F 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; $60-$71 Reserved PCI
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
359    
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  init.asm  Page 8



360                                 ;**************************************************************************
361                                 ; Check for program space overwriting of ISR starting at P:$72
362                                           IF      @CVS(N,*)>$71
364                                           ENDIF
365    
366       P:000072 P:000074                   ORG     P:$72,P:$74
367    
368                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
369                                 ; command converter
370                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
372                                           ENDIF
373    
374    
375                                 ;**************************************************************************
376    
377                                 ; Three non-maskable fast interrupt service routines for clearing PCI interrupts
378                                 ; The Host will use these to clear the INTA* after it has serviced the interrupt
379                                 ; which had been generated by the PCI board.
380    
381       P:000072 P:000074 0A8506            BCLR    #INTA,X:DCTR                      ; $72/3 - Clear PCI interrupt
382       P:000073 P:000075 000000            NOP
383    
384       P:000074 P:000076 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; clear interrupt flag
385       P:000075 P:000077 000000            NOP                                       ; needs to be fast addressing <
386    
387       P:000076 P:000078 0A0022            BSET    #FATAL_ERROR,X:<STATUS            ; $76/7 - driver PCI_MESSAGE_TO_HOST error
388       P:000077 P:000079 000000            NOP
389    
390                                 ; Interrupt locations for 7 available commands on PCI board
391                                 ; Each JSR takes up 2 locations in the table
392       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            000340
393       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            000315
394       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            000361
395       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            00036A
396                                 ; software reset is the same as cleaning up the PCI - use same routine
397                                 ; when HOST does a RESET then this routine is run
398       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            000449
399       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            000461
400       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            00043A
401       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            000375
402    
403                                 ; QT - set command
404       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            000393
405       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            00042A
406    
407                                 ; Quiet RP mode, clear buffer full flag
408       P:00008C P:00008E 0A0015            BCLR    #RP_BUFFER_FULL,X:<STATUS         ; $8C
409       P:00008D P:00008F 000000            NOP
410    
411                                 ; ; Mode setting pretty-fast interrupt
412                                 ;       JSR     MODE_SET_FAST                   ; $8E
413    
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  init.asm  Page 9



414                                 ; ***********************************************************************
415                                 ; For now have boot code starting from P:$100
416                                 ; just to make debugging tidier etc.
417    
418       P:000100 P:000102                   ORG     P:$100,P:$102
419    
420                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
421                                 ; command converter
422                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
424                                           ENDIF
425                                 ; ***********************************************************************
426    
427    
428    
429                                 ; ******************************************************************
430                                 ;
431                                 ;       AA0 = RDFIFO* of incoming fiber optic data
432                                 ;       AA1 = EEPROM access
433                                 ;       AA2 = DRAM access
434                                 ;       AA3 = output to parallel data connector, for a video pixel clock
435                                 ;       $FFxxxx = Write to fiber optic transmitter
436                                 ;
437                                 ; ******************************************************************
438    
439    
440       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
441                                 ;       MOVEP   #>$100000,X:DCTR                ; Set PCI mode
442       P:000102 P:000104 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
443       P:000103 P:000105 0A8515            BCLR    #21,X:DCTR
444       P:000104 P:000106 0A8516            BCLR    #22,X:DCTR
445       P:000105 P:000107 000000            NOP
446       P:000106 P:000108 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
447       P:000107 P:000109 000000            NOP
448       P:000108 P:00010A 000000            NOP                                       ; End of PCI programming
449    
450    
451                                 ; Set operation mode register OMR to normal expanded
452       P:000109 P:00010B 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
453       P:00010A P:00010C 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
454    
455                                 ; Program the serial port ESSI0 = Port C for serial transmission to
456                                 ;   the timing board
457       P:00010B P:00010D 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
458                                 ;**********************************************************************
459       P:00010D P:00010F 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
460                                                                                     ; DC0-CD4 = 0 for non-network operation
461                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
462                                                                                     ; SSC1 = 0 for SC1 not used
463                                 ;************************************************************************
464       P:00010F P:000111 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
                            010120
465                                                                                     ; SHFD = 0 for MSB shifted first
466                                                                                     ; CKP = 0 for rising clock edge transitions
467                                                                                     ; TE0 = 1 to enable transmitter #0
468                                                                                     ; MOD = 0 for normal, non-networked mode
469                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
470       P:000111 P:000113 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
471                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  init.asm  Page 10



472                                 ;********************************************************************************
473       P:000113 P:000115 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
474       P:000115 P:000117 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
                            000000
475                                 ;***********************************************************************************
476                                 ; 250MHz
477                                 ; Conversion from software bits to schematic labels for Port C and D
478                                 ;       PC0 = SC00 = AUX3               PD0 = SC10 = EF*
479                                 ;       PC1 = SC01 = A/B* = input       PD1 = SC11 = HF*
480                                 ;       PC2 = SC02 = No connect         PD2 = SC12 = RS*
481                                 ;       PC3 = SCK0 = No connect         PD3 = SCK1 = NWRFIFO*
482                                 ;       PC4 = SRD0 = AUX1               PD4 = SRD1 = No connect (** in 50Mhz this was MODE selec
t for 16 or 32 bit FO)
483                                 ;       PC5 = STD0 = No connect         PD5 = STD1 = WRFIFO*
484                                 ; ***********************************************************************************
485    
486    
487                                 ; ****************************************************************************
488                                 ; Program the serial port ESSI1 = Port D for general purpose I/O (GPIO)
489    
490       P:000117 P:000119 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
491       P:000119 P:00011B 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            00001C
492       P:00011B P:00011D 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
493       P:00011D P:00011F 060AA0            REP     #10
494       P:00011E P:000120 000000            NOP
495       P:00011F P:000121 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
496                                                                                     ; was %011100
497    
498                                 ; Program the SCI port to benign values
499       P:000121 P:000123 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
500       P:000123 P:000125 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
501       P:000125 P:000127 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
502                                 ;       PE0 = RXD
503                                 ;       PE1 = TXD
504                                 ;       PE2 = SCLK
505    
506                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
507       P:000127 P:000129 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
508       P:000129 P:00012B 07F40B            MOVEP             #$2800,X:TCSR1
                            002800
509       P:00012B P:00012D 07F407            MOVEP             #$2800,X:TCSR2
                            002800
510    
511    
512                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
513       P:00012D P:00012F 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
514       P:00012F P:000131 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
e
                            008929
515       P:000131 P:000133 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  init.asm  Page 11



                            000122
516    
517    
518                                 ; Program the DRAM memory access and addressing
519       P:000133 P:000135 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
                            020022
520       P:000135 P:000137 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
521    
522    
523                                 ; Clear all PCI error conditions
524       P:000137 P:000139 084E0A            MOVEP             X:DPSR,A
525       P:000138 P:00013A 0140C2            OR      #$1FE,A
                            0001FE
526       P:00013A P:00013C 000000            NOP
527       P:00013B P:00013D 08CE0A            MOVEP             A,X:DPSR
528    
529                                 ; Status word and interrupt configuration.
530       P:00013C P:00013E 08F4BF            MOVEP             #>MY_IPRC,X:IPRC
                            0001C0
531       P:00013E P:000140 08F4BE            MOVEP             #>MY_IPRP,X:IPRP
                            000002
532       P:000140 P:000142 05F439            MOVE              #>MY_SR,SR
                            000100
533    
534    
535                                 ;--------------------------------------------------------------------------
536                                 ; Initialize the fiber optic serial transmitter to zero
537       P:000142 P:000144 01B786            JCLR    #TDE,X:SSISR0,*
                            000142
538       P:000144 P:000146 07F43C            MOVEP             #$000000,X:TX00
                            000000
539    
540                                 ;--------------------------------------------------------------------
541    
542                                 ; clear DTXM - PCI master transmitter
543       P:000146 P:000148 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
544       P:000147 P:000149 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000147
545    
546                                 ;----------------------------------------------------------------------
547                                 ; clear DRXR - PCI receiver
548    
549       P:000149 P:00014B 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014E
550       P:00014B P:00014D 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
551       P:00014C P:00014E 000000            NOP
552       P:00014D P:00014F 0C0149            JMP     <CLR0
553                                 CLR1
554    
555                                 ;-----------------------------------------------------------------------------
556                                 ; copy parameter table from P memory into X memory
557    
558                                 ; but not frame_count and num_dumped - don't want these reset by fatal error...
559                                 ; they will be reset by new packet or pci_reset ISR
560    
561       P:00014E P:000150 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000006
562       P:000150 P:000152 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
                            000002
563    
564                                 ; Move the table of constants from P: space to X: space
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  init.asm  Page 12



565       P:000152 P:000154 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            0006CD
566       P:000154 P:000156 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
567       P:000155 P:000157 065D80            DO      #VAR_TBL_LENGTH,X_WRITE
                            000158
568       P:000157 P:000159 07D984            MOVE              P:(R1)+,X0
569       P:000158 P:00015A 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
570                                 X_WRITE
571    
572       P:000159 P:00015B 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
                            000006
573       P:00015B P:00015D 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000002
574    
575                                 ;-------------------------------------------------------------------------------
576                                 ; Initialise MODE; packet choke and PCI interrupts are ON.
577                                 ;  - that's MODE=0 .
578    
579    
580                                 ;----------------------------------------------------------------------------
581                                 ; Initialize PCI controller again, after booting, to make sure it sticks
582    
583                                 ;       MOVEP   #>$000000,X:DCTR
584       P:00015D P:00015F 0A8514            BCLR    #20,X:DCTR                        ; Terminate and reset mode
585       P:00015E P:000160 000000            NOP
586       P:00015F P:000161 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00015F
587                                 ;       MOVEP   #>$100000,X:DCTR
588       P:000161 P:000163 000000            NOP
589       P:000162 P:000164 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
590       P:000163 P:000165 000000            NOP
591       P:000164 P:000166 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000164
592    
593       
594       P:000166 P:000168 0BF080            JSR     CLEAR_FO_FIFO                     ; Clear the fibre fifo!
                            0004DF
595       P:000168 P:00016A 013D24            BSET    #AUX1,X:PDRC                      ; Enable byte-swapping - still necc. on ARC-
64
596       P:000169 P:00016B 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host interrupts
597       P:00016A P:00016C 0BF080            JSR     TIMER_DISABLE                     ; Disable NFY timer
                            000653
598       P:00016C P:00016E 0BF080            JSR     TIMER_STORE_INIT                  ; Initialize timing buffer
                            000667
599    
601                                           INCLUDE 'main.asm'
602    
603                                                 COMMENT *
604    
605                                 Main section of the pci card code.
606    
607                                 See info.asm for versioning and authors.
608    
609                                         *
610                                           PAGE    132                               ; Printronix page width - 132 columns
611                                           OPT     CEX                               ; print DC evaluations
612    
616    
617                                 PACKET_IN
618    
619       
620       P:00016E P:000170 0A0017            BCLR    #MAIN_LOOP_POLL,X:<STATUS
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 13



621    
622       
623       P:00016F P:000171 0A00B6            JSET    #FREEZER,X:<STATUS,PACKET_IN
                            00016E
624    
625       
626       P:000171 P:000173 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
627    
628       
629       P:000173 P:000175 0A01A0            JSET    #MODE_APPLICATION,X:<MODE,APPLICATION
                            000800
630    
631       
632       P:000175 P:000177 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            000659
633    
634       
635       P:000177 P:000179 0B00B4            JSSET   #QT_FLUSH,X:STATUS,BUFFER_INFORM
                            0006A3
636    
637       
638       P:000179 P:00017B 0D0475            JSR     <CHECK_FO
639       P:00017A P:00017C 0B00A3            JSSET   #FO_WRD_RCV,X:STATUS,HANDLE_FIFO
                            000183
640    
641       
642       P:00017C P:00017E 0B00AB            JSSET   #CON_MCE,X:STATUS,CON_TRANSMIT
                            000282
643       P:00017E P:000180 0B00AA            JSSET   #CON_DEMAND,X:STATUS,CON_BUFFER
                            000265
644    
645       
646       P:000180 P:000182 000000            NOP
647       P:000181 P:000183 000000            NOP
648    
649       
650       P:000182 P:000184 0C016E            JMP     PACKET_IN
651    
655    
656    
657    
659    
660                                 HANDLE_FIFO
661       P:000183 P:000185 54F400            MOVE              #>$A00,A1
                            000A00
662       P:000185 P:000187 0BF080            JSR     TIMER_STORE_A1
                            000670
663       P:000187 P:000189 0BF080            JSR     TIMER_STORE
                            00066E
664    
665       
666       P:000189 P:00018B 60F400            MOVE              #>HEAD_W1_0,R0
                            00000F
667       P:00018B P:00018D 44F400            MOVE              #>$00FFFF,X0            ; Mask lower 16 bits
                            00FFFF
668       P:00018D P:00018F 220800            MOVE              R0,A0
669       P:00018E P:000190 060880            DO      #8,HANDLE_FIFO_CHECK_PREAMBLE
                            000199
670                                 HANDLE_FIFO_WAIT
671       P:000190 P:000192 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000190
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 14



672       P:000192 P:000194 000000            NOP
673       P:000193 P:000195 000000            NOP
674       P:000194 P:000196 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000190
675       P:000196 P:000198 094E3F            MOVEP             Y:RDFIFO,A
676       P:000197 P:000199 200046            AND     X0,A
677       P:000198 P:00019A 000000            NOP
678       P:000199 P:00019B 545800            MOVE              A1,X:(R0)+
679    
680                                 HANDLE_FIFO_CHECK_PREAMBLE
681       P:00019A P:00019C 60F400            MOVE              #>HEAD_W1_0,R0
                            00000F
682       P:00019C P:00019E 20001B            CLR     B
683       P:00019D P:00019F 200013            CLR     A
684       P:00019E P:0001A0 57D800            MOVE              X:(R0)+,B
685       P:00019F P:0001A1 0140CD            CMP     #>$A5A5,B
                            00A5A5
686       P:0001A1 P:0001A3 0AF0A2            JNE     PRE_ERROR
                            0001C8
687       P:0001A3 P:0001A5 57D800            MOVE              X:(R0)+,B
688       P:0001A4 P:0001A6 0140CD            CMP     #>$A5A5,B
                            00A5A5
689       P:0001A6 P:0001A8 0AF0A2            JNE     PRE_ERROR
                            0001C8
690       P:0001A8 P:0001AA 57D800            MOVE              X:(R0)+,B
691       P:0001A9 P:0001AB 0140CD            CMP     #>$5A5A,B
                            005A5A
692       P:0001AB P:0001AD 0AF0A2            JNE     PRE_ERROR
                            0001C8
693       P:0001AD P:0001AF 57D800            MOVE              X:(R0)+,B
694       P:0001AE P:0001B0 0140CD            CMP     #>$5A5A,B
                            005A5A
695       P:0001B0 P:0001B2 0AF0A2            JNE     PRE_ERROR
                            0001C8
696    
697       
698       P:0001B2 P:0001B4 50F000            MOVE              X:>(HEAD_W1_0+6),A0
                            000015
699       P:0001B4 P:0001B6 44F000            MOVE              X:>(HEAD_W1_0+7),X0
                            000016
700       P:0001B6 P:0001B8 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
701    
702       
703       P:0001B8 P:0001BA 0BF080            JSR     PACKET_PARTITIONS
                            0005E9
704       P:0001BA P:0001BC 0BF080            JSR     TIMER_STORE
                            00066E
705    
707       P:0001BC P:0001BE 56F000            MOVE              X:HEAD_W3_0,A
                            000013
708    
709       P:0001BE P:0001C0 0140C5            CMP     #>'RP',A
                            005250
710       P:0001C0 P:0001C2 0AF0AA            JEQ     HANDLE_RP
                            0001DC
711    
712       P:0001C2 P:0001C4 0140C5            CMP     #>'DA',A
                            004441
713       P:0001C4 P:0001C6 0AF0AA            JEQ     HANDLE_DA
                            000224
714    
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 15



715       P:0001C6 P:0001C8 0AF080            JMP     QT_PTYPE_ERROR
                            0001CE
716    
717                                 ; Error recording.
718    
719                                 PRE_ERROR
720       P:0001C8 P:0001CA 60F400            MOVE              #>PREAMBLE_ERRORS,R0
                            00002C
721       P:0001CA P:0001CC 0BF080            JSR     INCR_X_R0
                            0001D7
722       P:0001CC P:0001CE 0AF080            JMP     CLEAR_FO_FIFO                     ; empty the fifo (2 ms!)
                            0004DF
723    
724                                 QT_PTYPE_ERROR
725       P:0001CE P:0001D0 60F400            MOVE              #>PTYPE_ERRORS,R0
                            00002D
726       P:0001D0 P:0001D2 0AF080            JMP     INCR_X_R0
                            0001D7
727                                 QT_FSIZE_ERROR
728       P:0001D2 P:0001D4 60F400            MOVE              #>PSIZE_ERRORS,R0
                            00002E
729       P:0001D4 P:0001D6 0AF080            JMP     INCR_X_R0
                            0001D7
730                                 RETURN_NOW
731       P:0001D6 P:0001D8 00000C            RTS
732    
733                                 INCR_X_R0
734       
735       P:0001D7 P:0001D9 50E000            MOVE              X:(R0),A0
736       P:0001D8 P:0001DA 000008            INC     A
737       P:0001D9 P:0001DB 000000            NOP
738       P:0001DA P:0001DC 506000            MOVE              A0,X:(R0)
739       P:0001DB P:0001DD 00000C            RTS
740    
741    
742    
745    
746                                 HANDLE_RP
747       
748       P:0001DC P:0001DE 0A0183            JCLR    #MODE_RP_BUFFER,X:MODE,MCE_PACKET
                            0002A3
749    
750       
751       P:0001DE P:0001E0 0A00B5            JSET    #RP_BUFFER_FULL,X:STATUS,HANDLE_RP_DROP
                            00021F
752    
753       
754       P:0001E0 P:0001E2 61F400            MOVE              #>REPLY_BUFFER,R1
                            100000
755       P:0001E2 P:0001E4 0BF080            JSR     BUFFER_PACKET
                            0005F6
756    
757       P:0001E4 P:0001E6 54F400            MOVE              #>$b00,A1
                            000B00
758       P:0001E6 P:0001E8 0BF080            JSR     TIMER_STORE_A1
                            000670
759       P:0001E8 P:0001EA 0BF080            JSR     TIMER_STORE
                            00066E
760    
761       
762       P:0001EA P:0001EC 60F400            MOVE              #RP_BASE_LO,R0
                            00004B
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 16



763       P:0001EC P:0001EE 0BF080            JSR     LOAD_HILO_ADDRESS
                            0006BC
764    
765       P:0001EE P:0001F0 60F400            MOVE              #BURST_DEST_LO,R0
                            000031
766       P:0001F0 P:0001F2 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006C4
767    
768       
769       P:0001F2 P:0001F4 200013            CLR     A
770       P:0001F3 P:0001F5 20001B            CLR     B
771       P:0001F4 P:0001F6 50F000            MOVE              X:PACKET_SIZE,A0
                            00002B
772       P:0001F6 P:0001F8 0C1D04            ASL     #2,A,A                            ; Size in bytes
773       P:0001F7 P:0001F9 51F000            MOVE              X:RP_MAX_SIZE,B0
                            00004D
774    
775       P:0001F9 P:0001FB 200005            CMP     B,A                               ; A ? B
776       P:0001FA P:0001FC 0AF0AF            JLE     HANDLE_RP1
                            0001FD
777       P:0001FC P:0001FE 21EE00            MOVE              B,A
778    
779                                 HANDLE_RP1
780       
781       P:0001FD P:0001FF 44F400            MOVE              #>REPLY_BUFFER,X0
                            100000
782       P:0001FF P:000201 507000            MOVE              A0,X:BLOCK_SIZE
                            00002F
783       P:000201 P:000203 447000            MOVE              X0,X:YMEM_SRC
                            000035
784       P:000203 P:000205 0BF080            JSR     TIMER_STORE
                            00066E
785       P:000205 P:000207 0BF080            JSR     BLOCK_TRANSFER
                            000543
786       P:000207 P:000209 0BF080            JSR     TIMER_STORE
                            00066E
787    
788       
789       P:000209 P:00020B 0BF080            JSR     PCI_LOCKDOWN                      ; Disable host IRQ
                            000498
790       P:00020B P:00020D 44F400            MOVE              #'NFY',X0
                            4E4659
791       P:00020D P:00020F 447000            MOVE              X0,X:DTXS_WD1
                            00000B
792       P:00020F P:000211 44F400            MOVE              #'RPQ',X0
                            525051
793       P:000211 P:000213 447000            MOVE              X0,X:DTXS_WD2
                            00000C
794       P:000213 P:000215 507000            MOVE              A0,X:DTXS_WD3           ; A0=block_size
                            00000D
795       P:000215 P:000217 547000            MOVE              A1,X:DTXS_WD4           ; A1=0
                            00000E
796    
797       
798       P:000217 P:000219 0A7035            BSET    #RP_BUFFER_FULL,X:STATUS
                            000000
799       P:000219 P:00021B 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00049E
800       P:00021B P:00021D 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
801    
802       P:00021C P:00021E 0BF080            JSR     TIMER_STORE
                            00066E
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 17



803       P:00021E P:000220 00000C            RTS                                       ; Back to main loop
804    
805                                 HANDLE_RP_DROP
806       P:00021F P:000221 60F400            MOVE              #RP_DROPS,R0
                            00004E
807       P:000221 P:000223 0D01D7            JSR     INCR_X_R0
808       P:000222 P:000224 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            000639
809    
811    
812    
815    
816    
817                                 HANDLE_DA
818       
819       P:000224 P:000226 60F400            MOVE              #FRAME_COUNT,R0
                            000002
820       P:000226 P:000228 0D01D7            JSR     INCR_X_R0
821    
822       
823       P:000227 P:000229 0A0182            JCLR    #MODE_QT,X:MODE,MCE_PACKET
                            0002A3
824    
825       
826       P:000229 P:00022B 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
827       P:00022B P:00022D 0BF080            JSR     BUFFER_PACKET
                            0005F6
828    
829       P:00022D P:00022F 54F400            MOVE              #$e00,A1
                            000E00
830       P:00022F P:000231 0BF080            JSR     TIMER_STORE_A1
                            000670
831       P:000231 P:000233 0BF080            JSR     TIMER_STORE
                            00066E
832    
833       
834       P:000233 P:000235 56F000            MOVE              X:QT_BUF_HEAD,A
                            000045
835       P:000235 P:000237 014180            ADD     #1,A
836       P:000236 P:000238 57F000            MOVE              X:QT_BUF_MAX,B
                            000042
837       P:000238 P:00023A 20000D            CMP     A,B
838       P:000239 P:00023B 0AF0A1            JGE     HANDLE_DA_MATH
                            00023C
839       P:00023B P:00023D 2E0000            MOVE              #0,A
840                                 HANDLE_DA_MATH
841       P:00023C P:00023E 57F000            MOVE              X:QT_BUF_TAIL,B
                            000046
842       P:00023E P:000240 20000D            CMP     A,B
843       P:00023F P:000241 0AF0AA            JEQ     HANDLE_DA_DROP
                            000260
844    
845       
846       P:000241 P:000243 200013            CLR     A
847       P:000242 P:000244 50F000            MOVE              X:PACKET_SIZE,A0
                            00002B
848    
849       P:000244 P:000246 014088            ADD     #0,B                              ; Clear carry
850       P:000245 P:000247 0C1D04            ASL     #2,A,A                            ; Size, in bytes
851    
852       
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 18



853       P:000246 P:000248 20001B            CLR     B
854       P:000247 P:000249 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            000043
855       P:000249 P:00024B 20000D            CMP     A,B
856       P:00024A P:00024C 0E21D2            JNE     QT_FSIZE_ERROR
857    
858       
859       P:00024B P:00024D 517000            MOVE              B0,X:BLOCK_SIZE
                            00002F
860       P:00024D P:00024F 557000            MOVE              B1,X:YMEM_SRC           ; Y:0
                            000035
861    
862       P:00024F P:000251 60F400            MOVE              #QT_DEST_LO,R0
                            000047
863       P:000251 P:000253 0BF080            JSR     LOAD_HILO_ADDRESS
                            0006BC
864       P:000253 P:000255 60F400            MOVE              #BURST_DEST_LO,R0
                            000031
865       P:000255 P:000257 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006C4
866    
867       
868       P:000257 P:000259 0BF080            JSR     BLOCK_TRANSFER
                            000543
869    
870       P:000259 P:00025B 0BF080            JSR     TIMER_STORE
                            00066E
871    
872       
873       P:00025B P:00025D 0BF080            JSR     BUFFER_INCR
                            000678
874    
875       
876       P:00025D P:00025F 0BF080            JSR     BUFFER_INFORM_CHECK
                            000696
877    
878       P:00025F P:000261 00000C            RTS
879    
880                                 HANDLE_DA_DROP
881       
882       P:000260 P:000262 60F400            MOVE              #QT_DROPS,R0
                            00004A
883       P:000262 P:000264 0D01D7            JSR     INCR_X_R0
884       P:000263 P:000265 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            000639
885    
887    
888    
889                                 ;----------------------------------------------
890                                 CON_BUFFER
891                                 ; This routine will copy an MCE command from the PC to Y memory.
892                                 ; The source RAM address has already been stored in CON_SRC_LO.
893                                 ; The destination address is always Y:COMMAND_BUFFER.
894                                 ;----------------------------------------------
895    
896       P:000265 P:000267 54F400            MOVE              #>$C00,A1
                            000C00
897       P:000267 P:000269 0BF080            JSR     TIMER_STORE_A1
                            000670
898       P:000269 P:00026B 0BF080            JSR     TIMER_STORE
                            00066E
899    
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 19



900       
901       P:00026B P:00026D 60F400            MOVE              #>CON_SRC_LO,R0
                            00004F
902       P:00026D P:00026F 0BF080            JSR     LOAD_HILO_ADDRESS
                            0006BC
903       P:00026F P:000271 60F400            MOVE              #>BURST_SRC_LO,R0
                            000033
904       P:000271 P:000273 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006C4
905       P:000273 P:000275 51F400            MOVE              #>COMMAND_BUFFER,B0
                            200000
906       P:000275 P:000277 50F400            MOVE              #>256,A0
                            000100
907       P:000277 P:000279 517000            MOVE              B0,X:YMEM_DEST
                            000036
908       P:000279 P:00027B 507000            MOVE              A0,X:BLOCK_SIZE
                            00002F
909       P:00027B P:00027D 0BF080            JSR     CON_TRANSFER
                            00057D
910    
911       P:00027D P:00027F 0A702B            BSET    #CON_MCE,X:STATUS
                            000000
912       P:00027F P:000281 0BF080            JSR     TIMER_STORE
                            00066E
913       P:000281 P:000283 00000C            RTS                                       ; Back to main loop
914    
915                                 ;----------------------------------------------
916                                 CON_TRANSMIT
917                                 ; This routine will copy the MCE command from Y:COMMAND_BUFFER to
918                                 ; the MCE command transmitter.
919                                 ;----------------------------------------------
920    
921       P:000282 P:000284 0BF080            JSR     TIMER_STORE
                            00066E
922    
923       P:000284 P:000286 66F400            MOVE              #>COMMAND_BUFFER,R6
                            200000
924       P:000286 P:000288 068080            DO      #128,CON_TRANSMIT1                ; block size = 16bit x 128 (256 bytes)
                            00028F
925       P:000288 P:00028A 5CDE00            MOVE                          Y:(R6)+,A1  ; b2, b1  (lsb)
926       P:000289 P:00028B 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
927       P:00028A P:00028C 0140C6            AND     #>$FF,A
                            0000FF
928       P:00028C P:00028E 547000            MOVE              A1,X:FO_SEND
                            FFF000
929       P:00028E P:000290 557000            MOVE              B1,X:FO_SEND
                            FFF000
930    
931                                 CON_TRANSMIT1
932       P:000290 P:000292 0A0121            BSET    #MODE_MCE,X:<MODE                 ; enable processing of MCE replies/data
933    
934       
935       P:000291 P:000293 0A700B            BCLR    #CON_MCE,X:STATUS
                            000000
936       P:000293 P:000295 0A700A            BCLR    #CON_DEMAND,X:STATUS
                            000000
937    
938       P:000295 P:000297 0BF080            JSR     TIMER_STORE
                            00066E
939    
940       
941       P:000297 P:000299 0BF080            JSR     PCI_LOCKDOWN
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 20



                            000498
942       P:000299 P:00029B 44F400            MOVE              #'CON',X0
                            434F4E
943       P:00029B P:00029D 0BF080            JSR     VCOM_PREPARE_REPLY
                            0002E6
944       P:00029D P:00029F 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00049E
945       P:00029F P:0002A1 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
946    
947       P:0002A0 P:0002A2 0BF080            JSR     TIMER_STORE
                            00066E
948       P:0002A2 P:0002A4 00000C            RTS                                       ; Back to main loop
949    
950    
951    
952    
954    
955                                 ; --------------------------------------------------------------------------
956                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
957                                 ; --------------------------------------------------------------------------
958    
959                                 ; prepare notify to inform host that a packet has arrived.
960    
961                                 MCE_PACKET
962       P:0002A3 P:0002A5 0BF080            JSR     PCI_LOCKDOWN                      ; Disable host IRQ
                            000498
963       P:0002A5 P:0002A7 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
964    
965       P:0002A6 P:0002A8 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
966       P:0002A8 P:0002AA 440B00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
967    
968       P:0002A9 P:0002AB 449300            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
969       P:0002AA P:0002AC 440C00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
970    
971       P:0002AB P:0002AD 449500            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
972       P:0002AC P:0002AE 440D00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
973    
974       P:0002AD P:0002AF 449600            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
975       P:0002AE P:0002B0 440E00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sage
976    
977       
978       P:0002AF P:0002B1 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
979       P:0002B0 P:0002B2 0D049E            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
980       P:0002B1 P:0002B3 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
981       P:0002B2 P:0002B4 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
982    
983       P:0002B3 P:0002B5 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
984       P:0002B5 P:0002B7 0BF080            JSR     BUFFER_PACKET
                            0005F6
985    
986       
987    
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 21



988       P:0002B7 P:0002B9 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; on fatal error, re-init.
                            000100
989       P:0002B9 P:0002BB 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; Set in 'send_packet_to_host' ISR
                            0002B7
990    
991       
992       P:0002BB P:0002BD 44F400            MOVE              #>IMAGE_BUFFER,X0
                            000000
993       P:0002BD P:0002BF 56F000            MOVE              X:PACKET_SIZE,A
                            00002B
994       P:0002BF P:0002C1 0C1D04            ASL     #2,A,A
995       P:0002C0 P:0002C2 447000            MOVE              X0,X:YMEM_SRC
                            000035
996       P:0002C2 P:0002C4 547000            MOVE              A1,X:BLOCK_SIZE
                            00002F
997       P:0002C4 P:0002C6 0BF080            JSR     BLOCK_TRANSFER
                            000543
998    
999       P:0002C6 P:0002C8 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
1000   
1001      
1002      P:0002C8 P:0002CA 0BF080            JSR     PCI_LOCKDOWN                      ; Disable host IRQ
                            000498
1003      P:0002CA P:0002CC 44F400            MOVE              #'HST',X0
                            485354
1004      P:0002CC P:0002CE 0BF080            JSR     VCOM_PREPARE_REPLY
                            0002E6
1005      P:0002CE P:0002D0 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00049E
1006      P:0002D0 P:0002D2 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
1007      P:0002D1 P:0002D3 00000C            RTS
1008   
1009                                ;----------------------------------------------------------
1010                                ; clear out the fifo after an HST timeout...
1011                                ;----------------------------------------------------------
1012   
1013                                DUMP_FIFO
1014      P:0002D2 P:0002D4 61F400            MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
1015      P:0002D4 P:0002D6 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
1016      P:0002D6 P:0002D8 200013            CLR     A
1017      P:0002D7 P:0002D9 320000            MOVE              #0,R2                   ; use R2 as a dump count
1018                                NEXT_DUMP
1019      P:0002D8 P:0002DA 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E3
1020      P:0002DA P:0002DC 000000            NOP
1021      P:0002DB P:0002DD 000000            NOP
1022      P:0002DC P:0002DE 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E3
1023   
1024      P:0002DE P:0002E0 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
1025      P:0002DF P:0002E1 205A00            MOVE              (R2)+                   ; inc dump count
1026      P:0002E0 P:0002E2 224E00            MOVE              R2,A                    ;
1027      P:0002E1 P:0002E3 200045            CMP     X0,A                              ; check we've not hit dump limit
1028      P:0002E2 P:0002E4 0E22D8            JNE     NEXT_DUMP                         ; not hit limit?
1029                                FIFO_EMPTY
1030      P:0002E3 P:0002E5 627000            MOVE              R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000006
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 22



1031      P:0002E5 P:0002E7 0C0100            JMP     <START                            ; re-initialise
1032   
1033   
1034                                ; -------------------------------------------------------------------------------------
1035                                ;                              END OF MAIN PACKET HANDLING CODE
1036                                ; -------------------------------------------------------------------------------------
1037   
1038   
1039   
1040                                ; -------------------------------------------------------------------------------------
1041                                ;
1042                                ;                              INTERRUPT SERVICE ROUTINES
1043                                ;
1044                                ; -------------------------------------------------------------------------------------
1045   
1046                                ; ---------------
1047                                ; Rules:  Don't use N#, or any R# except R0 unless you add them to the saved register set.
1048   
1049   
1050                                ; ----------------------------------------------------------------------------
1051                                ; VCOM_* - routines: utility functions for hosty command vector communication.
1052                                ;-----------------------------------------------------------------------------
1053   
1054   
1055                                ; VCOM_PREPARE_REPLY
1056                                ;
1057                                ; Prepare the reply packet, using X0 as the command name (second word).  The
1058                                ; message defaults to 'ACK' with NULL data.  The user may subsequenty fill in
1059                                ; the data field (word 4) and mark the packet as error if necessary.
1060   
1061                                VCOM_PREPARE_REPLY
1062      
1063      
1064      P:0002E6 P:0002E8 50F400            MOVE              #'REP',A0
                            524550
1065      P:0002E8 P:0002EA 447000            MOVE              X0,X:DTXS_WD2           ; Command
                            00000C
1066      P:0002EA P:0002EC 507000            MOVE              A0,X:DTXS_WD1
                            00000B
1067   
1068      P:0002EC P:0002EE 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
                            41434B
1069      P:0002EE P:0002F0 000000            NOP
1070      P:0002EF P:0002F1 547000            MOVE              A1,X:DTXS_WD3           ; ACK
                            00000D
1071      P:0002F1 P:0002F3 507000            MOVE              A0,X:DTXS_WD4           ; no comment
                            00000E
1072      P:0002F3 P:0002F5 00000C            RTS
1073   
1074   
1075                                ; VCOM_CHECK
1076                                ;
1077                                ; Compares DRXR_WD1 to X0.  If they are equal, Z is set on return.  If they
1078                                ; are not equal then Z is cleared and the reply will be marked as ERR with
1079                                ; 'CNE' in the last word.
1080                                ; Trashes A and B always and X0 on error.
1081   
1082                                VCOM_CHECK
1083      P:0002F4 P:0002F6 208E00            MOVE              X0,A
1084      P:0002F5 P:0002F7 57F000            MOVE              X:DRXR_WD1,B
                            000007
1085      P:0002F7 P:0002F9 20000D            CMP     A,B
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 23



1086      P:0002F8 P:0002FA 0AF0AA            JEQ     VCOM_RTS
                            000302
1087   
1088      P:0002FA P:0002FC 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
1089      P:0002FC P:0002FE 50F400            MOVE              #'ERR',A0
                            455252
1090      P:0002FE P:000300 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1091      P:000300 P:000302 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1092                                VCOM_RTS
1093      P:000302 P:000304 00000C            RTS
1094   
1095   
1096                                ; VCOM_INTRO
1097                                ;
1098                                ; Read DSP command from DRXR.  Prepare the reply packet and verify that it
1099                                ; matches the key in X1.  If it does not, mark the reply as error and set
1100                                ; the Z flag.
1101   
1102                                VCOM_INTRO
1103      P:000303 P:000305 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            0004B7
1104      P:000305 P:000307 20A400            MOVE              X1,X0
1105      P:000306 P:000308 0D02E6            JSR     VCOM_PREPARE_REPLY
1106      P:000307 P:000309 0D02F4            JSR     VCOM_CHECK
1107      P:000308 P:00030A 00000C            RTS
1108   
1109   
1110                                ; VCOM_EXIT_ERROR_X0
1111                                ; VCOM_EXIT_X0
1112                                ; VCOM_EXIT
1113                                ;
1114                                ; For returning from host command vector interrupts only.  These three
1115                                ; routines do the following (respectively):
1116                                ; a) Mark reply as error, then (b)
1117                                ; b) Put X0 into last word of reply, then (c)
1118                                ; c) Restore registers and RTI.
1119   
1120                                VCOM_EXIT_ERROR_X0
1121      P:000309 P:00030B 50F400            MOVE              #'ERR',A0
                            455252
1122      P:00030B P:00030D 000000            NOP
1123      P:00030C P:00030E 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1124                                VCOM_EXIT_X0
1125      P:00030E P:000310 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1126                                VCOM_EXIT
1127      P:000310 P:000312 0BF080            JSR     RESTORE_REGISTERS
                            0004BE
1128      P:000312 P:000314 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00049E
1129      P:000314 P:000316 000004            RTI
1130   
1131   
1132                                ; ----------------------------------------------------------------------------
1133                                READ_MEMORY
1134                                ;-----------------------------------------------------------------------------
1135                                ;Read command:
1136                                ; word 1 = command = 'RDM'
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 24



1137                                ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
1138                                ; word 3 = address in memory
1139                                ; word 4 = not used
1140                                ;Version query:
1141                                ; word 1 = 'VER'
1142                                ; word 2-4 unused
1143   
1144      P:000315 P:000317 0BF080            JSR     SAVE_REGISTERS
                            0004CB
1145      P:000317 P:000319 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            0004B7
1146   
1147      P:000319 P:00031B 44F400            MOVE              #'RDM',X0
                            52444D
1148      P:00031B P:00031D 0D02E6            JSR     VCOM_PREPARE_REPLY
1149      P:00031C P:00031E 0D02F4            JSR     VCOM_CHECK
1150      P:00031D P:00031F 0AF0AA            JEQ     READ_MEMORY_XYP
                            000327
1151   
1152      
1153      P:00031F P:000321 44F400            MOVE              #'VER',X0
                            564552
1154      P:000321 P:000323 0D02E6            JSR     VCOM_PREPARE_REPLY
1155      P:000322 P:000324 0D02F4            JSR     VCOM_CHECK
1156      P:000323 P:000325 0E2310            JNE     VCOM_EXIT
1157   
1158      P:000324 P:000326 44F000            MOVE              X:REV_NUMBER,X0
                            000003
1159      P:000326 P:000328 0C030E            JMP     VCOM_EXIT_X0
1160   
1161                                READ_MEMORY_XYP
1162   
1163      
1164      P:000327 P:000329 56F000            MOVE              X:DRXR_WD2,A
                            000008
1165      P:000329 P:00032B 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1166   
1167      P:00032B P:00032D 0140C5            CMP     #'_X',A
                            005F58
1168      P:00032D P:00032F 0AF0AA            JEQ     READ_MEMORY_X
                            00033A
1169   
1170      P:00032F P:000331 0140C5            CMP     #'_Y',A
                            005F59
1171      P:000331 P:000333 0AF0AA            JEQ     READ_MEMORY_Y
                            00033C
1172   
1173      P:000333 P:000335 0140C5            CMP     #'_P',A
                            005F50
1174      P:000335 P:000337 0AF0AA            JEQ     READ_MEMORY_P
                            00033E
1175   
1176      P:000337 P:000339 44F400            MOVE              #'MTE',X0
                            4D5445
1177      P:000339 P:00033B 0C0309            JMP     VCOM_EXIT_ERROR_X0
1178   
1179                                READ_MEMORY_X
1180      P:00033A P:00033C 44E000            MOVE              X:(R0),X0
1181      P:00033B P:00033D 0C030E            JMP     VCOM_EXIT_X0
1182                                READ_MEMORY_Y
1183      P:00033C P:00033E 4CE000            MOVE                          Y:(R0),X0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 25



1184      P:00033D P:00033F 0C030E            JMP     VCOM_EXIT_X0
1185                                READ_MEMORY_P
1186      P:00033E P:000340 07E084            MOVE              P:(R0),X0
1187      P:00033F P:000341 0C030E            JMP     VCOM_EXIT_X0
1188   
1189   
1190                                ;--------------------------------------------------------------
1191                                WRITE_MEMORY
1192                                ;---------------------------------------------------------------
1193                                ; word 1 = command = 'WRM'
1194                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1195                                ; word 3 = address in memory
1196                                ; word 4 = value
1197   
1198      P:000340 P:000342 0BF080            JSR     SAVE_REGISTERS
                            0004CB
1199      P:000342 P:000344 45F400            MOVE              #'WRM',X1
                            57524D
1200      P:000344 P:000346 0D0303            JSR     VCOM_INTRO
1201      P:000345 P:000347 0E2310            JNE     VCOM_EXIT
1202   
1203      
1204      P:000346 P:000348 56F000            MOVE              X:DRXR_WD2,A
                            000008
1205      P:000348 P:00034A 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1206      P:00034A P:00034C 44F000            MOVE              X:DRXR_WD4,X0
                            00000A
1207   
1208      P:00034C P:00034E 0140C5            CMP     #'_X',A
                            005F58
1209      P:00034E P:000350 0AF0AA            JEQ     WRITE_MEMORY_X
                            00035B
1210   
1211      P:000350 P:000352 0140C5            CMP     #'_Y',A
                            005F59
1212      P:000352 P:000354 0AF0AA            JEQ     WRITE_MEMORY_Y
                            00035D
1213   
1214      P:000354 P:000356 0140C5            CMP     #'_P',A
                            005F50
1215      P:000356 P:000358 0AF0AA            JEQ     WRITE_MEMORY_P
                            00035F
1216   
1217      P:000358 P:00035A 44F400            MOVE              #'MTE',X0
                            4D5445
1218      P:00035A P:00035C 0C0309            JMP     VCOM_EXIT_ERROR_X0
1219   
1220                                WRITE_MEMORY_X
1221      P:00035B P:00035D 446000            MOVE              X0,X:(R0)
1222      P:00035C P:00035E 0C030E            JMP     VCOM_EXIT_X0
1223                                WRITE_MEMORY_Y
1224      P:00035D P:00035F 4C6000            MOVE                          X0,Y:(R0)
1225      P:00035E P:000360 0C030E            JMP     VCOM_EXIT_X0
1226                                WRITE_MEMORY_P
1227      P:00035F P:000361 076084            MOVE              X0,P:(R0)
1228      P:000360 P:000362 0C030E            JMP     VCOM_EXIT_X0
1229   
1230   
1231                                ;-----------------------------------------------------------------------------
1232                                START_APPLICATION
1233                                ; an application should already have been downloaded to the PCI memory.
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 26



1234                                ; this command will execute it.
1235                                ; ----------------------------------------------------------------------
1236                                ; word 1 = command = 'GOA'
1237                                ; word 2-4 unused
1238   
1239      P:000361 P:000363 0BF080            JSR     SAVE_REGISTERS
                            0004CB
1240      P:000363 P:000365 45F400            MOVE              #'GOA',X1
                            474F41
1241   
1242      P:000365 P:000367 0D0303            JSR     VCOM_INTRO
1243      P:000366 P:000368 0E2310            JNE     VCOM_EXIT
1244   
1245      P:000367 P:000369 0A7020            BSET    #MODE_APPLICATION,X:MODE
                            000001
1246      P:000369 P:00036B 000004            RTI                                       ; Application will reply.
1247   
1248   
1249                                ; ---------------------------------------------------------
1250                                STOP_APPLICATION
1251                                ; this command stops an application that is currently running
1252                                ; used for applications that once started run contiunually
1253                                ;-----------------------------------------------------------
1254                                ; word 1 = command = ' STP'
1255                                ; word 2-4 unused
1256   
1257      P:00036A P:00036C 0BF080            JSR     SAVE_REGISTERS
                            0004CB
1258      P:00036C P:00036E 45F400            MOVE              #'STP',X1
                            535450
1259   
1260      P:00036E P:000370 0D0303            JSR     VCOM_INTRO
1261      P:00036F P:000371 0E2310            JNE     VCOM_EXIT
1262   
1263      P:000370 P:000372 0A7000            BCLR    #MODE_APPLICATION,X:MODE
                            000001
1264      P:000372 P:000374 0A7000            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1265      P:000374 P:000376 0C0310            JMP     VCOM_EXIT
1266   
1267   
1268                                ;-----------------------------------------------------------------------------
1269                                RESET_CONTROLLER
1270                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1271                                ;-----------------------------------------------------------------------------
1272                                ; word 1 = command = 'RCO'
1273                                ; word 2-4 unused
1274   
1275      P:000375 P:000377 0BF080            JSR     SAVE_REGISTERS
                            0004CB
1276      P:000377 P:000379 45F400            MOVE              #'RCO',X1
                            52434F
1277      P:000379 P:00037B 0D0303            JSR     VCOM_INTRO
1278      P:00037A P:00037C 0E2310            JNE     VCOM_EXIT
1279   
1280      P:00037B P:00037D 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1281      P:00037C P:00037E 000000            NOP
1282      P:00037D P:00037F 000000            NOP
1283      P:00037E P:000380 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1284      P:000380 P:000382 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 27



1285      P:000382 P:000384 446000            MOVE              X0,X:(R0)
1286      P:000383 P:000385 0606A0            REP     #6                                ; Wait for transmission to complete
1287      P:000384 P:000386 000000            NOP
1288      P:000385 P:000387 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1289   
1290                                ; Wait for a bit for MCE to be reset.......
1291      P:000386 P:000388 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1292      P:000388 P:00038A 06C400            DO      X0,L_DELAY
                            00038E
1293      P:00038A P:00038C 06E883            DO      #1000,L_RDFIFO
                            00038D
1294      P:00038C P:00038E 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1295      P:00038D P:00038F 000000            NOP                                       ;   receiver empty
1296                                L_RDFIFO
1297      P:00038E P:000390 000000            NOP
1298                                L_DELAY
1299      P:00038F P:000391 000000            NOP
1300   
1301      P:000390 P:000392 44F400            MOVE              #'000',X0
                            303030
1302      P:000392 P:000394 0C030E            JMP     VCOM_EXIT_X0
1303   
1304                                ;-----------------------------------------------------------------------------
1305                                QUIET_TRANSFER_SET
1306                                ;-----------------------------------------------------------------------------
1307                                ;Quiet transfer mode configuration
1308                                ; word 1 = command = 'QTS'
1309                                ; word 2 = parameter to set
1310                                ; word 3-4 = arguments
1311   
1312      P:000393 P:000395 0BF080            JSR     SAVE_REGISTERS                    ; standard opening
                            0004CB
1313      P:000395 P:000397 45F400            MOVE              #'QTS',X1
                            515453
1314      P:000397 P:000399 0D0303            JSR     VCOM_INTRO
1315      P:000398 P:00039A 0E2310            JNE     VCOM_EXIT
1316   
1317      P:000399 P:00039B 60F400            MOVE              #BDEBUG0,R0
                            000053
1318      P:00039B P:00039D 0D01D7            JSR     INCR_X_R0
1319   
1320      P:00039C P:00039E 56F000            MOVE              X:DRXR_WD2,A            ; Parameter id
                            000008
1321      P:00039E P:0003A0 44F000            MOVE              X:DRXR_WD3,X0           ; First arg
                            000009
1322      P:0003A0 P:0003A2 45F000            MOVE              X:DRXR_WD4,X1           ; Second arg
                            00000A
1323   
1324      P:0003A2 P:0003A4 0140C5            CMP     #'BAS',A
                            424153
1325      P:0003A4 P:0003A6 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            00041F
1326   
1327      P:0003A6 P:0003A8 0140C5            CMP     #'DEL',A
                            44454C
1328      P:0003A8 P:0003AA 60F400            MOVE              #QT_BUF_SIZE,R0
                            000041
1329      P:0003AA P:0003AC 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041C
1330   
1331      P:0003AC P:0003AE 0140C5            CMP     #'NUM',A
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 28



                            4E554D
1332      P:0003AE P:0003B0 60F400            MOVE              #QT_BUF_MAX,R0
                            000042
1333      P:0003B0 P:0003B2 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041C
1334   
1335      P:0003B2 P:0003B4 0140C5            CMP     #'INF',A
                            494E46
1336      P:0003B4 P:0003B6 60F400            MOVE              #QT_INFORM,R0
                            000044
1337      P:0003B6 P:0003B8 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041C
1338   
1339      P:0003B8 P:0003BA 0140C5            CMP     #'SIZ',A
                            53495A
1340      P:0003BA P:0003BC 60F400            MOVE              #QT_FRAME_SIZE,R0
                            000043
1341      P:0003BC P:0003BE 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041C
1342   
1343      P:0003BE P:0003C0 0140C5            CMP     #'TAI',A
                            544149
1344      P:0003C0 P:0003C2 60F400            MOVE              #QT_BUF_TAIL,R0
                            000046
1345      P:0003C2 P:0003C4 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041C
1346   
1347      P:0003C4 P:0003C6 0140C5            CMP     #'HEA',A
                            484541
1348      P:0003C6 P:0003C8 60F400            MOVE              #QT_BUF_HEAD,R0
                            000045
1349      P:0003C8 P:0003CA 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041C
1350   
1351      P:0003CA P:0003CC 0140C5            CMP     #'DRO',A
                            44524F
1352      P:0003CC P:0003CE 60F400            MOVE              #QT_DROPS,R0
                            00004A
1353      P:0003CE P:0003D0 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041C
1354   
1355      P:0003D0 P:0003D2 0140C5            CMP     #'PER',A
                            504552
1356      P:0003D2 P:0003D4 60F400            MOVE              #TCPR0,R0
                            FFFF8D
1357      P:0003D4 P:0003D6 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041C
1358   
1359      P:0003D6 P:0003D8 0140C5            CMP     #'FLU',A
                            464C55
1360      P:0003D8 P:0003DA 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            000401
1361   
1362      P:0003DA P:0003DC 0140C5            CMP     #'SET',A
                            534554
1363      P:0003DC P:0003DE 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            00040B
1364   
1365      P:0003DE P:0003E0 0140C5            CMP     #'RPS',A
                            525053
1366      P:0003E0 P:0003E2 60F400            MOVE              #RP_MAX_SIZE,R0
                            00004D
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 29



1367      P:0003E2 P:0003E4 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041C
1368   
1369      P:0003E4 P:0003E6 0140C5            CMP     #'RPB',A
                            525042
1370      P:0003E6 P:0003E8 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_BASE
                            0003EF
1371   
1372      P:0003E8 P:0003EA 0140C5            CMP     #'RPE',A
                            525045
1373      P:0003EA P:0003EC 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_ENABLED
                            0003F5
1374   
1375      P:0003EC P:0003EE 44F400            MOVE              #'MTE',X0
                            4D5445
1376      P:0003EE P:0003F0 0C0309            JMP     VCOM_EXIT_ERROR_X0
1377   
1378                                QUIET_TRANSFER_SET_RP_BASE
1379      P:0003EF P:0003F1 447000            MOVE              X0,X:RP_BASE_LO
                            00004B
1380      P:0003F1 P:0003F3 457000            MOVE              X1,X:RP_BASE_HI
                            00004C
1381      P:0003F3 P:0003F5 0AF080            JMP     VCOM_EXITX
                            000427
1382   
1383                                QUIET_TRANSFER_SET_RP_ENABLED
1384      P:0003F5 P:0003F7 0A7003            BCLR    #MODE_RP_BUFFER,X:MODE
                            000001
1385      P:0003F7 P:0003F9 208E00            MOVE              X0,A
1386      P:0003F8 P:0003FA 200003            TST     A
1387      P:0003F9 P:0003FB 0AF0AA            JEQ     VCOM_EXITX
                            000427
1388      P:0003FB P:0003FD 0A7023            BSET    #MODE_RP_BUFFER,X:MODE
                            000001
1389      P:0003FD P:0003FF 0A7015            BCLR    #RP_BUFFER_FULL,X:STATUS
                            000000
1390      P:0003FF P:000401 0AF080            JMP     VCOM_EXITX
                            000427
1391   
1392                                QUIET_TRANSFER_SET_FLUSH
1393      P:000401 P:000403 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
1394      P:000403 P:000405 208E00            MOVE              X0,A
1395      P:000404 P:000406 200003            TST     A
1396      P:000405 P:000407 0AF0AA            JEQ     VCOM_EXITX
                            000427
1397      P:000407 P:000409 0A7034            BSET    #QT_FLUSH,X:STATUS
                            000000
1398      P:000409 P:00040B 0AF080            JMP     VCOM_EXITX
                            000427
1399   
1400                                QUIET_TRANSFER_SET_ENABLED
1401      P:00040B P:00040D 0A7002            BCLR    #MODE_QT,X:MODE
                            000001
1402      P:00040D P:00040F 0BF080            JSR     TIMER_DISABLE
                            000653
1403      P:00040F P:000411 208E00            MOVE              X0,A
1404      P:000410 P:000412 200003            TST     A
1405      P:000411 P:000413 0AF0AA            JEQ     VCOM_EXITX
                            000427
1406      P:000413 P:000415 280000            MOVE              #0,A0
1407      P:000414 P:000416 0A7022            BSET    #MODE_QT,X:MODE
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 30



                            000001
1408      P:000416 P:000418 507000            MOVE              A0,X:TLR0
                            FFFF8E
1409      P:000418 P:00041A 0BF080            JSR     TIMER_ENABLE
                            00064D
1410      P:00041A P:00041C 0AF080            JMP     VCOM_EXITX
                            000427
1411   
1412                                QUIET_TRANSFER_SET_R0
1413      P:00041C P:00041E 446000            MOVE              X0,X:(R0)
1414      P:00041D P:00041F 0AF080            JMP     VCOM_EXITX
                            000427
1415   
1416                                QUIET_TRANSFER_SET_BASE
1417      P:00041F P:000421 447000            MOVE              X0,X:QT_BASE_LO
                            00003F
1418      P:000421 P:000423 457000            MOVE              X1,X:QT_BASE_HI
                            000040
1419   
1420      P:000423 P:000425 0BF080            JSR     BUFFER_RESET
                            00068A
1421   
1422      P:000425 P:000427 0AF080            JMP     VCOM_EXITX
                            000427
1423   
1424                                VCOM_EXITX
1425      P:000427 P:000429 44F000            MOVE              X:BDEBUG0,X0
                            000053
1426      P:000429 P:00042B 0C030E            JMP     VCOM_EXIT_X0
1427   
1428   
1429                                ; ;-----------------------------------------------------------------------------
1430                                ; MODE_SET_FAST
1431                                ; ;-----------------------------------------------------------------------------
1432                                ; ; This is a 'fast' command in the sense that it does not reply to the host.
1433                                ; ; It is used to set various communication parameters prior to issuing normal
1434                                ; ; DSP commands.  A single word is read from the DRXR into DRXR_WD1 as data.
1435   
1436                                ;       MOVE    X0,X:SV_X0              ; Save X0
1437                                ;       JCLR    #SRRQ,X:DSR,*           ; Wait for data
1438                                ;       MOVEP   X:DRXR,X0               ; Low 16 bits
1439                                ;       NOP
1440                                ;       MOVE    X0,X:MODE
1441                                ;       MOVE    X:SV_X0,X0
1442                                ;       RTI
1443   
1444   
1445                                ;-----------------------------------------------------------------------------
1446                                SYSTEM_RESET
1447                                ;-----------------------------------------------------------------------------
1448   
1449      P:00042A P:00042C 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1450      P:00042B P:00042D 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1451                                                                                    ; set to zero except for interrupts
1452      P:00042D P:00042F 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1453                                                                                    ; so first set to 0
1454      P:00042E P:000430 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1455                                                                                    ; therefore,return to initialization
1456      P:000430 P:000432 000000            NOP
1457      P:000431 P:000433 000004            RTI                                       ; return from ISR - to START
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 31



1458   
1459   
1460                                ;--------------------------------------------------------------------
1461                                CLEAN_UP_PCI
1462                                ;--------------------------------------------------------------------
1463                                ; Clean up the PCI board from wherever it was executing
1464   
1465      P:000432 P:000434 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1466      P:000433 P:000435 05F43D            MOVEC             #$000200,SSL            ; SR = zero except for interrupts
                            000200
1467      P:000435 P:000437 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1468      P:000436 P:000438 05F43C            MOVEC             #START,SSH              ; Set PC to for full initialization
                            000100
1469      P:000438 P:00043A 000000            NOP
1470      P:000439 P:00043B 000004            RTI
1471   
1472   
1473                                ; ------------------------------------------------------------------------------------
1474                                SEND_PACKET_TO_HOST
1475                                ; this command is received from the Host and actions the PCI board to pick up an address
1476                                ; pointer from DRXR which the PCI board then uses to write packets from the
1477                                ; MCE to the host memory starting at the address given.
1478                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1479                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1480                                ; HST after packet sent (unless error).
1481                                ; --------------------------------------------------------------------------------------
1482                                ; word 1 = command = 'HST'
1483                                ; word 2 = host high address
1484                                ; word 3 = host low address
1485                                ; word 4 = not used but read
1486   
1487                                ; save some registers but not B
1488   
1489      P:00043A P:00043C 0D04CB            JSR     <SAVE_REGISTERS                   ; save working registers
1490      P:00043B P:00043D 45F400            MOVE              #'HST',X1
                            485354
1491      P:00043D P:00043F 0D0303            JSR     VCOM_INTRO
1492      P:00043E P:000440 0E2310            JNE     VCOM_EXIT
1493   
1494      
1495      P:00043F P:000441 448800            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
1496      P:000440 P:000442 518900            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1497   
1498      P:000441 P:000443 447000            MOVE              X0,X:BURST_DEST_HI
                            000032
1499      P:000443 P:000445 517000            MOVE              B0,X:BURST_DEST_LO
                            000031
1500   
1501      P:000445 P:000447 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1502   
1503      P:000446 P:000448 0BF080            JSR     RESTORE_REGISTERS
                            0004BE
1504      P:000448 P:00044A 000004            RTI                                       ; Main loop will reply after packet transfer
!
1505   
1506   
1507                                ; --------------------------------------------------------------------
1508                                SOFTWARE_RESET
1509                                ;----------------------------------------------------------------------
1510                                ; word 1 = command = 'RST'
1511                                ; word 2-4 unused
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 32



1512   
1513      P:000449 P:00044B 0BF080            JSR     SAVE_REGISTERS
                            0004CB
1514      P:00044B P:00044D 45F400            MOVE              #'RST',X1
                            525354
1515      P:00044D P:00044F 0D0303            JSR     VCOM_INTRO
1516      P:00044E P:000450 0E2310            JNE     VCOM_EXIT
1517   
1518                                ; RST command OK so reply to host
1519                                FINISH_RST
1520      P:00044F P:000451 44F400            MOVE              #'000',X0
                            303030
1521      P:000451 P:000453 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1522      P:000453 P:000455 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00049E
1523   
1524      P:000455 P:000457 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            000455
1525   
1526      P:000457 P:000459 0A0100            BCLR    #MODE_APPLICATION,X:<MODE         ; clear app flag
1527                                ;         BCLR  #PREAMBLE_ERROR,X:<STATUS       ; clear preamble error
1528      P:000458 P:00045A 0A0000            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1529   
1530                                ; remember we are in a ISR so can't just jump to start.
1531   
1532      P:000459 P:00045B 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1533      P:00045A P:00045C 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1534                                                                                    ; set to zero except for interrupts
1535      P:00045C P:00045E 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1536                                                                                    ; so first set to 0
1537      P:00045D P:00045F 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1538                                                                                    ; therefore,return to initialization
1539      P:00045F P:000461 000000            NOP
1540      P:000460 P:000462 000004            RTI                                       ; return from ISR - to START
1541   
1542   
1543                                SEND_PACKET_TO_CONTROLLER
1544   
1545                                ;       Host command identifying location of an MCE command to send to
1546                                ;       the MCE.  Since this can come at any time, just record the
1547                                ;       request and then do the CONning from the main loop.
1548   
1549                                ; word 1 = command = 'CON'
1550                                ; word 2 = source host bus address, bits 31:16
1551                                ; word 3 = source host bus address, bits 15:0
1552                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1553                                ;        = '1' --> when MCE command is GO
1554   
1555      P:000461 P:000463 0D04CB            JSR     <SAVE_REGISTERS                   ; save working registers
1556   
1557      
1558      P:000462 P:000464 45F400            MOVE              #'CON',X1
                            434F4E
1559      P:000464 P:000466 0D0303            JSR     VCOM_INTRO
1560      P:000465 P:000467 0E2310            JNE     VCOM_EXIT
1561   
1562      
1563      P:000466 P:000468 44F400            MOVE              #'BUS',X0
                            425553
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 33



1564      P:000468 P:00046A 0A00AA            JSET    #CON_DEMAND,X:STATUS,VCOM_EXIT_ERROR_X0
                            000309
1565   
1566      
1567      P:00046A P:00046C 0A702A            BSET    #CON_DEMAND,X:STATUS
                            000000
1568      P:00046C P:00046E 448800            MOVE              X:<DRXR_WD2,X0
1569      P:00046D P:00046F 458900            MOVE              X:<DRXR_WD3,X1
1570      P:00046E P:000470 447000            MOVE              X0,X:CON_SRC_HI
                            000050
1571      P:000470 P:000472 457000            MOVE              X1,X:CON_SRC_LO
                            00004F
1572   
1573                                ;       ;; Fourth word indicates if this is a go.  Who cares?
1574                                ;       MOVE    X:<DRXR_WD4,A           ; read word 4 - GO command?
1575                                ;       MOVE    #0,X0
1576                                ;       CMP     X0,A
1577                                ;       JEQ     BLOCK_CON
1578   
1579      
1580      P:000472 P:000474 0BF080            JSR     RESTORE_REGISTERS
                            0004BE
1581      P:000474 P:000476 000004            RTI
1582   
1584   
1585   
1586                                ;---------------------------------------------------------------
1587                                ;
1588                                ;                          * END OF ISRs *
1589                                ;
1590                                ;--------------------------------------------------------------
1591   
1592   
1593   
1594                                ;----------------------------------------------------------------
1595                                ;
1596                                ;                     * Beginning of SUBROUTINES *
1597                                ;
1598                                ;-----------------------------------------------------------------
1599   
1600   
1601                                CHECK_FO
1602      P:000475 P:000477 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            000493
1603      P:000477 P:000479 000000            NOP
1604      P:000478 P:00047A 000000            NOP
1605      P:000479 P:00047B 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            000493
1606   
1607      P:00047B P:00047D 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1608      P:00047C P:00047E 00000C            RTS
1609   
1610   
1611                                ;---------------------------------------------------------------
1612                                GET_FO_WRD
1613                                ;--------------------------------------------------------------
1614                                ; Anything in fibre receive FIFO?   If so store in X0
1615   
1616      P:00047D P:00047F 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            000493
1617      P:00047F P:000481 000000            NOP
1618      P:000480 P:000482 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 34



1619      P:000481 P:000483 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS             ; check twice for FO metastability.
                            000493
1620      P:000483 P:000485 0AF080            JMP     RD_FO_WD
                            00048B
1621   
1622                                WT_FIFO
1623      P:000485 P:000487 01AD80            JCLR    #EF,X:PDRD,*                      ; Wait till something in FIFO flagged
                            000485
1624      P:000487 P:000489 000000            NOP
1625      P:000488 P:00048A 000000            NOP
1626      P:000489 P:00048B 01AD80            JCLR    #EF,X:PDRD,WT_FIFO                ; check twice.....
                            000485
1627   
1628                                ; Read one word from the fiber optics FIFO, check it and put it in A1
1629                                RD_FO_WD
1630      P:00048B P:00048D 09443F            MOVEP             Y:RDFIFO,X0             ; then read to X0
1631      P:00048C P:00048E 54F400            MOVE              #$00FFFF,A1             ; mask off top 2 bytes ($FC)
                            00FFFF
1632      P:00048E P:000490 200046            AND     X0,A                              ; since receiving 16 bits in 24bit register
1633      P:00048F P:000491 000000            NOP
1634      P:000490 P:000492 218400            MOVE              A1,X0
1635      P:000491 P:000493 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1636      P:000492 P:000494 00000C            RTS
1637                                CLR_FO_RTS
1638      P:000493 P:000495 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1639      P:000494 P:000496 00000C            RTS
1640   
1641   
1642                                ; PCI semaphore
1643                                ;
1644                                ; In order for routines in non-interrupt context to write to the
1645                                ; DTXS, (via PCI_MESSAGE_TO_HOST) they need to end up with
1646                                ; interrupts disabled and HCF3 cleared.
1647                                ;
1648                                ; Non-interrupt PCIers must call PCI_LOCKDOWN before proceeding to
1649                                ; fill DTXS_WD? and call PCI_MESSAGE_TO_HOST.
1650                                ;
1651                                ; Restore with PCI_LOCKUP, or just re-enable HCIE.
1652   
1653                                PCI_LOCKDOWN_AGAIN
1654      P:000495 P:000497 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Re-enable host IRQ
1655      P:000496 P:000498 0632A0            REP     #50                               ; Delay for ~us
1656      P:000497 P:000499 000000            NOP
1657   
1658                                PCI_LOCKDOWN
1659      
1660      P:000498 P:00049A 0A8500            BCLR    #DCTR_HCIE,X:DCTR                 ; Disable host IRQ
1661      P:000499 P:00049B 0A85A3            JSET    #DCTR_HF3,X:DCTR,PCI_LOCKDOWN_AGAIN
                            000495
1662      P:00049B P:00049D 00000C            RTS
1663   
1664                                PCI_LOCKUP
1665      P:00049C P:00049E 0A8500            BCLR    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
1666      P:00049D P:00049F 00000C            RTS
1667   
1668   
1669                                ;----------------------------------------------------------------------------
1670                                PCI_MESSAGE_TO_HOST
1671                                ;----------------------------------------------------------------------------
1672                                ; Subroutine to send 4 words as a reply from PCI to the Host
1673                                ; using the DTXS-HRXS data path.  The DSP signals the host by raising
1674                                ; HF3 and (when !MODE_NOIRQ) INTA.
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 35



1675                                ;
1676                                ; When MODE_HANDSHAKE, the DSP and Host interact as follows:
1677                                ; - to show that the Host is handling the interrupt, Host raises HF0
1678                                ; - when DSP sees HF0 go high, it lowers INTA and HF3
1679                                ; - when Host is done handling the interrupt (i.e. it has read the reply),
1680                                ;   and when HF3 is low, Host lowers HF0.
1681                                ; - when DSP sees HF0 go low, the routine finishes.
1682                                ;
1683                                ; The primary advantage of this hand-shaking scheme is that host vector
1684                                ; commands are not needed to clear HF3 and INTA.
1685                                ;
1686                                ; This routine should not block for anything other than the Host handshake.
1687   
1688      P:00049E P:0004A0 60F400            MOVE              #>DTXS_WD1,R0
                            00000B
1689   
1690      P:0004A0 P:0004A2 060480            DO      #4,PCI_MESSAGE_TO_HOST_10
                            0004A4
1691      P:0004A2 P:0004A4 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            0004A2
1692      P:0004A4 P:0004A6 08D88D            MOVEP             X:(R0)+,X:DTXS
1693   
1694                                PCI_MESSAGE_TO_HOST_10
1695      P:0004A5 P:0004A7 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            00001D
1696      P:0004A7 P:0004A9 60F000            MOVE              X:SV_R0,R0              ; restore R0
                            000021
1697      P:0004A9 P:0004AB 0A8523            BSET    #DCTR_HF3,X:DCTR                  ; Raise HF3 (handshake)
1698   
1699                                                                                    ; Only interrupt in irq mode
1700      P:0004AA P:0004AC 0A89A5            JSET    #DSR_HF2,X:DSR,PCI_MESSAGE_TO_HOST_20
                            0004AD
1701      P:0004AC P:0004AE 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1702   
1703                                PCI_MESSAGE_TO_HOST_20
1704      P:0004AD P:0004AF 0A89A4            JSET    #DSR_HF1,X:DSR,PCI_MESSAGE_TO_HOST_HANDSHAKE
                            0004B0
1705      P:0004AF P:0004B1 00000C            RTS
1706   
1707                                PCI_MESSAGE_TO_HOST_HANDSHAKE
1708      P:0004B0 P:0004B2 0A8983            JCLR    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            0004B0
1709      P:0004B2 P:0004B4 0A8506            BCLR    #INTA,X:DCTR                      ; Clear interrupt
1710      P:0004B3 P:0004B5 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; Clear hand-shake bit
1711      P:0004B4 P:0004B6 0A89A3            JSET    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            0004B4
1712      P:0004B6 P:0004B8 00000C            RTS
1713   
1714   
1715                                ;---------------------------------------------------------------
1716                                RD_DRXR
1717                                ;--------------------------------------------------------------
1718                                ; Routine to read from HTXR-DRXR data path.  For HCTR = 0x900,
1719                                ; 3 LSB of each 32-bit word written by the host is returned on
1720                                ; each read.  This only polls for first word, not all of them.
1721      P:0004B7 P:0004B9 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            0004B7
1722      P:0004B9 P:0004BB 63F400            MOVE              #DRXR_WD1,R3
                            000007
1723      P:0004BB P:0004BD 0604A0            REP     #4
1724      P:0004BC P:0004BE 085B8B            MOVEP             X:DRXR,X:(R3)+
1725      P:0004BD P:0004BF 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 36



1726   
1727   
1728                                ;------------------------------------------------------------------------------------
1729                                RESTORE_REGISTERS
1730                                ;-------------------------------------------------------------------------------------
1731   
1732      P:0004BE P:0004C0 05A239            MOVEC             X:<SV_SR,SR
1733   
1734      P:0004BF P:0004C1 509700            MOVE              X:<SV_A0,A0
1735      P:0004C0 P:0004C2 549800            MOVE              X:<SV_A1,A1
1736      P:0004C1 P:0004C3 529900            MOVE              X:<SV_A2,A2
1737   
1738      P:0004C2 P:0004C4 519A00            MOVE              X:<SV_B0,B0
1739      P:0004C3 P:0004C5 559B00            MOVE              X:<SV_B1,B1
1740      P:0004C4 P:0004C6 539C00            MOVE              X:<SV_B2,B2
1741   
1742      P:0004C5 P:0004C7 449D00            MOVE              X:<SV_X0,X0
1743      P:0004C6 P:0004C8 459E00            MOVE              X:<SV_X1,X1
1744   
1745      P:0004C7 P:0004C9 469F00            MOVE              X:<SV_Y0,Y0
1746      P:0004C8 P:0004CA 47A000            MOVE              X:<SV_Y1,Y1
1747   
1748      P:0004C9 P:0004CB 60A100            MOVE              X:<SV_R0,R0
1749      P:0004CA P:0004CC 00000C            RTS
1750   
1751                                ;-------------------------------------------------------------------------------------
1752                                SAVE_REGISTERS
1753                                ;-------------------------------------------------------------------------------------
1754   
1755      P:0004CB P:0004CD 052239            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1756   
1757      P:0004CC P:0004CE 501700            MOVE              A0,X:<SV_A0
1758      P:0004CD P:0004CF 541800            MOVE              A1,X:<SV_A1
1759      P:0004CE P:0004D0 521900            MOVE              A2,X:<SV_A2
1760   
1761      P:0004CF P:0004D1 511A00            MOVE              B0,X:<SV_B0
1762      P:0004D0 P:0004D2 551B00            MOVE              B1,X:<SV_B1
1763      P:0004D1 P:0004D3 531C00            MOVE              B2,X:<SV_B2
1764   
1765      P:0004D2 P:0004D4 441D00            MOVE              X0,X:<SV_X0
1766      P:0004D3 P:0004D5 451E00            MOVE              X1,X:<SV_X1
1767   
1768      P:0004D4 P:0004D6 461F00            MOVE              Y0,X:<SV_Y0
1769      P:0004D5 P:0004D7 472000            MOVE              Y1,X:<SV_Y1
1770   
1771      P:0004D6 P:0004D8 602100            MOVE              R0,X:<SV_R0
1772      P:0004D7 P:0004D9 00000C            RTS
1773   
1774   
1775                                ;----------------------------------------------
1776                                FLUSH_PCI_FIFO
1777                                ;----------------------------------------------
1778      P:0004D8 P:0004DA 0A8A84            JCLR    #MARQ,X:DPSR,*
                            0004D8
1779      P:0004DA P:0004DC 0A862E            BSET    #CLRT,X:DPCR
1780      P:0004DB P:0004DD 000000            NOP
1781      P:0004DC P:0004DE 0A86AE            JSET    #CLRT,X:DPCR,*
                            0004DC
1782      P:0004DE P:0004E0 00000C            RTS
1783   
1784                                ;----------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 37



1785                                CLEAR_FO_FIFO
1786                                ;----------------------------------------------
1787      P:0004DF P:0004E1 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
1788      P:0004E1 P:0004E3 44F400            MOVE              #200000,X0
                            030D40
1789      P:0004E3 P:0004E5 06C400            DO      X0,*+3
                            0004E5
1790      P:0004E5 P:0004E7 000000            NOP
1791      P:0004E6 P:0004E8 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
1792      P:0004E8 P:0004EA 00000C            RTS
1793   
1794   
1795                                ;---------------------------------------------------------
1796                                ; PCI burst routines
1797                                ;
1798                                ; For transfer between Host memory and DSP Y memory.
1799                                ;
1800                                ; Major entry points are
1801                                ;       CON_TRANSFER (PC -> DSP)
1802                                ;       BLOCK_TRANSFER (DSP -> PC)
1803                                ;---------------------------------------------------------
1804   
1805                                ;---------------------------------------------------------
1806                                PCI_ERROR_CLEAR
1807                                ;-----------------------------------------------
1808      
1809      
1810      
1811      
1812      
1813      
1814   
1815      P:0004E9 P:0004EB 50F000            MOVE              X:DMA_ERRORS,A0
                            000037
1816      P:0004EB P:0004ED 000008            INC     A
1817      P:0004EC P:0004EE 000000            NOP
1818      P:0004ED P:0004EF 507000            MOVE              A0,X:DMA_ERRORS
                            000037
1819   
1820      P:0004EF P:0004F1 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            0004FD
1821      P:0004F1 P:0004F3 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
                            000507
1822      P:0004F3 P:0004F5 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            000511
1823      P:0004F5 P:0004F7 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            00051B
1824      P:0004F7 P:0004F9 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            000525
1825      P:0004F9 P:0004FB 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            00052F
1826      P:0004FB P:0004FD 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            000539
1827   
1828                                ERROR_TRTY
1829      P:0004FD P:0004FF 50F000            MOVE              X:EC_TRTY,A0
                            000038
1830      P:0004FF P:000501 000008            INC     A
1831      P:000500 P:000502 08F48A            MOVEP             #>$0400,X:DPSR          ; Clear target retry error bit
                            000400
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 38



1832      P:000502 P:000504 507000            MOVE              A0,X:EC_TRTY
                            000038
1833      P:000504 P:000506 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1834      P:000506 P:000508 00000C            RTS
1835                                ERROR_TO
1836      P:000507 P:000509 50F000            MOVE              X:EC_TO,A0
                            000039
1837      P:000509 P:00050B 000008            INC     A
1838      P:00050A P:00050C 08F48A            MOVEP             #>$0800,X:DPSR          ; Clear timeout error bit
                            000800
1839      P:00050C P:00050E 507000            MOVE              A0,X:EC_TO
                            000039
1840      P:00050E P:000510 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1841      P:000510 P:000512 00000C            RTS
1842                                ERROR_TDIS
1843      P:000511 P:000513 50F000            MOVE              X:EC_TDIS,A0
                            00003A
1844      P:000513 P:000515 000008            INC     A
1845      P:000514 P:000516 08F48A            MOVEP             #>$0200,X:DPSR          ; Clear target disconnect bit
                            000200
1846      P:000516 P:000518 507000            MOVE              A0,X:EC_TDIS
                            00003A
1847      P:000518 P:00051A 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1848      P:00051A P:00051C 00000C            RTS
1849                                ERROR_TAB
1850      P:00051B P:00051D 50F000            MOVE              X:EC_TAB,A0
                            00003B
1851      P:00051D P:00051F 000008            INC     A
1852      P:00051E P:000520 08F48A            MOVEP             #>$0100,X:DPSR          ; Clear target abort error bit
                            000100
1853      P:000520 P:000522 507000            MOVE              A0,X:EC_TAB
                            00003B
1854      P:000522 P:000524 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1855      P:000524 P:000526 00000C            RTS
1856                                ERROR_MAB
1857      P:000525 P:000527 50F000            MOVE              X:EC_MAB,A0
                            00003C
1858      P:000527 P:000529 000008            INC     A
1859      P:000528 P:00052A 08F48A            MOVEP             #>$0080,X:DPSR          ; Clear master abort error bit
                            000080
1860      P:00052A P:00052C 507000            MOVE              A0,X:EC_MAB
                            00003C
1861      P:00052C P:00052E 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1862      P:00052E P:000530 00000C            RTS
1863                                ERROR_DPER
1864      P:00052F P:000531 50F000            MOVE              X:EC_DPER,A0
                            00003D
1865      P:000531 P:000533 000008            INC     A
1866      P:000532 P:000534 08F48A            MOVEP             #>$0040,X:DPSR          ; Clear data parity error bit
                            000040
1867      P:000534 P:000536 507000            MOVE              A0,X:EC_DPER
                            00003D
1868      P:000536 P:000538 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1869      P:000538 P:00053A 00000C            RTS
1870                                ERROR_APER
1871      P:000539 P:00053B 50F000            MOVE              X:EC_APER,A0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 39



                            00003E
1872      P:00053B P:00053D 000008            INC     A
1873      P:00053C P:00053E 08F48A            MOVEP             #>$0020,X:DPSR          ; Clear address parity error bit
                            000020
1874      P:00053E P:000540 507000            MOVE              A0,X:EC_APER
                            00003E
1875      P:000540 P:000542 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1876      P:000542 P:000544 00000C            RTS
1877   
1878   
1879   
1880                                ;----------------------------------------------
1881                                BLOCK_TRANSFER
1882                                ;----------------------------------------------
1883                                ;   In:
1884                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
1885                                ;   - BLOCK_SIZE is packet size, in bytes
1886                                ;   - YMEM_SRC is start of data in Y memory
1887                                ;  Out:
1888                                ;   - BLOCK_SIZE will be decremented to zero.
1889                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
1890                                ;   - YMEM_SRC will be incremented by BLOCK_SIZE/2
1891                                ;  Trashes:
1892                                ;   - A and B at least
1893   
1894      P:000543 P:000545 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002F
1895      P:000545 P:000547 014085            CMP     #0,A                              ; Still bytes to transfer?
1896      P:000546 P:000548 0AF0A2            JNE     BLOCK_TRANSFER0
                            000549
1897      P:000548 P:00054A 00000C            RTS
1898   
1899                                BLOCK_TRANSFER0
1900      
1901      
1902      P:000549 P:00054B 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000051
1903   
1904      P:00054B P:00054D 200005            CMP     B,A                               ; A ? B
1905      P:00054C P:00054E 0E154E            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
1906      P:00054D P:00054F 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1907                                BLOCK_TRANSFER1
1908      
1909      P:00054E P:000550 200014            SUB     B,A                               ; A -= B
1910      P:00054F P:000551 014088            ADD     #0,B                              ; Clear carry bit
1911      P:000550 P:000552 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002F
1912      P:000552 P:000554 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            000030
1913      P:000554 P:000556 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1914   
1915      
1916      P:000555 P:000557 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
1917      P:000557 P:000559 50F000            MOVE              X:YMEM_SRC,A0
                            000035
1918      P:000559 P:00055B 08C82F            MOVEP             A0,X:DSR0               ; DMA source
1919      P:00055A P:00055C 200010            ADD     B,A
1920      P:00055B P:00055D 00000B            DEC     B
1921      P:00055C P:00055E 507000            MOVE              A0,X:YMEM_SRC           ; BURST_SRC += BURST_SIZE/2
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 40



                            000035
1922   
1923      P:00055E P:000560 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1924   
1925      
1926      P:00055F P:000561 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
1927   
1928                                BLOCK_TRANSFER_PCI
1929      P:000561 P:000563 44F400            MOVE              #>$7,X0                 ; Memory write
                            000007
1930      P:000563 P:000565 60F400            MOVE              #BURST_DEST_LO,R0       ; RAM address
                            000031
1931      P:000565 P:000567 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            0005B8
1932   
1933      
1934      P:000567 P:000569 0A8A84            JCLR    #MARQ,X:DPSR,*
                            000567
1935   
1936      
1937      P:000569 P:00056B 0A8A8E            JCLR    #MDT,X:DPSR,BLOCK_TRANSFER_HANDLE_ERRORS
                            000571
1938   
1939      P:00056B P:00056D 20001B            CLR     B
1940      P:00056C P:00056E 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            000030
1941      P:00056E P:000570 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            0006C2
1942      P:000570 P:000572 0C0543            JMP     BLOCK_TRANSFER                    ; Next burst in block
1943   
1944                                BLOCK_TRANSFER_HANDLE_ERRORS
1945      
1946      P:000571 P:000573 0D04E9            JSR     PCI_ERROR_CLEAR
1947   
1948      P:000572 P:000574 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1949      P:000574 P:000576 0E8561            JCS     BLOCK_TRANSFER_PCI                ; Restart PCI burst
1950   
1951      P:000575 P:000577 0A7012            BCLR    #PCIDMA_RETRY,X:STATUS            ; Test and clear
                            000000
1952      P:000577 P:000579 0E0543            JCC     BLOCK_TRANSFER                    ; Error but no error? Redo this burst.
1953   
1954      
1955      P:000578 P:00057A 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            0005CE
1956      P:00057A P:00057C 0BF080            JSR     PCI_UPDATE_R0
                            0005DE
1957      P:00057C P:00057E 0C0561            JMP     BLOCK_TRANSFER_PCI
1958   
1959   
1960                                ;----------------------------------------------
1961                                CON_TRANSFER
1962                                ;----------------------------------------------
1963                                ;   In:
1964                                ;   - BURST_SRC_HI:BURST_SRC_LO is PC RAM address
1965                                ;   - BLOCK_SIZE is packet size, in bytes
1966                                ;   - YMEM_DEST is start of data in Y memory
1967                                ;  Out:
1968                                ;   - BLOCK_SIZE will be decremented to zero.
1969                                ;   - BURST_SRC_HI:LO will be incremented by BLOCK_SIZE
1970                                ;   - YMEM_DEST will be incremented by BLOCK_SIZE/2
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 41



1971                                ;  Trashes:
1972                                ;   - A and B, R0, X0
1973   
1974      P:00057D P:00057F 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002F
1975      P:00057F P:000581 014085            CMP     #0,A                              ; Still bytes to transfer?
1976      P:000580 P:000582 0AF0A2            JNE     CON_TRANSFER0
                            000583
1977      P:000582 P:000584 00000C            RTS
1978   
1979                                CON_TRANSFER0
1980      
1981      
1982      P:000583 P:000585 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000051
1983   
1984      P:000585 P:000587 200005            CMP     B,A                               ; A ? B
1985      P:000586 P:000588 0E1588            JGE     <CON_TRANSFER1                    ; jump if A >= B
1986      P:000587 P:000589 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1987                                CON_TRANSFER1
1988      
1989      P:000588 P:00058A 200014            SUB     B,A                               ; A -= B
1990      P:000589 P:00058B 014088            ADD     #0,B                              ; Clear carry bit
1991      P:00058A P:00058C 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002F
1992      P:00058C P:00058E 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            000030
1993      P:00058E P:000590 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1994   
1995      
1996      P:00058F P:000591 50F000            MOVE              X:YMEM_DEST,A0
                            000036
1997      P:000591 P:000593 507000            MOVE              A0,X:DDR0               ; DMA dest'n
                            FFFFEE
1998      P:000593 P:000595 08F4AF            MOVEP             #>DRXR,X:DSR0           ; DMA source
                            FFFFCB
1999      P:000595 P:000597 200010            ADD     B,A
2000      P:000596 P:000598 00000B            DEC     B
2001      P:000597 P:000599 507000            MOVE              A0,X:YMEM_DEST          ; YMEM_DEST += BURST_SIZE/2
                            000036
2002   
2003      P:000599 P:00059B 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
2004   
2005      
2006      P:00059A P:00059C 08F4AC            MOVEP             #$8EEAC4,X:DCR0
                            8EEAC4
2007   
2008                                CON_TRANSFER_PCI
2009      P:00059C P:00059E 44F400            MOVE              #>$6,X0                 ; Memory write
                            000006
2010      P:00059E P:0005A0 60F400            MOVE              #BURST_SRC_LO,R0        ; RAM address
                            000033
2011      P:0005A0 P:0005A2 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            0005B8
2012   
2013      
2014      P:0005A2 P:0005A4 0A8A84            JCLR    #MARQ,X:DPSR,*
                            0005A2
2015   
2016      
2017      P:0005A4 P:0005A6 0A8A8E            JCLR    #MDT,X:DPSR,CON_TRANSFER_HANDLE_ERRORS
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 42



                            0005AC
2018   
2019      P:0005A6 P:0005A8 20001B            CLR     B
2020      P:0005A7 P:0005A9 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            000030
2021      P:0005A9 P:0005AB 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            0006C2
2022      P:0005AB P:0005AD 0C057D            JMP     CON_TRANSFER                      ; Next burst in block
2023   
2024                                CON_TRANSFER_HANDLE_ERRORS
2025      
2026      P:0005AC P:0005AE 0D04E9            JSR     PCI_ERROR_CLEAR
2027   
2028      P:0005AD P:0005AF 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
2029      P:0005AF P:0005B1 0E859C            JCS     CON_TRANSFER_PCI                  ; Restart PCI burst
2030   
2031      P:0005B0 P:0005B2 0A7012            BCLR    #PCIDMA_RETRY,X:STATUS            ; Test and clear
                            000000
2032      P:0005B2 P:0005B4 0E057D            JCC     CON_TRANSFER                      ; Error but no error? Redo this burst.
2033   
2034      
2035      P:0005B3 P:0005B5 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            0005CE
2036      P:0005B5 P:0005B7 0BF080            JSR     PCI_UPDATE_R0
                            0005DE
2037      P:0005B7 P:0005B9 0C059C            JMP     CON_TRANSFER_PCI
2038   
2039                                ; Utility routines for BLOCK_TRANSFER and CON_TRANSFER
2040   
2041                                PCI_GO
2042                                ; Initiate PCI read/write of BURST_SIZE bytes.
2043                                ; R0 must point to the hi-lo PCI address source/dest address
2044                                ; X0 is the PCI command (6 is read, 7 is write).
2045                                ; Trashes A and B but not R0 and X0.
2046      P:0005B8 P:0005BA 200013            CLR     A
2047      P:0005B9 P:0005BB 20001B            CLR     B
2048      P:0005BA P:0005BC 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            000030
2049      P:0005BC P:0005BE 00000B            DEC     B                                 ; n8 - 1
2050      P:0005BD P:0005BF 014088            ADD     #0,B                              ; Clear carry
2051      P:0005BE P:0005C0 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
2052      P:0005BF P:0005C1 014088            ADD     #0,B                              ; Clear carry
2053      P:0005C0 P:0005C2 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
2054   
2055      P:0005C1 P:0005C3 0200D8            MOVE              X:(R0+1),A0             ; PCI HI address
2056   
2057      P:0005C2 P:0005C4 200010            ADD     B,A
2058      P:0005C3 P:0005C5 000000            NOP
2059      P:0005C4 P:0005C6 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
2060   
2061      P:0005C6 P:0005C8 208800            MOVE              X0,A0
2062      P:0005C7 P:0005C9 014088            ADD     #0,B                              ; Clear carry
2063      P:0005C8 P:0005CA 0C1D20            ASL     #16,A,A                           ; Command into bits 19:16
2064      P:0005C9 P:0005CB 51E000            MOVE              X:(R0),B0
2065      P:0005CA P:0005CC 200010            ADD     B,A
2066      P:0005CB P:0005CD 000000            NOP
2067   
2068      P:0005CC P:0005CE 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
2069      P:0005CD P:0005CF 00000C            RTS
2070   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 43



2071   
2072                                PCI_RECOVER_COUNT
2073                                ; Calculate number of PCI words not transferred.
2074                                ; Correct BURST_SIZE.  Returns:
2075                                ;   B: bytes not transferred
2076                                ;   A: bytes transferred
2077      P:0005CE P:0005D0 200013            CLR     A
2078      P:0005CF P:0005D1 20001B            CLR     B
2079      P:0005D0 P:0005D2 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
2080      P:0005D1 P:0005D3 0A8A8F            JCLR    #RDCQ,X:DPSR,PCI_RECOVER_COUNT1
                            0005D4
2081      P:0005D3 P:0005D5 000009            INC     B
2082                                PCI_RECOVER_COUNT1
2083      P:0005D4 P:0005D6 000009            INC     B                                 ; We want N, not N-1.
2084      P:0005D5 P:0005D7 014088            ADD     #0,B                              ; Clear carry
2085      P:0005D6 P:0005D8 0C1C20            ASR     #16,A,A
2086      P:0005D7 P:0005D9 200018            ADD     A,B                               ; B is words remaining
2087      P:0005D8 P:0005DA 014088            ADD     #0,B                              ; Clear carry
2088      P:0005D9 P:0005DB 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
2089      P:0005DA P:0005DC 50F000            MOVE              X:BURST_SIZE,A0
                            000030
2090      P:0005DC P:0005DE 200014            SUB     B,A                               ; A is bytes written
2091      P:0005DD P:0005DF 00000C            RTS
2092   
2093   
2094                                PCI_UPDATE_R0
2095                                ;  Subtract A0 from BURST_SIZE and add A to the 32-bit hi-lo address at X:[R0].
2096                                ;  Caller can check Z flag to see if BURST_SIZE is now 0.
2097      P:0005DE P:0005E0 210500            MOVE              A0,X1                   ; Save A for later
2098      P:0005DF P:0005E1 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2099      P:0005E0 P:0005E2 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates [R0] = [R0] + B
                            0006C2
2100   
2101      P:0005E2 P:0005E4 57F000            MOVE              X:BURST_SIZE,B
                            000030
2102      P:0005E4 P:0005E6 20006C            SUB     X1,B                              ; Zero flag must be preserved!
2103      P:0005E5 P:0005E7 000000            NOP
2104      P:0005E6 P:0005E8 557000            MOVE              B1,X:BURST_SIZE
                            000030
2105      P:0005E8 P:0005EA 00000C            RTS
2106   
2107   
2108                                ;----------------------------------------------;
2109                                ;  MCE PACKET PROCESSING                       ;
2110                                ;----------------------------------------------;
2111   
2112                                ;       Given a dword count in A, computes number of half FIFOs and
2113                                ;       number of left over FIFO reads required to get the whole
2114                                ;       packet.
2115   
2116                                ;       Input: A is packet size, in dwords
2117                                ;       Output: sets X:TOTAL_BUFFS and X:LEFT_TO_READ
2118                                ;       Trashes: A,B,X0
2119   
2120   
2121                                PACKET_PARTITIONS
2122      P:0005E9 P:0005EB 507000            MOVE              A0,X:PACKET_SIZE
                            00002B
2123   
2124      P:0005EB P:0005ED 014088            ADD     #0,B                              ; Clear carry
2125      P:0005EC P:0005EE 0C1D02            ASL     #1,A,A                            ;  * 2
2126      P:0005ED P:0005EF 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 44



2127      P:0005EE P:0005F0 240000            MOVE              #0,X0
2128      P:0005EF P:0005F1 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
2129   
2130      P:0005F1 P:0005F3 557000            MOVE              B1,X:TOTAL_BUFFS
                            000027
2131      P:0005F3 P:0005F5 507000            MOVE              A0,X:LEFT_TO_READ
                            000028
2132      P:0005F5 P:0005F7 00000C            RTS
2133   
2134   
2135                                ; BUFFER_PACKET
2136                                ;
2137                                ; Copies the packet in the FIFO to Y memory.
2138                                ;
2139                                ; In: TOTAL_BUFFS and LEFT_TO_READ must be pre-set (via PACKET_PARTITIONS);
2140                                ;     R1 is the destination index in Y memory.
2141                                ; Trashes: R1 is updated to point to the end of the copied data.
2142   
2143                                BUFFER_PACKET
2144   
2145      P:0005F6 P:0005F8 54F400            MOVE              #>$b00,A1
                            000B00
2146      P:0005F8 P:0005FA 0BF080            JSR     TIMER_STORE_A1
                            000670
2147      P:0005FA P:0005FC 0BF080            JSR     TIMER_STORE
                            00066E
2148   
2149      P:0005FC P:0005FE 062700            DO      X:TOTAL_BUFFS,BUFFER_PACKET_HALFS_DONE
                            000606
2150      P:0005FE P:000600 0BF080            JSR     WAIT_FIFO_HALF
                            000623
2151      P:000600 P:000602 0BF080            JSR     TIMER_STORE
                            00066E
2152      P:000602 P:000604 0BF080            JSR     BUFFER_PACKET_HALF
                            00061E
2153      P:000604 P:000606 0BF080            JSR     TIMER_STORE
                            00066E
2154      P:000606 P:000608 000000            NOP
2155                                BUFFER_PACKET_HALFS_DONE
2156   
2157      
2158      
2159      
2160      
2161      P:000607 P:000609 01AD81            JCLR    #HF,X:PDRD,BUFFER_PACKET_SINGLES_FAST
                            00061A
2162   
2163      
2164      
2165   
2166                                BUFFER_PACKET_SINGLES
2167      
2168      
2169      P:000609 P:00060B 200013            CLR     A
2170      P:00060A P:00060C 20001B            CLR     B
2171      P:00060B P:00060D 51F000            MOVE              X:TCR0,B0               ; Store timer value (50 MHz)
                            FFFF8C
2172      P:00060D P:00060F 0C1C85            ASR     #2,B,B                            ; / 4
2173      P:00060E P:000610 062800            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_DONE
                            000616
2174                                BUFFER_PACKET_SINGLES_WAIT
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 45



2175      P:000610 P:000612 50F000            MOVE              X:TCR0,A0
                            FFFF8C
2176      P:000612 P:000614 0C1C04            ASR     #2,A,A
2177      P:000613 P:000615 20000D            CMP     A,B
2178      P:000614 P:000616 0EA610            JEQ     BUFFER_PACKET_SINGLES_WAIT
2179      P:000615 P:000617 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2180      P:000616 P:000618 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2181                                BUFFER_PACKET_SINGLES_DONE
2182      P:000617 P:000619 0BF080            JSR     TIMER_STORE
                            00066E
2183      P:000619 P:00061B 00000C            RTS
2184   
2185                                ;---------------------------------------------------------
2186   
2187                                BUFFER_PACKET_SINGLES_FAST
2188      P:00061A P:00061C 062800            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_FAST_DONE
                            00061C
2189      P:00061C P:00061E 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2190                                BUFFER_PACKET_SINGLES_FAST_DONE
2191      P:00061D P:00061F 00000C            RTS
2192   
2193                                ;---------------------------------------------------------
2194                                BUFFER_PACKET_HALF
2195      
2196      P:00061E P:000620 060082            DO      #512,BUFFER_PACKET_HALF_DONE
                            000621
2197      P:000620 P:000622 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2198      P:000621 P:000623 000000            NOP
2199                                BUFFER_PACKET_HALF_DONE
2200      P:000622 P:000624 00000C            RTS
2201   
2202                                ;---------------------------------------------------------
2203                                WAIT_FIFO_HALF
2204      P:000623 P:000625 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            000638
2205      P:000625 P:000627 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
                            000623
2206      P:000627 P:000629 000000            NOP
2207      P:000628 P:00062A 000000            NOP
2208      P:000629 P:00062B 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            000623
2209      P:00062B P:00062D 00000C            RTS
2210   
2211                                ;---------------------------------------------------------
2212   
2213                                ; This is the old single-buffering routine, which polls the EF.
2214                                BUFFER_PACKET_SINGLES_POLL
2215      P:00062C P:00062E 062800            DO      X:LEFT_TO_READ,BUFFER_PACKET_DONE
                            000636
2216                                BUFFER_PACKET_SINGLE
2217      P:00062E P:000630 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D2
2218      P:000630 P:000632 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE
                            00062E
2219      P:000632 P:000634 000000            NOP
2220      P:000633 P:000635 000000            NOP
2221      P:000634 P:000636 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE   ; Protect against metastability
                            00062E
2222      P:000636 P:000638 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2223                                BUFFER_PACKET_DONE
2224      P:000637 P:000639 00000C            RTS
2225   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 46



2226                                ;---------------------------------------------------------
2227   
2228                                FATALITY_HANDLER
2229      P:000638 P:00063A 0C0100            JMP     START                             ; What could possibly go wrong?
2230   
2231   
2232                                ; DROP_PACKET
2233                                ;
2234                                ; Reads a packet from the fifo, discarding it.
2235                                ;
2236                                ; In: TOTAL_BUFFS & LEFT_TO_READ
2237                                ; Trashes: A0
2238   
2239                                DROP_PACKET
2240      P:000639 P:00063B 062700            DO      X:TOTAL_BUFFS,DROP_PACKET_SINGLES
                            00063E
2241      P:00063B P:00063D 0D0623            JSR     WAIT_FIFO_HALF
2242      P:00063C P:00063E 0BF080            JSR     DROP_FIFO_HALF
                            000649
2243      P:00063E P:000640 000000            NOP
2244                                DROP_PACKET_SINGLES
2245      P:00063F P:000641 062800            DO      X:LEFT_TO_READ,DROP_PACKET_DONE
                            000647
2246                                DROP_PACKET_SINGLE
2247      P:000641 P:000643 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D2
2248      P:000643 P:000645 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE
                            000641
2249      P:000645 P:000647 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE     ; Protect against metastability
                            000641
2250      P:000647 P:000649 09483F            MOVEP             Y:RDFIFO,A0
2251                                DROP_PACKET_DONE
2252      P:000648 P:00064A 00000C            RTS
2253   
2254                                DROP_FIFO_HALF
2255      
2256      P:000649 P:00064B 060082            DO      #512,DROP_FIFO_DONE
                            00064B
2257      P:00064B P:00064D 09483F            MOVEP             Y:RDFIFO,A0
2258                                DROP_FIFO_DONE
2259      P:00064C P:00064E 00000C            RTS
2260   
2261   
2262                                ;----------------------------------------------;
2263                                ;  TIMER HANDLING                              ;
2264                                ;----------------------------------------------;
2265   
2266                                ; Start value is TLR, count is in TCR, int occurs at TCPR
2267                                ; Must set TCSR[TCIE] to enable int
2268                                ; Must set TCSR[T] for timer to restart
2269   
2270                                TIMER_ENABLE
2271      P:00064D P:00064F 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2272      P:00064F P:000651 000000            NOP
2273      P:000650 P:000652 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2274      P:000652 P:000654 00000C            RTS
2275   
2276                                TIMER_DISABLE
2277      P:000653 P:000655 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
                            300200
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 47



2278      P:000655 P:000657 000000            NOP
2279      P:000656 P:000658 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2280      P:000658 P:00065A 00000C            RTS
2281   
2283                                TIMER_ACTION
2284      P:000659 P:00065B 56F000            MOVE              X:QT_INFORM_IDX,A
                            000049
2285      P:00065B P:00065D 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2286      P:00065D P:00065F 000000            NOP
2287      P:00065E P:000660 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2288      P:000660 P:000662 0140C5            CMP     #>0,A                             ; If inform_idx != 0
                            000000
2289      P:000662 P:000664 0AF0AA            JEQ     TIMER_ACTION_OK
                            000666
2290      P:000664 P:000666 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2291                                TIMER_ACTION_OK
2292      P:000666 P:000668 00000C            RTS
2293   
2294   
2295                                ;----------------------------------------------;
2296                                ;  TIMER UTILITY                               ;
2297                                ;----------------------------------------------;
2298   
2299                                TIMER_STORE_INIT
2300      P:000667 P:000669 50F400            MOVE              #>TIMER_BUFFER,A0
                            201000
2301      P:000669 P:00066B 000000            NOP
2302      P:00066A P:00066C 507000            MOVE              A0,X:TIMER_INDEX
                            000052
2303      P:00066C P:00066E 211400            MOVE              A0,R4
2304      P:00066D P:00066F 00000C            RTS
2305   
2306                                TIMER_STORE
2307      
2308      
2309      P:00066E P:000670 56F000            MOVE              X:TCR0,A
                            FFFF8C
2310                                                                                    ; Fall-through
2311   
2312                                TIMER_STORE_A1
2313      
2314      P:000670 P:000672 5C5C00            MOVE                          A1,Y:(R4)+
2315      P:000671 P:000673 228C00            MOVE              R4,A1
2316      P:000672 P:000674 0140C5            CMP     #>TIMER_BUFFER_END,A
                            202000
2317      P:000674 P:000676 547000            MOVE              A1,X:TIMER_INDEX
                            000052
2318      P:000676 P:000678 0E1667            JGE     TIMER_STORE_INIT
2319      P:000677 P:000679 00000C            RTS
2320   
2321   
2322                                ;----------------------------------------------;
2323                                ;  CIRCULAR BUFFER HANDLING                    ;
2324                                ;----------------------------------------------;
2325   
2326                                BUFFER_INCR
2327   
2328      P:000678 P:00067A 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 48



                            000045
2329      P:00067A P:00067C 014180            ADD     #1,A                              ;
2330      P:00067B P:00067D 57F000            MOVE              X:QT_BUF_MAX,B          ;
                            000042
2331      P:00067D P:00067F 20000D            CMP     A,B                               ;
2332      P:00067E P:000680 0AF0AF            JLE     BUFFER_RESET                      ;       head = 0
                            00068A
2333                                                                                    ; else
2334      P:000680 P:000682 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            000045
2335   
2336      P:000682 P:000684 20001B            CLR     B
2337      P:000683 P:000685 51F000            MOVE              X:QT_BUF_SIZE,B0
                            000041
2338      P:000685 P:000687 60F400            MOVE              #QT_DEST_LO,R0
                            000047
2339      P:000687 P:000689 0BF080            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
                            0006C2
2340   
2341      P:000689 P:00068B 00000C            RTS
2342   
2343   
2344                                BUFFER_RESET
2345      P:00068A P:00068C 60F400            MOVE              #QT_BASE_LO,R0
                            00003F
2346      P:00068C P:00068E 0BF080            JSR     LOAD_HILO_ADDRESS
                            0006BC
2347      P:00068E P:000690 60F400            MOVE              #QT_DEST_LO,R0
                            000047
2348      P:000690 P:000692 0BF080            JSR     SAVE_HILO_ADDRESS                 ; QT_DEST_LO = QT_BASE_LO
                            0006C4
2349   
2350      P:000692 P:000694 240000            MOVE              #0,X0
2351      P:000693 P:000695 447000            MOVE              X0,X:QT_BUF_HEAD        ; HEAD = 0
                            000045
2352      P:000695 P:000697 00000C            RTS
2353   
2354   
2355                                BUFFER_INFORM_CHECK
2356      P:000696 P:000698 56F000            MOVE              X:QT_INFORM_IDX,A
                            000049
2357      P:000698 P:00069A 014180            ADD     #1,A
2358      P:000699 P:00069B 57F000            MOVE              X:QT_INFORM,B
                            000044
2359      P:00069B P:00069D 20000D            CMP     A,B
2360      P:00069C P:00069E 0AF0A7            JGT     BUFFER_INFORM_OK                  ; If inform_idx + 1 <= inform
                            0006A0
2361      P:00069E P:0006A0 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2362   
2363                                BUFFER_INFORM_OK
2364      P:0006A0 P:0006A2 567000            MOVE              A,X:QT_INFORM_IDX       ; inform_idx = inform_idx + 1
                            000049
2365      P:0006A2 P:0006A4 00000C            RTS
2366   
2367   
2368                                ;---------------------------------------------------------------
2369                                BUFFER_INFORM
2370                                ;---------------------------------------------------------------
2371                                ; Informs host of current buffer status
2372   
2373      
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 49



2374      P:0006A3 P:0006A5 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            0006BB
2375      P:0006A5 P:0006A7 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            0006BB
2376   
2377      P:0006A7 P:0006A9 0D0498            JSR     PCI_LOCKDOWN                      ; Disable host IRQ
2378   
2379      P:0006A8 P:0006AA 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2380      P:0006AA P:0006AC 440B00            MOVE              X0,X:<DTXS_WD1
2381   
2382      P:0006AB P:0006AD 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            000045
2383      P:0006AD P:0006AF 440C00            MOVE              X0,X:<DTXS_WD2
2384   
2385      P:0006AE P:0006B0 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000046
2386      P:0006B0 P:0006B2 440D00            MOVE              X0,X:<DTXS_WD3
2387   
2388      P:0006B1 P:0006B3 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
                            00004A
2389      P:0006B3 P:0006B5 440E00            MOVE              X0,X:<DTXS_WD4
2390   
2391      P:0006B4 P:0006B6 0D049E            JSR     PCI_MESSAGE_TO_HOST
2392   
2393      P:0006B5 P:0006B7 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
2394      P:0006B7 P:0006B9 240000            MOVE              #0,X0                   ; Reset inform index
2395      P:0006B8 P:0006BA 447000            MOVE              X0,X:QT_INFORM_IDX
                            000049
2396      P:0006BA P:0006BC 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
2397                                INFORM_EXIT
2398      P:0006BB P:0006BD 00000C            RTS
2399   
2400   
2401   
2402                                ;----------------------------------------------;
2403                                ;  ADDRESS HANDLING                            ;
2404                                ;----------------------------------------------;
2405   
2409   
2410                                LOAD_HILO_ADDRESS
2411      
2412      
2413      P:0006BC P:0006BE 200013            CLR     A
2414      P:0006BD P:0006BF 50D800            MOVE              X:(R0)+,A0
2415      P:0006BE P:0006C0 44D000            MOVE              X:(R0)-,X0
2416      P:0006BF P:0006C1 0C1940            INSERT  #$010010,X0,A
                            010010
2417      P:0006C1 P:0006C3 00000C            RTS
2418   
2419                                ADD_HILO_ADDRESS
2420      
2421      
2422   
2423      P:0006C2 P:0006C4 0D06BC            JSR     LOAD_HILO_ADDRESS
2424      P:0006C3 P:0006C5 200010            ADD     B,A
2425   
2426                                SAVE_HILO_ADDRESS
2427      
2428      
2429   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  main.asm  Page 50



2430      P:0006C4 P:0006C6 445800            MOVE              X0,X:(R0)+              ; pre-increment
2431      P:0006C5 P:0006C7 240000            MOVE              #0,X0
2432      P:0006C6 P:0006C8 0C1D11            ASL     #8,A,B
2433      P:0006C7 P:0006C9 0C1940            INSERT  #$008010,X0,A
                            008010
2434      P:0006C9 P:0006CB 555000            MOVE              B1,X:(R0)-              ; store hi16
2435      P:0006CA P:0006CC 506000            MOVE              A0,X:(R0)
2436      P:0006CB P:0006CD 0C1C90            ASR     #8,B,A
2437      P:0006CC P:0006CE 00000C            RTS
2438   
2439   
2440                                BOOTCODE_END
2441                                 BOOTEND_ADDR
2442      0006CD                              EQU     @CVI(BOOTCODE_END)
2443   
2444                                PROGRAM_END
2445      0006CD                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2446                                          INCLUDE 'vars.asm'
2447                                      COMMENT *
2448   
2449                                Variable table and bit defines for our variables.
2450   
2451                                See info.asm for versioning and authors.
2452   
2453                                        *
2454   
2455   
2456                                ; The variable table is mapped to X memory but stored inline in the
2457                                ; eeprom / P memory after the main code (but before the application
2458                                ; area).
2459   
2460      X:000000 P:0006CF                   ORG     X:VAR_TBL,P:
2461   
2462   
2463                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2464                                 VAR_TBL_START
2465      0006CD                              EQU     @LCV(L)-2
2466                                          ENDIF
2467   
2468                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2470                                          ENDIF
2471   
2472                                ; -----------------------------------------------
2473 d    X:000000 P:0006CF 000000  STATUS    DC      0                                 ; Internal control flags
2474 d    X:000001 P:0006D0 000000  MODE      DC      0                                 ; Configure special options
2475   
2476 d                               FRAME_COUNT
2477 d    X:000002 P:0006D1 000000            DC      0
2478 d    X:000003 P:0006D2 550105  REV_NUMBER DC     $550105                           ; byte 0 = minor revision #
2479                                                                                    ; byte 1 = major revision #
2480                                                                                    ; byte 2 = release Version (ascii letter)
2481 d    X:000004 P:0006D3 000000  REV_DATA  DC      $000000                           ; data: day-month-year
2482 d    X:000005 P:0006D4 2EF490  P_CHECKSUM DC     $2EF490                           ;**** DO NOT CHANGE
2483                                ; -------------------------------------------------
2484 d    X:000006 P:0006D5 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2485                                ; ----------------------------------------------------------------------------------------------
----------------
2486   
2487 d    X:000007 P:0006D6 000000  DRXR_WD1  DC      0
2488 d    X:000008 P:0006D7 000000  DRXR_WD2  DC      0
2489 d    X:000009 P:0006D8 000000  DRXR_WD3  DC      0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  vars.asm  Page 51



2490 d    X:00000A P:0006D9 000000  DRXR_WD4  DC      0
2491 d    X:00000B P:0006DA 000000  DTXS_WD1  DC      0
2492 d    X:00000C P:0006DB 000000  DTXS_WD2  DC      0
2493 d    X:00000D P:0006DC 000000  DTXS_WD3  DC      0
2494 d    X:00000E P:0006DD 000000  DTXS_WD4  DC      0
2495   
2496 d    X:00000F P:0006DE 000000  HEAD_W1_0 DC      0                                 ; Preamble $A5A5
2497 d    X:000010 P:0006DF 000000  HEAD_W1_1 DC      0                                 ;          $A5A5
2498 d    X:000011 P:0006E0 000000  HEAD_W2_0 DC      0                                 ;          $5A5A
2499 d    X:000012 P:0006E1 000000  HEAD_W2_1 DC      0                                 ;          $5A5A
2500 d    X:000013 P:0006E2 000000  HEAD_W3_0 DC      0                                 ; 'RP' or 'DA'
2501 d    X:000014 P:0006E3 000000  HEAD_W3_1 DC      0                                 ; '  '   $2020
2502 d    X:000015 P:0006E4 000000  HEAD_W4_0 DC      0                                 ; Packet size LSW
2503 d    X:000016 P:0006E5 000000  HEAD_W4_1 DC      0                                 ;             MSW
2504   
2505 d    X:000017 P:0006E6 000000  SV_A0     DC      0
2506 d    X:000018 P:0006E7 000000  SV_A1     DC      0
2507 d    X:000019 P:0006E8 000000  SV_A2     DC      0
2508 d    X:00001A P:0006E9 000000  SV_B0     DC      0
2509 d    X:00001B P:0006EA 000000  SV_B1     DC      0
2510 d    X:00001C P:0006EB 000000  SV_B2     DC      0
2511 d    X:00001D P:0006EC 000000  SV_X0     DC      0
2512 d    X:00001E P:0006ED 000000  SV_X1     DC      0
2513 d    X:00001F P:0006EE 000000  SV_Y0     DC      0
2514 d    X:000020 P:0006EF 000000  SV_Y1     DC      0
2515 d    X:000021 P:0006F0 000000  SV_R0     DC      0
2516   
2517 d    X:000022 P:0006F1 000000  SV_SR     DC      0                                 ; stauts register save.
2518   
2519 d                               PACKET_SIZE_LOW
2520 d    X:000023 P:0006F2 000000            DC      0
2521 d                               PACKET_SIZE_HIH
2522 d    X:000024 P:0006F3 000000            DC      0
2523   
2524 d    X:000025 P:0006F4 00A5A5  PREAMB1   DC      $A5A5                             ; pramble 16-bit word....2 of which make up 
first preamble 32bit word
2525 d    X:000026 P:0006F5 005A5A  PREAMB2   DC      $5A5A                             ; preamble 16-bit word....2 of which make up
 second preamble 32bit word
2526   
2527 d                               TOTAL_BUFFS
2528 d    X:000027 P:0006F6 000000            DC      0                                 ; total number of 512 buffers in packet
2529 d                               LEFT_TO_READ
2530 d    X:000028 P:0006F7 000000            DC      0                                 ; number of words (16 bit) left to read afte
r last 512 buffer
2531 d                               LEFT_TO_WRITE
2532 d    X:000029 P:0006F8 000000            DC      0                                 ; number of woreds (32 bit) to write to host
 i.e. half of those left over read
2533 d                               NUM_LEFTOVER_BLOCKS
2534 d    X:00002A P:0006F9 000000            DC      0                                 ; small block DMA burst transfer
2535   
2536 d                               PACKET_SIZE
2537 d    X:00002B P:0006FA 000000            DC      0                                 ; Size, in dwords of most recent packet from
 MCE.
2538   
2539   
2541 d                               PREAMBLE_ERRORS
2542 d    X:00002C P:0006FB 000000            DC      0                                 ; Failed on preamble processing
2543 d                               PTYPE_ERRORS
2544 d    X:00002D P:0006FC 000000            DC      0                                 ; Failed on packet type
2545 d                               PSIZE_ERRORS
2546 d    X:00002E P:0006FD 000000            DC      0                                 ; Failed on packet size test
2547   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  vars.asm  Page 52



2549   
2550 d    X:00002F P:0006FE 000000  BLOCK_SIZE DC     0
2551 d    X:000030 P:0006FF 000000  BURST_SIZE DC     0
2552 d                               BURST_DEST_LO
2553 d    X:000031 P:000700 000000            DC      0
2554 d                               BURST_DEST_HI
2555 d    X:000032 P:000701 000000            DC      0
2556 d                               BURST_SRC_LO
2557 d    X:000033 P:000702 000000            DC      0
2558 d                               BURST_SRC_HI
2559 d    X:000034 P:000703 000000            DC      0
2560 d    X:000035 P:000704 000000  YMEM_SRC  DC      0
2561 d    X:000036 P:000705 000000  YMEM_DEST DC      0
2562   
2563 d    X:000037 P:000706 000000  DMA_ERRORS DC     0
2564 d    X:000038 P:000707 000000  EC_TRTY   DC      0
2565 d    X:000039 P:000708 000000  EC_TO     DC      0
2566 d    X:00003A P:000709 000000  EC_TDIS   DC      0
2567 d    X:00003B P:00070A 000000  EC_TAB    DC      0
2568 d    X:00003C P:00070B 000000  EC_MAB    DC      0
2569 d    X:00003D P:00070C 000000  EC_DPER   DC      0
2570 d    X:00003E P:00070D 000000  EC_APER   DC      0
2571   
2572   
2574   
2575 d    X:00003F P:00070E 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2576 d    X:000040 P:00070F 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2577 d                               QT_BUF_SIZE
2578 d    X:000041 P:000710 000000            DC      0                                 ; Separation of buffers, in bytes
2579 d    X:000042 P:000711 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2580 d                               QT_FRAME_SIZE
2581 d    X:000043 P:000712 000000            DC      0                                 ; Expected data packet size, in bytes
2582 d    X:000044 P:000713 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2583   
2584 d                               QT_BUF_HEAD
2585 d    X:000045 P:000714 000000            DC      0                                 ; Index of buf for next write
2586 d                               QT_BUF_TAIL
2587 d    X:000046 P:000715 000000            DC      0                                 ; Index at which we must not write
2588   
2589 d    X:000047 P:000716 000000  QT_DEST_LO DC     0                                 ; PC address for next write
2590 d    X:000048 P:000717 000000  QT_DEST_HI DC     0                                 ;
2591 d                               QT_INFORM_IDX
2592 d    X:000049 P:000718 000000            DC      0                                 ; Number of packets since last inform
2593 d    X:00004A P:000719 000000  QT_DROPS  DC      0                                 ; Dropped packets
2594   
2595   
2597 d    X:00004B P:00071A 000000  RP_BASE_LO DC     0
2598 d    X:00004C P:00071B 000000  RP_BASE_HI DC     0
2599 d                               RP_MAX_SIZE
2600 d    X:00004D P:00071C 000000            DC      0
2601 d    X:00004E P:00071D 000000  RP_DROPS  DC      0
2602   
2604 d    X:00004F P:00071E 000000  CON_SRC_LO DC     0
2605 d    X:000050 P:00071F 000000  CON_SRC_HI DC     0
2606   
2608 d                               PCI_BURST_SIZE
2609 d    X:000051 P:000720 000040            DC      $40                               ; Should be < 4*latency assigned by OS
2610   
2612 d                               TIMER_INDEX
2613 d    X:000052 P:000721 000000            DC      0
2614   
2616   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  vars.asm  Page 53



2617 d    X:000053 P:000722 000000  BDEBUG0   DC      0
2618 d    X:000054 P:000723 000000  BDEBUG1   DC      0
2619 d    X:000055 P:000724 000000  BDEBUG2   DC      0
2620 d    X:000056 P:000725 000000  BDEBUG3   DC      0
2621 d    X:000057 P:000726 000000  BDEBUG4   DC      0
2622 d    X:000058 P:000727 000000  BDEBUG5   DC      0
2623 d    X:000059 P:000728 000000  BDEBUG6   DC      0
2624 d    X:00005A P:000729 000000  BDEBUG7   DC      0
2625 d    X:00005B P:00072A 000000  BDEBUG8   DC      0
2626 d    X:00005C P:00072B 000000  BDEBUG9   DC      0
2627   
2628                                ;----------------------------------------------------------
2629   
2631   
2632                                 APPLICATION_RUNNING
2633      000000                              EQU     0                                 ; Indicates application is in progress
2634                                 SEND_TO_HOST
2635      000001                              EQU     1                                 ; set in HST ISR when host ready for packet 
(stays set until after HST reply)
2636                                 FATAL_ERROR
2637      000002                              EQU     2                                 ; PCI message to host error detected by driv
er....
2638      000003                    FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays s
et till packet processed
2639   
2640                                ; PREAMBLE_ERROR                EQU     6   ; set if preamble error detected
2641                                ; DATA_DLY              EQU     7   ; set in CON ISR if MCE command is 'GO'.  USed to add delay 
to first returned data packet
2642   
2643      000009                    HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of p
acket (stays set until after HST reply)
2644   
2645      00000A                    CON_DEMAND EQU    10                                ; Host has requested an MCE command be sent
2646      00000B                    CON_MCE   EQU     11                                ; Command has been copied and we should send
 it to the MCE
2647   
2648                                 PCIDMA_RESTART
2649      000010                              EQU     16                                ; DMA flags used for error recovery
2650                                 PCIDMA_RESUME
2651      000011                              EQU     17
2652                                 PCIDMA_RETRY
2653      000012                              EQU     18
2654   
2655      000014                    QT_FLUSH  EQU     20                                ; Set when it is time to inform Host of curr
ent buffer position.
2656                                 RP_BUFFER_FULL
2657      000015                              EQU     21                                ; Set when Quiet RP buffer is occupied.
2658   
2659      000016                    FREEZER   EQU     22                                ; Suspend operations and just idle in the ma
in loop
2660                                 MAIN_LOOP_POLL
2661      000017                              EQU     23                                ; Cleared by the main loop, use to check for
 DSP lock-up
2662   
2664   
2665                                 MODE_APPLICATION
2666      000000                              EQU     0                                 ; set if PCI application to run
2667      000001                    MODE_MCE  EQU     1                                 ; process packets from MCE
2668      000002                    MODE_QT   EQU     2                                 ; Quiet transfer for data packets
2669                                 MODE_RP_BUFFER
2670      000003                              EQU     3                                 ; Quiet transfer for reply packets
2671   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  vars.asm  Page 54



2672                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2673                                 VAR_TBL_END
2674      00072A                              EQU     @LCV(L)-2
2675                                          ENDIF
2676   
2677                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2679                                          ENDIF
2680   
2681                                 VAR_TBL_LENGTH
2682      00005D                              EQU     VAR_TBL_END-VAR_TBL_START
2683                                          INCLUDE 'app.asm'
2684                                        COMMENT *
2685   
2686                                Auxiliary application area.
2687   
2688                                See info.asm for versioning and authors.
2689   
2690                                        *
2691                                          PAGE    132                               ; Printronix page width - 132 columns
2692                                          OPT     CEX                               ; print DC evaluations
2693   
2694                                          IF      @CVS(N,*)>=APPLICATION
2696                                          ENDIF
2697   
2698   
2699                                ;--------------------------------------------
2700                                ; APPLICATION AREA
2701                                ;---------------------------------------------
2702                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2703      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2704                                          ENDIF
2705   
2706                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2708                                          ENDIF
2709   
2710                                ; starts with no application loaded
2711                                ; so just reply with an error if we get a GOA command
2712   
2713      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2714      P:000802 P:000804 440B00            MOVE              X0,X:<DTXS_WD1          ; REPly
2715      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2716      P:000805 P:000807 440C00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2717      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2718      P:000808 P:00080A 440D00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2719      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2720      P:00080B P:00080D 440E00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2721      P:00080C P:00080E 0D04BE            JSR     <RESTORE_REGISTERS
2722      P:00080D P:00080F 0D049E            JSR     <PCI_MESSAGE_TO_HOST
2723      P:00080E P:000810 0A0100            BCLR    #MODE_APPLICATION,X:<MODE
2724      P:00080F P:000811 0C016E            JMP     PACKET_IN
2725   
2726   
2727      000812                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM
2728   
2729   
2730   

0    Errors
Motorola DSP56300 Assembler  Version 6.3.4   09-06-06  22:27:45  build.asm  Page 55



0    Warnings


