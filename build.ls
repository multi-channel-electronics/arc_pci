Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  build.asm  Page 1



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  header.asm  Page 2



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  header.asm  Page 3



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
150       000004           DCTR_HF4  EQU     4                                 ;
151       000005           DCTR_HF5  EQU     5                                 ;
152       000006           INTA      EQU     6                                 ; Request PCI interrupt
153    
154                        ; The DSR host flags are written by the PCI host and read by the DSP
155       000003           DSR_HF0   EQU     3                                 ; PC side INTA hand-shaking
156       000004           DSR_HF1   EQU     4                                 ;
157       000005           DSR_HF2   EQU     5                                 ;
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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  header.asm  Page 4



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  header.asm  Page 5



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  init.asm  Page 6



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
                            0004D3
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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  init.asm  Page 7



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  init.asm  Page 8



360                                 ;**************************************************************************
361                                 ; Check for program space overwriting of ISR starting at P:$72
362                                           IF      @CVS(N,*)>$71
364                                           ENDIF
365    
366                                 ;       ORG     P:$72,P:$72
367       P:000072 P:000074                   ORG     P:$72,P:$74
368    
369                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
370                                 ; command converter
371                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
373                                           ENDIF
374    
375    
376                                 ;**************************************************************************
377    
378                                 ; Three non-maskable fast interrupt service routines for clearing PCI interrupts
379                                 ; The Host will use these to clear the INTA* after it has serviced the interrupt
380                                 ; which had been generated by the PCI board.
381    
382       P:000072 P:000074 0A8506            BCLR    #INTA,X:DCTR                      ; $72/3 - Clear PCI interrupt
383       P:000073 P:000075 000000            NOP
384    
385       P:000074 P:000076 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; clear interrupt flag
386       P:000075 P:000077 000000            NOP                                       ; needs to be fast addressing <
387    
388       P:000076 P:000078 0A0022            BSET    #FATAL_ERROR,X:<STATUS            ; $76/7 - driver PCI_MESSAGE_TO_HOST error
389       P:000077 P:000079 000000            NOP
390    
391                                 ; Interrupt locations for 7 available commands on PCI board
392                                 ; Each JSR takes up 2 locations in the table
393       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            0003D5
394       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            0003AA
395       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            0003F6
396       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            0003FF
397                                 ; software reset is the same as cleaning up the PCI - use same routine
398                                 ; when HOST does a RESET then this routine is run
399       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            0004EA
400       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            000502
401       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            0004DB
402       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            00040A
403    
404                                 ; QT - set command
405       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            000428
406       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            0004CA
407    
408                                 ; Quiet RP mode, clear buffer full flag
409       P:00008C P:00008E 0A0015            BCLR    #RP_BUFFER_FULL,X:<STATUS         ; $8C
410       P:00008D P:00008F 000000            NOP
411    
412                                 ; Disable PCI interrupts
413       P:00008E P:000090 0A0124            BSET    #MODE_NOIRQ,X:<MODE               ; $8E
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  init.asm  Page 9



414       P:00008F P:000091 000000            NOP
415    
416                                 ; Enable PCI interrupts
417       P:000090 P:000092 0A0104            BCLR    #MODE_NOIRQ,X:<MODE               ; $90
418       P:000091 P:000093 000000            NOP
419    
420                                 ; Enable interrupt hand-shaking
421       P:000092 P:000094 0A0125            BSET    #MODE_HANDSHAKE,X:<MODE           ; $92
422       P:000093 P:000095 000000            NOP
423    
424                                 ; Mode setting pretty-fast interrupt
425       P:000094 P:000096 0BF080            JSR     MODE_SET_FAST                     ; $94
                            0004BF
426    
427                                 ; ***********************************************************************
428                                 ; For now have boot code starting from P:$100
429                                 ; just to make debugging tidier etc.
430    
431       P:000100 P:000102                   ORG     P:$100,P:$102
432    
433                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
434                                 ; command converter
435                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
437                                           ENDIF
438                                 ; ***********************************************************************
439    
440    
441    
442                                 ; ******************************************************************
443                                 ;
444                                 ;       AA0 = RDFIFO* of incoming fiber optic data
445                                 ;       AA1 = EEPROM access
446                                 ;       AA2 = DRAM access
447                                 ;       AA3 = output to parallel data connector, for a video pixel clock
448                                 ;       $FFxxxx = Write to fiber optic transmitter
449                                 ;
450                                 ; ******************************************************************
451    
452    
453       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
454       P:000102 P:000104 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
455       P:000103 P:000105 0A8515            BCLR    #21,X:DCTR
456       P:000104 P:000106 0A8516            BCLR    #22,X:DCTR
457       P:000105 P:000107 000000            NOP
458       P:000106 P:000108 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
459       P:000107 P:000109 000000            NOP
460       P:000108 P:00010A 000000            NOP                                       ; End of PCI programming
461    
462    
463                                 ; Set operation mode register OMR to normal expanded
464       P:000109 P:00010B 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
465       P:00010A P:00010C 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
466    
467                                 ; Program the serial port ESSI0 = Port C for serial transmission to
468                                 ;   the timing board
469       P:00010B P:00010D 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
470                                 ;**********************************************************************
471       P:00010D P:00010F 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
472                                                                                     ; DC0-CD4 = 0 for non-network operation
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  init.asm  Page 10



473                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
474                                                                                     ; SSC1 = 0 for SC1 not used
475                                 ;************************************************************************
476       P:00010F P:000111 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
                            010120
477                                                                                     ; SHFD = 0 for MSB shifted first
478                                                                                     ; CKP = 0 for rising clock edge transitions
479                                                                                     ; TE0 = 1 to enable transmitter #0
480                                                                                     ; MOD = 0 for normal, non-networked mode
481                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
482       P:000111 P:000113 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
483                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
484                                 ;********************************************************************************
485       P:000113 P:000115 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
486       P:000115 P:000117 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
                            000000
487                                 ;***********************************************************************************
488                                 ; 250MHz
489                                 ; Conversion from software bits to schematic labels for Port C and D
490                                 ;       PC0 = SC00 = AUX3               PD0 = SC10 = EF*
491                                 ;       PC1 = SC01 = A/B* = input       PD1 = SC11 = HF*
492                                 ;       PC2 = SC02 = No connect         PD2 = SC12 = RS*
493                                 ;       PC3 = SCK0 = No connect         PD3 = SCK1 = NWRFIFO*
494                                 ;       PC4 = SRD0 = AUX1               PD4 = SRD1 = No connect (** in 50Mhz this was MODE selec
t for 16 or 32 bit FO)
495                                 ;       PC5 = STD0 = No connect         PD5 = STD1 = WRFIFO*
496                                 ; ***********************************************************************************
497    
498    
499                                 ; ****************************************************************************
500                                 ; Program the serial port ESSI1 = Port D for general purpose I/O (GPIO)
501    
502       P:000117 P:000119 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
503       P:000119 P:00011B 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            00001C
504       P:00011B P:00011D 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
505       P:00011D P:00011F 060AA0            REP     #10
506       P:00011E P:000120 000000            NOP
507       P:00011F P:000121 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
508                                                                                     ; was %011100
509    
510                                 ; Program the SCI port to benign values
511       P:000121 P:000123 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
512       P:000123 P:000125 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
513       P:000125 P:000127 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
514                                 ;       PE0 = RXD
515                                 ;       PE1 = TXD
516                                 ;       PE2 = SCLK
517    
518                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
519       P:000127 P:000129 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  init.asm  Page 11



520       P:000129 P:00012B 07F40B            MOVEP             #$2800,X:TCSR1
                            002800
521       P:00012B P:00012D 07F407            MOVEP             #$2800,X:TCSR2
                            002800
522    
523    
524                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
525       P:00012D P:00012F 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
526       P:00012F P:000131 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
e
                            008929
527       P:000131 P:000133 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
                            000122
528    
529    
530                                 ; Program the DRAM memory access and addressing
531       P:000133 P:000135 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
                            020022
532       P:000135 P:000137 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
533    
534    
535                                 ; Clear all PCI error conditions
536       P:000137 P:000139 084E0A            MOVEP             X:DPSR,A
537       P:000138 P:00013A 0140C2            OR      #$1FE,A
                            0001FE
538       P:00013A P:00013C 000000            NOP
539       P:00013B P:00013D 08CE0A            MOVEP             A,X:DPSR
540    
541                                 ; Status word and interrupt configuration.
542       P:00013C P:00013E 08F4BF            MOVEP             #>MY_IPRC,X:IPRC
                            0001C0
543       P:00013E P:000140 08F4BE            MOVEP             #>MY_IPRP,X:IPRP
                            000002
544       P:000140 P:000142 05F439            MOVE              #>MY_SR,SR
                            000100
545    
546    
547                                 ;--------------------------------------------------------------------------
548                                 ; Initialize the fiber optic serial transmitter to zero
549       P:000142 P:000144 01B786            JCLR    #TDE,X:SSISR0,*
                            000142
550       P:000144 P:000146 07F43C            MOVEP             #$000000,X:TX00
                            000000
551    
552                                 ;--------------------------------------------------------------------
553    
554                                 ; clear DTXM - PCI master transmitter
555       P:000146 P:000148 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
556       P:000147 P:000149 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000147
557    
558                                 ;----------------------------------------------------------------------
559                                 ; clear DRXR - PCI receiver
560    
561       P:000149 P:00014B 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014E
562       P:00014B P:00014D 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
563       P:00014C P:00014E 000000            NOP
564       P:00014D P:00014F 0C0149            JMP     <CLR0
565                                 CLR1
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  init.asm  Page 12



566    
567                                 ;-----------------------------------------------------------------------------
568                                 ; copy parameter table from P memory into X memory
569    
570                                 ; but not frame_count and num_dumped - don't want these reset by fatal error...
571                                 ; they will be reset by new packet or pci_reset ISR
572    
573       P:00014E P:000150 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000006
574       P:000150 P:000152 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
                            000002
575    
576                                 ; Move the table of constants from P: space to X: space
577       P:000152 P:000154 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            000764
578       P:000154 P:000156 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
579       P:000155 P:000157 065D80            DO      #VAR_TBL_LENGTH,X_WRITE
                            000158
580       P:000157 P:000159 07D984            MOVE              P:(R1)+,X0
581       P:000158 P:00015A 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
582                                 X_WRITE
583    
584       P:000159 P:00015B 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
                            000006
585       P:00015B P:00015D 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000002
586    
587                                 ;-------------------------------------------------------------------------------
588                                 ; Initialise MODE; packet choke and PCI interrupts are ON.
589                                 ;  - that's MODE=0 .
590    
591    
592                                 ;----------------------------------------------------------------------------
593                                 ; Initialize PCI controller again, after booting, to make sure it sticks
594       P:00015D P:00015F 0A8514            BCLR    #20,X:DCTR                        ; Terminate and reset mode
595       P:00015E P:000160 000000            NOP
596       P:00015F P:000161 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00015F
597       P:000161 P:000163 000000            NOP
598       P:000162 P:000164 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
599       P:000163 P:000165 000000            NOP
600       P:000164 P:000166 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000164
601    
602       
603       P:000166 P:000168 0BF080            JSR     CLEAR_FO_FIFO                     ; Clear the fibre fifo!
                            0005B8
604       P:000168 P:00016A 013D24            BSET    #AUX1,X:PDRC                      ; Enable byte-swapping - still necc. on ARC-
64
605       P:000169 P:00016B 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host interrupts
606       P:00016A P:00016C 0BF080            JSR     TIMER_DISABLE                     ; Disable NFY timer
                            0006EA
607       P:00016C P:00016E 0BF080            JSR     TIMER_STORE_INIT                  ; Initialize timing buffer
                            0006FE
608    
610                                           INCLUDE 'main.asm'
611    
612                                                 COMMENT *
613    
614                                 Main section of the pci card code.
615    
616                                 See info.asm for versioning and authors.
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 13



617    
618                                         *
619                                           PAGE    132                               ; Printronix page width - 132 columns
620                                           OPT     CEX                               ; print DC evaluations
621    
625    
626                                 PACKET_IN
627    
628       
629       P:00016E P:000170 0A0017            BCLR    #MAIN_LOOP_POLL,X:<STATUS
630    
631       
632       P:00016F P:000171 0A00B6            JSET    #FREEZER,X:<STATUS,PACKET_IN
                            00016E
633    
634       
635       P:000171 P:000173 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
636    
637       
638       P:000173 P:000175 0A01A0            JSET    #MODE_APPLICATION,X:<MODE,APPLICATION
                            000800
639    
640       
641       P:000175 P:000177 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            0006F0
642    
643       
644       P:000177 P:000179 0B00B4            JSSET   #QT_FLUSH,X:STATUS,BUFFER_INFORM
                            00073A
645    
646       
647       P:000179 P:00017B 0D0516            JSR     <CHECK_FO
648       P:00017A P:00017C 0B00A3            JSSET   #FO_WRD_RCV,X:STATUS,HANDLE_FIFO
                            000183
649    
650       
651       P:00017C P:00017E 0B00AB            JSSET   #CON_MCE,X:STATUS,CON_NOW_TRANSMIT
                            000317
652       P:00017E P:000180 0B00AA            JSSET   #CON_DEMAND,X:STATUS,CON_NOW
                            0002FB
653    
654       
655       P:000180 P:000182 000000            NOP
656       P:000181 P:000183 000000            NOP
657    
658       
659       P:000182 P:000184 0C016E            JMP     PACKET_IN
660    
664    
665    
666    
668    
669                                 HANDLE_FIFO
670       P:000183 P:000185 54F400            MOVE              #>$A00,A1
                            000A00
671       P:000185 P:000187 0BF080            JSR     TIMER_STORE_A1
                            000707
672       P:000187 P:000189 0BF080            JSR     TIMER_STORE
                            000705
673    
674       
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 14



675       P:000189 P:00018B 60F400            MOVE              #>HEAD_W1_0,R0
                            00000F
676       P:00018B P:00018D 44F400            MOVE              #>$00FFFF,X0            ; Mask lower 16 bits
                            00FFFF
677       P:00018D P:00018F 220800            MOVE              R0,A0
678       P:00018E P:000190 060880            DO      #8,HANDLE_FIFO_CHECK_PREAMBLE
                            000199
679                                 HANDLE_FIFO_WAIT
680       P:000190 P:000192 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000190
681       P:000192 P:000194 000000            NOP
682       P:000193 P:000195 000000            NOP
683       P:000194 P:000196 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000190
684       P:000196 P:000198 094E3F            MOVEP             Y:RDFIFO,A
685       P:000197 P:000199 200046            AND     X0,A
686       P:000198 P:00019A 000000            NOP
687       P:000199 P:00019B 545800            MOVE              A1,X:(R0)+
688    
689                                 HANDLE_FIFO_CHECK_PREAMBLE
690       P:00019A P:00019C 60F400            MOVE              #>HEAD_W1_0,R0
                            00000F
691       P:00019C P:00019E 20001B            CLR     B
692       P:00019D P:00019F 200013            CLR     A
693       P:00019E P:0001A0 57D800            MOVE              X:(R0)+,B
694       P:00019F P:0001A1 0140CD            CMP     #>$A5A5,B
                            00A5A5
695       P:0001A1 P:0001A3 0AF0A2            JNE     PRE_ERROR
                            0001F4
696       P:0001A3 P:0001A5 57D800            MOVE              X:(R0)+,B
697       P:0001A4 P:0001A6 0140CD            CMP     #>$A5A5,B
                            00A5A5
698       P:0001A6 P:0001A8 0AF0A2            JNE     PRE_ERROR
                            0001F4
699       P:0001A8 P:0001AA 57D800            MOVE              X:(R0)+,B
700       P:0001A9 P:0001AB 0140CD            CMP     #>$5A5A,B
                            005A5A
701       P:0001AB P:0001AD 0AF0A2            JNE     PRE_ERROR
                            0001F4
702       P:0001AD P:0001AF 57D800            MOVE              X:(R0)+,B
703       P:0001AE P:0001B0 0140CD            CMP     #>$5A5A,B
                            005A5A
704       P:0001B0 P:0001B2 0AF0A2            JNE     PRE_ERROR
                            0001F4
705    
706       
707       P:0001B2 P:0001B4 50F000            MOVE              X:>(HEAD_W1_0+6),A0
                            000015
708       P:0001B4 P:0001B6 44F000            MOVE              X:>(HEAD_W1_0+7),X0
                            000016
709       P:0001B6 P:0001B8 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
710    
711       
712       P:0001B8 P:0001BA 0BF080            JSR     PACKET_PARTITIONS
                            000684
713       P:0001BA P:0001BC 0AF080            JMP     XXXX
                            0001E6
714    
715                                 OLD_HANDLE_FIFO
716    
717       P:0001BC P:0001BE 0A0181            JCLR    #MODE_MCE,X:<MODE,RETURN_NOW      ; IF MCE Packet choke on - just keep clearin
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 15



g FIFO.
                            000202
718       P:0001BE P:0001C0 440F00            MOVE              X0,X:<HEAD_W1_0         ;store received word
719       P:0001BF P:0001C1 56F000            MOVE              X:PREAMB1,A
                            000025
720       P:0001C1 P:0001C3 200045            CMP     X0,A                              ; check it is correct
721       P:0001C2 P:0001C4 0E21F4            JNE     <PRE_ERROR                        ; if not go to start
722    
723       P:0001C3 P:0001C5 0D0526            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
724       P:0001C4 P:0001C6 441000            MOVE              X0,X:<HEAD_W1_1         ;store received word
725       P:0001C5 P:0001C7 56F000            MOVE              X:PREAMB1,A
                            000025
726       P:0001C7 P:0001C9 200045            CMP     X0,A                              ; check it is correct
727       P:0001C8 P:0001CA 0E21F4            JNE     <PRE_ERROR                        ; if not go to start
728    
729       P:0001C9 P:0001CB 0D0526            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
730       P:0001CA P:0001CC 441100            MOVE              X0,X:<HEAD_W2_0         ;store received word
731       P:0001CB P:0001CD 56F000            MOVE              X:PREAMB2,A
                            000026
732       P:0001CD P:0001CF 200045            CMP     X0,A                              ; check it is correct
733       P:0001CE P:0001D0 0E21F4            JNE     <PRE_ERROR                        ; if not go to start
734    
735       P:0001CF P:0001D1 0D0526            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
736       P:0001D0 P:0001D2 441200            MOVE              X0,X:<HEAD_W2_1         ;store received word
737       P:0001D1 P:0001D3 56F000            MOVE              X:PREAMB2,A
                            000026
738       P:0001D3 P:0001D5 200045            CMP     X0,A                              ; check it is correct
739       P:0001D4 P:0001D6 0E21F4            JNE     <PRE_ERROR                        ; if not go to start
740    
741                                 PACKET_INFO                                         ; packet preamble valid
742       P:0001D5 P:0001D7 0D0526            JSR     <WT_FIFO
743       P:0001D6 P:0001D8 441300            MOVE              X0,X:<HEAD_W3_0         ; RP or DA
744       P:0001D7 P:0001D9 0D0526            JSR     <WT_FIFO
745       P:0001D8 P:0001DA 441400            MOVE              X0,X:<HEAD_W3_1         ; $2020
746    
747       P:0001D9 P:0001DB 0D0526            JSR     <WT_FIFO
748       P:0001DA P:0001DC 441500            MOVE              X0,X:<HEAD_W4_0         ; packet size lo
749       P:0001DB P:0001DD 0D0526            JSR     <WT_FIFO
750       P:0001DC P:0001DE 441600            MOVE              X0,X:<HEAD_W4_1         ; packet size hi
751    
752       
753       P:0001DD P:0001DF 200013            CLR     A
754       P:0001DE P:0001E0 50F000            MOVE              X:HEAD_W4_0,A0
                            000015
755       P:0001E0 P:0001E2 44F000            MOVE              X:HEAD_W4_1,X0
                            000016
756       P:0001E2 P:0001E4 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
757    
758       
759       P:0001E4 P:0001E6 0BF080            JSR     PACKET_PARTITIONS
                            000684
760                                 XXXX
761       P:0001E6 P:0001E8 0BF080            JSR     TIMER_STORE
                            000705
763       P:0001E8 P:0001EA 56F000            MOVE              X:HEAD_W3_0,A
                            000013
764    
765       P:0001EA P:0001EC 0140C5            CMP     #>'RP',A
                            005250
766       P:0001EC P:0001EE 0AF0AA            JEQ     HANDLE_RP
                            000208
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 16



767    
768       P:0001EE P:0001F0 0140C5            CMP     #>'DA',A
                            004441
769       P:0001F0 P:0001F2 0AF0AA            JEQ     HANDLE_DA
                            000250
770    
771       P:0001F2 P:0001F4 0AF080            JMP     QT_PTYPE_ERROR
                            0001FA
772    
773                                 PRE_ERROR
774       P:0001F4 P:0001F6 60F400            MOVE              #>PREAMBLE_ERRORS,R0
                            00002C
775       P:0001F6 P:0001F8 0BF080            JSR     INCR_X_R0
                            000203
776       P:0001F8 P:0001FA 0AF080            JMP     CLEAR_FO_FIFO                     ; empty the fifo (2 ms!)
                            0005B8
777    
778                                 QT_PTYPE_ERROR
779       P:0001FA P:0001FC 60F400            MOVE              #>PTYPE_ERRORS,R0
                            00002D
780       P:0001FC P:0001FE 0AF080            JMP     INCR_X_R0
                            000203
781                                 QT_FSIZE_ERROR
782       P:0001FE P:000200 60F400            MOVE              #>PSIZE_ERRORS,R0
                            00002E
783       P:000200 P:000202 0AF080            JMP     INCR_X_R0
                            000203
784                                 RETURN_NOW
785       P:000202 P:000204 00000C            RTS
786    
787                                 INCR_X_R0
788       
789       P:000203 P:000205 50E000            MOVE              X:(R0),A0
790       P:000204 P:000206 000008            INC     A
791       P:000205 P:000207 000000            NOP
792       P:000206 P:000208 506000            MOVE              A0,X:(R0)
793       P:000207 P:000209 00000C            RTS
794    
795    
796    
799    
800                                 HANDLE_RP
801       
802       P:000208 P:00020A 0A0183            JCLR    #MODE_RP_BUFFER,X:MODE,MCE_PACKET
                            000338
803    
804       
805       P:00020A P:00020C 0A00B5            JSET    #RP_BUFFER_FULL,X:STATUS,HANDLE_RP_DROP
                            00024B
806    
807       
808       P:00020C P:00020E 61F400            MOVE              #>REPLY_BUFFER,R1
                            100000
809       P:00020E P:000210 0BF080            JSR     BUFFER_PACKET
                            000691
810    
811       P:000210 P:000212 54F400            MOVE              #>$b00,A1
                            000B00
812       P:000212 P:000214 0BF080            JSR     TIMER_STORE_A1
                            000707
813       P:000214 P:000216 0BF080            JSR     TIMER_STORE
                            000705
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 17



814    
815       
816       P:000216 P:000218 60F400            MOVE              #RP_BASE_LO,R0
                            00004B
817       P:000218 P:00021A 0BF080            JSR     LOAD_HILO_ADDRESS
                            000753
818    
819       P:00021A P:00021C 60F400            MOVE              #BURST_DEST_LO,R0
                            000031
820       P:00021C P:00021E 0BF080            JSR     SAVE_HILO_ADDRESS
                            00075B
821    
822       
823       P:00021E P:000220 200013            CLR     A
824       P:00021F P:000221 20001B            CLR     B
825       P:000220 P:000222 50F000            MOVE              X:PACKET_SIZE,A0
                            00002B
826       P:000222 P:000224 0C1D04            ASL     #2,A,A                            ; Size in bytes
827       P:000223 P:000225 51F000            MOVE              X:RP_MAX_SIZE,B0
                            00004D
828    
829       P:000225 P:000227 200005            CMP     B,A                               ; A ? B
830       P:000226 P:000228 0AF0AF            JLE     HANDLE_RP1
                            000229
831       P:000228 P:00022A 21EE00            MOVE              B,A
832    
833                                 HANDLE_RP1
834       
835       P:000229 P:00022B 44F400            MOVE              #>REPLY_BUFFER,X0
                            100000
836       P:00022B P:00022D 507000            MOVE              A0,X:BLOCK_SIZE
                            00002F
837       P:00022D P:00022F 447000            MOVE              X0,X:YMEM_SRC
                            000035
838       P:00022F P:000231 0BF080            JSR     TIMER_STORE
                            000705
839       P:000231 P:000233 0BF080            JSR     BLOCK_TRANSFER
                            00061C
840       P:000233 P:000235 0BF080            JSR     TIMER_STORE
                            000705
841    
842       
843       P:000235 P:000237 0BF080            JSR     PCI_LOCKDOWN                      ; Disable host IRQ
                            00053A
844       P:000237 P:000239 44F400            MOVE              #'NFY',X0
                            4E4659
845       P:000239 P:00023B 447000            MOVE              X0,X:DTXS_WD1
                            00000B
846       P:00023B P:00023D 44F400            MOVE              #'RPQ',X0
                            525051
847       P:00023D P:00023F 447000            MOVE              X0,X:DTXS_WD2
                            00000C
848       P:00023F P:000241 507000            MOVE              A0,X:DTXS_WD3           ; A0=block_size
                            00000D
849       P:000241 P:000243 547000            MOVE              A1,X:DTXS_WD4           ; A1=0
                            00000E
850    
851       
852       P:000243 P:000245 0A7035            BSET    #RP_BUFFER_FULL,X:STATUS
                            000000
853       P:000245 P:000247 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000540
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 18



854       P:000247 P:000249 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
855    
856       P:000248 P:00024A 0BF080            JSR     TIMER_STORE
                            000705
857    
858       
859       P:00024A P:00024C 00000C            RTS
860    
861                                 HANDLE_RP_DROP
862       P:00024B P:00024D 60F400            MOVE              #RP_DROPS,R0
                            00004E
863       P:00024D P:00024F 0D0203            JSR     INCR_X_R0
864       P:00024E P:000250 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            0006D0
865    
867    
868    
871    
872    
873                                 HANDLE_DA
874       
875       P:000250 P:000252 60F400            MOVE              #FRAME_COUNT,R0
                            000002
876       P:000252 P:000254 0D0203            JSR     INCR_X_R0
877    
878       
879       P:000253 P:000255 0A0182            JCLR    #MODE_QT,X:MODE,MCE_PACKET
                            000338
880    
881       
882       P:000255 P:000257 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
883       P:000257 P:000259 0BF080            JSR     BUFFER_PACKET
                            000691
884    
885       P:000259 P:00025B 54F400            MOVE              #$e00,A1
                            000E00
886       P:00025B P:00025D 0BF080            JSR     TIMER_STORE_A1
                            000707
887       P:00025D P:00025F 0BF080            JSR     TIMER_STORE
                            000705
888    
889       
890       P:00025F P:000261 56F000            MOVE              X:QT_BUF_HEAD,A
                            000045
891       P:000261 P:000263 014180            ADD     #1,A
892       P:000262 P:000264 57F000            MOVE              X:QT_BUF_MAX,B
                            000042
893       P:000264 P:000266 20000D            CMP     A,B
894       P:000265 P:000267 0AF0A1            JGE     HANDLE_DA_MATH
                            000268
895       P:000267 P:000269 2E0000            MOVE              #0,A
896                                 HANDLE_DA_MATH
897       P:000268 P:00026A 57F000            MOVE              X:QT_BUF_TAIL,B
                            000046
898       P:00026A P:00026C 20000D            CMP     A,B
899       P:00026B P:00026D 0AF0AA            JEQ     HANDLE_DA_DROP
                            00028C
900    
901       
902       P:00026D P:00026F 200013            CLR     A
903       P:00026E P:000270 50F000            MOVE              X:PACKET_SIZE,A0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 19



                            00002B
904    
905       P:000270 P:000272 014088            ADD     #0,B                              ; Clear carry
906       P:000271 P:000273 0C1D04            ASL     #2,A,A                            ; Size, in bytes
907    
908       
909       P:000272 P:000274 20001B            CLR     B
910       P:000273 P:000275 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            000043
911       P:000275 P:000277 20000D            CMP     A,B
912       P:000276 P:000278 0E21FE            JNE     QT_FSIZE_ERROR
913    
914       
915       P:000277 P:000279 517000            MOVE              B0,X:BLOCK_SIZE
                            00002F
916       P:000279 P:00027B 557000            MOVE              B1,X:YMEM_SRC           ; Y:0
                            000035
917    
918       P:00027B P:00027D 60F400            MOVE              #QT_DEST_LO,R0
                            000047
919       P:00027D P:00027F 0BF080            JSR     LOAD_HILO_ADDRESS
                            000753
920       P:00027F P:000281 60F400            MOVE              #BURST_DEST_LO,R0
                            000031
921       P:000281 P:000283 0BF080            JSR     SAVE_HILO_ADDRESS
                            00075B
922    
923       
924       P:000283 P:000285 0BF080            JSR     BLOCK_TRANSFER
                            00061C
925    
926       P:000285 P:000287 0BF080            JSR     TIMER_STORE
                            000705
927    
928       
929       P:000287 P:000289 0BF080            JSR     BUFFER_INCR
                            00070F
930    
931       
932       P:000289 P:00028B 0BF080            JSR     BUFFER_INFORM_CHECK
                            00072D
933    
934       P:00028B P:00028D 00000C            RTS
935    
936                                 HANDLE_DA_DROP
937       
938       P:00028C P:00028E 60F400            MOVE              #QT_DROPS,R0
                            00004A
939       P:00028E P:000290 0D0203            JSR     INCR_X_R0
940       P:00028F P:000291 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            0006D0
941    
943    
944    
945    
947    
948                                 ;----------------------------------------------
949                                 CON_TRANSFER
950                                 ;----------------------------------------------
951                                 ;   In:
952                                 ;   - BURST_SRC_HI:BURST_SRC_LO is PC RAM address
953                                 ;   - BLOCK_SIZE is packet size, in bytes
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 20



954                                 ;   - YMEM_DEST is start of data in Y memory
955                                 ;  Out:
956                                 ;   - BLOCK_SIZE will be decremented to zero.
957                                 ;   - BURST_SRC_HI:LO will be incremented by BLOCK_SIZE
958                                 ;   - YMEM_DEST will be incremented by BLOCK_SIZE/2
959                                 ;  Trashes:
960                                 ;   - A and B
961    
962       
963       P:000291 P:000293 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002F
964    
965       P:000293 P:000295 014085            CMP     #0,A
966       P:000294 P:000296 0AF0AA            JEQ     XBLOCK_DONE
                            0002DA
967    
968       
969       P:000296 P:000298 20001B            CLR     B
970       P:000297 P:000299 55F000            MOVE              X:PCI_BURST_SIZE,B1
                            000051
971    
972       P:000299 P:00029B 200005            CMP     B,A                               ; A ? B
973       P:00029A P:00029C 0E129C            JGE     <XBLOCK_TRANSFER1                 ; jump if A >= B
974       P:00029B P:00029D 21CF00            MOVE              A,B                     ; This only moves A1,B1.
975                                 XBLOCK_TRANSFER1
976       P:00029C P:00029E 200014            SUB     B,A                               ; A -= B
977       P:00029D P:00029F 014088            ADD     #0,B                              ; Clear carry bit
978       P:00029E P:0002A0 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002F
979       P:0002A0 P:0002A2 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            000030
980       P:0002A2 P:0002A4 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
981    
982       
983       P:0002A3 P:0002A5 50F000            MOVE              X:YMEM_DEST,A0
                            000036
984       P:0002A5 P:0002A7 507000            MOVE              A0,X:DDR0               ; DMA dest'n
                            FFFFEE
985       P:0002A7 P:0002A9 08F4AF            MOVEP             #>DRXR,X:DSR0           ; DMA source
                            FFFFCB
986       P:0002A9 P:0002AB 200010            ADD     B,A
987       P:0002AA P:0002AC 00000B            DEC     B
988       P:0002AB P:0002AD 507000            MOVE              A0,X:YMEM_DEST          ; YMEM_DEST += BURST_SIZE/2
                            000036
989    
990       P:0002AD P:0002AF 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
991    
992       
993       P:0002AE P:0002B0 08F4AC            MOVEP             #$8EEAC4,X:DCR0
                            8EEAC4
994    
995                                 XBLOCK_PCI
996       
997       P:0002B0 P:0002B2 200013            CLR     A
998       P:0002B1 P:0002B3 20001B            CLR     B
999       P:0002B2 P:0002B4 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            000030
1000      P:0002B4 P:0002B6 00000B            DEC     B                                 ; n8 - 1
1001      P:0002B5 P:0002B7 014088            ADD     #0,B                              ; Clear carry
1002      P:0002B6 P:0002B8 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
1003      P:0002B7 P:0002B9 014088            ADD     #0,B                              ; Clear carry
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 21



1004      P:0002B8 P:0002BA 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
1005   
1006      P:0002B9 P:0002BB 50F000            MOVE              X:BURST_SRC_HI,A0
                            000034
1007   
1008      P:0002BB P:0002BD 200010            ADD     B,A
1009      P:0002BC P:0002BE 000000            NOP
1010      P:0002BD P:0002BF 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
1011   
1012      P:0002BF P:0002C1 280600            MOVE              #$06,A0                 ; This is a read.
1013      P:0002C0 P:0002C2 014088            ADD     #0,B                              ; Clear carry
1014      P:0002C1 P:0002C3 0C1D20            ASL     #16,A,A
1015      P:0002C2 P:0002C4 51F000            MOVE              X:BURST_SRC_LO,B0
                            000033
1016      P:0002C4 P:0002C6 200010            ADD     B,A
1017      P:0002C5 P:0002C7 000000            NOP
1018      P:0002C6 P:0002C8 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
1019   
1020   
1021   
1022                                XBLOCK_CHECK
1023      P:0002C7 P:0002C9 000000            NOP
1024      P:0002C8 P:0002CA 000000            NOP
1025      P:0002C9 P:0002CB 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for burst termination
                            0002C9
1026   
1027      
1028      P:0002CB P:0002CD 0A8AAE            JSET    #MDT,X:DPSR,XBLOCK_OK
                            0002D5
1029   
1030      P:0002CD P:0002CF 0BF080            JSR     PCI_ERROR_CLEAR
                            0005C2
1031   
1032      P:0002CF P:0002D1 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1033      P:0002D1 P:0002D3 0E82DB            JCS     <XBLOCK_RESTART
1034   
1035      P:0002D2 P:0002D4 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
1036      P:0002D4 P:0002D6 0E82DC            JCS     <XBLOCK_RESUME
1037   
1038                                XBLOCK_OK
1039      P:0002D5 P:0002D7 50F000            MOVE              X:BURST_SIZE,A0         ; Pass # of words written to updater
                            000030
1040      P:0002D7 P:0002D9 0BF080            JSR     XBLOCK_UPDATE
                            0002EE
1041      P:0002D9 P:0002DB 0C0291            JMP     CON_TRANSFER                      ; Finish the block
1042                                XBLOCK_DONE
1043      P:0002DA P:0002DC 00000C            RTS                                       ; Done
1044   
1045                                XBLOCK_RESTART
1046      P:0002DB P:0002DD 0C02B0            JMP     XBLOCK_PCI                        ; Recalculate pci and resend
1047   
1048                                XBLOCK_RESUME
1049      P:0002DC P:0002DE 200013            CLR     A
1050      P:0002DD P:0002DF 20001B            CLR     B
1051      P:0002DE P:0002E0 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
1052      P:0002DF P:0002E1 0A8A8F            JCLR    #RDCQ,X:DPSR,XBLOCK_RESUME1
                            0002E2
1053   
1054      P:0002E1 P:0002E3 000009            INC     B
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 22



1055   
1056                                XBLOCK_RESUME1
1057   
1058      P:0002E2 P:0002E4 000009            INC     B                                 ; We want N, not N-1.
1059      P:0002E3 P:0002E5 014088            ADD     #0,B                              ; Clear carry
1060      P:0002E4 P:0002E6 0C1C20            ASR     #16,A,A
1061      P:0002E5 P:0002E7 200018            ADD     A,B                               ; B is words remaining
1062      P:0002E6 P:0002E8 014088            ADD     #0,B                              ; Clear carry
1063      P:0002E7 P:0002E9 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
1064      P:0002E8 P:0002EA 50F000            MOVE              X:BURST_SIZE,A0
                            000030
1065      P:0002EA P:0002EC 200014            SUB     B,A                               ; A is words written
1066   
1067      P:0002EB P:0002ED 0BF080            JSR     XBLOCK_UPDATE
                            0002EE
1068      P:0002ED P:0002EF 0C02B0            JMP     XBLOCK_PCI                        ; Recalculate pci and resend
1069   
1070                                ; BLOCK_UPDATE
1071                                ;  Subtract A from BURST_SIZE and add A to BURST_DEST_LO
1072                                ;  Caller can check Z flag to see if BURST_SIZE is now 0.
1073                                XBLOCK_UPDATE
1074      P:0002EE P:0002F0 210500            MOVE              A0,X1                   ; Save A for later
1075      P:0002EF P:0002F1 0C1D01            ASL     #0,A,B                            ; MOVE A,B
1076   
1077      P:0002F0 P:0002F2 60F400            MOVE              #BURST_SRC_LO,R0        ;
                            000033
1078      P:0002F2 P:0002F4 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates BURST_DEST to BURST_DEST + B
                            000759
1079   
1080      P:0002F4 P:0002F6 57F000            MOVE              X:BURST_SIZE,B
                            000030
1081      P:0002F6 P:0002F8 20006C            SUB     X1,B                              ; Zero flag must be preserved!
1082      P:0002F7 P:0002F9 000000            NOP
1083      P:0002F8 P:0002FA 557000            MOVE              B1,X:BURST_SIZE
                            000030
1084   
1085      P:0002FA P:0002FC 00000C            RTS
1086   
1087   
1088   
1089   
1090                                CON_NOW
1091                                ;       This routine runs after the PC sends a 'CON' command, and will
1092                                ;       copy the command to the MCE and then reply to the PC.
1093   
1094      P:0002FB P:0002FD 54F400            MOVE              #>$C00,A1
                            000C00
1095      P:0002FD P:0002FF 0BF080            JSR     TIMER_STORE_A1
                            000707
1096      P:0002FF P:000301 0BF080            JSR     TIMER_STORE
                            000705
1097   
1098      
1099      P:000301 P:000303 60F400            MOVE              #>CON_SRC_LO,R0
                            00004F
1100      P:000303 P:000305 0BF080            JSR     LOAD_HILO_ADDRESS
                            000753
1101      P:000305 P:000307 60F400            MOVE              #>BURST_SRC_LO,R0
                            000033
1102      P:000307 P:000309 0BF080            JSR     SAVE_HILO_ADDRESS
                            00075B
1103      P:000309 P:00030B 51F400            MOVE              #>COMMAND_BUFFER,B0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 23



                            200000
1104      P:00030B P:00030D 50F400            MOVE              #>256,A0
                            000100
1105      P:00030D P:00030F 517000            MOVE              B0,X:YMEM_DEST
                            000036
1106      P:00030F P:000311 507000            MOVE              A0,X:BLOCK_SIZE
                            00002F
1107      P:000311 P:000313 0D0291            JSR     CON_TRANSFER
1108   
1109      P:000312 P:000314 0A702B            BSET    #CON_MCE,X:STATUS
                            000000
1110      P:000314 P:000316 0BF080            JSR     TIMER_STORE
                            000705
1111      P:000316 P:000318 00000C            RTS
1112   
1113   
1114                                CON_NOW_TRANSMIT
1115      
1116      P:000317 P:000319 0BF080            JSR     TIMER_STORE
                            000705
1117   
1118      P:000319 P:00031B 66F400            MOVE              #>COMMAND_BUFFER,R6
                            200000
1119      P:00031B P:00031D 068080            DO      #128,CON_NOW_CLEANUP              ; block size = 16bit x 128 (256 bytes)
                            000324
1120      P:00031D P:00031F 5CDE00            MOVE                          Y:(R6)+,A1  ; b2, b1  (lsb)
1121      P:00031E P:000320 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
1122      P:00031F P:000321 0140C6            AND     #>$FF,A
                            0000FF
1123      P:000321 P:000323 547000            MOVE              A1,X:FO_SEND
                            FFF000
1124      P:000323 P:000325 557000            MOVE              B1,X:FO_SEND
                            FFF000
1125   
1126                                CON_NOW_CLEANUP
1127      P:000325 P:000327 0A0121            BSET    #MODE_MCE,X:<MODE                 ; enable response handling
1128                                                                                    ; comms now open with MCE and packets will b
e processed.
1129   
1130      
1131      P:000326 P:000328 0A700B            BCLR    #CON_MCE,X:STATUS
                            000000
1132      P:000328 P:00032A 0A700A            BCLR    #CON_DEMAND,X:STATUS
                            000000
1133   
1134      P:00032A P:00032C 0BF080            JSR     TIMER_STORE
                            000705
1135   
1136      
1137      P:00032C P:00032E 0BF080            JSR     PCI_LOCKDOWN
                            00053A
1138      P:00032E P:000330 44F400            MOVE              #'CON',X0
                            434F4E
1139      P:000330 P:000332 0BF080            JSR     VCOM_PREPARE_REPLY
                            00037B
1140      P:000332 P:000334 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000540
1141      P:000334 P:000336 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
1142   
1143      P:000335 P:000337 0BF080            JSR     TIMER_STORE
                            000705
1144      P:000337 P:000339 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 24



1145   
1146   
1147   
1148   
1150   
1151                                ; --------------------------------------------------------------------------
1152                                ; --------------------- MAIN PACKET HANDLING CODE --------------------------
1153                                ; --------------------------------------------------------------------------
1154   
1155                                ; prepare notify to inform host that a packet has arrived.
1156   
1157                                MCE_PACKET
1158      P:000338 P:00033A 0BF080            JSR     PCI_LOCKDOWN                      ; Disable host IRQ
                            00053A
1159      P:00033A P:00033C 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
1160   
1161      P:00033B P:00033D 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
1162      P:00033D P:00033F 440B00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
1163   
1164      P:00033E P:000340 449300            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
1165      P:00033F P:000341 440C00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
1166   
1167      P:000340 P:000342 449500            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
1168      P:000341 P:000343 440D00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
1169   
1170      P:000342 P:000344 449600            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
1171      P:000343 P:000345 440E00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sage
1172   
1173      
1174      P:000344 P:000346 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
1175      P:000345 P:000347 0D0540            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
1176      P:000346 P:000348 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
1177      P:000347 P:000349 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
1178   
1179      P:000348 P:00034A 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
1180      P:00034A P:00034C 0BF080            JSR     BUFFER_PACKET
                            000691
1181   
1182      
1183   
1184      P:00034C P:00034E 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; on fatal error, re-init.
                            000100
1185      P:00034E P:000350 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; Set in 'send_packet_to_host' ISR
                            00034C
1186   
1187      
1188      P:000350 P:000352 44F400            MOVE              #>IMAGE_BUFFER,X0
                            000000
1189      P:000352 P:000354 56F000            MOVE              X:PACKET_SIZE,A
                            00002B
1190      P:000354 P:000356 0C1D04            ASL     #2,A,A
1191      P:000355 P:000357 447000            MOVE              X0,X:YMEM_SRC
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 25



                            000035
1192      P:000357 P:000359 547000            MOVE              A1,X:BLOCK_SIZE
                            00002F
1193      P:000359 P:00035B 0BF080            JSR     BLOCK_TRANSFER
                            00061C
1194   
1195      P:00035B P:00035D 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
1196   
1197      
1198      P:00035D P:00035F 0BF080            JSR     PCI_LOCKDOWN                      ; Disable host IRQ
                            00053A
1199      P:00035F P:000361 44F400            MOVE              #'HST',X0
                            485354
1200      P:000361 P:000363 0BF080            JSR     VCOM_PREPARE_REPLY
                            00037B
1201      P:000363 P:000365 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000540
1202      P:000365 P:000367 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
1203      P:000366 P:000368 00000C            RTS
1204   
1205                                ;----------------------------------------------------------
1206                                ; clear out the fifo after an HST timeout...
1207                                ;----------------------------------------------------------
1208   
1209                                DUMP_FIFO
1210      P:000367 P:000369 61F400            MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
1211      P:000369 P:00036B 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
1212      P:00036B P:00036D 200013            CLR     A
1213      P:00036C P:00036E 320000            MOVE              #0,R2                   ; use R2 as a dump count
1214                                NEXT_DUMP
1215      P:00036D P:00036F 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000378
1216      P:00036F P:000371 000000            NOP
1217      P:000370 P:000372 000000            NOP
1218      P:000371 P:000373 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000378
1219   
1220      P:000373 P:000375 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
1221      P:000374 P:000376 205A00            MOVE              (R2)+                   ; inc dump count
1222      P:000375 P:000377 224E00            MOVE              R2,A                    ;
1223      P:000376 P:000378 200045            CMP     X0,A                              ; check we've not hit dump limit
1224      P:000377 P:000379 0E236D            JNE     NEXT_DUMP                         ; not hit limit?
1225                                FIFO_EMPTY
1226      P:000378 P:00037A 627000            MOVE              R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000006
1227      P:00037A P:00037C 0C0100            JMP     <START                            ; re-initialise
1228   
1229   
1230                                ; -------------------------------------------------------------------------------------
1231                                ;                              END OF MAIN PACKET HANDLING CODE
1232                                ; -------------------------------------------------------------------------------------
1233   
1234   
1235   
1236                                ; -------------------------------------------------------------------------------------
1237                                ;
1238                                ;                              INTERRUPT SERVICE ROUTINES
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 26



1239                                ;
1240                                ; -------------------------------------------------------------------------------------
1241   
1242                                ; ---------------
1243                                ; Rules:  Don't use N#, or any R# except R0 unless you add them to the saved register set.
1244   
1245   
1246                                ; ----------------------------------------------------------------------------
1247                                ; VCOM_* - routines: utility functions for hosty command vector communication.
1248                                ;-----------------------------------------------------------------------------
1249   
1250   
1251                                ; VCOM_PREPARE_REPLY
1252                                ;
1253                                ; Prepare the reply packet, using X0 as the command name (second word).  The
1254                                ; message defaults to 'ACK' with NULL data.  The user may subsequenty fill in
1255                                ; the data field (word 4) and mark the packet as error if necessary.
1256   
1257                                VCOM_PREPARE_REPLY
1258      
1259      
1260      P:00037B P:00037D 50F400            MOVE              #'REP',A0
                            524550
1261      P:00037D P:00037F 447000            MOVE              X0,X:DTXS_WD2           ; Command
                            00000C
1262      P:00037F P:000381 507000            MOVE              A0,X:DTXS_WD1
                            00000B
1263   
1264      P:000381 P:000383 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
                            41434B
1265      P:000383 P:000385 000000            NOP
1266      P:000384 P:000386 547000            MOVE              A1,X:DTXS_WD3           ; ACK
                            00000D
1267      P:000386 P:000388 507000            MOVE              A0,X:DTXS_WD4           ; no comment
                            00000E
1268      P:000388 P:00038A 00000C            RTS
1269   
1270   
1271                                ; VCOM_CHECK
1272                                ;
1273                                ; Compares DRXR_WD1 to X0.  If they are equal, Z is set on return.  If they
1274                                ; are not equal then Z is cleared and the reply will be marked as ERR with
1275                                ; 'CNE' in the last word.
1276                                ; Trashes A and B always and X0 on error.
1277   
1278                                VCOM_CHECK
1279      P:000389 P:00038B 208E00            MOVE              X0,A
1280      P:00038A P:00038C 57F000            MOVE              X:DRXR_WD1,B
                            000007
1281      P:00038C P:00038E 20000D            CMP     A,B
1282      P:00038D P:00038F 0AF0AA            JEQ     VCOM_RTS
                            000397
1283   
1284      P:00038F P:000391 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
1285      P:000391 P:000393 50F400            MOVE              #'ERR',A0
                            455252
1286      P:000393 P:000395 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1287      P:000395 P:000397 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1288                                VCOM_RTS
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 27



1289      P:000397 P:000399 00000C            RTS
1290   
1291   
1292                                ; VCOM_INTRO
1293                                ;
1294                                ; Read DSP command from DRXR.  Prepare the reply packet and verify that it
1295                                ; matches the key in X1.  If it does not, mark the reply as error and set
1296                                ; the Z flag.
1297   
1298                                VCOM_INTRO
1299      P:000398 P:00039A 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            000558
1300      P:00039A P:00039C 20A400            MOVE              X1,X0
1301      P:00039B P:00039D 0D037B            JSR     VCOM_PREPARE_REPLY
1302      P:00039C P:00039E 0D0389            JSR     VCOM_CHECK
1303      P:00039D P:00039F 00000C            RTS
1304   
1305   
1306                                ; VCOM_EXIT_ERROR_X0
1307                                ; VCOM_EXIT_X0
1308                                ; VCOM_EXIT
1309                                ;
1310                                ; For returning from host command vector interrupts only.  These three
1311                                ; routines do the following (respectively):
1312                                ; a) Mark reply as error, then (b)
1313                                ; b) Put X0 into last word of reply, then (c)
1314                                ; c) Restore registers and RTI.
1315   
1316                                VCOM_EXIT_ERROR_X0
1317      P:00039E P:0003A0 50F400            MOVE              #'ERR',A0
                            455252
1318      P:0003A0 P:0003A2 000000            NOP
1319      P:0003A1 P:0003A3 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1320                                VCOM_EXIT_X0
1321      P:0003A3 P:0003A5 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1322                                VCOM_EXIT
1323      P:0003A5 P:0003A7 0BF080            JSR     RESTORE_REGISTERS
                            00057F
1324      P:0003A7 P:0003A9 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000540
1325      P:0003A9 P:0003AB 000004            RTI
1326   
1327   
1328   
1329   
1330                                ; ----------------------------------------------------------------------------
1331                                READ_MEMORY
1332                                ;-----------------------------------------------------------------------------
1333                                ;Read command:
1334                                ; word 1 = command = 'RDM'
1335                                ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
1336                                ; word 3 = address in memory
1337                                ; word 4 = not used
1338                                ;Version query:
1339                                ; word 1 = 'VER'
1340                                ; word 2-4 unused
1341   
1342      P:0003AA P:0003AC 0BF080            JSR     SAVE_REGISTERS
                            00058C
1343      P:0003AC P:0003AE 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 28



                            000558
1344   
1345      P:0003AE P:0003B0 44F400            MOVE              #'RDM',X0
                            52444D
1346      P:0003B0 P:0003B2 0D037B            JSR     VCOM_PREPARE_REPLY
1347      P:0003B1 P:0003B3 0D0389            JSR     VCOM_CHECK
1348      P:0003B2 P:0003B4 0AF0AA            JEQ     READ_MEMORY_XYP
                            0003BC
1349   
1350      
1351      P:0003B4 P:0003B6 44F400            MOVE              #'VER',X0
                            564552
1352      P:0003B6 P:0003B8 0D037B            JSR     VCOM_PREPARE_REPLY
1353      P:0003B7 P:0003B9 0D0389            JSR     VCOM_CHECK
1354      P:0003B8 P:0003BA 0E23A5            JNE     VCOM_EXIT
1355   
1356      P:0003B9 P:0003BB 44F000            MOVE              X:REV_NUMBER,X0
                            000003
1357      P:0003BB P:0003BD 0C03A3            JMP     VCOM_EXIT_X0
1358   
1359                                READ_MEMORY_XYP
1360   
1361      
1362      P:0003BC P:0003BE 56F000            MOVE              X:DRXR_WD2,A
                            000008
1363      P:0003BE P:0003C0 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1364   
1365      P:0003C0 P:0003C2 0140C5            CMP     #'_X',A
                            005F58
1366      P:0003C2 P:0003C4 0AF0AA            JEQ     READ_MEMORY_X
                            0003CF
1367   
1368      P:0003C4 P:0003C6 0140C5            CMP     #'_Y',A
                            005F59
1369      P:0003C6 P:0003C8 0AF0AA            JEQ     READ_MEMORY_Y
                            0003D1
1370   
1371      P:0003C8 P:0003CA 0140C5            CMP     #'_P',A
                            005F50
1372      P:0003CA P:0003CC 0AF0AA            JEQ     READ_MEMORY_P
                            0003D3
1373   
1374      P:0003CC P:0003CE 44F400            MOVE              #'MTE',X0
                            4D5445
1375      P:0003CE P:0003D0 0C039E            JMP     VCOM_EXIT_ERROR_X0
1376   
1377                                READ_MEMORY_X
1378      P:0003CF P:0003D1 44E000            MOVE              X:(R0),X0
1379      P:0003D0 P:0003D2 0C03A3            JMP     VCOM_EXIT_X0
1380                                READ_MEMORY_Y
1381      P:0003D1 P:0003D3 4CE000            MOVE                          Y:(R0),X0
1382      P:0003D2 P:0003D4 0C03A3            JMP     VCOM_EXIT_X0
1383                                READ_MEMORY_P
1384      P:0003D3 P:0003D5 07E084            MOVE              P:(R0),X0
1385      P:0003D4 P:0003D6 0C03A3            JMP     VCOM_EXIT_X0
1386   
1387   
1388                                ;--------------------------------------------------------------
1389                                WRITE_MEMORY
1390                                ;---------------------------------------------------------------
1391                                ; word 1 = command = 'WRM'
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 29



1392                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1393                                ; word 3 = address in memory
1394                                ; word 4 = value
1395   
1396      P:0003D5 P:0003D7 0BF080            JSR     SAVE_REGISTERS
                            00058C
1397      P:0003D7 P:0003D9 45F400            MOVE              #'WRM',X1
                            57524D
1398      P:0003D9 P:0003DB 0D0398            JSR     VCOM_INTRO
1399      P:0003DA P:0003DC 0E23A5            JNE     VCOM_EXIT
1400   
1401      
1402      P:0003DB P:0003DD 56F000            MOVE              X:DRXR_WD2,A
                            000008
1403      P:0003DD P:0003DF 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1404      P:0003DF P:0003E1 44F000            MOVE              X:DRXR_WD4,X0
                            00000A
1405   
1406      P:0003E1 P:0003E3 0140C5            CMP     #'_X',A
                            005F58
1407      P:0003E3 P:0003E5 0AF0AA            JEQ     WRITE_MEMORY_X
                            0003F0
1408   
1409      P:0003E5 P:0003E7 0140C5            CMP     #'_Y',A
                            005F59
1410      P:0003E7 P:0003E9 0AF0AA            JEQ     WRITE_MEMORY_Y
                            0003F2
1411   
1412      P:0003E9 P:0003EB 0140C5            CMP     #'_P',A
                            005F50
1413      P:0003EB P:0003ED 0AF0AA            JEQ     WRITE_MEMORY_P
                            0003F4
1414   
1415      P:0003ED P:0003EF 44F400            MOVE              #'MTE',X0
                            4D5445
1416      P:0003EF P:0003F1 0C039E            JMP     VCOM_EXIT_ERROR_X0
1417   
1418                                WRITE_MEMORY_X
1419      P:0003F0 P:0003F2 446000            MOVE              X0,X:(R0)
1420      P:0003F1 P:0003F3 0C03A3            JMP     VCOM_EXIT_X0
1421                                WRITE_MEMORY_Y
1422      P:0003F2 P:0003F4 4C6000            MOVE                          X0,Y:(R0)
1423      P:0003F3 P:0003F5 0C03A3            JMP     VCOM_EXIT_X0
1424                                WRITE_MEMORY_P
1425      P:0003F4 P:0003F6 076084            MOVE              X0,P:(R0)
1426      P:0003F5 P:0003F7 0C03A3            JMP     VCOM_EXIT_X0
1427   
1428   
1429                                ;-----------------------------------------------------------------------------
1430                                START_APPLICATION
1431                                ; an application should already have been downloaded to the PCI memory.
1432                                ; this command will execute it.
1433                                ; ----------------------------------------------------------------------
1434                                ; word 1 = command = 'GOA'
1435                                ; word 2-4 unused
1436   
1437      P:0003F6 P:0003F8 0BF080            JSR     SAVE_REGISTERS
                            00058C
1438      P:0003F8 P:0003FA 45F400            MOVE              #'GOA',X1
                            474F41
1439   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 30



1440      P:0003FA P:0003FC 0D0398            JSR     VCOM_INTRO
1441      P:0003FB P:0003FD 0E23A5            JNE     VCOM_EXIT
1442   
1443      P:0003FC P:0003FE 0A7020            BSET    #MODE_APPLICATION,X:MODE
                            000001
1444      P:0003FE P:000400 000004            RTI                                       ; Application will reply.
1445   
1446   
1447                                ; ---------------------------------------------------------
1448                                STOP_APPLICATION
1449                                ; this command stops an application that is currently running
1450                                ; used for applications that once started run contiunually
1451                                ;-----------------------------------------------------------
1452                                ; word 1 = command = ' STP'
1453                                ; word 2-4 unused
1454   
1455      P:0003FF P:000401 0BF080            JSR     SAVE_REGISTERS
                            00058C
1456      P:000401 P:000403 45F400            MOVE              #'STP',X1
                            535450
1457   
1458      P:000403 P:000405 0D0398            JSR     VCOM_INTRO
1459      P:000404 P:000406 0E23A5            JNE     VCOM_EXIT
1460   
1461      P:000405 P:000407 0A7000            BCLR    #MODE_APPLICATION,X:MODE
                            000001
1462      P:000407 P:000409 0A7000            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1463      P:000409 P:00040B 0C03A5            JMP     VCOM_EXIT
1464   
1465   
1466                                ;-----------------------------------------------------------------------------
1467                                RESET_CONTROLLER
1468                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1469                                ;-----------------------------------------------------------------------------
1470                                ; word 1 = command = 'RCO'
1471                                ; word 2-4 unused
1472   
1473      P:00040A P:00040C 0BF080            JSR     SAVE_REGISTERS
                            00058C
1474      P:00040C P:00040E 45F400            MOVE              #'RCO',X1
                            52434F
1475      P:00040E P:000410 0D0398            JSR     VCOM_INTRO
1476      P:00040F P:000411 0E23A5            JNE     VCOM_EXIT
1477   
1478      P:000410 P:000412 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1479      P:000411 P:000413 000000            NOP
1480      P:000412 P:000414 000000            NOP
1481      P:000413 P:000415 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1482      P:000415 P:000417 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1483      P:000417 P:000419 446000            MOVE              X0,X:(R0)
1484      P:000418 P:00041A 0606A0            REP     #6                                ; Wait for transmission to complete
1485      P:000419 P:00041B 000000            NOP
1486      P:00041A P:00041C 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1487   
1488                                ; Wait for a bit for MCE to be reset.......
1489      P:00041B P:00041D 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1490      P:00041D P:00041F 06C400            DO      X0,L_DELAY
                            000423
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 31



1491      P:00041F P:000421 06E883            DO      #1000,L_RDFIFO
                            000422
1492      P:000421 P:000423 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1493      P:000422 P:000424 000000            NOP                                       ;   receiver empty
1494                                L_RDFIFO
1495      P:000423 P:000425 000000            NOP
1496                                L_DELAY
1497      P:000424 P:000426 000000            NOP
1498   
1499      P:000425 P:000427 44F400            MOVE              #'000',X0
                            303030
1500      P:000427 P:000429 0C03A3            JMP     VCOM_EXIT_X0
1501   
1502                                ;-----------------------------------------------------------------------------
1503                                QUIET_TRANSFER_SET
1504                                ;-----------------------------------------------------------------------------
1505                                ;Quiet transfer mode configuration
1506                                ; word 1 = command = 'QTS'
1507                                ; word 2 = parameter to set
1508                                ; word 3-4 = arguments
1509   
1510      P:000428 P:00042A 0BF080            JSR     SAVE_REGISTERS                    ; standard opening
                            00058C
1511      P:00042A P:00042C 45F400            MOVE              #'QTS',X1
                            515453
1512      P:00042C P:00042E 0D0398            JSR     VCOM_INTRO
1513      P:00042D P:00042F 0E23A5            JNE     VCOM_EXIT
1514   
1515      P:00042E P:000430 60F400            MOVE              #BDEBUG0,R0
                            000053
1516      P:000430 P:000432 0D0203            JSR     INCR_X_R0
1517   
1518      P:000431 P:000433 56F000            MOVE              X:DRXR_WD2,A            ; Parameter id
                            000008
1519      P:000433 P:000435 44F000            MOVE              X:DRXR_WD3,X0           ; First arg
                            000009
1520      P:000435 P:000437 45F000            MOVE              X:DRXR_WD4,X1           ; Second arg
                            00000A
1521   
1522      P:000437 P:000439 0140C5            CMP     #'BAS',A
                            424153
1523      P:000439 P:00043B 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            0004B4
1524   
1525      P:00043B P:00043D 0140C5            CMP     #'DEL',A
                            44454C
1526      P:00043D P:00043F 60F400            MOVE              #QT_BUF_SIZE,R0
                            000041
1527      P:00043F P:000441 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0004B1
1528   
1529      P:000441 P:000443 0140C5            CMP     #'NUM',A
                            4E554D
1530      P:000443 P:000445 60F400            MOVE              #QT_BUF_MAX,R0
                            000042
1531      P:000445 P:000447 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0004B1
1532   
1533      P:000447 P:000449 0140C5            CMP     #'INF',A
                            494E46
1534      P:000449 P:00044B 60F400            MOVE              #QT_INFORM,R0
                            000044
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 32



1535      P:00044B P:00044D 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0004B1
1536   
1537      P:00044D P:00044F 0140C5            CMP     #'SIZ',A
                            53495A
1538      P:00044F P:000451 60F400            MOVE              #QT_FRAME_SIZE,R0
                            000043
1539      P:000451 P:000453 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0004B1
1540   
1541      P:000453 P:000455 0140C5            CMP     #'TAI',A
                            544149
1542      P:000455 P:000457 60F400            MOVE              #QT_BUF_TAIL,R0
                            000046
1543      P:000457 P:000459 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0004B1
1544   
1545      P:000459 P:00045B 0140C5            CMP     #'HEA',A
                            484541
1546      P:00045B P:00045D 60F400            MOVE              #QT_BUF_HEAD,R0
                            000045
1547      P:00045D P:00045F 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0004B1
1548   
1549      P:00045F P:000461 0140C5            CMP     #'DRO',A
                            44524F
1550      P:000461 P:000463 60F400            MOVE              #QT_DROPS,R0
                            00004A
1551      P:000463 P:000465 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0004B1
1552   
1553      P:000465 P:000467 0140C5            CMP     #'PER',A
                            504552
1554      P:000467 P:000469 60F400            MOVE              #TCPR0,R0
                            FFFF8D
1555      P:000469 P:00046B 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0004B1
1556   
1557      P:00046B P:00046D 0140C5            CMP     #'FLU',A
                            464C55
1558      P:00046D P:00046F 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            000496
1559   
1560      P:00046F P:000471 0140C5            CMP     #'SET',A
                            534554
1561      P:000471 P:000473 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            0004A0
1562   
1563      P:000473 P:000475 0140C5            CMP     #'RPS',A
                            525053
1564      P:000475 P:000477 60F400            MOVE              #RP_MAX_SIZE,R0
                            00004D
1565      P:000477 P:000479 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0004B1
1566   
1567      P:000479 P:00047B 0140C5            CMP     #'RPB',A
                            525042
1568      P:00047B P:00047D 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_BASE
                            000484
1569   
1570      P:00047D P:00047F 0140C5            CMP     #'RPE',A
                            525045
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 33



1571      P:00047F P:000481 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_ENABLED
                            00048A
1572   
1573      P:000481 P:000483 44F400            MOVE              #'MTE',X0
                            4D5445
1574      P:000483 P:000485 0C039E            JMP     VCOM_EXIT_ERROR_X0
1575   
1576                                QUIET_TRANSFER_SET_RP_BASE
1577      P:000484 P:000486 447000            MOVE              X0,X:RP_BASE_LO
                            00004B
1578      P:000486 P:000488 457000            MOVE              X1,X:RP_BASE_HI
                            00004C
1579      P:000488 P:00048A 0AF080            JMP     VCOM_EXITX
                            0004BC
1580   
1581                                QUIET_TRANSFER_SET_RP_ENABLED
1582      P:00048A P:00048C 0A7003            BCLR    #MODE_RP_BUFFER,X:MODE
                            000001
1583      P:00048C P:00048E 208E00            MOVE              X0,A
1584      P:00048D P:00048F 200003            TST     A
1585      P:00048E P:000490 0AF0AA            JEQ     VCOM_EXITX
                            0004BC
1586      P:000490 P:000492 0A7023            BSET    #MODE_RP_BUFFER,X:MODE
                            000001
1587      P:000492 P:000494 0A7015            BCLR    #RP_BUFFER_FULL,X:STATUS
                            000000
1588      P:000494 P:000496 0AF080            JMP     VCOM_EXITX
                            0004BC
1589   
1590                                QUIET_TRANSFER_SET_FLUSH
1591      P:000496 P:000498 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
1592      P:000498 P:00049A 208E00            MOVE              X0,A
1593      P:000499 P:00049B 200003            TST     A
1594      P:00049A P:00049C 0AF0AA            JEQ     VCOM_EXITX
                            0004BC
1595      P:00049C P:00049E 0A7034            BSET    #QT_FLUSH,X:STATUS
                            000000
1596      P:00049E P:0004A0 0AF080            JMP     VCOM_EXITX
                            0004BC
1597   
1598                                QUIET_TRANSFER_SET_ENABLED
1599      P:0004A0 P:0004A2 0A7002            BCLR    #MODE_QT,X:MODE
                            000001
1600      P:0004A2 P:0004A4 0BF080            JSR     TIMER_DISABLE
                            0006EA
1601      P:0004A4 P:0004A6 208E00            MOVE              X0,A
1602      P:0004A5 P:0004A7 200003            TST     A
1603      P:0004A6 P:0004A8 0AF0AA            JEQ     VCOM_EXITX
                            0004BC
1604      P:0004A8 P:0004AA 280000            MOVE              #0,A0
1605      P:0004A9 P:0004AB 0A7022            BSET    #MODE_QT,X:MODE
                            000001
1606      P:0004AB P:0004AD 507000            MOVE              A0,X:TLR0
                            FFFF8E
1607      P:0004AD P:0004AF 0BF080            JSR     TIMER_ENABLE
                            0006E4
1608      P:0004AF P:0004B1 0AF080            JMP     VCOM_EXITX
                            0004BC
1609   
1610                                QUIET_TRANSFER_SET_R0
1611      P:0004B1 P:0004B3 446000            MOVE              X0,X:(R0)
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 34



1612      P:0004B2 P:0004B4 0AF080            JMP     VCOM_EXITX
                            0004BC
1613   
1614                                QUIET_TRANSFER_SET_BASE
1615      P:0004B4 P:0004B6 447000            MOVE              X0,X:QT_BASE_LO
                            00003F
1616      P:0004B6 P:0004B8 457000            MOVE              X1,X:QT_BASE_HI
                            000040
1617   
1618      P:0004B8 P:0004BA 0BF080            JSR     BUFFER_RESET
                            000721
1619   
1620      P:0004BA P:0004BC 0AF080            JMP     VCOM_EXITX
                            0004BC
1621   
1622                                VCOM_EXITX
1623      P:0004BC P:0004BE 44F000            MOVE              X:BDEBUG0,X0
                            000053
1624      P:0004BE P:0004C0 0C03A3            JMP     VCOM_EXIT_X0
1625   
1626   
1627                                ;-----------------------------------------------------------------------------
1628                                MODE_SET_FAST
1629                                ;-----------------------------------------------------------------------------
1630                                ; This is a 'fast' command in the sense that it does not reply to the host.
1631                                ; It is used to set various communication parameters prior to issuing normal
1632                                ; DSP commands.  A single word is read from the DRXR into DRXR_WD1 as data.
1633   
1634      P:0004BF P:0004C1 447000            MOVE              X0,X:SV_X0              ; Save X0
                            00001D
1635      P:0004C1 P:0004C3 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for data
                            0004C1
1636      P:0004C3 P:0004C5 44F000            MOVE              X:DRXR,X0
                            FFFFCB
1637      P:0004C5 P:0004C7 447000            MOVE              X0,X:MODE
                            000001
1638      P:0004C7 P:0004C9 44F000            MOVE              X:SV_X0,X0
                            00001D
1639      P:0004C9 P:0004CB 000004            RTI
1640   
1641   
1642                                ;-----------------------------------------------------------------------------
1643                                SYSTEM_RESET
1644                                ;-----------------------------------------------------------------------------
1645   
1646      P:0004CA P:0004CC 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1647      P:0004CB P:0004CD 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1648                                                                                    ; set to zero except for interrupts
1649      P:0004CD P:0004CF 05F43B            MOVEC             #>MY_SR,SP              ; Writing to SSH preincrements the SP
                            000100
1650                                                                                    ; so first set to 0
1651      P:0004CF P:0004D1 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1652                                                                                    ; therefore,return to initialization
1653      P:0004D1 P:0004D3 000000            NOP
1654      P:0004D2 P:0004D4 000004            RTI                                       ; return from ISR - to START
1655   
1656   
1657                                ;--------------------------------------------------------------------
1658                                CLEAN_UP_PCI
1659                                ;--------------------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 35



1660                                ; Clean up the PCI board from wherever it was executing
1661   
1662      P:0004D3 P:0004D5 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1663      P:0004D4 P:0004D6 05F43D            MOVEC             #$000200,SSL            ; SR = zero except for interrupts
                            000200
1664      P:0004D6 P:0004D8 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1665      P:0004D7 P:0004D9 05F43C            MOVEC             #START,SSH              ; Set PC to for full initialization
                            000100
1666      P:0004D9 P:0004DB 000000            NOP
1667      P:0004DA P:0004DC 000004            RTI
1668   
1669   
1670                                ; ------------------------------------------------------------------------------------
1671                                SEND_PACKET_TO_HOST
1672                                ; this command is received from the Host and actions the PCI board to pick up an address
1673                                ; pointer from DRXR which the PCI board then uses to write packets from the
1674                                ; MCE to the host memory starting at the address given.
1675                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1676                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1677                                ; HST after packet sent (unless error).
1678                                ; --------------------------------------------------------------------------------------
1679                                ; word 1 = command = 'HST'
1680                                ; word 2 = host high address
1681                                ; word 3 = host low address
1682                                ; word 4 = not used but read
1683   
1684                                ; save some registers but not B
1685   
1686      P:0004DB P:0004DD 0D058C            JSR     <SAVE_REGISTERS                   ; save working registers
1687      P:0004DC P:0004DE 45F400            MOVE              #'HST',X1
                            485354
1688      P:0004DE P:0004E0 0D0398            JSR     VCOM_INTRO
1689      P:0004DF P:0004E1 0E23A5            JNE     VCOM_EXIT
1690   
1691      
1692      P:0004E0 P:0004E2 448800            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
1693      P:0004E1 P:0004E3 518900            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1694   
1695      P:0004E2 P:0004E4 447000            MOVE              X0,X:BURST_DEST_HI
                            000032
1696      P:0004E4 P:0004E6 517000            MOVE              B0,X:BURST_DEST_LO
                            000031
1697   
1698      P:0004E6 P:0004E8 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1699   
1700      P:0004E7 P:0004E9 0BF080            JSR     RESTORE_REGISTERS
                            00057F
1701      P:0004E9 P:0004EB 000004            RTI                                       ; Main loop will reply after packet transfer
!
1702   
1703   
1704                                ; --------------------------------------------------------------------
1705                                SOFTWARE_RESET
1706                                ;----------------------------------------------------------------------
1707                                ; word 1 = command = 'RST'
1708                                ; word 2-4 unused
1709   
1710      P:0004EA P:0004EC 0BF080            JSR     SAVE_REGISTERS
                            00058C
1711      P:0004EC P:0004EE 45F400            MOVE              #'RST',X1
                            525354
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 36



1712      P:0004EE P:0004F0 0D0398            JSR     VCOM_INTRO
1713      P:0004EF P:0004F1 0E23A5            JNE     VCOM_EXIT
1714   
1715                                ; RST command OK so reply to host
1716                                FINISH_RST
1717      P:0004F0 P:0004F2 44F400            MOVE              #'000',X0
                            303030
1718      P:0004F2 P:0004F4 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1719      P:0004F4 P:0004F6 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000540
1720   
1721      P:0004F6 P:0004F8 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            0004F6
1722   
1723      P:0004F8 P:0004FA 0A0100            BCLR    #MODE_APPLICATION,X:<MODE         ; clear app flag
1724                                ;         BCLR  #PREAMBLE_ERROR,X:<STATUS       ; clear preamble error
1725      P:0004F9 P:0004FB 0A0000            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1726   
1727                                ; remember we are in a ISR so can't just jump to start.
1728   
1729      P:0004FA P:0004FC 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1730      P:0004FB P:0004FD 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1731                                                                                    ; set to zero except for interrupts
1732      P:0004FD P:0004FF 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1733                                                                                    ; so first set to 0
1734      P:0004FE P:000500 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1735                                                                                    ; therefore,return to initialization
1736      P:000500 P:000502 000000            NOP
1737      P:000501 P:000503 000004            RTI                                       ; return from ISR - to START
1738   
1739   
1740                                SEND_PACKET_TO_CONTROLLER
1741   
1742                                ;       Host command identifying location of an MCE command to send to
1743                                ;       the MCE.  Since this can come at any time, just record the
1744                                ;       request and then do the CONning from the main loop.
1745   
1746                                ; word 1 = command = 'CON'
1747                                ; word 2 = source host bus address, bits 31:16
1748                                ; word 3 = source host bus address, bits 15:0
1749                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1750                                ;        = '1' --> when MCE command is GO
1751   
1752      P:000502 P:000504 0D058C            JSR     <SAVE_REGISTERS                   ; save working registers
1753   
1754      
1755      P:000503 P:000505 45F400            MOVE              #'CON',X1
                            434F4E
1756      P:000505 P:000507 0D0398            JSR     VCOM_INTRO
1757      P:000506 P:000508 0E23A5            JNE     VCOM_EXIT
1758   
1759      
1760      P:000507 P:000509 44F400            MOVE              #'BUS',X0
                            425553
1761      P:000509 P:00050B 0A00AA            JSET    #CON_DEMAND,X:STATUS,VCOM_EXIT_ERROR_X0
                            00039E
1762   
1763      
1764      P:00050B P:00050D 0A702A            BSET    #CON_DEMAND,X:STATUS
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 37



                            000000
1765      P:00050D P:00050F 448800            MOVE              X:<DRXR_WD2,X0
1766      P:00050E P:000510 458900            MOVE              X:<DRXR_WD3,X1
1767      P:00050F P:000511 447000            MOVE              X0,X:CON_SRC_HI
                            000050
1768      P:000511 P:000513 457000            MOVE              X1,X:CON_SRC_LO
                            00004F
1769   
1770                                ;       ;; Fourth word indicates if this is a go.  Who cares?
1771                                ;       MOVE    X:<DRXR_WD4,A           ; read word 4 - GO command?
1772                                ;       MOVE    #0,X0
1773                                ;       CMP     X0,A
1774                                ;       JEQ     BLOCK_CON
1775   
1776      
1777      P:000513 P:000515 0BF080            JSR     RESTORE_REGISTERS
                            00057F
1778      P:000515 P:000517 000004            RTI
1779   
1781   
1782   
1783                                ;---------------------------------------------------------------
1784                                ;
1785                                ;                          * END OF ISRs *
1786                                ;
1787                                ;--------------------------------------------------------------
1788   
1789   
1790   
1791                                ;----------------------------------------------------------------
1792                                ;
1793                                ;                     * Beginning of SUBROUTINES *
1794                                ;
1795                                ;-----------------------------------------------------------------
1796   
1797   
1798                                CHECK_FO
1799      P:000516 P:000518 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            000534
1800      P:000518 P:00051A 000000            NOP
1801      P:000519 P:00051B 000000            NOP
1802      P:00051A P:00051C 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            000534
1803   
1804      P:00051C P:00051E 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1805      P:00051D P:00051F 00000C            RTS
1806   
1807   
1808                                ;---------------------------------------------------------------
1809                                GET_FO_WRD
1810                                ;--------------------------------------------------------------
1811                                ; Anything in fibre receive FIFO?   If so store in X0
1812   
1813      P:00051E P:000520 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            000534
1814      P:000520 P:000522 000000            NOP
1815      P:000521 P:000523 000000            NOP
1816      P:000522 P:000524 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS             ; check twice for FO metastability.
                            000534
1817      P:000524 P:000526 0AF080            JMP     RD_FO_WD
                            00052C
1818   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 38



1819                                WT_FIFO
1820      P:000526 P:000528 01AD80            JCLR    #EF,X:PDRD,*                      ; Wait till something in FIFO flagged
                            000526
1821      P:000528 P:00052A 000000            NOP
1822      P:000529 P:00052B 000000            NOP
1823      P:00052A P:00052C 01AD80            JCLR    #EF,X:PDRD,WT_FIFO                ; check twice.....
                            000526
1824   
1825                                ; Read one word from the fiber optics FIFO, check it and put it in A1
1826                                RD_FO_WD
1827      P:00052C P:00052E 09443F            MOVEP             Y:RDFIFO,X0             ; then read to X0
1828      P:00052D P:00052F 54F400            MOVE              #$00FFFF,A1             ; mask off top 2 bytes ($FC)
                            00FFFF
1829      P:00052F P:000531 200046            AND     X0,A                              ; since receiving 16 bits in 24bit register
1830      P:000530 P:000532 000000            NOP
1831      P:000531 P:000533 218400            MOVE              A1,X0
1832      P:000532 P:000534 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1833      P:000533 P:000535 00000C            RTS
1834                                CLR_FO_RTS
1835      P:000534 P:000536 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1836      P:000535 P:000537 00000C            RTS
1837   
1838   
1839                                ; PCI semaphore
1840                                ;
1841                                ; In order for routines in non-interrupt context to write to the
1842                                ; DTXS, (via PCI_MESSAGE_TO_HOST) they need to end up with
1843                                ; interrupts disabled and HCF3 cleared.
1844                                ;
1845                                ; Non-interrupt PCIers must call PCI_LOCKDOWN before proceeding to
1846                                ; fill DTXS_WD? and call PCI_MESSAGE_TO_HOST.
1847                                ;
1848                                ; Restore with PCI_LOCKUP, or just re-enable HCIE.
1849   
1850                                PCI_LOCKDOWN_AGAIN
1851      P:000536 P:000538 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Re-enable host IRQ
1852      P:000537 P:000539 063280            DO      #50,PCI_LOCKDOWN                  ; Delay for ~us
                            000539
1853      P:000539 P:00053B 000000            NOP
1854   
1855                                PCI_LOCKDOWN
1856      
1857      P:00053A P:00053C 0A8500            BCLR    #DCTR_HCIE,X:DCTR                 ; Disable host IRQ
1858      P:00053B P:00053D 0A85A3            JSET    #DCTR_HF3,X:DCTR,PCI_LOCKDOWN_AGAIN
                            000536
1859      P:00053D P:00053F 00000C            RTS
1860   
1861                                PCI_LOCKUP
1862      P:00053E P:000540 0A8500            BCLR    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
1863      P:00053F P:000541 00000C            RTS
1864   
1865   
1866                                ; ----------------------------------------------------------------------------
1867                                PCI_MESSAGE_TO_HOST
1868                                ;----------------------------------------------------------------------------
1869   
1870                                ; subroutine to send 4 words as a reply from PCI to the Host
1871                                ; using the DTXS-HRXS data path
1872                                ; PCI card writes here first then causes an interrupt INTA on
1873                                ; the PCI bus to alert the host to the reply message
1874   
1875                                ; This routine cannot block for anything because it is always
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 39



1876                                ; called either from a host interrupt handler or with host
1877                                ; interrupts disabled.
1878   
1879                                ;       JSET    #DCTR_HF3,X:DCTR,*      ; make sure host ready to receive interrupt
1880                                ;                                       ; cleared via fast interrupt if host out of its ISR
1881                                ;       JSET    #INTA,X:DCTR,*          ; This should be cleared by host before HF.
1882      P:000540 P:000542 60F400            MOVE              #>DTXS_WD1,R0
                            00000B
1883   
1884      P:000542 P:000544 060480            DO      #4,PCI_MESSAGE_TO_HOST_RESTORE
                            000546
1885      P:000544 P:000546 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            000544
1886      P:000546 P:000548 08D88D            MOVEP             X:(R0)+,X:DTXS
1887   
1888                                PCI_MESSAGE_TO_HOST_RESTORE
1889   
1890      
1891      P:000547 P:000549 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            00001D
1892      P:000549 P:00054B 60F000            MOVE              X:SV_R0,R0              ; restore X0
                            000021
1893   
1894                                                                                    ; Use HF3 as additional handshake
1895      P:00054B P:00054D 0A8523            BSET    #DCTR_HF3,X:DCTR
1896                                                                                    ; only interrupt in irq mode
1897      P:00054C P:00054E 0A01A4            JSET    #MODE_NOIRQ,X:MODE,PCI_MESSAGE_TO_HOST_HANDSHAKE
                            00054F
1898      P:00054E P:000550 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1899                                PCI_MESSAGE_TO_HOST_HANDSHAKE
1900      P:00054F P:000551 0A0185            JCLR    #MODE_HANDSHAKE,X:MODE,PCI_MESSAGE_TO_HOST_RETURN
                            000557
1901      
1902      P:000551 P:000553 0A8983            JCLR    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            000551
1903      P:000553 P:000555 0A8506            BCLR    #INTA,X:DCTR                      ; Clear interrupt
1904      P:000554 P:000556 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; Clear hand-shake bit
1905      P:000555 P:000557 0A89A3            JSET    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            000555
1906                                PCI_MESSAGE_TO_HOST_RETURN
1907      P:000557 P:000559 00000C            RTS
1908   
1909   
1910                                ;---------------------------------------------------------------
1911                                RD_DRXR
1912                                ;--------------------------------------------------------------
1913                                ; Routine to read from HTXR-DRXR data path.  This is where the host
1914                                ; puts data prior to issuing a vector command.
1915                                ;
1916                                ; HCTR[HTF] determines how the data written by the host is decoded
1917                                ; here.  Typically HCTR = 0x900, meaning the 3 LSBs of each 32-bit
1918                                ; word written by the host are returned in each read of DRXR.
1919                                ;
1920                                ; We only check for non-empty FIFO here, so all 4 words must be
1921                                ; written to the FIFO before calling this routine.
1922   
1923      P:000558 P:00055A 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            000558
1924                                                                                    ; implies that host has written words
1925      P:00055A P:00055C 63F400            MOVE              #DRXR_WD1,R3
                            000007
1926      P:00055C P:00055E 0604A0            REP     #4
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 40



1927      P:00055D P:00055F 085B8B            MOVEP             X:DRXR,X:(R3)+
1928      P:00055E P:000560 00000C            RTS
1929   
1930                                ;---------------------------------------------------------------
1931                                READ_FROM_PCI
1932                                ;--------------------------------------------------------------
1933                                ; sub routine to read a 24 bit word in from PCI bus --> Y memory
1934                                ; 32bit host address in accumulator B.
1935   
1936                                ; read as master
1937   
1938      P:00055F P:000561 0C1890            EXTRACTU #$010010,B,A                     ; Get D31-16 bits only
                            010010
1939      P:000561 P:000563 000000            NOP
1940   
1941      P:000562 P:000564 210C00            MOVE              A0,A1
1942      P:000563 P:000565 000000            NOP
1943      P:000564 P:000566 547000            MOVE              A1,X:DPMC               ; high 16bits of address in DSP master cntr 
reg.
                            FFFFC7
1944                                                                                    ; 32 bit read so FC1 = 0 and FC0 = 0
1945   
1946      P:000566 P:000568 000000            NOP
1947      P:000567 P:000569 0C1890            EXTRACTU #$010000,B,A
                            010000
1948      P:000569 P:00056B 000000            NOP
1949      P:00056A P:00056C 210C00            MOVE              A0,A1
1950      P:00056B P:00056D 0140C2            OR      #$060000,A                        ; A1 gets written to DPAR register
                            060000
1951      P:00056D P:00056F 000000            NOP                                       ; C3-C0 of DPAR=0110 for memory read
1952      P:00056E P:000570 08CC08  WRT_ADD   MOVEP             A1,X:DPAR               ; Write address to PCI bus - PCI READ action
1953      P:00056F P:000571 000000            NOP                                       ; Pipeline delay
1954      P:000570 P:000572 0A8AA2  RD_PCI    JSET    #MRRQ,X:DPSR,GET_DAT              ; If MTRQ = 1 go read the word from host via
 FIFO
                            000579
1955      P:000572 P:000574 0A8A8A            JCLR    #TRTY,X:DPSR,RD_PCI               ; Bit is set if its a retry
                            000570
1956      P:000574 P:000576 08F48A            MOVEP             #$0400,X:DPSR           ; Clear bit 10 = target retry bit
                            000400
1957      P:000576 P:000578 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for PCI addressing to be complete
                            000576
1958      P:000578 P:00057A 0C056E            JMP     <WRT_ADD
1959   
1960      P:000579 P:00057B 08480B  GET_DAT   MOVEP             X:DRXR,A0               ; Read 1st 16 bits of 32 bit word from host 
memory
1961      P:00057A P:00057C 084C0B            MOVEP             X:DRXR,A1               ; Read 2nd 16 bits of 32 bit word from host 
memory
1962   
1963                                ; note that we now have 4 bytes in X0 and X1.
1964                                ; The 32bit word was in host memory in little endian format
1965                                ; If form LSB --> MSB the bytes are b1, b2, b3, b4 in host memory
1966                                ; in progressing through the HTRX/DRXR FIFO the
1967                                ; bytes end up like this.....
1968                                ; then X0 = $00 b2 b1
1969                                ; and  X1 = $00 b4 b3
1970   
1971      P:00057B P:00057D 0604A0            REP     #4                                ; increment PCI address by four bytes.
1972      P:00057C P:00057E 000009            INC     B
1973      P:00057D P:00057F 000000            NOP
1974      P:00057E P:000580 00000C            RTS
1975   
1976                                ;------------------------------------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 41



1977                                RESTORE_REGISTERS
1978                                ;-------------------------------------------------------------------------------------
1979   
1980      P:00057F P:000581 05A239            MOVEC             X:<SV_SR,SR
1981   
1982      P:000580 P:000582 509700            MOVE              X:<SV_A0,A0
1983      P:000581 P:000583 549800            MOVE              X:<SV_A1,A1
1984      P:000582 P:000584 529900            MOVE              X:<SV_A2,A2
1985   
1986      P:000583 P:000585 519A00            MOVE              X:<SV_B0,B0
1987      P:000584 P:000586 559B00            MOVE              X:<SV_B1,B1
1988      P:000585 P:000587 539C00            MOVE              X:<SV_B2,B2
1989   
1990      P:000586 P:000588 449D00            MOVE              X:<SV_X0,X0
1991      P:000587 P:000589 459E00            MOVE              X:<SV_X1,X1
1992   
1993      P:000588 P:00058A 469F00            MOVE              X:<SV_Y0,Y0
1994      P:000589 P:00058B 47A000            MOVE              X:<SV_Y1,Y1
1995   
1996      P:00058A P:00058C 60A100            MOVE              X:<SV_R0,R0
1997      P:00058B P:00058D 00000C            RTS
1998   
1999                                ;-------------------------------------------------------------------------------------
2000                                SAVE_REGISTERS
2001                                ;-------------------------------------------------------------------------------------
2002   
2003      P:00058C P:00058E 052239            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
2004   
2005      P:00058D P:00058F 501700            MOVE              A0,X:<SV_A0
2006      P:00058E P:000590 541800            MOVE              A1,X:<SV_A1
2007      P:00058F P:000591 521900            MOVE              A2,X:<SV_A2
2008   
2009      P:000590 P:000592 511A00            MOVE              B0,X:<SV_B0
2010      P:000591 P:000593 551B00            MOVE              B1,X:<SV_B1
2011      P:000592 P:000594 531C00            MOVE              B2,X:<SV_B2
2012   
2013      P:000593 P:000595 441D00            MOVE              X0,X:<SV_X0
2014      P:000594 P:000596 451E00            MOVE              X1,X:<SV_X1
2015   
2016      P:000595 P:000597 461F00            MOVE              Y0,X:<SV_Y0
2017      P:000596 P:000598 472000            MOVE              Y1,X:<SV_Y1
2018   
2019      P:000597 P:000599 602100            MOVE              R0,X:<SV_R0
2020      P:000598 P:00059A 00000C            RTS
2021   
2022                                ;-------------------------------------------------------
2023                                XMT_WD_FIBRE
2024                                ;-----------------------------------------------------
2025                                ; 250 MHz code - Transmit contents of Accumulator A1 to the MCE
2026                                ; we want to send 32bit word in little endian fomat to the host.
2027                                ; i.e. b4b3b2b1 goes b1, b2, b3, b4
2028                                ; currently the bytes are in this order:
2029                                ;  A0 = $00 b2 b1
2030                                ;  A1 = $00 b4 b3
2031                                ;  A = $00 00 b4 b3 00 b2 b1
2032   
2033      
2034   
2035      P:000599 P:00059B 212400            MOVE              B0,X0                   ; Save B
2036      P:00059A P:00059C 21A500            MOVE              B1,X1
2037   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 42



2038      P:00059B P:00059D 0C1D31            ASL     #24,A,B
2039      P:00059C P:00059E 0140CE            AND     #>$0000FF,B                       ; B1=b1
                            0000FF
2040      P:00059E P:0005A0 557000            MOVE              B1,X:FO_SEND
                            FFF000
2041   
2042      P:0005A0 P:0005A2 0C1D21            ASL     #16,A,B
2043      P:0005A1 P:0005A3 0140CE            AND     #>$0000FF,B
                            0000FF
2044      P:0005A3 P:0005A5 557000            MOVE              B1,X:FO_SEND            ; B1=b2
                            FFF000
2045   
2046      P:0005A5 P:0005A7 0C1C11            ASR     #8,A,B
2047      P:0005A6 P:0005A8 0140C6            AND     #>$0000FF,A
                            0000FF
2048      P:0005A8 P:0005AA 547000            MOVE              A1,X:FO_SEND            ; A1=b3
                            FFF000
2049   
2050      P:0005AA P:0005AC 0140CE            AND     #>$0000FF,B
                            0000FF
2051      P:0005AC P:0005AE 557000            MOVE              B1,X:FO_SEND            ; B1=b4
                            FFF000
2052   
2053      P:0005AE P:0005B0 208900            MOVE              X0,B0                   ; Restore B
2054      P:0005AF P:0005B1 20AD00            MOVE              X1,B1
2055      P:0005B0 P:0005B2 00000C            RTS
2056   
2057   
2058                                ;----------------------------------------------
2059                                FLUSH_PCI_FIFO
2060                                ;----------------------------------------------
2061      P:0005B1 P:0005B3 0A8A84            JCLR    #MARQ,X:DPSR,*
                            0005B1
2062      P:0005B3 P:0005B5 0A862E            BSET    #CLRT,X:DPCR
2063      P:0005B4 P:0005B6 000000            NOP
2064      P:0005B5 P:0005B7 0A86AE            JSET    #CLRT,X:DPCR,*
                            0005B5
2065      P:0005B7 P:0005B9 00000C            RTS
2066   
2067                                ;----------------------------------------------
2068                                CLEAR_FO_FIFO
2069                                ;----------------------------------------------
2070      P:0005B8 P:0005BA 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
2071      P:0005BA P:0005BC 44F400            MOVE              #200000,X0
                            030D40
2072      P:0005BC P:0005BE 06C400            DO      X0,*+3
                            0005BE
2073      P:0005BE P:0005C0 000000            NOP
2074      P:0005BF P:0005C1 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
2075      P:0005C1 P:0005C3 00000C            RTS
2076   
2077   
2078                                ;-----------------------------------------------
2079                                PCI_ERROR_CLEAR
2080                                ;-----------------------------------------------
2081      
2082      
2083      
2084      
2085      
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 43



2086      
2087   
2088      P:0005C2 P:0005C4 50F000            MOVE              X:DMA_ERRORS,A0
                            000037
2089      P:0005C4 P:0005C6 000008            INC     A
2090      P:0005C5 P:0005C7 000000            NOP
2091      P:0005C6 P:0005C8 507000            MOVE              A0,X:DMA_ERRORS
                            000037
2092   
2093      P:0005C8 P:0005CA 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            0005D6
2094      P:0005CA P:0005CC 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
                            0005E0
2095      P:0005CC P:0005CE 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            0005EA
2096      P:0005CE P:0005D0 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            0005F4
2097      P:0005D0 P:0005D2 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            0005FE
2098      P:0005D2 P:0005D4 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            000608
2099      P:0005D4 P:0005D6 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            000612
2100   
2101                                ERROR_TRTY
2102      P:0005D6 P:0005D8 50F000            MOVE              X:EC_TRTY,A0
                            000038
2103      P:0005D8 P:0005DA 000008            INC     A
2104      P:0005D9 P:0005DB 08F48A            MOVEP             #>$0400,X:DPSR          ; Clear target retry error bit
                            000400
2105      P:0005DB P:0005DD 507000            MOVE              A0,X:EC_TRTY
                            000038
2106      P:0005DD P:0005DF 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2107      P:0005DF P:0005E1 00000C            RTS
2108                                ERROR_TO
2109      P:0005E0 P:0005E2 50F000            MOVE              X:EC_TO,A0
                            000039
2110      P:0005E2 P:0005E4 000008            INC     A
2111      P:0005E3 P:0005E5 08F48A            MOVEP             #>$0800,X:DPSR          ; Clear timeout error bit
                            000800
2112      P:0005E5 P:0005E7 507000            MOVE              A0,X:EC_TO
                            000039
2113      P:0005E7 P:0005E9 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
2114      P:0005E9 P:0005EB 00000C            RTS
2115                                ERROR_TDIS
2116      P:0005EA P:0005EC 50F000            MOVE              X:EC_TDIS,A0
                            00003A
2117      P:0005EC P:0005EE 000008            INC     A
2118      P:0005ED P:0005EF 08F48A            MOVEP             #>$0200,X:DPSR          ; Clear target disconnect bit
                            000200
2119      P:0005EF P:0005F1 507000            MOVE              A0,X:EC_TDIS
                            00003A
2120      P:0005F1 P:0005F3 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
2121      P:0005F3 P:0005F5 00000C            RTS
2122                                ERROR_TAB
2123      P:0005F4 P:0005F6 50F000            MOVE              X:EC_TAB,A0
                            00003B
2124      P:0005F6 P:0005F8 000008            INC     A
2125      P:0005F7 P:0005F9 08F48A            MOVEP             #>$0100,X:DPSR          ; Clear target abort error bit
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 44



                            000100
2126      P:0005F9 P:0005FB 507000            MOVE              A0,X:EC_TAB
                            00003B
2127      P:0005FB P:0005FD 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2128      P:0005FD P:0005FF 00000C            RTS
2129                                ERROR_MAB
2130      P:0005FE P:000600 50F000            MOVE              X:EC_MAB,A0
                            00003C
2131      P:000600 P:000602 000008            INC     A
2132      P:000601 P:000603 08F48A            MOVEP             #>$0080,X:DPSR          ; Clear master abort error bit
                            000080
2133      P:000603 P:000605 507000            MOVE              A0,X:EC_MAB
                            00003C
2134      P:000605 P:000607 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2135      P:000607 P:000609 00000C            RTS
2136                                ERROR_DPER
2137      P:000608 P:00060A 50F000            MOVE              X:EC_DPER,A0
                            00003D
2138      P:00060A P:00060C 000008            INC     A
2139      P:00060B P:00060D 08F48A            MOVEP             #>$0040,X:DPSR          ; Clear data parity error bit
                            000040
2140      P:00060D P:00060F 507000            MOVE              A0,X:EC_DPER
                            00003D
2141      P:00060F P:000611 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2142      P:000611 P:000613 00000C            RTS
2143                                ERROR_APER
2144      P:000612 P:000614 50F000            MOVE              X:EC_APER,A0
                            00003E
2145      P:000614 P:000616 000008            INC     A
2146      P:000615 P:000617 08F48A            MOVEP             #>$0020,X:DPSR          ; Clear address parity error bit
                            000020
2147      P:000617 P:000619 507000            MOVE              A0,X:EC_APER
                            00003E
2148      P:000619 P:00061B 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2149      P:00061B P:00061D 00000C            RTS
2150   
2151   
2152                                ;----------------------------------------------
2153                                BLOCK_TRANSFER
2154                                ;----------------------------------------------
2155                                ;   In:
2156                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
2157                                ;   - BLOCK_SIZE is packet size, in bytes
2158                                ;   - BURST_SRC is start of data in Y memory
2159                                ;  Out:
2160                                ;   - BLOCK_SIZE will be decremented to zero.
2161                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
2162                                ;   - BURST_SRC will be incremented by BLOCK_SIZE/2
2163                                ;  Trashes:
2164                                ;   - A and B
2165   
2166      
2167      P:00061C P:00061E 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002F
2168   
2169      P:00061E P:000620 014085            CMP     #0,A
2170      P:00061F P:000621 0AF0AA            JEQ     BLOCK_DONE
                            000663
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 45



2171   
2172      
2173   
2174      P:000621 P:000623 20001B            CLR     B
2175      P:000622 P:000624 55F000            MOVE              X:PCI_BURST_SIZE,B1
                            000051
2176   
2177      P:000624 P:000626 200005            CMP     B,A                               ; A ? B
2178      P:000625 P:000627 0E1627            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
2179      P:000626 P:000628 21CF00            MOVE              A,B                     ; This only moves A1,B1.
2180                                BLOCK_TRANSFER1
2181      P:000627 P:000629 200014            SUB     B,A                               ; A -= B
2182      P:000628 P:00062A 014088            ADD     #0,B                              ; Clear carry bit
2183      P:000629 P:00062B 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002F
2184      P:00062B P:00062D 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            000030
2185      P:00062D P:00062F 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
2186   
2187      
2188      P:00062E P:000630 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
2189      P:000630 P:000632 50F000            MOVE              X:YMEM_SRC,A0
                            000035
2190      P:000632 P:000634 08C82F            MOVEP             A0,X:DSR0               ; DMA source
2191      P:000633 P:000635 200010            ADD     B,A
2192      P:000634 P:000636 00000B            DEC     B
2193      P:000635 P:000637 507000            MOVE              A0,X:YMEM_SRC           ; BURST_SRC += BURST_SIZE/2
                            000035
2194   
2195      P:000637 P:000639 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
2196   
2197      
2198      P:000638 P:00063A 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
2199   
2200                                BLOCK_PCI
2201      
2202      P:00063A P:00063C 200013            CLR     A
2203      P:00063B P:00063D 20001B            CLR     B
2204      P:00063C P:00063E 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            000030
2205      P:00063E P:000640 00000B            DEC     B                                 ; n8 - 1
2206      P:00063F P:000641 014088            ADD     #0,B                              ; Clear carry
2207      P:000640 P:000642 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
2208      P:000641 P:000643 014088            ADD     #0,B                              ; Clear carry
2209      P:000642 P:000644 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
2210   
2211      P:000643 P:000645 50F000            MOVE              X:BURST_DEST_HI,A0
                            000032
2212   
2213      P:000645 P:000647 200010            ADD     B,A
2214      P:000646 P:000648 000000            NOP
2215      P:000647 P:000649 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
2216   
2217      P:000649 P:00064B 280700            MOVE              #$07,A0
2218      P:00064A P:00064C 014088            ADD     #0,B                              ; Clear carry
2219      P:00064B P:00064D 0C1D20            ASL     #16,A,A
2220      P:00064C P:00064E 51F000            MOVE              X:BURST_DEST_LO,B0
                            000031
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 46



2221      P:00064E P:000650 200010            ADD     B,A
2222      P:00064F P:000651 000000            NOP
2223   
2224      P:000650 P:000652 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
2225   
2226                                BLOCK_CHECK
2227      P:000651 P:000653 000000            NOP
2228      P:000652 P:000654 000000            NOP
2229      P:000653 P:000655 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for burst termination
                            000653
2230   
2231      
2232      P:000655 P:000657 0A8AAE            JSET    #MDT,X:DPSR,BLOCK_OK
                            00065E
2233   
2234      P:000657 P:000659 0D05C2            JSR     PCI_ERROR_CLEAR
2235   
2236      P:000658 P:00065A 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
2237      P:00065A P:00065C 0E8664            JCS     <BLOCK_RESTART
2238   
2239      P:00065B P:00065D 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
2240      P:00065D P:00065F 0E8665            JCS     <BLOCK_RESUME
2241   
2242                                BLOCK_OK
2243      P:00065E P:000660 50F000            MOVE              X:BURST_SIZE,A0         ; Pass # of words written to updater
                            000030
2244      P:000660 P:000662 0BF080            JSR     BLOCK_UPDATE
                            000677
2245      P:000662 P:000664 0C061C            JMP     BLOCK_TRANSFER                    ; Finish the block
2246                                BLOCK_DONE
2247      P:000663 P:000665 00000C            RTS                                       ; Done
2248   
2249                                BLOCK_RESTART
2250      P:000664 P:000666 0C063A            JMP     BLOCK_PCI                         ; Recalculate pci and resend
2251   
2252                                BLOCK_RESUME
2253      P:000665 P:000667 200013            CLR     A
2254      P:000666 P:000668 20001B            CLR     B
2255      P:000667 P:000669 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
2256      P:000668 P:00066A 0A8A8F            JCLR    #RDCQ,X:DPSR,BLOCK_RESUME1
                            00066B
2257   
2258      P:00066A P:00066C 000009            INC     B
2259   
2260                                BLOCK_RESUME1
2261   
2262      P:00066B P:00066D 000009            INC     B                                 ; We want N, not N-1.
2263      P:00066C P:00066E 014088            ADD     #0,B                              ; Clear carry
2264      P:00066D P:00066F 0C1C20            ASR     #16,A,A
2265      P:00066E P:000670 200018            ADD     A,B                               ; B is words remaining
2266      P:00066F P:000671 014088            ADD     #0,B                              ; Clear carry
2267      P:000670 P:000672 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
2268      P:000671 P:000673 50F000            MOVE              X:BURST_SIZE,A0
                            000030
2269      P:000673 P:000675 200014            SUB     B,A                               ; A is words written
2270   
2271      P:000674 P:000676 0BF080            JSR     BLOCK_UPDATE
                            000677
2272      P:000676 P:000678 0C063A            JMP     BLOCK_PCI                         ; Recalculate pci and resend
2273   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 47



2274                                ; BLOCK_UPDATE
2275                                ;  Subtract A from BURST_SIZE and add A to BURST_DEST_LO
2276                                ;  Caller can check Z flag to see if BURST_SIZE is now 0.
2277                                BLOCK_UPDATE
2278      P:000677 P:000679 210500            MOVE              A0,X1                   ; Save A for later
2279      P:000678 P:00067A 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2280   
2281      P:000679 P:00067B 60F400            MOVE              #BURST_DEST_LO,R0       ;
                            000031
2282      P:00067B P:00067D 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates BURST_DEST to BURST_DEST + B
                            000759
2283   
2284      P:00067D P:00067F 57F000            MOVE              X:BURST_SIZE,B
                            000030
2285      P:00067F P:000681 20006C            SUB     X1,B                              ; Zero flag must be preserved!
2286      P:000680 P:000682 000000            NOP
2287      P:000681 P:000683 557000            MOVE              B1,X:BURST_SIZE
                            000030
2288   
2289      P:000683 P:000685 00000C            RTS
2290   
2291   
2292                                ;----------------------------------------------;
2293                                ;  MCE PACKET PROCESSING                       ;
2294                                ;----------------------------------------------;
2295   
2296                                ;       Given a dword count in A, computes number of half FIFOs and
2297                                ;       number of left over FIFO reads required to get the whole
2298                                ;       packet.
2299   
2300                                ;       Input: A is packet size, in dwords
2301                                ;       Output: sets X:TOTAL_BUFFS and X:LEFT_TO_READ
2302                                ;       Trashes: A,B,X0
2303   
2304   
2305                                PACKET_PARTITIONS
2306      P:000684 P:000686 507000            MOVE              A0,X:PACKET_SIZE
                            00002B
2307   
2308      P:000686 P:000688 014088            ADD     #0,B                              ; Clear carry
2309      P:000687 P:000689 0C1D02            ASL     #1,A,A                            ;  * 2
2310      P:000688 P:00068A 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
2311      P:000689 P:00068B 240000            MOVE              #0,X0
2312      P:00068A P:00068C 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
2313   
2314      P:00068C P:00068E 557000            MOVE              B1,X:TOTAL_BUFFS
                            000027
2315      P:00068E P:000690 507000            MOVE              A0,X:LEFT_TO_READ
                            000028
2316      P:000690 P:000692 00000C            RTS
2317   
2318   
2319                                ; BUFFER_PACKET
2320                                ;
2321                                ; Copies the packet in the FIFO to Y memory.
2322   
2323                                ; In: TOTAL_BUFFS and LEFT_TO_READ must be pre-set (via PACKET_PARTITIONS);
2324                                ;     R1 is the destination index in Y memory.
2325                                ; Trashes: R1 is updated to point to the end of the copied data.
2326   
2327                                BUFFER_PACKET
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 48



2328   
2329      P:000691 P:000693 54F400            MOVE              #>$b00,A1
                            000B00
2330      P:000693 P:000695 0BF080            JSR     TIMER_STORE_A1
                            000707
2331      P:000695 P:000697 0BF080            JSR     TIMER_STORE
                            000705
2332   
2333      P:000697 P:000699 062700            DO      X:TOTAL_BUFFS,BUFFER_PACKET_HALFS_DONE
                            0006A1
2334      P:000699 P:00069B 0BF080            JSR     WAIT_FIFO_HALF
                            0006C6
2335      P:00069B P:00069D 0BF080            JSR     TIMER_STORE
                            000705
2336      P:00069D P:00069F 0BF080            JSR     BUFFER_PACKET_HALF
                            0006C1
2337      P:00069F P:0006A1 0BF080            JSR     TIMER_STORE
                            000705
2338      P:0006A1 P:0006A3 000000            NOP
2339                                BUFFER_PACKET_HALFS_DONE
2340   
2341      
2342      
2343      P:0006A2 P:0006A4 01AD81            JCLR    #HF,X:PDRD,BUFFER_PACKET_SINGLES_FAST
                            0006B2
2345      P:0006A4 P:0006A6 0AF080            JMP     BUFFER_PACKET_SINGLES_NOT_QUITE_SO_FAST
                            0006B6
2346   
2347      P:0006A6 P:0006A8 062800            DO      X:LEFT_TO_READ,BUFFER_PACKET_DONE
                            0006B0
2348                                BUFFER_PACKET_SINGLE
2349      P:0006A8 P:0006AA 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            000367
2350      P:0006AA P:0006AC 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE
                            0006A8
2351      P:0006AC P:0006AE 000000            NOP
2352      P:0006AD P:0006AF 000000            NOP
2353      P:0006AE P:0006B0 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE   ; Protect against metastability
                            0006A8
2354      P:0006B0 P:0006B2 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2355                                BUFFER_PACKET_DONE
2356      P:0006B1 P:0006B3 00000C            RTS
2357   
2358                                BUFFER_PACKET_SINGLES_FAST
2359      P:0006B2 P:0006B4 062800            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_FAST_DONE
                            0006B4
2360      P:0006B4 P:0006B6 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2361                                BUFFER_PACKET_SINGLES_FAST_DONE
2362      P:0006B5 P:0006B7 00000C            RTS
2363   
2366   
2367                                BUFFER_PACKET_SINGLES_NOT_QUITE_SO_FAST
2368      P:0006B6 P:0006B8 062800            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_NOT_QUITE_SO_FAST_DONE
                            0006B8
2369      P:0006B8 P:0006BA 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2370                                BUFFER_PACKET_SINGLES_NOT_QUITE_SO_FAST_DONE
2371      P:0006B9 P:0006BB 000000            NOP
2372      P:0006BA P:0006BC 000000            NOP
2373      P:0006BB P:0006BD 000000            NOP
2374      P:0006BC P:0006BE 0BF080            JSR     TIMER_STORE
                            000705
2375      P:0006BE P:0006C0 0BF080            JSR     TIMER_STORE
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 49



                            000705
2376      P:0006C0 P:0006C2 00000C            RTS
2377   
2378                                BUFFER_PACKET_HALF
2379      
2380      P:0006C1 P:0006C3 060082            DO      #512,BUFFER_PACKET_HALF_DONE
                            0006C4
2381      P:0006C3 P:0006C5 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2382      P:0006C4 P:0006C6 000000            NOP
2383                                BUFFER_PACKET_HALF_DONE
2384      P:0006C5 P:0006C7 00000C            RTS
2385   
2386                                WAIT_FIFO_HALF
2387      P:0006C6 P:0006C8 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            0006CF
2388      P:0006C8 P:0006CA 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
                            0006C6
2389      P:0006CA P:0006CC 000000            NOP
2390      P:0006CB P:0006CD 000000            NOP
2391      P:0006CC P:0006CE 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            0006C6
2392      P:0006CE P:0006D0 00000C            RTS
2393   
2394                                FATALITY_HANDLER
2395      P:0006CF P:0006D1 0C0100            JMP     START                             ; What could possibly go wrong?
2396   
2397   
2398                                ; DROP_PACKET
2399                                ;
2400                                ; Reads a packet from the fifo, discarding it.
2401                                ;
2402                                ; In: TOTAL_BUFFS & LEFT_TO_READ
2403                                ; Trashes: A0
2404   
2405                                DROP_PACKET
2406      P:0006D0 P:0006D2 062700            DO      X:TOTAL_BUFFS,DROP_PACKET_SINGLES
                            0006D5
2407      P:0006D2 P:0006D4 0D06C6            JSR     WAIT_FIFO_HALF
2408      P:0006D3 P:0006D5 0BF080            JSR     DROP_FIFO_HALF
                            0006E0
2409      P:0006D5 P:0006D7 000000            NOP
2410                                DROP_PACKET_SINGLES
2411      P:0006D6 P:0006D8 062800            DO      X:LEFT_TO_READ,DROP_PACKET_DONE
                            0006DE
2412                                DROP_PACKET_SINGLE
2413      P:0006D8 P:0006DA 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            000367
2414      P:0006DA P:0006DC 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE
                            0006D8
2415      P:0006DC P:0006DE 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE     ; Protect against metastability
                            0006D8
2416      P:0006DE P:0006E0 09483F            MOVEP             Y:RDFIFO,A0
2417                                DROP_PACKET_DONE
2418      P:0006DF P:0006E1 00000C            RTS
2419   
2420                                DROP_FIFO_HALF
2421      
2422      P:0006E0 P:0006E2 060082            DO      #512,DROP_FIFO_DONE
                            0006E2
2423      P:0006E2 P:0006E4 09483F            MOVEP             Y:RDFIFO,A0
2424                                DROP_FIFO_DONE
2425      P:0006E3 P:0006E5 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 50



2426   
2427   
2428                                ;----------------------------------------------;
2429                                ;  TIMER HANDLING                              ;
2430                                ;----------------------------------------------;
2431   
2432                                ; Start value is TLR, count is in TCR, int occurs at TCPR
2433                                ; Must set TCSR[TCIE] to enable int
2434                                ; Must set TCSR[T] for timer to restart
2435   
2436                                TIMER_ENABLE
2437      P:0006E4 P:0006E6 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2438      P:0006E6 P:0006E8 000000            NOP
2439      P:0006E7 P:0006E9 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2440      P:0006E9 P:0006EB 00000C            RTS
2441   
2442                                TIMER_DISABLE
2443      P:0006EA P:0006EC 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
                            300200
2444      P:0006EC P:0006EE 000000            NOP
2445      P:0006ED P:0006EF 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2446      P:0006EF P:0006F1 00000C            RTS
2447   
2449                                TIMER_ACTION
2450      P:0006F0 P:0006F2 56F000            MOVE              X:QT_INFORM_IDX,A
                            000049
2451      P:0006F2 P:0006F4 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2452      P:0006F4 P:0006F6 000000            NOP
2453      P:0006F5 P:0006F7 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2454      P:0006F7 P:0006F9 0140C5            CMP     #>0,A                             ; If inform_idx != 0
                            000000
2455      P:0006F9 P:0006FB 0AF0AA            JEQ     TIMER_ACTION_OK
                            0006FD
2456      P:0006FB P:0006FD 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2457                                TIMER_ACTION_OK
2458      P:0006FD P:0006FF 00000C            RTS
2459   
2460   
2461                                ;----------------------------------------------;
2462                                ;  TIMER UTILITY                               ;
2463                                ;----------------------------------------------;
2464   
2465                                TIMER_STORE_INIT
2466      P:0006FE P:000700 50F400            MOVE              #>TIMER_BUFFER,A0
                            201000
2467      P:000700 P:000702 000000            NOP
2468      P:000701 P:000703 507000            MOVE              A0,X:TIMER_INDEX
                            000052
2469      P:000703 P:000705 211400            MOVE              A0,R4
2470      P:000704 P:000706 00000C            RTS
2471   
2472                                TIMER_STORE
2473      
2474      
2475      P:000705 P:000707 56F000            MOVE              X:TCR0,A
                            FFFF8C
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 51



2476                                                                                    ; Fall-through
2477   
2478                                TIMER_STORE_A1
2479      
2480      P:000707 P:000709 5C5C00            MOVE                          A1,Y:(R4)+
2481      P:000708 P:00070A 228C00            MOVE              R4,A1
2482      P:000709 P:00070B 0140C5            CMP     #>TIMER_BUFFER_END,A
                            202000
2483      P:00070B P:00070D 547000            MOVE              A1,X:TIMER_INDEX
                            000052
2484      P:00070D P:00070F 0E16FE            JGE     TIMER_STORE_INIT
2485      P:00070E P:000710 00000C            RTS
2486   
2487   
2488                                ;----------------------------------------------;
2489                                ;  CIRCULAR BUFFER HANDLING                    ;
2490                                ;----------------------------------------------;
2491   
2492                                BUFFER_INCR
2493   
2494      P:00070F P:000711 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
                            000045
2495      P:000711 P:000713 014180            ADD     #1,A                              ;
2496      P:000712 P:000714 57F000            MOVE              X:QT_BUF_MAX,B          ;
                            000042
2497      P:000714 P:000716 20000D            CMP     A,B                               ;
2498      P:000715 P:000717 0AF0AF            JLE     BUFFER_RESET                      ;       head = 0
                            000721
2499                                                                                    ; else
2500      P:000717 P:000719 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            000045
2501   
2502      P:000719 P:00071B 20001B            CLR     B
2503      P:00071A P:00071C 51F000            MOVE              X:QT_BUF_SIZE,B0
                            000041
2504      P:00071C P:00071E 60F400            MOVE              #QT_DEST_LO,R0
                            000047
2505      P:00071E P:000720 0BF080            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
                            000759
2506   
2507      P:000720 P:000722 00000C            RTS
2508   
2509   
2510                                BUFFER_RESET
2511      P:000721 P:000723 60F400            MOVE              #QT_BASE_LO,R0
                            00003F
2512      P:000723 P:000725 0BF080            JSR     LOAD_HILO_ADDRESS
                            000753
2513      P:000725 P:000727 60F400            MOVE              #QT_DEST_LO,R0
                            000047
2514      P:000727 P:000729 0BF080            JSR     SAVE_HILO_ADDRESS                 ; QT_DEST_LO = QT_BASE_LO
                            00075B
2515   
2516      P:000729 P:00072B 240000            MOVE              #0,X0
2517      P:00072A P:00072C 447000            MOVE              X0,X:QT_BUF_HEAD        ; HEAD = 0
                            000045
2518      P:00072C P:00072E 00000C            RTS
2519   
2520   
2521                                BUFFER_INFORM_CHECK
2522      P:00072D P:00072F 56F000            MOVE              X:QT_INFORM_IDX,A
                            000049
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 52



2523      P:00072F P:000731 014180            ADD     #1,A
2524      P:000730 P:000732 57F000            MOVE              X:QT_INFORM,B
                            000044
2525      P:000732 P:000734 20000D            CMP     A,B
2526      P:000733 P:000735 0AF0A7            JGT     BUFFER_INFORM_OK                  ; If inform_idx + 1 <= inform
                            000737
2527      P:000735 P:000737 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2528   
2529                                BUFFER_INFORM_OK
2530      P:000737 P:000739 567000            MOVE              A,X:QT_INFORM_IDX       ; inform_idx = inform_idx + 1
                            000049
2531      P:000739 P:00073B 00000C            RTS
2532   
2533   
2534                                ;---------------------------------------------------------------
2535                                BUFFER_INFORM
2536                                ;---------------------------------------------------------------
2537                                ; Informs host of current buffer status
2538   
2539      
2540      P:00073A P:00073C 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            000752
2541      P:00073C P:00073E 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            000752
2542   
2543      P:00073E P:000740 0D053A            JSR     PCI_LOCKDOWN                      ; Disable host IRQ
2544   
2545      P:00073F P:000741 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2546      P:000741 P:000743 440B00            MOVE              X0,X:<DTXS_WD1
2547   
2548      P:000742 P:000744 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            000045
2549      P:000744 P:000746 440C00            MOVE              X0,X:<DTXS_WD2
2550   
2551      P:000745 P:000747 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000046
2552      P:000747 P:000749 440D00            MOVE              X0,X:<DTXS_WD3
2553   
2554      P:000748 P:00074A 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
                            00004A
2555      P:00074A P:00074C 440E00            MOVE              X0,X:<DTXS_WD4
2556   
2557      P:00074B P:00074D 0D0540            JSR     PCI_MESSAGE_TO_HOST
2558   
2559      P:00074C P:00074E 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
2560      P:00074E P:000750 240000            MOVE              #0,X0                   ; Reset inform index
2561      P:00074F P:000751 447000            MOVE              X0,X:QT_INFORM_IDX
                            000049
2562      P:000751 P:000753 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host IRQ
2563                                INFORM_EXIT
2564      P:000752 P:000754 00000C            RTS
2565   
2566   
2567   
2568                                ;----------------------------------------------;
2569                                ;  ADDRESS HANDLING                            ;
2570                                ;----------------------------------------------;
2571   
2575   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  main.asm  Page 53



2576                                LOAD_HILO_ADDRESS
2577      
2578      
2579      P:000753 P:000755 200013            CLR     A
2580      P:000754 P:000756 50D800            MOVE              X:(R0)+,A0
2581      P:000755 P:000757 44D000            MOVE              X:(R0)-,X0
2582      P:000756 P:000758 0C1940            INSERT  #$010010,X0,A
                            010010
2583      P:000758 P:00075A 00000C            RTS
2584   
2585                                ADD_HILO_ADDRESS
2586      
2587      
2588   
2589      P:000759 P:00075B 0D0753            JSR     LOAD_HILO_ADDRESS
2590      P:00075A P:00075C 200010            ADD     B,A
2591   
2592                                SAVE_HILO_ADDRESS
2593      
2594      
2595   
2596      P:00075B P:00075D 445800            MOVE              X0,X:(R0)+              ; pre-increment
2597      P:00075C P:00075E 240000            MOVE              #0,X0
2598      P:00075D P:00075F 0C1D11            ASL     #8,A,B
2599      P:00075E P:000760 0C1940            INSERT  #$008010,X0,A
                            008010
2600      P:000760 P:000762 555000            MOVE              B1,X:(R0)-              ; store hi16
2601      P:000761 P:000763 506000            MOVE              A0,X:(R0)
2602      P:000762 P:000764 0C1C90            ASR     #8,B,A
2603      P:000763 P:000765 00000C            RTS
2604   
2605   
2606                                BOOTCODE_END
2607                                 BOOTEND_ADDR
2608      000764                              EQU     @CVI(BOOTCODE_END)
2609   
2610                                PROGRAM_END
2611      000764                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2612                                          INCLUDE 'vars.asm'
2613                                      COMMENT *
2614   
2615                                Variable table and bit defines for our variables.
2616   
2617                                See info.asm for versioning and authors.
2618   
2619                                        *
2620   
2621   
2622                                ; The variable table is mapped to X memory but stored inline in the
2623                                ; eeprom / P memory after the main code (but before the application
2624                                ; area).
2625   
2626      X:000000 P:000766                   ORG     X:VAR_TBL,P:
2627   
2628   
2629                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2630                                 VAR_TBL_START
2631      000764                              EQU     @LCV(L)-2
2632                                          ENDIF
2633   
2634                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2636                                          ENDIF
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  vars.asm  Page 54



2637   
2638                                ; -----------------------------------------------
2639 d    X:000000 P:000766 000000  STATUS    DC      0                                 ; Internal control flags
2640 d    X:000001 P:000767 000000  MODE      DC      0                                 ; Configure special options
2641   
2642 d                               FRAME_COUNT
2643 d    X:000002 P:000768 000000            DC      0
2644 d    X:000003 P:000769 550105  REV_NUMBER DC     $550105                           ; byte 0 = minor revision #
2645                                                                                    ; byte 1 = major revision #
2646                                                                                    ; byte 2 = release Version (ascii letter)
2647 d    X:000004 P:00076A 000000  REV_DATA  DC      $000000                           ; data: day-month-year
2648 d    X:000005 P:00076B 2EF490  P_CHECKSUM DC     $2EF490                           ;**** DO NOT CHANGE
2649                                ; -------------------------------------------------
2650 d    X:000006 P:00076C 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2651                                ; ----------------------------------------------------------------------------------------------
----------------
2652   
2653 d    X:000007 P:00076D 000000  DRXR_WD1  DC      0
2654 d    X:000008 P:00076E 000000  DRXR_WD2  DC      0
2655 d    X:000009 P:00076F 000000  DRXR_WD3  DC      0
2656 d    X:00000A P:000770 000000  DRXR_WD4  DC      0
2657 d    X:00000B P:000771 000000  DTXS_WD1  DC      0
2658 d    X:00000C P:000772 000000  DTXS_WD2  DC      0
2659 d    X:00000D P:000773 000000  DTXS_WD3  DC      0
2660 d    X:00000E P:000774 000000  DTXS_WD4  DC      0
2661   
2662 d    X:00000F P:000775 000000  HEAD_W1_0 DC      0                                 ; Preamble $A5A5
2663 d    X:000010 P:000776 000000  HEAD_W1_1 DC      0                                 ;          $A5A5
2664 d    X:000011 P:000777 000000  HEAD_W2_0 DC      0                                 ;          $5A5A
2665 d    X:000012 P:000778 000000  HEAD_W2_1 DC      0                                 ;          $5A5A
2666 d    X:000013 P:000779 000000  HEAD_W3_0 DC      0                                 ; 'RP' or 'DA'
2667 d    X:000014 P:00077A 000000  HEAD_W3_1 DC      0                                 ; '  '   $2020
2668 d    X:000015 P:00077B 000000  HEAD_W4_0 DC      0                                 ; Packet size LSW
2669 d    X:000016 P:00077C 000000  HEAD_W4_1 DC      0                                 ;             MSW
2670   
2671 d    X:000017 P:00077D 000000  SV_A0     DC      0
2672 d    X:000018 P:00077E 000000  SV_A1     DC      0
2673 d    X:000019 P:00077F 000000  SV_A2     DC      0
2674 d    X:00001A P:000780 000000  SV_B0     DC      0
2675 d    X:00001B P:000781 000000  SV_B1     DC      0
2676 d    X:00001C P:000782 000000  SV_B2     DC      0
2677 d    X:00001D P:000783 000000  SV_X0     DC      0
2678 d    X:00001E P:000784 000000  SV_X1     DC      0
2679 d    X:00001F P:000785 000000  SV_Y0     DC      0
2680 d    X:000020 P:000786 000000  SV_Y1     DC      0
2681 d    X:000021 P:000787 000000  SV_R0     DC      0
2682   
2683 d    X:000022 P:000788 000000  SV_SR     DC      0                                 ; stauts register save.
2684   
2685 d                               PACKET_SIZE_LOW
2686 d    X:000023 P:000789 000000            DC      0
2687 d                               PACKET_SIZE_HIH
2688 d    X:000024 P:00078A 000000            DC      0
2689   
2690 d    X:000025 P:00078B 00A5A5  PREAMB1   DC      $A5A5                             ; pramble 16-bit word....2 of which make up 
first preamble 32bit word
2691 d    X:000026 P:00078C 005A5A  PREAMB2   DC      $5A5A                             ; preamble 16-bit word....2 of which make up
 second preamble 32bit word
2692   
2693 d                               TOTAL_BUFFS
2694 d    X:000027 P:00078D 000000            DC      0                                 ; total number of 512 buffers in packet
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  vars.asm  Page 55



2695 d                               LEFT_TO_READ
2696 d    X:000028 P:00078E 000000            DC      0                                 ; number of words (16 bit) left to read afte
r last 512 buffer
2697 d                               LEFT_TO_WRITE
2698 d    X:000029 P:00078F 000000            DC      0                                 ; number of woreds (32 bit) to write to host
 i.e. half of those left over read
2699 d                               NUM_LEFTOVER_BLOCKS
2700 d    X:00002A P:000790 000000            DC      0                                 ; small block DMA burst transfer
2701   
2702 d                               PACKET_SIZE
2703 d    X:00002B P:000791 000000            DC      0                                 ; Size, in dwords of most recent packet from
 MCE.
2704   
2705   
2707 d                               PREAMBLE_ERRORS
2708 d    X:00002C P:000792 000000            DC      0                                 ; Failed on preamble processing
2709 d                               PTYPE_ERRORS
2710 d    X:00002D P:000793 000000            DC      0                                 ; Failed on packet type
2711 d                               PSIZE_ERRORS
2712 d    X:00002E P:000794 000000            DC      0                                 ; Failed on packet size test
2713   
2715   
2716 d    X:00002F P:000795 000000  BLOCK_SIZE DC     0
2717 d    X:000030 P:000796 000000  BURST_SIZE DC     0
2718 d                               BURST_DEST_LO
2719 d    X:000031 P:000797 000000            DC      0
2720 d                               BURST_DEST_HI
2721 d    X:000032 P:000798 000000            DC      0
2722 d                               BURST_SRC_LO
2723 d    X:000033 P:000799 000000            DC      0
2724 d                               BURST_SRC_HI
2725 d    X:000034 P:00079A 000000            DC      0
2726 d    X:000035 P:00079B 000000  YMEM_SRC  DC      0
2727 d    X:000036 P:00079C 000000  YMEM_DEST DC      0
2728   
2729 d    X:000037 P:00079D 000000  DMA_ERRORS DC     0
2730 d    X:000038 P:00079E 000000  EC_TRTY   DC      0
2731 d    X:000039 P:00079F 000000  EC_TO     DC      0
2732 d    X:00003A P:0007A0 000000  EC_TDIS   DC      0
2733 d    X:00003B P:0007A1 000000  EC_TAB    DC      0
2734 d    X:00003C P:0007A2 000000  EC_MAB    DC      0
2735 d    X:00003D P:0007A3 000000  EC_DPER   DC      0
2736 d    X:00003E P:0007A4 000000  EC_APER   DC      0
2737   
2738   
2740   
2741 d    X:00003F P:0007A5 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2742 d    X:000040 P:0007A6 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2743 d                               QT_BUF_SIZE
2744 d    X:000041 P:0007A7 000000            DC      0                                 ; Separation of buffers, in bytes
2745 d    X:000042 P:0007A8 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2746 d                               QT_FRAME_SIZE
2747 d    X:000043 P:0007A9 000000            DC      0                                 ; Expected data packet size, in bytes
2748 d    X:000044 P:0007AA 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2749   
2750 d                               QT_BUF_HEAD
2751 d    X:000045 P:0007AB 000000            DC      0                                 ; Index of buf for next write
2752 d                               QT_BUF_TAIL
2753 d    X:000046 P:0007AC 000000            DC      0                                 ; Index at which we must not write
2754   
2755 d    X:000047 P:0007AD 000000  QT_DEST_LO DC     0                                 ; PC address for next write
2756 d    X:000048 P:0007AE 000000  QT_DEST_HI DC     0                                 ;
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  vars.asm  Page 56



2757 d                               QT_INFORM_IDX
2758 d    X:000049 P:0007AF 000000            DC      0                                 ; Number of packets since last inform
2759 d    X:00004A P:0007B0 000000  QT_DROPS  DC      0                                 ; Dropped packets
2760   
2761   
2763 d    X:00004B P:0007B1 000000  RP_BASE_LO DC     0
2764 d    X:00004C P:0007B2 000000  RP_BASE_HI DC     0
2765 d                               RP_MAX_SIZE
2766 d    X:00004D P:0007B3 000000            DC      0
2767 d    X:00004E P:0007B4 000000  RP_DROPS  DC      0
2768   
2770 d    X:00004F P:0007B5 000000  CON_SRC_LO DC     0
2771 d    X:000050 P:0007B6 000000  CON_SRC_HI DC     0
2772   
2774 d                               PCI_BURST_SIZE
2775 d    X:000051 P:0007B7 000040            DC      $40                               ; Should be < 4*latency assigned by OS
2776   
2778 d                               TIMER_INDEX
2779 d    X:000052 P:0007B8 000000            DC      0
2780   
2782   
2783 d    X:000053 P:0007B9 000000  BDEBUG0   DC      0
2784 d    X:000054 P:0007BA 000000  BDEBUG1   DC      0
2785 d    X:000055 P:0007BB 000000  BDEBUG2   DC      0
2786 d    X:000056 P:0007BC 000000  BDEBUG3   DC      0
2787 d    X:000057 P:0007BD 000000  BDEBUG4   DC      0
2788 d    X:000058 P:0007BE 000000  BDEBUG5   DC      0
2789 d    X:000059 P:0007BF 000000  BDEBUG6   DC      0
2790 d    X:00005A P:0007C0 000000  BDEBUG7   DC      0
2791 d    X:00005B P:0007C1 000000  BDEBUG8   DC      0
2792 d    X:00005C P:0007C2 000000  BDEBUG9   DC      0
2793   
2794                                ;----------------------------------------------------------
2795   
2797   
2798                                 APPLICATION_RUNNING
2799      000000                              EQU     0                                 ; Indicates application is in progress
2800                                 SEND_TO_HOST
2801      000001                              EQU     1                                 ; set in HST ISR when host ready for packet 
(stays set until after HST reply)
2802                                 FATAL_ERROR
2803      000002                              EQU     2                                 ; PCI message to host error detected by driv
er....
2804      000003                    FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays s
et till packet processed
2805   
2806                                ; PREAMBLE_ERROR                EQU     6   ; set if preamble error detected
2807                                ; DATA_DLY              EQU     7   ; set in CON ISR if MCE command is 'GO'.  USed to add delay 
to first returned data packet
2808   
2809      000009                    HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of p
acket (stays set until after HST reply)
2810   
2811      00000A                    CON_DEMAND EQU    10                                ; Host has requested an MCE command be sent
2812      00000B                    CON_MCE   EQU     11                                ; Command has been copied and we should send
 it to the MCE
2813   
2814                                 PCIDMA_RESTART
2815      000010                              EQU     16                                ; DMA flags used for error recovery
2816                                 PCIDMA_RESUME
2817      000011                              EQU     17
2818                                 PCIDMA_RETRY
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  vars.asm  Page 57



2819      000012                              EQU     18
2820   
2821      000014                    QT_FLUSH  EQU     20                                ; Set when it is time to inform Host of curr
ent buffer position.
2822                                 RP_BUFFER_FULL
2823      000015                              EQU     21                                ; Set when Quiet RP buffer is occupied.
2824   
2825      000016                    FREEZER   EQU     22                                ; Suspend operations and just idle in the ma
in loop
2826                                 MAIN_LOOP_POLL
2827      000017                              EQU     23                                ; Cleared by the main loop, use to check for
 DSP lock-up
2828   
2830   
2831                                 MODE_APPLICATION
2832      000000                              EQU     0                                 ; set if PCI application to run
2833      000001                    MODE_MCE  EQU     1                                 ; process packets from MCE
2834      000002                    MODE_QT   EQU     2                                 ; Quiet transfer for data packets
2835                                 MODE_RP_BUFFER
2836      000003                              EQU     3                                 ; Quiet transfer for reply packets
2837      000004                    MODE_NOIRQ EQU    4                                 ; Disbale PCI interrupts on NFY
2838                                 MODE_HANDSHAKE
2839      000005                              EQU     5                                 ; Enable IRQ hand-shaking
2840   
2841                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2842                                 VAR_TBL_END
2843      0007C1                              EQU     @LCV(L)-2
2844                                          ENDIF
2845   
2846                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2848                                          ENDIF
2849   
2850                                 VAR_TBL_LENGTH
2851      00005D                              EQU     VAR_TBL_END-VAR_TBL_START
2852                                          INCLUDE 'app.asm'
2853                                        COMMENT *
2854   
2855                                Auxiliary application area.
2856   
2857                                See info.asm for versioning and authors.
2858   
2859                                        *
2860                                          PAGE    132                               ; Printronix page width - 132 columns
2861                                          OPT     CEX                               ; print DC evaluations
2862   
2863                                          IF      @CVS(N,*)>=APPLICATION
2865                                          ENDIF
2866   
2867   
2868                                ;--------------------------------------------
2869                                ; APPLICATION AREA
2870                                ;---------------------------------------------
2871                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2872      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2873                                          ENDIF
2874   
2875                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2877                                          ENDIF
2878   
2879                                ; starts with no application loaded
2880                                ; so just reply with an error if we get a GOA command
2881   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-02  16:05:04  app.asm  Page 58



2882      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2883      P:000802 P:000804 440B00            MOVE              X0,X:<DTXS_WD1          ; REPly
2884      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2885      P:000805 P:000807 440C00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2886      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2887      P:000808 P:00080A 440D00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2888      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2889      P:00080B P:00080D 440E00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2890      P:00080C P:00080E 0D057F            JSR     <RESTORE_REGISTERS
2891      P:00080D P:00080F 0D0540            JSR     <PCI_MESSAGE_TO_HOST
2892      P:00080E P:000810 0A0100            BCLR    #MODE_APPLICATION,X:<MODE
2893      P:00080F P:000811 0C016E            JMP     PACKET_IN
2894   
2895   
2896      000812                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM
2897   
2898   
2899   

0    Errors
0    Warnings


