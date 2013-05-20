Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  build.asm  Page 1



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
27                         Version:     U0106
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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  header.asm  Page 2



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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  header.asm  Page 3



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
149       000003           DCTR_HF3  EQU     3                                 ; Semaphore for INTA handshaking
150       000004           DCTR_HF4  EQU     4                                 ;
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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  header.asm  Page 4



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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  header.asm  Page 5



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
285  d    P:000000 P:000000 0009FA            DC      END_ADR-INIT-2                    ; Number of boot words
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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  init.asm  Page 6



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
312       P:000014 P:000016 0BF080            JSR     SYSTEM_RESET                      ; $14 - Software reset switch
                            000434
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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  init.asm  Page 7



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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  init.asm  Page 8



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
                            000349
393       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            00031F
394       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            00036A
395       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            000373
396                                 ; software reset is the same as cleaning up the PCI - use same routine
397                                 ; when HOST does a RESET then this routine is run
398       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            00044B
399       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            00045C
400       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            00043C
401       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            00037E
402    
403                                 ; QT - set command
404       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            00039C
405       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            000434
406    
407                                 ; Quiet RP mode, clear buffer full flag
408       P:00008C P:00008E 0A0015            BCLR    #RP_BUFFER_FULL,X:<STATUS         ; $8C
409       P:00008D P:00008F 000000            NOP
410    
411                                 ; ***********************************************************************
412                                 ; For now have boot code starting from P:$100
413                                 ; just to make debugging tidier etc.
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  init.asm  Page 9



414    
415       P:000100 P:000102                   ORG     P:$100,P:$102
416    
417                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
418                                 ; command converter
419                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
421                                           ENDIF
422                                 ; ***********************************************************************
423    
424    
425    
426                                 ; ******************************************************************
427                                 ;
428                                 ;       AA0 = RDFIFO* of incoming fiber optic data
429                                 ;       AA1 = EEPROM access
430                                 ;       AA2 = DRAM access
431                                 ;       AA3 = output to parallel data connector, for a video pixel clock
432                                 ;       $FFxxxx = Write to fiber optic transmitter
433                                 ;
434                                 ; ******************************************************************
435    
436    
437       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
438       P:000102 P:000104 08F485            MOVEP             #>$100000,X:DCTR        ; Set PCI mode
                            100000
439       P:000104 P:000106 000000            NOP
440       P:000105 P:000107 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
441       P:000106 P:000108 000000            NOP
442       P:000107 P:000109 000000            NOP                                       ; End of PCI programming
443    
444    
445                                 ; Set operation mode register OMR to normal expanded
446       P:000108 P:00010A 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
447       P:000109 P:00010B 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
448    
449                                 ; Program the serial port ESSI0 = Port C for serial transmission to
450                                 ;   the timing board
451       P:00010A P:00010C 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
452                                 ;**********************************************************************
453       P:00010C P:00010E 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
454                                                                                     ; DC0-CD4 = 0 for non-network operation
455                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
456                                                                                     ; SSC1 = 0 for SC1 not used
457                                 ;************************************************************************
458       P:00010E P:000110 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
                            010120
459                                                                                     ; SHFD = 0 for MSB shifted first
460                                                                                     ; CKP = 0 for rising clock edge transitions
461                                                                                     ; TE0 = 1 to enable transmitter #0
462                                                                                     ; MOD = 0 for normal, non-networked mode
463                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
464       P:000110 P:000112 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
465                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
466                                 ;********************************************************************************
467       P:000112 P:000114 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
468       P:000114 P:000116 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  init.asm  Page 10



                            000000
469                                 ;***********************************************************************************
470                                 ; 250MHz
471                                 ; Conversion from software bits to schematic labels for Port C and D
472                                 ;       PC0 = SC00 = AUX3               PD0 = SC10 = EF*
473                                 ;       PC1 = SC01 = A/B* = input       PD1 = SC11 = HF*
474                                 ;       PC2 = SC02 = No connect         PD2 = SC12 = RS*
475                                 ;       PC3 = SCK0 = No connect         PD3 = SCK1 = NWRFIFO*
476                                 ;       PC4 = SRD0 = AUX1               PD4 = SRD1 = No connect (** in 50Mhz this was MODE selec
t for 16 or 32 bit FO)
477                                 ;       PC5 = STD0 = No connect         PD5 = STD1 = WRFIFO*
478                                 ; ***********************************************************************************
479    
480    
481                                 ; ****************************************************************************
482                                 ; Program the serial port ESSI1 = Port D for general purpose I/O (GPIO)
483    
484       P:000116 P:000118 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
485       P:000118 P:00011A 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            00001C
486       P:00011A P:00011C 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
487       P:00011C P:00011E 060AA0            REP     #10
488       P:00011D P:00011F 000000            NOP
489       P:00011E P:000120 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
490                                                                                     ; was %011100
491    
492                                 ; Program the SCI port to benign values
493       P:000120 P:000122 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
494       P:000122 P:000124 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
495       P:000124 P:000126 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
496                                 ;       PE0 = RXD
497                                 ;       PE1 = TXD
498                                 ;       PE2 = SCLK
499    
500                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
501       P:000126 P:000128 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
502       P:000128 P:00012A 07F40B            MOVEP             #$2800,X:TCSR1
                            002800
503       P:00012A P:00012C 07F407            MOVEP             #$2800,X:TCSR2
                            002800
504    
505    
506                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
507       P:00012C P:00012E 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
508       P:00012E P:000130 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
e
                            008929
509       P:000130 P:000132 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
                            000122
510    
511    
512                                 ; Program the DRAM memory access and addressing
513       P:000132 P:000134 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  init.asm  Page 11



                            020022
514       P:000134 P:000136 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
515    
516    
517                                 ; Clear all PCI error conditions
518       P:000136 P:000138 084E0A            MOVEP             X:DPSR,A
519       P:000137 P:000139 0140C2            OR      #$1FE,A
                            0001FE
520       P:000139 P:00013B 000000            NOP
521       P:00013A P:00013C 08CE0A            MOVEP             A,X:DPSR
522    
523                                 ; Status word and interrupt configuration.
524       P:00013B P:00013D 08F4BF            MOVEP             #>MY_IPRC,X:IPRC
                            0001C0
525       P:00013D P:00013F 08F4BE            MOVEP             #>MY_IPRP,X:IPRP
                            000002
526       P:00013F P:000141 05F439            MOVE              #>MY_SR,SR
                            000100
527    
528    
529                                 ;--------------------------------------------------------------------------
530                                 ; Initialize the fiber optic serial transmitter to zero
531       P:000141 P:000143 01B786            JCLR    #TDE,X:SSISR0,*
                            000141
532       P:000143 P:000145 07F43C            MOVEP             #$000000,X:TX00
                            000000
533    
534                                 ;--------------------------------------------------------------------
535    
536                                 ; clear DTXM - PCI master transmitter
537       P:000145 P:000147 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
538       P:000146 P:000148 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000146
539    
540                                 ;----------------------------------------------------------------------
541                                 ; clear DRXR - PCI receiver
542    
543       P:000148 P:00014A 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014D
544       P:00014A P:00014C 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
545       P:00014B P:00014D 000000            NOP
546       P:00014C P:00014E 0C0148            JMP     <CLR0
547                                 CLR1
548    
549                                 ;-----------------------------------------------------------------------------
550                                 ; copy parameter table from P memory into X memory
551    
552                                 ; but not frame_count and num_dumped - don't want these reset by fatal error...
553                                 ; they will be reset by new packet or pci_reset ISR
554    
555       P:00014D P:00014F 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000006
556       P:00014F P:000151 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
                            000002
557    
558                                 ; Move the table of constants from P: space to X: space
559       P:000151 P:000153 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            0006AC
560       P:000153 P:000155 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
561       P:000154 P:000156 069680            DO      #VAR_TBL_LENGTH,X_WRITE
                            000157
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  init.asm  Page 12



562       P:000156 P:000158 07D984            MOVE              P:(R1)+,X0
563       P:000157 P:000159 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
564                                 X_WRITE
565    
566       P:000158 P:00015A 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
                            000006
567       P:00015A P:00015C 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000002
568    
569    
570                                 ;----------------------------------------------------------------------------
571                                 ; Initialize PCI controller again, after booting, to make sure it sticks
572    
573       P:00015C P:00015E 08F485            MOVEP             #>$000000,X:DCTR
                            000000
574       P:00015E P:000160 000000            NOP
575       P:00015F P:000161 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00015F
576       P:000161 P:000163 08F485            MOVEP             #>$100000,X:DCTR
                            100000
577       P:000163 P:000165 000000            NOP
578       P:000164 P:000166 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000164
579    
580       
581       P:000166 P:000168 0BF080            JSR     CLEAR_FO_FIFO                     ; Clear the fibre fifo!
                            0004B4
582       P:000168 P:00016A 013D24            BSET    #AUX1,X:PDRC                      ; Enable byte-swapping - still necc. on ARC-
64
583       P:000169 P:00016B 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host interrupts
584       P:00016A P:00016C 0BF080            JSR     TIMER_DEFAULT                     ; Enable timer (channel 0) for misc. uses
                            00062E
585       P:00016C P:00016E 0BF080            JSR     TIMER_STORE_INIT                  ; Initialize timing buffer
                            000646
586    
588                                           INCLUDE 'main.asm'
589    
590                                                 COMMENT *
591    
592                                 Main section of the pci card code.
593    
594                                 See info.asm for versioning and authors.
595    
596                                         *
597                                           PAGE    132                               ; Printronix page width - 132 columns
598                                           OPT     CEX                               ; print DC evaluations
599    
600    
601    
605    
606                                 PACKET_IN
607    
608       
609       P:00016E P:000170 0A0017            BCLR    #MAIN_LOOP_POLL,X:<STATUS
610    
611       
612       P:00016F P:000171 0A00B6            JSET    #FREEZER,X:<STATUS,PACKET_IN
                            00016E
613    
614       
615       P:000171 P:000173 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 13



616    
617       
618       P:000173 P:000175 0A01A0            JSET    #MODE_APPLICATION,X:<MODE,APPLICATION
                            000800
619    
620       
621       P:000175 P:000177 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            000636
622    
623       
624       P:000177 P:000179 0B00B4            JSSET   #QT_FLUSH,X:STATUS,BUFFER_INFORM
                            000682
625    
626       
627       P:000179 P:00017B 0D0470            JSR     <CHECK_FO
628       P:00017A P:00017C 0B00A3            JSSET   #FO_WRD_RCV,X:STATUS,HANDLE_FIFO
                            00018A
629    
630       
631       P:00017C P:00017E 0B00AB            JSSET   #CON_MCE,X:STATUS,CON_TRANSMIT
                            000288
632       P:00017E P:000180 0B00AA            JSSET   #CON_DEMAND,X:STATUS,CON_BUFFER
                            00026B
633    
634       
635       P:000180 P:000182 0BF080            JSR     HACK_ENTRY
                            000810
636    
637       
638       P:000182 P:000184 0C016E            JMP     PACKET_IN
639    
643    
644                                 ; PCI semaphore
645                                 ;
646                                 ; In order for routines in non-interrupt context to write to the
647                                 ; DTXS, (via PCI_MESSAGE_TO_HOST) they need to end up with
648                                 ; interrupts disabled and HCF3 cleared.
649                                 ;
650                                 ; Non-interrupt PCIers should use macro
651                                 ;       PCI_LOCKDOWN
652                                 ; to get exclusive access and then release it with
653                                 ;       PCI_LOCKUP
654                                 ; after calling PCI_MESSAGE_TO_HOST.
655    
656                                  PCI_LOCKDOWN
657                                           MACRO
658  m                                        JSR     PCI_LOCKDOWN_ENTRY
659  m                                        ENDM
660    
661                                 PCI_LOCKUP MACRO
662  m                                        BSET    #DCTR_HCIE,X:DCTR                 ; Enable host interrupts
663  m                                        ENDM
664    
665    
666                                 PCI_LOCKDOWN_AGAIN
667       P:000183 P:000185 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Re-enable host IRQ
668       P:000184 P:000186 0632A0            REP     #50                               ; Delay for ~us
669       P:000185 P:000187 000000            NOP
670    
671                                 PCI_LOCKDOWN_ENTRY
672       
673       P:000186 P:000188 0A8500            BCLR    #DCTR_HCIE,X:DCTR                 ; Disable host IRQ
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 14



674       P:000187 P:000189 0A85A3            JSET    #DCTR_HF3,X:DCTR,PCI_LOCKDOWN_AGAIN
                            000183
675       P:000189 P:00018B 00000C            RTS
676    
677    
679    
680                                 HANDLE_FIFO
681       P:00018A P:00018C 54F400            MOVE              #>$A00,A1
                            000A00
682       P:00018C P:00018E 0BF080            JSR     TIMER_STORE_A1
                            00064F
683       P:00018E P:000190 0BF080            JSR     TIMER_STORE
                            00064D
684    
685       
686       P:000190 P:000192 60F400            MOVE              #>HEAD_W1_0,R0
                            00001B
687       P:000192 P:000194 44F400            MOVE              #>$00FFFF,X0            ; Mask lower 16 bits
                            00FFFF
688       P:000194 P:000196 220800            MOVE              R0,A0
689       P:000195 P:000197 060880            DO      #8,HANDLE_FIFO_CHECK_PREAMBLE
                            0001A0
690                                 HANDLE_FIFO_WAIT
691       P:000197 P:000199 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000197
692       P:000199 P:00019B 000000            NOP
693       P:00019A P:00019C 000000            NOP
694       P:00019B P:00019D 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000197
695       P:00019D P:00019F 094E3F            MOVEP             Y:RDFIFO,A
696       P:00019E P:0001A0 200046            AND     X0,A
697       P:00019F P:0001A1 000000            NOP
698       P:0001A0 P:0001A2 545800            MOVE              A1,X:(R0)+
699    
700                                 HANDLE_FIFO_CHECK_PREAMBLE
701       P:0001A1 P:0001A3 60F400            MOVE              #>HEAD_W1_0,R0
                            00001B
702       P:0001A3 P:0001A5 20001B            CLR     B
703       P:0001A4 P:0001A6 200013            CLR     A
704       P:0001A5 P:0001A7 57D800            MOVE              X:(R0)+,B
705       P:0001A6 P:0001A8 0140CD            CMP     #>$A5A5,B
                            00A5A5
706       P:0001A8 P:0001AA 0AF0A2            JNE     PRE_ERROR
                            0001CF
707       P:0001AA P:0001AC 57D800            MOVE              X:(R0)+,B
708       P:0001AB P:0001AD 0140CD            CMP     #>$A5A5,B
                            00A5A5
709       P:0001AD P:0001AF 0AF0A2            JNE     PRE_ERROR
                            0001CF
710       P:0001AF P:0001B1 57D800            MOVE              X:(R0)+,B
711       P:0001B0 P:0001B2 0140CD            CMP     #>$5A5A,B
                            005A5A
712       P:0001B2 P:0001B4 0AF0A2            JNE     PRE_ERROR
                            0001CF
713       P:0001B4 P:0001B6 57D800            MOVE              X:(R0)+,B
714       P:0001B5 P:0001B7 0140CD            CMP     #>$5A5A,B
                            005A5A
715       P:0001B7 P:0001B9 0AF0A2            JNE     PRE_ERROR
                            0001CF
716    
717       
718       P:0001B9 P:0001BB 50F000            MOVE              X:>(HEAD_W1_0+6),A0
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 15



                            000021
719       P:0001BB P:0001BD 44F000            MOVE              X:>(HEAD_W1_0+7),X0
                            000022
720       P:0001BD P:0001BF 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
721    
722       
723       P:0001BF P:0001C1 0BF080            JSR     PACKET_PARTITIONS
                            0005BE
724       P:0001C1 P:0001C3 0BF080            JSR     TIMER_STORE
                            00064D
725    
727       P:0001C3 P:0001C5 56F000            MOVE              X:HEAD_W3_0,A
                            00001F
728    
729       P:0001C5 P:0001C7 0140C5            CMP     #>'RP',A
                            005250
730       P:0001C7 P:0001C9 0AF0AA            JEQ     HANDLE_RP
                            0001E3
731    
732       P:0001C9 P:0001CB 0140C5            CMP     #>'DA',A
                            004441
733       P:0001CB P:0001CD 0AF0AA            JEQ     HANDLE_DA
                            00022A
734    
735       P:0001CD P:0001CF 0AF080            JMP     QT_PTYPE_ERROR
                            0001D5
736    
737                                 ; Error recording.
738    
739                                 PRE_ERROR
740       P:0001CF P:0001D1 60F400            MOVE              #>PREAMBLE_ERRORS,R0
                            000026
741       P:0001D1 P:0001D3 0BF080            JSR     INCR_X_R0
                            0001DE
742       P:0001D3 P:0001D5 0AF080            JMP     CLEAR_FO_FIFO                     ; empty the fifo (2 ms!)
                            0004B4
743    
744                                 QT_PTYPE_ERROR
745       P:0001D5 P:0001D7 60F400            MOVE              #>PTYPE_ERRORS,R0
                            000027
746       P:0001D7 P:0001D9 0AF080            JMP     INCR_X_R0
                            0001DE
747                                 QT_FSIZE_ERROR
748       P:0001D9 P:0001DB 60F400            MOVE              #>PSIZE_ERRORS,R0
                            000028
749       P:0001DB P:0001DD 0AF080            JMP     INCR_X_R0
                            0001DE
750                                 RETURN_NOW
751       P:0001DD P:0001DF 00000C            RTS
752    
753                                 INCR_X_R0
754       
755       P:0001DE P:0001E0 50E000            MOVE              X:(R0),A0
756       P:0001DF P:0001E1 000008            INC     A
757       P:0001E0 P:0001E2 000000            NOP
758       P:0001E1 P:0001E3 506000            MOVE              A0,X:(R0)
759       P:0001E2 P:0001E4 00000C            RTS
760    
761    
762    
765    
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 16



766                                 HANDLE_RP
767       
768       P:0001E3 P:0001E5 0A0183            JCLR    #MODE_RP_BUFFER,X:MODE,MCE_PACKET
                            0002A8
769    
770       
771       P:0001E5 P:0001E7 0A00B5            JSET    #RP_BUFFER_FULL,X:STATUS,HANDLE_RP_DROP
                            000225
772    
773       
774       P:0001E7 P:0001E9 61F400            MOVE              #>REPLY_BUFFER,R1
                            100000
775       P:0001E9 P:0001EB 0BF080            JSR     BUFFER_PACKET
                            0005CB
776    
777       P:0001EB P:0001ED 54F400            MOVE              #>$b00,A1
                            000B00
778       P:0001ED P:0001EF 0BF080            JSR     TIMER_STORE_A1
                            00064F
779       P:0001EF P:0001F1 0BF080            JSR     TIMER_STORE
                            00064D
780    
781       
782       P:0001F1 P:0001F3 60F400            MOVE              #RP_BASE_LO,R0
                            000048
783       P:0001F3 P:0001F5 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
784    
785       P:0001F5 P:0001F7 60F400            MOVE              #BURST_DEST_LO,R0
                            00002F
786       P:0001F7 P:0001F9 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
787    
788       
789       P:0001F9 P:0001FB 200013            CLR     A
790       P:0001FA P:0001FC 20001B            CLR     B
791       P:0001FB P:0001FD 50F000            MOVE              X:PACKET_SIZE,A0
                            000023
792       P:0001FD P:0001FF 0C1D04            ASL     #2,A,A                            ; Size in bytes
793       P:0001FE P:000200 51F000            MOVE              X:RP_MAX_SIZE,B0
                            00004A
794    
795       P:000200 P:000202 200005            CMP     B,A                               ; A ? B
796       P:000201 P:000203 0AF0AF            JLE     HANDLE_RP1
                            000204
797       P:000203 P:000205 21EE00            MOVE              B,A
798    
799                                 HANDLE_RP1
800       
801       P:000204 P:000206 44F400            MOVE              #>REPLY_BUFFER,X0
                            100000
802       P:000206 P:000208 507000            MOVE              A0,X:BLOCK_SIZE
                            00002B
803       P:000208 P:00020A 447000            MOVE              X0,X:YMEM_SRC
                            00002E
804       P:00020A P:00020C 0BF080            JSR     TIMER_STORE
                            00064D
805       P:00020C P:00020E 0BF080            JSR     BLOCK_TRANSFER
                            000518
806       P:00020E P:000210 0BF080            JSR     TIMER_STORE
                            00064D
807    
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 17



808       
809                                           PCI_LOCKDOWN                              ; Disable host IRQ
811       P:000211 P:000213 44F400            MOVE              #'NFY',X0
                            4E4659
812       P:000213 P:000215 447000            MOVE              X0,X:DTXS_WD1
                            00000B
813       P:000215 P:000217 44F400            MOVE              #'RPQ',X0
                            525051
814       P:000217 P:000219 447000            MOVE              X0,X:DTXS_WD2
                            00000C
815       P:000219 P:00021B 507000            MOVE              A0,X:DTXS_WD3           ; A0=block_size
                            00000D
816       P:00021B P:00021D 547000            MOVE              A1,X:DTXS_WD4           ; A1=0
                            00000E
817    
818       
819       P:00021D P:00021F 0A7035            BSET    #RP_BUFFER_FULL,X:STATUS
                            000000
820       P:00021F P:000221 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
821                                           PCI_LOCKUP                                ; Enable host IRQ
823    
824       P:000222 P:000224 0BF080            JSR     TIMER_STORE
                            00064D
825       P:000224 P:000226 00000C            RTS                                       ; Back to main loop
826    
827                                 HANDLE_RP_DROP
828       P:000225 P:000227 60F400            MOVE              #RP_DROPS,R0
                            00004B
829       P:000227 P:000229 0D01DE            JSR     INCR_X_R0
830       P:000228 P:00022A 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            00060E
831    
833    
834    
837    
838    
839                                 HANDLE_DA
840       
841       P:00022A P:00022C 60F400            MOVE              #FRAME_COUNT,R0
                            000002
842       P:00022C P:00022E 0D01DE            JSR     INCR_X_R0
843    
844       
845       P:00022D P:00022F 0A0182            JCLR    #MODE_QT,X:MODE,MCE_PACKET
                            0002A8
846    
847       
848       P:00022F P:000231 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
849       P:000231 P:000233 0BF080            JSR     BUFFER_PACKET
                            0005CB
850    
851       P:000233 P:000235 54F400            MOVE              #$e00,A1
                            000E00
852       P:000235 P:000237 0BF080            JSR     TIMER_STORE_A1
                            00064F
853       P:000237 P:000239 0BF080            JSR     TIMER_STORE
                            00064D
854    
855       
856       P:000239 P:00023B 56F000            MOVE              X:QT_BUF_HEAD,A
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 18



                            000042
857       P:00023B P:00023D 014180            ADD     #1,A
858       P:00023C P:00023E 57F000            MOVE              X:QT_BUF_MAX,B
                            00003F
859       P:00023E P:000240 20000D            CMP     A,B
860       P:00023F P:000241 0AF0A1            JGE     HANDLE_DA_MATH
                            000242
861       P:000241 P:000243 2E0000            MOVE              #0,A
862                                 HANDLE_DA_MATH
863       P:000242 P:000244 57F000            MOVE              X:QT_BUF_TAIL,B
                            000043
864       P:000244 P:000246 20000D            CMP     A,B
865       P:000245 P:000247 0AF0AA            JEQ     HANDLE_DA_DROP
                            000266
866    
867       
868       P:000247 P:000249 200013            CLR     A
869       P:000248 P:00024A 50F000            MOVE              X:PACKET_SIZE,A0
                            000023
870    
871       P:00024A P:00024C 014088            ADD     #0,B                              ; Clear carry
872       P:00024B P:00024D 0C1D04            ASL     #2,A,A                            ; Size, in bytes
873    
874       
875       P:00024C P:00024E 20001B            CLR     B
876       P:00024D P:00024F 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            000040
877       P:00024F P:000251 20000D            CMP     A,B
878       P:000250 P:000252 0E21D9            JNE     QT_FSIZE_ERROR
879    
880       
881       P:000251 P:000253 517000            MOVE              B0,X:BLOCK_SIZE
                            00002B
882       P:000253 P:000255 557000            MOVE              B1,X:YMEM_SRC           ; Y:0
                            00002E
883    
884       P:000255 P:000257 60F400            MOVE              #QT_DEST_LO,R0
                            000044
885       P:000257 P:000259 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
886       P:000259 P:00025B 60F400            MOVE              #BURST_DEST_LO,R0
                            00002F
887       P:00025B P:00025D 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
888    
889       
890       P:00025D P:00025F 0BF080            JSR     BLOCK_TRANSFER
                            000518
891    
892       P:00025F P:000261 0BF080            JSR     TIMER_STORE
                            00064D
893    
894       
895       P:000261 P:000263 0BF080            JSR     BUFFER_INCR
                            000657
896    
897       
898       P:000263 P:000265 0BF080            JSR     BUFFER_INFORM_CHECK
                            000675
899    
900       P:000265 P:000267 00000C            RTS
901    
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 19



902                                 HANDLE_DA_DROP
903       
904       P:000266 P:000268 60F400            MOVE              #QT_DROPS,R0
                            000047
905       P:000268 P:00026A 0D01DE            JSR     INCR_X_R0
906       P:000269 P:00026B 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            00060E
907    
909    
910    
911                                 ;----------------------------------------------
912                                 CON_BUFFER
913                                 ; This routine will copy an MCE command from the PC to Y memory.
914                                 ; The source RAM address has already been stored in CON_SRC_LO.
915                                 ; The destination address is always Y:COMMAND_BUFFER.
916                                 ;----------------------------------------------
917    
918       P:00026B P:00026D 54F400            MOVE              #>$C00,A1
                            000C00
919       P:00026D P:00026F 0BF080            JSR     TIMER_STORE_A1
                            00064F
920       P:00026F P:000271 0BF080            JSR     TIMER_STORE
                            00064D
921    
922       
923       P:000271 P:000273 60F400            MOVE              #>CON_SRC_LO,R0
                            00002C
924       P:000273 P:000275 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
925       P:000275 P:000277 60F400            MOVE              #>BURST_SRC_LO,R0
                            000031
926       P:000277 P:000279 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
927       P:000279 P:00027B 51F400            MOVE              #>COMMAND_BUFFER,B0
                            200000
928       P:00027B P:00027D 50F400            MOVE              #>256,A0
                            000100
929       P:00027D P:00027F 517000            MOVE              B0,X:YMEM_DEST
                            000033
930       P:00027F P:000281 507000            MOVE              A0,X:BLOCK_SIZE
                            00002B
931       P:000281 P:000283 0BF080            JSR     CON_TRANSFER
                            000552
932    
933       P:000283 P:000285 0A702B            BSET    #CON_MCE,X:STATUS
                            000000
934       P:000285 P:000287 0BF080            JSR     TIMER_STORE
                            00064D
935       P:000287 P:000289 00000C            RTS                                       ; Back to main loop
936    
937                                 ;----------------------------------------------
938                                 CON_TRANSMIT
939                                 ; This routine will copy the MCE command from Y:COMMAND_BUFFER to
940                                 ; the MCE command transmitter.
941                                 ;----------------------------------------------
942    
943       P:000288 P:00028A 0BF080            JSR     TIMER_STORE
                            00064D
944    
945       P:00028A P:00028C 66F400            MOVE              #>COMMAND_BUFFER,R6
                            200000
946       P:00028C P:00028E 068080            DO      #128,CON_TRANSMIT1                ; block size = 16bit x 128 (256 bytes)
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 20



                            000295
947       P:00028E P:000290 5CDE00            MOVE                          Y:(R6)+,A1  ; b2, b1  (lsb)
948       P:00028F P:000291 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
949       P:000290 P:000292 0140C6            AND     #>$FF,A
                            0000FF
950       P:000292 P:000294 547000            MOVE              A1,X:FO_SEND
                            FFF000
951       P:000294 P:000296 557000            MOVE              B1,X:FO_SEND
                            FFF000
952    
953                                 CON_TRANSMIT1
954       P:000296 P:000298 0A0121            BSET    #MODE_MCE,X:<MODE                 ; enable processing of MCE replies/data
955    
956       
957       P:000297 P:000299 0A700B            BCLR    #CON_MCE,X:STATUS
                            000000
958       P:000299 P:00029B 0A700A            BCLR    #CON_DEMAND,X:STATUS
                            000000
959    
960       P:00029B P:00029D 0BF080            JSR     TIMER_STORE
                            00064D
961    
962       
963                                           PCI_LOCKDOWN
965       P:00029E P:0002A0 44F400            MOVE              #'CON',X0
                            434F4E
966       P:0002A0 P:0002A2 0BF080            JSR     VCOM_PREPARE_REPLY
                            0002E9
967       P:0002A2 P:0002A4 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
968                                           PCI_LOCKUP                                ; Enable host IRQ
970    
971       P:0002A5 P:0002A7 0BF080            JSR     TIMER_STORE
                            00064D
972       P:0002A7 P:0002A9 00000C            RTS                                       ; Back to main loop
973    
974    
975    
976    
978    
979                                 ; --------------------------------------------------------------------------
980                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
981                                 ; --------------------------------------------------------------------------
982    
983                                 ; prepare notify to inform host that a packet has arrived.
984    
985                                 MCE_PACKET
986                                           PCI_LOCKDOWN                              ; Disable host IRQ
988       P:0002A9 P:0002AB 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
989    
990       P:0002AA P:0002AC 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
991       P:0002AC P:0002AE 440B00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
992    
993       P:0002AD P:0002AF 449F00            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
994       P:0002AE P:0002B0 440C00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
995    
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 21



996       P:0002AF P:0002B1 44A100            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
997       P:0002B0 P:0002B2 440D00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
998    
999       P:0002B1 P:0002B3 44A200            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
1000      P:0002B2 P:0002B4 440E00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sage
1001   
1002      
1003      P:0002B3 P:0002B5 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
1004      P:0002B4 P:0002B6 0D047A            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
1005      P:0002B5 P:0002B7 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
1006                                          PCI_LOCKUP
1008   
1009      P:0002B7 P:0002B9 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
1010      P:0002B9 P:0002BB 0BF080            JSR     BUFFER_PACKET
                            0005CB
1011   
1012      
1013   
1014      P:0002BB P:0002BD 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; on fatal error, re-init.
                            000100
1015      P:0002BD P:0002BF 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; Set in 'send_packet_to_host' ISR
                            0002BB
1016   
1017      
1018      P:0002BF P:0002C1 44F400            MOVE              #>IMAGE_BUFFER,X0
                            000000
1019      P:0002C1 P:0002C3 56F000            MOVE              X:PACKET_SIZE,A
                            000023
1020      P:0002C3 P:0002C5 0C1D04            ASL     #2,A,A
1021      P:0002C4 P:0002C6 447000            MOVE              X0,X:YMEM_SRC
                            00002E
1022      P:0002C6 P:0002C8 547000            MOVE              A1,X:BLOCK_SIZE
                            00002B
1023      P:0002C8 P:0002CA 0BF080            JSR     BLOCK_TRANSFER
                            000518
1024   
1025      P:0002CA P:0002CC 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
1026   
1027      
1028                                          PCI_LOCKDOWN                              ; Disable host IRQ
1030      P:0002CD P:0002CF 44F400            MOVE              #'HST',X0
                            485354
1031      P:0002CF P:0002D1 0BF080            JSR     VCOM_PREPARE_REPLY
                            0002E9
1032      P:0002D1 P:0002D3 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1033                                          PCI_LOCKUP                                ; Enable host IRQ
1035      P:0002D4 P:0002D6 00000C            RTS
1036   
1037                                ;----------------------------------------------------------
1038                                ; clear out the fifo after an HST timeout...
1039                                ;----------------------------------------------------------
1040   
1041                                DUMP_FIFO
1042      P:0002D5 P:0002D7 61F400            MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
1043      P:0002D7 P:0002D9 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 22



ifo
                            000200
1044      P:0002D9 P:0002DB 200013            CLR     A
1045      P:0002DA P:0002DC 320000            MOVE              #0,R2                   ; use R2 as a dump count
1046                                NEXT_DUMP
1047      P:0002DB P:0002DD 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E6
1048      P:0002DD P:0002DF 000000            NOP
1049      P:0002DE P:0002E0 000000            NOP
1050      P:0002DF P:0002E1 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E6
1051   
1052      P:0002E1 P:0002E3 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
1053      P:0002E2 P:0002E4 205A00            MOVE              (R2)+                   ; inc dump count
1054      P:0002E3 P:0002E5 224E00            MOVE              R2,A                    ;
1055      P:0002E4 P:0002E6 200045            CMP     X0,A                              ; check we've not hit dump limit
1056      P:0002E5 P:0002E7 0E22DB            JNE     NEXT_DUMP                         ; not hit limit?
1057                                FIFO_EMPTY
1058      P:0002E6 P:0002E8 627000            MOVE              R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000006
1059      P:0002E8 P:0002EA 0C0100            JMP     <START                            ; re-initialise
1060   
1061   
1062                                ; -------------------------------------------------------------------------------------
1063                                ;                              END OF MAIN PACKET HANDLING CODE
1064                                ; -------------------------------------------------------------------------------------
1065   
1066   
1067   
1068                                ; -------------------------------------------------------------------------------------
1069                                ;
1070                                ;                              INTERRUPT SERVICE ROUTINES
1071                                ;
1072                                ; -------------------------------------------------------------------------------------
1073   
1074                                ; ---------------
1075                                ; Rules:  Don't use N#, or any R# except R0 unless you add them to the saved register set.
1076   
1077   
1078                                ; ----------------------------------------------------------------------------
1079                                ; VCOM_* - routines: utility functions for hosty command vector communication.
1080                                ;-----------------------------------------------------------------------------
1081   
1082   
1083                                ; VCOM_PREPARE_REPLY
1084                                ;
1085                                ; Prepare the reply packet, using X0 as the command name (second word).  The
1086                                ; message defaults to 'ACK' with NULL data.  The user may subsequenty fill in
1087                                ; the data field (word 4) and mark the packet as error if necessary.
1088   
1089                                VCOM_PREPARE_REPLY
1090      
1091      
1092      P:0002E9 P:0002EB 50F400            MOVE              #'REP',A0
                            524550
1093      P:0002EB P:0002ED 447000            MOVE              X0,X:DTXS_WD2           ; Command
                            00000C
1094      P:0002ED P:0002EF 507000            MOVE              A0,X:DTXS_WD1
                            00000B
1095   
1096      P:0002EF P:0002F1 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 23



                            41434B
1097      P:0002F1 P:0002F3 000000            NOP
1098      P:0002F2 P:0002F4 547000            MOVE              A1,X:DTXS_WD3           ; ACK
                            00000D
1099      P:0002F4 P:0002F6 507000            MOVE              A0,X:DTXS_WD4           ; no comment
                            00000E
1100      P:0002F6 P:0002F8 00000C            RTS
1101   
1102   
1103                                ; VCOM_CHECK
1104                                ;
1105                                ; Compares DRXR_WD1 to X0.  If they are equal, Z is set on return.  If they
1106                                ; are not equal then Z is cleared and the reply will be marked as ERR with
1107                                ; 'CNE' in the last word.
1108                                ; Trashes A and B always and X0 on error.
1109   
1110                                VCOM_CHECK
1111      P:0002F7 P:0002F9 208E00            MOVE              X0,A
1112      P:0002F8 P:0002FA 57F000            MOVE              X:DRXR_WD1,B
                            000007
1113      P:0002FA P:0002FC 20000D            CMP     A,B
1114      P:0002FB P:0002FD 0AF0AA            JEQ     VCOM_RTS
                            000305
1115   
1116      P:0002FD P:0002FF 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
1117      P:0002FF P:000301 50F400            MOVE              #'ERR',A0
                            455252
1118      P:000301 P:000303 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1119      P:000303 P:000305 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1120                                VCOM_RTS
1121      P:000305 P:000307 00000C            RTS
1122   
1123   
1124                                ; VCOM_INTRO
1125                                ;
1126                                ; Read DSP command from DRXR.  Prepare the reply packet and verify that it
1127                                ; matches the key in X1.  If it does not, mark the reply as error and set
1128                                ; the Z flag.
1129   
1130                                VCOM_INTRO
1131      P:000306 P:000308 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            000318
1132      P:000308 P:00030A 20A400            MOVE              X1,X0
1133      P:000309 P:00030B 0D02E9            JSR     VCOM_PREPARE_REPLY
1134      P:00030A P:00030C 0D02F7            JSR     VCOM_CHECK
1135      P:00030B P:00030D 00000C            RTS
1136   
1137   
1138                                ; VCOM_EXIT_ERROR_X0
1139                                ; VCOM_EXIT_X0
1140                                ; VCOM_EXIT
1141                                ;
1142                                ; For returning from host command vector interrupts only.  These three
1143                                ; routines do the following (respectively):
1144                                ; a) Mark reply as error, then (b)
1145                                ; b) Put X0 into last word of reply, then (c)
1146                                ; c) Restore registers and RTI.
1147   
1148                                VCOM_EXIT_ERROR_X0
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 24



1149      P:00030C P:00030E 50F400            MOVE              #'ERR',A0
                            455252
1150      P:00030E P:000310 000000            NOP
1151      P:00030F P:000311 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1152                                VCOM_EXIT_X0
1153      P:000311 P:000313 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1154                                VCOM_EXIT
1155      P:000313 P:000315 0BF080            JSR     RESTORE_REGISTERS
                            000493
1156      P:000315 P:000317 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1157      P:000317 P:000319 000004            RTI
1158   
1159   
1160                                ;---------------------------------------------------------------
1161                                RD_DRXR
1162                                ;--------------------------------------------------------------
1163                                ; Routine to read from HTXR-DRXR data path.  For HCTR = 0x900,
1164                                ; 3 LSB of each 32-bit word written by the host is returned on
1165                                ; each read.  This only polls for first word, not all of them.
1166      P:000318 P:00031A 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            000318
1167      P:00031A P:00031C 63F400            MOVE              #DRXR_WD1,R3
                            000007
1168      P:00031C P:00031E 0604A0            REP     #4
1169      P:00031D P:00031F 085B8B            MOVEP             X:DRXR,X:(R3)+
1170      P:00031E P:000320 00000C            RTS
1171   
1172   
1173                                ; ----------------------------------------------------------------------------
1174                                READ_MEMORY
1175                                ;-----------------------------------------------------------------------------
1176                                ;Read command:
1177                                ; word 1 = command = 'RDM'
1178                                ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
1179                                ; word 3 = address in memory
1180                                ; word 4 = not used
1181                                ;Version query:
1182                                ; word 1 = 'VER'
1183                                ; word 2-4 unused
1184   
1185      P:00031F P:000321 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1186      P:000321 P:000323 0D0318            JSR     RD_DRXR                           ; Loads DRXR_WD*
1187   
1188      P:000322 P:000324 44F400            MOVE              #'RDM',X0
                            52444D
1189      P:000324 P:000326 0D02E9            JSR     VCOM_PREPARE_REPLY
1190      P:000325 P:000327 0D02F7            JSR     VCOM_CHECK
1191      P:000326 P:000328 0AF0AA            JEQ     READ_MEMORY_XYP
                            000330
1192   
1193      
1194      P:000328 P:00032A 44F400            MOVE              #'VER',X0
                            564552
1195      P:00032A P:00032C 0D02E9            JSR     VCOM_PREPARE_REPLY
1196      P:00032B P:00032D 0D02F7            JSR     VCOM_CHECK
1197      P:00032C P:00032E 0E2313            JNE     VCOM_EXIT
1198   
1199      P:00032D P:00032F 44F000            MOVE              X:REV_NUMBER,X0
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 25



                            000003
1200      P:00032F P:000331 0C0311            JMP     VCOM_EXIT_X0
1201   
1202                                READ_MEMORY_XYP
1203   
1204      
1205      P:000330 P:000332 56F000            MOVE              X:DRXR_WD2,A
                            000008
1206      P:000332 P:000334 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1207   
1208      P:000334 P:000336 0140C5            CMP     #'_X',A
                            005F58
1209      P:000336 P:000338 0AF0AA            JEQ     READ_MEMORY_X
                            000343
1210   
1211      P:000338 P:00033A 0140C5            CMP     #'_Y',A
                            005F59
1212      P:00033A P:00033C 0AF0AA            JEQ     READ_MEMORY_Y
                            000345
1213   
1214      P:00033C P:00033E 0140C5            CMP     #'_P',A
                            005F50
1215      P:00033E P:000340 0AF0AA            JEQ     READ_MEMORY_P
                            000347
1216   
1217      P:000340 P:000342 44F400            MOVE              #'MTE',X0
                            4D5445
1218      P:000342 P:000344 0C030C            JMP     VCOM_EXIT_ERROR_X0
1219   
1220                                READ_MEMORY_X
1221      P:000343 P:000345 44E000            MOVE              X:(R0),X0
1222      P:000344 P:000346 0C0311            JMP     VCOM_EXIT_X0
1223                                READ_MEMORY_Y
1224      P:000345 P:000347 4CE000            MOVE                          Y:(R0),X0
1225      P:000346 P:000348 0C0311            JMP     VCOM_EXIT_X0
1226                                READ_MEMORY_P
1227      P:000347 P:000349 07E084            MOVE              P:(R0),X0
1228      P:000348 P:00034A 0C0311            JMP     VCOM_EXIT_X0
1229   
1230   
1231                                ;--------------------------------------------------------------
1232                                WRITE_MEMORY
1233                                ;---------------------------------------------------------------
1234                                ; word 1 = command = 'WRM'
1235                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1236                                ; word 3 = address in memory
1237                                ; word 4 = value
1238   
1239      P:000349 P:00034B 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1240      P:00034B P:00034D 45F400            MOVE              #'WRM',X1
                            57524D
1241      P:00034D P:00034F 0D0306            JSR     VCOM_INTRO
1242      P:00034E P:000350 0E2313            JNE     VCOM_EXIT
1243   
1244      
1245      P:00034F P:000351 56F000            MOVE              X:DRXR_WD2,A
                            000008
1246      P:000351 P:000353 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1247      P:000353 P:000355 44F000            MOVE              X:DRXR_WD4,X0
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 26



                            00000A
1248   
1249      P:000355 P:000357 0140C5            CMP     #'_X',A
                            005F58
1250      P:000357 P:000359 0AF0AA            JEQ     WRITE_MEMORY_X
                            000364
1251   
1252      P:000359 P:00035B 0140C5            CMP     #'_Y',A
                            005F59
1253      P:00035B P:00035D 0AF0AA            JEQ     WRITE_MEMORY_Y
                            000366
1254   
1255      P:00035D P:00035F 0140C5            CMP     #'_P',A
                            005F50
1256      P:00035F P:000361 0AF0AA            JEQ     WRITE_MEMORY_P
                            000368
1257   
1258      P:000361 P:000363 44F400            MOVE              #'MTE',X0
                            4D5445
1259      P:000363 P:000365 0C030C            JMP     VCOM_EXIT_ERROR_X0
1260   
1261                                WRITE_MEMORY_X
1262      P:000364 P:000366 446000            MOVE              X0,X:(R0)
1263      P:000365 P:000367 0C0311            JMP     VCOM_EXIT_X0
1264                                WRITE_MEMORY_Y
1265      P:000366 P:000368 4C6000            MOVE                          X0,Y:(R0)
1266      P:000367 P:000369 0C0311            JMP     VCOM_EXIT_X0
1267                                WRITE_MEMORY_P
1268      P:000368 P:00036A 076084            MOVE              X0,P:(R0)
1269      P:000369 P:00036B 0C0311            JMP     VCOM_EXIT_X0
1270   
1271   
1272                                ;-----------------------------------------------------------------------------
1273                                START_APPLICATION
1274                                ; an application should already have been downloaded to the PCI memory.
1275                                ; this command will execute it.
1276                                ; ----------------------------------------------------------------------
1277                                ; word 1 = command = 'GOA'
1278                                ; word 2-4 unused
1279   
1280      P:00036A P:00036C 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1281      P:00036C P:00036E 45F400            MOVE              #'GOA',X1
                            474F41
1282   
1283      P:00036E P:000370 0D0306            JSR     VCOM_INTRO
1284      P:00036F P:000371 0E2313            JNE     VCOM_EXIT
1285   
1286      P:000370 P:000372 0A7020            BSET    #MODE_APPLICATION,X:MODE
                            000001
1287      P:000372 P:000374 000004            RTI                                       ; Application will reply.
1288   
1289   
1290                                ; ---------------------------------------------------------
1291                                STOP_APPLICATION
1292                                ; this command stops an application that is currently running
1293                                ; used for applications that once started run contiunually
1294                                ;-----------------------------------------------------------
1295                                ; word 1 = command = ' STP'
1296                                ; word 2-4 unused
1297   
1298      P:000373 P:000375 0BF080            JSR     SAVE_REGISTERS
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 27



                            0004A0
1299      P:000375 P:000377 45F400            MOVE              #'STP',X1
                            535450
1300   
1301      P:000377 P:000379 0D0306            JSR     VCOM_INTRO
1302      P:000378 P:00037A 0E2313            JNE     VCOM_EXIT
1303   
1304      P:000379 P:00037B 0A7000            BCLR    #MODE_APPLICATION,X:MODE
                            000001
1305      P:00037B P:00037D 0A7000            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1306      P:00037D P:00037F 0C0313            JMP     VCOM_EXIT
1307   
1308   
1309                                ;-----------------------------------------------------------------------------
1310                                RESET_CONTROLLER
1311                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1312                                ;-----------------------------------------------------------------------------
1313                                ; word 1 = command = 'RCO'
1314                                ; word 2-4 unused
1315   
1316      P:00037E P:000380 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1317      P:000380 P:000382 45F400            MOVE              #'RCO',X1
                            52434F
1318      P:000382 P:000384 0D0306            JSR     VCOM_INTRO
1319      P:000383 P:000385 0E2313            JNE     VCOM_EXIT
1320   
1321      P:000384 P:000386 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1322      P:000385 P:000387 000000            NOP
1323      P:000386 P:000388 000000            NOP
1324      P:000387 P:000389 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1325      P:000389 P:00038B 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1326      P:00038B P:00038D 446000            MOVE              X0,X:(R0)
1327      P:00038C P:00038E 0606A0            REP     #6                                ; Wait for transmission to complete
1328      P:00038D P:00038F 000000            NOP
1329      P:00038E P:000390 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1330   
1331                                ; Wait for a bit for MCE to be reset.......
1332      P:00038F P:000391 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1333      P:000391 P:000393 06C400            DO      X0,L_DELAY
                            000397
1334      P:000393 P:000395 06E883            DO      #1000,L_RDFIFO
                            000396
1335      P:000395 P:000397 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1336      P:000396 P:000398 000000            NOP                                       ;   receiver empty
1337                                L_RDFIFO
1338      P:000397 P:000399 000000            NOP
1339                                L_DELAY
1340      P:000398 P:00039A 000000            NOP
1341   
1342      P:000399 P:00039B 44F400            MOVE              #'000',X0
                            303030
1343      P:00039B P:00039D 0C0311            JMP     VCOM_EXIT_X0
1344   
1345                                ;-----------------------------------------------------------------------------
1346                                QUIET_TRANSFER_SET
1347                                ;-----------------------------------------------------------------------------
1348                                ;Quiet transfer mode configuration
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 28



1349                                ; word 1 = command = 'QTS'
1350                                ; word 2 = parameter to set
1351                                ; word 3-4 = arguments
1352   
1353      P:00039C P:00039E 0BF080            JSR     SAVE_REGISTERS                    ; standard opening
                            0004A0
1354      P:00039E P:0003A0 45F400            MOVE              #'QTS',X1
                            515453
1355      P:0003A0 P:0003A2 0D0306            JSR     VCOM_INTRO
1356      P:0003A1 P:0003A3 0E2313            JNE     VCOM_EXIT
1357   
1358      P:0003A2 P:0003A4 60F400            MOVE              #BDEBUG0,R0
                            00004D
1359      P:0003A4 P:0003A6 0D01DE            JSR     INCR_X_R0
1360   
1361      P:0003A5 P:0003A7 56F000            MOVE              X:DRXR_WD2,A            ; Parameter id
                            000008
1362      P:0003A7 P:0003A9 44F000            MOVE              X:DRXR_WD3,X0           ; First arg
                            000009
1363      P:0003A9 P:0003AB 45F000            MOVE              X:DRXR_WD4,X1           ; Second arg
                            00000A
1364   
1365      P:0003AB P:0003AD 0140C5            CMP     #'BAS',A
                            424153
1366      P:0003AD P:0003AF 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            00042D
1367   
1368      P:0003AF P:0003B1 0140C5            CMP     #'DEL',A
                            44454C
1369      P:0003B1 P:0003B3 60F400            MOVE              #QT_BUF_SIZE,R0
                            00003E
1370      P:0003B3 P:0003B5 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1371   
1372      P:0003B5 P:0003B7 0140C5            CMP     #'NUM',A
                            4E554D
1373      P:0003B7 P:0003B9 60F400            MOVE              #QT_BUF_MAX,R0
                            00003F
1374      P:0003B9 P:0003BB 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1375   
1376      P:0003BB P:0003BD 0140C5            CMP     #'INF',A
                            494E46
1377      P:0003BD P:0003BF 60F400            MOVE              #QT_INFORM,R0
                            000041
1378      P:0003BF P:0003C1 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1379   
1380      P:0003C1 P:0003C3 0140C5            CMP     #'SIZ',A
                            53495A
1381      P:0003C3 P:0003C5 60F400            MOVE              #QT_FRAME_SIZE,R0
                            000040
1382      P:0003C5 P:0003C7 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1383   
1384      P:0003C7 P:0003C9 0140C5            CMP     #'TAI',A
                            544149
1385      P:0003C9 P:0003CB 60F400            MOVE              #QT_BUF_TAIL,R0
                            000043
1386      P:0003CB P:0003CD 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1387   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 29



1388      P:0003CD P:0003CF 0140C5            CMP     #'HEA',A
                            484541
1389      P:0003CF P:0003D1 60F400            MOVE              #QT_BUF_HEAD,R0
                            000042
1390      P:0003D1 P:0003D3 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1391   
1392      P:0003D3 P:0003D5 0140C5            CMP     #'DRO',A
                            44524F
1393      P:0003D5 P:0003D7 60F400            MOVE              #QT_DROPS,R0
                            000047
1394      P:0003D7 P:0003D9 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1395   
1396      P:0003D9 P:0003DB 0140C5            CMP     #'PER',A
                            504552
1397      P:0003DB P:0003DD 60F400            MOVE              #TCPR0,R0
                            FFFF8D
1398      P:0003DD P:0003DF 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1399   
1400      P:0003DF P:0003E1 0140C5            CMP     #'FLU',A
                            464C55
1401      P:0003E1 P:0003E3 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            00040D
1402   
1403      P:0003E3 P:0003E5 0140C5            CMP     #'SET',A
                            534554
1404      P:0003E5 P:0003E7 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            000415
1405   
1406      P:0003E7 P:0003E9 0140C5            CMP     #'RPS',A
                            525053
1407      P:0003E9 P:0003EB 60F400            MOVE              #RP_MAX_SIZE,R0
                            00004A
1408      P:0003EB P:0003ED 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1409   
1410      P:0003ED P:0003EF 0140C5            CMP     #'RPB',A
                            525042
1411      P:0003EF P:0003F1 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_BASE
                            0003FE
1412   
1413      P:0003F1 P:0003F3 0140C5            CMP     #'RPE',A
                            525045
1414      P:0003F3 P:0003F5 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_ENABLED
                            000403
1415   
1416      P:0003F5 P:0003F7 0140C5            CMP     #'BUR',A
                            425552
1417      P:0003F7 P:0003F9 60F400            MOVE              #PCI_BURST_SIZE,R0
                            000029
1418      P:0003F9 P:0003FB 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0_PERSISTENT
                            000425
1419   
1420      P:0003FB P:0003FD 44F400            MOVE              #'MTE',X0
                            4D5445
1421      P:0003FD P:0003FF 0C030C            JMP     VCOM_EXIT_ERROR_X0
1422   
1423                                QUIET_TRANSFER_SET_RP_BASE
1424      P:0003FE P:000400 447000            MOVE              X0,X:RP_BASE_LO
                            000048
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 30



1425      P:000400 P:000402 457000            MOVE              X1,X:RP_BASE_HI
                            000049
1426      P:000402 P:000404 0C0313            JMP     VCOM_EXIT
1427   
1428                                QUIET_TRANSFER_SET_RP_ENABLED
1429      P:000403 P:000405 0A7003            BCLR    #MODE_RP_BUFFER,X:MODE
                            000001
1430      P:000405 P:000407 208E00            MOVE              X0,A
1431      P:000406 P:000408 200003            TST     A
1432      P:000407 P:000409 0EA313            JEQ     VCOM_EXIT
1433      P:000408 P:00040A 0A7023            BSET    #MODE_RP_BUFFER,X:MODE
                            000001
1434      P:00040A P:00040C 0A7015            BCLR    #RP_BUFFER_FULL,X:STATUS
                            000000
1435      P:00040C P:00040E 0C0313            JMP     VCOM_EXIT
1436   
1437                                QUIET_TRANSFER_SET_FLUSH
1438      P:00040D P:00040F 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
1439      P:00040F P:000411 208E00            MOVE              X0,A
1440      P:000410 P:000412 200003            TST     A
1441      P:000411 P:000413 0EA313            JEQ     VCOM_EXIT
1442      P:000412 P:000414 0A7034            BSET    #QT_FLUSH,X:STATUS
                            000000
1443      P:000414 P:000416 0C0313            JMP     VCOM_EXIT
1444   
1445                                QUIET_TRANSFER_SET_ENABLED
1446      P:000415 P:000417 208E00            MOVE              X0,A
1447      P:000416 P:000418 200003            TST     A
1448      P:000417 P:000419 0AF0AA            JEQ     QUIET_TRANSFER_SET_DISABLED
                            00041E
1449      P:000419 P:00041B 0A7022            BSET    #MODE_QT,X:MODE
                            000001
1450      P:00041B P:00041D 0BF080            JSR     TIMER_ENABLE
                            000622
1451      P:00041D P:00041F 0C0313            JMP     VCOM_EXIT
1452   
1453                                QUIET_TRANSFER_SET_DISABLED
1454      P:00041E P:000420 0A7002            BCLR    #MODE_QT,X:MODE
                            000001
1455      P:000420 P:000422 0BF080            JSR     TIMER_DEFAULT
                            00062E
1456      P:000422 P:000424 0C0313            JMP     VCOM_EXIT
1457   
1458                                QUIET_TRANSFER_SET_R0
1459      P:000423 P:000425 446000            MOVE              X0,X:(R0)
1460      P:000424 P:000426 0C0313            JMP     VCOM_EXIT
1461   
1462                                QUIET_TRANSFER_SET_R0_PERSISTENT
1463      
1464      
1465      
1466      P:000425 P:000427 446000            MOVE              X0,X:(R0)
1467      P:000426 P:000428 57F400            MOVE              #>VAR_TBL_START,B
                            0006AC
1468      P:000428 P:00042A 220E00            MOVE              R0,A
1469      P:000429 P:00042B 200018            ADD     A,B
**** 1470 [main.asm 873]: WARNING --- Pipeline stall reading register B written in previous instruction (X data move field)
**** 1470 [main.asm 873]: WARNING --- Pipeline stall reading register written in previous instruction (X data move field)
1470      P:00042A P:00042C 21F000            MOVE              B,R0
**** 1471 [main.asm 874]: WARNING --- Pipeline stall reading register written in instruction at address: P:00042A (X data move field
)
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 31



1471      P:00042B P:00042D 076084            MOVE              X0,P:(R0)
1472      P:00042C P:00042E 0C0313            JMP     VCOM_EXIT
1473   
1474                                QUIET_TRANSFER_SET_BASE
1475      P:00042D P:00042F 447000            MOVE              X0,X:QT_BASE_LO
                            00003C
1476      P:00042F P:000431 457000            MOVE              X1,X:QT_BASE_HI
                            00003D
1477   
1478      P:000431 P:000433 0BF080            JSR     BUFFER_RESET
                            000669
1479   
1480      P:000433 P:000435 0C0313            JMP     VCOM_EXIT
1481   
1482   
1483                                ;-----------------------------------------------------------------------------
1484                                SYSTEM_RESET
1485                                ;-----------------------------------------------------------------------------
1486   
1487      P:000434 P:000436 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1488      P:000435 P:000437 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1489                                                                                    ; set to zero except for interrupts
1490      P:000437 P:000439 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1491                                                                                    ; so first set to 0
1492      P:000438 P:00043A 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1493                                                                                    ; therefore,return to initialization
1494      P:00043A P:00043C 000000            NOP
1495      P:00043B P:00043D 000004            RTI                                       ; return from ISR - to START
1496   
1497   
1498                                ; ------------------------------------------------------------------------------------
1499                                SEND_PACKET_TO_HOST
1500                                ; this command is received from the Host and actions the PCI board to pick up an address
1501                                ; pointer from DRXR which the PCI board then uses to write packets from the
1502                                ; MCE to the host memory starting at the address given.
1503                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1504                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1505                                ; HST after packet sent (unless error).
1506                                ; --------------------------------------------------------------------------------------
1507                                ; word 1 = command = 'HST'
1508                                ; word 2 = host high address
1509                                ; word 3 = host low address
1510                                ; word 4 = not used but read
1511   
1512      P:00043C P:00043E 0D04A0            JSR     <SAVE_REGISTERS                   ; save working registers
1513      P:00043D P:00043F 45F400            MOVE              #'HST',X1
                            485354
1514      P:00043F P:000441 0D0306            JSR     VCOM_INTRO
1515      P:000440 P:000442 0E2313            JNE     VCOM_EXIT
1516   
1517      
1518      P:000441 P:000443 448800            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
1519      P:000442 P:000444 518900            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1520   
1521      P:000443 P:000445 447000            MOVE              X0,X:BURST_DEST_HI
                            000030
1522      P:000445 P:000447 517000            MOVE              B0,X:BURST_DEST_LO
                            00002F
1523   
1524      P:000447 P:000449 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 32



memory
1525   
1526      P:000448 P:00044A 0BF080            JSR     RESTORE_REGISTERS
                            000493
1527      P:00044A P:00044C 000004            RTI                                       ; Main loop will reply after packet transfer
!
1528   
1529   
1530                                ; --------------------------------------------------------------------
1531                                SOFTWARE_RESET
1532                                ;----------------------------------------------------------------------
1533                                ; word 1 = command = 'RST'
1534                                ; word 2-4 unused
1535   
1536      P:00044B P:00044D 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1537      P:00044D P:00044F 45F400            MOVE              #'RST',X1
                            525354
1538      P:00044F P:000451 0D0306            JSR     VCOM_INTRO
1539      P:000450 P:000452 0E2313            JNE     VCOM_EXIT
1540   
1541                                ; RST command OK so reply to host
1542                                FINISH_RST
1543      P:000451 P:000453 44F400            MOVE              #'000',X0
                            303030
1544      P:000453 P:000455 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1545      P:000455 P:000457 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1546   
1547      P:000457 P:000459 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            000457
1548   
1549      P:000459 P:00045B 0A0100            BCLR    #MODE_APPLICATION,X:<MODE         ; clear app flag
1550      P:00045A P:00045C 0A0000            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1551   
1552      P:00045B P:00045D 0C0434            JMP     SYSTEM_RESET                      ; Handle the stack and stuff...
1553   
1554   
1555                                SEND_PACKET_TO_CONTROLLER
1556   
1557                                ;       Host command identifying location of an MCE command to send to
1558                                ;       the MCE.  Since this can come at any time, just record the
1559                                ;       request and then do the CONning from the main loop.
1560   
1561                                ; word 1 = command = 'CON'
1562                                ; word 2 = source host bus address, bits 31:16
1563                                ; word 3 = source host bus address, bits 15:0
1564                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1565                                ;        = '1' --> when MCE command is GO
1566   
1567      P:00045C P:00045E 0D04A0            JSR     <SAVE_REGISTERS                   ; save working registers
1568   
1569      
1570      P:00045D P:00045F 45F400            MOVE              #'CON',X1
                            434F4E
1571      P:00045F P:000461 0D0306            JSR     VCOM_INTRO
1572      P:000460 P:000462 0E2313            JNE     VCOM_EXIT
1573   
1574      
1575      P:000461 P:000463 44F400            MOVE              #'BUS',X0
                            425553
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 33



1576      P:000463 P:000465 0A00AA            JSET    #CON_DEMAND,X:STATUS,VCOM_EXIT_ERROR_X0
                            00030C
1577   
1578      
1579      P:000465 P:000467 0A702A            BSET    #CON_DEMAND,X:STATUS
                            000000
1580      P:000467 P:000469 448800            MOVE              X:<DRXR_WD2,X0
1581      P:000468 P:00046A 458900            MOVE              X:<DRXR_WD3,X1
1582      P:000469 P:00046B 447000            MOVE              X0,X:CON_SRC_HI
                            00002D
1583      P:00046B P:00046D 457000            MOVE              X1,X:CON_SRC_LO
                            00002C
1584   
1585                                ;       ;; Fourth word indicates if this is a go.  Who cares?
1586                                ;       MOVE    X:<DRXR_WD4,A           ; read word 4 - GO command?
1587                                ;       MOVE    #0,X0
1588                                ;       CMP     X0,A
1589                                ;       JEQ     BLOCK_CON
1590   
1591      
1592      P:00046D P:00046F 0BF080            JSR     RESTORE_REGISTERS
                            000493
1593      P:00046F P:000471 000004            RTI
1594   
1596   
1597   
1598                                ;---------------------------------------------------------------
1599                                ;
1600                                ;                          * END OF ISRs *
1601                                ;
1602                                ;--------------------------------------------------------------
1603   
1604   
1605   
1606                                ;----------------------------------------------------------------
1607                                ;
1608                                ;                     * Beginning of SUBROUTINES *
1609                                ;
1610                                ;-----------------------------------------------------------------
1611   
1612   
1613                                CHECK_FO
1614      P:000470 P:000472 01AD80            JCLR    #EF,X:PDRD,CHECK_FO_CLEAR
                            000478
1615      P:000472 P:000474 000000            NOP
1616      P:000473 P:000475 000000            NOP
1617      P:000474 P:000476 01AD80            JCLR    #EF,X:PDRD,CHECK_FO_CLEAR
                            000478
1618      P:000476 P:000478 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1619      P:000477 P:000479 00000C            RTS
1620   
1621                                CHECK_FO_CLEAR
1622      P:000478 P:00047A 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1623      P:000479 P:00047B 00000C            RTS
1624   
1625   
1626   
1627                                ;----------------------------------------------------------------------------
1628                                PCI_MESSAGE_TO_HOST
1629                                ;----------------------------------------------------------------------------
1630                                ; Subroutine to send 4 words as a reply from PCI to the Host
1631                                ; using the DTXS-HRXS data path.  The DSP signals the host by raising
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 34



1632                                ; HF3 and (when !MODE_NOIRQ) INTA.
1633                                ;
1634                                ; When MODE_HANDSHAKE, the DSP and Host interact as follows:
1635                                ; - to show that the Host is handling the interrupt, Host raises HF0
1636                                ; - when DSP sees HF0 go high, it lowers INTA and HF3
1637                                ; - when Host is done handling the interrupt (i.e. it has read the reply),
1638                                ;   and when HF3 is low, Host lowers HF0.
1639                                ; - when DSP sees HF0 go low, the routine finishes.
1640                                ;
1641                                ; The primary advantage of this hand-shaking scheme is that host vector
1642                                ; commands are not needed to clear HF3 and INTA.
1643                                ;
1644                                ; This routine should not block for anything other than the Host handshake.
1645   
1646      P:00047A P:00047C 60F400            MOVE              #>DTXS_WD1,R0
                            00000B
1647   
1648      P:00047C P:00047E 060480            DO      #4,PCI_MESSAGE_TO_HOST_10
                            000480
1649      P:00047E P:000480 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            00047E
1650      P:000480 P:000482 08D88D            MOVEP             X:(R0)+,X:DTXS
1651   
1652                                PCI_MESSAGE_TO_HOST_10
1653      P:000481 P:000483 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            000015
1654      P:000483 P:000485 60F000            MOVE              X:SV_R0,R0              ; restore R0
                            000019
1655      P:000485 P:000487 0A8523            BSET    #DCTR_HF3,X:DCTR                  ; Raise HF3 (handshake)
1656   
1657                                                                                    ; Only interrupt in irq mode
1658      
1659      P:000486 P:000488 000000            NOP
1660      P:000487 P:000489 000000            NOP
1661      P:000488 P:00048A 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1662   
1663                                PCI_MESSAGE_TO_HOST_20
1664      P:000489 P:00048B 0A89A4            JSET    #DSR_HF1,X:DSR,PCI_MESSAGE_TO_HOST_HANDSHAKE
                            00048C
1665      P:00048B P:00048D 00000C            RTS
1666   
1667                                PCI_MESSAGE_TO_HOST_HANDSHAKE
1668      P:00048C P:00048E 0A8983            JCLR    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            00048C
1669      P:00048E P:000490 0A8506            BCLR    #INTA,X:DCTR                      ; Clear interrupt
1670      P:00048F P:000491 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; Clear hand-shake bit
1671      P:000490 P:000492 0A89A3            JSET    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            000490
1672      P:000492 P:000494 00000C            RTS
1673   
1674   
1675                                ;------------------------------------------------------------------------------------
1676                                RESTORE_REGISTERS
1677                                ;-------------------------------------------------------------------------------------
1678   
1679      P:000493 P:000495 059A39            MOVEC             X:<SV_SR,SR
1680   
1681      P:000494 P:000496 508F00            MOVE              X:<SV_A0,A0
1682      P:000495 P:000497 549000            MOVE              X:<SV_A1,A1
1683      P:000496 P:000498 529100            MOVE              X:<SV_A2,A2
1684   
1685      P:000497 P:000499 519200            MOVE              X:<SV_B0,B0
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 35



1686      P:000498 P:00049A 559300            MOVE              X:<SV_B1,B1
1687      P:000499 P:00049B 539400            MOVE              X:<SV_B2,B2
1688   
1689      P:00049A P:00049C 449500            MOVE              X:<SV_X0,X0
1690      P:00049B P:00049D 459600            MOVE              X:<SV_X1,X1
1691   
1692      P:00049C P:00049E 469700            MOVE              X:<SV_Y0,Y0
1693      P:00049D P:00049F 479800            MOVE              X:<SV_Y1,Y1
1694   
1695      P:00049E P:0004A0 609900            MOVE              X:<SV_R0,R0
1696      P:00049F P:0004A1 00000C            RTS
1697   
1698                                ;-------------------------------------------------------------------------------------
1699                                SAVE_REGISTERS
1700                                ;-------------------------------------------------------------------------------------
1701   
1702      P:0004A0 P:0004A2 051A39            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1703   
1704      P:0004A1 P:0004A3 500F00            MOVE              A0,X:<SV_A0
1705      P:0004A2 P:0004A4 541000            MOVE              A1,X:<SV_A1
1706      P:0004A3 P:0004A5 521100            MOVE              A2,X:<SV_A2
1707   
1708      P:0004A4 P:0004A6 511200            MOVE              B0,X:<SV_B0
1709      P:0004A5 P:0004A7 551300            MOVE              B1,X:<SV_B1
1710      P:0004A6 P:0004A8 531400            MOVE              B2,X:<SV_B2
1711   
1712      P:0004A7 P:0004A9 441500            MOVE              X0,X:<SV_X0
1713      P:0004A8 P:0004AA 451600            MOVE              X1,X:<SV_X1
1714   
1715      P:0004A9 P:0004AB 461700            MOVE              Y0,X:<SV_Y0
1716      P:0004AA P:0004AC 471800            MOVE              Y1,X:<SV_Y1
1717   
1718      P:0004AB P:0004AD 601900            MOVE              R0,X:<SV_R0
1719      P:0004AC P:0004AE 00000C            RTS
1720   
1721   
1722                                ;----------------------------------------------
1723                                FLUSH_PCI_FIFO
1724                                ;----------------------------------------------
1725      P:0004AD P:0004AF 0A8A84            JCLR    #MARQ,X:DPSR,*
                            0004AD
1726      P:0004AF P:0004B1 0A862E            BSET    #CLRT,X:DPCR
1727      P:0004B0 P:0004B2 000000            NOP
1728      P:0004B1 P:0004B3 0A86AE            JSET    #CLRT,X:DPCR,*
                            0004B1
1729      P:0004B3 P:0004B5 00000C            RTS
1730   
1731                                ;----------------------------------------------
1732                                CLEAR_FO_FIFO
1733                                ;----------------------------------------------
1734      P:0004B4 P:0004B6 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
1735      P:0004B6 P:0004B8 44F400            MOVE              #200000,X0
                            030D40
1736      P:0004B8 P:0004BA 06C400            DO      X0,*+3
                            0004BA
1737      P:0004BA P:0004BC 000000            NOP
1738      P:0004BB P:0004BD 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
1739      P:0004BD P:0004BF 00000C            RTS
1740   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 36



1741   
1742                                ;---------------------------------------------------------
1743                                ; PCI burst routines
1744                                ;
1745                                ; For transfer between Host memory and DSP Y memory.
1746                                ;
1747                                ; Major entry points are
1748                                ;       CON_TRANSFER (PC -> DSP)
1749                                ;       BLOCK_TRANSFER (DSP -> PC)
1750                                ;---------------------------------------------------------
1751   
1752                                ;---------------------------------------------------------
1753                                PCI_ERROR_CLEAR
1754                                ;-----------------------------------------------
1755      
1756      
1757      
1758      
1759      
1760      
1761   
1762      P:0004BE P:0004C0 50F000            MOVE              X:DMA_ERRORS,A0
                            000034
1763      P:0004C0 P:0004C2 000008            INC     A
1764      P:0004C1 P:0004C3 000000            NOP
1765      P:0004C2 P:0004C4 507000            MOVE              A0,X:DMA_ERRORS
                            000034
1766   
1767      P:0004C4 P:0004C6 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            0004D2
1768      P:0004C6 P:0004C8 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
                            0004DC
1769      P:0004C8 P:0004CA 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            0004E6
1770      P:0004CA P:0004CC 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            0004F0
1771      P:0004CC P:0004CE 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            0004FA
1772      P:0004CE P:0004D0 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            000504
1773      P:0004D0 P:0004D2 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            00050E
1774   
1775                                ERROR_TRTY
1776      P:0004D2 P:0004D4 50F000            MOVE              X:EC_TRTY,A0
                            000035
1777      P:0004D4 P:0004D6 000008            INC     A
1778      P:0004D5 P:0004D7 08F48A            MOVEP             #>$0400,X:DPSR          ; Clear target retry error bit
                            000400
1779      P:0004D7 P:0004D9 507000            MOVE              A0,X:EC_TRTY
                            000035
1780      P:0004D9 P:0004DB 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1781      P:0004DB P:0004DD 00000C            RTS
1782                                ERROR_TO
1783      P:0004DC P:0004DE 50F000            MOVE              X:EC_TO,A0
                            000036
1784      P:0004DE P:0004E0 000008            INC     A
1785      P:0004DF P:0004E1 08F48A            MOVEP             #>$0800,X:DPSR          ; Clear timeout error bit
                            000800
1786      P:0004E1 P:0004E3 507000            MOVE              A0,X:EC_TO
                            000036
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 37



1787      P:0004E3 P:0004E5 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1788      P:0004E5 P:0004E7 00000C            RTS
1789                                ERROR_TDIS
1790      P:0004E6 P:0004E8 50F000            MOVE              X:EC_TDIS,A0
                            000037
1791      P:0004E8 P:0004EA 000008            INC     A
1792      P:0004E9 P:0004EB 08F48A            MOVEP             #>$0200,X:DPSR          ; Clear target disconnect bit
                            000200
1793      P:0004EB P:0004ED 507000            MOVE              A0,X:EC_TDIS
                            000037
1794      P:0004ED P:0004EF 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1795      P:0004EF P:0004F1 00000C            RTS
1796                                ERROR_TAB
1797      P:0004F0 P:0004F2 50F000            MOVE              X:EC_TAB,A0
                            000038
1798      P:0004F2 P:0004F4 000008            INC     A
1799      P:0004F3 P:0004F5 08F48A            MOVEP             #>$0100,X:DPSR          ; Clear target abort error bit
                            000100
1800      P:0004F5 P:0004F7 507000            MOVE              A0,X:EC_TAB
                            000038
1801      P:0004F7 P:0004F9 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1802      P:0004F9 P:0004FB 00000C            RTS
1803                                ERROR_MAB
1804      P:0004FA P:0004FC 50F000            MOVE              X:EC_MAB,A0
                            000039
1805      P:0004FC P:0004FE 000008            INC     A
1806      P:0004FD P:0004FF 08F48A            MOVEP             #>$0080,X:DPSR          ; Clear master abort error bit
                            000080
1807      P:0004FF P:000501 507000            MOVE              A0,X:EC_MAB
                            000039
1808      P:000501 P:000503 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1809      P:000503 P:000505 00000C            RTS
1810                                ERROR_DPER
1811      P:000504 P:000506 50F000            MOVE              X:EC_DPER,A0
                            00003A
1812      P:000506 P:000508 000008            INC     A
1813      P:000507 P:000509 08F48A            MOVEP             #>$0040,X:DPSR          ; Clear data parity error bit
                            000040
1814      P:000509 P:00050B 507000            MOVE              A0,X:EC_DPER
                            00003A
1815      P:00050B P:00050D 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1816      P:00050D P:00050F 00000C            RTS
1817                                ERROR_APER
1818      P:00050E P:000510 50F000            MOVE              X:EC_APER,A0
                            00003B
1819      P:000510 P:000512 000008            INC     A
1820      P:000511 P:000513 08F48A            MOVEP             #>$0020,X:DPSR          ; Clear address parity error bit
                            000020
1821      P:000513 P:000515 507000            MOVE              A0,X:EC_APER
                            00003B
1822      P:000515 P:000517 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1823      P:000517 P:000519 00000C            RTS
1824   
1825   
1826   
1827                                ;----------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 38



1828                                BLOCK_TRANSFER
1829                                ;----------------------------------------------
1830                                ;   In:
1831                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
1832                                ;   - BLOCK_SIZE is packet size, in bytes
1833                                ;   - YMEM_SRC is start of data in Y memory
1834                                ;  Out:
1835                                ;   - BLOCK_SIZE will be decremented to zero.
1836                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
1837                                ;   - YMEM_SRC will be incremented by BLOCK_SIZE/2
1838                                ;  Trashes:
1839                                ;   - A and B at least
1840   
1841      P:000518 P:00051A 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002B
1842      P:00051A P:00051C 014085            CMP     #0,A                              ; Still bytes to transfer?
1843      P:00051B P:00051D 0AF0A2            JNE     BLOCK_TRANSFER0
                            00051E
1844      P:00051D P:00051F 00000C            RTS
1845   
1846                                BLOCK_TRANSFER0
1847      
1848      
1849      P:00051E P:000520 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000029
1850   
1851      P:000520 P:000522 200005            CMP     B,A                               ; A ? B
1852      P:000521 P:000523 0E1523            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
1853      P:000522 P:000524 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1854                                BLOCK_TRANSFER1
1855      
1856      P:000523 P:000525 200014            SUB     B,A                               ; A -= B
1857      P:000524 P:000526 014088            ADD     #0,B                              ; Clear carry bit
1858      P:000525 P:000527 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002B
1859      P:000527 P:000529 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002A
1860      P:000529 P:00052B 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1861   
1862      
1863      P:00052A P:00052C 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
1864      P:00052C P:00052E 50F000            MOVE              X:YMEM_SRC,A0
                            00002E
1865      P:00052E P:000530 08C82F            MOVEP             A0,X:DSR0               ; DMA source
1866      P:00052F P:000531 200010            ADD     B,A
1867      P:000530 P:000532 00000B            DEC     B
1868      P:000531 P:000533 507000            MOVE              A0,X:YMEM_SRC           ; BURST_SRC += BURST_SIZE/2
                            00002E
1869   
1870      P:000533 P:000535 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1871   
1872      
1873      P:000534 P:000536 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
1874   
1875                                BLOCK_TRANSFER_PCI
1876      P:000536 P:000538 44F400            MOVE              #>$7,X0                 ; Memory write
                            000007
1877      P:000538 P:00053A 60F400            MOVE              #BURST_DEST_LO,R0       ; RAM address
                            00002F
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 39



1878      P:00053A P:00053C 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            00058D
1879   
1880      
1881      P:00053C P:00053E 0A8A84            JCLR    #MARQ,X:DPSR,*
                            00053C
1882   
1883      
1884      P:00053E P:000540 0A8A8E            JCLR    #MDT,X:DPSR,BLOCK_TRANSFER_HANDLE_ERRORS
                            000546
1885   
1886      P:000540 P:000542 20001B            CLR     B
1887      P:000541 P:000543 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            00002A
1888      P:000543 P:000545 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            0006A1
1889      P:000545 P:000547 0C0518            JMP     BLOCK_TRANSFER                    ; Next burst in block
1890   
1891                                BLOCK_TRANSFER_HANDLE_ERRORS
1892      
1893      P:000546 P:000548 0D04BE            JSR     PCI_ERROR_CLEAR
1894   
1895      P:000547 P:000549 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1896      P:000549 P:00054B 0E8536            JCS     BLOCK_TRANSFER_PCI                ; Restart PCI burst
1897   
1898      P:00054A P:00054C 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
1899      P:00054C P:00054E 0E0518            JCC     BLOCK_TRANSFER                    ; Error but no error? Redo this burst.
1900   
1901      
1902      P:00054D P:00054F 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            0005A3
1903      P:00054F P:000551 0BF080            JSR     PCI_UPDATE_R0
                            0005B3
1904      P:000551 P:000553 0C0536            JMP     BLOCK_TRANSFER_PCI
1905   
1906   
1907                                ;----------------------------------------------
1908                                CON_TRANSFER
1909                                ;----------------------------------------------
1910                                ;   In:
1911                                ;   - BURST_SRC_HI:BURST_SRC_LO is PC RAM address
1912                                ;   - BLOCK_SIZE is packet size, in bytes
1913                                ;   - YMEM_DEST is start of data in Y memory
1914                                ;  Out:
1915                                ;   - BLOCK_SIZE will be decremented to zero.
1916                                ;   - BURST_SRC_HI:LO will be incremented by BLOCK_SIZE
1917                                ;   - YMEM_DEST will be incremented by BLOCK_SIZE/2
1918                                ;  Trashes:
1919                                ;   - A and B, R0, X0
1920   
1921      P:000552 P:000554 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002B
1922      P:000554 P:000556 014085            CMP     #0,A                              ; Still bytes to transfer?
1923      P:000555 P:000557 0AF0A2            JNE     CON_TRANSFER0
                            000558
1924      P:000557 P:000559 00000C            RTS
1925   
1926                                CON_TRANSFER0
1927      
1928      
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 40



1929      P:000558 P:00055A 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000029
1930   
1931      P:00055A P:00055C 200005            CMP     B,A                               ; A ? B
1932      P:00055B P:00055D 0E155D            JGE     <CON_TRANSFER1                    ; jump if A >= B
1933      P:00055C P:00055E 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1934                                CON_TRANSFER1
1935      
1936      P:00055D P:00055F 200014            SUB     B,A                               ; A -= B
1937      P:00055E P:000560 014088            ADD     #0,B                              ; Clear carry bit
1938      P:00055F P:000561 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002B
1939      P:000561 P:000563 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002A
1940      P:000563 P:000565 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1941   
1942      
1943      P:000564 P:000566 50F000            MOVE              X:YMEM_DEST,A0
                            000033
1944      P:000566 P:000568 507000            MOVE              A0,X:DDR0               ; DMA dest'n
                            FFFFEE
1945      P:000568 P:00056A 08F4AF            MOVEP             #>DRXR,X:DSR0           ; DMA source
                            FFFFCB
1946      P:00056A P:00056C 200010            ADD     B,A
1947      P:00056B P:00056D 00000B            DEC     B
1948      P:00056C P:00056E 507000            MOVE              A0,X:YMEM_DEST          ; YMEM_DEST += BURST_SIZE/2
                            000033
1949   
1950      P:00056E P:000570 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1951   
1952      
1953      P:00056F P:000571 08F4AC            MOVEP             #$8EEAC4,X:DCR0
                            8EEAC4
1954   
1955                                CON_TRANSFER_PCI
1956      P:000571 P:000573 44F400            MOVE              #>$6,X0                 ; Memory write
                            000006
1957      P:000573 P:000575 60F400            MOVE              #BURST_SRC_LO,R0        ; RAM address
                            000031
1958      P:000575 P:000577 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            00058D
1959   
1960      
1961      P:000577 P:000579 0A8A84            JCLR    #MARQ,X:DPSR,*
                            000577
1962   
1963      
1964      P:000579 P:00057B 0A8A8E            JCLR    #MDT,X:DPSR,CON_TRANSFER_HANDLE_ERRORS
                            000581
1965   
1966      P:00057B P:00057D 20001B            CLR     B
1967      P:00057C P:00057E 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            00002A
1968      P:00057E P:000580 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            0006A1
1969      P:000580 P:000582 0C0552            JMP     CON_TRANSFER                      ; Next burst in block
1970   
1971                                CON_TRANSFER_HANDLE_ERRORS
1972      
1973      P:000581 P:000583 0D04BE            JSR     PCI_ERROR_CLEAR
1974   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 41



1975      P:000582 P:000584 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1976      P:000584 P:000586 0E8571            JCS     CON_TRANSFER_PCI                  ; Restart PCI burst
1977   
1978      P:000585 P:000587 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
1979      P:000587 P:000589 0E0552            JCC     CON_TRANSFER                      ; Error but no error? Redo this burst.
1980   
1981      
1982      P:000588 P:00058A 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            0005A3
1983      P:00058A P:00058C 0BF080            JSR     PCI_UPDATE_R0
                            0005B3
1984      P:00058C P:00058E 0C0571            JMP     CON_TRANSFER_PCI
1985   
1986                                ; Utility routines for BLOCK_TRANSFER and CON_TRANSFER
1987   
1988                                PCI_GO
1989                                ; Initiate PCI read/write of BURST_SIZE bytes.
1990                                ; R0 must point to the hi-lo PCI address source/dest address
1991                                ; X0 is the PCI command (6 is read, 7 is write).
1992                                ; Trashes A and B but not R0 and X0.
1993      P:00058D P:00058F 200013            CLR     A
1994      P:00058E P:000590 20001B            CLR     B
1995      P:00058F P:000591 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            00002A
1996      P:000591 P:000593 00000B            DEC     B                                 ; n8 - 1
1997      P:000592 P:000594 014088            ADD     #0,B                              ; Clear carry
1998      P:000593 P:000595 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
1999      P:000594 P:000596 014088            ADD     #0,B                              ; Clear carry
2000      P:000595 P:000597 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
2001   
2002      P:000596 P:000598 0200D8            MOVE              X:(R0+1),A0             ; PCI HI address
2003   
2004      P:000597 P:000599 200010            ADD     B,A
2005      P:000598 P:00059A 000000            NOP
2006      P:000599 P:00059B 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
2007   
2008      P:00059B P:00059D 208800            MOVE              X0,A0
2009      P:00059C P:00059E 014088            ADD     #0,B                              ; Clear carry
2010      P:00059D P:00059F 0C1D20            ASL     #16,A,A                           ; Command into bits 19:16
2011      P:00059E P:0005A0 51E000            MOVE              X:(R0),B0
2012      P:00059F P:0005A1 200010            ADD     B,A
2013      P:0005A0 P:0005A2 000000            NOP
2014   
2015      P:0005A1 P:0005A3 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
2016      P:0005A2 P:0005A4 00000C            RTS
2017   
2018   
2019                                PCI_RECOVER_COUNT
2020                                ; Calculate number of PCI words not transferred.
2021                                ; Correct BURST_SIZE.  Returns:
2022                                ;   B: bytes not transferred
2023                                ;   A: bytes transferred
2024      P:0005A3 P:0005A5 200013            CLR     A
2025      P:0005A4 P:0005A6 20001B            CLR     B
2026      P:0005A5 P:0005A7 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
2027      P:0005A6 P:0005A8 0A8A8F            JCLR    #RDCQ,X:DPSR,PCI_RECOVER_COUNT1
                            0005A9
2028      P:0005A8 P:0005AA 000009            INC     B
2029                                PCI_RECOVER_COUNT1
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 42



2030      P:0005A9 P:0005AB 000009            INC     B                                 ; We want N, not N-1.
2031      P:0005AA P:0005AC 014088            ADD     #0,B                              ; Clear carry
2032      P:0005AB P:0005AD 0C1C20            ASR     #16,A,A
2033      P:0005AC P:0005AE 200018            ADD     A,B                               ; B is words remaining
2034      P:0005AD P:0005AF 014088            ADD     #0,B                              ; Clear carry
2035      P:0005AE P:0005B0 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
2036      P:0005AF P:0005B1 50F000            MOVE              X:BURST_SIZE,A0
                            00002A
2037      P:0005B1 P:0005B3 200014            SUB     B,A                               ; A is bytes written
2038      P:0005B2 P:0005B4 00000C            RTS
2039   
2040   
2041                                PCI_UPDATE_R0
2042                                ;  Subtract A0 from BURST_SIZE and add A to the 32-bit hi-lo address at X:[R0].
2043                                ;  Caller can check Z flag to see if BURST_SIZE is now 0.
2044      P:0005B3 P:0005B5 210500            MOVE              A0,X1                   ; Save A for later
2045      P:0005B4 P:0005B6 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2046      P:0005B5 P:0005B7 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates [R0] = [R0] + B
                            0006A1
2047   
2048      P:0005B7 P:0005B9 57F000            MOVE              X:BURST_SIZE,B
                            00002A
2049      P:0005B9 P:0005BB 20006C            SUB     X1,B                              ; Zero flag must be preserved!
2050      P:0005BA P:0005BC 000000            NOP
2051      P:0005BB P:0005BD 557000            MOVE              B1,X:BURST_SIZE
                            00002A
2052      P:0005BD P:0005BF 00000C            RTS
2053   
2054   
2055                                ;----------------------------------------------;
2056                                ;  MCE PACKET PROCESSING                       ;
2057                                ;----------------------------------------------;
2058   
2059                                ;       Given a dword count in A, computes number of half FIFOs and
2060                                ;       number of left over FIFO reads required to get the whole
2061                                ;       packet.
2062   
2063                                ;       Input: A is packet size, in dwords
2064                                ;       Output: sets X:TOTAL_BUFFS and X:LEFT_TO_READ
2065                                ;       Trashes: A,B,X0
2066   
2067   
2068                                PACKET_PARTITIONS
2069      P:0005BE P:0005C0 507000            MOVE              A0,X:PACKET_SIZE
                            000023
2070   
2071      P:0005C0 P:0005C2 014088            ADD     #0,B                              ; Clear carry
2072      P:0005C1 P:0005C3 0C1D02            ASL     #1,A,A                            ;  * 2
2073      P:0005C2 P:0005C4 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
2074      P:0005C3 P:0005C5 240000            MOVE              #0,X0
2075      P:0005C4 P:0005C6 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
2076   
2077      P:0005C6 P:0005C8 557000            MOVE              B1,X:TOTAL_BUFFS
                            000024
2078      P:0005C8 P:0005CA 507000            MOVE              A0,X:LEFT_TO_READ
                            000025
2079      P:0005CA P:0005CC 00000C            RTS
2080   
2081   
2082                                ; BUFFER_PACKET
2083                                ;
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 43



2084                                ; Copies the packet in the FIFO to Y memory.
2085                                ;
2086                                ; In: TOTAL_BUFFS and LEFT_TO_READ must be pre-set (via PACKET_PARTITIONS);
2087                                ;     R1 is the destination index in Y memory.
2088                                ; Trashes: R1 is updated to point to the end of the copied data.
2089   
2090                                BUFFER_PACKET
2091   
2092      P:0005CB P:0005CD 54F400            MOVE              #>$b00,A1
                            000B00
2093      P:0005CD P:0005CF 0BF080            JSR     TIMER_STORE_A1
                            00064F
2094      P:0005CF P:0005D1 0BF080            JSR     TIMER_STORE
                            00064D
2095   
2096      P:0005D1 P:0005D3 062400            DO      X:TOTAL_BUFFS,BUFFER_PACKET_HALFS_DONE
                            0005DB
2097      P:0005D3 P:0005D5 0BF080            JSR     WAIT_FIFO_HALF
                            0005F8
2098      P:0005D5 P:0005D7 0BF080            JSR     TIMER_STORE
                            00064D
2099      P:0005D7 P:0005D9 0BF080            JSR     BUFFER_PACKET_HALF
                            0005F3
2100      P:0005D9 P:0005DB 0BF080            JSR     TIMER_STORE
                            00064D
2101      P:0005DB P:0005DD 000000            NOP
2102                                BUFFER_PACKET_HALFS_DONE
2103   
2104      
2105      
2106      
2107      
2108      P:0005DC P:0005DE 01AD81            JCLR    #HF,X:PDRD,BUFFER_PACKET_SINGLES_FAST
                            0005EF
2109   
2110      
2111      
2112   
2113                                BUFFER_PACKET_SINGLES
2114      
2115      
2116      P:0005DE P:0005E0 200013            CLR     A
2117      P:0005DF P:0005E1 20001B            CLR     B
2118      P:0005E0 P:0005E2 51F000            MOVE              X:TCR0,B0               ; Store timer value (50 MHz)
                            FFFF8C
2119      P:0005E2 P:0005E4 0C1C85            ASR     #2,B,B                            ; / 4
2120      P:0005E3 P:0005E5 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_DONE
                            0005EB
2121                                BUFFER_PACKET_SINGLES_WAIT
2122      P:0005E5 P:0005E7 50F000            MOVE              X:TCR0,A0
                            FFFF8C
2123      P:0005E7 P:0005E9 0C1C04            ASR     #2,A,A
2124      P:0005E8 P:0005EA 20000D            CMP     A,B
2125      P:0005E9 P:0005EB 0EA5E5            JEQ     BUFFER_PACKET_SINGLES_WAIT
2126      P:0005EA P:0005EC 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2127      P:0005EB P:0005ED 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2128                                BUFFER_PACKET_SINGLES_DONE
2129      P:0005EC P:0005EE 0BF080            JSR     TIMER_STORE
                            00064D
2130      P:0005EE P:0005F0 00000C            RTS
2131   
2132                                ;---------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 44



2133   
2134                                BUFFER_PACKET_SINGLES_FAST
2135      P:0005EF P:0005F1 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_FAST_DONE
                            0005F1
2136      P:0005F1 P:0005F3 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2137                                BUFFER_PACKET_SINGLES_FAST_DONE
2138      P:0005F2 P:0005F4 00000C            RTS
2139   
2140                                ;---------------------------------------------------------
2141                                BUFFER_PACKET_HALF
2142      
2143      P:0005F3 P:0005F5 060082            DO      #512,BUFFER_PACKET_HALF_DONE
                            0005F6
2144      P:0005F5 P:0005F7 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2145      P:0005F6 P:0005F8 000000            NOP
2146                                BUFFER_PACKET_HALF_DONE
2147      P:0005F7 P:0005F9 00000C            RTS
2148   
2149                                ;---------------------------------------------------------
2150                                WAIT_FIFO_HALF
2151      P:0005F8 P:0005FA 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            00060D
2152      P:0005FA P:0005FC 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
                            0005F8
2153      P:0005FC P:0005FE 000000            NOP
2154      P:0005FD P:0005FF 000000            NOP
2155      P:0005FE P:000600 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            0005F8
2156      P:000600 P:000602 00000C            RTS
2157   
2158                                ;---------------------------------------------------------
2159   
2160                                ; This is the old single-buffering routine, which polls the EF.
2161                                BUFFER_PACKET_SINGLES_POLL
2162      P:000601 P:000603 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_DONE
                            00060B
2163                                BUFFER_PACKET_SINGLE
2164      P:000603 P:000605 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D5
2165      P:000605 P:000607 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE
                            000603
2166      P:000607 P:000609 000000            NOP
2167      P:000608 P:00060A 000000            NOP
2168      P:000609 P:00060B 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE   ; Protect against metastability
                            000603
2169      P:00060B P:00060D 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2170                                BUFFER_PACKET_DONE
2171      P:00060C P:00060E 00000C            RTS
2172   
2173                                ;---------------------------------------------------------
2174   
2175                                FATALITY_HANDLER
2176      P:00060D P:00060F 0C0100            JMP     START                             ; What could possibly go wrong?
2177   
2178   
2179                                ; DROP_PACKET
2180                                ;
2181                                ; Reads a packet from the fifo, discarding it.
2182                                ;
2183                                ; In: TOTAL_BUFFS & LEFT_TO_READ
2184                                ; Trashes: A0
2185   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 45



2186                                DROP_PACKET
2187      P:00060E P:000610 062400            DO      X:TOTAL_BUFFS,DROP_PACKET_SINGLES
                            000613
2188      P:000610 P:000612 0D05F8            JSR     WAIT_FIFO_HALF
2189      P:000611 P:000613 0BF080            JSR     DROP_FIFO_HALF
                            00061E
2190      P:000613 P:000615 000000            NOP
2191                                DROP_PACKET_SINGLES
2192      P:000614 P:000616 062500            DO      X:LEFT_TO_READ,DROP_PACKET_DONE
                            00061C
2193                                DROP_PACKET_SINGLE
2194      P:000616 P:000618 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D5
2195      P:000618 P:00061A 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE
                            000616
2196      P:00061A P:00061C 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE     ; Protect against metastability
                            000616
2197      P:00061C P:00061E 09483F            MOVEP             Y:RDFIFO,A0
2198                                DROP_PACKET_DONE
2199      P:00061D P:00061F 00000C            RTS
2200   
2201                                DROP_FIFO_HALF
2202      
2203      P:00061E P:000620 060082            DO      #512,DROP_FIFO_DONE
                            000620
2204      P:000620 P:000622 09483F            MOVEP             Y:RDFIFO,A0
2205                                DROP_FIFO_DONE
2206      P:000621 P:000623 00000C            RTS
2207   
2208   
2209                                ;----------------------------------------------;
2210                                ;  TIMER HANDLING                              ;
2211                                ;----------------------------------------------;
2212   
2213                                ; Start value is TLR, count is in TCR, flag marked at TCPR
2214                                ; Must set TCSR[TCIE] to enable int
2215                                ; Must set TCSR[T] for timer to restart
2216   
2217                                TIMER_ENABLE
2218      P:000622 P:000624 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2219      P:000624 P:000626 000000            NOP
2220      P:000625 P:000627 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2221      P:000627 P:000629 00000C            RTS
2222   
2223                                TIMER_DISABLE
2224      P:000628 P:00062A 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
                            300200
2225      P:00062A P:00062C 000000            NOP
2226      P:00062B P:00062D 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2227      P:00062D P:00062F 00000C            RTS
2228   
2229                                TIMER_DEFAULT
2230      P:00062E P:000630 0D0628            JSR     TIMER_DISABLE
2231      P:00062F P:000631 44F400            MOVE              #$4C4B40,X0             ; 5M -> 10 Hz.
                            4C4B40
2232      P:000631 P:000633 000000            NOP
2233      P:000632 P:000634 447000            MOVE              X0,X:TCPR0
                            FFFF8D
2234      P:000634 P:000636 0D0622            JSR     TIMER_ENABLE
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 46



2235      P:000635 P:000637 00000C            RTS
2236   
2237   
2239                                TIMER_ACTION
2240      P:000636 P:000638 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2241      P:000638 P:00063A 000000            NOP
2242      P:000639 P:00063B 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2243      P:00063B P:00063D 56F000            MOVE              X:QT_INFORM_IDX,A       ; QT inform time?
                            000046
2244      P:00063D P:00063F 0A0182            JCLR    #MODE_QT,X:MODE,TIMER_ACTION_OK
                            000645
2245      P:00063F P:000641 0140C5            CMP     #>0,A                             ; If inform_idx != 0
                            000000
2246      P:000641 P:000643 0AF0AA            JEQ     TIMER_ACTION_OK
                            000645
2247      P:000643 P:000645 0A7034            BSET    #QT_FLUSH,X:STATUS                ;    schedule inform
                            000000
2248                                TIMER_ACTION_OK
2249      P:000645 P:000647 00000C            RTS
2250   
2251   
2252                                ;----------------------------------------------;
2253                                ;  TIMER UTILITY                               ;
2254                                ;----------------------------------------------;
2255   
2256                                 TIMER_SOURCE
2257      FFFF8C                              EQU     TCR0
2258   
2259                                TIMER_STORE_INIT
2260      P:000646 P:000648 50F400            MOVE              #>TIMER_BUFFER,A0
                            201000
2261      P:000648 P:00064A 000000            NOP
2262      P:000649 P:00064B 507000            MOVE              A0,X:TIMER_INDEX
                            00004C
2263      P:00064B P:00064D 211400            MOVE              A0,R4
2264      P:00064C P:00064E 00000C            RTS
2265   
2266                                TIMER_STORE
2267      
2268      
2269      P:00064D P:00064F 56F000            MOVE              X:TIMER_SOURCE,A
                            FFFF8C
2270                                                                                    ; Fall-through
2271   
2272                                TIMER_STORE_A1
2273      
2274      P:00064F P:000651 5C5C00            MOVE                          A1,Y:(R4)+
2275      P:000650 P:000652 228C00            MOVE              R4,A1
2276      P:000651 P:000653 0140C5            CMP     #>TIMER_BUFFER_END,A
                            202000
2277      P:000653 P:000655 547000            MOVE              A1,X:TIMER_INDEX
                            00004C
2278      P:000655 P:000657 0E1646            JGE     TIMER_STORE_INIT
2279      P:000656 P:000658 00000C            RTS
2280   
2281   
2282                                ;----------------------------------------------;
2283                                ;  CIRCULAR BUFFER HANDLING                    ;
2284                                ;----------------------------------------------;
2285   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 47



2286                                BUFFER_INCR
2287   
2288      P:000657 P:000659 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
                            000042
2289      P:000659 P:00065B 014180            ADD     #1,A                              ;
2290      P:00065A P:00065C 57F000            MOVE              X:QT_BUF_MAX,B          ;
                            00003F
2291      P:00065C P:00065E 20000D            CMP     A,B                               ;
2292      P:00065D P:00065F 0AF0AF            JLE     BUFFER_RESET                      ;       head = 0
                            000669
2293                                                                                    ; else
2294      P:00065F P:000661 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            000042
2295   
2296      P:000661 P:000663 20001B            CLR     B
2297      P:000662 P:000664 51F000            MOVE              X:QT_BUF_SIZE,B0
                            00003E
2298      P:000664 P:000666 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2299      P:000666 P:000668 0BF080            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
                            0006A1
2300   
2301      P:000668 P:00066A 00000C            RTS
2302   
2303   
2304                                BUFFER_RESET
2305      P:000669 P:00066B 60F400            MOVE              #QT_BASE_LO,R0
                            00003C
2306      P:00066B P:00066D 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
2307      P:00066D P:00066F 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2308      P:00066F P:000671 0BF080            JSR     SAVE_HILO_ADDRESS                 ; QT_DEST_LO = QT_BASE_LO
                            0006A3
2309   
2310      P:000671 P:000673 240000            MOVE              #0,X0
2311      P:000672 P:000674 447000            MOVE              X0,X:QT_BUF_HEAD        ; HEAD = 0
                            000042
2312      P:000674 P:000676 00000C            RTS
2313   
2314   
2315                                BUFFER_INFORM_CHECK
2316      P:000675 P:000677 56F000            MOVE              X:QT_INFORM_IDX,A
                            000046
2317      P:000677 P:000679 014180            ADD     #1,A
2318      P:000678 P:00067A 57F000            MOVE              X:QT_INFORM,B
                            000041
2319      P:00067A P:00067C 20000D            CMP     A,B
2320      P:00067B P:00067D 0AF0A7            JGT     BUFFER_INFORM_OK                  ; If inform_idx + 1 <= inform
                            00067F
2321      P:00067D P:00067F 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2322   
2323                                BUFFER_INFORM_OK
2324      P:00067F P:000681 567000            MOVE              A,X:QT_INFORM_IDX       ; inform_idx = inform_idx + 1
                            000046
2325      P:000681 P:000683 00000C            RTS
2326   
2327   
2328                                ;---------------------------------------------------------------
2329                                BUFFER_INFORM
2330                                ;---------------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 48



2331                                ; Informs host of current buffer status
2332   
2333      
2334      P:000682 P:000684 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            00069A
2335      P:000684 P:000686 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            00069A
2336   
2337                                          PCI_LOCKDOWN                              ; Disable host IRQ
2339   
2340      P:000687 P:000689 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2341      P:000689 P:00068B 440B00            MOVE              X0,X:<DTXS_WD1
2342   
2343      P:00068A P:00068C 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            000042
2344      P:00068C P:00068E 440C00            MOVE              X0,X:<DTXS_WD2
2345   
2346      P:00068D P:00068F 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000043
2347      P:00068F P:000691 440D00            MOVE              X0,X:<DTXS_WD3
2348   
2349      P:000690 P:000692 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
                            000047
2350      P:000692 P:000694 440E00            MOVE              X0,X:<DTXS_WD4
2351   
2352      P:000693 P:000695 0D047A            JSR     PCI_MESSAGE_TO_HOST
2353   
2354      P:000694 P:000696 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
2355      P:000696 P:000698 240000            MOVE              #0,X0                   ; Reset inform index
2356      P:000697 P:000699 447000            MOVE              X0,X:QT_INFORM_IDX
                            000046
2357                                          PCI_LOCKUP                                ; Enable host IRQ
2359                                INFORM_EXIT
2360      P:00069A P:00069C 00000C            RTS
2361   
2362   
2363   
2364                                ;----------------------------------------------;
2365                                ;  ADDRESS HANDLING                            ;
2366                                ;----------------------------------------------;
2367   
2371   
2372                                LOAD_HILO_ADDRESS
2373      
2374      
2375      P:00069B P:00069D 200013            CLR     A
2376      P:00069C P:00069E 50D800            MOVE              X:(R0)+,A0
2377      P:00069D P:00069F 44D000            MOVE              X:(R0)-,X0
2378      P:00069E P:0006A0 0C1940            INSERT  #$010010,X0,A
                            010010
2379      P:0006A0 P:0006A2 00000C            RTS
2380   
2381                                ADD_HILO_ADDRESS
2382      
2383      
2384   
2385      P:0006A1 P:0006A3 0D069B            JSR     LOAD_HILO_ADDRESS
2386      P:0006A2 P:0006A4 200010            ADD     B,A
2387   
2388                                SAVE_HILO_ADDRESS
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  main.asm  Page 49



2389      
2390      
2391   
2392      P:0006A3 P:0006A5 445800            MOVE              X0,X:(R0)+              ; pre-increment
2393      P:0006A4 P:0006A6 240000            MOVE              #0,X0
2394      P:0006A5 P:0006A7 0C1D11            ASL     #8,A,B
2395      P:0006A6 P:0006A8 0C1940            INSERT  #$008010,X0,A
                            008010
2396      P:0006A8 P:0006AA 555000            MOVE              B1,X:(R0)-              ; store hi16
2397      P:0006A9 P:0006AB 506000            MOVE              A0,X:(R0)
2398      P:0006AA P:0006AC 0C1C90            ASR     #8,B,A
2399      P:0006AB P:0006AD 00000C            RTS
2400   
2401   
2402   
2403   
2404                                BOOTCODE_END
2405                                 BOOTEND_ADDR
2406      0006AC                              EQU     @CVI(BOOTCODE_END)
2407   
2408                                PROGRAM_END
2409      0006AC                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2410                                          INCLUDE 'vars.asm'
2411                                      COMMENT *
2412   
2413                                Variable table and bit defines for our variables.
2414   
2415                                See info.asm for versioning and authors.
2416   
2417                                        *
2418   
2419   
2420                                ; The variable table is mapped to X memory but stored inline in the
2421                                ; eeprom / P memory after the main code (but before the application
2422                                ; area).
2423   
2424      X:000000 P:0006AE                   ORG     X:VAR_TBL,P:
2425   
2426   
2427                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2428                                 VAR_TBL_START
2429      0006AC                              EQU     @LCV(L)-2
2430                                          ENDIF
2431   
2432                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2434                                          ENDIF
2435   
2436   
2437                                ;-----------------------------------------------
2438 d    X:000000 P:0006AE 000000  STATUS    DC      0                                 ; Internal status flags
2439 d    X:000001 P:0006AF 000000  MODE      DC      0                                 ; Operating mode control
2440 d                               FRAME_COUNT
2441 d    X:000002 P:0006B0 000000            DC      0                                 ; Count of data frames from MCE
2442   
2443                                ;-----------------------------------------------
2444 d    X:000003 P:0006B1 550106  REV_NUMBER DC     $550106                           ; byte 0 = minor revision #
2445                                                                                    ; byte 1 = major revision #
2446                                                                                    ; byte 2 = release Version (ascii letter)
2447 d    X:000004 P:0006B2 000000  REV_DATA  DC      $000000                           ; Not used by UBC
2448 d    X:000005 P:0006B3 2EF490  P_CHECKSUM DC     $2EF490                           ; Not used by UBC
2449   
2450                                ;-----------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  vars.asm  Page 50



2451 d    X:000006 P:0006B4 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2452                                ;-----------------------------------------------
2453   
2454   
2456   
2457 d    X:000007 P:0006B5 000000  DRXR_WD1  DC      0                                 ; Storage for words read from PC during vect
or command
2458 d    X:000008 P:0006B6 000000  DRXR_WD2  DC      0
2459 d    X:000009 P:0006B7 000000  DRXR_WD3  DC      0
2460 d    X:00000A P:0006B8 000000  DRXR_WD4  DC      0
2461   
2462 d    X:00000B P:0006B9 000000  DTXS_WD1  DC      0                                 ; Storage for words to be written to PC as r
eply
2463 d    X:00000C P:0006BA 000000  DTXS_WD2  DC      0
2464 d    X:00000D P:0006BB 000000  DTXS_WD3  DC      0
2465 d    X:00000E P:0006BC 000000  DTXS_WD4  DC      0
2466   
2467   
2469   
2470 d    X:00000F P:0006BD 000000  SV_A0     DC      0
2471 d    X:000010 P:0006BE 000000  SV_A1     DC      0
2472 d    X:000011 P:0006BF 000000  SV_A2     DC      0
2473 d    X:000012 P:0006C0 000000  SV_B0     DC      0
2474 d    X:000013 P:0006C1 000000  SV_B1     DC      0
2475 d    X:000014 P:0006C2 000000  SV_B2     DC      0
2476 d    X:000015 P:0006C3 000000  SV_X0     DC      0
2477 d    X:000016 P:0006C4 000000  SV_X1     DC      0
2478 d    X:000017 P:0006C5 000000  SV_Y0     DC      0
2479 d    X:000018 P:0006C6 000000  SV_Y1     DC      0
2480 d    X:000019 P:0006C7 000000  SV_R0     DC      0
2481 d    X:00001A P:0006C8 000000  SV_SR     DC      0
2482   
2483   
2485   
2486 d    X:00001B P:0006C9 000000  HEAD_W1_0 DC      0                                 ; Preamble $A5A5
2487 d    X:00001C P:0006CA 000000  HEAD_W1_1 DC      0                                 ;          $A5A5
2488 d    X:00001D P:0006CB 000000  HEAD_W2_0 DC      0                                 ;          $5A5A
2489 d    X:00001E P:0006CC 000000  HEAD_W2_1 DC      0                                 ;          $5A5A
2490 d    X:00001F P:0006CD 000000  HEAD_W3_0 DC      0                                 ; 'RP' or 'DA'
2491 d    X:000020 P:0006CE 000000  HEAD_W3_1 DC      0                                 ; '  '   $2020
2492 d    X:000021 P:0006CF 000000  HEAD_W4_0 DC      0                                 ; Packet size LSW
2493 d    X:000022 P:0006D0 000000  HEAD_W4_1 DC      0                                 ;             MSW
2494   
2495   
2497   
2498 d                               PACKET_SIZE
2499 d    X:000023 P:0006D1 000000            DC      0                                 ; Size, in dwords of most recent packet from
 MCE.
2500 d                               TOTAL_BUFFS
2501 d    X:000024 P:0006D2 000000            DC      0                                 ; Number of 512 word half-buffers in packet.
2502 d                               LEFT_TO_READ
2503 d    X:000025 P:0006D3 000000            DC      0                                 ; Number of words left to read after last 51
2 buffer
2504   
2505 d                               PREAMBLE_ERRORS
2506 d    X:000026 P:0006D4 000000            DC      0                                 ; Failed on preamble processing
2507 d                               PTYPE_ERRORS
2508 d    X:000027 P:0006D5 000000            DC      0                                 ; Failed on packet type
2509 d                               PSIZE_ERRORS
2510 d    X:000028 P:0006D6 000000            DC      0                                 ; Failed on packet size test
2511   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  vars.asm  Page 51



2512   
2514   
2515 d                               PCI_BURST_SIZE
2516 d    X:000029 P:0006D7 000040            DC      $40                               ; Should be < 4*latency assigned by OS
2517 d    X:00002A P:0006D8 000000  BURST_SIZE DC     0
2518 d    X:00002B P:0006D9 000000  BLOCK_SIZE DC     0
2519   
2520 d    X:00002C P:0006DA 000000  CON_SRC_LO DC     0                                 ; Set by CON host command
2521 d    X:00002D P:0006DB 000000  CON_SRC_HI DC     0
2522   
2523 d    X:00002E P:0006DC 000000  YMEM_SRC  DC      0                                 ; Vars for YMEM -> PC transfers
2524 d                               BURST_DEST_LO
2525 d    X:00002F P:0006DD 000000            DC      0
2526 d                               BURST_DEST_HI
2527 d    X:000030 P:0006DE 000000            DC      0
2528   
2529 d                               BURST_SRC_LO
2530 d    X:000031 P:0006DF 000000            DC      0                                 ; Vars for PC -> YMEM transfers
2531 d                               BURST_SRC_HI
2532 d    X:000032 P:0006E0 000000            DC      0
2533 d    X:000033 P:0006E1 000000  YMEM_DEST DC      0
2534   
2535 d    X:000034 P:0006E2 000000  DMA_ERRORS DC     0                                 ; Error counting
2536 d    X:000035 P:0006E3 000000  EC_TRTY   DC      0
2537 d    X:000036 P:0006E4 000000  EC_TO     DC      0
2538 d    X:000037 P:0006E5 000000  EC_TDIS   DC      0
2539 d    X:000038 P:0006E6 000000  EC_TAB    DC      0
2540 d    X:000039 P:0006E7 000000  EC_MAB    DC      0
2541 d    X:00003A P:0006E8 000000  EC_DPER   DC      0
2542 d    X:00003B P:0006E9 000000  EC_APER   DC      0
2543   
2544   
2546   
2547 d    X:00003C P:0006EA 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2548 d    X:00003D P:0006EB 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2549 d                               QT_BUF_SIZE
2550 d    X:00003E P:0006EC 000000            DC      0                                 ; Separation of buffers, in bytes
2551 d    X:00003F P:0006ED 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2552 d                               QT_FRAME_SIZE
2553 d    X:000040 P:0006EE 000000            DC      0                                 ; Expected data packet size, in bytes
2554 d    X:000041 P:0006EF 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2555   
2556 d                               QT_BUF_HEAD
2557 d    X:000042 P:0006F0 000000            DC      0                                 ; Index of buf for next write
2558 d                               QT_BUF_TAIL
2559 d    X:000043 P:0006F1 000000            DC      0                                 ; Index at which we must not write
2560   
2561 d    X:000044 P:0006F2 000000  QT_DEST_LO DC     0                                 ; PC address for next write
2562 d    X:000045 P:0006F3 000000  QT_DEST_HI DC     0                                 ;
2563 d                               QT_INFORM_IDX
2564 d    X:000046 P:0006F4 000000            DC      0                                 ; Number of packets since last inform
2565 d    X:000047 P:0006F5 000000  QT_DROPS  DC      0                                 ; Dropped packet count
2566   
2567   
2569   
2570 d    X:000048 P:0006F6 000000  RP_BASE_LO DC     0                                 ; PC buffer start address
2571 d    X:000049 P:0006F7 000000  RP_BASE_HI DC     0                                 ;
2572 d                               RP_MAX_SIZE
2573 d    X:00004A P:0006F8 000000            DC      0                                 ; Maximum reply size, dwords
2574 d    X:00004B P:0006F9 000000  RP_DROPS  DC      0                                 ; Dropped packet count
2575   
2576   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  vars.asm  Page 52



2578   
2579 d                               TIMER_INDEX
2580 d    X:00004C P:0006FA 000000            DC      0
2581   
2582   
2584   
2585 d    X:00004D P:0006FB 000000  BDEBUG0   DC      0
2586 d    X:00004E P:0006FC 000000  BDEBUG1   DC      0
2587 d    X:00004F P:0006FD 000000  BDEBUG2   DC      0
2595   
2597 d                               TRIGGER_FAKE
2598 d    X:000050 P:0006FE 000000            DC      0
2599   
2600 d    X:000051 P:0006FF 000000  CMD_SIZE  DC      0
2601 d    X:000052 P:000700 000000  CMD_WORD  DC      0
2602   
2603 d                               REP_BUS_ADDR
2604 d    X:000053 P:000701 000000            DC      0,0
     d                      000000
2605   
2606 d    X:000055 P:000703 000000  XMEM_SRC  DC      0
2607   
2608      000100                    CMD_BUFFER EQU    $100
2609   
2610   
2616                                 REP_BUFFER_SIZE
2617      000040                              EQU     64                                ; This MUST be even, so that effective numbe
r
2618                                                                                    ; of 32-bit words is integral
2619                                 REP_BUFFER1
2620      X:000056 P:000704                   DS      REP_BUFFER_SIZE
2621   
2622      000000                    RB_VERSION EQU    0
2623      000001                    RB_SIZE   EQU     1
2624      000010                    RB_DATA   EQU     16
2625   
2627      000066                    REP_RSTAT EQU     REP_BUFFER1+RB_DATA
2628      000067                    REP_RSIZE EQU     REP_BUFFER1+RB_DATA+1
2629                                 REP_RPAYLOAD
2630      000068                              EQU     REP_BUFFER1+RB_DATA+2
2631   
2632   
2633   
2634                                ;----------------------------------------------------------
2635   
2637   
2638                                 APPLICATION_RUNNING
2639      000000                              EQU     0                                 ; Indicates application is in progress
2640                                 SEND_TO_HOST
2641      000001                              EQU     1                                 ; set in HST ISR when host ready for packet 
(stays set until after HST reply)
2642                                 FATAL_ERROR
2643      000002                              EQU     2                                 ; PCI message to host error detected by driv
er....
2644      000003                    FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays s
et till packet processed
2645   
2646      000009                    HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of p
acket (stays set until after HST reply)
2647   
2648      00000A                    CON_DEMAND EQU    10                                ; Host has requested an MCE command be sent
2649      00000B                    CON_MCE   EQU     11                                ; Command has been copied to Y buffer and sh
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  vars.asm  Page 53



ould be sent to MCE
2650   
2651                                 PCIDMA_RESTART
2652      000010                              EQU     16                                ; DMA flags used for error recovery
2653                                 PCIDMA_RESUME
2654      000011                              EQU     17
2655   
2656      000014                    QT_FLUSH  EQU     20                                ; Set when it is time to inform Host of curr
ent buffer position.
2657                                 RP_BUFFER_FULL
2658      000015                              EQU     21                                ; Set when Quiet RP buffer is occupied.
2659   
2660      000016                    FREEZER   EQU     22                                ; Suspend operations and just idle in the ma
in loop
2661                                 MAIN_LOOP_POLL
2662      000017                              EQU     23                                ; Cleared by the main loop, use to check for
 DSP lock-up
2663   
2664   
2666   
2667                                 MODE_APPLICATION
2668      000000                              EQU     0                                 ; set if PCI application to run
2669      000001                    MODE_MCE  EQU     1                                 ; process packets from MCE (!choke)
2670      000002                    MODE_QT   EQU     2                                 ; Quiet transfer for data packets (QT mode)
2671                                 MODE_RP_BUFFER
2672      000003                              EQU     3                                 ; Quiet transfer for reply packets (Quiet-RP
)
2673   
2674   
2676   
2677                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2678                                 VAR_TBL_END
2679      000742                              EQU     @LCV(L)-2
2680                                          ENDIF
2681   
2682                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2684                                          ENDIF
2685   
2686                                 VAR_TBL_LENGTH
2687      000096                              EQU     VAR_TBL_END-VAR_TBL_START
2688                                          INCLUDE 'app.asm'
2689                                        COMMENT *
2690   
2691                                Auxiliary application area.
2692   
2693                                See info.asm for versioning and authors.
2694   
2695                                        *
2696                                          PAGE    132                               ; Printronix page width - 132 columns
2697                                          OPT     CEX                               ; print DC evaluations
2698   
2699                                          IF      @CVS(N,*)>=APPLICATION
2701                                          ENDIF
2702   
2703   
2704                                ;--------------------------------------------
2705                                ; APPLICATION AREA
2706                                ;---------------------------------------------
2707                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2708      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2709                                          ENDIF
2710   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  app.asm  Page 54



2711                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2713                                          ENDIF
2714   
2715                                ; starts with no application loaded
2716                                ; so just reply with an error if we get a GOA command
2717   
2718      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2719      P:000802 P:000804 440B00            MOVE              X0,X:<DTXS_WD1          ; REPly
2720      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2721      P:000805 P:000807 440C00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2722      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2723      P:000808 P:00080A 440D00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2724      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2725      P:00080B P:00080D 440E00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2726      P:00080C P:00080E 0D0493            JSR     <RESTORE_REGISTERS
2727      P:00080D P:00080F 0D047A            JSR     <PCI_MESSAGE_TO_HOST
2728      P:00080E P:000810 0A0100            BCLR    #MODE_APPLICATION,X:<MODE
2729      P:00080F P:000811 0C016E            JMP     PACKET_IN
2730   
2731   
2732   
2733                                          INCLUDE 'hacking.asm'
2734                                                COMMENT *
2735   
2736                                        This implementation does communication with the host using PCI
2737                                        master writes only.
2738   
2739                                        *
2740                                          PAGE    132                               ; Printronix page width - 132 columns
2741                                          OPT     CEX                               ; print DC evaluations
2742   
2743   
2744                                HACK_ENTRY
2745      
2746      P:000810 P:000812 0A8985            JCLR    #DSR_HF2,X:DSR,HACK_EXIT
                            000821
2747      
2748      P:000812 P:000814 0A8524            BSET    #DCTR_HF4,X:DCTR
2749   
2750      
2751      P:000813 P:000815 200013            CLR     A
2752      P:000814 P:000816 000000            NOP
2753      P:000815 P:000817 507000            MOVE              A0,X:CMD_WORD
                            000052
2754      P:000817 P:000819 507000            MOVE              A0,X:TRIGGER_FAKE
                            000050
2755   
2756      P:000819 P:00081B 0BF080            JSR     REPLY_BUFFER_INIT
                            000823
2757   
2758      
2759      
2760      
2761                                HACK_LOOP
2762      
2763      P:00081B P:00081D 0BF080            JSR     PROCESS_PC_CMD
                            00092E
2764   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  hacking.asm  Page 55



2765      
2766      P:00081D P:00081F 0BF080            JSR     PROCESS_REPLY
                            000900
2767   
2768      
2769      
2770   
2771      
2772      P:00081F P:000821 0A89A5            JSET    #DSR_HF2,X:DSR,HACK_LOOP
                            00081B
2773   
2774                                HACK_EXIT
2775      P:000821 P:000823 0A8504            BCLR    #DCTR_HF4,X:DCTR
2776      P:000822 P:000824 00000C            RTS
2777   
2778   
2779                                REPLY_BUFFER_INIT
2780      
2781      P:000823 P:000825 305600            MOVE              #REP_BUFFER1,R0
2782      P:000824 P:000826 44F400            MOVE              #>1,X0
                            000001
2783      P:000826 P:000828 50F400            MOVE              #>REP_BUFFER_SIZE,A0
                            000040
2784      P:000828 P:00082A 020084            MOVE              X0,X:(R0+RB_VERSION)
2785      P:000829 P:00082B 0200C8            MOVE              A0,X:(R0+RB_SIZE)
2786      P:00082A P:00082C 44F400            MOVE              #>0,X0
                            000000
2787      P:00082C P:00082E 447000            MOVE              X0,X:REP_RSTAT
                            000066
2788      P:00082E P:000830 00000C            RTS
2789   
2790   
2791                                ;----------------------------------------------
2792                                BLOCK_TRANSFERX
2793                                ;----------------------------------------------
2794                                ;   In:
2795                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address (16:16)
2796                                ;   - BLOCK_SIZE is packet size, in bytes
2797                                ;   - XMEM_SRC is start of data in X memory
2798                                ;  Out:
2799                                ;   - BLOCK_SIZE will be decremented to zero.
2800                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
2801                                ;   - XMEM_SRC will be incremented by BLOCK_SIZE/2
2802                                ;  Trashes:
2803                                ;   - A and B at least
2804   
2805      P:00082F P:000831 200013            CLR     A
2806      P:000830 P:000832 56AB00            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
2807      P:000831 P:000833 014085            CMP     #0,A                              ; Still bytes to transfer?
2808      P:000832 P:000834 0AF0A2            JNE     BLOCK_TRANSFERX0
                            000835
2809      P:000834 P:000836 00000C            RTS
2810   
2811                                BLOCK_TRANSFERX0
2812      
2813      
2814      P:000835 P:000837 57A900            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
2815   
2816      P:000836 P:000838 200005            CMP     B,A                               ; A ? B
2817      P:000837 P:000839 0E1839            JGE     <BLOCK_TRANSFERX1                 ; jump if A >= B
2818      P:000838 P:00083A 21CF00            MOVE              A,B                     ; This only moves A1,B1.
2819                                BLOCK_TRANSFERX1
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  hacking.asm  Page 56



2820      
2821      P:000839 P:00083B 200014            SUB     B,A                               ; A -= B
2822      P:00083A P:00083C 014088            ADD     #0,B                              ; Clear carry bit
2823      P:00083B P:00083D 562B00            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
2824      P:00083C P:00083E 572A00            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
2825      P:00083D P:00083F 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
2826   
2827      
2828      P:00083E P:000840 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
2829      P:000840 P:000842 50F000            MOVE              X:XMEM_SRC,A0
                            000055
2830      P:000842 P:000844 08C82F            MOVEP             A0,X:DSR0               ; DMA source
2831      P:000843 P:000845 200010            ADD     B,A
2832      P:000844 P:000846 00000B            DEC     B
2833      P:000845 P:000847 507000            MOVE              A0,X:XMEM_SRC           ; BURST_SRC += BURST_SIZE/2
                            000055
2834   
2835      P:000847 P:000849 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
2836   
2837      
2838      P:000848 P:00084A 08F4AC            MOVEP             #$8EFA50,X:DCR0         ; X to X
                            8EFA50
2839   
2840                                BLOCK_TRANSFERX_PCI
2841      P:00084A P:00084C 44F400            MOVE              #>$7,X0                 ; Memory write
                            000007
2842      P:00084C P:00084E 302F00            MOVE              #BURST_DEST_LO,R0       ; RAM address
2843      P:00084D P:00084F 0D058D            JSR     PCI_GO                            ; Initiate PCI burst
2844   
2845      
2846      P:00084E P:000850 0A8A84            JCLR    #MARQ,X:DPSR,*
                            00084E
2847   
2848      
2849      P:000850 P:000852 0A8A8E            JCLR    #MDT,X:DPSR,BLOCK_TRANSFERX_HANDLE_ERRORS
                            000856
2850   
2851      P:000852 P:000854 20001B            CLR     B
2852      P:000853 P:000855 51AA00            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
2853      P:000854 P:000856 0D06A1            JSR     ADD_HILO_ADDRESS                  ; Update source address
2854      P:000855 P:000857 0C082F            JMP     BLOCK_TRANSFERX                   ; Next burst in block
2855   
2856                                BLOCK_TRANSFERX_HANDLE_ERRORS
2857      
2858      P:000856 P:000858 0D04BE            JSR     PCI_ERROR_CLEAR
2859   
2860      P:000857 P:000859 0A0010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
2861      P:000858 P:00085A 0E884A            JCS     BLOCK_TRANSFERX_PCI               ; Restart PCI burst
2862   
2863      P:000859 P:00085B 0A0011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
2864      P:00085A P:00085C 0E082F            JCC     BLOCK_TRANSFERX                   ; Error but no error? Redo this burst.
2865   
2866      
2867      P:00085B P:00085D 0D05A3            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
2868      P:00085C P:00085E 0D05B3            JSR     PCI_UPDATE_R0
2869      P:00085D P:00085F 0C084A            JMP     BLOCK_TRANSFERX_PCI
2870   
2871   
2873      P:000900 P:000902                   ORG     P:$900,P:$902
2874   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  hacking.asm  Page 57



2875                                PROCESS_REPLY
2876      P:000900 P:000902 200013            CLR     A
2877      P:000901 P:000903 56F000            MOVE              X:REP_RSTAT,A
                            000066
2878      P:000903 P:000905 014085            CMP     #0,A
2879      P:000904 P:000906 0AF0A2            JNE     PROCESS_REPLY_1
                            000907
2880      P:000906 P:000908 00000C            RTS
2881   
2882                                PROCESS_REPLY_1
2883      
2884      P:000907 P:000909 60F400            MOVE              #>REP_BUS_ADDR,R0
                            000053
2885      P:000909 P:00090B 61F400            MOVE              #>BURST_DEST_LO,R1
                            00002F
2886                                          .loop   #2
2888      P:00090D P:00090F 44D800            MOVE              X:(R0)+,X0
2889      P:00090E P:000910 445900            MOVE              X0,X:(R1)+
2890                                          .endl
2892   
2893      P:00090F P:000911 44F400            MOVE              #>(REP_BUFFER_SIZE*2),X0
                            000080
2894      P:000911 P:000913 442B00            MOVE              X0,X:BLOCK_SIZE
2895      P:000912 P:000914 44F400            MOVE              #>REP_BUFFER1,X0
                            000056
2896      P:000914 P:000916 447000            MOVE              X0,X:XMEM_SRC
                            000055
2897   
2898      
2899      P:000916 P:000918 0D082F            JSR     BLOCK_TRANSFERX
2900   
2901      P:000917 P:000919 44F400            MOVE              #>0,X0
                            000000
2902      P:000919 P:00091B 447000            MOVE              X0,X:REP_RSTAT          ; mark as sent!
                            000066
2903   
2904      
2905      P:00091B P:00091D 0A8526            BSET    #INTA,X:DCTR
2906   
2907      P:00091C P:00091E 0A8983            JCLR    #DSR_HF0,X:DSR,*
                            00091C
2908      P:00091E P:000920 0A8506            BCLR    #INTA,X:DCTR
2909   
2910      P:00091F P:000921 0A89A3            JSET    #DSR_HF0,X:DSR,*
                            00091F
2911   
2912      P:000921 P:000923 00000C            RTS
2913   
2914                                TOGGLED_HANDLER_WHY_DOES_THIS_NOT_WORK_QUESTION
2916      P:000922 P:000924 0A8983            JCLR    #DSR_HF0,X:DSR,INT_WAIT_SET
                            000929
2917   
2918                                INT_WAIT_CLR
2919      P:000924 P:000926 0A8526            BSET    #INTA,X:DCTR                      ; Assert interrupt
2920      P:000925 P:000927 0A89A3            JSET    #DSR_HF0,X:DSR,*
                            000925
2921      P:000927 P:000929 0A8506            BCLR    #INTA,X:DCTR
2922      P:000928 P:00092A 00000C            RTS
2923                                INT_WAIT_SET
2924      P:000929 P:00092B 0A8526            BSET    #INTA,X:DCTR                      ; Assert interrupt
2925      P:00092A P:00092C 0A8983            JCLR    #DSR_HF0,X:DSR,*
                            00092A
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  hacking.asm  Page 58



2926      P:00092C P:00092E 0A8506            BCLR    #INTA,X:DCTR
2927      P:00092D P:00092F 00000C            RTS
2928   
2932   
2934      000001                    CMD_READ_P EQU    1
2935      000002                    CMD_READ_X EQU    2
2936      000003                    CMD_READ_Y EQU    3
2937   
2938                                 CMD_WRITE_P
2939      000005                              EQU     5
2940                                 CMD_WRITE_X
2941      000006                              EQU     6
2942                                 CMD_WRITE_Y
2943      000007                              EQU     7
2944   
2945                                 CMD_SET_REP_BUF
2946      000009                              EQU     9
2947   
2948                                 CMD_READ_CODED
2949      000011                              EQU     $11
2950                                 CMD_WRITE_CODED
2951      000012                              EQU     $12
2952   
2953                                 CMD_SEND_MCE
2954      000021                              EQU     $21
2955   
2956                                 CMD_SEND_STUFF
2957      000031                              EQU     $31
2958   
2959      000065                    CMD_STATUS EQU    $65
2960                                 CMD_RECV_MCE
2961      000066                              EQU     $66
2962   
2963   
2964   
2965                                PROCESS_PC_CMD
2966      
2967      P:00092E P:000930 0A89A2            JSET    #SRRQ,X:DSR,PROCESS_PC_CMD_1
                            000931
2968      P:000930 P:000932 00000C            RTS
2969   
2970                                PROCESS_PC_CMD_1
2971      
2972      P:000931 P:000933 08440B            MOVEP             X:DRXR,X0
2973      P:000932 P:000934 305100            MOVE              #CMD_SIZE,R0
2974      P:000933 P:000935 0BF080            JSR     PROCESS_SPLIT_X0_XR0
                            0009C9
2975   
2976      
2977      P:000935 P:000937 200013            CLR     A
2978      P:000936 P:000938 60F400            MOVE              #CMD_BUFFER,R0
                            000100
2979      P:000938 P:00093A 54F000            MOVE              X:CMD_SIZE,A1
                            000051
2980      
2981      P:00093A P:00093C 014085            CMP     #0,A
2982      P:00093B P:00093D 0AF0AA            JEQ     PROCESS_PC_CMD_2
                            000944
2983                                          .loop   A1
2985      P:00093F P:000941 0A8982            JCLR    #SRRQ,X:DSR,*
                            00093F
2986      P:000941 P:000943 08588B            MOVEP             X:DRXR,X:(R0)+
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  hacking.asm  Page 59



2987      P:000942 P:000944 000000            NOP
2988      P:000943 P:000945 000000            NOP
2989                                          .endl
2991   
2992                                PROCESS_PC_CMD_2
2993      
2994      P:000944 P:000946 57F000            MOVE              X:CMD_WORD,B
                            000052
2995   
2996      
2997      P:000946 P:000948 60F000            MOVE              X:CMD_BUFFER,R0
                            000100
2998      P:000948 P:00094A 44F000            MOVE              X:(CMD_BUFFER+1),X0
                            000101
2999   
3000      P:00094A P:00094C 01418D            CMP     #CMD_READ_P,B
3001      P:00094B P:00094D 0AF0AA            JEQ     PROCESS_READ_P
                            000966
3002   
3003      P:00094D P:00094F 01428D            CMP     #CMD_READ_X,B
3004      P:00094E P:000950 0AF0AA            JEQ     PROCESS_READ_X
                            000969
3005   
3006      P:000950 P:000952 01438D            CMP     #CMD_READ_Y,B
3007      P:000951 P:000953 0AF0AA            JEQ     PROCESS_READ_Y
                            00096C
3008   
3009      P:000953 P:000955 01458D            CMP     #CMD_WRITE_P,B
3010      P:000954 P:000956 0AF0AA            JEQ     PROCESS_WRITE_P
                            00097A
3011   
3012      P:000956 P:000958 01468D            CMP     #CMD_WRITE_X,B
3013      P:000957 P:000959 0AF0AA            JEQ     PROCESS_WRITE_X
                            00097D
3014   
3015      P:000959 P:00095B 01478D            CMP     #CMD_WRITE_Y,B
3016      P:00095A P:00095C 0AF0AA            JEQ     PROCESS_WRITE_Y
                            000980
3017   
3018      P:00095C P:00095E 01618D            CMP     #CMD_SEND_MCE,B
3019      P:00095D P:00095F 0AF0AA            JEQ     PROCESS_SEND_MCE
                            0009A2
3020   
3021      P:00095F P:000961 01498D            CMP     #CMD_SET_REP_BUF,B
3022      P:000960 P:000962 0AF0AA            JEQ     PROCESS_SET_BUFFER
                            000983
3023   
3024      P:000962 P:000964 01718D            CMP     #CMD_SEND_STUFF,B
3025      P:000963 P:000965 0AF0AA            JEQ     PROCESS_SEND_STUFF
                            000999
3026   
3027   
3028      
3029      P:000965 P:000967 00000C            RTS
3030   
3031                                PROCESS_READ_P
3032      P:000966 P:000968 07E084            MOVE              P:(R0),X0
3033      P:000967 P:000969 0AF080            JMP     PROCESS_READ_EXIT
                            00096D
3034                                PROCESS_READ_X
3035      P:000969 P:00096B 44E000            MOVE              X:(R0),X0
3036      P:00096A P:00096C 0AF080            JMP     PROCESS_READ_EXIT
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  hacking.asm  Page 60



                            00096D
3037                                PROCESS_READ_Y
3038      P:00096C P:00096E 4CE000            MOVE                          Y:(R0),X0
3039      
3040      
3041                                PROCESS_READ_EXIT
3042      
3043      P:00096D P:00096F 60F400            MOVE              #>REP_RPAYLOAD,R0
                            000068
3044      P:00096F P:000971 0BF080            JSR     PROCESS_SPLIT_X0_XR0
                            0009C9
3045      
3046      P:000971 P:000973 44F000            MOVE              X:CMD_WORD,X0
                            000052
3047      P:000973 P:000975 447000            MOVE              X0,X:REP_RSTAT
                            000066
3048      P:000975 P:000977 44F400            MOVE              #>1,X0
                            000001
3049      P:000977 P:000979 447000            MOVE              X0,X:REP_RSIZE
                            000067
3050      P:000979 P:00097B 00000C            RTS
3051   
3052                                PROCESS_WRITE_P
3053      P:00097A P:00097C 076084            MOVE              X0,P:(R0)
3054      P:00097B P:00097D 0AF080            JMP     PROCESS_SIMPLE_EXIT
                            0009B0
3055                                PROCESS_WRITE_X
3056      P:00097D P:00097F 446000            MOVE              X0,X:(R0)
3057      P:00097E P:000980 0AF080            JMP     PROCESS_SIMPLE_EXIT
                            0009B0
3058                                PROCESS_WRITE_Y
3059      P:000980 P:000982 4C6000            MOVE                          X0,Y:(R0)
3060      P:000981 P:000983 0AF080            JMP     PROCESS_SIMPLE_EXIT
                            0009B0
3061   
3062   
3063                                PROCESS_SET_BUFFER
3064      
3065      
3066      P:000983 P:000985 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3067      P:000985 P:000987 315300            MOVE              #REP_BUS_ADDR,R1
3068                                          .loop   #2
3070      P:000988 P:00098A 44D800            MOVE              X:(R0)+,X0
3071      P:000989 P:00098B 445900            MOVE              X0,X:(R1)+
3072                                          .endl
3074   
3075      
3076      P:00098A P:00098C 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3077      P:00098C P:00098E 61F400            MOVE              #>$100,R1
                            000100
3078                                          .loop   #2
3080      P:000990 P:000992 44D800            MOVE              X:(R0)+,X0
3081      P:000991 P:000993 4C5900            MOVE                          X0,Y:(R1)+
3082                                          .endl
3084   
3085      
3086      P:000992 P:000994 44F400            MOVE              #>0,X0
                            000000
3087      P:000994 P:000996 447000            MOVE              X0,X:REP_RSTAT
                            000066
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  hacking.asm  Page 61



3088      P:000996 P:000998 447000            MOVE              X0,X:REP_RSIZE
                            000067
3089      P:000998 P:00099A 00000C            RTS
3090   
3091                                PROCESS_SEND_STUFF
3092      
3093      P:000999 P:00099B 0A7021            BSET    #1,X:TRIGGER_FAKE
                            000050
3094   
3095      P:00099B P:00099D 44F400            MOVE              #>0,X0
                            000000
3096      P:00099D P:00099F 447000            MOVE              X0,X:REP_RSTAT
                            000066
3097      P:00099F P:0009A1 447000            MOVE              X0,X:REP_RSIZE
                            000067
3098      P:0009A1 P:0009A3 00000C            RTS
3099   
3100   
3101                                PROCESS_SEND_MCE
3102      
3103      
3104      
3105      P:0009A2 P:0009A4 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3106                                          .loop   #128
3108      P:0009A6 P:0009A8 54D800            MOVE              X:(R0)+,A1              ; b2, b1  (lsb)
3109      P:0009A7 P:0009A9 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
3110      P:0009A8 P:0009AA 0140C6            AND     #>$FF,A
                            0000FF
3111      P:0009AA P:0009AC 547000            MOVE              A1,X:FO_SEND
                            FFF000
3112      P:0009AC P:0009AE 557000            MOVE              B1,X:FO_SEND
                            FFF000
3113                                          .endl
3115      P:0009AE P:0009B0 0AF080            JMP     PROCESS_SIMPLE_EXIT
                            0009B0
3116   
3117                                PROCESS_SIMPLE_EXIT
3118      
3119      P:0009B0 P:0009B2 44F000            MOVE              X:CMD_WORD,X0
                            000052
3120      P:0009B2 P:0009B4 447000            MOVE              X0,X:REP_RSTAT
                            000066
3121      P:0009B4 P:0009B6 44F400            MOVE              #>0,X0
                            000000
3122      P:0009B6 P:0009B8 447000            MOVE              X0,X:REP_RSIZE
                            000067
3123      P:0009B8 P:0009BA 00000C            RTS
3124   
3125   
3126                                OLD_PROCESS_SPLIT_X0_XR0
3127      
3128      
3129      P:0009B9 P:0009BB 208800            MOVE              X0,A0
3130      P:0009BA P:0009BC 0C1881            EXTRACTU #$008010,A,B                     ; Put
                            008010
3131      P:0009BC P:0009BE 0C1880            EXTRACTU #$010000,A,A
                            010000
3132      P:0009BE P:0009C0 515800            MOVE              B0,X:(R0)+
3133      P:0009BF P:0009C1 505800            MOVE              A0,X:(R0)+
3134      P:0009C0 P:0009C2 00000C            RTS
3135   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  hacking.asm  Page 62



3136                                OLD_PROCESS_SPLIT_X0_YR0
3137      
3138      
3139      P:0009C1 P:0009C3 208800            MOVE              X0,A0
3140      P:0009C2 P:0009C4 0C1881            EXTRACTU #$008010,A,B                     ; Put
                            008010
3141      P:0009C4 P:0009C6 0C1880            EXTRACTU #$010000,A,A
                            010000
3142      P:0009C6 P:0009C8 595800            MOVE                          B0,Y:(R0)+
3143      P:0009C7 P:0009C9 585800            MOVE                          A0,Y:(R0)+
3144      P:0009C8 P:0009CA 00000C            RTS
3145   
3146                                PROCESS_SPLIT_X0_XR0
3147      
3148      
3149      P:0009C9 P:0009CB 208800            MOVE              X0,A0
3150      P:0009CA P:0009CC 0C1881            EXTRACTU #$010000,A,B
                            010000
3151      P:0009CC P:0009CE 0C1880            EXTRACTU #$008010,A,A                     ; Put
                            008010
3152      P:0009CE P:0009D0 515800            MOVE              B0,X:(R0)+
3153      P:0009CF P:0009D1 505800            MOVE              A0,X:(R0)+
3154      P:0009D0 P:0009D2 00000C            RTS
3155   
3156                                PROCESS_SPLIT_X0_YR0
3157      
3158      
3159      P:0009D1 P:0009D3 208800            MOVE              X0,A0
3160      P:0009D2 P:0009D4 0C1881            EXTRACTU #$010000,A,B
                            010000
3161      P:0009D4 P:0009D6 0C1880            EXTRACTU #$008010,A,A                     ; Put
                            008010
3162      P:0009D6 P:0009D8 595800            MOVE                          B0,Y:(R0)+
3163      P:0009D7 P:0009D9 585800            MOVE                          A0,Y:(R0)+
3164      P:0009D8 P:0009DA 00000C            RTS
3165   
3166   
3167   
3171   
3172                                FAKE_PACKET
3173      P:0009D9 P:0009DB 200013            CLR     A
3174      P:0009DA P:0009DC 54F000            MOVE              X:TRIGGER_FAKE,A1
                            000050
3175      P:0009DC P:0009DE 014085            CMP     #0,A
3176      P:0009DD P:0009DF 0AF0AA            JEQ     FAKE_PACKET_2
                            0009F5
3177   
3179      P:0009DF P:0009E1 0D0907            JSR     PROCESS_REPLY_1
3180      P:0009E0 P:0009E2 44F400            MOVE              #>0,X0
                            000000
3181      P:0009E2 P:0009E4 447000            MOVE              X0,X:TRIGGER_FAKE
                            000050
3182      P:0009E4 P:0009E6 00000C            RTS
3183   
3184      
3185      P:0009E5 P:0009E7 60F400            MOVE              #>REP_BUS_ADDR,R0
                            000053
3186      P:0009E7 P:0009E9 61F400            MOVE              #>BURST_DEST_LO,R1
                            00002F
3187                                          .loop   #2
3189      P:0009EB P:0009ED 44D800            MOVE              X:(R0)+,X0
3190      P:0009EC P:0009EE 445900            MOVE              X0,X:(R1)+
Motorola DSP56300 Assembler  Version 6.3.4   13-05-20  12:47:42  hacking.asm  Page 63



3191                                          .endl
3193   
3194      P:0009ED P:0009EF 44F400            MOVE              #>(REP_BUFFER_SIZE*2),X0
                            000080
3195      P:0009EF P:0009F1 442B00            MOVE              X0,X:BLOCK_SIZE
3196      P:0009F0 P:0009F2 44F400            MOVE              #>REP_BUFFER1,X0
                            000056
3197      P:0009F2 P:0009F4 447000            MOVE              X0,X:XMEM_SRC
                            000055
3198   
3199      
3200      P:0009F4 P:0009F6 0D082F            JSR     BLOCK_TRANSFERX
3201                                FAKE_PACKET_2
3202      P:0009F5 P:0009F7 00000C            RTS
3203   
3204   
3205                                DEBUG_UP
3206      P:0009F6 P:0009F8 0A8525            BSET    #DCTR_HF5,X:DCTR
3207      P:0009F7 P:0009F9 00000C            RTS
3208   
3209                                DEBUG_DOWN
3210      P:0009F8 P:0009FA 0A8505            BCLR    #DCTR_HF5,X:DCTR
3211      P:0009F9 P:0009FB 00000C            RTS
3212   
3213      0009FC                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM

0    Errors
3    Warnings


