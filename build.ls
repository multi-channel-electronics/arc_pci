Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  build.asm  Page 1



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  header.asm  Page 2



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  header.asm  Page 3



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  header.asm  Page 4



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  header.asm  Page 5



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  init.asm  Page 6



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
                            000426
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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  init.asm  Page 7



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  init.asm  Page 8



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
                            00043D
399       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            00044E
400       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            00042E
401       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            00037E
402    
403                                 ; QT - set command
404       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            00039C
405       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            000426
406    
407                                 ; Quiet RP mode, clear buffer full flag
408       P:00008C P:00008E 0A0015            BCLR    #RP_BUFFER_FULL,X:<STATUS         ; $8C
409       P:00008D P:00008F 000000            NOP
410    
411                                 ; ***********************************************************************
412                                 ; For now have boot code starting from P:$100
413                                 ; just to make debugging tidier etc.
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  init.asm  Page 9



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  init.asm  Page 10



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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  init.asm  Page 11



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
                            00069E
560       P:000153 P:000155 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
561       P:000154 P:000156 065780            DO      #VAR_TBL_LENGTH,X_WRITE
                            000157
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  init.asm  Page 12



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
                            0004A6
582       P:000168 P:00016A 013D24            BSET    #AUX1,X:PDRC                      ; Enable byte-swapping - still necc. on ARC-
64
583       P:000169 P:00016B 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host interrupts
584       P:00016A P:00016C 0BF080            JSR     TIMER_DEFAULT                     ; Enable timer (channel 0) for misc. uses
                            000620
585       P:00016C P:00016E 0BF080            JSR     TIMER_STORE_INIT                  ; Initialize timing buffer
                            000638
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
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 13



616    
617       
618       P:000173 P:000175 0A01A0            JSET    #MODE_APPLICATION,X:<MODE,APPLICATION
                            000800
619    
620       
621       P:000175 P:000177 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            000628
622    
623       
624       P:000177 P:000179 0B00B4            JSSET   #QT_FLUSH,X:STATUS,BUFFER_INFORM
                            000674
625    
626       
627       P:000179 P:00017B 0D0462            JSR     <CHECK_FO
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
635       P:000180 P:000182 000000            NOP
636       P:000181 P:000183 000000            NOP
637    
638       
639       P:000182 P:000184 0C016E            JMP     PACKET_IN
640    
644    
645                                 ; PCI semaphore
646                                 ;
647                                 ; In order for routines in non-interrupt context to write to the
648                                 ; DTXS, (via PCI_MESSAGE_TO_HOST) they need to end up with
649                                 ; interrupts disabled and HCF3 cleared.
650                                 ;
651                                 ; Non-interrupt PCIers should use macro
652                                 ;       PCI_LOCKDOWN
653                                 ; to get exclusive access and then release it with
654                                 ;       PCI_LOCKUP
655                                 ; after calling PCI_MESSAGE_TO_HOST.
656    
657                                  PCI_LOCKDOWN
658                                           MACRO
659  m                                        JSR     PCI_LOCKDOWN_ENTRY
660  m                                        ENDM
661    
662                                 PCI_LOCKUP MACRO
663  m                                        BCLR    #DCTR_HCIE,X:DCTR
664  m                                        ENDM
665    
666    
667                                 PCI_LOCKDOWN_AGAIN
668       P:000183 P:000185 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Re-enable host IRQ
669       P:000184 P:000186 0632A0            REP     #50                               ; Delay for ~us
670       P:000185 P:000187 000000            NOP
671    
672                                 PCI_LOCKDOWN_ENTRY
673       
674       P:000186 P:000188 0A8500            BCLR    #DCTR_HCIE,X:DCTR                 ; Disable host IRQ
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 14



675       P:000187 P:000189 0A85A3            JSET    #DCTR_HF3,X:DCTR,PCI_LOCKDOWN_AGAIN
                            000183
676       P:000189 P:00018B 00000C            RTS
677    
678    
680    
681                                 HANDLE_FIFO
682       P:00018A P:00018C 54F400            MOVE              #>$A00,A1
                            000A00
683       P:00018C P:00018E 0BF080            JSR     TIMER_STORE_A1
                            000641
684       P:00018E P:000190 0BF080            JSR     TIMER_STORE
                            00063F
685    
686       
687       P:000190 P:000192 60F400            MOVE              #>HEAD_W1_0,R0
                            00001B
688       P:000192 P:000194 44F400            MOVE              #>$00FFFF,X0            ; Mask lower 16 bits
                            00FFFF
689       P:000194 P:000196 220800            MOVE              R0,A0
690       P:000195 P:000197 060880            DO      #8,HANDLE_FIFO_CHECK_PREAMBLE
                            0001A0
691                                 HANDLE_FIFO_WAIT
692       P:000197 P:000199 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000197
693       P:000199 P:00019B 000000            NOP
694       P:00019A P:00019C 000000            NOP
695       P:00019B P:00019D 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000197
696       P:00019D P:00019F 094E3F            MOVEP             Y:RDFIFO,A
697       P:00019E P:0001A0 200046            AND     X0,A
698       P:00019F P:0001A1 000000            NOP
699       P:0001A0 P:0001A2 545800            MOVE              A1,X:(R0)+
700    
701                                 HANDLE_FIFO_CHECK_PREAMBLE
702       P:0001A1 P:0001A3 60F400            MOVE              #>HEAD_W1_0,R0
                            00001B
703       P:0001A3 P:0001A5 20001B            CLR     B
704       P:0001A4 P:0001A6 200013            CLR     A
705       P:0001A5 P:0001A7 57D800            MOVE              X:(R0)+,B
706       P:0001A6 P:0001A8 0140CD            CMP     #>$A5A5,B
                            00A5A5
707       P:0001A8 P:0001AA 0AF0A2            JNE     PRE_ERROR
                            0001CF
708       P:0001AA P:0001AC 57D800            MOVE              X:(R0)+,B
709       P:0001AB P:0001AD 0140CD            CMP     #>$A5A5,B
                            00A5A5
710       P:0001AD P:0001AF 0AF0A2            JNE     PRE_ERROR
                            0001CF
711       P:0001AF P:0001B1 57D800            MOVE              X:(R0)+,B
712       P:0001B0 P:0001B2 0140CD            CMP     #>$5A5A,B
                            005A5A
713       P:0001B2 P:0001B4 0AF0A2            JNE     PRE_ERROR
                            0001CF
714       P:0001B4 P:0001B6 57D800            MOVE              X:(R0)+,B
715       P:0001B5 P:0001B7 0140CD            CMP     #>$5A5A,B
                            005A5A
716       P:0001B7 P:0001B9 0AF0A2            JNE     PRE_ERROR
                            0001CF
717    
718       
719       P:0001B9 P:0001BB 50F000            MOVE              X:>(HEAD_W1_0+6),A0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 15



                            000021
720       P:0001BB P:0001BD 44F000            MOVE              X:>(HEAD_W1_0+7),X0
                            000022
721       P:0001BD P:0001BF 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
722    
723       
724       P:0001BF P:0001C1 0BF080            JSR     PACKET_PARTITIONS
                            0005B0
725       P:0001C1 P:0001C3 0BF080            JSR     TIMER_STORE
                            00063F
726    
728       P:0001C3 P:0001C5 56F000            MOVE              X:HEAD_W3_0,A
                            00001F
729    
730       P:0001C5 P:0001C7 0140C5            CMP     #>'RP',A
                            005250
731       P:0001C7 P:0001C9 0AF0AA            JEQ     HANDLE_RP
                            0001E3
732    
733       P:0001C9 P:0001CB 0140C5            CMP     #>'DA',A
                            004441
734       P:0001CB P:0001CD 0AF0AA            JEQ     HANDLE_DA
                            00022A
735    
736       P:0001CD P:0001CF 0AF080            JMP     QT_PTYPE_ERROR
                            0001D5
737    
738                                 ; Error recording.
739    
740                                 PRE_ERROR
741       P:0001CF P:0001D1 60F400            MOVE              #>PREAMBLE_ERRORS,R0
                            000026
742       P:0001D1 P:0001D3 0BF080            JSR     INCR_X_R0
                            0001DE
743       P:0001D3 P:0001D5 0AF080            JMP     CLEAR_FO_FIFO                     ; empty the fifo (2 ms!)
                            0004A6
744    
745                                 QT_PTYPE_ERROR
746       P:0001D5 P:0001D7 60F400            MOVE              #>PTYPE_ERRORS,R0
                            000027
747       P:0001D7 P:0001D9 0AF080            JMP     INCR_X_R0
                            0001DE
748                                 QT_FSIZE_ERROR
749       P:0001D9 P:0001DB 60F400            MOVE              #>PSIZE_ERRORS,R0
                            000028
750       P:0001DB P:0001DD 0AF080            JMP     INCR_X_R0
                            0001DE
751                                 RETURN_NOW
752       P:0001DD P:0001DF 00000C            RTS
753    
754                                 INCR_X_R0
755       
756       P:0001DE P:0001E0 50E000            MOVE              X:(R0),A0
757       P:0001DF P:0001E1 000008            INC     A
758       P:0001E0 P:0001E2 000000            NOP
759       P:0001E1 P:0001E3 506000            MOVE              A0,X:(R0)
760       P:0001E2 P:0001E4 00000C            RTS
761    
762    
763    
766    
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 16



767                                 HANDLE_RP
768       
769       P:0001E3 P:0001E5 0A0183            JCLR    #MODE_RP_BUFFER,X:MODE,MCE_PACKET
                            0002A8
770    
771       
772       P:0001E5 P:0001E7 0A00B5            JSET    #RP_BUFFER_FULL,X:STATUS,HANDLE_RP_DROP
                            000225
773    
774       
775       P:0001E7 P:0001E9 61F400            MOVE              #>REPLY_BUFFER,R1
                            100000
776       P:0001E9 P:0001EB 0BF080            JSR     BUFFER_PACKET
                            0005BD
777    
778       P:0001EB P:0001ED 54F400            MOVE              #>$b00,A1
                            000B00
779       P:0001ED P:0001EF 0BF080            JSR     TIMER_STORE_A1
                            000641
780       P:0001EF P:0001F1 0BF080            JSR     TIMER_STORE
                            00063F
781    
782       
783       P:0001F1 P:0001F3 60F400            MOVE              #RP_BASE_LO,R0
                            000048
784       P:0001F3 P:0001F5 0BF080            JSR     LOAD_HILO_ADDRESS
                            00068D
785    
786       P:0001F5 P:0001F7 60F400            MOVE              #BURST_DEST_LO,R0
                            00002F
787       P:0001F7 P:0001F9 0BF080            JSR     SAVE_HILO_ADDRESS
                            000695
788    
789       
790       P:0001F9 P:0001FB 200013            CLR     A
791       P:0001FA P:0001FC 20001B            CLR     B
792       P:0001FB P:0001FD 50F000            MOVE              X:PACKET_SIZE,A0
                            000023
793       P:0001FD P:0001FF 0C1D04            ASL     #2,A,A                            ; Size in bytes
794       P:0001FE P:000200 51F000            MOVE              X:RP_MAX_SIZE,B0
                            00004A
795    
796       P:000200 P:000202 200005            CMP     B,A                               ; A ? B
797       P:000201 P:000203 0AF0AF            JLE     HANDLE_RP1
                            000204
798       P:000203 P:000205 21EE00            MOVE              B,A
799    
800                                 HANDLE_RP1
801       
802       P:000204 P:000206 44F400            MOVE              #>REPLY_BUFFER,X0
                            100000
803       P:000206 P:000208 507000            MOVE              A0,X:BLOCK_SIZE
                            00002B
804       P:000208 P:00020A 447000            MOVE              X0,X:YMEM_SRC
                            00002E
805       P:00020A P:00020C 0BF080            JSR     TIMER_STORE
                            00063F
806       P:00020C P:00020E 0BF080            JSR     BLOCK_TRANSFER
                            00050A
807       P:00020E P:000210 0BF080            JSR     TIMER_STORE
                            00063F
808    
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 17



809       
810                                           PCI_LOCKDOWN                              ; Disable host IRQ
812       P:000211 P:000213 44F400            MOVE              #'NFY',X0
                            4E4659
813       P:000213 P:000215 447000            MOVE              X0,X:DTXS_WD1
                            00000B
814       P:000215 P:000217 44F400            MOVE              #'RPQ',X0
                            525051
815       P:000217 P:000219 447000            MOVE              X0,X:DTXS_WD2
                            00000C
816       P:000219 P:00021B 507000            MOVE              A0,X:DTXS_WD3           ; A0=block_size
                            00000D
817       P:00021B P:00021D 547000            MOVE              A1,X:DTXS_WD4           ; A1=0
                            00000E
818    
819       
820       P:00021D P:00021F 0A7035            BSET    #RP_BUFFER_FULL,X:STATUS
                            000000
821       P:00021F P:000221 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00046C
822                                           PCI_LOCKUP                                ; Enable host IRQ
824    
825       P:000222 P:000224 0BF080            JSR     TIMER_STORE
                            00063F
826       P:000224 P:000226 00000C            RTS                                       ; Back to main loop
827    
828                                 HANDLE_RP_DROP
829       P:000225 P:000227 60F400            MOVE              #RP_DROPS,R0
                            00004B
830       P:000227 P:000229 0D01DE            JSR     INCR_X_R0
831       P:000228 P:00022A 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            000600
832    
834    
835    
838    
839    
840                                 HANDLE_DA
841       
842       P:00022A P:00022C 60F400            MOVE              #FRAME_COUNT,R0
                            000002
843       P:00022C P:00022E 0D01DE            JSR     INCR_X_R0
844    
845       
846       P:00022D P:00022F 0A0182            JCLR    #MODE_QT,X:MODE,MCE_PACKET
                            0002A8
847    
848       
849       P:00022F P:000231 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
850       P:000231 P:000233 0BF080            JSR     BUFFER_PACKET
                            0005BD
851    
852       P:000233 P:000235 54F400            MOVE              #$e00,A1
                            000E00
853       P:000235 P:000237 0BF080            JSR     TIMER_STORE_A1
                            000641
854       P:000237 P:000239 0BF080            JSR     TIMER_STORE
                            00063F
855    
856       
857       P:000239 P:00023B 56F000            MOVE              X:QT_BUF_HEAD,A
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 18



                            000042
858       P:00023B P:00023D 014180            ADD     #1,A
859       P:00023C P:00023E 57F000            MOVE              X:QT_BUF_MAX,B
                            00003F
860       P:00023E P:000240 20000D            CMP     A,B
861       P:00023F P:000241 0AF0A1            JGE     HANDLE_DA_MATH
                            000242
862       P:000241 P:000243 2E0000            MOVE              #0,A
863                                 HANDLE_DA_MATH
864       P:000242 P:000244 57F000            MOVE              X:QT_BUF_TAIL,B
                            000043
865       P:000244 P:000246 20000D            CMP     A,B
866       P:000245 P:000247 0AF0AA            JEQ     HANDLE_DA_DROP
                            000266
867    
868       
869       P:000247 P:000249 200013            CLR     A
870       P:000248 P:00024A 50F000            MOVE              X:PACKET_SIZE,A0
                            000023
871    
872       P:00024A P:00024C 014088            ADD     #0,B                              ; Clear carry
873       P:00024B P:00024D 0C1D04            ASL     #2,A,A                            ; Size, in bytes
874    
875       
876       P:00024C P:00024E 20001B            CLR     B
877       P:00024D P:00024F 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            000040
878       P:00024F P:000251 20000D            CMP     A,B
879       P:000250 P:000252 0E21D9            JNE     QT_FSIZE_ERROR
880    
881       
882       P:000251 P:000253 517000            MOVE              B0,X:BLOCK_SIZE
                            00002B
883       P:000253 P:000255 557000            MOVE              B1,X:YMEM_SRC           ; Y:0
                            00002E
884    
885       P:000255 P:000257 60F400            MOVE              #QT_DEST_LO,R0
                            000044
886       P:000257 P:000259 0BF080            JSR     LOAD_HILO_ADDRESS
                            00068D
887       P:000259 P:00025B 60F400            MOVE              #BURST_DEST_LO,R0
                            00002F
888       P:00025B P:00025D 0BF080            JSR     SAVE_HILO_ADDRESS
                            000695
889    
890       
891       P:00025D P:00025F 0BF080            JSR     BLOCK_TRANSFER
                            00050A
892    
893       P:00025F P:000261 0BF080            JSR     TIMER_STORE
                            00063F
894    
895       
896       P:000261 P:000263 0BF080            JSR     BUFFER_INCR
                            000649
897    
898       
899       P:000263 P:000265 0BF080            JSR     BUFFER_INFORM_CHECK
                            000667
900    
901       P:000265 P:000267 00000C            RTS
902    
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 19



903                                 HANDLE_DA_DROP
904       
905       P:000266 P:000268 60F400            MOVE              #QT_DROPS,R0
                            000047
906       P:000268 P:00026A 0D01DE            JSR     INCR_X_R0
907       P:000269 P:00026B 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            000600
908    
910    
911    
912                                 ;----------------------------------------------
913                                 CON_BUFFER
914                                 ; This routine will copy an MCE command from the PC to Y memory.
915                                 ; The source RAM address has already been stored in CON_SRC_LO.
916                                 ; The destination address is always Y:COMMAND_BUFFER.
917                                 ;----------------------------------------------
918    
919       P:00026B P:00026D 54F400            MOVE              #>$C00,A1
                            000C00
920       P:00026D P:00026F 0BF080            JSR     TIMER_STORE_A1
                            000641
921       P:00026F P:000271 0BF080            JSR     TIMER_STORE
                            00063F
922    
923       
924       P:000271 P:000273 60F400            MOVE              #>CON_SRC_LO,R0
                            00002C
925       P:000273 P:000275 0BF080            JSR     LOAD_HILO_ADDRESS
                            00068D
926       P:000275 P:000277 60F400            MOVE              #>BURST_SRC_LO,R0
                            000031
927       P:000277 P:000279 0BF080            JSR     SAVE_HILO_ADDRESS
                            000695
928       P:000279 P:00027B 51F400            MOVE              #>COMMAND_BUFFER,B0
                            200000
929       P:00027B P:00027D 50F400            MOVE              #>256,A0
                            000100
930       P:00027D P:00027F 517000            MOVE              B0,X:YMEM_DEST
                            000033
931       P:00027F P:000281 507000            MOVE              A0,X:BLOCK_SIZE
                            00002B
932       P:000281 P:000283 0BF080            JSR     CON_TRANSFER
                            000544
933    
934       P:000283 P:000285 0A702B            BSET    #CON_MCE,X:STATUS
                            000000
935       P:000285 P:000287 0BF080            JSR     TIMER_STORE
                            00063F
936       P:000287 P:000289 00000C            RTS                                       ; Back to main loop
937    
938                                 ;----------------------------------------------
939                                 CON_TRANSMIT
940                                 ; This routine will copy the MCE command from Y:COMMAND_BUFFER to
941                                 ; the MCE command transmitter.
942                                 ;----------------------------------------------
943    
944       P:000288 P:00028A 0BF080            JSR     TIMER_STORE
                            00063F
945    
946       P:00028A P:00028C 66F400            MOVE              #>COMMAND_BUFFER,R6
                            200000
947       P:00028C P:00028E 068080            DO      #128,CON_TRANSMIT1                ; block size = 16bit x 128 (256 bytes)
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 20



                            000295
948       P:00028E P:000290 5CDE00            MOVE                          Y:(R6)+,A1  ; b2, b1  (lsb)
949       P:00028F P:000291 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
950       P:000290 P:000292 0140C6            AND     #>$FF,A
                            0000FF
951       P:000292 P:000294 547000            MOVE              A1,X:FO_SEND
                            FFF000
952       P:000294 P:000296 557000            MOVE              B1,X:FO_SEND
                            FFF000
953    
954                                 CON_TRANSMIT1
955       P:000296 P:000298 0A0121            BSET    #MODE_MCE,X:<MODE                 ; enable processing of MCE replies/data
956    
957       
958       P:000297 P:000299 0A700B            BCLR    #CON_MCE,X:STATUS
                            000000
959       P:000299 P:00029B 0A700A            BCLR    #CON_DEMAND,X:STATUS
                            000000
960    
961       P:00029B P:00029D 0BF080            JSR     TIMER_STORE
                            00063F
962    
963       
964                                           PCI_LOCKDOWN
966       P:00029E P:0002A0 44F400            MOVE              #'CON',X0
                            434F4E
967       P:0002A0 P:0002A2 0BF080            JSR     VCOM_PREPARE_REPLY
                            0002E9
968       P:0002A2 P:0002A4 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00046C
969                                           PCI_LOCKUP                                ; Enable host IRQ
971    
972       P:0002A5 P:0002A7 0BF080            JSR     TIMER_STORE
                            00063F
973       P:0002A7 P:0002A9 00000C            RTS                                       ; Back to main loop
974    
975    
976    
977    
979    
980                                 ; --------------------------------------------------------------------------
981                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
982                                 ; --------------------------------------------------------------------------
983    
984                                 ; prepare notify to inform host that a packet has arrived.
985    
986                                 MCE_PACKET
987                                           PCI_LOCKDOWN                              ; Disable host IRQ
989       P:0002A9 P:0002AB 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
990    
991       P:0002AA P:0002AC 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
992       P:0002AC P:0002AE 440B00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
993    
994       P:0002AD P:0002AF 449F00            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
995       P:0002AE P:0002B0 440C00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
996    
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 21



997       P:0002AF P:0002B1 44A100            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
998       P:0002B0 P:0002B2 440D00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
999    
1000      P:0002B1 P:0002B3 44A200            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
1001      P:0002B2 P:0002B4 440E00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sage
1002   
1003      
1004      P:0002B3 P:0002B5 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
1005      P:0002B4 P:0002B6 0D046C            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
1006      P:0002B5 P:0002B7 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
1007                                          PCI_LOCKUP
1009   
1010      P:0002B7 P:0002B9 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
1011      P:0002B9 P:0002BB 0BF080            JSR     BUFFER_PACKET
                            0005BD
1012   
1013      
1014   
1015      P:0002BB P:0002BD 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; on fatal error, re-init.
                            000100
1016      P:0002BD P:0002BF 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; Set in 'send_packet_to_host' ISR
                            0002BB
1017   
1018      
1019      P:0002BF P:0002C1 44F400            MOVE              #>IMAGE_BUFFER,X0
                            000000
1020      P:0002C1 P:0002C3 56F000            MOVE              X:PACKET_SIZE,A
                            000023
1021      P:0002C3 P:0002C5 0C1D04            ASL     #2,A,A
1022      P:0002C4 P:0002C6 447000            MOVE              X0,X:YMEM_SRC
                            00002E
1023      P:0002C6 P:0002C8 547000            MOVE              A1,X:BLOCK_SIZE
                            00002B
1024      P:0002C8 P:0002CA 0BF080            JSR     BLOCK_TRANSFER
                            00050A
1025   
1026      P:0002CA P:0002CC 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
1027   
1028      
1029                                          PCI_LOCKDOWN                              ; Disable host IRQ
1031      P:0002CD P:0002CF 44F400            MOVE              #'HST',X0
                            485354
1032      P:0002CF P:0002D1 0BF080            JSR     VCOM_PREPARE_REPLY
                            0002E9
1033      P:0002D1 P:0002D3 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00046C
1034                                          PCI_LOCKUP                                ; Enable host IRQ
1036      P:0002D4 P:0002D6 00000C            RTS
1037   
1038                                ;----------------------------------------------------------
1039                                ; clear out the fifo after an HST timeout...
1040                                ;----------------------------------------------------------
1041   
1042                                DUMP_FIFO
1043      P:0002D5 P:0002D7 61F400            MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
1044      P:0002D7 P:0002D9 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 22



ifo
                            000200
1045      P:0002D9 P:0002DB 200013            CLR     A
1046      P:0002DA P:0002DC 320000            MOVE              #0,R2                   ; use R2 as a dump count
1047                                NEXT_DUMP
1048      P:0002DB P:0002DD 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E6
1049      P:0002DD P:0002DF 000000            NOP
1050      P:0002DE P:0002E0 000000            NOP
1051      P:0002DF P:0002E1 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E6
1052   
1053      P:0002E1 P:0002E3 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
1054      P:0002E2 P:0002E4 205A00            MOVE              (R2)+                   ; inc dump count
1055      P:0002E3 P:0002E5 224E00            MOVE              R2,A                    ;
1056      P:0002E4 P:0002E6 200045            CMP     X0,A                              ; check we've not hit dump limit
1057      P:0002E5 P:0002E7 0E22DB            JNE     NEXT_DUMP                         ; not hit limit?
1058                                FIFO_EMPTY
1059      P:0002E6 P:0002E8 627000            MOVE              R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000006
1060      P:0002E8 P:0002EA 0C0100            JMP     <START                            ; re-initialise
1061   
1062   
1063                                ; -------------------------------------------------------------------------------------
1064                                ;                              END OF MAIN PACKET HANDLING CODE
1065                                ; -------------------------------------------------------------------------------------
1066   
1067   
1068   
1069                                ; -------------------------------------------------------------------------------------
1070                                ;
1071                                ;                              INTERRUPT SERVICE ROUTINES
1072                                ;
1073                                ; -------------------------------------------------------------------------------------
1074   
1075                                ; ---------------
1076                                ; Rules:  Don't use N#, or any R# except R0 unless you add them to the saved register set.
1077   
1078   
1079                                ; ----------------------------------------------------------------------------
1080                                ; VCOM_* - routines: utility functions for hosty command vector communication.
1081                                ;-----------------------------------------------------------------------------
1082   
1083   
1084                                ; VCOM_PREPARE_REPLY
1085                                ;
1086                                ; Prepare the reply packet, using X0 as the command name (second word).  The
1087                                ; message defaults to 'ACK' with NULL data.  The user may subsequenty fill in
1088                                ; the data field (word 4) and mark the packet as error if necessary.
1089   
1090                                VCOM_PREPARE_REPLY
1091      
1092      
1093      P:0002E9 P:0002EB 50F400            MOVE              #'REP',A0
                            524550
1094      P:0002EB P:0002ED 447000            MOVE              X0,X:DTXS_WD2           ; Command
                            00000C
1095      P:0002ED P:0002EF 507000            MOVE              A0,X:DTXS_WD1
                            00000B
1096   
1097      P:0002EF P:0002F1 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 23



                            41434B
1098      P:0002F1 P:0002F3 000000            NOP
1099      P:0002F2 P:0002F4 547000            MOVE              A1,X:DTXS_WD3           ; ACK
                            00000D
1100      P:0002F4 P:0002F6 507000            MOVE              A0,X:DTXS_WD4           ; no comment
                            00000E
1101      P:0002F6 P:0002F8 00000C            RTS
1102   
1103   
1104                                ; VCOM_CHECK
1105                                ;
1106                                ; Compares DRXR_WD1 to X0.  If they are equal, Z is set on return.  If they
1107                                ; are not equal then Z is cleared and the reply will be marked as ERR with
1108                                ; 'CNE' in the last word.
1109                                ; Trashes A and B always and X0 on error.
1110   
1111                                VCOM_CHECK
1112      P:0002F7 P:0002F9 208E00            MOVE              X0,A
1113      P:0002F8 P:0002FA 57F000            MOVE              X:DRXR_WD1,B
                            000007
1114      P:0002FA P:0002FC 20000D            CMP     A,B
1115      P:0002FB P:0002FD 0AF0AA            JEQ     VCOM_RTS
                            000305
1116   
1117      P:0002FD P:0002FF 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
1118      P:0002FF P:000301 50F400            MOVE              #'ERR',A0
                            455252
1119      P:000301 P:000303 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1120      P:000303 P:000305 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1121                                VCOM_RTS
1122      P:000305 P:000307 00000C            RTS
1123   
1124   
1125                                ; VCOM_INTRO
1126                                ;
1127                                ; Read DSP command from DRXR.  Prepare the reply packet and verify that it
1128                                ; matches the key in X1.  If it does not, mark the reply as error and set
1129                                ; the Z flag.
1130   
1131                                VCOM_INTRO
1132      P:000306 P:000308 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            000318
1133      P:000308 P:00030A 20A400            MOVE              X1,X0
1134      P:000309 P:00030B 0D02E9            JSR     VCOM_PREPARE_REPLY
1135      P:00030A P:00030C 0D02F7            JSR     VCOM_CHECK
1136      P:00030B P:00030D 00000C            RTS
1137   
1138   
1139                                ; VCOM_EXIT_ERROR_X0
1140                                ; VCOM_EXIT_X0
1141                                ; VCOM_EXIT
1142                                ;
1143                                ; For returning from host command vector interrupts only.  These three
1144                                ; routines do the following (respectively):
1145                                ; a) Mark reply as error, then (b)
1146                                ; b) Put X0 into last word of reply, then (c)
1147                                ; c) Restore registers and RTI.
1148   
1149                                VCOM_EXIT_ERROR_X0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 24



1150      P:00030C P:00030E 50F400            MOVE              #'ERR',A0
                            455252
1151      P:00030E P:000310 000000            NOP
1152      P:00030F P:000311 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1153                                VCOM_EXIT_X0
1154      P:000311 P:000313 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1155                                VCOM_EXIT
1156      P:000313 P:000315 0BF080            JSR     RESTORE_REGISTERS
                            000485
1157      P:000315 P:000317 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00046C
1158      P:000317 P:000319 000004            RTI
1159   
1160   
1161                                ;---------------------------------------------------------------
1162                                RD_DRXR
1163                                ;--------------------------------------------------------------
1164                                ; Routine to read from HTXR-DRXR data path.  For HCTR = 0x900,
1165                                ; 3 LSB of each 32-bit word written by the host is returned on
1166                                ; each read.  This only polls for first word, not all of them.
1167      P:000318 P:00031A 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            000318
1168      P:00031A P:00031C 63F400            MOVE              #DRXR_WD1,R3
                            000007
1169      P:00031C P:00031E 0604A0            REP     #4
1170      P:00031D P:00031F 085B8B            MOVEP             X:DRXR,X:(R3)+
1171      P:00031E P:000320 00000C            RTS
1172   
1173   
1174                                ; ----------------------------------------------------------------------------
1175                                READ_MEMORY
1176                                ;-----------------------------------------------------------------------------
1177                                ;Read command:
1178                                ; word 1 = command = 'RDM'
1179                                ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
1180                                ; word 3 = address in memory
1181                                ; word 4 = not used
1182                                ;Version query:
1183                                ; word 1 = 'VER'
1184                                ; word 2-4 unused
1185   
1186      P:00031F P:000321 0BF080            JSR     SAVE_REGISTERS
                            000492
1187      P:000321 P:000323 0D0318            JSR     RD_DRXR                           ; Loads DRXR_WD*
1188   
1189      P:000322 P:000324 44F400            MOVE              #'RDM',X0
                            52444D
1190      P:000324 P:000326 0D02E9            JSR     VCOM_PREPARE_REPLY
1191      P:000325 P:000327 0D02F7            JSR     VCOM_CHECK
1192      P:000326 P:000328 0AF0AA            JEQ     READ_MEMORY_XYP
                            000330
1193   
1194      
1195      P:000328 P:00032A 44F400            MOVE              #'VER',X0
                            564552
1196      P:00032A P:00032C 0D02E9            JSR     VCOM_PREPARE_REPLY
1197      P:00032B P:00032D 0D02F7            JSR     VCOM_CHECK
1198      P:00032C P:00032E 0E2313            JNE     VCOM_EXIT
1199   
1200      P:00032D P:00032F 44F000            MOVE              X:REV_NUMBER,X0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 25



                            000003
1201      P:00032F P:000331 0C0311            JMP     VCOM_EXIT_X0
1202   
1203                                READ_MEMORY_XYP
1204   
1205      
1206      P:000330 P:000332 56F000            MOVE              X:DRXR_WD2,A
                            000008
1207      P:000332 P:000334 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1208   
1209      P:000334 P:000336 0140C5            CMP     #'_X',A
                            005F58
1210      P:000336 P:000338 0AF0AA            JEQ     READ_MEMORY_X
                            000343
1211   
1212      P:000338 P:00033A 0140C5            CMP     #'_Y',A
                            005F59
1213      P:00033A P:00033C 0AF0AA            JEQ     READ_MEMORY_Y
                            000345
1214   
1215      P:00033C P:00033E 0140C5            CMP     #'_P',A
                            005F50
1216      P:00033E P:000340 0AF0AA            JEQ     READ_MEMORY_P
                            000347
1217   
1218      P:000340 P:000342 44F400            MOVE              #'MTE',X0
                            4D5445
1219      P:000342 P:000344 0C030C            JMP     VCOM_EXIT_ERROR_X0
1220   
1221                                READ_MEMORY_X
1222      P:000343 P:000345 44E000            MOVE              X:(R0),X0
1223      P:000344 P:000346 0C0311            JMP     VCOM_EXIT_X0
1224                                READ_MEMORY_Y
1225      P:000345 P:000347 4CE000            MOVE                          Y:(R0),X0
1226      P:000346 P:000348 0C0311            JMP     VCOM_EXIT_X0
1227                                READ_MEMORY_P
1228      P:000347 P:000349 07E084            MOVE              P:(R0),X0
1229      P:000348 P:00034A 0C0311            JMP     VCOM_EXIT_X0
1230   
1231   
1232                                ;--------------------------------------------------------------
1233                                WRITE_MEMORY
1234                                ;---------------------------------------------------------------
1235                                ; word 1 = command = 'WRM'
1236                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1237                                ; word 3 = address in memory
1238                                ; word 4 = value
1239   
1240      P:000349 P:00034B 0BF080            JSR     SAVE_REGISTERS
                            000492
1241      P:00034B P:00034D 45F400            MOVE              #'WRM',X1
                            57524D
1242      P:00034D P:00034F 0D0306            JSR     VCOM_INTRO
1243      P:00034E P:000350 0E2313            JNE     VCOM_EXIT
1244   
1245      
1246      P:00034F P:000351 56F000            MOVE              X:DRXR_WD2,A
                            000008
1247      P:000351 P:000353 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1248      P:000353 P:000355 44F000            MOVE              X:DRXR_WD4,X0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 26



                            00000A
1249   
1250      P:000355 P:000357 0140C5            CMP     #'_X',A
                            005F58
1251      P:000357 P:000359 0AF0AA            JEQ     WRITE_MEMORY_X
                            000364
1252   
1253      P:000359 P:00035B 0140C5            CMP     #'_Y',A
                            005F59
1254      P:00035B P:00035D 0AF0AA            JEQ     WRITE_MEMORY_Y
                            000366
1255   
1256      P:00035D P:00035F 0140C5            CMP     #'_P',A
                            005F50
1257      P:00035F P:000361 0AF0AA            JEQ     WRITE_MEMORY_P
                            000368
1258   
1259      P:000361 P:000363 44F400            MOVE              #'MTE',X0
                            4D5445
1260      P:000363 P:000365 0C030C            JMP     VCOM_EXIT_ERROR_X0
1261   
1262                                WRITE_MEMORY_X
1263      P:000364 P:000366 446000            MOVE              X0,X:(R0)
1264      P:000365 P:000367 0C0311            JMP     VCOM_EXIT_X0
1265                                WRITE_MEMORY_Y
1266      P:000366 P:000368 4C6000            MOVE                          X0,Y:(R0)
1267      P:000367 P:000369 0C0311            JMP     VCOM_EXIT_X0
1268                                WRITE_MEMORY_P
1269      P:000368 P:00036A 076084            MOVE              X0,P:(R0)
1270      P:000369 P:00036B 0C0311            JMP     VCOM_EXIT_X0
1271   
1272   
1273                                ;-----------------------------------------------------------------------------
1274                                START_APPLICATION
1275                                ; an application should already have been downloaded to the PCI memory.
1276                                ; this command will execute it.
1277                                ; ----------------------------------------------------------------------
1278                                ; word 1 = command = 'GOA'
1279                                ; word 2-4 unused
1280   
1281      P:00036A P:00036C 0BF080            JSR     SAVE_REGISTERS
                            000492
1282      P:00036C P:00036E 45F400            MOVE              #'GOA',X1
                            474F41
1283   
1284      P:00036E P:000370 0D0306            JSR     VCOM_INTRO
1285      P:00036F P:000371 0E2313            JNE     VCOM_EXIT
1286   
1287      P:000370 P:000372 0A7020            BSET    #MODE_APPLICATION,X:MODE
                            000001
1288      P:000372 P:000374 000004            RTI                                       ; Application will reply.
1289   
1290   
1291                                ; ---------------------------------------------------------
1292                                STOP_APPLICATION
1293                                ; this command stops an application that is currently running
1294                                ; used for applications that once started run contiunually
1295                                ;-----------------------------------------------------------
1296                                ; word 1 = command = ' STP'
1297                                ; word 2-4 unused
1298   
1299      P:000373 P:000375 0BF080            JSR     SAVE_REGISTERS
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 27



                            000492
1300      P:000375 P:000377 45F400            MOVE              #'STP',X1
                            535450
1301   
1302      P:000377 P:000379 0D0306            JSR     VCOM_INTRO
1303      P:000378 P:00037A 0E2313            JNE     VCOM_EXIT
1304   
1305      P:000379 P:00037B 0A7000            BCLR    #MODE_APPLICATION,X:MODE
                            000001
1306      P:00037B P:00037D 0A7000            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1307      P:00037D P:00037F 0C0313            JMP     VCOM_EXIT
1308   
1309   
1310                                ;-----------------------------------------------------------------------------
1311                                RESET_CONTROLLER
1312                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1313                                ;-----------------------------------------------------------------------------
1314                                ; word 1 = command = 'RCO'
1315                                ; word 2-4 unused
1316   
1317      P:00037E P:000380 0BF080            JSR     SAVE_REGISTERS
                            000492
1318      P:000380 P:000382 45F400            MOVE              #'RCO',X1
                            52434F
1319      P:000382 P:000384 0D0306            JSR     VCOM_INTRO
1320      P:000383 P:000385 0E2313            JNE     VCOM_EXIT
1321   
1322      P:000384 P:000386 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1323      P:000385 P:000387 000000            NOP
1324      P:000386 P:000388 000000            NOP
1325      P:000387 P:000389 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1326      P:000389 P:00038B 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1327      P:00038B P:00038D 446000            MOVE              X0,X:(R0)
1328      P:00038C P:00038E 0606A0            REP     #6                                ; Wait for transmission to complete
1329      P:00038D P:00038F 000000            NOP
1330      P:00038E P:000390 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1331   
1332                                ; Wait for a bit for MCE to be reset.......
1333      P:00038F P:000391 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1334      P:000391 P:000393 06C400            DO      X0,L_DELAY
                            000397
1335      P:000393 P:000395 06E883            DO      #1000,L_RDFIFO
                            000396
1336      P:000395 P:000397 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1337      P:000396 P:000398 000000            NOP                                       ;   receiver empty
1338                                L_RDFIFO
1339      P:000397 P:000399 000000            NOP
1340                                L_DELAY
1341      P:000398 P:00039A 000000            NOP
1342   
1343      P:000399 P:00039B 44F400            MOVE              #'000',X0
                            303030
1344      P:00039B P:00039D 0C0311            JMP     VCOM_EXIT_X0
1345   
1346                                ;-----------------------------------------------------------------------------
1347                                QUIET_TRANSFER_SET
1348                                ;-----------------------------------------------------------------------------
1349                                ;Quiet transfer mode configuration
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 28



1350                                ; word 1 = command = 'QTS'
1351                                ; word 2 = parameter to set
1352                                ; word 3-4 = arguments
1353   
1354      P:00039C P:00039E 0BF080            JSR     SAVE_REGISTERS                    ; standard opening
                            000492
1355      P:00039E P:0003A0 45F400            MOVE              #'QTS',X1
                            515453
1356      P:0003A0 P:0003A2 0D0306            JSR     VCOM_INTRO
1357      P:0003A1 P:0003A3 0E2313            JNE     VCOM_EXIT
1358   
1359      P:0003A2 P:0003A4 60F400            MOVE              #BDEBUG0,R0
                            00004D
1360      P:0003A4 P:0003A6 0D01DE            JSR     INCR_X_R0
1361   
1362      P:0003A5 P:0003A7 56F000            MOVE              X:DRXR_WD2,A            ; Parameter id
                            000008
1363      P:0003A7 P:0003A9 44F000            MOVE              X:DRXR_WD3,X0           ; First arg
                            000009
1364      P:0003A9 P:0003AB 45F000            MOVE              X:DRXR_WD4,X1           ; Second arg
                            00000A
1365   
1366      P:0003AB P:0003AD 0140C5            CMP     #'BAS',A
                            424153
1367      P:0003AD P:0003AF 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            00041F
1368   
1369      P:0003AF P:0003B1 0140C5            CMP     #'DEL',A
                            44454C
1370      P:0003B1 P:0003B3 60F400            MOVE              #QT_BUF_SIZE,R0
                            00003E
1371      P:0003B3 P:0003B5 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041D
1372   
1373      P:0003B5 P:0003B7 0140C5            CMP     #'NUM',A
                            4E554D
1374      P:0003B7 P:0003B9 60F400            MOVE              #QT_BUF_MAX,R0
                            00003F
1375      P:0003B9 P:0003BB 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041D
1376   
1377      P:0003BB P:0003BD 0140C5            CMP     #'INF',A
                            494E46
1378      P:0003BD P:0003BF 60F400            MOVE              #QT_INFORM,R0
                            000041
1379      P:0003BF P:0003C1 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041D
1380   
1381      P:0003C1 P:0003C3 0140C5            CMP     #'SIZ',A
                            53495A
1382      P:0003C3 P:0003C5 60F400            MOVE              #QT_FRAME_SIZE,R0
                            000040
1383      P:0003C5 P:0003C7 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041D
1384   
1385      P:0003C7 P:0003C9 0140C5            CMP     #'TAI',A
                            544149
1386      P:0003C9 P:0003CB 60F400            MOVE              #QT_BUF_TAIL,R0
                            000043
1387      P:0003CB P:0003CD 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041D
1388   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 29



1389      P:0003CD P:0003CF 0140C5            CMP     #'HEA',A
                            484541
1390      P:0003CF P:0003D1 60F400            MOVE              #QT_BUF_HEAD,R0
                            000042
1391      P:0003D1 P:0003D3 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041D
1392   
1393      P:0003D3 P:0003D5 0140C5            CMP     #'DRO',A
                            44524F
1394      P:0003D5 P:0003D7 60F400            MOVE              #QT_DROPS,R0
                            000047
1395      P:0003D7 P:0003D9 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041D
1396   
1397      P:0003D9 P:0003DB 0140C5            CMP     #'PER',A
                            504552
1398      P:0003DB P:0003DD 60F400            MOVE              #TCPR0,R0
                            FFFF8D
1399      P:0003DD P:0003DF 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041D
1400   
1401      P:0003DF P:0003E1 0140C5            CMP     #'FLU',A
                            464C55
1402      P:0003E1 P:0003E3 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            000407
1403   
1404      P:0003E3 P:0003E5 0140C5            CMP     #'SET',A
                            534554
1405      P:0003E5 P:0003E7 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            00040F
1406   
1407      P:0003E7 P:0003E9 0140C5            CMP     #'RPS',A
                            525053
1408      P:0003E9 P:0003EB 60F400            MOVE              #RP_MAX_SIZE,R0
                            00004A
1409      P:0003EB P:0003ED 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            00041D
1410   
1411      P:0003ED P:0003EF 0140C5            CMP     #'RPB',A
                            525042
1412      P:0003EF P:0003F1 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_BASE
                            0003F8
1413   
1414      P:0003F1 P:0003F3 0140C5            CMP     #'RPE',A
                            525045
1415      P:0003F3 P:0003F5 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_ENABLED
                            0003FD
1416   
1417      P:0003F5 P:0003F7 44F400            MOVE              #'MTE',X0
                            4D5445
1418      P:0003F7 P:0003F9 0C030C            JMP     VCOM_EXIT_ERROR_X0
1419   
1420                                QUIET_TRANSFER_SET_RP_BASE
1421      P:0003F8 P:0003FA 447000            MOVE              X0,X:RP_BASE_LO
                            000048
1422      P:0003FA P:0003FC 457000            MOVE              X1,X:RP_BASE_HI
                            000049
1423      P:0003FC P:0003FE 0C0313            JMP     VCOM_EXIT
1424   
1425                                QUIET_TRANSFER_SET_RP_ENABLED
1426      P:0003FD P:0003FF 0A7003            BCLR    #MODE_RP_BUFFER,X:MODE
                            000001
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 30



1427      P:0003FF P:000401 208E00            MOVE              X0,A
1428      P:000400 P:000402 200003            TST     A
1429      P:000401 P:000403 0EA313            JEQ     VCOM_EXIT
1430      P:000402 P:000404 0A7023            BSET    #MODE_RP_BUFFER,X:MODE
                            000001
1431      P:000404 P:000406 0A7015            BCLR    #RP_BUFFER_FULL,X:STATUS
                            000000
1432      P:000406 P:000408 0C0313            JMP     VCOM_EXIT
1433   
1434                                QUIET_TRANSFER_SET_FLUSH
1435      P:000407 P:000409 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
1436      P:000409 P:00040B 208E00            MOVE              X0,A
1437      P:00040A P:00040C 200003            TST     A
1438      P:00040B P:00040D 0EA313            JEQ     VCOM_EXIT
1439      P:00040C P:00040E 0A7034            BSET    #QT_FLUSH,X:STATUS
                            000000
1440      P:00040E P:000410 0C0313            JMP     VCOM_EXIT
1441   
1442                                QUIET_TRANSFER_SET_ENABLED
1443      P:00040F P:000411 208E00            MOVE              X0,A
1444      P:000410 P:000412 200003            TST     A
1445      P:000411 P:000413 0AF0AA            JEQ     QUIET_TRANSFER_SET_DISABLED
                            000418
1446      P:000413 P:000415 0A7022            BSET    #MODE_QT,X:MODE
                            000001
1447      P:000415 P:000417 0BF080            JSR     TIMER_ENABLE
                            000614
1448      P:000417 P:000419 0C0313            JMP     VCOM_EXIT
1449   
1450                                QUIET_TRANSFER_SET_DISABLED
1451      P:000418 P:00041A 0A7002            BCLR    #MODE_QT,X:MODE
                            000001
1452      P:00041A P:00041C 0BF080            JSR     TIMER_DEFAULT
                            000620
1453      P:00041C P:00041E 0C0313            JMP     VCOM_EXIT
1454   
1455                                QUIET_TRANSFER_SET_R0
1456      P:00041D P:00041F 446000            MOVE              X0,X:(R0)
1457      P:00041E P:000420 0C0313            JMP     VCOM_EXIT
1458   
1459                                QUIET_TRANSFER_SET_BASE
1460      P:00041F P:000421 447000            MOVE              X0,X:QT_BASE_LO
                            00003C
1461      P:000421 P:000423 457000            MOVE              X1,X:QT_BASE_HI
                            00003D
1462   
1463      P:000423 P:000425 0BF080            JSR     BUFFER_RESET
                            00065B
1464   
1465      P:000425 P:000427 0C0313            JMP     VCOM_EXIT
1466   
1467   
1468                                ;-----------------------------------------------------------------------------
1469                                SYSTEM_RESET
1470                                ;-----------------------------------------------------------------------------
1471   
1472      P:000426 P:000428 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1473      P:000427 P:000429 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1474                                                                                    ; set to zero except for interrupts
1475      P:000429 P:00042B 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 31



1476                                                                                    ; so first set to 0
1477      P:00042A P:00042C 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1478                                                                                    ; therefore,return to initialization
1479      P:00042C P:00042E 000000            NOP
1480      P:00042D P:00042F 000004            RTI                                       ; return from ISR - to START
1481   
1482   
1483                                ; ------------------------------------------------------------------------------------
1484                                SEND_PACKET_TO_HOST
1485                                ; this command is received from the Host and actions the PCI board to pick up an address
1486                                ; pointer from DRXR which the PCI board then uses to write packets from the
1487                                ; MCE to the host memory starting at the address given.
1488                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1489                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1490                                ; HST after packet sent (unless error).
1491                                ; --------------------------------------------------------------------------------------
1492                                ; word 1 = command = 'HST'
1493                                ; word 2 = host high address
1494                                ; word 3 = host low address
1495                                ; word 4 = not used but read
1496   
1497                                ; save some registers but not B
1498   
1499      P:00042E P:000430 0D0492            JSR     <SAVE_REGISTERS                   ; save working registers
1500      P:00042F P:000431 45F400            MOVE              #'HST',X1
                            485354
1501      P:000431 P:000433 0D0306            JSR     VCOM_INTRO
1502      P:000432 P:000434 0E2313            JNE     VCOM_EXIT
1503   
1504      
1505      P:000433 P:000435 448800            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
1506      P:000434 P:000436 518900            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1507   
1508      P:000435 P:000437 447000            MOVE              X0,X:BURST_DEST_HI
                            000030
1509      P:000437 P:000439 517000            MOVE              B0,X:BURST_DEST_LO
                            00002F
1510   
1511      P:000439 P:00043B 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1512   
1513      P:00043A P:00043C 0BF080            JSR     RESTORE_REGISTERS
                            000485
1514      P:00043C P:00043E 000004            RTI                                       ; Main loop will reply after packet transfer
!
1515   
1516   
1517                                ; --------------------------------------------------------------------
1518                                SOFTWARE_RESET
1519                                ;----------------------------------------------------------------------
1520                                ; word 1 = command = 'RST'
1521                                ; word 2-4 unused
1522   
1523      P:00043D P:00043F 0BF080            JSR     SAVE_REGISTERS
                            000492
1524      P:00043F P:000441 45F400            MOVE              #'RST',X1
                            525354
1525      P:000441 P:000443 0D0306            JSR     VCOM_INTRO
1526      P:000442 P:000444 0E2313            JNE     VCOM_EXIT
1527   
1528                                ; RST command OK so reply to host
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 32



1529                                FINISH_RST
1530      P:000443 P:000445 44F400            MOVE              #'000',X0
                            303030
1531      P:000445 P:000447 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1532      P:000447 P:000449 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00046C
1533   
1534      P:000449 P:00044B 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            000449
1535   
1536      P:00044B P:00044D 0A0100            BCLR    #MODE_APPLICATION,X:<MODE         ; clear app flag
1537      P:00044C P:00044E 0A0000            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1538   
1539      P:00044D P:00044F 0C0426            JMP     SYSTEM_RESET                      ; Handle the stack and stuff...
1540   
1541   
1542                                SEND_PACKET_TO_CONTROLLER
1543   
1544                                ;       Host command identifying location of an MCE command to send to
1545                                ;       the MCE.  Since this can come at any time, just record the
1546                                ;       request and then do the CONning from the main loop.
1547   
1548                                ; word 1 = command = 'CON'
1549                                ; word 2 = source host bus address, bits 31:16
1550                                ; word 3 = source host bus address, bits 15:0
1551                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1552                                ;        = '1' --> when MCE command is GO
1553   
1554      P:00044E P:000450 0D0492            JSR     <SAVE_REGISTERS                   ; save working registers
1555   
1556      
1557      P:00044F P:000451 45F400            MOVE              #'CON',X1
                            434F4E
1558      P:000451 P:000453 0D0306            JSR     VCOM_INTRO
1559      P:000452 P:000454 0E2313            JNE     VCOM_EXIT
1560   
1561      
1562      P:000453 P:000455 44F400            MOVE              #'BUS',X0
                            425553
1563      P:000455 P:000457 0A00AA            JSET    #CON_DEMAND,X:STATUS,VCOM_EXIT_ERROR_X0
                            00030C
1564   
1565      
1566      P:000457 P:000459 0A702A            BSET    #CON_DEMAND,X:STATUS
                            000000
1567      P:000459 P:00045B 448800            MOVE              X:<DRXR_WD2,X0
1568      P:00045A P:00045C 458900            MOVE              X:<DRXR_WD3,X1
1569      P:00045B P:00045D 447000            MOVE              X0,X:CON_SRC_HI
                            00002D
1570      P:00045D P:00045F 457000            MOVE              X1,X:CON_SRC_LO
                            00002C
1571   
1572                                ;       ;; Fourth word indicates if this is a go.  Who cares?
1573                                ;       MOVE    X:<DRXR_WD4,A           ; read word 4 - GO command?
1574                                ;       MOVE    #0,X0
1575                                ;       CMP     X0,A
1576                                ;       JEQ     BLOCK_CON
1577   
1578      
1579      P:00045F P:000461 0BF080            JSR     RESTORE_REGISTERS
                            000485
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 33



1580      P:000461 P:000463 000004            RTI
1581   
1583   
1584   
1585                                ;---------------------------------------------------------------
1586                                ;
1587                                ;                          * END OF ISRs *
1588                                ;
1589                                ;--------------------------------------------------------------
1590   
1591   
1592   
1593                                ;----------------------------------------------------------------
1594                                ;
1595                                ;                     * Beginning of SUBROUTINES *
1596                                ;
1597                                ;-----------------------------------------------------------------
1598   
1599   
1600                                CHECK_FO
1601      P:000462 P:000464 01AD80            JCLR    #EF,X:PDRD,CHECK_FO_CLEAR
                            00046A
1602      P:000464 P:000466 000000            NOP
1603      P:000465 P:000467 000000            NOP
1604      P:000466 P:000468 01AD80            JCLR    #EF,X:PDRD,CHECK_FO_CLEAR
                            00046A
1605      P:000468 P:00046A 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1606      P:000469 P:00046B 00000C            RTS
1607   
1608                                CHECK_FO_CLEAR
1609      P:00046A P:00046C 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1610      P:00046B P:00046D 00000C            RTS
1611   
1612   
1613   
1614                                ;----------------------------------------------------------------------------
1615                                PCI_MESSAGE_TO_HOST
1616                                ;----------------------------------------------------------------------------
1617                                ; Subroutine to send 4 words as a reply from PCI to the Host
1618                                ; using the DTXS-HRXS data path.  The DSP signals the host by raising
1619                                ; HF3 and (when !MODE_NOIRQ) INTA.
1620                                ;
1621                                ; When MODE_HANDSHAKE, the DSP and Host interact as follows:
1622                                ; - to show that the Host is handling the interrupt, Host raises HF0
1623                                ; - when DSP sees HF0 go high, it lowers INTA and HF3
1624                                ; - when Host is done handling the interrupt (i.e. it has read the reply),
1625                                ;   and when HF3 is low, Host lowers HF0.
1626                                ; - when DSP sees HF0 go low, the routine finishes.
1627                                ;
1628                                ; The primary advantage of this hand-shaking scheme is that host vector
1629                                ; commands are not needed to clear HF3 and INTA.
1630                                ;
1631                                ; This routine should not block for anything other than the Host handshake.
1632   
1633      P:00046C P:00046E 60F400            MOVE              #>DTXS_WD1,R0
                            00000B
1634   
1635      P:00046E P:000470 060480            DO      #4,PCI_MESSAGE_TO_HOST_10
                            000472
1636      P:000470 P:000472 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            000470
1637      P:000472 P:000474 08D88D            MOVEP             X:(R0)+,X:DTXS
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 34



1638   
1639                                PCI_MESSAGE_TO_HOST_10
1640      P:000473 P:000475 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            000015
1641      P:000475 P:000477 60F000            MOVE              X:SV_R0,R0              ; restore R0
                            000019
1642      P:000477 P:000479 0A8523            BSET    #DCTR_HF3,X:DCTR                  ; Raise HF3 (handshake)
1643   
1644                                                                                    ; Only interrupt in irq mode
1645      P:000478 P:00047A 0A89A5            JSET    #DSR_HF2,X:DSR,PCI_MESSAGE_TO_HOST_20
                            00047B
1646      P:00047A P:00047C 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1647   
1648                                PCI_MESSAGE_TO_HOST_20
1649      P:00047B P:00047D 0A89A4            JSET    #DSR_HF1,X:DSR,PCI_MESSAGE_TO_HOST_HANDSHAKE
                            00047E
1650      P:00047D P:00047F 00000C            RTS
1651   
1652                                PCI_MESSAGE_TO_HOST_HANDSHAKE
1653      P:00047E P:000480 0A8983            JCLR    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            00047E
1654      P:000480 P:000482 0A8506            BCLR    #INTA,X:DCTR                      ; Clear interrupt
1655      P:000481 P:000483 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; Clear hand-shake bit
1656      P:000482 P:000484 0A89A3            JSET    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            000482
1657      P:000484 P:000486 00000C            RTS
1658   
1659   
1660                                ;------------------------------------------------------------------------------------
1661                                RESTORE_REGISTERS
1662                                ;-------------------------------------------------------------------------------------
1663   
1664      P:000485 P:000487 059A39            MOVEC             X:<SV_SR,SR
1665   
1666      P:000486 P:000488 508F00            MOVE              X:<SV_A0,A0
1667      P:000487 P:000489 549000            MOVE              X:<SV_A1,A1
1668      P:000488 P:00048A 529100            MOVE              X:<SV_A2,A2
1669   
1670      P:000489 P:00048B 519200            MOVE              X:<SV_B0,B0
1671      P:00048A P:00048C 559300            MOVE              X:<SV_B1,B1
1672      P:00048B P:00048D 539400            MOVE              X:<SV_B2,B2
1673   
1674      P:00048C P:00048E 449500            MOVE              X:<SV_X0,X0
1675      P:00048D P:00048F 459600            MOVE              X:<SV_X1,X1
1676   
1677      P:00048E P:000490 469700            MOVE              X:<SV_Y0,Y0
1678      P:00048F P:000491 479800            MOVE              X:<SV_Y1,Y1
1679   
1680      P:000490 P:000492 609900            MOVE              X:<SV_R0,R0
1681      P:000491 P:000493 00000C            RTS
1682   
1683                                ;-------------------------------------------------------------------------------------
1684                                SAVE_REGISTERS
1685                                ;-------------------------------------------------------------------------------------
1686   
1687      P:000492 P:000494 051A39            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1688   
1689      P:000493 P:000495 500F00            MOVE              A0,X:<SV_A0
1690      P:000494 P:000496 541000            MOVE              A1,X:<SV_A1
1691      P:000495 P:000497 521100            MOVE              A2,X:<SV_A2
1692   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 35



1693      P:000496 P:000498 511200            MOVE              B0,X:<SV_B0
1694      P:000497 P:000499 551300            MOVE              B1,X:<SV_B1
1695      P:000498 P:00049A 531400            MOVE              B2,X:<SV_B2
1696   
1697      P:000499 P:00049B 441500            MOVE              X0,X:<SV_X0
1698      P:00049A P:00049C 451600            MOVE              X1,X:<SV_X1
1699   
1700      P:00049B P:00049D 461700            MOVE              Y0,X:<SV_Y0
1701      P:00049C P:00049E 471800            MOVE              Y1,X:<SV_Y1
1702   
1703      P:00049D P:00049F 601900            MOVE              R0,X:<SV_R0
1704      P:00049E P:0004A0 00000C            RTS
1705   
1706   
1707                                ;----------------------------------------------
1708                                FLUSH_PCI_FIFO
1709                                ;----------------------------------------------
1710      P:00049F P:0004A1 0A8A84            JCLR    #MARQ,X:DPSR,*
                            00049F
1711      P:0004A1 P:0004A3 0A862E            BSET    #CLRT,X:DPCR
1712      P:0004A2 P:0004A4 000000            NOP
1713      P:0004A3 P:0004A5 0A86AE            JSET    #CLRT,X:DPCR,*
                            0004A3
1714      P:0004A5 P:0004A7 00000C            RTS
1715   
1716                                ;----------------------------------------------
1717                                CLEAR_FO_FIFO
1718                                ;----------------------------------------------
1719      P:0004A6 P:0004A8 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
1720      P:0004A8 P:0004AA 44F400            MOVE              #200000,X0
                            030D40
1721      P:0004AA P:0004AC 06C400            DO      X0,*+3
                            0004AC
1722      P:0004AC P:0004AE 000000            NOP
1723      P:0004AD P:0004AF 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
1724      P:0004AF P:0004B1 00000C            RTS
1725   
1726   
1727                                ;---------------------------------------------------------
1728                                ; PCI burst routines
1729                                ;
1730                                ; For transfer between Host memory and DSP Y memory.
1731                                ;
1732                                ; Major entry points are
1733                                ;       CON_TRANSFER (PC -> DSP)
1734                                ;       BLOCK_TRANSFER (DSP -> PC)
1735                                ;---------------------------------------------------------
1736   
1737                                ;---------------------------------------------------------
1738                                PCI_ERROR_CLEAR
1739                                ;-----------------------------------------------
1740      
1741      
1742      
1743      
1744      
1745      
1746   
1747      P:0004B0 P:0004B2 50F000            MOVE              X:DMA_ERRORS,A0
                            000034
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 36



1748      P:0004B2 P:0004B4 000008            INC     A
1749      P:0004B3 P:0004B5 000000            NOP
1750      P:0004B4 P:0004B6 507000            MOVE              A0,X:DMA_ERRORS
                            000034
1751   
1752      P:0004B6 P:0004B8 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            0004C4
1753      P:0004B8 P:0004BA 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
                            0004CE
1754      P:0004BA P:0004BC 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            0004D8
1755      P:0004BC P:0004BE 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            0004E2
1756      P:0004BE P:0004C0 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            0004EC
1757      P:0004C0 P:0004C2 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            0004F6
1758      P:0004C2 P:0004C4 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            000500
1759   
1760                                ERROR_TRTY
1761      P:0004C4 P:0004C6 50F000            MOVE              X:EC_TRTY,A0
                            000035
1762      P:0004C6 P:0004C8 000008            INC     A
1763      P:0004C7 P:0004C9 08F48A            MOVEP             #>$0400,X:DPSR          ; Clear target retry error bit
                            000400
1764      P:0004C9 P:0004CB 507000            MOVE              A0,X:EC_TRTY
                            000035
1765      P:0004CB P:0004CD 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1766      P:0004CD P:0004CF 00000C            RTS
1767                                ERROR_TO
1768      P:0004CE P:0004D0 50F000            MOVE              X:EC_TO,A0
                            000036
1769      P:0004D0 P:0004D2 000008            INC     A
1770      P:0004D1 P:0004D3 08F48A            MOVEP             #>$0800,X:DPSR          ; Clear timeout error bit
                            000800
1771      P:0004D3 P:0004D5 507000            MOVE              A0,X:EC_TO
                            000036
1772      P:0004D5 P:0004D7 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1773      P:0004D7 P:0004D9 00000C            RTS
1774                                ERROR_TDIS
1775      P:0004D8 P:0004DA 50F000            MOVE              X:EC_TDIS,A0
                            000037
1776      P:0004DA P:0004DC 000008            INC     A
1777      P:0004DB P:0004DD 08F48A            MOVEP             #>$0200,X:DPSR          ; Clear target disconnect bit
                            000200
1778      P:0004DD P:0004DF 507000            MOVE              A0,X:EC_TDIS
                            000037
1779      P:0004DF P:0004E1 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1780      P:0004E1 P:0004E3 00000C            RTS
1781                                ERROR_TAB
1782      P:0004E2 P:0004E4 50F000            MOVE              X:EC_TAB,A0
                            000038
1783      P:0004E4 P:0004E6 000008            INC     A
1784      P:0004E5 P:0004E7 08F48A            MOVEP             #>$0100,X:DPSR          ; Clear target abort error bit
                            000100
1785      P:0004E7 P:0004E9 507000            MOVE              A0,X:EC_TAB
                            000038
1786      P:0004E9 P:0004EB 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 37



                            000000
1787      P:0004EB P:0004ED 00000C            RTS
1788                                ERROR_MAB
1789      P:0004EC P:0004EE 50F000            MOVE              X:EC_MAB,A0
                            000039
1790      P:0004EE P:0004F0 000008            INC     A
1791      P:0004EF P:0004F1 08F48A            MOVEP             #>$0080,X:DPSR          ; Clear master abort error bit
                            000080
1792      P:0004F1 P:0004F3 507000            MOVE              A0,X:EC_MAB
                            000039
1793      P:0004F3 P:0004F5 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1794      P:0004F5 P:0004F7 00000C            RTS
1795                                ERROR_DPER
1796      P:0004F6 P:0004F8 50F000            MOVE              X:EC_DPER,A0
                            00003A
1797      P:0004F8 P:0004FA 000008            INC     A
1798      P:0004F9 P:0004FB 08F48A            MOVEP             #>$0040,X:DPSR          ; Clear data parity error bit
                            000040
1799      P:0004FB P:0004FD 507000            MOVE              A0,X:EC_DPER
                            00003A
1800      P:0004FD P:0004FF 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1801      P:0004FF P:000501 00000C            RTS
1802                                ERROR_APER
1803      P:000500 P:000502 50F000            MOVE              X:EC_APER,A0
                            00003B
1804      P:000502 P:000504 000008            INC     A
1805      P:000503 P:000505 08F48A            MOVEP             #>$0020,X:DPSR          ; Clear address parity error bit
                            000020
1806      P:000505 P:000507 507000            MOVE              A0,X:EC_APER
                            00003B
1807      P:000507 P:000509 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1808      P:000509 P:00050B 00000C            RTS
1809   
1810   
1811   
1812                                ;----------------------------------------------
1813                                BLOCK_TRANSFER
1814                                ;----------------------------------------------
1815                                ;   In:
1816                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
1817                                ;   - BLOCK_SIZE is packet size, in bytes
1818                                ;   - YMEM_SRC is start of data in Y memory
1819                                ;  Out:
1820                                ;   - BLOCK_SIZE will be decremented to zero.
1821                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
1822                                ;   - YMEM_SRC will be incremented by BLOCK_SIZE/2
1823                                ;  Trashes:
1824                                ;   - A and B at least
1825   
1826      P:00050A P:00050C 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002B
1827      P:00050C P:00050E 014085            CMP     #0,A                              ; Still bytes to transfer?
1828      P:00050D P:00050F 0AF0A2            JNE     BLOCK_TRANSFER0
                            000510
1829      P:00050F P:000511 00000C            RTS
1830   
1831                                BLOCK_TRANSFER0
1832      
1833      
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 38



1834      P:000510 P:000512 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000029
1835   
1836      P:000512 P:000514 200005            CMP     B,A                               ; A ? B
1837      P:000513 P:000515 0E1515            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
1838      P:000514 P:000516 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1839                                BLOCK_TRANSFER1
1840      
1841      P:000515 P:000517 200014            SUB     B,A                               ; A -= B
1842      P:000516 P:000518 014088            ADD     #0,B                              ; Clear carry bit
1843      P:000517 P:000519 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002B
1844      P:000519 P:00051B 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002A
1845      P:00051B P:00051D 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1846   
1847      
1848      P:00051C P:00051E 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
1849      P:00051E P:000520 50F000            MOVE              X:YMEM_SRC,A0
                            00002E
1850      P:000520 P:000522 08C82F            MOVEP             A0,X:DSR0               ; DMA source
1851      P:000521 P:000523 200010            ADD     B,A
1852      P:000522 P:000524 00000B            DEC     B
1853      P:000523 P:000525 507000            MOVE              A0,X:YMEM_SRC           ; BURST_SRC += BURST_SIZE/2
                            00002E
1854   
1855      P:000525 P:000527 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1856   
1857      
1858      P:000526 P:000528 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
1859   
1860                                BLOCK_TRANSFER_PCI
1861      P:000528 P:00052A 44F400            MOVE              #>$7,X0                 ; Memory write
                            000007
1862      P:00052A P:00052C 60F400            MOVE              #BURST_DEST_LO,R0       ; RAM address
                            00002F
1863      P:00052C P:00052E 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            00057F
1864   
1865      
1866      P:00052E P:000530 0A8A84            JCLR    #MARQ,X:DPSR,*
                            00052E
1867   
1868      
1869      P:000530 P:000532 0A8A8E            JCLR    #MDT,X:DPSR,BLOCK_TRANSFER_HANDLE_ERRORS
                            000538
1870   
1871      P:000532 P:000534 20001B            CLR     B
1872      P:000533 P:000535 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            00002A
1873      P:000535 P:000537 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            000693
1874      P:000537 P:000539 0C050A            JMP     BLOCK_TRANSFER                    ; Next burst in block
1875   
1876                                BLOCK_TRANSFER_HANDLE_ERRORS
1877      
1878      P:000538 P:00053A 0D04B0            JSR     PCI_ERROR_CLEAR
1879   
1880      P:000539 P:00053B 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 39



                            000000
1881      P:00053B P:00053D 0E8528            JCS     BLOCK_TRANSFER_PCI                ; Restart PCI burst
1882   
1883      P:00053C P:00053E 0A7012            BCLR    #PCIDMA_RETRY,X:STATUS            ; Test and clear
                            000000
1884      P:00053E P:000540 0E050A            JCC     BLOCK_TRANSFER                    ; Error but no error? Redo this burst.
1885   
1886      
1887      P:00053F P:000541 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            000595
1888      P:000541 P:000543 0BF080            JSR     PCI_UPDATE_R0
                            0005A5
1889      P:000543 P:000545 0C0528            JMP     BLOCK_TRANSFER_PCI
1890   
1891   
1892                                ;----------------------------------------------
1893                                CON_TRANSFER
1894                                ;----------------------------------------------
1895                                ;   In:
1896                                ;   - BURST_SRC_HI:BURST_SRC_LO is PC RAM address
1897                                ;   - BLOCK_SIZE is packet size, in bytes
1898                                ;   - YMEM_DEST is start of data in Y memory
1899                                ;  Out:
1900                                ;   - BLOCK_SIZE will be decremented to zero.
1901                                ;   - BURST_SRC_HI:LO will be incremented by BLOCK_SIZE
1902                                ;   - YMEM_DEST will be incremented by BLOCK_SIZE/2
1903                                ;  Trashes:
1904                                ;   - A and B, R0, X0
1905   
1906      P:000544 P:000546 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002B
1907      P:000546 P:000548 014085            CMP     #0,A                              ; Still bytes to transfer?
1908      P:000547 P:000549 0AF0A2            JNE     CON_TRANSFER0
                            00054A
1909      P:000549 P:00054B 00000C            RTS
1910   
1911                                CON_TRANSFER0
1912      
1913      
1914      P:00054A P:00054C 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000029
1915   
1916      P:00054C P:00054E 200005            CMP     B,A                               ; A ? B
1917      P:00054D P:00054F 0E154F            JGE     <CON_TRANSFER1                    ; jump if A >= B
1918      P:00054E P:000550 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1919                                CON_TRANSFER1
1920      
1921      P:00054F P:000551 200014            SUB     B,A                               ; A -= B
1922      P:000550 P:000552 014088            ADD     #0,B                              ; Clear carry bit
1923      P:000551 P:000553 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002B
1924      P:000553 P:000555 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002A
1925      P:000555 P:000557 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1926   
1927      
1928      P:000556 P:000558 50F000            MOVE              X:YMEM_DEST,A0
                            000033
1929      P:000558 P:00055A 507000            MOVE              A0,X:DDR0               ; DMA dest'n
                            FFFFEE
1930      P:00055A P:00055C 08F4AF            MOVEP             #>DRXR,X:DSR0           ; DMA source
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 40



                            FFFFCB
1931      P:00055C P:00055E 200010            ADD     B,A
1932      P:00055D P:00055F 00000B            DEC     B
1933      P:00055E P:000560 507000            MOVE              A0,X:YMEM_DEST          ; YMEM_DEST += BURST_SIZE/2
                            000033
1934   
1935      P:000560 P:000562 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1936   
1937      
1938      P:000561 P:000563 08F4AC            MOVEP             #$8EEAC4,X:DCR0
                            8EEAC4
1939   
1940                                CON_TRANSFER_PCI
1941      P:000563 P:000565 44F400            MOVE              #>$6,X0                 ; Memory write
                            000006
1942      P:000565 P:000567 60F400            MOVE              #BURST_SRC_LO,R0        ; RAM address
                            000031
1943      P:000567 P:000569 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            00057F
1944   
1945      
1946      P:000569 P:00056B 0A8A84            JCLR    #MARQ,X:DPSR,*
                            000569
1947   
1948      
1949      P:00056B P:00056D 0A8A8E            JCLR    #MDT,X:DPSR,CON_TRANSFER_HANDLE_ERRORS
                            000573
1950   
1951      P:00056D P:00056F 20001B            CLR     B
1952      P:00056E P:000570 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            00002A
1953      P:000570 P:000572 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            000693
1954      P:000572 P:000574 0C0544            JMP     CON_TRANSFER                      ; Next burst in block
1955   
1956                                CON_TRANSFER_HANDLE_ERRORS
1957      
1958      P:000573 P:000575 0D04B0            JSR     PCI_ERROR_CLEAR
1959   
1960      P:000574 P:000576 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1961      P:000576 P:000578 0E8563            JCS     CON_TRANSFER_PCI                  ; Restart PCI burst
1962   
1963      P:000577 P:000579 0A7012            BCLR    #PCIDMA_RETRY,X:STATUS            ; Test and clear
                            000000
1964      P:000579 P:00057B 0E0544            JCC     CON_TRANSFER                      ; Error but no error? Redo this burst.
1965   
1966      
1967      P:00057A P:00057C 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            000595
1968      P:00057C P:00057E 0BF080            JSR     PCI_UPDATE_R0
                            0005A5
1969      P:00057E P:000580 0C0563            JMP     CON_TRANSFER_PCI
1970   
1971                                ; Utility routines for BLOCK_TRANSFER and CON_TRANSFER
1972   
1973                                PCI_GO
1974                                ; Initiate PCI read/write of BURST_SIZE bytes.
1975                                ; R0 must point to the hi-lo PCI address source/dest address
1976                                ; X0 is the PCI command (6 is read, 7 is write).
1977                                ; Trashes A and B but not R0 and X0.
1978      P:00057F P:000581 200013            CLR     A
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 41



1979      P:000580 P:000582 20001B            CLR     B
1980      P:000581 P:000583 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            00002A
1981      P:000583 P:000585 00000B            DEC     B                                 ; n8 - 1
1982      P:000584 P:000586 014088            ADD     #0,B                              ; Clear carry
1983      P:000585 P:000587 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
1984      P:000586 P:000588 014088            ADD     #0,B                              ; Clear carry
1985      P:000587 P:000589 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
1986   
1987      P:000588 P:00058A 0200D8            MOVE              X:(R0+1),A0             ; PCI HI address
1988   
1989      P:000589 P:00058B 200010            ADD     B,A
1990      P:00058A P:00058C 000000            NOP
1991      P:00058B P:00058D 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
1992   
1993      P:00058D P:00058F 208800            MOVE              X0,A0
1994      P:00058E P:000590 014088            ADD     #0,B                              ; Clear carry
1995      P:00058F P:000591 0C1D20            ASL     #16,A,A                           ; Command into bits 19:16
1996      P:000590 P:000592 51E000            MOVE              X:(R0),B0
1997      P:000591 P:000593 200010            ADD     B,A
1998      P:000592 P:000594 000000            NOP
1999   
2000      P:000593 P:000595 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
2001      P:000594 P:000596 00000C            RTS
2002   
2003   
2004                                PCI_RECOVER_COUNT
2005                                ; Calculate number of PCI words not transferred.
2006                                ; Correct BURST_SIZE.  Returns:
2007                                ;   B: bytes not transferred
2008                                ;   A: bytes transferred
2009      P:000595 P:000597 200013            CLR     A
2010      P:000596 P:000598 20001B            CLR     B
2011      P:000597 P:000599 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
2012      P:000598 P:00059A 0A8A8F            JCLR    #RDCQ,X:DPSR,PCI_RECOVER_COUNT1
                            00059B
2013      P:00059A P:00059C 000009            INC     B
2014                                PCI_RECOVER_COUNT1
2015      P:00059B P:00059D 000009            INC     B                                 ; We want N, not N-1.
2016      P:00059C P:00059E 014088            ADD     #0,B                              ; Clear carry
2017      P:00059D P:00059F 0C1C20            ASR     #16,A,A
2018      P:00059E P:0005A0 200018            ADD     A,B                               ; B is words remaining
2019      P:00059F P:0005A1 014088            ADD     #0,B                              ; Clear carry
2020      P:0005A0 P:0005A2 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
2021      P:0005A1 P:0005A3 50F000            MOVE              X:BURST_SIZE,A0
                            00002A
2022      P:0005A3 P:0005A5 200014            SUB     B,A                               ; A is bytes written
2023      P:0005A4 P:0005A6 00000C            RTS
2024   
2025   
2026                                PCI_UPDATE_R0
2027                                ;  Subtract A0 from BURST_SIZE and add A to the 32-bit hi-lo address at X:[R0].
2028                                ;  Caller can check Z flag to see if BURST_SIZE is now 0.
2029      P:0005A5 P:0005A7 210500            MOVE              A0,X1                   ; Save A for later
2030      P:0005A6 P:0005A8 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2031      P:0005A7 P:0005A9 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates [R0] = [R0] + B
                            000693
2032   
2033      P:0005A9 P:0005AB 57F000            MOVE              X:BURST_SIZE,B
                            00002A
2034      P:0005AB P:0005AD 20006C            SUB     X1,B                              ; Zero flag must be preserved!
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 42



2035      P:0005AC P:0005AE 000000            NOP
2036      P:0005AD P:0005AF 557000            MOVE              B1,X:BURST_SIZE
                            00002A
2037      P:0005AF P:0005B1 00000C            RTS
2038   
2039   
2040                                ;----------------------------------------------;
2041                                ;  MCE PACKET PROCESSING                       ;
2042                                ;----------------------------------------------;
2043   
2044                                ;       Given a dword count in A, computes number of half FIFOs and
2045                                ;       number of left over FIFO reads required to get the whole
2046                                ;       packet.
2047   
2048                                ;       Input: A is packet size, in dwords
2049                                ;       Output: sets X:TOTAL_BUFFS and X:LEFT_TO_READ
2050                                ;       Trashes: A,B,X0
2051   
2052   
2053                                PACKET_PARTITIONS
2054      P:0005B0 P:0005B2 507000            MOVE              A0,X:PACKET_SIZE
                            000023
2055   
2056      P:0005B2 P:0005B4 014088            ADD     #0,B                              ; Clear carry
2057      P:0005B3 P:0005B5 0C1D02            ASL     #1,A,A                            ;  * 2
2058      P:0005B4 P:0005B6 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
2059      P:0005B5 P:0005B7 240000            MOVE              #0,X0
2060      P:0005B6 P:0005B8 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
2061   
2062      P:0005B8 P:0005BA 557000            MOVE              B1,X:TOTAL_BUFFS
                            000024
2063      P:0005BA P:0005BC 507000            MOVE              A0,X:LEFT_TO_READ
                            000025
2064      P:0005BC P:0005BE 00000C            RTS
2065   
2066   
2067                                ; BUFFER_PACKET
2068                                ;
2069                                ; Copies the packet in the FIFO to Y memory.
2070                                ;
2071                                ; In: TOTAL_BUFFS and LEFT_TO_READ must be pre-set (via PACKET_PARTITIONS);
2072                                ;     R1 is the destination index in Y memory.
2073                                ; Trashes: R1 is updated to point to the end of the copied data.
2074   
2075                                BUFFER_PACKET
2076   
2077      P:0005BD P:0005BF 54F400            MOVE              #>$b00,A1
                            000B00
2078      P:0005BF P:0005C1 0BF080            JSR     TIMER_STORE_A1
                            000641
2079      P:0005C1 P:0005C3 0BF080            JSR     TIMER_STORE
                            00063F
2080   
2081      P:0005C3 P:0005C5 062400            DO      X:TOTAL_BUFFS,BUFFER_PACKET_HALFS_DONE
                            0005CD
2082      P:0005C5 P:0005C7 0BF080            JSR     WAIT_FIFO_HALF
                            0005EA
2083      P:0005C7 P:0005C9 0BF080            JSR     TIMER_STORE
                            00063F
2084      P:0005C9 P:0005CB 0BF080            JSR     BUFFER_PACKET_HALF
                            0005E5
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 43



2085      P:0005CB P:0005CD 0BF080            JSR     TIMER_STORE
                            00063F
2086      P:0005CD P:0005CF 000000            NOP
2087                                BUFFER_PACKET_HALFS_DONE
2088   
2089      
2090      
2091      
2092      
2093      P:0005CE P:0005D0 01AD81            JCLR    #HF,X:PDRD,BUFFER_PACKET_SINGLES_FAST
                            0005E1
2094   
2095      
2096      
2097   
2098                                BUFFER_PACKET_SINGLES
2099      
2100      
2101      P:0005D0 P:0005D2 200013            CLR     A
2102      P:0005D1 P:0005D3 20001B            CLR     B
2103      P:0005D2 P:0005D4 51F000            MOVE              X:TCR0,B0               ; Store timer value (50 MHz)
                            FFFF8C
2104      P:0005D4 P:0005D6 0C1C85            ASR     #2,B,B                            ; / 4
2105      P:0005D5 P:0005D7 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_DONE
                            0005DD
2106                                BUFFER_PACKET_SINGLES_WAIT
2107      P:0005D7 P:0005D9 50F000            MOVE              X:TCR0,A0
                            FFFF8C
2108      P:0005D9 P:0005DB 0C1C04            ASR     #2,A,A
2109      P:0005DA P:0005DC 20000D            CMP     A,B
2110      P:0005DB P:0005DD 0EA5D7            JEQ     BUFFER_PACKET_SINGLES_WAIT
2111      P:0005DC P:0005DE 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2112      P:0005DD P:0005DF 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2113                                BUFFER_PACKET_SINGLES_DONE
2114      P:0005DE P:0005E0 0BF080            JSR     TIMER_STORE
                            00063F
2115      P:0005E0 P:0005E2 00000C            RTS
2116   
2117                                ;---------------------------------------------------------
2118   
2119                                BUFFER_PACKET_SINGLES_FAST
2120      P:0005E1 P:0005E3 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_FAST_DONE
                            0005E3
2121      P:0005E3 P:0005E5 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2122                                BUFFER_PACKET_SINGLES_FAST_DONE
2123      P:0005E4 P:0005E6 00000C            RTS
2124   
2125                                ;---------------------------------------------------------
2126                                BUFFER_PACKET_HALF
2127      
2128      P:0005E5 P:0005E7 060082            DO      #512,BUFFER_PACKET_HALF_DONE
                            0005E8
2129      P:0005E7 P:0005E9 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2130      P:0005E8 P:0005EA 000000            NOP
2131                                BUFFER_PACKET_HALF_DONE
2132      P:0005E9 P:0005EB 00000C            RTS
2133   
2134                                ;---------------------------------------------------------
2135                                WAIT_FIFO_HALF
2136      P:0005EA P:0005EC 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            0005FF
2137      P:0005EC P:0005EE 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 44



                            0005EA
2138      P:0005EE P:0005F0 000000            NOP
2139      P:0005EF P:0005F1 000000            NOP
2140      P:0005F0 P:0005F2 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            0005EA
2141      P:0005F2 P:0005F4 00000C            RTS
2142   
2143                                ;---------------------------------------------------------
2144   
2145                                ; This is the old single-buffering routine, which polls the EF.
2146                                BUFFER_PACKET_SINGLES_POLL
2147      P:0005F3 P:0005F5 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_DONE
                            0005FD
2148                                BUFFER_PACKET_SINGLE
2149      P:0005F5 P:0005F7 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D5
2150      P:0005F7 P:0005F9 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE
                            0005F5
2151      P:0005F9 P:0005FB 000000            NOP
2152      P:0005FA P:0005FC 000000            NOP
2153      P:0005FB P:0005FD 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE   ; Protect against metastability
                            0005F5
2154      P:0005FD P:0005FF 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2155                                BUFFER_PACKET_DONE
2156      P:0005FE P:000600 00000C            RTS
2157   
2158                                ;---------------------------------------------------------
2159   
2160                                FATALITY_HANDLER
2161      P:0005FF P:000601 0C0100            JMP     START                             ; What could possibly go wrong?
2162   
2163   
2164                                ; DROP_PACKET
2165                                ;
2166                                ; Reads a packet from the fifo, discarding it.
2167                                ;
2168                                ; In: TOTAL_BUFFS & LEFT_TO_READ
2169                                ; Trashes: A0
2170   
2171                                DROP_PACKET
2172      P:000600 P:000602 062400            DO      X:TOTAL_BUFFS,DROP_PACKET_SINGLES
                            000605
2173      P:000602 P:000604 0D05EA            JSR     WAIT_FIFO_HALF
2174      P:000603 P:000605 0BF080            JSR     DROP_FIFO_HALF
                            000610
2175      P:000605 P:000607 000000            NOP
2176                                DROP_PACKET_SINGLES
2177      P:000606 P:000608 062500            DO      X:LEFT_TO_READ,DROP_PACKET_DONE
                            00060E
2178                                DROP_PACKET_SINGLE
2179      P:000608 P:00060A 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D5
2180      P:00060A P:00060C 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE
                            000608
2181      P:00060C P:00060E 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE     ; Protect against metastability
                            000608
2182      P:00060E P:000610 09483F            MOVEP             Y:RDFIFO,A0
2183                                DROP_PACKET_DONE
2184      P:00060F P:000611 00000C            RTS
2185   
2186                                DROP_FIFO_HALF
2187      
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 45



2188      P:000610 P:000612 060082            DO      #512,DROP_FIFO_DONE
                            000612
2189      P:000612 P:000614 09483F            MOVEP             Y:RDFIFO,A0
2190                                DROP_FIFO_DONE
2191      P:000613 P:000615 00000C            RTS
2192   
2193   
2194                                ;----------------------------------------------;
2195                                ;  TIMER HANDLING                              ;
2196                                ;----------------------------------------------;
2197   
2198                                ; Start value is TLR, count is in TCR, flag marked at TCPR
2199                                ; Must set TCSR[TCIE] to enable int
2200                                ; Must set TCSR[T] for timer to restart
2201   
2202                                TIMER_ENABLE
2203      P:000614 P:000616 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2204      P:000616 P:000618 000000            NOP
2205      P:000617 P:000619 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2206      P:000619 P:00061B 00000C            RTS
2207   
2208                                TIMER_DISABLE
2209      P:00061A P:00061C 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
                            300200
2210      P:00061C P:00061E 000000            NOP
2211      P:00061D P:00061F 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2212      P:00061F P:000621 00000C            RTS
2213   
2214                                TIMER_DEFAULT
2215      P:000620 P:000622 0D061A            JSR     TIMER_DISABLE
2216      P:000621 P:000623 44F400            MOVE              #$4C4B40,X0             ; 5M -> 10 Hz.
                            4C4B40
2217      P:000623 P:000625 000000            NOP
2218      P:000624 P:000626 447000            MOVE              X0,X:TCPR0
                            FFFF8D
2219      P:000626 P:000628 0D0614            JSR     TIMER_ENABLE
2220      P:000627 P:000629 00000C            RTS
2221   
2222   
2224                                TIMER_ACTION
2225      P:000628 P:00062A 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2226      P:00062A P:00062C 000000            NOP
2227      P:00062B P:00062D 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2228      P:00062D P:00062F 56F000            MOVE              X:QT_INFORM_IDX,A       ; QT inform time?
                            000046
2229      P:00062F P:000631 0A0182            JCLR    #MODE_QT,X:MODE,TIMER_ACTION_OK
                            000637
2230      P:000631 P:000633 0140C5            CMP     #>0,A                             ; If inform_idx != 0
                            000000
2231      P:000633 P:000635 0AF0AA            JEQ     TIMER_ACTION_OK
                            000637
2232      P:000635 P:000637 0A7034            BSET    #QT_FLUSH,X:STATUS                ;    schedule inform
                            000000
2233                                TIMER_ACTION_OK
2234      P:000637 P:000639 00000C            RTS
2235   
2236   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 46



2237                                ;----------------------------------------------;
2238                                ;  TIMER UTILITY                               ;
2239                                ;----------------------------------------------;
2240   
2241                                 TIMER_SOURCE
2242      FFFF8C                              EQU     TCR0
2243   
2244                                TIMER_STORE_INIT
2245      P:000638 P:00063A 50F400            MOVE              #>TIMER_BUFFER,A0
                            201000
2246      P:00063A P:00063C 000000            NOP
2247      P:00063B P:00063D 507000            MOVE              A0,X:TIMER_INDEX
                            00004C
2248      P:00063D P:00063F 211400            MOVE              A0,R4
2249      P:00063E P:000640 00000C            RTS
2250   
2251                                TIMER_STORE
2252      
2253      
2254      P:00063F P:000641 56F000            MOVE              X:TIMER_SOURCE,A
                            FFFF8C
2255                                                                                    ; Fall-through
2256   
2257                                TIMER_STORE_A1
2258      
2259      P:000641 P:000643 5C5C00            MOVE                          A1,Y:(R4)+
2260      P:000642 P:000644 228C00            MOVE              R4,A1
2261      P:000643 P:000645 0140C5            CMP     #>TIMER_BUFFER_END,A
                            202000
2262      P:000645 P:000647 547000            MOVE              A1,X:TIMER_INDEX
                            00004C
2263      P:000647 P:000649 0E1638            JGE     TIMER_STORE_INIT
2264      P:000648 P:00064A 00000C            RTS
2265   
2266   
2267                                ;----------------------------------------------;
2268                                ;  CIRCULAR BUFFER HANDLING                    ;
2269                                ;----------------------------------------------;
2270   
2271                                BUFFER_INCR
2272   
2273      P:000649 P:00064B 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
                            000042
2274      P:00064B P:00064D 014180            ADD     #1,A                              ;
2275      P:00064C P:00064E 57F000            MOVE              X:QT_BUF_MAX,B          ;
                            00003F
2276      P:00064E P:000650 20000D            CMP     A,B                               ;
2277      P:00064F P:000651 0AF0AF            JLE     BUFFER_RESET                      ;       head = 0
                            00065B
2278                                                                                    ; else
2279      P:000651 P:000653 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            000042
2280   
2281      P:000653 P:000655 20001B            CLR     B
2282      P:000654 P:000656 51F000            MOVE              X:QT_BUF_SIZE,B0
                            00003E
2283      P:000656 P:000658 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2284      P:000658 P:00065A 0BF080            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
                            000693
2285   
2286      P:00065A P:00065C 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 47



2287   
2288   
2289                                BUFFER_RESET
2290      P:00065B P:00065D 60F400            MOVE              #QT_BASE_LO,R0
                            00003C
2291      P:00065D P:00065F 0BF080            JSR     LOAD_HILO_ADDRESS
                            00068D
2292      P:00065F P:000661 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2293      P:000661 P:000663 0BF080            JSR     SAVE_HILO_ADDRESS                 ; QT_DEST_LO = QT_BASE_LO
                            000695
2294   
2295      P:000663 P:000665 240000            MOVE              #0,X0
2296      P:000664 P:000666 447000            MOVE              X0,X:QT_BUF_HEAD        ; HEAD = 0
                            000042
2297      P:000666 P:000668 00000C            RTS
2298   
2299   
2300                                BUFFER_INFORM_CHECK
2301      P:000667 P:000669 56F000            MOVE              X:QT_INFORM_IDX,A
                            000046
2302      P:000669 P:00066B 014180            ADD     #1,A
2303      P:00066A P:00066C 57F000            MOVE              X:QT_INFORM,B
                            000041
2304      P:00066C P:00066E 20000D            CMP     A,B
2305      P:00066D P:00066F 0AF0A7            JGT     BUFFER_INFORM_OK                  ; If inform_idx + 1 <= inform
                            000671
2306      P:00066F P:000671 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2307   
2308                                BUFFER_INFORM_OK
2309      P:000671 P:000673 567000            MOVE              A,X:QT_INFORM_IDX       ; inform_idx = inform_idx + 1
                            000046
2310      P:000673 P:000675 00000C            RTS
2311   
2312   
2313                                ;---------------------------------------------------------------
2314                                BUFFER_INFORM
2315                                ;---------------------------------------------------------------
2316                                ; Informs host of current buffer status
2317   
2318      
2319      P:000674 P:000676 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            00068C
2320      P:000676 P:000678 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            00068C
2321   
2322                                          PCI_LOCKDOWN                              ; Disable host IRQ
2324   
2325      P:000679 P:00067B 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2326      P:00067B P:00067D 440B00            MOVE              X0,X:<DTXS_WD1
2327   
2328      P:00067C P:00067E 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            000042
2329      P:00067E P:000680 440C00            MOVE              X0,X:<DTXS_WD2
2330   
2331      P:00067F P:000681 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000043
2332      P:000681 P:000683 440D00            MOVE              X0,X:<DTXS_WD3
2333   
2334      P:000682 P:000684 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  main.asm  Page 48



                            000047
2335      P:000684 P:000686 440E00            MOVE              X0,X:<DTXS_WD4
2336   
2337      P:000685 P:000687 0D046C            JSR     PCI_MESSAGE_TO_HOST
2338   
2339      P:000686 P:000688 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
2340      P:000688 P:00068A 240000            MOVE              #0,X0                   ; Reset inform index
2341      P:000689 P:00068B 447000            MOVE              X0,X:QT_INFORM_IDX
                            000046
2342                                          PCI_LOCKUP                                ; Enable host IRQ
2344                                INFORM_EXIT
2345      P:00068C P:00068E 00000C            RTS
2346   
2347   
2348   
2349                                ;----------------------------------------------;
2350                                ;  ADDRESS HANDLING                            ;
2351                                ;----------------------------------------------;
2352   
2356   
2357                                LOAD_HILO_ADDRESS
2358      
2359      
2360      P:00068D P:00068F 200013            CLR     A
2361      P:00068E P:000690 50D800            MOVE              X:(R0)+,A0
2362      P:00068F P:000691 44D000            MOVE              X:(R0)-,X0
2363      P:000690 P:000692 0C1940            INSERT  #$010010,X0,A
                            010010
2364      P:000692 P:000694 00000C            RTS
2365   
2366                                ADD_HILO_ADDRESS
2367      
2368      
2369   
2370      P:000693 P:000695 0D068D            JSR     LOAD_HILO_ADDRESS
2371      P:000694 P:000696 200010            ADD     B,A
2372   
2373                                SAVE_HILO_ADDRESS
2374      
2375      
2376   
2377      P:000695 P:000697 445800            MOVE              X0,X:(R0)+              ; pre-increment
2378      P:000696 P:000698 240000            MOVE              #0,X0
2379      P:000697 P:000699 0C1D11            ASL     #8,A,B
2380      P:000698 P:00069A 0C1940            INSERT  #$008010,X0,A
                            008010
2381      P:00069A P:00069C 555000            MOVE              B1,X:(R0)-              ; store hi16
2382      P:00069B P:00069D 506000            MOVE              A0,X:(R0)
2383      P:00069C P:00069E 0C1C90            ASR     #8,B,A
2384      P:00069D P:00069F 00000C            RTS
2385   
2386   
2387                                BOOTCODE_END
2388                                 BOOTEND_ADDR
2389      00069E                              EQU     @CVI(BOOTCODE_END)
2390   
2391                                PROGRAM_END
2392      00069E                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2393                                          INCLUDE 'vars.asm'
2394                                      COMMENT *
2395   
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  vars.asm  Page 49



2396                                Variable table and bit defines for our variables.
2397   
2398                                See info.asm for versioning and authors.
2399   
2400                                        *
2401   
2402   
2403                                ; The variable table is mapped to X memory but stored inline in the
2404                                ; eeprom / P memory after the main code (but before the application
2405                                ; area).
2406   
2407      X:000000 P:0006A0                   ORG     X:VAR_TBL,P:
2408   
2409   
2410                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2411                                 VAR_TBL_START
2412      00069E                              EQU     @LCV(L)-2
2413                                          ENDIF
2414   
2415                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2417                                          ENDIF
2418   
2419   
2420                                ;-----------------------------------------------
2421 d    X:000000 P:0006A0 000000  STATUS    DC      0                                 ; Internal status flags
2422 d    X:000001 P:0006A1 000000  MODE      DC      0                                 ; Operating mode control
2423 d                               FRAME_COUNT
2424 d    X:000002 P:0006A2 000000            DC      0                                 ; Count of data frames from MCE
2425   
2426                                ;-----------------------------------------------
2427 d    X:000003 P:0006A3 550105  REV_NUMBER DC     $550105                           ; byte 0 = minor revision #
2428                                                                                    ; byte 1 = major revision #
2429                                                                                    ; byte 2 = release Version (ascii letter)
2430 d    X:000004 P:0006A4 000000  REV_DATA  DC      $000000                           ; Not used by UBC
2431 d    X:000005 P:0006A5 2EF490  P_CHECKSUM DC     $2EF490                           ; Not used by UBC
2432   
2433                                ;-----------------------------------------------
2434 d    X:000006 P:0006A6 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2435                                ;-----------------------------------------------
2436   
2437   
2439   
2440 d    X:000007 P:0006A7 000000  DRXR_WD1  DC      0                                 ; Storage for words read from PC during vect
or command
2441 d    X:000008 P:0006A8 000000  DRXR_WD2  DC      0
2442 d    X:000009 P:0006A9 000000  DRXR_WD3  DC      0
2443 d    X:00000A P:0006AA 000000  DRXR_WD4  DC      0
2444   
2445 d    X:00000B P:0006AB 000000  DTXS_WD1  DC      0                                 ; Storage for words to be written to PC as r
eply
2446 d    X:00000C P:0006AC 000000  DTXS_WD2  DC      0
2447 d    X:00000D P:0006AD 000000  DTXS_WD3  DC      0
2448 d    X:00000E P:0006AE 000000  DTXS_WD4  DC      0
2449   
2450   
2452   
2453 d    X:00000F P:0006AF 000000  SV_A0     DC      0
2454 d    X:000010 P:0006B0 000000  SV_A1     DC      0
2455 d    X:000011 P:0006B1 000000  SV_A2     DC      0
2456 d    X:000012 P:0006B2 000000  SV_B0     DC      0
2457 d    X:000013 P:0006B3 000000  SV_B1     DC      0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  vars.asm  Page 50



2458 d    X:000014 P:0006B4 000000  SV_B2     DC      0
2459 d    X:000015 P:0006B5 000000  SV_X0     DC      0
2460 d    X:000016 P:0006B6 000000  SV_X1     DC      0
2461 d    X:000017 P:0006B7 000000  SV_Y0     DC      0
2462 d    X:000018 P:0006B8 000000  SV_Y1     DC      0
2463 d    X:000019 P:0006B9 000000  SV_R0     DC      0
2464 d    X:00001A P:0006BA 000000  SV_SR     DC      0
2465   
2466   
2468   
2469 d    X:00001B P:0006BB 000000  HEAD_W1_0 DC      0                                 ; Preamble $A5A5
2470 d    X:00001C P:0006BC 000000  HEAD_W1_1 DC      0                                 ;          $A5A5
2471 d    X:00001D P:0006BD 000000  HEAD_W2_0 DC      0                                 ;          $5A5A
2472 d    X:00001E P:0006BE 000000  HEAD_W2_1 DC      0                                 ;          $5A5A
2473 d    X:00001F P:0006BF 000000  HEAD_W3_0 DC      0                                 ; 'RP' or 'DA'
2474 d    X:000020 P:0006C0 000000  HEAD_W3_1 DC      0                                 ; '  '   $2020
2475 d    X:000021 P:0006C1 000000  HEAD_W4_0 DC      0                                 ; Packet size LSW
2476 d    X:000022 P:0006C2 000000  HEAD_W4_1 DC      0                                 ;             MSW
2477   
2478   
2480   
2481 d                               PACKET_SIZE
2482 d    X:000023 P:0006C3 000000            DC      0                                 ; Size, in dwords of most recent packet from
 MCE.
2483 d                               TOTAL_BUFFS
2484 d    X:000024 P:0006C4 000000            DC      0                                 ; Number of 512 word half-buffers in packet.
2485 d                               LEFT_TO_READ
2486 d    X:000025 P:0006C5 000000            DC      0                                 ; Number of words left to read after last 51
2 buffer
2487   
2488 d                               PREAMBLE_ERRORS
2489 d    X:000026 P:0006C6 000000            DC      0                                 ; Failed on preamble processing
2490 d                               PTYPE_ERRORS
2491 d    X:000027 P:0006C7 000000            DC      0                                 ; Failed on packet type
2492 d                               PSIZE_ERRORS
2493 d    X:000028 P:0006C8 000000            DC      0                                 ; Failed on packet size test
2494   
2495   
2497   
2498 d                               PCI_BURST_SIZE
2499 d    X:000029 P:0006C9 000040            DC      $40                               ; Should be < 4*latency assigned by OS
2500 d    X:00002A P:0006CA 000000  BURST_SIZE DC     0
2501 d    X:00002B P:0006CB 000000  BLOCK_SIZE DC     0
2502   
2503 d    X:00002C P:0006CC 000000  CON_SRC_LO DC     0                                 ; Set by CON host command
2504 d    X:00002D P:0006CD 000000  CON_SRC_HI DC     0
2505   
2506 d    X:00002E P:0006CE 000000  YMEM_SRC  DC      0                                 ; Vars for YMEM -> PC transfers
2507 d                               BURST_DEST_LO
2508 d    X:00002F P:0006CF 000000            DC      0
2509 d                               BURST_DEST_HI
2510 d    X:000030 P:0006D0 000000            DC      0
2511   
2512 d                               BURST_SRC_LO
2513 d    X:000031 P:0006D1 000000            DC      0                                 ; Vars for PC -> YMEM transfers
2514 d                               BURST_SRC_HI
2515 d    X:000032 P:0006D2 000000            DC      0
2516 d    X:000033 P:0006D3 000000  YMEM_DEST DC      0
2517   
2518 d    X:000034 P:0006D4 000000  DMA_ERRORS DC     0                                 ; Error counting
2519 d    X:000035 P:0006D5 000000  EC_TRTY   DC      0
2520 d    X:000036 P:0006D6 000000  EC_TO     DC      0
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  vars.asm  Page 51



2521 d    X:000037 P:0006D7 000000  EC_TDIS   DC      0
2522 d    X:000038 P:0006D8 000000  EC_TAB    DC      0
2523 d    X:000039 P:0006D9 000000  EC_MAB    DC      0
2524 d    X:00003A P:0006DA 000000  EC_DPER   DC      0
2525 d    X:00003B P:0006DB 000000  EC_APER   DC      0
2526   
2527   
2529   
2530 d    X:00003C P:0006DC 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2531 d    X:00003D P:0006DD 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2532 d                               QT_BUF_SIZE
2533 d    X:00003E P:0006DE 000000            DC      0                                 ; Separation of buffers, in bytes
2534 d    X:00003F P:0006DF 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2535 d                               QT_FRAME_SIZE
2536 d    X:000040 P:0006E0 000000            DC      0                                 ; Expected data packet size, in bytes
2537 d    X:000041 P:0006E1 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2538   
2539 d                               QT_BUF_HEAD
2540 d    X:000042 P:0006E2 000000            DC      0                                 ; Index of buf for next write
2541 d                               QT_BUF_TAIL
2542 d    X:000043 P:0006E3 000000            DC      0                                 ; Index at which we must not write
2543   
2544 d    X:000044 P:0006E4 000000  QT_DEST_LO DC     0                                 ; PC address for next write
2545 d    X:000045 P:0006E5 000000  QT_DEST_HI DC     0                                 ;
2546 d                               QT_INFORM_IDX
2547 d    X:000046 P:0006E6 000000            DC      0                                 ; Number of packets since last inform
2548 d    X:000047 P:0006E7 000000  QT_DROPS  DC      0                                 ; Dropped packet count
2549   
2550   
2552   
2553 d    X:000048 P:0006E8 000000  RP_BASE_LO DC     0                                 ; PC buffer start address
2554 d    X:000049 P:0006E9 000000  RP_BASE_HI DC     0                                 ;
2555 d                               RP_MAX_SIZE
2556 d    X:00004A P:0006EA 000000            DC      0                                 ; Maximum reply size, dwords
2557 d    X:00004B P:0006EB 000000  RP_DROPS  DC      0                                 ; Dropped packet count
2558   
2559   
2561   
2562 d                               TIMER_INDEX
2563 d    X:00004C P:0006EC 000000            DC      0
2564   
2565   
2567   
2568 d    X:00004D P:0006ED 000000  BDEBUG0   DC      0
2569 d    X:00004E P:0006EE 000000  BDEBUG1   DC      0
2570 d    X:00004F P:0006EF 000000  BDEBUG2   DC      0
2571 d    X:000050 P:0006F0 000000  BDEBUG3   DC      0
2572 d    X:000051 P:0006F1 000000  BDEBUG4   DC      0
2573 d    X:000052 P:0006F2 000000  BDEBUG5   DC      0
2574 d    X:000053 P:0006F3 000000  BDEBUG6   DC      0
2575 d    X:000054 P:0006F4 000000  BDEBUG7   DC      0
2576 d    X:000055 P:0006F5 000000  BDEBUG8   DC      0
2577 d    X:000056 P:0006F6 000000  BDEBUG9   DC      0
2578   
2579                                ;----------------------------------------------------------
2580   
2582   
2583                                 APPLICATION_RUNNING
2584      000000                              EQU     0                                 ; Indicates application is in progress
2585                                 SEND_TO_HOST
2586      000001                              EQU     1                                 ; set in HST ISR when host ready for packet 
(stays set until after HST reply)
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  vars.asm  Page 52



2587                                 FATAL_ERROR
2588      000002                              EQU     2                                 ; PCI message to host error detected by driv
er....
2589      000003                    FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays s
et till packet processed
2590   
2591      000009                    HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of p
acket (stays set until after HST reply)
2592   
2593      00000A                    CON_DEMAND EQU    10                                ; Host has requested an MCE command be sent
2594      00000B                    CON_MCE   EQU     11                                ; Command has been copied to Y buffer and sh
ould be sent to MCE
2595   
2596                                 PCIDMA_RESTART
2597      000010                              EQU     16                                ; DMA flags used for error recovery
2598                                 PCIDMA_RESUME
2599      000011                              EQU     17
2600                                 PCIDMA_RETRY
2601      000012                              EQU     18
2602   
2603      000014                    QT_FLUSH  EQU     20                                ; Set when it is time to inform Host of curr
ent buffer position.
2604                                 RP_BUFFER_FULL
2605      000015                              EQU     21                                ; Set when Quiet RP buffer is occupied.
2606   
2607      000016                    FREEZER   EQU     22                                ; Suspend operations and just idle in the ma
in loop
2608                                 MAIN_LOOP_POLL
2609      000017                              EQU     23                                ; Cleared by the main loop, use to check for
 DSP lock-up
2610   
2611   
2613   
2614                                 MODE_APPLICATION
2615      000000                              EQU     0                                 ; set if PCI application to run
2616      000001                    MODE_MCE  EQU     1                                 ; process packets from MCE (!choke)
2617      000002                    MODE_QT   EQU     2                                 ; Quiet transfer for data packets (QT mode)
2618                                 MODE_RP_BUFFER
2619      000003                              EQU     3                                 ; Quiet transfer for reply packets (Quiet-RP
)
2620   
2621   
2623   
2624                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2625                                 VAR_TBL_END
2626      0006F5                              EQU     @LCV(L)-2
2627                                          ENDIF
2628   
2629                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2631                                          ENDIF
2632   
2633                                 VAR_TBL_LENGTH
2634      000057                              EQU     VAR_TBL_END-VAR_TBL_START
2635                                          INCLUDE 'app.asm'
2636                                        COMMENT *
2637   
2638                                Auxiliary application area.
2639   
2640                                See info.asm for versioning and authors.
2641   
2642                                        *
2643                                          PAGE    132                               ; Printronix page width - 132 columns
Motorola DSP56300 Assembler  Version 6.3.4   09-06-08  20:54:47  app.asm  Page 53



2644                                          OPT     CEX                               ; print DC evaluations
2645   
2646                                          IF      @CVS(N,*)>=APPLICATION
2648                                          ENDIF
2649   
2650   
2651                                ;--------------------------------------------
2652                                ; APPLICATION AREA
2653                                ;---------------------------------------------
2654                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2655      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2656                                          ENDIF
2657   
2658                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2660                                          ENDIF
2661   
2662                                ; starts with no application loaded
2663                                ; so just reply with an error if we get a GOA command
2664   
2665      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2666      P:000802 P:000804 440B00            MOVE              X0,X:<DTXS_WD1          ; REPly
2667      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2668      P:000805 P:000807 440C00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2669      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2670      P:000808 P:00080A 440D00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2671      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2672      P:00080B P:00080D 440E00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2673      P:00080C P:00080E 0D0485            JSR     <RESTORE_REGISTERS
2674      P:00080D P:00080F 0D046C            JSR     <PCI_MESSAGE_TO_HOST
2675      P:00080E P:000810 0A0100            BCLR    #MODE_APPLICATION,X:<MODE
2676      P:00080F P:000811 0C016E            JMP     PACKET_IN
2677   
2678   
2679      000812                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM
2680   
2681   
2682   

0    Errors
0    Warnings


