Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  build.asm  Page 1



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
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  header.asm  Page 2



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
78        300000                     EQU     $300000
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
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  header.asm  Page 3



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
149       000002           DCTR_SRIE EQU     2                                 ; Slave request interrupt enable
150       000003           DCTR_HF3  EQU     3                                 ; Semaphore for INTA handshaking
151       000004           DCTR_HF4  EQU     4                                 ;
152       000005           DCTR_HF5  EQU     5                                 ;
153       000006           INTA      EQU     6                                 ; Request PCI interrupt
154    
155                        ; The DSR host flags are written by the PCI host and read by the DSP
156       000003           DSR_HF0   EQU     3                                 ; PC side INTA hand-shaking
157       000004           DSR_HF1   EQU     4                                 ; PC side hand-shaking enabled
158       000005           DSR_HF2   EQU     5                                 ; PC side INTA disable (polling mode)
159    
160                        ; DPCR bit definitions
161       00000E           CLRT      EQU     14                                ; Clear transmitter
162       000012           MACE      EQU     18                                ; Master access counter enable
163       000015           IAE       EQU     21                                ; Insert Address Enable
164    
165    
166    
167                        ; Addresses of ESSI port
168       FFFFBC           TX00      EQU     $FFFFBC                           ; Transmit Data Register 0
169       FFFFB7           SSISR0    EQU     $FFFFB7                           ; Status Register
170       FFFFB6           CRB0      EQU     $FFFFB6                           ; Control Register B
171       FFFFB5           CRA0      EQU     $FFFFB5                           ; Control Register A
172    
173                        ; SSI Control Register A Bit Flags
174       000006           TDE       EQU     6                                 ; Set when transmitter data register is empty
175    
176                        ; Miscellaneous addresses
177       FFFFFF           RDFIFO    EQU     $FFFFFF                           ; Read the FIFO for incoming fiber optic data
178    
179                        ; Timer registers
180       FFFF8F           TCSR0     EQU     $FFFF8F                           ; Control and status register
181       FFFF8E           TLR0      EQU     $FFFF8E                           ; Load register
182       FFFF8D           TCPR0     EQU     $FFFF8D                           ; Compare register
183       FFFF8C           TCR0      EQU     $FFFF8C                           ; Count register
184       FFFF8B           TCSR1     EQU     $FFFF8B                           ; Control and status register
185       FFFF8A           TLR1      EQU     $FFFF8A                           ; Load register
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  header.asm  Page 4



186       FFFF89           TCPR1     EQU     $FFFF89                           ; Compare register
187       FFFF88           TCR1      EQU     $FFFF88                           ; Count register
188       FFFF87           TCSR2     EQU     $FFFF87                           ; Control and status register
189       FFFF86           TLR2      EQU     $FFFF86                           ; Load register
190       FFFF85           TCPR2     EQU     $FFFF85                           ; Compare register
191       FFFF84           TCR2      EQU     $FFFF84                           ; Count register
192    
193                        ;***************************************************************
194                        ; Phase Locked Loop initialization
195       050003           PLL_INIT  EQU     $050003                           ; PLL = 25 MHz x 4 = 100 MHz
196                        ;****************************************************************
197    
198                        ; Port C is Enhanced Synchronous Serial Port 0
199       FFFFBF           PCRC      EQU     $FFFFBF                           ; Port C Control Register
200       FFFFBE           PRRC      EQU     $FFFFBE                           ; Port C Data direction Register
201       FFFFBD           PDRC      EQU     $FFFFBD                           ; Port C GPIO Data Register
202    
203                        ; Port D is Enhanced Synchronous Serial Port 1
204       FFFFAF           PCRD      EQU     $FFFFAF                           ; Port D Control Register
205       FFFFAE           PRRD      EQU     $FFFFAE                           ; Port D Data direction Register
206       FFFFAD           PDRD      EQU     $FFFFAD                           ; Port D GPIO Data Register
207    
208                        ; Bit number definitions of GPIO pins on Port C
209       000002           ROM_FIFO  EQU     2                                 ; Select ROM or FIFO accesses for AA1
210       000004           AUX1      EQU     4                                 ; enable/disable byte swapping
211    
212                        ; Bit number definitions of GPIO pins on Port D
213       000000           EF        EQU     0                                 ; FIFO Empty flag, low true
214       000001           HF        EQU     1                                 ; FIFO half full flag, low true
215       000002           RS        EQU     2                                 ; FIFO reset signal, low true
216       000003           FSYNC     EQU     3                                 ; High during image transmission
217       000005           WRFIFO    EQU     5                                 ; Low true if FIFO is being written to
218    
219    
220                        ; Errors - self test application
221    
222       000000           Y_MEM_ER  EQU     0                                 ; y memory corrupted
223       000001           X_MEM_ER  EQU     1                                 ; x memory corrupted
224       000002           P_MEM_ER  EQU     2                                 ; p memory corrupted
225       000003           FO_EMPTY  EQU     3                                 ; no transmitted data in FIFO
226    
227       000004           FO_OVER   EQU     4                                 ; too much data received
228       000005           FO_UNDER  EQU     5                                 ; not enough data receiv
229       000006           FO_RX_ER  EQU     6                                 ; received data in FIFO incorrect.
230       000007           DEBUG     EQU     7                                 ; debug bit
231    
232    
234       000000           TE        EQU     0
235       000001           TOIE      EQU     1
236       000002           TCIE      EQU     2
237       000014           TOF       EQU     20
238       000015           TCF       EQU     21
239    
240    
242    
243       FFF000           FO_SEND   EQU     $FFF000
244    
245                        ;--------------------------------------------------------------------
246                        ; Interrupt configuration
247                        ;--------------------------------------------------------------------
248                        ;  IPRC determines core interrupt modes and levels.
249                        ;   - [5:3] IRQB mode|level - FIFO half full
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  header.asm  Page 5



250                        ;   - [8:6] IRQC mode|level - reset switch
251                        ;  So $1C0 is 111|000|000 = IRQC level-triggered priority 3 and IRQB disabled.
252                        ;
253                        ;  IPRP determines peripheral interrupt levels.
254                        ;   - [1:0] HI-32 level.  Must be higher than SR IPL mask
255                        ;  So set to 2, and ensure SR[I] > 1.
256                        ;
257                        ;  SR determines many things... but most importantly
258                        ;   - [9:8] Interrupt mask - must be smaller than HI-32 IPL
259                        ;  So set to $100
260    
261       0001C0           MY_IPRC   EQU     $0001C0
262       000002           MY_IPRP   EQU     $000002
263       000100           MY_SR     EQU     $000100
264    
265                                  INCLUDE 'init.asm'
266                                COMMENT *
267    
268                        Initial configuration, and ISR vector definitions.
269    
270                        See info.asm for versioning and authors.
271    
272                                *
273                                  PAGE    132                               ; Printronix page width - 132 columns
274                                  OPT     CEX                               ; print DC evaluations
275    
276                        ; The EEPROM boot code expects first to read 3 bytes specifying the number of
277                        ; program words, then 3 bytes specifying the address to start loading the
278                        ; program words and then 3 bytes for each program word to be loaded.
279                        ; The program words will be condensed into 24 bit words and stored in contiguous
280                        ; PRAM memory starting at the specified starting address. Program execution
281                        ; starts from the same address where loading started.
282    
283                        ; Special address for two words for the DSP to bootstrap code from the EEPROM
284                                  IF      @SCP("ROM","ROM")                 ; Boot from ROM on power-on
285       P:000000 P:000000                   ORG     P:0,P:0
286  d    P:000000 P:000000 000BDB            DC      END_ADR-INIT-2                    ; Number of boot words
287  d    P:000001 P:000001 000000            DC      INIT                              ; Starting address
288       P:000000 P:000002                   ORG     P:0,P:2
289       P:000000 P:000002 0C0030  INIT      JMP     <INIT_PCI                         ; Configure PCI port
290       P:000001 P:000003 000000            NOP
291                                           ENDIF
292    
293    
294                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
295                                 ; command converter
296                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
300                                           ENDIF
301    
302                                 ; Vectored interrupt table, addresses at the beginning are reserved
303  d    P:000002 P:000004 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0       ; $02-$0f Reserved
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
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  init.asm  Page 6



     d                      000000
     d                      000000
304  d    P:000010 P:000012 000000            DC      0,0                               ; $10-$13 Reserved
     d                      000000
305    
306                                 ; FIFO HF* flag interrupt vector is here at $12 - this is connected to the
307                                 ; IRQB* interrupt line so its ISR vector must be here
308  d    P:000012 P:000014 000000            DC      0,0                               ; $was ld scatter routine ...HF*
     d                      000000
309    
310                                 ; a software reset button on the font panel of the card is connected to the IRQC*
311                                 ; line which if pressed causes the DSP to jump to an ISR which causes the program
312                                 ; counter to the beginning of the program INIT and sets the stack pointer to TOP.
313       P:000014 P:000016 0BF080            JSR     SYSTEM_RESET                      ; $14 - Software reset switch
                            000434
314    
315  d    P:000016 P:000018 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Reserved interrupts
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
316  d    P:000022 P:000024 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0
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
317    
318                                 ; Now we're at P:$30, where some unused vector addresses are located
319                                 ; This is ROM only code that is only executed once on power-up when the
320                                 ; ROM code is downloaded. It is skipped over on OnCE downloads.
321    
322                                 ; A few seconds after power up on the Host, it interrogates the PCI bus to find
323                                 ; out what boards are installed and configures this PCI board. The EEPROM booting
324                                 ; procedure ends with program execution  starting at P:$0 where the EEPROM has
325                                 ; inserted a JMP INIT_PCI instruction. This routine sets the PLL paramter and
326                                 ; does a self configuration and software reset of the PCI controller in the DSP.
327                                 ; After configuring the PCI controller the DSP program overwrites the instruction
328                                 ; at P:$0 with a new JMP START to skip over the INIT_PCI routine. The program at
329                                 ; START address begins configuring the DSP and processing commands.
330                                 ; Similarly the ONCE option places a JMP START at P:$0 to skip over the
331                                 ; INIT_PCI routine. If this routine where executed after the host computer had booted
332                                 ; it would cause it to crash since the host computer would overwrite the
333                                 ; configuration space with its own values and doesn't tolerate foreign values.
334    
335                                 ; Initialize the PLL - phase locked loop
336                                 INIT_PCI
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  init.asm  Page 7



337       P:000030 P:000032 08F4BD            MOVEP             #PLL_INIT,X:PCTL        ; Initialize PLL
                            050003
338       P:000032 P:000034 000000            NOP
339    
340                                 ; Program the PCI self-configuration registers
341       P:000033 P:000035 240000            MOVE              #0,X0
342       P:000034 P:000036 08F485            MOVEP             #$500000,X:DCTR         ; Set self-configuration mode
                            500000
343       P:000036 P:000038 0604A0            REP     #4
344       P:000037 P:000039 08C408            MOVEP             X0,X:DPAR               ; Dummy writes to configuration space
345       P:000038 P:00003A 08F487            MOVEP             #>$0000,X:DPMC          ; Subsystem ID
                            000000
346       P:00003A P:00003C 08F488            MOVEP             #>$0000,X:DPAR          ; Subsystem Vendor ID
                            000000
347    
348                                 ; PCI Personal reset
349       P:00003C P:00003E 08C405            MOVEP             X0,X:DCTR               ; Personal software reset
350       P:00003D P:00003F 000000            NOP
351       P:00003E P:000040 000000            NOP
352       P:00003F P:000041 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00003F
353       P:000041 P:000043 07F084            MOVE              P:(*+3),X0              ; Trick to write "JMP <START" to P:0
                            000044
354       P:000043 P:000045 070004            MOVE              X0,P:(0)
355       P:000044 P:000046 0C0100            JMP     <START
356    
357  d    P:000045 P:000047 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
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
358  d    P:000051 P:000053 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
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
360  d    P:00005D P:00005F 000000            DC      0,0,0,0,0,0,0,0,0,0,0
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
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  init.asm  Page 8



361    
362       
363       P:00006A P:00006C                   ORG     P:$6A,P:$6C
364                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
366                                           ENDIF
367       P:00006A P:00006C 0BF080            JSR     BUFFER_PC_CMD_INT_HANDLER
                            0009B3
368    
369    
370    
371                                 ;**************************************************************************
372                                 ; Check for program space overwriting of ISR starting at P:$72
373                                           IF      @CVS(N,*)>$71
375                                           ENDIF
376    
377       P:000072 P:000074                   ORG     P:$72,P:$74
378    
379                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
380                                 ; command converter
381                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
383                                           ENDIF
384    
385    
386                                 ;**************************************************************************
387    
388                                 ; Three non-maskable fast interrupt service routines for clearing PCI interrupts
389                                 ; The Host will use these to clear the INTA* after it has serviced the interrupt
390                                 ; which had been generated by the PCI board.
391    
392       P:000072 P:000074 0A8506            BCLR    #INTA,X:DCTR                      ; $72/3 - Clear PCI interrupt
393       P:000073 P:000075 000000            NOP
394    
395       P:000074 P:000076 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; clear interrupt flag
396       P:000075 P:000077 000000            NOP                                       ; needs to be fast addressing <
397    
398       P:000076 P:000078 0A0022            BSET    #FATAL_ERROR,X:<STATUS            ; $76/7 - driver PCI_MESSAGE_TO_HOST error
399       P:000077 P:000079 000000            NOP
400    
401                                 ; Interrupt locations for 7 available commands on PCI board
402                                 ; Each JSR takes up 2 locations in the table
403       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            000349
404       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            00031F
405       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            00036A
406       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            000373
407                                 ; software reset is the same as cleaning up the PCI - use same routine
408                                 ; when HOST does a RESET then this routine is run
409       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            00044B
410       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            00045C
411       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            00043C
412       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            00037E
413    
414                                 ; QT - set command
415       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            00039C
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  init.asm  Page 9



416       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            000434
417    
418                                 ; Quiet RP mode, clear buffer full flag
419       P:00008C P:00008E 0A0015            BCLR    #RP_BUFFER_FULL,X:<STATUS         ; $8C
420       P:00008D P:00008F 000000            NOP
421    
422                                 ; Reset MCE (new-style command, no reply)
423       P:00008E P:000090 0BF080            JSR     RESET_MCE_COMMS                   ; $8E
                            000AD6
424    
425                                 ; ***********************************************************************
426                                 ; For now have boot code starting from P:$100
427                                 ; just to make debugging tidier etc.
428    
429       P:000100 P:000102                   ORG     P:$100,P:$102
430    
431                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
432                                 ; command converter
433                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
435                                           ENDIF
436                                 ; ***********************************************************************
437    
438    
439    
440                                 ; ******************************************************************
441                                 ;
442                                 ;       AA0 = RDFIFO* of incoming fiber optic data
443                                 ;       AA1 = EEPROM access
444                                 ;       AA2 = DRAM access
445                                 ;       AA3 = output to parallel data connector, for a video pixel clock
446                                 ;       $FFxxxx = Write to fiber optic transmitter
447                                 ;
448                                 ; ******************************************************************
449    
450    
451       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
452       P:000102 P:000104 08F485            MOVEP             #>$100000,X:DCTR        ; Set PCI mode
                            100000
453       P:000104 P:000106 000000            NOP
454       P:000105 P:000107 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
455       P:000106 P:000108 000000            NOP
456       P:000107 P:000109 000000            NOP                                       ; End of PCI programming
457    
458    
459                                 ; Set operation mode register OMR to normal expanded
460       P:000108 P:00010A 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
461       P:000109 P:00010B 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
462    
463                                 ; Program the serial port ESSI0 = Port C for serial transmission to
464                                 ;   the timing board
465       P:00010A P:00010C 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
466                                 ;**********************************************************************
467       P:00010C P:00010E 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
468                                                                                     ; DC0-CD4 = 0 for non-network operation
469                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
470                                                                                     ; SSC1 = 0 for SC1 not used
471                                 ;************************************************************************
472       P:00010E P:000110 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  init.asm  Page 10



                            010120
473                                                                                     ; SHFD = 0 for MSB shifted first
474                                                                                     ; CKP = 0 for rising clock edge transitions
475                                                                                     ; TE0 = 1 to enable transmitter #0
476                                                                                     ; MOD = 0 for normal, non-networked mode
477                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
478       P:000110 P:000112 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
479                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
480                                 ;********************************************************************************
481       P:000112 P:000114 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
482       P:000114 P:000116 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
                            000000
483                                 ;***********************************************************************************
484                                 ; 250MHz
485                                 ; Conversion from software bits to schematic labels for Port C and D
486                                 ;       PC0 = SC00 = AUX3               PD0 = SC10 = EF*
487                                 ;       PC1 = SC01 = A/B* = input       PD1 = SC11 = HF*
488                                 ;       PC2 = SC02 = No connect         PD2 = SC12 = RS*
489                                 ;       PC3 = SCK0 = No connect         PD3 = SCK1 = NWRFIFO*
490                                 ;       PC4 = SRD0 = AUX1               PD4 = SRD1 = No connect (** in 50Mhz this was MODE selec
t for 16 or 32 bit FO)
491                                 ;       PC5 = STD0 = No connect         PD5 = STD1 = WRFIFO*
492                                 ; ***********************************************************************************
493    
494    
495                                 ; ****************************************************************************
496                                 ; Program the serial port ESSI1 = Port D for general purpose I/O (GPIO)
497    
498       P:000116 P:000118 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
499       P:000118 P:00011A 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            00001C
500       P:00011A P:00011C 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
501       P:00011C P:00011E 060AA0            REP     #10
502       P:00011D P:00011F 000000            NOP
503       P:00011E P:000120 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
504                                                                                     ; was %011100
505    
506                                 ; Program the SCI port to benign values
507       P:000120 P:000122 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
508       P:000122 P:000124 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
509       P:000124 P:000126 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
510                                 ;       PE0 = RXD
511                                 ;       PE1 = TXD
512                                 ;       PE2 = SCLK
513    
514                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
515       P:000126 P:000128 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
516       P:000128 P:00012A 07F40B            MOVEP             #$2800,X:TCSR1
                            002800
517       P:00012A P:00012C 07F407            MOVEP             #$2800,X:TCSR2
                            002800
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  init.asm  Page 11



518    
519    
520                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
521       P:00012C P:00012E 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
522       P:00012E P:000130 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
e
                            008929
523       P:000130 P:000132 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
                            000122
524    
525    
526                                 ; Program the DRAM memory access and addressing
527       P:000132 P:000134 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
                            020022
528       P:000134 P:000136 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
529    
530    
531                                 ; Clear all PCI error conditions
532       P:000136 P:000138 084E0A            MOVEP             X:DPSR,A
533       P:000137 P:000139 0140C2            OR      #$1FE,A
                            0001FE
534       P:000139 P:00013B 000000            NOP
535       P:00013A P:00013C 08CE0A            MOVEP             A,X:DPSR
536    
537                                 ; Status word and interrupt configuration.
538       P:00013B P:00013D 08F4BF            MOVEP             #>MY_IPRC,X:IPRC
                            0001C0
539       P:00013D P:00013F 08F4BE            MOVEP             #>MY_IPRP,X:IPRP
                            000002
540       P:00013F P:000141 05F439            MOVE              #>MY_SR,SR
                            000100
541    
542    
543                                 ;--------------------------------------------------------------------------
544                                 ; Initialize the fiber optic serial transmitter to zero
545       P:000141 P:000143 01B786            JCLR    #TDE,X:SSISR0,*
                            000141
546       P:000143 P:000145 07F43C            MOVEP             #$000000,X:TX00
                            000000
547    
548                                 ;--------------------------------------------------------------------
549    
550                                 ; clear DTXM - PCI master transmitter
551       P:000145 P:000147 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
552       P:000146 P:000148 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000146
553    
554                                 ;----------------------------------------------------------------------
555                                 ; clear DRXR - PCI receiver
556    
557       P:000148 P:00014A 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014D
558       P:00014A P:00014C 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
559       P:00014B P:00014D 000000            NOP
560       P:00014C P:00014E 0C0148            JMP     <CLR0
561                                 CLR1
562    
563                                 ;-----------------------------------------------------------------------------
564                                 ; copy parameter table from P memory into X memory
565    
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  init.asm  Page 12



566                                 ; but not frame_count and num_dumped - don't want these reset by fatal error...
567                                 ; they will be reset by new packet or pci_reset ISR
568    
569       P:00014D P:00014F 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000006
570       P:00014F P:000151 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
                            000002
571    
572                                 ; Move the table of constants from P: space to X: space
573       P:000151 P:000153 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            0006AC
574       P:000153 P:000155 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
575       P:000154 P:000156 064782            DO      #VAR_TBL_LENGTH,X_WRITE
                            000157
576       P:000156 P:000158 07D984            MOVE              P:(R1)+,X0
577       P:000157 P:000159 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
578                                 X_WRITE
579    
580       P:000158 P:00015A 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
                            000006
581       P:00015A P:00015C 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000002
582    
583    
584                                 ;----------------------------------------------------------------------------
585                                 ; Initialize PCI controller again, after booting, to make sure it sticks
586    
587       P:00015C P:00015E 08F485            MOVEP             #>$000000,X:DCTR
                            000000
588       P:00015E P:000160 000000            NOP
589       P:00015F P:000161 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00015F
590       P:000161 P:000163 08F485            MOVEP             #>$100000,X:DCTR
                            100000
591       P:000163 P:000165 000000            NOP
592       P:000164 P:000166 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000164
593    
594       
595       P:000166 P:000168 0BF080            JSR     CLEAR_FO_FIFO                     ; Clear the fibre fifo!
                            0004B4
596       P:000168 P:00016A 013D24            BSET    #AUX1,X:PDRC                      ; Enable byte-swapping - still necc. on ARC-
64
597       P:000169 P:00016B 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host interrupts
598       P:00016A P:00016C 0BF080            JSR     TIMER_DEFAULT                     ; Enable timer (channel 0) for misc. uses
                            00062E
599       P:00016C P:00016E 0BF080            JSR     TIMER_STORE_INIT                  ; Initialize timing buffer
                            000646
600    
602                                           INCLUDE 'main.asm'
603    
604                                                 COMMENT *
605    
606                                 Main section of the pci card code.
607    
608                                 See info.asm for versioning and authors.
609    
610                                         *
611                                           PAGE    132                               ; Printronix page width - 132 columns
612                                           OPT     CEX                               ; print DC evaluations
613    
614    
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 13



615    
619    
620                                 PACKET_IN
621    
622       
623       P:00016E P:000170 0A0017            BCLR    #MAIN_LOOP_POLL,X:<STATUS
624    
625       
626       P:00016F P:000171 0A00B6            JSET    #FREEZER,X:<STATUS,PACKET_IN
                            00016E
627    
628       
629       P:000171 P:000173 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
630    
631       
632       P:000173 P:000175 0A01A0            JSET    #MODE_APPLICATION,X:<MODE,APPLICATION
                            000800
633    
634       
635       P:000175 P:000177 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            000636
636    
637       
638       P:000177 P:000179 0B00B4            JSSET   #QT_FLUSH,X:STATUS,BUFFER_INFORM
                            000682
639    
640       
641       P:000179 P:00017B 0D0470            JSR     <CHECK_FO
642       P:00017A P:00017C 0B00A3            JSSET   #FO_WRD_RCV,X:STATUS,HANDLE_FIFO
                            00018A
643    
644       
645       P:00017C P:00017E 0B00AB            JSSET   #CON_MCE,X:STATUS,CON_TRANSMIT
                            000288
646       P:00017E P:000180 0B00AA            JSSET   #CON_DEMAND,X:STATUS,CON_BUFFER
                            00026B
647    
648       
649       P:000180 P:000182 0A89A5            JSET    #DSR_HF2,X:DSR,NEW_COMMS_INIT
                            000810
650    
651       
652       P:000182 P:000184 0C016E            JMP     PACKET_IN
653    
657    
658                                 ; PCI semaphore
659                                 ;
660                                 ; In order for routines in non-interrupt context to write to the
661                                 ; DTXS, (via PCI_MESSAGE_TO_HOST) they need to end up with
662                                 ; interrupts disabled and HCF3 cleared.
663                                 ;
664                                 ; Non-interrupt PCIers should use macro
665                                 ;       PCI_LOCKDOWN
666                                 ; to get exclusive access and then release it with
667                                 ;       PCI_LOCKUP
668                                 ; after calling PCI_MESSAGE_TO_HOST.
669    
670                                  PCI_LOCKDOWN
671                                           MACRO
672  m                                        JSR     PCI_LOCKDOWN_ENTRY
673  m                                        ENDM
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 14



674    
675                                 PCI_LOCKUP MACRO
676  m                                        BSET    #DCTR_HCIE,X:DCTR                 ; Enable host interrupts
677  m                                        ENDM
678    
679    
680                                 PCI_LOCKDOWN_AGAIN
681       P:000183 P:000185 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Re-enable host IRQ
682       P:000184 P:000186 0632A0            REP     #50                               ; Delay for ~us
683       P:000185 P:000187 000000            NOP
684    
685                                 PCI_LOCKDOWN_ENTRY
686       
687       P:000186 P:000188 0A8500            BCLR    #DCTR_HCIE,X:DCTR                 ; Disable host IRQ
688       P:000187 P:000189 0A85A3            JSET    #DCTR_HF3,X:DCTR,PCI_LOCKDOWN_AGAIN
                            000183
689       P:000189 P:00018B 00000C            RTS
690    
691    
693    
694                                 HANDLE_FIFO
695       P:00018A P:00018C 54F400            MOVE              #>$A00,A1
                            000A00
696       P:00018C P:00018E 0BF080            JSR     TIMER_STORE_A1
                            00064F
697       P:00018E P:000190 0BF080            JSR     TIMER_STORE
                            00064D
698    
699       
700       P:000190 P:000192 60F400            MOVE              #>HEAD_W1_0,R0
                            00001B
701       P:000192 P:000194 44F400            MOVE              #>$00FFFF,X0            ; Mask lower 16 bits
                            00FFFF
702       P:000194 P:000196 220800            MOVE              R0,A0
703       P:000195 P:000197 060880            DO      #8,HANDLE_FIFO_CHECK_PREAMBLE
                            0001A0
704                                 HANDLE_FIFO_WAIT
705       P:000197 P:000199 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000197
706       P:000199 P:00019B 000000            NOP
707       P:00019A P:00019C 000000            NOP
708       P:00019B P:00019D 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000197
709       P:00019D P:00019F 094E3F            MOVEP             Y:RDFIFO,A
710       P:00019E P:0001A0 200046            AND     X0,A
711       P:00019F P:0001A1 000000            NOP
712       P:0001A0 P:0001A2 545800            MOVE              A1,X:(R0)+
713    
714                                 HANDLE_FIFO_CHECK_PREAMBLE
715       P:0001A1 P:0001A3 60F400            MOVE              #>HEAD_W1_0,R0
                            00001B
716       P:0001A3 P:0001A5 20001B            CLR     B
717       P:0001A4 P:0001A6 200013            CLR     A
718       P:0001A5 P:0001A7 57D800            MOVE              X:(R0)+,B
719       P:0001A6 P:0001A8 0140CD            CMP     #>$A5A5,B
                            00A5A5
720       P:0001A8 P:0001AA 0AF0A2            JNE     PRE_ERROR
                            0001CF
721       P:0001AA P:0001AC 57D800            MOVE              X:(R0)+,B
722       P:0001AB P:0001AD 0140CD            CMP     #>$A5A5,B
                            00A5A5
723       P:0001AD P:0001AF 0AF0A2            JNE     PRE_ERROR
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 15



                            0001CF
724       P:0001AF P:0001B1 57D800            MOVE              X:(R0)+,B
725       P:0001B0 P:0001B2 0140CD            CMP     #>$5A5A,B
                            005A5A
726       P:0001B2 P:0001B4 0AF0A2            JNE     PRE_ERROR
                            0001CF
727       P:0001B4 P:0001B6 57D800            MOVE              X:(R0)+,B
728       P:0001B5 P:0001B7 0140CD            CMP     #>$5A5A,B
                            005A5A
729       P:0001B7 P:0001B9 0AF0A2            JNE     PRE_ERROR
                            0001CF
730    
731       
732       P:0001B9 P:0001BB 50F000            MOVE              X:>(HEAD_W1_0+6),A0
                            000021
733       P:0001BB P:0001BD 44F000            MOVE              X:>(HEAD_W1_0+7),X0
                            000022
734       P:0001BD P:0001BF 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
735    
736       
737       P:0001BF P:0001C1 0BF080            JSR     PACKET_PARTITIONS
                            0005BE
738       P:0001C1 P:0001C3 0BF080            JSR     TIMER_STORE
                            00064D
739    
741       P:0001C3 P:0001C5 56F000            MOVE              X:HEAD_W3_0,A
                            00001F
742    
743       P:0001C5 P:0001C7 0140C5            CMP     #>'RP',A
                            005250
744       P:0001C7 P:0001C9 0AF0AA            JEQ     HANDLE_RP
                            0001E3
745    
746       P:0001C9 P:0001CB 0140C5            CMP     #>'DA',A
                            004441
747       P:0001CB P:0001CD 0AF0AA            JEQ     HANDLE_DA
                            00022A
748    
749       P:0001CD P:0001CF 0AF080            JMP     QT_PTYPE_ERROR
                            0001D5
750    
751                                 ; Error recording.
752    
753                                 PRE_ERROR
754       P:0001CF P:0001D1 60F400            MOVE              #>PREAMBLE_ERRORS,R0
                            000026
755       P:0001D1 P:0001D3 0BF080            JSR     INCR_X_R0
                            0001DE
756       P:0001D3 P:0001D5 0AF080            JMP     CLEAR_FO_FIFO                     ; empty the fifo (2 ms!)
                            0004B4
757    
758                                 QT_PTYPE_ERROR
759       P:0001D5 P:0001D7 60F400            MOVE              #>PTYPE_ERRORS,R0
                            000027
760       P:0001D7 P:0001D9 0AF080            JMP     INCR_X_R0
                            0001DE
761                                 QT_FSIZE_ERROR
762       P:0001D9 P:0001DB 60F400            MOVE              #>PSIZE_ERRORS,R0
                            000028
763       P:0001DB P:0001DD 0AF080            JMP     INCR_X_R0
                            0001DE
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 16



764                                 RETURN_NOW
765       P:0001DD P:0001DF 00000C            RTS
766    
767                                 INCR_X_R0
768       
769       P:0001DE P:0001E0 50E000            MOVE              X:(R0),A0
770       P:0001DF P:0001E1 000008            INC     A
771       P:0001E0 P:0001E2 000000            NOP
772       P:0001E1 P:0001E3 506000            MOVE              A0,X:(R0)
773       P:0001E2 P:0001E4 00000C            RTS
774    
775    
776    
779    
780                                 HANDLE_RP
781       
782       P:0001E3 P:0001E5 0A0183            JCLR    #MODE_RP_BUFFER,X:MODE,MCE_PACKET
                            0002A8
783    
784       
785       P:0001E5 P:0001E7 0A00B5            JSET    #RP_BUFFER_FULL,X:STATUS,HANDLE_RP_DROP
                            000225
786    
787       
788       P:0001E7 P:0001E9 61F400            MOVE              #>REPLY_BUFFER,R1
                            100000
789       P:0001E9 P:0001EB 0BF080            JSR     BUFFER_PACKET
                            0005CB
790    
791       P:0001EB P:0001ED 54F400            MOVE              #>$b00,A1
                            000B00
792       P:0001ED P:0001EF 0BF080            JSR     TIMER_STORE_A1
                            00064F
793       P:0001EF P:0001F1 0BF080            JSR     TIMER_STORE
                            00064D
794    
795       
796       P:0001F1 P:0001F3 60F400            MOVE              #RP_BASE_LO,R0
                            000048
797       P:0001F3 P:0001F5 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
798    
799       P:0001F5 P:0001F7 60F400            MOVE              #BURST_DEST_LO,R0
                            00002F
800       P:0001F7 P:0001F9 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
801    
802       
803       P:0001F9 P:0001FB 200013            CLR     A
804       P:0001FA P:0001FC 20001B            CLR     B
805       P:0001FB P:0001FD 50F000            MOVE              X:PACKET_SIZE,A0
                            000023
806       P:0001FD P:0001FF 0C1D04            ASL     #2,A,A                            ; Size in bytes
807       P:0001FE P:000200 51F000            MOVE              X:RP_MAX_SIZE,B0
                            00004A
808    
809       P:000200 P:000202 200005            CMP     B,A                               ; A ? B
810       P:000201 P:000203 0AF0AF            JLE     HANDLE_RP1
                            000204
811       P:000203 P:000205 21EE00            MOVE              B,A
812    
813                                 HANDLE_RP1
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 17



814       
815       P:000204 P:000206 44F400            MOVE              #>REPLY_BUFFER,X0
                            100000
816       P:000206 P:000208 507000            MOVE              A0,X:BLOCK_SIZE
                            00002B
817       P:000208 P:00020A 447000            MOVE              X0,X:YMEM_SRC
                            00002E
818       P:00020A P:00020C 0BF080            JSR     TIMER_STORE
                            00064D
819       P:00020C P:00020E 0BF080            JSR     BLOCK_TRANSFER
                            000518
820       P:00020E P:000210 0BF080            JSR     TIMER_STORE
                            00064D
821    
822       
823                                           PCI_LOCKDOWN                              ; Disable host IRQ
825       P:000211 P:000213 44F400            MOVE              #'NFY',X0
                            4E4659
826       P:000213 P:000215 447000            MOVE              X0,X:DTXS_WD1
                            00000B
827       P:000215 P:000217 44F400            MOVE              #'RPQ',X0
                            525051
828       P:000217 P:000219 447000            MOVE              X0,X:DTXS_WD2
                            00000C
829       P:000219 P:00021B 507000            MOVE              A0,X:DTXS_WD3           ; A0=block_size
                            00000D
830       P:00021B P:00021D 547000            MOVE              A1,X:DTXS_WD4           ; A1=0
                            00000E
831    
832       
833       P:00021D P:00021F 0A7035            BSET    #RP_BUFFER_FULL,X:STATUS
                            000000
834       P:00021F P:000221 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
835                                           PCI_LOCKUP                                ; Enable host IRQ
837    
838       P:000222 P:000224 0BF080            JSR     TIMER_STORE
                            00064D
839       P:000224 P:000226 00000C            RTS                                       ; Back to main loop
840    
841                                 HANDLE_RP_DROP
842       P:000225 P:000227 60F400            MOVE              #RP_DROPS,R0
                            00004B
843       P:000227 P:000229 0D01DE            JSR     INCR_X_R0
844       P:000228 P:00022A 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            00060E
845    
847    
848    
851    
852    
853                                 HANDLE_DA
854       
855       P:00022A P:00022C 60F400            MOVE              #FRAME_COUNT,R0
                            000002
856       P:00022C P:00022E 0D01DE            JSR     INCR_X_R0
857    
858       
859       P:00022D P:00022F 0A0182            JCLR    #MODE_QT,X:MODE,MCE_PACKET
                            0002A8
860    
861       
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 18



862       P:00022F P:000231 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
863       P:000231 P:000233 0BF080            JSR     BUFFER_PACKET
                            0005CB
864    
865       P:000233 P:000235 54F400            MOVE              #$e00,A1
                            000E00
866       P:000235 P:000237 0BF080            JSR     TIMER_STORE_A1
                            00064F
867       P:000237 P:000239 0BF080            JSR     TIMER_STORE
                            00064D
868    
869       
870       P:000239 P:00023B 56F000            MOVE              X:QT_BUF_HEAD,A
                            000042
871       P:00023B P:00023D 014180            ADD     #1,A
872       P:00023C P:00023E 57F000            MOVE              X:QT_BUF_MAX,B
                            00003F
873       P:00023E P:000240 20000D            CMP     A,B
874       P:00023F P:000241 0AF0A1            JGE     HANDLE_DA_MATH
                            000242
875       P:000241 P:000243 2E0000            MOVE              #0,A
876                                 HANDLE_DA_MATH
877       P:000242 P:000244 57F000            MOVE              X:QT_BUF_TAIL,B
                            000043
878       P:000244 P:000246 20000D            CMP     A,B
879       P:000245 P:000247 0AF0AA            JEQ     HANDLE_DA_DROP
                            000266
880    
881       
882       P:000247 P:000249 200013            CLR     A
883       P:000248 P:00024A 50F000            MOVE              X:PACKET_SIZE,A0
                            000023
884    
885       P:00024A P:00024C 014088            ADD     #0,B                              ; Clear carry
886       P:00024B P:00024D 0C1D04            ASL     #2,A,A                            ; Size, in bytes
887    
888       
889       P:00024C P:00024E 20001B            CLR     B
890       P:00024D P:00024F 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            000040
891       P:00024F P:000251 20000D            CMP     A,B
892       P:000250 P:000252 0E21D9            JNE     QT_FSIZE_ERROR
893    
894       
895       P:000251 P:000253 517000            MOVE              B0,X:BLOCK_SIZE
                            00002B
896       P:000253 P:000255 557000            MOVE              B1,X:YMEM_SRC           ; Y:0
                            00002E
897    
898       P:000255 P:000257 60F400            MOVE              #QT_DEST_LO,R0
                            000044
899       P:000257 P:000259 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
900       P:000259 P:00025B 60F400            MOVE              #BURST_DEST_LO,R0
                            00002F
901       P:00025B P:00025D 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
902    
903       
904       P:00025D P:00025F 0BF080            JSR     BLOCK_TRANSFER
                            000518
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 19



905    
906       P:00025F P:000261 0BF080            JSR     TIMER_STORE
                            00064D
907    
908       
909       P:000261 P:000263 0BF080            JSR     BUFFER_INCR
                            000657
910    
911       
912       P:000263 P:000265 0BF080            JSR     BUFFER_INFORM_CHECK
                            000675
913    
914       P:000265 P:000267 00000C            RTS
915    
916                                 HANDLE_DA_DROP
917       
918       P:000266 P:000268 60F400            MOVE              #QT_DROPS,R0
                            000047
919       P:000268 P:00026A 0D01DE            JSR     INCR_X_R0
920       P:000269 P:00026B 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            00060E
921    
923    
924    
925                                 ;----------------------------------------------
926                                 CON_BUFFER
927                                 ; This routine will copy an MCE command from the PC to Y memory.
928                                 ; The source RAM address has already been stored in CON_SRC_LO.
929                                 ; The destination address is always Y:COMMAND_BUFFER.
930                                 ;----------------------------------------------
931    
932       P:00026B P:00026D 54F400            MOVE              #>$C00,A1
                            000C00
933       P:00026D P:00026F 0BF080            JSR     TIMER_STORE_A1
                            00064F
934       P:00026F P:000271 0BF080            JSR     TIMER_STORE
                            00064D
935    
936       
937       P:000271 P:000273 60F400            MOVE              #>CON_SRC_LO,R0
                            00002C
938       P:000273 P:000275 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
939       P:000275 P:000277 60F400            MOVE              #>BURST_SRC_LO,R0
                            000031
940       P:000277 P:000279 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
941       P:000279 P:00027B 51F400            MOVE              #>COMMAND_BUFFER,B0
                            200000
942       P:00027B P:00027D 50F400            MOVE              #>256,A0
                            000100
943       P:00027D P:00027F 517000            MOVE              B0,X:YMEM_DEST
                            000033
944       P:00027F P:000281 507000            MOVE              A0,X:BLOCK_SIZE
                            00002B
945       P:000281 P:000283 0BF080            JSR     CON_TRANSFER
                            000552
946    
947       P:000283 P:000285 0A702B            BSET    #CON_MCE,X:STATUS
                            000000
948       P:000285 P:000287 0BF080            JSR     TIMER_STORE
                            00064D
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 20



949       P:000287 P:000289 00000C            RTS                                       ; Back to main loop
950    
951                                 ;----------------------------------------------
952                                 CON_TRANSMIT
953                                 ; This routine will copy the MCE command from Y:COMMAND_BUFFER to
954                                 ; the MCE command transmitter.
955                                 ;----------------------------------------------
956    
957       P:000288 P:00028A 0BF080            JSR     TIMER_STORE
                            00064D
958    
959       P:00028A P:00028C 66F400            MOVE              #>COMMAND_BUFFER,R6
                            200000
960       P:00028C P:00028E 068080            DO      #128,CON_TRANSMIT1                ; block size = 16bit x 128 (256 bytes)
                            000295
961       P:00028E P:000290 5CDE00            MOVE                          Y:(R6)+,A1  ; b2, b1  (lsb)
962       P:00028F P:000291 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
963       P:000290 P:000292 0140C6            AND     #>$FF,A
                            0000FF
964       P:000292 P:000294 547000            MOVE              A1,X:FO_SEND
                            FFF000
965       P:000294 P:000296 557000            MOVE              B1,X:FO_SEND
                            FFF000
966    
967                                 CON_TRANSMIT1
968       P:000296 P:000298 0A0121            BSET    #MODE_MCE,X:<MODE                 ; enable processing of MCE replies/data
969    
970       
971       P:000297 P:000299 0A700B            BCLR    #CON_MCE,X:STATUS
                            000000
972       P:000299 P:00029B 0A700A            BCLR    #CON_DEMAND,X:STATUS
                            000000
973    
974       P:00029B P:00029D 0BF080            JSR     TIMER_STORE
                            00064D
975    
976       
977                                           PCI_LOCKDOWN
979       P:00029E P:0002A0 44F400            MOVE              #'CON',X0
                            434F4E
980       P:0002A0 P:0002A2 0BF080            JSR     VCOM_PREPARE_REPLY
                            0002E9
981       P:0002A2 P:0002A4 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
982                                           PCI_LOCKUP                                ; Enable host IRQ
984    
985       P:0002A5 P:0002A7 0BF080            JSR     TIMER_STORE
                            00064D
986       P:0002A7 P:0002A9 00000C            RTS                                       ; Back to main loop
987    
988    
989    
990    
992    
993                                 ; --------------------------------------------------------------------------
994                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
995                                 ; --------------------------------------------------------------------------
996    
997                                 ; prepare notify to inform host that a packet has arrived.
998    
999                                 MCE_PACKET
1000                                          PCI_LOCKDOWN                              ; Disable host IRQ
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 21



1002      P:0002A9 P:0002AB 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
1003   
1004      P:0002AA P:0002AC 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
1005      P:0002AC P:0002AE 440B00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
1006   
1007      P:0002AD P:0002AF 449F00            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
1008      P:0002AE P:0002B0 440C00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
1009   
1010      P:0002AF P:0002B1 44A100            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
1011      P:0002B0 P:0002B2 440D00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
1012   
1013      P:0002B1 P:0002B3 44A200            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
1014      P:0002B2 P:0002B4 440E00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sage
1015   
1016      
1017      P:0002B3 P:0002B5 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
1018      P:0002B4 P:0002B6 0D047A            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
1019      P:0002B5 P:0002B7 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
1020                                          PCI_LOCKUP
1022   
1023      P:0002B7 P:0002B9 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
1024      P:0002B9 P:0002BB 0BF080            JSR     BUFFER_PACKET
                            0005CB
1025   
1026      
1027   
1028      P:0002BB P:0002BD 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; on fatal error, re-init.
                            000100
1029      P:0002BD P:0002BF 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; Set in 'send_packet_to_host' ISR
                            0002BB
1030   
1031      
1032      P:0002BF P:0002C1 44F400            MOVE              #>IMAGE_BUFFER,X0
                            000000
1033      P:0002C1 P:0002C3 56F000            MOVE              X:PACKET_SIZE,A
                            000023
1034      P:0002C3 P:0002C5 0C1D04            ASL     #2,A,A
1035      P:0002C4 P:0002C6 447000            MOVE              X0,X:YMEM_SRC
                            00002E
1036      P:0002C6 P:0002C8 547000            MOVE              A1,X:BLOCK_SIZE
                            00002B
1037      P:0002C8 P:0002CA 0BF080            JSR     BLOCK_TRANSFER
                            000518
1038   
1039      P:0002CA P:0002CC 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
1040   
1041      
1042                                          PCI_LOCKDOWN                              ; Disable host IRQ
1044      P:0002CD P:0002CF 44F400            MOVE              #'HST',X0
                            485354
1045      P:0002CF P:0002D1 0BF080            JSR     VCOM_PREPARE_REPLY
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 22



                            0002E9
1046      P:0002D1 P:0002D3 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1047                                          PCI_LOCKUP                                ; Enable host IRQ
1049      P:0002D4 P:0002D6 00000C            RTS
1050   
1051                                ;----------------------------------------------------------
1052                                ; clear out the fifo after an HST timeout...
1053                                ;----------------------------------------------------------
1054   
1055                                DUMP_FIFO
1056      P:0002D5 P:0002D7 61F400            MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
1057      P:0002D7 P:0002D9 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
1058      P:0002D9 P:0002DB 200013            CLR     A
1059      P:0002DA P:0002DC 320000            MOVE              #0,R2                   ; use R2 as a dump count
1060                                NEXT_DUMP
1061      P:0002DB P:0002DD 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E6
1062      P:0002DD P:0002DF 000000            NOP
1063      P:0002DE P:0002E0 000000            NOP
1064      P:0002DF P:0002E1 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E6
1065   
1066      P:0002E1 P:0002E3 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
1067      P:0002E2 P:0002E4 205A00            MOVE              (R2)+                   ; inc dump count
1068      P:0002E3 P:0002E5 224E00            MOVE              R2,A                    ;
1069      P:0002E4 P:0002E6 200045            CMP     X0,A                              ; check we've not hit dump limit
1070      P:0002E5 P:0002E7 0E22DB            JNE     NEXT_DUMP                         ; not hit limit?
1071                                FIFO_EMPTY
1072      P:0002E6 P:0002E8 627000            MOVE              R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000006
1073      P:0002E8 P:0002EA 0C0100            JMP     <START                            ; re-initialise
1074   
1075   
1076                                ; -------------------------------------------------------------------------------------
1077                                ;                              END OF MAIN PACKET HANDLING CODE
1078                                ; -------------------------------------------------------------------------------------
1079   
1080   
1081   
1082                                ; -------------------------------------------------------------------------------------
1083                                ;
1084                                ;                              INTERRUPT SERVICE ROUTINES
1085                                ;
1086                                ; -------------------------------------------------------------------------------------
1087   
1088                                ; ---------------
1089                                ; Rules:  Don't use N#, or any R# except R0 unless you add them to the saved register set.
1090   
1091   
1092                                ; ----------------------------------------------------------------------------
1093                                ; VCOM_* - routines: utility functions for hosty command vector communication.
1094                                ;-----------------------------------------------------------------------------
1095   
1096   
1097                                ; VCOM_PREPARE_REPLY
1098                                ;
1099                                ; Prepare the reply packet, using X0 as the command name (second word).  The
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 23



1100                                ; message defaults to 'ACK' with NULL data.  The user may subsequenty fill in
1101                                ; the data field (word 4) and mark the packet as error if necessary.
1102   
1103                                VCOM_PREPARE_REPLY
1104      
1105      
1106      P:0002E9 P:0002EB 50F400            MOVE              #'REP',A0
                            524550
1107      P:0002EB P:0002ED 447000            MOVE              X0,X:DTXS_WD2           ; Command
                            00000C
1108      P:0002ED P:0002EF 507000            MOVE              A0,X:DTXS_WD1
                            00000B
1109   
1110      P:0002EF P:0002F1 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
                            41434B
1111      P:0002F1 P:0002F3 000000            NOP
1112      P:0002F2 P:0002F4 547000            MOVE              A1,X:DTXS_WD3           ; ACK
                            00000D
1113      P:0002F4 P:0002F6 507000            MOVE              A0,X:DTXS_WD4           ; no comment
                            00000E
1114      P:0002F6 P:0002F8 00000C            RTS
1115   
1116   
1117                                ; VCOM_CHECK
1118                                ;
1119                                ; Compares DRXR_WD1 to X0.  If they are equal, Z is set on return.  If they
1120                                ; are not equal then Z is cleared and the reply will be marked as ERR with
1121                                ; 'CNE' in the last word.
1122                                ; Trashes A and B always and X0 on error.
1123   
1124                                VCOM_CHECK
1125      P:0002F7 P:0002F9 208E00            MOVE              X0,A
1126      P:0002F8 P:0002FA 57F000            MOVE              X:DRXR_WD1,B
                            000007
1127      P:0002FA P:0002FC 20000D            CMP     A,B
1128      P:0002FB P:0002FD 0AF0AA            JEQ     VCOM_RTS
                            000305
1129   
1130      P:0002FD P:0002FF 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
1131      P:0002FF P:000301 50F400            MOVE              #'ERR',A0
                            455252
1132      P:000301 P:000303 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1133      P:000303 P:000305 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1134                                VCOM_RTS
1135      P:000305 P:000307 00000C            RTS
1136   
1137   
1138                                ; VCOM_INTRO
1139                                ;
1140                                ; Read DSP command from DRXR.  Prepare the reply packet and verify that it
1141                                ; matches the key in X1.  If it does not, mark the reply as error and set
1142                                ; the Z flag.
1143   
1144                                VCOM_INTRO
1145      P:000306 P:000308 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            000318
1146      P:000308 P:00030A 20A400            MOVE              X1,X0
1147      P:000309 P:00030B 0D02E9            JSR     VCOM_PREPARE_REPLY
1148      P:00030A P:00030C 0D02F7            JSR     VCOM_CHECK
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 24



1149      P:00030B P:00030D 00000C            RTS
1150   
1151   
1152                                ; VCOM_EXIT_ERROR_X0
1153                                ; VCOM_EXIT_X0
1154                                ; VCOM_EXIT
1155                                ;
1156                                ; For returning from host command vector interrupts only.  These three
1157                                ; routines do the following (respectively):
1158                                ; a) Mark reply as error, then (b)
1159                                ; b) Put X0 into last word of reply, then (c)
1160                                ; c) Restore registers and RTI.
1161   
1162                                VCOM_EXIT_ERROR_X0
1163      P:00030C P:00030E 50F400            MOVE              #'ERR',A0
                            455252
1164      P:00030E P:000310 000000            NOP
1165      P:00030F P:000311 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1166                                VCOM_EXIT_X0
1167      P:000311 P:000313 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1168                                VCOM_EXIT
1169      P:000313 P:000315 0BF080            JSR     RESTORE_REGISTERS
                            000493
1170      P:000315 P:000317 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1171      P:000317 P:000319 000004            RTI
1172   
1173   
1174                                ;---------------------------------------------------------------
1175                                RD_DRXR
1176                                ;--------------------------------------------------------------
1177                                ; Routine to read from HTXR-DRXR data path.  For HCTR = 0x900,
1178                                ; 3 LSB of each 32-bit word written by the host is returned on
1179                                ; each read.  This only polls for first word, not all of them.
1180      P:000318 P:00031A 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            000318
1181      P:00031A P:00031C 63F400            MOVE              #DRXR_WD1,R3
                            000007
1182      P:00031C P:00031E 0604A0            REP     #4
1183      P:00031D P:00031F 085B8B            MOVEP             X:DRXR,X:(R3)+
1184      P:00031E P:000320 00000C            RTS
1185   
1186   
1187                                ; ----------------------------------------------------------------------------
1188                                READ_MEMORY
1189                                ;-----------------------------------------------------------------------------
1190                                ;Read command:
1191                                ; word 1 = command = 'RDM'
1192                                ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
1193                                ; word 3 = address in memory
1194                                ; word 4 = not used
1195                                ;Version query:
1196                                ; word 1 = 'VER'
1197                                ; word 2-4 unused
1198   
1199      P:00031F P:000321 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1200      P:000321 P:000323 0D0318            JSR     RD_DRXR                           ; Loads DRXR_WD*
1201   
1202      P:000322 P:000324 44F400            MOVE              #'RDM',X0
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 25



                            52444D
1203      P:000324 P:000326 0D02E9            JSR     VCOM_PREPARE_REPLY
1204      P:000325 P:000327 0D02F7            JSR     VCOM_CHECK
1205      P:000326 P:000328 0AF0AA            JEQ     READ_MEMORY_XYP
                            000330
1206   
1207      
1208      P:000328 P:00032A 44F400            MOVE              #'VER',X0
                            564552
1209      P:00032A P:00032C 0D02E9            JSR     VCOM_PREPARE_REPLY
1210      P:00032B P:00032D 0D02F7            JSR     VCOM_CHECK
1211      P:00032C P:00032E 0E2313            JNE     VCOM_EXIT
1212   
1213      P:00032D P:00032F 44F000            MOVE              X:REV_NUMBER,X0
                            000003
1214      P:00032F P:000331 0C0311            JMP     VCOM_EXIT_X0
1215   
1216                                READ_MEMORY_XYP
1217   
1218      
1219      P:000330 P:000332 56F000            MOVE              X:DRXR_WD2,A
                            000008
1220      P:000332 P:000334 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1221   
1222      P:000334 P:000336 0140C5            CMP     #'_X',A
                            005F58
1223      P:000336 P:000338 0AF0AA            JEQ     READ_MEMORY_X
                            000343
1224   
1225      P:000338 P:00033A 0140C5            CMP     #'_Y',A
                            005F59
1226      P:00033A P:00033C 0AF0AA            JEQ     READ_MEMORY_Y
                            000345
1227   
1228      P:00033C P:00033E 0140C5            CMP     #'_P',A
                            005F50
1229      P:00033E P:000340 0AF0AA            JEQ     READ_MEMORY_P
                            000347
1230   
1231      P:000340 P:000342 44F400            MOVE              #'MTE',X0
                            4D5445
1232      P:000342 P:000344 0C030C            JMP     VCOM_EXIT_ERROR_X0
1233   
1234                                READ_MEMORY_X
1235      P:000343 P:000345 44E000            MOVE              X:(R0),X0
1236      P:000344 P:000346 0C0311            JMP     VCOM_EXIT_X0
1237                                READ_MEMORY_Y
1238      P:000345 P:000347 4CE000            MOVE                          Y:(R0),X0
1239      P:000346 P:000348 0C0311            JMP     VCOM_EXIT_X0
1240                                READ_MEMORY_P
1241      P:000347 P:000349 07E084            MOVE              P:(R0),X0
1242      P:000348 P:00034A 0C0311            JMP     VCOM_EXIT_X0
1243   
1244   
1245                                ;--------------------------------------------------------------
1246                                WRITE_MEMORY
1247                                ;---------------------------------------------------------------
1248                                ; word 1 = command = 'WRM'
1249                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1250                                ; word 3 = address in memory
1251                                ; word 4 = value
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 26



1252   
1253      P:000349 P:00034B 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1254      P:00034B P:00034D 45F400            MOVE              #'WRM',X1
                            57524D
1255      P:00034D P:00034F 0D0306            JSR     VCOM_INTRO
1256      P:00034E P:000350 0E2313            JNE     VCOM_EXIT
1257   
1258      
1259      P:00034F P:000351 56F000            MOVE              X:DRXR_WD2,A
                            000008
1260      P:000351 P:000353 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1261      P:000353 P:000355 44F000            MOVE              X:DRXR_WD4,X0
                            00000A
1262   
1263      P:000355 P:000357 0140C5            CMP     #'_X',A
                            005F58
1264      P:000357 P:000359 0AF0AA            JEQ     WRITE_MEMORY_X
                            000364
1265   
1266      P:000359 P:00035B 0140C5            CMP     #'_Y',A
                            005F59
1267      P:00035B P:00035D 0AF0AA            JEQ     WRITE_MEMORY_Y
                            000366
1268   
1269      P:00035D P:00035F 0140C5            CMP     #'_P',A
                            005F50
1270      P:00035F P:000361 0AF0AA            JEQ     WRITE_MEMORY_P
                            000368
1271   
1272      P:000361 P:000363 44F400            MOVE              #'MTE',X0
                            4D5445
1273      P:000363 P:000365 0C030C            JMP     VCOM_EXIT_ERROR_X0
1274   
1275                                WRITE_MEMORY_X
1276      P:000364 P:000366 446000            MOVE              X0,X:(R0)
1277      P:000365 P:000367 0C0311            JMP     VCOM_EXIT_X0
1278                                WRITE_MEMORY_Y
1279      P:000366 P:000368 4C6000            MOVE                          X0,Y:(R0)
1280      P:000367 P:000369 0C0311            JMP     VCOM_EXIT_X0
1281                                WRITE_MEMORY_P
1282      P:000368 P:00036A 076084            MOVE              X0,P:(R0)
1283      P:000369 P:00036B 0C0311            JMP     VCOM_EXIT_X0
1284   
1285   
1286                                ;-----------------------------------------------------------------------------
1287                                START_APPLICATION
1288                                ; an application should already have been downloaded to the PCI memory.
1289                                ; this command will execute it.
1290                                ; ----------------------------------------------------------------------
1291                                ; word 1 = command = 'GOA'
1292                                ; word 2-4 unused
1293   
1294      P:00036A P:00036C 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1295      P:00036C P:00036E 45F400            MOVE              #'GOA',X1
                            474F41
1296   
1297      P:00036E P:000370 0D0306            JSR     VCOM_INTRO
1298      P:00036F P:000371 0E2313            JNE     VCOM_EXIT
1299   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 27



1300      P:000370 P:000372 0A7020            BSET    #MODE_APPLICATION,X:MODE
                            000001
1301      P:000372 P:000374 000004            RTI                                       ; Application will reply.
1302   
1303   
1304                                ; ---------------------------------------------------------
1305                                STOP_APPLICATION
1306                                ; this command stops an application that is currently running
1307                                ; used for applications that once started run contiunually
1308                                ;-----------------------------------------------------------
1309                                ; word 1 = command = ' STP'
1310                                ; word 2-4 unused
1311   
1312      P:000373 P:000375 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1313      P:000375 P:000377 45F400            MOVE              #'STP',X1
                            535450
1314   
1315      P:000377 P:000379 0D0306            JSR     VCOM_INTRO
1316      P:000378 P:00037A 0E2313            JNE     VCOM_EXIT
1317   
1318      P:000379 P:00037B 0A7000            BCLR    #MODE_APPLICATION,X:MODE
                            000001
1319      P:00037B P:00037D 0A7000            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1320      P:00037D P:00037F 0C0313            JMP     VCOM_EXIT
1321   
1322   
1323                                ;-----------------------------------------------------------------------------
1324                                RESET_CONTROLLER
1325                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1326                                ;-----------------------------------------------------------------------------
1327                                ; word 1 = command = 'RCO'
1328                                ; word 2-4 unused
1329   
1330      P:00037E P:000380 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1331      P:000380 P:000382 45F400            MOVE              #'RCO',X1
                            52434F
1332      P:000382 P:000384 0D0306            JSR     VCOM_INTRO
1333      P:000383 P:000385 0E2313            JNE     VCOM_EXIT
1334   
1335      P:000384 P:000386 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1336      P:000385 P:000387 000000            NOP
1337      P:000386 P:000388 000000            NOP
1338      P:000387 P:000389 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1339      P:000389 P:00038B 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1340      P:00038B P:00038D 446000            MOVE              X0,X:(R0)
1341      P:00038C P:00038E 0606A0            REP     #6                                ; Wait for transmission to complete
1342      P:00038D P:00038F 000000            NOP
1343      P:00038E P:000390 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1344   
1345                                ; Wait for a bit for MCE to be reset.......
1346      P:00038F P:000391 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1347      P:000391 P:000393 06C400            DO      X0,L_DELAY
                            000397
1348      P:000393 P:000395 06E883            DO      #1000,L_RDFIFO
                            000396
1349      P:000395 P:000397 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 28



1350      P:000396 P:000398 000000            NOP                                       ;   receiver empty
1351                                L_RDFIFO
1352      P:000397 P:000399 000000            NOP
1353                                L_DELAY
1354      P:000398 P:00039A 000000            NOP
1355   
1356      P:000399 P:00039B 44F400            MOVE              #'000',X0
                            303030
1357      P:00039B P:00039D 0C0311            JMP     VCOM_EXIT_X0
1358   
1359                                ;-----------------------------------------------------------------------------
1360                                QUIET_TRANSFER_SET
1361                                ;-----------------------------------------------------------------------------
1362                                ;Quiet transfer mode configuration
1363                                ; word 1 = command = 'QTS'
1364                                ; word 2 = parameter to set
1365                                ; word 3-4 = arguments
1366   
1367      P:00039C P:00039E 0BF080            JSR     SAVE_REGISTERS                    ; standard opening
                            0004A0
1368      P:00039E P:0003A0 45F400            MOVE              #'QTS',X1
                            515453
1369      P:0003A0 P:0003A2 0D0306            JSR     VCOM_INTRO
1370      P:0003A1 P:0003A3 0E2313            JNE     VCOM_EXIT
1371   
1372      P:0003A2 P:0003A4 60F400            MOVE              #BDEBUG0,R0
                            00004D
1373      P:0003A4 P:0003A6 0D01DE            JSR     INCR_X_R0
1374   
1375      P:0003A5 P:0003A7 56F000            MOVE              X:DRXR_WD2,A            ; Parameter id
                            000008
1376      P:0003A7 P:0003A9 44F000            MOVE              X:DRXR_WD3,X0           ; First arg
                            000009
1377      P:0003A9 P:0003AB 45F000            MOVE              X:DRXR_WD4,X1           ; Second arg
                            00000A
1378   
1379      P:0003AB P:0003AD 0140C5            CMP     #'BAS',A
                            424153
1380      P:0003AD P:0003AF 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            00042D
1381   
1382      P:0003AF P:0003B1 0140C5            CMP     #'DEL',A
                            44454C
1383      P:0003B1 P:0003B3 60F400            MOVE              #QT_BUF_SIZE,R0
                            00003E
1384      P:0003B3 P:0003B5 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1385   
1386      P:0003B5 P:0003B7 0140C5            CMP     #'NUM',A
                            4E554D
1387      P:0003B7 P:0003B9 60F400            MOVE              #QT_BUF_MAX,R0
                            00003F
1388      P:0003B9 P:0003BB 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1389   
1390      P:0003BB P:0003BD 0140C5            CMP     #'INF',A
                            494E46
1391      P:0003BD P:0003BF 60F400            MOVE              #QT_INFORM,R0
                            000041
1392      P:0003BF P:0003C1 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1393   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 29



1394      P:0003C1 P:0003C3 0140C5            CMP     #'SIZ',A
                            53495A
1395      P:0003C3 P:0003C5 60F400            MOVE              #QT_FRAME_SIZE,R0
                            000040
1396      P:0003C5 P:0003C7 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1397   
1398      P:0003C7 P:0003C9 0140C5            CMP     #'TAI',A
                            544149
1399      P:0003C9 P:0003CB 60F400            MOVE              #QT_BUF_TAIL,R0
                            000043
1400      P:0003CB P:0003CD 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1401   
1402      P:0003CD P:0003CF 0140C5            CMP     #'HEA',A
                            484541
1403      P:0003CF P:0003D1 60F400            MOVE              #QT_BUF_HEAD,R0
                            000042
1404      P:0003D1 P:0003D3 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1405   
1406      P:0003D3 P:0003D5 0140C5            CMP     #'DRO',A
                            44524F
1407      P:0003D5 P:0003D7 60F400            MOVE              #QT_DROPS,R0
                            000047
1408      P:0003D7 P:0003D9 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1409   
1410      P:0003D9 P:0003DB 0140C5            CMP     #'PER',A
                            504552
1411      P:0003DB P:0003DD 60F400            MOVE              #TCPR0,R0
                            FFFF8D
1412      P:0003DD P:0003DF 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1413   
1414      P:0003DF P:0003E1 0140C5            CMP     #'FLU',A
                            464C55
1415      P:0003E1 P:0003E3 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            00040D
1416   
1417      P:0003E3 P:0003E5 0140C5            CMP     #'SET',A
                            534554
1418      P:0003E5 P:0003E7 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            000415
1419   
1420      P:0003E7 P:0003E9 0140C5            CMP     #'RPS',A
                            525053
1421      P:0003E9 P:0003EB 60F400            MOVE              #RP_MAX_SIZE,R0
                            00004A
1422      P:0003EB P:0003ED 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1423   
1424      P:0003ED P:0003EF 0140C5            CMP     #'RPB',A
                            525042
1425      P:0003EF P:0003F1 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_BASE
                            0003FE
1426   
1427      P:0003F1 P:0003F3 0140C5            CMP     #'RPE',A
                            525045
1428      P:0003F3 P:0003F5 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_ENABLED
                            000403
1429   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 30



1430      P:0003F5 P:0003F7 0140C5            CMP     #'BUR',A
                            425552
1431      P:0003F7 P:0003F9 60F400            MOVE              #PCI_BURST_SIZE,R0
                            000029
1432      P:0003F9 P:0003FB 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0_PERSISTENT
                            000425
1433   
1434      P:0003FB P:0003FD 44F400            MOVE              #'MTE',X0
                            4D5445
1435      P:0003FD P:0003FF 0C030C            JMP     VCOM_EXIT_ERROR_X0
1436   
1437                                QUIET_TRANSFER_SET_RP_BASE
1438      P:0003FE P:000400 447000            MOVE              X0,X:RP_BASE_LO
                            000048
1439      P:000400 P:000402 457000            MOVE              X1,X:RP_BASE_HI
                            000049
1440      P:000402 P:000404 0C0313            JMP     VCOM_EXIT
1441   
1442                                QUIET_TRANSFER_SET_RP_ENABLED
1443      P:000403 P:000405 0A7003            BCLR    #MODE_RP_BUFFER,X:MODE
                            000001
1444      P:000405 P:000407 208E00            MOVE              X0,A
1445      P:000406 P:000408 200003            TST     A
1446      P:000407 P:000409 0EA313            JEQ     VCOM_EXIT
1447      P:000408 P:00040A 0A7023            BSET    #MODE_RP_BUFFER,X:MODE
                            000001
1448      P:00040A P:00040C 0A7015            BCLR    #RP_BUFFER_FULL,X:STATUS
                            000000
1449      P:00040C P:00040E 0C0313            JMP     VCOM_EXIT
1450   
1451                                QUIET_TRANSFER_SET_FLUSH
1452      P:00040D P:00040F 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
1453      P:00040F P:000411 208E00            MOVE              X0,A
1454      P:000410 P:000412 200003            TST     A
1455      P:000411 P:000413 0EA313            JEQ     VCOM_EXIT
1456      P:000412 P:000414 0A7034            BSET    #QT_FLUSH,X:STATUS
                            000000
1457      P:000414 P:000416 0C0313            JMP     VCOM_EXIT
1458   
1459                                QUIET_TRANSFER_SET_ENABLED
1460      P:000415 P:000417 208E00            MOVE              X0,A
1461      P:000416 P:000418 200003            TST     A
1462      P:000417 P:000419 0AF0AA            JEQ     QUIET_TRANSFER_SET_DISABLED
                            00041E
1463      P:000419 P:00041B 0A7022            BSET    #MODE_QT,X:MODE
                            000001
1464      P:00041B P:00041D 0BF080            JSR     TIMER_ENABLE
                            000622
1465      P:00041D P:00041F 0C0313            JMP     VCOM_EXIT
1466   
1467                                QUIET_TRANSFER_SET_DISABLED
1468      P:00041E P:000420 0A7002            BCLR    #MODE_QT,X:MODE
                            000001
1469      P:000420 P:000422 0BF080            JSR     TIMER_DEFAULT
                            00062E
1470      P:000422 P:000424 0C0313            JMP     VCOM_EXIT
1471   
1472                                QUIET_TRANSFER_SET_R0
1473      P:000423 P:000425 446000            MOVE              X0,X:(R0)
1474      P:000424 P:000426 0C0313            JMP     VCOM_EXIT
1475   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 31



1476                                QUIET_TRANSFER_SET_R0_PERSISTENT
1477      
1478      
1479      
1480      P:000425 P:000427 446000            MOVE              X0,X:(R0)
1481      P:000426 P:000428 57F400            MOVE              #>VAR_TBL_START,B
                            0006AC
1482      P:000428 P:00042A 220E00            MOVE              R0,A
1483      P:000429 P:00042B 200018            ADD     A,B
**** 1484 [main.asm 873]: WARNING --- Pipeline stall reading register B written in previous instruction (X data move field)
**** 1484 [main.asm 873]: WARNING --- Pipeline stall reading register written in previous instruction (X data move field)
1484      P:00042A P:00042C 21F000            MOVE              B,R0
**** 1485 [main.asm 874]: WARNING --- Pipeline stall reading register written in instruction at address: P:00042A (X data move field
)
1485      P:00042B P:00042D 076084            MOVE              X0,P:(R0)
1486      P:00042C P:00042E 0C0313            JMP     VCOM_EXIT
1487   
1488                                QUIET_TRANSFER_SET_BASE
1489      P:00042D P:00042F 447000            MOVE              X0,X:QT_BASE_LO
                            00003C
1490      P:00042F P:000431 457000            MOVE              X1,X:QT_BASE_HI
                            00003D
1491   
1492      P:000431 P:000433 0BF080            JSR     BUFFER_RESET
                            000669
1493   
1494      P:000433 P:000435 0C0313            JMP     VCOM_EXIT
1495   
1496   
1497                                ;-----------------------------------------------------------------------------
1498                                SYSTEM_RESET
1499                                ;-----------------------------------------------------------------------------
1500   
1501      P:000434 P:000436 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1502      P:000435 P:000437 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1503                                                                                    ; set to zero except for interrupts
1504      P:000437 P:000439 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1505                                                                                    ; so first set to 0
1506      P:000438 P:00043A 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1507                                                                                    ; therefore,return to initialization
1508      P:00043A P:00043C 000000            NOP
1509      P:00043B P:00043D 000004            RTI                                       ; return from ISR - to START
1510   
1511   
1512                                ; ------------------------------------------------------------------------------------
1513                                SEND_PACKET_TO_HOST
1514                                ; this command is received from the Host and actions the PCI board to pick up an address
1515                                ; pointer from DRXR which the PCI board then uses to write packets from the
1516                                ; MCE to the host memory starting at the address given.
1517                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1518                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1519                                ; HST after packet sent (unless error).
1520                                ; --------------------------------------------------------------------------------------
1521                                ; word 1 = command = 'HST'
1522                                ; word 2 = host high address
1523                                ; word 3 = host low address
1524                                ; word 4 = not used but read
1525   
1526      P:00043C P:00043E 0D04A0            JSR     <SAVE_REGISTERS                   ; save working registers
1527      P:00043D P:00043F 45F400            MOVE              #'HST',X1
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 32



                            485354
1528      P:00043F P:000441 0D0306            JSR     VCOM_INTRO
1529      P:000440 P:000442 0E2313            JNE     VCOM_EXIT
1530   
1531      
1532      P:000441 P:000443 448800            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
1533      P:000442 P:000444 518900            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1534   
1535      P:000443 P:000445 447000            MOVE              X0,X:BURST_DEST_HI
                            000030
1536      P:000445 P:000447 517000            MOVE              B0,X:BURST_DEST_LO
                            00002F
1537   
1538      P:000447 P:000449 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1539   
1540      P:000448 P:00044A 0BF080            JSR     RESTORE_REGISTERS
                            000493
1541      P:00044A P:00044C 000004            RTI                                       ; Main loop will reply after packet transfer
!
1542   
1543   
1544                                ; --------------------------------------------------------------------
1545                                SOFTWARE_RESET
1546                                ;----------------------------------------------------------------------
1547                                ; word 1 = command = 'RST'
1548                                ; word 2-4 unused
1549   
1550      P:00044B P:00044D 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1551      P:00044D P:00044F 45F400            MOVE              #'RST',X1
                            525354
1552      P:00044F P:000451 0D0306            JSR     VCOM_INTRO
1553      P:000450 P:000452 0E2313            JNE     VCOM_EXIT
1554   
1555                                ; RST command OK so reply to host
1556                                FINISH_RST
1557      P:000451 P:000453 44F400            MOVE              #'000',X0
                            303030
1558      P:000453 P:000455 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1559      P:000455 P:000457 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1560   
1561      P:000457 P:000459 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            000457
1562   
1563      P:000459 P:00045B 0A0100            BCLR    #MODE_APPLICATION,X:<MODE         ; clear app flag
1564      P:00045A P:00045C 0A0000            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1565   
1566      P:00045B P:00045D 0C0434            JMP     SYSTEM_RESET                      ; Handle the stack and stuff...
1567   
1568   
1569                                SEND_PACKET_TO_CONTROLLER
1570   
1571                                ;       Host command identifying location of an MCE command to send to
1572                                ;       the MCE.  Since this can come at any time, just record the
1573                                ;       request and then do the CONning from the main loop.
1574   
1575                                ; word 1 = command = 'CON'
1576                                ; word 2 = source host bus address, bits 31:16
1577                                ; word 3 = source host bus address, bits 15:0
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 33



1578                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1579                                ;        = '1' --> when MCE command is GO
1580   
1581      P:00045C P:00045E 0D04A0            JSR     <SAVE_REGISTERS                   ; save working registers
1582   
1583      
1584      P:00045D P:00045F 45F400            MOVE              #'CON',X1
                            434F4E
1585      P:00045F P:000461 0D0306            JSR     VCOM_INTRO
1586      P:000460 P:000462 0E2313            JNE     VCOM_EXIT
1587   
1588      
1589      P:000461 P:000463 44F400            MOVE              #'BUS',X0
                            425553
1590      P:000463 P:000465 0A00AA            JSET    #CON_DEMAND,X:STATUS,VCOM_EXIT_ERROR_X0
                            00030C
1591   
1592      
1593      P:000465 P:000467 0A702A            BSET    #CON_DEMAND,X:STATUS
                            000000
1594      P:000467 P:000469 448800            MOVE              X:<DRXR_WD2,X0
1595      P:000468 P:00046A 458900            MOVE              X:<DRXR_WD3,X1
1596      P:000469 P:00046B 447000            MOVE              X0,X:CON_SRC_HI
                            00002D
1597      P:00046B P:00046D 457000            MOVE              X1,X:CON_SRC_LO
                            00002C
1598   
1599                                ;       ;; Fourth word indicates if this is a go.  Who cares?
1600                                ;       MOVE    X:<DRXR_WD4,A           ; read word 4 - GO command?
1601                                ;       MOVE    #0,X0
1602                                ;       CMP     X0,A
1603                                ;       JEQ     BLOCK_CON
1604   
1605      
1606      P:00046D P:00046F 0BF080            JSR     RESTORE_REGISTERS
                            000493
1607      P:00046F P:000471 000004            RTI
1608   
1610   
1611   
1612                                ;---------------------------------------------------------------
1613                                ;
1614                                ;                          * END OF ISRs *
1615                                ;
1616                                ;--------------------------------------------------------------
1617   
1618   
1619   
1620                                ;----------------------------------------------------------------
1621                                ;
1622                                ;                     * Beginning of SUBROUTINES *
1623                                ;
1624                                ;-----------------------------------------------------------------
1625   
1626   
1627                                CHECK_FO
1628      P:000470 P:000472 01AD80            JCLR    #EF,X:PDRD,CHECK_FO_CLEAR
                            000478
1629      P:000472 P:000474 000000            NOP
1630      P:000473 P:000475 000000            NOP
1631      P:000474 P:000476 01AD80            JCLR    #EF,X:PDRD,CHECK_FO_CLEAR
                            000478
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 34



1632      P:000476 P:000478 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1633      P:000477 P:000479 00000C            RTS
1634   
1635                                CHECK_FO_CLEAR
1636      P:000478 P:00047A 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1637      P:000479 P:00047B 00000C            RTS
1638   
1639   
1640   
1641                                ;----------------------------------------------------------------------------
1642                                PCI_MESSAGE_TO_HOST
1643                                ;----------------------------------------------------------------------------
1644                                ; Subroutine to send 4 words as a reply from PCI to the Host
1645                                ; using the DTXS-HRXS data path.  The DSP signals the host by raising
1646                                ; HF3 and (when !MODE_NOIRQ) INTA.
1647                                ;
1648                                ; When MODE_HANDSHAKE, the DSP and Host interact as follows:
1649                                ; - to show that the Host is handling the interrupt, Host raises HF0
1650                                ; - when DSP sees HF0 go high, it lowers INTA and HF3
1651                                ; - when Host is done handling the interrupt (i.e. it has read the reply),
1652                                ;   and when HF3 is low, Host lowers HF0.
1653                                ; - when DSP sees HF0 go low, the routine finishes.
1654                                ;
1655                                ; The primary advantage of this hand-shaking scheme is that host vector
1656                                ; commands are not needed to clear HF3 and INTA.
1657                                ;
1658                                ; This routine should not block for anything other than the Host handshake.
1659   
1660      P:00047A P:00047C 60F400            MOVE              #>DTXS_WD1,R0
                            00000B
1661   
1662      P:00047C P:00047E 060480            DO      #4,PCI_MESSAGE_TO_HOST_10
                            000480
1663      P:00047E P:000480 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            00047E
1664      P:000480 P:000482 08D88D            MOVEP             X:(R0)+,X:DTXS
1665   
1666                                PCI_MESSAGE_TO_HOST_10
1667      P:000481 P:000483 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            000015
1668      P:000483 P:000485 60F000            MOVE              X:SV_R0,R0              ; restore R0
                            000019
1669      P:000485 P:000487 0A8523            BSET    #DCTR_HF3,X:DCTR                  ; Raise HF3 (handshake)
1670   
1671                                                                                    ; Only interrupt in irq mode
1672      
1673      P:000486 P:000488 000000            NOP
1674      P:000487 P:000489 000000            NOP
1675      P:000488 P:00048A 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1676   
1677                                PCI_MESSAGE_TO_HOST_20
1678      P:000489 P:00048B 0A89A4            JSET    #DSR_HF1,X:DSR,PCI_MESSAGE_TO_HOST_HANDSHAKE
                            00048C
1679      P:00048B P:00048D 00000C            RTS
1680   
1681                                PCI_MESSAGE_TO_HOST_HANDSHAKE
1682      P:00048C P:00048E 0A8983            JCLR    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            00048C
1683      P:00048E P:000490 0A8506            BCLR    #INTA,X:DCTR                      ; Clear interrupt
1684      P:00048F P:000491 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; Clear hand-shake bit
1685      P:000490 P:000492 0A89A3            JSET    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            000490
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 35



1686      P:000492 P:000494 00000C            RTS
1687   
1688   
1689                                ;------------------------------------------------------------------------------------
1690                                RESTORE_REGISTERS
1691                                ;-------------------------------------------------------------------------------------
1692   
1693      P:000493 P:000495 059A39            MOVEC             X:<SV_SR,SR
1694   
1695      P:000494 P:000496 508F00            MOVE              X:<SV_A0,A0
1696      P:000495 P:000497 549000            MOVE              X:<SV_A1,A1
1697      P:000496 P:000498 529100            MOVE              X:<SV_A2,A2
1698   
1699      P:000497 P:000499 519200            MOVE              X:<SV_B0,B0
1700      P:000498 P:00049A 559300            MOVE              X:<SV_B1,B1
1701      P:000499 P:00049B 539400            MOVE              X:<SV_B2,B2
1702   
1703      P:00049A P:00049C 449500            MOVE              X:<SV_X0,X0
1704      P:00049B P:00049D 459600            MOVE              X:<SV_X1,X1
1705   
1706      P:00049C P:00049E 469700            MOVE              X:<SV_Y0,Y0
1707      P:00049D P:00049F 479800            MOVE              X:<SV_Y1,Y1
1708   
1709      P:00049E P:0004A0 609900            MOVE              X:<SV_R0,R0
1710      P:00049F P:0004A1 00000C            RTS
1711   
1712                                ;-------------------------------------------------------------------------------------
1713                                SAVE_REGISTERS
1714                                ;-------------------------------------------------------------------------------------
1715   
1716      P:0004A0 P:0004A2 051A39            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1717   
1718      P:0004A1 P:0004A3 500F00            MOVE              A0,X:<SV_A0
1719      P:0004A2 P:0004A4 541000            MOVE              A1,X:<SV_A1
1720      P:0004A3 P:0004A5 521100            MOVE              A2,X:<SV_A2
1721   
1722      P:0004A4 P:0004A6 511200            MOVE              B0,X:<SV_B0
1723      P:0004A5 P:0004A7 551300            MOVE              B1,X:<SV_B1
1724      P:0004A6 P:0004A8 531400            MOVE              B2,X:<SV_B2
1725   
1726      P:0004A7 P:0004A9 441500            MOVE              X0,X:<SV_X0
1727      P:0004A8 P:0004AA 451600            MOVE              X1,X:<SV_X1
1728   
1729      P:0004A9 P:0004AB 461700            MOVE              Y0,X:<SV_Y0
1730      P:0004AA P:0004AC 471800            MOVE              Y1,X:<SV_Y1
1731   
1732      P:0004AB P:0004AD 601900            MOVE              R0,X:<SV_R0
1733      P:0004AC P:0004AE 00000C            RTS
1734   
1735   
1736                                ;----------------------------------------------
1737                                FLUSH_PCI_FIFO
1738                                ;----------------------------------------------
1739      P:0004AD P:0004AF 0A8A84            JCLR    #MARQ,X:DPSR,*
                            0004AD
1740      P:0004AF P:0004B1 0A862E            BSET    #CLRT,X:DPCR
1741      P:0004B0 P:0004B2 000000            NOP
1742      P:0004B1 P:0004B3 0A86AE            JSET    #CLRT,X:DPCR,*
                            0004B1
1743      P:0004B3 P:0004B5 00000C            RTS
1744   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 36



1745                                ;----------------------------------------------
1746                                CLEAR_FO_FIFO
1747                                ;----------------------------------------------
1748      P:0004B4 P:0004B6 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
1749      P:0004B6 P:0004B8 44F400            MOVE              #200000,X0
                            030D40
1750      P:0004B8 P:0004BA 06C400            DO      X0,*+3
                            0004BA
1751      P:0004BA P:0004BC 000000            NOP
1752      P:0004BB P:0004BD 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
1753      P:0004BD P:0004BF 00000C            RTS
1754   
1755   
1756                                ;---------------------------------------------------------
1757                                ; PCI burst routines
1758                                ;
1759                                ; For transfer between Host memory and DSP Y memory.
1760                                ;
1761                                ; Major entry points are
1762                                ;       CON_TRANSFER (PC -> DSP)
1763                                ;       BLOCK_TRANSFER (DSP -> PC)
1764                                ;---------------------------------------------------------
1765   
1766                                ;---------------------------------------------------------
1767                                PCI_ERROR_CLEAR
1768                                ;-----------------------------------------------
1769      
1770      
1771      
1772      
1773      
1774      
1775   
1776      P:0004BE P:0004C0 50F000            MOVE              X:DMA_ERRORS,A0
                            000034
1777      P:0004C0 P:0004C2 000008            INC     A
1778      P:0004C1 P:0004C3 000000            NOP
1779      P:0004C2 P:0004C4 507000            MOVE              A0,X:DMA_ERRORS
                            000034
1780   
1781      P:0004C4 P:0004C6 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            0004D2
1782      P:0004C6 P:0004C8 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
                            0004DC
1783      P:0004C8 P:0004CA 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            0004E6
1784      P:0004CA P:0004CC 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            0004F0
1785      P:0004CC P:0004CE 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            0004FA
1786      P:0004CE P:0004D0 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            000504
1787      P:0004D0 P:0004D2 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            00050E
1788   
1789                                ERROR_TRTY
1790      P:0004D2 P:0004D4 50F000            MOVE              X:EC_TRTY,A0
                            000035
1791      P:0004D4 P:0004D6 000008            INC     A
1792      P:0004D5 P:0004D7 08F48A            MOVEP             #>$0400,X:DPSR          ; Clear target retry error bit
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 37



                            000400
1793      P:0004D7 P:0004D9 507000            MOVE              A0,X:EC_TRTY
                            000035
1794      P:0004D9 P:0004DB 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1795      P:0004DB P:0004DD 00000C            RTS
1796                                ERROR_TO
1797      P:0004DC P:0004DE 50F000            MOVE              X:EC_TO,A0
                            000036
1798      P:0004DE P:0004E0 000008            INC     A
1799      P:0004DF P:0004E1 08F48A            MOVEP             #>$0800,X:DPSR          ; Clear timeout error bit
                            000800
1800      P:0004E1 P:0004E3 507000            MOVE              A0,X:EC_TO
                            000036
1801      P:0004E3 P:0004E5 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1802      P:0004E5 P:0004E7 00000C            RTS
1803                                ERROR_TDIS
1804      P:0004E6 P:0004E8 50F000            MOVE              X:EC_TDIS,A0
                            000037
1805      P:0004E8 P:0004EA 000008            INC     A
1806      P:0004E9 P:0004EB 08F48A            MOVEP             #>$0200,X:DPSR          ; Clear target disconnect bit
                            000200
1807      P:0004EB P:0004ED 507000            MOVE              A0,X:EC_TDIS
                            000037
1808      P:0004ED P:0004EF 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1809      P:0004EF P:0004F1 00000C            RTS
1810                                ERROR_TAB
1811      P:0004F0 P:0004F2 50F000            MOVE              X:EC_TAB,A0
                            000038
1812      P:0004F2 P:0004F4 000008            INC     A
1813      P:0004F3 P:0004F5 08F48A            MOVEP             #>$0100,X:DPSR          ; Clear target abort error bit
                            000100
1814      P:0004F5 P:0004F7 507000            MOVE              A0,X:EC_TAB
                            000038
1815      P:0004F7 P:0004F9 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1816      P:0004F9 P:0004FB 00000C            RTS
1817                                ERROR_MAB
1818      P:0004FA P:0004FC 50F000            MOVE              X:EC_MAB,A0
                            000039
1819      P:0004FC P:0004FE 000008            INC     A
1820      P:0004FD P:0004FF 08F48A            MOVEP             #>$0080,X:DPSR          ; Clear master abort error bit
                            000080
1821      P:0004FF P:000501 507000            MOVE              A0,X:EC_MAB
                            000039
1822      P:000501 P:000503 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1823      P:000503 P:000505 00000C            RTS
1824                                ERROR_DPER
1825      P:000504 P:000506 50F000            MOVE              X:EC_DPER,A0
                            00003A
1826      P:000506 P:000508 000008            INC     A
1827      P:000507 P:000509 08F48A            MOVEP             #>$0040,X:DPSR          ; Clear data parity error bit
                            000040
1828      P:000509 P:00050B 507000            MOVE              A0,X:EC_DPER
                            00003A
1829      P:00050B P:00050D 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1830      P:00050D P:00050F 00000C            RTS
1831                                ERROR_APER
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 38



1832      P:00050E P:000510 50F000            MOVE              X:EC_APER,A0
                            00003B
1833      P:000510 P:000512 000008            INC     A
1834      P:000511 P:000513 08F48A            MOVEP             #>$0020,X:DPSR          ; Clear address parity error bit
                            000020
1835      P:000513 P:000515 507000            MOVE              A0,X:EC_APER
                            00003B
1836      P:000515 P:000517 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1837      P:000517 P:000519 00000C            RTS
1838   
1839   
1840   
1841                                ;----------------------------------------------
1842                                BLOCK_TRANSFER
1843                                ;----------------------------------------------
1844                                ;   In:
1845                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
1846                                ;   - BLOCK_SIZE is packet size, in bytes
1847                                ;   - YMEM_SRC is start of data in Y memory
1848                                ;  Out:
1849                                ;   - BLOCK_SIZE will be decremented to zero.
1850                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
1851                                ;   - YMEM_SRC will be incremented by BLOCK_SIZE/2
1852                                ;  Trashes:
1853                                ;   - A and B at least
1854   
1855      P:000518 P:00051A 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002B
1856      P:00051A P:00051C 014085            CMP     #0,A                              ; Still bytes to transfer?
1857      P:00051B P:00051D 0AF0A2            JNE     BLOCK_TRANSFER0
                            00051E
1858      P:00051D P:00051F 00000C            RTS
1859   
1860                                BLOCK_TRANSFER0
1861      
1862      
1863      P:00051E P:000520 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000029
1864   
1865      P:000520 P:000522 200005            CMP     B,A                               ; A ? B
1866      P:000521 P:000523 0E1523            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
1867      P:000522 P:000524 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1868                                BLOCK_TRANSFER1
1869      
1870      P:000523 P:000525 200014            SUB     B,A                               ; A -= B
1871      P:000524 P:000526 014088            ADD     #0,B                              ; Clear carry bit
1872      P:000525 P:000527 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002B
1873      P:000527 P:000529 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002A
1874      P:000529 P:00052B 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1875   
1876      
1877      P:00052A P:00052C 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
1878      P:00052C P:00052E 50F000            MOVE              X:YMEM_SRC,A0
                            00002E
1879      P:00052E P:000530 08C82F            MOVEP             A0,X:DSR0               ; DMA source
1880      P:00052F P:000531 200010            ADD     B,A
1881      P:000530 P:000532 00000B            DEC     B
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 39



1882      P:000531 P:000533 507000            MOVE              A0,X:YMEM_SRC           ; BURST_SRC += BURST_SIZE/2
                            00002E
1883   
1884      P:000533 P:000535 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1885   
1886      
1887      P:000534 P:000536 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
1888   
1889                                BLOCK_TRANSFER_PCI
1890      P:000536 P:000538 44F400            MOVE              #>$7,X0                 ; Memory write
                            000007
1891      P:000538 P:00053A 60F400            MOVE              #BURST_DEST_LO,R0       ; RAM address
                            00002F
1892      P:00053A P:00053C 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            00058D
1893   
1894      
1895      P:00053C P:00053E 0A8A84            JCLR    #MARQ,X:DPSR,*
                            00053C
1896   
1897      
1898      P:00053E P:000540 0A8A8E            JCLR    #MDT,X:DPSR,BLOCK_TRANSFER_HANDLE_ERRORS
                            000546
1899   
1900      P:000540 P:000542 20001B            CLR     B
1901      P:000541 P:000543 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            00002A
1902      P:000543 P:000545 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            0006A1
1903      P:000545 P:000547 0C0518            JMP     BLOCK_TRANSFER                    ; Next burst in block
1904   
1905                                BLOCK_TRANSFER_HANDLE_ERRORS
1906      
1907      P:000546 P:000548 0D04BE            JSR     PCI_ERROR_CLEAR
1908   
1909      P:000547 P:000549 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1910      P:000549 P:00054B 0E8536            JCS     BLOCK_TRANSFER_PCI                ; Restart PCI burst
1911   
1912      P:00054A P:00054C 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
1913      P:00054C P:00054E 0E0518            JCC     BLOCK_TRANSFER                    ; Error but no error? Redo this burst.
1914   
1915      
1916      P:00054D P:00054F 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            0005A3
1917      P:00054F P:000551 0BF080            JSR     PCI_UPDATE_R0
                            0005B3
1918      P:000551 P:000553 0C0536            JMP     BLOCK_TRANSFER_PCI
1919   
1920   
1921                                ;----------------------------------------------
1922                                CON_TRANSFER
1923                                ;----------------------------------------------
1924                                ;   In:
1925                                ;   - BURST_SRC_HI:BURST_SRC_LO is PC RAM address
1926                                ;   - BLOCK_SIZE is packet size, in bytes
1927                                ;   - YMEM_DEST is start of data in Y memory
1928                                ;  Out:
1929                                ;   - BLOCK_SIZE will be decremented to zero.
1930                                ;   - BURST_SRC_HI:LO will be incremented by BLOCK_SIZE
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 40



1931                                ;   - YMEM_DEST will be incremented by BLOCK_SIZE/2
1932                                ;  Trashes:
1933                                ;   - A and B, R0, X0
1934   
1935      P:000552 P:000554 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002B
1936      P:000554 P:000556 014085            CMP     #0,A                              ; Still bytes to transfer?
1937      P:000555 P:000557 0AF0A2            JNE     CON_TRANSFER0
                            000558
1938      P:000557 P:000559 00000C            RTS
1939   
1940                                CON_TRANSFER0
1941      
1942      
1943      P:000558 P:00055A 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000029
1944   
1945      P:00055A P:00055C 200005            CMP     B,A                               ; A ? B
1946      P:00055B P:00055D 0E155D            JGE     <CON_TRANSFER1                    ; jump if A >= B
1947      P:00055C P:00055E 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1948                                CON_TRANSFER1
1949      
1950      P:00055D P:00055F 200014            SUB     B,A                               ; A -= B
1951      P:00055E P:000560 014088            ADD     #0,B                              ; Clear carry bit
1952      P:00055F P:000561 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002B
1953      P:000561 P:000563 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002A
1954      P:000563 P:000565 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1955   
1956      
1957      P:000564 P:000566 50F000            MOVE              X:YMEM_DEST,A0
                            000033
1958      P:000566 P:000568 507000            MOVE              A0,X:DDR0               ; DMA dest'n
                            FFFFEE
1959      P:000568 P:00056A 08F4AF            MOVEP             #>DRXR,X:DSR0           ; DMA source
                            FFFFCB
1960      P:00056A P:00056C 200010            ADD     B,A
1961      P:00056B P:00056D 00000B            DEC     B
1962      P:00056C P:00056E 507000            MOVE              A0,X:YMEM_DEST          ; YMEM_DEST += BURST_SIZE/2
                            000033
1963   
1964      P:00056E P:000570 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1965   
1966      
1967      P:00056F P:000571 08F4AC            MOVEP             #$8EEAC4,X:DCR0
                            8EEAC4
1968   
1969                                CON_TRANSFER_PCI
1970      P:000571 P:000573 44F400            MOVE              #>$6,X0                 ; Memory write
                            000006
1971      P:000573 P:000575 60F400            MOVE              #BURST_SRC_LO,R0        ; RAM address
                            000031
1972      P:000575 P:000577 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            00058D
1973   
1974      
1975      P:000577 P:000579 0A8A84            JCLR    #MARQ,X:DPSR,*
                            000577
1976   
1977      
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 41



1978      P:000579 P:00057B 0A8A8E            JCLR    #MDT,X:DPSR,CON_TRANSFER_HANDLE_ERRORS
                            000581
1979   
1980      P:00057B P:00057D 20001B            CLR     B
1981      P:00057C P:00057E 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            00002A
1982      P:00057E P:000580 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            0006A1
1983      P:000580 P:000582 0C0552            JMP     CON_TRANSFER                      ; Next burst in block
1984   
1985                                CON_TRANSFER_HANDLE_ERRORS
1986      
1987      P:000581 P:000583 0D04BE            JSR     PCI_ERROR_CLEAR
1988   
1989      P:000582 P:000584 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1990      P:000584 P:000586 0E8571            JCS     CON_TRANSFER_PCI                  ; Restart PCI burst
1991   
1992      P:000585 P:000587 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
1993      P:000587 P:000589 0E0552            JCC     CON_TRANSFER                      ; Error but no error? Redo this burst.
1994   
1995      
1996      P:000588 P:00058A 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            0005A3
1997      P:00058A P:00058C 0BF080            JSR     PCI_UPDATE_R0
                            0005B3
1998      P:00058C P:00058E 0C0571            JMP     CON_TRANSFER_PCI
1999   
2000                                ; Utility routines for BLOCK_TRANSFER and CON_TRANSFER
2001   
2002                                PCI_GO
2003                                ; Initiate PCI read/write of BURST_SIZE bytes.
2004                                ; R0 must point to the hi-lo PCI address source/dest address
2005                                ; X0 is the PCI command (6 is read, 7 is write).
2006                                ; Trashes A and B but not R0 and X0.
2007      P:00058D P:00058F 200013            CLR     A
2008      P:00058E P:000590 20001B            CLR     B
2009      P:00058F P:000591 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            00002A
2010      P:000591 P:000593 00000B            DEC     B                                 ; n8 - 1
2011      P:000592 P:000594 014088            ADD     #0,B                              ; Clear carry
2012      P:000593 P:000595 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
2013      P:000594 P:000596 014088            ADD     #0,B                              ; Clear carry
2014      P:000595 P:000597 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
2015   
2016      P:000596 P:000598 0200D8            MOVE              X:(R0+1),A0             ; PCI HI address
2017   
2018      P:000597 P:000599 200010            ADD     B,A
2019      P:000598 P:00059A 000000            NOP
2020      P:000599 P:00059B 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
2021   
2022      P:00059B P:00059D 208800            MOVE              X0,A0
2023      P:00059C P:00059E 014088            ADD     #0,B                              ; Clear carry
2024      P:00059D P:00059F 0C1D20            ASL     #16,A,A                           ; Command into bits 19:16
2025      P:00059E P:0005A0 51E000            MOVE              X:(R0),B0
2026      P:00059F P:0005A1 200010            ADD     B,A
2027      P:0005A0 P:0005A2 000000            NOP
2028   
2029      P:0005A1 P:0005A3 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
2030      P:0005A2 P:0005A4 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 42



2031   
2032   
2033                                PCI_RECOVER_COUNT
2034                                ; Calculate number of PCI words not transferred.
2035                                ; Correct BURST_SIZE.  Returns:
2036                                ;   B: bytes not transferred
2037                                ;   A: bytes transferred
2038      P:0005A3 P:0005A5 200013            CLR     A
2039      P:0005A4 P:0005A6 20001B            CLR     B
2040      P:0005A5 P:0005A7 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
2041      P:0005A6 P:0005A8 0A8A8F            JCLR    #RDCQ,X:DPSR,PCI_RECOVER_COUNT1
                            0005A9
2042      P:0005A8 P:0005AA 000009            INC     B
2043                                PCI_RECOVER_COUNT1
2044      P:0005A9 P:0005AB 000009            INC     B                                 ; We want N, not N-1.
2045      P:0005AA P:0005AC 014088            ADD     #0,B                              ; Clear carry
2046      P:0005AB P:0005AD 0C1C20            ASR     #16,A,A
2047      P:0005AC P:0005AE 200018            ADD     A,B                               ; B is words remaining
2048      P:0005AD P:0005AF 014088            ADD     #0,B                              ; Clear carry
2049      P:0005AE P:0005B0 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
2050      P:0005AF P:0005B1 50F000            MOVE              X:BURST_SIZE,A0
                            00002A
2051      P:0005B1 P:0005B3 200014            SUB     B,A                               ; A is bytes written
2052      P:0005B2 P:0005B4 00000C            RTS
2053   
2054   
2055                                PCI_UPDATE_R0
2056                                ;  Subtract A0 from BURST_SIZE and add A to the 32-bit hi-lo address at X:[R0].
2057                                ;  Caller can check Z flag to see if BURST_SIZE is now 0.
2058      P:0005B3 P:0005B5 210500            MOVE              A0,X1                   ; Save A for later
2059      P:0005B4 P:0005B6 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2060      P:0005B5 P:0005B7 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates [R0] = [R0] + B
                            0006A1
2061   
2062      P:0005B7 P:0005B9 57F000            MOVE              X:BURST_SIZE,B
                            00002A
2063      P:0005B9 P:0005BB 20006C            SUB     X1,B                              ; Zero flag must be preserved!
2064      P:0005BA P:0005BC 000000            NOP
2065      P:0005BB P:0005BD 557000            MOVE              B1,X:BURST_SIZE
                            00002A
2066      P:0005BD P:0005BF 00000C            RTS
2067   
2068   
2069                                ;----------------------------------------------;
2070                                ;  MCE PACKET PROCESSING                       ;
2071                                ;----------------------------------------------;
2072   
2073                                ;       Given a dword count in A, computes number of half FIFOs and
2074                                ;       number of left over FIFO reads required to get the whole
2075                                ;       packet.
2076   
2077                                ;       Input: A is packet size, in dwords
2078                                ;       Output: sets X:TOTAL_BUFFS and X:LEFT_TO_READ
2079                                ;       Trashes: A,B,X0
2080   
2081   
2082                                PACKET_PARTITIONS
2083      P:0005BE P:0005C0 507000            MOVE              A0,X:PACKET_SIZE
                            000023
2084   
2085      P:0005C0 P:0005C2 014088            ADD     #0,B                              ; Clear carry
2086      P:0005C1 P:0005C3 0C1D02            ASL     #1,A,A                            ;  * 2
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 43



2087      P:0005C2 P:0005C4 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
2088      P:0005C3 P:0005C5 240000            MOVE              #0,X0
2089      P:0005C4 P:0005C6 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
2090   
2091      P:0005C6 P:0005C8 557000            MOVE              B1,X:TOTAL_BUFFS
                            000024
2092      P:0005C8 P:0005CA 507000            MOVE              A0,X:LEFT_TO_READ
                            000025
2093      P:0005CA P:0005CC 00000C            RTS
2094   
2095   
2096                                ; BUFFER_PACKET
2097                                ;
2098                                ; Copies the packet in the FIFO to Y memory.
2099                                ;
2100                                ; In: TOTAL_BUFFS and LEFT_TO_READ must be pre-set (via PACKET_PARTITIONS);
2101                                ;     R1 is the destination index in Y memory.
2102                                ; Trashes: R1 is updated to point to the end of the copied data.
2103   
2104                                BUFFER_PACKET
2105   
2106      P:0005CB P:0005CD 54F400            MOVE              #>$b00,A1
                            000B00
2107      P:0005CD P:0005CF 0BF080            JSR     TIMER_STORE_A1
                            00064F
2108      P:0005CF P:0005D1 0BF080            JSR     TIMER_STORE
                            00064D
2109   
2110      P:0005D1 P:0005D3 062400            DO      X:TOTAL_BUFFS,BUFFER_PACKET_HALFS_DONE
                            0005DB
2111      P:0005D3 P:0005D5 0BF080            JSR     WAIT_FIFO_HALF
                            0005F8
2112      P:0005D5 P:0005D7 0BF080            JSR     TIMER_STORE
                            00064D
2113      P:0005D7 P:0005D9 0BF080            JSR     BUFFER_PACKET_HALF
                            0005F3
2114      P:0005D9 P:0005DB 0BF080            JSR     TIMER_STORE
                            00064D
2115      P:0005DB P:0005DD 000000            NOP
2116                                BUFFER_PACKET_HALFS_DONE
2117   
2118      
2119      
2120      
2121      
2122      P:0005DC P:0005DE 01AD81            JCLR    #HF,X:PDRD,BUFFER_PACKET_SINGLES_FAST
                            0005EF
2123   
2124      
2125      
2126   
2127                                BUFFER_PACKET_SINGLES
2128      
2129      
2130      P:0005DE P:0005E0 200013            CLR     A
2131      P:0005DF P:0005E1 20001B            CLR     B
2132      P:0005E0 P:0005E2 51F000            MOVE              X:TCR0,B0               ; Store timer value (50 MHz)
                            FFFF8C
2133      P:0005E2 P:0005E4 0C1C85            ASR     #2,B,B                            ; / 4
2134      P:0005E3 P:0005E5 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_DONE
                            0005EB
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 44



2135                                BUFFER_PACKET_SINGLES_WAIT
2136      P:0005E5 P:0005E7 50F000            MOVE              X:TCR0,A0
                            FFFF8C
2137      P:0005E7 P:0005E9 0C1C04            ASR     #2,A,A
2138      P:0005E8 P:0005EA 20000D            CMP     A,B
2139      P:0005E9 P:0005EB 0EA5E5            JEQ     BUFFER_PACKET_SINGLES_WAIT
2140      P:0005EA P:0005EC 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2141      P:0005EB P:0005ED 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2142                                BUFFER_PACKET_SINGLES_DONE
2143      P:0005EC P:0005EE 0BF080            JSR     TIMER_STORE
                            00064D
2144      P:0005EE P:0005F0 00000C            RTS
2145   
2146                                ;---------------------------------------------------------
2147   
2148                                BUFFER_PACKET_SINGLES_FAST
2149      P:0005EF P:0005F1 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_FAST_DONE
                            0005F1
2150      P:0005F1 P:0005F3 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2151                                BUFFER_PACKET_SINGLES_FAST_DONE
2152      P:0005F2 P:0005F4 00000C            RTS
2153   
2154                                ;---------------------------------------------------------
2155                                BUFFER_PACKET_HALF
2156      
2157      P:0005F3 P:0005F5 060082            DO      #512,BUFFER_PACKET_HALF_DONE
                            0005F6
2158      P:0005F5 P:0005F7 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2159      P:0005F6 P:0005F8 000000            NOP
2160                                BUFFER_PACKET_HALF_DONE
2161      P:0005F7 P:0005F9 00000C            RTS
2162   
2163                                ;---------------------------------------------------------
2164                                WAIT_FIFO_HALF
2165      P:0005F8 P:0005FA 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            00060D
2166      P:0005FA P:0005FC 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
                            0005F8
2167      P:0005FC P:0005FE 000000            NOP
2168      P:0005FD P:0005FF 000000            NOP
2169      P:0005FE P:000600 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            0005F8
2170      P:000600 P:000602 00000C            RTS
2171   
2172                                ;---------------------------------------------------------
2173   
2174                                ; This is the old single-buffering routine, which polls the EF.
2175                                BUFFER_PACKET_SINGLES_POLL
2176      P:000601 P:000603 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_DONE
                            00060B
2177                                BUFFER_PACKET_SINGLE
2178      P:000603 P:000605 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D5
2179      P:000605 P:000607 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE
                            000603
2180      P:000607 P:000609 000000            NOP
2181      P:000608 P:00060A 000000            NOP
2182      P:000609 P:00060B 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE   ; Protect against metastability
                            000603
2183      P:00060B P:00060D 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2184                                BUFFER_PACKET_DONE
2185      P:00060C P:00060E 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 45



2186   
2187                                ;---------------------------------------------------------
2188   
2189                                FATALITY_HANDLER
2190      P:00060D P:00060F 0C0100            JMP     START                             ; What could possibly go wrong?
2191   
2192   
2193                                ; DROP_PACKET
2194                                ;
2195                                ; Reads a packet from the fifo, discarding it.
2196                                ;
2197                                ; In: TOTAL_BUFFS & LEFT_TO_READ
2198                                ; Trashes: A0
2199   
2200                                DROP_PACKET
2201      P:00060E P:000610 062400            DO      X:TOTAL_BUFFS,DROP_PACKET_SINGLES
                            000613
2202      P:000610 P:000612 0D05F8            JSR     WAIT_FIFO_HALF
2203      P:000611 P:000613 0BF080            JSR     DROP_FIFO_HALF
                            00061E
2204      P:000613 P:000615 000000            NOP
2205                                DROP_PACKET_SINGLES
2206      P:000614 P:000616 062500            DO      X:LEFT_TO_READ,DROP_PACKET_DONE
                            00061C
2207                                DROP_PACKET_SINGLE
2208      P:000616 P:000618 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D5
2209      P:000618 P:00061A 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE
                            000616
2210      P:00061A P:00061C 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE     ; Protect against metastability
                            000616
2211      P:00061C P:00061E 09483F            MOVEP             Y:RDFIFO,A0
2212                                DROP_PACKET_DONE
2213      P:00061D P:00061F 00000C            RTS
2214   
2215                                DROP_FIFO_HALF
2216      
2217      P:00061E P:000620 060082            DO      #512,DROP_FIFO_DONE
                            000620
2218      P:000620 P:000622 09483F            MOVEP             Y:RDFIFO,A0
2219                                DROP_FIFO_DONE
2220      P:000621 P:000623 00000C            RTS
2221   
2222   
2223                                ;----------------------------------------------;
2224                                ;  TIMER HANDLING                              ;
2225                                ;----------------------------------------------;
2226   
2227                                ; Start value is TLR, count is in TCR, flag marked at TCPR
2228                                ; Must set TCSR[TCIE] to enable int
2229                                ; Must set TCSR[T] for timer to restart
2230   
2231                                TIMER_ENABLE
2232      P:000622 P:000624 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2233      P:000624 P:000626 000000            NOP
2234      P:000625 P:000627 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2235      P:000627 P:000629 00000C            RTS
2236   
2237                                TIMER_DISABLE
2238      P:000628 P:00062A 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 46



                            300200
2239      P:00062A P:00062C 000000            NOP
2240      P:00062B P:00062D 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2241      P:00062D P:00062F 00000C            RTS
2242   
2243                                TIMER_DEFAULT
2244      P:00062E P:000630 0D0628            JSR     TIMER_DISABLE
2245      P:00062F P:000631 44F400            MOVE              #$4C4B40,X0             ; 5M -> 10 Hz.
                            4C4B40
2246      P:000631 P:000633 000000            NOP
2247      P:000632 P:000634 447000            MOVE              X0,X:TCPR0
                            FFFF8D
2248      P:000634 P:000636 0D0622            JSR     TIMER_ENABLE
2249      P:000635 P:000637 00000C            RTS
2250   
2251   
2253                                TIMER_ACTION
2254      P:000636 P:000638 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2255      P:000638 P:00063A 000000            NOP
2256      P:000639 P:00063B 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2257      P:00063B P:00063D 56F000            MOVE              X:QT_INFORM_IDX,A       ; QT inform time?
                            000046
2258      P:00063D P:00063F 0A0182            JCLR    #MODE_QT,X:MODE,TIMER_ACTION_OK
                            000645
2259      P:00063F P:000641 0140C5            CMP     #>0,A                             ; If inform_idx != 0
                            000000
2260      P:000641 P:000643 0AF0AA            JEQ     TIMER_ACTION_OK
                            000645
2261      P:000643 P:000645 0A7034            BSET    #QT_FLUSH,X:STATUS                ;    schedule inform
                            000000
2262                                TIMER_ACTION_OK
2263      P:000645 P:000647 00000C            RTS
2264   
2265   
2266                                ;----------------------------------------------;
2267                                ;  TIMER UTILITY                               ;
2268                                ;----------------------------------------------;
2269   
2270                                 TIMER_SOURCE
2271      FFFF8C                              EQU     TCR0
2272   
2273                                TIMER_STORE_INIT
2274      P:000646 P:000648 50F400            MOVE              #>TIMER_BUFFER,A0
                            201000
2275      P:000648 P:00064A 000000            NOP
2276      P:000649 P:00064B 507000            MOVE              A0,X:TIMER_INDEX
                            00004C
2277      P:00064B P:00064D 211400            MOVE              A0,R4
2278      P:00064C P:00064E 00000C            RTS
2279   
2280                                TIMER_STORE
2281      
2282      
2283      P:00064D P:00064F 56F000            MOVE              X:TIMER_SOURCE,A
                            FFFF8C
2284                                                                                    ; Fall-through
2285   
2286                                TIMER_STORE_A1
2287      
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 47



2288      P:00064F P:000651 5C5C00            MOVE                          A1,Y:(R4)+
2289      P:000650 P:000652 228C00            MOVE              R4,A1
2290      P:000651 P:000653 0140C5            CMP     #>TIMER_BUFFER_END,A
                            300000
2291      P:000653 P:000655 547000            MOVE              A1,X:TIMER_INDEX
                            00004C
2292      P:000655 P:000657 0E1646            JGE     TIMER_STORE_INIT
2293      P:000656 P:000658 00000C            RTS
2294   
2295   
2296                                ;----------------------------------------------;
2297                                ;  CIRCULAR BUFFER HANDLING                    ;
2298                                ;----------------------------------------------;
2299   
2300                                BUFFER_INCR
2301   
2302      P:000657 P:000659 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
                            000042
2303      P:000659 P:00065B 014180            ADD     #1,A                              ;
2304      P:00065A P:00065C 57F000            MOVE              X:QT_BUF_MAX,B          ;
                            00003F
2305      P:00065C P:00065E 20000D            CMP     A,B                               ;
2306      P:00065D P:00065F 0AF0AF            JLE     BUFFER_RESET                      ;       head = 0
                            000669
2307                                                                                    ; else
2308      P:00065F P:000661 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            000042
2309   
2310      P:000661 P:000663 20001B            CLR     B
2311      P:000662 P:000664 51F000            MOVE              X:QT_BUF_SIZE,B0
                            00003E
2312      P:000664 P:000666 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2313      P:000666 P:000668 0BF080            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
                            0006A1
2314   
2315      P:000668 P:00066A 00000C            RTS
2316   
2317   
2318                                BUFFER_RESET
2319      P:000669 P:00066B 60F400            MOVE              #QT_BASE_LO,R0
                            00003C
2320      P:00066B P:00066D 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
2321      P:00066D P:00066F 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2322      P:00066F P:000671 0BF080            JSR     SAVE_HILO_ADDRESS                 ; QT_DEST_LO = QT_BASE_LO
                            0006A3
2323   
2324      P:000671 P:000673 240000            MOVE              #0,X0
2325      P:000672 P:000674 447000            MOVE              X0,X:QT_BUF_HEAD        ; HEAD = 0
                            000042
2326      P:000674 P:000676 00000C            RTS
2327   
2328   
2329                                BUFFER_INFORM_CHECK
2330      P:000675 P:000677 56F000            MOVE              X:QT_INFORM_IDX,A
                            000046
2331      P:000677 P:000679 014180            ADD     #1,A
2332      P:000678 P:00067A 57F000            MOVE              X:QT_INFORM,B
                            000041
2333      P:00067A P:00067C 20000D            CMP     A,B
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 48



2334      P:00067B P:00067D 0AF0A7            JGT     BUFFER_INFORM_OK                  ; If inform_idx + 1 <= inform
                            00067F
2335      P:00067D P:00067F 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2336   
2337                                BUFFER_INFORM_OK
2338      P:00067F P:000681 567000            MOVE              A,X:QT_INFORM_IDX       ; inform_idx = inform_idx + 1
                            000046
2339      P:000681 P:000683 00000C            RTS
2340   
2341   
2342                                ;---------------------------------------------------------------
2343                                BUFFER_INFORM
2344                                ;---------------------------------------------------------------
2345                                ; Informs host of current buffer status
2346   
2347      
2348      P:000682 P:000684 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            00069A
2349      P:000684 P:000686 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            00069A
2350   
2351                                          PCI_LOCKDOWN                              ; Disable host IRQ
2353   
2354      P:000687 P:000689 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2355      P:000689 P:00068B 440B00            MOVE              X0,X:<DTXS_WD1
2356   
2357      P:00068A P:00068C 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            000042
2358      P:00068C P:00068E 440C00            MOVE              X0,X:<DTXS_WD2
2359   
2360      P:00068D P:00068F 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000043
2361      P:00068F P:000691 440D00            MOVE              X0,X:<DTXS_WD3
2362   
2363      P:000690 P:000692 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
                            000047
2364      P:000692 P:000694 440E00            MOVE              X0,X:<DTXS_WD4
2365   
2366      P:000693 P:000695 0D047A            JSR     PCI_MESSAGE_TO_HOST
2367   
2368      P:000694 P:000696 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
2369      P:000696 P:000698 240000            MOVE              #0,X0                   ; Reset inform index
2370      P:000697 P:000699 447000            MOVE              X0,X:QT_INFORM_IDX
                            000046
2371                                          PCI_LOCKUP                                ; Enable host IRQ
2373                                INFORM_EXIT
2374      P:00069A P:00069C 00000C            RTS
2375   
2376   
2377   
2378                                ;----------------------------------------------;
2379                                ;  ADDRESS HANDLING                            ;
2380                                ;----------------------------------------------;
2381   
2385   
2386                                LOAD_HILO_ADDRESS
2387      
2388      
2389      P:00069B P:00069D 200013            CLR     A
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  main.asm  Page 49



2390      P:00069C P:00069E 50D800            MOVE              X:(R0)+,A0
2391      P:00069D P:00069F 44D000            MOVE              X:(R0)-,X0
2392      P:00069E P:0006A0 0C1940            INSERT  #$010010,X0,A
                            010010
2393      P:0006A0 P:0006A2 00000C            RTS
2394   
2395                                ADD_HILO_ADDRESS
2396      
2397      
2398   
2399      P:0006A1 P:0006A3 0D069B            JSR     LOAD_HILO_ADDRESS
2400      P:0006A2 P:0006A4 200010            ADD     B,A
2401   
2402                                SAVE_HILO_ADDRESS
2403      
2404      
2405   
2406      P:0006A3 P:0006A5 445800            MOVE              X0,X:(R0)+              ; pre-increment
2407      P:0006A4 P:0006A6 240000            MOVE              #0,X0
2408      P:0006A5 P:0006A7 0C1D11            ASL     #8,A,B
2409      P:0006A6 P:0006A8 0C1940            INSERT  #$008010,X0,A
                            008010
2410      P:0006A8 P:0006AA 555000            MOVE              B1,X:(R0)-              ; store hi16
2411      P:0006A9 P:0006AB 506000            MOVE              A0,X:(R0)
2412      P:0006AA P:0006AC 0C1C90            ASR     #8,B,A
2413      P:0006AB P:0006AD 00000C            RTS
2414   
2415   
2416   
2417   
2418                                BOOTCODE_END
2419                                 BOOTEND_ADDR
2420      0006AC                              EQU     @CVI(BOOTCODE_END)
2421   
2422                                PROGRAM_END
2423      0006AC                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2424                                          INCLUDE 'vars.asm'
2425                                      COMMENT *
2426   
2427                                Variable table and bit defines for our variables.
2428   
2429                                See info.asm for versioning and authors.
2430   
2431                                        *
2432   
2433   
2434                                ; The variable table is mapped to X memory but stored inline in the
2435                                ; eeprom / P memory after the main code (but before the application
2436                                ; area).
2437   
2438      X:000000 P:0006AE                   ORG     X:VAR_TBL,P:
2439   
2440   
2441                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2442                                 VAR_TBL_START
2443      0006AC                              EQU     @LCV(L)-2
2444                                          ENDIF
2445   
2446                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2448                                          ENDIF
2449   
2450   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  vars.asm  Page 50



2451                                ;-----------------------------------------------
2452 d    X:000000 P:0006AE 000000  STATUS    DC      0                                 ; Internal status flags
2453 d    X:000001 P:0006AF 000000  MODE      DC      0                                 ; Operating mode control
2454 d                               FRAME_COUNT
2455 d    X:000002 P:0006B0 000000            DC      0                                 ; Count of data frames from MCE
2456   
2457                                ;-----------------------------------------------
2458 d    X:000003 P:0006B1 550106  REV_NUMBER DC     $550106                           ; byte 0 = minor revision #
2459                                                                                    ; byte 1 = major revision #
2460                                                                                    ; byte 2 = release Version (ascii letter)
2461 d    X:000004 P:0006B2 000000  REV_DATA  DC      $000000                           ; Not used by UBC
2462 d    X:000005 P:0006B3 2EF490  P_CHECKSUM DC     $2EF490                           ; Not used by UBC
2463   
2464                                ;-----------------------------------------------
2465 d    X:000006 P:0006B4 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2466                                ;-----------------------------------------------
2467   
2468   
2470   
2471 d    X:000007 P:0006B5 000000  DRXR_WD1  DC      0                                 ; Storage for words read from PC during vect
or command
2472 d    X:000008 P:0006B6 000000  DRXR_WD2  DC      0
2473 d    X:000009 P:0006B7 000000  DRXR_WD3  DC      0
2474 d    X:00000A P:0006B8 000000  DRXR_WD4  DC      0
2475   
2476 d    X:00000B P:0006B9 000000  DTXS_WD1  DC      0                                 ; Storage for words to be written to PC as r
eply
2477 d    X:00000C P:0006BA 000000  DTXS_WD2  DC      0
2478 d    X:00000D P:0006BB 000000  DTXS_WD3  DC      0
2479 d    X:00000E P:0006BC 000000  DTXS_WD4  DC      0
2480   
2481   
2483   
2484 d    X:00000F P:0006BD 000000  SV_A0     DC      0
2485 d    X:000010 P:0006BE 000000  SV_A1     DC      0
2486 d    X:000011 P:0006BF 000000  SV_A2     DC      0
2487 d    X:000012 P:0006C0 000000  SV_B0     DC      0
2488 d    X:000013 P:0006C1 000000  SV_B1     DC      0
2489 d    X:000014 P:0006C2 000000  SV_B2     DC      0
2490 d    X:000015 P:0006C3 000000  SV_X0     DC      0
2491 d    X:000016 P:0006C4 000000  SV_X1     DC      0
2492 d    X:000017 P:0006C5 000000  SV_Y0     DC      0
2493 d    X:000018 P:0006C6 000000  SV_Y1     DC      0
2494 d    X:000019 P:0006C7 000000  SV_R0     DC      0
2495 d    X:00001A P:0006C8 000000  SV_SR     DC      0
2496   
2497   
2499   
2500 d    X:00001B P:0006C9 000000  HEAD_W1_0 DC      0                                 ; Preamble $A5A5
2501 d    X:00001C P:0006CA 000000  HEAD_W1_1 DC      0                                 ;          $A5A5
2502 d    X:00001D P:0006CB 000000  HEAD_W2_0 DC      0                                 ;          $5A5A
2503 d    X:00001E P:0006CC 000000  HEAD_W2_1 DC      0                                 ;          $5A5A
2504 d    X:00001F P:0006CD 000000  HEAD_W3_0 DC      0                                 ; 'RP' or 'DA'
2505 d    X:000020 P:0006CE 000000  HEAD_W3_1 DC      0                                 ; '  '   $2020
2506 d    X:000021 P:0006CF 000000  HEAD_W4_0 DC      0                                 ; Packet size LSW
2507 d    X:000022 P:0006D0 000000  HEAD_W4_1 DC      0                                 ;             MSW
2508   
2509   
2511   
2512 d                               PACKET_SIZE
2513 d    X:000023 P:0006D1 000000            DC      0                                 ; Size, in dwords of most recent packet from
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  vars.asm  Page 51



 MCE.
2514 d                               TOTAL_BUFFS
2515 d    X:000024 P:0006D2 000000            DC      0                                 ; Number of 512 word half-buffers in packet.
2516 d                               LEFT_TO_READ
2517 d    X:000025 P:0006D3 000000            DC      0                                 ; Number of words left to read after last 51
2 buffer
2518   
2519 d                               PREAMBLE_ERRORS
2520 d    X:000026 P:0006D4 000000            DC      0                                 ; Failed on preamble processing
2521 d                               PTYPE_ERRORS
2522 d    X:000027 P:0006D5 000000            DC      0                                 ; Failed on packet type
2523 d                               PSIZE_ERRORS
2524 d    X:000028 P:0006D6 000000            DC      0                                 ; Failed on packet size test
2525   
2526   
2528   
2529 d                               PCI_BURST_SIZE
2530 d    X:000029 P:0006D7 000040            DC      $40                               ; Should be < 4*latency assigned by OS
2531 d    X:00002A P:0006D8 000000  BURST_SIZE DC     0
2532 d    X:00002B P:0006D9 000000  BLOCK_SIZE DC     0
2533   
2534 d    X:00002C P:0006DA 000000  CON_SRC_LO DC     0                                 ; Set by CON host command
2535 d    X:00002D P:0006DB 000000  CON_SRC_HI DC     0
2536   
2537 d    X:00002E P:0006DC 000000  YMEM_SRC  DC      0                                 ; Vars for YMEM -> PC transfers
2538 d                               BURST_DEST_LO
2539 d    X:00002F P:0006DD 000000            DC      0
2540 d                               BURST_DEST_HI
2541 d    X:000030 P:0006DE 000000            DC      0
2542   
2543 d                               BURST_SRC_LO
2544 d    X:000031 P:0006DF 000000            DC      0                                 ; Vars for PC -> YMEM transfers
2545 d                               BURST_SRC_HI
2546 d    X:000032 P:0006E0 000000            DC      0
2547 d    X:000033 P:0006E1 000000  YMEM_DEST DC      0
2548   
2549 d    X:000034 P:0006E2 000000  DMA_ERRORS DC     0                                 ; Error counting
2550 d    X:000035 P:0006E3 000000  EC_TRTY   DC      0
2551 d    X:000036 P:0006E4 000000  EC_TO     DC      0
2552 d    X:000037 P:0006E5 000000  EC_TDIS   DC      0
2553 d    X:000038 P:0006E6 000000  EC_TAB    DC      0
2554 d    X:000039 P:0006E7 000000  EC_MAB    DC      0
2555 d    X:00003A P:0006E8 000000  EC_DPER   DC      0
2556 d    X:00003B P:0006E9 000000  EC_APER   DC      0
2557   
2558   
2560   
2561 d    X:00003C P:0006EA 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2562 d    X:00003D P:0006EB 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2563 d                               QT_BUF_SIZE
2564 d    X:00003E P:0006EC 000000            DC      0                                 ; Separation of buffers, in bytes
2565 d    X:00003F P:0006ED 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2566 d                               QT_FRAME_SIZE
2567 d    X:000040 P:0006EE 000000            DC      0                                 ; Expected data packet size, in bytes
2568 d    X:000041 P:0006EF 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2569   
2570 d                               QT_BUF_HEAD
2571 d    X:000042 P:0006F0 000000            DC      0                                 ; Index of buf for next write
2572 d                               QT_BUF_TAIL
2573 d    X:000043 P:0006F1 000000            DC      0                                 ; Index at which we must not write
2574   
2575 d    X:000044 P:0006F2 000000  QT_DEST_LO DC     0                                 ; PC address for next write
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  vars.asm  Page 52



2576 d    X:000045 P:0006F3 000000  QT_DEST_HI DC     0                                 ;
2577 d                               QT_INFORM_IDX
2578 d    X:000046 P:0006F4 000000            DC      0                                 ; Number of packets since last inform
2579 d    X:000047 P:0006F5 000000  QT_DROPS  DC      0                                 ; Dropped packet count
2580   
2581   
2583   
2584 d    X:000048 P:0006F6 000000  RP_BASE_LO DC     0                                 ; PC buffer start address
2585 d    X:000049 P:0006F7 000000  RP_BASE_HI DC     0                                 ;
2586 d                               RP_MAX_SIZE
2587 d    X:00004A P:0006F8 000000            DC      0                                 ; Maximum reply size, dwords
2588 d    X:00004B P:0006F9 000000  RP_DROPS  DC      0                                 ; Dropped packet count
2589   
2590   
2592   
2593 d                               TIMER_INDEX
2594 d    X:00004C P:0006FA 000000            DC      0
2595   
2596   
2598   
2599 d    X:00004D P:0006FB 000000  BDEBUG0   DC      0
2600 d    X:00004E P:0006FC 000000  BDEBUG1   DC      0
2601 d    X:00004F P:0006FD 000000  BDEBUG2   DC      0
2602 d    X:000050 P:0006FE 000000  BDEBUG3   DC      0
2603 d    X:000051 P:0006FF 000000  BDEBUG4   DC      0
2604 d    X:000052 P:000700 000000  BDEBUG5   DC      0
2605 d    X:000053 P:000701 000000  BDEBUG6   DC      0
2606 d    X:000054 P:000702 000000  BDEBUG7   DC      0
2607 d    X:000055 P:000703 000000  BDEBUG8   DC      0
2608 d    X:000056 P:000704 000000  BDEBUG9   DC      0
2609   
2610   
2614   
2616      000004                    COMM_REP  EQU     4                                 ; Reply needs to be sent
2617      000005                    COMM_CMD  EQU     5                                 ; Command needs to be processed
2618                                 COMM_MCEREP
2619      000006                              EQU     6                                 ; MCE reply has been buffered for send to ho
st
2620                                 COMM_MCEDATA
2621      000007                              EQU     7                                 ; MCE data " "
2622      000008                    COMM_ERR  EQU     8                                 ; Command not recognized or whatever
2623                                 COMM_REP_ENABLED
2624      00000C                              EQU     12                                ;
2625                                 COMM_BUF_UPDATE
2626      00000D                              EQU     13                                ; Data has been written to buffer
2627                                 COMM_TFR_YMEM
2628      00000E                              EQU     14                                ; PCI burst is coming from Y mem, not X mem.
2629   
2630   
2631 d    X:000057 P:000705 000000  FIFO_FAILS DC     0
2632 d                               PTYPE_FAILS
2633 d    X:000058 P:000706 000000            DC      0
2634 d    X:000059 P:000707 000000  DA_COUNT  DC      0
2635   
2637 d    X:00005A P:000708 000000  CMD_SIZE  DC      0
2638 d    X:00005B P:000709 000000  CMD_WORD  DC      0
2639   
2640 d                               REP_BUS_ADDR
2641 d    X:00005C P:00070A 000000            DC      0,0
     d                      000000
2642   
2643 d    X:00005E P:00070C 000000  MEM_SRC   DC      0
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  vars.asm  Page 53



2644   
2645 d                               INT_DEBUG_BUF_IDX
2646 d    X:00005F P:00070D 000000            DC      0
2647 d                               DEBUG_BUF_IDX
2648 d    X:000060 P:00070E 000000            DC      0
2649   
2650                                 QT_MAX_BLOCKS
2651      000014                              EQU     20
2652                                ; struct QT_DATA:
2653                                 QT_BLOCK__ADDR
2654      000000                              EQU     0
2655                                 QT_BLOCK__END_IDX
2656      000002                              EQU     2
2657                                 QT_BLOCK___SIZE
2658      000003                              EQU     3
2659 d    X:000061 P:00070F 000000  QT_N_BLOCK DC     0
2660 d                               QT_BLOCK_PTR
2661 d    X:000062 P:000710 000000            DC      0
2662      X:000063 P:000711         QT_BLOCKS DS      (QT_MAX_BLOCKS*QT_BLOCK___SIZE)
2663   
2664   
2666                                 MCEHDR_PREAMBLE
2667      X:00009F P:00074D                   DS      (4)
2668                                 MCEHDR_TYPE
2669      X:0000A3 P:000751                   DS      (2)
2670                                 MCEHDR_SIZE
2671      X:0000A5 P:000753                   DS      (2)
2672      00009F                    MCEHDR    EQU     MCEHDR_PREAMBLE
2673   
2675                                 MCEREP__TYPE
2676      000000                              EQU     0
2677                                 MCEREP__SIZE
2678      000002                              EQU     2
2679                                 MCEREP__PAYLOAD
2680      000004                              EQU     4
2681                                 MCEREP___SIZE
2682      000084                              EQU     MCEREP__PAYLOAD+128
2683   
2684   
2688      X:0000A7 P:000755         CMD_BUFFER DS     (256)
2689   
2690   
2696   
2697      0000A0                    DG__SIZE  EQU     128+32                            ; This MUST be even, so that effective numbe
r
2698                                                                                    ; of 32-bit words is integral
2699   
2700                                 DG_VERS_CODE
2701      000001                              EQU     1                                 ; Datagram protocol version
2702                                 DG_TYPE_DSP_REP
2703      000001                              EQU     1                                 ; DSP reply     | Packet type codes
2704                                 DG_TYPE_MCE_REP
2705      000002                              EQU     2                                 ; MCE reply     |
2706                                 DG_TYPE_BUF_INF
2707      000003                              EQU     3                                 ; Buffer status |
2708   
2710                                 DGRAM_BUFFER
2711      X:0001A7 P:000855                   DS      DG__SIZE
2712   
2714                                 DGRAM_VERSION
2715      0001A7                              EQU     DGRAM_BUFFER+0                    ; Datagram protocol version
2716      0001A8                    DGRAM_SIZE EQU    DGRAM_BUFFER+1                    ; Datagram payload size in 32-bit words
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  vars.asm  Page 54



2717      0001A9                    DGRAM_TYPE EQU    DGRAM_BUFFER+2                    ; Datagram type
2718                                 DGRAM_FWREV
2719      0001AB                              EQU     DGRAM_BUFFER+4                    ; FW rev.
2720      0001B7                    DGRAM_DATA EQU    DGRAM_BUFFER+16                   ; Start of DSP or MCE reply data
2721                                 DGRAM__HEADER_SIZE
2722      000010                              EQU     (DGRAM_DATA-DGRAM_VERSION)        ; Whatever
2723   
2725      0001B7                    REP_RSTAT EQU     DGRAM_DATA+0
2726      0001B8                    REP_RSIZE EQU     DGRAM_DATA+1
2727      0001B9                    REP_RCMD  EQU     DGRAM_DATA+2
2728                                 REP_RPAYLOAD
2729      0001BB                              EQU     DGRAM_DATA+4
2730      0001CB                    REP_REND  EQU     REP_RPAYLOAD+16                   ; Whatever.
2731   
2732   
2735                                 RB_REP_SIZE
2736      000024                              EQU     (REP_REND-DGRAM_DATA+DGRAM__HEADER_SIZE)
2737                                 RB_MCE_SIZE
2738      000088                              EQU     (8+128)
2739                                 RB_INF_SIZE
2740      000016                              EQU     (REP_REND-DGRAM_DATA+2)
2741   
2742   
2743                                ;
2744                                ; Buffer locations, Y memory.
2745                                ;
2746      000000                    MCECMD_BUF EQU    $0
2747   
2748      000100                    MCEREP_BUF EQU    $100                              ; Y-mem location for mce reply buffer?
2749   
2750                                 MCEDATA_BUF
2751      000200                              EQU     $200
2752   
2753                                 MCE_PACKET_DUMP
2754      002000                              EQU     $2000
2755   
2756      008000                    DEBUG_BUF EQU     $8000
2757   
2758   
2759                                ;
2760                                ; Commands for U0107 comms protocol
2761                                ;
2762   
2763      000001                    CMD_READ_P EQU    1
2764      000002                    CMD_READ_X EQU    2
2765      000003                    CMD_READ_Y EQU    3
2766   
2767                                 CMD_WRITE_P
2768      000005                              EQU     5
2769                                 CMD_WRITE_X
2770      000006                              EQU     6
2771                                 CMD_WRITE_Y
2772      000007                              EQU     7
2773   
2774                                 CMD_SET_REP_BUF
2775      000009                              EQU     9
2776                                 CMD_SET_DATA_BUF_MULTI
2777      00000B                              EQU     $B
2778   
2779                                 CMD_SET_TAIL_INF
2780      000012                              EQU     $12
2781   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  vars.asm  Page 55



2782                                 CMD_SEND_MCE
2783      000021                              EQU     $21
2784                                 CMD_POST_MCE
2785      000022                              EQU     $22
2786   
2787   
2788   
2789   
2790                                ;----------------------------------------------------------
2791   
2793   
2794                                 APPLICATION_RUNNING
2795      000000                              EQU     0                                 ; Indicates application is in progress
2796                                 SEND_TO_HOST
2797      000001                              EQU     1                                 ; set in HST ISR when host ready for packet 
(stays set until after HST reply)
2798                                 FATAL_ERROR
2799      000002                              EQU     2                                 ; PCI message to host error detected by driv
er....
2800      000003                    FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays s
et till packet processed
2801   
2802      000009                    HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of p
acket (stays set until after HST reply)
2803   
2804      00000A                    CON_DEMAND EQU    10                                ; Host has requested an MCE command be sent
2805      00000B                    CON_MCE   EQU     11                                ; Command has been copied to Y buffer and sh
ould be sent to MCE
2806   
2807                                 PCIDMA_RESTART
2808      000010                              EQU     16                                ; DMA flags used for error recovery
2809                                 PCIDMA_RESUME
2810      000011                              EQU     17
2811   
2812      000014                    QT_FLUSH  EQU     20                                ; Set when it is time to inform Host of curr
ent buffer position.
2813                                 RP_BUFFER_FULL
2814      000015                              EQU     21                                ; Set when Quiet RP buffer is occupied.
2815   
2816      000016                    FREEZER   EQU     22                                ; Suspend operations and just idle in the ma
in loop
2817                                 MAIN_LOOP_POLL
2818      000017                              EQU     23                                ; Cleared by the main loop, use to check for
 DSP lock-up
2819   
2820   
2822   
2823                                 MODE_APPLICATION
2824      000000                              EQU     0                                 ; set if PCI application to run
2825      000001                    MODE_MCE  EQU     1                                 ; process packets from MCE (!choke)
2826      000002                    MODE_QT   EQU     2                                 ; Quiet transfer for data packets (QT mode)
2827                                 MODE_RP_BUFFER
2828      000003                              EQU     3                                 ; Quiet transfer for reply packets (Quiet-RP
)
2829   
2830   
2832   
2833                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2834                                 VAR_TBL_END
2835      0008F3                              EQU     @LCV(L)-2
2836                                          ENDIF
2837   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  vars.asm  Page 56



2838                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2840                                          ENDIF
2841   
2842                                 VAR_TBL_LENGTH
2843      000247                              EQU     VAR_TBL_END-VAR_TBL_START
2844                                          INCLUDE 'app.asm'
2845                                        COMMENT *
2846   
2847                                Auxiliary application area.
2848   
2849                                See info.asm for versioning and authors.
2850   
2851                                        *
2852                                          PAGE    132                               ; Printronix page width - 132 columns
2853                                          OPT     CEX                               ; print DC evaluations
2854   
2855                                          IF      @CVS(N,*)>=APPLICATION
2857                                          ENDIF
2858   
2859   
2860                                ;--------------------------------------------
2861                                ; APPLICATION AREA
2862                                ;---------------------------------------------
2863                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2864      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2865                                          ENDIF
2866   
2867                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2869                                          ENDIF
2870   
2871                                ; starts with no application loaded
2872                                ; so just reply with an error if we get a GOA command
2873   
2874      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2875      P:000802 P:000804 440B00            MOVE              X0,X:<DTXS_WD1          ; REPly
2876      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2877      P:000805 P:000807 440C00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2878      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2879      P:000808 P:00080A 440D00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2880      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2881      P:00080B P:00080D 440E00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2882      P:00080C P:00080E 0D0493            JSR     <RESTORE_REGISTERS
2883      P:00080D P:00080F 0D047A            JSR     <PCI_MESSAGE_TO_HOST
2884      P:00080E P:000810 0A0100            BCLR    #MODE_APPLICATION,X:<MODE
2885      P:00080F P:000811 0C016E            JMP     PACKET_IN
2886   
2887   
2888   
2889                                          INCLUDE 'hacking.asm'
2890                                                COMMENT *
2891   
2892                                        This implementation does communication with the host using PCI
2893                                        master writes only.
2894   
2895                                        *
2896                                          PAGE    132                               ; Printronix page width - 132 columns
2897                                          OPT     CEX                               ; print DC evaluations
2898   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 57



2899                                ;----------------------------------------------
2900                                ; NEW_COMMS_ENTRY
2901                                ;----------------------------------------------
2902   
2903                                NEW_COMMS_INIT
2904      
2905      P:000810 P:000812 0BF080            JSR     RESET_FIFO
                            000B76
2906   
2907      
2908      P:000812 P:000814 0A0005            BCLR    #COMM_CMD,X:STATUS
2909      P:000813 P:000815 0A0004            BCLR    #COMM_REP,X:STATUS
2910      P:000814 P:000816 0A0006            BCLR    #COMM_MCEREP,X:STATUS
2911      P:000815 P:000817 0A0007            BCLR    #COMM_MCEDATA,X:STATUS
2912      P:000816 P:000818 0A0008            BCLR    #COMM_ERR,X:STATUS
2913      P:000817 P:000819 0A000C            BCLR    #COMM_REP_ENABLED,X:STATUS
2914      P:000818 P:00081A 0A000D            BCLR    #COMM_BUF_UPDATE,X:STATUS
2915   
2916      
2917      P:000819 P:00081B 0BF080            JSR     INIT_DATAGRAM_BUFFER
                            0008D1
2918   
2919      P:00081B P:00081D 0BF080            JSR     TIMERX_STORE_INIT
                            00083A
2920   
2921      
2922      P:00081D P:00081F 0A8522            BSET    #DCTR_SRIE,X:DCTR
2923   
2924      
2925      P:00081E P:000820 0A8524            BSET    #DCTR_HF4,X:DCTR
2926   
2927                                ;
2928                                ; Main loop
2929                                ;
2930                                NEW_COMMS_MAIN_LOOP
2931      
2932      P:00081F P:000821 0B00A5            JSSET   #COMM_CMD,X:STATUS,PROCESS_PC_CMD
                            0009DF
2933   
2934      
2935      P:000821 P:000823 0B00A4            JSSET   #COMM_REP,X:STATUS,PROCESS_REPLY
                            000904
2936   
2937      
2938      P:000823 P:000825 0BF080            JSR     CHECK_FOR_DATA
                            000AE2
2939   
2940      
2941      P:000825 P:000827 0B00A6            JSSET   #COMM_MCEREP,X:STATUS,PROCESS_MCE_REPLY
                            000910
2942      P:000827 P:000829 0B00A7            JSSET   #COMM_MCEDATA,X:STATUS,PROCESS_MCE_DATA
                            000931
2943   
2944      
2945      P:000829 P:00082B 01CFB5            JSSET   #TCF,X:TCSR0,TIMERX_ACTION
                            000833
2946   
2947      
2948      P:00082B P:00082D 0B00B4            JSSET   #QT_FLUSH,X:STATUS,SEND_BUF_INFO
                            00099C
2949   
2950      
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 58



2951      P:00082D P:00082F 0A89A5            JSET    #DSR_HF2,X:DSR,NEW_COMMS_MAIN_LOOP
                            00081F
2952   
2953   
2954                                ;
2955                                ; Cleanup and exit to standard loop.
2956                                ;
2957      
2958      P:00082F P:000831 0A0014            BCLR    #QT_FLUSH,X:STATUS
2959   
2960      
2961      P:000830 P:000832 0A8502            BCLR    #DCTR_SRIE,X:DCTR
2962   
2963      
2964      P:000831 P:000833 0A8504            BCLR    #DCTR_HF4,X:DCTR
2965      P:000832 P:000834 00000C            RTS
2966   
2967   
2968   
2969                                ;----------------------------------------------
2970                                ; Timer tick handler and buffer management
2971                                ;----------------------------------------------
2972   
2973                                TIMERX_ACTION
2974      P:000833 P:000835 07F40F            MOVEP             #$300201,X:TCSR0        ; Clear TOF, TCF, leave timer enabled.
                            300201
2975      
2976      P:000835 P:000837 0A000D            BCLR    #COMM_BUF_UPDATE,X:STATUS
2977      P:000836 P:000838 0AF0A0            JCC     TIMERX_ACTION_OK
                            000839
2978      P:000838 P:00083A 0A0034            BSET    #QT_FLUSH,X:STATUS                ;    schedule inform
2979                                TIMERX_ACTION_OK
2980      P:000839 P:00083B 00000C            RTS
2981   
2982   
2983                                ;----------------------------------------------
2984                                TIMERX_STORE_INIT
2985                                ; Set buffer pointer to start of TIMER_BUFFER.
2986      P:00083A P:00083C 50F400            MOVE              #>TIMER_BUFFER,A0
                            201000
2987      P:00083C P:00083E 000000            NOP
2988      P:00083D P:00083F 507000            MOVE              A0,X:TIMER_INDEX
                            00004C
2989      P:00083F P:000841 00000C            RTS
2990   
2991                                ;----------------------------------------------
2992                                TIMERX_STORE_RAW
2993                                ; Write current timer value to timer buffer.  Trashes A, R5.
2994                                ; No interrupt protection.
2995      P:000840 P:000842 56F000            MOVE              X:TIMER_SOURCE,A
                            FFFF8C
2996                                ; Fall-through!
2997                                ;----------------------------------------------
2998                                TIMERX_STORE_A1_RAW
2999                                ; Write A1 to to timer buffer. Trashes A, R5.
3000      P:000842 P:000844 65F000            MOVE              X:TIMER_INDEX,R5
                            00004C
3001      P:000844 P:000846 000000            NOP
3002      P:000845 P:000847 000000            NOP
3003      P:000846 P:000848 5C5D00            MOVE                          A1,Y:(R5)+
3004      P:000847 P:000849 22AE00            MOVE              R5,A
3005      P:000848 P:00084A 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 59



3006      P:000849 P:00084B 0140C5            CMP     #>(TIMER_BUFFER_END-1),A
                            2FFFFF
3007      P:00084B P:00084D 0E183A            JGE     TIMERX_STORE_INIT
3008      P:00084C P:00084E 547000            MOVE              A1,X:TIMER_INDEX
                            00004C
3009      P:00084E P:000850 00000C            RTS
3010   
3011                                ;----------------------------------------------
3012                                TIMERX_STORE
3013                                ; Interrupt-protected version of TIMERX_STORE_RAW.
3014      P:00084F P:000851 0A8502            BCLR    #DCTR_SRIE,X:DCTR
3015      P:000850 P:000852 0D0840            JSR     TIMERX_STORE_RAW
3016      P:000851 P:000853 0A8522            BSET    #DCTR_SRIE,X:DCTR
3017      P:000852 P:000854 00000C            RTS
3018   
3019                                ;----------------------------------------------
3020                                TIMERX_STORE_A1
3021                                ; Interrupt-protected version of TIMERX_STORE_A1_RAW.
3022      P:000853 P:000855 0A8502            BCLR    #DCTR_SRIE,X:DCTR
3023      P:000854 P:000856 0D0842            JSR     TIMERX_STORE_A1_RAW
3024      P:000855 P:000857 0A8522            BSET    #DCTR_SRIE,X:DCTR
3025      P:000856 P:000858 00000C            RTS
3026   
3027   
3028                                ;
3029                                ; Utility functions for 32/24-bit <-> 16+16 conversion.
3030                                ;
3031   
3032                                PROCESS_SPLIT_X0_XR0
3033      
3034      
3035      P:000857 P:000859 208800            MOVE              X0,A0
3036      P:000858 P:00085A 0C1881            EXTRACTU #$010000,A,B
                            010000
3037      P:00085A P:00085C 0C1880            EXTRACTU #$008010,A,A                     ; Put
                            008010
3038      P:00085C P:00085E 515800            MOVE              B0,X:(R0)+
3039      P:00085D P:00085F 505800            MOVE              A0,X:(R0)+
3040      P:00085E P:000860 00000C            RTS
3041   
3042                                PROCESS_JOIN_XR0_A
3043      
3044      
3045      P:00085F P:000861 200013            CLR     A
3046      P:000860 P:000862 50D800            MOVE              X:(R0)+,A0
3047      P:000861 P:000863 44D800            MOVE              X:(R0)+,X0
3048      P:000862 P:000864 0C1940            INSERT  #$010010,X0,A
                            010010
3049      P:000864 P:000866 00000C            RTS
3050   
3051   
3052                                ;
3053                                ; PCI burst code
3054                                ;
3055                                ; BLOCK_TRANSFERX and CON_TRANSFERX are replacements for
3056                                ; BLOCK_TRANSFER and CON_TRANSFER.  They can read/write to X or Y
3057                                ; memory, depending on X:STATUS[COMM_TFR_YMEM].
3058   
3059                                ;----------------------------------------------
3060                                BLOCK_TRANSFERX
3061                                ;----------------------------------------------
3062                                ;   In:
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 60



3063                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address (16:16)
3064                                ;   - BLOCK_SIZE is packet size, in bytes
3065                                ;   - MEM_SRC is start of data in X or Y memory
3066                                ;   - STATUS[COMM_TFR_YMEM] is used to determine X or Y
3067                                ;  Out:
3068                                ;   - BLOCK_SIZE will be decremented to zero.
3069                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
3070                                ;   - MEM_SRC will be incremented by BLOCK_SIZE/2
3071                                ;  Trashes:
3072                                ;   - A and B at least
3073   
3074      P:000865 P:000867 200013            CLR     A
3075      P:000866 P:000868 56AB00            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
3076      P:000867 P:000869 014085            CMP     #0,A                              ; Still bytes to transfer?
3077      P:000868 P:00086A 0AF0A2            JNE     BLOCK_TRANSFERX0
                            00086B
3078      P:00086A P:00086C 00000C            RTS
3079   
3080                                BLOCK_TRANSFERX0
3081      
3082      
3083      P:00086B P:00086D 57A900            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
3084   
3085      P:00086C P:00086E 200005            CMP     B,A                               ; A ? B
3086      P:00086D P:00086F 0E186F            JGE     <BLOCK_TRANSFERX1                 ; jump if A >= B
3087      P:00086E P:000870 21CF00            MOVE              A,B                     ; This only moves A1,B1.
3088                                BLOCK_TRANSFERX1
3089      
3090      P:00086F P:000871 200014            SUB     B,A                               ; A -= B
3091      P:000870 P:000872 014088            ADD     #0,B                              ; Clear carry bit
3092      P:000871 P:000873 562B00            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
3093      P:000872 P:000874 572A00            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
3094      P:000873 P:000875 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
3095   
3096      
3097      P:000874 P:000876 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
3098      P:000876 P:000878 50F000            MOVE              X:MEM_SRC,A0
                            00005E
3099      P:000878 P:00087A 08C82F            MOVEP             A0,X:DSR0               ; DMA source
3100      P:000879 P:00087B 200010            ADD     B,A
3101      P:00087A P:00087C 00000B            DEC     B
3102      P:00087B P:00087D 507000            MOVE              A0,X:MEM_SRC            ; BURST_SRC += BURST_SIZE/2
                            00005E
3103   
3104      P:00087D P:00087F 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
3105   
3106      
3107      P:00087E P:000880 0A00AE            JSET    #COMM_TFR_YMEM,X:STATUS,BLOCK_TRANSFERX1_YMEM
                            000884
3108   
3109                                BLOCK_TRANSFERX1_XMEM
3110      P:000880 P:000882 08F4AC            MOVEP             #$8EFA50,X:DCR0         ; X to X
                            8EFA50
3111      P:000882 P:000884 0AF080            JMP     BLOCK_TRANSFERX_PCI
                            000888
3112   
3113                                BLOCK_TRANSFERX1_YMEM
3114      P:000884 P:000886 08F4AC            MOVEP             #$8EFA51,X:DCR0         ; X to Y
                            8EFA51
3115      P:000886 P:000888 0AF080            JMP     BLOCK_TRANSFERX_PCI
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 61



                            000888
3116   
3117   
3118                                BLOCK_TRANSFERX_PCI
3119      P:000888 P:00088A 44F400            MOVE              #>$7,X0                 ; Memory write
                            000007
3120      P:00088A P:00088C 302F00            MOVE              #BURST_DEST_LO,R0       ; RAM address
3121      P:00088B P:00088D 0D058D            JSR     PCI_GO                            ; Initiate PCI burst
3122   
3123                                BLOCK_TRANSFERX_PCI_WAIT
3124      P:00088C P:00088E 0BF080            JSR     CHECK_FOR_DATA
                            000AE2
3125      P:00088E P:000890 0A8A84            JCLR    #MARQ,X:DPSR,BLOCK_TRANSFERX_PCI_WAIT
                            00088C
3126   
3127      
3128      P:000890 P:000892 302F00            MOVE              #BURST_DEST_LO,R0       ; RAM address
3129   
3130      
3131      P:000891 P:000893 0A8A8E            JCLR    #MDT,X:DPSR,BLOCK_TRANSFERX_HANDLE_ERRORS
                            000897
3132   
3133      P:000893 P:000895 20001B            CLR     B
3134      P:000894 P:000896 51AA00            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
3135      P:000895 P:000897 0D06A1            JSR     ADD_HILO_ADDRESS                  ; Update source address
3136      P:000896 P:000898 0C0865            JMP     BLOCK_TRANSFERX                   ; Next burst in block
3137   
3138                                BLOCK_TRANSFERX_HANDLE_ERRORS
3139      
3140      P:000897 P:000899 0D04BE            JSR     PCI_ERROR_CLEAR
3141   
3142      P:000898 P:00089A 0A0010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
3143      P:000899 P:00089B 0E8888            JCS     BLOCK_TRANSFERX_PCI               ; Restart PCI burst
3144   
3145      P:00089A P:00089C 0A0011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
3146      P:00089B P:00089D 0E0865            JCC     BLOCK_TRANSFERX                   ; Error but no error? Redo this burst.
3147   
3148      
3149      P:00089C P:00089E 0D05A3            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
3150      P:00089D P:00089F 0D05B3            JSR     PCI_UPDATE_R0
3151      P:00089E P:0008A0 0C0888            JMP     BLOCK_TRANSFERX_PCI
3152   
3153   
3154   
3155                                ;----------------------------------------------
3156                                CON_TRANSFERX
3157                                ;----------------------------------------------
3158                                ;   In:
3159                                ;   - BURST_SRC_HI:BURST_SRC_LO is PC RAM address
3160                                ;   - BLOCK_SIZE is packet size, in bytes
3161                                ;   - YMEM_DEST is start of data in Y memory
3162                                ;  Out:
3163                                ;   - BLOCK_SIZE will be decremented to zero.
3164                                ;   - BURST_SRC_HI:LO will be incremented by BLOCK_SIZE
3165                                ;   - YMEM_DEST will be incremented by BLOCK_SIZE/2
3166                                ;  Trashes:
3167                                ;   - A and B, R0, X0
3168   
3169      P:00089F P:0008A1 200013            CLR     A
3170      P:0008A0 P:0008A2 54AB00            MOVE              X:BLOCK_SIZE,A1         ; A1 = BLOCK_SIZE
3171      P:0008A1 P:0008A3 014085            CMP     #0,A                              ; Still bytes to transfer?
3172      P:0008A2 P:0008A4 0AF0A2            JNE     CON_TRANSFERX0
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 62



                            0008A5
3173      P:0008A4 P:0008A6 00000C            RTS
3174   
3175                                CON_TRANSFERX0
3176      
3177      
3178      P:0008A5 P:0008A7 57A900            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
3179   
3180      P:0008A6 P:0008A8 200005            CMP     B,A                               ; A ? B
3181      P:0008A7 P:0008A9 0E18A9            JGE     <CON_TRANSFERX1                   ; jump if A >= B
3182      P:0008A8 P:0008AA 21CF00            MOVE              A,B                     ; This only moves A1,B1.
3183                                CON_TRANSFERX1
3184      
3185      P:0008A9 P:0008AB 200014            SUB     B,A                               ; A -= B
3186      P:0008AA P:0008AC 014088            ADD     #0,B                              ; Clear carry bit
3187      P:0008AB P:0008AD 562B00            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
3188      P:0008AC P:0008AE 572A00            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
3189      P:0008AD P:0008AF 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
3190   
3191      
3192      P:0008AE P:0008B0 50B300            MOVE              X:YMEM_DEST,A0
3193      P:0008AF P:0008B1 000000            NOP
3194      P:0008B0 P:0008B2 507000            MOVE              A0,X:DDR0               ; DMA dest'n
                            FFFFEE
3195      P:0008B2 P:0008B4 08F4AF            MOVEP             #>DRXR,X:DSR0           ; DMA source
                            FFFFCB
3196      P:0008B4 P:0008B6 200010            ADD     B,A
3197      P:0008B5 P:0008B7 00000B            DEC     B
3198      P:0008B6 P:0008B8 503300            MOVE              A0,X:YMEM_DEST          ; YMEM_DEST += BURST_SIZE/2
3199   
3200      P:0008B7 P:0008B9 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
3201   
3202      
3203      P:0008B8 P:0008BA 08F4AC            MOVEP             #$8EEAC4,X:DCR0
                            8EEAC4
3204   
3205                                CON_TRANSFERX_PCI
3206      P:0008BA P:0008BC 44F400            MOVE              #>$6,X0                 ; Memory write
                            000006
3207      P:0008BC P:0008BE 303100            MOVE              #BURST_SRC_LO,R0        ; RAM address
3208      P:0008BD P:0008BF 0D058D            JSR     PCI_GO                            ; Initiate PCI burst
3209   
3210      
3211                                CON_TRANSFERX_PCI_WAIT
3212      P:0008BE P:0008C0 0BF080            JSR     CHECK_FOR_DATA
                            000AE2
3213   
3214      
3215      P:0008C0 P:0008C2 0A8A84            JCLR    #MARQ,X:DPSR,CON_TRANSFERX_PCI_WAIT
                            0008BE
3216   
3217      
3218      P:0008C2 P:0008C4 303100            MOVE              #BURST_SRC_LO,R0        ; RAM address
3219   
3220      
3221      P:0008C3 P:0008C5 0A8A8E            JCLR    #MDT,X:DPSR,CON_TRANSFERX_HANDLE_ERRORS
                            0008C9
3222   
3223      P:0008C5 P:0008C7 20001B            CLR     B
3224      P:0008C6 P:0008C8 51AA00            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
3225      P:0008C7 P:0008C9 0D06A1            JSR     ADD_HILO_ADDRESS                  ; Update source address
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 63



3226      P:0008C8 P:0008CA 0C089F            JMP     CON_TRANSFERX                     ; Next burst in block
3227   
3228                                CON_TRANSFERX_HANDLE_ERRORS
3229      
3230      P:0008C9 P:0008CB 0D04BE            JSR     PCI_ERROR_CLEAR
3231   
3232      P:0008CA P:0008CC 0A0010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
3233      P:0008CB P:0008CD 0E88BA            JCS     CON_TRANSFERX_PCI                 ; Restart PCI burst
3234   
3235      P:0008CC P:0008CE 0A0011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
3236      P:0008CD P:0008CF 0E089F            JCC     CON_TRANSFERX                     ; Error but no error? Redo this burst.
3237   
3238      
3239      P:0008CE P:0008D0 0D05A3            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
3240      P:0008CF P:0008D1 0D05B3            JSR     PCI_UPDATE_R0
3241      P:0008D0 P:0008D2 0C08BA            JMP     CON_TRANSFERX_PCI
3242   
3243   
3244   
3248   
3249   
3250                                ;----------------------------------------------
3251                                INIT_DATAGRAM_BUFFER
3252                                ;----------------------------------------------
3253                                ; Initialize header of reply packet.
3254      P:0008D1 P:0008D3 240000            MOVE              #0,X0
3255      P:0008D2 P:0008D4 60F400            MOVE              #>DGRAM_BUFFER,R0
                            0001A7
3256                                          .loop   #DG__SIZE
3258      P:0008D6 P:0008D8 445800            MOVE              X0,X:(R0)+
3259                                          .endl
3261      P:0008D7 P:0008D9 44F400            MOVE              #>DG_VERS_CODE,X0
                            000001
3262      P:0008D9 P:0008DB 45F400            MOVE              #>(RB_REP_SIZE/2),X1
                            000012
3263      P:0008DB P:0008DD 447000            MOVE              X0,X:DGRAM_VERSION
                            0001A7
3264      P:0008DD P:0008DF 457000            MOVE              X1,X:DGRAM_SIZE
                            0001A8
3265      P:0008DF P:0008E1 60F400            MOVE              #>DGRAM_FWREV,R0
                            0001AB
3266      P:0008E1 P:0008E3 448300            MOVE              X:REV_NUMBER,X0
3267      P:0008E2 P:0008E4 0D0857            JSR     PROCESS_SPLIT_X0_XR0
3268      P:0008E3 P:0008E5 00000C            RTS
3269   
3270   
3271                                ;----------------------------------------------
3272                                COPY_DATAGRAM_TO_HOST
3273                                ;----------------------------------------------
3274                                ; Caller should have already loaded the packet buffer, especially:
3275                                ;   DGRAM_TYPE
3276                                ;   DGRAM_SIZE
3277                                ; This routine DMAs the data into RAM and hand-shakes and interrupt
3278                                ; with the driver.
3279   
3280      
3281      P:0008E4 P:0008E6 0A00AC            JSET    #COMM_REP_ENABLED,X:STATUS,COPY_DATAGRAM_TO_HOST1
                            0008E7
3282      P:0008E6 P:0008E8 00000C            RTS
3283   
3284                                COPY_DATAGRAM_TO_HOST1
3285      
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 64



3286      P:0008E7 P:0008E9 60F400            MOVE              #>REP_BUS_ADDR,R0
                            00005C
3287      P:0008E9 P:0008EB 61F400            MOVE              #>BURST_DEST_LO,R1
                            00002F
3288                                          .loop   #2
3290      P:0008ED P:0008EF 44D800            MOVE              X:(R0)+,X0
3291      P:0008EE P:0008F0 445900            MOVE              X0,X:(R1)+
3292                                          .endl
3294   
3295      
3296      P:0008EF P:0008F1 56F000            MOVE              X:DGRAM_SIZE,A
                            0001A8
3297      P:0008F1 P:0008F3 0C1D04            ASL     #2,A,A
3298      P:0008F2 P:0008F4 000000            NOP
3299      P:0008F3 P:0008F5 218400            MOVE              A1,X0
3300      P:0008F4 P:0008F6 442B00            MOVE              X0,X:BLOCK_SIZE
3301      
3302      P:0008F5 P:0008F7 44F400            MOVE              #>DGRAM_BUFFER,X0
                            0001A7
3303      P:0008F7 P:0008F9 447000            MOVE              X0,X:MEM_SRC
                            00005E
3304      P:0008F9 P:0008FB 0A000E            BCLR    #COMM_TFR_YMEM,X:STATUS
3305   
3306      
3307      P:0008FA P:0008FC 0BF080            JSR     CHECK_FOR_DATA
                            000AE2
3308   
3309      
3310      P:0008FC P:0008FE 0D0865            JSR     BLOCK_TRANSFERX
3311   
3312      
3313      P:0008FD P:0008FF 0A8526            BSET    #INTA,X:DCTR
3314      P:0008FE P:000900 0A8983            JCLR    #DSR_HF0,X:DSR,*
                            0008FE
3315   
3316      P:000900 P:000902 0A8506            BCLR    #INTA,X:DCTR
3317      P:000901 P:000903 0A89A3            JSET    #DSR_HF0,X:DSR,*
                            000901
3318      P:000903 P:000905 00000C            RTS
3319   
3320   
3321   
3322                                ;----------------------------------------------
3323                                PROCESS_REPLY
3324                                ;----------------------------------------------
3325                                ; Create a Datagram for a DSP reply and send it.  User has, previously,
3326                                ; put some data in the payload.
3327      
3328      P:000904 P:000906 54F400            MOVE              #>DG_TYPE_DSP_REP,A1
                            000001
3329      P:000906 P:000908 45F400            MOVE              #>(RB_REP_SIZE/2),X1
                            000012
3330      P:000908 P:00090A 547000            MOVE              A1,X:DGRAM_TYPE
                            0001A9
3331      P:00090A P:00090C 457000            MOVE              X1,X:DGRAM_SIZE
                            0001A8
3332   
3333      
3334      P:00090C P:00090E 0D08E4            JSR     COPY_DATAGRAM_TO_HOST
3335   
3336      
3337      P:00090D P:00090F 0A0004            BCLR    #COMM_REP,X:STATUS
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 65



3338      P:00090E P:000910 0D084F            JSR     TIMERX_STORE
3339      P:00090F P:000911 00000C            RTS
3340   
3341                                ;----------------------------------------------
3342                                PROCESS_MCE_REPLY
3343                                ;----------------------------------------------
3344                                ; Copy MCE reply into X mem and send to PC.
3345      P:000910 P:000912 56F400            MOVE              #>$b10000,A
                            B10000
3346      P:000912 P:000914 0D0853            JSR     TIMERX_STORE_A1
3347      P:000913 P:000915 0D084F            JSR     TIMERX_STORE
3348   
3349      
3350      P:000914 P:000916 60F400            MOVE              #(DGRAM_DATA),R0
                            0001B7
3351      P:000916 P:000918 63F400            MOVE              #(MCEREP_BUF+MCEREP__TYPE),R3
                            000100
3352      P:000918 P:00091A 4FF000            MOVE                          Y:(MCEREP_BUF+MCEREP__SIZE),Y1
                            000102
3353                                          .loop   #4
3355      P:00091C P:00091E 4EDB00            MOVE                          Y:(R3)+,Y0
3356      P:00091D P:00091F 465800            MOVE              Y0,X:(R0)+
3357                                          .endl
3359                                          .loop   Y1
3361      P:000920 P:000922 4EDB00            MOVE                          Y:(R3)+,Y0
3362      P:000921 P:000923 465800            MOVE              Y0,X:(R0)+
3363      P:000922 P:000924 4EDB00            MOVE                          Y:(R3)+,Y0
3364      P:000923 P:000925 465800            MOVE              Y0,X:(R0)+
3365                                          .endl
3367   
3368      P:000924 P:000926 0D084F            JSR     TIMERX_STORE
3369   
3370      
3371      P:000925 P:000927 54F400            MOVE              #>DG_TYPE_MCE_REP,A1
                            000002
3372      P:000927 P:000929 45F400            MOVE              #>(RB_MCE_SIZE/2),X1    ; size in 32-bit words.
                            000044
3373      P:000929 P:00092B 547000            MOVE              A1,X:DGRAM_TYPE
                            0001A9
3374      P:00092B P:00092D 457000            MOVE              X1,X:DGRAM_SIZE
                            0001A8
3375   
3376      
3377      P:00092D P:00092F 0D08E4            JSR     COPY_DATAGRAM_TO_HOST
3378   
3379      
3380      P:00092E P:000930 0A0006            BCLR    #COMM_MCEREP,X:STATUS
3381      P:00092F P:000931 0D084F            JSR     TIMERX_STORE
3382      P:000930 P:000932 00000C            RTS
3383   
3384   
3385                                ;----------------------------------------------
3386                                PROCESS_MCE_DATA
3387                                ;----------------------------------------------
3388                                ; Copy data frame to next PC buffer location.  Increment buffer
3389                                ; pointers and possibly schedule an information (QT_FLUSH).
3390      P:000931 P:000933 56F400            MOVE              #>$b20000,A
                            B20000
3391      P:000933 P:000935 0D0853            JSR     TIMERX_STORE_A1
3392      P:000934 P:000936 0D084F            JSR     TIMERX_STORE
3393   
3394      
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 66



3395      P:000935 P:000937 56F000            MOVE              X:QT_BUF_HEAD,A
                            000042
3396      P:000937 P:000939 014180            ADD     #1,A
3397      P:000938 P:00093A 57BF00            MOVE              X:QT_BUF_MAX,B
3398      P:000939 P:00093B 20000D            CMP     A,B                               ; End of buffer? [B ? A]
3399      P:00093A P:00093C 0AF0A7            JGT     PROCESS_MCE_DATA__CHECK_TAIL
                            00093D
3400      P:00093C P:00093E 2E0000            MOVE              #0,A                    ; Start over
3401                                PROCESS_MCE_DATA__CHECK_TAIL
3402      P:00093D P:00093F 57F000            MOVE              X:QT_BUF_TAIL,B         ; Buffer full?
                            000043
3403      P:00093F P:000941 20000D            CMP     A,B
3404      P:000940 P:000942 0AF0AA            JEQ     PROCESS_MCE_DATA__DROP_FRAME
                            000966
3405      
3406      
3407   
3408      
3409      P:000942 P:000944 56F000            MOVE              X:QT_FRAME_SIZE,A
                            000040
3410      P:000944 P:000946 000000            NOP
3411      P:000945 P:000947 542B00            MOVE              A1,X:BLOCK_SIZE
3412   
3413      
3414      P:000946 P:000948 60F400            MOVE              #>QT_DEST_LO,R0
                            000044
3415      P:000948 P:00094A 61F400            MOVE              #>BURST_DEST_LO,R1
                            00002F
3416                                          .loop   #2
3418      P:00094C P:00094E 44D800            MOVE              X:(R0)+,X0
3419      P:00094D P:00094F 445900            MOVE              X0,X:(R1)+
3420                                          .endl
3422   
3423      
3424      P:00094E P:000950 44F400            MOVE              #>MCEDATA_BUF,X0
                            000200
3425      P:000950 P:000952 447000            MOVE              X0,X:MEM_SRC
                            00005E
3426      P:000952 P:000954 0A002E            BSET    #COMM_TFR_YMEM,X:STATUS           ; DMA from Y-mem
3427   
3428      
3429      P:000953 P:000955 0D0865            JSR     BLOCK_TRANSFERX
3430   
3431      
3432      P:000954 P:000956 0BF080            JSR     BUFFER_INCR_MULTI
                            000987
3433   
3434                                PROCESS_MCE_DATA__DONE
3435      
3436      P:000956 P:000958 0D084F            JSR     TIMERX_STORE
3437      P:000957 P:000959 0A0007            BCLR    #COMM_MCEDATA,X:STATUS
3438   
3439      
3440      
3441      
3442      P:000958 P:00095A 0A002D            BSET    #COMM_BUF_UPDATE,X:STATUS
3443      P:000959 P:00095B 56F000            MOVE              X:QT_INFORM_IDX,A
                            000046
3444      P:00095B P:00095D 014180            ADD     #1,A
3445      P:00095C P:00095E 57F000            MOVE              X:QT_INFORM,B
                            000041
3446      P:00095E P:000960 567000            MOVE              A,X:QT_INFORM_IDX
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 67



                            000046
3447      P:000960 P:000962 20000D            CMP     A,B
3448      P:000961 P:000963 0AF0A2            JNE     PROCESS_MCE_DATA__DONE2
                            000965
3449      P:000963 P:000965 0A0034            BSET    #QT_FLUSH,X:STATUS
3450      P:000964 P:000966 0A000D            BCLR    #COMM_BUF_UPDATE,X:STATUS         ; So we don't trigger a timer int too
3451                                PROCESS_MCE_DATA__DONE2
3452      P:000965 P:000967 00000C            RTS
3453   
3454                                PROCESS_MCE_DATA__DROP_FRAME
3455      P:000966 P:000968 56F000            MOVE              X:QT_DROPS,A
                            000047
3456      P:000968 P:00096A 014180            ADD     #1,A
3457      P:000969 P:00096B 000000            NOP
3458      P:00096A P:00096C 567000            MOVE              A,X:QT_DROPS
                            000047
3459      P:00096C P:00096E 0C0956            JMP     PROCESS_MCE_DATA__DONE
3460   
3461   
3462                                ;----------------------------------------
3463                                BUFFER_RESET_MULTI
3464                                ;----------------------------------------
3465                                ; Reset pointers to the very start of the buffer.
3466      P:00096D P:00096F 44F400            MOVE              #>0,X0
                            000000
3467      P:00096F P:000971 45F400            MOVE              #>QT_BLOCKS,X1
                            000063
3468      P:000971 P:000973 447000            MOVE              X0,X:QT_BUF_HEAD
                            000042
3469      P:000973 P:000975 457000            MOVE              X1,X:QT_BLOCK_PTR
                            000062
3470      P:000975 P:000977 0AF080            JMP     BUFFER_SET_MULTI_BLOCK
                            00097E
3471   
3472                                ;----------------------------------------
3473                                BUFFER_SET_NEXT_MULTI_BLOCK
3474                                ;----------------------------------------
3475      P:000977 P:000979 56F000            MOVE              X:QT_BLOCK_PTR,A
                            000062
3476      P:000979 P:00097B 0140C0            ADD     #>QT_BLOCK___SIZE,A
                            000003
3477      P:00097B P:00097D 000000            NOP
3478      P:00097C P:00097E 567000            MOVE              A,X:QT_BLOCK_PTR
                            000062
3479      
3480   
3481                                ;----------------------------------------
3482                                BUFFER_SET_MULTI_BLOCK
3483                                ;----------------------------------------
3484                                ; Set QT_DEST to the address of the block described at QT_BLOCK_PTR
3485      P:00097E P:000980 60F000            MOVE              X:QT_BLOCK_PTR,R0
                            000062
3486      P:000980 P:000982 314400            MOVE              #QT_DEST_LO,R1
3487      P:000981 P:000983 000000            NOP
3488      P:000982 P:000984 020094            MOVE              X:(R0+QT_BLOCK__ADDR+0),X0
3489      P:000983 P:000985 446100            MOVE              X0,X:(R1)
3490      P:000984 P:000986 0200D4            MOVE              X:(R0+QT_BLOCK__ADDR+1),X0
3491      P:000985 P:000987 0201C4            MOVE              X0,X:(R1+1)
3492      P:000986 P:000988 00000C            RTS
3493   
3494                                ;----------------------------------------
3495                                BUFFER_INCR_MULTI
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 68



3496                                ;----------------------------------------
3497                                ; Increment the buf_head index (possibly wrapping it back to 0).
3498                                ; Set up QT_DEST_LO to point to the next RAM location, which may
3499                                ; be in a different RAM block.
3500      P:000987 P:000989 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
                            000042
3501      P:000989 P:00098B 014180            ADD     #1,A                              ;
3502      P:00098A P:00098C 57BF00            MOVE              X:QT_BUF_MAX,B          ;
3503      P:00098B P:00098D 20000D            CMP     A,B                               ;
3504      P:00098C P:00098E 0EF96D            JLE     BUFFER_RESET_MULTI                ;       head = 0
3505                                                                                    ; else
3506      P:00098D P:00098F 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            000042
3507   
3508      
3509      P:00098F P:000991 60F000            MOVE              X:QT_BLOCK_PTR,R0
                            000062
3510      P:000991 P:000993 000000            NOP
3511      P:000992 P:000994 000000            NOP
3512      P:000993 P:000995 02089F            MOVE              X:(R0+QT_BLOCK__END_IDX),B
3513      P:000994 P:000996 000000            NOP
3514      P:000995 P:000997 20000D            CMP     A,B                               ; (block_end [?] head)
3515      P:000996 P:000998 0EF977            JLE     BUFFER_SET_NEXT_MULTI_BLOCK
3516   
3517      P:000997 P:000999 20001B            CLR     B
3518      P:000998 P:00099A 51BE00            MOVE              X:QT_BUF_SIZE,B0
3519      P:000999 P:00099B 304400            MOVE              #QT_DEST_LO,R0
3520      P:00099A P:00099C 0D06A1            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
3521   
3522      P:00099B P:00099D 00000C            RTS
3523   
3524   
3525                                ;
3526                                ; Other activities
3527                                ;
3528   
3529                                ;----------------------------------------
3530                                SEND_BUF_INFO
3531                                ;----------------------------------------
3532                                ; Inform the the PC of the current data buffer state.
3533                                ;
3534      P:00099C P:00099E 56F400            MOVE              #>$b80000,A
                            B80000
3535      P:00099E P:0009A0 0D0853            JSR     TIMERX_STORE_A1
3536      P:00099F P:0009A1 0D084F            JSR     TIMERX_STORE
3537   
3538      
3539      P:0009A0 P:0009A2 44F000            MOVE              X:QT_BUF_HEAD,X0
                            000042
3540      P:0009A2 P:0009A4 447000            MOVE              X0,X:DGRAM_DATA
                            0001B7
3541      P:0009A4 P:0009A6 240000            MOVE              #0,X0
3542      P:0009A5 P:0009A7 447000            MOVE              X0,X:(DGRAM_DATA+1)
                            0001B8
3543   
3544      
3545      P:0009A7 P:0009A9 54F400            MOVE              #>DG_TYPE_BUF_INF,A1
                            000003
3546      P:0009A9 P:0009AB 45F400            MOVE              #>(RB_INF_SIZE/2),X1    ; size in 32-bit words.
                            00000B
3547      P:0009AB P:0009AD 547000            MOVE              A1,X:DGRAM_TYPE
                            0001A9
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 69



3548      P:0009AD P:0009AF 457000            MOVE              X1,X:DGRAM_SIZE
                            0001A8
3549   
3550      P:0009AF P:0009B1 0D08E4            JSR     COPY_DATAGRAM_TO_HOST
3551   
3552      P:0009B0 P:0009B2 0A0014            BCLR    #QT_FLUSH,X:STATUS
3553      P:0009B1 P:0009B3 0D084F            JSR     TIMERX_STORE
3554      P:0009B2 P:0009B4 00000C            RTS
3555   
3556   
3557   
3561   
3562                                ;------------------------
3563                                BUFFER_PC_CMD_INT_HANDLER
3564                                ;------------------------
3565                                ; Interrupt handler for DRXR.  Reads command from PC, by polling.
3566   
3567      
3568      P:0009B3 P:0009B5 0D04A0            JSR     SAVE_REGISTERS                    ; This does not save all the registers...
3569      P:0009B4 P:0009B6 0A8502            BCLR    #DCTR_SRIE,X:DCTR
3570   
3571      
3572      P:0009B5 P:0009B7 54F400            MOVE              #>$CC0000,A1
                            CC0000
3573      P:0009B7 P:0009B9 0D0842            JSR     TIMERX_STORE_A1_RAW
3574      P:0009B8 P:0009BA 0D0840            JSR     TIMERX_STORE_RAW
3575   
3576      
3577      P:0009B9 P:0009BB 0A8523            BSET    #DCTR_HF3,X:DCTR
3578   
3579      
3580      P:0009BA P:0009BC 0A8982            JCLR    #SRRQ,X:DSR,BUFFER_PC_CMD_INT_HANDLER_EXIT
                            0009DA
3581   
3582      P:0009BC P:0009BE 08440B            MOVEP             X:DRXR,X0               ; 16-bit word #0 = the command
3583      P:0009BD P:0009BF 447000            MOVE              X0,X:CMD_WORD
                            00005B
3584      P:0009BF P:0009C1 000000            NOP
3585      P:0009C0 P:0009C2 000000            NOP
3586      P:0009C1 P:0009C3 0A8982            JCLR    #SRRQ,X:DSR,*
                            0009C1
3587      P:0009C3 P:0009C5 08440B            MOVEP             X:DRXR,X0
3588      P:0009C4 P:0009C6 447000            MOVE              X0,X:CMD_SIZE           ; 16-bit word #1 = size of upcoming data,
                            00005A
3589                                                                                    ; in 32-bit words.
3590   
3591      
3592      P:0009C6 P:0009C8 200013            CLR     A
3593      P:0009C7 P:0009C9 30A700            MOVE              #CMD_BUFFER,R0
3594      P:0009C8 P:0009CA 54F000            MOVE              X:CMD_SIZE,A1
                            00005A
3595   
3596      
3597      P:0009CA P:0009CC 014085            CMP     #0,A
3598      P:0009CB P:0009CD 0AF0AA            JEQ     BUFFER_PC_CMD_INT_HANDLER_OK
                            0009D9
3599   
3600                                          .loop   A1
3602      P:0009CF P:0009D1 0A8982            JCLR    #SRRQ,X:DSR,*
                            0009CF
3603      P:0009D1 P:0009D3 08588B            MOVEP             X:DRXR,X:(R0)+
3604      P:0009D2 P:0009D4 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 70



3605      P:0009D3 P:0009D5 000000            NOP
3606      P:0009D4 P:0009D6 0A8982            JCLR    #SRRQ,X:DSR,*
                            0009D4
3607      P:0009D6 P:0009D8 08588B            MOVEP             X:DRXR,X:(R0)+
3608      P:0009D7 P:0009D9 000000            NOP
3609      P:0009D8 P:0009DA 000000            NOP
3610                                          .endl
3612   
3613                                BUFFER_PC_CMD_INT_HANDLER_OK
3614      
3615      P:0009D9 P:0009DB 0A0025            BSET    #COMM_CMD,X:STATUS
3616   
3617                                BUFFER_PC_CMD_INT_HANDLER_EXIT
3618      
3619      P:0009DA P:0009DC 0D0840            JSR     TIMERX_STORE_RAW
3620   
3621      
3622      P:0009DB P:0009DD 0A8522            BSET    #DCTR_SRIE,X:DCTR
3623      P:0009DC P:0009DE 0A8503            BCLR    #DCTR_HF3,X:DCTR
3624      P:0009DD P:0009DF 0D0493            JSR     RESTORE_REGISTERS
3625      P:0009DE P:0009E0 000004            RTI
3626   
3627   
3628                                ;------------------------
3629                                PROCESS_PC_CMD
3630                                ;------------------------
3631                                ; Parse the command from host and pass to a specialized processor.
3632                                ;
3633      P:0009DF P:0009E1 54F400            MOVE              #>$cd0000,A1
                            CD0000
3634      P:0009E1 P:0009E3 0D0853            JSR     TIMERX_STORE_A1
3635   
3636      
3637      P:0009E2 P:0009E4 54F400            MOVE              #>DG_TYPE_DSP_REP,A1
                            000001
3638      P:0009E4 P:0009E6 57F000            MOVE              X:CMD_WORD,B            ; this will be used in the switch below.
                            00005B
3639      P:0009E6 P:0009E8 547000            MOVE              A1,X:DGRAM_TYPE         ; type is "dsp reply"
                            0001A9
3640      P:0009E8 P:0009EA 557000            MOVE              B1,X:REP_RCMD           ; copy of command word
                            0001B9
3641      P:0009EA P:0009EC 517000            MOVE              B0,X:REP_RSTAT          ; status = 0
                            0001B7
3642      P:0009EC P:0009EE 517000            MOVE              B0,X:REP_RSIZE          ; data size = 0
                            0001B8
3643   
3644      P:0009EE P:0009F0 21EE00            MOVE              B,A
3645      P:0009EF P:0009F1 0D0853            JSR     TIMERX_STORE_A1
3646      P:0009F0 P:0009F2 0D084F            JSR     TIMERX_STORE
3647   
3648      
3649      
3650      
3651      
3652      P:0009F1 P:0009F3 60F400            MOVE              #>CMD_BUFFER,R0
                            0000A7
3653      P:0009F3 P:0009F5 0D085F            JSR     PROCESS_JOIN_XR0_A
3654      P:0009F4 P:0009F6 211100            MOVE              A0,R1                   ; "address"
3655      P:0009F5 P:0009F7 0D085F            JSR     PROCESS_JOIN_XR0_A
3656      P:0009F6 P:0009F8 210500            MOVE              A0,X1                   ; "data"
3657   
3658      P:0009F7 P:0009F9 0140CD            CMP     #>CMD_READ_P,B
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 71



                            000001
3659      P:0009F9 P:0009FB 0AF0AA            JEQ     PROCESS_READ_P
                            000A27
3660   
3661      P:0009FB P:0009FD 0140CD            CMP     #>CMD_READ_X,B
                            000002
3662      P:0009FD P:0009FF 0AF0AA            JEQ     PROCESS_READ_X
                            000A2A
3663   
3664      P:0009FF P:000A01 0140CD            CMP     #>CMD_READ_Y,B
                            000003
3665      P:000A01 P:000A03 0AF0AA            JEQ     PROCESS_READ_Y
                            000A2D
3666   
3667      P:000A03 P:000A05 0140CD            CMP     #>CMD_WRITE_P,B
                            000005
3668      P:000A05 P:000A07 0AF0AA            JEQ     PROCESS_WRITE_P
                            000A3B
3669   
3670      P:000A07 P:000A09 0140CD            CMP     #>CMD_WRITE_X,B
                            000006
3671      P:000A09 P:000A0B 0AF0AA            JEQ     PROCESS_WRITE_X
                            000A3E
3672   
3673      P:000A0B P:000A0D 0140CD            CMP     #>CMD_WRITE_Y,B
                            000007
3674      P:000A0D P:000A0F 0AF0AA            JEQ     PROCESS_WRITE_Y
                            000A41
3675   
3676      P:000A0F P:000A11 0140CD            CMP     #>CMD_SET_REP_BUF,B
                            000009
3677      P:000A11 P:000A13 0AF0AA            JEQ     PROCESS_SET_REP_BUFFER
                            000A48
3678   
3679      P:000A13 P:000A15 0140CD            CMP     #>CMD_SET_DATA_BUF_MULTI,B
                            00000B
3680      P:000A15 P:000A17 0AF0AA            JEQ     PROCESS_SET_DATA_BUFFER_MULTI
                            000A5A
3681   
3682      P:000A17 P:000A19 0140CD            CMP     #>CMD_SEND_MCE,B
                            000021
3683      P:000A19 P:000A1B 0AF0AA            JEQ     PROCESS_SEND_MCE
                            000A8F
3684   
3685      P:000A1B P:000A1D 0140CD            CMP     #>CMD_POST_MCE,B
                            000022
3686      P:000A1D P:000A1F 0AF0AA            JEQ     PROCESS_POST_MCE
                            000A9F
3687   
3688      P:000A1F P:000A21 0140CD            CMP     #>CMD_SET_TAIL_INF,B
                            000012
3689      P:000A21 P:000A23 0AF0AA            JEQ     PROCESS_SET_TAIL_INF
                            000AC0
3690   
3691      
3692      P:000A23 P:000A25 0A0005            BCLR    #COMM_CMD,X:STATUS
3693      P:000A24 P:000A26 0A0028            BSET    #COMM_ERR,X:STATUS
3694   
3695      P:000A25 P:000A27 0D084F            JSR     TIMERX_STORE
3696      P:000A26 P:000A28 00000C            RTS
3697   
3698   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 72



3699                                ;----------------------------------------------
3700                                ; READ and WRITE MEMORY command handling
3701                                ;----------------------------------------------
3702                                ; PROCESS_[READ|WRITE]_[P|X|Y] are called by PROCESS_PC_CMD.
3703                                ; They assume that R1 is the pointer for read/write, and for writes
3704                                ; that X1 contains the data.
3705   
3706                                PROCESS_READ_P
3707      P:000A27 P:000A29 07E184            MOVE              P:(R1),X0
3708      P:000A28 P:000A2A 0AF080            JMP     PROCESS_READ_EXIT
                            000A30
3709                                PROCESS_READ_X
3710      P:000A2A P:000A2C 44E100            MOVE              X:(R1),X0
3711      P:000A2B P:000A2D 0AF080            JMP     PROCESS_READ_EXIT
                            000A30
3712                                PROCESS_READ_Y
3713      P:000A2D P:000A2F 4CE100            MOVE                          Y:(R1),X0
3714      P:000A2E P:000A30 0AF080            JMP     PROCESS_READ_EXIT
                            000A30
3715   
3716                                PROCESS_READ_EXIT
3717      
3718      P:000A30 P:000A32 60F400            MOVE              #>REP_RPAYLOAD,R0
                            0001BB
3719      P:000A32 P:000A34 0D0857            JSR     PROCESS_SPLIT_X0_XR0
3720      
3721      P:000A33 P:000A35 44F400            MOVE              #>1,X0
                            000001
3722      P:000A35 P:000A37 447000            MOVE              X0,X:REP_RSIZE
                            0001B8
3723      
3724      P:000A37 P:000A39 0A0005            BCLR    #COMM_CMD,X:STATUS
3725      P:000A38 P:000A3A 0A0024            BSET    #COMM_REP,X:STATUS
3726      P:000A39 P:000A3B 0D084F            JSR     TIMERX_STORE
3727      P:000A3A P:000A3C 00000C            RTS
3728   
3729                                PROCESS_WRITE_P
3730      P:000A3B P:000A3D 076185            MOVE              X1,P:(R1)
3731      P:000A3C P:000A3E 0AF080            JMP     PROCESS_WRITE_EXIT
                            000A44
3732                                PROCESS_WRITE_X
3733      P:000A3E P:000A40 456100            MOVE              X1,X:(R1)
3734      P:000A3F P:000A41 0AF080            JMP     PROCESS_WRITE_EXIT
                            000A44
3735                                PROCESS_WRITE_Y
3736      P:000A41 P:000A43 4D6100            MOVE                          X1,Y:(R1)
3737      P:000A42 P:000A44 0AF080            JMP     PROCESS_WRITE_EXIT
                            000A44
3738   
3739                                PROCESS_WRITE_EXIT
3740      
3741      P:000A44 P:000A46 0A0005            BCLR    #COMM_CMD,X:STATUS
3742      P:000A45 P:000A47 0A0024            BSET    #COMM_REP,X:STATUS
3743      P:000A46 P:000A48 0D084F            JSR     TIMERX_STORE
3744      P:000A47 P:000A49 00000C            RTS
3745   
3746                                ;----------------------------------------------
3747                                PROCESS_SET_REP_BUFFER
3748                                ;----------------------------------------------
3749                                ; Store the PC reply buffer bus address, and enable replies.
3750                                ;
3751                                ; Format of the command payload:
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 73



3752                                ;       CMD_BUFFER+0   lo 16 bits | Buffer bus address
3753                                ;       CMD_BUFFER+1   hi 16 bits |
3754                                ;
3755                                ; Triggers a reply to host.
3756      P:000A48 P:000A4A 200013            CLR     A
3757      P:000A49 P:000A4B 30A700            MOVE              #CMD_BUFFER,R0
3758      P:000A4A P:000A4C 315C00            MOVE              #REP_BUS_ADDR,R1
3759                                          .loop   #2
3761      P:000A4D P:000A4F 44D800            MOVE              X:(R0)+,X0
3762      P:000A4E P:000A50 445900            MOVE              X0,X:(R1)+
3763      P:000A4F P:000A51 200042            OR      X0,A                              ; If there is a 1 in that address, we will f
ind it.
3764                                          .endl
3766   
3767      
3768      P:000A50 P:000A52 0A0005            BCLR    #COMM_CMD,X:STATUS
3769      P:000A51 P:000A53 0A0024            BSET    #COMM_REP,X:STATUS
3770      P:000A52 P:000A54 0D084F            JSR     TIMERX_STORE
3771   
3772      
3773      P:000A53 P:000A55 014085            CMP     #0,A
3774      P:000A54 P:000A56 0AF0AA            JEQ     PROCESS_SET_REP_BUFFER_DISABLE
                            000A58
3775   
3776      P:000A56 P:000A58 0A002C            BSET    #COMM_REP_ENABLED,X:STATUS
3777      P:000A57 P:000A59 00000C            RTS
3778   
3779                                PROCESS_SET_REP_BUFFER_DISABLE
3780      P:000A58 P:000A5A 0A000C            BCLR    #COMM_REP_ENABLED,X:STATUS
3781      P:000A59 P:000A5B 00000C            RTS
3782   
3783   
3784                                ;-------------------------------
3785                                PROCESS_SET_DATA_BUFFER_MULTI
3786                                ;-------------------------------
3787                                ; Store the PC data frame buffer information.  Resets all frame data
3788                                ; counters and pointers.
3789                                ;
3790                                ;
3791                                ; Format of the command payload:
3792                                ;       CMD_BUFFER+0   Total frame container count (max 65535)
3793                                ;       CMD_BUFFER+2   Frame container size (max 65535)
3794                                ;       CMD_BUFFER+4   Frame size (max 65535)
3795                                ;       CMD_BUFFER+6   Number of contiguous RAM blocks
3796                                ;       CMD_BUFFER+8   Block data; 6 words per block.
3797                                ;
3798                                ;       BLOCK_START+0  lo 16  | Buffer bus address
3799                                ;       BLOCK_START+1  hi 16  |
3800                                ;       BLOCK_START+2  First index of buffer
3801                                ;       BLOCK_START+4  Last index of buffer + 1
3802                                ;
3803      P:000A5A P:000A5C 60F400            MOVE              #>CMD_BUFFER,R0
                            0000A7
3804      P:000A5C P:000A5E 000000            NOP
3805      P:000A5D P:000A5F 000000            NOP
3806      P:000A5E P:000A60 44D800            MOVE              X:(R0)+,X0              ; 0
3807      P:000A5F P:000A61 443F00            MOVE              X0,X:QT_BUF_MAX
3808      P:000A60 P:000A62 44D800            MOVE              X:(R0)+,X0
3809   
3810      P:000A61 P:000A63 44D800            MOVE              X:(R0)+,X0              ; 1
3811      P:000A62 P:000A64 443E00            MOVE              X0,X:QT_BUF_SIZE
3812      P:000A63 P:000A65 44D800            MOVE              X:(R0)+,X0
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 74



3813   
3814      P:000A64 P:000A66 44D800            MOVE              X:(R0)+,X0              ; 2
3815      P:000A65 P:000A67 447000            MOVE              X0,X:QT_FRAME_SIZE
                            000040
3816      P:000A67 P:000A69 44D800            MOVE              X:(R0)+,X0
3817   
3818      
3819      P:000A68 P:000A6A 44D800            MOVE              X:(R0)+,X0              ; 3
3820      P:000A69 P:000A6B 447000            MOVE              X0,X:QT_N_BLOCK
                            000061
3821      P:000A6B P:000A6D 44D800            MOVE              X:(R0)+,X0
3822   
3823      
3824      
3825   
3826      P:000A6C P:000A6E 61F400            MOVE              #>QT_BLOCKS,R1
                            000063
3827      P:000A6E P:000A70 45F000            MOVE              X:QT_N_BLOCK,X1
                            000061
3828      P:000A70 P:000A72 000000            NOP
3829   
3830                                          .loop   X1
3832   
3833      P:000A73 P:000A75 44D800            MOVE              X:(R0)+,X0              ; BUF+0
3834      P:000A74 P:000A76 445900            MOVE              X0,X:(R1)+              ;  addr_lo
3835      P:000A75 P:000A77 44D800            MOVE              X:(R0)+,X0
3836      P:000A76 P:000A78 445900            MOVE              X0,X:(R1)+              ;  addr_hi
3837   
3838      P:000A77 P:000A79 44D800            MOVE              X:(R0)+,X0              ; BUF+1
3839      P:000A78 P:000A7A 44D800            MOVE              X:(R0)+,X0              ;
3840   
3841      P:000A79 P:000A7B 44D800            MOVE              X:(R0)+,X0              ; BUF+2
3842      P:000A7A P:000A7C 445900            MOVE              X0,X:(R1)+              ;  end_idx
3843      P:000A7B P:000A7D 44D800            MOVE              X:(R0)+,X0              ;
3844   
3845                                          .endl
3847   
3848      
3849      P:000A7C P:000A7E 44F400            MOVE              #>0,X0
                            000000
3850      P:000A7E P:000A80 45F400            MOVE              #>1,X1
                            000001
3851      P:000A80 P:000A82 447000            MOVE              X0,X:QT_BUF_HEAD
                            000042
3852      P:000A82 P:000A84 447000            MOVE              X0,X:QT_BUF_TAIL
                            000043
3853      P:000A84 P:000A86 447000            MOVE              X0,X:QT_DROPS
                            000047
3854      P:000A86 P:000A88 447000            MOVE              X0,X:QT_INFORM_IDX
                            000046
3855      P:000A88 P:000A8A 457000            MOVE              X1,X:QT_INFORM
                            000041
3856   
3857      
3858      P:000A8A P:000A8C 0D096D            JSR     BUFFER_RESET_MULTI
3859   
3860      
3861      P:000A8B P:000A8D 0A0005            BCLR    #COMM_CMD,X:STATUS
3862      P:000A8C P:000A8E 0A0024            BSET    #COMM_REP,X:STATUS
3863      P:000A8D P:000A8F 0D084F            JSR     TIMERX_STORE
3864      P:000A8E P:000A90 00000C            RTS
3865   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 75



3866   
3867                                ;-------------------------------
3868                                PROCESS_SEND_MCE
3869                                ;-------------------------------
3870                                ; The command includes an MCE command packet; send it to the MCE
3871                                ; Data are stored as 128 x 16bit units, starting at X:CMD_BUFFER.
3872      P:000A8F P:000A91 30A700            MOVE              #CMD_BUFFER,R0
3873                                          .loop   #128
3875      P:000A92 P:000A94 54D800            MOVE              X:(R0)+,A1              ; get hi 16
3876      P:000A93 P:000A95 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
3877      P:000A94 P:000A96 0140C6            AND     #>$FF,A
                            0000FF
3878      P:000A96 P:000A98 547000            MOVE              A1,X:FO_SEND
                            FFF000
3879      P:000A98 P:000A9A 557000            MOVE              B1,X:FO_SEND
                            FFF000
3880                                          .endl
3882   
3883      P:000A9A P:000A9C 000000            NOP
3884      P:000A9B P:000A9D 0A0005            BCLR    #COMM_CMD,X:STATUS
3885      P:000A9C P:000A9E 0A0024            BSET    #COMM_REP,X:STATUS
3886      P:000A9D P:000A9F 0D084F            JSR     TIMERX_STORE
3887      P:000A9E P:000AA0 00000C            RTS
3888   
3889   
3890                                ;-------------------------------
3891                                PROCESS_POST_MCE
3892                                ;-------------------------------
3893                                ; The command indicates that an MCE command has been placed in memory
3894                                ; and should be copied to the MCE.  The bus address is at X:CMD_BUFFER.
3895                                ;
3896      P:000A9F P:000AA1 60F400            MOVE              #>CMD_BUFFER,R0
                            0000A7
3897      P:000AA1 P:000AA3 61F400            MOVE              #>BURST_SRC_LO,R1
                            000031
3898      P:000AA3 P:000AA5 000000            NOP
3899                                          .loop   #2
3901      P:000AA6 P:000AA8 44D800            MOVE              X:(R0)+,X0
3902      P:000AA7 P:000AA9 445900            MOVE              X0,X:(R1)+
3903                                          .endl
3905   
3906      P:000AA8 P:000AAA 44F400            MOVE              #>MCECMD_BUF,X0
                            000000
3907      P:000AAA P:000AAC 443300            MOVE              X0,X:YMEM_DEST
3908      P:000AAB P:000AAD 44F400            MOVE              #>256,X0
                            000100
3909      P:000AAD P:000AAF 442B00            MOVE              X0,X:BLOCK_SIZE
3910   
3911      P:000AAE P:000AB0 0D089F            JSR     CON_TRANSFERX
3912   
3913      P:000AAF P:000AB1 0D084F            JSR     TIMERX_STORE
3914   
3915      
3916      
3917      P:000AB0 P:000AB2 330000            MOVE              #MCECMD_BUF,R3
3918                                          .loop   #128
3920      P:000AB3 P:000AB5 5CDB00            MOVE                          Y:(R3)+,A1  ; get hi 16
3921      P:000AB4 P:000AB6 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
3922      P:000AB5 P:000AB7 0140C6            AND     #>$FF,A
                            0000FF
3923      P:000AB7 P:000AB9 547000            MOVE              A1,X:FO_SEND
                            FFF000
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 76



3924      P:000AB9 P:000ABB 557000            MOVE              B1,X:FO_SEND
                            FFF000
3925                                          .endl
3927   
3928      P:000ABB P:000ABD 000000            NOP
3929      P:000ABC P:000ABE 0A0005            BCLR    #COMM_CMD,X:STATUS
3930      P:000ABD P:000ABF 0A0024            BSET    #COMM_REP,X:STATUS
3931      P:000ABE P:000AC0 0D084F            JSR     TIMERX_STORE
3932      P:000ABF P:000AC1 00000C            RTS
3933   
3934   
3935                                ;-------------------------------
3936                                PROCESS_SET_TAIL_INF
3937                                ;-------------------------------
3938                                ; Host has sent a buffer information packet; update buffer state.
3939                                ;
3940                                ;       CMD_BUFFER+0    New circular buffer tail index.
3941                                ;       CMD_BUFFER+2    New value for QT_INFORM.
3942                                ;
3943      P:000AC0 P:000AC2 60F400            MOVE              #>CMD_BUFFER,R0
                            0000A7
3944      P:000AC2 P:000AC4 000000            NOP
3945      P:000AC3 P:000AC5 000000            NOP
3946      P:000AC4 P:000AC6 44D800            MOVE              X:(R0)+,X0
3947      P:000AC5 P:000AC7 447000            MOVE              X0,X:QT_BUF_TAIL
                            000043
3948      P:000AC7 P:000AC9 44D800            MOVE              X:(R0)+,X0
3949   
3950      
3951      P:000AC8 P:000ACA 200013            CLR     A
3952      P:000AC9 P:000ACB 44D800            MOVE              X:(R0)+,X0
3953      P:000ACA P:000ACC 200045            CMP     X0,A
3954      P:000ACB P:000ACD 0AF0AA            JEQ     PROCESS_SET_TAIL_INF_1
                            000AD1
3955      P:000ACD P:000ACF 447000            MOVE              X0,X:QT_INFORM
                            000041
3956      P:000ACF P:000AD1 547000            MOVE              A1,X:QT_INFORM_IDX
                            000046
3957                                PROCESS_SET_TAIL_INF_1
3958      P:000AD1 P:000AD3 44D800            MOVE              X:(R0)+,X0
3959   
3960      
3961      P:000AD2 P:000AD4 0A0005            BCLR    #COMM_CMD,X:STATUS
3962      P:000AD3 P:000AD5 0A0024            BSET    #COMM_REP,X:STATUS
3963      P:000AD4 P:000AD6 0D084F            JSR     TIMERX_STORE
3964      P:000AD5 P:000AD7 00000C            RTS
3965   
3966   
3967                                ;----------------------------------------
3968                                RESET_MCE_COMMS
3969                                ;----------------------------------------
3970                                ; Vector interrupt to send special reset signal to MCE rx.
3971      P:000AD6 P:000AD8 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
3972      P:000AD7 P:000AD9 000000            NOP
3973      P:000AD8 P:000ADA 000000            NOP
3974      P:000AD9 P:000ADB 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
3975      P:000ADB P:000ADD 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
3976      P:000ADD P:000ADF 446000            MOVE              X0,X:(R0)
3977      P:000ADE P:000AE0 0606A0            REP     #6                                ; Wait for transmission to complete
3978      P:000ADF P:000AE1 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 77



3979      P:000AE0 P:000AE2 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
3980      P:000AE1 P:000AE3 000004            RTI
3981   
3982   
3983                                ;------------------------
3984                                CHECK_FOR_DATA
3985                                ;------------------------
3986      P:000AE2 P:000AE4 0A8525            BSET    #DCTR_HF5,X:DCTR
3987      P:000AE3 P:000AE5 01AD80            JCLR    #EF,X:PDRD,CHECK_FOR_DATA_EXIT
                            000B3C
3988      P:000AE5 P:000AE7 000000            NOP
3989      P:000AE6 P:000AE8 000000            NOP
3990      P:000AE7 P:000AE9 01AD80            JCLR    #EF,X:PDRD,CHECK_FOR_DATA_EXIT
                            000B3C
3991      
3992      P:000AE9 P:000AEB 54F400            MOVE              #>$cf0000,A1
                            CF0000
3993      P:000AEB P:000AED 0D0853            JSR     TIMERX_STORE_A1
3994      P:000AEC P:000AEE 0D084F            JSR     TIMERX_STORE
3995   
3996      
3997      
3998      
3999   
4000      P:000AED P:000AEF 200013            CLR     A                                 ; A0=0
4001      P:000AEE P:000AF0 60F400            MOVE              #>MCEHDR,R0
                            00009F
4002   
4003      P:000AF0 P:000AF2 094C3F            MOVEP             Y:RDFIFO,A1
4004      P:000AF1 P:000AF3 0140C6            AND     #>$00FFFF,A
                            00FFFF
4005      P:000AF3 P:000AF5 545800            MOVE              A1,X:(R0)+
4006      P:000AF4 P:000AF6 0140C5            CMP     #>$00A5A5,A
                            00A5A5
4007      P:000AF6 P:000AF8 0AF0A2            JNE     RESET_FIFO                        ; Empty the FIFO, and return to main loop.
                            000B76
4008   
4009      P:000AF8 P:000AFA 01AD80            JCLR    #EF,X:PDRD,*
                            000AF8
4010   
4011      P:000AFA P:000AFC 094C3F            MOVEP             Y:RDFIFO,A1
4012      P:000AFB P:000AFD 0140C6            AND     #>$00FFFF,A
                            00FFFF
4013      P:000AFD P:000AFF 545800            MOVE              A1,X:(R0)+
4014      P:000AFE P:000B00 0140C5            CMP     #>$00A5A5,A
                            00A5A5
4015      P:000B00 P:000B02 0AF0A2            JNE     RESET_FIFO
                            000B76
4016   
4017      P:000B02 P:000B04 01AD80            JCLR    #EF,X:PDRD,*
                            000B02
4018   
4019      P:000B04 P:000B06 094C3F            MOVEP             Y:RDFIFO,A1
4020      P:000B05 P:000B07 0140C6            AND     #>$00FFFF,A
                            00FFFF
4021      P:000B07 P:000B09 545800            MOVE              A1,X:(R0)+
4022      P:000B08 P:000B0A 0140C5            CMP     #>$005A5A,A
                            005A5A
4023      P:000B0A P:000B0C 0AF0A2            JNE     RESET_FIFO
                            000B76
4024   
4025      P:000B0C P:000B0E 01AD80            JCLR    #EF,X:PDRD,*
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 78



                            000B0C
4026   
4027      P:000B0E P:000B10 094C3F            MOVEP             Y:RDFIFO,A1
4028      P:000B0F P:000B11 0140C6            AND     #>$00FFFF,A
                            00FFFF
4029      P:000B11 P:000B13 545800            MOVE              A1,X:(R0)+
4030      P:000B12 P:000B14 0140C5            CMP     #>$005A5A,A
                            005A5A
4031      P:000B14 P:000B16 0AF0A2            JNE     RESET_FIFO
                            000B76
4032   
4033      
4034                                          .loop   #4
4036      P:000B18 P:000B1A 01AD80            JCLR    #EF,X:PDRD,*
                            000B18
4037      P:000B1A P:000B1C 000000            NOP
4038      P:000B1B P:000B1D 000000            NOP
4039      P:000B1C P:000B1E 01AD80            JCLR    #EF,X:PDRD,*
                            000B1C
4040      P:000B1E P:000B20 094C3F            MOVEP             Y:RDFIFO,A1
4041      P:000B1F P:000B21 0140C6            AND     #>$00ffff,A
                            00FFFF
4042      P:000B21 P:000B23 000000            NOP
4043      P:000B22 P:000B24 545800            MOVE              A1,X:(R0)+
4044                                          .endl
4046   
4047      P:000B23 P:000B25 0D084F            JSR     TIMERX_STORE
4048   
4049      
4050      
4051      P:000B24 P:000B26 200013            CLR     A
4052      P:000B25 P:000B27 50F000            MOVE              X:MCEHDR_SIZE,A0
                            0000A5
4053      P:000B27 P:000B29 0D05BE            JSR     PACKET_PARTITIONS
4054   
4055      P:000B28 P:000B2A 0D084F            JSR     TIMERX_STORE
4056   
4057      P:000B29 P:000B2B 200013            CLR     A
4058      P:000B2A P:000B2C 54F000            MOVE              X:MCEHDR_TYPE,A1
                            0000A3
4059   
4060      
4061      P:000B2C P:000B2E 0140C5            CMP     #'RP',A
                            005250
4062      P:000B2E P:000B30 0AF0AA            JEQ     CHECK_FOR_DATA__BUFFER_REPLY
                            000B3E
4063   
4064      P:000B30 P:000B32 0140C5            CMP     #'DA',A
                            004441
4065      P:000B32 P:000B34 0AF0AA            JEQ     CHECK_FOR_DATA__BUFFER_DATA
                            000B51
4066   
4067      
4068      P:000B34 P:000B36 50F000            MOVE              X:PTYPE_FAILS,A0
                            000058
4069      P:000B36 P:000B38 000008            INC     A
4070      P:000B37 P:000B39 000000            NOP
4071      P:000B38 P:000B3A 507000            MOVE              A0,X:PTYPE_FAILS
                            000058
4072      P:000B3A P:000B3C 0BF080            JSR     RESET_FIFO
                            000B76
4073   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 79



4074                                CHECK_FOR_DATA_EXIT
4075      P:000B3C P:000B3E 0A8505            BCLR    #DCTR_HF5,X:DCTR
4076      P:000B3D P:000B3F 00000C            RTS
4077   
4078   
4079                                CHECK_FOR_DATA__BUFFER_REPLY
4080      
4081      P:000B3E P:000B40 64F400            MOVE              #>MCE_PACKET_DUMP,R4
                            002000
4082   
4083      
4084      P:000B40 P:000B42 0A00A6            JSET    #COMM_MCEREP,X:STATUS,CHECK_FOR_DATA__BUFFER_REPLY1
                            000B4C
4085   
4086      
4087      P:000B42 P:000B44 0A0026            BSET    #COMM_MCEREP,X:STATUS
4088   
4089      
4090      P:000B43 P:000B45 60F400            MOVE              #>MCEHDR_TYPE,R0
                            0000A3
4091      P:000B45 P:000B47 64F400            MOVE              #>MCEREP_BUF,R4
                            000100
4092                                          .loop   #4
4094      P:000B49 P:000B4B 44D800            MOVE              X:(R0)+,X0
4095      P:000B4A P:000B4C 000000            NOP
4096      P:000B4B P:000B4D 4C5C00            MOVE                          X0,Y:(R4)+
4097                                          .endl
4099   
4100      
4101   
4102                                CHECK_FOR_DATA__BUFFER_REPLY1
4103      P:000B4C P:000B4E 44F000            MOVE              X:MCEHDR_SIZE,X0
                            0000A5
4104      P:000B4E P:000B50 0BF080            JSR     CHECK_FOR_DATA__BUFFER_LARGE
                            000B97
4105   
4106      P:000B50 P:000B52 0C0B3C            JMP     CHECK_FOR_DATA_EXIT
4107   
4108   
4109                                CHECK_FOR_DATA__BUFFER_DATA
4110      
4111      P:000B51 P:000B53 64F400            MOVE              #>MCE_PACKET_DUMP,R4
                            002000
4112   
4113      
4114      P:000B53 P:000B55 0A00A7            JSET    #COMM_MCEDATA,X:STATUS,CHECK_FOR_DATA__BUFFER_DATA1
                            000B58
4115   
4116      
4117      P:000B55 P:000B57 0A0027            BSET    #COMM_MCEDATA,X:STATUS
4118      P:000B56 P:000B58 64F400            MOVE              #MCEDATA_BUF,R4
                            000200
4119   
4120                                CHECK_FOR_DATA__BUFFER_DATA1
4121      
4122      P:000B58 P:000B5A 50F000            MOVE              X:DA_COUNT,A0
                            000059
4123      P:000B5A P:000B5C 000008            INC     A
4124      P:000B5B P:000B5D 000000            NOP
4125      P:000B5C P:000B5E 507000            MOVE              A0,X:DA_COUNT
                            000059
4126   
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 80



4127      
4128      P:000B5E P:000B60 44F000            MOVE              X:MCEHDR_SIZE,X0
                            0000A5
4129   
4130      P:000B60 P:000B62 0BF080            JSR     CHECK_FOR_DATA__BUFFER_LARGE
                            000B97
4131   
4132      
4133      P:000B62 P:000B64 44F400            MOVE              #$af000,X0
                            0AF000
4134      P:000B64 P:000B66 4C6400            MOVE                          X0,Y:(R4)
4135   
4136      P:000B65 P:000B67 0C0B3C            JMP     CHECK_FOR_DATA_EXIT
4137   
4139                                CHECK_FOR_DATA__BUFFER
4140      
4141      
4142      
4143      
4144      
4145      
4146   
4147                                          .loop   #2
4149                                          .loop   X0
4151      P:000B6A P:000B6C 01AD80            JCLR    #EF,X:PDRD,*
                            000B6A
4152      P:000B6C P:000B6E 094E3F            MOVEP             Y:RDFIFO,A
4153      P:000B6D P:000B6F 0140C6            AND     #>$00ffff,A
                            00FFFF
4154      P:000B6F P:000B71 000000            NOP
4155      P:000B70 P:000B72 5C5C00            MOVE                          A1,Y:(R4)+
4156                                          .endl
4158      P:000B71 P:000B73 000000            NOP
4159                                          .endl
4161   
4162      
4163      P:000B72 P:000B74 44F400            MOVE              #$ff1111,X0
                            FF1111
4164      P:000B74 P:000B76 4C6400            MOVE                          X0,Y:(R4)
4165   
4166      P:000B75 P:000B77 00000C            RTS
4167   
4168   
4169   
4170                                ;----------------------------------------------
4171                                RESET_FIFO
4172                                ;----------------------------------------------
4173      
4174      P:000B76 P:000B78 63F000            MOVE              X:DEBUG_BUF_IDX,R3
                            000060
4175      P:000B78 P:000B7A 000000            NOP
4176      P:000B79 P:000B7B 000000            NOP
4177                                RESET_FIFO1
4178      P:000B7A P:000B7C 5C5B00            MOVE                          A1,Y:(R3)+
4179      P:000B7B P:000B7D 01AD80            JCLR    #EF,X:PDRD,RESET_FIFO2
                            000B80
4180      P:000B7D P:000B7F 5EF000            MOVE                          Y:RDFIFO,A
                            FFFFFF
4181      P:000B7F P:000B81 0C0B7A            JMP     RESET_FIFO1
4182   
4183                                RESET_FIFO2
4184      P:000B80 P:000B82 56F400            MOVE              #>$aa1122,A
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 81



                            AA1122
4185      P:000B82 P:000B84 000000            NOP
4186      P:000B83 P:000B85 5C5B00            MOVE                          A1,Y:(R3)+
4187      P:000B84 P:000B86 637000            MOVE              R3,X:DEBUG_BUF_IDX
                            000060
4188   
4189      
4190      P:000B86 P:000B88 50F000            MOVE              X:FIFO_FAILS,A0
                            000057
4191      P:000B88 P:000B8A 000008            INC     A
4192      P:000B89 P:000B8B 000000            NOP
4193      P:000B8A P:000B8C 507000            MOVE              A0,X:FIFO_FAILS
                            000057
4194      P:000B8C P:000B8E 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* briefly.
                            000018
4195      P:000B8E P:000B90 44F400            MOVE              #>25000,X0
                            0061A8
4196                                          .loop   X0
4198      P:000B92 P:000B94 000000            NOP
4199                                          .endl
4201      P:000B93 P:000B95 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
4202   
4203      P:000B95 P:000B97 0A8505            BCLR    #DCTR_HF5,X:DCTR
4204      P:000B96 P:000B98 00000C            RTS
4205   
4206   
4210   
4211                                ;---------------------------
4212                                CHECK_FOR_DATA__BUFFER_LARGE
4213                                ;---------------------------
4214      
4215      P:000B97 P:000B99 56A400            MOVE              X:TOTAL_BUFFS,A
4216      P:000B98 P:000B9A 014085            CMP     #0,A
4217      P:000B99 P:000B9B 0AF0AA            JEQ     FINISHED_BUFFS
                            000BB4
4218   
4219      P:000B9B P:000B9D 46F400            MOVE              #>$aa0000,Y0
                            AA0000
4220   
4221      
4222      
4223      
4224   
4225      P:000B9D P:000B9F 57F400            MOVE              #>$1,B
                            000001
4226      P:000B9F P:000BA1 0C1C99            ASR     #12,B,B                           ;B = 2**(24-12) = 2**12
4227   
4228      P:000BA0 P:000BA2 062400            DO      X:TOTAL_BUFFS,FINISHED_BUFFS
                            000BB3
4229      P:000BA2 P:000BA4 200013            CLR     A
4230      P:000BA3 P:000BA5 4E6400            MOVE                          Y0,Y:(R4)
4231                                BLOCK_FOR_HALF
4232      P:000BA4 P:000BA6 000008            INC     A
4233      P:000BA5 P:000BA7 20000D            CMP     A,B
4234      P:000BA6 P:000BA8 0AF0AF            JLE     GO_FOR_HALF                       ; I give up.
                            000BAE
4235      P:000BA8 P:000BAA 01ADA1            JSET    #HF,X:PDRD,BLOCK_FOR_HALF
                            000BA4
4236      P:000BAA P:000BAC 000000            NOP
4237      P:000BAB P:000BAD 000000            NOP
4238      P:000BAC P:000BAE 01ADA1            JSET    #HF,X:PDRD,BLOCK_FOR_HALF
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 82



                            000BA4
4239                                GO_FOR_HALF
4240                                          .loop   #512
4242      P:000BB0 P:000BB2 095CFF            MOVEP             Y:RDFIFO,Y:(R4)+
4243                                          .endl
4245   
4246      P:000BB1 P:000BB3 000000            NOP
4247      P:000BB2 P:000BB4 0D084F            JSR     TIMERX_STORE
4248      P:000BB3 P:000BB5 000000            NOP
4249   
4250                                FINISHED_BUFFS
4251      
4252   
4253      
4254      
4255      P:000BB4 P:000BB6 01ADA1            JSET    #HF,X:PDRD,BUFFER_PACKET_SINGLES_TIMED
                            000BBE
4256   
4257      
4258                                          .loop   X:LEFT_TO_READ
4260      P:000BB8 P:000BBA 095CFF            MOVEP             Y:RDFIFO,Y:(R4)+
4261                                          .endl
4263   
4264      P:000BB9 P:000BBB 0D084F            JSR     TIMERX_STORE
4265   
4266      P:000BBA P:000BBC 46F400            MOVE              #>$ab0000,Y0
                            AB0000
4267      P:000BBC P:000BBE 4E6400            MOVE                          Y0,Y:(R4)   ; Where was we?
4268      P:000BBD P:000BBF 00000C            RTS
4269   
4270                                BUFFER_PACKET_SINGLES_TIMED
4271      
4272      P:000BBE P:000BC0 200013            CLR     A
4273      P:000BBF P:000BC1 20001B            CLR     B
4274      P:000BC0 P:000BC2 51F000            MOVE              X:TCR0,B0               ; Store timer value (50 MHz)
                            FFFF8C
4275      P:000BC2 P:000BC4 46F400            MOVE              #>$ac0000,Y0
                            AC0000
4276      P:000BC4 P:000BC6 0C1C85            ASR     #2,B,B                            ; / 4
4277                                          .loop   X:LEFT_TO_READ
4279                                BUFFER_PACKET_SINGLES_WAIT_X
4280      P:000BC7 P:000BC9 50F000            MOVE              X:TCR0,A0
                            FFFF8C
4281      P:000BC9 P:000BCB 0C1C04            ASR     #2,A,A
4282      P:000BCA P:000BCC 20000D            CMP     A,B
4283      P:000BCB P:000BCD 0EABC7            JEQ     BUFFER_PACKET_SINGLES_WAIT_X
4284      P:000BCC P:000BCE 095CFF            MOVEP             Y:RDFIFO,Y:(R4)+
4285      P:000BCD P:000BCF 4E6400            MOVE                          Y0,Y:(R4)   ; Where was we?
4286      P:000BCE P:000BD0 0C1D01            ASL     #0,A,B                            ; MOVE A,B
4287                                          .endl
4289      P:000BCF P:000BD1 000000            NOP
4290      P:000BD0 P:000BD2 000000            NOP
4291      P:000BD1 P:000BD3 0D084F            JSR     TIMERX_STORE
4292      P:000BD2 P:000BD4 00000C            RTS
4293   
4294                                BUFFER_PACKET_SINGLES_POLLED
4295                                          .loop   X:LEFT_TO_READ
4297      P:000BD5 P:000BD7 01AD80            JCLR    #EF,X:PDRD,*
                            000BD5
4298      P:000BD7 P:000BD9 095CFF            MOVEP             Y:RDFIFO,Y:(R4)+
4299                                          .endl
4301      P:000BD8 P:000BDA 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   13-09-20  01:16:05  hacking.asm  Page 83



4302      P:000BD9 P:000BDB 000000            NOP
4303      P:000BDA P:000BDC 00000C            RTS
4304   
4305      000BDD                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM

0    Errors
3    Warnings


