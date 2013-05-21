Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  build.asm  Page 1



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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  header.asm  Page 2



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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  header.asm  Page 3



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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  header.asm  Page 4



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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  header.asm  Page 5



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
286  d    P:000000 P:000000 000A10            DC      END_ADR-INIT-2                    ; Number of boot words
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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  init.asm  Page 6



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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  init.asm  Page 7



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
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  init.asm  Page 8



361    
362       
363       
364       
365       
366       P:000068 P:00006A 0BF080            JSR     PROCESS_PC_CMD_INT                ; PCI slave req vector
                            000922
367    
368                                 ;**************************************************************************
369                                 ; Check for program space overwriting of ISR starting at P:$72
370                                           IF      @CVS(N,*)>$71
372                                           ENDIF
373    
374       P:000072 P:000074                   ORG     P:$72,P:$74
375    
376                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
377                                 ; command converter
378                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
380                                           ENDIF
381    
382    
383                                 ;**************************************************************************
384    
385                                 ; Three non-maskable fast interrupt service routines for clearing PCI interrupts
386                                 ; The Host will use these to clear the INTA* after it has serviced the interrupt
387                                 ; which had been generated by the PCI board.
388    
389       P:000072 P:000074 0A8506            BCLR    #INTA,X:DCTR                      ; $72/3 - Clear PCI interrupt
390       P:000073 P:000075 000000            NOP
391    
392       P:000074 P:000076 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; clear interrupt flag
393       P:000075 P:000077 000000            NOP                                       ; needs to be fast addressing <
394    
395       P:000076 P:000078 0A0022            BSET    #FATAL_ERROR,X:<STATUS            ; $76/7 - driver PCI_MESSAGE_TO_HOST error
396       P:000077 P:000079 000000            NOP
397    
398                                 ; Interrupt locations for 7 available commands on PCI board
399                                 ; Each JSR takes up 2 locations in the table
400       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            000349
401       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            00031F
402       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            00036A
403       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            000373
404                                 ; software reset is the same as cleaning up the PCI - use same routine
405                                 ; when HOST does a RESET then this routine is run
406       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            00044B
407       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            00045C
408       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            00043C
409       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            00037E
410    
411                                 ; QT - set command
412       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            00039C
413       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            000434
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  init.asm  Page 9



414    
415                                 ; Quiet RP mode, clear buffer full flag
416       P:00008C P:00008E 0A0015            BCLR    #RP_BUFFER_FULL,X:<STATUS         ; $8C
417       P:00008D P:00008F 000000            NOP
418    
419                                 ; ***********************************************************************
420                                 ; For now have boot code starting from P:$100
421                                 ; just to make debugging tidier etc.
422    
423       P:000100 P:000102                   ORG     P:$100,P:$102
424    
425                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
426                                 ; command converter
427                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
429                                           ENDIF
430                                 ; ***********************************************************************
431    
432    
433    
434                                 ; ******************************************************************
435                                 ;
436                                 ;       AA0 = RDFIFO* of incoming fiber optic data
437                                 ;       AA1 = EEPROM access
438                                 ;       AA2 = DRAM access
439                                 ;       AA3 = output to parallel data connector, for a video pixel clock
440                                 ;       $FFxxxx = Write to fiber optic transmitter
441                                 ;
442                                 ; ******************************************************************
443    
444    
445       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
446       P:000102 P:000104 08F485            MOVEP             #>$100000,X:DCTR        ; Set PCI mode
                            100000
447       P:000104 P:000106 000000            NOP
448       P:000105 P:000107 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
449       P:000106 P:000108 000000            NOP
450       P:000107 P:000109 000000            NOP                                       ; End of PCI programming
451    
452    
453                                 ; Set operation mode register OMR to normal expanded
454       P:000108 P:00010A 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
455       P:000109 P:00010B 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
456    
457                                 ; Program the serial port ESSI0 = Port C for serial transmission to
458                                 ;   the timing board
459       P:00010A P:00010C 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
460                                 ;**********************************************************************
461       P:00010C P:00010E 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
462                                                                                     ; DC0-CD4 = 0 for non-network operation
463                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
464                                                                                     ; SSC1 = 0 for SC1 not used
465                                 ;************************************************************************
466       P:00010E P:000110 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
                            010120
467                                                                                     ; SHFD = 0 for MSB shifted first
468                                                                                     ; CKP = 0 for rising clock edge transitions
469                                                                                     ; TE0 = 1 to enable transmitter #0
470                                                                                     ; MOD = 0 for normal, non-networked mode
471                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  init.asm  Page 10



472       P:000110 P:000112 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
473                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
474                                 ;********************************************************************************
475       P:000112 P:000114 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
476       P:000114 P:000116 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
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
492       P:000116 P:000118 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
493       P:000118 P:00011A 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            00001C
494       P:00011A P:00011C 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
495       P:00011C P:00011E 060AA0            REP     #10
496       P:00011D P:00011F 000000            NOP
497       P:00011E P:000120 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
498                                                                                     ; was %011100
499    
500                                 ; Program the SCI port to benign values
501       P:000120 P:000122 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
502       P:000122 P:000124 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
503       P:000124 P:000126 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
504                                 ;       PE0 = RXD
505                                 ;       PE1 = TXD
506                                 ;       PE2 = SCLK
507    
508                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
509       P:000126 P:000128 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
510       P:000128 P:00012A 07F40B            MOVEP             #$2800,X:TCSR1
                            002800
511       P:00012A P:00012C 07F407            MOVEP             #$2800,X:TCSR2
                            002800
512    
513    
514                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
515       P:00012C P:00012E 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
516       P:00012E P:000130 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  init.asm  Page 11



e
                            008929
517       P:000130 P:000132 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
                            000122
518    
519    
520                                 ; Program the DRAM memory access and addressing
521       P:000132 P:000134 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
                            020022
522       P:000134 P:000136 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
523    
524    
525                                 ; Clear all PCI error conditions
526       P:000136 P:000138 084E0A            MOVEP             X:DPSR,A
527       P:000137 P:000139 0140C2            OR      #$1FE,A
                            0001FE
528       P:000139 P:00013B 000000            NOP
529       P:00013A P:00013C 08CE0A            MOVEP             A,X:DPSR
530    
531                                 ; Status word and interrupt configuration.
532       P:00013B P:00013D 08F4BF            MOVEP             #>MY_IPRC,X:IPRC
                            0001C0
533       P:00013D P:00013F 08F4BE            MOVEP             #>MY_IPRP,X:IPRP
                            000002
534       P:00013F P:000141 05F439            MOVE              #>MY_SR,SR
                            000100
535    
536    
537                                 ;--------------------------------------------------------------------------
538                                 ; Initialize the fiber optic serial transmitter to zero
539       P:000141 P:000143 01B786            JCLR    #TDE,X:SSISR0,*
                            000141
540       P:000143 P:000145 07F43C            MOVEP             #$000000,X:TX00
                            000000
541    
542                                 ;--------------------------------------------------------------------
543    
544                                 ; clear DTXM - PCI master transmitter
545       P:000145 P:000147 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
546       P:000146 P:000148 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000146
547    
548                                 ;----------------------------------------------------------------------
549                                 ; clear DRXR - PCI receiver
550    
551       P:000148 P:00014A 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014D
552       P:00014A P:00014C 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
553       P:00014B P:00014D 000000            NOP
554       P:00014C P:00014E 0C0148            JMP     <CLR0
555                                 CLR1
556    
557                                 ;-----------------------------------------------------------------------------
558                                 ; copy parameter table from P memory into X memory
559    
560                                 ; but not frame_count and num_dumped - don't want these reset by fatal error...
561                                 ; they will be reset by new packet or pci_reset ISR
562    
563       P:00014D P:00014F 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000006
564       P:00014F P:000151 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  init.asm  Page 12



                            000002
565    
566                                 ; Move the table of constants from P: space to X: space
567       P:000151 P:000153 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            0006AC
568       P:000153 P:000155 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
569       P:000154 P:000156 069880            DO      #VAR_TBL_LENGTH,X_WRITE
                            000157
570       P:000156 P:000158 07D984            MOVE              P:(R1)+,X0
571       P:000157 P:000159 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
572                                 X_WRITE
573    
574       P:000158 P:00015A 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
                            000006
575       P:00015A P:00015C 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000002
576    
577    
578                                 ;----------------------------------------------------------------------------
579                                 ; Initialize PCI controller again, after booting, to make sure it sticks
580    
581       P:00015C P:00015E 08F485            MOVEP             #>$000000,X:DCTR
                            000000
582       P:00015E P:000160 000000            NOP
583       P:00015F P:000161 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00015F
584       P:000161 P:000163 08F485            MOVEP             #>$100000,X:DCTR
                            100000
585       P:000163 P:000165 000000            NOP
586       P:000164 P:000166 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000164
587    
588       
589       P:000166 P:000168 0BF080            JSR     CLEAR_FO_FIFO                     ; Clear the fibre fifo!
                            0004B4
590       P:000168 P:00016A 013D24            BSET    #AUX1,X:PDRC                      ; Enable byte-swapping - still necc. on ARC-
64
591       P:000169 P:00016B 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host interrupts
592       P:00016A P:00016C 0BF080            JSR     TIMER_DEFAULT                     ; Enable timer (channel 0) for misc. uses
                            00062E
593       P:00016C P:00016E 0BF080            JSR     TIMER_STORE_INIT                  ; Initialize timing buffer
                            000646
594    
596                                           INCLUDE 'main.asm'
597    
598                                                 COMMENT *
599    
600                                 Main section of the pci card code.
601    
602                                 See info.asm for versioning and authors.
603    
604                                         *
605                                           PAGE    132                               ; Printronix page width - 132 columns
606                                           OPT     CEX                               ; print DC evaluations
607    
608    
609    
613    
614                                 PACKET_IN
615    
616       
617       P:00016E P:000170 0A0017            BCLR    #MAIN_LOOP_POLL,X:<STATUS
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 13



618    
619       
620       P:00016F P:000171 0A00B6            JSET    #FREEZER,X:<STATUS,PACKET_IN
                            00016E
621    
622       
623       P:000171 P:000173 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
624    
625       
626       P:000173 P:000175 0A01A0            JSET    #MODE_APPLICATION,X:<MODE,APPLICATION
                            000800
627    
628       
629       P:000175 P:000177 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            000636
630    
631       
632       P:000177 P:000179 0B00B4            JSSET   #QT_FLUSH,X:STATUS,BUFFER_INFORM
                            000682
633    
634       
635       P:000179 P:00017B 0D0470            JSR     <CHECK_FO
636       P:00017A P:00017C 0B00A3            JSSET   #FO_WRD_RCV,X:STATUS,HANDLE_FIFO
                            00018A
637    
638       
639       P:00017C P:00017E 0B00AB            JSSET   #CON_MCE,X:STATUS,CON_TRANSMIT
                            000288
640       P:00017E P:000180 0B00AA            JSSET   #CON_DEMAND,X:STATUS,CON_BUFFER
                            00026B
641    
642       
643       P:000180 P:000182 0BF080            JSR     HACK_ENTRY
                            000810
644    
645       
646       P:000182 P:000184 0C016E            JMP     PACKET_IN
647    
651    
652                                 ; PCI semaphore
653                                 ;
654                                 ; In order for routines in non-interrupt context to write to the
655                                 ; DTXS, (via PCI_MESSAGE_TO_HOST) they need to end up with
656                                 ; interrupts disabled and HCF3 cleared.
657                                 ;
658                                 ; Non-interrupt PCIers should use macro
659                                 ;       PCI_LOCKDOWN
660                                 ; to get exclusive access and then release it with
661                                 ;       PCI_LOCKUP
662                                 ; after calling PCI_MESSAGE_TO_HOST.
663    
664                                  PCI_LOCKDOWN
665                                           MACRO
666  m                                        JSR     PCI_LOCKDOWN_ENTRY
667  m                                        ENDM
668    
669                                 PCI_LOCKUP MACRO
670  m                                        BSET    #DCTR_HCIE,X:DCTR                 ; Enable host interrupts
671  m                                        ENDM
672    
673    
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 14



674                                 PCI_LOCKDOWN_AGAIN
675       P:000183 P:000185 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Re-enable host IRQ
676       P:000184 P:000186 0632A0            REP     #50                               ; Delay for ~us
677       P:000185 P:000187 000000            NOP
678    
679                                 PCI_LOCKDOWN_ENTRY
680       
681       P:000186 P:000188 0A8500            BCLR    #DCTR_HCIE,X:DCTR                 ; Disable host IRQ
682       P:000187 P:000189 0A85A3            JSET    #DCTR_HF3,X:DCTR,PCI_LOCKDOWN_AGAIN
                            000183
683       P:000189 P:00018B 00000C            RTS
684    
685    
687    
688                                 HANDLE_FIFO
689       P:00018A P:00018C 54F400            MOVE              #>$A00,A1
                            000A00
690       P:00018C P:00018E 0BF080            JSR     TIMER_STORE_A1
                            00064F
691       P:00018E P:000190 0BF080            JSR     TIMER_STORE
                            00064D
692    
693       
694       P:000190 P:000192 60F400            MOVE              #>HEAD_W1_0,R0
                            00001B
695       P:000192 P:000194 44F400            MOVE              #>$00FFFF,X0            ; Mask lower 16 bits
                            00FFFF
696       P:000194 P:000196 220800            MOVE              R0,A0
697       P:000195 P:000197 060880            DO      #8,HANDLE_FIFO_CHECK_PREAMBLE
                            0001A0
698                                 HANDLE_FIFO_WAIT
699       P:000197 P:000199 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000197
700       P:000199 P:00019B 000000            NOP
701       P:00019A P:00019C 000000            NOP
702       P:00019B P:00019D 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000197
703       P:00019D P:00019F 094E3F            MOVEP             Y:RDFIFO,A
704       P:00019E P:0001A0 200046            AND     X0,A
705       P:00019F P:0001A1 000000            NOP
706       P:0001A0 P:0001A2 545800            MOVE              A1,X:(R0)+
707    
708                                 HANDLE_FIFO_CHECK_PREAMBLE
709       P:0001A1 P:0001A3 60F400            MOVE              #>HEAD_W1_0,R0
                            00001B
710       P:0001A3 P:0001A5 20001B            CLR     B
711       P:0001A4 P:0001A6 200013            CLR     A
712       P:0001A5 P:0001A7 57D800            MOVE              X:(R0)+,B
713       P:0001A6 P:0001A8 0140CD            CMP     #>$A5A5,B
                            00A5A5
714       P:0001A8 P:0001AA 0AF0A2            JNE     PRE_ERROR
                            0001CF
715       P:0001AA P:0001AC 57D800            MOVE              X:(R0)+,B
716       P:0001AB P:0001AD 0140CD            CMP     #>$A5A5,B
                            00A5A5
717       P:0001AD P:0001AF 0AF0A2            JNE     PRE_ERROR
                            0001CF
718       P:0001AF P:0001B1 57D800            MOVE              X:(R0)+,B
719       P:0001B0 P:0001B2 0140CD            CMP     #>$5A5A,B
                            005A5A
720       P:0001B2 P:0001B4 0AF0A2            JNE     PRE_ERROR
                            0001CF
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 15



721       P:0001B4 P:0001B6 57D800            MOVE              X:(R0)+,B
722       P:0001B5 P:0001B7 0140CD            CMP     #>$5A5A,B
                            005A5A
723       P:0001B7 P:0001B9 0AF0A2            JNE     PRE_ERROR
                            0001CF
724    
725       
726       P:0001B9 P:0001BB 50F000            MOVE              X:>(HEAD_W1_0+6),A0
                            000021
727       P:0001BB P:0001BD 44F000            MOVE              X:>(HEAD_W1_0+7),X0
                            000022
728       P:0001BD P:0001BF 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
729    
730       
731       P:0001BF P:0001C1 0BF080            JSR     PACKET_PARTITIONS
                            0005BE
732       P:0001C1 P:0001C3 0BF080            JSR     TIMER_STORE
                            00064D
733    
735       P:0001C3 P:0001C5 56F000            MOVE              X:HEAD_W3_0,A
                            00001F
736    
737       P:0001C5 P:0001C7 0140C5            CMP     #>'RP',A
                            005250
738       P:0001C7 P:0001C9 0AF0AA            JEQ     HANDLE_RP
                            0001E3
739    
740       P:0001C9 P:0001CB 0140C5            CMP     #>'DA',A
                            004441
741       P:0001CB P:0001CD 0AF0AA            JEQ     HANDLE_DA
                            00022A
742    
743       P:0001CD P:0001CF 0AF080            JMP     QT_PTYPE_ERROR
                            0001D5
744    
745                                 ; Error recording.
746    
747                                 PRE_ERROR
748       P:0001CF P:0001D1 60F400            MOVE              #>PREAMBLE_ERRORS,R0
                            000026
749       P:0001D1 P:0001D3 0BF080            JSR     INCR_X_R0
                            0001DE
750       P:0001D3 P:0001D5 0AF080            JMP     CLEAR_FO_FIFO                     ; empty the fifo (2 ms!)
                            0004B4
751    
752                                 QT_PTYPE_ERROR
753       P:0001D5 P:0001D7 60F400            MOVE              #>PTYPE_ERRORS,R0
                            000027
754       P:0001D7 P:0001D9 0AF080            JMP     INCR_X_R0
                            0001DE
755                                 QT_FSIZE_ERROR
756       P:0001D9 P:0001DB 60F400            MOVE              #>PSIZE_ERRORS,R0
                            000028
757       P:0001DB P:0001DD 0AF080            JMP     INCR_X_R0
                            0001DE
758                                 RETURN_NOW
759       P:0001DD P:0001DF 00000C            RTS
760    
761                                 INCR_X_R0
762       
763       P:0001DE P:0001E0 50E000            MOVE              X:(R0),A0
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 16



764       P:0001DF P:0001E1 000008            INC     A
765       P:0001E0 P:0001E2 000000            NOP
766       P:0001E1 P:0001E3 506000            MOVE              A0,X:(R0)
767       P:0001E2 P:0001E4 00000C            RTS
768    
769    
770    
773    
774                                 HANDLE_RP
775       
776       P:0001E3 P:0001E5 0A0183            JCLR    #MODE_RP_BUFFER,X:MODE,MCE_PACKET
                            0002A8
777    
778       
779       P:0001E5 P:0001E7 0A00B5            JSET    #RP_BUFFER_FULL,X:STATUS,HANDLE_RP_DROP
                            000225
780    
781       
782       P:0001E7 P:0001E9 61F400            MOVE              #>REPLY_BUFFER,R1
                            100000
783       P:0001E9 P:0001EB 0BF080            JSR     BUFFER_PACKET
                            0005CB
784    
785       P:0001EB P:0001ED 54F400            MOVE              #>$b00,A1
                            000B00
786       P:0001ED P:0001EF 0BF080            JSR     TIMER_STORE_A1
                            00064F
787       P:0001EF P:0001F1 0BF080            JSR     TIMER_STORE
                            00064D
788    
789       
790       P:0001F1 P:0001F3 60F400            MOVE              #RP_BASE_LO,R0
                            000048
791       P:0001F3 P:0001F5 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
792    
793       P:0001F5 P:0001F7 60F400            MOVE              #BURST_DEST_LO,R0
                            00002F
794       P:0001F7 P:0001F9 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
795    
796       
797       P:0001F9 P:0001FB 200013            CLR     A
798       P:0001FA P:0001FC 20001B            CLR     B
799       P:0001FB P:0001FD 50F000            MOVE              X:PACKET_SIZE,A0
                            000023
800       P:0001FD P:0001FF 0C1D04            ASL     #2,A,A                            ; Size in bytes
801       P:0001FE P:000200 51F000            MOVE              X:RP_MAX_SIZE,B0
                            00004A
802    
803       P:000200 P:000202 200005            CMP     B,A                               ; A ? B
804       P:000201 P:000203 0AF0AF            JLE     HANDLE_RP1
                            000204
805       P:000203 P:000205 21EE00            MOVE              B,A
806    
807                                 HANDLE_RP1
808       
809       P:000204 P:000206 44F400            MOVE              #>REPLY_BUFFER,X0
                            100000
810       P:000206 P:000208 507000            MOVE              A0,X:BLOCK_SIZE
                            00002B
811       P:000208 P:00020A 447000            MOVE              X0,X:YMEM_SRC
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 17



                            00002E
812       P:00020A P:00020C 0BF080            JSR     TIMER_STORE
                            00064D
813       P:00020C P:00020E 0BF080            JSR     BLOCK_TRANSFER
                            000518
814       P:00020E P:000210 0BF080            JSR     TIMER_STORE
                            00064D
815    
816       
817                                           PCI_LOCKDOWN                              ; Disable host IRQ
819       P:000211 P:000213 44F400            MOVE              #'NFY',X0
                            4E4659
820       P:000213 P:000215 447000            MOVE              X0,X:DTXS_WD1
                            00000B
821       P:000215 P:000217 44F400            MOVE              #'RPQ',X0
                            525051
822       P:000217 P:000219 447000            MOVE              X0,X:DTXS_WD2
                            00000C
823       P:000219 P:00021B 507000            MOVE              A0,X:DTXS_WD3           ; A0=block_size
                            00000D
824       P:00021B P:00021D 547000            MOVE              A1,X:DTXS_WD4           ; A1=0
                            00000E
825    
826       
827       P:00021D P:00021F 0A7035            BSET    #RP_BUFFER_FULL,X:STATUS
                            000000
828       P:00021F P:000221 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
829                                           PCI_LOCKUP                                ; Enable host IRQ
831    
832       P:000222 P:000224 0BF080            JSR     TIMER_STORE
                            00064D
833       P:000224 P:000226 00000C            RTS                                       ; Back to main loop
834    
835                                 HANDLE_RP_DROP
836       P:000225 P:000227 60F400            MOVE              #RP_DROPS,R0
                            00004B
837       P:000227 P:000229 0D01DE            JSR     INCR_X_R0
838       P:000228 P:00022A 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            00060E
839    
841    
842    
845    
846    
847                                 HANDLE_DA
848       
849       P:00022A P:00022C 60F400            MOVE              #FRAME_COUNT,R0
                            000002
850       P:00022C P:00022E 0D01DE            JSR     INCR_X_R0
851    
852       
853       P:00022D P:00022F 0A0182            JCLR    #MODE_QT,X:MODE,MCE_PACKET
                            0002A8
854    
855       
856       P:00022F P:000231 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
857       P:000231 P:000233 0BF080            JSR     BUFFER_PACKET
                            0005CB
858    
859       P:000233 P:000235 54F400            MOVE              #$e00,A1
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 18



                            000E00
860       P:000235 P:000237 0BF080            JSR     TIMER_STORE_A1
                            00064F
861       P:000237 P:000239 0BF080            JSR     TIMER_STORE
                            00064D
862    
863       
864       P:000239 P:00023B 56F000            MOVE              X:QT_BUF_HEAD,A
                            000042
865       P:00023B P:00023D 014180            ADD     #1,A
866       P:00023C P:00023E 57F000            MOVE              X:QT_BUF_MAX,B
                            00003F
867       P:00023E P:000240 20000D            CMP     A,B
868       P:00023F P:000241 0AF0A1            JGE     HANDLE_DA_MATH
                            000242
869       P:000241 P:000243 2E0000            MOVE              #0,A
870                                 HANDLE_DA_MATH
871       P:000242 P:000244 57F000            MOVE              X:QT_BUF_TAIL,B
                            000043
872       P:000244 P:000246 20000D            CMP     A,B
873       P:000245 P:000247 0AF0AA            JEQ     HANDLE_DA_DROP
                            000266
874    
875       
876       P:000247 P:000249 200013            CLR     A
877       P:000248 P:00024A 50F000            MOVE              X:PACKET_SIZE,A0
                            000023
878    
879       P:00024A P:00024C 014088            ADD     #0,B                              ; Clear carry
880       P:00024B P:00024D 0C1D04            ASL     #2,A,A                            ; Size, in bytes
881    
882       
883       P:00024C P:00024E 20001B            CLR     B
884       P:00024D P:00024F 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            000040
885       P:00024F P:000251 20000D            CMP     A,B
886       P:000250 P:000252 0E21D9            JNE     QT_FSIZE_ERROR
887    
888       
889       P:000251 P:000253 517000            MOVE              B0,X:BLOCK_SIZE
                            00002B
890       P:000253 P:000255 557000            MOVE              B1,X:YMEM_SRC           ; Y:0
                            00002E
891    
892       P:000255 P:000257 60F400            MOVE              #QT_DEST_LO,R0
                            000044
893       P:000257 P:000259 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
894       P:000259 P:00025B 60F400            MOVE              #BURST_DEST_LO,R0
                            00002F
895       P:00025B P:00025D 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
896    
897       
898       P:00025D P:00025F 0BF080            JSR     BLOCK_TRANSFER
                            000518
899    
900       P:00025F P:000261 0BF080            JSR     TIMER_STORE
                            00064D
901    
902       
903       P:000261 P:000263 0BF080            JSR     BUFFER_INCR
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 19



                            000657
904    
905       
906       P:000263 P:000265 0BF080            JSR     BUFFER_INFORM_CHECK
                            000675
907    
908       P:000265 P:000267 00000C            RTS
909    
910                                 HANDLE_DA_DROP
911       
912       P:000266 P:000268 60F400            MOVE              #QT_DROPS,R0
                            000047
913       P:000268 P:00026A 0D01DE            JSR     INCR_X_R0
914       P:000269 P:00026B 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            00060E
915    
917    
918    
919                                 ;----------------------------------------------
920                                 CON_BUFFER
921                                 ; This routine will copy an MCE command from the PC to Y memory.
922                                 ; The source RAM address has already been stored in CON_SRC_LO.
923                                 ; The destination address is always Y:COMMAND_BUFFER.
924                                 ;----------------------------------------------
925    
926       P:00026B P:00026D 54F400            MOVE              #>$C00,A1
                            000C00
927       P:00026D P:00026F 0BF080            JSR     TIMER_STORE_A1
                            00064F
928       P:00026F P:000271 0BF080            JSR     TIMER_STORE
                            00064D
929    
930       
931       P:000271 P:000273 60F400            MOVE              #>CON_SRC_LO,R0
                            00002C
932       P:000273 P:000275 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
933       P:000275 P:000277 60F400            MOVE              #>BURST_SRC_LO,R0
                            000031
934       P:000277 P:000279 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
935       P:000279 P:00027B 51F400            MOVE              #>COMMAND_BUFFER,B0
                            200000
936       P:00027B P:00027D 50F400            MOVE              #>256,A0
                            000100
937       P:00027D P:00027F 517000            MOVE              B0,X:YMEM_DEST
                            000033
938       P:00027F P:000281 507000            MOVE              A0,X:BLOCK_SIZE
                            00002B
939       P:000281 P:000283 0BF080            JSR     CON_TRANSFER
                            000552
940    
941       P:000283 P:000285 0A702B            BSET    #CON_MCE,X:STATUS
                            000000
942       P:000285 P:000287 0BF080            JSR     TIMER_STORE
                            00064D
943       P:000287 P:000289 00000C            RTS                                       ; Back to main loop
944    
945                                 ;----------------------------------------------
946                                 CON_TRANSMIT
947                                 ; This routine will copy the MCE command from Y:COMMAND_BUFFER to
948                                 ; the MCE command transmitter.
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 20



949                                 ;----------------------------------------------
950    
951       P:000288 P:00028A 0BF080            JSR     TIMER_STORE
                            00064D
952    
953       P:00028A P:00028C 66F400            MOVE              #>COMMAND_BUFFER,R6
                            200000
954       P:00028C P:00028E 068080            DO      #128,CON_TRANSMIT1                ; block size = 16bit x 128 (256 bytes)
                            000295
955       P:00028E P:000290 5CDE00            MOVE                          Y:(R6)+,A1  ; b2, b1  (lsb)
956       P:00028F P:000291 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
957       P:000290 P:000292 0140C6            AND     #>$FF,A
                            0000FF
958       P:000292 P:000294 547000            MOVE              A1,X:FO_SEND
                            FFF000
959       P:000294 P:000296 557000            MOVE              B1,X:FO_SEND
                            FFF000
960    
961                                 CON_TRANSMIT1
962       P:000296 P:000298 0A0121            BSET    #MODE_MCE,X:<MODE                 ; enable processing of MCE replies/data
963    
964       
965       P:000297 P:000299 0A700B            BCLR    #CON_MCE,X:STATUS
                            000000
966       P:000299 P:00029B 0A700A            BCLR    #CON_DEMAND,X:STATUS
                            000000
967    
968       P:00029B P:00029D 0BF080            JSR     TIMER_STORE
                            00064D
969    
970       
971                                           PCI_LOCKDOWN
973       P:00029E P:0002A0 44F400            MOVE              #'CON',X0
                            434F4E
974       P:0002A0 P:0002A2 0BF080            JSR     VCOM_PREPARE_REPLY
                            0002E9
975       P:0002A2 P:0002A4 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
976                                           PCI_LOCKUP                                ; Enable host IRQ
978    
979       P:0002A5 P:0002A7 0BF080            JSR     TIMER_STORE
                            00064D
980       P:0002A7 P:0002A9 00000C            RTS                                       ; Back to main loop
981    
982    
983    
984    
986    
987                                 ; --------------------------------------------------------------------------
988                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
989                                 ; --------------------------------------------------------------------------
990    
991                                 ; prepare notify to inform host that a packet has arrived.
992    
993                                 MCE_PACKET
994                                           PCI_LOCKDOWN                              ; Disable host IRQ
996       P:0002A9 P:0002AB 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
997    
998       P:0002AA P:0002AC 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 21



999       P:0002AC P:0002AE 440B00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
1000   
1001      P:0002AD P:0002AF 449F00            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
1002      P:0002AE P:0002B0 440C00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
1003   
1004      P:0002AF P:0002B1 44A100            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
1005      P:0002B0 P:0002B2 440D00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
1006   
1007      P:0002B1 P:0002B3 44A200            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
1008      P:0002B2 P:0002B4 440E00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sage
1009   
1010      
1011      P:0002B3 P:0002B5 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
1012      P:0002B4 P:0002B6 0D047A            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
1013      P:0002B5 P:0002B7 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
1014                                          PCI_LOCKUP
1016   
1017      P:0002B7 P:0002B9 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
1018      P:0002B9 P:0002BB 0BF080            JSR     BUFFER_PACKET
                            0005CB
1019   
1020      
1021   
1022      P:0002BB P:0002BD 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; on fatal error, re-init.
                            000100
1023      P:0002BD P:0002BF 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; Set in 'send_packet_to_host' ISR
                            0002BB
1024   
1025      
1026      P:0002BF P:0002C1 44F400            MOVE              #>IMAGE_BUFFER,X0
                            000000
1027      P:0002C1 P:0002C3 56F000            MOVE              X:PACKET_SIZE,A
                            000023
1028      P:0002C3 P:0002C5 0C1D04            ASL     #2,A,A
1029      P:0002C4 P:0002C6 447000            MOVE              X0,X:YMEM_SRC
                            00002E
1030      P:0002C6 P:0002C8 547000            MOVE              A1,X:BLOCK_SIZE
                            00002B
1031      P:0002C8 P:0002CA 0BF080            JSR     BLOCK_TRANSFER
                            000518
1032   
1033      P:0002CA P:0002CC 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
1034   
1035      
1036                                          PCI_LOCKDOWN                              ; Disable host IRQ
1038      P:0002CD P:0002CF 44F400            MOVE              #'HST',X0
                            485354
1039      P:0002CF P:0002D1 0BF080            JSR     VCOM_PREPARE_REPLY
                            0002E9
1040      P:0002D1 P:0002D3 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1041                                          PCI_LOCKUP                                ; Enable host IRQ
1043      P:0002D4 P:0002D6 00000C            RTS
1044   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 22



1045                                ;----------------------------------------------------------
1046                                ; clear out the fifo after an HST timeout...
1047                                ;----------------------------------------------------------
1048   
1049                                DUMP_FIFO
1050      P:0002D5 P:0002D7 61F400            MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
1051      P:0002D7 P:0002D9 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
1052      P:0002D9 P:0002DB 200013            CLR     A
1053      P:0002DA P:0002DC 320000            MOVE              #0,R2                   ; use R2 as a dump count
1054                                NEXT_DUMP
1055      P:0002DB P:0002DD 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E6
1056      P:0002DD P:0002DF 000000            NOP
1057      P:0002DE P:0002E0 000000            NOP
1058      P:0002DF P:0002E1 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E6
1059   
1060      P:0002E1 P:0002E3 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
1061      P:0002E2 P:0002E4 205A00            MOVE              (R2)+                   ; inc dump count
1062      P:0002E3 P:0002E5 224E00            MOVE              R2,A                    ;
1063      P:0002E4 P:0002E6 200045            CMP     X0,A                              ; check we've not hit dump limit
1064      P:0002E5 P:0002E7 0E22DB            JNE     NEXT_DUMP                         ; not hit limit?
1065                                FIFO_EMPTY
1066      P:0002E6 P:0002E8 627000            MOVE              R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000006
1067      P:0002E8 P:0002EA 0C0100            JMP     <START                            ; re-initialise
1068   
1069   
1070                                ; -------------------------------------------------------------------------------------
1071                                ;                              END OF MAIN PACKET HANDLING CODE
1072                                ; -------------------------------------------------------------------------------------
1073   
1074   
1075   
1076                                ; -------------------------------------------------------------------------------------
1077                                ;
1078                                ;                              INTERRUPT SERVICE ROUTINES
1079                                ;
1080                                ; -------------------------------------------------------------------------------------
1081   
1082                                ; ---------------
1083                                ; Rules:  Don't use N#, or any R# except R0 unless you add them to the saved register set.
1084   
1085   
1086                                ; ----------------------------------------------------------------------------
1087                                ; VCOM_* - routines: utility functions for hosty command vector communication.
1088                                ;-----------------------------------------------------------------------------
1089   
1090   
1091                                ; VCOM_PREPARE_REPLY
1092                                ;
1093                                ; Prepare the reply packet, using X0 as the command name (second word).  The
1094                                ; message defaults to 'ACK' with NULL data.  The user may subsequenty fill in
1095                                ; the data field (word 4) and mark the packet as error if necessary.
1096   
1097                                VCOM_PREPARE_REPLY
1098      
1099      
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 23



1100      P:0002E9 P:0002EB 50F400            MOVE              #'REP',A0
                            524550
1101      P:0002EB P:0002ED 447000            MOVE              X0,X:DTXS_WD2           ; Command
                            00000C
1102      P:0002ED P:0002EF 507000            MOVE              A0,X:DTXS_WD1
                            00000B
1103   
1104      P:0002EF P:0002F1 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
                            41434B
1105      P:0002F1 P:0002F3 000000            NOP
1106      P:0002F2 P:0002F4 547000            MOVE              A1,X:DTXS_WD3           ; ACK
                            00000D
1107      P:0002F4 P:0002F6 507000            MOVE              A0,X:DTXS_WD4           ; no comment
                            00000E
1108      P:0002F6 P:0002F8 00000C            RTS
1109   
1110   
1111                                ; VCOM_CHECK
1112                                ;
1113                                ; Compares DRXR_WD1 to X0.  If they are equal, Z is set on return.  If they
1114                                ; are not equal then Z is cleared and the reply will be marked as ERR with
1115                                ; 'CNE' in the last word.
1116                                ; Trashes A and B always and X0 on error.
1117   
1118                                VCOM_CHECK
1119      P:0002F7 P:0002F9 208E00            MOVE              X0,A
1120      P:0002F8 P:0002FA 57F000            MOVE              X:DRXR_WD1,B
                            000007
1121      P:0002FA P:0002FC 20000D            CMP     A,B
1122      P:0002FB P:0002FD 0AF0AA            JEQ     VCOM_RTS
                            000305
1123   
1124      P:0002FD P:0002FF 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
1125      P:0002FF P:000301 50F400            MOVE              #'ERR',A0
                            455252
1126      P:000301 P:000303 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1127      P:000303 P:000305 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1128                                VCOM_RTS
1129      P:000305 P:000307 00000C            RTS
1130   
1131   
1132                                ; VCOM_INTRO
1133                                ;
1134                                ; Read DSP command from DRXR.  Prepare the reply packet and verify that it
1135                                ; matches the key in X1.  If it does not, mark the reply as error and set
1136                                ; the Z flag.
1137   
1138                                VCOM_INTRO
1139      P:000306 P:000308 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            000318
1140      P:000308 P:00030A 20A400            MOVE              X1,X0
1141      P:000309 P:00030B 0D02E9            JSR     VCOM_PREPARE_REPLY
1142      P:00030A P:00030C 0D02F7            JSR     VCOM_CHECK
1143      P:00030B P:00030D 00000C            RTS
1144   
1145   
1146                                ; VCOM_EXIT_ERROR_X0
1147                                ; VCOM_EXIT_X0
1148                                ; VCOM_EXIT
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 24



1149                                ;
1150                                ; For returning from host command vector interrupts only.  These three
1151                                ; routines do the following (respectively):
1152                                ; a) Mark reply as error, then (b)
1153                                ; b) Put X0 into last word of reply, then (c)
1154                                ; c) Restore registers and RTI.
1155   
1156                                VCOM_EXIT_ERROR_X0
1157      P:00030C P:00030E 50F400            MOVE              #'ERR',A0
                            455252
1158      P:00030E P:000310 000000            NOP
1159      P:00030F P:000311 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1160                                VCOM_EXIT_X0
1161      P:000311 P:000313 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1162                                VCOM_EXIT
1163      P:000313 P:000315 0BF080            JSR     RESTORE_REGISTERS
                            000493
1164      P:000315 P:000317 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1165      P:000317 P:000319 000004            RTI
1166   
1167   
1168                                ;---------------------------------------------------------------
1169                                RD_DRXR
1170                                ;--------------------------------------------------------------
1171                                ; Routine to read from HTXR-DRXR data path.  For HCTR = 0x900,
1172                                ; 3 LSB of each 32-bit word written by the host is returned on
1173                                ; each read.  This only polls for first word, not all of them.
1174      P:000318 P:00031A 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            000318
1175      P:00031A P:00031C 63F400            MOVE              #DRXR_WD1,R3
                            000007
1176      P:00031C P:00031E 0604A0            REP     #4
1177      P:00031D P:00031F 085B8B            MOVEP             X:DRXR,X:(R3)+
1178      P:00031E P:000320 00000C            RTS
1179   
1180   
1181                                ; ----------------------------------------------------------------------------
1182                                READ_MEMORY
1183                                ;-----------------------------------------------------------------------------
1184                                ;Read command:
1185                                ; word 1 = command = 'RDM'
1186                                ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
1187                                ; word 3 = address in memory
1188                                ; word 4 = not used
1189                                ;Version query:
1190                                ; word 1 = 'VER'
1191                                ; word 2-4 unused
1192   
1193      P:00031F P:000321 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1194      P:000321 P:000323 0D0318            JSR     RD_DRXR                           ; Loads DRXR_WD*
1195   
1196      P:000322 P:000324 44F400            MOVE              #'RDM',X0
                            52444D
1197      P:000324 P:000326 0D02E9            JSR     VCOM_PREPARE_REPLY
1198      P:000325 P:000327 0D02F7            JSR     VCOM_CHECK
1199      P:000326 P:000328 0AF0AA            JEQ     READ_MEMORY_XYP
                            000330
1200   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 25



1201      
1202      P:000328 P:00032A 44F400            MOVE              #'VER',X0
                            564552
1203      P:00032A P:00032C 0D02E9            JSR     VCOM_PREPARE_REPLY
1204      P:00032B P:00032D 0D02F7            JSR     VCOM_CHECK
1205      P:00032C P:00032E 0E2313            JNE     VCOM_EXIT
1206   
1207      P:00032D P:00032F 44F000            MOVE              X:REV_NUMBER,X0
                            000003
1208      P:00032F P:000331 0C0311            JMP     VCOM_EXIT_X0
1209   
1210                                READ_MEMORY_XYP
1211   
1212      
1213      P:000330 P:000332 56F000            MOVE              X:DRXR_WD2,A
                            000008
1214      P:000332 P:000334 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1215   
1216      P:000334 P:000336 0140C5            CMP     #'_X',A
                            005F58
1217      P:000336 P:000338 0AF0AA            JEQ     READ_MEMORY_X
                            000343
1218   
1219      P:000338 P:00033A 0140C5            CMP     #'_Y',A
                            005F59
1220      P:00033A P:00033C 0AF0AA            JEQ     READ_MEMORY_Y
                            000345
1221   
1222      P:00033C P:00033E 0140C5            CMP     #'_P',A
                            005F50
1223      P:00033E P:000340 0AF0AA            JEQ     READ_MEMORY_P
                            000347
1224   
1225      P:000340 P:000342 44F400            MOVE              #'MTE',X0
                            4D5445
1226      P:000342 P:000344 0C030C            JMP     VCOM_EXIT_ERROR_X0
1227   
1228                                READ_MEMORY_X
1229      P:000343 P:000345 44E000            MOVE              X:(R0),X0
1230      P:000344 P:000346 0C0311            JMP     VCOM_EXIT_X0
1231                                READ_MEMORY_Y
1232      P:000345 P:000347 4CE000            MOVE                          Y:(R0),X0
1233      P:000346 P:000348 0C0311            JMP     VCOM_EXIT_X0
1234                                READ_MEMORY_P
1235      P:000347 P:000349 07E084            MOVE              P:(R0),X0
1236      P:000348 P:00034A 0C0311            JMP     VCOM_EXIT_X0
1237   
1238   
1239                                ;--------------------------------------------------------------
1240                                WRITE_MEMORY
1241                                ;---------------------------------------------------------------
1242                                ; word 1 = command = 'WRM'
1243                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1244                                ; word 3 = address in memory
1245                                ; word 4 = value
1246   
1247      P:000349 P:00034B 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1248      P:00034B P:00034D 45F400            MOVE              #'WRM',X1
                            57524D
1249      P:00034D P:00034F 0D0306            JSR     VCOM_INTRO
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 26



1250      P:00034E P:000350 0E2313            JNE     VCOM_EXIT
1251   
1252      
1253      P:00034F P:000351 56F000            MOVE              X:DRXR_WD2,A
                            000008
1254      P:000351 P:000353 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1255      P:000353 P:000355 44F000            MOVE              X:DRXR_WD4,X0
                            00000A
1256   
1257      P:000355 P:000357 0140C5            CMP     #'_X',A
                            005F58
1258      P:000357 P:000359 0AF0AA            JEQ     WRITE_MEMORY_X
                            000364
1259   
1260      P:000359 P:00035B 0140C5            CMP     #'_Y',A
                            005F59
1261      P:00035B P:00035D 0AF0AA            JEQ     WRITE_MEMORY_Y
                            000366
1262   
1263      P:00035D P:00035F 0140C5            CMP     #'_P',A
                            005F50
1264      P:00035F P:000361 0AF0AA            JEQ     WRITE_MEMORY_P
                            000368
1265   
1266      P:000361 P:000363 44F400            MOVE              #'MTE',X0
                            4D5445
1267      P:000363 P:000365 0C030C            JMP     VCOM_EXIT_ERROR_X0
1268   
1269                                WRITE_MEMORY_X
1270      P:000364 P:000366 446000            MOVE              X0,X:(R0)
1271      P:000365 P:000367 0C0311            JMP     VCOM_EXIT_X0
1272                                WRITE_MEMORY_Y
1273      P:000366 P:000368 4C6000            MOVE                          X0,Y:(R0)
1274      P:000367 P:000369 0C0311            JMP     VCOM_EXIT_X0
1275                                WRITE_MEMORY_P
1276      P:000368 P:00036A 076084            MOVE              X0,P:(R0)
1277      P:000369 P:00036B 0C0311            JMP     VCOM_EXIT_X0
1278   
1279   
1280                                ;-----------------------------------------------------------------------------
1281                                START_APPLICATION
1282                                ; an application should already have been downloaded to the PCI memory.
1283                                ; this command will execute it.
1284                                ; ----------------------------------------------------------------------
1285                                ; word 1 = command = 'GOA'
1286                                ; word 2-4 unused
1287   
1288      P:00036A P:00036C 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1289      P:00036C P:00036E 45F400            MOVE              #'GOA',X1
                            474F41
1290   
1291      P:00036E P:000370 0D0306            JSR     VCOM_INTRO
1292      P:00036F P:000371 0E2313            JNE     VCOM_EXIT
1293   
1294      P:000370 P:000372 0A7020            BSET    #MODE_APPLICATION,X:MODE
                            000001
1295      P:000372 P:000374 000004            RTI                                       ; Application will reply.
1296   
1297   
1298                                ; ---------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 27



1299                                STOP_APPLICATION
1300                                ; this command stops an application that is currently running
1301                                ; used for applications that once started run contiunually
1302                                ;-----------------------------------------------------------
1303                                ; word 1 = command = ' STP'
1304                                ; word 2-4 unused
1305   
1306      P:000373 P:000375 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1307      P:000375 P:000377 45F400            MOVE              #'STP',X1
                            535450
1308   
1309      P:000377 P:000379 0D0306            JSR     VCOM_INTRO
1310      P:000378 P:00037A 0E2313            JNE     VCOM_EXIT
1311   
1312      P:000379 P:00037B 0A7000            BCLR    #MODE_APPLICATION,X:MODE
                            000001
1313      P:00037B P:00037D 0A7000            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1314      P:00037D P:00037F 0C0313            JMP     VCOM_EXIT
1315   
1316   
1317                                ;-----------------------------------------------------------------------------
1318                                RESET_CONTROLLER
1319                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1320                                ;-----------------------------------------------------------------------------
1321                                ; word 1 = command = 'RCO'
1322                                ; word 2-4 unused
1323   
1324      P:00037E P:000380 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1325      P:000380 P:000382 45F400            MOVE              #'RCO',X1
                            52434F
1326      P:000382 P:000384 0D0306            JSR     VCOM_INTRO
1327      P:000383 P:000385 0E2313            JNE     VCOM_EXIT
1328   
1329      P:000384 P:000386 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1330      P:000385 P:000387 000000            NOP
1331      P:000386 P:000388 000000            NOP
1332      P:000387 P:000389 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1333      P:000389 P:00038B 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1334      P:00038B P:00038D 446000            MOVE              X0,X:(R0)
1335      P:00038C P:00038E 0606A0            REP     #6                                ; Wait for transmission to complete
1336      P:00038D P:00038F 000000            NOP
1337      P:00038E P:000390 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1338   
1339                                ; Wait for a bit for MCE to be reset.......
1340      P:00038F P:000391 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1341      P:000391 P:000393 06C400            DO      X0,L_DELAY
                            000397
1342      P:000393 P:000395 06E883            DO      #1000,L_RDFIFO
                            000396
1343      P:000395 P:000397 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1344      P:000396 P:000398 000000            NOP                                       ;   receiver empty
1345                                L_RDFIFO
1346      P:000397 P:000399 000000            NOP
1347                                L_DELAY
1348      P:000398 P:00039A 000000            NOP
1349   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 28



1350      P:000399 P:00039B 44F400            MOVE              #'000',X0
                            303030
1351      P:00039B P:00039D 0C0311            JMP     VCOM_EXIT_X0
1352   
1353                                ;-----------------------------------------------------------------------------
1354                                QUIET_TRANSFER_SET
1355                                ;-----------------------------------------------------------------------------
1356                                ;Quiet transfer mode configuration
1357                                ; word 1 = command = 'QTS'
1358                                ; word 2 = parameter to set
1359                                ; word 3-4 = arguments
1360   
1361      P:00039C P:00039E 0BF080            JSR     SAVE_REGISTERS                    ; standard opening
                            0004A0
1362      P:00039E P:0003A0 45F400            MOVE              #'QTS',X1
                            515453
1363      P:0003A0 P:0003A2 0D0306            JSR     VCOM_INTRO
1364      P:0003A1 P:0003A3 0E2313            JNE     VCOM_EXIT
1365   
1366      P:0003A2 P:0003A4 60F400            MOVE              #BDEBUG0,R0
                            00004D
1367      P:0003A4 P:0003A6 0D01DE            JSR     INCR_X_R0
1368   
1369      P:0003A5 P:0003A7 56F000            MOVE              X:DRXR_WD2,A            ; Parameter id
                            000008
1370      P:0003A7 P:0003A9 44F000            MOVE              X:DRXR_WD3,X0           ; First arg
                            000009
1371      P:0003A9 P:0003AB 45F000            MOVE              X:DRXR_WD4,X1           ; Second arg
                            00000A
1372   
1373      P:0003AB P:0003AD 0140C5            CMP     #'BAS',A
                            424153
1374      P:0003AD P:0003AF 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            00042D
1375   
1376      P:0003AF P:0003B1 0140C5            CMP     #'DEL',A
                            44454C
1377      P:0003B1 P:0003B3 60F400            MOVE              #QT_BUF_SIZE,R0
                            00003E
1378      P:0003B3 P:0003B5 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1379   
1380      P:0003B5 P:0003B7 0140C5            CMP     #'NUM',A
                            4E554D
1381      P:0003B7 P:0003B9 60F400            MOVE              #QT_BUF_MAX,R0
                            00003F
1382      P:0003B9 P:0003BB 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1383   
1384      P:0003BB P:0003BD 0140C5            CMP     #'INF',A
                            494E46
1385      P:0003BD P:0003BF 60F400            MOVE              #QT_INFORM,R0
                            000041
1386      P:0003BF P:0003C1 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1387   
1388      P:0003C1 P:0003C3 0140C5            CMP     #'SIZ',A
                            53495A
1389      P:0003C3 P:0003C5 60F400            MOVE              #QT_FRAME_SIZE,R0
                            000040
1390      P:0003C5 P:0003C7 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 29



1391   
1392      P:0003C7 P:0003C9 0140C5            CMP     #'TAI',A
                            544149
1393      P:0003C9 P:0003CB 60F400            MOVE              #QT_BUF_TAIL,R0
                            000043
1394      P:0003CB P:0003CD 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1395   
1396      P:0003CD P:0003CF 0140C5            CMP     #'HEA',A
                            484541
1397      P:0003CF P:0003D1 60F400            MOVE              #QT_BUF_HEAD,R0
                            000042
1398      P:0003D1 P:0003D3 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1399   
1400      P:0003D3 P:0003D5 0140C5            CMP     #'DRO',A
                            44524F
1401      P:0003D5 P:0003D7 60F400            MOVE              #QT_DROPS,R0
                            000047
1402      P:0003D7 P:0003D9 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1403   
1404      P:0003D9 P:0003DB 0140C5            CMP     #'PER',A
                            504552
1405      P:0003DB P:0003DD 60F400            MOVE              #TCPR0,R0
                            FFFF8D
1406      P:0003DD P:0003DF 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1407   
1408      P:0003DF P:0003E1 0140C5            CMP     #'FLU',A
                            464C55
1409      P:0003E1 P:0003E3 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            00040D
1410   
1411      P:0003E3 P:0003E5 0140C5            CMP     #'SET',A
                            534554
1412      P:0003E5 P:0003E7 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            000415
1413   
1414      P:0003E7 P:0003E9 0140C5            CMP     #'RPS',A
                            525053
1415      P:0003E9 P:0003EB 60F400            MOVE              #RP_MAX_SIZE,R0
                            00004A
1416      P:0003EB P:0003ED 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1417   
1418      P:0003ED P:0003EF 0140C5            CMP     #'RPB',A
                            525042
1419      P:0003EF P:0003F1 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_BASE
                            0003FE
1420   
1421      P:0003F1 P:0003F3 0140C5            CMP     #'RPE',A
                            525045
1422      P:0003F3 P:0003F5 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_ENABLED
                            000403
1423   
1424      P:0003F5 P:0003F7 0140C5            CMP     #'BUR',A
                            425552
1425      P:0003F7 P:0003F9 60F400            MOVE              #PCI_BURST_SIZE,R0
                            000029
1426      P:0003F9 P:0003FB 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0_PERSISTENT
                            000425
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 30



1427   
1428      P:0003FB P:0003FD 44F400            MOVE              #'MTE',X0
                            4D5445
1429      P:0003FD P:0003FF 0C030C            JMP     VCOM_EXIT_ERROR_X0
1430   
1431                                QUIET_TRANSFER_SET_RP_BASE
1432      P:0003FE P:000400 447000            MOVE              X0,X:RP_BASE_LO
                            000048
1433      P:000400 P:000402 457000            MOVE              X1,X:RP_BASE_HI
                            000049
1434      P:000402 P:000404 0C0313            JMP     VCOM_EXIT
1435   
1436                                QUIET_TRANSFER_SET_RP_ENABLED
1437      P:000403 P:000405 0A7003            BCLR    #MODE_RP_BUFFER,X:MODE
                            000001
1438      P:000405 P:000407 208E00            MOVE              X0,A
1439      P:000406 P:000408 200003            TST     A
1440      P:000407 P:000409 0EA313            JEQ     VCOM_EXIT
1441      P:000408 P:00040A 0A7023            BSET    #MODE_RP_BUFFER,X:MODE
                            000001
1442      P:00040A P:00040C 0A7015            BCLR    #RP_BUFFER_FULL,X:STATUS
                            000000
1443      P:00040C P:00040E 0C0313            JMP     VCOM_EXIT
1444   
1445                                QUIET_TRANSFER_SET_FLUSH
1446      P:00040D P:00040F 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
1447      P:00040F P:000411 208E00            MOVE              X0,A
1448      P:000410 P:000412 200003            TST     A
1449      P:000411 P:000413 0EA313            JEQ     VCOM_EXIT
1450      P:000412 P:000414 0A7034            BSET    #QT_FLUSH,X:STATUS
                            000000
1451      P:000414 P:000416 0C0313            JMP     VCOM_EXIT
1452   
1453                                QUIET_TRANSFER_SET_ENABLED
1454      P:000415 P:000417 208E00            MOVE              X0,A
1455      P:000416 P:000418 200003            TST     A
1456      P:000417 P:000419 0AF0AA            JEQ     QUIET_TRANSFER_SET_DISABLED
                            00041E
1457      P:000419 P:00041B 0A7022            BSET    #MODE_QT,X:MODE
                            000001
1458      P:00041B P:00041D 0BF080            JSR     TIMER_ENABLE
                            000622
1459      P:00041D P:00041F 0C0313            JMP     VCOM_EXIT
1460   
1461                                QUIET_TRANSFER_SET_DISABLED
1462      P:00041E P:000420 0A7002            BCLR    #MODE_QT,X:MODE
                            000001
1463      P:000420 P:000422 0BF080            JSR     TIMER_DEFAULT
                            00062E
1464      P:000422 P:000424 0C0313            JMP     VCOM_EXIT
1465   
1466                                QUIET_TRANSFER_SET_R0
1467      P:000423 P:000425 446000            MOVE              X0,X:(R0)
1468      P:000424 P:000426 0C0313            JMP     VCOM_EXIT
1469   
1470                                QUIET_TRANSFER_SET_R0_PERSISTENT
1471      
1472      
1473      
1474      P:000425 P:000427 446000            MOVE              X0,X:(R0)
1475      P:000426 P:000428 57F400            MOVE              #>VAR_TBL_START,B
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 31



                            0006AC
1476      P:000428 P:00042A 220E00            MOVE              R0,A
1477      P:000429 P:00042B 200018            ADD     A,B
**** 1478 [main.asm 873]: WARNING --- Pipeline stall reading register B written in previous instruction (X data move field)
**** 1478 [main.asm 873]: WARNING --- Pipeline stall reading register written in previous instruction (X data move field)
1478      P:00042A P:00042C 21F000            MOVE              B,R0
**** 1479 [main.asm 874]: WARNING --- Pipeline stall reading register written in instruction at address: P:00042A (X data move field
)
1479      P:00042B P:00042D 076084            MOVE              X0,P:(R0)
1480      P:00042C P:00042E 0C0313            JMP     VCOM_EXIT
1481   
1482                                QUIET_TRANSFER_SET_BASE
1483      P:00042D P:00042F 447000            MOVE              X0,X:QT_BASE_LO
                            00003C
1484      P:00042F P:000431 457000            MOVE              X1,X:QT_BASE_HI
                            00003D
1485   
1486      P:000431 P:000433 0BF080            JSR     BUFFER_RESET
                            000669
1487   
1488      P:000433 P:000435 0C0313            JMP     VCOM_EXIT
1489   
1490   
1491                                ;-----------------------------------------------------------------------------
1492                                SYSTEM_RESET
1493                                ;-----------------------------------------------------------------------------
1494   
1495      P:000434 P:000436 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1496      P:000435 P:000437 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1497                                                                                    ; set to zero except for interrupts
1498      P:000437 P:000439 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1499                                                                                    ; so first set to 0
1500      P:000438 P:00043A 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1501                                                                                    ; therefore,return to initialization
1502      P:00043A P:00043C 000000            NOP
1503      P:00043B P:00043D 000004            RTI                                       ; return from ISR - to START
1504   
1505   
1506                                ; ------------------------------------------------------------------------------------
1507                                SEND_PACKET_TO_HOST
1508                                ; this command is received from the Host and actions the PCI board to pick up an address
1509                                ; pointer from DRXR which the PCI board then uses to write packets from the
1510                                ; MCE to the host memory starting at the address given.
1511                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1512                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1513                                ; HST after packet sent (unless error).
1514                                ; --------------------------------------------------------------------------------------
1515                                ; word 1 = command = 'HST'
1516                                ; word 2 = host high address
1517                                ; word 3 = host low address
1518                                ; word 4 = not used but read
1519   
1520      P:00043C P:00043E 0D04A0            JSR     <SAVE_REGISTERS                   ; save working registers
1521      P:00043D P:00043F 45F400            MOVE              #'HST',X1
                            485354
1522      P:00043F P:000441 0D0306            JSR     VCOM_INTRO
1523      P:000440 P:000442 0E2313            JNE     VCOM_EXIT
1524   
1525      
1526      P:000441 P:000443 448800            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 32



1527      P:000442 P:000444 518900            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1528   
1529      P:000443 P:000445 447000            MOVE              X0,X:BURST_DEST_HI
                            000030
1530      P:000445 P:000447 517000            MOVE              B0,X:BURST_DEST_LO
                            00002F
1531   
1532      P:000447 P:000449 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1533   
1534      P:000448 P:00044A 0BF080            JSR     RESTORE_REGISTERS
                            000493
1535      P:00044A P:00044C 000004            RTI                                       ; Main loop will reply after packet transfer
!
1536   
1537   
1538                                ; --------------------------------------------------------------------
1539                                SOFTWARE_RESET
1540                                ;----------------------------------------------------------------------
1541                                ; word 1 = command = 'RST'
1542                                ; word 2-4 unused
1543   
1544      P:00044B P:00044D 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1545      P:00044D P:00044F 45F400            MOVE              #'RST',X1
                            525354
1546      P:00044F P:000451 0D0306            JSR     VCOM_INTRO
1547      P:000450 P:000452 0E2313            JNE     VCOM_EXIT
1548   
1549                                ; RST command OK so reply to host
1550                                FINISH_RST
1551      P:000451 P:000453 44F400            MOVE              #'000',X0
                            303030
1552      P:000453 P:000455 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1553      P:000455 P:000457 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1554   
1555      P:000457 P:000459 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            000457
1556   
1557      P:000459 P:00045B 0A0100            BCLR    #MODE_APPLICATION,X:<MODE         ; clear app flag
1558      P:00045A P:00045C 0A0000            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1559   
1560      P:00045B P:00045D 0C0434            JMP     SYSTEM_RESET                      ; Handle the stack and stuff...
1561   
1562   
1563                                SEND_PACKET_TO_CONTROLLER
1564   
1565                                ;       Host command identifying location of an MCE command to send to
1566                                ;       the MCE.  Since this can come at any time, just record the
1567                                ;       request and then do the CONning from the main loop.
1568   
1569                                ; word 1 = command = 'CON'
1570                                ; word 2 = source host bus address, bits 31:16
1571                                ; word 3 = source host bus address, bits 15:0
1572                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1573                                ;        = '1' --> when MCE command is GO
1574   
1575      P:00045C P:00045E 0D04A0            JSR     <SAVE_REGISTERS                   ; save working registers
1576   
1577      
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 33



1578      P:00045D P:00045F 45F400            MOVE              #'CON',X1
                            434F4E
1579      P:00045F P:000461 0D0306            JSR     VCOM_INTRO
1580      P:000460 P:000462 0E2313            JNE     VCOM_EXIT
1581   
1582      
1583      P:000461 P:000463 44F400            MOVE              #'BUS',X0
                            425553
1584      P:000463 P:000465 0A00AA            JSET    #CON_DEMAND,X:STATUS,VCOM_EXIT_ERROR_X0
                            00030C
1585   
1586      
1587      P:000465 P:000467 0A702A            BSET    #CON_DEMAND,X:STATUS
                            000000
1588      P:000467 P:000469 448800            MOVE              X:<DRXR_WD2,X0
1589      P:000468 P:00046A 458900            MOVE              X:<DRXR_WD3,X1
1590      P:000469 P:00046B 447000            MOVE              X0,X:CON_SRC_HI
                            00002D
1591      P:00046B P:00046D 457000            MOVE              X1,X:CON_SRC_LO
                            00002C
1592   
1593                                ;       ;; Fourth word indicates if this is a go.  Who cares?
1594                                ;       MOVE    X:<DRXR_WD4,A           ; read word 4 - GO command?
1595                                ;       MOVE    #0,X0
1596                                ;       CMP     X0,A
1597                                ;       JEQ     BLOCK_CON
1598   
1599      
1600      P:00046D P:00046F 0BF080            JSR     RESTORE_REGISTERS
                            000493
1601      P:00046F P:000471 000004            RTI
1602   
1604   
1605   
1606                                ;---------------------------------------------------------------
1607                                ;
1608                                ;                          * END OF ISRs *
1609                                ;
1610                                ;--------------------------------------------------------------
1611   
1612   
1613   
1614                                ;----------------------------------------------------------------
1615                                ;
1616                                ;                     * Beginning of SUBROUTINES *
1617                                ;
1618                                ;-----------------------------------------------------------------
1619   
1620   
1621                                CHECK_FO
1622      P:000470 P:000472 01AD80            JCLR    #EF,X:PDRD,CHECK_FO_CLEAR
                            000478
1623      P:000472 P:000474 000000            NOP
1624      P:000473 P:000475 000000            NOP
1625      P:000474 P:000476 01AD80            JCLR    #EF,X:PDRD,CHECK_FO_CLEAR
                            000478
1626      P:000476 P:000478 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1627      P:000477 P:000479 00000C            RTS
1628   
1629                                CHECK_FO_CLEAR
1630      P:000478 P:00047A 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1631      P:000479 P:00047B 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 34



1632   
1633   
1634   
1635                                ;----------------------------------------------------------------------------
1636                                PCI_MESSAGE_TO_HOST
1637                                ;----------------------------------------------------------------------------
1638                                ; Subroutine to send 4 words as a reply from PCI to the Host
1639                                ; using the DTXS-HRXS data path.  The DSP signals the host by raising
1640                                ; HF3 and (when !MODE_NOIRQ) INTA.
1641                                ;
1642                                ; When MODE_HANDSHAKE, the DSP and Host interact as follows:
1643                                ; - to show that the Host is handling the interrupt, Host raises HF0
1644                                ; - when DSP sees HF0 go high, it lowers INTA and HF3
1645                                ; - when Host is done handling the interrupt (i.e. it has read the reply),
1646                                ;   and when HF3 is low, Host lowers HF0.
1647                                ; - when DSP sees HF0 go low, the routine finishes.
1648                                ;
1649                                ; The primary advantage of this hand-shaking scheme is that host vector
1650                                ; commands are not needed to clear HF3 and INTA.
1651                                ;
1652                                ; This routine should not block for anything other than the Host handshake.
1653   
1654      P:00047A P:00047C 60F400            MOVE              #>DTXS_WD1,R0
                            00000B
1655   
1656      P:00047C P:00047E 060480            DO      #4,PCI_MESSAGE_TO_HOST_10
                            000480
1657      P:00047E P:000480 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            00047E
1658      P:000480 P:000482 08D88D            MOVEP             X:(R0)+,X:DTXS
1659   
1660                                PCI_MESSAGE_TO_HOST_10
1661      P:000481 P:000483 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            000015
1662      P:000483 P:000485 60F000            MOVE              X:SV_R0,R0              ; restore R0
                            000019
1663      P:000485 P:000487 0A8523            BSET    #DCTR_HF3,X:DCTR                  ; Raise HF3 (handshake)
1664   
1665                                                                                    ; Only interrupt in irq mode
1666      
1667      P:000486 P:000488 000000            NOP
1668      P:000487 P:000489 000000            NOP
1669      P:000488 P:00048A 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1670   
1671                                PCI_MESSAGE_TO_HOST_20
1672      P:000489 P:00048B 0A89A4            JSET    #DSR_HF1,X:DSR,PCI_MESSAGE_TO_HOST_HANDSHAKE
                            00048C
1673      P:00048B P:00048D 00000C            RTS
1674   
1675                                PCI_MESSAGE_TO_HOST_HANDSHAKE
1676      P:00048C P:00048E 0A8983            JCLR    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            00048C
1677      P:00048E P:000490 0A8506            BCLR    #INTA,X:DCTR                      ; Clear interrupt
1678      P:00048F P:000491 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; Clear hand-shake bit
1679      P:000490 P:000492 0A89A3            JSET    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            000490
1680      P:000492 P:000494 00000C            RTS
1681   
1682   
1683                                ;------------------------------------------------------------------------------------
1684                                RESTORE_REGISTERS
1685                                ;-------------------------------------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 35



1686   
1687      P:000493 P:000495 059A39            MOVEC             X:<SV_SR,SR
1688   
1689      P:000494 P:000496 508F00            MOVE              X:<SV_A0,A0
1690      P:000495 P:000497 549000            MOVE              X:<SV_A1,A1
1691      P:000496 P:000498 529100            MOVE              X:<SV_A2,A2
1692   
1693      P:000497 P:000499 519200            MOVE              X:<SV_B0,B0
1694      P:000498 P:00049A 559300            MOVE              X:<SV_B1,B1
1695      P:000499 P:00049B 539400            MOVE              X:<SV_B2,B2
1696   
1697      P:00049A P:00049C 449500            MOVE              X:<SV_X0,X0
1698      P:00049B P:00049D 459600            MOVE              X:<SV_X1,X1
1699   
1700      P:00049C P:00049E 469700            MOVE              X:<SV_Y0,Y0
1701      P:00049D P:00049F 479800            MOVE              X:<SV_Y1,Y1
1702   
1703      P:00049E P:0004A0 609900            MOVE              X:<SV_R0,R0
1704      P:00049F P:0004A1 00000C            RTS
1705   
1706                                ;-------------------------------------------------------------------------------------
1707                                SAVE_REGISTERS
1708                                ;-------------------------------------------------------------------------------------
1709   
1710      P:0004A0 P:0004A2 051A39            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1711   
1712      P:0004A1 P:0004A3 500F00            MOVE              A0,X:<SV_A0
1713      P:0004A2 P:0004A4 541000            MOVE              A1,X:<SV_A1
1714      P:0004A3 P:0004A5 521100            MOVE              A2,X:<SV_A2
1715   
1716      P:0004A4 P:0004A6 511200            MOVE              B0,X:<SV_B0
1717      P:0004A5 P:0004A7 551300            MOVE              B1,X:<SV_B1
1718      P:0004A6 P:0004A8 531400            MOVE              B2,X:<SV_B2
1719   
1720      P:0004A7 P:0004A9 441500            MOVE              X0,X:<SV_X0
1721      P:0004A8 P:0004AA 451600            MOVE              X1,X:<SV_X1
1722   
1723      P:0004A9 P:0004AB 461700            MOVE              Y0,X:<SV_Y0
1724      P:0004AA P:0004AC 471800            MOVE              Y1,X:<SV_Y1
1725   
1726      P:0004AB P:0004AD 601900            MOVE              R0,X:<SV_R0
1727      P:0004AC P:0004AE 00000C            RTS
1728   
1729   
1730                                ;----------------------------------------------
1731                                FLUSH_PCI_FIFO
1732                                ;----------------------------------------------
1733      P:0004AD P:0004AF 0A8A84            JCLR    #MARQ,X:DPSR,*
                            0004AD
1734      P:0004AF P:0004B1 0A862E            BSET    #CLRT,X:DPCR
1735      P:0004B0 P:0004B2 000000            NOP
1736      P:0004B1 P:0004B3 0A86AE            JSET    #CLRT,X:DPCR,*
                            0004B1
1737      P:0004B3 P:0004B5 00000C            RTS
1738   
1739                                ;----------------------------------------------
1740                                CLEAR_FO_FIFO
1741                                ;----------------------------------------------
1742      P:0004B4 P:0004B6 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
1743      P:0004B6 P:0004B8 44F400            MOVE              #200000,X0
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 36



                            030D40
1744      P:0004B8 P:0004BA 06C400            DO      X0,*+3
                            0004BA
1745      P:0004BA P:0004BC 000000            NOP
1746      P:0004BB P:0004BD 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
1747      P:0004BD P:0004BF 00000C            RTS
1748   
1749   
1750                                ;---------------------------------------------------------
1751                                ; PCI burst routines
1752                                ;
1753                                ; For transfer between Host memory and DSP Y memory.
1754                                ;
1755                                ; Major entry points are
1756                                ;       CON_TRANSFER (PC -> DSP)
1757                                ;       BLOCK_TRANSFER (DSP -> PC)
1758                                ;---------------------------------------------------------
1759   
1760                                ;---------------------------------------------------------
1761                                PCI_ERROR_CLEAR
1762                                ;-----------------------------------------------
1763      
1764      
1765      
1766      
1767      
1768      
1769   
1770      P:0004BE P:0004C0 50F000            MOVE              X:DMA_ERRORS,A0
                            000034
1771      P:0004C0 P:0004C2 000008            INC     A
1772      P:0004C1 P:0004C3 000000            NOP
1773      P:0004C2 P:0004C4 507000            MOVE              A0,X:DMA_ERRORS
                            000034
1774   
1775      P:0004C4 P:0004C6 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            0004D2
1776      P:0004C6 P:0004C8 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
                            0004DC
1777      P:0004C8 P:0004CA 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            0004E6
1778      P:0004CA P:0004CC 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            0004F0
1779      P:0004CC P:0004CE 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            0004FA
1780      P:0004CE P:0004D0 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            000504
1781      P:0004D0 P:0004D2 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            00050E
1782   
1783                                ERROR_TRTY
1784      P:0004D2 P:0004D4 50F000            MOVE              X:EC_TRTY,A0
                            000035
1785      P:0004D4 P:0004D6 000008            INC     A
1786      P:0004D5 P:0004D7 08F48A            MOVEP             #>$0400,X:DPSR          ; Clear target retry error bit
                            000400
1787      P:0004D7 P:0004D9 507000            MOVE              A0,X:EC_TRTY
                            000035
1788      P:0004D9 P:0004DB 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1789      P:0004DB P:0004DD 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 37



1790                                ERROR_TO
1791      P:0004DC P:0004DE 50F000            MOVE              X:EC_TO,A0
                            000036
1792      P:0004DE P:0004E0 000008            INC     A
1793      P:0004DF P:0004E1 08F48A            MOVEP             #>$0800,X:DPSR          ; Clear timeout error bit
                            000800
1794      P:0004E1 P:0004E3 507000            MOVE              A0,X:EC_TO
                            000036
1795      P:0004E3 P:0004E5 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1796      P:0004E5 P:0004E7 00000C            RTS
1797                                ERROR_TDIS
1798      P:0004E6 P:0004E8 50F000            MOVE              X:EC_TDIS,A0
                            000037
1799      P:0004E8 P:0004EA 000008            INC     A
1800      P:0004E9 P:0004EB 08F48A            MOVEP             #>$0200,X:DPSR          ; Clear target disconnect bit
                            000200
1801      P:0004EB P:0004ED 507000            MOVE              A0,X:EC_TDIS
                            000037
1802      P:0004ED P:0004EF 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1803      P:0004EF P:0004F1 00000C            RTS
1804                                ERROR_TAB
1805      P:0004F0 P:0004F2 50F000            MOVE              X:EC_TAB,A0
                            000038
1806      P:0004F2 P:0004F4 000008            INC     A
1807      P:0004F3 P:0004F5 08F48A            MOVEP             #>$0100,X:DPSR          ; Clear target abort error bit
                            000100
1808      P:0004F5 P:0004F7 507000            MOVE              A0,X:EC_TAB
                            000038
1809      P:0004F7 P:0004F9 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1810      P:0004F9 P:0004FB 00000C            RTS
1811                                ERROR_MAB
1812      P:0004FA P:0004FC 50F000            MOVE              X:EC_MAB,A0
                            000039
1813      P:0004FC P:0004FE 000008            INC     A
1814      P:0004FD P:0004FF 08F48A            MOVEP             #>$0080,X:DPSR          ; Clear master abort error bit
                            000080
1815      P:0004FF P:000501 507000            MOVE              A0,X:EC_MAB
                            000039
1816      P:000501 P:000503 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1817      P:000503 P:000505 00000C            RTS
1818                                ERROR_DPER
1819      P:000504 P:000506 50F000            MOVE              X:EC_DPER,A0
                            00003A
1820      P:000506 P:000508 000008            INC     A
1821      P:000507 P:000509 08F48A            MOVEP             #>$0040,X:DPSR          ; Clear data parity error bit
                            000040
1822      P:000509 P:00050B 507000            MOVE              A0,X:EC_DPER
                            00003A
1823      P:00050B P:00050D 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1824      P:00050D P:00050F 00000C            RTS
1825                                ERROR_APER
1826      P:00050E P:000510 50F000            MOVE              X:EC_APER,A0
                            00003B
1827      P:000510 P:000512 000008            INC     A
1828      P:000511 P:000513 08F48A            MOVEP             #>$0020,X:DPSR          ; Clear address parity error bit
                            000020
1829      P:000513 P:000515 507000            MOVE              A0,X:EC_APER
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 38



                            00003B
1830      P:000515 P:000517 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1831      P:000517 P:000519 00000C            RTS
1832   
1833   
1834   
1835                                ;----------------------------------------------
1836                                BLOCK_TRANSFER
1837                                ;----------------------------------------------
1838                                ;   In:
1839                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
1840                                ;   - BLOCK_SIZE is packet size, in bytes
1841                                ;   - YMEM_SRC is start of data in Y memory
1842                                ;  Out:
1843                                ;   - BLOCK_SIZE will be decremented to zero.
1844                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
1845                                ;   - YMEM_SRC will be incremented by BLOCK_SIZE/2
1846                                ;  Trashes:
1847                                ;   - A and B at least
1848   
1849      P:000518 P:00051A 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002B
1850      P:00051A P:00051C 014085            CMP     #0,A                              ; Still bytes to transfer?
1851      P:00051B P:00051D 0AF0A2            JNE     BLOCK_TRANSFER0
                            00051E
1852      P:00051D P:00051F 00000C            RTS
1853   
1854                                BLOCK_TRANSFER0
1855      
1856      
1857      P:00051E P:000520 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000029
1858   
1859      P:000520 P:000522 200005            CMP     B,A                               ; A ? B
1860      P:000521 P:000523 0E1523            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
1861      P:000522 P:000524 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1862                                BLOCK_TRANSFER1
1863      
1864      P:000523 P:000525 200014            SUB     B,A                               ; A -= B
1865      P:000524 P:000526 014088            ADD     #0,B                              ; Clear carry bit
1866      P:000525 P:000527 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002B
1867      P:000527 P:000529 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002A
1868      P:000529 P:00052B 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1869   
1870      
1871      P:00052A P:00052C 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
1872      P:00052C P:00052E 50F000            MOVE              X:YMEM_SRC,A0
                            00002E
1873      P:00052E P:000530 08C82F            MOVEP             A0,X:DSR0               ; DMA source
1874      P:00052F P:000531 200010            ADD     B,A
1875      P:000530 P:000532 00000B            DEC     B
1876      P:000531 P:000533 507000            MOVE              A0,X:YMEM_SRC           ; BURST_SRC += BURST_SIZE/2
                            00002E
1877   
1878      P:000533 P:000535 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1879   
1880      
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 39



1881      P:000534 P:000536 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
1882   
1883                                BLOCK_TRANSFER_PCI
1884      P:000536 P:000538 44F400            MOVE              #>$7,X0                 ; Memory write
                            000007
1885      P:000538 P:00053A 60F400            MOVE              #BURST_DEST_LO,R0       ; RAM address
                            00002F
1886      P:00053A P:00053C 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            00058D
1887   
1888      
1889      P:00053C P:00053E 0A8A84            JCLR    #MARQ,X:DPSR,*
                            00053C
1890   
1891      
1892      P:00053E P:000540 0A8A8E            JCLR    #MDT,X:DPSR,BLOCK_TRANSFER_HANDLE_ERRORS
                            000546
1893   
1894      P:000540 P:000542 20001B            CLR     B
1895      P:000541 P:000543 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            00002A
1896      P:000543 P:000545 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            0006A1
1897      P:000545 P:000547 0C0518            JMP     BLOCK_TRANSFER                    ; Next burst in block
1898   
1899                                BLOCK_TRANSFER_HANDLE_ERRORS
1900      
1901      P:000546 P:000548 0D04BE            JSR     PCI_ERROR_CLEAR
1902   
1903      P:000547 P:000549 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1904      P:000549 P:00054B 0E8536            JCS     BLOCK_TRANSFER_PCI                ; Restart PCI burst
1905   
1906      P:00054A P:00054C 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
1907      P:00054C P:00054E 0E0518            JCC     BLOCK_TRANSFER                    ; Error but no error? Redo this burst.
1908   
1909      
1910      P:00054D P:00054F 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            0005A3
1911      P:00054F P:000551 0BF080            JSR     PCI_UPDATE_R0
                            0005B3
1912      P:000551 P:000553 0C0536            JMP     BLOCK_TRANSFER_PCI
1913   
1914   
1915                                ;----------------------------------------------
1916                                CON_TRANSFER
1917                                ;----------------------------------------------
1918                                ;   In:
1919                                ;   - BURST_SRC_HI:BURST_SRC_LO is PC RAM address
1920                                ;   - BLOCK_SIZE is packet size, in bytes
1921                                ;   - YMEM_DEST is start of data in Y memory
1922                                ;  Out:
1923                                ;   - BLOCK_SIZE will be decremented to zero.
1924                                ;   - BURST_SRC_HI:LO will be incremented by BLOCK_SIZE
1925                                ;   - YMEM_DEST will be incremented by BLOCK_SIZE/2
1926                                ;  Trashes:
1927                                ;   - A and B, R0, X0
1928   
1929      P:000552 P:000554 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002B
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 40



1930      P:000554 P:000556 014085            CMP     #0,A                              ; Still bytes to transfer?
1931      P:000555 P:000557 0AF0A2            JNE     CON_TRANSFER0
                            000558
1932      P:000557 P:000559 00000C            RTS
1933   
1934                                CON_TRANSFER0
1935      
1936      
1937      P:000558 P:00055A 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000029
1938   
1939      P:00055A P:00055C 200005            CMP     B,A                               ; A ? B
1940      P:00055B P:00055D 0E155D            JGE     <CON_TRANSFER1                    ; jump if A >= B
1941      P:00055C P:00055E 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1942                                CON_TRANSFER1
1943      
1944      P:00055D P:00055F 200014            SUB     B,A                               ; A -= B
1945      P:00055E P:000560 014088            ADD     #0,B                              ; Clear carry bit
1946      P:00055F P:000561 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002B
1947      P:000561 P:000563 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002A
1948      P:000563 P:000565 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1949   
1950      
1951      P:000564 P:000566 50F000            MOVE              X:YMEM_DEST,A0
                            000033
1952      P:000566 P:000568 507000            MOVE              A0,X:DDR0               ; DMA dest'n
                            FFFFEE
1953      P:000568 P:00056A 08F4AF            MOVEP             #>DRXR,X:DSR0           ; DMA source
                            FFFFCB
1954      P:00056A P:00056C 200010            ADD     B,A
1955      P:00056B P:00056D 00000B            DEC     B
1956      P:00056C P:00056E 507000            MOVE              A0,X:YMEM_DEST          ; YMEM_DEST += BURST_SIZE/2
                            000033
1957   
1958      P:00056E P:000570 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1959   
1960      
1961      P:00056F P:000571 08F4AC            MOVEP             #$8EEAC4,X:DCR0
                            8EEAC4
1962   
1963                                CON_TRANSFER_PCI
1964      P:000571 P:000573 44F400            MOVE              #>$6,X0                 ; Memory write
                            000006
1965      P:000573 P:000575 60F400            MOVE              #BURST_SRC_LO,R0        ; RAM address
                            000031
1966      P:000575 P:000577 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            00058D
1967   
1968      
1969      P:000577 P:000579 0A8A84            JCLR    #MARQ,X:DPSR,*
                            000577
1970   
1971      
1972      P:000579 P:00057B 0A8A8E            JCLR    #MDT,X:DPSR,CON_TRANSFER_HANDLE_ERRORS
                            000581
1973   
1974      P:00057B P:00057D 20001B            CLR     B
1975      P:00057C P:00057E 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            00002A
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 41



1976      P:00057E P:000580 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            0006A1
1977      P:000580 P:000582 0C0552            JMP     CON_TRANSFER                      ; Next burst in block
1978   
1979                                CON_TRANSFER_HANDLE_ERRORS
1980      
1981      P:000581 P:000583 0D04BE            JSR     PCI_ERROR_CLEAR
1982   
1983      P:000582 P:000584 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1984      P:000584 P:000586 0E8571            JCS     CON_TRANSFER_PCI                  ; Restart PCI burst
1985   
1986      P:000585 P:000587 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
1987      P:000587 P:000589 0E0552            JCC     CON_TRANSFER                      ; Error but no error? Redo this burst.
1988   
1989      
1990      P:000588 P:00058A 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            0005A3
1991      P:00058A P:00058C 0BF080            JSR     PCI_UPDATE_R0
                            0005B3
1992      P:00058C P:00058E 0C0571            JMP     CON_TRANSFER_PCI
1993   
1994                                ; Utility routines for BLOCK_TRANSFER and CON_TRANSFER
1995   
1996                                PCI_GO
1997                                ; Initiate PCI read/write of BURST_SIZE bytes.
1998                                ; R0 must point to the hi-lo PCI address source/dest address
1999                                ; X0 is the PCI command (6 is read, 7 is write).
2000                                ; Trashes A and B but not R0 and X0.
2001      P:00058D P:00058F 200013            CLR     A
2002      P:00058E P:000590 20001B            CLR     B
2003      P:00058F P:000591 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            00002A
2004      P:000591 P:000593 00000B            DEC     B                                 ; n8 - 1
2005      P:000592 P:000594 014088            ADD     #0,B                              ; Clear carry
2006      P:000593 P:000595 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
2007      P:000594 P:000596 014088            ADD     #0,B                              ; Clear carry
2008      P:000595 P:000597 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
2009   
2010      P:000596 P:000598 0200D8            MOVE              X:(R0+1),A0             ; PCI HI address
2011   
2012      P:000597 P:000599 200010            ADD     B,A
2013      P:000598 P:00059A 000000            NOP
2014      P:000599 P:00059B 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
2015   
2016      P:00059B P:00059D 208800            MOVE              X0,A0
2017      P:00059C P:00059E 014088            ADD     #0,B                              ; Clear carry
2018      P:00059D P:00059F 0C1D20            ASL     #16,A,A                           ; Command into bits 19:16
2019      P:00059E P:0005A0 51E000            MOVE              X:(R0),B0
2020      P:00059F P:0005A1 200010            ADD     B,A
2021      P:0005A0 P:0005A2 000000            NOP
2022   
2023      P:0005A1 P:0005A3 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
2024      P:0005A2 P:0005A4 00000C            RTS
2025   
2026   
2027                                PCI_RECOVER_COUNT
2028                                ; Calculate number of PCI words not transferred.
2029                                ; Correct BURST_SIZE.  Returns:
2030                                ;   B: bytes not transferred
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 42



2031                                ;   A: bytes transferred
2032      P:0005A3 P:0005A5 200013            CLR     A
2033      P:0005A4 P:0005A6 20001B            CLR     B
2034      P:0005A5 P:0005A7 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
2035      P:0005A6 P:0005A8 0A8A8F            JCLR    #RDCQ,X:DPSR,PCI_RECOVER_COUNT1
                            0005A9
2036      P:0005A8 P:0005AA 000009            INC     B
2037                                PCI_RECOVER_COUNT1
2038      P:0005A9 P:0005AB 000009            INC     B                                 ; We want N, not N-1.
2039      P:0005AA P:0005AC 014088            ADD     #0,B                              ; Clear carry
2040      P:0005AB P:0005AD 0C1C20            ASR     #16,A,A
2041      P:0005AC P:0005AE 200018            ADD     A,B                               ; B is words remaining
2042      P:0005AD P:0005AF 014088            ADD     #0,B                              ; Clear carry
2043      P:0005AE P:0005B0 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
2044      P:0005AF P:0005B1 50F000            MOVE              X:BURST_SIZE,A0
                            00002A
2045      P:0005B1 P:0005B3 200014            SUB     B,A                               ; A is bytes written
2046      P:0005B2 P:0005B4 00000C            RTS
2047   
2048   
2049                                PCI_UPDATE_R0
2050                                ;  Subtract A0 from BURST_SIZE and add A to the 32-bit hi-lo address at X:[R0].
2051                                ;  Caller can check Z flag to see if BURST_SIZE is now 0.
2052      P:0005B3 P:0005B5 210500            MOVE              A0,X1                   ; Save A for later
2053      P:0005B4 P:0005B6 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2054      P:0005B5 P:0005B7 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates [R0] = [R0] + B
                            0006A1
2055   
2056      P:0005B7 P:0005B9 57F000            MOVE              X:BURST_SIZE,B
                            00002A
2057      P:0005B9 P:0005BB 20006C            SUB     X1,B                              ; Zero flag must be preserved!
2058      P:0005BA P:0005BC 000000            NOP
2059      P:0005BB P:0005BD 557000            MOVE              B1,X:BURST_SIZE
                            00002A
2060      P:0005BD P:0005BF 00000C            RTS
2061   
2062   
2063                                ;----------------------------------------------;
2064                                ;  MCE PACKET PROCESSING                       ;
2065                                ;----------------------------------------------;
2066   
2067                                ;       Given a dword count in A, computes number of half FIFOs and
2068                                ;       number of left over FIFO reads required to get the whole
2069                                ;       packet.
2070   
2071                                ;       Input: A is packet size, in dwords
2072                                ;       Output: sets X:TOTAL_BUFFS and X:LEFT_TO_READ
2073                                ;       Trashes: A,B,X0
2074   
2075   
2076                                PACKET_PARTITIONS
2077      P:0005BE P:0005C0 507000            MOVE              A0,X:PACKET_SIZE
                            000023
2078   
2079      P:0005C0 P:0005C2 014088            ADD     #0,B                              ; Clear carry
2080      P:0005C1 P:0005C3 0C1D02            ASL     #1,A,A                            ;  * 2
2081      P:0005C2 P:0005C4 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
2082      P:0005C3 P:0005C5 240000            MOVE              #0,X0
2083      P:0005C4 P:0005C6 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
2084   
2085      P:0005C6 P:0005C8 557000            MOVE              B1,X:TOTAL_BUFFS
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 43



                            000024
2086      P:0005C8 P:0005CA 507000            MOVE              A0,X:LEFT_TO_READ
                            000025
2087      P:0005CA P:0005CC 00000C            RTS
2088   
2089   
2090                                ; BUFFER_PACKET
2091                                ;
2092                                ; Copies the packet in the FIFO to Y memory.
2093                                ;
2094                                ; In: TOTAL_BUFFS and LEFT_TO_READ must be pre-set (via PACKET_PARTITIONS);
2095                                ;     R1 is the destination index in Y memory.
2096                                ; Trashes: R1 is updated to point to the end of the copied data.
2097   
2098                                BUFFER_PACKET
2099   
2100      P:0005CB P:0005CD 54F400            MOVE              #>$b00,A1
                            000B00
2101      P:0005CD P:0005CF 0BF080            JSR     TIMER_STORE_A1
                            00064F
2102      P:0005CF P:0005D1 0BF080            JSR     TIMER_STORE
                            00064D
2103   
2104      P:0005D1 P:0005D3 062400            DO      X:TOTAL_BUFFS,BUFFER_PACKET_HALFS_DONE
                            0005DB
2105      P:0005D3 P:0005D5 0BF080            JSR     WAIT_FIFO_HALF
                            0005F8
2106      P:0005D5 P:0005D7 0BF080            JSR     TIMER_STORE
                            00064D
2107      P:0005D7 P:0005D9 0BF080            JSR     BUFFER_PACKET_HALF
                            0005F3
2108      P:0005D9 P:0005DB 0BF080            JSR     TIMER_STORE
                            00064D
2109      P:0005DB P:0005DD 000000            NOP
2110                                BUFFER_PACKET_HALFS_DONE
2111   
2112      
2113      
2114      
2115      
2116      P:0005DC P:0005DE 01AD81            JCLR    #HF,X:PDRD,BUFFER_PACKET_SINGLES_FAST
                            0005EF
2117   
2118      
2119      
2120   
2121                                BUFFER_PACKET_SINGLES
2122      
2123      
2124      P:0005DE P:0005E0 200013            CLR     A
2125      P:0005DF P:0005E1 20001B            CLR     B
2126      P:0005E0 P:0005E2 51F000            MOVE              X:TCR0,B0               ; Store timer value (50 MHz)
                            FFFF8C
2127      P:0005E2 P:0005E4 0C1C85            ASR     #2,B,B                            ; / 4
2128      P:0005E3 P:0005E5 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_DONE
                            0005EB
2129                                BUFFER_PACKET_SINGLES_WAIT
2130      P:0005E5 P:0005E7 50F000            MOVE              X:TCR0,A0
                            FFFF8C
2131      P:0005E7 P:0005E9 0C1C04            ASR     #2,A,A
2132      P:0005E8 P:0005EA 20000D            CMP     A,B
2133      P:0005E9 P:0005EB 0EA5E5            JEQ     BUFFER_PACKET_SINGLES_WAIT
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 44



2134      P:0005EA P:0005EC 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2135      P:0005EB P:0005ED 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2136                                BUFFER_PACKET_SINGLES_DONE
2137      P:0005EC P:0005EE 0BF080            JSR     TIMER_STORE
                            00064D
2138      P:0005EE P:0005F0 00000C            RTS
2139   
2140                                ;---------------------------------------------------------
2141   
2142                                BUFFER_PACKET_SINGLES_FAST
2143      P:0005EF P:0005F1 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_FAST_DONE
                            0005F1
2144      P:0005F1 P:0005F3 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2145                                BUFFER_PACKET_SINGLES_FAST_DONE
2146      P:0005F2 P:0005F4 00000C            RTS
2147   
2148                                ;---------------------------------------------------------
2149                                BUFFER_PACKET_HALF
2150      
2151      P:0005F3 P:0005F5 060082            DO      #512,BUFFER_PACKET_HALF_DONE
                            0005F6
2152      P:0005F5 P:0005F7 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2153      P:0005F6 P:0005F8 000000            NOP
2154                                BUFFER_PACKET_HALF_DONE
2155      P:0005F7 P:0005F9 00000C            RTS
2156   
2157                                ;---------------------------------------------------------
2158                                WAIT_FIFO_HALF
2159      P:0005F8 P:0005FA 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            00060D
2160      P:0005FA P:0005FC 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
                            0005F8
2161      P:0005FC P:0005FE 000000            NOP
2162      P:0005FD P:0005FF 000000            NOP
2163      P:0005FE P:000600 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            0005F8
2164      P:000600 P:000602 00000C            RTS
2165   
2166                                ;---------------------------------------------------------
2167   
2168                                ; This is the old single-buffering routine, which polls the EF.
2169                                BUFFER_PACKET_SINGLES_POLL
2170      P:000601 P:000603 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_DONE
                            00060B
2171                                BUFFER_PACKET_SINGLE
2172      P:000603 P:000605 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D5
2173      P:000605 P:000607 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE
                            000603
2174      P:000607 P:000609 000000            NOP
2175      P:000608 P:00060A 000000            NOP
2176      P:000609 P:00060B 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE   ; Protect against metastability
                            000603
2177      P:00060B P:00060D 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2178                                BUFFER_PACKET_DONE
2179      P:00060C P:00060E 00000C            RTS
2180   
2181                                ;---------------------------------------------------------
2182   
2183                                FATALITY_HANDLER
2184      P:00060D P:00060F 0C0100            JMP     START                             ; What could possibly go wrong?
2185   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 45



2186   
2187                                ; DROP_PACKET
2188                                ;
2189                                ; Reads a packet from the fifo, discarding it.
2190                                ;
2191                                ; In: TOTAL_BUFFS & LEFT_TO_READ
2192                                ; Trashes: A0
2193   
2194                                DROP_PACKET
2195      P:00060E P:000610 062400            DO      X:TOTAL_BUFFS,DROP_PACKET_SINGLES
                            000613
2196      P:000610 P:000612 0D05F8            JSR     WAIT_FIFO_HALF
2197      P:000611 P:000613 0BF080            JSR     DROP_FIFO_HALF
                            00061E
2198      P:000613 P:000615 000000            NOP
2199                                DROP_PACKET_SINGLES
2200      P:000614 P:000616 062500            DO      X:LEFT_TO_READ,DROP_PACKET_DONE
                            00061C
2201                                DROP_PACKET_SINGLE
2202      P:000616 P:000618 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D5
2203      P:000618 P:00061A 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE
                            000616
2204      P:00061A P:00061C 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE     ; Protect against metastability
                            000616
2205      P:00061C P:00061E 09483F            MOVEP             Y:RDFIFO,A0
2206                                DROP_PACKET_DONE
2207      P:00061D P:00061F 00000C            RTS
2208   
2209                                DROP_FIFO_HALF
2210      
2211      P:00061E P:000620 060082            DO      #512,DROP_FIFO_DONE
                            000620
2212      P:000620 P:000622 09483F            MOVEP             Y:RDFIFO,A0
2213                                DROP_FIFO_DONE
2214      P:000621 P:000623 00000C            RTS
2215   
2216   
2217                                ;----------------------------------------------;
2218                                ;  TIMER HANDLING                              ;
2219                                ;----------------------------------------------;
2220   
2221                                ; Start value is TLR, count is in TCR, flag marked at TCPR
2222                                ; Must set TCSR[TCIE] to enable int
2223                                ; Must set TCSR[T] for timer to restart
2224   
2225                                TIMER_ENABLE
2226      P:000622 P:000624 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2227      P:000624 P:000626 000000            NOP
2228      P:000625 P:000627 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2229      P:000627 P:000629 00000C            RTS
2230   
2231                                TIMER_DISABLE
2232      P:000628 P:00062A 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
                            300200
2233      P:00062A P:00062C 000000            NOP
2234      P:00062B P:00062D 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2235      P:00062D P:00062F 00000C            RTS
2236   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 46



2237                                TIMER_DEFAULT
2238      P:00062E P:000630 0D0628            JSR     TIMER_DISABLE
2239      P:00062F P:000631 44F400            MOVE              #$4C4B40,X0             ; 5M -> 10 Hz.
                            4C4B40
2240      P:000631 P:000633 000000            NOP
2241      P:000632 P:000634 447000            MOVE              X0,X:TCPR0
                            FFFF8D
2242      P:000634 P:000636 0D0622            JSR     TIMER_ENABLE
2243      P:000635 P:000637 00000C            RTS
2244   
2245   
2247                                TIMER_ACTION
2248      P:000636 P:000638 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2249      P:000638 P:00063A 000000            NOP
2250      P:000639 P:00063B 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2251      P:00063B P:00063D 56F000            MOVE              X:QT_INFORM_IDX,A       ; QT inform time?
                            000046
2252      P:00063D P:00063F 0A0182            JCLR    #MODE_QT,X:MODE,TIMER_ACTION_OK
                            000645
2253      P:00063F P:000641 0140C5            CMP     #>0,A                             ; If inform_idx != 0
                            000000
2254      P:000641 P:000643 0AF0AA            JEQ     TIMER_ACTION_OK
                            000645
2255      P:000643 P:000645 0A7034            BSET    #QT_FLUSH,X:STATUS                ;    schedule inform
                            000000
2256                                TIMER_ACTION_OK
2257      P:000645 P:000647 00000C            RTS
2258   
2259   
2260                                ;----------------------------------------------;
2261                                ;  TIMER UTILITY                               ;
2262                                ;----------------------------------------------;
2263   
2264                                 TIMER_SOURCE
2265      FFFF8C                              EQU     TCR0
2266   
2267                                TIMER_STORE_INIT
2268      P:000646 P:000648 50F400            MOVE              #>TIMER_BUFFER,A0
                            201000
2269      P:000648 P:00064A 000000            NOP
2270      P:000649 P:00064B 507000            MOVE              A0,X:TIMER_INDEX
                            00004C
2271      P:00064B P:00064D 211400            MOVE              A0,R4
2272      P:00064C P:00064E 00000C            RTS
2273   
2274                                TIMER_STORE
2275      
2276      
2277      P:00064D P:00064F 56F000            MOVE              X:TIMER_SOURCE,A
                            FFFF8C
2278                                                                                    ; Fall-through
2279   
2280                                TIMER_STORE_A1
2281      
2282      P:00064F P:000651 5C5C00            MOVE                          A1,Y:(R4)+
2283      P:000650 P:000652 228C00            MOVE              R4,A1
2284      P:000651 P:000653 0140C5            CMP     #>TIMER_BUFFER_END,A
                            202000
2285      P:000653 P:000655 547000            MOVE              A1,X:TIMER_INDEX
                            00004C
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 47



2286      P:000655 P:000657 0E1646            JGE     TIMER_STORE_INIT
2287      P:000656 P:000658 00000C            RTS
2288   
2289   
2290                                ;----------------------------------------------;
2291                                ;  CIRCULAR BUFFER HANDLING                    ;
2292                                ;----------------------------------------------;
2293   
2294                                BUFFER_INCR
2295   
2296      P:000657 P:000659 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
                            000042
2297      P:000659 P:00065B 014180            ADD     #1,A                              ;
2298      P:00065A P:00065C 57F000            MOVE              X:QT_BUF_MAX,B          ;
                            00003F
2299      P:00065C P:00065E 20000D            CMP     A,B                               ;
2300      P:00065D P:00065F 0AF0AF            JLE     BUFFER_RESET                      ;       head = 0
                            000669
2301                                                                                    ; else
2302      P:00065F P:000661 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            000042
2303   
2304      P:000661 P:000663 20001B            CLR     B
2305      P:000662 P:000664 51F000            MOVE              X:QT_BUF_SIZE,B0
                            00003E
2306      P:000664 P:000666 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2307      P:000666 P:000668 0BF080            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
                            0006A1
2308   
2309      P:000668 P:00066A 00000C            RTS
2310   
2311   
2312                                BUFFER_RESET
2313      P:000669 P:00066B 60F400            MOVE              #QT_BASE_LO,R0
                            00003C
2314      P:00066B P:00066D 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
2315      P:00066D P:00066F 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2316      P:00066F P:000671 0BF080            JSR     SAVE_HILO_ADDRESS                 ; QT_DEST_LO = QT_BASE_LO
                            0006A3
2317   
2318      P:000671 P:000673 240000            MOVE              #0,X0
2319      P:000672 P:000674 447000            MOVE              X0,X:QT_BUF_HEAD        ; HEAD = 0
                            000042
2320      P:000674 P:000676 00000C            RTS
2321   
2322   
2323                                BUFFER_INFORM_CHECK
2324      P:000675 P:000677 56F000            MOVE              X:QT_INFORM_IDX,A
                            000046
2325      P:000677 P:000679 014180            ADD     #1,A
2326      P:000678 P:00067A 57F000            MOVE              X:QT_INFORM,B
                            000041
2327      P:00067A P:00067C 20000D            CMP     A,B
2328      P:00067B P:00067D 0AF0A7            JGT     BUFFER_INFORM_OK                  ; If inform_idx + 1 <= inform
                            00067F
2329      P:00067D P:00067F 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2330   
2331                                BUFFER_INFORM_OK
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 48



2332      P:00067F P:000681 567000            MOVE              A,X:QT_INFORM_IDX       ; inform_idx = inform_idx + 1
                            000046
2333      P:000681 P:000683 00000C            RTS
2334   
2335   
2336                                ;---------------------------------------------------------------
2337                                BUFFER_INFORM
2338                                ;---------------------------------------------------------------
2339                                ; Informs host of current buffer status
2340   
2341      
2342      P:000682 P:000684 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            00069A
2343      P:000684 P:000686 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            00069A
2344   
2345                                          PCI_LOCKDOWN                              ; Disable host IRQ
2347   
2348      P:000687 P:000689 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2349      P:000689 P:00068B 440B00            MOVE              X0,X:<DTXS_WD1
2350   
2351      P:00068A P:00068C 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            000042
2352      P:00068C P:00068E 440C00            MOVE              X0,X:<DTXS_WD2
2353   
2354      P:00068D P:00068F 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000043
2355      P:00068F P:000691 440D00            MOVE              X0,X:<DTXS_WD3
2356   
2357      P:000690 P:000692 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
                            000047
2358      P:000692 P:000694 440E00            MOVE              X0,X:<DTXS_WD4
2359   
2360      P:000693 P:000695 0D047A            JSR     PCI_MESSAGE_TO_HOST
2361   
2362      P:000694 P:000696 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
2363      P:000696 P:000698 240000            MOVE              #0,X0                   ; Reset inform index
2364      P:000697 P:000699 447000            MOVE              X0,X:QT_INFORM_IDX
                            000046
2365                                          PCI_LOCKUP                                ; Enable host IRQ
2367                                INFORM_EXIT
2368      P:00069A P:00069C 00000C            RTS
2369   
2370   
2371   
2372                                ;----------------------------------------------;
2373                                ;  ADDRESS HANDLING                            ;
2374                                ;----------------------------------------------;
2375   
2379   
2380                                LOAD_HILO_ADDRESS
2381      
2382      
2383      P:00069B P:00069D 200013            CLR     A
2384      P:00069C P:00069E 50D800            MOVE              X:(R0)+,A0
2385      P:00069D P:00069F 44D000            MOVE              X:(R0)-,X0
2386      P:00069E P:0006A0 0C1940            INSERT  #$010010,X0,A
                            010010
2387      P:0006A0 P:0006A2 00000C            RTS
2388   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  main.asm  Page 49



2389                                ADD_HILO_ADDRESS
2390      
2391      
2392   
2393      P:0006A1 P:0006A3 0D069B            JSR     LOAD_HILO_ADDRESS
2394      P:0006A2 P:0006A4 200010            ADD     B,A
2395   
2396                                SAVE_HILO_ADDRESS
2397      
2398      
2399   
2400      P:0006A3 P:0006A5 445800            MOVE              X0,X:(R0)+              ; pre-increment
2401      P:0006A4 P:0006A6 240000            MOVE              #0,X0
2402      P:0006A5 P:0006A7 0C1D11            ASL     #8,A,B
2403      P:0006A6 P:0006A8 0C1940            INSERT  #$008010,X0,A
                            008010
2404      P:0006A8 P:0006AA 555000            MOVE              B1,X:(R0)-              ; store hi16
2405      P:0006A9 P:0006AB 506000            MOVE              A0,X:(R0)
2406      P:0006AA P:0006AC 0C1C90            ASR     #8,B,A
2407      P:0006AB P:0006AD 00000C            RTS
2408   
2409   
2410   
2411   
2412                                BOOTCODE_END
2413                                 BOOTEND_ADDR
2414      0006AC                              EQU     @CVI(BOOTCODE_END)
2415   
2416                                PROGRAM_END
2417      0006AC                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2418                                          INCLUDE 'vars.asm'
2419                                      COMMENT *
2420   
2421                                Variable table and bit defines for our variables.
2422   
2423                                See info.asm for versioning and authors.
2424   
2425                                        *
2426   
2427   
2428                                ; The variable table is mapped to X memory but stored inline in the
2429                                ; eeprom / P memory after the main code (but before the application
2430                                ; area).
2431   
2432      X:000000 P:0006AE                   ORG     X:VAR_TBL,P:
2433   
2434   
2435                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2436                                 VAR_TBL_START
2437      0006AC                              EQU     @LCV(L)-2
2438                                          ENDIF
2439   
2440                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2442                                          ENDIF
2443   
2444   
2445                                ;-----------------------------------------------
2446 d    X:000000 P:0006AE 000000  STATUS    DC      0                                 ; Internal status flags
2447 d    X:000001 P:0006AF 000000  MODE      DC      0                                 ; Operating mode control
2448 d                               FRAME_COUNT
2449 d    X:000002 P:0006B0 000000            DC      0                                 ; Count of data frames from MCE
2450   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  vars.asm  Page 50



2451                                ;-----------------------------------------------
2452 d    X:000003 P:0006B1 550106  REV_NUMBER DC     $550106                           ; byte 0 = minor revision #
2453                                                                                    ; byte 1 = major revision #
2454                                                                                    ; byte 2 = release Version (ascii letter)
2455 d    X:000004 P:0006B2 000000  REV_DATA  DC      $000000                           ; Not used by UBC
2456 d    X:000005 P:0006B3 2EF490  P_CHECKSUM DC     $2EF490                           ; Not used by UBC
2457   
2458                                ;-----------------------------------------------
2459 d    X:000006 P:0006B4 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2460                                ;-----------------------------------------------
2461   
2462   
2464   
2465 d    X:000007 P:0006B5 000000  DRXR_WD1  DC      0                                 ; Storage for words read from PC during vect
or command
2466 d    X:000008 P:0006B6 000000  DRXR_WD2  DC      0
2467 d    X:000009 P:0006B7 000000  DRXR_WD3  DC      0
2468 d    X:00000A P:0006B8 000000  DRXR_WD4  DC      0
2469   
2470 d    X:00000B P:0006B9 000000  DTXS_WD1  DC      0                                 ; Storage for words to be written to PC as r
eply
2471 d    X:00000C P:0006BA 000000  DTXS_WD2  DC      0
2472 d    X:00000D P:0006BB 000000  DTXS_WD3  DC      0
2473 d    X:00000E P:0006BC 000000  DTXS_WD4  DC      0
2474   
2475   
2477   
2478 d    X:00000F P:0006BD 000000  SV_A0     DC      0
2479 d    X:000010 P:0006BE 000000  SV_A1     DC      0
2480 d    X:000011 P:0006BF 000000  SV_A2     DC      0
2481 d    X:000012 P:0006C0 000000  SV_B0     DC      0
2482 d    X:000013 P:0006C1 000000  SV_B1     DC      0
2483 d    X:000014 P:0006C2 000000  SV_B2     DC      0
2484 d    X:000015 P:0006C3 000000  SV_X0     DC      0
2485 d    X:000016 P:0006C4 000000  SV_X1     DC      0
2486 d    X:000017 P:0006C5 000000  SV_Y0     DC      0
2487 d    X:000018 P:0006C6 000000  SV_Y1     DC      0
2488 d    X:000019 P:0006C7 000000  SV_R0     DC      0
2489 d    X:00001A P:0006C8 000000  SV_SR     DC      0
2490   
2491   
2493   
2494 d    X:00001B P:0006C9 000000  HEAD_W1_0 DC      0                                 ; Preamble $A5A5
2495 d    X:00001C P:0006CA 000000  HEAD_W1_1 DC      0                                 ;          $A5A5
2496 d    X:00001D P:0006CB 000000  HEAD_W2_0 DC      0                                 ;          $5A5A
2497 d    X:00001E P:0006CC 000000  HEAD_W2_1 DC      0                                 ;          $5A5A
2498 d    X:00001F P:0006CD 000000  HEAD_W3_0 DC      0                                 ; 'RP' or 'DA'
2499 d    X:000020 P:0006CE 000000  HEAD_W3_1 DC      0                                 ; '  '   $2020
2500 d    X:000021 P:0006CF 000000  HEAD_W4_0 DC      0                                 ; Packet size LSW
2501 d    X:000022 P:0006D0 000000  HEAD_W4_1 DC      0                                 ;             MSW
2502   
2503   
2505   
2506 d                               PACKET_SIZE
2507 d    X:000023 P:0006D1 000000            DC      0                                 ; Size, in dwords of most recent packet from
 MCE.
2508 d                               TOTAL_BUFFS
2509 d    X:000024 P:0006D2 000000            DC      0                                 ; Number of 512 word half-buffers in packet.
2510 d                               LEFT_TO_READ
2511 d    X:000025 P:0006D3 000000            DC      0                                 ; Number of words left to read after last 51
2 buffer
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  vars.asm  Page 51



2512   
2513 d                               PREAMBLE_ERRORS
2514 d    X:000026 P:0006D4 000000            DC      0                                 ; Failed on preamble processing
2515 d                               PTYPE_ERRORS
2516 d    X:000027 P:0006D5 000000            DC      0                                 ; Failed on packet type
2517 d                               PSIZE_ERRORS
2518 d    X:000028 P:0006D6 000000            DC      0                                 ; Failed on packet size test
2519   
2520   
2522   
2523 d                               PCI_BURST_SIZE
2524 d    X:000029 P:0006D7 000040            DC      $40                               ; Should be < 4*latency assigned by OS
2525 d    X:00002A P:0006D8 000000  BURST_SIZE DC     0
2526 d    X:00002B P:0006D9 000000  BLOCK_SIZE DC     0
2527   
2528 d    X:00002C P:0006DA 000000  CON_SRC_LO DC     0                                 ; Set by CON host command
2529 d    X:00002D P:0006DB 000000  CON_SRC_HI DC     0
2530   
2531 d    X:00002E P:0006DC 000000  YMEM_SRC  DC      0                                 ; Vars for YMEM -> PC transfers
2532 d                               BURST_DEST_LO
2533 d    X:00002F P:0006DD 000000            DC      0
2534 d                               BURST_DEST_HI
2535 d    X:000030 P:0006DE 000000            DC      0
2536   
2537 d                               BURST_SRC_LO
2538 d    X:000031 P:0006DF 000000            DC      0                                 ; Vars for PC -> YMEM transfers
2539 d                               BURST_SRC_HI
2540 d    X:000032 P:0006E0 000000            DC      0
2541 d    X:000033 P:0006E1 000000  YMEM_DEST DC      0
2542   
2543 d    X:000034 P:0006E2 000000  DMA_ERRORS DC     0                                 ; Error counting
2544 d    X:000035 P:0006E3 000000  EC_TRTY   DC      0
2545 d    X:000036 P:0006E4 000000  EC_TO     DC      0
2546 d    X:000037 P:0006E5 000000  EC_TDIS   DC      0
2547 d    X:000038 P:0006E6 000000  EC_TAB    DC      0
2548 d    X:000039 P:0006E7 000000  EC_MAB    DC      0
2549 d    X:00003A P:0006E8 000000  EC_DPER   DC      0
2550 d    X:00003B P:0006E9 000000  EC_APER   DC      0
2551   
2552   
2554   
2555 d    X:00003C P:0006EA 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2556 d    X:00003D P:0006EB 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2557 d                               QT_BUF_SIZE
2558 d    X:00003E P:0006EC 000000            DC      0                                 ; Separation of buffers, in bytes
2559 d    X:00003F P:0006ED 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2560 d                               QT_FRAME_SIZE
2561 d    X:000040 P:0006EE 000000            DC      0                                 ; Expected data packet size, in bytes
2562 d    X:000041 P:0006EF 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2563   
2564 d                               QT_BUF_HEAD
2565 d    X:000042 P:0006F0 000000            DC      0                                 ; Index of buf for next write
2566 d                               QT_BUF_TAIL
2567 d    X:000043 P:0006F1 000000            DC      0                                 ; Index at which we must not write
2568   
2569 d    X:000044 P:0006F2 000000  QT_DEST_LO DC     0                                 ; PC address for next write
2570 d    X:000045 P:0006F3 000000  QT_DEST_HI DC     0                                 ;
2571 d                               QT_INFORM_IDX
2572 d    X:000046 P:0006F4 000000            DC      0                                 ; Number of packets since last inform
2573 d    X:000047 P:0006F5 000000  QT_DROPS  DC      0                                 ; Dropped packet count
2574   
2575   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  vars.asm  Page 52



2577   
2578 d    X:000048 P:0006F6 000000  RP_BASE_LO DC     0                                 ; PC buffer start address
2579 d    X:000049 P:0006F7 000000  RP_BASE_HI DC     0                                 ;
2580 d                               RP_MAX_SIZE
2581 d    X:00004A P:0006F8 000000            DC      0                                 ; Maximum reply size, dwords
2582 d    X:00004B P:0006F9 000000  RP_DROPS  DC      0                                 ; Dropped packet count
2583   
2584   
2586   
2587 d                               TIMER_INDEX
2588 d    X:00004C P:0006FA 000000            DC      0
2589   
2590   
2592   
2593 d    X:00004D P:0006FB 000000  BDEBUG0   DC      0
2594 d    X:00004E P:0006FC 000000  BDEBUG1   DC      0
2595 d    X:00004F P:0006FD 000000  BDEBUG2   DC      0
2603   
2605 d                               TRIGGER_FAKE
2606 d    X:000050 P:0006FE 000000            DC      0
2607   
2608 d    X:000051 P:0006FF 000000  CMD_SIZE  DC      0
2609 d    X:000052 P:000700 000000  CMD_WORD  DC      0
2610   
2611 d                               REP_BUS_ADDR
2612 d    X:000053 P:000701 000000            DC      0,0
     d                      000000
2613 d                               DATA_BUS_ADDR
2614 d    X:000055 P:000703 000000            DC      0,0
     d                      000000
2615   
2617      000004                    COMM_REP  EQU     4                                 ; Reply needs to be sent
2618      000007                    COMM_CMD  EQU     7                                 ; Command needs to be processed
2619   
2620 d    X:000057 P:000705 000000  XMEM_SRC  DC      0
2621   
2622      000100                    CMD_BUFFER EQU    $100
2623   
2624   
2630   
2631      000040                    RB_SIZE   EQU     64                                ; This MUST be even, so that effective numbe
r
2632                                                                                    ; of 32-bit words is integral
2633   
2634      000001                    RB_VERSION EQU    1                                 ; Version of this datagram
2635                                 RB_TYPE_DSP_REP
2636      000001                              EQU     1                                 ;
2637                                 RB_TYPE_MCE_REP
2638      000002                              EQU     2                                 ;
2639                                 RB_TYPE_DATA_INF
2640      000003                              EQU     3                                 ;
2641   
2643                                 REP_BUFFER1
2644      X:000058 P:000706                   DS      RB_SIZE
2645   
2647                                 REP_VERSION
2648      000058                              EQU     REP_BUFFER1+0                     ;
2649      000059                    REP_SIZE  EQU     REP_BUFFER1+1                     ;
2650      00005A                    REP_TYPE  EQU     REP_BUFFER1+2                     ;
2651      000068                    REP_DATA  EQU     REP_BUFFER1+16                    ; Start of DSP or MCE reply data
2653      000068                    REP_RSTAT EQU     REP_DATA+0
2654      000069                    REP_RSIZE EQU     REP_DATA+1
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  vars.asm  Page 53



2655      00006A                    REP_RCMD  EQU     REP_DATA+2
2656                                 REP_RPAYLOAD
2657      00006C                              EQU     REP_DATA+4
2658   
2659   
2660   
2661                                ;----------------------------------------------------------
2662   
2664   
2665                                 APPLICATION_RUNNING
2666      000000                              EQU     0                                 ; Indicates application is in progress
2667                                 SEND_TO_HOST
2668      000001                              EQU     1                                 ; set in HST ISR when host ready for packet 
(stays set until after HST reply)
2669                                 FATAL_ERROR
2670      000002                              EQU     2                                 ; PCI message to host error detected by driv
er....
2671      000003                    FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays s
et till packet processed
2672   
2673      000009                    HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of p
acket (stays set until after HST reply)
2674   
2675      00000A                    CON_DEMAND EQU    10                                ; Host has requested an MCE command be sent
2676      00000B                    CON_MCE   EQU     11                                ; Command has been copied to Y buffer and sh
ould be sent to MCE
2677   
2678                                 PCIDMA_RESTART
2679      000010                              EQU     16                                ; DMA flags used for error recovery
2680                                 PCIDMA_RESUME
2681      000011                              EQU     17
2682   
2683      000014                    QT_FLUSH  EQU     20                                ; Set when it is time to inform Host of curr
ent buffer position.
2684                                 RP_BUFFER_FULL
2685      000015                              EQU     21                                ; Set when Quiet RP buffer is occupied.
2686   
2687      000016                    FREEZER   EQU     22                                ; Suspend operations and just idle in the ma
in loop
2688                                 MAIN_LOOP_POLL
2689      000017                              EQU     23                                ; Cleared by the main loop, use to check for
 DSP lock-up
2690   
2691   
2693   
2694                                 MODE_APPLICATION
2695      000000                              EQU     0                                 ; set if PCI application to run
2696      000001                    MODE_MCE  EQU     1                                 ; process packets from MCE (!choke)
2697      000002                    MODE_QT   EQU     2                                 ; Quiet transfer for data packets (QT mode)
2698                                 MODE_RP_BUFFER
2699      000003                              EQU     3                                 ; Quiet transfer for reply packets (Quiet-RP
)
2700   
2701   
2703   
2704                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2705                                 VAR_TBL_END
2706      000744                              EQU     @LCV(L)-2
2707                                          ENDIF
2708   
2709                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2711                                          ENDIF
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  vars.asm  Page 54



2712   
2713                                 VAR_TBL_LENGTH
2714      000098                              EQU     VAR_TBL_END-VAR_TBL_START
2715                                          INCLUDE 'app.asm'
2716                                        COMMENT *
2717   
2718                                Auxiliary application area.
2719   
2720                                See info.asm for versioning and authors.
2721   
2722                                        *
2723                                          PAGE    132                               ; Printronix page width - 132 columns
2724                                          OPT     CEX                               ; print DC evaluations
2725   
2726                                          IF      @CVS(N,*)>=APPLICATION
2728                                          ENDIF
2729   
2730   
2731                                ;--------------------------------------------
2732                                ; APPLICATION AREA
2733                                ;---------------------------------------------
2734                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2735      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2736                                          ENDIF
2737   
2738                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2740                                          ENDIF
2741   
2742                                ; starts with no application loaded
2743                                ; so just reply with an error if we get a GOA command
2744   
2745      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2746      P:000802 P:000804 440B00            MOVE              X0,X:<DTXS_WD1          ; REPly
2747      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2748      P:000805 P:000807 440C00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2749      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2750      P:000808 P:00080A 440D00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2751      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2752      P:00080B P:00080D 440E00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2753      P:00080C P:00080E 0D0493            JSR     <RESTORE_REGISTERS
2754      P:00080D P:00080F 0D047A            JSR     <PCI_MESSAGE_TO_HOST
2755      P:00080E P:000810 0A0100            BCLR    #MODE_APPLICATION,X:<MODE
2756      P:00080F P:000811 0C016E            JMP     PACKET_IN
2757   
2758   
2759   
2760                                          INCLUDE 'hacking.asm'
2761                                                COMMENT *
2762   
2763                                        This implementation does communication with the host using PCI
2764                                        master writes only.
2765   
2766                                        *
2767                                          PAGE    132                               ; Printronix page width - 132 columns
2768                                          OPT     CEX                               ; print DC evaluations
2769   
2770   
2771                                HACK_ENTRY
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  hacking.asm  Page 55



2772      
2773      P:000810 P:000812 0A8985            JCLR    #DSR_HF2,X:DSR,HACK_EXIT
                            000826
2774   
2775                                HACK_INIT
2776      
2777      P:000812 P:000814 0A8524            BSET    #DCTR_HF4,X:DCTR
2778   
2779      
2780      P:000813 P:000815 200013            CLR     A
2781      P:000814 P:000816 000000            NOP
2782      P:000815 P:000817 507000            MOVE              A0,X:CMD_WORD
                            000052
2783      P:000817 P:000819 507000            MOVE              A0,X:TRIGGER_FAKE
                            000050
2784   
2785      P:000819 P:00081B 0BF080            JSR     REPLY_BUFFER_INIT
                            000829
2786   
2787      
2788      P:00081B P:00081D 44F400            MOVE              #>TIMER_BUFFER_END,X0
                            202000
2789      P:00081D P:00081F 4C7000            MOVE                          X0,Y:TIMER_BUFFER_END
                            202000
2790   
2791      
2792      P:00081F P:000821 0A8522            BSET    #DCTR_SRIE,X:DCTR
2793   
2794      
2795      
2796      
2797                                HACK_LOOP
2798      
2800      
2802      P:000820 P:000822 0B00A7            JSSET   #COMM_CMD,X:STATUS,PROCESS_PC_CMD_2
                            000953
2803   
2804      
2805      P:000822 P:000824 0B00A4            JSSET   #COMM_REP,X:STATUS,PROCESS_REPLY
                            000900
2806   
2807      
2808      
2809   
2810      
2811      P:000824 P:000826 0A89A5            JSET    #DSR_HF2,X:DSR,HACK_LOOP
                            000820
2812   
2813                                HACK_EXIT
2814      
2815      P:000826 P:000828 0A8502            BCLR    #DCTR_SRIE,X:DCTR
2816   
2817      P:000827 P:000829 0A8504            BCLR    #DCTR_HF4,X:DCTR
2818      P:000828 P:00082A 00000C            RTS
2819   
2820   
2824   
2825   
2826                                REPLY_BUFFER_INIT
2827      
2828      P:000829 P:00082B 200013            CLR     A
2829      P:00082A P:00082C 60F400            MOVE              #>REP_BUFFER1,R0
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  hacking.asm  Page 56



                            000058
2830                                          .loop   #RB_SIZE
2832      P:00082E P:000830 505800            MOVE              A0,X:(R0)+
2833                                          .endl
2835      P:00082F P:000831 44F400            MOVE              #>RB_VERSION,X0
                            000001
2836      P:000831 P:000833 54F400            MOVE              #>RB_SIZE,A1
                            000040
2837      P:000833 P:000835 447000            MOVE              X0,X:REP_VERSION
                            000058
2838      P:000835 P:000837 547000            MOVE              A1,X:REP_SIZE
                            000059
2839      P:000837 P:000839 0A0004            BCLR    #COMM_REP,X:STATUS
2840      P:000838 P:00083A 00000C            RTS
2841   
2842   
2843                                ;----------------------------------------------
2844                                BLOCK_TRANSFERX
2845                                ;----------------------------------------------
2846                                ;   In:
2847                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address (16:16)
2848                                ;   - BLOCK_SIZE is packet size, in bytes
2849                                ;   - XMEM_SRC is start of data in X memory
2850                                ;  Out:
2851                                ;   - BLOCK_SIZE will be decremented to zero.
2852                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
2853                                ;   - XMEM_SRC will be incremented by BLOCK_SIZE/2
2854                                ;  Trashes:
2855                                ;   - A and B at least
2856   
2857      P:000839 P:00083B 200013            CLR     A
2858      P:00083A P:00083C 56AB00            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
2859      P:00083B P:00083D 014085            CMP     #0,A                              ; Still bytes to transfer?
2860      P:00083C P:00083E 0AF0A2            JNE     BLOCK_TRANSFERX0
                            00083F
2861      P:00083E P:000840 00000C            RTS
2862   
2863                                BLOCK_TRANSFERX0
2864      
2865      
2866      P:00083F P:000841 57A900            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
2867   
2868      P:000840 P:000842 200005            CMP     B,A                               ; A ? B
2869      P:000841 P:000843 0E1843            JGE     <BLOCK_TRANSFERX1                 ; jump if A >= B
2870      P:000842 P:000844 21CF00            MOVE              A,B                     ; This only moves A1,B1.
2871                                BLOCK_TRANSFERX1
2872      
2873      P:000843 P:000845 200014            SUB     B,A                               ; A -= B
2874      P:000844 P:000846 014088            ADD     #0,B                              ; Clear carry bit
2875      P:000845 P:000847 562B00            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
2876      P:000846 P:000848 572A00            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
2877      P:000847 P:000849 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
2878   
2879      
2880      P:000848 P:00084A 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
2881      P:00084A P:00084C 50F000            MOVE              X:XMEM_SRC,A0
                            000057
2882      P:00084C P:00084E 08C82F            MOVEP             A0,X:DSR0               ; DMA source
2883      P:00084D P:00084F 200010            ADD     B,A
2884      P:00084E P:000850 00000B            DEC     B
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  hacking.asm  Page 57



2885      P:00084F P:000851 507000            MOVE              A0,X:XMEM_SRC           ; BURST_SRC += BURST_SIZE/2
                            000057
2886   
2887      P:000851 P:000853 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
2888   
2889      
2890      P:000852 P:000854 08F4AC            MOVEP             #$8EFA50,X:DCR0         ; X to X
                            8EFA50
2891   
2892                                BLOCK_TRANSFERX_PCI
2893      P:000854 P:000856 44F400            MOVE              #>$7,X0                 ; Memory write
                            000007
2894      P:000856 P:000858 302F00            MOVE              #BURST_DEST_LO,R0       ; RAM address
2895      P:000857 P:000859 0D058D            JSR     PCI_GO                            ; Initiate PCI burst
2896   
2897      
2898      P:000858 P:00085A 0A8A84            JCLR    #MARQ,X:DPSR,*
                            000858
2899   
2900      
2901      P:00085A P:00085C 0A8A8E            JCLR    #MDT,X:DPSR,BLOCK_TRANSFERX_HANDLE_ERRORS
                            000860
2902   
2903      P:00085C P:00085E 20001B            CLR     B
2904      P:00085D P:00085F 51AA00            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
2905      P:00085E P:000860 0D06A1            JSR     ADD_HILO_ADDRESS                  ; Update source address
2906      P:00085F P:000861 0C0839            JMP     BLOCK_TRANSFERX                   ; Next burst in block
2907   
2908                                BLOCK_TRANSFERX_HANDLE_ERRORS
2909      
2910      P:000860 P:000862 0D04BE            JSR     PCI_ERROR_CLEAR
2911   
2912      P:000861 P:000863 0A0010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
2913      P:000862 P:000864 0E8854            JCS     BLOCK_TRANSFERX_PCI               ; Restart PCI burst
2914   
2915      P:000863 P:000865 0A0011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
2916      P:000864 P:000866 0E0839            JCC     BLOCK_TRANSFERX                   ; Error but no error? Redo this burst.
2917   
2918      
2919      P:000865 P:000867 0D05A3            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
2920      P:000866 P:000868 0D05B3            JSR     PCI_UPDATE_R0
2921      P:000867 P:000869 0C0854            JMP     BLOCK_TRANSFERX_PCI
2922   
2923   
2925      P:000900 P:000902                   ORG     P:$900,P:$902
2926   
2927                                PROCESS_REPLY
2933   
2935      
2936      P:000900 P:000902 60F400            MOVE              #>REP_BUS_ADDR,R0
                            000053
2937      P:000902 P:000904 61F400            MOVE              #>BURST_DEST_LO,R1
                            00002F
2938                                          .loop   #2
2940      P:000906 P:000908 44D800            MOVE              X:(R0)+,X0
2941      P:000907 P:000909 445900            MOVE              X0,X:(R1)+
2942                                          .endl
2944   
2945      
2946      P:000908 P:00090A 44F400            MOVE              #>(RB_SIZE*2),X0
                            000080
2947      P:00090A P:00090C 442B00            MOVE              X0,X:BLOCK_SIZE
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  hacking.asm  Page 58



2948      P:00090B P:00090D 44F400            MOVE              #>REP_BUFFER1,X0
                            000058
2949      P:00090D P:00090F 447000            MOVE              X0,X:XMEM_SRC
                            000057
2950   
2951      
2952      P:00090F P:000911 0D0839            JSR     BLOCK_TRANSFERX
2953   
2954      
2955      P:000910 P:000912 0A0004            BCLR    #COMM_REP,X:STATUS
2956      P:000911 P:000913 44F400            MOVE              #>0,X0
                            000000
2957      P:000913 P:000915 44F000            MOVE              X:CMD_WORD,X0
                            000052
2958      P:000915 P:000917 54F400            MOVE              #>1,A1
                            000001
2959      P:000917 P:000919 447000            MOVE              X0,X:REP_RSTAT          ; mark as sent!
                            000068
2960      P:000919 P:00091B 547000            MOVE              A1,X:REP_RSIZE
                            000069
2961   
2962      
2963      P:00091B P:00091D 0A8526            BSET    #INTA,X:DCTR
2964   
2965      P:00091C P:00091E 0A8983            JCLR    #DSR_HF0,X:DSR,*
                            00091C
2966      P:00091E P:000920 0A8506            BCLR    #INTA,X:DCTR
2967   
2968      P:00091F P:000921 0A89A3            JSET    #DSR_HF0,X:DSR,*
                            00091F
2969   
2970      P:000921 P:000923 00000C            RTS
2971   
2972   
2976   
2978      000001                    CMD_READ_P EQU    1
2979      000002                    CMD_READ_X EQU    2
2980      000003                    CMD_READ_Y EQU    3
2981   
2982                                 CMD_WRITE_P
2983      000005                              EQU     5
2984                                 CMD_WRITE_X
2985      000006                              EQU     6
2986                                 CMD_WRITE_Y
2987      000007                              EQU     7
2988   
2989                                 CMD_SET_REP_BUF
2990      000009                              EQU     9
2991                                 CMD_SET_DATA_BUF
2992      00000A                              EQU     $A
2993   
2994   
2995                                 CMD_READ_CODED
2996      000011                              EQU     $11
2997                                 CMD_WRITE_CODED
2998      000012                              EQU     $12
2999   
3000                                 CMD_SEND_MCE
3001      000021                              EQU     $21
3002   
3003                                 CMD_SEND_STUFF
3004      000031                              EQU     $31
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  hacking.asm  Page 59



3005   
3006      000065                    CMD_STATUS EQU    $65
3007                                 CMD_RECV_MCE
3008      000066                              EQU     $66
3009   
3010   
3011                                PROCESS_PC_CMD_INT
3012      
3013      P:000922 P:000924 0D04A0            JSR     SAVE_REGISTERS
3014      P:000923 P:000925 0A8502            BCLR    #DCTR_SRIE,X:DCTR
3015   
3016      
3017      P:000924 P:000926 0A8982            JCLR    #SRRQ,X:DSR,PROCESS_PC_CMD_INT_EXIT
                            00093A
3018   
3019      
3020      
3021      P:000926 P:000928 08440B            MOVEP             X:DRXR,X0
3022      P:000927 P:000929 305100            MOVE              #CMD_SIZE,R0
3023      P:000928 P:00092A 0BF080            JSR     PROCESS_SPLIT_X0_XR0
                            0009DF
3024   
3025      
3026      P:00092A P:00092C 200013            CLR     A
3027      P:00092B P:00092D 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3028      P:00092D P:00092F 54F000            MOVE              X:CMD_SIZE,A1
                            000051
3029      
3030      P:00092F P:000931 014085            CMP     #0,A
3031      P:000930 P:000932 0AF0AA            JEQ     PROCESS_PC_CMD_INT_OK
                            000939
3032                                          .loop   A1
3034      P:000934 P:000936 0A8982            JCLR    #SRRQ,X:DSR,*
                            000934
3035      P:000936 P:000938 08588B            MOVEP             X:DRXR,X:(R0)+
3036      P:000937 P:000939 000000            NOP
3037      P:000938 P:00093A 000000            NOP
3038                                          .endl
3040   
3041                                PROCESS_PC_CMD_INT_OK
3042      P:000939 P:00093B 0A0027            BSET    #COMM_CMD,X:STATUS
3043   
3044                                PROCESS_PC_CMD_INT_EXIT
3045      
3046      P:00093A P:00093C 0A8522            BSET    #DCTR_SRIE,X:DCTR
3047      P:00093B P:00093D 0D0493            JSR     RESTORE_REGISTERS
3048      P:00093C P:00093E 000004            RTI
3049   
3050   
3051                                PROCESS_PC_CMD
3052      
3053      P:00093D P:00093F 0A89A2            JSET    #SRRQ,X:DSR,PROCESS_PC_CMD_1
                            000940
3054      P:00093F P:000941 00000C            RTS
3055   
3056                                PROCESS_PC_CMD_1
3057      
3058      P:000940 P:000942 08440B            MOVEP             X:DRXR,X0
3059      P:000941 P:000943 305100            MOVE              #CMD_SIZE,R0
3060      P:000942 P:000944 0BF080            JSR     PROCESS_SPLIT_X0_XR0
                            0009DF
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  hacking.asm  Page 60



3061   
3062      
3063      P:000944 P:000946 200013            CLR     A
3064      P:000945 P:000947 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3065      P:000947 P:000949 54F000            MOVE              X:CMD_SIZE,A1
                            000051
3066      
3067      P:000949 P:00094B 014085            CMP     #0,A
3068      P:00094A P:00094C 0AF0AA            JEQ     PROCESS_PC_CMD_2
                            000953
3069                                          .loop   A1
3071      P:00094E P:000950 0A8982            JCLR    #SRRQ,X:DSR,*
                            00094E
3072      P:000950 P:000952 08588B            MOVEP             X:DRXR,X:(R0)+
3073      P:000951 P:000953 000000            NOP
3074      P:000952 P:000954 000000            NOP
3075                                          .endl
3077   
3078                                PROCESS_PC_CMD_2
3079      
3080      P:000953 P:000955 54F400            MOVE              #>RB_TYPE_DSP_REP,A1
                            000001
3081      P:000955 P:000957 57F000            MOVE              X:CMD_WORD,B
                            000052
3082      P:000957 P:000959 547000            MOVE              A1,X:REP_TYPE           ; type is "dsp reply"
                            00005A
3083      P:000959 P:00095B 557000            MOVE              B1,X:REP_RCMD           ; copy of command word
                            00006A
3084      P:00095B P:00095D 517000            MOVE              B0,X:REP_RSTAT          ; status = 0
                            000068
3085      P:00095D P:00095F 517000            MOVE              B0,X:REP_RSIZE          ; data size = 0
                            000069
3086   
3087      
3088   
3089      
3090      P:00095F P:000961 60F000            MOVE              X:CMD_BUFFER,R0
                            000100
3091      P:000961 P:000963 44F000            MOVE              X:(CMD_BUFFER+1),X0
                            000101
3092   
3093      P:000963 P:000965 01418D            CMP     #CMD_READ_P,B
3094      P:000964 P:000966 0AF0AA            JEQ     PROCESS_READ_P
                            000982
3095   
3096      P:000966 P:000968 01428D            CMP     #CMD_READ_X,B
3097      P:000967 P:000969 0AF0AA            JEQ     PROCESS_READ_X
                            000985
3098   
3099      P:000969 P:00096B 01438D            CMP     #CMD_READ_Y,B
3100      P:00096A P:00096C 0AF0AA            JEQ     PROCESS_READ_Y
                            000988
3101   
3102      P:00096C P:00096E 01458D            CMP     #CMD_WRITE_P,B
3103      P:00096D P:00096F 0AF0AA            JEQ     PROCESS_WRITE_P
                            000996
3104   
3105      P:00096F P:000971 01468D            CMP     #CMD_WRITE_X,B
3106      P:000970 P:000972 0AF0AA            JEQ     PROCESS_WRITE_X
                            000999
3107   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  hacking.asm  Page 61



3108      P:000972 P:000974 01478D            CMP     #CMD_WRITE_Y,B
3109      P:000973 P:000975 0AF0AA            JEQ     PROCESS_WRITE_Y
                            00099C
3110   
3111      P:000975 P:000977 01618D            CMP     #CMD_SEND_MCE,B
3112      P:000976 P:000978 0AF0AA            JEQ     PROCESS_SEND_MCE
                            0009CE
3113   
3114      P:000978 P:00097A 01498D            CMP     #CMD_SET_REP_BUF,B
3115      P:000979 P:00097B 0AF0AA            JEQ     PROCESS_SET_REP_BUFFER
                            00099F
3116   
3117      P:00097B P:00097D 01498D            CMP     #CMD_SET_REP_BUF,B
3118      P:00097C P:00097E 0AF0AA            JEQ     PROCESS_SET_DATA_BUFFER
                            0009B6
3119   
3120      P:00097E P:000980 01718D            CMP     #CMD_SEND_STUFF,B
3121      P:00097F P:000981 0AF0AA            JEQ     PROCESS_SEND_STUFF
                            0009C5
3122   
3123   
3124      
3125      P:000981 P:000983 00000C            RTS
3126   
3127                                PROCESS_READ_P
3128      P:000982 P:000984 07E084            MOVE              P:(R0),X0
3129      P:000983 P:000985 0AF080            JMP     PROCESS_READ_EXIT
                            00098B
3130                                PROCESS_READ_X
3131      P:000985 P:000987 44E000            MOVE              X:(R0),X0
3132      P:000986 P:000988 0AF080            JMP     PROCESS_READ_EXIT
                            00098B
3133                                PROCESS_READ_Y
3134      P:000988 P:00098A 4CE000            MOVE                          Y:(R0),X0
3135      P:000989 P:00098B 0AF080            JMP     PROCESS_READ_EXIT
                            00098B
3136   
3137                                PROCESS_READ_EXIT
3138      
3139      P:00098B P:00098D 60F400            MOVE              #>REP_RPAYLOAD,R0
                            00006C
3140      P:00098D P:00098F 0BF080            JSR     PROCESS_SPLIT_X0_XR0
                            0009DF
3141      
3142      P:00098F P:000991 44F400            MOVE              #>1,X0
                            000001
3143      P:000991 P:000993 447000            MOVE              X0,X:REP_RSIZE
                            000069
3144      
3145      P:000993 P:000995 0A0007            BCLR    #COMM_CMD,X:STATUS
3146      P:000994 P:000996 0A0024            BSET    #COMM_REP,X:STATUS
3147      P:000995 P:000997 00000C            RTS
3148   
3149                                PROCESS_WRITE_P
3150      P:000996 P:000998 076084            MOVE              X0,P:(R0)
3151      P:000997 P:000999 0AF080            JMP     PROCESS_SIMPLE_EXIT
                            0009DC
3152                                PROCESS_WRITE_X
3153      P:000999 P:00099B 446000            MOVE              X0,X:(R0)
3154      P:00099A P:00099C 0AF080            JMP     PROCESS_SIMPLE_EXIT
                            0009DC
3155                                PROCESS_WRITE_Y
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  hacking.asm  Page 62



3156      P:00099C P:00099E 4C6000            MOVE                          X0,Y:(R0)
3157      P:00099D P:00099F 0AF080            JMP     PROCESS_SIMPLE_EXIT
                            0009DC
3158   
3159   
3160                                PROCESS_SET_REP_BUFFER
3161      
3162      
3163      P:00099F P:0009A1 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3164      P:0009A1 P:0009A3 315300            MOVE              #REP_BUS_ADDR,R1
3165                                          .loop   #2
3167      P:0009A4 P:0009A6 44D800            MOVE              X:(R0)+,X0
3168      P:0009A5 P:0009A7 445900            MOVE              X0,X:(R1)+
3169                                          .endl
3171   
3172      
3173      P:0009A6 P:0009A8 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3174      P:0009A8 P:0009AA 61F400            MOVE              #>$100,R1
                            000100
3175                                          .loop   #2
3177      P:0009AC P:0009AE 44D800            MOVE              X:(R0)+,X0
3178      P:0009AD P:0009AF 4C5900            MOVE                          X0,Y:(R1)+
3179                                          .endl
3181   
3182      
3183      P:0009AE P:0009B0 44F400            MOVE              #>0,X0
                            000000
3184      P:0009B0 P:0009B2 447000            MOVE              X0,X:REP_RSTAT
                            000068
3185      P:0009B2 P:0009B4 447000            MOVE              X0,X:REP_RSIZE
                            000069
3186      P:0009B4 P:0009B6 0A0007            BCLR    #COMM_CMD,X:STATUS
3187      P:0009B5 P:0009B7 00000C            RTS
3188   
3189                                PROCESS_SET_DATA_BUFFER
3190      
3191      
3192      P:0009B6 P:0009B8 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3193      P:0009B8 P:0009BA 315500            MOVE              #DATA_BUS_ADDR,R1
3194                                          .loop   #2
3196      P:0009BB P:0009BD 44D800            MOVE              X:(R0)+,X0
3197      P:0009BC P:0009BE 445900            MOVE              X0,X:(R1)+
3198                                          .endl
3200   
3201      
3202      P:0009BD P:0009BF 44F400            MOVE              #>0,X0
                            000000
3203      P:0009BF P:0009C1 447000            MOVE              X0,X:REP_RSTAT
                            000068
3204      P:0009C1 P:0009C3 447000            MOVE              X0,X:REP_RSIZE
                            000069
3205      P:0009C3 P:0009C5 0A0007            BCLR    #COMM_CMD,X:STATUS
3206      P:0009C4 P:0009C6 00000C            RTS
3207   
3208                                PROCESS_SEND_STUFF
3209      
3210      P:0009C5 P:0009C7 0A7021            BSET    #1,X:TRIGGER_FAKE
                            000050
3211   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  hacking.asm  Page 63



3212      P:0009C7 P:0009C9 44F400            MOVE              #>0,X0
                            000000
3213      P:0009C9 P:0009CB 447000            MOVE              X0,X:REP_RSTAT
                            000068
3214      P:0009CB P:0009CD 447000            MOVE              X0,X:REP_RSIZE
                            000069
3215      P:0009CD P:0009CF 00000C            RTS
3216   
3217   
3218                                PROCESS_SEND_MCE
3219      
3220      
3221      
3222      P:0009CE P:0009D0 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3223                                          .loop   #128
3225      P:0009D2 P:0009D4 54D800            MOVE              X:(R0)+,A1              ; b2, b1  (lsb)
3226      P:0009D3 P:0009D5 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
3227      P:0009D4 P:0009D6 0140C6            AND     #>$FF,A
                            0000FF
3228      P:0009D6 P:0009D8 547000            MOVE              A1,X:FO_SEND
                            FFF000
3229      P:0009D8 P:0009DA 557000            MOVE              B1,X:FO_SEND
                            FFF000
3230                                          .endl
3232      P:0009DA P:0009DC 0AF080            JMP     PROCESS_SIMPLE_EXIT
                            0009DC
3233   
3234                                PROCESS_SIMPLE_EXIT
3235      
3236      P:0009DC P:0009DE 0A0007            BCLR    #COMM_CMD,X:STATUS
3237      P:0009DD P:0009DF 0A0024            BSET    #COMM_REP,X:STATUS
3238      P:0009DE P:0009E0 00000C            RTS
3239   
3240   
3241                                PROCESS_SPLIT_X0_XR0
3242      
3243      
3244      P:0009DF P:0009E1 208800            MOVE              X0,A0
3245      P:0009E0 P:0009E2 0C1881            EXTRACTU #$010000,A,B
                            010000
3246      P:0009E2 P:0009E4 0C1880            EXTRACTU #$008010,A,A                     ; Put
                            008010
3247      P:0009E4 P:0009E6 515800            MOVE              B0,X:(R0)+
3248      P:0009E5 P:0009E7 505800            MOVE              A0,X:(R0)+
3249      P:0009E6 P:0009E8 00000C            RTS
3250   
3251                                PROCESS_SPLIT_X0_YR0
3252      
3253      
3254      P:0009E7 P:0009E9 208800            MOVE              X0,A0
3255      P:0009E8 P:0009EA 0C1881            EXTRACTU #$010000,A,B
                            010000
3256      P:0009EA P:0009EC 0C1880            EXTRACTU #$008010,A,A                     ; Put
                            008010
3257      P:0009EC P:0009EE 595800            MOVE                          B0,Y:(R0)+
3258      P:0009ED P:0009EF 585800            MOVE                          A0,Y:(R0)+
3259      P:0009EE P:0009F0 00000C            RTS
3260   
3261   
3262   
3267   
Motorola DSP56300 Assembler  Version 6.3.4   13-05-21  15:18:35  hacking.asm  Page 64



3268                                FAKE_PACKET
3269      P:0009EF P:0009F1 200013            CLR     A
3270      P:0009F0 P:0009F2 54F000            MOVE              X:TRIGGER_FAKE,A1
                            000050
3271      P:0009F2 P:0009F4 014085            CMP     #0,A
3272      P:0009F3 P:0009F5 0AF0AA            JEQ     FAKE_PACKET_2
                            000A0B
3273   
3275      P:0009F5 P:0009F7 0D0900            JSR     PROCESS_REPLY
3276      P:0009F6 P:0009F8 44F400            MOVE              #>0,X0
                            000000
3277      P:0009F8 P:0009FA 447000            MOVE              X0,X:TRIGGER_FAKE
                            000050
3278      P:0009FA P:0009FC 00000C            RTS
3279   
3280      
3281      P:0009FB P:0009FD 60F400            MOVE              #>REP_BUS_ADDR,R0
                            000053
3282      P:0009FD P:0009FF 61F400            MOVE              #>BURST_DEST_LO,R1
                            00002F
3283                                          .loop   #2
3285      P:000A01 P:000A03 44D800            MOVE              X:(R0)+,X0
3286      P:000A02 P:000A04 445900            MOVE              X0,X:(R1)+
3287                                          .endl
3289   
3290      P:000A03 P:000A05 44F400            MOVE              #>(RB_SIZE*2),X0
                            000080
3291      P:000A05 P:000A07 442B00            MOVE              X0,X:BLOCK_SIZE
3292      P:000A06 P:000A08 44F400            MOVE              #>REP_BUFFER1,X0
                            000058
3293      P:000A08 P:000A0A 447000            MOVE              X0,X:XMEM_SRC
                            000057
3294   
3295      
3296      P:000A0A P:000A0C 0D0839            JSR     BLOCK_TRANSFERX
3297                                FAKE_PACKET_2
3298      P:000A0B P:000A0D 00000C            RTS
3299   
3300   
3301                                DEBUG_UP
3302      P:000A0C P:000A0E 0A8525            BSET    #DCTR_HF5,X:DCTR
3303      P:000A0D P:000A0F 00000C            RTS
3304   
3305                                DEBUG_DOWN
3306      P:000A0E P:000A10 0A8505            BCLR    #DCTR_HF5,X:DCTR
3307      P:000A0F P:000A11 00000C            RTS
3308   
3309      000A12                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM

0    Errors
3    Warnings


