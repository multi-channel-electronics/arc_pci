Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  build.asm  Page 1



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
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  header.asm  Page 2



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
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  header.asm  Page 3



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
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  header.asm  Page 4



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
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  header.asm  Page 5



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
286  d    P:000000 P:000000 000BC1            DC      END_ADR-INIT-2                    ; Number of boot words
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
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  init.asm  Page 6



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
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  init.asm  Page 7



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
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  init.asm  Page 8



361    
362       P:00006A P:00006C                   ORG     P:$6A,P:$6C
363                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
365                                           ENDIF
366    
367       P:00006A P:00006C 0BF080            JSR     PROCESS_PC_CMD_INT                ; PCI slave req vector
                            0009BD
368    
369                                 ;**************************************************************************
370                                 ; Check for program space overwriting of ISR starting at P:$72
371                                           IF      @CVS(N,*)>$71
373                                           ENDIF
374    
375       P:000072 P:000074                   ORG     P:$72,P:$74
376    
377                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
378                                 ; command converter
379                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
381                                           ENDIF
382    
383    
384                                 ;**************************************************************************
385    
386                                 ; Three non-maskable fast interrupt service routines for clearing PCI interrupts
387                                 ; The Host will use these to clear the INTA* after it has serviced the interrupt
388                                 ; which had been generated by the PCI board.
389    
390       P:000072 P:000074 0A8506            BCLR    #INTA,X:DCTR                      ; $72/3 - Clear PCI interrupt
391       P:000073 P:000075 000000            NOP
392    
393       P:000074 P:000076 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; clear interrupt flag
394       P:000075 P:000077 000000            NOP                                       ; needs to be fast addressing <
395    
396       P:000076 P:000078 0A0022            BSET    #FATAL_ERROR,X:<STATUS            ; $76/7 - driver PCI_MESSAGE_TO_HOST error
397       P:000077 P:000079 000000            NOP
398    
399                                 ; Interrupt locations for 7 available commands on PCI board
400                                 ; Each JSR takes up 2 locations in the table
401       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            000349
402       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            00031F
403       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            00036A
404       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            000373
405                                 ; software reset is the same as cleaning up the PCI - use same routine
406                                 ; when HOST does a RESET then this routine is run
407       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            00044B
408       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            00045C
409       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            00043C
410       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            00037E
411    
412                                 ; QT - set command
413       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            00039C
414       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            000434
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  init.asm  Page 9



415    
416                                 ; Quiet RP mode, clear buffer full flag
417       P:00008C P:00008E 0A0015            BCLR    #RP_BUFFER_FULL,X:<STATUS         ; $8C
418       P:00008D P:00008F 000000            NOP
419    
420                                 ; ***********************************************************************
421                                 ; For now have boot code starting from P:$100
422                                 ; just to make debugging tidier etc.
423    
424       P:000100 P:000102                   ORG     P:$100,P:$102
425    
426                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
427                                 ; command converter
428                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
430                                           ENDIF
431                                 ; ***********************************************************************
432    
433    
434    
435                                 ; ******************************************************************
436                                 ;
437                                 ;       AA0 = RDFIFO* of incoming fiber optic data
438                                 ;       AA1 = EEPROM access
439                                 ;       AA2 = DRAM access
440                                 ;       AA3 = output to parallel data connector, for a video pixel clock
441                                 ;       $FFxxxx = Write to fiber optic transmitter
442                                 ;
443                                 ; ******************************************************************
444    
445    
446       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
447       P:000102 P:000104 08F485            MOVEP             #>$100000,X:DCTR        ; Set PCI mode
                            100000
448       P:000104 P:000106 000000            NOP
449       P:000105 P:000107 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
450       P:000106 P:000108 000000            NOP
451       P:000107 P:000109 000000            NOP                                       ; End of PCI programming
452    
453    
454                                 ; Set operation mode register OMR to normal expanded
455       P:000108 P:00010A 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
456       P:000109 P:00010B 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
457    
458                                 ; Program the serial port ESSI0 = Port C for serial transmission to
459                                 ;   the timing board
460       P:00010A P:00010C 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
461                                 ;**********************************************************************
462       P:00010C P:00010E 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
463                                                                                     ; DC0-CD4 = 0 for non-network operation
464                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
465                                                                                     ; SSC1 = 0 for SC1 not used
466                                 ;************************************************************************
467       P:00010E P:000110 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
                            010120
468                                                                                     ; SHFD = 0 for MSB shifted first
469                                                                                     ; CKP = 0 for rising clock edge transitions
470                                                                                     ; TE0 = 1 to enable transmitter #0
471                                                                                     ; MOD = 0 for normal, non-networked mode
472                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  init.asm  Page 10



473       P:000110 P:000112 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
474                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
475                                 ;********************************************************************************
476       P:000112 P:000114 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
477       P:000114 P:000116 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
                            000000
478                                 ;***********************************************************************************
479                                 ; 250MHz
480                                 ; Conversion from software bits to schematic labels for Port C and D
481                                 ;       PC0 = SC00 = AUX3               PD0 = SC10 = EF*
482                                 ;       PC1 = SC01 = A/B* = input       PD1 = SC11 = HF*
483                                 ;       PC2 = SC02 = No connect         PD2 = SC12 = RS*
484                                 ;       PC3 = SCK0 = No connect         PD3 = SCK1 = NWRFIFO*
485                                 ;       PC4 = SRD0 = AUX1               PD4 = SRD1 = No connect (** in 50Mhz this was MODE selec
t for 16 or 32 bit FO)
486                                 ;       PC5 = STD0 = No connect         PD5 = STD1 = WRFIFO*
487                                 ; ***********************************************************************************
488    
489    
490                                 ; ****************************************************************************
491                                 ; Program the serial port ESSI1 = Port D for general purpose I/O (GPIO)
492    
493       P:000116 P:000118 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
494       P:000118 P:00011A 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            00001C
495       P:00011A P:00011C 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
496       P:00011C P:00011E 060AA0            REP     #10
497       P:00011D P:00011F 000000            NOP
498       P:00011E P:000120 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
499                                                                                     ; was %011100
500    
501                                 ; Program the SCI port to benign values
502       P:000120 P:000122 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
503       P:000122 P:000124 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
504       P:000124 P:000126 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
505                                 ;       PE0 = RXD
506                                 ;       PE1 = TXD
507                                 ;       PE2 = SCLK
508    
509                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
510       P:000126 P:000128 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
511       P:000128 P:00012A 07F40B            MOVEP             #$2800,X:TCSR1
                            002800
512       P:00012A P:00012C 07F407            MOVEP             #$2800,X:TCSR2
                            002800
513    
514    
515                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
516       P:00012C P:00012E 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
517       P:00012E P:000130 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  init.asm  Page 11



e
                            008929
518       P:000130 P:000132 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
                            000122
519    
520    
521                                 ; Program the DRAM memory access and addressing
522       P:000132 P:000134 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
                            020022
523       P:000134 P:000136 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
524    
525    
526                                 ; Clear all PCI error conditions
527       P:000136 P:000138 084E0A            MOVEP             X:DPSR,A
528       P:000137 P:000139 0140C2            OR      #$1FE,A
                            0001FE
529       P:000139 P:00013B 000000            NOP
530       P:00013A P:00013C 08CE0A            MOVEP             A,X:DPSR
531    
532                                 ; Status word and interrupt configuration.
533       P:00013B P:00013D 08F4BF            MOVEP             #>MY_IPRC,X:IPRC
                            0001C0
534       P:00013D P:00013F 08F4BE            MOVEP             #>MY_IPRP,X:IPRP
                            000002
535       P:00013F P:000141 05F439            MOVE              #>MY_SR,SR
                            000100
536    
537    
538                                 ;--------------------------------------------------------------------------
539                                 ; Initialize the fiber optic serial transmitter to zero
540       P:000141 P:000143 01B786            JCLR    #TDE,X:SSISR0,*
                            000141
541       P:000143 P:000145 07F43C            MOVEP             #$000000,X:TX00
                            000000
542    
543                                 ;--------------------------------------------------------------------
544    
545                                 ; clear DTXM - PCI master transmitter
546       P:000145 P:000147 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
547       P:000146 P:000148 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000146
548    
549                                 ;----------------------------------------------------------------------
550                                 ; clear DRXR - PCI receiver
551    
552       P:000148 P:00014A 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014D
553       P:00014A P:00014C 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
554       P:00014B P:00014D 000000            NOP
555       P:00014C P:00014E 0C0148            JMP     <CLR0
556                                 CLR1
557    
558                                 ;-----------------------------------------------------------------------------
559                                 ; copy parameter table from P memory into X memory
560    
561                                 ; but not frame_count and num_dumped - don't want these reset by fatal error...
562                                 ; they will be reset by new packet or pci_reset ISR
563    
564       P:00014D P:00014F 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000006
565       P:00014F P:000151 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  init.asm  Page 12



                            000002
566    
567                                 ; Move the table of constants from P: space to X: space
568       P:000151 P:000153 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            0006AC
569       P:000153 P:000155 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
570       P:000154 P:000156 06FF80            DO      #VAR_TBL_LENGTH,X_WRITE
                            000157
571       P:000156 P:000158 07D984            MOVE              P:(R1)+,X0
572       P:000157 P:000159 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
573                                 X_WRITE
574    
575       P:000158 P:00015A 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
                            000006
576       P:00015A P:00015C 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000002
577    
578    
579                                 ;----------------------------------------------------------------------------
580                                 ; Initialize PCI controller again, after booting, to make sure it sticks
581    
582       P:00015C P:00015E 08F485            MOVEP             #>$000000,X:DCTR
                            000000
583       P:00015E P:000160 000000            NOP
584       P:00015F P:000161 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00015F
585       P:000161 P:000163 08F485            MOVEP             #>$100000,X:DCTR
                            100000
586       P:000163 P:000165 000000            NOP
587       P:000164 P:000166 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000164
588    
589       
590       P:000166 P:000168 0BF080            JSR     CLEAR_FO_FIFO                     ; Clear the fibre fifo!
                            0004B4
591       P:000168 P:00016A 013D24            BSET    #AUX1,X:PDRC                      ; Enable byte-swapping - still necc. on ARC-
64
592       P:000169 P:00016B 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Enable host interrupts
593       P:00016A P:00016C 0BF080            JSR     TIMER_DEFAULT                     ; Enable timer (channel 0) for misc. uses
                            00062E
594       P:00016C P:00016E 0BF080            JSR     TIMER_STORE_INIT                  ; Initialize timing buffer
                            000646
595    
597                                           INCLUDE 'main.asm'
598    
599                                                 COMMENT *
600    
601                                 Main section of the pci card code.
602    
603                                 See info.asm for versioning and authors.
604    
605                                         *
606                                           PAGE    132                               ; Printronix page width - 132 columns
607                                           OPT     CEX                               ; print DC evaluations
608    
609    
610    
614    
615                                 PACKET_IN
616    
617       
618       P:00016E P:000170 0A0017            BCLR    #MAIN_LOOP_POLL,X:<STATUS
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 13



619    
620       
621       P:00016F P:000171 0A00B6            JSET    #FREEZER,X:<STATUS,PACKET_IN
                            00016E
622    
623       
624       P:000171 P:000173 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
625    
626       
627       P:000173 P:000175 0A01A0            JSET    #MODE_APPLICATION,X:<MODE,APPLICATION
                            000800
628    
629       
630       P:000175 P:000177 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            000636
631    
632       
633       P:000177 P:000179 0B00B4            JSSET   #QT_FLUSH,X:STATUS,BUFFER_INFORM
                            000682
634    
635       
636       P:000179 P:00017B 0D0470            JSR     <CHECK_FO
637       P:00017A P:00017C 0B00A3            JSSET   #FO_WRD_RCV,X:STATUS,HANDLE_FIFO
                            00018A
638    
639       
640       P:00017C P:00017E 0B00AB            JSSET   #CON_MCE,X:STATUS,CON_TRANSMIT
                            000288
641       P:00017E P:000180 0B00AA            JSSET   #CON_DEMAND,X:STATUS,CON_BUFFER
                            00026B
642    
643       
644       P:000180 P:000182 0BF080            JSR     HACK_ENTRY
                            000810
645    
646       
647       P:000182 P:000184 0C016E            JMP     PACKET_IN
648    
652    
653                                 ; PCI semaphore
654                                 ;
655                                 ; In order for routines in non-interrupt context to write to the
656                                 ; DTXS, (via PCI_MESSAGE_TO_HOST) they need to end up with
657                                 ; interrupts disabled and HCF3 cleared.
658                                 ;
659                                 ; Non-interrupt PCIers should use macro
660                                 ;       PCI_LOCKDOWN
661                                 ; to get exclusive access and then release it with
662                                 ;       PCI_LOCKUP
663                                 ; after calling PCI_MESSAGE_TO_HOST.
664    
665                                  PCI_LOCKDOWN
666                                           MACRO
667  m                                        JSR     PCI_LOCKDOWN_ENTRY
668  m                                        ENDM
669    
670                                 PCI_LOCKUP MACRO
671  m                                        BSET    #DCTR_HCIE,X:DCTR                 ; Enable host interrupts
672  m                                        ENDM
673    
674    
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 14



675                                 PCI_LOCKDOWN_AGAIN
676       P:000183 P:000185 0A8520            BSET    #DCTR_HCIE,X:DCTR                 ; Re-enable host IRQ
677       P:000184 P:000186 0632A0            REP     #50                               ; Delay for ~us
678       P:000185 P:000187 000000            NOP
679    
680                                 PCI_LOCKDOWN_ENTRY
681       
682       P:000186 P:000188 0A8500            BCLR    #DCTR_HCIE,X:DCTR                 ; Disable host IRQ
683       P:000187 P:000189 0A85A3            JSET    #DCTR_HF3,X:DCTR,PCI_LOCKDOWN_AGAIN
                            000183
684       P:000189 P:00018B 00000C            RTS
685    
686    
688    
689                                 HANDLE_FIFO
690       P:00018A P:00018C 54F400            MOVE              #>$A00,A1
                            000A00
691       P:00018C P:00018E 0BF080            JSR     TIMER_STORE_A1
                            00064F
692       P:00018E P:000190 0BF080            JSR     TIMER_STORE
                            00064D
693    
694       
695       P:000190 P:000192 60F400            MOVE              #>HEAD_W1_0,R0
                            00001B
696       P:000192 P:000194 44F400            MOVE              #>$00FFFF,X0            ; Mask lower 16 bits
                            00FFFF
697       P:000194 P:000196 220800            MOVE              R0,A0
698       P:000195 P:000197 060880            DO      #8,HANDLE_FIFO_CHECK_PREAMBLE
                            0001A0
699                                 HANDLE_FIFO_WAIT
700       P:000197 P:000199 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000197
701       P:000199 P:00019B 000000            NOP
702       P:00019A P:00019C 000000            NOP
703       P:00019B P:00019D 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000197
704       P:00019D P:00019F 094E3F            MOVEP             Y:RDFIFO,A
705       P:00019E P:0001A0 200046            AND     X0,A
706       P:00019F P:0001A1 000000            NOP
707       P:0001A0 P:0001A2 545800            MOVE              A1,X:(R0)+
708    
709                                 HANDLE_FIFO_CHECK_PREAMBLE
710       P:0001A1 P:0001A3 60F400            MOVE              #>HEAD_W1_0,R0
                            00001B
711       P:0001A3 P:0001A5 20001B            CLR     B
712       P:0001A4 P:0001A6 200013            CLR     A
713       P:0001A5 P:0001A7 57D800            MOVE              X:(R0)+,B
714       P:0001A6 P:0001A8 0140CD            CMP     #>$A5A5,B
                            00A5A5
715       P:0001A8 P:0001AA 0AF0A2            JNE     PRE_ERROR
                            0001CF
716       P:0001AA P:0001AC 57D800            MOVE              X:(R0)+,B
717       P:0001AB P:0001AD 0140CD            CMP     #>$A5A5,B
                            00A5A5
718       P:0001AD P:0001AF 0AF0A2            JNE     PRE_ERROR
                            0001CF
719       P:0001AF P:0001B1 57D800            MOVE              X:(R0)+,B
720       P:0001B0 P:0001B2 0140CD            CMP     #>$5A5A,B
                            005A5A
721       P:0001B2 P:0001B4 0AF0A2            JNE     PRE_ERROR
                            0001CF
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 15



722       P:0001B4 P:0001B6 57D800            MOVE              X:(R0)+,B
723       P:0001B5 P:0001B7 0140CD            CMP     #>$5A5A,B
                            005A5A
724       P:0001B7 P:0001B9 0AF0A2            JNE     PRE_ERROR
                            0001CF
725    
726       
727       P:0001B9 P:0001BB 50F000            MOVE              X:>(HEAD_W1_0+6),A0
                            000021
728       P:0001BB P:0001BD 44F000            MOVE              X:>(HEAD_W1_0+7),X0
                            000022
729       P:0001BD P:0001BF 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
730    
731       
732       P:0001BF P:0001C1 0BF080            JSR     PACKET_PARTITIONS
                            0005BE
733       P:0001C1 P:0001C3 0BF080            JSR     TIMER_STORE
                            00064D
734    
736       P:0001C3 P:0001C5 56F000            MOVE              X:HEAD_W3_0,A
                            00001F
737    
738       P:0001C5 P:0001C7 0140C5            CMP     #>'RP',A
                            005250
739       P:0001C7 P:0001C9 0AF0AA            JEQ     HANDLE_RP
                            0001E3
740    
741       P:0001C9 P:0001CB 0140C5            CMP     #>'DA',A
                            004441
742       P:0001CB P:0001CD 0AF0AA            JEQ     HANDLE_DA
                            00022A
743    
744       P:0001CD P:0001CF 0AF080            JMP     QT_PTYPE_ERROR
                            0001D5
745    
746                                 ; Error recording.
747    
748                                 PRE_ERROR
749       P:0001CF P:0001D1 60F400            MOVE              #>PREAMBLE_ERRORS,R0
                            000026
750       P:0001D1 P:0001D3 0BF080            JSR     INCR_X_R0
                            0001DE
751       P:0001D3 P:0001D5 0AF080            JMP     CLEAR_FO_FIFO                     ; empty the fifo (2 ms!)
                            0004B4
752    
753                                 QT_PTYPE_ERROR
754       P:0001D5 P:0001D7 60F400            MOVE              #>PTYPE_ERRORS,R0
                            000027
755       P:0001D7 P:0001D9 0AF080            JMP     INCR_X_R0
                            0001DE
756                                 QT_FSIZE_ERROR
757       P:0001D9 P:0001DB 60F400            MOVE              #>PSIZE_ERRORS,R0
                            000028
758       P:0001DB P:0001DD 0AF080            JMP     INCR_X_R0
                            0001DE
759                                 RETURN_NOW
760       P:0001DD P:0001DF 00000C            RTS
761    
762                                 INCR_X_R0
763       
764       P:0001DE P:0001E0 50E000            MOVE              X:(R0),A0
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 16



765       P:0001DF P:0001E1 000008            INC     A
766       P:0001E0 P:0001E2 000000            NOP
767       P:0001E1 P:0001E3 506000            MOVE              A0,X:(R0)
768       P:0001E2 P:0001E4 00000C            RTS
769    
770    
771    
774    
775                                 HANDLE_RP
776       
777       P:0001E3 P:0001E5 0A0183            JCLR    #MODE_RP_BUFFER,X:MODE,MCE_PACKET
                            0002A8
778    
779       
780       P:0001E5 P:0001E7 0A00B5            JSET    #RP_BUFFER_FULL,X:STATUS,HANDLE_RP_DROP
                            000225
781    
782       
783       P:0001E7 P:0001E9 61F400            MOVE              #>REPLY_BUFFER,R1
                            100000
784       P:0001E9 P:0001EB 0BF080            JSR     BUFFER_PACKET
                            0005CB
785    
786       P:0001EB P:0001ED 54F400            MOVE              #>$b00,A1
                            000B00
787       P:0001ED P:0001EF 0BF080            JSR     TIMER_STORE_A1
                            00064F
788       P:0001EF P:0001F1 0BF080            JSR     TIMER_STORE
                            00064D
789    
790       
791       P:0001F1 P:0001F3 60F400            MOVE              #RP_BASE_LO,R0
                            000048
792       P:0001F3 P:0001F5 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
793    
794       P:0001F5 P:0001F7 60F400            MOVE              #BURST_DEST_LO,R0
                            00002F
795       P:0001F7 P:0001F9 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
796    
797       
798       P:0001F9 P:0001FB 200013            CLR     A
799       P:0001FA P:0001FC 20001B            CLR     B
800       P:0001FB P:0001FD 50F000            MOVE              X:PACKET_SIZE,A0
                            000023
801       P:0001FD P:0001FF 0C1D04            ASL     #2,A,A                            ; Size in bytes
802       P:0001FE P:000200 51F000            MOVE              X:RP_MAX_SIZE,B0
                            00004A
803    
804       P:000200 P:000202 200005            CMP     B,A                               ; A ? B
805       P:000201 P:000203 0AF0AF            JLE     HANDLE_RP1
                            000204
806       P:000203 P:000205 21EE00            MOVE              B,A
807    
808                                 HANDLE_RP1
809       
810       P:000204 P:000206 44F400            MOVE              #>REPLY_BUFFER,X0
                            100000
811       P:000206 P:000208 507000            MOVE              A0,X:BLOCK_SIZE
                            00002B
812       P:000208 P:00020A 447000            MOVE              X0,X:YMEM_SRC
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 17



                            00002E
813       P:00020A P:00020C 0BF080            JSR     TIMER_STORE
                            00064D
814       P:00020C P:00020E 0BF080            JSR     BLOCK_TRANSFER
                            000518
815       P:00020E P:000210 0BF080            JSR     TIMER_STORE
                            00064D
816    
817       
818                                           PCI_LOCKDOWN                              ; Disable host IRQ
820       P:000211 P:000213 44F400            MOVE              #'NFY',X0
                            4E4659
821       P:000213 P:000215 447000            MOVE              X0,X:DTXS_WD1
                            00000B
822       P:000215 P:000217 44F400            MOVE              #'RPQ',X0
                            525051
823       P:000217 P:000219 447000            MOVE              X0,X:DTXS_WD2
                            00000C
824       P:000219 P:00021B 507000            MOVE              A0,X:DTXS_WD3           ; A0=block_size
                            00000D
825       P:00021B P:00021D 547000            MOVE              A1,X:DTXS_WD4           ; A1=0
                            00000E
826    
827       
828       P:00021D P:00021F 0A7035            BSET    #RP_BUFFER_FULL,X:STATUS
                            000000
829       P:00021F P:000221 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
830                                           PCI_LOCKUP                                ; Enable host IRQ
832    
833       P:000222 P:000224 0BF080            JSR     TIMER_STORE
                            00064D
834       P:000224 P:000226 00000C            RTS                                       ; Back to main loop
835    
836                                 HANDLE_RP_DROP
837       P:000225 P:000227 60F400            MOVE              #RP_DROPS,R0
                            00004B
838       P:000227 P:000229 0D01DE            JSR     INCR_X_R0
839       P:000228 P:00022A 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            00060E
840    
842    
843    
846    
847    
848                                 HANDLE_DA
849       
850       P:00022A P:00022C 60F400            MOVE              #FRAME_COUNT,R0
                            000002
851       P:00022C P:00022E 0D01DE            JSR     INCR_X_R0
852    
853       
854       P:00022D P:00022F 0A0182            JCLR    #MODE_QT,X:MODE,MCE_PACKET
                            0002A8
855    
856       
857       P:00022F P:000231 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
858       P:000231 P:000233 0BF080            JSR     BUFFER_PACKET
                            0005CB
859    
860       P:000233 P:000235 54F400            MOVE              #$e00,A1
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 18



                            000E00
861       P:000235 P:000237 0BF080            JSR     TIMER_STORE_A1
                            00064F
862       P:000237 P:000239 0BF080            JSR     TIMER_STORE
                            00064D
863    
864       
865       P:000239 P:00023B 56F000            MOVE              X:QT_BUF_HEAD,A
                            000042
866       P:00023B P:00023D 014180            ADD     #1,A
867       P:00023C P:00023E 57F000            MOVE              X:QT_BUF_MAX,B
                            00003F
868       P:00023E P:000240 20000D            CMP     A,B
869       P:00023F P:000241 0AF0A1            JGE     HANDLE_DA_MATH
                            000242
870       P:000241 P:000243 2E0000            MOVE              #0,A
871                                 HANDLE_DA_MATH
872       P:000242 P:000244 57F000            MOVE              X:QT_BUF_TAIL,B
                            000043
873       P:000244 P:000246 20000D            CMP     A,B
874       P:000245 P:000247 0AF0AA            JEQ     HANDLE_DA_DROP
                            000266
875    
876       
877       P:000247 P:000249 200013            CLR     A
878       P:000248 P:00024A 50F000            MOVE              X:PACKET_SIZE,A0
                            000023
879    
880       P:00024A P:00024C 014088            ADD     #0,B                              ; Clear carry
881       P:00024B P:00024D 0C1D04            ASL     #2,A,A                            ; Size, in bytes
882    
883       
884       P:00024C P:00024E 20001B            CLR     B
885       P:00024D P:00024F 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            000040
886       P:00024F P:000251 20000D            CMP     A,B
887       P:000250 P:000252 0E21D9            JNE     QT_FSIZE_ERROR
888    
889       
890       P:000251 P:000253 517000            MOVE              B0,X:BLOCK_SIZE
                            00002B
891       P:000253 P:000255 557000            MOVE              B1,X:YMEM_SRC           ; Y:0
                            00002E
892    
893       P:000255 P:000257 60F400            MOVE              #QT_DEST_LO,R0
                            000044
894       P:000257 P:000259 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
895       P:000259 P:00025B 60F400            MOVE              #BURST_DEST_LO,R0
                            00002F
896       P:00025B P:00025D 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
897    
898       
899       P:00025D P:00025F 0BF080            JSR     BLOCK_TRANSFER
                            000518
900    
901       P:00025F P:000261 0BF080            JSR     TIMER_STORE
                            00064D
902    
903       
904       P:000261 P:000263 0BF080            JSR     BUFFER_INCR
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 19



                            000657
905    
906       
907       P:000263 P:000265 0BF080            JSR     BUFFER_INFORM_CHECK
                            000675
908    
909       P:000265 P:000267 00000C            RTS
910    
911                                 HANDLE_DA_DROP
912       
913       P:000266 P:000268 60F400            MOVE              #QT_DROPS,R0
                            000047
914       P:000268 P:00026A 0D01DE            JSR     INCR_X_R0
915       P:000269 P:00026B 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            00060E
916    
918    
919    
920                                 ;----------------------------------------------
921                                 CON_BUFFER
922                                 ; This routine will copy an MCE command from the PC to Y memory.
923                                 ; The source RAM address has already been stored in CON_SRC_LO.
924                                 ; The destination address is always Y:COMMAND_BUFFER.
925                                 ;----------------------------------------------
926    
927       P:00026B P:00026D 54F400            MOVE              #>$C00,A1
                            000C00
928       P:00026D P:00026F 0BF080            JSR     TIMER_STORE_A1
                            00064F
929       P:00026F P:000271 0BF080            JSR     TIMER_STORE
                            00064D
930    
931       
932       P:000271 P:000273 60F400            MOVE              #>CON_SRC_LO,R0
                            00002C
933       P:000273 P:000275 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
934       P:000275 P:000277 60F400            MOVE              #>BURST_SRC_LO,R0
                            000031
935       P:000277 P:000279 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006A3
936       P:000279 P:00027B 51F400            MOVE              #>COMMAND_BUFFER,B0
                            200000
937       P:00027B P:00027D 50F400            MOVE              #>256,A0
                            000100
938       P:00027D P:00027F 517000            MOVE              B0,X:YMEM_DEST
                            000033
939       P:00027F P:000281 507000            MOVE              A0,X:BLOCK_SIZE
                            00002B
940       P:000281 P:000283 0BF080            JSR     CON_TRANSFER
                            000552
941    
942       P:000283 P:000285 0A702B            BSET    #CON_MCE,X:STATUS
                            000000
943       P:000285 P:000287 0BF080            JSR     TIMER_STORE
                            00064D
944       P:000287 P:000289 00000C            RTS                                       ; Back to main loop
945    
946                                 ;----------------------------------------------
947                                 CON_TRANSMIT
948                                 ; This routine will copy the MCE command from Y:COMMAND_BUFFER to
949                                 ; the MCE command transmitter.
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 20



950                                 ;----------------------------------------------
951    
952       P:000288 P:00028A 0BF080            JSR     TIMER_STORE
                            00064D
953    
954       P:00028A P:00028C 66F400            MOVE              #>COMMAND_BUFFER,R6
                            200000
955       P:00028C P:00028E 068080            DO      #128,CON_TRANSMIT1                ; block size = 16bit x 128 (256 bytes)
                            000295
956       P:00028E P:000290 5CDE00            MOVE                          Y:(R6)+,A1  ; b2, b1  (lsb)
957       P:00028F P:000291 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
958       P:000290 P:000292 0140C6            AND     #>$FF,A
                            0000FF
959       P:000292 P:000294 547000            MOVE              A1,X:FO_SEND
                            FFF000
960       P:000294 P:000296 557000            MOVE              B1,X:FO_SEND
                            FFF000
961    
962                                 CON_TRANSMIT1
963       P:000296 P:000298 0A0121            BSET    #MODE_MCE,X:<MODE                 ; enable processing of MCE replies/data
964    
965       
966       P:000297 P:000299 0A700B            BCLR    #CON_MCE,X:STATUS
                            000000
967       P:000299 P:00029B 0A700A            BCLR    #CON_DEMAND,X:STATUS
                            000000
968    
969       P:00029B P:00029D 0BF080            JSR     TIMER_STORE
                            00064D
970    
971       
972                                           PCI_LOCKDOWN
974       P:00029E P:0002A0 44F400            MOVE              #'CON',X0
                            434F4E
975       P:0002A0 P:0002A2 0BF080            JSR     VCOM_PREPARE_REPLY
                            0002E9
976       P:0002A2 P:0002A4 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
977                                           PCI_LOCKUP                                ; Enable host IRQ
979    
980       P:0002A5 P:0002A7 0BF080            JSR     TIMER_STORE
                            00064D
981       P:0002A7 P:0002A9 00000C            RTS                                       ; Back to main loop
982    
983    
984    
985    
987    
988                                 ; --------------------------------------------------------------------------
989                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
990                                 ; --------------------------------------------------------------------------
991    
992                                 ; prepare notify to inform host that a packet has arrived.
993    
994                                 MCE_PACKET
995                                           PCI_LOCKDOWN                              ; Disable host IRQ
997       P:0002A9 P:0002AB 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
998    
999       P:0002AA P:0002AC 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 21



1000      P:0002AC P:0002AE 440B00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
1001   
1002      P:0002AD P:0002AF 449F00            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
1003      P:0002AE P:0002B0 440C00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
1004   
1005      P:0002AF P:0002B1 44A100            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
1006      P:0002B0 P:0002B2 440D00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
1007   
1008      P:0002B1 P:0002B3 44A200            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
1009      P:0002B2 P:0002B4 440E00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sage
1010   
1011      
1012      P:0002B3 P:0002B5 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
1013      P:0002B4 P:0002B6 0D047A            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
1014      P:0002B5 P:0002B7 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
1015                                          PCI_LOCKUP
1017   
1018      P:0002B7 P:0002B9 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
1019      P:0002B9 P:0002BB 0BF080            JSR     BUFFER_PACKET
                            0005CB
1020   
1021      
1022   
1023      P:0002BB P:0002BD 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; on fatal error, re-init.
                            000100
1024      P:0002BD P:0002BF 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; Set in 'send_packet_to_host' ISR
                            0002BB
1025   
1026      
1027      P:0002BF P:0002C1 44F400            MOVE              #>IMAGE_BUFFER,X0
                            000000
1028      P:0002C1 P:0002C3 56F000            MOVE              X:PACKET_SIZE,A
                            000023
1029      P:0002C3 P:0002C5 0C1D04            ASL     #2,A,A
1030      P:0002C4 P:0002C6 447000            MOVE              X0,X:YMEM_SRC
                            00002E
1031      P:0002C6 P:0002C8 547000            MOVE              A1,X:BLOCK_SIZE
                            00002B
1032      P:0002C8 P:0002CA 0BF080            JSR     BLOCK_TRANSFER
                            000518
1033   
1034      P:0002CA P:0002CC 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
1035   
1036      
1037                                          PCI_LOCKDOWN                              ; Disable host IRQ
1039      P:0002CD P:0002CF 44F400            MOVE              #'HST',X0
                            485354
1040      P:0002CF P:0002D1 0BF080            JSR     VCOM_PREPARE_REPLY
                            0002E9
1041      P:0002D1 P:0002D3 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1042                                          PCI_LOCKUP                                ; Enable host IRQ
1044      P:0002D4 P:0002D6 00000C            RTS
1045   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 22



1046                                ;----------------------------------------------------------
1047                                ; clear out the fifo after an HST timeout...
1048                                ;----------------------------------------------------------
1049   
1050                                DUMP_FIFO
1051      P:0002D5 P:0002D7 61F400            MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
1052      P:0002D7 P:0002D9 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
1053      P:0002D9 P:0002DB 200013            CLR     A
1054      P:0002DA P:0002DC 320000            MOVE              #0,R2                   ; use R2 as a dump count
1055                                NEXT_DUMP
1056      P:0002DB P:0002DD 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E6
1057      P:0002DD P:0002DF 000000            NOP
1058      P:0002DE P:0002E0 000000            NOP
1059      P:0002DF P:0002E1 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            0002E6
1060   
1061      P:0002E1 P:0002E3 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
1062      P:0002E2 P:0002E4 205A00            MOVE              (R2)+                   ; inc dump count
1063      P:0002E3 P:0002E5 224E00            MOVE              R2,A                    ;
1064      P:0002E4 P:0002E6 200045            CMP     X0,A                              ; check we've not hit dump limit
1065      P:0002E5 P:0002E7 0E22DB            JNE     NEXT_DUMP                         ; not hit limit?
1066                                FIFO_EMPTY
1067      P:0002E6 P:0002E8 627000            MOVE              R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000006
1068      P:0002E8 P:0002EA 0C0100            JMP     <START                            ; re-initialise
1069   
1070   
1071                                ; -------------------------------------------------------------------------------------
1072                                ;                              END OF MAIN PACKET HANDLING CODE
1073                                ; -------------------------------------------------------------------------------------
1074   
1075   
1076   
1077                                ; -------------------------------------------------------------------------------------
1078                                ;
1079                                ;                              INTERRUPT SERVICE ROUTINES
1080                                ;
1081                                ; -------------------------------------------------------------------------------------
1082   
1083                                ; ---------------
1084                                ; Rules:  Don't use N#, or any R# except R0 unless you add them to the saved register set.
1085   
1086   
1087                                ; ----------------------------------------------------------------------------
1088                                ; VCOM_* - routines: utility functions for hosty command vector communication.
1089                                ;-----------------------------------------------------------------------------
1090   
1091   
1092                                ; VCOM_PREPARE_REPLY
1093                                ;
1094                                ; Prepare the reply packet, using X0 as the command name (second word).  The
1095                                ; message defaults to 'ACK' with NULL data.  The user may subsequenty fill in
1096                                ; the data field (word 4) and mark the packet as error if necessary.
1097   
1098                                VCOM_PREPARE_REPLY
1099      
1100      
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 23



1101      P:0002E9 P:0002EB 50F400            MOVE              #'REP',A0
                            524550
1102      P:0002EB P:0002ED 447000            MOVE              X0,X:DTXS_WD2           ; Command
                            00000C
1103      P:0002ED P:0002EF 507000            MOVE              A0,X:DTXS_WD1
                            00000B
1104   
1105      P:0002EF P:0002F1 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
                            41434B
1106      P:0002F1 P:0002F3 000000            NOP
1107      P:0002F2 P:0002F4 547000            MOVE              A1,X:DTXS_WD3           ; ACK
                            00000D
1108      P:0002F4 P:0002F6 507000            MOVE              A0,X:DTXS_WD4           ; no comment
                            00000E
1109      P:0002F6 P:0002F8 00000C            RTS
1110   
1111   
1112                                ; VCOM_CHECK
1113                                ;
1114                                ; Compares DRXR_WD1 to X0.  If they are equal, Z is set on return.  If they
1115                                ; are not equal then Z is cleared and the reply will be marked as ERR with
1116                                ; 'CNE' in the last word.
1117                                ; Trashes A and B always and X0 on error.
1118   
1119                                VCOM_CHECK
1120      P:0002F7 P:0002F9 208E00            MOVE              X0,A
1121      P:0002F8 P:0002FA 57F000            MOVE              X:DRXR_WD1,B
                            000007
1122      P:0002FA P:0002FC 20000D            CMP     A,B
1123      P:0002FB P:0002FD 0AF0AA            JEQ     VCOM_RTS
                            000305
1124   
1125      P:0002FD P:0002FF 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
1126      P:0002FF P:000301 50F400            MOVE              #'ERR',A0
                            455252
1127      P:000301 P:000303 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1128      P:000303 P:000305 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1129                                VCOM_RTS
1130      P:000305 P:000307 00000C            RTS
1131   
1132   
1133                                ; VCOM_INTRO
1134                                ;
1135                                ; Read DSP command from DRXR.  Prepare the reply packet and verify that it
1136                                ; matches the key in X1.  If it does not, mark the reply as error and set
1137                                ; the Z flag.
1138   
1139                                VCOM_INTRO
1140      P:000306 P:000308 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            000318
1141      P:000308 P:00030A 20A400            MOVE              X1,X0
1142      P:000309 P:00030B 0D02E9            JSR     VCOM_PREPARE_REPLY
1143      P:00030A P:00030C 0D02F7            JSR     VCOM_CHECK
1144      P:00030B P:00030D 00000C            RTS
1145   
1146   
1147                                ; VCOM_EXIT_ERROR_X0
1148                                ; VCOM_EXIT_X0
1149                                ; VCOM_EXIT
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 24



1150                                ;
1151                                ; For returning from host command vector interrupts only.  These three
1152                                ; routines do the following (respectively):
1153                                ; a) Mark reply as error, then (b)
1154                                ; b) Put X0 into last word of reply, then (c)
1155                                ; c) Restore registers and RTI.
1156   
1157                                VCOM_EXIT_ERROR_X0
1158      P:00030C P:00030E 50F400            MOVE              #'ERR',A0
                            455252
1159      P:00030E P:000310 000000            NOP
1160      P:00030F P:000311 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1161                                VCOM_EXIT_X0
1162      P:000311 P:000313 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1163                                VCOM_EXIT
1164      P:000313 P:000315 0BF080            JSR     RESTORE_REGISTERS
                            000493
1165      P:000315 P:000317 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1166      P:000317 P:000319 000004            RTI
1167   
1168   
1169                                ;---------------------------------------------------------------
1170                                RD_DRXR
1171                                ;--------------------------------------------------------------
1172                                ; Routine to read from HTXR-DRXR data path.  For HCTR = 0x900,
1173                                ; 3 LSB of each 32-bit word written by the host is returned on
1174                                ; each read.  This only polls for first word, not all of them.
1175      P:000318 P:00031A 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            000318
1176      P:00031A P:00031C 63F400            MOVE              #DRXR_WD1,R3
                            000007
1177      P:00031C P:00031E 0604A0            REP     #4
1178      P:00031D P:00031F 085B8B            MOVEP             X:DRXR,X:(R3)+
1179      P:00031E P:000320 00000C            RTS
1180   
1181   
1182                                ; ----------------------------------------------------------------------------
1183                                READ_MEMORY
1184                                ;-----------------------------------------------------------------------------
1185                                ;Read command:
1186                                ; word 1 = command = 'RDM'
1187                                ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
1188                                ; word 3 = address in memory
1189                                ; word 4 = not used
1190                                ;Version query:
1191                                ; word 1 = 'VER'
1192                                ; word 2-4 unused
1193   
1194      P:00031F P:000321 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1195      P:000321 P:000323 0D0318            JSR     RD_DRXR                           ; Loads DRXR_WD*
1196   
1197      P:000322 P:000324 44F400            MOVE              #'RDM',X0
                            52444D
1198      P:000324 P:000326 0D02E9            JSR     VCOM_PREPARE_REPLY
1199      P:000325 P:000327 0D02F7            JSR     VCOM_CHECK
1200      P:000326 P:000328 0AF0AA            JEQ     READ_MEMORY_XYP
                            000330
1201   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 25



1202      
1203      P:000328 P:00032A 44F400            MOVE              #'VER',X0
                            564552
1204      P:00032A P:00032C 0D02E9            JSR     VCOM_PREPARE_REPLY
1205      P:00032B P:00032D 0D02F7            JSR     VCOM_CHECK
1206      P:00032C P:00032E 0E2313            JNE     VCOM_EXIT
1207   
1208      P:00032D P:00032F 44F000            MOVE              X:REV_NUMBER,X0
                            000003
1209      P:00032F P:000331 0C0311            JMP     VCOM_EXIT_X0
1210   
1211                                READ_MEMORY_XYP
1212   
1213      
1214      P:000330 P:000332 56F000            MOVE              X:DRXR_WD2,A
                            000008
1215      P:000332 P:000334 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1216   
1217      P:000334 P:000336 0140C5            CMP     #'_X',A
                            005F58
1218      P:000336 P:000338 0AF0AA            JEQ     READ_MEMORY_X
                            000343
1219   
1220      P:000338 P:00033A 0140C5            CMP     #'_Y',A
                            005F59
1221      P:00033A P:00033C 0AF0AA            JEQ     READ_MEMORY_Y
                            000345
1222   
1223      P:00033C P:00033E 0140C5            CMP     #'_P',A
                            005F50
1224      P:00033E P:000340 0AF0AA            JEQ     READ_MEMORY_P
                            000347
1225   
1226      P:000340 P:000342 44F400            MOVE              #'MTE',X0
                            4D5445
1227      P:000342 P:000344 0C030C            JMP     VCOM_EXIT_ERROR_X0
1228   
1229                                READ_MEMORY_X
1230      P:000343 P:000345 44E000            MOVE              X:(R0),X0
1231      P:000344 P:000346 0C0311            JMP     VCOM_EXIT_X0
1232                                READ_MEMORY_Y
1233      P:000345 P:000347 4CE000            MOVE                          Y:(R0),X0
1234      P:000346 P:000348 0C0311            JMP     VCOM_EXIT_X0
1235                                READ_MEMORY_P
1236      P:000347 P:000349 07E084            MOVE              P:(R0),X0
1237      P:000348 P:00034A 0C0311            JMP     VCOM_EXIT_X0
1238   
1239   
1240                                ;--------------------------------------------------------------
1241                                WRITE_MEMORY
1242                                ;---------------------------------------------------------------
1243                                ; word 1 = command = 'WRM'
1244                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1245                                ; word 3 = address in memory
1246                                ; word 4 = value
1247   
1248      P:000349 P:00034B 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1249      P:00034B P:00034D 45F400            MOVE              #'WRM',X1
                            57524D
1250      P:00034D P:00034F 0D0306            JSR     VCOM_INTRO
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 26



1251      P:00034E P:000350 0E2313            JNE     VCOM_EXIT
1252   
1253      
1254      P:00034F P:000351 56F000            MOVE              X:DRXR_WD2,A
                            000008
1255      P:000351 P:000353 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1256      P:000353 P:000355 44F000            MOVE              X:DRXR_WD4,X0
                            00000A
1257   
1258      P:000355 P:000357 0140C5            CMP     #'_X',A
                            005F58
1259      P:000357 P:000359 0AF0AA            JEQ     WRITE_MEMORY_X
                            000364
1260   
1261      P:000359 P:00035B 0140C5            CMP     #'_Y',A
                            005F59
1262      P:00035B P:00035D 0AF0AA            JEQ     WRITE_MEMORY_Y
                            000366
1263   
1264      P:00035D P:00035F 0140C5            CMP     #'_P',A
                            005F50
1265      P:00035F P:000361 0AF0AA            JEQ     WRITE_MEMORY_P
                            000368
1266   
1267      P:000361 P:000363 44F400            MOVE              #'MTE',X0
                            4D5445
1268      P:000363 P:000365 0C030C            JMP     VCOM_EXIT_ERROR_X0
1269   
1270                                WRITE_MEMORY_X
1271      P:000364 P:000366 446000            MOVE              X0,X:(R0)
1272      P:000365 P:000367 0C0311            JMP     VCOM_EXIT_X0
1273                                WRITE_MEMORY_Y
1274      P:000366 P:000368 4C6000            MOVE                          X0,Y:(R0)
1275      P:000367 P:000369 0C0311            JMP     VCOM_EXIT_X0
1276                                WRITE_MEMORY_P
1277      P:000368 P:00036A 076084            MOVE              X0,P:(R0)
1278      P:000369 P:00036B 0C0311            JMP     VCOM_EXIT_X0
1279   
1280   
1281                                ;-----------------------------------------------------------------------------
1282                                START_APPLICATION
1283                                ; an application should already have been downloaded to the PCI memory.
1284                                ; this command will execute it.
1285                                ; ----------------------------------------------------------------------
1286                                ; word 1 = command = 'GOA'
1287                                ; word 2-4 unused
1288   
1289      P:00036A P:00036C 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1290      P:00036C P:00036E 45F400            MOVE              #'GOA',X1
                            474F41
1291   
1292      P:00036E P:000370 0D0306            JSR     VCOM_INTRO
1293      P:00036F P:000371 0E2313            JNE     VCOM_EXIT
1294   
1295      P:000370 P:000372 0A7020            BSET    #MODE_APPLICATION,X:MODE
                            000001
1296      P:000372 P:000374 000004            RTI                                       ; Application will reply.
1297   
1298   
1299                                ; ---------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 27



1300                                STOP_APPLICATION
1301                                ; this command stops an application that is currently running
1302                                ; used for applications that once started run contiunually
1303                                ;-----------------------------------------------------------
1304                                ; word 1 = command = ' STP'
1305                                ; word 2-4 unused
1306   
1307      P:000373 P:000375 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1308      P:000375 P:000377 45F400            MOVE              #'STP',X1
                            535450
1309   
1310      P:000377 P:000379 0D0306            JSR     VCOM_INTRO
1311      P:000378 P:00037A 0E2313            JNE     VCOM_EXIT
1312   
1313      P:000379 P:00037B 0A7000            BCLR    #MODE_APPLICATION,X:MODE
                            000001
1314      P:00037B P:00037D 0A7000            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1315      P:00037D P:00037F 0C0313            JMP     VCOM_EXIT
1316   
1317   
1318                                ;-----------------------------------------------------------------------------
1319                                RESET_CONTROLLER
1320                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1321                                ;-----------------------------------------------------------------------------
1322                                ; word 1 = command = 'RCO'
1323                                ; word 2-4 unused
1324   
1325      P:00037E P:000380 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1326      P:000380 P:000382 45F400            MOVE              #'RCO',X1
                            52434F
1327      P:000382 P:000384 0D0306            JSR     VCOM_INTRO
1328      P:000383 P:000385 0E2313            JNE     VCOM_EXIT
1329   
1330      P:000384 P:000386 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1331      P:000385 P:000387 000000            NOP
1332      P:000386 P:000388 000000            NOP
1333      P:000387 P:000389 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1334      P:000389 P:00038B 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1335      P:00038B P:00038D 446000            MOVE              X0,X:(R0)
1336      P:00038C P:00038E 0606A0            REP     #6                                ; Wait for transmission to complete
1337      P:00038D P:00038F 000000            NOP
1338      P:00038E P:000390 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1339   
1340                                ; Wait for a bit for MCE to be reset.......
1341      P:00038F P:000391 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1342      P:000391 P:000393 06C400            DO      X0,L_DELAY
                            000397
1343      P:000393 P:000395 06E883            DO      #1000,L_RDFIFO
                            000396
1344      P:000395 P:000397 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1345      P:000396 P:000398 000000            NOP                                       ;   receiver empty
1346                                L_RDFIFO
1347      P:000397 P:000399 000000            NOP
1348                                L_DELAY
1349      P:000398 P:00039A 000000            NOP
1350   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 28



1351      P:000399 P:00039B 44F400            MOVE              #'000',X0
                            303030
1352      P:00039B P:00039D 0C0311            JMP     VCOM_EXIT_X0
1353   
1354                                ;-----------------------------------------------------------------------------
1355                                QUIET_TRANSFER_SET
1356                                ;-----------------------------------------------------------------------------
1357                                ;Quiet transfer mode configuration
1358                                ; word 1 = command = 'QTS'
1359                                ; word 2 = parameter to set
1360                                ; word 3-4 = arguments
1361   
1362      P:00039C P:00039E 0BF080            JSR     SAVE_REGISTERS                    ; standard opening
                            0004A0
1363      P:00039E P:0003A0 45F400            MOVE              #'QTS',X1
                            515453
1364      P:0003A0 P:0003A2 0D0306            JSR     VCOM_INTRO
1365      P:0003A1 P:0003A3 0E2313            JNE     VCOM_EXIT
1366   
1367      P:0003A2 P:0003A4 60F400            MOVE              #BDEBUG0,R0
                            00004D
1368      P:0003A4 P:0003A6 0D01DE            JSR     INCR_X_R0
1369   
1370      P:0003A5 P:0003A7 56F000            MOVE              X:DRXR_WD2,A            ; Parameter id
                            000008
1371      P:0003A7 P:0003A9 44F000            MOVE              X:DRXR_WD3,X0           ; First arg
                            000009
1372      P:0003A9 P:0003AB 45F000            MOVE              X:DRXR_WD4,X1           ; Second arg
                            00000A
1373   
1374      P:0003AB P:0003AD 0140C5            CMP     #'BAS',A
                            424153
1375      P:0003AD P:0003AF 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            00042D
1376   
1377      P:0003AF P:0003B1 0140C5            CMP     #'DEL',A
                            44454C
1378      P:0003B1 P:0003B3 60F400            MOVE              #QT_BUF_SIZE,R0
                            00003E
1379      P:0003B3 P:0003B5 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1380   
1381      P:0003B5 P:0003B7 0140C5            CMP     #'NUM',A
                            4E554D
1382      P:0003B7 P:0003B9 60F400            MOVE              #QT_BUF_MAX,R0
                            00003F
1383      P:0003B9 P:0003BB 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1384   
1385      P:0003BB P:0003BD 0140C5            CMP     #'INF',A
                            494E46
1386      P:0003BD P:0003BF 60F400            MOVE              #QT_INFORM,R0
                            000041
1387      P:0003BF P:0003C1 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1388   
1389      P:0003C1 P:0003C3 0140C5            CMP     #'SIZ',A
                            53495A
1390      P:0003C3 P:0003C5 60F400            MOVE              #QT_FRAME_SIZE,R0
                            000040
1391      P:0003C5 P:0003C7 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 29



1392   
1393      P:0003C7 P:0003C9 0140C5            CMP     #'TAI',A
                            544149
1394      P:0003C9 P:0003CB 60F400            MOVE              #QT_BUF_TAIL,R0
                            000043
1395      P:0003CB P:0003CD 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1396   
1397      P:0003CD P:0003CF 0140C5            CMP     #'HEA',A
                            484541
1398      P:0003CF P:0003D1 60F400            MOVE              #QT_BUF_HEAD,R0
                            000042
1399      P:0003D1 P:0003D3 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1400   
1401      P:0003D3 P:0003D5 0140C5            CMP     #'DRO',A
                            44524F
1402      P:0003D5 P:0003D7 60F400            MOVE              #QT_DROPS,R0
                            000047
1403      P:0003D7 P:0003D9 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1404   
1405      P:0003D9 P:0003DB 0140C5            CMP     #'PER',A
                            504552
1406      P:0003DB P:0003DD 60F400            MOVE              #TCPR0,R0
                            FFFF8D
1407      P:0003DD P:0003DF 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1408   
1409      P:0003DF P:0003E1 0140C5            CMP     #'FLU',A
                            464C55
1410      P:0003E1 P:0003E3 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            00040D
1411   
1412      P:0003E3 P:0003E5 0140C5            CMP     #'SET',A
                            534554
1413      P:0003E5 P:0003E7 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            000415
1414   
1415      P:0003E7 P:0003E9 0140C5            CMP     #'RPS',A
                            525053
1416      P:0003E9 P:0003EB 60F400            MOVE              #RP_MAX_SIZE,R0
                            00004A
1417      P:0003EB P:0003ED 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000423
1418   
1419      P:0003ED P:0003EF 0140C5            CMP     #'RPB',A
                            525042
1420      P:0003EF P:0003F1 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_BASE
                            0003FE
1421   
1422      P:0003F1 P:0003F3 0140C5            CMP     #'RPE',A
                            525045
1423      P:0003F3 P:0003F5 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_ENABLED
                            000403
1424   
1425      P:0003F5 P:0003F7 0140C5            CMP     #'BUR',A
                            425552
1426      P:0003F7 P:0003F9 60F400            MOVE              #PCI_BURST_SIZE,R0
                            000029
1427      P:0003F9 P:0003FB 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0_PERSISTENT
                            000425
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 30



1428   
1429      P:0003FB P:0003FD 44F400            MOVE              #'MTE',X0
                            4D5445
1430      P:0003FD P:0003FF 0C030C            JMP     VCOM_EXIT_ERROR_X0
1431   
1432                                QUIET_TRANSFER_SET_RP_BASE
1433      P:0003FE P:000400 447000            MOVE              X0,X:RP_BASE_LO
                            000048
1434      P:000400 P:000402 457000            MOVE              X1,X:RP_BASE_HI
                            000049
1435      P:000402 P:000404 0C0313            JMP     VCOM_EXIT
1436   
1437                                QUIET_TRANSFER_SET_RP_ENABLED
1438      P:000403 P:000405 0A7003            BCLR    #MODE_RP_BUFFER,X:MODE
                            000001
1439      P:000405 P:000407 208E00            MOVE              X0,A
1440      P:000406 P:000408 200003            TST     A
1441      P:000407 P:000409 0EA313            JEQ     VCOM_EXIT
1442      P:000408 P:00040A 0A7023            BSET    #MODE_RP_BUFFER,X:MODE
                            000001
1443      P:00040A P:00040C 0A7015            BCLR    #RP_BUFFER_FULL,X:STATUS
                            000000
1444      P:00040C P:00040E 0C0313            JMP     VCOM_EXIT
1445   
1446                                QUIET_TRANSFER_SET_FLUSH
1447      P:00040D P:00040F 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
1448      P:00040F P:000411 208E00            MOVE              X0,A
1449      P:000410 P:000412 200003            TST     A
1450      P:000411 P:000413 0EA313            JEQ     VCOM_EXIT
1451      P:000412 P:000414 0A7034            BSET    #QT_FLUSH,X:STATUS
                            000000
1452      P:000414 P:000416 0C0313            JMP     VCOM_EXIT
1453   
1454                                QUIET_TRANSFER_SET_ENABLED
1455      P:000415 P:000417 208E00            MOVE              X0,A
1456      P:000416 P:000418 200003            TST     A
1457      P:000417 P:000419 0AF0AA            JEQ     QUIET_TRANSFER_SET_DISABLED
                            00041E
1458      P:000419 P:00041B 0A7022            BSET    #MODE_QT,X:MODE
                            000001
1459      P:00041B P:00041D 0BF080            JSR     TIMER_ENABLE
                            000622
1460      P:00041D P:00041F 0C0313            JMP     VCOM_EXIT
1461   
1462                                QUIET_TRANSFER_SET_DISABLED
1463      P:00041E P:000420 0A7002            BCLR    #MODE_QT,X:MODE
                            000001
1464      P:000420 P:000422 0BF080            JSR     TIMER_DEFAULT
                            00062E
1465      P:000422 P:000424 0C0313            JMP     VCOM_EXIT
1466   
1467                                QUIET_TRANSFER_SET_R0
1468      P:000423 P:000425 446000            MOVE              X0,X:(R0)
1469      P:000424 P:000426 0C0313            JMP     VCOM_EXIT
1470   
1471                                QUIET_TRANSFER_SET_R0_PERSISTENT
1472      
1473      
1474      
1475      P:000425 P:000427 446000            MOVE              X0,X:(R0)
1476      P:000426 P:000428 57F400            MOVE              #>VAR_TBL_START,B
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 31



                            0006AC
1477      P:000428 P:00042A 220E00            MOVE              R0,A
1478      P:000429 P:00042B 200018            ADD     A,B
**** 1479 [main.asm 873]: WARNING --- Pipeline stall reading register B written in previous instruction (X data move field)
**** 1479 [main.asm 873]: WARNING --- Pipeline stall reading register written in previous instruction (X data move field)
1479      P:00042A P:00042C 21F000            MOVE              B,R0
**** 1480 [main.asm 874]: WARNING --- Pipeline stall reading register written in instruction at address: P:00042A (X data move field
)
1480      P:00042B P:00042D 076084            MOVE              X0,P:(R0)
1481      P:00042C P:00042E 0C0313            JMP     VCOM_EXIT
1482   
1483                                QUIET_TRANSFER_SET_BASE
1484      P:00042D P:00042F 447000            MOVE              X0,X:QT_BASE_LO
                            00003C
1485      P:00042F P:000431 457000            MOVE              X1,X:QT_BASE_HI
                            00003D
1486   
1487      P:000431 P:000433 0BF080            JSR     BUFFER_RESET
                            000669
1488   
1489      P:000433 P:000435 0C0313            JMP     VCOM_EXIT
1490   
1491   
1492                                ;-----------------------------------------------------------------------------
1493                                SYSTEM_RESET
1494                                ;-----------------------------------------------------------------------------
1495   
1496      P:000434 P:000436 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1497      P:000435 P:000437 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1498                                                                                    ; set to zero except for interrupts
1499      P:000437 P:000439 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1500                                                                                    ; so first set to 0
1501      P:000438 P:00043A 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1502                                                                                    ; therefore,return to initialization
1503      P:00043A P:00043C 000000            NOP
1504      P:00043B P:00043D 000004            RTI                                       ; return from ISR - to START
1505   
1506   
1507                                ; ------------------------------------------------------------------------------------
1508                                SEND_PACKET_TO_HOST
1509                                ; this command is received from the Host and actions the PCI board to pick up an address
1510                                ; pointer from DRXR which the PCI board then uses to write packets from the
1511                                ; MCE to the host memory starting at the address given.
1512                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1513                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1514                                ; HST after packet sent (unless error).
1515                                ; --------------------------------------------------------------------------------------
1516                                ; word 1 = command = 'HST'
1517                                ; word 2 = host high address
1518                                ; word 3 = host low address
1519                                ; word 4 = not used but read
1520   
1521      P:00043C P:00043E 0D04A0            JSR     <SAVE_REGISTERS                   ; save working registers
1522      P:00043D P:00043F 45F400            MOVE              #'HST',X1
                            485354
1523      P:00043F P:000441 0D0306            JSR     VCOM_INTRO
1524      P:000440 P:000442 0E2313            JNE     VCOM_EXIT
1525   
1526      
1527      P:000441 P:000443 448800            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 32



1528      P:000442 P:000444 518900            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1529   
1530      P:000443 P:000445 447000            MOVE              X0,X:BURST_DEST_HI
                            000030
1531      P:000445 P:000447 517000            MOVE              B0,X:BURST_DEST_LO
                            00002F
1532   
1533      P:000447 P:000449 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1534   
1535      P:000448 P:00044A 0BF080            JSR     RESTORE_REGISTERS
                            000493
1536      P:00044A P:00044C 000004            RTI                                       ; Main loop will reply after packet transfer
!
1537   
1538   
1539                                ; --------------------------------------------------------------------
1540                                SOFTWARE_RESET
1541                                ;----------------------------------------------------------------------
1542                                ; word 1 = command = 'RST'
1543                                ; word 2-4 unused
1544   
1545      P:00044B P:00044D 0BF080            JSR     SAVE_REGISTERS
                            0004A0
1546      P:00044D P:00044F 45F400            MOVE              #'RST',X1
                            525354
1547      P:00044F P:000451 0D0306            JSR     VCOM_INTRO
1548      P:000450 P:000452 0E2313            JNE     VCOM_EXIT
1549   
1550                                ; RST command OK so reply to host
1551                                FINISH_RST
1552      P:000451 P:000453 44F400            MOVE              #'000',X0
                            303030
1553      P:000453 P:000455 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1554      P:000455 P:000457 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            00047A
1555   
1556      P:000457 P:000459 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            000457
1557   
1558      P:000459 P:00045B 0A0100            BCLR    #MODE_APPLICATION,X:<MODE         ; clear app flag
1559      P:00045A P:00045C 0A0000            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1560   
1561      P:00045B P:00045D 0C0434            JMP     SYSTEM_RESET                      ; Handle the stack and stuff...
1562   
1563   
1564                                SEND_PACKET_TO_CONTROLLER
1565   
1566                                ;       Host command identifying location of an MCE command to send to
1567                                ;       the MCE.  Since this can come at any time, just record the
1568                                ;       request and then do the CONning from the main loop.
1569   
1570                                ; word 1 = command = 'CON'
1571                                ; word 2 = source host bus address, bits 31:16
1572                                ; word 3 = source host bus address, bits 15:0
1573                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1574                                ;        = '1' --> when MCE command is GO
1575   
1576      P:00045C P:00045E 0D04A0            JSR     <SAVE_REGISTERS                   ; save working registers
1577   
1578      
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 33



1579      P:00045D P:00045F 45F400            MOVE              #'CON',X1
                            434F4E
1580      P:00045F P:000461 0D0306            JSR     VCOM_INTRO
1581      P:000460 P:000462 0E2313            JNE     VCOM_EXIT
1582   
1583      
1584      P:000461 P:000463 44F400            MOVE              #'BUS',X0
                            425553
1585      P:000463 P:000465 0A00AA            JSET    #CON_DEMAND,X:STATUS,VCOM_EXIT_ERROR_X0
                            00030C
1586   
1587      
1588      P:000465 P:000467 0A702A            BSET    #CON_DEMAND,X:STATUS
                            000000
1589      P:000467 P:000469 448800            MOVE              X:<DRXR_WD2,X0
1590      P:000468 P:00046A 458900            MOVE              X:<DRXR_WD3,X1
1591      P:000469 P:00046B 447000            MOVE              X0,X:CON_SRC_HI
                            00002D
1592      P:00046B P:00046D 457000            MOVE              X1,X:CON_SRC_LO
                            00002C
1593   
1594                                ;       ;; Fourth word indicates if this is a go.  Who cares?
1595                                ;       MOVE    X:<DRXR_WD4,A           ; read word 4 - GO command?
1596                                ;       MOVE    #0,X0
1597                                ;       CMP     X0,A
1598                                ;       JEQ     BLOCK_CON
1599   
1600      
1601      P:00046D P:00046F 0BF080            JSR     RESTORE_REGISTERS
                            000493
1602      P:00046F P:000471 000004            RTI
1603   
1605   
1606   
1607                                ;---------------------------------------------------------------
1608                                ;
1609                                ;                          * END OF ISRs *
1610                                ;
1611                                ;--------------------------------------------------------------
1612   
1613   
1614   
1615                                ;----------------------------------------------------------------
1616                                ;
1617                                ;                     * Beginning of SUBROUTINES *
1618                                ;
1619                                ;-----------------------------------------------------------------
1620   
1621   
1622                                CHECK_FO
1623      P:000470 P:000472 01AD80            JCLR    #EF,X:PDRD,CHECK_FO_CLEAR
                            000478
1624      P:000472 P:000474 000000            NOP
1625      P:000473 P:000475 000000            NOP
1626      P:000474 P:000476 01AD80            JCLR    #EF,X:PDRD,CHECK_FO_CLEAR
                            000478
1627      P:000476 P:000478 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1628      P:000477 P:000479 00000C            RTS
1629   
1630                                CHECK_FO_CLEAR
1631      P:000478 P:00047A 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1632      P:000479 P:00047B 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 34



1633   
1634   
1635   
1636                                ;----------------------------------------------------------------------------
1637                                PCI_MESSAGE_TO_HOST
1638                                ;----------------------------------------------------------------------------
1639                                ; Subroutine to send 4 words as a reply from PCI to the Host
1640                                ; using the DTXS-HRXS data path.  The DSP signals the host by raising
1641                                ; HF3 and (when !MODE_NOIRQ) INTA.
1642                                ;
1643                                ; When MODE_HANDSHAKE, the DSP and Host interact as follows:
1644                                ; - to show that the Host is handling the interrupt, Host raises HF0
1645                                ; - when DSP sees HF0 go high, it lowers INTA and HF3
1646                                ; - when Host is done handling the interrupt (i.e. it has read the reply),
1647                                ;   and when HF3 is low, Host lowers HF0.
1648                                ; - when DSP sees HF0 go low, the routine finishes.
1649                                ;
1650                                ; The primary advantage of this hand-shaking scheme is that host vector
1651                                ; commands are not needed to clear HF3 and INTA.
1652                                ;
1653                                ; This routine should not block for anything other than the Host handshake.
1654   
1655      P:00047A P:00047C 60F400            MOVE              #>DTXS_WD1,R0
                            00000B
1656   
1657      P:00047C P:00047E 060480            DO      #4,PCI_MESSAGE_TO_HOST_10
                            000480
1658      P:00047E P:000480 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            00047E
1659      P:000480 P:000482 08D88D            MOVEP             X:(R0)+,X:DTXS
1660   
1661                                PCI_MESSAGE_TO_HOST_10
1662      P:000481 P:000483 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            000015
1663      P:000483 P:000485 60F000            MOVE              X:SV_R0,R0              ; restore R0
                            000019
1664      P:000485 P:000487 0A8523            BSET    #DCTR_HF3,X:DCTR                  ; Raise HF3 (handshake)
1665   
1666                                                                                    ; Only interrupt in irq mode
1667      
1668      P:000486 P:000488 000000            NOP
1669      P:000487 P:000489 000000            NOP
1670      P:000488 P:00048A 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1671   
1672                                PCI_MESSAGE_TO_HOST_20
1673      P:000489 P:00048B 0A89A4            JSET    #DSR_HF1,X:DSR,PCI_MESSAGE_TO_HOST_HANDSHAKE
                            00048C
1674      P:00048B P:00048D 00000C            RTS
1675   
1676                                PCI_MESSAGE_TO_HOST_HANDSHAKE
1677      P:00048C P:00048E 0A8983            JCLR    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            00048C
1678      P:00048E P:000490 0A8506            BCLR    #INTA,X:DCTR                      ; Clear interrupt
1679      P:00048F P:000491 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; Clear hand-shake bit
1680      P:000490 P:000492 0A89A3            JSET    #DSR_HF0,X:DSR,*                  ; Wait for host to ack
                            000490
1681      P:000492 P:000494 00000C            RTS
1682   
1683   
1684                                ;------------------------------------------------------------------------------------
1685                                RESTORE_REGISTERS
1686                                ;-------------------------------------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 35



1687   
1688      P:000493 P:000495 059A39            MOVEC             X:<SV_SR,SR
1689   
1690      P:000494 P:000496 508F00            MOVE              X:<SV_A0,A0
1691      P:000495 P:000497 549000            MOVE              X:<SV_A1,A1
1692      P:000496 P:000498 529100            MOVE              X:<SV_A2,A2
1693   
1694      P:000497 P:000499 519200            MOVE              X:<SV_B0,B0
1695      P:000498 P:00049A 559300            MOVE              X:<SV_B1,B1
1696      P:000499 P:00049B 539400            MOVE              X:<SV_B2,B2
1697   
1698      P:00049A P:00049C 449500            MOVE              X:<SV_X0,X0
1699      P:00049B P:00049D 459600            MOVE              X:<SV_X1,X1
1700   
1701      P:00049C P:00049E 469700            MOVE              X:<SV_Y0,Y0
1702      P:00049D P:00049F 479800            MOVE              X:<SV_Y1,Y1
1703   
1704      P:00049E P:0004A0 609900            MOVE              X:<SV_R0,R0
1705      P:00049F P:0004A1 00000C            RTS
1706   
1707                                ;-------------------------------------------------------------------------------------
1708                                SAVE_REGISTERS
1709                                ;-------------------------------------------------------------------------------------
1710   
1711      P:0004A0 P:0004A2 051A39            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1712   
1713      P:0004A1 P:0004A3 500F00            MOVE              A0,X:<SV_A0
1714      P:0004A2 P:0004A4 541000            MOVE              A1,X:<SV_A1
1715      P:0004A3 P:0004A5 521100            MOVE              A2,X:<SV_A2
1716   
1717      P:0004A4 P:0004A6 511200            MOVE              B0,X:<SV_B0
1718      P:0004A5 P:0004A7 551300            MOVE              B1,X:<SV_B1
1719      P:0004A6 P:0004A8 531400            MOVE              B2,X:<SV_B2
1720   
1721      P:0004A7 P:0004A9 441500            MOVE              X0,X:<SV_X0
1722      P:0004A8 P:0004AA 451600            MOVE              X1,X:<SV_X1
1723   
1724      P:0004A9 P:0004AB 461700            MOVE              Y0,X:<SV_Y0
1725      P:0004AA P:0004AC 471800            MOVE              Y1,X:<SV_Y1
1726   
1727      P:0004AB P:0004AD 601900            MOVE              R0,X:<SV_R0
1728      P:0004AC P:0004AE 00000C            RTS
1729   
1730   
1731                                ;----------------------------------------------
1732                                FLUSH_PCI_FIFO
1733                                ;----------------------------------------------
1734      P:0004AD P:0004AF 0A8A84            JCLR    #MARQ,X:DPSR,*
                            0004AD
1735      P:0004AF P:0004B1 0A862E            BSET    #CLRT,X:DPCR
1736      P:0004B0 P:0004B2 000000            NOP
1737      P:0004B1 P:0004B3 0A86AE            JSET    #CLRT,X:DPCR,*
                            0004B1
1738      P:0004B3 P:0004B5 00000C            RTS
1739   
1740                                ;----------------------------------------------
1741                                CLEAR_FO_FIFO
1742                                ;----------------------------------------------
1743      P:0004B4 P:0004B6 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
1744      P:0004B6 P:0004B8 44F400            MOVE              #200000,X0
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 36



                            030D40
1745      P:0004B8 P:0004BA 06C400            DO      X0,*+3
                            0004BA
1746      P:0004BA P:0004BC 000000            NOP
1747      P:0004BB P:0004BD 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
1748      P:0004BD P:0004BF 00000C            RTS
1749   
1750   
1751                                ;---------------------------------------------------------
1752                                ; PCI burst routines
1753                                ;
1754                                ; For transfer between Host memory and DSP Y memory.
1755                                ;
1756                                ; Major entry points are
1757                                ;       CON_TRANSFER (PC -> DSP)
1758                                ;       BLOCK_TRANSFER (DSP -> PC)
1759                                ;---------------------------------------------------------
1760   
1761                                ;---------------------------------------------------------
1762                                PCI_ERROR_CLEAR
1763                                ;-----------------------------------------------
1764      
1765      
1766      
1767      
1768      
1769      
1770   
1771      P:0004BE P:0004C0 50F000            MOVE              X:DMA_ERRORS,A0
                            000034
1772      P:0004C0 P:0004C2 000008            INC     A
1773      P:0004C1 P:0004C3 000000            NOP
1774      P:0004C2 P:0004C4 507000            MOVE              A0,X:DMA_ERRORS
                            000034
1775   
1776      P:0004C4 P:0004C6 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            0004D2
1777      P:0004C6 P:0004C8 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
                            0004DC
1778      P:0004C8 P:0004CA 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            0004E6
1779      P:0004CA P:0004CC 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            0004F0
1780      P:0004CC P:0004CE 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            0004FA
1781      P:0004CE P:0004D0 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            000504
1782      P:0004D0 P:0004D2 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            00050E
1783   
1784                                ERROR_TRTY
1785      P:0004D2 P:0004D4 50F000            MOVE              X:EC_TRTY,A0
                            000035
1786      P:0004D4 P:0004D6 000008            INC     A
1787      P:0004D5 P:0004D7 08F48A            MOVEP             #>$0400,X:DPSR          ; Clear target retry error bit
                            000400
1788      P:0004D7 P:0004D9 507000            MOVE              A0,X:EC_TRTY
                            000035
1789      P:0004D9 P:0004DB 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1790      P:0004DB P:0004DD 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 37



1791                                ERROR_TO
1792      P:0004DC P:0004DE 50F000            MOVE              X:EC_TO,A0
                            000036
1793      P:0004DE P:0004E0 000008            INC     A
1794      P:0004DF P:0004E1 08F48A            MOVEP             #>$0800,X:DPSR          ; Clear timeout error bit
                            000800
1795      P:0004E1 P:0004E3 507000            MOVE              A0,X:EC_TO
                            000036
1796      P:0004E3 P:0004E5 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1797      P:0004E5 P:0004E7 00000C            RTS
1798                                ERROR_TDIS
1799      P:0004E6 P:0004E8 50F000            MOVE              X:EC_TDIS,A0
                            000037
1800      P:0004E8 P:0004EA 000008            INC     A
1801      P:0004E9 P:0004EB 08F48A            MOVEP             #>$0200,X:DPSR          ; Clear target disconnect bit
                            000200
1802      P:0004EB P:0004ED 507000            MOVE              A0,X:EC_TDIS
                            000037
1803      P:0004ED P:0004EF 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1804      P:0004EF P:0004F1 00000C            RTS
1805                                ERROR_TAB
1806      P:0004F0 P:0004F2 50F000            MOVE              X:EC_TAB,A0
                            000038
1807      P:0004F2 P:0004F4 000008            INC     A
1808      P:0004F3 P:0004F5 08F48A            MOVEP             #>$0100,X:DPSR          ; Clear target abort error bit
                            000100
1809      P:0004F5 P:0004F7 507000            MOVE              A0,X:EC_TAB
                            000038
1810      P:0004F7 P:0004F9 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1811      P:0004F9 P:0004FB 00000C            RTS
1812                                ERROR_MAB
1813      P:0004FA P:0004FC 50F000            MOVE              X:EC_MAB,A0
                            000039
1814      P:0004FC P:0004FE 000008            INC     A
1815      P:0004FD P:0004FF 08F48A            MOVEP             #>$0080,X:DPSR          ; Clear master abort error bit
                            000080
1816      P:0004FF P:000501 507000            MOVE              A0,X:EC_MAB
                            000039
1817      P:000501 P:000503 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1818      P:000503 P:000505 00000C            RTS
1819                                ERROR_DPER
1820      P:000504 P:000506 50F000            MOVE              X:EC_DPER,A0
                            00003A
1821      P:000506 P:000508 000008            INC     A
1822      P:000507 P:000509 08F48A            MOVEP             #>$0040,X:DPSR          ; Clear data parity error bit
                            000040
1823      P:000509 P:00050B 507000            MOVE              A0,X:EC_DPER
                            00003A
1824      P:00050B P:00050D 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1825      P:00050D P:00050F 00000C            RTS
1826                                ERROR_APER
1827      P:00050E P:000510 50F000            MOVE              X:EC_APER,A0
                            00003B
1828      P:000510 P:000512 000008            INC     A
1829      P:000511 P:000513 08F48A            MOVEP             #>$0020,X:DPSR          ; Clear address parity error bit
                            000020
1830      P:000513 P:000515 507000            MOVE              A0,X:EC_APER
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 38



                            00003B
1831      P:000515 P:000517 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1832      P:000517 P:000519 00000C            RTS
1833   
1834   
1835   
1836                                ;----------------------------------------------
1837                                BLOCK_TRANSFER
1838                                ;----------------------------------------------
1839                                ;   In:
1840                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
1841                                ;   - BLOCK_SIZE is packet size, in bytes
1842                                ;   - YMEM_SRC is start of data in Y memory
1843                                ;  Out:
1844                                ;   - BLOCK_SIZE will be decremented to zero.
1845                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
1846                                ;   - YMEM_SRC will be incremented by BLOCK_SIZE/2
1847                                ;  Trashes:
1848                                ;   - A and B at least
1849   
1850      P:000518 P:00051A 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002B
1851      P:00051A P:00051C 014085            CMP     #0,A                              ; Still bytes to transfer?
1852      P:00051B P:00051D 0AF0A2            JNE     BLOCK_TRANSFER0
                            00051E
1853      P:00051D P:00051F 00000C            RTS
1854   
1855                                BLOCK_TRANSFER0
1856      
1857      
1858      P:00051E P:000520 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000029
1859   
1860      P:000520 P:000522 200005            CMP     B,A                               ; A ? B
1861      P:000521 P:000523 0E1523            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
1862      P:000522 P:000524 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1863                                BLOCK_TRANSFER1
1864      
1865      P:000523 P:000525 200014            SUB     B,A                               ; A -= B
1866      P:000524 P:000526 014088            ADD     #0,B                              ; Clear carry bit
1867      P:000525 P:000527 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002B
1868      P:000527 P:000529 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002A
1869      P:000529 P:00052B 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1870   
1871      
1872      P:00052A P:00052C 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
1873      P:00052C P:00052E 50F000            MOVE              X:YMEM_SRC,A0
                            00002E
1874      P:00052E P:000530 08C82F            MOVEP             A0,X:DSR0               ; DMA source
1875      P:00052F P:000531 200010            ADD     B,A
1876      P:000530 P:000532 00000B            DEC     B
1877      P:000531 P:000533 507000            MOVE              A0,X:YMEM_SRC           ; BURST_SRC += BURST_SIZE/2
                            00002E
1878   
1879      P:000533 P:000535 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1880   
1881      
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 39



1882      P:000534 P:000536 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
1883   
1884                                BLOCK_TRANSFER_PCI
1885      P:000536 P:000538 44F400            MOVE              #>$7,X0                 ; Memory write
                            000007
1886      P:000538 P:00053A 60F400            MOVE              #BURST_DEST_LO,R0       ; RAM address
                            00002F
1887      P:00053A P:00053C 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            00058D
1888   
1889      
1890      P:00053C P:00053E 0A8A84            JCLR    #MARQ,X:DPSR,*
                            00053C
1891   
1892      
1893      P:00053E P:000540 0A8A8E            JCLR    #MDT,X:DPSR,BLOCK_TRANSFER_HANDLE_ERRORS
                            000546
1894   
1895      P:000540 P:000542 20001B            CLR     B
1896      P:000541 P:000543 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            00002A
1897      P:000543 P:000545 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            0006A1
1898      P:000545 P:000547 0C0518            JMP     BLOCK_TRANSFER                    ; Next burst in block
1899   
1900                                BLOCK_TRANSFER_HANDLE_ERRORS
1901      
1902      P:000546 P:000548 0D04BE            JSR     PCI_ERROR_CLEAR
1903   
1904      P:000547 P:000549 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1905      P:000549 P:00054B 0E8536            JCS     BLOCK_TRANSFER_PCI                ; Restart PCI burst
1906   
1907      P:00054A P:00054C 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
1908      P:00054C P:00054E 0E0518            JCC     BLOCK_TRANSFER                    ; Error but no error? Redo this burst.
1909   
1910      
1911      P:00054D P:00054F 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            0005A3
1912      P:00054F P:000551 0BF080            JSR     PCI_UPDATE_R0
                            0005B3
1913      P:000551 P:000553 0C0536            JMP     BLOCK_TRANSFER_PCI
1914   
1915   
1916                                ;----------------------------------------------
1917                                CON_TRANSFER
1918                                ;----------------------------------------------
1919                                ;   In:
1920                                ;   - BURST_SRC_HI:BURST_SRC_LO is PC RAM address
1921                                ;   - BLOCK_SIZE is packet size, in bytes
1922                                ;   - YMEM_DEST is start of data in Y memory
1923                                ;  Out:
1924                                ;   - BLOCK_SIZE will be decremented to zero.
1925                                ;   - BURST_SRC_HI:LO will be incremented by BLOCK_SIZE
1926                                ;   - YMEM_DEST will be incremented by BLOCK_SIZE/2
1927                                ;  Trashes:
1928                                ;   - A and B, R0, X0
1929   
1930      P:000552 P:000554 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002B
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 40



1931      P:000554 P:000556 014085            CMP     #0,A                              ; Still bytes to transfer?
1932      P:000555 P:000557 0AF0A2            JNE     CON_TRANSFER0
                            000558
1933      P:000557 P:000559 00000C            RTS
1934   
1935                                CON_TRANSFER0
1936      
1937      
1938      P:000558 P:00055A 57F000            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
                            000029
1939   
1940      P:00055A P:00055C 200005            CMP     B,A                               ; A ? B
1941      P:00055B P:00055D 0E155D            JGE     <CON_TRANSFER1                    ; jump if A >= B
1942      P:00055C P:00055E 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1943                                CON_TRANSFER1
1944      
1945      P:00055D P:00055F 200014            SUB     B,A                               ; A -= B
1946      P:00055E P:000560 014088            ADD     #0,B                              ; Clear carry bit
1947      P:00055F P:000561 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002B
1948      P:000561 P:000563 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002A
1949      P:000563 P:000565 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1950   
1951      
1952      P:000564 P:000566 50F000            MOVE              X:YMEM_DEST,A0
                            000033
1953      P:000566 P:000568 507000            MOVE              A0,X:DDR0               ; DMA dest'n
                            FFFFEE
1954      P:000568 P:00056A 08F4AF            MOVEP             #>DRXR,X:DSR0           ; DMA source
                            FFFFCB
1955      P:00056A P:00056C 200010            ADD     B,A
1956      P:00056B P:00056D 00000B            DEC     B
1957      P:00056C P:00056E 507000            MOVE              A0,X:YMEM_DEST          ; YMEM_DEST += BURST_SIZE/2
                            000033
1958   
1959      P:00056E P:000570 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1960   
1961      
1962      P:00056F P:000571 08F4AC            MOVEP             #$8EEAC4,X:DCR0
                            8EEAC4
1963   
1964                                CON_TRANSFER_PCI
1965      P:000571 P:000573 44F400            MOVE              #>$6,X0                 ; Memory write
                            000006
1966      P:000573 P:000575 60F400            MOVE              #BURST_SRC_LO,R0        ; RAM address
                            000031
1967      P:000575 P:000577 0BF080            JSR     PCI_GO                            ; Initiate PCI burst
                            00058D
1968   
1969      
1970      P:000577 P:000579 0A8A84            JCLR    #MARQ,X:DPSR,*
                            000577
1971   
1972      
1973      P:000579 P:00057B 0A8A8E            JCLR    #MDT,X:DPSR,CON_TRANSFER_HANDLE_ERRORS
                            000581
1974   
1975      P:00057B P:00057D 20001B            CLR     B
1976      P:00057C P:00057E 51F000            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
                            00002A
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 41



1977      P:00057E P:000580 0BF080            JSR     ADD_HILO_ADDRESS                  ; Update source address
                            0006A1
1978      P:000580 P:000582 0C0552            JMP     CON_TRANSFER                      ; Next burst in block
1979   
1980                                CON_TRANSFER_HANDLE_ERRORS
1981      
1982      P:000581 P:000583 0D04BE            JSR     PCI_ERROR_CLEAR
1983   
1984      P:000582 P:000584 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1985      P:000584 P:000586 0E8571            JCS     CON_TRANSFER_PCI                  ; Restart PCI burst
1986   
1987      P:000585 P:000587 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
1988      P:000587 P:000589 0E0552            JCC     CON_TRANSFER                      ; Error but no error? Redo this burst.
1989   
1990      
1991      P:000588 P:00058A 0BF080            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
                            0005A3
1992      P:00058A P:00058C 0BF080            JSR     PCI_UPDATE_R0
                            0005B3
1993      P:00058C P:00058E 0C0571            JMP     CON_TRANSFER_PCI
1994   
1995                                ; Utility routines for BLOCK_TRANSFER and CON_TRANSFER
1996   
1997                                PCI_GO
1998                                ; Initiate PCI read/write of BURST_SIZE bytes.
1999                                ; R0 must point to the hi-lo PCI address source/dest address
2000                                ; X0 is the PCI command (6 is read, 7 is write).
2001                                ; Trashes A and B but not R0 and X0.
2002      P:00058D P:00058F 200013            CLR     A
2003      P:00058E P:000590 20001B            CLR     B
2004      P:00058F P:000591 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            00002A
2005      P:000591 P:000593 00000B            DEC     B                                 ; n8 - 1
2006      P:000592 P:000594 014088            ADD     #0,B                              ; Clear carry
2007      P:000593 P:000595 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
2008      P:000594 P:000596 014088            ADD     #0,B                              ; Clear carry
2009      P:000595 P:000597 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
2010   
2011      P:000596 P:000598 0200D8            MOVE              X:(R0+1),A0             ; PCI HI address
2012   
2013      P:000597 P:000599 200010            ADD     B,A
2014      P:000598 P:00059A 000000            NOP
2015      P:000599 P:00059B 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
2016   
2017      P:00059B P:00059D 208800            MOVE              X0,A0
2018      P:00059C P:00059E 014088            ADD     #0,B                              ; Clear carry
2019      P:00059D P:00059F 0C1D20            ASL     #16,A,A                           ; Command into bits 19:16
2020      P:00059E P:0005A0 51E000            MOVE              X:(R0),B0
2021      P:00059F P:0005A1 200010            ADD     B,A
2022      P:0005A0 P:0005A2 000000            NOP
2023   
2024      P:0005A1 P:0005A3 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
2025      P:0005A2 P:0005A4 00000C            RTS
2026   
2027   
2028                                PCI_RECOVER_COUNT
2029                                ; Calculate number of PCI words not transferred.
2030                                ; Correct BURST_SIZE.  Returns:
2031                                ;   B: bytes not transferred
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 42



2032                                ;   A: bytes transferred
2033      P:0005A3 P:0005A5 200013            CLR     A
2034      P:0005A4 P:0005A6 20001B            CLR     B
2035      P:0005A5 P:0005A7 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
2036      P:0005A6 P:0005A8 0A8A8F            JCLR    #RDCQ,X:DPSR,PCI_RECOVER_COUNT1
                            0005A9
2037      P:0005A8 P:0005AA 000009            INC     B
2038                                PCI_RECOVER_COUNT1
2039      P:0005A9 P:0005AB 000009            INC     B                                 ; We want N, not N-1.
2040      P:0005AA P:0005AC 014088            ADD     #0,B                              ; Clear carry
2041      P:0005AB P:0005AD 0C1C20            ASR     #16,A,A
2042      P:0005AC P:0005AE 200018            ADD     A,B                               ; B is words remaining
2043      P:0005AD P:0005AF 014088            ADD     #0,B                              ; Clear carry
2044      P:0005AE P:0005B0 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
2045      P:0005AF P:0005B1 50F000            MOVE              X:BURST_SIZE,A0
                            00002A
2046      P:0005B1 P:0005B3 200014            SUB     B,A                               ; A is bytes written
2047      P:0005B2 P:0005B4 00000C            RTS
2048   
2049   
2050                                PCI_UPDATE_R0
2051                                ;  Subtract A0 from BURST_SIZE and add A to the 32-bit hi-lo address at X:[R0].
2052                                ;  Caller can check Z flag to see if BURST_SIZE is now 0.
2053      P:0005B3 P:0005B5 210500            MOVE              A0,X1                   ; Save A for later
2054      P:0005B4 P:0005B6 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2055      P:0005B5 P:0005B7 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates [R0] = [R0] + B
                            0006A1
2056   
2057      P:0005B7 P:0005B9 57F000            MOVE              X:BURST_SIZE,B
                            00002A
2058      P:0005B9 P:0005BB 20006C            SUB     X1,B                              ; Zero flag must be preserved!
2059      P:0005BA P:0005BC 000000            NOP
2060      P:0005BB P:0005BD 557000            MOVE              B1,X:BURST_SIZE
                            00002A
2061      P:0005BD P:0005BF 00000C            RTS
2062   
2063   
2064                                ;----------------------------------------------;
2065                                ;  MCE PACKET PROCESSING                       ;
2066                                ;----------------------------------------------;
2067   
2068                                ;       Given a dword count in A, computes number of half FIFOs and
2069                                ;       number of left over FIFO reads required to get the whole
2070                                ;       packet.
2071   
2072                                ;       Input: A is packet size, in dwords
2073                                ;       Output: sets X:TOTAL_BUFFS and X:LEFT_TO_READ
2074                                ;       Trashes: A,B,X0
2075   
2076   
2077                                PACKET_PARTITIONS
2078      P:0005BE P:0005C0 507000            MOVE              A0,X:PACKET_SIZE
                            000023
2079   
2080      P:0005C0 P:0005C2 014088            ADD     #0,B                              ; Clear carry
2081      P:0005C1 P:0005C3 0C1D02            ASL     #1,A,A                            ;  * 2
2082      P:0005C2 P:0005C4 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
2083      P:0005C3 P:0005C5 240000            MOVE              #0,X0
2084      P:0005C4 P:0005C6 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
2085   
2086      P:0005C6 P:0005C8 557000            MOVE              B1,X:TOTAL_BUFFS
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 43



                            000024
2087      P:0005C8 P:0005CA 507000            MOVE              A0,X:LEFT_TO_READ
                            000025
2088      P:0005CA P:0005CC 00000C            RTS
2089   
2090   
2091                                ; BUFFER_PACKET
2092                                ;
2093                                ; Copies the packet in the FIFO to Y memory.
2094                                ;
2095                                ; In: TOTAL_BUFFS and LEFT_TO_READ must be pre-set (via PACKET_PARTITIONS);
2096                                ;     R1 is the destination index in Y memory.
2097                                ; Trashes: R1 is updated to point to the end of the copied data.
2098   
2099                                BUFFER_PACKET
2100   
2101      P:0005CB P:0005CD 54F400            MOVE              #>$b00,A1
                            000B00
2102      P:0005CD P:0005CF 0BF080            JSR     TIMER_STORE_A1
                            00064F
2103      P:0005CF P:0005D1 0BF080            JSR     TIMER_STORE
                            00064D
2104   
2105      P:0005D1 P:0005D3 062400            DO      X:TOTAL_BUFFS,BUFFER_PACKET_HALFS_DONE
                            0005DB
2106      P:0005D3 P:0005D5 0BF080            JSR     WAIT_FIFO_HALF
                            0005F8
2107      P:0005D5 P:0005D7 0BF080            JSR     TIMER_STORE
                            00064D
2108      P:0005D7 P:0005D9 0BF080            JSR     BUFFER_PACKET_HALF
                            0005F3
2109      P:0005D9 P:0005DB 0BF080            JSR     TIMER_STORE
                            00064D
2110      P:0005DB P:0005DD 000000            NOP
2111                                BUFFER_PACKET_HALFS_DONE
2112   
2113      
2114      
2115      
2116      
2117      P:0005DC P:0005DE 01AD81            JCLR    #HF,X:PDRD,BUFFER_PACKET_SINGLES_FAST
                            0005EF
2118   
2119      
2120      
2121   
2122                                BUFFER_PACKET_SINGLES
2123      
2124      
2125      P:0005DE P:0005E0 200013            CLR     A
2126      P:0005DF P:0005E1 20001B            CLR     B
2127      P:0005E0 P:0005E2 51F000            MOVE              X:TCR0,B0               ; Store timer value (50 MHz)
                            FFFF8C
2128      P:0005E2 P:0005E4 0C1C85            ASR     #2,B,B                            ; / 4
2129      P:0005E3 P:0005E5 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_DONE
                            0005EB
2130                                BUFFER_PACKET_SINGLES_WAIT
2131      P:0005E5 P:0005E7 50F000            MOVE              X:TCR0,A0
                            FFFF8C
2132      P:0005E7 P:0005E9 0C1C04            ASR     #2,A,A
2133      P:0005E8 P:0005EA 20000D            CMP     A,B
2134      P:0005E9 P:0005EB 0EA5E5            JEQ     BUFFER_PACKET_SINGLES_WAIT
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 44



2135      P:0005EA P:0005EC 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2136      P:0005EB P:0005ED 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2137                                BUFFER_PACKET_SINGLES_DONE
2138      P:0005EC P:0005EE 0BF080            JSR     TIMER_STORE
                            00064D
2139      P:0005EE P:0005F0 00000C            RTS
2140   
2141                                ;---------------------------------------------------------
2142   
2143                                BUFFER_PACKET_SINGLES_FAST
2144      P:0005EF P:0005F1 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_FAST_DONE
                            0005F1
2145      P:0005F1 P:0005F3 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2146                                BUFFER_PACKET_SINGLES_FAST_DONE
2147      P:0005F2 P:0005F4 00000C            RTS
2148   
2149                                ;---------------------------------------------------------
2150                                BUFFER_PACKET_HALF
2151      
2152      P:0005F3 P:0005F5 060082            DO      #512,BUFFER_PACKET_HALF_DONE
                            0005F6
2153      P:0005F5 P:0005F7 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2154      P:0005F6 P:0005F8 000000            NOP
2155                                BUFFER_PACKET_HALF_DONE
2156      P:0005F7 P:0005F9 00000C            RTS
2157   
2158                                ;---------------------------------------------------------
2159                                WAIT_FIFO_HALF
2160      P:0005F8 P:0005FA 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            00060D
2161      P:0005FA P:0005FC 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
                            0005F8
2162      P:0005FC P:0005FE 000000            NOP
2163      P:0005FD P:0005FF 000000            NOP
2164      P:0005FE P:000600 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            0005F8
2165      P:000600 P:000602 00000C            RTS
2166   
2167                                ;---------------------------------------------------------
2168   
2169                                ; This is the old single-buffering routine, which polls the EF.
2170                                BUFFER_PACKET_SINGLES_POLL
2171      P:000601 P:000603 062500            DO      X:LEFT_TO_READ,BUFFER_PACKET_DONE
                            00060B
2172                                BUFFER_PACKET_SINGLE
2173      P:000603 P:000605 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D5
2174      P:000605 P:000607 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE
                            000603
2175      P:000607 P:000609 000000            NOP
2176      P:000608 P:00060A 000000            NOP
2177      P:000609 P:00060B 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE   ; Protect against metastability
                            000603
2178      P:00060B P:00060D 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2179                                BUFFER_PACKET_DONE
2180      P:00060C P:00060E 00000C            RTS
2181   
2182                                ;---------------------------------------------------------
2183   
2184                                FATALITY_HANDLER
2185      P:00060D P:00060F 0C0100            JMP     START                             ; What could possibly go wrong?
2186   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 45



2187   
2188                                ; DROP_PACKET
2189                                ;
2190                                ; Reads a packet from the fifo, discarding it.
2191                                ;
2192                                ; In: TOTAL_BUFFS & LEFT_TO_READ
2193                                ; Trashes: A0
2194   
2195                                DROP_PACKET
2196      P:00060E P:000610 062400            DO      X:TOTAL_BUFFS,DROP_PACKET_SINGLES
                            000613
2197      P:000610 P:000612 0D05F8            JSR     WAIT_FIFO_HALF
2198      P:000611 P:000613 0BF080            JSR     DROP_FIFO_HALF
                            00061E
2199      P:000613 P:000615 000000            NOP
2200                                DROP_PACKET_SINGLES
2201      P:000614 P:000616 062500            DO      X:LEFT_TO_READ,DROP_PACKET_DONE
                            00061C
2202                                DROP_PACKET_SINGLE
2203      P:000616 P:000618 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            0002D5
2204      P:000618 P:00061A 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE
                            000616
2205      P:00061A P:00061C 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE     ; Protect against metastability
                            000616
2206      P:00061C P:00061E 09483F            MOVEP             Y:RDFIFO,A0
2207                                DROP_PACKET_DONE
2208      P:00061D P:00061F 00000C            RTS
2209   
2210                                DROP_FIFO_HALF
2211      
2212      P:00061E P:000620 060082            DO      #512,DROP_FIFO_DONE
                            000620
2213      P:000620 P:000622 09483F            MOVEP             Y:RDFIFO,A0
2214                                DROP_FIFO_DONE
2215      P:000621 P:000623 00000C            RTS
2216   
2217   
2218                                ;----------------------------------------------;
2219                                ;  TIMER HANDLING                              ;
2220                                ;----------------------------------------------;
2221   
2222                                ; Start value is TLR, count is in TCR, flag marked at TCPR
2223                                ; Must set TCSR[TCIE] to enable int
2224                                ; Must set TCSR[T] for timer to restart
2225   
2226                                TIMER_ENABLE
2227      P:000622 P:000624 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2228      P:000624 P:000626 000000            NOP
2229      P:000625 P:000627 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2230      P:000627 P:000629 00000C            RTS
2231   
2232                                TIMER_DISABLE
2233      P:000628 P:00062A 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
                            300200
2234      P:00062A P:00062C 000000            NOP
2235      P:00062B P:00062D 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2236      P:00062D P:00062F 00000C            RTS
2237   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 46



2238                                TIMER_DEFAULT
2239      P:00062E P:000630 0D0628            JSR     TIMER_DISABLE
2240      P:00062F P:000631 44F400            MOVE              #$4C4B40,X0             ; 5M -> 10 Hz.
                            4C4B40
2241      P:000631 P:000633 000000            NOP
2242      P:000632 P:000634 447000            MOVE              X0,X:TCPR0
                            FFFF8D
2243      P:000634 P:000636 0D0622            JSR     TIMER_ENABLE
2244      P:000635 P:000637 00000C            RTS
2245   
2246   
2248                                TIMER_ACTION
2249      P:000636 P:000638 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2250      P:000638 P:00063A 000000            NOP
2251      P:000639 P:00063B 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2252      P:00063B P:00063D 56F000            MOVE              X:QT_INFORM_IDX,A       ; QT inform time?
                            000046
2253      P:00063D P:00063F 0A0182            JCLR    #MODE_QT,X:MODE,TIMER_ACTION_OK
                            000645
2254      P:00063F P:000641 0140C5            CMP     #>0,A                             ; If inform_idx != 0
                            000000
2255      P:000641 P:000643 0AF0AA            JEQ     TIMER_ACTION_OK
                            000645
2256      P:000643 P:000645 0A7034            BSET    #QT_FLUSH,X:STATUS                ;    schedule inform
                            000000
2257                                TIMER_ACTION_OK
2258      P:000645 P:000647 00000C            RTS
2259   
2260   
2261                                ;----------------------------------------------;
2262                                ;  TIMER UTILITY                               ;
2263                                ;----------------------------------------------;
2264   
2265                                 TIMER_SOURCE
2266      FFFF8C                              EQU     TCR0
2267   
2268                                TIMER_STORE_INIT
2269      P:000646 P:000648 50F400            MOVE              #>TIMER_BUFFER,A0
                            201000
2270      P:000648 P:00064A 000000            NOP
2271      P:000649 P:00064B 507000            MOVE              A0,X:TIMER_INDEX
                            00004C
2272      P:00064B P:00064D 211400            MOVE              A0,R4
2273      P:00064C P:00064E 00000C            RTS
2274   
2275                                TIMER_STORE
2276      
2277      
2278      P:00064D P:00064F 56F000            MOVE              X:TIMER_SOURCE,A
                            FFFF8C
2279                                                                                    ; Fall-through
2280   
2281                                TIMER_STORE_A1
2282      
2283      P:00064F P:000651 5C5C00            MOVE                          A1,Y:(R4)+
2284      P:000650 P:000652 228C00            MOVE              R4,A1
2285      P:000651 P:000653 0140C5            CMP     #>TIMER_BUFFER_END,A
                            202000
2286      P:000653 P:000655 547000            MOVE              A1,X:TIMER_INDEX
                            00004C
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 47



2287      P:000655 P:000657 0E1646            JGE     TIMER_STORE_INIT
2288      P:000656 P:000658 00000C            RTS
2289   
2290   
2291                                ;----------------------------------------------;
2292                                ;  CIRCULAR BUFFER HANDLING                    ;
2293                                ;----------------------------------------------;
2294   
2295                                BUFFER_INCR
2296   
2297      P:000657 P:000659 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
                            000042
2298      P:000659 P:00065B 014180            ADD     #1,A                              ;
2299      P:00065A P:00065C 57F000            MOVE              X:QT_BUF_MAX,B          ;
                            00003F
2300      P:00065C P:00065E 20000D            CMP     A,B                               ;
2301      P:00065D P:00065F 0AF0AF            JLE     BUFFER_RESET                      ;       head = 0
                            000669
2302                                                                                    ; else
2303      P:00065F P:000661 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            000042
2304   
2305      P:000661 P:000663 20001B            CLR     B
2306      P:000662 P:000664 51F000            MOVE              X:QT_BUF_SIZE,B0
                            00003E
2307      P:000664 P:000666 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2308      P:000666 P:000668 0BF080            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
                            0006A1
2309   
2310      P:000668 P:00066A 00000C            RTS
2311   
2312   
2313                                BUFFER_RESET
2314      P:000669 P:00066B 60F400            MOVE              #QT_BASE_LO,R0
                            00003C
2315      P:00066B P:00066D 0BF080            JSR     LOAD_HILO_ADDRESS
                            00069B
2316      P:00066D P:00066F 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2317      P:00066F P:000671 0BF080            JSR     SAVE_HILO_ADDRESS                 ; QT_DEST_LO = QT_BASE_LO
                            0006A3
2318   
2319      P:000671 P:000673 240000            MOVE              #0,X0
2320      P:000672 P:000674 447000            MOVE              X0,X:QT_BUF_HEAD        ; HEAD = 0
                            000042
2321      P:000674 P:000676 00000C            RTS
2322   
2323   
2324                                BUFFER_INFORM_CHECK
2325      P:000675 P:000677 56F000            MOVE              X:QT_INFORM_IDX,A
                            000046
2326      P:000677 P:000679 014180            ADD     #1,A
2327      P:000678 P:00067A 57F000            MOVE              X:QT_INFORM,B
                            000041
2328      P:00067A P:00067C 20000D            CMP     A,B
2329      P:00067B P:00067D 0AF0A7            JGT     BUFFER_INFORM_OK                  ; If inform_idx + 1 <= inform
                            00067F
2330      P:00067D P:00067F 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2331   
2332                                BUFFER_INFORM_OK
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 48



2333      P:00067F P:000681 567000            MOVE              A,X:QT_INFORM_IDX       ; inform_idx = inform_idx + 1
                            000046
2334      P:000681 P:000683 00000C            RTS
2335   
2336   
2337                                ;---------------------------------------------------------------
2338                                BUFFER_INFORM
2339                                ;---------------------------------------------------------------
2340                                ; Informs host of current buffer status
2341   
2342      
2343      P:000682 P:000684 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            00069A
2344      P:000684 P:000686 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            00069A
2345   
2346                                          PCI_LOCKDOWN                              ; Disable host IRQ
2348   
2349      P:000687 P:000689 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2350      P:000689 P:00068B 440B00            MOVE              X0,X:<DTXS_WD1
2351   
2352      P:00068A P:00068C 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            000042
2353      P:00068C P:00068E 440C00            MOVE              X0,X:<DTXS_WD2
2354   
2355      P:00068D P:00068F 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000043
2356      P:00068F P:000691 440D00            MOVE              X0,X:<DTXS_WD3
2357   
2358      P:000690 P:000692 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
                            000047
2359      P:000692 P:000694 440E00            MOVE              X0,X:<DTXS_WD4
2360   
2361      P:000693 P:000695 0D047A            JSR     PCI_MESSAGE_TO_HOST
2362   
2363      P:000694 P:000696 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
2364      P:000696 P:000698 240000            MOVE              #0,X0                   ; Reset inform index
2365      P:000697 P:000699 447000            MOVE              X0,X:QT_INFORM_IDX
                            000046
2366                                          PCI_LOCKUP                                ; Enable host IRQ
2368                                INFORM_EXIT
2369      P:00069A P:00069C 00000C            RTS
2370   
2371   
2372   
2373                                ;----------------------------------------------;
2374                                ;  ADDRESS HANDLING                            ;
2375                                ;----------------------------------------------;
2376   
2380   
2381                                LOAD_HILO_ADDRESS
2382      
2383      
2384      P:00069B P:00069D 200013            CLR     A
2385      P:00069C P:00069E 50D800            MOVE              X:(R0)+,A0
2386      P:00069D P:00069F 44D000            MOVE              X:(R0)-,X0
2387      P:00069E P:0006A0 0C1940            INSERT  #$010010,X0,A
                            010010
2388      P:0006A0 P:0006A2 00000C            RTS
2389   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  main.asm  Page 49



2390                                ADD_HILO_ADDRESS
2391      
2392      
2393   
2394      P:0006A1 P:0006A3 0D069B            JSR     LOAD_HILO_ADDRESS
2395      P:0006A2 P:0006A4 200010            ADD     B,A
2396   
2397                                SAVE_HILO_ADDRESS
2398      
2399      
2400   
2401      P:0006A3 P:0006A5 445800            MOVE              X0,X:(R0)+              ; pre-increment
2402      P:0006A4 P:0006A6 240000            MOVE              #0,X0
2403      P:0006A5 P:0006A7 0C1D11            ASL     #8,A,B
2404      P:0006A6 P:0006A8 0C1940            INSERT  #$008010,X0,A
                            008010
2405      P:0006A8 P:0006AA 555000            MOVE              B1,X:(R0)-              ; store hi16
2406      P:0006A9 P:0006AB 506000            MOVE              A0,X:(R0)
2407      P:0006AA P:0006AC 0C1C90            ASR     #8,B,A
2408      P:0006AB P:0006AD 00000C            RTS
2409   
2410   
2411   
2412   
2413                                BOOTCODE_END
2414                                 BOOTEND_ADDR
2415      0006AC                              EQU     @CVI(BOOTCODE_END)
2416   
2417                                PROGRAM_END
2418      0006AC                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2419                                          INCLUDE 'vars.asm'
2420                                      COMMENT *
2421   
2422                                Variable table and bit defines for our variables.
2423   
2424                                See info.asm for versioning and authors.
2425   
2426                                        *
2427   
2428   
2429                                ; The variable table is mapped to X memory but stored inline in the
2430                                ; eeprom / P memory after the main code (but before the application
2431                                ; area).
2432   
2433      X:000000 P:0006AE                   ORG     X:VAR_TBL,P:
2434   
2435   
2436                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2437                                 VAR_TBL_START
2438      0006AC                              EQU     @LCV(L)-2
2439                                          ENDIF
2440   
2441                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2443                                          ENDIF
2444   
2445   
2446                                ;-----------------------------------------------
2447 d    X:000000 P:0006AE 000000  STATUS    DC      0                                 ; Internal status flags
2448 d    X:000001 P:0006AF 000000  MODE      DC      0                                 ; Operating mode control
2449 d                               FRAME_COUNT
2450 d    X:000002 P:0006B0 000000            DC      0                                 ; Count of data frames from MCE
2451   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  vars.asm  Page 50



2452                                ;-----------------------------------------------
2453 d    X:000003 P:0006B1 550106  REV_NUMBER DC     $550106                           ; byte 0 = minor revision #
2454                                                                                    ; byte 1 = major revision #
2455                                                                                    ; byte 2 = release Version (ascii letter)
2456 d    X:000004 P:0006B2 000000  REV_DATA  DC      $000000                           ; Not used by UBC
2457 d    X:000005 P:0006B3 2EF490  P_CHECKSUM DC     $2EF490                           ; Not used by UBC
2458   
2459                                ;-----------------------------------------------
2460 d    X:000006 P:0006B4 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2461                                ;-----------------------------------------------
2462   
2463   
2465   
2466 d    X:000007 P:0006B5 000000  DRXR_WD1  DC      0                                 ; Storage for words read from PC during vect
or command
2467 d    X:000008 P:0006B6 000000  DRXR_WD2  DC      0
2468 d    X:000009 P:0006B7 000000  DRXR_WD3  DC      0
2469 d    X:00000A P:0006B8 000000  DRXR_WD4  DC      0
2470   
2471 d    X:00000B P:0006B9 000000  DTXS_WD1  DC      0                                 ; Storage for words to be written to PC as r
eply
2472 d    X:00000C P:0006BA 000000  DTXS_WD2  DC      0
2473 d    X:00000D P:0006BB 000000  DTXS_WD3  DC      0
2474 d    X:00000E P:0006BC 000000  DTXS_WD4  DC      0
2475   
2476   
2478   
2479 d    X:00000F P:0006BD 000000  SV_A0     DC      0
2480 d    X:000010 P:0006BE 000000  SV_A1     DC      0
2481 d    X:000011 P:0006BF 000000  SV_A2     DC      0
2482 d    X:000012 P:0006C0 000000  SV_B0     DC      0
2483 d    X:000013 P:0006C1 000000  SV_B1     DC      0
2484 d    X:000014 P:0006C2 000000  SV_B2     DC      0
2485 d    X:000015 P:0006C3 000000  SV_X0     DC      0
2486 d    X:000016 P:0006C4 000000  SV_X1     DC      0
2487 d    X:000017 P:0006C5 000000  SV_Y0     DC      0
2488 d    X:000018 P:0006C6 000000  SV_Y1     DC      0
2489 d    X:000019 P:0006C7 000000  SV_R0     DC      0
2490 d    X:00001A P:0006C8 000000  SV_SR     DC      0
2491   
2492   
2494   
2495 d    X:00001B P:0006C9 000000  HEAD_W1_0 DC      0                                 ; Preamble $A5A5
2496 d    X:00001C P:0006CA 000000  HEAD_W1_1 DC      0                                 ;          $A5A5
2497 d    X:00001D P:0006CB 000000  HEAD_W2_0 DC      0                                 ;          $5A5A
2498 d    X:00001E P:0006CC 000000  HEAD_W2_1 DC      0                                 ;          $5A5A
2499 d    X:00001F P:0006CD 000000  HEAD_W3_0 DC      0                                 ; 'RP' or 'DA'
2500 d    X:000020 P:0006CE 000000  HEAD_W3_1 DC      0                                 ; '  '   $2020
2501 d    X:000021 P:0006CF 000000  HEAD_W4_0 DC      0                                 ; Packet size LSW
2502 d    X:000022 P:0006D0 000000  HEAD_W4_1 DC      0                                 ;             MSW
2503   
2504   
2506   
2507 d                               PACKET_SIZE
2508 d    X:000023 P:0006D1 000000            DC      0                                 ; Size, in dwords of most recent packet from
 MCE.
2509 d                               TOTAL_BUFFS
2510 d    X:000024 P:0006D2 000000            DC      0                                 ; Number of 512 word half-buffers in packet.
2511 d                               LEFT_TO_READ
2512 d    X:000025 P:0006D3 000000            DC      0                                 ; Number of words left to read after last 51
2 buffer
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  vars.asm  Page 51



2513   
2514 d                               PREAMBLE_ERRORS
2515 d    X:000026 P:0006D4 000000            DC      0                                 ; Failed on preamble processing
2516 d                               PTYPE_ERRORS
2517 d    X:000027 P:0006D5 000000            DC      0                                 ; Failed on packet type
2518 d                               PSIZE_ERRORS
2519 d    X:000028 P:0006D6 000000            DC      0                                 ; Failed on packet size test
2520   
2521   
2523   
2524 d                               PCI_BURST_SIZE
2525 d    X:000029 P:0006D7 000040            DC      $40                               ; Should be < 4*latency assigned by OS
2526 d    X:00002A P:0006D8 000000  BURST_SIZE DC     0
2527 d    X:00002B P:0006D9 000000  BLOCK_SIZE DC     0
2528   
2529 d    X:00002C P:0006DA 000000  CON_SRC_LO DC     0                                 ; Set by CON host command
2530 d    X:00002D P:0006DB 000000  CON_SRC_HI DC     0
2531   
2532 d    X:00002E P:0006DC 000000  YMEM_SRC  DC      0                                 ; Vars for YMEM -> PC transfers
2533 d                               BURST_DEST_LO
2534 d    X:00002F P:0006DD 000000            DC      0
2535 d                               BURST_DEST_HI
2536 d    X:000030 P:0006DE 000000            DC      0
2537   
2538 d                               BURST_SRC_LO
2539 d    X:000031 P:0006DF 000000            DC      0                                 ; Vars for PC -> YMEM transfers
2540 d                               BURST_SRC_HI
2541 d    X:000032 P:0006E0 000000            DC      0
2542 d    X:000033 P:0006E1 000000  YMEM_DEST DC      0
2543   
2544 d    X:000034 P:0006E2 000000  DMA_ERRORS DC     0                                 ; Error counting
2545 d    X:000035 P:0006E3 000000  EC_TRTY   DC      0
2546 d    X:000036 P:0006E4 000000  EC_TO     DC      0
2547 d    X:000037 P:0006E5 000000  EC_TDIS   DC      0
2548 d    X:000038 P:0006E6 000000  EC_TAB    DC      0
2549 d    X:000039 P:0006E7 000000  EC_MAB    DC      0
2550 d    X:00003A P:0006E8 000000  EC_DPER   DC      0
2551 d    X:00003B P:0006E9 000000  EC_APER   DC      0
2552   
2553   
2555   
2556 d    X:00003C P:0006EA 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2557 d    X:00003D P:0006EB 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2558 d                               QT_BUF_SIZE
2559 d    X:00003E P:0006EC 000000            DC      0                                 ; Separation of buffers, in bytes
2560 d    X:00003F P:0006ED 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2561 d                               QT_FRAME_SIZE
2562 d    X:000040 P:0006EE 000000            DC      0                                 ; Expected data packet size, in bytes
2563 d    X:000041 P:0006EF 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2564   
2565 d                               QT_BUF_HEAD
2566 d    X:000042 P:0006F0 000000            DC      0                                 ; Index of buf for next write
2567 d                               QT_BUF_TAIL
2568 d    X:000043 P:0006F1 000000            DC      0                                 ; Index at which we must not write
2569   
2570 d    X:000044 P:0006F2 000000  QT_DEST_LO DC     0                                 ; PC address for next write
2571 d    X:000045 P:0006F3 000000  QT_DEST_HI DC     0                                 ;
2572 d                               QT_INFORM_IDX
2573 d    X:000046 P:0006F4 000000            DC      0                                 ; Number of packets since last inform
2574 d    X:000047 P:0006F5 000000  QT_DROPS  DC      0                                 ; Dropped packet count
2575   
2576   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  vars.asm  Page 52



2578   
2579 d    X:000048 P:0006F6 000000  RP_BASE_LO DC     0                                 ; PC buffer start address
2580 d    X:000049 P:0006F7 000000  RP_BASE_HI DC     0                                 ;
2581 d                               RP_MAX_SIZE
2582 d    X:00004A P:0006F8 000000            DC      0                                 ; Maximum reply size, dwords
2583 d    X:00004B P:0006F9 000000  RP_DROPS  DC      0                                 ; Dropped packet count
2584   
2585   
2587   
2588 d                               TIMER_INDEX
2589 d    X:00004C P:0006FA 000000            DC      0
2590   
2591   
2593   
2594 d    X:00004D P:0006FB 000000  BDEBUG0   DC      0
2595 d    X:00004E P:0006FC 000000  BDEBUG1   DC      0
2596 d    X:00004F P:0006FD 000000  BDEBUG2   DC      0
2604   
2605 d                               TRIGGER_FAKE
2606 d    X:000050 P:0006FE 000000            DC      0
2607   
2608 d    X:000051 P:0006FF 000000  FIFO_FAILS DC     0
2609 d                               PTYPE_FAILS
2610 d    X:000052 P:000700 000000            DC      0
2611 d    X:000053 P:000701 000000  DA_COUNT  DC      0
2612   
2614 d    X:000054 P:000702 000000  CMD_SIZE  DC      0
2615 d    X:000055 P:000703 000000  CMD_WORD  DC      0
2616   
2617 d                               REP_BUS_ADDR
2618 d    X:000056 P:000704 000000            DC      0,0
     d                      000000
2619 d                               DATA_BUS_ADDR
2620 d    X:000058 P:000706 000000            DC      0,0
     d                      000000
2621   
2623      000004                    COMM_REP  EQU     4                                 ; Reply needs to be sent
2624      000005                    COMM_CMD  EQU     5                                 ; Command needs to be processed
2625                                 COMM_MCEREP
2626      000006                              EQU     6                                 ; MCE reply has been buffered for send to ho
st
2627                                 COMM_MCEDATA
2628      000007                              EQU     7                                 ; MCE data " "
2629      000008                    COMM_ERR  EQU     8                                 ; Command not recognized or whatever
2630                                 COMM_REP_ENABLED
2631      00000C                              EQU     12                                ;
2632                                 COMM_BUF_UPDATE
2633      00000D                              EQU     13                                ; Data has been written to buffer
2634                                 COMM_TFR_YMEM
2635      00000E                              EQU     14                                ; PCI burst is coming from Y mem, not X mem.
2636   
2637   
2638 d    X:00005A P:000708 000000  MEM_SRC   DC      0
2639   
2640 d                               INT_DEBUG_BUF_IDX
2641 d    X:00005B P:000709 000000            DC      0
2642 d                               DEBUG_BUF_IDX
2643 d    X:00005C P:00070A 000000            DC      0
2644   
2645   
2646      000100                    CMD_BUFFER EQU    $100
2647   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  vars.asm  Page 53



2648   
2655   
2656      0000A0                    RB_SIZE   EQU     128+32                            ; This MUST be even, so that effective numbe
r
2657                                                                                    ; of 32-bit words is integral
2658   
2659      000001                    RB_VERSION EQU    1                                 ; Version of this datagram
2660                                 RB_TYPE_DSP_REP
2661      000001                              EQU     1                                 ;
2662                                 RB_TYPE_MCE_REP
2663      000002                              EQU     2                                 ;
2664                                 RB_TYPE_BUF_INF
2665      000003                              EQU     3                                 ;
2666   
2668                                 REP_BUFFER1
2669      X:00005D P:00070B                   DS      RB_SIZE
2670   
2672                                 REP_VERSION
2673      00005D                              EQU     REP_BUFFER1+0                     ;
2674      00005E                    REP_SIZE  EQU     REP_BUFFER1+1                     ;
2675      00005F                    REP_TYPE  EQU     REP_BUFFER1+2                     ;
2676      00006D                    REP_DATA  EQU     REP_BUFFER1+16                    ; Start of DSP or MCE reply data
2677                                 REP_HEADER_SIZE
2678      000010                              EQU     (REP_DATA-REP_VERSION)            ; Whatever
2679   
2681      00006D                    REP_RSTAT EQU     REP_DATA+0
2682      00006E                    REP_RSIZE EQU     REP_DATA+1
2683      00006F                    REP_RCMD  EQU     REP_DATA+2
2684                                 REP_RPAYLOAD
2685      000071                              EQU     REP_DATA+4
2686      000081                    REP_REND  EQU     REP_RPAYLOAD+16                   ; Whatever.
2687   
2688   
2689      000000                    MCEREP_BUF EQU    0                                 ; Y-mem location for mce reply buffer?
2691                                 MCEREP_PRE0
2692      000000                              EQU     0
2693                                 MCEREP_PRE1
2694      000002                              EQU     2
2695                                 MCEREP_TYPE
2696      000004                              EQU     4
2697                                 MCEREP_SIZE
2698      000006                              EQU     6
2699                                 MCEREP_PAYLOAD
2700      000008                              EQU     8
2701      000088                    MCEREP_END EQU    MCEREP_PAYLOAD+128                ; MCE replies are max 256 bytes
2702   
2704                                 MCEDATA_BUF
2705      000008                              EQU     $8                                ;Debugging.
2706   
2709                                 RB_REP_SIZE
2710      000024                              EQU     (REP_REND-REP_DATA+REP_HEADER_SIZE)
2711                                 RB_MCE_SIZE
2712      000098                              EQU     (MCEREP_END+REP_HEADER_SIZE)
2713                                 RB_INF_SIZE
2714      000016                              EQU     (REP_REND-REP_DATA+2)
2715   
2716   
2717      002000                    DEBUG_BUF EQU     $2000
2718      002100                    DEBUG_DUMP EQU    $2100
2719                                 INT_DEBUG_BUF
2720      008000                              EQU     $8000
2721   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  vars.asm  Page 54



2722   
2726 d                               CIRCBUF_HEAD
2727 d    X:0000FD P:0007AB 000000            DC      0                                 ; Write index
2728 d                               CIRCBUF_TAIL
2729 d    X:0000FE P:0007AC 000000            DC      0                                 ; Read  index
2730                                 CIRCBUF_START
2731      000000                              EQU     0                                 ; Buffer start in Y mem.
2732                                 CIRCBUF_SIZE
2733      100000                              EQU     $100000                           ; 1 million locations = 2 MB
2734   
2735   
2736                                ;----------------------------------------------------------
2737   
2739   
2740                                 APPLICATION_RUNNING
2741      000000                              EQU     0                                 ; Indicates application is in progress
2742                                 SEND_TO_HOST
2743      000001                              EQU     1                                 ; set in HST ISR when host ready for packet 
(stays set until after HST reply)
2744                                 FATAL_ERROR
2745      000002                              EQU     2                                 ; PCI message to host error detected by driv
er....
2746      000003                    FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays s
et till packet processed
2747   
2748      000009                    HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of p
acket (stays set until after HST reply)
2749   
2750      00000A                    CON_DEMAND EQU    10                                ; Host has requested an MCE command be sent
2751      00000B                    CON_MCE   EQU     11                                ; Command has been copied to Y buffer and sh
ould be sent to MCE
2752   
2753                                 PCIDMA_RESTART
2754      000010                              EQU     16                                ; DMA flags used for error recovery
2755                                 PCIDMA_RESUME
2756      000011                              EQU     17
2757   
2758      000014                    QT_FLUSH  EQU     20                                ; Set when it is time to inform Host of curr
ent buffer position.
2759                                 RP_BUFFER_FULL
2760      000015                              EQU     21                                ; Set when Quiet RP buffer is occupied.
2761   
2762      000016                    FREEZER   EQU     22                                ; Suspend operations and just idle in the ma
in loop
2763                                 MAIN_LOOP_POLL
2764      000017                              EQU     23                                ; Cleared by the main loop, use to check for
 DSP lock-up
2765   
2766   
2768   
2769                                 MODE_APPLICATION
2770      000000                              EQU     0                                 ; set if PCI application to run
2771      000001                    MODE_MCE  EQU     1                                 ; process packets from MCE (!choke)
2772      000002                    MODE_QT   EQU     2                                 ; Quiet transfer for data packets (QT mode)
2773                                 MODE_RP_BUFFER
2774      000003                              EQU     3                                 ; Quiet transfer for reply packets (Quiet-RP
)
2775   
2776   
2778   
2779                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2780                                 VAR_TBL_END
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  vars.asm  Page 55



2781      0007AB                              EQU     @LCV(L)-2
2782                                          ENDIF
2783   
2784                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2786                                          ENDIF
2787   
2788                                 VAR_TBL_LENGTH
2789      0000FF                              EQU     VAR_TBL_END-VAR_TBL_START
2790                                          INCLUDE 'app.asm'
2791                                        COMMENT *
2792   
2793                                Auxiliary application area.
2794   
2795                                See info.asm for versioning and authors.
2796   
2797                                        *
2798                                          PAGE    132                               ; Printronix page width - 132 columns
2799                                          OPT     CEX                               ; print DC evaluations
2800   
2801                                          IF      @CVS(N,*)>=APPLICATION
2803                                          ENDIF
2804   
2805   
2806                                ;--------------------------------------------
2807                                ; APPLICATION AREA
2808                                ;---------------------------------------------
2809                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2810      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2811                                          ENDIF
2812   
2813                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2815                                          ENDIF
2816   
2817                                ; starts with no application loaded
2818                                ; so just reply with an error if we get a GOA command
2819   
2820      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2821      P:000802 P:000804 440B00            MOVE              X0,X:<DTXS_WD1          ; REPly
2822      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2823      P:000805 P:000807 440C00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2824      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2825      P:000808 P:00080A 440D00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2826      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2827      P:00080B P:00080D 440E00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2828      P:00080C P:00080E 0D0493            JSR     <RESTORE_REGISTERS
2829      P:00080D P:00080F 0D047A            JSR     <PCI_MESSAGE_TO_HOST
2830      P:00080E P:000810 0A0100            BCLR    #MODE_APPLICATION,X:<MODE
2831      P:00080F P:000811 0C016E            JMP     PACKET_IN
2832   
2833   
2834   
2835                                          INCLUDE 'hacking.asm'
2836                                                COMMENT *
2837   
2838                                        This implementation does communication with the host using PCI
2839                                        master writes only.
2840   
2841                                        *
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 56



2842                                          PAGE    132                               ; Printronix page width - 132 columns
2843                                          OPT     CEX                               ; print DC evaluations
2844   
2846                                HACK_ENTRY
2848   
2849      
2850      P:000810 P:000812 0A8985            JCLR    #DSR_HF2,X:DSR,HACK_EXIT
                            00083C
2851   
2852                                HACK_INIT
2853      
2854      P:000812 P:000814 0BF080            JSR     RESET_FIFO
                            000B6E
2855   
2856      
2857      P:000814 P:000816 0A0005            BCLR    #COMM_CMD,X:STATUS
2858      P:000815 P:000817 0A0004            BCLR    #COMM_REP,X:STATUS
2859      P:000816 P:000818 0A0006            BCLR    #COMM_MCEREP,X:STATUS
2860      P:000817 P:000819 0A0007            BCLR    #COMM_MCEDATA,X:STATUS
2861      P:000818 P:00081A 0A0008            BCLR    #COMM_ERR,X:STATUS
2862      P:000819 P:00081B 0A000C            BCLR    #COMM_REP_ENABLED,X:STATUS
2863      P:00081A P:00081C 0A000D            BCLR    #COMM_BUF_UPDATE,X:STATUS
2864   
2865      
2866      P:00081B P:00081D 0BF080            JSR     REPLY_BUFFER_INIT
                            000846
2867   
2868      
2869      P:00081D P:00081F 44F400            MOVE              #>INT_DEBUG_BUF,X0
                            008000
2870      P:00081F P:000821 447000            MOVE              X0,X:INT_DEBUG_BUF_IDX
                            00005B
2871      P:000821 P:000823 44F400            MOVE              #>DEBUG_BUF,X0
                            002000
2872      P:000823 P:000825 447000            MOVE              X0,X:DEBUG_BUF_IDX
                            00005C
2873   
2874      P:000825 P:000827 44F400            MOVE              #>TIMER_BUFFER,X0
                            201000
2875      P:000827 P:000829 447000            MOVE              X0,X:TIMER_INDEX
                            00004C
2876   
2877      
2878      P:000829 P:00082B 0A8522            BSET    #DCTR_SRIE,X:DCTR
2879   
2880      
2881      P:00082A P:00082C 0A8524            BSET    #DCTR_HF4,X:DCTR
2882   
2883      
2884      
2885      
2886                                HACK_LOOP
2887      
2888      P:00082B P:00082D 0B00A5            JSSET   #COMM_CMD,X:STATUS,PROCESS_PC_CMD_2
                            000A0E
2889   
2890      
2891      P:00082D P:00082F 0B00A4            JSSET   #COMM_REP,X:STATUS,PROCESS_REPLY
                            000900
2892   
2893      
2894      P:00082F P:000831 0BF080            JSR     CHECK_FOR_DATA
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 57



                            000ADA
2895   
2896      
2897      P:000831 P:000833 0B00A6            JSSET   #COMM_MCEREP,X:STATUS,PROCESS_MCE_REPLY
                            000929
2898      P:000833 P:000835 0B00A7            JSSET   #COMM_MCEDATA,X:STATUS,PROCESS_MCE_DATA
                            00095B
2899   
2900      
2901      P:000835 P:000837 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION_X
                            00083F
2902   
2903      
2904      P:000837 P:000839 0B00B4            JSSET   #QT_FLUSH,X:STATUS,SEND_BUF_INFO
                            000990
2905   
2906      
2907      P:000839 P:00083B 0A89A5            JSET    #DSR_HF2,X:DSR,HACK_LOOP
                            00082B
2908   
2909   
2910      
2911      P:00083B P:00083D 0A0014            BCLR    #QT_FLUSH,X:STATUS
2912   
2913                                HACK_EXIT
2914      
2915      P:00083C P:00083E 0A8502            BCLR    #DCTR_SRIE,X:DCTR
2916   
2917      
2918      P:00083D P:00083F 0A8504            BCLR    #DCTR_HF4,X:DCTR
2919      P:00083E P:000840 00000C            RTS
2920   
2921   
2922                                TIMER_ACTION_X
2923      P:00083F P:000841 07F40F            MOVEP             #$300201,X:TCSR0        ; Clear TOF, TCF, leave timer enabled.
                            300201
2924      
2925      P:000841 P:000843 0A000D            BCLR    #COMM_BUF_UPDATE,X:STATUS
2926      P:000842 P:000844 0AF0A0            JCC     TIMER_ACTION_X_OK
                            000845
2927      P:000844 P:000846 0A0034            BSET    #QT_FLUSH,X:STATUS                ;    schedule inform
2928                                TIMER_ACTION_X_OK
2929      P:000845 P:000847 00000C            RTS
2930   
2931   
2935   
2936   
2937                                REPLY_BUFFER_INIT
2938      
2939      P:000846 P:000848 240000            MOVE              #0,X0
2940      P:000847 P:000849 60F400            MOVE              #>REP_BUFFER1,R0
                            00005D
2941                                          .loop   #RB_SIZE
2943      P:00084B P:00084D 445800            MOVE              X0,X:(R0)+
2944                                          .endl
2946      P:00084C P:00084E 44F400            MOVE              #>RB_VERSION,X0
                            000001
2947      P:00084E P:000850 45F400            MOVE              #>(RB_REP_SIZE/2),X1
                            000012
2948      P:000850 P:000852 447000            MOVE              X0,X:REP_VERSION
                            00005D
2949      P:000852 P:000854 457000            MOVE              X1,X:REP_SIZE
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 58



                            00005E
2950      P:000854 P:000856 00000C            RTS
2951   
2952   
2953                                ;----------------------------------------------
2954                                BLOCK_TRANSFERX
2955                                ;----------------------------------------------
2956                                ;   In:
2957                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address (16:16)
2958                                ;   - BLOCK_SIZE is packet size, in bytes
2959                                ;   - MEM_SRC is start of data in X or Y memory
2960                                ;   - STATUS[COMM_TFR_YMEM] is used to determine X or Y
2961                                ;  Out:
2962                                ;   - BLOCK_SIZE will be decremented to zero.
2963                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
2964                                ;   - MEM_SRC will be incremented by BLOCK_SIZE/2
2965                                ;  Trashes:
2966                                ;   - A and B at least
2967   
2968      P:000855 P:000857 200013            CLR     A
2969      P:000856 P:000858 56AB00            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
2970      P:000857 P:000859 014085            CMP     #0,A                              ; Still bytes to transfer?
2971      P:000858 P:00085A 0AF0A2            JNE     BLOCK_TRANSFERX0
                            00085B
2972      P:00085A P:00085C 00000C            RTS
2973   
2974                                BLOCK_TRANSFERX0
2975      
2976      
2977      P:00085B P:00085D 57A900            MOVE              X:PCI_BURST_SIZE,B      ; B1 = burst size (256)
2978   
2979      P:00085C P:00085E 200005            CMP     B,A                               ; A ? B
2980      P:00085D P:00085F 0E185F            JGE     <BLOCK_TRANSFERX1                 ; jump if A >= B
2981      P:00085E P:000860 21CF00            MOVE              A,B                     ; This only moves A1,B1.
2982                                BLOCK_TRANSFERX1
2983      
2984      P:00085F P:000861 200014            SUB     B,A                               ; A -= B
2985      P:000860 P:000862 014088            ADD     #0,B                              ; Clear carry bit
2986      P:000861 P:000863 562B00            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
2987      P:000862 P:000864 572A00            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
2988      P:000863 P:000865 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
2989   
2990      
2991      P:000864 P:000866 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
2992      P:000866 P:000868 50F000            MOVE              X:MEM_SRC,A0
                            00005A
2993      P:000868 P:00086A 08C82F            MOVEP             A0,X:DSR0               ; DMA source
2994      P:000869 P:00086B 200010            ADD     B,A
2995      P:00086A P:00086C 00000B            DEC     B
2996      P:00086B P:00086D 507000            MOVE              A0,X:MEM_SRC            ; BURST_SRC += BURST_SIZE/2
                            00005A
2997   
2998      P:00086D P:00086F 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
2999   
3000      
3001      P:00086E P:000870 0A00AE            JSET    #COMM_TFR_YMEM,X:STATUS,BLOCK_TRANSFERX1_YMEM
                            000874
3002   
3003                                BLOCK_TRANSFERX1_XMEM
3004      P:000870 P:000872 08F4AC            MOVEP             #$8EFA50,X:DCR0         ; X to X
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 59



                            8EFA50
3005      P:000872 P:000874 0AF080            JMP     BLOCK_TRANSFERX_PCI
                            000878
3006   
3007                                BLOCK_TRANSFERX1_YMEM
3008      P:000874 P:000876 08F4AC            MOVEP             #$8EFA51,X:DCR0         ; X to Y
                            8EFA51
3009      P:000876 P:000878 0AF080            JMP     BLOCK_TRANSFERX_PCI
                            000878
3010   
3011   
3012                                BLOCK_TRANSFERX_PCI
3013      P:000878 P:00087A 44F400            MOVE              #>$7,X0                 ; Memory write
                            000007
3014      P:00087A P:00087C 302F00            MOVE              #BURST_DEST_LO,R0       ; RAM address
3015      P:00087B P:00087D 0D058D            JSR     PCI_GO                            ; Initiate PCI burst
3016   
3017      
3018      P:00087C P:00087E 0A8A84            JCLR    #MARQ,X:DPSR,*
                            00087C
3019   
3020      
3021      P:00087E P:000880 0A8A8E            JCLR    #MDT,X:DPSR,BLOCK_TRANSFERX_HANDLE_ERRORS
                            000884
3022   
3023      P:000880 P:000882 20001B            CLR     B
3024      P:000881 P:000883 51AA00            MOVE              X:BURST_SIZE,B0         ; All bytes were transferred
3025      P:000882 P:000884 0D06A1            JSR     ADD_HILO_ADDRESS                  ; Update source address
3026      P:000883 P:000885 0C0855            JMP     BLOCK_TRANSFERX                   ; Next burst in block
3027   
3028                                BLOCK_TRANSFERX_HANDLE_ERRORS
3029      
3030      P:000884 P:000886 0D04BE            JSR     PCI_ERROR_CLEAR
3031   
3032      P:000885 P:000887 0A0010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
3033      P:000886 P:000888 0E8878            JCS     BLOCK_TRANSFERX_PCI               ; Restart PCI burst
3034   
3035      P:000887 P:000889 0A0011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
3036      P:000888 P:00088A 0E0855            JCC     BLOCK_TRANSFERX                   ; Error but no error? Redo this burst.
3037   
3038      
3039      P:000889 P:00088B 0D05A3            JSR     PCI_RECOVER_COUNT                 ; Get transferred byte count in A.
3040      P:00088A P:00088C 0D05B3            JSR     PCI_UPDATE_R0
3041      P:00088B P:00088D 0C0878            JMP     BLOCK_TRANSFERX_PCI
3042   
3043   
3045      P:000900 P:000902                   ORG     P:$900,P:$902
3046   
3047                                ;----------------------------------------------
3048                                PROCESS_REPLY
3049                                ;----------------------------------------------
3050      
3051      P:000900 P:000902 0A00AC            JSET    #COMM_REP_ENABLED,X:STATUS,PROCESS_REPLY1
                            000904
3052      P:000902 P:000904 0A0004            BCLR    #COMM_REP,X:STATUS                ; Mark as... handled.
3053      P:000903 P:000905 00000C            RTS
3054   
3055                                PROCESS_REPLY1
3056      
3057      P:000904 P:000906 54F400            MOVE              #>RB_TYPE_DSP_REP,A1
                            000001
3058      P:000906 P:000908 45F400            MOVE              #>(RB_REP_SIZE/2),X1
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 60



                            000012
3059      P:000908 P:00090A 547000            MOVE              A1,X:REP_TYPE
                            00005F
3060      P:00090A P:00090C 457000            MOVE              X1,X:REP_SIZE
                            00005E
3061   
3062      
3063      P:00090C P:00090E 60F400            MOVE              #>REP_BUS_ADDR,R0
                            000056
3064      P:00090E P:000910 61F400            MOVE              #>BURST_DEST_LO,R1
                            00002F
3065                                          .loop   #2
3067      P:000912 P:000914 44D800            MOVE              X:(R0)+,X0
3068      P:000913 P:000915 445900            MOVE              X0,X:(R1)+
3069                                          .endl
3071   
3072      
3073      P:000914 P:000916 56F000            MOVE              X:REP_SIZE,A
                            00005E
3074      P:000916 P:000918 0C1D04            ASL     #2,A,A
3075      P:000917 P:000919 000000            NOP
3076      P:000918 P:00091A 218400            MOVE              A1,X0
3077      P:000919 P:00091B 442B00            MOVE              X0,X:BLOCK_SIZE
3078      P:00091A P:00091C 44F400            MOVE              #>REP_BUFFER1,X0
                            00005D
3079      P:00091C P:00091E 447000            MOVE              X0,X:MEM_SRC
                            00005A
3080   
3081      
3082      P:00091E P:000920 0A000E            BCLR    #COMM_TFR_YMEM,X:STATUS
3083      P:00091F P:000921 0D0855            JSR     BLOCK_TRANSFERX
3084   
3085      
3086      P:000920 P:000922 0A0004            BCLR    #COMM_REP,X:STATUS
3087   
3088      
3089      P:000921 P:000923 0A8526            BSET    #INTA,X:DCTR
3090      P:000922 P:000924 0A8983            JCLR    #DSR_HF0,X:DSR,*
                            000922
3091   
3092      P:000924 P:000926 0A0004            BCLR    #COMM_REP,X:STATUS                ; Mark as sent.
3093   
3094      P:000925 P:000927 0A8506            BCLR    #INTA,X:DCTR
3095      P:000926 P:000928 0A89A3            JSET    #DSR_HF0,X:DSR,*
                            000926
3096   
3097      P:000928 P:00092A 00000C            RTS
3098   
3099                                PROCESS_MCE_REPLY
3100      
3101      P:000929 P:00092B 330400            MOVE              #(MCEREP_BUF+MCEREP_TYPE),R3
3102      P:00092A P:00092C 4F8600            MOVE                          Y:(MCEREP_BUF+MCEREP_SIZE),Y1
3103      P:00092B P:00092D 306D00            MOVE              #(REP_DATA),R0
3104                                          .loop   #4
3106      P:00092E P:000930 4EDB00            MOVE                          Y:(R3)+,Y0
3107      P:00092F P:000931 465800            MOVE              Y0,X:(R0)+
3108                                          .endl
3110                                          .loop   #2
3112                                          .loop   Y1
3114      P:000934 P:000936 4EDB00            MOVE                          Y:(R3)+,Y0
3115      P:000935 P:000937 465800            MOVE              Y0,X:(R0)+
3116                                          .endl
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 61



3118      P:000936 P:000938 000000            nop
3119                                          .endl
3121   
3122      
3123      P:000937 P:000939 54F400            MOVE              #>RB_TYPE_MCE_REP,A1
                            000002
3124      P:000939 P:00093B 45F400            MOVE              #>(RB_MCE_SIZE/2),X1    ; size in 32-bit words.
                            00004C
3125      P:00093B P:00093D 547000            MOVE              A1,X:REP_TYPE
                            00005F
3126      P:00093D P:00093F 457000            MOVE              X1,X:REP_SIZE
                            00005E
3127   
3128      
3129      P:00093F P:000941 60F400            MOVE              #>REP_BUS_ADDR,R0
                            000056
3130      P:000941 P:000943 61F400            MOVE              #>BURST_DEST_LO,R1
                            00002F
3131                                          .loop   #2
3133      P:000945 P:000947 44D800            MOVE              X:(R0)+,X0
3134      P:000946 P:000948 445900            MOVE              X0,X:(R1)+
3135                                          .endl
3137   
3138      
3139      P:000947 P:000949 56F000            MOVE              X:REP_SIZE,A
                            00005E
3140      P:000949 P:00094B 0C1D04            ASL     #2,A,A
3141      P:00094A P:00094C 000000            NOP
3142      P:00094B P:00094D 218400            MOVE              A1,X0
3143      P:00094C P:00094E 442B00            MOVE              X0,X:BLOCK_SIZE
3144      P:00094D P:00094F 44F400            MOVE              #>REP_BUFFER1,X0
                            00005D
3145      P:00094F P:000951 447000            MOVE              X0,X:MEM_SRC
                            00005A
3146      P:000951 P:000953 0A000E            BCLR    #COMM_TFR_YMEM,X:STATUS
3147   
3148      
3149      P:000952 P:000954 0D0855            JSR     BLOCK_TRANSFERX
3150   
3151      
3152      P:000953 P:000955 0A0006            BCLR    #COMM_MCEREP,X:STATUS
3153   
3154      
3155      P:000954 P:000956 0A8526            BSET    #INTA,X:DCTR
3156      P:000955 P:000957 0A8983            JCLR    #DSR_HF0,X:DSR,*
                            000955
3157      P:000957 P:000959 0A8506            BCLR    #INTA,X:DCTR
3158      P:000958 P:00095A 0A89A3            JSET    #DSR_HF0,X:DSR,*
                            000958
3159   
3160      P:00095A P:00095C 00000C            RTS
3161   
3162                                ;----------------------------------------------
3163                                PROCESS_MCE_DATA
3164                                ;----------------------------------------------
3165      
3166      P:00095B P:00095D 56F000            MOVE              X:QT_BUF_HEAD,A
                            000042
3167      P:00095D P:00095F 014180            ADD     #1,A
3168      P:00095E P:000960 57BF00            MOVE              X:QT_BUF_MAX,B
3169      P:00095F P:000961 20000D            CMP     A,B                               ; End of buffer? [B ? A]
3170      P:000960 P:000962 0AF0A7            JGT     PROCESS_MCE_DATA__CHECK_TAIL
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 62



                            000963
3171      P:000962 P:000964 2E0000            MOVE              #0,A                    ; Start over
3172                                PROCESS_MCE_DATA__CHECK_TAIL
3173      P:000963 P:000965 57F000            MOVE              X:QT_BUF_TAIL,B         ; Buffer full?
                            000043
3174      P:000965 P:000967 20000D            CMP     A,B
3175      P:000966 P:000968 0AF0AA            JEQ     PROCESS_MCE_DATA__DROP_FRAME
                            000989
3176      
3177      
3178   
3179      
3180      P:000968 P:00096A 5E8600            MOVE                          Y:(MCEREP_BUF+MCEREP_SIZE),A
3181      P:000969 P:00096B 0C1D04            ASL     #2,A,A
3182      P:00096A P:00096C 000000            NOP
3183      P:00096B P:00096D 542B00            MOVE              A1,X:BLOCK_SIZE
3184   
3185      
3186      P:00096C P:00096E 60F400            MOVE              #>QT_DEST_LO,R0
                            000044
3187      P:00096E P:000970 61F400            MOVE              #>BURST_DEST_LO,R1
                            00002F
3188                                          .loop   #2
3190      P:000972 P:000974 44D800            MOVE              X:(R0)+,X0
3191      P:000973 P:000975 445900            MOVE              X0,X:(R1)+
3192                                          .endl
3194   
3195      P:000974 P:000976 44F400            MOVE              #>(MCEREP_BUF+MCEREP_PAYLOAD),X0
                            000008
3196      P:000976 P:000978 447000            MOVE              X0,X:MEM_SRC
                            00005A
3197      P:000978 P:00097A 0A002E            BSET    #COMM_TFR_YMEM,X:STATUS
3198   
3199      
3200      P:000979 P:00097B 0D0855            JSR     BLOCK_TRANSFERX
3201   
3202      
3203      P:00097A P:00097C 0D0657            JSR     BUFFER_INCR
3204   
3205                                PROCESS_MCE_DATA__DONE
3206      
3207      P:00097B P:00097D 0A0007            BCLR    #COMM_MCEDATA,X:STATUS
3208   
3209      
3210      
3211      
3212      P:00097C P:00097E 0A002D            BSET    #COMM_BUF_UPDATE,X:STATUS
3213      P:00097D P:00097F 56F000            MOVE              X:QT_INFORM_IDX,A
                            000046
3214      P:00097F P:000981 014180            ADD     #1,A
3215      P:000980 P:000982 57F000            MOVE              X:QT_INFORM,B
                            000041
3216      P:000982 P:000984 567000            MOVE              A,X:QT_INFORM_IDX
                            000046
3217      P:000984 P:000986 20000D            CMP     A,B
3218      P:000985 P:000987 0AF0A2            JNE     PROCESS_MCE_DATA__DONE2
                            000988
3219      P:000987 P:000989 0A0034            BSET    #QT_FLUSH,X:STATUS
3220                                PROCESS_MCE_DATA__DONE2
3221      P:000988 P:00098A 00000C            RTS
3222   
3223                                PROCESS_MCE_DATA__DROP_FRAME
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 63



3224      P:000989 P:00098B 56F000            MOVE              X:QT_DROPS,A
                            000047
3225      P:00098B P:00098D 014180            ADD     #1,A
3226      P:00098C P:00098E 000000            NOP
3227      P:00098D P:00098F 567000            MOVE              A,X:QT_DROPS
                            000047
3228      P:00098F P:000991 00000C            RTS
3229   
3230   
3231                                ;--------------------
3232                                SEND_BUF_INFO
3233                                ;--------------------
3234      P:000990 P:000992 0A0014            BCLR    #QT_FLUSH,X:STATUS
3235      
3236      
3237      
3238      
3239      
3240      
3241      P:000991 P:000993 0A008C            JCLR    #COMM_REP_ENABLED,X:STATUS,SEND_BUF_INFO_EXIT
                            0009BC
3242   
3243      
3244      P:000993 P:000995 44F000            MOVE              X:QT_BUF_HEAD,X0
                            000042
3245      P:000995 P:000997 447000            MOVE              X0,X:REP_DATA
                            00006D
3246      P:000997 P:000999 240000            MOVE              #0,X0
3247      P:000998 P:00099A 447000            MOVE              X0,X:(REP_DATA+1)
                            00006E
3248   
3249      
3250      P:00099A P:00099C 54F400            MOVE              #>RB_TYPE_BUF_INF,A1
                            000003
3251      P:00099C P:00099E 45F400            MOVE              #>(RB_INF_SIZE/2),X1    ; size in 32-bit words.
                            00000B
3252      P:00099E P:0009A0 547000            MOVE              A1,X:REP_TYPE
                            00005F
3253      P:0009A0 P:0009A2 457000            MOVE              X1,X:REP_SIZE
                            00005E
3254   
3255      
3256      P:0009A2 P:0009A4 60F400            MOVE              #>REP_BUS_ADDR,R0
                            000056
3257      P:0009A4 P:0009A6 61F400            MOVE              #>BURST_DEST_LO,R1
                            00002F
3258                                          .loop   #2
3260      P:0009A8 P:0009AA 44D800            MOVE              X:(R0)+,X0
3261      P:0009A9 P:0009AB 445900            MOVE              X0,X:(R1)+
3262                                          .endl
3264   
3265      
3266      P:0009AA P:0009AC 56F000            MOVE              X:REP_SIZE,A
                            00005E
3267      P:0009AC P:0009AE 0C1D04            ASL     #2,A,A
3268      P:0009AD P:0009AF 000000            NOP
3269      P:0009AE P:0009B0 218400            MOVE              A1,X0
3270      P:0009AF P:0009B1 442B00            MOVE              X0,X:BLOCK_SIZE
3271      P:0009B0 P:0009B2 44F400            MOVE              #>REP_BUFFER1,X0
                            00005D
3272      P:0009B2 P:0009B4 447000            MOVE              X0,X:MEM_SRC
                            00005A
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 64



3273      P:0009B4 P:0009B6 0A000E            BCLR    #COMM_TFR_YMEM,X:STATUS
3274   
3275      
3276      P:0009B5 P:0009B7 0D0855            JSR     BLOCK_TRANSFERX
3277   
3278      
3279      P:0009B6 P:0009B8 0A8526            BSET    #INTA,X:DCTR
3280      P:0009B7 P:0009B9 0A8983            JCLR    #DSR_HF0,X:DSR,*
                            0009B7
3281      P:0009B9 P:0009BB 0A8506            BCLR    #INTA,X:DCTR
3282      P:0009BA P:0009BC 0A89A3            JSET    #DSR_HF0,X:DSR,*
                            0009BA
3283   
3284                                SEND_BUF_INFO_EXIT
3285      P:0009BC P:0009BE 00000C            RTS
3286   
3287   
3288   
3289   
3290   
3291   
3292   
3296   
3298      000001                    CMD_READ_P EQU    1
3299      000002                    CMD_READ_X EQU    2
3300      000003                    CMD_READ_Y EQU    3
3301   
3302                                 CMD_WRITE_P
3303      000005                              EQU     5
3304                                 CMD_WRITE_X
3305      000006                              EQU     6
3306                                 CMD_WRITE_Y
3307      000007                              EQU     7
3308   
3309                                 CMD_SET_REP_BUF
3310      000009                              EQU     9
3311                                 CMD_SET_DATA_BUF
3312      00000A                              EQU     $A
3313   
3314                                 CMD_SET_TAIL
3315      000011                              EQU     $11
3316   
3317                                 CMD_SEND_MCE
3318      000021                              EQU     $21
3319   
3320   
3332   
3339   
3340                                ;------------------------
3341                                PROCESS_PC_CMD_INT
3342                                ;------------------------
3343      
3344      P:0009BD P:0009BF 0D04A0            JSR     SAVE_REGISTERS                    ; This does not save all the registers...
3345      P:0009BE P:0009C0 0A8502            BCLR    #DCTR_SRIE,X:DCTR
3346   
3347      
3348      P:0009BF P:0009C1 0A8523            BSET    #DCTR_HF3,X:DCTR
3349   
3350      
3351      P:0009C0 P:0009C2 0A8982            JCLR    #SRRQ,X:DSR,PROCESS_PC_CMD_INT_EXIT
                            0009F4
3352   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 65



3353      
3354      
3355      
3356      
3357      
3358   
3359      P:0009C2 P:0009C4 08440B            MOVEP             X:DRXR,X0               ; 16-bit word #0 = the command
3360      P:0009C3 P:0009C5 447000            MOVE              X0,X:CMD_WORD
                            000055
3361      P:0009C5 P:0009C7 000000            NOP
3362      P:0009C6 P:0009C8 000000            NOP
3363      P:0009C7 P:0009C9 0A8982            JCLR    #SRRQ,X:DSR,*
                            0009C7
3364      P:0009C9 P:0009CB 08440B            MOVEP             X:DRXR,X0
3365      P:0009CA P:0009CC 447000            MOVE              X0,X:CMD_SIZE           ; 16-bit word #1 = size of upcoming data,
                            000054
3366                                                                                    ; in 32-bit words.
3367   
3368      
3369      P:0009CC P:0009CE 200013            CLR     A
3370      P:0009CD P:0009CF 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3371      P:0009CF P:0009D1 54F000            MOVE              X:CMD_SIZE,A1
                            000054
3372   
3373      
3374      P:0009D1 P:0009D3 014085            CMP     #0,A
3375      P:0009D2 P:0009D4 0AF0AA            JEQ     PROCESS_PC_CMD_INT_OK
                            0009E0
3376   
3377                                          .loop   A1
3379      P:0009D6 P:0009D8 0A8982            JCLR    #SRRQ,X:DSR,*
                            0009D6
3380      P:0009D8 P:0009DA 08588B            MOVEP             X:DRXR,X:(R0)+
3381      P:0009D9 P:0009DB 000000            NOP
3382      P:0009DA P:0009DC 000000            NOP
3383      P:0009DB P:0009DD 0A8982            JCLR    #SRRQ,X:DSR,*
                            0009DB
3384      P:0009DD P:0009DF 08588B            MOVEP             X:DRXR,X:(R0)+
3385      P:0009DE P:0009E0 000000            NOP
3386      P:0009DF P:0009E1 000000            NOP
3387                                          .endl
3389   
3390                                PROCESS_PC_CMD_INT_OK
3391      P:0009E0 P:0009E2 60F000            MOVE              X:INT_DEBUG_BUF_IDX,R0
                            00005B
3392      P:0009E2 P:0009E4 44F000            MOVE              X:CMD_WORD,X0
                            000055
3393      P:0009E4 P:0009E6 4C5800            MOVE                          X0,Y:(R0)+
3394      P:0009E5 P:0009E7 44F000            MOVE              X:CMD_SIZE,X0
                            000054
3395      P:0009E7 P:0009E9 4C5800            MOVE                          X0,Y:(R0)+
3396      P:0009E8 P:0009EA 44F000            MOVE              X:CMD_BUFFER,X0
                            000100
3397      P:0009EA P:0009EC 4C5800            MOVE                          X0,Y:(R0)+
3398      P:0009EB P:0009ED 44F000            MOVE              X:(CMD_BUFFER+1),X0
                            000101
3399      P:0009ED P:0009EF 4C5800            MOVE                          X0,Y:(R0)+
3400      P:0009EE P:0009F0 44F400            MOVE              #>$aabbcc,X0            ; end-of-data
                            AABBCC
3401      P:0009F0 P:0009F2 4C5800            MOVE                          X0,Y:(R0)+
3402   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 66



3403      P:0009F1 P:0009F3 607000            MOVE              R0,X:INT_DEBUG_BUF_IDX
                            00005B
3404   
3405      
3406      P:0009F3 P:0009F5 0A0025            BSET    #COMM_CMD,X:STATUS
3407   
3408                                PROCESS_PC_CMD_INT_EXIT
3409      
3410      P:0009F4 P:0009F6 0A8522            BSET    #DCTR_SRIE,X:DCTR
3411      P:0009F5 P:0009F7 0A8503            BCLR    #DCTR_HF3,X:DCTR
3412      P:0009F6 P:0009F8 0D0493            JSR     RESTORE_REGISTERS
3413      P:0009F7 P:0009F9 000004            RTI
3414   
3416   
3417                                PROCESS_PC_CMD
3418      
3419      P:0009F8 P:0009FA 0A89A2            JSET    #SRRQ,X:DSR,PROCESS_PC_CMD_1
                            0009FB
3420      P:0009FA P:0009FC 00000C            RTS
3421   
3422                                PROCESS_PC_CMD_1
3423      
3424      P:0009FB P:0009FD 08440B            MOVEP             X:DRXR,X0
3425      P:0009FC P:0009FE 305400            MOVE              #CMD_SIZE,R0
3427      P:0009FD P:0009FF 0BF080            JSR     PROCESS_SPLIT_X0_XR0
                            000AC4
3428   
3429      
3430      P:0009FF P:000A01 200013            CLR     A
3431      P:000A00 P:000A02 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3432      P:000A02 P:000A04 54F000            MOVE              X:CMD_SIZE,A1
                            000054
3433      
3434      P:000A04 P:000A06 014085            CMP     #0,A
3435      P:000A05 P:000A07 0AF0AA            JEQ     PROCESS_PC_CMD_2
                            000A0E
3436                                          .loop   A1
3438      P:000A09 P:000A0B 0A8982            JCLR    #SRRQ,X:DSR,*
                            000A09
3439      P:000A0B P:000A0D 08588B            MOVEP             X:DRXR,X:(R0)+
3440      P:000A0C P:000A0E 000000            NOP
3441      P:000A0D P:000A0F 000000            NOP
3442                                          .endl
3444   
3447   
3448                                PROCESS_PC_CMD_2
3449      
3450      P:000A0E P:000A10 54F400            MOVE              #>RB_TYPE_DSP_REP,A1
                            000001
3451      P:000A10 P:000A12 57F000            MOVE              X:CMD_WORD,B            ; this will be used in the switch below.
                            000055
3452      P:000A12 P:000A14 547000            MOVE              A1,X:REP_TYPE           ; type is "dsp reply"
                            00005F
3453      P:000A14 P:000A16 557000            MOVE              B1,X:REP_RCMD           ; copy of command word
                            00006F
3454      P:000A16 P:000A18 517000            MOVE              B0,X:REP_RSTAT          ; status = 0
                            00006D
3455      P:000A18 P:000A1A 517000            MOVE              B0,X:REP_RSIZE          ; data size = 0
                            00006E
3456   
3457      
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 67



3458      
3459      
3460      
3461      P:000A1A P:000A1C 60F400            MOVE              #>CMD_BUFFER,R0
                            000100
3462      P:000A1C P:000A1E 0BF080            JSR     PROCESS_JOIN_XR0_A
                            000AD4
3463      P:000A1E P:000A20 211100            MOVE              A0,R1                   ; "address"
3464      P:000A1F P:000A21 0BF080            JSR     PROCESS_JOIN_XR0_A
                            000AD4
3465      P:000A21 P:000A23 210500            MOVE              A0,X1                   ; "data"
3466   
3467      P:000A22 P:000A24 0140CD            CMP     #>CMD_READ_P,B
                            000001
3468      P:000A24 P:000A26 0AF0AA            JEQ     PROCESS_READ_P
                            000A4D
3469   
3470      P:000A26 P:000A28 0140CD            CMP     #>CMD_READ_X,B
                            000002
3471      P:000A28 P:000A2A 0AF0AA            JEQ     PROCESS_READ_X
                            000A50
3472   
3473      P:000A2A P:000A2C 0140CD            CMP     #>CMD_READ_Y,B
                            000003
3474      P:000A2C P:000A2E 0AF0AA            JEQ     PROCESS_READ_Y
                            000A53
3475   
3476      P:000A2E P:000A30 0140CD            CMP     #>CMD_WRITE_P,B
                            000005
3477      P:000A30 P:000A32 0AF0AA            JEQ     PROCESS_WRITE_P
                            000A61
3478   
3479      P:000A32 P:000A34 0140CD            CMP     #>CMD_WRITE_X,B
                            000006
3480      P:000A34 P:000A36 0AF0AA            JEQ     PROCESS_WRITE_X
                            000A64
3481   
3482      P:000A36 P:000A38 0140CD            CMP     #>CMD_WRITE_Y,B
                            000007
3483      P:000A38 P:000A3A 0AF0AA            JEQ     PROCESS_WRITE_Y
                            000A67
3484   
3485      P:000A3A P:000A3C 0140CD            CMP     #>CMD_SET_REP_BUF,B
                            000009
3486      P:000A3C P:000A3E 0AF0AA            JEQ     PROCESS_SET_REP_BUFFER
                            000A6D
3487   
3488      P:000A3E P:000A40 0140CD            CMP     #>CMD_SET_DATA_BUF,B
                            00000A
3489      P:000A40 P:000A42 0AF0AA            JEQ     PROCESS_SET_DATA_BUFFER
                            000A7F
3490   
3491      P:000A42 P:000A44 0140CD            CMP     #>CMD_SEND_MCE,B
                            000021
3492      P:000A44 P:000A46 0AF0AA            JEQ     PROCESS_SEND_MCE
                            000AAD
3493   
3494      P:000A46 P:000A48 0140CD            CMP     #>CMD_SET_TAIL,B
                            000011
3495      P:000A48 P:000A4A 0AF0AA            JEQ     PROCESS_SET_TAIL
                            000ABD
3496   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 68



3497      
3498      P:000A4A P:000A4C 0A0005            BCLR    #COMM_CMD,X:STATUS
3499      P:000A4B P:000A4D 0A0028            BSET    #COMM_ERR,X:STATUS
3500      P:000A4C P:000A4E 00000C            RTS
3501   
3502                                PROCESS_READ_P
3503      P:000A4D P:000A4F 07E184            MOVE              P:(R1),X0
3504      P:000A4E P:000A50 0AF080            JMP     PROCESS_READ_EXIT
                            000A56
3505                                PROCESS_READ_X
3506      P:000A50 P:000A52 44E100            MOVE              X:(R1),X0
3507      P:000A51 P:000A53 0AF080            JMP     PROCESS_READ_EXIT
                            000A56
3508                                PROCESS_READ_Y
3509      P:000A53 P:000A55 4CE100            MOVE                          Y:(R1),X0
3510      P:000A54 P:000A56 0AF080            JMP     PROCESS_READ_EXIT
                            000A56
3511   
3512                                PROCESS_READ_EXIT
3513      
3514      P:000A56 P:000A58 60F400            MOVE              #>REP_RPAYLOAD,R0
                            000071
3515      P:000A58 P:000A5A 0BF080            JSR     PROCESS_SPLIT_X0_XR0
                            000AC4
3516      
3517      P:000A5A P:000A5C 44F400            MOVE              #>1,X0
                            000001
3518      P:000A5C P:000A5E 447000            MOVE              X0,X:REP_RSIZE
                            00006E
3519      
3520      P:000A5E P:000A60 0A0005            BCLR    #COMM_CMD,X:STATUS
3521      P:000A5F P:000A61 0A0024            BSET    #COMM_REP,X:STATUS
3522      P:000A60 P:000A62 00000C            RTS
3523   
3524                                PROCESS_WRITE_P
3525      P:000A61 P:000A63 076185            MOVE              X1,P:(R1)
3526      P:000A62 P:000A64 0AF080            JMP     PROCESS_WRITE_EXIT
                            000A6A
3527                                PROCESS_WRITE_X
3528      P:000A64 P:000A66 456100            MOVE              X1,X:(R1)
3529      P:000A65 P:000A67 0AF080            JMP     PROCESS_WRITE_EXIT
                            000A6A
3530                                PROCESS_WRITE_Y
3531      P:000A67 P:000A69 4D6100            MOVE                          X1,Y:(R1)
3532      P:000A68 P:000A6A 0AF080            JMP     PROCESS_WRITE_EXIT
                            000A6A
3533   
3534                                PROCESS_WRITE_EXIT
3535      
3536      P:000A6A P:000A6C 0A0005            BCLR    #COMM_CMD,X:STATUS
3537      P:000A6B P:000A6D 0A0024            BSET    #COMM_REP,X:STATUS
3538      P:000A6C P:000A6E 00000C            RTS
3539   
3540   
3541                                PROCESS_SET_REP_BUFFER
3542      
3543      
3544      P:000A6D P:000A6F 200013            CLR     A
3545      P:000A6E P:000A70 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3546      P:000A70 P:000A72 315600            MOVE              #REP_BUS_ADDR,R1
3547                                          .loop   #2
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 69



3549      P:000A73 P:000A75 44D800            MOVE              X:(R0)+,X0
3550      P:000A74 P:000A76 445900            MOVE              X0,X:(R1)+
3551      P:000A75 P:000A77 200042            OR      X0,A                              ; If there is a 1 in that address, we will f
ind it.
3552                                          .endl
3554   
3555      
3556      P:000A76 P:000A78 0A0005            BCLR    #COMM_CMD,X:STATUS
3557      P:000A77 P:000A79 0A0024            BSET    #COMM_REP,X:STATUS
3558   
3559      
3560      P:000A78 P:000A7A 014085            CMP     #0,A
3561      P:000A79 P:000A7B 0AF0AA            JEQ     PROCESS_SET_REP_BUFFER_DISABLE
                            000A7D
3562   
3563      P:000A7B P:000A7D 0A002C            BSET    #COMM_REP_ENABLED,X:STATUS
3564      P:000A7C P:000A7E 00000C            RTS
3565   
3566                                PROCESS_SET_REP_BUFFER_DISABLE
3567      P:000A7D P:000A7F 0A000C            BCLR    #COMM_REP_ENABLED,X:STATUS
3568      P:000A7E P:000A80 00000C            RTS
3569   
3570   
3571                                PROCESS_SET_DATA_BUFFER
3572      
3573      P:000A7F P:000A81 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3574      P:000A81 P:000A83 000000            NOP
3575      P:000A82 P:000A84 000000            NOP
3576      P:000A83 P:000A85 44D800            MOVE              X:(R0)+,X0              ; 0
3577      P:000A84 P:000A86 443C00            MOVE              X0,X:QT_BASE_LO
3578      P:000A85 P:000A87 44D800            MOVE              X:(R0)+,X0
3579      P:000A86 P:000A88 443D00            MOVE              X0,X:QT_BASE_HI
3580   
3581      P:000A87 P:000A89 44D800            MOVE              X:(R0)+,X0              ; 1
3582      P:000A88 P:000A8A 443F00            MOVE              X0,X:QT_BUF_MAX
3583      P:000A89 P:000A8B 44D800            MOVE              X:(R0)+,X0              ;
3584   
3585      P:000A8A P:000A8C 44D800            MOVE              X:(R0)+,X0              ; 2
3586      P:000A8B P:000A8D 443E00            MOVE              X0,X:QT_BUF_SIZE
3587      P:000A8C P:000A8E 44D800            MOVE              X:(R0)+,X0
3588   
3589      P:000A8D P:000A8F 44D800            MOVE              X:(R0)+,X0              ; 3
3590      P:000A8E P:000A90 447000            MOVE              X0,X:QT_FRAME_SIZE
                            000040
3591      P:000A90 P:000A92 44D800            MOVE              X:(R0)+,X0
3592   
3593      P:000A91 P:000A93 44D800            MOVE              X:(R0)+,X0              ; 4
3594      P:000A92 P:000A94 447000            MOVE              X0,X:QT_INFORM
                            000041
3595      P:000A94 P:000A96 44D800            MOVE              X:(R0)+,X0
3596   
3597      P:000A95 P:000A97 44D800            MOVE              X:(R0)+,X0              ; 5
3598      P:000A96 P:000A98 447000            MOVE              X0,X:TCPR0              ;  ->Right into the time-out counter
                            FFFF8D
3599      P:000A98 P:000A9A 44D800            MOVE              X:(R0)+,X0
3600   
3601      P:000A99 P:000A9B 44D800            MOVE              X:(R0)+,X0              ; 6
3602      P:000A9A P:000A9C 447000            MOVE              X0,X:QT_BUF_HEAD
                            000042
3603      P:000A9C P:000A9E 44D800            MOVE              X:(R0)+,X0
3604   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 70



3605      P:000A9D P:000A9F 44D800            MOVE              X:(R0)+,X0              ; 7
3606      P:000A9E P:000AA0 447000            MOVE              X0,X:QT_BUF_TAIL
                            000043
3607      P:000AA0 P:000AA2 44D800            MOVE              X:(R0)+,X0
3608   
3609      P:000AA1 P:000AA3 44D800            MOVE              X:(R0)+,X0              ; 8
3610      P:000AA2 P:000AA4 447000            MOVE              X0,X:QT_DROPS
                            000047
3611      P:000AA4 P:000AA6 44D800            MOVE              X:(R0)+,X0
3612   
3613      
3614      P:000AA5 P:000AA7 0D0669            JSR     BUFFER_RESET
3615   
3616      
3617      P:000AA6 P:000AA8 44F400            MOVE              #>0,X0
                            000000
3618      P:000AA8 P:000AAA 447000            MOVE              X0,X:QT_INFORM_IDX
                            000046
3619   
3620      
3621      P:000AAA P:000AAC 0A0005            BCLR    #COMM_CMD,X:STATUS
3622      P:000AAB P:000AAD 0A0024            BSET    #COMM_REP,X:STATUS
3623      P:000AAC P:000AAE 00000C            RTS
3624   
3625   
3626                                PROCESS_SEND_MCE
3627      
3628      
3629      P:000AAD P:000AAF 60F400            MOVE              #CMD_BUFFER,R0
                            000100
3630                                          .loop   #128
3632      P:000AB1 P:000AB3 54D800            MOVE              X:(R0)+,A1              ; get hi 16
3633      P:000AB2 P:000AB4 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
3634      P:000AB3 P:000AB5 0140C6            AND     #>$FF,A
                            0000FF
3635      P:000AB5 P:000AB7 547000            MOVE              A1,X:FO_SEND
                            FFF000
3636      P:000AB7 P:000AB9 557000            MOVE              B1,X:FO_SEND
                            FFF000
3637                                          .endl
3639   
3640      P:000AB9 P:000ABB 000000            NOP
3641      P:000ABA P:000ABC 0A0005            BCLR    #COMM_CMD,X:STATUS
3642      P:000ABB P:000ABD 0A0004            BCLR    #COMM_REP,X:STATUS
3643      P:000ABC P:000ABE 00000C            RTS
3644   
3645   
3646                                PROCESS_SET_TAIL
3647      
3648      P:000ABD P:000ABF 44F000            MOVE              X:CMD_BUFFER,X0
                            000100
3649      P:000ABF P:000AC1 447000            MOVE              X0,X:QT_BUF_TAIL
                            000043
3650   
3651      
3652      P:000AC1 P:000AC3 0A0005            BCLR    #COMM_CMD,X:STATUS
3653      P:000AC2 P:000AC4 0A0024            BSET    #COMM_REP,X:STATUS
3654      P:000AC3 P:000AC5 00000C            RTS
3655   
3656   
3657                                PROCESS_SPLIT_X0_XR0
3658      
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 71



3659      
3660      P:000AC4 P:000AC6 208800            MOVE              X0,A0
3661      P:000AC5 P:000AC7 0C1881            EXTRACTU #$010000,A,B
                            010000
3662      P:000AC7 P:000AC9 0C1880            EXTRACTU #$008010,A,A                     ; Put
                            008010
3663      P:000AC9 P:000ACB 515800            MOVE              B0,X:(R0)+
3664      P:000ACA P:000ACC 505800            MOVE              A0,X:(R0)+
3665      P:000ACB P:000ACD 00000C            RTS
3666   
3667                                PROCESS_SPLIT_X0_YR0
3668      
3669      
3670      P:000ACC P:000ACE 208800            MOVE              X0,A0
3671      P:000ACD P:000ACF 0C1881            EXTRACTU #$010000,A,B
                            010000
3672      P:000ACF P:000AD1 0C1880            EXTRACTU #$008010,A,A                     ; Put
                            008010
3673      P:000AD1 P:000AD3 595800            MOVE                          B0,Y:(R0)+
3674      P:000AD2 P:000AD4 585800            MOVE                          A0,Y:(R0)+
3675      P:000AD3 P:000AD5 00000C            RTS
3676   
3677                                PROCESS_JOIN_XR0_A
3678      
3679      
3680      P:000AD4 P:000AD6 200013            CLR     A
3681      P:000AD5 P:000AD7 50D800            MOVE              X:(R0)+,A0
3682      P:000AD6 P:000AD8 44D800            MOVE              X:(R0)+,X0
3683      P:000AD7 P:000AD9 0C1940            INSERT  #$010010,X0,A
                            010010
3684      P:000AD9 P:000ADB 00000C            RTS
3685   
3686   
3687   
3688                                ;------------------------
3689                                CHECK_FOR_DATA
3690                                ;------------------------
3691      P:000ADA P:000ADC 01AD80            JCLR    #EF,X:PDRD,CHECK_FOR_DATA_EXIT
                            000B6D
3692      P:000ADC P:000ADE 000000            NOP
3693      P:000ADD P:000ADF 000000            NOP
3694      P:000ADE P:000AE0 01AD80            JCLR    #EF,X:PDRD,CHECK_FOR_DATA_EXIT
                            000B6D
3695      
3696   
3697      
3698      
3699      
3700   
3701      P:000AE0 P:000AE2 200013            CLR     A                                 ; A0=0
3702      P:000AE1 P:000AE3 64F400            MOVE              #>MCEREP_BUF,R4
                            000000
3703   
3704      P:000AE3 P:000AE5 094C3F            MOVEP             Y:RDFIFO,A1
3705      P:000AE4 P:000AE6 0140C6            AND     #>$00FFFF,A
                            00FFFF
3706      P:000AE6 P:000AE8 5C5C00            MOVE                          A1,Y:(R4)+
3707      P:000AE7 P:000AE9 0140C5            CMP     #>$00A5A5,A
                            00A5A5
3708      P:000AE9 P:000AEB 0AF0A2            JNE     RESET_FIFO                        ; Empty the FIFO, and return to main loop.
                            000B6E
3709   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 72



3710      P:000AEB P:000AED 01AD80            JCLR    #EF,X:PDRD,*
                            000AEB
3711      P:000AED P:000AEF 000000            NOP
3712      P:000AEE P:000AF0 000000            NOP
3713      P:000AEF P:000AF1 01AD80            JCLR    #EF,X:PDRD,*
                            000AEF
3714   
3715      P:000AF1 P:000AF3 094C3F            MOVEP             Y:RDFIFO,A1
3716      P:000AF2 P:000AF4 0140C6            AND     #>$00FFFF,A
                            00FFFF
3717      P:000AF4 P:000AF6 5C5C00            MOVE                          A1,Y:(R4)+
3718      P:000AF5 P:000AF7 0140C5            CMP     #>$00A5A5,A
                            00A5A5
3719      P:000AF7 P:000AF9 0AF0A2            JNE     FIFO_RESYNC                       ; Sure, give simple resync a chance
                            000B03
3720   
3721      P:000AF9 P:000AFB 01AD80            JCLR    #EF,X:PDRD,*
                            000AF9
3722      P:000AFB P:000AFD 000000            NOP
3723      P:000AFC P:000AFE 000000            NOP
3724      P:000AFD P:000AFF 01AD80            JCLR    #EF,X:PDRD,*
                            000AFD
3725   
3726      P:000AFF P:000B01 094C3F            MOVEP             Y:RDFIFO,A1
3727      P:000B00 P:000B02 0140C6            AND     #>$00FFFF,A
                            00FFFF
3728      P:000B02 P:000B04 5C5C00            MOVE                          A1,Y:(R4)+
3729                                FIFO_RESYNC
3730      P:000B03 P:000B05 0140C5            CMP     #>$005A5A,A
                            005A5A
3731      P:000B05 P:000B07 0AF0A2            JNE     RESET_FIFO
                            000B6E
3732   
3733      P:000B07 P:000B09 01AD80            JCLR    #EF,X:PDRD,*
                            000B07
3734      P:000B09 P:000B0B 000000            NOP
3735      P:000B0A P:000B0C 000000            NOP
3736      P:000B0B P:000B0D 01AD80            JCLR    #EF,X:PDRD,*
                            000B0B
3737   
3738      P:000B0D P:000B0F 094C3F            MOVEP             Y:RDFIFO,A1
3739      P:000B0E P:000B10 0140C6            AND     #>$00FFFF,A
                            00FFFF
3740      P:000B10 P:000B12 5C5C00            MOVE                          A1,Y:(R4)+
3741      P:000B11 P:000B13 0140C5            CMP     #>$005A5A,A
                            005A5A
3742      P:000B13 P:000B15 0AF0A2            JNE     RESET_FIFO
                            000B6E
3743   
3744      
3745                                          .loop   #4
3747      P:000B17 P:000B19 01AD80            JCLR    #EF,X:PDRD,*
                            000B17
3748      P:000B19 P:000B1B 000000            NOP
3749      P:000B1A P:000B1C 000000            NOP
3750      P:000B1B P:000B1D 01AD80            JCLR    #EF,X:PDRD,*
                            000B1B
3751      P:000B1D P:000B1F 094C3F            MOVEP             Y:RDFIFO,A1
3752      P:000B1E P:000B20 0140C6            AND     #>$00ffff,A
                            00FFFF
3753      P:000B20 P:000B22 000000            NOP
3754      P:000B21 P:000B23 5C5C00            MOVE                          A1,Y:(R4)+
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 73



3755                                          .endl
3757   
3758      
3759      
3760      P:000B22 P:000B24 200013            CLR     A
3761      P:000B23 P:000B25 588600            MOVE                          Y:(MCEREP_BUF+MCEREP_SIZE),A0
3762      P:000B24 P:000B26 0D05BE            JSR     PACKET_PARTITIONS
3763   
3764      
3765      P:000B25 P:000B27 200013            CLR     A
3766      P:000B26 P:000B28 5C8400            MOVE                          Y:(MCEREP_BUF+MCEREP_TYPE),A1
3767      P:000B27 P:000B29 0140C5            CMP     #'RP',A
                            005250
3768      P:000B29 P:000B2B 0AF0AA            JEQ     CHECK_FOR_DATA__BUFFER_REPLY
                            000B39
3769   
3770      P:000B2B P:000B2D 0140C5            CMP     #'DA',A
                            004441
3771      P:000B2D P:000B2F 0AF0AA            JEQ     CHECK_FOR_DATA__BUFFER_DATA
                            000B46
3772   
3773      
3774      P:000B2F P:000B31 50F000            MOVE              X:PTYPE_FAILS,A0
                            000052
3775      P:000B31 P:000B33 000008            INC     A
3776      P:000B32 P:000B34 000000            NOP
3777      P:000B33 P:000B35 507000            MOVE              A0,X:PTYPE_FAILS
                            000052
3778      P:000B35 P:000B37 0BF080            JSR     RESET_FIFO
                            000B6E
3779      P:000B37 P:000B39 0AF080            JMP     CHECK_FOR_DATA_EXIT
                            000B6D
3780   
3781   
3782                                CHECK_FOR_DATA__BUFFER_REPLY
3783      
3784      P:000B39 P:000B3B 64F400            MOVE              #$8000,R4
                            008000
3785   
3786      
3787      P:000B3B P:000B3D 0A00A7            JSET    #COMM_MCEDATA,X:STATUS,CHECK_FOR_DATA__BUFFER_REPLY1
                            000B41
3788      P:000B3D P:000B3F 0A00A7            JSET    #COMM_MCEDATA,X:STATUS,CHECK_FOR_DATA__BUFFER_REPLY1
                            000B41
3789   
3790      
3791      P:000B3F P:000B41 0A0026            BSET    #COMM_MCEREP,X:STATUS
3792      P:000B40 P:000B42 340800            MOVE              #(MCEREP_BUF+MCEREP_PAYLOAD),R4
3793                                CHECK_FOR_DATA__BUFFER_REPLY1
3794      P:000B41 P:000B43 4C8600            MOVE                          Y:(MCEREP_BUF+MCEREP_SIZE),X0
3795      P:000B42 P:000B44 0BF080            JSR     CHECK_FOR_DATA__BUFFER_LARGE
                            000B96
3796   
3797      P:000B44 P:000B46 0AF080            JMP     CHECK_FOR_DATA_EXIT
                            000B6D
3798   
3799                                CHECK_FOR_DATA__BUFFER_DATA
3800      
3801      P:000B46 P:000B48 64F400            MOVE              #$8000,R4
                            008000
3802   
3803      
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 74



3804      P:000B48 P:000B4A 0A00A7            JSET    #COMM_MCEDATA,X:STATUS,CHECK_FOR_DATA__BUFFER_DATA1
                            000B4E
3805      P:000B4A P:000B4C 0A00A7            JSET    #COMM_MCEDATA,X:STATUS,CHECK_FOR_DATA__BUFFER_DATA1
                            000B4E
3806   
3807      
3808      P:000B4C P:000B4E 0A0027            BSET    #COMM_MCEDATA,X:STATUS
3809      P:000B4D P:000B4F 340800            MOVE              #MCEDATA_BUF,R4
3810                                CHECK_FOR_DATA__BUFFER_DATA1
3811   
3812      
3813      P:000B4E P:000B50 50F000            MOVE              X:DA_COUNT,A0
                            000053
3814      P:000B50 P:000B52 000008            INC     A
3815      P:000B51 P:000B53 000000            NOP
3816      P:000B52 P:000B54 507000            MOVE              A0,X:DA_COUNT
                            000053
3817   
3818      
3819      
3820   
3821      
3822      P:000B54 P:000B56 0A0027            BSET    #COMM_MCEDATA,X:STATUS
3823      
3824      
3825      
3826   
3827                                CHECK_FOR_DATA__BUFFER_DATA2
3828      
3829      P:000B55 P:000B57 4C8600            MOVE                          Y:(MCEREP_BUF+MCEREP_SIZE),X0
3830   
3831      P:000B56 P:000B58 0BF080            JSR     CHECK_FOR_DATA__BUFFER_LARGE
                            000B96
3832      
3833      P:000B58 P:000B5A 44F400            MOVE              #$ff1112,X0
                            FF1112
3834      P:000B5A P:000B5C 4C6400            MOVE                          X0,Y:(R4)
3835   
3836      P:000B5B P:000B5D 0AF080            JMP     CHECK_FOR_DATA_EXIT
                            000B6D
3837   
3839                                CHECK_FOR_DATA__BUFFER
3840      
3841      
3842      
3843      
3844      
3845      
3846   
3847                                          .loop   #2
3849                                          .loop   X0
3851      P:000B61 P:000B63 01AD80            JCLR    #EF,X:PDRD,*
                            000B61
3852      P:000B63 P:000B65 094E3F            MOVEP             Y:RDFIFO,A
3853      P:000B64 P:000B66 0140C6            AND     #>$00ffff,A
                            00FFFF
3854      P:000B66 P:000B68 000000            NOP
3855      P:000B67 P:000B69 5C5C00            MOVE                          A1,Y:(R4)+
3856                                          .endl
3858      P:000B68 P:000B6A 000000            NOP
3859                                          .endl
3861   
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 75



3862      
3863      P:000B69 P:000B6B 44F400            MOVE              #$ff1111,X0
                            FF1111
3864      P:000B6B P:000B6D 4C6400            MOVE                          X0,Y:(R4)
3865   
3866      P:000B6C P:000B6E 00000C            RTS
3867   
3868                                CHECK_FOR_DATA_EXIT
3869      P:000B6D P:000B6F 00000C            RTS
3870   
3871                                ;----------------------------------------------
3872                                RESET_FIFO
3873                                ;----------------------------------------------
3874      
3875      P:000B6E P:000B70 63F000            MOVE              X:DEBUG_BUF_IDX,R3
                            00005C
3876      P:000B70 P:000B72 000000            NOP
3877      P:000B71 P:000B73 000000            NOP
3878      P:000B72 P:000B74 585B00            MOVE                          A0,Y:(R3)+
3879      P:000B73 P:000B75 5C5B00            MOVE                          A1,Y:(R3)+
3880      P:000B74 P:000B76 0140C5            CMP     #>$00A5A5,A
                            00A5A5
3881      P:000B76 P:000B78 0AF0AA            JEQ     RESET_FIFO1
                            000B79
3882      P:000B78 P:000B7A 6B5B00            MOVE                          R3,Y:(R3)+
3883                                RESET_FIFO1
3884      P:000B79 P:000B7B 000000            NOP
3885      P:000B7A P:000B7C 000000            NOP
3886      P:000B7B P:000B7D 5EF000            MOVE                          Y:RDFIFO,A
                            FFFFFF
3887      P:000B7D P:000B7F 5C5B00            MOVE                          A1,Y:(R3)+
3888      P:000B7E P:000B80 01ADA0            JSET    #EF,X:PDRD,RESET_FIFO1
                            000B79
3889   
3890      P:000B80 P:000B82 56F400            MOVE              #>$aa1122,A
                            AA1122
3891      P:000B82 P:000B84 000000            NOP
3892      P:000B83 P:000B85 5C5B00            MOVE                          A1,Y:(R3)+
3893      P:000B84 P:000B86 637000            MOVE              R3,X:DEBUG_BUF_IDX
                            00005C
3894   
3895      
3896      P:000B86 P:000B88 50F000            MOVE              X:FIFO_FAILS,A0
                            000051
3897      P:000B88 P:000B8A 000008            INC     A
3898      P:000B89 P:000B8B 000000            NOP
3899      P:000B8A P:000B8C 507000            MOVE              A0,X:FIFO_FAILS
                            000051
3900      P:000B8C P:000B8E 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* briefly.
                            000018
3901      P:000B8E P:000B90 44F400            MOVE              #>25000,X0
                            0061A8
3902                                          .loop   X0
3904      P:000B92 P:000B94 000000            NOP
3905                                          .endl
3907      P:000B93 P:000B95 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
3908      P:000B95 P:000B97 00000C            RTS
3909   
3913   
3914                                ;---------------------------
3915                                CHECK_FOR_DATA__BUFFER_LARGE
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 76



3916                                ;---------------------------
3917      
3918      P:000B96 P:000B98 56A400            MOVE              X:TOTAL_BUFFS,A
3919      P:000B97 P:000B99 014085            CMP     #0,A
3920      P:000B98 P:000B9A 0AF0AA            JEQ     FINISHED_BUFFS
                            000BA2
3921   
3922      P:000B9A P:000B9C 06CC00            DO      A1,FINISHED_BUFFS
                            000BA1
3923      
3925      P:000B9C P:000B9E 01ADA1            JSET    #HF,X:PDRD,*
                            000B9C
3926      
3927      
3928      
3929      
3930                                          .loop   #512
3932      P:000BA0 P:000BA2 095CFF            MOVEP             Y:RDFIFO,Y:(R4)+
3933                                          .endl
3935      P:000BA1 P:000BA3 000000            NOP
3936   
3937                                FINISHED_BUFFS
3938      
3939   
3940      
3941      
3942      P:000BA2 P:000BA4 01ADA1            JSET    #HF,X:PDRD,BUFFER_PACKET_SINGLES_TIMED
                            000BA8
3943   
3944      
3945                                          .loop   X:LEFT_TO_READ
3947      P:000BA6 P:000BA8 095CFF            MOVEP             Y:RDFIFO,Y:(R4)+
3948                                          .endl
3950      P:000BA7 P:000BA9 00000C            RTS
3951   
3952                                BUFFER_PACKET_SINGLES_TIMED
3953      
3954      P:000BA8 P:000BAA 200013            CLR     A
3955      P:000BA9 P:000BAB 20001B            CLR     B
3956      P:000BAA P:000BAC 51F000            MOVE              X:TCR0,B0               ; Store timer value (50 MHz)
                            FFFF8C
3957      P:000BAC P:000BAE 0C1C85            ASR     #2,B,B                            ; / 4
3958                                          .loop   X:LEFT_TO_READ
3960                                BUFFER_PACKET_SINGLES_WAIT_X
3961      P:000BAF P:000BB1 50F000            MOVE              X:TCR0,A0
                            FFFF8C
3962      P:000BB1 P:000BB3 0C1C04            ASR     #2,A,A
3963      P:000BB2 P:000BB4 20000D            CMP     A,B
3964      P:000BB3 P:000BB5 0EABAF            JEQ     BUFFER_PACKET_SINGLES_WAIT_X
3965      P:000BB4 P:000BB6 095CFF            MOVEP             Y:RDFIFO,Y:(R4)+
3966      P:000BB5 P:000BB7 0C1D01            ASL     #0,A,B                            ; MOVE A,B
3967                                          .endl
3969      P:000BB6 P:000BB8 000000            NOP
3970      P:000BB7 P:000BB9 000000            NOP
3971      P:000BB8 P:000BBA 00000C            RTS
3972   
3973                                BUFFER_PACKET_SINGLES_POLLED
3974                                          .loop   X:LEFT_TO_READ
3976      P:000BBB P:000BBD 01AD80            JCLR    #EF,X:PDRD,*
                            000BBB
3977      P:000BBD P:000BBF 095CFF            MOVEP             Y:RDFIFO,Y:(R4)+
3978                                          .endl
Motorola DSP56300 Assembler  Version 6.3.4   13-06-02  13:53:11  hacking.asm  Page 77



3980      P:000BBE P:000BC0 000000            NOP
3981      P:000BBF P:000BC1 000000            NOP
3982      P:000BC0 P:000BC2 00000C            RTS
3983   
3984   
3985      000BC3                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM

0    Errors
3    Warnings


