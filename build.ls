Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  build.asm  Page 1



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
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  header.asm  Page 2



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
75     
76     
77                         ; HST timeout recovery....
78     
79        000200           MAX_DUMP  EQU     512                               ; if HST timeout.. max number that could be in FIFO i
s 511..
80        001000           DUMP_BUFF EQU     $1000                             ; store in Y memory above normal data buffer: in off-
chip RAM
81     
82     
83     
84                         ; Various addressing control registers
85        FFFFFB           BCR       EQU     $FFFFFB                           ; Bus Control Register
86        FFFFFA           DCR       EQU     $FFFFFA                           ; DRAM Control Register
87        FFFFF9           AAR0      EQU     $FFFFF9                           ; Address Attribute Register, channel 0
88        FFFFF8           AAR1      EQU     $FFFFF8                           ; Address Attribute Register, channel 1
89        FFFFF7           AAR2      EQU     $FFFFF7                           ; Address Attribute Register, channel 2
90        FFFFF6           AAR3      EQU     $FFFFF6                           ; Address Attribute Register, channel 3
91        FFFFFD           PCTL      EQU     $FFFFFD                           ; PLL control register
92        FFFFFE           IPRP      EQU     $FFFFFE                           ; Interrupt Priority register - Peripheral
93        FFFFFF           IPRC      EQU     $FFFFFF                           ; Interrupt Priority register - Core
94     
95                         ; PCI control register
96        FFFFCD           DTXS      EQU     $FFFFCD                           ; DSP Slave transmit data FIFO
97        FFFFCC           DTXM      EQU     $FFFFCC                           ; DSP Master transmit data FIFO
98        FFFFCB           DRXR      EQU     $FFFFCB                           ; DSP Receive data FIFO
99        FFFFCA           DPSR      EQU     $FFFFCA                           ; DSP PCI Status Register
100       FFFFC9           DSR       EQU     $FFFFC9                           ; DSP Status Register
101       FFFFC8           DPAR      EQU     $FFFFC8                           ; DSP PCI Address Register
102       FFFFC7           DPMC      EQU     $FFFFC7                           ; DSP PCI Master Control Register
103       FFFFC6           DPCR      EQU     $FFFFC6                           ; DSP PCI Control Register
104       FFFFC5           DCTR      EQU     $FFFFC5                           ; DSP Control Register
105    
106                        ; Port E is the Synchronous Communications Interface (SCI) port
107       FFFF9F           PCRE      EQU     $FFFF9F                           ; Port Control Register
108       FFFF9E           PRRE      EQU     $FFFF9E                           ; Port Direction Register
109       FFFF9D           PDRE      EQU     $FFFF9D                           ; Port Data Register
110    
111                        ; Various PCI register bit equates
112       000001           STRQ      EQU     1                                 ; Slave transmit data request (DSR)
113       000002           SRRQ      EQU     2                                 ; Slave receive data request (DSR)
114       000017           HACT      EQU     23                                ; Host active, low true (DSR)
115       000001           MTRQ      EQU     1                                 ; Set whem master transmitter is not full (DPSR)
116       000004           MARQ      EQU     4                                 ; Master address request (DPSR)
117       000002           MRRQ      EQU     2                                 ; Master Receive Request (DPSR)
118       00000A           TRTY      EQU     10                                ; PCI Target Retry (DPSR)
119    
120       000005           APER      EQU     5                                 ; Address parity error
121       000006           DPER      EQU     6                                 ; Data parity error
122       000007           MAB       EQU     7                                 ; Master Abort
123       000008           TAB       EQU     8                                 ; Target Abort
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  header.asm  Page 3



124       000009           TDIS      EQU     9                                 ; Target Disconnect
125       00000B           TO        EQU     11                                ; Timeout
126       00000E           MDT       EQU     14                                ; Master Data Transfer complete
127       00000F           RDCQ      EQU     15                                ; Remaining Data Count Qualifier
128    
129       000002           SCLK      EQU     2                                 ; SCLK = transmitter special code
130    
131                        ; bits in DPMC
132    
133       000017           FC1       EQU     23
134       000016           FC0       EQU     22
135    
136    
137                        ; DMA register definitions
138       FFFFEF           DSR0      EQU     $FFFFEF                           ; Source address register
139       FFFFEE           DDR0      EQU     $FFFFEE                           ; Destination address register
140       FFFFED           DCO0      EQU     $FFFFED                           ; Counter register
141       FFFFEC           DCR0      EQU     $FFFFEC                           ; Control register
142    
143                        ; The DCTR host flags are written by the DSP and read by PCI host
144       000003           DCTR_HF3  EQU     3                                 ; used as a semiphore for INTA handshaking
145       000004           DCTR_HF4  EQU     4                                 ;
146       000005           DCTR_HF5  EQU     5                                 ;
147       000006           INTA      EQU     6                                 ; Request PCI interrupt
148    
149                        ; The DSR host flags are written by the PCI host and read by the DSP
150       000004           DSR_BUF0  EQU     4                                 ; PCI host sets this when copying buffer 0
151       000005           DSR_BUF1  EQU     5                                 ; PCI host sets this when copying buffer 1
152    
153                        ; DPCR bit definitions
154       00000E           CLRT      EQU     14                                ; Clear transmitter
155       000012           MACE      EQU     18                                ; Master access counter enable
156       000015           IAE       EQU     21                                ; Insert Address Enable
157    
158                        ; Addresses of ESSI port
159       FFFFBC           TX00      EQU     $FFFFBC                           ; Transmit Data Register 0
160       FFFFB7           SSISR0    EQU     $FFFFB7                           ; Status Register
161       FFFFB6           CRB0      EQU     $FFFFB6                           ; Control Register B
162       FFFFB5           CRA0      EQU     $FFFFB5                           ; Control Register A
163    
164                        ; SSI Control Register A Bit Flags
165       000006           TDE       EQU     6                                 ; Set when transmitter data register is empty
166    
167                        ; Miscellaneous addresses
168       FFFFFF           RDFIFO    EQU     $FFFFFF                           ; Read the FIFO for incoming fiber optic data
169    
170                        ; Timer registers
171       FFFF8F           TCSR0     EQU     $FFFF8F                           ; Control and status register
172       FFFF8E           TLR0      EQU     $FFFF8E                           ; Load register
173       FFFF8D           TCPR0     EQU     $FFFF8D                           ; Compare register
174       FFFF8C           TCR0      EQU     $FFFF8C                           ; Count register
175       FFFF8B           TCSR1     EQU     $FFFF8B                           ; Control and status register
176       FFFF8A           TLR1      EQU     $FFFF8A                           ; Load register
177       FFFF89           TCPR1     EQU     $FFFF89                           ; Compare register
178       FFFF88           TCR1      EQU     $FFFF88                           ; Count register
179       FFFF87           TCSR2     EQU     $FFFF87                           ; Control and status register
180       FFFF86           TLR2      EQU     $FFFF86                           ; Load register
181       FFFF85           TCPR2     EQU     $FFFF85                           ; Compare register
182       FFFF84           TCR2      EQU     $FFFF84                           ; Count register
183    
184                        ;***************************************************************
185                        ; Phase Locked Loop initialization
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  header.asm  Page 4



186       050003           PLL_INIT  EQU     $050003                           ; PLL = 25 MHz x 4 = 100 MHz
187                        ;****************************************************************
188    
189                        ; Port C is Enhanced Synchronous Serial Port 0
190       FFFFBF           PCRC      EQU     $FFFFBF                           ; Port C Control Register
191       FFFFBE           PRRC      EQU     $FFFFBE                           ; Port C Data direction Register
192       FFFFBD           PDRC      EQU     $FFFFBD                           ; Port C GPIO Data Register
193    
194                        ; Port D is Enhanced Synchronous Serial Port 1
195       FFFFAF           PCRD      EQU     $FFFFAF                           ; Port D Control Register
196       FFFFAE           PRRD      EQU     $FFFFAE                           ; Port D Data direction Register
197       FFFFAD           PDRD      EQU     $FFFFAD                           ; Port D GPIO Data Register
198    
199                        ; Bit number definitions of GPIO pins on Port C
200       000002           ROM_FIFO  EQU     2                                 ; Select ROM or FIFO accesses for AA1
201       000004           AUX1      EQU     4                                 ; enable/disable byte swapping
202    
203                        ; Bit number definitions of GPIO pins on Port D
204       000000           EF        EQU     0                                 ; FIFO Empty flag, low true
205       000001           HF        EQU     1                                 ; FIFO half full flag, low true
206       000002           RS        EQU     2                                 ; FIFO reset signal, low true
207       000003           FSYNC     EQU     3                                 ; High during image transmission
208       000005           WRFIFO    EQU     5                                 ; Low true if FIFO is being written to
209    
210    
211                        ; Errors - self test application
212    
213       000000           Y_MEM_ER  EQU     0                                 ; y memory corrupted
214       000001           X_MEM_ER  EQU     1                                 ; x memory corrupted
215       000002           P_MEM_ER  EQU     2                                 ; p memory corrupted
216       000003           FO_EMPTY  EQU     3                                 ; no transmitted data in FIFO
217    
218       000004           FO_OVER   EQU     4                                 ; too much data received
219       000005           FO_UNDER  EQU     5                                 ; not enough data receiv
220       000006           FO_RX_ER  EQU     6                                 ; received data in FIFO incorrect.
221       000007           DEBUG     EQU     7                                 ; debug bit
222    
223    
225       000000           TE        EQU     0
226       000001           TOIE      EQU     1
227       000002           TCIE      EQU     2
228       000014           TOF       EQU     20
229       000015           TCF       EQU     21
230    
231    
233    
234       FFF000           FO_SEND   EQU     $FFF000
235                                  INCLUDE 'init.asm'
236                                COMMENT *
237    
238                        Initial configuration, and ISR vector definitions.
239    
240                        See info.asm for versioning and authors.
241    
242                                *
243                                  PAGE    132                               ; Printronix page width - 132 columns
244                                  OPT     CEX                               ; print DC evaluations
245    
246                        ; The EEPROM boot code expects first to read 3 bytes specifying the number of
247                        ; program words, then 3 bytes specifying the address to start loading the
248                        ; program words and then 3 bytes for each program word to be loaded.
249                        ; The program words will be condensed into 24 bit words and stored in contiguous
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  init.asm  Page 5



250                        ; PRAM memory starting at the specified starting address. Program execution
251                        ; starts from the same address where loading started.
252    
253                        ; Special address for two words for the DSP to bootstrap code from the EEPROM
254                                  IF      @SCP("ROM","ROM")                 ; Boot from ROM on power-on
255       P:000000 P:000000                   ORG     P:0,P:0
256  d    P:000000 P:000000 000810            DC      END_ADR-INIT-2                    ; Number of boot words
257  d    P:000001 P:000001 000000            DC      INIT                              ; Starting address
258       P:000000 P:000002                   ORG     P:0,P:2
259       P:000000 P:000002 0C0030  INIT      JMP     <INIT_PCI                         ; Configure PCI port
260       P:000001 P:000003 000000            NOP
261                                           ENDIF
262    
263    
264                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
265                                 ; command converter
266                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
270                                           ENDIF
271    
272                                 ; Vectored interrupt table, addresses at the beginning are reserved
273  d    P:000002 P:000004 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0       ; $02-$0f Reserved
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
274  d    P:000010 P:000012 000000            DC      0,0                               ; $10-$13 Reserved
     d                      000000
275    
276                                 ; FIFO HF* flag interrupt vector is here at $12 - this is connected to the
277                                 ; IRQB* interrupt line so its ISR vector must be here
278  d    P:000012 P:000014 000000            DC      0,0                               ; $was ld scatter routine ...HF*
     d                      000000
279    
280                                 ; a software reset button on the font panel of the card is connected to the IRQC*
281                                 ; line which if pressed causes the DSP to jump to an ISR which causes the program
282                                 ; counter to the beginning of the program INIT and sets the stack pointer to TOP.
283       P:000014 P:000016 0BF080            JSR     CLEAN_UP_PCI                      ; $14 - Software reset switch
                            000466
284    
285  d    P:000016 P:000018 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Reserved interrupts
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
286  d    P:000022 P:000024 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0
     d                      000000
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  init.asm  Page 6



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
287    
288                                 ; Now we're at P:$30, where some unused vector addresses are located
289                                 ; This is ROM only code that is only executed once on power-up when the
290                                 ; ROM code is downloaded. It is skipped over on OnCE downloads.
291    
292                                 ; A few seconds after power up on the Host, it interrogates the PCI bus to find
293                                 ; out what boards are installed and configures this PCI board. The EEPROM booting
294                                 ; procedure ends with program execution  starting at P:$0 where the EEPROM has
295                                 ; inserted a JMP INIT_PCI instruction. This routine sets the PLL paramter and
296                                 ; does a self configuration and software reset of the PCI controller in the DSP.
297                                 ; After configuring the PCI controller the DSP program overwrites the instruction
298                                 ; at P:$0 with a new JMP START to skip over the INIT_PCI routine. The program at
299                                 ; START address begins configuring the DSP and processing commands.
300                                 ; Similarly the ONCE option places a JMP START at P:$0 to skip over the
301                                 ; INIT_PCI routine. If this routine where executed after the host computer had booted
302                                 ; it would cause it to crash since the host computer would overwrite the
303                                 ; configuration space with its own values and doesn't tolerate foreign values.
304    
305                                 ; Initialize the PLL - phase locked loop
306                                 INIT_PCI
307       P:000030 P:000032 08F4BD            MOVEP             #PLL_INIT,X:PCTL        ; Initialize PLL
                            050003
308       P:000032 P:000034 000000            NOP
309    
310                                 ; Program the PCI self-configuration registers
311       P:000033 P:000035 240000            MOVE              #0,X0
312       P:000034 P:000036 08F485            MOVEP             #$500000,X:DCTR         ; Set self-configuration mode
                            500000
313       P:000036 P:000038 0604A0            REP     #4
314       P:000037 P:000039 08C408            MOVEP             X0,X:DPAR               ; Dummy writes to configuration space
315       P:000038 P:00003A 08F487            MOVEP             #>$0000,X:DPMC          ; Subsystem ID
                            000000
316       P:00003A P:00003C 08F488            MOVEP             #>$0000,X:DPAR          ; Subsystem Vendor ID
                            000000
317    
318                                 ; PCI Personal reset
319       P:00003C P:00003E 08C405            MOVEP             X0,X:DCTR               ; Personal software reset
320       P:00003D P:00003F 000000            NOP
321       P:00003E P:000040 000000            NOP
322       P:00003F P:000041 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00003F
323       P:000041 P:000043 07F084            MOVE              P:(*+3),X0              ; Trick to write "JMP <START" to P:0
                            000044
324       P:000043 P:000045 070004            MOVE              X0,P:(0)
325       P:000044 P:000046 0C0100            JMP     <START
326    
327  d    P:000045 P:000047 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
     d                      000000
     d                      000000
     d                      000000
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  init.asm  Page 7



     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
328  d    P:000051 P:000053 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
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
329  d    P:00005D P:00005F 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; $60-$71 Reserved PCI
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
330    
331                                 ;**************************************************************************
332                                 ; Check for program space overwriting of ISR starting at P:$72
333                                           IF      @CVS(N,*)>$71
335                                           ENDIF
336    
337                                 ;       ORG     P:$72,P:$72
338       P:000072 P:000074                   ORG     P:$72,P:$74
339    
340                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
341                                 ; command converter
342                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
344                                           ENDIF
345    
346    
347                                 ;**************************************************************************
348    
349                                 ; Three non-maskable fast interrupt service routines for clearing PCI interrupts
350                                 ; The Host will use these to clear the INTA* after it has serviced the interrupt
351                                 ; which had been generated by the PCI board.
352    
353       P:000072 P:000074 0A8506            BCLR    #INTA,X:DCTR                      ; $72/3 - Clear PCI interrupt
354       P:000073 P:000075 000000            NOP
355    
356       P:000074 P:000076 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; clear interrupt flag
357       P:000075 P:000077 000000            NOP                                       ; needs to be fast addressing <
358    
359       P:000076 P:000078 0A0022            BSET    #FATAL_ERROR,X:<STATUS            ; $76/7 - driver PCI_MESSAGE_TO_HOST error
360       P:000077 P:000079 000000            NOP
361    
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  init.asm  Page 8



362                                 ; Interrupt locations for 7 available commands on PCI board
363                                 ; Each JSR takes up 2 locations in the table
364       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            000383
365       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            000358
366       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            0003A4
367       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            0003AD
368                                 ; software reset is the same as cleaning up the PCI - use same routine
369                                 ; when HOST does a RESET then this routine is run
370       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            00047D
371       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            000496
372       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            00046E
373       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            0003B8
374    
375                                 ; QT - set command
376       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            0003D6
377       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            00045E
378    
379                                 ; Quiet RP mode, clear buffer full flag
380       P:00008C P:00008E 0A0015            BCLR    #RP_BUFFER_FULL,X:<STATUS         ; $8C
381       P:00008D P:00008F 000000            NOP
382    
383                                 ; Disable PCI interrupts
384       P:00008E P:000090 0A0104            BCLR    #MODE_IRQ,X:<MODE                 ; $8E
385       P:00008F P:000091 000000            NOP
386    
387                                 ; Enable PCI interrupts
388       P:000090 P:000092 0A0124            BSET    #MODE_IRQ,X:<MODE                 ; $90
389       P:000091 P:000093 000000            NOP
390    
391                                 ; ***********************************************************************
392                                 ; For now have boot code starting from P:$100
393                                 ; just to make debugging tidier etc.
394    
395       P:000100 P:000102                   ORG     P:$100,P:$102
396    
397                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
398                                 ; command converter
399                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
401                                           ENDIF
402                                 ; ***********************************************************************
403    
404    
405    
406                                 ; ******************************************************************
407                                 ;
408                                 ;       AA0 = RDFIFO* of incoming fiber optic data
409                                 ;       AA1 = EEPROM access
410                                 ;       AA2 = DRAM access
411                                 ;       AA3 = output to parallel data connector, for a video pixel clock
412                                 ;       $FFxxxx = Write to fiber optic transmitter
413                                 ;
414                                 ; ******************************************************************
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  init.asm  Page 9



415    
416    
417       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
418       P:000102 P:000104 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
419       P:000103 P:000105 0A8515            BCLR    #21,X:DCTR
420       P:000104 P:000106 0A8516            BCLR    #22,X:DCTR
421       P:000105 P:000107 000000            NOP
422       P:000106 P:000108 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
423       P:000107 P:000109 000000            NOP
424       P:000108 P:00010A 000000            NOP                                       ; End of PCI programming
425    
426    
427                                 ; Set operation mode register OMR to normal expanded
428       P:000109 P:00010B 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
429       P:00010A P:00010C 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
430    
431                                 ; Program the serial port ESSI0 = Port C for serial transmission to
432                                 ;   the timing board
433       P:00010B P:00010D 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
434                                 ;**********************************************************************
435       P:00010D P:00010F 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
436                                                                                     ; DC0-CD4 = 0 for non-network operation
437                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
438                                                                                     ; SSC1 = 0 for SC1 not used
439                                 ;************************************************************************
440       P:00010F P:000111 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
                            010120
441                                                                                     ; SHFD = 0 for MSB shifted first
442                                                                                     ; CKP = 0 for rising clock edge transitions
443                                                                                     ; TE0 = 1 to enable transmitter #0
444                                                                                     ; MOD = 0 for normal, non-networked mode
445                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
446       P:000111 P:000113 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
447                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
448                                 ;********************************************************************************
449       P:000113 P:000115 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
450       P:000115 P:000117 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
                            000000
451                                 ;***********************************************************************************
452                                 ; 250MHz
453                                 ; Conversion from software bits to schematic labels for Port C and D
454                                 ;       PC0 = SC00 = AUX3               PD0 = SC10 = EF*
455                                 ;       PC1 = SC01 = A/B* = input       PD1 = SC11 = HF*
456                                 ;       PC2 = SC02 = No connect         PD2 = SC12 = RS*
457                                 ;       PC3 = SCK0 = No connect         PD3 = SCK1 = NWRFIFO*
458                                 ;       PC4 = SRD0 = AUX1               PD4 = SRD1 = No connect (** in 50Mhz this was MODE selec
t for 16 or 32 bit FO)
459                                 ;       PC5 = STD0 = No connect         PD5 = STD1 = WRFIFO*
460                                 ; ***********************************************************************************
461    
462    
463                                 ; ****************************************************************************
464                                 ; Program the serial port ESSI1 = Port D for general purpose I/O (GPIO)
465    
466       P:000117 P:000119 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  init.asm  Page 10



467       P:000119 P:00011B 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            00001C
468       P:00011B P:00011D 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
469       P:00011D P:00011F 060AA0            REP     #10
470       P:00011E P:000120 000000            NOP
471       P:00011F P:000121 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
472                                                                                     ; was %011100
473    
474                                 ; Program the SCI port to benign values
475       P:000121 P:000123 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
476       P:000123 P:000125 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
477       P:000125 P:000127 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
478                                 ;       PE0 = RXD
479                                 ;       PE1 = TXD
480                                 ;       PE2 = SCLK
481    
482                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
483       P:000127 P:000129 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
484       P:000129 P:00012B 07F40B            MOVEP             #$2800,X:TCSR1
                            002800
485       P:00012B P:00012D 07F407            MOVEP             #$2800,X:TCSR2
                            002800
486    
487    
488                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
489       P:00012D P:00012F 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
490       P:00012F P:000131 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
e
                            008929
491       P:000131 P:000133 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
                            000122
492    
493    
494                                 ; Program the DRAM memory access and addressing
495       P:000133 P:000135 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
                            020022
496       P:000135 P:000137 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
497    
498    
499                                 ; Clear all PCI error conditions
500       P:000137 P:000139 084E0A            MOVEP             X:DPSR,A
501       P:000138 P:00013A 0140C2            OR      #$1FE,A
                            0001FE
502       P:00013A P:00013C 000000            NOP
503       P:00013B P:00013D 08CE0A            MOVEP             A,X:DPSR
504    
505                                 ;--------------------------------------------------------------------
506                                 ; Enable one interrupt only: software reset switch
507       P:00013C P:00013E 08F4BF            MOVEP             #$0001C0,X:IPRC         ; IRQB priority = 1 (FIFO half full HF*)
                            0001C0
508                                                                                     ; IRQC priority = 2 (reset switch)
509       P:00013E P:000140 05F439            MOVE              #$200,SR                ; Mask set up for reset switch only
                            000200
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  init.asm  Page 11



510    
511    
512                                 ;--------------------------------------------------------------------------
513                                 ; Initialize the fiber optic serial transmitter to zero
514       P:000140 P:000142 01B786            JCLR    #TDE,X:SSISR0,*
                            000140
515       P:000142 P:000144 07F43C            MOVEP             #$000000,X:TX00
                            000000
516    
517                                 ;--------------------------------------------------------------------
518    
519                                 ; clear DTXM - PCI master transmitter
520       P:000144 P:000146 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
521       P:000145 P:000147 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000145
522    
523                                 ;----------------------------------------------------------------------
524                                 ; clear DRXR - PCI receiver
525    
526       P:000147 P:000149 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014C
527       P:000149 P:00014B 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
528       P:00014A P:00014C 000000            NOP
529       P:00014B P:00014D 0C0147            JMP     <CLR0
530                                 CLR1
531    
532                                 ;-----------------------------------------------------------------------------
533                                 ; copy parameter table from P memory into X memory
534    
535                                 ; but not frame_count and num_dumped - don't want these reset by fatal error...
536                                 ; they will be reset by new packet or pci_reset ISR
537    
538       P:00014C P:00014E 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000006
539       P:00014E P:000150 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
                            000002
540    
541                                 ; Move the table of constants from P: space to X: space
542       P:000150 P:000152 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            0006BE
543       P:000152 P:000154 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
544       P:000153 P:000155 065980            DO      #VAR_TBL_LENGTH,X_WRITE
                            000156
545       P:000155 P:000157 07D984            MOVE              P:(R1)+,X0
546       P:000156 P:000158 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
547                                 X_WRITE
548    
549       P:000157 P:000159 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
                            000006
550       P:000159 P:00015B 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000002
551    
552                                 ;-------------------------------------------------------------------------------
553                                 ; initialise MODE; packet choke and PCI interrupts are ON.
554       P:00015B P:00015D 0A7021            BSET    #MODE_CHOKE,X:MODE
                            000001
555       P:00015D P:00015F 0A7024            BSET    #MODE_IRQ,X:MODE
                            000001
556    
557                                 ;----------------------------------------------------------------------------
558                                 ; Disable byte swapping - enabled after first command to MCE.
559                                 ; i.e after first 'CON'
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  init.asm  Page 12



560       
561                                 ;       BCLR    #AUX1,X:PDRC
562    
563                                 ;----------------------------------------------------------------------------
564                                 ; Initialize PCI controller again, after booting, to make sure it sticks
565       P:00015F P:000161 0A8514            BCLR    #20,X:DCTR                        ; Terminate and reset mode
566       P:000160 P:000162 000000            NOP
567       P:000161 P:000163 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            000161
568       P:000163 P:000165 000000            NOP
569       P:000164 P:000166 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
570       P:000165 P:000167 000000            NOP
571       P:000166 P:000168 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000166
572    
573       
574       P:000168 P:00016A 0BF080            JSR     CLEAR_FO_FIFO                     ; Clear the fibre fifo!
                            00053C
575       P:00016A P:00016C 0BF080            JSR     TIMER_DISABLE                     ; Disable NFY timer
                            000657
576    
578                                           INCLUDE 'main.asm'
579                                         COMMENT *
580    
581                                 Main section of the pci card code.
582    
583                                 See info.asm for versioning and authors.
584    
585                                         *
586                                           PAGE    132                               ; Printronix page width - 132 columns
587                                           OPT     CEX                               ; print DC evaluations
588    
592    
593                                 PACKET_IN
594    
595       
596       P:00016C P:00016E 0A0017            BCLR    #MAIN_LOOP_POLL,X:<STATUS
597    
598       
599       P:00016D P:00016F 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
600    
601       
602       P:00016F P:000171 0A01A0            JSET    #MODE_APPLICATION,X:<MODE,APPLICATION
                            000800
603    
604       
605       P:000171 P:000173 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            00065D
606    
607       
608       P:000173 P:000175 0B00B4            JSSET   #QT_FLUSH,X:STATUS,BUFFER_INFORM
                            000696
609    
610       
611       P:000175 P:000177 0D04AA            JSR     <CHECK_FO
612       P:000176 P:000178 0B00A3            JSSET   #FO_WRD_RCV,X:STATUS,HANDLE_FIFO
                            00017D
613    
614       
615       P:000178 P:00017A 0B00AA            JSSET   #CON_DEMAND,X:STATUS,CON_NOW
                            0002C5
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 13



616    
617       
618       P:00017A P:00017C 000000            NOP
619       P:00017B P:00017D 000000            NOP
620    
621       
622       P:00017C P:00017E 0C016C            JMP     PACKET_IN
623    
627    
628    
629    
631    
632                                 HANDLE_FIFO
633       
634       P:00017D P:00017F 60F400            MOVE              #>HEAD_W1_0,R0
                            00000F
635       P:00017F P:000181 060880            DO      #8,HANDLE_FIFO_CHECK_PREAMBLE
                            000188
636                                 HANDLE_FIFO_WAIT
637       P:000181 P:000183 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000181
638       P:000183 P:000185 000000            NOP
639       P:000184 P:000186 000000            NOP
640       P:000185 P:000187 01AD80            JCLR    #EF,X:PDRD,HANDLE_FIFO_WAIT
                            000181
641       P:000187 P:000189 08443F            MOVEP             X:RDFIFO,X0
642       P:000188 P:00018A 445800            MOVE              X0,X:(R0)+
643    
644                                 HANDLE_FIFO_CHECK_PREAMBLE
645       P:000189 P:00018B 60F400            MOVE              #>HEAD_W1_0,R0
                            00000F
646       P:00018B P:00018D 56F400            MOVE              #>$A5A5,A
                            00A5A5
647       P:00018D P:00018F 44D800            MOVE              X:(R0)+,X0
648       P:00018E P:000190 200045            CMP     X0,A
649       P:00018F P:000191 0E21DC            JNE     <PRE_ERROR
650       P:000190 P:000192 44D800            MOVE              X:(R0)+,X0
651       P:000191 P:000193 200045            CMP     X0,A
652       P:000192 P:000194 0E21DC            JNE     <PRE_ERROR
653       P:000193 P:000195 56F400            MOVE              #>$5A5A,A
                            005A5A
654       P:000195 P:000197 44D800            MOVE              X:(R0)+,X0
655       P:000196 P:000198 200045            CMP     X0,A
656       P:000197 P:000199 0E21DC            JNE     <PRE_ERROR
657       P:000198 P:00019A 44D800            MOVE              X:(R0)+,X0
658       P:000199 P:00019B 200045            CMP     X0,A
659       P:00019A P:00019C 0E21DC            JNE     <PRE_ERROR
660    
661       
662       P:00019B P:00019D 200013            CLR     A
663       P:00019C P:00019E 50F000            MOVE              X:>(HEAD_W1_0+6),A0
                            000015
664       P:00019E P:0001A0 44F000            MOVE              X:>(HEAD_W1_0+7),X0
                            000016
665       P:0001A0 P:0001A2 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
666    
667       
668       P:0001A2 P:0001A4 0BF080            JSR     PACKET_PARTITIONS
                            000608
669       P:0001A4 P:0001A6 0AF080            JMP     XXXX
                            0001D0
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 14



670    
671                                 OLD_HANDLE_FIFO
672    
673       P:0001A6 P:0001A8 0A01A1            JSET    #MODE_CHOKE,X:<MODE,RETURN_NOW    ; IF MCE Packet choke on - just keep clearin
g FIFO.
                            0001DF
674       P:0001A8 P:0001AA 440F00            MOVE              X0,X:<HEAD_W1_0         ;store received word
675       P:0001A9 P:0001AB 56F000            MOVE              X:PREAMB1,A
                            000025
676       P:0001AB P:0001AD 200045            CMP     X0,A                              ; check it is correct
677       P:0001AC P:0001AE 0E21DC            JNE     <PRE_ERROR                        ; if not go to start
678    
679       P:0001AD P:0001AF 0D04BA            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
680       P:0001AE P:0001B0 441000            MOVE              X0,X:<HEAD_W1_1         ;store received word
681       P:0001AF P:0001B1 56F000            MOVE              X:PREAMB1,A
                            000025
682       P:0001B1 P:0001B3 200045            CMP     X0,A                              ; check it is correct
683       P:0001B2 P:0001B4 0E21DC            JNE     <PRE_ERROR                        ; if not go to start
684    
685       P:0001B3 P:0001B5 0D04BA            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
686       P:0001B4 P:0001B6 441100            MOVE              X0,X:<HEAD_W2_0         ;store received word
687       P:0001B5 P:0001B7 56F000            MOVE              X:PREAMB2,A
                            000026
688       P:0001B7 P:0001B9 200045            CMP     X0,A                              ; check it is correct
689       P:0001B8 P:0001BA 0E21DC            JNE     <PRE_ERROR                        ; if not go to start
690    
691       P:0001B9 P:0001BB 0D04BA            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
692       P:0001BA P:0001BC 441200            MOVE              X0,X:<HEAD_W2_1         ;store received word
693       P:0001BB P:0001BD 56F000            MOVE              X:PREAMB2,A
                            000026
694       P:0001BD P:0001BF 200045            CMP     X0,A                              ; check it is correct
695       P:0001BE P:0001C0 0E21DC            JNE     <PRE_ERROR                        ; if not go to start
696    
697                                 PACKET_INFO                                         ; packet preamble valid
698       P:0001BF P:0001C1 0D04BA            JSR     <WT_FIFO
699       P:0001C0 P:0001C2 441300            MOVE              X0,X:<HEAD_W3_0         ; RP or DA
700       P:0001C1 P:0001C3 0D04BA            JSR     <WT_FIFO
701       P:0001C2 P:0001C4 441400            MOVE              X0,X:<HEAD_W3_1         ; $2020
702    
703       P:0001C3 P:0001C5 0D04BA            JSR     <WT_FIFO
704       P:0001C4 P:0001C6 441500            MOVE              X0,X:<HEAD_W4_0         ; packet size lo
705       P:0001C5 P:0001C7 0D04BA            JSR     <WT_FIFO
706       P:0001C6 P:0001C8 441600            MOVE              X0,X:<HEAD_W4_1         ; packet size hi
707    
708       
709       P:0001C7 P:0001C9 200013            CLR     A
710       P:0001C8 P:0001CA 50F000            MOVE              X:HEAD_W4_0,A0
                            000015
711       P:0001CA P:0001CC 44F000            MOVE              X:HEAD_W4_1,X0
                            000016
712       P:0001CC P:0001CE 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
713    
714       
715       P:0001CE P:0001D0 0BF080            JSR     PACKET_PARTITIONS
                            000608
716                                 XXXX
718       P:0001D0 P:0001D2 56F000            MOVE              X:HEAD_W3_0,A
                            000013
719    
720       P:0001D2 P:0001D4 0140C5            CMP     #>'RP',A
                            005250
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 15



721       P:0001D4 P:0001D6 0AF0AA            JEQ     HANDLE_RP
                            0001E0
722    
723       P:0001D6 P:0001D8 0140C5            CMP     #>'DA',A
                            004441
724       P:0001D8 P:0001DA 0AF0AA            JEQ     HANDLE_DA
                            00021C
725    
726       P:0001DA P:0001DC 0AF080            JMP     QT_PTYPE_ERROR
                            0001DF
727    
728                                 PRE_ERROR
729       P:0001DC P:0001DE 0A0026            BSET    #PREAMBLE_ERROR,X:<STATUS         ; indicate a preamble error
730       P:0001DD P:0001DF 0BF080            JSR     CLEAR_FO_FIFO                     ; empty the fifo (2 ms!)
                            00053C
731    
732                                 QT_PTYPE_ERROR
733                                 QT_FSIZE_ERROR
734                                 RETURN_NOW
735       P:0001DF P:0001E1 00000C            RTS
736    
737    
738    
741    
742                                 HANDLE_RP
743       
744       P:0001E0 P:0001E2 0A0183            JCLR    #MODE_RP_BUFFER,X:MODE,MCE_PACKET
                            0002EC
745    
746       
747       P:0001E2 P:0001E4 0A00B5            JSET    #RP_BUFFER_FULL,X:STATUS,HANDLE_RP_DROP
                            000214
748    
749       
750       P:0001E4 P:0001E6 61F400            MOVE              #>REPLY_BUFFER,R1
                            100000
751       P:0001E6 P:0001E8 0BF080            JSR     BUFFER_PACKET
                            000615
752    
753       
754       P:0001E8 P:0001EA 60F400            MOVE              #RP_BASE_LO,R0
                            000048
755       P:0001EA P:0001EC 0BF080            JSR     LOAD_HILO_ADDRESS
                            0006AD
756    
757       P:0001EC P:0001EE 60F400            MOVE              #BURST_DEST_LO,R0
                            00002E
758       P:0001EE P:0001F0 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006B5
759    
760       
761       P:0001F0 P:0001F2 200013            CLR     A
762       P:0001F1 P:0001F3 20001B            CLR     B
763       P:0001F2 P:0001F4 50F000            MOVE              X:PACKET_SIZE,A0
                            00002B
764       P:0001F4 P:0001F6 0C1D04            ASL     #2,A,A                            ; Size in bytes
765       P:0001F5 P:0001F7 51F000            MOVE              X:RP_MAX_SIZE,B0
                            00004A
766    
767       P:0001F7 P:0001F9 200005            CMP     B,A                               ; A ? B
768       P:0001F8 P:0001FA 0AF0AF            JLE     HANDLE_RP1
                            0001FB
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 16



769       P:0001FA P:0001FC 21EE00            MOVE              B,A
770    
771                                 HANDLE_RP1
772       
773    
774       P:0001FB P:0001FD 44F400            MOVE              #'NFY',X0
                            4E4659
775       P:0001FD P:0001FF 447000            MOVE              X0,X:DTXS_WD1
                            00000B
776       P:0001FF P:000201 44F400            MOVE              #'RPQ',X0
                            525051
777       P:000201 P:000203 447000            MOVE              X0,X:DTXS_WD2
                            00000C
778       P:000203 P:000205 507000            MOVE              A0,X:DTXS_WD3           ; A0=block_size
                            00000D
779       P:000205 P:000207 547000            MOVE              A1,X:DTXS_WD4           ; A1=0
                            00000E
780    
781       
782       P:000207 P:000209 44F400            MOVE              #>REPLY_BUFFER,X0
                            100000
783       P:000209 P:00020B 507000            MOVE              A0,X:BLOCK_SIZE
                            00002C
784       P:00020B P:00020D 447000            MOVE              X0,X:YMEM_SRC
                            000032
785       P:00020D P:00020F 0BF080            JSR     BLOCK_TRANSFER
                            0005A0
786    
787       
788       P:00020F P:000211 0A7035            BSET    #RP_BUFFER_FULL,X:STATUS
                            000000
789       P:000211 P:000213 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            0004CA
790    
791       
792       P:000213 P:000215 00000C            RTS
793    
794                                 HANDLE_RP_DROP
795       P:000214 P:000216 56F000            MOVE              X:RP_DROPS,A
                            00004B
796       P:000216 P:000218 014180            ADD     #1,A
797       P:000217 P:000219 000000            NOP
798       P:000218 P:00021A 567000            MOVE              A,X:RP_DROPS
                            00004B
799       P:00021A P:00021C 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            00063D
800    
802    
803    
806    
807    
808                                 HANDLE_DA
809    
810       
811       P:00021C P:00021E 56F000            MOVE              X:FRAME_COUNT,A
                            000002
812       P:00021E P:000220 0140C0            ADD     #>1,A
                            000001
813       P:000220 P:000222 000000            NOP
814       P:000221 P:000223 560200            MOVE              A,X:<FRAME_COUNT
815    
816       
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 17



817       P:000222 P:000224 0A0182            JCLR    #MODE_QT,X:MODE,MCE_PACKET
                            0002EC
818    
819       
820       P:000224 P:000226 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
821       P:000226 P:000228 0BF080            JSR     BUFFER_PACKET
                            000615
822    
823       
824       P:000228 P:00022A 56F000            MOVE              X:QT_BUF_HEAD,A
                            000042
825       P:00022A P:00022C 014180            ADD     #1,A
826       P:00022B P:00022D 57F000            MOVE              X:QT_BUF_MAX,B
                            00003F
827       P:00022D P:00022F 20000D            CMP     A,B
828       P:00022E P:000230 0AF0A1            JGE     HANDLE_DA_MATH
                            000231
829       P:000230 P:000232 2E0000            MOVE              #0,A
830                                 HANDLE_DA_MATH
831       P:000231 P:000233 57F000            MOVE              X:QT_BUF_TAIL,B
                            000043
832       P:000233 P:000235 20000D            CMP     A,B
833       P:000234 P:000236 0AF0AA            JEQ     HANDLE_DA_DROP
                            000253
834    
835       
836       P:000236 P:000238 200013            CLR     A
837       P:000237 P:000239 50F000            MOVE              X:PACKET_SIZE,A0
                            00002B
838    
839       P:000239 P:00023B 014088            ADD     #0,B                              ; Clear carry
840       P:00023A P:00023C 0C1D04            ASL     #2,A,A                            ; Size, in bytes
841    
842       
843       P:00023B P:00023D 20001B            CLR     B
844       P:00023C P:00023E 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            000040
845       P:00023E P:000240 20000D            CMP     A,B
846       P:00023F P:000241 0E21DF            JNE     QT_FSIZE_ERROR
847    
848       
849       P:000240 P:000242 517000            MOVE              B0,X:BLOCK_SIZE
                            00002C
850       P:000242 P:000244 557000            MOVE              B1,X:YMEM_SRC           ; Y:0
                            000032
851    
852       P:000244 P:000246 60F400            MOVE              #QT_DEST_LO,R0
                            000044
853       P:000246 P:000248 0BF080            JSR     LOAD_HILO_ADDRESS
                            0006AD
854       P:000248 P:00024A 60F400            MOVE              #BURST_DEST_LO,R0
                            00002E
855       P:00024A P:00024C 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006B5
856    
857       
858       P:00024C P:00024E 0BF080            JSR     BLOCK_TRANSFER
                            0005A0
859    
860       
861       P:00024E P:000250 0BF080            JSR     BUFFER_INCR
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 18



                            00066B
862    
863       
864       P:000250 P:000252 0BF080            JSR     BUFFER_INFORM_CHECK
                            000689
865    
866       P:000252 P:000254 00000C            RTS
867    
868                                 HANDLE_DA_DROP
869       
870       P:000253 P:000255 56F000            MOVE              X:QT_DROPS,A
                            000047
871       P:000255 P:000257 014180            ADD     #1,A
872       P:000256 P:000258 000000            NOP
873       P:000257 P:000259 567000            MOVE              A,X:QT_DROPS
                            000047
874       P:000259 P:00025B 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            00063D
875    
877    
878    
879    
881    
882                                 ;----------------------------------------------
883                                 CON_TRANSFER
884                                 ;----------------------------------------------
885                                 ;   In:
886                                 ;   - BURST_SRC_HI:BURST_SRC_LO is PC RAM address
887                                 ;   - BLOCK_SIZE is packet size, in bytes
888                                 ;   - YMEM_DEST is start of data in Y memory
889                                 ;  Out:
890                                 ;   - BLOCK_SIZE will be decremented to zero.
891                                 ;   - BURST_SRC_HI:LO will be incremented by BLOCK_SIZE
892                                 ;   - YMEM_DEST will be incremented by BLOCK_SIZE/2
893                                 ;  Trashes:
894                                 ;   - A and B
895    
896       
897       P:00025B P:00025D 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002C
898    
899       P:00025D P:00025F 014085            CMP     #0,A
900       P:00025E P:000260 0AF0AA            JEQ     XBLOCK_DONE
                            0002A4
901    
902       
903       P:000260 P:000262 20001B            CLR     B
904       P:000261 P:000263 55F000            MOVE              X:PCI_BURST_SIZE,B1
                            00004E
905    
906       P:000263 P:000265 200005            CMP     B,A                               ; A ? B
907       P:000264 P:000266 0E1266            JGE     <XBLOCK_TRANSFER1                 ; jump if A >= B
908       P:000265 P:000267 21CF00            MOVE              A,B                     ; This only moves A1,B1.
909                                 XBLOCK_TRANSFER1
910       P:000266 P:000268 200014            SUB     B,A                               ; A -= B
911       P:000267 P:000269 014088            ADD     #0,B                              ; Clear carry bit
912       P:000268 P:00026A 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002C
913       P:00026A P:00026C 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002D
914       P:00026C P:00026E 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 19



915    
916       
917       P:00026D P:00026F 50F000            MOVE              X:YMEM_DEST,A0
                            000033
918       P:00026F P:000271 507000            MOVE              A0,X:DDR0               ; DMA dest'n
                            FFFFEE
919       P:000271 P:000273 08F4AF            MOVEP             #>DRXR,X:DSR0           ; DMA source
                            FFFFCB
920       P:000273 P:000275 200010            ADD     B,A
921       P:000274 P:000276 00000B            DEC     B
922       P:000275 P:000277 507000            MOVE              A0,X:YMEM_DEST          ; YMEM_DEST += BURST_SIZE/2
                            000033
923    
924       P:000277 P:000279 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
925    
926       
927       P:000278 P:00027A 08F4AC            MOVEP             #$8EEAC4,X:DCR0
                            8EEAC4
928    
929                                 XBLOCK_PCI
930       
931       P:00027A P:00027C 200013            CLR     A
932       P:00027B P:00027D 20001B            CLR     B
933       P:00027C P:00027E 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            00002D
934       P:00027E P:000280 00000B            DEC     B                                 ; n8 - 1
935       P:00027F P:000281 014088            ADD     #0,B                              ; Clear carry
936       P:000280 P:000282 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
937       P:000281 P:000283 014088            ADD     #0,B                              ; Clear carry
938       P:000282 P:000284 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
939    
940       P:000283 P:000285 50F000            MOVE              X:BURST_SRC_HI,A0
                            000031
941    
942       P:000285 P:000287 200010            ADD     B,A
943       P:000286 P:000288 000000            NOP
944       P:000287 P:000289 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
945    
946       P:000289 P:00028B 280600            MOVE              #$06,A0                 ; This is a read.
947       P:00028A P:00028C 014088            ADD     #0,B                              ; Clear carry
948       P:00028B P:00028D 0C1D20            ASL     #16,A,A
949       P:00028C P:00028E 51F000            MOVE              X:BURST_SRC_LO,B0
                            000030
950       P:00028E P:000290 200010            ADD     B,A
951       P:00028F P:000291 000000            NOP
952       P:000290 P:000292 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
953    
954    
955    
956                                 XBLOCK_CHECK
957       P:000291 P:000293 000000            NOP
958       P:000292 P:000294 000000            NOP
959       P:000293 P:000295 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for burst termination
                            000293
960    
961       
962       P:000295 P:000297 0A8AAE            JSET    #MDT,X:DPSR,XBLOCK_OK
                            00029F
963    
964       P:000297 P:000299 0BF080            JSR     PCI_ERROR_CLEAR
                            000546
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 20



965    
966       P:000299 P:00029B 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
967       P:00029B P:00029D 0E82A5            JCS     <XBLOCK_RESTART
968    
969       P:00029C P:00029E 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
970       P:00029E P:0002A0 0E82A6            JCS     <XBLOCK_RESUME
971    
972                                 XBLOCK_OK
973       P:00029F P:0002A1 50F000            MOVE              X:BURST_SIZE,A0         ; Pass # of words written to updater
                            00002D
974       P:0002A1 P:0002A3 0BF080            JSR     XBLOCK_UPDATE
                            0002B8
975       P:0002A3 P:0002A5 0C025B            JMP     CON_TRANSFER                      ; Finish the block
976                                 XBLOCK_DONE
977       P:0002A4 P:0002A6 00000C            RTS                                       ; Done
978    
979                                 XBLOCK_RESTART
980       P:0002A5 P:0002A7 0C027A            JMP     XBLOCK_PCI                        ; Recalculate pci and resend
981    
982                                 XBLOCK_RESUME
983       P:0002A6 P:0002A8 200013            CLR     A
984       P:0002A7 P:0002A9 20001B            CLR     B
985       P:0002A8 P:0002AA 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
986       P:0002A9 P:0002AB 0A8A8F            JCLR    #RDCQ,X:DPSR,XBLOCK_RESUME1
                            0002AC
987    
988       P:0002AB P:0002AD 000009            INC     B
989    
990                                 XBLOCK_RESUME1
991    
992       P:0002AC P:0002AE 000009            INC     B                                 ; We want N, not N-1.
993       P:0002AD P:0002AF 014088            ADD     #0,B                              ; Clear carry
994       P:0002AE P:0002B0 0C1C20            ASR     #16,A,A
995       P:0002AF P:0002B1 200018            ADD     A,B                               ; B is words remaining
996       P:0002B0 P:0002B2 014088            ADD     #0,B                              ; Clear carry
997       P:0002B1 P:0002B3 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
998       P:0002B2 P:0002B4 50F000            MOVE              X:BURST_SIZE,A0
                            00002D
999       P:0002B4 P:0002B6 200014            SUB     B,A                               ; A is words written
1000   
1001      P:0002B5 P:0002B7 0BF080            JSR     XBLOCK_UPDATE
                            0002B8
1002      P:0002B7 P:0002B9 0C027A            JMP     XBLOCK_PCI                        ; Recalculate pci and resend
1003   
1004                                ; BLOCK_UPDATE
1005                                ;  Subtract A from BURST_SIZE and add A to BURST_DEST_LO
1006                                ;  Caller can check Z flag to see if BURST_SIZE is now 0.
1007                                XBLOCK_UPDATE
1008      P:0002B8 P:0002BA 210500            MOVE              A0,X1                   ; Save A for later
1009      P:0002B9 P:0002BB 0C1D01            ASL     #0,A,B                            ; MOVE A,B
1010   
1011      P:0002BA P:0002BC 60F400            MOVE              #BURST_SRC_LO,R0        ;
                            000030
1012      P:0002BC P:0002BE 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates BURST_DEST to BURST_DEST + B
                            0006B3
1013   
1014      P:0002BE P:0002C0 57F000            MOVE              X:BURST_SIZE,B
                            00002D
1015      P:0002C0 P:0002C2 20006C            SUB     X1,B                              ; Zero flag must be preserved!
1016      P:0002C1 P:0002C3 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 21



1017      P:0002C2 P:0002C4 557000            MOVE              B1,X:BURST_SIZE
                            00002D
1018   
1019      P:0002C4 P:0002C6 00000C            RTS
1020   
1021   
1022   
1023   
1024                                CON_NOW
1025                                ;       This routine runs after the PC sends a 'CON' command, and will
1026                                ;       copy the command to the MCE and then reply to the PC.
1027   
1028      
1029      P:0002C5 P:0002C7 60F400            MOVE              #>CON_SRC_LO,R0
                            00004C
1030      P:0002C7 P:0002C9 0BF080            JSR     LOAD_HILO_ADDRESS
                            0006AD
1031      P:0002C9 P:0002CB 60F400            MOVE              #>BURST_SRC_LO,R0
                            000030
1032      P:0002CB P:0002CD 0BF080            JSR     SAVE_HILO_ADDRESS
                            0006B5
1033      P:0002CD P:0002CF 51F400            MOVE              #>COMMAND_BUFFER,B0
                            200000
1034      P:0002CF P:0002D1 50F400            MOVE              #>256,A0
                            000100
1035      P:0002D1 P:0002D3 517000            MOVE              B0,X:YMEM_DEST
                            000033
1036      P:0002D3 P:0002D5 507000            MOVE              A0,X:BLOCK_SIZE
                            00002C
1037      P:0002D5 P:0002D7 0D025B            JSR     CON_TRANSFER
1038   
1039                                CON_NOW_TRANSMIT
1040      
1041   
1042      P:0002D6 P:0002D8 66F400            MOVE              #>COMMAND_BUFFER,R6
                            200000
1043      P:0002D8 P:0002DA 068080            DO      #128,CON_NOW_CLEANUP              ; block size = 16bit x 128 (256 bytes)
                            0002E1
1044      P:0002DA P:0002DC 5CDE00            MOVE                          Y:(R6)+,A1  ; b2, b1  (lsb)
1045      P:0002DB P:0002DD 0C1C11            ASR     #8,A,B                            ; Shift b2 into B1
1046      P:0002DC P:0002DE 0140C6            AND     #>$FF,A
                            0000FF
1047      P:0002DE P:0002E0 547000            MOVE              A1,X:FO_SEND
                            FFF000
1048      P:0002E0 P:0002E2 557000            MOVE              B1,X:FO_SEND
                            FFF000
1049   
1050                                CON_NOW_CLEANUP
1051      P:0002E2 P:0002E4 0A0101            BCLR    #MODE_CHOKE,X:<MODE               ; disable packet choke...
1052                                                                                    ; comms now open with MCE and packets will b
e processed.
1053      
1054                                ;       BSET    #AUX1,X:PDRC            ; enable hardware
1055   
1056      
1057      P:0002E3 P:0002E5 0A700A            BCLR    #CON_DEMAND,X:STATUS
                            000000
1058   
1059      
1060      P:0002E5 P:0002E7 44F400            MOVE              #'CON',X0
                            434F4E
1061      P:0002E7 P:0002E9 0BF080            JSR     VCOM_PREPARE_REPLY
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 22



                            000329
1062      P:0002E9 P:0002EB 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            0004CA
1063   
1064      P:0002EB P:0002ED 00000C            RTS
1065   
1066   
1067   
1068   
1070   
1071                                ; --------------------------------------------------------------------------
1072                                ; --------------------- MAIN PACKET HANDLING CODE --------------------------
1073                                ; --------------------------------------------------------------------------
1074   
1075                                ; prepare notify to inform host that a packet has arrived.
1076   
1077                                MCE_PACKET
1078      P:0002EC P:0002EE 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
1079   
1080      P:0002ED P:0002EF 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
1081      P:0002EF P:0002F1 440B00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
1082   
1083      P:0002F0 P:0002F2 449300            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
1084      P:0002F1 P:0002F3 440C00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
1085   
1086      P:0002F2 P:0002F4 449500            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
1087      P:0002F3 P:0002F5 440D00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
1088   
1089      P:0002F4 P:0002F6 449600            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
1090      P:0002F5 P:0002F7 440E00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sage
1091   
1092      
1093      P:0002F6 P:0002F8 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
1094      P:0002F7 P:0002F9 0D04CA            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
1095      P:0002F8 P:0002FA 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
1096   
1097   
1098      P:0002F9 P:0002FB 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
1099      P:0002FB P:0002FD 0BF080            JSR     BUFFER_PACKET
                            000615
1100   
1101      
1102   
1103      P:0002FD P:0002FF 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; on fatal error, re-init.
                            000100
1104      P:0002FF P:000301 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; Set in 'send_packet_to_host' ISR
                            0002FD
1105   
1106      
1107      P:000301 P:000303 44F400            MOVE              #>IMAGE_BUFFER,X0
                            000000
1108      P:000303 P:000305 56F000            MOVE              X:PACKET_SIZE,A
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 23



                            00002B
1109      P:000305 P:000307 0C1D04            ASL     #2,A,A
1110      P:000306 P:000308 447000            MOVE              X0,X:YMEM_SRC
                            000032
1111      P:000308 P:00030A 547000            MOVE              A1,X:BLOCK_SIZE
                            00002C
1112      P:00030A P:00030C 0BF080            JSR     BLOCK_TRANSFER
                            0005A0
1113   
1114      P:00030C P:00030E 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
1115   
1116      
1117      P:00030E P:000310 44F400            MOVE              #'HST',X0
                            485354
1118      P:000310 P:000312 0BF080            JSR     VCOM_PREPARE_REPLY
                            000329
1119      P:000312 P:000314 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            0004CA
1120      P:000314 P:000316 00000C            RTS
1121   
1122                                ;----------------------------------------------------------
1123                                ; clear out the fifo after an HST timeout...
1124                                ;----------------------------------------------------------
1125   
1126                                DUMP_FIFO
1127      P:000315 P:000317 61F400            MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
1128      P:000317 P:000319 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
1129      P:000319 P:00031B 200013            CLR     A
1130      P:00031A P:00031C 320000            MOVE              #0,R2                   ; use R2 as a dump count
1131                                NEXT_DUMP
1132      P:00031B P:00031D 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000326
1133      P:00031D P:00031F 000000            NOP
1134      P:00031E P:000320 000000            NOP
1135      P:00031F P:000321 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000326
1136   
1137      P:000321 P:000323 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
1138      P:000322 P:000324 205A00            MOVE              (R2)+                   ; inc dump count
1139      P:000323 P:000325 224E00            MOVE              R2,A                    ;
1140      P:000324 P:000326 200045            CMP     X0,A                              ; check we've not hit dump limit
1141      P:000325 P:000327 0E231B            JNE     NEXT_DUMP                         ; not hit limit?
1142                                FIFO_EMPTY
1143      P:000326 P:000328 627000            MOVE              R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000006
1144      P:000328 P:00032A 0C0100            JMP     <START                            ; re-initialise
1145   
1146   
1147                                ; -------------------------------------------------------------------------------------
1148                                ;                              END OF MAIN PACKET HANDLING CODE
1149                                ; -------------------------------------------------------------------------------------
1150   
1151   
1152   
1153                                ; -------------------------------------------------------------------------------------
1154                                ;
1155                                ;                              INTERRUPT SERVICE ROUTINES
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 24



1156                                ;
1157                                ; -------------------------------------------------------------------------------------
1158   
1159                                ; ---------------
1160                                ; Rules:  Don't use N#, or any R# except R0 unless you add them to the saved register set.
1161   
1162   
1163                                ; ----------------------------------------------------------------------------
1164                                ; VCOM_* - routines: utility functions for hosty command vector communication.
1165                                ;-----------------------------------------------------------------------------
1166   
1167   
1168                                ; VCOM_PREPARE_REPLY
1169                                ;
1170                                ; Prepare the reply packet, using X0 as the command name (second word).  The
1171                                ; message defaults to 'ACK' with NULL data.  The user may subsequenty fill in
1172                                ; the data field (word 4) and mark the packet as error if necessary.
1173   
1174                                VCOM_PREPARE_REPLY
1175      
1176      
1177      P:000329 P:00032B 50F400            MOVE              #'REP',A0
                            524550
1178      P:00032B P:00032D 447000            MOVE              X0,X:DTXS_WD2           ; Command
                            00000C
1179      P:00032D P:00032F 507000            MOVE              A0,X:DTXS_WD1
                            00000B
1180   
1181      P:00032F P:000331 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
                            41434B
1182      P:000331 P:000333 000000            NOP
1183      P:000332 P:000334 547000            MOVE              A1,X:DTXS_WD3           ; ACK
                            00000D
1184      P:000334 P:000336 507000            MOVE              A0,X:DTXS_WD4           ; no comment
                            00000E
1185      P:000336 P:000338 00000C            RTS
1186   
1187   
1188                                ; VCOM_CHECK
1189                                ;
1190                                ; Compares DRXR_WD1 to X0.  If they are equal, Z is set on return.  If they
1191                                ; are not equal then Z is cleared and the reply will be marked as ERR with
1192                                ; 'CNE' in the last word.
1193                                ; Trashes A and B always and X0 on error.
1194   
1195                                VCOM_CHECK
1196      P:000337 P:000339 208E00            MOVE              X0,A
1197      P:000338 P:00033A 57F000            MOVE              X:DRXR_WD1,B
                            000007
1198      P:00033A P:00033C 20000D            CMP     A,B
1199      P:00033B P:00033D 0AF0AA            JEQ     VCOM_RTS
                            000345
1200   
1201      P:00033D P:00033F 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
1202      P:00033F P:000341 50F400            MOVE              #'ERR',A0
                            455252
1203      P:000341 P:000343 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1204      P:000343 P:000345 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1205                                VCOM_RTS
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 25



1206      P:000345 P:000347 00000C            RTS
1207   
1208   
1209                                ; VCOM_INTRO
1210                                ;
1211                                ; Read DSP command from DRXR.  Prepare the reply packet and verify that it
1212                                ; matches the key in X1.  If it does not, mark the reply as error and set
1213                                ; the Z flag.
1214   
1215                                VCOM_INTRO
1216      P:000346 P:000348 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            0004DC
1217      P:000348 P:00034A 20A400            MOVE              X1,X0
1218      P:000349 P:00034B 0D0329            JSR     VCOM_PREPARE_REPLY
1219      P:00034A P:00034C 0D0337            JSR     VCOM_CHECK
1220      P:00034B P:00034D 00000C            RTS
1221   
1222   
1223                                ; VCOM_EXIT_ERROR_X0
1224                                ; VCOM_EXIT_X0
1225                                ; VCOM_EXIT
1226                                ;
1227                                ; For returning from host command vector interrupts only.  These three
1228                                ; routines do the following (respectively):
1229                                ; a) Mark reply as error, then (b)
1230                                ; b) Put X0 into last word of reply, then (c)
1231                                ; c) Restore registers and RTI.
1232   
1233                                VCOM_EXIT_ERROR_X0
1234      P:00034C P:00034E 50F400            MOVE              #'ERR',A0
                            455252
1235      P:00034E P:000350 000000            NOP
1236      P:00034F P:000351 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1237                                VCOM_EXIT_X0
1238      P:000351 P:000353 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1239                                VCOM_EXIT
1240      P:000353 P:000355 0BF080            JSR     RESTORE_REGISTERS
                            000503
1241      P:000355 P:000357 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            0004CA
1242      P:000357 P:000359 000004            RTI
1243   
1244   
1245   
1246   
1247                                ; ----------------------------------------------------------------------------
1248                                READ_MEMORY
1249                                ;-----------------------------------------------------------------------------
1250                                ;Read command:
1251                                ; word 1 = command = 'RDM'
1252                                ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
1253                                ; word 3 = address in memory
1254                                ; word 4 = not used
1255                                ;Version query:
1256                                ; word 1 = 'VER'
1257                                ; word 2-4 unused
1258   
1259      P:000358 P:00035A 0BF080            JSR     SAVE_REGISTERS
                            000510
1260      P:00035A P:00035C 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 26



                            0004DC
1261   
1262      P:00035C P:00035E 44F400            MOVE              #'RDM',X0
                            52444D
1263      P:00035E P:000360 0D0329            JSR     VCOM_PREPARE_REPLY
1264      P:00035F P:000361 0D0337            JSR     VCOM_CHECK
1265      P:000360 P:000362 0AF0AA            JEQ     READ_MEMORY_XYP
                            00036A
1266   
1267      
1268      P:000362 P:000364 44F400            MOVE              #'VER',X0
                            564552
1269      P:000364 P:000366 0D0329            JSR     VCOM_PREPARE_REPLY
1270      P:000365 P:000367 0D0337            JSR     VCOM_CHECK
1271      P:000366 P:000368 0E2353            JNE     VCOM_EXIT
1272   
1273      P:000367 P:000369 44F000            MOVE              X:REV_NUMBER,X0
                            000003
1274      P:000369 P:00036B 0C0351            JMP     VCOM_EXIT_X0
1275   
1276                                READ_MEMORY_XYP
1277   
1278      
1279      P:00036A P:00036C 56F000            MOVE              X:DRXR_WD2,A
                            000008
1280      P:00036C P:00036E 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1281   
1282      P:00036E P:000370 0140C5            CMP     #'_X',A
                            005F58
1283      P:000370 P:000372 0AF0AA            JEQ     READ_MEMORY_X
                            00037D
1284   
1285      P:000372 P:000374 0140C5            CMP     #'_Y',A
                            005F59
1286      P:000374 P:000376 0AF0AA            JEQ     READ_MEMORY_Y
                            00037F
1287   
1288      P:000376 P:000378 0140C5            CMP     #'_P',A
                            005F50
1289      P:000378 P:00037A 0AF0AA            JEQ     READ_MEMORY_P
                            000381
1290   
1291      P:00037A P:00037C 44F400            MOVE              #'MTE',X0
                            4D5445
1292      P:00037C P:00037E 0C034C            JMP     VCOM_EXIT_ERROR_X0
1293   
1294                                READ_MEMORY_X
1295      P:00037D P:00037F 44E000            MOVE              X:(R0),X0
1296      P:00037E P:000380 0C0351            JMP     VCOM_EXIT_X0
1297                                READ_MEMORY_Y
1298      P:00037F P:000381 4CE000            MOVE                          Y:(R0),X0
1299      P:000380 P:000382 0C0351            JMP     VCOM_EXIT_X0
1300                                READ_MEMORY_P
1301      P:000381 P:000383 07E084            MOVE              P:(R0),X0
1302      P:000382 P:000384 0C0351            JMP     VCOM_EXIT_X0
1303   
1304   
1305                                ;--------------------------------------------------------------
1306                                WRITE_MEMORY
1307                                ;---------------------------------------------------------------
1308                                ; word 1 = command = 'WRM'
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 27



1309                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1310                                ; word 3 = address in memory
1311                                ; word 4 = value
1312   
1313      P:000383 P:000385 0BF080            JSR     SAVE_REGISTERS
                            000510
1314      P:000385 P:000387 45F400            MOVE              #'WRM',X1
                            57524D
1315      P:000387 P:000389 0D0346            JSR     VCOM_INTRO
1316      P:000388 P:00038A 0E2353            JNE     VCOM_EXIT
1317   
1318      
1319      P:000389 P:00038B 56F000            MOVE              X:DRXR_WD2,A
                            000008
1320      P:00038B P:00038D 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1321      P:00038D P:00038F 44F000            MOVE              X:DRXR_WD4,X0
                            00000A
1322   
1323      P:00038F P:000391 0140C5            CMP     #'_X',A
                            005F58
1324      P:000391 P:000393 0AF0AA            JEQ     WRITE_MEMORY_X
                            00039E
1325   
1326      P:000393 P:000395 0140C5            CMP     #'_Y',A
                            005F59
1327      P:000395 P:000397 0AF0AA            JEQ     WRITE_MEMORY_Y
                            0003A0
1328   
1329      P:000397 P:000399 0140C5            CMP     #'_P',A
                            005F50
1330      P:000399 P:00039B 0AF0AA            JEQ     WRITE_MEMORY_P
                            0003A2
1331   
1332      P:00039B P:00039D 44F400            MOVE              #'MTE',X0
                            4D5445
1333      P:00039D P:00039F 0C034C            JMP     VCOM_EXIT_ERROR_X0
1334   
1335                                WRITE_MEMORY_X
1336      P:00039E P:0003A0 446000            MOVE              X0,X:(R0)
1337      P:00039F P:0003A1 0C0351            JMP     VCOM_EXIT_X0
1338                                WRITE_MEMORY_Y
1339      P:0003A0 P:0003A2 4C6000            MOVE                          X0,Y:(R0)
1340      P:0003A1 P:0003A3 0C0351            JMP     VCOM_EXIT_X0
1341                                WRITE_MEMORY_P
1342      P:0003A2 P:0003A4 076084            MOVE              X0,P:(R0)
1343      P:0003A3 P:0003A5 0C0351            JMP     VCOM_EXIT_X0
1344   
1345   
1346                                ;-----------------------------------------------------------------------------
1347                                START_APPLICATION
1348                                ; an application should already have been downloaded to the PCI memory.
1349                                ; this command will execute it.
1350                                ; ----------------------------------------------------------------------
1351                                ; word 1 = command = 'GOA'
1352                                ; word 2-4 unused
1353   
1354      P:0003A4 P:0003A6 0BF080            JSR     SAVE_REGISTERS
                            000510
1355      P:0003A6 P:0003A8 45F400            MOVE              #'GOA',X1
                            474F41
1356   
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 28



1357      P:0003A8 P:0003AA 0D0346            JSR     VCOM_INTRO
1358      P:0003A9 P:0003AB 0E2353            JNE     VCOM_EXIT
1359   
1360      P:0003AA P:0003AC 0A7020            BSET    #MODE_APPLICATION,X:MODE
                            000001
1361      P:0003AC P:0003AE 000004            RTI                                       ; Application will reply.
1362   
1363   
1364                                ; ---------------------------------------------------------
1365                                STOP_APPLICATION
1366                                ; this command stops an application that is currently running
1367                                ; used for applications that once started run contiunually
1368                                ;-----------------------------------------------------------
1369                                ; word 1 = command = ' STP'
1370                                ; word 2-4 unused
1371   
1372      P:0003AD P:0003AF 0BF080            JSR     SAVE_REGISTERS
                            000510
1373      P:0003AF P:0003B1 45F400            MOVE              #'STP',X1
                            535450
1374   
1375      P:0003B1 P:0003B3 0D0346            JSR     VCOM_INTRO
1376      P:0003B2 P:0003B4 0E2353            JNE     VCOM_EXIT
1377   
1378      P:0003B3 P:0003B5 0A7000            BCLR    #MODE_APPLICATION,X:MODE
                            000001
1379      P:0003B5 P:0003B7 0A7000            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1380      P:0003B7 P:0003B9 0C0353            JMP     VCOM_EXIT
1381   
1382   
1383                                ;-----------------------------------------------------------------------------
1384                                RESET_CONTROLLER
1385                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1386                                ;-----------------------------------------------------------------------------
1387                                ; word 1 = command = 'RCO'
1388                                ; word 2-4 unused
1389   
1390      P:0003B8 P:0003BA 0BF080            JSR     SAVE_REGISTERS
                            000510
1391      P:0003BA P:0003BC 45F400            MOVE              #'RCO',X1
                            52434F
1392      P:0003BC P:0003BE 0D0346            JSR     VCOM_INTRO
1393      P:0003BD P:0003BF 0E2353            JNE     VCOM_EXIT
1394   
1395      P:0003BE P:0003C0 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1396      P:0003BF P:0003C1 000000            NOP
1397      P:0003C0 P:0003C2 000000            NOP
1398      P:0003C1 P:0003C3 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1399      P:0003C3 P:0003C5 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1400      P:0003C5 P:0003C7 446000            MOVE              X0,X:(R0)
1401      P:0003C6 P:0003C8 0606A0            REP     #6                                ; Wait for transmission to complete
1402      P:0003C7 P:0003C9 000000            NOP
1403      P:0003C8 P:0003CA 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1404   
1405                                ; Wait for a bit for MCE to be reset.......
1406      P:0003C9 P:0003CB 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1407      P:0003CB P:0003CD 06C400            DO      X0,L_DELAY
                            0003D1
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 29



1408      P:0003CD P:0003CF 06E883            DO      #1000,L_RDFIFO
                            0003D0
1409      P:0003CF P:0003D1 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1410      P:0003D0 P:0003D2 000000            NOP                                       ;   receiver empty
1411                                L_RDFIFO
1412      P:0003D1 P:0003D3 000000            NOP
1413                                L_DELAY
1414      P:0003D2 P:0003D4 000000            NOP
1415   
1416      P:0003D3 P:0003D5 44F400            MOVE              #'000',X0
                            303030
1417      P:0003D5 P:0003D7 0C0351            JMP     VCOM_EXIT_X0
1418   
1419                                ;-----------------------------------------------------------------------------
1420                                QUIET_TRANSFER_SET
1421                                ;-----------------------------------------------------------------------------
1422                                ;Quiet transfer mode configuration
1423                                ; word 1 = command = 'QTS'
1424                                ; word 2 = parameter to set
1425                                ; word 3-4 = arguments
1426   
1427      P:0003D6 P:0003D8 0BF080            JSR     SAVE_REGISTERS                    ; standard opening
                            000510
1428      P:0003D8 P:0003DA 45F400            MOVE              #'QTS',X1
                            515453
1429      P:0003DA P:0003DC 0D0346            JSR     VCOM_INTRO
1430      P:0003DB P:0003DD 0E2353            JNE     VCOM_EXIT
1431   
1432      P:0003DC P:0003DE 56F000            MOVE              X:DRXR_WD2,A            ; Parameter id
                            000008
1433      P:0003DE P:0003E0 44F000            MOVE              X:DRXR_WD3,X0           ; First arg
                            000009
1434      P:0003E0 P:0003E2 45F000            MOVE              X:DRXR_WD4,X1           ; Second arg
                            00000A
1435   
1436      P:0003E2 P:0003E4 0140C5            CMP     #'BAS',A
                            424153
1437      P:0003E4 P:0003E6 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            000457
1438   
1439      P:0003E6 P:0003E8 0140C5            CMP     #'DEL',A
                            44454C
1440      P:0003E8 P:0003EA 60F400            MOVE              #QT_BUF_SIZE,R0
                            00003E
1441      P:0003EA P:0003EC 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000455
1442   
1443      P:0003EC P:0003EE 0140C5            CMP     #'NUM',A
                            4E554D
1444      P:0003EE P:0003F0 60F400            MOVE              #QT_BUF_MAX,R0
                            00003F
1445      P:0003F0 P:0003F2 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000455
1446   
1447      P:0003F2 P:0003F4 0140C5            CMP     #'INF',A
                            494E46
1448      P:0003F4 P:0003F6 60F400            MOVE              #QT_INFORM,R0
                            000041
1449      P:0003F6 P:0003F8 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000455
1450   
1451      P:0003F8 P:0003FA 0140C5            CMP     #'SIZ',A
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 30



                            53495A
1452      P:0003FA P:0003FC 60F400            MOVE              #QT_FRAME_SIZE,R0
                            000040
1453      P:0003FC P:0003FE 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000455
1454   
1455      P:0003FE P:000400 0140C5            CMP     #'TAI',A
                            544149
1456      P:000400 P:000402 60F400            MOVE              #QT_BUF_TAIL,R0
                            000043
1457      P:000402 P:000404 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000455
1458   
1459      P:000404 P:000406 0140C5            CMP     #'HEA',A
                            484541
1460      P:000406 P:000408 60F400            MOVE              #QT_BUF_HEAD,R0
                            000042
1461      P:000408 P:00040A 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000455
1462   
1463      P:00040A P:00040C 0140C5            CMP     #'DRO',A
                            44524F
1464      P:00040C P:00040E 60F400            MOVE              #QT_DROPS,R0
                            000047
1465      P:00040E P:000410 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000455
1466   
1467      P:000410 P:000412 0140C5            CMP     #'PER',A
                            504552
1468      P:000412 P:000414 60F400            MOVE              #TCPR0,R0
                            FFFF8D
1469      P:000414 P:000416 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000455
1470   
1471      P:000416 P:000418 0140C5            CMP     #'FLU',A
                            464C55
1472      P:000418 P:00041A 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            00043E
1473   
1474      P:00041A P:00041C 0140C5            CMP     #'SET',A
                            534554
1475      P:00041C P:00041E 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            000446
1476   
1477      P:00041E P:000420 0140C5            CMP     #'RPS',A
                            525053
1478      P:000420 P:000422 60F400            MOVE              #RP_MAX_SIZE,R0
                            00004A
1479      P:000422 P:000424 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            000455
1480   
1481      P:000424 P:000426 0140C5            CMP     #'RPB',A
                            525042
1482      P:000426 P:000428 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_BASE
                            00042F
1483   
1484      P:000428 P:00042A 0140C5            CMP     #'RPE',A
                            525045
1485      P:00042A P:00042C 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_ENABLED
                            000434
1486   
1487      P:00042C P:00042E 44F400            MOVE              #'MTE',X0
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 31



                            4D5445
1488      P:00042E P:000430 0C034C            JMP     VCOM_EXIT_ERROR_X0
1489   
1490                                QUIET_TRANSFER_SET_RP_BASE
1491      P:00042F P:000431 447000            MOVE              X0,X:RP_BASE_LO
                            000048
1492      P:000431 P:000433 457000            MOVE              X1,X:RP_BASE_HI
                            000049
1493      P:000433 P:000435 0C0353            JMP     VCOM_EXIT
1494   
1495                                QUIET_TRANSFER_SET_RP_ENABLED
1496      P:000434 P:000436 0A7003            BCLR    #MODE_RP_BUFFER,X:MODE
                            000001
1497      P:000436 P:000438 208E00            MOVE              X0,A
1498      P:000437 P:000439 200003            TST     A
1499      P:000438 P:00043A 0EA353            JEQ     VCOM_EXIT
1500      P:000439 P:00043B 0A7023            BSET    #MODE_RP_BUFFER,X:MODE
                            000001
1501      P:00043B P:00043D 0A7015            BCLR    #RP_BUFFER_FULL,X:STATUS
                            000000
1502      P:00043D P:00043F 0C0353            JMP     VCOM_EXIT
1503   
1504                                QUIET_TRANSFER_SET_FLUSH
1505      P:00043E P:000440 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
1506      P:000440 P:000442 208E00            MOVE              X0,A
1507      P:000441 P:000443 200003            TST     A
1508      P:000442 P:000444 0EA353            JEQ     VCOM_EXIT
1509      P:000443 P:000445 0A7034            BSET    #QT_FLUSH,X:STATUS
                            000000
1510      P:000445 P:000447 0C0353            JMP     VCOM_EXIT
1511   
1512                                QUIET_TRANSFER_SET_ENABLED
1513      P:000446 P:000448 0A7002            BCLR    #MODE_QT,X:MODE
                            000001
1514      P:000448 P:00044A 0BF080            JSR     TIMER_DISABLE
                            000657
1515      P:00044A P:00044C 208E00            MOVE              X0,A
1516      P:00044B P:00044D 200003            TST     A
1517      P:00044C P:00044E 0EA353            JEQ     VCOM_EXIT
1518      P:00044D P:00044F 280000            MOVE              #0,A0
1519      P:00044E P:000450 0A7022            BSET    #MODE_QT,X:MODE
                            000001
1520      P:000450 P:000452 507000            MOVE              A0,X:TLR0
                            FFFF8E
1521      P:000452 P:000454 0BF080            JSR     TIMER_ENABLE
                            000651
1522      P:000454 P:000456 0C0353            JMP     VCOM_EXIT
1523   
1524                                QUIET_TRANSFER_SET_R0
1525      P:000455 P:000457 446000            MOVE              X0,X:(R0)
1526      P:000456 P:000458 0C0353            JMP     VCOM_EXIT
1527   
1528                                QUIET_TRANSFER_SET_BASE
1529      P:000457 P:000459 447000            MOVE              X0,X:QT_BASE_LO
                            00003C
1530      P:000459 P:00045B 457000            MOVE              X1,X:QT_BASE_HI
                            00003D
1531   
1532      P:00045B P:00045D 0BF080            JSR     BUFFER_RESET
                            00067D
1533   
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 32



1534      P:00045D P:00045F 0C0353            JMP     VCOM_EXIT
1535   
1536   
1537                                ;-----------------------------------------------------------------------------
1538                                SYSTEM_RESET
1539                                ;-----------------------------------------------------------------------------
1540   
1541      P:00045E P:000460 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1542      P:00045F P:000461 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1543                                                                                    ; set to zero except for interrupts
1544      P:000461 P:000463 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1545                                                                                    ; so first set to 0
1546      P:000462 P:000464 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1547                                                                                    ; therefore,return to initialization
1548      P:000464 P:000466 000000            NOP
1549      P:000465 P:000467 000004            RTI                                       ; return from ISR - to START
1550   
1551   
1552                                ;--------------------------------------------------------------------
1553                                CLEAN_UP_PCI
1554                                ;--------------------------------------------------------------------
1555                                ; Clean up the PCI board from wherever it was executing
1556   
1557      P:000466 P:000468 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1558      P:000467 P:000469 05F43D            MOVEC             #$000200,SSL            ; SR = zero except for interrupts
                            000200
1559      P:000469 P:00046B 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1560      P:00046A P:00046C 05F43C            MOVEC             #START,SSH              ; Set PC to for full initialization
                            000100
1561      P:00046C P:00046E 000000            NOP
1562      P:00046D P:00046F 000004            RTI
1563   
1564   
1565                                ; ------------------------------------------------------------------------------------
1566                                SEND_PACKET_TO_HOST
1567                                ; this command is received from the Host and actions the PCI board to pick up an address
1568                                ; pointer from DRXR which the PCI board then uses to write packets from the
1569                                ; MCE to the host memory starting at the address given.
1570                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1571                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1572                                ; HST after packet sent (unless error).
1573                                ; --------------------------------------------------------------------------------------
1574                                ; word 1 = command = 'HST'
1575                                ; word 2 = host high address
1576                                ; word 3 = host low address
1577                                ; word 4 = not used but read
1578   
1579                                ; save some registers but not B
1580   
1581      P:00046E P:000470 0D0510            JSR     <SAVE_REGISTERS                   ; save working registers
1582      P:00046F P:000471 45F400            MOVE              #'HST',X1
                            485354
1583      P:000471 P:000473 0D0346            JSR     VCOM_INTRO
1584      P:000472 P:000474 0E2353            JNE     VCOM_EXIT
1585   
1586      
1587      P:000473 P:000475 448800            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
1588      P:000474 P:000476 518900            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1589   
1590      P:000475 P:000477 447000            MOVE              X0,X:BURST_DEST_HI
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 33



                            00002F
1591      P:000477 P:000479 517000            MOVE              B0,X:BURST_DEST_LO
                            00002E
1592   
1593      P:000479 P:00047B 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1594   
1595      P:00047A P:00047C 0BF080            JSR     RESTORE_REGISTERS
                            000503
1596      P:00047C P:00047E 000004            RTI                                       ; Main loop will reply after packet transfer
!
1597   
1598   
1599                                ; --------------------------------------------------------------------
1600                                SOFTWARE_RESET
1601                                ;----------------------------------------------------------------------
1602                                ; word 1 = command = 'RST'
1603                                ; word 2-4 unused
1604   
1605      P:00047D P:00047F 0BF080            JSR     SAVE_REGISTERS
                            000510
1606      P:00047F P:000481 45F400            MOVE              #'RST',X1
                            525354
1607      P:000481 P:000483 0D0346            JSR     VCOM_INTRO
1608      P:000482 P:000484 0E2353            JNE     VCOM_EXIT
1609   
1610                                ; RST command OK so reply to host
1611                                FINISH_RST
1612      P:000483 P:000485 44F400            MOVE              #'000',X0
                            303030
1613      P:000485 P:000487 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1614      P:000487 P:000489 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            0004CA
1615   
1616      P:000489 P:00048B 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            000489
1617   
1618      P:00048B P:00048D 0A0100            BCLR    #MODE_APPLICATION,X:<MODE         ; clear app flag
1619      P:00048C P:00048E 0A0006            BCLR    #PREAMBLE_ERROR,X:<STATUS         ; clear preamble error
1620      P:00048D P:00048F 0A0000            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1621   
1622                                ; remember we are in a ISR so can't just jump to start.
1623   
1624      P:00048E P:000490 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1625      P:00048F P:000491 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1626                                                                                    ; set to zero except for interrupts
1627      P:000491 P:000493 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1628                                                                                    ; so first set to 0
1629      P:000492 P:000494 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1630                                                                                    ; therefore,return to initialization
1631      P:000494 P:000496 000000            NOP
1632      P:000495 P:000497 000004            RTI                                       ; return from ISR - to START
1633   
1634   
1635                                SEND_PACKET_TO_CONTROLLER
1636   
1637                                ;       Host command identifying location of an MCE command to send to
1638                                ;       the MCE.  Since this can come at any time, just record the
1639                                ;       request and then do the CONning from the main loop.
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 34



1640   
1641                                ; word 1 = command = 'CON'
1642                                ; word 2 = source host bus address, bits 31:16
1643                                ; word 3 = source host bus address, bits 15:0
1644                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1645                                ;        = '1' --> when MCE command is GO
1646   
1647      P:000496 P:000498 0D0510            JSR     <SAVE_REGISTERS                   ; save working registers
1648   
1649      
1650      P:000497 P:000499 45F400            MOVE              #'CON',X1
                            434F4E
1651      P:000499 P:00049B 0D0346            JSR     VCOM_INTRO
1652      P:00049A P:00049C 0E2353            JNE     VCOM_EXIT
1653   
1654      
1655      P:00049B P:00049D 44F400            MOVE              #'BUS',X0
                            425553
1656      P:00049D P:00049F 0A00AA            JSET    #CON_DEMAND,X:STATUS,VCOM_EXIT_ERROR_X0
                            00034C
1657   
1658      
1659      P:00049F P:0004A1 0A702A            BSET    #CON_DEMAND,X:STATUS
                            000000
1660      P:0004A1 P:0004A3 448800            MOVE              X:<DRXR_WD2,X0
1661      P:0004A2 P:0004A4 458900            MOVE              X:<DRXR_WD3,X1
1662      P:0004A3 P:0004A5 447000            MOVE              X0,X:CON_SRC_HI
                            00004D
1663      P:0004A5 P:0004A7 457000            MOVE              X1,X:CON_SRC_LO
                            00004C
1664   
1665                                ;       ;; Fourth word indicates if this is a go.  Who cares?
1666                                ;       MOVE    X:<DRXR_WD4,A           ; read word 4 - GO command?
1667                                ;       MOVE    #0,X0
1668                                ;       CMP     X0,A
1669                                ;       JEQ     BLOCK_CON
1670   
1671      
1672      P:0004A7 P:0004A9 0BF080            JSR     RESTORE_REGISTERS
                            000503
1673      P:0004A9 P:0004AB 000004            RTI
1674   
1676   
1677   
1678                                ;---------------------------------------------------------------
1679                                ;
1680                                ;                          * END OF ISRs *
1681                                ;
1682                                ;--------------------------------------------------------------
1683   
1684   
1685   
1686                                ;----------------------------------------------------------------
1687                                ;
1688                                ;                     * Beginning of SUBROUTINES *
1689                                ;
1690                                ;-----------------------------------------------------------------
1691   
1692   
1693                                CHECK_FO
1694      P:0004AA P:0004AC 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            0004C8
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 35



1695      P:0004AC P:0004AE 000000            NOP
1696      P:0004AD P:0004AF 000000            NOP
1697      P:0004AE P:0004B0 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            0004C8
1698   
1699      P:0004B0 P:0004B2 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1700      P:0004B1 P:0004B3 00000C            RTS
1701   
1702   
1703                                ;---------------------------------------------------------------
1704                                GET_FO_WRD
1705                                ;--------------------------------------------------------------
1706                                ; Anything in fibre receive FIFO?   If so store in X0
1707   
1708      P:0004B2 P:0004B4 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            0004C8
1709      P:0004B4 P:0004B6 000000            NOP
1710      P:0004B5 P:0004B7 000000            NOP
1711      P:0004B6 P:0004B8 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS             ; check twice for FO metastability.
                            0004C8
1712      P:0004B8 P:0004BA 0AF080            JMP     RD_FO_WD
                            0004C0
1713   
1714                                WT_FIFO
1715      P:0004BA P:0004BC 01AD80            JCLR    #EF,X:PDRD,*                      ; Wait till something in FIFO flagged
                            0004BA
1716      P:0004BC P:0004BE 000000            NOP
1717      P:0004BD P:0004BF 000000            NOP
1718      P:0004BE P:0004C0 01AD80            JCLR    #EF,X:PDRD,WT_FIFO                ; check twice.....
                            0004BA
1719   
1720                                ; Read one word from the fiber optics FIFO, check it and put it in A1
1721                                RD_FO_WD
1722      P:0004C0 P:0004C2 09443F            MOVEP             Y:RDFIFO,X0             ; then read to X0
1723      P:0004C1 P:0004C3 54F400            MOVE              #$00FFFF,A1             ; mask off top 2 bytes ($FC)
                            00FFFF
1724      P:0004C3 P:0004C5 200046            AND     X0,A                              ; since receiving 16 bits in 24bit register
1725      P:0004C4 P:0004C6 000000            NOP
1726      P:0004C5 P:0004C7 218400            MOVE              A1,X0
1727      P:0004C6 P:0004C8 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1728      P:0004C7 P:0004C9 00000C            RTS
1729                                CLR_FO_RTS
1730      P:0004C8 P:0004CA 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1731      P:0004C9 P:0004CB 00000C            RTS
1732   
1733   
1734                                ; ----------------------------------------------------------------------------
1735                                PCI_MESSAGE_TO_HOST
1736                                ;----------------------------------------------------------------------------
1737   
1738                                ; subroutine to send 4 words as a reply from PCI to the Host
1739                                ; using the DTXS-HRXS data path
1740                                ; PCI card writes here first then causes an interrupt INTA on
1741                                ; the PCI bus to alert the host to the reply message
1742   
1743      P:0004CA P:0004CC 0A85A3            JSET    #DCTR_HF3,X:DCTR,*                ; make sure host ready to receive interrupt
                            0004CA
1744                                                                                    ; cleared via fast interrupt if host out of 
its ISR
1745      P:0004CC P:0004CE 60F400            MOVE              #>DTXS_WD1,R0
                            00000B
1746   
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 36



1747      P:0004CE P:0004D0 060480            DO      #4,PCI_MESSAGE_TO_HOST_RESTORE
                            0004D2
1748      P:0004D0 P:0004D2 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            0004D0
1749      P:0004D2 P:0004D4 08D88D            MOVEP             X:(R0)+,X:DTXS
1750   
1751                                PCI_MESSAGE_TO_HOST_RESTORE
1752   
1753      
1754      P:0004D3 P:0004D5 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            00001D
1755      P:0004D5 P:0004D7 60F000            MOVE              X:SV_R0,R0              ; restore X0
                            000021
1756   
1757                                ; all the transmit words are in the FIFO, interrupt the Host
1758                                ; the Host should clear this interrupt once it is detected.
1759                                ; It does this by writing to HCVR to cause a fast interrupt.
1760   
1761                                                                                    ; set flag to handshake interrupt (INTA) wit
h host.
1762      P:0004D7 P:0004D9 0A8523            BSET    #DCTR_HF3,X:DCTR
1763                                                                                    ; only interrupt in irq mode
1764      P:0004D8 P:0004DA 0A0184            JCLR    #MODE_IRQ,X:MODE,PCI_MESSAGE_TO_HOST_RETURN
                            0004DB
1765      P:0004DA P:0004DC 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1766                                PCI_MESSAGE_TO_HOST_RETURN
1767      P:0004DB P:0004DD 00000C            RTS
1768   
1769   
1770                                ;---------------------------------------------------------------
1771                                RD_DRXR
1772                                ;--------------------------------------------------------------
1773                                ; Routine to read from HTXR-DRXR data path.  This is where the host
1774                                ; puts data prior to issuing a vector command.
1775                                ;
1776                                ; HCTR[HTF] determines how the data written by the host is decoded
1777                                ; here.  Typically HCTR = 0x900, meaning the 3 LSBs of each 32-bit
1778                                ; word written by the host are returned in each read of DRXR.
1779                                ;
1780                                ; We only check for non-empty FIFO here, so all 4 words must be
1781                                ; written to the FIFO before calling this routine.
1782   
1783      P:0004DC P:0004DE 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            0004DC
1784                                                                                    ; implies that host has written words
1785      P:0004DE P:0004E0 63F400            MOVE              #DRXR_WD1,R3
                            000007
1786      P:0004E0 P:0004E2 0604A0            REP     #4
1787      P:0004E1 P:0004E3 085B8B            MOVEP             X:DRXR,X:(R3)+
1788      P:0004E2 P:0004E4 00000C            RTS
1789   
1790                                ;---------------------------------------------------------------
1791                                READ_FROM_PCI
1792                                ;--------------------------------------------------------------
1793                                ; sub routine to read a 24 bit word in from PCI bus --> Y memory
1794                                ; 32bit host address in accumulator B.
1795   
1796                                ; read as master
1797   
1798      P:0004E3 P:0004E5 0C1890            EXTRACTU #$010010,B,A                     ; Get D31-16 bits only
                            010010
1799      P:0004E5 P:0004E7 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 37



1800   
1801      P:0004E6 P:0004E8 210C00            MOVE              A0,A1
1802      P:0004E7 P:0004E9 000000            NOP
1803      P:0004E8 P:0004EA 547000            MOVE              A1,X:DPMC               ; high 16bits of address in DSP master cntr 
reg.
                            FFFFC7
1804                                                                                    ; 32 bit read so FC1 = 0 and FC0 = 0
1805   
1806      P:0004EA P:0004EC 000000            NOP
1807      P:0004EB P:0004ED 0C1890            EXTRACTU #$010000,B,A
                            010000
1808      P:0004ED P:0004EF 000000            NOP
1809      P:0004EE P:0004F0 210C00            MOVE              A0,A1
1810      P:0004EF P:0004F1 0140C2            OR      #$060000,A                        ; A1 gets written to DPAR register
                            060000
1811      P:0004F1 P:0004F3 000000            NOP                                       ; C3-C0 of DPAR=0110 for memory read
1812      P:0004F2 P:0004F4 08CC08  WRT_ADD   MOVEP             A1,X:DPAR               ; Write address to PCI bus - PCI READ action
1813      P:0004F3 P:0004F5 000000            NOP                                       ; Pipeline delay
1814      P:0004F4 P:0004F6 0A8AA2  RD_PCI    JSET    #MRRQ,X:DPSR,GET_DAT              ; If MTRQ = 1 go read the word from host via
 FIFO
                            0004FD
1815      P:0004F6 P:0004F8 0A8A8A            JCLR    #TRTY,X:DPSR,RD_PCI               ; Bit is set if its a retry
                            0004F4
1816      P:0004F8 P:0004FA 08F48A            MOVEP             #$0400,X:DPSR           ; Clear bit 10 = target retry bit
                            000400
1817      P:0004FA P:0004FC 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for PCI addressing to be complete
                            0004FA
1818      P:0004FC P:0004FE 0C04F2            JMP     <WRT_ADD
1819   
1820      P:0004FD P:0004FF 08480B  GET_DAT   MOVEP             X:DRXR,A0               ; Read 1st 16 bits of 32 bit word from host 
memory
1821      P:0004FE P:000500 084C0B            MOVEP             X:DRXR,A1               ; Read 2nd 16 bits of 32 bit word from host 
memory
1822   
1823                                ; note that we now have 4 bytes in X0 and X1.
1824                                ; The 32bit word was in host memory in little endian format
1825                                ; If form LSB --> MSB the bytes are b1, b2, b3, b4 in host memory
1826                                ; in progressing through the HTRX/DRXR FIFO the
1827                                ; bytes end up like this.....
1828                                ; then X0 = $00 b2 b1
1829                                ; and  X1 = $00 b4 b3
1830   
1831      P:0004FF P:000501 0604A0            REP     #4                                ; increment PCI address by four bytes.
1832      P:000500 P:000502 000009            INC     B
1833      P:000501 P:000503 000000            NOP
1834      P:000502 P:000504 00000C            RTS
1835   
1836                                ;------------------------------------------------------------------------------------
1837                                RESTORE_REGISTERS
1838                                ;-------------------------------------------------------------------------------------
1839   
1840      P:000503 P:000505 05A239            MOVEC             X:<SV_SR,SR
1841   
1842      P:000504 P:000506 509700            MOVE              X:<SV_A0,A0
1843      P:000505 P:000507 549800            MOVE              X:<SV_A1,A1
1844      P:000506 P:000508 529900            MOVE              X:<SV_A2,A2
1845   
1846      P:000507 P:000509 519A00            MOVE              X:<SV_B0,B0
1847      P:000508 P:00050A 559B00            MOVE              X:<SV_B1,B1
1848      P:000509 P:00050B 539C00            MOVE              X:<SV_B2,B2
1849   
1850      P:00050A P:00050C 449D00            MOVE              X:<SV_X0,X0
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 38



1851      P:00050B P:00050D 459E00            MOVE              X:<SV_X1,X1
1852   
1853      P:00050C P:00050E 469F00            MOVE              X:<SV_Y0,Y0
1854      P:00050D P:00050F 47A000            MOVE              X:<SV_Y1,Y1
1855   
1856      P:00050E P:000510 60A100            MOVE              X:<SV_R0,R0
1857      P:00050F P:000511 00000C            RTS
1858   
1859                                ;-------------------------------------------------------------------------------------
1860                                SAVE_REGISTERS
1861                                ;-------------------------------------------------------------------------------------
1862   
1863      P:000510 P:000512 052239            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1864   
1865      P:000511 P:000513 501700            MOVE              A0,X:<SV_A0
1866      P:000512 P:000514 541800            MOVE              A1,X:<SV_A1
1867      P:000513 P:000515 521900            MOVE              A2,X:<SV_A2
1868   
1869      P:000514 P:000516 511A00            MOVE              B0,X:<SV_B0
1870      P:000515 P:000517 551B00            MOVE              B1,X:<SV_B1
1871      P:000516 P:000518 531C00            MOVE              B2,X:<SV_B2
1872   
1873      P:000517 P:000519 441D00            MOVE              X0,X:<SV_X0
1874      P:000518 P:00051A 451E00            MOVE              X1,X:<SV_X1
1875   
1876      P:000519 P:00051B 461F00            MOVE              Y0,X:<SV_Y0
1877      P:00051A P:00051C 472000            MOVE              Y1,X:<SV_Y1
1878   
1879      P:00051B P:00051D 602100            MOVE              R0,X:<SV_R0
1880      P:00051C P:00051E 00000C            RTS
1881   
1882                                ;-------------------------------------------------------
1883                                XMT_WD_FIBRE
1884                                ;-----------------------------------------------------
1885                                ; 250 MHz code - Transmit contents of Accumulator A1 to the MCE
1886                                ; we want to send 32bit word in little endian fomat to the host.
1887                                ; i.e. b4b3b2b1 goes b1, b2, b3, b4
1888                                ; currently the bytes are in this order:
1889                                ;  A0 = $00 b2 b1
1890                                ;  A1 = $00 b4 b3
1891                                ;  A = $00 00 b4 b3 00 b2 b1
1892   
1893      
1894   
1895      P:00051D P:00051F 212400            MOVE              B0,X0                   ; Save B
1896      P:00051E P:000520 21A500            MOVE              B1,X1
1897   
1898      P:00051F P:000521 0C1D31            ASL     #24,A,B
1899      P:000520 P:000522 0140CE            AND     #>$0000FF,B                       ; B1=b1
                            0000FF
1900      P:000522 P:000524 557000            MOVE              B1,X:FO_SEND
                            FFF000
1901   
1902      P:000524 P:000526 0C1D21            ASL     #16,A,B
1903      P:000525 P:000527 0140CE            AND     #>$0000FF,B
                            0000FF
1904      P:000527 P:000529 557000            MOVE              B1,X:FO_SEND            ; B1=b2
                            FFF000
1905   
1906      P:000529 P:00052B 0C1C11            ASR     #8,A,B
1907      P:00052A P:00052C 0140C6            AND     #>$0000FF,A
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 39



                            0000FF
1908      P:00052C P:00052E 547000            MOVE              A1,X:FO_SEND            ; A1=b3
                            FFF000
1909   
1910      P:00052E P:000530 0140CE            AND     #>$0000FF,B
                            0000FF
1911      P:000530 P:000532 557000            MOVE              B1,X:FO_SEND            ; B1=b4
                            FFF000
1912   
1913      P:000532 P:000534 208900            MOVE              X0,B0                   ; Restore B
1914      P:000533 P:000535 20AD00            MOVE              X1,B1
1915      P:000534 P:000536 00000C            RTS
1916   
1917   
1918                                ;----------------------------------------------
1919                                FLUSH_PCI_FIFO
1920                                ;----------------------------------------------
1921      P:000535 P:000537 0A8A84            JCLR    #MARQ,X:DPSR,*
                            000535
1922      P:000537 P:000539 0A862E            BSET    #CLRT,X:DPCR
1923      P:000538 P:00053A 000000            NOP
1924      P:000539 P:00053B 0A86AE            JSET    #CLRT,X:DPCR,*
                            000539
1925      P:00053B P:00053D 00000C            RTS
1926   
1927                                ;----------------------------------------------
1928                                CLEAR_FO_FIFO
1929                                ;----------------------------------------------
1930      P:00053C P:00053E 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
1931      P:00053E P:000540 44F400            MOVE              #200000,X0
                            030D40
1932      P:000540 P:000542 06C400            DO      X0,*+3
                            000542
1933      P:000542 P:000544 000000            NOP
1934      P:000543 P:000545 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
1935      P:000545 P:000547 00000C            RTS
1936   
1937   
1938                                ;-----------------------------------------------
1939                                PCI_ERROR_CLEAR
1940                                ;-----------------------------------------------
1941      
1942      
1943      
1944      
1945      
1946      
1947   
1948      P:000546 P:000548 50F000            MOVE              X:DMA_ERRORS,A0
                            000034
1949      P:000548 P:00054A 000008            INC     A
1950      P:000549 P:00054B 000000            NOP
1951      P:00054A P:00054C 507000            MOVE              A0,X:DMA_ERRORS
                            000034
1952   
1953      P:00054C P:00054E 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            00055A
1954      P:00054E P:000550 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
                            000564
1955      P:000550 P:000552 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 40



                            00056E
1956      P:000552 P:000554 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            000578
1957      P:000554 P:000556 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            000582
1958      P:000556 P:000558 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            00058C
1959      P:000558 P:00055A 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            000596
1960   
1961                                ERROR_TRTY
1962      P:00055A P:00055C 50F000            MOVE              X:EC_TRTY,A0
                            000035
1963      P:00055C P:00055E 000008            INC     A
1964      P:00055D P:00055F 08F48A            MOVEP             #>$0400,X:DPSR          ; Clear target retry error bit
                            000400
1965      P:00055F P:000561 507000            MOVE              A0,X:EC_TRTY
                            000035
1966      P:000561 P:000563 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1967      P:000563 P:000565 00000C            RTS
1968                                ERROR_TO
1969      P:000564 P:000566 50F000            MOVE              X:EC_TO,A0
                            000036
1970      P:000566 P:000568 000008            INC     A
1971      P:000567 P:000569 08F48A            MOVEP             #>$0800,X:DPSR          ; Clear timeout error bit
                            000800
1972      P:000569 P:00056B 507000            MOVE              A0,X:EC_TO
                            000036
1973      P:00056B P:00056D 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1974      P:00056D P:00056F 00000C            RTS
1975                                ERROR_TDIS
1976      P:00056E P:000570 50F000            MOVE              X:EC_TDIS,A0
                            000037
1977      P:000570 P:000572 000008            INC     A
1978      P:000571 P:000573 08F48A            MOVEP             #>$0200,X:DPSR          ; Clear target disconnect bit
                            000200
1979      P:000573 P:000575 507000            MOVE              A0,X:EC_TDIS
                            000037
1980      P:000575 P:000577 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1981      P:000577 P:000579 00000C            RTS
1982                                ERROR_TAB
1983      P:000578 P:00057A 50F000            MOVE              X:EC_TAB,A0
                            000038
1984      P:00057A P:00057C 000008            INC     A
1985      P:00057B P:00057D 08F48A            MOVEP             #>$0100,X:DPSR          ; Clear target abort error bit
                            000100
1986      P:00057D P:00057F 507000            MOVE              A0,X:EC_TAB
                            000038
1987      P:00057F P:000581 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1988      P:000581 P:000583 00000C            RTS
1989                                ERROR_MAB
1990      P:000582 P:000584 50F000            MOVE              X:EC_MAB,A0
                            000039
1991      P:000584 P:000586 000008            INC     A
1992      P:000585 P:000587 08F48A            MOVEP             #>$0080,X:DPSR          ; Clear master abort error bit
                            000080
1993      P:000587 P:000589 507000            MOVE              A0,X:EC_MAB
                            000039
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 41



1994      P:000589 P:00058B 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1995      P:00058B P:00058D 00000C            RTS
1996                                ERROR_DPER
1997      P:00058C P:00058E 50F000            MOVE              X:EC_DPER,A0
                            00003A
1998      P:00058E P:000590 000008            INC     A
1999      P:00058F P:000591 08F48A            MOVEP             #>$0040,X:DPSR          ; Clear data parity error bit
                            000040
2000      P:000591 P:000593 507000            MOVE              A0,X:EC_DPER
                            00003A
2001      P:000593 P:000595 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2002      P:000595 P:000597 00000C            RTS
2003                                ERROR_APER
2004      P:000596 P:000598 50F000            MOVE              X:EC_APER,A0
                            00003B
2005      P:000598 P:00059A 000008            INC     A
2006      P:000599 P:00059B 08F48A            MOVEP             #>$0020,X:DPSR          ; Clear address parity error bit
                            000020
2007      P:00059B P:00059D 507000            MOVE              A0,X:EC_APER
                            00003B
2008      P:00059D P:00059F 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2009      P:00059F P:0005A1 00000C            RTS
2010   
2011   
2012                                ;----------------------------------------------
2013                                BLOCK_TRANSFER
2014                                ;----------------------------------------------
2015                                ;   In:
2016                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
2017                                ;   - BLOCK_SIZE is packet size, in bytes
2018                                ;   - BURST_SRC is start of data in Y memory
2019                                ;  Out:
2020                                ;   - BLOCK_SIZE will be decremented to zero.
2021                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
2022                                ;   - BURST_SRC will be incremented by BLOCK_SIZE/2
2023                                ;  Trashes:
2024                                ;   - A and B
2025   
2026      
2027      P:0005A0 P:0005A2 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002C
2028   
2029      P:0005A2 P:0005A4 014085            CMP     #0,A
2030      P:0005A3 P:0005A5 0AF0AA            JEQ     BLOCK_DONE
                            0005E7
2031   
2032      
2033   
2034      P:0005A5 P:0005A7 20001B            CLR     B
2035      P:0005A6 P:0005A8 55F000            MOVE              X:PCI_BURST_SIZE,B1
                            00004E
2036   
2037      P:0005A8 P:0005AA 200005            CMP     B,A                               ; A ? B
2038      P:0005A9 P:0005AB 0E15AB            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
2039      P:0005AA P:0005AC 21CF00            MOVE              A,B                     ; This only moves A1,B1.
2040                                BLOCK_TRANSFER1
2041      P:0005AB P:0005AD 200014            SUB     B,A                               ; A -= B
2042      P:0005AC P:0005AE 014088            ADD     #0,B                              ; Clear carry bit
2043      P:0005AD P:0005AF 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 42



                            00002C
2044      P:0005AF P:0005B1 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002D
2045      P:0005B1 P:0005B3 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
2046   
2047      
2048      P:0005B2 P:0005B4 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
2049      P:0005B4 P:0005B6 50F000            MOVE              X:YMEM_SRC,A0
                            000032
2050      P:0005B6 P:0005B8 08C82F            MOVEP             A0,X:DSR0               ; DMA source
2051      P:0005B7 P:0005B9 200010            ADD     B,A
2052      P:0005B8 P:0005BA 00000B            DEC     B
2053      P:0005B9 P:0005BB 507000            MOVE              A0,X:YMEM_SRC           ; BURST_SRC += BURST_SIZE/2
                            000032
2054   
2055      P:0005BB P:0005BD 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
2056   
2057      
2058      P:0005BC P:0005BE 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
2059   
2060                                BLOCK_PCI
2061      
2062      P:0005BE P:0005C0 200013            CLR     A
2063      P:0005BF P:0005C1 20001B            CLR     B
2064      P:0005C0 P:0005C2 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            00002D
2065      P:0005C2 P:0005C4 00000B            DEC     B                                 ; n8 - 1
2066      P:0005C3 P:0005C5 014088            ADD     #0,B                              ; Clear carry
2067      P:0005C4 P:0005C6 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
2068      P:0005C5 P:0005C7 014088            ADD     #0,B                              ; Clear carry
2069      P:0005C6 P:0005C8 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
2070   
2071      P:0005C7 P:0005C9 50F000            MOVE              X:BURST_DEST_HI,A0
                            00002F
2072   
2073      P:0005C9 P:0005CB 200010            ADD     B,A
2074      P:0005CA P:0005CC 000000            NOP
2075      P:0005CB P:0005CD 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
2076   
2077      P:0005CD P:0005CF 280700            MOVE              #$07,A0
2078      P:0005CE P:0005D0 014088            ADD     #0,B                              ; Clear carry
2079      P:0005CF P:0005D1 0C1D20            ASL     #16,A,A
2080      P:0005D0 P:0005D2 51F000            MOVE              X:BURST_DEST_LO,B0
                            00002E
2081      P:0005D2 P:0005D4 200010            ADD     B,A
2082      P:0005D3 P:0005D5 000000            NOP
2083   
2084      P:0005D4 P:0005D6 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
2085   
2086                                BLOCK_CHECK
2087      P:0005D5 P:0005D7 000000            NOP
2088      P:0005D6 P:0005D8 000000            NOP
2089      P:0005D7 P:0005D9 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for burst termination
                            0005D7
2090   
2091      
2092      P:0005D9 P:0005DB 0A8AAE            JSET    #MDT,X:DPSR,BLOCK_OK
                            0005E2
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 43



2093   
2094      P:0005DB P:0005DD 0D0546            JSR     PCI_ERROR_CLEAR
2095   
2096      P:0005DC P:0005DE 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
2097      P:0005DE P:0005E0 0E85E8            JCS     <BLOCK_RESTART
2098   
2099      P:0005DF P:0005E1 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
2100      P:0005E1 P:0005E3 0E85E9            JCS     <BLOCK_RESUME
2101   
2102                                BLOCK_OK
2103      P:0005E2 P:0005E4 50F000            MOVE              X:BURST_SIZE,A0         ; Pass # of words written to updater
                            00002D
2104      P:0005E4 P:0005E6 0BF080            JSR     BLOCK_UPDATE
                            0005FB
2105      P:0005E6 P:0005E8 0C05A0            JMP     BLOCK_TRANSFER                    ; Finish the block
2106                                BLOCK_DONE
2107      P:0005E7 P:0005E9 00000C            RTS                                       ; Done
2108   
2109                                BLOCK_RESTART
2110      P:0005E8 P:0005EA 0C05BE            JMP     BLOCK_PCI                         ; Recalculate pci and resend
2111   
2112                                BLOCK_RESUME
2113      P:0005E9 P:0005EB 200013            CLR     A
2114      P:0005EA P:0005EC 20001B            CLR     B
2115      P:0005EB P:0005ED 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
2116      P:0005EC P:0005EE 0A8A8F            JCLR    #RDCQ,X:DPSR,BLOCK_RESUME1
                            0005EF
2117   
2118      P:0005EE P:0005F0 000009            INC     B
2119   
2120                                BLOCK_RESUME1
2121   
2122      P:0005EF P:0005F1 000009            INC     B                                 ; We want N, not N-1.
2123      P:0005F0 P:0005F2 014088            ADD     #0,B                              ; Clear carry
2124      P:0005F1 P:0005F3 0C1C20            ASR     #16,A,A
2125      P:0005F2 P:0005F4 200018            ADD     A,B                               ; B is words remaining
2126      P:0005F3 P:0005F5 014088            ADD     #0,B                              ; Clear carry
2127      P:0005F4 P:0005F6 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
2128      P:0005F5 P:0005F7 50F000            MOVE              X:BURST_SIZE,A0
                            00002D
2129      P:0005F7 P:0005F9 200014            SUB     B,A                               ; A is words written
2130   
2131      P:0005F8 P:0005FA 0BF080            JSR     BLOCK_UPDATE
                            0005FB
2132      P:0005FA P:0005FC 0C05BE            JMP     BLOCK_PCI                         ; Recalculate pci and resend
2133   
2134                                ; BLOCK_UPDATE
2135                                ;  Subtract A from BURST_SIZE and add A to BURST_DEST_LO
2136                                ;  Caller can check Z flag to see if BURST_SIZE is now 0.
2137                                BLOCK_UPDATE
2138      P:0005FB P:0005FD 210500            MOVE              A0,X1                   ; Save A for later
2139      P:0005FC P:0005FE 0C1D01            ASL     #0,A,B                            ; MOVE A,B
2140   
2141      P:0005FD P:0005FF 60F400            MOVE              #BURST_DEST_LO,R0       ;
                            00002E
2142      P:0005FF P:000601 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates BURST_DEST to BURST_DEST + B
                            0006B3
2143   
2144      P:000601 P:000603 57F000            MOVE              X:BURST_SIZE,B
                            00002D
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 44



2145      P:000603 P:000605 20006C            SUB     X1,B                              ; Zero flag must be preserved!
2146      P:000604 P:000606 000000            NOP
2147      P:000605 P:000607 557000            MOVE              B1,X:BURST_SIZE
                            00002D
2148   
2149      P:000607 P:000609 00000C            RTS
2150   
2151   
2152                                ;----------------------------------------------;
2153                                ;  MCE PACKET PROCESSING                       ;
2154                                ;----------------------------------------------;
2155   
2156                                ;       Given a dword count in A, computes number of half FIFOs and
2157                                ;       number of left over FIFO reads required to get the whole
2158                                ;       packet.
2159   
2160                                ;       Input: A is packet size, in dwords
2161                                ;       Output: sets X:TOTAL_BUFFS and X:LEFT_TO_READ
2162                                ;       Trashes: A,B,X0
2163   
2164   
2165                                PACKET_PARTITIONS
2166      P:000608 P:00060A 507000            MOVE              A0,X:PACKET_SIZE
                            00002B
2167   
2168      P:00060A P:00060C 014088            ADD     #0,B                              ; Clear carry
2169      P:00060B P:00060D 0C1D02            ASL     #1,A,A                            ;  * 2
2170      P:00060C P:00060E 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
2171      P:00060D P:00060F 240000            MOVE              #0,X0
2172      P:00060E P:000610 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
2173   
2174      P:000610 P:000612 557000            MOVE              B1,X:TOTAL_BUFFS
                            000027
2175      P:000612 P:000614 507000            MOVE              A0,X:LEFT_TO_READ
                            000028
2176      P:000614 P:000616 00000C            RTS
2177   
2178   
2179                                ; BUFFER_PACKET
2180                                ;
2181                                ; Copies the packet in the FIFO to Y memory.
2182   
2183                                ; In: TOTAL_BUFFS and LEFT_TO_READ must be pre-set (via PACKET_PARTITIONS);
2184                                ;     R1 is the destination index in Y memory.
2185                                ; Trashes: R1 is updated to point to the end of the copied data.
2186   
2187                                BUFFER_PACKET
2188      P:000615 P:000617 062700            DO      X:TOTAL_BUFFS,BUFFER_PACKET_HALFS_DONE
                            00061B
2189      P:000617 P:000619 0BF080            JSR     WAIT_FIFO_HALF
                            000633
2190      P:000619 P:00061B 0BF080            JSR     BUFFER_PACKET_HALF
                            00062E
2191      P:00061B P:00061D 000000            NOP
2192                                BUFFER_PACKET_HALFS_DONE
2193   
2194      
2195      
2196      P:00061C P:00061E 01AD81            JCLR    #HF,X:PDRD,BUFFER_PACKET_SINGLES_FAST
                            00062A
2197      P:00061E P:000620 062800            DO      X:LEFT_TO_READ,BUFFER_PACKET_DONE
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 45



                            000628
2198                                BUFFER_PACKET_SINGLE
2199      P:000620 P:000622 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            000315
2200      P:000622 P:000624 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE
                            000620
2201      P:000624 P:000626 000000            NOP
2202      P:000625 P:000627 000000            NOP
2203      P:000626 P:000628 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE   ; Protect against metastability
                            000620
2204      P:000628 P:00062A 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2205                                BUFFER_PACKET_DONE
2206      P:000629 P:00062B 00000C            RTS
2207   
2208                                BUFFER_PACKET_SINGLES_FAST
2209      P:00062A P:00062C 062800            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_FAST_DONE
                            00062C
2210      P:00062C P:00062E 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2211                                BUFFER_PACKET_SINGLES_FAST_DONE
2212      P:00062D P:00062F 00000C            RTS
2213   
2214                                BUFFER_PACKET_HALF
2215      
2216      P:00062E P:000630 060082            DO      #512,BUFFER_PACKET_HALF_DONE
                            000631
2217      P:000630 P:000632 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2218      P:000631 P:000633 000000            NOP
2219                                BUFFER_PACKET_HALF_DONE
2220      P:000632 P:000634 00000C            RTS
2221   
2222                                WAIT_FIFO_HALF
2223      P:000633 P:000635 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            00063C
2224      P:000635 P:000637 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
                            000633
2225      P:000637 P:000639 000000            NOP
2226      P:000638 P:00063A 000000            NOP
2227      P:000639 P:00063B 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            000633
2228      P:00063B P:00063D 00000C            RTS
2229   
2230                                FATALITY_HANDLER
2231      P:00063C P:00063E 0C0100            JMP     START                             ; What could possibly go wrong?
2232   
2233   
2234                                ; DROP_PACKET
2235                                ;
2236                                ; Reads a packet from the fifo, discarding it.
2237                                ;
2238                                ; In: TOTAL_BUFFS & LEFT_TO_READ
2239                                ; Trashes: A0
2240   
2241                                DROP_PACKET
2242      P:00063D P:00063F 062700            DO      X:TOTAL_BUFFS,DROP_PACKET_SINGLES
                            000642
2243      P:00063F P:000641 0D0633            JSR     WAIT_FIFO_HALF
2244      P:000640 P:000642 0BF080            JSR     DROP_FIFO_HALF
                            00064D
2245      P:000642 P:000644 000000            NOP
2246                                DROP_PACKET_SINGLES
2247      P:000643 P:000645 062800            DO      X:LEFT_TO_READ,DROP_PACKET_DONE
                            00064B
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 46



2248                                DROP_PACKET_SINGLE
2249      P:000645 P:000647 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            000315
2250      P:000647 P:000649 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE
                            000645
2251      P:000649 P:00064B 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE     ; Protect against metastability
                            000645
2252      P:00064B P:00064D 09483F            MOVEP             Y:RDFIFO,A0
2253                                DROP_PACKET_DONE
2254      P:00064C P:00064E 00000C            RTS
2255   
2256                                DROP_FIFO_HALF
2257      
2258      P:00064D P:00064F 060082            DO      #512,DROP_FIFO_DONE
                            00064F
2259      P:00064F P:000651 09483F            MOVEP             Y:RDFIFO,A0
2260                                DROP_FIFO_DONE
2261      P:000650 P:000652 00000C            RTS
2262   
2263   
2264                                ;----------------------------------------------;
2265                                ;  TIMER HANDLING                              ;
2266                                ;----------------------------------------------;
2267   
2268                                ; Start value is TLR, count is in TCR, int occurs at TCPR
2269                                ; Must set TCSR[TCIE] to enable int
2270                                ; Must set TCSR[T] for timer to restart
2271   
2272                                TIMER_ENABLE
2273      P:000651 P:000653 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2274      P:000653 P:000655 000000            NOP
2275      P:000654 P:000656 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2276      P:000656 P:000658 00000C            RTS
2277   
2278                                TIMER_DISABLE
2279      P:000657 P:000659 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
                            300200
2280      P:000659 P:00065B 000000            NOP
2281      P:00065A P:00065C 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2282      P:00065C P:00065E 00000C            RTS
2283   
2285                                TIMER_ACTION
2286      P:00065D P:00065F 56F000            MOVE              X:QT_INFORM_IDX,A
                            000046
2287      P:00065F P:000661 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2288      P:000661 P:000663 000000            NOP
2289      P:000662 P:000664 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2290      P:000664 P:000666 0140C5            CMP     #>0,A                             ; If inform_idx != 0
                            000000
2291      P:000666 P:000668 0AF0AA            JEQ     TIMER_ACTION_OK
                            00066A
2292      P:000668 P:00066A 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2293                                TIMER_ACTION_OK
2294      P:00066A P:00066C 00000C            RTS
2295   
2296   
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 47



2297   
2298                                ;----------------------------------------------;
2299                                ;  CIRCULAR BUFFER HANDLING                    ;
2300                                ;----------------------------------------------;
2301   
2302                                BUFFER_INCR
2303   
2304      P:00066B P:00066D 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
                            000042
2305      P:00066D P:00066F 014180            ADD     #1,A                              ;
2306      P:00066E P:000670 57F000            MOVE              X:QT_BUF_MAX,B          ;
                            00003F
2307      P:000670 P:000672 20000D            CMP     A,B                               ;
2308      P:000671 P:000673 0AF0AF            JLE     BUFFER_RESET                      ;       head = 0
                            00067D
2309                                                                                    ; else
2310      P:000673 P:000675 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            000042
2311   
2312      P:000675 P:000677 20001B            CLR     B
2313      P:000676 P:000678 51F000            MOVE              X:QT_BUF_SIZE,B0
                            00003E
2314      P:000678 P:00067A 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2315      P:00067A P:00067C 0BF080            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
                            0006B3
2316   
2317      P:00067C P:00067E 00000C            RTS
2318   
2319   
2320                                BUFFER_RESET
2321      P:00067D P:00067F 60F400            MOVE              #QT_BASE_LO,R0
                            00003C
2322      P:00067F P:000681 0BF080            JSR     LOAD_HILO_ADDRESS
                            0006AD
2323      P:000681 P:000683 60F400            MOVE              #QT_DEST_LO,R0
                            000044
2324      P:000683 P:000685 0BF080            JSR     SAVE_HILO_ADDRESS                 ; QT_DEST_LO = QT_BASE_LO
                            0006B5
2325   
2326      P:000685 P:000687 240000            MOVE              #0,X0
2327      P:000686 P:000688 447000            MOVE              X0,X:QT_BUF_HEAD        ; HEAD = 0
                            000042
2328      P:000688 P:00068A 00000C            RTS
2329   
2330   
2331                                BUFFER_INFORM_CHECK
2332      P:000689 P:00068B 56F000            MOVE              X:QT_INFORM_IDX,A
                            000046
2333      P:00068B P:00068D 014180            ADD     #1,A
2334      P:00068C P:00068E 57F000            MOVE              X:QT_INFORM,B
                            000041
2335      P:00068E P:000690 20000D            CMP     A,B
2336      P:00068F P:000691 0AF0A7            JGT     BUFFER_INFORM_OK                  ; If inform_idx + 1 <= inform
                            000693
2337      P:000691 P:000693 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2338   
2339                                BUFFER_INFORM_OK
2340      P:000693 P:000695 567000            MOVE              A,X:QT_INFORM_IDX       ; inform_idx = inform_idx + 1
                            000046
2341      P:000695 P:000697 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 48



2342   
2343   
2344                                ;---------------------------------------------------------------
2345                                BUFFER_INFORM
2346                                ;---------------------------------------------------------------
2347                                ; Informs host of current buffer status
2348   
2349      P:000696 P:000698 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2350      P:000698 P:00069A 440B00            MOVE              X0,X:<DTXS_WD1
2351   
2352      P:000699 P:00069B 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            000042
2353      P:00069B P:00069D 440C00            MOVE              X0,X:<DTXS_WD2
2354   
2355      P:00069C P:00069E 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000043
2356      P:00069E P:0006A0 440D00            MOVE              X0,X:<DTXS_WD3
2357   
2358      P:00069F P:0006A1 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
                            000047
2359      P:0006A1 P:0006A3 440E00            MOVE              X0,X:<DTXS_WD4
2360   
2361   
2362      P:0006A2 P:0006A4 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            0006AC
2363      P:0006A4 P:0006A6 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            0006AC
2364   
2365      P:0006A6 P:0006A8 0D04CA            JSR     PCI_MESSAGE_TO_HOST
2366   
2367      P:0006A7 P:0006A9 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
2368      P:0006A9 P:0006AB 240000            MOVE              #0,X0                   ; Reset inform index
2369      P:0006AA P:0006AC 447000            MOVE              X0,X:QT_INFORM_IDX
                            000046
2370                                INFORM_EXIT
2371      P:0006AC P:0006AE 00000C            RTS
2372   
2373   
2374   
2375                                ;----------------------------------------------;
2376                                ;  ADDRESS HANDLING                            ;
2377                                ;----------------------------------------------;
2378   
2382   
2383                                LOAD_HILO_ADDRESS
2384      
2385      
2386      P:0006AD P:0006AF 200013            CLR     A
2387      P:0006AE P:0006B0 50D800            MOVE              X:(R0)+,A0
2388      P:0006AF P:0006B1 44D000            MOVE              X:(R0)-,X0
2389      P:0006B0 P:0006B2 0C1940            INSERT  #$010010,X0,A
                            010010
2390      P:0006B2 P:0006B4 00000C            RTS
2391   
2392                                ADD_HILO_ADDRESS
2393      
2394      
2395   
2396      P:0006B3 P:0006B5 0D06AD            JSR     LOAD_HILO_ADDRESS
2397      P:0006B4 P:0006B6 200010            ADD     B,A
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  main.asm  Page 49



2398   
2399                                SAVE_HILO_ADDRESS
2400      
2401      
2402   
2403      P:0006B5 P:0006B7 445800            MOVE              X0,X:(R0)+              ; pre-increment
2404      P:0006B6 P:0006B8 240000            MOVE              #0,X0
2405      P:0006B7 P:0006B9 0C1D11            ASL     #8,A,B
2406      P:0006B8 P:0006BA 0C1940            INSERT  #$008010,X0,A
                            008010
2407      P:0006BA P:0006BC 555000            MOVE              B1,X:(R0)-              ; store hi16
2408      P:0006BB P:0006BD 506000            MOVE              A0,X:(R0)
2409      P:0006BC P:0006BE 0C1C90            ASR     #8,B,A
2410      P:0006BD P:0006BF 00000C            RTS
2411   
2412   
2413                                BOOTCODE_END
2414                                 BOOTEND_ADDR
2415      0006BE                              EQU     @CVI(BOOTCODE_END)
2416   
2417                                PROGRAM_END
2418      0006BE                    PEND_ADDR EQU     @CVI(PROGRAM_END)
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
2433      X:000000 P:0006C0                   ORG     X:VAR_TBL,P:
2434   
2435   
2436                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2437                                 VAR_TBL_START
2438      0006BE                              EQU     @LCV(L)-2
2439                                          ENDIF
2440   
2441                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2443                                          ENDIF
2444   
2445                                ; -----------------------------------------------
2446 d    X:000000 P:0006C0 000000  STATUS    DC      0                                 ; Internal control flags
2447 d    X:000001 P:0006C1 000000  MODE      DC      0                                 ; Configure special options
2448   
2449 d                               FRAME_COUNT
2450 d    X:000002 P:0006C2 000000            DC      0
2451 d    X:000003 P:0006C3 550105  REV_NUMBER DC     $550105                           ; byte 0 = minor revision #
2452                                                                                    ; byte 1 = major revision #
2453                                                                                    ; byte 2 = release Version (ascii letter)
2454 d    X:000004 P:0006C4 000000  REV_DATA  DC      $000000                           ; data: day-month-year
2455 d    X:000005 P:0006C5 2EF490  P_CHECKSUM DC     $2EF490                           ;**** DO NOT CHANGE
2456                                ; -------------------------------------------------
2457 d    X:000006 P:0006C6 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2458                                ; ----------------------------------------------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  vars.asm  Page 50



----------------
2459   
2460 d    X:000007 P:0006C7 000000  DRXR_WD1  DC      0
2461 d    X:000008 P:0006C8 000000  DRXR_WD2  DC      0
2462 d    X:000009 P:0006C9 000000  DRXR_WD3  DC      0
2463 d    X:00000A P:0006CA 000000  DRXR_WD4  DC      0
2464 d    X:00000B P:0006CB 000000  DTXS_WD1  DC      0
2465 d    X:00000C P:0006CC 000000  DTXS_WD2  DC      0
2466 d    X:00000D P:0006CD 000000  DTXS_WD3  DC      0
2467 d    X:00000E P:0006CE 000000  DTXS_WD4  DC      0
2468   
2469 d    X:00000F P:0006CF 000000  HEAD_W1_0 DC      0                                 ; Preamble $A5A5
2470 d    X:000010 P:0006D0 000000  HEAD_W1_1 DC      0                                 ;          $A5A5
2471 d    X:000011 P:0006D1 000000  HEAD_W2_0 DC      0                                 ;          $5A5A
2472 d    X:000012 P:0006D2 000000  HEAD_W2_1 DC      0                                 ;          $5A5A
2473 d    X:000013 P:0006D3 000000  HEAD_W3_0 DC      0                                 ; 'RP' or 'DA'
2474 d    X:000014 P:0006D4 000000  HEAD_W3_1 DC      0                                 ; '  '   $2020
2475 d    X:000015 P:0006D5 000000  HEAD_W4_0 DC      0                                 ; Packet size LSW
2476 d    X:000016 P:0006D6 000000  HEAD_W4_1 DC      0                                 ;             MSW
2477   
2478 d    X:000017 P:0006D7 000000  SV_A0     DC      0
2479 d    X:000018 P:0006D8 000000  SV_A1     DC      0
2480 d    X:000019 P:0006D9 000000  SV_A2     DC      0
2481 d    X:00001A P:0006DA 000000  SV_B0     DC      0
2482 d    X:00001B P:0006DB 000000  SV_B1     DC      0
2483 d    X:00001C P:0006DC 000000  SV_B2     DC      0
2484 d    X:00001D P:0006DD 000000  SV_X0     DC      0
2485 d    X:00001E P:0006DE 000000  SV_X1     DC      0
2486 d    X:00001F P:0006DF 000000  SV_Y0     DC      0
2487 d    X:000020 P:0006E0 000000  SV_Y1     DC      0
2488 d    X:000021 P:0006E1 000000  SV_R0     DC      0
2489   
2490 d    X:000022 P:0006E2 000000  SV_SR     DC      0                                 ; stauts register save.
2491   
2492 d                               PACKET_SIZE_LOW
2493 d    X:000023 P:0006E3 000000            DC      0
2494 d                               PACKET_SIZE_HIH
2495 d    X:000024 P:0006E4 000000            DC      0
2496   
2497 d    X:000025 P:0006E5 00A5A5  PREAMB1   DC      $A5A5                             ; pramble 16-bit word....2 of which make up 
first preamble 32bit word
2498 d    X:000026 P:0006E6 005A5A  PREAMB2   DC      $5A5A                             ; preamble 16-bit word....2 of which make up
 second preamble 32bit word
2499   
2500 d                               TOTAL_BUFFS
2501 d    X:000027 P:0006E7 000000            DC      0                                 ; total number of 512 buffers in packet
2502 d                               LEFT_TO_READ
2503 d    X:000028 P:0006E8 000000            DC      0                                 ; number of words (16 bit) left to read afte
r last 512 buffer
2504 d                               LEFT_TO_WRITE
2505 d    X:000029 P:0006E9 000000            DC      0                                 ; number of woreds (32 bit) to write to host
 i.e. half of those left over read
2506 d                               NUM_LEFTOVER_BLOCKS
2507 d    X:00002A P:0006EA 000000            DC      0                                 ; small block DMA burst transfer
2508   
2509 d                               PACKET_SIZE
2510 d    X:00002B P:0006EB 000000            DC      0                                 ; Size, in dwords of most recent packet from
 MCE.
2511   
2512   
2514   
2515 d    X:00002C P:0006EC 000000  BLOCK_SIZE DC     0
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  vars.asm  Page 51



2516 d    X:00002D P:0006ED 000000  BURST_SIZE DC     0
2517 d                               BURST_DEST_LO
2518 d    X:00002E P:0006EE 000000            DC      0
2519 d                               BURST_DEST_HI
2520 d    X:00002F P:0006EF 000000            DC      0
2521 d                               BURST_SRC_LO
2522 d    X:000030 P:0006F0 000000            DC      0
2523 d                               BURST_SRC_HI
2524 d    X:000031 P:0006F1 000000            DC      0
2525 d    X:000032 P:0006F2 000000  YMEM_SRC  DC      0
2526 d    X:000033 P:0006F3 000000  YMEM_DEST DC      0
2527   
2528 d    X:000034 P:0006F4 000000  DMA_ERRORS DC     0
2529 d    X:000035 P:0006F5 000000  EC_TRTY   DC      0
2530 d    X:000036 P:0006F6 000000  EC_TO     DC      0
2531 d    X:000037 P:0006F7 000000  EC_TDIS   DC      0
2532 d    X:000038 P:0006F8 000000  EC_TAB    DC      0
2533 d    X:000039 P:0006F9 000000  EC_MAB    DC      0
2534 d    X:00003A P:0006FA 000000  EC_DPER   DC      0
2535 d    X:00003B P:0006FB 000000  EC_APER   DC      0
2536   
2537   
2539   
2540 d    X:00003C P:0006FC 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2541 d    X:00003D P:0006FD 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2542 d                               QT_BUF_SIZE
2543 d    X:00003E P:0006FE 000000            DC      0                                 ; Separation of buffers, in bytes
2544 d    X:00003F P:0006FF 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2545 d                               QT_FRAME_SIZE
2546 d    X:000040 P:000700 000000            DC      0                                 ; Expected data packet size, in bytes
2547 d    X:000041 P:000701 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2548   
2549 d                               QT_BUF_HEAD
2550 d    X:000042 P:000702 000000            DC      0                                 ; Index of buf for next write
2551 d                               QT_BUF_TAIL
2552 d    X:000043 P:000703 000000            DC      0                                 ; Index at which we must not write
2553   
2554 d    X:000044 P:000704 000000  QT_DEST_LO DC     0                                 ; PC address for next write
2555 d    X:000045 P:000705 000000  QT_DEST_HI DC     0                                 ;
2556 d                               QT_INFORM_IDX
2557 d    X:000046 P:000706 000000            DC      0                                 ; Number of packets since last inform
2558 d    X:000047 P:000707 000000  QT_DROPS  DC      0                                 ; Dropped packets
2559   
2560   
2562 d    X:000048 P:000708 000000  RP_BASE_LO DC     0
2563 d    X:000049 P:000709 000000  RP_BASE_HI DC     0
2564 d                               RP_MAX_SIZE
2565 d    X:00004A P:00070A 000000            DC      0
2566 d    X:00004B P:00070B 000000  RP_DROPS  DC      0
2567   
2569 d    X:00004C P:00070C 000000  CON_SRC_LO DC     0
2570 d    X:00004D P:00070D 000000  CON_SRC_HI DC     0
2571   
2573 d                               PCI_BURST_SIZE
2574 d    X:00004E P:00070E 000040            DC      $40                               ; Should be < 4*latency assigned by OS
2575   
2576 d    X:00004F P:00070F 000000  BDEBUG0   DC      0
2577 d    X:000050 P:000710 000000  BDEBUG1   DC      0
2578 d    X:000051 P:000711 000000  BDEBUG2   DC      0
2579 d    X:000052 P:000712 000000  BDEBUG3   DC      0
2580 d    X:000053 P:000713 000000  BDEBUG4   DC      0
2581 d    X:000054 P:000714 000000  BDEBUG5   DC      0
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  vars.asm  Page 52



2582 d    X:000055 P:000715 000000  BDEBUG6   DC      0
2583 d    X:000056 P:000716 000000  BDEBUG7   DC      0
2584 d    X:000057 P:000717 000000  BDEBUG8   DC      0
2585 d    X:000058 P:000718 000000  BDEBUG9   DC      0
2586   
2587                                ;----------------------------------------------------------
2588   
2590   
2591                                 APPLICATION_RUNNING
2592      000000                              EQU     0                                 ; Indicates application is in progress
2593                                 SEND_TO_HOST
2594      000001                              EQU     1                                 ; set in HST ISR when host ready for packet 
(stays set until after HST reply)
2595                                 FATAL_ERROR
2596      000002                              EQU     2                                 ; PCI message to host error detected by driv
er....
2597      000003                    FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays s
et till packet processed
2598   
2599                                 PREAMBLE_ERROR
2600      000006                              EQU     6                                 ; set if preamble error detected
2601      000007                    DATA_DLY  EQU     7                                 ; set in CON ISR if MCE command is 'GO'.  US
ed to add delay to first returned data packet
2602   
2603      000009                    HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of p
acket (stays set until after HST reply)
2604   
2605      00000A                    CON_DEMAND EQU    10                                ; Host has requested an MCE command be sent
2606   
2607                                 PCIDMA_RESTART
2608      000010                              EQU     16                                ; DMA flags used for error recovery
2609                                 PCIDMA_RESUME
2610      000011                              EQU     17
2611                                 PCIDMA_RETRY
2612      000012                              EQU     18
2613   
2614      000014                    QT_FLUSH  EQU     20                                ; Set when it is time to inform Host of curr
ent buffer position.
2615                                 RP_BUFFER_FULL
2616      000015                              EQU     21                                ; Set when Quiet RP buffer is occupied.
2617   
2618                                 MAIN_LOOP_POLL
2619      000017                              EQU     23                                ; Cleared by the main loop, use to check for
 DSP lock-up
2620   
2622   
2623                                 MODE_APPLICATION
2624      000000                              EQU     0                                 ; set if PCI application to run
2625      000001                    MODE_CHOKE EQU    1                                 ; drop all packets from MCE
2626      000002                    MODE_QT   EQU     2                                 ; Quiet transfer for data packets
2627                                 MODE_RP_BUFFER
2628      000003                              EQU     3                                 ; Quiet transfer for reply packets
2629      000004                    MODE_IRQ  EQU     4                                 ; Enable PCI interrupts on NFY
2630   
2631   
2632                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2633                                 VAR_TBL_END
2634      000717                              EQU     @LCV(L)-2
2635                                          ENDIF
2636   
2637                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2639                                          ENDIF
Motorola DSP56300 Assembler  Version 6.3.4   09-05-21  18:35:20  vars.asm  Page 53



2640   
2641                                 VAR_TBL_LENGTH
2642      000059                              EQU     VAR_TBL_END-VAR_TBL_START
2643                                          INCLUDE 'app.asm'
2644                                        COMMENT *
2645   
2646                                Auxiliary application area.
2647   
2648                                See info.asm for versioning and authors.
2649   
2650                                        *
2651                                          PAGE    132                               ; Printronix page width - 132 columns
2652                                          OPT     CEX                               ; print DC evaluations
2653   
2654                                          IF      @CVS(N,*)>=APPLICATION
2656                                          ENDIF
2657   
2658   
2659                                ;--------------------------------------------
2660                                ; APPLICATION AREA
2661                                ;---------------------------------------------
2662                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2663      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2664                                          ENDIF
2665   
2666                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2668                                          ENDIF
2669   
2670                                ; starts with no application loaded
2671                                ; so just reply with an error if we get a GOA command
2672   
2673      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2674      P:000802 P:000804 440B00            MOVE              X0,X:<DTXS_WD1          ; REPly
2675      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2676      P:000805 P:000807 440C00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2677      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2678      P:000808 P:00080A 440D00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2679      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2680      P:00080B P:00080D 440E00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2681      P:00080C P:00080E 0D0503            JSR     <RESTORE_REGISTERS
2682      P:00080D P:00080F 0D04CA            JSR     <PCI_MESSAGE_TO_HOST
2683      P:00080E P:000810 0A0100            BCLR    #MODE_APPLICATION,X:<MODE
2684      P:00080F P:000811 0C016C            JMP     PACKET_IN
2685   
2686   
2687      000812                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM
2688   
2689   
2690   

0    Errors
0    Warnings


