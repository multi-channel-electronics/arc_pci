Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  build.asm  Page 1



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
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  header.asm  Page 2



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
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  header.asm  Page 3



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
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  header.asm  Page 4



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
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  init.asm  Page 5



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
                            0003C6
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
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  init.asm  Page 6



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
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  init.asm  Page 7



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
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  init.asm  Page 8



362                                 ; Interrupt locations for 7 available commands on PCI board
363                                 ; Each JSR takes up 2 locations in the table
364       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            0002E3
365       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            0002B8
366       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            000304
367       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            00030D
368                                 ; software reset is the same as cleaning up the PCI - use same routine
369                                 ; when HOST does a RESET then this routine is run
370       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            0003DD
371       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            0003F6
372       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            0003CE
373       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            000318
374    
375                                 ; QT - set command
376       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            000336
377       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            0003BE
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
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  init.asm  Page 9



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
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  init.asm  Page 10



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
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  init.asm  Page 11



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
                            000615
543       P:000152 P:000154 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
544       P:000153 P:000155 065680            DO      #VAR_TBL_LENGTH,X_WRITE
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
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  init.asm  Page 12



560       P:00015F P:000161 013D04            BCLR    #AUX1,X:PDRC
561    
562                                 ;----------------------------------------------------------------------------
563                                 ; Initialize PCI controller again, after booting, to make sure it sticks
564       P:000160 P:000162 0A8514            BCLR    #20,X:DCTR                        ; Terminate and reset mode
565       P:000161 P:000163 000000            NOP
566       P:000162 P:000164 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            000162
567       P:000164 P:000166 000000            NOP
568       P:000165 P:000167 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
569       P:000166 P:000168 000000            NOP
570       P:000167 P:000169 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000167
571    
572       
573       P:000169 P:00016B 0BF080            JSR     CLEAR_FO_FIFO                     ; Clear the fibre fifo!
                            000494
574       P:00016B P:00016D 0BF080            JSR     TIMER_DISABLE                     ; Disable NFY timer
                            0005AE
575    
577                                           INCLUDE 'main.asm'
578                                         COMMENT *
579    
580                                 Main section of the pci card code.
581    
582                                 See info.asm for versioning and authors.
583    
584                                         *
585                                           PAGE    132                               ; Printronix page width - 132 columns
586                                           OPT     CEX                               ; print DC evaluations
587    
591    
592                                 PACKET_IN
593    
594       
595       P:00016D P:00016F 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
596    
597       
598       P:00016F P:000171 0A01A0            JSET    #MODE_APPLICATION,X:<MODE,APPLICATION
                            000800
599    
600       
601       P:000171 P:000173 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            0005B4
602    
603       
604       P:000173 P:000175 0B00B4            JSSET   #QT_FLUSH,X:STATUS,BUFFER_INFORM
                            0005ED
605    
606       
607       P:000175 P:000177 0D040A            JSR     <GET_FO_WRD
608       P:000176 P:000178 0B00A3            JSSET   #FO_WRD_RCV,X:STATUS,HANDLE_FIFO
                            00017D
609    
610       
611       P:000178 P:00017A 0B00AA            JSSET   #CON_DEMAND,X:STATUS,CON_NOW
                            000232
612    
613       
614       P:00017A P:00017C 000000            NOP
615       P:00017B P:00017D 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 13



616    
617       
618       P:00017C P:00017E 0C016D            JMP     PACKET_IN
619    
623    
624    
625    
627    
628                                 HANDLE_FIFO
629       P:00017D P:00017F 0A01A1            JSET    #MODE_CHOKE,X:<MODE,RETURN_NOW    ; IF MCE Packet choke on - just keep clearin
g FIFO.
                            0001B6
630       P:00017F P:000181 441000            MOVE              X0,X:<HEAD_W1_0         ;store received word
631       P:000180 P:000182 56F000            MOVE              X:PREAMB1,A
                            000025
632       P:000182 P:000184 200045            CMP     X0,A                              ; check it is correct
633       P:000183 P:000185 0E21B3            JNE     <PRE_ERROR                        ; if not go to start
634    
635       P:000184 P:000186 0D0412            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
636       P:000185 P:000187 440F00            MOVE              X0,X:<HEAD_W1_1         ;store received word
637       P:000186 P:000188 56F000            MOVE              X:PREAMB1,A
                            000025
638       P:000188 P:00018A 200045            CMP     X0,A                              ; check it is correct
639       P:000189 P:00018B 0E21B3            JNE     <PRE_ERROR                        ; if not go to start
640    
641       P:00018A P:00018C 0D0412            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
642       P:00018B P:00018D 441200            MOVE              X0,X:<HEAD_W2_0         ;store received word
643       P:00018C P:00018E 56F000            MOVE              X:PREAMB2,A
                            000026
644       P:00018E P:000190 200045            CMP     X0,A                              ; check it is correct
645       P:00018F P:000191 0E21B3            JNE     <PRE_ERROR                        ; if not go to start
646    
647       P:000190 P:000192 0D0412            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
648       P:000191 P:000193 441100            MOVE              X0,X:<HEAD_W2_1         ;store received word
649       P:000192 P:000194 56F000            MOVE              X:PREAMB2,A
                            000026
650       P:000194 P:000196 200045            CMP     X0,A                              ; check it is correct
651       P:000195 P:000197 0E21B3            JNE     <PRE_ERROR                        ; if not go to start
652    
653                                 PACKET_INFO                                         ; packet preamble valid
654    
655       P:000196 P:000198 0D0412            JSR     <WT_FIFO
656       P:000197 P:000199 441400            MOVE              X0,X:<HEAD_W3_0         ; RP or DA
657       P:000198 P:00019A 0D0412            JSR     <WT_FIFO
658       P:000199 P:00019B 441300            MOVE              X0,X:<HEAD_W3_1         ; $2020
659    
660       P:00019A P:00019C 0D0412            JSR     <WT_FIFO
661       P:00019B P:00019D 441600            MOVE              X0,X:<HEAD_W4_0         ; packet size lo
662       P:00019C P:00019E 0D0412            JSR     <WT_FIFO
663       P:00019D P:00019F 441500            MOVE              X0,X:<HEAD_W4_1         ; packet size hi
664    
665       
666       P:00019E P:0001A0 200013            CLR     A
667       P:00019F P:0001A1 50F000            MOVE              X:HEAD_W4_0,A0
                            000016
668       P:0001A1 P:0001A3 44F000            MOVE              X:HEAD_W4_1,X0
                            000015
669       P:0001A3 P:0001A5 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
670    
671       
672       P:0001A5 P:0001A7 0BF080            JSR     PACKET_PARTITIONS
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 14



                            000560
673    
675       P:0001A7 P:0001A9 56F000            MOVE              X:HEAD_W3_0,A
                            000014
676    
677       P:0001A9 P:0001AB 0140C5            CMP     #>'RP',A
                            005250
678       P:0001AB P:0001AD 0AF0AA            JEQ     HANDLE_RP
                            0001B7
679    
680       P:0001AD P:0001AF 0140C5            CMP     #>'DA',A
                            004441
681       P:0001AF P:0001B1 0AF0AA            JEQ     HANDLE_DA
                            0001F3
682    
683       P:0001B1 P:0001B3 0AF080            JMP     QT_PTYPE_ERROR
                            0001B6
684    
685                                 PRE_ERROR
686       P:0001B3 P:0001B5 0A0026            BSET    #PREAMBLE_ERROR,X:<STATUS         ; indicate a preamble error
687       P:0001B4 P:0001B6 0BF080            JSR     CLEAR_FO_FIFO                     ; empty the fifo (2 ms!)
                            000494
688    
689                                 QT_PTYPE_ERROR
690                                 QT_FSIZE_ERROR
691                                 RETURN_NOW
692       P:0001B6 P:0001B8 00000C            RTS
693    
694    
695    
698    
699                                 HANDLE_RP
700       
701       P:0001B7 P:0001B9 0A0183            JCLR    #MODE_RP_BUFFER,X:MODE,MCE_PACKET
                            00024C
702    
703       
704       P:0001B9 P:0001BB 0A00B5            JSET    #RP_BUFFER_FULL,X:STATUS,HANDLE_RP_DROP
                            0001EB
705    
706       
707       P:0001BB P:0001BD 61F400            MOVE              #>REPLY_BUFFER,R1
                            100000
708       P:0001BD P:0001BF 0BF080            JSR     BUFFER_PACKET
                            00056D
709    
710       
711       P:0001BF P:0001C1 60F400            MOVE              #RP_BASE_LO,R0
                            000045
712       P:0001C1 P:0001C3 0BF080            JSR     LOAD_HILO_ADDRESS
                            000604
713    
714       P:0001C3 P:0001C5 60F400            MOVE              #BURST_DEST_LO,R0
                            00002E
715       P:0001C5 P:0001C7 0BF080            JSR     SAVE_HILO_ADDRESS
                            00060C
716    
717       
718       P:0001C7 P:0001C9 200013            CLR     A
719       P:0001C8 P:0001CA 20001B            CLR     B
720       P:0001C9 P:0001CB 50F000            MOVE              X:PACKET_SIZE,A0
                            00002B
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 15



721       P:0001CB P:0001CD 0C1D04            ASL     #2,A,A                            ; Size in bytes
722       P:0001CC P:0001CE 51F000            MOVE              X:RP_MAX_SIZE,B0
                            000047
723    
724       P:0001CE P:0001D0 200005            CMP     B,A                               ; A ? B
725       P:0001CF P:0001D1 0AF0AF            JLE     HANDLE_RP1
                            0001D2
726       P:0001D1 P:0001D3 21EE00            MOVE              B,A
727    
728                                 HANDLE_RP1
729       
730    
731       P:0001D2 P:0001D4 44F400            MOVE              #'NFY',X0
                            4E4659
732       P:0001D4 P:0001D6 447000            MOVE              X0,X:DTXS_WD1
                            00000B
733       P:0001D6 P:0001D8 44F400            MOVE              #'RPQ',X0
                            525051
734       P:0001D8 P:0001DA 447000            MOVE              X0,X:DTXS_WD2
                            00000C
735       P:0001DA P:0001DC 507000            MOVE              A0,X:DTXS_WD3           ; A0=block_size
                            00000D
736       P:0001DC P:0001DE 547000            MOVE              A1,X:DTXS_WD4           ; A1=0
                            00000E
737    
738       
739       P:0001DE P:0001E0 44F400            MOVE              #>REPLY_BUFFER,X0
                            100000
740       P:0001E0 P:0001E2 507000            MOVE              A0,X:BLOCK_SIZE
                            00002C
741       P:0001E2 P:0001E4 447000            MOVE              X0,X:BURST_SRC
                            000030
742       P:0001E4 P:0001E6 0BF080            JSR     BLOCK_TRANSFER
                            0004F8
743    
744       
745       P:0001E6 P:0001E8 0A7035            BSET    #RP_BUFFER_FULL,X:STATUS
                            000000
746       P:0001E8 P:0001EA 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000422
747    
748       
749       P:0001EA P:0001EC 00000C            RTS
750    
751                                 HANDLE_RP_DROP
752       P:0001EB P:0001ED 56F000            MOVE              X:RP_DROPS,A
                            000048
753       P:0001ED P:0001EF 014180            ADD     #1,A
754       P:0001EE P:0001F0 000000            NOP
755       P:0001EF P:0001F1 567000            MOVE              A,X:RP_DROPS
                            000048
756       P:0001F1 P:0001F3 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            000594
757    
759    
760    
763    
764                                 HANDLE_DA
765    
766       
767       P:0001F3 P:0001F5 56F000            MOVE              X:FRAME_COUNT,A
                            000002
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 16



768       P:0001F5 P:0001F7 0140C0            ADD     #>1,A
                            000001
769       P:0001F7 P:0001F9 000000            NOP
770       P:0001F8 P:0001FA 560200            MOVE              A,X:<FRAME_COUNT
771    
772       
773       P:0001F9 P:0001FB 0A0182            JCLR    #MODE_QT,X:MODE,MCE_PACKET
                            00024C
774    
775       
776       P:0001FB P:0001FD 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
777       P:0001FD P:0001FF 0BF080            JSR     BUFFER_PACKET
                            00056D
778    
779       
780       P:0001FF P:000201 56F000            MOVE              X:QT_BUF_HEAD,A
                            00003F
781       P:000201 P:000203 014180            ADD     #1,A
782       P:000202 P:000204 57F000            MOVE              X:QT_BUF_MAX,B
                            00003C
783       P:000204 P:000206 20000D            CMP     A,B
784       P:000205 P:000207 0AF0A1            JGE     HANDLE_DA_MATH
                            000208
785       P:000207 P:000209 2E0000            MOVE              #0,A
786                                 HANDLE_DA_MATH
787       P:000208 P:00020A 57F000            MOVE              X:QT_BUF_TAIL,B
                            000040
788       P:00020A P:00020C 20000D            CMP     A,B
789       P:00020B P:00020D 0AF0AA            JEQ     HANDLE_DA_DROP
                            00022A
790    
791       
792       P:00020D P:00020F 200013            CLR     A
793       P:00020E P:000210 50F000            MOVE              X:PACKET_SIZE,A0
                            00002B
794    
795       P:000210 P:000212 014088            ADD     #0,B                              ; Clear carry
796       P:000211 P:000213 0C1D04            ASL     #2,A,A                            ; Size, in bytes
797    
798       
799       P:000212 P:000214 20001B            CLR     B
800       P:000213 P:000215 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            00003D
801       P:000215 P:000217 20000D            CMP     A,B
802       P:000216 P:000218 0E21B6            JNE     QT_FSIZE_ERROR
803    
804       
805       P:000217 P:000219 517000            MOVE              B0,X:BLOCK_SIZE
                            00002C
806       P:000219 P:00021B 557000            MOVE              B1,X:BURST_SRC          ; Y:0
                            000030
807    
808       P:00021B P:00021D 60F400            MOVE              #QT_DEST_LO,R0
                            000041
809       P:00021D P:00021F 0BF080            JSR     LOAD_HILO_ADDRESS
                            000604
810       P:00021F P:000221 60F400            MOVE              #BURST_DEST_LO,R0
                            00002E
811       P:000221 P:000223 0BF080            JSR     SAVE_HILO_ADDRESS
                            00060C
812    
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 17



813       
814       P:000223 P:000225 0BF080            JSR     BLOCK_TRANSFER
                            0004F8
815    
816       
817       P:000225 P:000227 0BF080            JSR     BUFFER_INCR
                            0005C2
818    
819       
820       P:000227 P:000229 0BF080            JSR     BUFFER_INFORM_CHECK
                            0005E0
821    
822       P:000229 P:00022B 00000C            RTS
823    
824                                 HANDLE_DA_DROP
825       
826       P:00022A P:00022C 56F000            MOVE              X:QT_DROPS,A
                            000044
827       P:00022C P:00022E 014180            ADD     #1,A
828       P:00022D P:00022F 000000            NOP
829       P:00022E P:000230 567000            MOVE              A,X:QT_DROPS
                            000044
830       P:000230 P:000232 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            000594
831    
833    
834    
835    
836                                 CON_NOW
837                                 ;       This routine runs after the PC sends a 'CON' command, and will
838                                 ;       copy the command to the MCE and then reply to the PC.
839    
840       P:000232 P:000234 60F400            MOVE              #>CON_SOURCE_LO,R0
                            000049
841       P:000234 P:000236 0BF080            JSR     LOAD_HILO_ADDRESS                 ; PCI address in A
                            000604
842       P:000236 P:000238 0C1D01            ASL     #0,A,B                            ; MOVE A,B
843    
844       
845       P:000237 P:000239 66F400            MOVE              #>COMMAND_BUFFER,R6
                            200000
846       P:000239 P:00023B 200013            CLR     A
847    
848       
849       P:00023A P:00023C 064080            DO      #64,CON_NOW1                      ; block size = 32bit x 64 (256 bytes)
                            000240
850       P:00023C P:00023E 0D043B            JSR     <READ_FROM_PCI                    ; get next 32 bit word from HOST
851    
852       P:00023D P:00023F 5C5E00            MOVE                          A1,Y:(R6)+  ; b4, b3 (msb)
853       P:00023E P:000240 585E00            MOVE                          A0,Y:(R6)+  ; b2, b1  (lsb)
854    
855       P:00023F P:000241 0D0475            JSR     <XMT_WD_FIBRE                     ; off it goes
856       P:000240 P:000242 000000            NOP
857                                 CON_NOW1
858    
859       P:000241 P:000243 0A0101            BCLR    #MODE_CHOKE,X:<MODE               ; disable packet choke...
860                                                                                     ; comms now open with MCE and packets will b
e processed.
861       P:000242 P:000244 013D24            BSET    #AUX1,X:PDRC                      ; enable hardware
862    
863       
864       P:000243 P:000245 0A700A            BCLR    #CON_DEMAND,X:STATUS
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 18



                            000000
865    
866       
867       P:000245 P:000247 44F400            MOVE              #'CON',X0
                            434F4E
868       P:000247 P:000249 0BF080            JSR     VCOM_PREPARE_REPLY
                            000289
869       P:000249 P:00024B 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000422
870       P:00024B P:00024D 00000C            RTS
871    
872    
873    
874    
876    
877                                 ; --------------------------------------------------------------------------
878                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
879                                 ; --------------------------------------------------------------------------
880    
881                                 ; prepare notify to inform host that a packet has arrived.
882    
883                                 MCE_PACKET
884       P:00024C P:00024E 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
885    
886       P:00024D P:00024F 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
887       P:00024F P:000251 440B00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
888    
889       P:000250 P:000252 449400            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
890       P:000251 P:000253 440C00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
891    
892       P:000252 P:000254 449600            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
893       P:000253 P:000255 440D00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
894    
895       P:000254 P:000256 449500            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
896       P:000255 P:000257 440E00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sage
897    
898       
899       P:000256 P:000258 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
900       P:000257 P:000259 0D0422            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
901       P:000258 P:00025A 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
902    
903    
904       P:000259 P:00025B 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
905       P:00025B P:00025D 0BF080            JSR     BUFFER_PACKET
                            00056D
906    
907       
908    
909       P:00025D P:00025F 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; on fatal error, re-init.
                            000100
910       P:00025F P:000261 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; Set in 'send_packet_to_host' ISR
                            00025D
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 19



911    
912       
913       P:000261 P:000263 44F400            MOVE              #>IMAGE_BUFFER,X0
                            000000
914       P:000263 P:000265 56F000            MOVE              X:PACKET_SIZE,A
                            00002B
915       P:000265 P:000267 0C1D04            ASL     #2,A,A
916       P:000266 P:000268 447000            MOVE              X0,X:BURST_SRC
                            000030
917       P:000268 P:00026A 547000            MOVE              A1,X:BLOCK_SIZE
                            00002C
918       P:00026A P:00026C 0BF080            JSR     BLOCK_TRANSFER
                            0004F8
919    
920       P:00026C P:00026E 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
921    
922       
923       P:00026E P:000270 44F400            MOVE              #'HST',X0
                            485354
924       P:000270 P:000272 0BF080            JSR     VCOM_PREPARE_REPLY
                            000289
925       P:000272 P:000274 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000422
926       P:000274 P:000276 00000C            RTS
927    
928                                 ;----------------------------------------------------------
929                                 ; clear out the fifo after an HST timeout...
930                                 ;----------------------------------------------------------
931    
932                                 DUMP_FIFO
933       P:000275 P:000277 61F400            MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
934       P:000277 P:000279 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
935       P:000279 P:00027B 200013            CLR     A
936       P:00027A P:00027C 320000            MOVE              #0,R2                   ; use R2 as a dump count
937                                 NEXT_DUMP
938       P:00027B P:00027D 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000286
939       P:00027D P:00027F 000000            NOP
940       P:00027E P:000280 000000            NOP
941       P:00027F P:000281 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000286
942    
943       P:000281 P:000283 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
944       P:000282 P:000284 205A00            MOVE              (R2)+                   ; inc dump count
945       P:000283 P:000285 224E00            MOVE              R2,A                    ;
946       P:000284 P:000286 200045            CMP     X0,A                              ; check we've not hit dump limit
947       P:000285 P:000287 0E227B            JNE     NEXT_DUMP                         ; not hit limit?
948                                 FIFO_EMPTY
949       P:000286 P:000288 627000            MOVE              R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000006
950       P:000288 P:00028A 0C0100            JMP     <START                            ; re-initialise
951    
952    
953                                 ; -------------------------------------------------------------------------------------
954                                 ;                              END OF MAIN PACKET HANDLING CODE
955                                 ; -------------------------------------------------------------------------------------
956    
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 20



957    
958    
959                                 ; -------------------------------------------------------------------------------------
960                                 ;
961                                 ;                              INTERRUPT SERVICE ROUTINES
962                                 ;
963                                 ; -------------------------------------------------------------------------------------
964    
965                                 ; ---------------
966                                 ; Rules:  Don't use N#, or any R# except R0 unless you add them to the saved register set.
967    
968    
969                                 ; ----------------------------------------------------------------------------
970                                 ; VCOM_* - routines: utility functions for hosty command vector communication.
971                                 ;-----------------------------------------------------------------------------
972    
973    
974                                 ; VCOM_PREPARE_REPLY
975                                 ;
976                                 ; Prepare the reply packet, using X0 as the command name (second word).  The
977                                 ; message defaults to 'ACK' with NULL data.  The user may subsequenty fill in
978                                 ; the data field (word 4) and mark the packet as error if necessary.
979    
980                                 VCOM_PREPARE_REPLY
981       
982       
983       P:000289 P:00028B 50F400            MOVE              #'REP',A0
                            524550
984       P:00028B P:00028D 447000            MOVE              X0,X:DTXS_WD2           ; Command
                            00000C
985       P:00028D P:00028F 507000            MOVE              A0,X:DTXS_WD1
                            00000B
986    
987       P:00028F P:000291 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
                            41434B
988       P:000291 P:000293 000000            NOP
989       P:000292 P:000294 547000            MOVE              A1,X:DTXS_WD3           ; ACK
                            00000D
990       P:000294 P:000296 507000            MOVE              A0,X:DTXS_WD4           ; no comment
                            00000E
991       P:000296 P:000298 00000C            RTS
992    
993    
994                                 ; VCOM_CHECK
995                                 ;
996                                 ; Compares DRXR_WD1 to X0.  If they are equal, Z is set on return.  If they
997                                 ; are not equal then Z is cleared and the reply will be marked as ERR with
998                                 ; 'CNE' in the last word.
999                                 ; Trashes A and B always and X0 on error.
1000   
1001                                VCOM_CHECK
1002      P:000297 P:000299 208E00            MOVE              X0,A
1003      P:000298 P:00029A 57F000            MOVE              X:DRXR_WD1,B
                            000007
1004      P:00029A P:00029C 20000D            CMP     A,B
1005      P:00029B P:00029D 0AF0AA            JEQ     VCOM_RTS
                            0002A5
1006   
1007      P:00029D P:00029F 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
1008      P:00029F P:0002A1 50F400            MOVE              #'ERR',A0
                            455252
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 21



1009      P:0002A1 P:0002A3 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1010      P:0002A3 P:0002A5 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1011                                VCOM_RTS
1012      P:0002A5 P:0002A7 00000C            RTS
1013   
1014   
1015                                ; VCOM_INTRO
1016                                ;
1017                                ; Read DSP command from DRXR.  Prepare the reply packet and verify that it
1018                                ; matches the key in X1.  If it does not, mark the reply as error and set
1019                                ; the Z flag.
1020   
1021                                VCOM_INTRO
1022      P:0002A6 P:0002A8 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            000434
1023      P:0002A8 P:0002AA 20A400            MOVE              X1,X0
1024      P:0002A9 P:0002AB 0D0289            JSR     VCOM_PREPARE_REPLY
1025      P:0002AA P:0002AC 0D0297            JSR     VCOM_CHECK
1026      P:0002AB P:0002AD 00000C            RTS
1027   
1028   
1029                                ; VCOM_EXIT_ERROR_X0
1030                                ; VCOM_EXIT_X0
1031                                ; VCOM_EXIT
1032                                ;
1033                                ; For returning from host command vector interrupts only.  These three
1034                                ; routines do the following (respectively):
1035                                ; a) Mark reply as error, then (b)
1036                                ; b) Put X0 into last word of reply, then (c)
1037                                ; c) Restore registers and RTI.
1038   
1039                                VCOM_EXIT_ERROR_X0
1040      P:0002AC P:0002AE 50F400            MOVE              #'ERR',A0
                            455252
1041      P:0002AE P:0002B0 000000            NOP
1042      P:0002AF P:0002B1 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1043                                VCOM_EXIT_X0
1044      P:0002B1 P:0002B3 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1045                                VCOM_EXIT
1046      P:0002B3 P:0002B5 0BF080            JSR     RESTORE_REGISTERS
                            00045B
1047      P:0002B5 P:0002B7 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000422
1048      P:0002B7 P:0002B9 000004            RTI
1049   
1050   
1051   
1052   
1053                                ; ----------------------------------------------------------------------------
1054                                READ_MEMORY
1055                                ;-----------------------------------------------------------------------------
1056                                ;Read command:
1057                                ; word 1 = command = 'RDM'
1058                                ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
1059                                ; word 3 = address in memory
1060                                ; word 4 = not used
1061                                ;Version query:
1062                                ; word 1 = 'VER'
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 22



1063                                ; word 2-4 unused
1064   
1065      P:0002B8 P:0002BA 0BF080            JSR     SAVE_REGISTERS
                            000468
1066      P:0002BA P:0002BC 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            000434
1067   
1068      P:0002BC P:0002BE 44F400            MOVE              #'RDM',X0
                            52444D
1069      P:0002BE P:0002C0 0D0289            JSR     VCOM_PREPARE_REPLY
1070      P:0002BF P:0002C1 0D0297            JSR     VCOM_CHECK
1071      P:0002C0 P:0002C2 0AF0AA            JEQ     READ_MEMORY_XYP
                            0002CA
1072   
1073      
1074      P:0002C2 P:0002C4 44F400            MOVE              #'VER',X0
                            564552
1075      P:0002C4 P:0002C6 0D0289            JSR     VCOM_PREPARE_REPLY
1076      P:0002C5 P:0002C7 0D0297            JSR     VCOM_CHECK
1077      P:0002C6 P:0002C8 0E22B3            JNE     VCOM_EXIT
1078   
1079      P:0002C7 P:0002C9 44F000            MOVE              X:REV_NUMBER,X0
                            000003
1080      P:0002C9 P:0002CB 0C02B1            JMP     VCOM_EXIT_X0
1081   
1082                                READ_MEMORY_XYP
1083   
1084      
1085      P:0002CA P:0002CC 56F000            MOVE              X:DRXR_WD2,A
                            000008
1086      P:0002CC P:0002CE 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1087   
1088      P:0002CE P:0002D0 0140C5            CMP     #'_X',A
                            005F58
1089      P:0002D0 P:0002D2 0AF0AA            JEQ     READ_MEMORY_X
                            0002DD
1090   
1091      P:0002D2 P:0002D4 0140C5            CMP     #'_Y',A
                            005F59
1092      P:0002D4 P:0002D6 0AF0AA            JEQ     READ_MEMORY_Y
                            0002DF
1093   
1094      P:0002D6 P:0002D8 0140C5            CMP     #'_P',A
                            005F50
1095      P:0002D8 P:0002DA 0AF0AA            JEQ     READ_MEMORY_P
                            0002E1
1096   
1097      P:0002DA P:0002DC 44F400            MOVE              #'MTE',X0
                            4D5445
1098      P:0002DC P:0002DE 0C02AC            JMP     VCOM_EXIT_ERROR_X0
1099   
1100                                READ_MEMORY_X
1101      P:0002DD P:0002DF 44E000            MOVE              X:(R0),X0
1102      P:0002DE P:0002E0 0C02B1            JMP     VCOM_EXIT_X0
1103                                READ_MEMORY_Y
1104      P:0002DF P:0002E1 4CE000            MOVE                          Y:(R0),X0
1105      P:0002E0 P:0002E2 0C02B1            JMP     VCOM_EXIT_X0
1106                                READ_MEMORY_P
1107      P:0002E1 P:0002E3 07E084            MOVE              P:(R0),X0
1108      P:0002E2 P:0002E4 0C02B1            JMP     VCOM_EXIT_X0
1109   
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 23



1110   
1111                                ;--------------------------------------------------------------
1112                                WRITE_MEMORY
1113                                ;---------------------------------------------------------------
1114                                ; word 1 = command = 'WRM'
1115                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1116                                ; word 3 = address in memory
1117                                ; word 4 = value
1118   
1119      P:0002E3 P:0002E5 0BF080            JSR     SAVE_REGISTERS
                            000468
1120      P:0002E5 P:0002E7 45F400            MOVE              #'WRM',X1
                            57524D
1121      P:0002E7 P:0002E9 0D02A6            JSR     VCOM_INTRO
1122      P:0002E8 P:0002EA 0E22B3            JNE     VCOM_EXIT
1123   
1124      
1125      P:0002E9 P:0002EB 56F000            MOVE              X:DRXR_WD2,A
                            000008
1126      P:0002EB P:0002ED 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1127      P:0002ED P:0002EF 44F000            MOVE              X:DRXR_WD4,X0
                            00000A
1128   
1129      P:0002EF P:0002F1 0140C5            CMP     #'_X',A
                            005F58
1130      P:0002F1 P:0002F3 0AF0AA            JEQ     WRITE_MEMORY_X
                            0002FE
1131   
1132      P:0002F3 P:0002F5 0140C5            CMP     #'_Y',A
                            005F59
1133      P:0002F5 P:0002F7 0AF0AA            JEQ     WRITE_MEMORY_Y
                            000300
1134   
1135      P:0002F7 P:0002F9 0140C5            CMP     #'_P',A
                            005F50
1136      P:0002F9 P:0002FB 0AF0AA            JEQ     WRITE_MEMORY_P
                            000302
1137   
1138      P:0002FB P:0002FD 44F400            MOVE              #'MTE',X0
                            4D5445
1139      P:0002FD P:0002FF 0C02AC            JMP     VCOM_EXIT_ERROR_X0
1140   
1141                                WRITE_MEMORY_X
1142      P:0002FE P:000300 446000            MOVE              X0,X:(R0)
1143      P:0002FF P:000301 0C02B1            JMP     VCOM_EXIT_X0
1144                                WRITE_MEMORY_Y
1145      P:000300 P:000302 4C6000            MOVE                          X0,Y:(R0)
1146      P:000301 P:000303 0C02B1            JMP     VCOM_EXIT_X0
1147                                WRITE_MEMORY_P
1148      P:000302 P:000304 076084            MOVE              X0,P:(R0)
1149      P:000303 P:000305 0C02B1            JMP     VCOM_EXIT_X0
1150   
1151   
1152                                ;-----------------------------------------------------------------------------
1153                                START_APPLICATION
1154                                ; an application should already have been downloaded to the PCI memory.
1155                                ; this command will execute it.
1156                                ; ----------------------------------------------------------------------
1157                                ; word 1 = command = 'GOA'
1158                                ; word 2-4 unused
1159   
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 24



1160      P:000304 P:000306 0BF080            JSR     SAVE_REGISTERS
                            000468
1161      P:000306 P:000308 45F400            MOVE              #'GOA',X1
                            474F41
1162   
1163      P:000308 P:00030A 0D02A6            JSR     VCOM_INTRO
1164      P:000309 P:00030B 0E22B3            JNE     VCOM_EXIT
1165   
1166      P:00030A P:00030C 0A7020            BSET    #MODE_APPLICATION,X:MODE
                            000001
1167      P:00030C P:00030E 000004            RTI                                       ; Application will reply.
1168   
1169   
1170                                ; ---------------------------------------------------------
1171                                STOP_APPLICATION
1172                                ; this command stops an application that is currently running
1173                                ; used for applications that once started run contiunually
1174                                ;-----------------------------------------------------------
1175                                ; word 1 = command = ' STP'
1176                                ; word 2-4 unused
1177   
1178      P:00030D P:00030F 0BF080            JSR     SAVE_REGISTERS
                            000468
1179      P:00030F P:000311 45F400            MOVE              #'STP',X1
                            535450
1180   
1181      P:000311 P:000313 0D02A6            JSR     VCOM_INTRO
1182      P:000312 P:000314 0E22B3            JNE     VCOM_EXIT
1183   
1184      P:000313 P:000315 0A7000            BCLR    #MODE_APPLICATION,X:MODE
                            000001
1185      P:000315 P:000317 0A7000            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1186      P:000317 P:000319 0C02B3            JMP     VCOM_EXIT
1187   
1188   
1189                                ;-----------------------------------------------------------------------------
1190                                RESET_CONTROLLER
1191                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1192                                ;-----------------------------------------------------------------------------
1193                                ; word 1 = command = 'RCO'
1194                                ; word 2-4 unused
1195   
1196      P:000318 P:00031A 0BF080            JSR     SAVE_REGISTERS
                            000468
1197      P:00031A P:00031C 45F400            MOVE              #'RCO',X1
                            52434F
1198      P:00031C P:00031E 0D02A6            JSR     VCOM_INTRO
1199      P:00031D P:00031F 0E22B3            JNE     VCOM_EXIT
1200   
1201      P:00031E P:000320 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1202      P:00031F P:000321 000000            NOP
1203      P:000320 P:000322 000000            NOP
1204      P:000321 P:000323 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1205      P:000323 P:000325 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1206      P:000325 P:000327 446000            MOVE              X0,X:(R0)
1207      P:000326 P:000328 0606A0            REP     #6                                ; Wait for transmission to complete
1208      P:000327 P:000329 000000            NOP
1209      P:000328 P:00032A 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1210   
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 25



1211                                ; Wait for a bit for MCE to be reset.......
1212      P:000329 P:00032B 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1213      P:00032B P:00032D 06C400            DO      X0,L_DELAY
                            000331
1214      P:00032D P:00032F 06E883            DO      #1000,L_RDFIFO
                            000330
1215      P:00032F P:000331 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1216      P:000330 P:000332 000000            NOP                                       ;   receiver empty
1217                                L_RDFIFO
1218      P:000331 P:000333 000000            NOP
1219                                L_DELAY
1220      P:000332 P:000334 000000            NOP
1221   
1222      P:000333 P:000335 44F400            MOVE              #'000',X0
                            303030
1223      P:000335 P:000337 0C02B1            JMP     VCOM_EXIT_X0
1224   
1225                                ;-----------------------------------------------------------------------------
1226                                QUIET_TRANSFER_SET
1227                                ;-----------------------------------------------------------------------------
1228                                ;Quiet transfer mode configuration
1229                                ; word 1 = command = 'QTS'
1230                                ; word 2 = parameter to set
1231                                ; word 3-4 = arguments
1232   
1233      P:000336 P:000338 0BF080            JSR     SAVE_REGISTERS                    ; standard opening
                            000468
1234      P:000338 P:00033A 45F400            MOVE              #'QTS',X1
                            515453
1235      P:00033A P:00033C 0D02A6            JSR     VCOM_INTRO
1236      P:00033B P:00033D 0E22B3            JNE     VCOM_EXIT
1237   
1238      P:00033C P:00033E 56F000            MOVE              X:DRXR_WD2,A            ; Parameter id
                            000008
1239      P:00033E P:000340 44F000            MOVE              X:DRXR_WD3,X0           ; First arg
                            000009
1240      P:000340 P:000342 45F000            MOVE              X:DRXR_WD4,X1           ; Second arg
                            00000A
1241   
1242      P:000342 P:000344 0140C5            CMP     #'BAS',A
                            424153
1243      P:000344 P:000346 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            0003B7
1244   
1245      P:000346 P:000348 0140C5            CMP     #'DEL',A
                            44454C
1246      P:000348 P:00034A 60F400            MOVE              #QT_BUF_SIZE,R0
                            00003B
1247      P:00034A P:00034C 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1248   
1249      P:00034C P:00034E 0140C5            CMP     #'NUM',A
                            4E554D
1250      P:00034E P:000350 60F400            MOVE              #QT_BUF_MAX,R0
                            00003C
1251      P:000350 P:000352 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1252   
1253      P:000352 P:000354 0140C5            CMP     #'INF',A
                            494E46
1254      P:000354 P:000356 60F400            MOVE              #QT_INFORM,R0
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 26



                            00003E
1255      P:000356 P:000358 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1256   
1257      P:000358 P:00035A 0140C5            CMP     #'SIZ',A
                            53495A
1258      P:00035A P:00035C 60F400            MOVE              #QT_FRAME_SIZE,R0
                            00003D
1259      P:00035C P:00035E 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1260   
1261      P:00035E P:000360 0140C5            CMP     #'TAI',A
                            544149
1262      P:000360 P:000362 60F400            MOVE              #QT_BUF_TAIL,R0
                            000040
1263      P:000362 P:000364 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1264   
1265      P:000364 P:000366 0140C5            CMP     #'HEA',A
                            484541
1266      P:000366 P:000368 60F400            MOVE              #QT_BUF_HEAD,R0
                            00003F
1267      P:000368 P:00036A 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1268   
1269      P:00036A P:00036C 0140C5            CMP     #'DRO',A
                            44524F
1270      P:00036C P:00036E 60F400            MOVE              #QT_DROPS,R0
                            000044
1271      P:00036E P:000370 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1272   
1273      P:000370 P:000372 0140C5            CMP     #'PER',A
                            504552
1274      P:000372 P:000374 60F400            MOVE              #TCPR0,R0
                            FFFF8D
1275      P:000374 P:000376 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1276   
1277      P:000376 P:000378 0140C5            CMP     #'FLU',A
                            464C55
1278      P:000378 P:00037A 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            00039E
1279   
1280      P:00037A P:00037C 0140C5            CMP     #'SET',A
                            534554
1281      P:00037C P:00037E 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            0003A6
1282   
1283      P:00037E P:000380 0140C5            CMP     #'RPS',A
                            525053
1284      P:000380 P:000382 60F400            MOVE              #RP_MAX_SIZE,R0
                            000047
1285      P:000382 P:000384 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1286   
1287      P:000384 P:000386 0140C5            CMP     #'RPB',A
                            525042
1288      P:000386 P:000388 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_BASE
                            00038F
1289   
1290      P:000388 P:00038A 0140C5            CMP     #'RPE',A
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 27



                            525045
1291      P:00038A P:00038C 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_ENABLED
                            000394
1292   
1293      P:00038C P:00038E 44F400            MOVE              #'MTE',X0
                            4D5445
1294      P:00038E P:000390 0C02AC            JMP     VCOM_EXIT_ERROR_X0
1295   
1296                                QUIET_TRANSFER_SET_RP_BASE
1297      P:00038F P:000391 447000            MOVE              X0,X:RP_BASE_LO
                            000045
1298      P:000391 P:000393 457000            MOVE              X1,X:RP_BASE_HI
                            000046
1299      P:000393 P:000395 0C02B3            JMP     VCOM_EXIT
1300   
1301                                QUIET_TRANSFER_SET_RP_ENABLED
1302      P:000394 P:000396 0A7003            BCLR    #MODE_RP_BUFFER,X:MODE
                            000001
1303      P:000396 P:000398 208E00            MOVE              X0,A
1304      P:000397 P:000399 200003            TST     A
1305      P:000398 P:00039A 0EA2B3            JEQ     VCOM_EXIT
1306      P:000399 P:00039B 0A7023            BSET    #MODE_RP_BUFFER,X:MODE
                            000001
1307      P:00039B P:00039D 0A7015            BCLR    #RP_BUFFER_FULL,X:STATUS
                            000000
1308      P:00039D P:00039F 0C02B3            JMP     VCOM_EXIT
1309   
1310                                QUIET_TRANSFER_SET_FLUSH
1311      P:00039E P:0003A0 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
1312      P:0003A0 P:0003A2 208E00            MOVE              X0,A
1313      P:0003A1 P:0003A3 200003            TST     A
1314      P:0003A2 P:0003A4 0EA2B3            JEQ     VCOM_EXIT
1315      P:0003A3 P:0003A5 0A7034            BSET    #QT_FLUSH,X:STATUS
                            000000
1316      P:0003A5 P:0003A7 0C02B3            JMP     VCOM_EXIT
1317   
1318                                QUIET_TRANSFER_SET_ENABLED
1319      P:0003A6 P:0003A8 0A7002            BCLR    #MODE_QT,X:MODE
                            000001
1320      P:0003A8 P:0003AA 0BF080            JSR     TIMER_DISABLE
                            0005AE
1321      P:0003AA P:0003AC 208E00            MOVE              X0,A
1322      P:0003AB P:0003AD 200003            TST     A
1323      P:0003AC P:0003AE 0EA2B3            JEQ     VCOM_EXIT
1324      P:0003AD P:0003AF 280000            MOVE              #0,A0
1325      P:0003AE P:0003B0 0A7022            BSET    #MODE_QT,X:MODE
                            000001
1326      P:0003B0 P:0003B2 507000            MOVE              A0,X:TLR0
                            FFFF8E
1327      P:0003B2 P:0003B4 0BF080            JSR     TIMER_ENABLE
                            0005A8
1328      P:0003B4 P:0003B6 0C02B3            JMP     VCOM_EXIT
1329   
1330                                QUIET_TRANSFER_SET_R0
1331      P:0003B5 P:0003B7 446000            MOVE              X0,X:(R0)
1332      P:0003B6 P:0003B8 0C02B3            JMP     VCOM_EXIT
1333   
1334                                QUIET_TRANSFER_SET_BASE
1335      P:0003B7 P:0003B9 447000            MOVE              X0,X:QT_BASE_LO
                            000039
1336      P:0003B9 P:0003BB 457000            MOVE              X1,X:QT_BASE_HI
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 28



                            00003A
1337   
1338      P:0003BB P:0003BD 0BF080            JSR     BUFFER_RESET
                            0005D4
1339   
1340      P:0003BD P:0003BF 0C02B3            JMP     VCOM_EXIT
1341   
1342   
1343                                ;-----------------------------------------------------------------------------
1344                                SYSTEM_RESET
1345                                ;-----------------------------------------------------------------------------
1346   
1347      P:0003BE P:0003C0 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1348      P:0003BF P:0003C1 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1349                                                                                    ; set to zero except for interrupts
1350      P:0003C1 P:0003C3 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1351                                                                                    ; so first set to 0
1352      P:0003C2 P:0003C4 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1353                                                                                    ; therefore,return to initialization
1354      P:0003C4 P:0003C6 000000            NOP
1355      P:0003C5 P:0003C7 000004            RTI                                       ; return from ISR - to START
1356   
1357   
1358                                ;--------------------------------------------------------------------
1359                                CLEAN_UP_PCI
1360                                ;--------------------------------------------------------------------
1361                                ; Clean up the PCI board from wherever it was executing
1362   
1363      P:0003C6 P:0003C8 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1364      P:0003C7 P:0003C9 05F43D            MOVEC             #$000200,SSL            ; SR = zero except for interrupts
                            000200
1365      P:0003C9 P:0003CB 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1366      P:0003CA P:0003CC 05F43C            MOVEC             #START,SSH              ; Set PC to for full initialization
                            000100
1367      P:0003CC P:0003CE 000000            NOP
1368      P:0003CD P:0003CF 000004            RTI
1369   
1370   
1371                                ; ------------------------------------------------------------------------------------
1372                                SEND_PACKET_TO_HOST
1373                                ; this command is received from the Host and actions the PCI board to pick up an address
1374                                ; pointer from DRXR which the PCI board then uses to write packets from the
1375                                ; MCE to the host memory starting at the address given.
1376                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1377                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1378                                ; HST after packet sent (unless error).
1379                                ; --------------------------------------------------------------------------------------
1380                                ; word 1 = command = 'HST'
1381                                ; word 2 = host high address
1382                                ; word 3 = host low address
1383                                ; word 4 = not used but read
1384   
1385                                ; save some registers but not B
1386   
1387      P:0003CE P:0003D0 0D0468            JSR     <SAVE_REGISTERS                   ; save working registers
1388      P:0003CF P:0003D1 45F400            MOVE              #'HST',X1
                            485354
1389      P:0003D1 P:0003D3 0D02A6            JSR     VCOM_INTRO
1390      P:0003D2 P:0003D4 0E22B3            JNE     VCOM_EXIT
1391   
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 29



1392      
1393      P:0003D3 P:0003D5 448800            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
1394      P:0003D4 P:0003D6 518900            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1395   
1396      P:0003D5 P:0003D7 447000            MOVE              X0,X:BURST_DEST_HI
                            00002F
1397      P:0003D7 P:0003D9 517000            MOVE              B0,X:BURST_DEST_LO
                            00002E
1398   
1399      P:0003D9 P:0003DB 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1400   
1401      P:0003DA P:0003DC 0BF080            JSR     RESTORE_REGISTERS
                            00045B
1402      P:0003DC P:0003DE 000004            RTI                                       ; Main loop will reply after packet transfer
!
1403   
1404   
1405                                ; --------------------------------------------------------------------
1406                                SOFTWARE_RESET
1407                                ;----------------------------------------------------------------------
1408                                ; word 1 = command = 'RST'
1409                                ; word 2-4 unused
1410   
1411      P:0003DD P:0003DF 0BF080            JSR     SAVE_REGISTERS
                            000468
1412      P:0003DF P:0003E1 45F400            MOVE              #'RST',X1
                            525354
1413      P:0003E1 P:0003E3 0D02A6            JSR     VCOM_INTRO
1414      P:0003E2 P:0003E4 0E22B3            JNE     VCOM_EXIT
1415   
1416                                ; RST command OK so reply to host
1417                                FINISH_RST
1418      P:0003E3 P:0003E5 44F400            MOVE              #'000',X0
                            303030
1419      P:0003E5 P:0003E7 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1420      P:0003E7 P:0003E9 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000422
1421   
1422      P:0003E9 P:0003EB 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            0003E9
1423   
1424      P:0003EB P:0003ED 0A0100            BCLR    #MODE_APPLICATION,X:<MODE         ; clear app flag
1425      P:0003EC P:0003EE 0A0006            BCLR    #PREAMBLE_ERROR,X:<STATUS         ; clear preamble error
1426      P:0003ED P:0003EF 0A0000            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1427   
1428                                ; remember we are in a ISR so can't just jump to start.
1429   
1430      P:0003EE P:0003F0 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1431      P:0003EF P:0003F1 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1432                                                                                    ; set to zero except for interrupts
1433      P:0003F1 P:0003F3 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1434                                                                                    ; so first set to 0
1435      P:0003F2 P:0003F4 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1436                                                                                    ; therefore,return to initialization
1437      P:0003F4 P:0003F6 000000            NOP
1438      P:0003F5 P:0003F7 000004            RTI                                       ; return from ISR - to START
1439   
1440   
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 30



1441                                SEND_PACKET_TO_CONTROLLER
1442   
1443                                ;       Host command identifying location of an MCE command to send to
1444                                ;       the MCE.  Since this can come at any time, just record the
1445                                ;       request and then do the CONning from the main loop.
1446   
1447                                ; word 1 = command = 'CON'
1448                                ; word 2 = source host bus address, bits 31:16
1449                                ; word 3 = source host bus address, bits 15:0
1450                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1451                                ;        = '1' --> when MCE command is GO
1452   
1453      P:0003F6 P:0003F8 0D0468            JSR     <SAVE_REGISTERS                   ; save working registers
1454   
1455      
1456      P:0003F7 P:0003F9 45F400            MOVE              #'CON',X1
                            434F4E
1457      P:0003F9 P:0003FB 0D02A6            JSR     VCOM_INTRO
1458      P:0003FA P:0003FC 0E22B3            JNE     VCOM_EXIT
1459   
1460      
1461      P:0003FB P:0003FD 44F400            MOVE              #'BUS',X0
                            425553
1462      P:0003FD P:0003FF 0A00AA            JSET    #CON_DEMAND,X:STATUS,VCOM_EXIT_ERROR_X0
                            0002AC
1463   
1464      
1465      P:0003FF P:000401 0A702A            BSET    #CON_DEMAND,X:STATUS
                            000000
1466      P:000401 P:000403 448800            MOVE              X:<DRXR_WD2,X0
1467      P:000402 P:000404 458900            MOVE              X:<DRXR_WD3,X1
1468      P:000403 P:000405 447000            MOVE              X0,X:CON_SOURCE_HI
                            00004A
1469      P:000405 P:000407 457000            MOVE              X1,X:CON_SOURCE_LO
                            000049
1470   
1471                                ;       ;; Fourth word indicates if this is a go.  Who cares?
1472                                ;       MOVE    X:<DRXR_WD4,A           ; read word 4 - GO command?
1473                                ;       MOVE    #0,X0
1474                                ;       CMP     X0,A
1475                                ;       JEQ     BLOCK_CON
1476   
1477      
1478      P:000407 P:000409 0BF080            JSR     RESTORE_REGISTERS
                            00045B
1479      P:000409 P:00040B 000004            RTI
1480   
1482   
1483   
1484                                ;---------------------------------------------------------------
1485                                ;
1486                                ;                          * END OF ISRs *
1487                                ;
1488                                ;--------------------------------------------------------------
1489   
1490   
1491   
1492                                ;----------------------------------------------------------------
1493                                ;
1494                                ;                     * Beginning of SUBROUTINES *
1495                                ;
1496                                ;-----------------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 31



1497   
1498   
1499                                ;---------------------------------------------------------------
1500                                GET_FO_WRD
1501                                ;--------------------------------------------------------------
1502                                ; Anything in fibre receive FIFO?   If so store in X0
1503   
1504      P:00040A P:00040C 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            000420
1505      P:00040C P:00040E 000000            NOP
1506      P:00040D P:00040F 000000            NOP
1507      P:00040E P:000410 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS             ; check twice for FO metastability.
                            000420
1508      P:000410 P:000412 0AF080            JMP     RD_FO_WD
                            000418
1509   
1510      P:000412 P:000414 01AD80  WT_FIFO   JCLR    #EF,X:PDRD,*                      ; Wait till something in FIFO flagged
                            000412
1511      P:000414 P:000416 000000            NOP
1512      P:000415 P:000417 000000            NOP
1513      P:000416 P:000418 01AD80            JCLR    #EF,X:PDRD,WT_FIFO                ; check twice.....
                            000412
1514   
1515                                ; Read one word from the fiber optics FIFO, check it and put it in A1
1516                                RD_FO_WD
1517      P:000418 P:00041A 09443F            MOVEP             Y:RDFIFO,X0             ; then read to X0
1518      P:000419 P:00041B 54F400            MOVE              #$00FFFF,A1             ; mask off top 2 bytes ($FC)
                            00FFFF
1519      P:00041B P:00041D 200046            AND     X0,A                              ; since receiving 16 bits in 24bit register
1520      P:00041C P:00041E 000000            NOP
1521      P:00041D P:00041F 218400            MOVE              A1,X0
1522      P:00041E P:000420 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1523      P:00041F P:000421 00000C            RTS
1524                                CLR_FO_RTS
1525      P:000420 P:000422 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1526      P:000421 P:000423 00000C            RTS
1527   
1528   
1529                                ; ----------------------------------------------------------------------------
1530                                PCI_MESSAGE_TO_HOST
1531                                ;----------------------------------------------------------------------------
1532   
1533                                ; subroutine to send 4 words as a reply from PCI to the Host
1534                                ; using the DTXS-HRXS data path
1535                                ; PCI card writes here first then causes an interrupt INTA on
1536                                ; the PCI bus to alert the host to the reply message
1537   
1538      P:000422 P:000424 0A85A3            JSET    #DCTR_HF3,X:DCTR,*                ; make sure host ready to receive interrupt
                            000422
1539                                                                                    ; cleared via fast interrupt if host out of 
its ISR
1540      P:000424 P:000426 60F400            MOVE              #>DTXS_WD1,R0
                            00000B
1541   
1542      P:000426 P:000428 060480            DO      #4,PCI_MESSAGE_TO_HOST_RESTORE
                            00042A
1543      P:000428 P:00042A 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            000428
1544      P:00042A P:00042C 08D88D            MOVEP             X:(R0)+,X:DTXS
1545   
1546                                PCI_MESSAGE_TO_HOST_RESTORE
1547   
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 32



1548      
1549      P:00042B P:00042D 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            00001D
1550      P:00042D P:00042F 60F000            MOVE              X:SV_R0,R0              ; restore X0
                            000021
1551   
1552                                ; all the transmit words are in the FIFO, interrupt the Host
1553                                ; the Host should clear this interrupt once it is detected.
1554                                ; It does this by writing to HCVR to cause a fast interrupt.
1555   
1556                                                                                    ; set flag to handshake interrupt (INTA) wit
h host.
1557      P:00042F P:000431 0A8523            BSET    #DCTR_HF3,X:DCTR
1558                                                                                    ; only interrupt in irq mode
1559      P:000430 P:000432 0A0184            JCLR    #MODE_IRQ,X:MODE,PCI_MESSAGE_TO_HOST_RETURN
                            000433
1560      P:000432 P:000434 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1561                                PCI_MESSAGE_TO_HOST_RETURN
1562      P:000433 P:000435 00000C            RTS
1563   
1564   
1565                                ;---------------------------------------------------------------
1566                                RD_DRXR
1567                                ;--------------------------------------------------------------
1568                                ; Routine to read from HTXR-DRXR data path.  This is where the host
1569                                ; puts data prior to issuing a vector command.
1570                                ;
1571                                ; HCTR[HTF] determines how the data written by the host is decoded
1572                                ; here.  Typically HCTR = 0x900, meaning the 3 LSBs of each 32-bit
1573                                ; word written by the host are returned in each read of DRXR.
1574                                ;
1575                                ; We only check for non-empty FIFO here, so all 4 words must be
1576                                ; written to the FIFO before calling this routine.
1577   
1578      P:000434 P:000436 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            000434
1579                                                                                    ; implies that host has written words
1580      P:000436 P:000438 63F400            MOVE              #DRXR_WD1,R3
                            000007
1581      P:000438 P:00043A 0604A0            REP     #4
1582      P:000439 P:00043B 085B8B            MOVEP             X:DRXR,X:(R3)+
1583      P:00043A P:00043C 00000C            RTS
1584   
1585                                ;---------------------------------------------------------------
1586                                READ_FROM_PCI
1587                                ;--------------------------------------------------------------
1588                                ; sub routine to read a 24 bit word in from PCI bus --> Y memory
1589                                ; 32bit host address in accumulator B.
1590   
1591                                ; read as master
1592   
1593      P:00043B P:00043D 0C1890            EXTRACTU #$010010,B,A                     ; Get D31-16 bits only
                            010010
1594      P:00043D P:00043F 000000            NOP
1595   
1596      P:00043E P:000440 210C00            MOVE              A0,A1
1597      P:00043F P:000441 000000            NOP
1598      P:000440 P:000442 547000            MOVE              A1,X:DPMC               ; high 16bits of address in DSP master cntr 
reg.
                            FFFFC7
1599                                                                                    ; 32 bit read so FC1 = 0 and FC0 = 0
1600   
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 33



1601      P:000442 P:000444 000000            NOP
1602      P:000443 P:000445 0C1890            EXTRACTU #$010000,B,A
                            010000
1603      P:000445 P:000447 000000            NOP
1604      P:000446 P:000448 210C00            MOVE              A0,A1
1605      P:000447 P:000449 0140C2            OR      #$060000,A                        ; A1 gets written to DPAR register
                            060000
1606      P:000449 P:00044B 000000            NOP                                       ; C3-C0 of DPAR=0110 for memory read
1607      P:00044A P:00044C 08CC08  WRT_ADD   MOVEP             A1,X:DPAR               ; Write address to PCI bus - PCI READ action
1608      P:00044B P:00044D 000000            NOP                                       ; Pipeline delay
1609      P:00044C P:00044E 0A8AA2  RD_PCI    JSET    #MRRQ,X:DPSR,GET_DAT              ; If MTRQ = 1 go read the word from host via
 FIFO
                            000455
1610      P:00044E P:000450 0A8A8A            JCLR    #TRTY,X:DPSR,RD_PCI               ; Bit is set if its a retry
                            00044C
1611      P:000450 P:000452 08F48A            MOVEP             #$0400,X:DPSR           ; Clear bit 10 = target retry bit
                            000400
1612      P:000452 P:000454 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for PCI addressing to be complete
                            000452
1613      P:000454 P:000456 0C044A            JMP     <WRT_ADD
1614   
1615      P:000455 P:000457 08480B  GET_DAT   MOVEP             X:DRXR,A0               ; Read 1st 16 bits of 32 bit word from host 
memory
1616      P:000456 P:000458 084C0B            MOVEP             X:DRXR,A1               ; Read 2nd 16 bits of 32 bit word from host 
memory
1617   
1618                                ; note that we now have 4 bytes in X0 and X1.
1619                                ; The 32bit word was in host memory in little endian format
1620                                ; If form LSB --> MSB the bytes are b1, b2, b3, b4 in host memory
1621                                ; in progressing through the HTRX/DRXR FIFO the
1622                                ; bytes end up like this.....
1623                                ; then X0 = $00 b2 b1
1624                                ; and  X1 = $00 b4 b3
1625   
1626      P:000457 P:000459 0604A0            REP     #4                                ; increment PCI address by four bytes.
1627      P:000458 P:00045A 000009            INC     B
1628      P:000459 P:00045B 000000            NOP
1629      P:00045A P:00045C 00000C            RTS
1630   
1631                                ;------------------------------------------------------------------------------------
1632                                RESTORE_REGISTERS
1633                                ;-------------------------------------------------------------------------------------
1634   
1635      P:00045B P:00045D 05A239            MOVEC             X:<SV_SR,SR
1636   
1637      P:00045C P:00045E 509700            MOVE              X:<SV_A0,A0
1638      P:00045D P:00045F 549800            MOVE              X:<SV_A1,A1
1639      P:00045E P:000460 529900            MOVE              X:<SV_A2,A2
1640   
1641      P:00045F P:000461 519A00            MOVE              X:<SV_B0,B0
1642      P:000460 P:000462 559B00            MOVE              X:<SV_B1,B1
1643      P:000461 P:000463 539C00            MOVE              X:<SV_B2,B2
1644   
1645      P:000462 P:000464 449D00            MOVE              X:<SV_X0,X0
1646      P:000463 P:000465 459E00            MOVE              X:<SV_X1,X1
1647   
1648      P:000464 P:000466 469F00            MOVE              X:<SV_Y0,Y0
1649      P:000465 P:000467 47A000            MOVE              X:<SV_Y1,Y1
1650   
1651      P:000466 P:000468 60A100            MOVE              X:<SV_R0,R0
1652      P:000467 P:000469 00000C            RTS
1653   
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 34



1654                                ;-------------------------------------------------------------------------------------
1655                                SAVE_REGISTERS
1656                                ;-------------------------------------------------------------------------------------
1657   
1658      P:000468 P:00046A 052239            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1659   
1660      P:000469 P:00046B 501700            MOVE              A0,X:<SV_A0
1661      P:00046A P:00046C 541800            MOVE              A1,X:<SV_A1
1662      P:00046B P:00046D 521900            MOVE              A2,X:<SV_A2
1663   
1664      P:00046C P:00046E 511A00            MOVE              B0,X:<SV_B0
1665      P:00046D P:00046F 551B00            MOVE              B1,X:<SV_B1
1666      P:00046E P:000470 531C00            MOVE              B2,X:<SV_B2
1667   
1668      P:00046F P:000471 441D00            MOVE              X0,X:<SV_X0
1669      P:000470 P:000472 451E00            MOVE              X1,X:<SV_X1
1670   
1671      P:000471 P:000473 461F00            MOVE              Y0,X:<SV_Y0
1672      P:000472 P:000474 472000            MOVE              Y1,X:<SV_Y1
1673   
1674      P:000473 P:000475 602100            MOVE              R0,X:<SV_R0
1675      P:000474 P:000476 00000C            RTS
1676   
1677                                ;-------------------------------------------------------
1678                                XMT_WD_FIBRE
1679                                ;-----------------------------------------------------
1680                                ; 250 MHz code - Transmit contents of Accumulator A1 to the MCE
1681                                ; we want to send 32bit word in little endian fomat to the host.
1682                                ; i.e. b4b3b2b1 goes b1, b2, b3, b4
1683                                ; currently the bytes are in this order:
1684                                ;  A0 = $00 b2 b1
1685                                ;  A1 = $00 b4 b3
1686                                ;  A = $00 00 b4 b3 00 b2 b1
1687   
1688      
1689   
1690      P:000475 P:000477 212400            MOVE              B0,X0                   ; Save B
1691      P:000476 P:000478 21A500            MOVE              B1,X1
1692   
1693      P:000477 P:000479 0C1D31            ASL     #24,A,B
1694      P:000478 P:00047A 0140CE            AND     #>$0000FF,B                       ; B1=b1
                            0000FF
1695      P:00047A P:00047C 557000            MOVE              B1,X:FO_SEND
                            FFF000
1696   
1697      P:00047C P:00047E 0C1D21            ASL     #16,A,B
1698      P:00047D P:00047F 0140CE            AND     #>$0000FF,B
                            0000FF
1699      P:00047F P:000481 557000            MOVE              B1,X:FO_SEND            ; B1=b2
                            FFF000
1700   
1701      P:000481 P:000483 0C1C11            ASR     #8,A,B
1702      P:000482 P:000484 0140C6            AND     #>$0000FF,A
                            0000FF
1703      P:000484 P:000486 547000            MOVE              A1,X:FO_SEND            ; A1=b3
                            FFF000
1704   
1705      P:000486 P:000488 0140CE            AND     #>$0000FF,B
                            0000FF
1706      P:000488 P:00048A 557000            MOVE              B1,X:FO_SEND            ; B1=b4
                            FFF000
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 35



1707   
1708      P:00048A P:00048C 208900            MOVE              X0,B0                   ; Restore B
1709      P:00048B P:00048D 20AD00            MOVE              X1,B1
1710      P:00048C P:00048E 00000C            RTS
1711   
1712   
1713                                ;----------------------------------------------
1714                                FLUSH_PCI_FIFO
1715                                ;----------------------------------------------
1716      P:00048D P:00048F 0A8A84            JCLR    #MARQ,X:DPSR,*
                            00048D
1717      P:00048F P:000491 0A862E            BSET    #CLRT,X:DPCR
1718      P:000490 P:000492 000000            NOP
1719      P:000491 P:000493 0A86AE            JSET    #CLRT,X:DPCR,*
                            000491
1720      P:000493 P:000495 00000C            RTS
1721   
1722                                ;----------------------------------------------
1723                                CLEAR_FO_FIFO
1724                                ;----------------------------------------------
1725      P:000494 P:000496 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
1726      P:000496 P:000498 44F400            MOVE              #200000,X0
                            030D40
1727      P:000498 P:00049A 06C400            DO      X0,*+3
                            00049A
1728      P:00049A P:00049C 000000            NOP
1729      P:00049B P:00049D 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
1730      P:00049D P:00049F 00000C            RTS
1731   
1732   
1733                                ;-----------------------------------------------
1734                                PCI_ERROR_CLEAR
1735                                ;-----------------------------------------------
1736      
1737      
1738      
1739      
1740      
1741      
1742   
1743      P:00049E P:0004A0 50F000            MOVE              X:DMA_ERRORS,A0
                            000031
1744      P:0004A0 P:0004A2 000008            INC     A
1745      P:0004A1 P:0004A3 000000            NOP
1746      P:0004A2 P:0004A4 507000            MOVE              A0,X:DMA_ERRORS
                            000031
1747   
1748      P:0004A4 P:0004A6 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            0004B2
1749      P:0004A6 P:0004A8 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
                            0004BC
1750      P:0004A8 P:0004AA 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            0004C6
1751      P:0004AA P:0004AC 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            0004D0
1752      P:0004AC P:0004AE 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            0004DA
1753      P:0004AE P:0004B0 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            0004E4
1754      P:0004B0 P:0004B2 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 36



                            0004EE
1755   
1756                                ERROR_TRTY
1757      P:0004B2 P:0004B4 50F000            MOVE              X:EC_TRTY,A0
                            000032
1758      P:0004B4 P:0004B6 000008            INC     A
1759      P:0004B5 P:0004B7 08F48A            MOVEP             #>$0400,X:DPSR          ; Clear target retry error bit
                            000400
1760      P:0004B7 P:0004B9 507000            MOVE              A0,X:EC_TRTY
                            000032
1761      P:0004B9 P:0004BB 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1762      P:0004BB P:0004BD 00000C            RTS
1763                                ERROR_TO
1764      P:0004BC P:0004BE 50F000            MOVE              X:EC_TO,A0
                            000033
1765      P:0004BE P:0004C0 000008            INC     A
1766      P:0004BF P:0004C1 08F48A            MOVEP             #>$0800,X:DPSR          ; Clear timeout error bit
                            000800
1767      P:0004C1 P:0004C3 507000            MOVE              A0,X:EC_TO
                            000033
1768      P:0004C3 P:0004C5 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1769      P:0004C5 P:0004C7 00000C            RTS
1770                                ERROR_TDIS
1771      P:0004C6 P:0004C8 50F000            MOVE              X:EC_TDIS,A0
                            000034
1772      P:0004C8 P:0004CA 000008            INC     A
1773      P:0004C9 P:0004CB 08F48A            MOVEP             #>$0200,X:DPSR          ; Clear target disconnect bit
                            000200
1774      P:0004CB P:0004CD 507000            MOVE              A0,X:EC_TDIS
                            000034
1775      P:0004CD P:0004CF 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1776      P:0004CF P:0004D1 00000C            RTS
1777                                ERROR_TAB
1778      P:0004D0 P:0004D2 50F000            MOVE              X:EC_TAB,A0
                            000035
1779      P:0004D2 P:0004D4 000008            INC     A
1780      P:0004D3 P:0004D5 08F48A            MOVEP             #>$0100,X:DPSR          ; Clear target abort error bit
                            000100
1781      P:0004D5 P:0004D7 507000            MOVE              A0,X:EC_TAB
                            000035
1782      P:0004D7 P:0004D9 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1783      P:0004D9 P:0004DB 00000C            RTS
1784                                ERROR_MAB
1785      P:0004DA P:0004DC 50F000            MOVE              X:EC_MAB,A0
                            000036
1786      P:0004DC P:0004DE 000008            INC     A
1787      P:0004DD P:0004DF 08F48A            MOVEP             #>$0080,X:DPSR          ; Clear master abort error bit
                            000080
1788      P:0004DF P:0004E1 507000            MOVE              A0,X:EC_MAB
                            000036
1789      P:0004E1 P:0004E3 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1790      P:0004E3 P:0004E5 00000C            RTS
1791                                ERROR_DPER
1792      P:0004E4 P:0004E6 50F000            MOVE              X:EC_DPER,A0
                            000037
1793      P:0004E6 P:0004E8 000008            INC     A
1794      P:0004E7 P:0004E9 08F48A            MOVEP             #>$0040,X:DPSR          ; Clear data parity error bit
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 37



                            000040
1795      P:0004E9 P:0004EB 507000            MOVE              A0,X:EC_DPER
                            000037
1796      P:0004EB P:0004ED 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1797      P:0004ED P:0004EF 00000C            RTS
1798                                ERROR_APER
1799      P:0004EE P:0004F0 50F000            MOVE              X:EC_APER,A0
                            000038
1800      P:0004F0 P:0004F2 000008            INC     A
1801      P:0004F1 P:0004F3 08F48A            MOVEP             #>$0020,X:DPSR          ; Clear address parity error bit
                            000020
1802      P:0004F3 P:0004F5 507000            MOVE              A0,X:EC_APER
                            000038
1803      P:0004F5 P:0004F7 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1804      P:0004F7 P:0004F9 00000C            RTS
1805   
1806   
1807                                ;----------------------------------------------
1808                                BLOCK_TRANSFER
1809                                ;----------------------------------------------
1810                                ;   In:
1811                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
1812                                ;   - BLOCK_SIZE is packet size, in bytes
1813                                ;   - BURST_SRC is start of data in Y memory
1814                                ;  Out:
1815                                ;   - BLOCK_SIZE will be decremented to zero.
1816                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
1817                                ;   - BURST_SRC will be incremented by BLOCK_SIZE/2
1818                                ;  Trashes:
1819                                ;   - A and B
1820   
1821      
1822      P:0004F8 P:0004FA 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002C
1823   
1824      P:0004FA P:0004FC 014085            CMP     #0,A
1825      P:0004FB P:0004FD 0AF0AA            JEQ     BLOCK_DONE
                            00053F
1826   
1827      
1828   
1829      P:0004FD P:0004FF 20001B            CLR     B
1830      P:0004FE P:000500 55F000            MOVE              X:PCI_BURST_SIZE,B1
                            00004B
1831   
1832      P:000500 P:000502 200005            CMP     B,A                               ; A ? B
1833      P:000501 P:000503 0E1503            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
1834      P:000502 P:000504 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1835                                BLOCK_TRANSFER1
1836      P:000503 P:000505 200014            SUB     B,A                               ; A -= B
1837      P:000504 P:000506 014088            ADD     #0,B                              ; Clear carry bit
1838      P:000505 P:000507 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002C
1839      P:000507 P:000509 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002D
1840      P:000509 P:00050B 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1841   
1842      
1843      P:00050A P:00050C 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 38



                            FFFFCC
1844      P:00050C P:00050E 50F000            MOVE              X:BURST_SRC,A0
                            000030
1845      P:00050E P:000510 08C82F            MOVEP             A0,X:DSR0               ; DMA source
1846      P:00050F P:000511 200010            ADD     B,A
1847      P:000510 P:000512 00000B            DEC     B
1848      P:000511 P:000513 507000            MOVE              A0,X:BURST_SRC          ; BURST_SRC += BURST_SIZE/2
                            000030
1849   
1850      P:000513 P:000515 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1851   
1852      
1853      P:000514 P:000516 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
1854   
1855                                BLOCK_PCI
1856      
1857      P:000516 P:000518 200013            CLR     A
1858      P:000517 P:000519 20001B            CLR     B
1859      P:000518 P:00051A 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            00002D
1860      P:00051A P:00051C 00000B            DEC     B                                 ; n8 - 1
1861      P:00051B P:00051D 014088            ADD     #0,B                              ; Clear carry
1862      P:00051C P:00051E 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
1863      P:00051D P:00051F 014088            ADD     #0,B                              ; Clear carry
1864      P:00051E P:000520 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
1865   
1866      P:00051F P:000521 50F000            MOVE              X:BURST_DEST_HI,A0
                            00002F
1867   
1868      P:000521 P:000523 200010            ADD     B,A
1869      P:000522 P:000524 000000            NOP
1870      P:000523 P:000525 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
1871   
1872      P:000525 P:000527 280700            MOVE              #$07,A0
1873      P:000526 P:000528 014088            ADD     #0,B                              ; Clear carry
1874      P:000527 P:000529 0C1D20            ASL     #16,A,A
1875      P:000528 P:00052A 51F000            MOVE              X:BURST_DEST_LO,B0
                            00002E
1876      P:00052A P:00052C 200010            ADD     B,A
1877      P:00052B P:00052D 000000            NOP
1878   
1879      P:00052C P:00052E 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
1880   
1881                                BLOCK_CHECK
1882      P:00052D P:00052F 000000            NOP
1883      P:00052E P:000530 000000            NOP
1884      P:00052F P:000531 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for burst termination
                            00052F
1885   
1886      
1887      P:000531 P:000533 0A8AAE            JSET    #MDT,X:DPSR,BLOCK_OK
                            00053A
1888   
1889      P:000533 P:000535 0D049E            JSR     PCI_ERROR_CLEAR
1890   
1891      P:000534 P:000536 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1892      P:000536 P:000538 0E8540            JCS     <BLOCK_RESTART
1893   
1894      P:000537 P:000539 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 39



                            000000
1895      P:000539 P:00053B 0E8541            JCS     <BLOCK_RESUME
1896   
1897                                BLOCK_OK
1898      P:00053A P:00053C 50F000            MOVE              X:BURST_SIZE,A0         ; Pass # of words written to updater
                            00002D
1899      P:00053C P:00053E 0BF080            JSR     BLOCK_UPDATE
                            000553
1900      P:00053E P:000540 0C04F8            JMP     BLOCK_TRANSFER                    ; Finish the block
1901                                BLOCK_DONE
1902      P:00053F P:000541 00000C            RTS                                       ; Done
1903   
1904                                BLOCK_RESTART
1905      P:000540 P:000542 0C0516            JMP     BLOCK_PCI                         ; Recalculate pci and resend
1906   
1907                                BLOCK_RESUME
1908      P:000541 P:000543 200013            CLR     A
1909      P:000542 P:000544 20001B            CLR     B
1910      P:000543 P:000545 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
1911      P:000544 P:000546 0A8A8F            JCLR    #RDCQ,X:DPSR,BLOCK_RESUME1
                            000547
1912   
1913      P:000546 P:000548 000009            INC     B
1914   
1915                                BLOCK_RESUME1
1916   
1917      P:000547 P:000549 000009            INC     B                                 ; We want N, not N-1.
1918      P:000548 P:00054A 014088            ADD     #0,B                              ; Clear carry
1919      P:000549 P:00054B 0C1C20            ASR     #16,A,A
1920      P:00054A P:00054C 200018            ADD     A,B                               ; B is words remaining
1921      P:00054B P:00054D 014088            ADD     #0,B                              ; Clear carry
1922      P:00054C P:00054E 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
1923      P:00054D P:00054F 50F000            MOVE              X:BURST_SIZE,A0
                            00002D
1924      P:00054F P:000551 200014            SUB     B,A                               ; A is words written
1925   
1926      P:000550 P:000552 0BF080            JSR     BLOCK_UPDATE
                            000553
1927      P:000552 P:000554 0C0516            JMP     BLOCK_PCI                         ; Recalculate pci and resend
1928   
1929                                ; BLOCK_UPDATE
1930                                ;  Subtract A from BURST_SIZE and add A to BURST_DEST_LO
1931                                ;  Caller can check Z flag to see if BURST_SIZE is now 0.
1932                                BLOCK_UPDATE
1933      P:000553 P:000555 210500            MOVE              A0,X1                   ; Save A for later
1934      P:000554 P:000556 0C1D01            ASL     #0,A,B                            ; MOVE A,B
1935   
1936      P:000555 P:000557 60F400            MOVE              #BURST_DEST_LO,R0       ;
                            00002E
1937      P:000557 P:000559 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates BURST_DEST to BURST_DEST + B
                            00060A
1938   
1939      P:000559 P:00055B 57F000            MOVE              X:BURST_SIZE,B
                            00002D
1940      P:00055B P:00055D 20006C            SUB     X1,B                              ; Zero flag must be preserved!
1941      P:00055C P:00055E 000000            NOP
1942      P:00055D P:00055F 557000            MOVE              B1,X:BURST_SIZE
                            00002D
1943   
1944      P:00055F P:000561 00000C            RTS
1945   
1946   
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 40



1947                                ;----------------------------------------------;
1948                                ;  MCE PACKET PROCESSING                       ;
1949                                ;----------------------------------------------;
1950   
1951                                ;       Given a dword count in A, computes number of half FIFOs and
1952                                ;       number of left over FIFO reads required to get the whole
1953                                ;       packet.
1954   
1955                                ;       Input: A is packet size, in dwords
1956                                ;       Output: sets X:TOTAL_BUFFS and X:LEFT_TO_READ
1957                                ;       Trashes: A,B,X0
1958   
1959   
1960                                PACKET_PARTITIONS
1961      P:000560 P:000562 507000            MOVE              A0,X:PACKET_SIZE
                            00002B
1962   
1963      P:000562 P:000564 014088            ADD     #0,B                              ; Clear carry
1964      P:000563 P:000565 0C1D02            ASL     #1,A,A                            ;  * 2
1965      P:000564 P:000566 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
1966      P:000565 P:000567 240000            MOVE              #0,X0
1967      P:000566 P:000568 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
1968   
1969      P:000568 P:00056A 557000            MOVE              B1,X:TOTAL_BUFFS
                            000027
1970      P:00056A P:00056C 507000            MOVE              A0,X:LEFT_TO_READ
                            000028
1971      P:00056C P:00056E 00000C            RTS
1972   
1973   
1974                                ; BUFFER_PACKET
1975                                ;
1976                                ; Copies the packet in the FIFO to Y memory.
1977   
1978                                ; In: TOTAL_BUFFS and LEFT_TO_READ must be pre-set (via PACKET_PARTITIONS);
1979                                ;     R1 is the destination index in Y memory.
1980                                ; Trashes: R1 is updated to point to the end of the copied data.
1981   
1982                                BUFFER_PACKET
1983   
1984                                BUFFER_PACKET_HALFS
1985      P:00056D P:00056F 062700            DO      X:TOTAL_BUFFS,BUFFER_PACKET_SINGLES
                            000574
1986      P:00056F P:000571 0BF080            JSR     WAIT_FIFO_HALF
                            00058A
1987      P:000571 P:000573 0BF080            JSR     BUFFER_PACKET_HALF
                            000585
1988   
1989      
1990      
1991      P:000573 P:000575 01AD81            JCLR    #HF,X:PDRD,BUFFER_PACKET_SINGLES_FAST
                            000581
1992   
1993                                BUFFER_PACKET_SINGLES
1994      P:000575 P:000577 062800            DO      X:LEFT_TO_READ,BUFFER_PACKET_DONE
                            00057F
1995                                BUFFER_PACKET_SINGLE
1996      P:000577 P:000579 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            000275
1997      P:000579 P:00057B 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE
                            000577
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 41



1998      P:00057B P:00057D 000000            NOP
1999      P:00057C P:00057E 000000            NOP
2000      P:00057D P:00057F 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE   ; Protect against metastability
                            000577
2001      P:00057F P:000581 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2002                                BUFFER_PACKET_DONE
2003      P:000580 P:000582 00000C            RTS
2004   
2005                                BUFFER_PACKET_SINGLES_FAST
2006      P:000581 P:000583 062800            DO      X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_FAST_DONE
                            000583
2007      P:000583 P:000585 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2008                                BUFFER_PACKET_SINGLES_FAST_DONE
2009      P:000584 P:000586 00000C            RTS
2010   
2011                                BUFFER_PACKET_HALF
2012      
2013      P:000585 P:000587 060082            DO      #512,BUFFER_PACKET_HALF_DONE
                            000588
2014      P:000587 P:000589 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2015      P:000588 P:00058A 000000            NOP
2016                                BUFFER_PACKET_HALF_DONE
2017      P:000589 P:00058B 00000C            RTS
2018   
2019                                WAIT_FIFO_HALF
2020      P:00058A P:00058C 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            000593
2021      P:00058C P:00058E 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
                            00058A
2022      P:00058E P:000590 000000            NOP
2023      P:00058F P:000591 000000            NOP
2024      P:000590 P:000592 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            00058A
2025      P:000592 P:000594 00000C            RTS
2026   
2027                                FATALITY_HANDLER
2028      P:000593 P:000595 0C0100            JMP     START                             ; What could possibly go wrong?
2029   
2030   
2031                                ; DROP_PACKET
2032                                ;
2033                                ; Reads a packet from the fifo, discarding it.
2034                                ;
2035                                ; In: TOTAL_BUFFS & LEFT_TO_READ
2036                                ; Trashes: A0
2037   
2038                                DROP_PACKET
2039      P:000594 P:000596 062700            DO      X:TOTAL_BUFFS,DROP_PACKET_SINGLES
                            000599
2040      P:000596 P:000598 0D058A            JSR     WAIT_FIFO_HALF
2041      P:000597 P:000599 0BF080            JSR     DROP_FIFO_HALF
                            0005A4
2042      P:000599 P:00059B 000000            NOP
2043                                DROP_PACKET_SINGLES
2044      P:00059A P:00059C 062800            DO      X:LEFT_TO_READ,DROP_PACKET_DONE
                            0005A2
2045                                DROP_PACKET_SINGLE
2046      P:00059C P:00059E 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            000275
2047      P:00059E P:0005A0 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE
                            00059C
2048      P:0005A0 P:0005A2 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE     ; Protect against metastability
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 42



                            00059C
2049      P:0005A2 P:0005A4 09483F            MOVEP             Y:RDFIFO,A0
2050                                DROP_PACKET_DONE
2051      P:0005A3 P:0005A5 00000C            RTS
2052   
2053                                DROP_FIFO_HALF
2054      
2055      P:0005A4 P:0005A6 060082            DO      #512,DROP_FIFO_DONE
                            0005A6
2056      P:0005A6 P:0005A8 09483F            MOVEP             Y:RDFIFO,A0
2057                                DROP_FIFO_DONE
2058      P:0005A7 P:0005A9 00000C            RTS
2059   
2060   
2061                                ;----------------------------------------------;
2062                                ;  TIMER HANDLING                              ;
2063                                ;----------------------------------------------;
2064   
2065                                ; Start value is TLR, count is in TCR, int occurs at TCPR
2066                                ; Must set TCSR[TCIE] to enable int
2067                                ; Must set TCSR[T] for timer to restart
2068   
2069                                TIMER_ENABLE
2070      P:0005A8 P:0005AA 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2071      P:0005AA P:0005AC 000000            NOP
2072      P:0005AB P:0005AD 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2073      P:0005AD P:0005AF 00000C            RTS
2074   
2075                                TIMER_DISABLE
2076      P:0005AE P:0005B0 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
                            300200
2077      P:0005B0 P:0005B2 000000            NOP
2078      P:0005B1 P:0005B3 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2079      P:0005B3 P:0005B5 00000C            RTS
2080   
2082                                TIMER_ACTION
2083      P:0005B4 P:0005B6 56F000            MOVE              X:QT_INFORM_IDX,A
                            000043
2084      P:0005B6 P:0005B8 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2085      P:0005B8 P:0005BA 000000            NOP
2086      P:0005B9 P:0005BB 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2087      P:0005BB P:0005BD 0140C5            CMP     #>0,A                             ; If inform_idx != 0
                            000000
2088      P:0005BD P:0005BF 0AF0AA            JEQ     TIMER_ACTION_OK
                            0005C1
2089      P:0005BF P:0005C1 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2090                                TIMER_ACTION_OK
2091      P:0005C1 P:0005C3 00000C            RTS
2092   
2093   
2094   
2095                                ;----------------------------------------------;
2096                                ;  CIRCULAR BUFFER HANDLING                    ;
2097                                ;----------------------------------------------;
2098   
2099                                BUFFER_INCR
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 43



2100   
2101      P:0005C2 P:0005C4 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
                            00003F
2102      P:0005C4 P:0005C6 014180            ADD     #1,A                              ;
2103      P:0005C5 P:0005C7 57F000            MOVE              X:QT_BUF_MAX,B          ;
                            00003C
2104      P:0005C7 P:0005C9 20000D            CMP     A,B                               ;
2105      P:0005C8 P:0005CA 0AF0AF            JLE     BUFFER_RESET                      ;       head = 0
                            0005D4
2106                                                                                    ; else
2107      P:0005CA P:0005CC 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            00003F
2108   
2109      P:0005CC P:0005CE 20001B            CLR     B
2110      P:0005CD P:0005CF 51F000            MOVE              X:QT_BUF_SIZE,B0
                            00003B
2111      P:0005CF P:0005D1 60F400            MOVE              #QT_DEST_LO,R0
                            000041
2112      P:0005D1 P:0005D3 0BF080            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
                            00060A
2113   
2114      P:0005D3 P:0005D5 00000C            RTS
2115   
2116   
2117                                BUFFER_RESET
2118      P:0005D4 P:0005D6 60F400            MOVE              #QT_BASE_LO,R0
                            000039
2119      P:0005D6 P:0005D8 0BF080            JSR     LOAD_HILO_ADDRESS
                            000604
2120      P:0005D8 P:0005DA 60F400            MOVE              #QT_DEST_LO,R0
                            000041
2121      P:0005DA P:0005DC 0BF080            JSR     SAVE_HILO_ADDRESS                 ; QT_DEST_LO = QT_BASE_LO
                            00060C
2122   
2123      P:0005DC P:0005DE 240000            MOVE              #0,X0
2124      P:0005DD P:0005DF 447000            MOVE              X0,X:QT_BUF_HEAD        ; HEAD = 0
                            00003F
2125      P:0005DF P:0005E1 00000C            RTS
2126   
2127   
2128                                BUFFER_INFORM_CHECK
2129      P:0005E0 P:0005E2 56F000            MOVE              X:QT_INFORM_IDX,A
                            000043
2130      P:0005E2 P:0005E4 014180            ADD     #1,A
2131      P:0005E3 P:0005E5 57F000            MOVE              X:QT_INFORM,B
                            00003E
2132      P:0005E5 P:0005E7 20000D            CMP     A,B
2133      P:0005E6 P:0005E8 0AF0A7            JGT     BUFFER_INFORM_OK                  ; If inform_idx + 1 <= inform
                            0005EA
2134      P:0005E8 P:0005EA 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2135   
2136                                BUFFER_INFORM_OK
2137      P:0005EA P:0005EC 567000            MOVE              A,X:QT_INFORM_IDX       ; inform_idx = inform_idx + 1
                            000043
2138      P:0005EC P:0005EE 00000C            RTS
2139   
2140   
2141                                ;---------------------------------------------------------------
2142                                BUFFER_INFORM
2143                                ;---------------------------------------------------------------
2144                                ; Informs host of current buffer status
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 44



2145   
2146      P:0005ED P:0005EF 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2147      P:0005EF P:0005F1 440B00            MOVE              X0,X:<DTXS_WD1
2148   
2149      P:0005F0 P:0005F2 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            00003F
2150      P:0005F2 P:0005F4 440C00            MOVE              X0,X:<DTXS_WD2
2151   
2152      P:0005F3 P:0005F5 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000040
2153      P:0005F5 P:0005F7 440D00            MOVE              X0,X:<DTXS_WD3
2154   
2155      P:0005F6 P:0005F8 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
                            000044
2156      P:0005F8 P:0005FA 440E00            MOVE              X0,X:<DTXS_WD4
2157   
2158   
2159      P:0005F9 P:0005FB 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            000603
2160      P:0005FB P:0005FD 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            000603
2161   
2162      P:0005FD P:0005FF 0D0422            JSR     PCI_MESSAGE_TO_HOST
2163   
2164      P:0005FE P:000600 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
2165      P:000600 P:000602 240000            MOVE              #0,X0                   ; Reset inform index
2166      P:000601 P:000603 447000            MOVE              X0,X:QT_INFORM_IDX
                            000043
2167                                INFORM_EXIT
2168      P:000603 P:000605 00000C            RTS
2169   
2170   
2171   
2172                                ;----------------------------------------------;
2173                                ;  ADDRESS HANDLING                            ;
2174                                ;----------------------------------------------;
2175   
2179   
2180                                LOAD_HILO_ADDRESS
2181      
2182      
2183      P:000604 P:000606 200013            CLR     A
2184      P:000605 P:000607 50D800            MOVE              X:(R0)+,A0
2185      P:000606 P:000608 44D000            MOVE              X:(R0)-,X0
2186      P:000607 P:000609 0C1940            INSERT  #$010010,X0,A
                            010010
2187      P:000609 P:00060B 00000C            RTS
2188   
2189                                ADD_HILO_ADDRESS
2190      
2191      
2192   
2193      P:00060A P:00060C 0D0604            JSR     LOAD_HILO_ADDRESS
2194      P:00060B P:00060D 200010            ADD     B,A
2195   
2196                                SAVE_HILO_ADDRESS
2197      
2198      
2199   
2200      P:00060C P:00060E 445800            MOVE              X0,X:(R0)+              ; pre-increment
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  main.asm  Page 45



2201      P:00060D P:00060F 240000            MOVE              #0,X0
2202      P:00060E P:000610 0C1D11            ASL     #8,A,B
2203      P:00060F P:000611 0C1940            INSERT  #$008010,X0,A
                            008010
2204      P:000611 P:000613 555000            MOVE              B1,X:(R0)-              ; store hi16
2205      P:000612 P:000614 506000            MOVE              A0,X:(R0)
2206      P:000613 P:000615 0C1C90            ASR     #8,B,A
2207      P:000614 P:000616 00000C            RTS
2208   
2209   
2210                                BOOTCODE_END
2211                                 BOOTEND_ADDR
2212      000615                              EQU     @CVI(BOOTCODE_END)
2213   
2214                                PROGRAM_END
2215      000615                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2216                                          INCLUDE 'vars.asm'
2217                                      COMMENT *
2218   
2219                                Variable table and bit defines for our variables.
2220   
2221                                See info.asm for versioning and authors.
2222   
2223                                        *
2224   
2225   
2226                                ; The variable table is mapped to X memory but stored inline in the
2227                                ; eeprom / P memory after the main code (but before the application
2228                                ; area).
2229   
2230      X:000000 P:000617                   ORG     X:VAR_TBL,P:
2231   
2232   
2233                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2234                                 VAR_TBL_START
2235      000615                              EQU     @LCV(L)-2
2236                                          ENDIF
2237   
2238                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2240                                          ENDIF
2241   
2242                                ; -----------------------------------------------
2243 d    X:000000 P:000617 000000  STATUS    DC      0                                 ; Internal control flags
2244 d    X:000001 P:000618 000000  MODE      DC      0                                 ; Configure special options
2245   
2246 d                               FRAME_COUNT
2247 d    X:000002 P:000619 000000            DC      0
2248 d    X:000003 P:00061A 550105  REV_NUMBER DC     $550105                           ; byte 0 = minor revision #
2249                                                                                    ; byte 1 = major revision #
2250                                                                                    ; byte 2 = release Version (ascii letter)
2251 d    X:000004 P:00061B 000000  REV_DATA  DC      $000000                           ; data: day-month-year
2252 d    X:000005 P:00061C 2EF490  P_CHECKSUM DC     $2EF490                           ;**** DO NOT CHANGE
2253                                ; -------------------------------------------------
2254 d    X:000006 P:00061D 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2255                                ; ----------------------------------------------------------------------------------------------
----------------
2256   
2257 d    X:000007 P:00061E 000000  DRXR_WD1  DC      0
2258 d    X:000008 P:00061F 000000  DRXR_WD2  DC      0
2259 d    X:000009 P:000620 000000  DRXR_WD3  DC      0
2260 d    X:00000A P:000621 000000  DRXR_WD4  DC      0
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  vars.asm  Page 46



2261 d    X:00000B P:000622 000000  DTXS_WD1  DC      0
2262 d    X:00000C P:000623 000000  DTXS_WD2  DC      0
2263 d    X:00000D P:000624 000000  DTXS_WD3  DC      0
2264 d    X:00000E P:000625 000000  DTXS_WD4  DC      0
2265   
2266 d    X:00000F P:000626 000000  HEAD_W1_1 DC      0
2267 d    X:000010 P:000627 000000  HEAD_W1_0 DC      0
2268 d    X:000011 P:000628 000000  HEAD_W2_1 DC      0
2269 d    X:000012 P:000629 000000  HEAD_W2_0 DC      0
2270 d    X:000013 P:00062A 000000  HEAD_W3_1 DC      0
2271 d    X:000014 P:00062B 000000  HEAD_W3_0 DC      0
2272 d    X:000015 P:00062C 000000  HEAD_W4_1 DC      0
2273 d    X:000016 P:00062D 000000  HEAD_W4_0 DC      0
2274   
2275 d    X:000017 P:00062E 000000  SV_A0     DC      0
2276 d    X:000018 P:00062F 000000  SV_A1     DC      0
2277 d    X:000019 P:000630 000000  SV_A2     DC      0
2278 d    X:00001A P:000631 000000  SV_B0     DC      0
2279 d    X:00001B P:000632 000000  SV_B1     DC      0
2280 d    X:00001C P:000633 000000  SV_B2     DC      0
2281 d    X:00001D P:000634 000000  SV_X0     DC      0
2282 d    X:00001E P:000635 000000  SV_X1     DC      0
2283 d    X:00001F P:000636 000000  SV_Y0     DC      0
2284 d    X:000020 P:000637 000000  SV_Y1     DC      0
2285 d    X:000021 P:000638 000000  SV_R0     DC      0
2286   
2287 d    X:000022 P:000639 000000  SV_SR     DC      0                                 ; stauts register save.
2288   
2289 d                               PACKET_SIZE_LOW
2290 d    X:000023 P:00063A 000000            DC      0
2291 d                               PACKET_SIZE_HIH
2292 d    X:000024 P:00063B 000000            DC      0
2293   
2294 d    X:000025 P:00063C 00A5A5  PREAMB1   DC      $A5A5                             ; pramble 16-bit word....2 of which make up 
first preamble 32bit word
2295 d    X:000026 P:00063D 005A5A  PREAMB2   DC      $5A5A                             ; preamble 16-bit word....2 of which make up
 second preamble 32bit word
2296   
2297 d                               TOTAL_BUFFS
2298 d    X:000027 P:00063E 000000            DC      0                                 ; total number of 512 buffers in packet
2299 d                               LEFT_TO_READ
2300 d    X:000028 P:00063F 000000            DC      0                                 ; number of words (16 bit) left to read afte
r last 512 buffer
2301 d                               LEFT_TO_WRITE
2302 d    X:000029 P:000640 000000            DC      0                                 ; number of woreds (32 bit) to write to host
 i.e. half of those left over read
2303 d                               NUM_LEFTOVER_BLOCKS
2304 d    X:00002A P:000641 000000            DC      0                                 ; small block DMA burst transfer
2305   
2306 d                               PACKET_SIZE
2307 d    X:00002B P:000642 000000            DC      0                                 ; Size, in dwords of most recent packet from
 MCE.
2308   
2309   
2311   
2312 d    X:00002C P:000643 000000  BLOCK_SIZE DC     0
2313 d    X:00002D P:000644 000000  BURST_SIZE DC     0
2314 d                               BURST_DEST_LO
2315 d    X:00002E P:000645 000000            DC      0
2316 d                               BURST_DEST_HI
2317 d    X:00002F P:000646 000000            DC      0
2318 d    X:000030 P:000647 000000  BURST_SRC DC      0
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  vars.asm  Page 47



2319   
2320 d    X:000031 P:000648 000000  DMA_ERRORS DC     0
2321 d    X:000032 P:000649 000000  EC_TRTY   DC      0
2322 d    X:000033 P:00064A 000000  EC_TO     DC      0
2323 d    X:000034 P:00064B 000000  EC_TDIS   DC      0
2324 d    X:000035 P:00064C 000000  EC_TAB    DC      0
2325 d    X:000036 P:00064D 000000  EC_MAB    DC      0
2326 d    X:000037 P:00064E 000000  EC_DPER   DC      0
2327 d    X:000038 P:00064F 000000  EC_APER   DC      0
2328   
2329   
2331   
2332 d    X:000039 P:000650 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2333 d    X:00003A P:000651 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2334 d                               QT_BUF_SIZE
2335 d    X:00003B P:000652 000000            DC      0                                 ; Separation of buffers, in bytes
2336 d    X:00003C P:000653 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2337 d                               QT_FRAME_SIZE
2338 d    X:00003D P:000654 000000            DC      0                                 ; Expected data packet size, in bytes
2339 d    X:00003E P:000655 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2340   
2341 d                               QT_BUF_HEAD
2342 d    X:00003F P:000656 000000            DC      0                                 ; Index of buf for next write
2343 d                               QT_BUF_TAIL
2344 d    X:000040 P:000657 000000            DC      0                                 ; Index at which we must not write
2345   
2346 d    X:000041 P:000658 000000  QT_DEST_LO DC     0                                 ; PC address for next write
2347 d    X:000042 P:000659 000000  QT_DEST_HI DC     0                                 ;
2348 d                               QT_INFORM_IDX
2349 d    X:000043 P:00065A 000000            DC      0                                 ; Number of packets since last inform
2350 d    X:000044 P:00065B 000000  QT_DROPS  DC      0                                 ; Dropped packets
2351   
2352   
2354 d    X:000045 P:00065C 000000  RP_BASE_LO DC     0
2355 d    X:000046 P:00065D 000000  RP_BASE_HI DC     0
2356 d                               RP_MAX_SIZE
2357 d    X:000047 P:00065E 000000            DC      0
2358 d    X:000048 P:00065F 000000  RP_DROPS  DC      0
2359   
2361 d                               CON_SOURCE_LO
2362 d    X:000049 P:000660 000000            DC      0
2363 d                               CON_SOURCE_HI
2364 d    X:00004A P:000661 000000            DC      0
2365   
2367 d                               PCI_BURST_SIZE
2368 d    X:00004B P:000662 000040            DC      $40                               ; Should be < 4*latency assigned by OS
2369   
2370 d    X:00004C P:000663 000000  BDEBUG0   DC      0
2371 d    X:00004D P:000664 000000  BDEBUG1   DC      0
2372 d    X:00004E P:000665 000000  BDEBUG2   DC      0
2373 d    X:00004F P:000666 000000  BDEBUG3   DC      0
2374 d    X:000050 P:000667 000000  BDEBUG4   DC      0
2375 d    X:000051 P:000668 000000  BDEBUG5   DC      0
2376 d    X:000052 P:000669 000000  BDEBUG6   DC      0
2377 d    X:000053 P:00066A 000000  BDEBUG7   DC      0
2378 d    X:000054 P:00066B 000000  BDEBUG8   DC      0
2379 d    X:000055 P:00066C 000000  BDEBUG9   DC      0
2380   
2381                                ;----------------------------------------------------------
2382   
2384   
2385                                 APPLICATION_RUNNING
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  vars.asm  Page 48



2386      000000                              EQU     0                                 ; Indicates application is in progress
2387                                 SEND_TO_HOST
2388      000001                              EQU     1                                 ; set in HST ISR when host ready for packet 
(stays set until after HST reply)
2389                                 FATAL_ERROR
2390      000002                              EQU     2                                 ; PCI message to host error detected by driv
er....
2391      000003                    FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays s
et till packet processed
2392   
2393                                 PREAMBLE_ERROR
2394      000006                              EQU     6                                 ; set if preamble error detected
2395      000007                    DATA_DLY  EQU     7                                 ; set in CON ISR if MCE command is 'GO'.  US
ed to add delay to first returned data packet
2396   
2397      000009                    HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of p
acket (stays set until after HST reply)
2398   
2399      00000A                    CON_DEMAND EQU    10                                ; Host has requested an MCE command be sent
2400   
2401                                 PCIDMA_RESTART
2402      000010                              EQU     16                                ; DMA flags used for error recovery
2403                                 PCIDMA_RESUME
2404      000011                              EQU     17
2405                                 PCIDMA_RETRY
2406      000012                              EQU     18
2407   
2408      000014                    QT_FLUSH  EQU     20                                ; Set when it is time to inform Host of curr
ent buffer position.
2409                                 RP_BUFFER_FULL
2410      000015                              EQU     21                                ; Set when Quiet RP buffer is occupied.
2411   
2413   
2414                                 MODE_APPLICATION
2415      000000                              EQU     0                                 ; set if PCI application to run
2416      000001                    MODE_CHOKE EQU    1                                 ; drop all packets from MCE
2417      000002                    MODE_QT   EQU     2                                 ; Quiet transfer for data packets
2418                                 MODE_RP_BUFFER
2419      000003                              EQU     3                                 ; Quiet transfer for reply packets
2420      000004                    MODE_IRQ  EQU     4                                 ; Enable PCI interrupts on NFY
2421   
2422   
2423                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2424                                 VAR_TBL_END
2425      00066B                              EQU     @LCV(L)-2
2426                                          ENDIF
2427   
2428                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2430                                          ENDIF
2431   
2432                                 VAR_TBL_LENGTH
2433      000056                              EQU     VAR_TBL_END-VAR_TBL_START
2434                                          INCLUDE 'app.asm'
2435                                        COMMENT *
2436   
2437                                Auxiliary application area.
2438   
2439                                See info.asm for versioning and authors.
2440   
2441                                        *
2442                                          PAGE    132                               ; Printronix page width - 132 columns
2443                                          OPT     CEX                               ; print DC evaluations
Motorola DSP56300 Assembler  Version 6.3.4   08-10-19  20:19:58  app.asm  Page 49



2444   
2445                                          IF      @CVS(N,*)>=APPLICATION
2447                                          ENDIF
2448   
2449   
2450                                ;--------------------------------------------
2451                                ; APPLICATION AREA
2452                                ;---------------------------------------------
2453                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2454      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2455                                          ENDIF
2456   
2457                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2459                                          ENDIF
2460   
2461                                ; starts with no application loaded
2462                                ; so just reply with an error if we get a GOA command
2463   
2464      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2465      P:000802 P:000804 440B00            MOVE              X0,X:<DTXS_WD1          ; REPly
2466      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2467      P:000805 P:000807 440C00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2468      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2469      P:000808 P:00080A 440D00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2470      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2471      P:00080B P:00080D 440E00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2472      P:00080C P:00080E 0D045B            JSR     <RESTORE_REGISTERS
2473      P:00080D P:00080F 0D0422            JSR     <PCI_MESSAGE_TO_HOST
2474      P:00080E P:000810 0A0100            BCLR    #MODE_APPLICATION,X:<MODE
2475      P:00080F P:000811 0C016D            JMP     PACKET_IN
2476   
2477   
2478      000812                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM
2479   
2480   
2481   

0    Errors
0    Warnings


