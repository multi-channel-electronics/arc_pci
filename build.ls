Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  build.asm  Page 1



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
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  header.asm  Page 2



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
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  header.asm  Page 3



124       000009           TDIS      EQU     9                                 ; Target Disconnect
125       00000B           TO        EQU     11                                ; Timeout
126       00000E           MDT       EQU     14                                ; Master Data Transfer complete
127       000002           SCLK      EQU     2                                 ; SCLK = transmitter special code
128    
129                        ; bits in DPMC
130    
131       000017           FC1       EQU     23
132       000016           FC0       EQU     22
133    
134    
135                        ; DMA register definitions
136       FFFFEF           DSR0      EQU     $FFFFEF                           ; Source address register
137       FFFFEE           DDR0      EQU     $FFFFEE                           ; Destination address register
138       FFFFED           DCO0      EQU     $FFFFED                           ; Counter register
139       FFFFEC           DCR0      EQU     $FFFFEC                           ; Control register
140    
141                        ; The DCTR host flags are written by the DSP and read by PCI host
142       000003           DCTR_HF3  EQU     3                                 ; used as a semiphore for INTA handshaking
143       000004           DCTR_HF4  EQU     4                                 ;
144       000005           DCTR_HF5  EQU     5                                 ;
145       000006           INTA      EQU     6                                 ; Request PCI interrupt
146    
147                        ; The DSR host flags are written by the PCI host and read by the DSP
148       000004           DSR_BUF0  EQU     4                                 ; PCI host sets this when copying buffer 0
149       000005           DSR_BUF1  EQU     5                                 ; PCI host sets this when copying buffer 1
150    
151                        ; DPCR bit definitions
152       00000E           CLRT      EQU     14                                ; Clear transmitter
153       000012           MACE      EQU     18                                ; Master access counter enable
154       000015           IAE       EQU     21                                ; Insert Address Enable
155    
156                        ; Addresses of ESSI port
157       FFFFBC           TX00      EQU     $FFFFBC                           ; Transmit Data Register 0
158       FFFFB7           SSISR0    EQU     $FFFFB7                           ; Status Register
159       FFFFB6           CRB0      EQU     $FFFFB6                           ; Control Register B
160       FFFFB5           CRA0      EQU     $FFFFB5                           ; Control Register A
161    
162                        ; SSI Control Register A Bit Flags
163       000006           TDE       EQU     6                                 ; Set when transmitter data register is empty
164    
165                        ; Miscellaneous addresses
166       FFFFFF           RDFIFO    EQU     $FFFFFF                           ; Read the FIFO for incoming fiber optic data
167    
168                        ; Timer registers
169       FFFF8F           TCSR0     EQU     $FFFF8F                           ; Control and status register
170       FFFF8E           TLR0      EQU     $FFFF8E                           ; Load register
171       FFFF8D           TCPR0     EQU     $FFFF8D                           ; Compare register
172       FFFF8C           TCR0      EQU     $FFFF8C                           ; Count register
173       FFFF8B           TCSR1     EQU     $FFFF8B                           ; Control and status register
174       FFFF8A           TLR1      EQU     $FFFF8A                           ; Load register
175       FFFF89           TCPR1     EQU     $FFFF89                           ; Compare register
176       FFFF88           TCR1      EQU     $FFFF88                           ; Count register
177       FFFF87           TCSR2     EQU     $FFFF87                           ; Control and status register
178       FFFF86           TLR2      EQU     $FFFF86                           ; Load register
179       FFFF85           TCPR2     EQU     $FFFF85                           ; Compare register
180       FFFF84           TCR2      EQU     $FFFF84                           ; Count register
181    
182                        ;***************************************************************
183                        ; Phase Locked Loop initialization
184       050003           PLL_INIT  EQU     $050003                           ; PLL = 25 MHz x 4 = 100 MHz
185                        ;****************************************************************
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  header.asm  Page 4



186    
187                        ; Port C is Enhanced Synchronous Serial Port 0
188       FFFFBF           PCRC      EQU     $FFFFBF                           ; Port C Control Register
189       FFFFBE           PRRC      EQU     $FFFFBE                           ; Port C Data direction Register
190       FFFFBD           PDRC      EQU     $FFFFBD                           ; Port C GPIO Data Register
191    
192                        ; Port D is Enhanced Synchronous Serial Port 1
193       FFFFAF           PCRD      EQU     $FFFFAF                           ; Port D Control Register
194       FFFFAE           PRRD      EQU     $FFFFAE                           ; Port D Data direction Register
195       FFFFAD           PDRD      EQU     $FFFFAD                           ; Port D GPIO Data Register
196    
197                        ; Bit number definitions of GPIO pins on Port C
198       000002           ROM_FIFO  EQU     2                                 ; Select ROM or FIFO accesses for AA1
199       000004           AUX1      EQU     4                                 ; enable/disable byte swapping
200    
201                        ; Bit number definitions of GPIO pins on Port D
202       000000           EF        EQU     0                                 ; FIFO Empty flag, low true
203       000001           HF        EQU     1                                 ; FIFO half full flag, low true
204       000002           RS        EQU     2                                 ; FIFO reset signal, low true
205       000003           FSYNC     EQU     3                                 ; High during image transmission
206       000005           WRFIFO    EQU     5                                 ; Low true if FIFO is being written to
207    
208    
209                        ; Errors - self test application
210    
211       000000           Y_MEM_ER  EQU     0                                 ; y memory corrupted
212       000001           X_MEM_ER  EQU     1                                 ; x memory corrupted
213       000002           P_MEM_ER  EQU     2                                 ; p memory corrupted
214       000003           FO_EMPTY  EQU     3                                 ; no transmitted data in FIFO
215    
216       000004           FO_OVER   EQU     4                                 ; too much data received
217       000005           FO_UNDER  EQU     5                                 ; not enough data receiv
218       000006           FO_RX_ER  EQU     6                                 ; received data in FIFO incorrect.
219       000007           DEBUG     EQU     7                                 ; debug bit
220    
221    
223       000000           TE        EQU     0
224       000001           TOIE      EQU     1
225       000002           TCIE      EQU     2
226       000014           TOF       EQU     20
227       000015           TCF       EQU     21
228    
229    
231    
232       FFF000           FO_SEND   EQU     $FFF000
233                                  INCLUDE 'init.asm'
234                                COMMENT *
235    
236                        Initial configuration, and ISR vector definitions.
237    
238                        See info.asm for versioning and authors.
239    
240                                *
241                                  PAGE    132                               ; Printronix page width - 132 columns
242                                  OPT     CEX                               ; print DC evaluations
243    
244                        ; The EEPROM boot code expects first to read 3 bytes specifying the number of
245                        ; program words, then 3 bytes specifying the address to start loading the
246                        ; program words and then 3 bytes for each program word to be loaded.
247                        ; The program words will be condensed into 24 bit words and stored in contiguous
248                        ; PRAM memory starting at the specified starting address. Program execution
249                        ; starts from the same address where loading started.
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  init.asm  Page 5



250    
251                        ; Special address for two words for the DSP to bootstrap code from the EEPROM
252                                  IF      @SCP("ROM","ROM")                 ; Boot from ROM on power-on
253       P:000000 P:000000                   ORG     P:0,P:0
254  d    P:000000 P:000000 000810            DC      END_ADR-INIT-2                    ; Number of boot words
255  d    P:000001 P:000001 000000            DC      INIT                              ; Starting address
256       P:000000 P:000002                   ORG     P:0,P:2
257       P:000000 P:000002 0C0030  INIT      JMP     <INIT_PCI                         ; Configure PCI port
258       P:000001 P:000003 000000            NOP
259                                           ENDIF
260    
261    
262                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
263                                 ; command converter
264                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
268                                           ENDIF
269    
270                                 ; Vectored interrupt table, addresses at the beginning are reserved
271  d    P:000002 P:000004 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0       ; $02-$0f Reserved
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
272  d    P:000010 P:000012 000000            DC      0,0                               ; $10-$13 Reserved
     d                      000000
273    
274                                 ; FIFO HF* flag interrupt vector is here at $12 - this is connected to the
275                                 ; IRQB* interrupt line so its ISR vector must be here
276  d    P:000012 P:000014 000000            DC      0,0                               ; $was ld scatter routine ...HF*
     d                      000000
277    
278                                 ; a software reset button on the font panel of the card is connected to the IRQC*
279                                 ; line which if pressed causes the DSP to jump to an ISR which causes the program
280                                 ; counter to the beginning of the program INIT and sets the stack pointer to TOP.
281       P:000014 P:000016 0BF080            JSR     CLEAN_UP_PCI                      ; $14 - Software reset switch
                            0003C6
282    
283  d    P:000016 P:000018 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Reserved interrupts
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
284  d    P:000022 P:000024 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0
     d                      000000
     d                      000000
     d                      000000
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  init.asm  Page 6



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
285    
286                                 ; Now we're at P:$30, where some unused vector addresses are located
287                                 ; This is ROM only code that is only executed once on power-up when the
288                                 ; ROM code is downloaded. It is skipped over on OnCE downloads.
289    
290                                 ; A few seconds after power up on the Host, it interrogates the PCI bus to find
291                                 ; out what boards are installed and configures this PCI board. The EEPROM booting
292                                 ; procedure ends with program execution  starting at P:$0 where the EEPROM has
293                                 ; inserted a JMP INIT_PCI instruction. This routine sets the PLL paramter and
294                                 ; does a self configuration and software reset of the PCI controller in the DSP.
295                                 ; After configuring the PCI controller the DSP program overwrites the instruction
296                                 ; at P:$0 with a new JMP START to skip over the INIT_PCI routine. The program at
297                                 ; START address begins configuring the DSP and processing commands.
298                                 ; Similarly the ONCE option places a JMP START at P:$0 to skip over the
299                                 ; INIT_PCI routine. If this routine where executed after the host computer had booted
300                                 ; it would cause it to crash since the host computer would overwrite the
301                                 ; configuration space with its own values and doesn't tolerate foreign values.
302    
303                                 ; Initialize the PLL - phase locked loop
304                                 INIT_PCI
305       P:000030 P:000032 08F4BD            MOVEP             #PLL_INIT,X:PCTL        ; Initialize PLL
                            050003
306       P:000032 P:000034 000000            NOP
307    
308                                 ; Program the PCI self-configuration registers
309       P:000033 P:000035 240000            MOVE              #0,X0
310       P:000034 P:000036 08F485            MOVEP             #$500000,X:DCTR         ; Set self-configuration mode
                            500000
311       P:000036 P:000038 0604A0            REP     #4
312       P:000037 P:000039 08C408            MOVEP             X0,X:DPAR               ; Dummy writes to configuration space
313       P:000038 P:00003A 08F487            MOVEP             #>$0000,X:DPMC          ; Subsystem ID
                            000000
314       P:00003A P:00003C 08F488            MOVEP             #>$0000,X:DPAR          ; Subsystem Vendor ID
                            000000
315    
316                                 ; PCI Personal reset
317       P:00003C P:00003E 08C405            MOVEP             X0,X:DCTR               ; Personal software reset
318       P:00003D P:00003F 000000            NOP
319       P:00003E P:000040 000000            NOP
320       P:00003F P:000041 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00003F
321       P:000041 P:000043 07F084            MOVE              P:(*+3),X0              ; Trick to write "JMP <START" to P:0
                            000044
322       P:000043 P:000045 070004            MOVE              X0,P:(0)
323       P:000044 P:000046 0C0100            JMP     <START
324    
325  d    P:000045 P:000047 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  init.asm  Page 7



     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
326  d    P:000051 P:000053 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
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
327  d    P:00005D P:00005F 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; $60-$71 Reserved PCI
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
328    
329                                 ;**************************************************************************
330                                 ; Check for program space overwriting of ISR starting at P:$72
331                                           IF      @CVS(N,*)>$71
333                                           ENDIF
334    
335                                 ;       ORG     P:$72,P:$72
336       P:000072 P:000074                   ORG     P:$72,P:$74
337    
338                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
339                                 ; command converter
340                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
342                                           ENDIF
343    
344    
345                                 ;**************************************************************************
346    
347                                 ; Three non-maskable fast interrupt service routines for clearing PCI interrupts
348                                 ; The Host will use these to clear the INTA* after it has serviced the interrupt
349                                 ; which had been generated by the PCI board.
350    
351       P:000072 P:000074 0A8506            BCLR    #INTA,X:DCTR                      ; $72/3 - Clear PCI interrupt
352       P:000073 P:000075 000000            NOP
353    
354       P:000074 P:000076 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; clear interrupt flag
355       P:000075 P:000077 000000            NOP                                       ; needs to be fast addressing <
356    
357       P:000076 P:000078 0A0022            BSET    #FATAL_ERROR,X:<STATUS            ; $76/7 - driver PCI_MESSAGE_TO_HOST error
358       P:000077 P:000079 000000            NOP
359    
360                                 ; Interrupt locations for 7 available commands on PCI board
361                                 ; Each JSR takes up 2 locations in the table
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  init.asm  Page 8



362       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            0002E3
363       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            0002B8
364       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            000304
365       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            00030D
366                                 ; software reset is the same as cleaning up the PCI - use same routine
367                                 ; when HOST does a RESET then this routine is run
368       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            0003DD
369       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            0003F6
370       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            0003CE
371       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            000318
372    
373                                 ; QT - set command
374       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            000336
375       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            0003BE
376    
377                                 ; Quiet RP mode, clear buffer full flag
378       P:00008C P:00008E 0A0015            BCLR    #RP_BUFFER_FULL,X:<STATUS         ; $8C
379       P:00008D P:00008F 000000            NOP
380    
381                                 ; Disable PCI interrupts
382       P:00008E P:000090 0A0104            BCLR    #MODE_IRQ,X:<MODE                 ; $8E
383       P:00008F P:000091 000000            NOP
384    
385                                 ; Enable PCI interrupts
386       P:000090 P:000092 0A0124            BSET    #MODE_IRQ,X:<MODE                 ; $90
387       P:000091 P:000093 000000            NOP
388    
389                                 ; ***********************************************************************
390                                 ; For now have boot code starting from P:$100
391                                 ; just to make debugging tidier etc.
392    
393       P:000100 P:000102                   ORG     P:$100,P:$102
394    
395                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
396                                 ; command converter
397                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
399                                           ENDIF
400                                 ; ***********************************************************************
401    
402    
403    
404                                 ; ******************************************************************
405                                 ;
406                                 ;       AA0 = RDFIFO* of incoming fiber optic data
407                                 ;       AA1 = EEPROM access
408                                 ;       AA2 = DRAM access
409                                 ;       AA3 = output to parallel data connector, for a video pixel clock
410                                 ;       $FFxxxx = Write to fiber optic transmitter
411                                 ;
412                                 ; ******************************************************************
413    
414    
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  init.asm  Page 9



415       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
416       P:000102 P:000104 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
417       P:000103 P:000105 0A8515            BCLR    #21,X:DCTR
418       P:000104 P:000106 0A8516            BCLR    #22,X:DCTR
419       P:000105 P:000107 000000            NOP
420       P:000106 P:000108 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
421       P:000107 P:000109 000000            NOP
422       P:000108 P:00010A 000000            NOP                                       ; End of PCI programming
423    
424    
425                                 ; Set operation mode register OMR to normal expanded
426       P:000109 P:00010B 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
427       P:00010A P:00010C 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
428    
429                                 ; Program the serial port ESSI0 = Port C for serial transmission to
430                                 ;   the timing board
431       P:00010B P:00010D 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
432                                 ;**********************************************************************
433       P:00010D P:00010F 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
434                                                                                     ; DC0-CD4 = 0 for non-network operation
435                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
436                                                                                     ; SSC1 = 0 for SC1 not used
437                                 ;************************************************************************
438       P:00010F P:000111 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
                            010120
439                                                                                     ; SHFD = 0 for MSB shifted first
440                                                                                     ; CKP = 0 for rising clock edge transitions
441                                                                                     ; TE0 = 1 to enable transmitter #0
442                                                                                     ; MOD = 0 for normal, non-networked mode
443                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
444       P:000111 P:000113 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
445                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
446                                 ;********************************************************************************
447       P:000113 P:000115 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
448       P:000115 P:000117 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
                            000000
449                                 ;***********************************************************************************
450                                 ; 250MHz
451                                 ; Conversion from software bits to schematic labels for Port C and D
452                                 ;       PC0 = SC00 = AUX3               PD0 = SC10 = EF*
453                                 ;       PC1 = SC01 = A/B* = input       PD1 = SC11 = HF*
454                                 ;       PC2 = SC02 = No connect         PD2 = SC12 = RS*
455                                 ;       PC3 = SCK0 = No connect         PD3 = SCK1 = NWRFIFO*
456                                 ;       PC4 = SRD0 = AUX1               PD4 = SRD1 = No connect (** in 50Mhz this was MODE selec
t for 16 or 32 bit FO)
457                                 ;       PC5 = STD0 = No connect         PD5 = STD1 = WRFIFO*
458                                 ; ***********************************************************************************
459    
460    
461                                 ; ****************************************************************************
462                                 ; Program the serial port ESSI1 = Port D for general purpose I/O (GPIO)
463    
464       P:000117 P:000119 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
465       P:000119 P:00011B 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  init.asm  Page 10



                            00001C
466       P:00011B P:00011D 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
467       P:00011D P:00011F 060AA0            REP     #10
468       P:00011E P:000120 000000            NOP
469       P:00011F P:000121 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
470                                                                                     ; was %011100
471    
472                                 ; Program the SCI port to benign values
473       P:000121 P:000123 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
474       P:000123 P:000125 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
475       P:000125 P:000127 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
476                                 ;       PE0 = RXD
477                                 ;       PE1 = TXD
478                                 ;       PE2 = SCLK
479    
480                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
481       P:000127 P:000129 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
482       P:000129 P:00012B 07F40B            MOVEP             #$2800,X:TCSR1
                            002800
483       P:00012B P:00012D 07F407            MOVEP             #$2800,X:TCSR2
                            002800
484    
485    
486                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
487       P:00012D P:00012F 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
488       P:00012F P:000131 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
e
                            008929
489       P:000131 P:000133 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
                            000122
490    
491    
492                                 ; Program the DRAM memory access and addressing
493       P:000133 P:000135 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
                            020022
494       P:000135 P:000137 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
495    
496    
497                                 ; Clear all PCI error conditions
498       P:000137 P:000139 084E0A            MOVEP             X:DPSR,A
499       P:000138 P:00013A 0140C2            OR      #$1FE,A
                            0001FE
500       P:00013A P:00013C 000000            NOP
501       P:00013B P:00013D 08CE0A            MOVEP             A,X:DPSR
502    
503                                 ;--------------------------------------------------------------------
504                                 ; Enable one interrupt only: software reset switch
505       P:00013C P:00013E 08F4BF            MOVEP             #$0001C0,X:IPRC         ; IRQB priority = 1 (FIFO half full HF*)
                            0001C0
506                                                                                     ; IRQC priority = 2 (reset switch)
507       P:00013E P:000140 05F439            MOVE              #$200,SR                ; Mask set up for reset switch only
                            000200
508    
509    
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  init.asm  Page 11



510                                 ;--------------------------------------------------------------------------
511                                 ; Initialize the fiber optic serial transmitter to zero
512       P:000140 P:000142 01B786            JCLR    #TDE,X:SSISR0,*
                            000140
513       P:000142 P:000144 07F43C            MOVEP             #$000000,X:TX00
                            000000
514    
515                                 ;--------------------------------------------------------------------
516    
517                                 ; clear DTXM - PCI master transmitter
518       P:000144 P:000146 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
519       P:000145 P:000147 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000145
520    
521                                 ;----------------------------------------------------------------------
522                                 ; clear DRXR - PCI receiver
523    
524       P:000147 P:000149 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014C
525       P:000149 P:00014B 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
526       P:00014A P:00014C 000000            NOP
527       P:00014B P:00014D 0C0147            JMP     <CLR0
528                                 CLR1
529    
530                                 ;-----------------------------------------------------------------------------
531                                 ; copy parameter table from P memory into X memory
532    
533                                 ; but not frame_count and num_dumped - don't want these reset by fatal error...
534                                 ; they will be reset by new packet or pci_reset ISR
535    
536       P:00014C P:00014E 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000006
537       P:00014E P:000150 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
                            000002
538    
539                                 ; Move the table of constants from P: space to X: space
540       P:000150 P:000152 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            000611
541       P:000152 P:000154 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
542       P:000153 P:000155 065680            DO      #VAR_TBL_LENGTH,X_WRITE
                            000156
543       P:000155 P:000157 07D984            MOVE              P:(R1)+,X0
544       P:000156 P:000158 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
545                                 X_WRITE
546    
547       P:000157 P:000159 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
                            000006
548       P:000159 P:00015B 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000002
549    
550                                 ;-------------------------------------------------------------------------------
551                                 ; initialise MODE; packet choke and PCI interrupts are ON.
552       P:00015B P:00015D 0A7021            BSET    #MODE_CHOKE,X:MODE
                            000001
553       P:00015D P:00015F 0A7024            BSET    #MODE_IRQ,X:MODE
                            000001
554    
555                                 ;----------------------------------------------------------------------------
556                                 ; Disable byte swapping - enabled after first command to MCE.
557                                 ; i.e after first 'CON'
558       P:00015F P:000161 013D04            BCLR    #AUX1,X:PDRC
559    
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  init.asm  Page 12



560                                 ;----------------------------------------------------------------------------
561                                 ; Initialize PCI controller again, after booting, to make sure it sticks
562       P:000160 P:000162 0A8514            BCLR    #20,X:DCTR                        ; Terminate and reset mode
563       P:000161 P:000163 000000            NOP
564       P:000162 P:000164 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            000162
565       P:000164 P:000166 000000            NOP
566       P:000165 P:000167 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
567       P:000166 P:000168 000000            NOP
568       P:000167 P:000169 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000167
569    
570       
571       P:000169 P:00016B 0BF080            JSR     CLEAR_FO_FIFO                     ; Clear the fibre fifo!
                            000496
572       P:00016B P:00016D 0BF080            JSR     TIMER_DISABLE                     ; Disable NFY timer
                            0005AA
573    
575                                           INCLUDE 'main.asm'
576                                         COMMENT *
577    
578                                 Main section of the pci card code.
579    
580                                 See info.asm for versioning and authors.
581    
582                                         *
583                                           PAGE    132                               ; Printronix page width - 132 columns
584                                           OPT     CEX                               ; print DC evaluations
585    
589    
590                                 PACKET_IN
591    
592       
593       P:00016D P:00016F 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
594    
595       
596       P:00016F P:000171 0A01A0            JSET    #MODE_APPLICATION,X:<MODE,APPLICATION
                            000800
597    
598       
599       P:000171 P:000173 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            0005B0
600    
601       
602       P:000173 P:000175 0B00B4            JSSET   #QT_FLUSH,X:STATUS,BUFFER_INFORM
                            0005E9
603    
604       
605       P:000175 P:000177 0D040A            JSR     <GET_FO_WRD
606       P:000176 P:000178 0B00A3            JSSET   #FO_WRD_RCV,X:STATUS,HANDLE_FIFO
                            00017D
607    
608       
609       P:000178 P:00017A 0B00AA            JSSET   #CON_DEMAND,X:STATUS,CON_NOW
                            000232
610    
611       
612       P:00017A P:00017C 000000            NOP
613       P:00017B P:00017D 000000            NOP
614    
615       
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 13



616       P:00017C P:00017E 0C016D            JMP     PACKET_IN
617    
621    
622    
623    
625    
626                                 HANDLE_FIFO
627       P:00017D P:00017F 0A01A1            JSET    #MODE_CHOKE,X:<MODE,RETURN_NOW    ; IF MCE Packet choke on - just keep clearin
g FIFO.
                            0001B6
628       P:00017F P:000181 441000            MOVE              X0,X:<HEAD_W1_0         ;store received word
629       P:000180 P:000182 56F000            MOVE              X:PREAMB1,A
                            000025
630       P:000182 P:000184 200045            CMP     X0,A                              ; check it is correct
631       P:000183 P:000185 0E21B3            JNE     <PRE_ERROR                        ; if not go to start
632    
633       P:000184 P:000186 0D0412            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
634       P:000185 P:000187 440F00            MOVE              X0,X:<HEAD_W1_1         ;store received word
635       P:000186 P:000188 56F000            MOVE              X:PREAMB1,A
                            000025
636       P:000188 P:00018A 200045            CMP     X0,A                              ; check it is correct
637       P:000189 P:00018B 0E21B3            JNE     <PRE_ERROR                        ; if not go to start
638    
639       P:00018A P:00018C 0D0412            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
640       P:00018B P:00018D 441200            MOVE              X0,X:<HEAD_W2_0         ;store received word
641       P:00018C P:00018E 56F000            MOVE              X:PREAMB2,A
                            000026
642       P:00018E P:000190 200045            CMP     X0,A                              ; check it is correct
643       P:00018F P:000191 0E21B3            JNE     <PRE_ERROR                        ; if not go to start
644    
645       P:000190 P:000192 0D0412            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
646       P:000191 P:000193 441100            MOVE              X0,X:<HEAD_W2_1         ;store received word
647       P:000192 P:000194 56F000            MOVE              X:PREAMB2,A
                            000026
648       P:000194 P:000196 200045            CMP     X0,A                              ; check it is correct
649       P:000195 P:000197 0E21B3            JNE     <PRE_ERROR                        ; if not go to start
650    
651                                 PACKET_INFO                                         ; packet preamble valid
652    
653       P:000196 P:000198 0D0412            JSR     <WT_FIFO
654       P:000197 P:000199 441400            MOVE              X0,X:<HEAD_W3_0         ; RP or DA
655       P:000198 P:00019A 0D0412            JSR     <WT_FIFO
656       P:000199 P:00019B 441300            MOVE              X0,X:<HEAD_W3_1         ; $2020
657    
658       P:00019A P:00019C 0D0412            JSR     <WT_FIFO
659       P:00019B P:00019D 441600            MOVE              X0,X:<HEAD_W4_0         ; packet size lo
660       P:00019C P:00019E 0D0412            JSR     <WT_FIFO
661       P:00019D P:00019F 441500            MOVE              X0,X:<HEAD_W4_1         ; packet size hi
662    
663       
664       P:00019E P:0001A0 200013            CLR     A
665       P:00019F P:0001A1 50F000            MOVE              X:HEAD_W4_0,A0
                            000016
666       P:0001A1 P:0001A3 44F000            MOVE              X:HEAD_W4_1,X0
                            000015
667       P:0001A3 P:0001A5 0C1940            INSERT  #$010010,X0,A                     ; A = size in dwords
                            010010
668    
669       
670       P:0001A5 P:0001A7 0BF080            JSR     PACKET_PARTITIONS
                            000564
671    
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 14



673       P:0001A7 P:0001A9 56F000            MOVE              X:HEAD_W3_0,A
                            000014
674    
675       P:0001A9 P:0001AB 0140C5            CMP     #>'RP',A
                            005250
676       P:0001AB P:0001AD 0AF0AA            JEQ     HANDLE_RP
                            0001B7
677    
678       P:0001AD P:0001AF 0140C5            CMP     #>'DA',A
                            004441
679       P:0001AF P:0001B1 0AF0AA            JEQ     HANDLE_DA
                            0001F3
680    
681       P:0001B1 P:0001B3 0AF080            JMP     QT_PTYPE_ERROR
                            0001B6
682    
683                                 PRE_ERROR
684       P:0001B3 P:0001B5 0A0026            BSET    #PREAMBLE_ERROR,X:<STATUS         ; indicate a preamble error
685       P:0001B4 P:0001B6 0BF080            JSR     CLEAR_FO_FIFO                     ; empty the fifo (2 ms!)
                            000496
686    
687                                 QT_PTYPE_ERROR
688                                 QT_FSIZE_ERROR
689                                 RETURN_NOW
690       P:0001B6 P:0001B8 00000C            RTS
691    
692    
693    
696    
697                                 HANDLE_RP
698       
699       P:0001B7 P:0001B9 0A0183            JCLR    #MODE_RP_BUFFER,X:MODE,MCE_PACKET
                            00024C
700    
701       
702       P:0001B9 P:0001BB 0A00B5            JSET    #RP_BUFFER_FULL,X:STATUS,HANDLE_RP_DROP
                            0001EB
703    
704       
705       P:0001BB P:0001BD 61F400            MOVE              #>REPLY_BUFFER,R1
                            100000
706       P:0001BD P:0001BF 0BF080            JSR     BUFFER_PACKET
                            000571
707    
708       
709       P:0001BF P:0001C1 60F400            MOVE              #RP_BASE_LO,R0
                            000045
710       P:0001C1 P:0001C3 0BF080            JSR     LOAD_HILO_ADDRESS
                            000600
711    
712       P:0001C3 P:0001C5 60F400            MOVE              #BURST_DEST_LO,R0
                            00002E
713       P:0001C5 P:0001C7 0BF080            JSR     SAVE_HILO_ADDRESS
                            000608
714    
715       
716       P:0001C7 P:0001C9 200013            CLR     A
717       P:0001C8 P:0001CA 20001B            CLR     B
718       P:0001C9 P:0001CB 50F000            MOVE              X:PACKET_SIZE,A0
                            00002B
719       P:0001CB P:0001CD 0C1D04            ASL     #2,A,A                            ; Size in bytes
720       P:0001CC P:0001CE 51F000            MOVE              X:RP_MAX_SIZE,B0
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 15



                            000047
721    
722       P:0001CE P:0001D0 200005            CMP     B,A                               ; A ? B
723       P:0001CF P:0001D1 0AF0AF            JLE     HANDLE_RP1
                            0001D2
724       P:0001D1 P:0001D3 21EE00            MOVE              B,A
725    
726                                 HANDLE_RP1
727       
728    
729       P:0001D2 P:0001D4 44F400            MOVE              #'NFY',X0
                            4E4659
730       P:0001D4 P:0001D6 447000            MOVE              X0,X:DTXS_WD1
                            00000B
731       P:0001D6 P:0001D8 44F400            MOVE              #'RPQ',X0
                            525051
732       P:0001D8 P:0001DA 447000            MOVE              X0,X:DTXS_WD2
                            00000C
733       P:0001DA P:0001DC 507000            MOVE              A0,X:DTXS_WD3           ; A0=block_size
                            00000D
734       P:0001DC P:0001DE 547000            MOVE              A1,X:DTXS_WD4           ; A1=0
                            00000E
735    
736       
737       P:0001DE P:0001E0 44F400            MOVE              #>REPLY_BUFFER,X0
                            100000
738       P:0001E0 P:0001E2 507000            MOVE              A0,X:BLOCK_SIZE
                            00002C
739       P:0001E2 P:0001E4 447000            MOVE              X0,X:BURST_SRC
                            000030
740       P:0001E4 P:0001E6 0BF080            JSR     BLOCK_TRANSFER
                            0004FA
741    
742       
743       P:0001E6 P:0001E8 0A7035            BSET    #RP_BUFFER_FULL,X:STATUS
                            000000
744       P:0001E8 P:0001EA 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000422
745    
746       
747       P:0001EA P:0001EC 00000C            RTS
748    
749                                 HANDLE_RP_DROP
750       P:0001EB P:0001ED 56F000            MOVE              X:RP_DROPS,A
                            000048
751       P:0001ED P:0001EF 014180            ADD     #1,A
752       P:0001EE P:0001F0 000000            NOP
753       P:0001EF P:0001F1 567000            MOVE              A,X:RP_DROPS
                            000048
754       P:0001F1 P:0001F3 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            000590
755    
757    
758    
761    
762                                 HANDLE_DA
763    
764       
765       P:0001F3 P:0001F5 56F000            MOVE              X:FRAME_COUNT,A
                            000002
766       P:0001F5 P:0001F7 0140C0            ADD     #>1,A
                            000001
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 16



767       P:0001F7 P:0001F9 000000            NOP
768       P:0001F8 P:0001FA 560200            MOVE              A,X:<FRAME_COUNT
769    
770       
771       P:0001F9 P:0001FB 0A0182            JCLR    #MODE_QT,X:MODE,MCE_PACKET
                            00024C
772    
773       
774       P:0001FB P:0001FD 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
775       P:0001FD P:0001FF 0BF080            JSR     BUFFER_PACKET
                            000571
776    
777       
778       P:0001FF P:000201 56F000            MOVE              X:QT_BUF_HEAD,A
                            00003F
779       P:000201 P:000203 014180            ADD     #1,A
780       P:000202 P:000204 57F000            MOVE              X:QT_BUF_MAX,B
                            00003C
781       P:000204 P:000206 20000D            CMP     A,B
782       P:000205 P:000207 0AF0A1            JGE     HANDLE_DA_MATH
                            000208
783       P:000207 P:000209 2E0000            MOVE              #0,A
784                                 HANDLE_DA_MATH
785       P:000208 P:00020A 57F000            MOVE              X:QT_BUF_TAIL,B
                            000040
786       P:00020A P:00020C 20000D            CMP     A,B
787       P:00020B P:00020D 0AF0AA            JEQ     HANDLE_DA_DROP
                            00022A
788    
789       
790       P:00020D P:00020F 200013            CLR     A
791       P:00020E P:000210 50F000            MOVE              X:PACKET_SIZE,A0
                            00002B
792    
793       P:000210 P:000212 014088            ADD     #0,B                              ; Clear carry
794       P:000211 P:000213 0C1D04            ASL     #2,A,A                            ; Size, in bytes
795    
796       
797       P:000212 P:000214 20001B            CLR     B
798       P:000213 P:000215 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            00003D
799       P:000215 P:000217 20000D            CMP     A,B
800       P:000216 P:000218 0E21B6            JNE     QT_FSIZE_ERROR
801    
802       
803       P:000217 P:000219 517000            MOVE              B0,X:BLOCK_SIZE
                            00002C
804       P:000219 P:00021B 557000            MOVE              B1,X:BURST_SRC          ; Y:0
                            000030
805    
806       P:00021B P:00021D 60F400            MOVE              #QT_DEST_LO,R0
                            000041
807       P:00021D P:00021F 0BF080            JSR     LOAD_HILO_ADDRESS
                            000600
808       P:00021F P:000221 60F400            MOVE              #BURST_DEST_LO,R0
                            00002E
809       P:000221 P:000223 0BF080            JSR     SAVE_HILO_ADDRESS
                            000608
810    
811       
812       P:000223 P:000225 0BF080            JSR     BLOCK_TRANSFER
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 17



                            0004FA
813    
814       
815       P:000225 P:000227 0BF080            JSR     BUFFER_INCR
                            0005BE
816    
817       
818       P:000227 P:000229 0BF080            JSR     BUFFER_INFORM_CHECK
                            0005DC
819    
820       P:000229 P:00022B 00000C            RTS
821    
822                                 HANDLE_DA_DROP
823       
824       P:00022A P:00022C 56F000            MOVE              X:QT_DROPS,A
                            000044
825       P:00022C P:00022E 014180            ADD     #1,A
826       P:00022D P:00022F 000000            NOP
827       P:00022E P:000230 567000            MOVE              A,X:QT_DROPS
                            000044
828       P:000230 P:000232 0AF080            JMP     DROP_PACKET                       ; Will RTS to main loop
                            000590
829    
831    
832    
833    
834                                 CON_NOW
835                                 ;       This routine runs after the PC sends a 'CON' command, and will
836                                 ;       copy the command to the MCE and then reply to the PC.
837    
838       P:000232 P:000234 60F400            MOVE              #>CON_SOURCE_LO,R0
                            000049
839       P:000234 P:000236 0BF080            JSR     LOAD_HILO_ADDRESS                 ; PCI address in A
                            000600
840       P:000236 P:000238 0C1D01            ASL     #0,A,B                            ; MOVE A,B
841    
842       
843       P:000237 P:000239 66F400            MOVE              #>COMMAND_BUFFER,R6
                            200000
844       P:000239 P:00023B 200013            CLR     A
845    
846       
847       P:00023A P:00023C 064080            DO      #64,CON_NOW1                      ; block size = 32bit x 64 (256 bytes)
                            000240
848       P:00023C P:00023E 0D043D            JSR     <READ_FROM_PCI                    ; get next 32 bit word from HOST
849    
850       P:00023D P:00023F 5C5E00            MOVE                          A1,Y:(R6)+  ; b4, b3 (msb)
851       P:00023E P:000240 585E00            MOVE                          A0,Y:(R6)+  ; b2, b1  (lsb)
852    
853       P:00023F P:000241 0D0477            JSR     <XMT_WD_FIBRE                     ; off it goes
854       P:000240 P:000242 000000            NOP
855                                 CON_NOW1
856    
857       P:000241 P:000243 0A0101            BCLR    #MODE_CHOKE,X:<MODE               ; disable packet choke...
858                                                                                     ; comms now open with MCE and packets will b
e processed.
859       P:000242 P:000244 013D24            BSET    #AUX1,X:PDRC                      ; enable hardware
860    
861       
862       P:000243 P:000245 0A700A            BCLR    #CON_DEMAND,X:STATUS
                            000000
863    
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 18



864       
865       P:000245 P:000247 44F400            MOVE              #'CON',X0
                            434F4E
866       P:000247 P:000249 0BF080            JSR     VCOM_PREPARE_REPLY
                            000289
867       P:000249 P:00024B 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000422
868       P:00024B P:00024D 00000C            RTS
869    
870    
871    
872    
874    
875                                 ; --------------------------------------------------------------------------
876                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
877                                 ; --------------------------------------------------------------------------
878    
879                                 ; prepare notify to inform host that a packet has arrived.
880    
881                                 MCE_PACKET
882       P:00024C P:00024E 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
883    
884       P:00024D P:00024F 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
885       P:00024F P:000251 440B00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
886    
887       P:000250 P:000252 449400            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
888       P:000251 P:000253 440C00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
889    
890       P:000252 P:000254 449600            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
891       P:000253 P:000255 440D00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
892    
893       P:000254 P:000256 449500            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
894       P:000255 P:000257 440E00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sage
895    
896       
897       P:000256 P:000258 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
898       P:000257 P:000259 0D0422            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
899       P:000258 P:00025A 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
900    
901    
902       P:000259 P:00025B 61F400            MOVE              #>IMAGE_BUFFER,R1
                            000000
903       P:00025B P:00025D 0BF080            JSR     BUFFER_PACKET
                            000571
904    
905       
906    
907       P:00025D P:00025F 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; on fatal error, re-init.
                            000100
908       P:00025F P:000261 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; Set in 'send_packet_to_host' ISR
                            00025D
909    
910       
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 19



911       P:000261 P:000263 44F400            MOVE              #>IMAGE_BUFFER,X0
                            000000
912       P:000263 P:000265 56F000            MOVE              X:PACKET_SIZE,A
                            00002B
913       P:000265 P:000267 0C1D04            ASL     #2,A,A
914       P:000266 P:000268 447000            MOVE              X0,X:BURST_SRC
                            000030
915       P:000268 P:00026A 547000            MOVE              A1,X:BLOCK_SIZE
                            00002C
916       P:00026A P:00026C 0BF080            JSR     BLOCK_TRANSFER
                            0004FA
917    
918       P:00026C P:00026E 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
919    
920       
921       P:00026E P:000270 44F400            MOVE              #'HST',X0
                            485354
922       P:000270 P:000272 0BF080            JSR     VCOM_PREPARE_REPLY
                            000289
923       P:000272 P:000274 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000422
924       P:000274 P:000276 00000C            RTS
925    
926                                 ;----------------------------------------------------------
927                                 ; clear out the fifo after an HST timeout...
928                                 ;----------------------------------------------------------
929    
930                                 DUMP_FIFO
931       P:000275 P:000277 61F400            MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
932       P:000277 P:000279 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
933       P:000279 P:00027B 200013            CLR     A
934       P:00027A P:00027C 320000            MOVE              #0,R2                   ; use R2 as a dump count
935                                 NEXT_DUMP
936       P:00027B P:00027D 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000286
937       P:00027D P:00027F 000000            NOP
938       P:00027E P:000280 000000            NOP
939       P:00027F P:000281 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000286
940    
941       P:000281 P:000283 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
942       P:000282 P:000284 205A00            MOVE              (R2)+                   ; inc dump count
943       P:000283 P:000285 224E00            MOVE              R2,A                    ;
944       P:000284 P:000286 200045            CMP     X0,A                              ; check we've not hit dump limit
945       P:000285 P:000287 0E227B            JNE     NEXT_DUMP                         ; not hit limit?
946                                 FIFO_EMPTY
947       P:000286 P:000288 627000            MOVE              R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000006
948       P:000288 P:00028A 0C0100            JMP     <START                            ; re-initialise
949    
950    
951                                 ; -------------------------------------------------------------------------------------
952                                 ;                              END OF MAIN PACKET HANDLING CODE
953                                 ; -------------------------------------------------------------------------------------
954    
955    
956    
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 20



957                                 ; -------------------------------------------------------------------------------------
958                                 ;
959                                 ;                              INTERRUPT SERVICE ROUTINES
960                                 ;
961                                 ; -------------------------------------------------------------------------------------
962    
963                                 ; ---------------
964                                 ; Rules:  Don't use N#, or any R# except R0 unless you add them to the saved register set.
965    
966    
967                                 ; ----------------------------------------------------------------------------
968                                 ; VCOM_* - routines: utility functions for hosty command vector communication.
969                                 ;-----------------------------------------------------------------------------
970    
971    
972                                 ; VCOM_PREPARE_REPLY
973                                 ;
974                                 ; Prepare the reply packet, using X0 as the command name (second word).  The
975                                 ; message defaults to 'ACK' with NULL data.  The user may subsequenty fill in
976                                 ; the data field (word 4) and mark the packet as error if necessary.
977    
978                                 VCOM_PREPARE_REPLY
979       
980       
981       P:000289 P:00028B 50F400            MOVE              #'REP',A0
                            524550
982       P:00028B P:00028D 447000            MOVE              X0,X:DTXS_WD2           ; Command
                            00000C
983       P:00028D P:00028F 507000            MOVE              A0,X:DTXS_WD1
                            00000B
984    
985       P:00028F P:000291 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
                            41434B
986       P:000291 P:000293 000000            NOP
987       P:000292 P:000294 547000            MOVE              A1,X:DTXS_WD3           ; ACK
                            00000D
988       P:000294 P:000296 507000            MOVE              A0,X:DTXS_WD4           ; no comment
                            00000E
989       P:000296 P:000298 00000C            RTS
990    
991    
992                                 ; VCOM_CHECK
993                                 ;
994                                 ; Compares DRXR_WD1 to X0.  If they are equal, Z is set on return.  If they
995                                 ; are not equal then Z is cleared and the reply will be marked as ERR with
996                                 ; 'CNE' in the last word.
997                                 ; Trashes A and B always and X0 on error.
998    
999                                 VCOM_CHECK
1000      P:000297 P:000299 208E00            MOVE              X0,A
1001      P:000298 P:00029A 57F000            MOVE              X:DRXR_WD1,B
                            000007
1002      P:00029A P:00029C 20000D            CMP     A,B
1003      P:00029B P:00029D 0AF0AA            JEQ     VCOM_RTS
                            0002A5
1004   
1005      P:00029D P:00029F 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
1006      P:00029F P:0002A1 50F400            MOVE              #'ERR',A0
                            455252
1007      P:0002A1 P:0002A3 447000            MOVE              X0,X:DTXS_WD4
                            00000E
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 21



1008      P:0002A3 P:0002A5 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1009                                VCOM_RTS
1010      P:0002A5 P:0002A7 00000C            RTS
1011   
1012   
1013                                ; VCOM_INTRO
1014                                ;
1015                                ; Read DSP command from DRXR.  Prepare the reply packet and verify that it
1016                                ; matches the key in X1.  If it does not, mark the reply as error and set
1017                                ; the Z flag.
1018   
1019                                VCOM_INTRO
1020      P:0002A6 P:0002A8 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            000434
1021      P:0002A8 P:0002AA 20A400            MOVE              X1,X0
1022      P:0002A9 P:0002AB 0D0289            JSR     VCOM_PREPARE_REPLY
1023      P:0002AA P:0002AC 0D0297            JSR     VCOM_CHECK
1024      P:0002AB P:0002AD 00000C            RTS
1025   
1026   
1027                                ; VCOM_EXIT_ERROR_X0
1028                                ; VCOM_EXIT_X0
1029                                ; VCOM_EXIT
1030                                ;
1031                                ; For returning from host command vector interrupts only.  These three
1032                                ; routines do the following (respectively):
1033                                ; a) Mark reply as error, then (b)
1034                                ; b) Put X0 into last word of reply, then (c)
1035                                ; c) Restore registers and RTI.
1036   
1037                                VCOM_EXIT_ERROR_X0
1038      P:0002AC P:0002AE 50F400            MOVE              #'ERR',A0
                            455252
1039      P:0002AE P:0002B0 000000            NOP
1040      P:0002AF P:0002B1 507000            MOVE              A0,X:DTXS_WD3
                            00000D
1041                                VCOM_EXIT_X0
1042      P:0002B1 P:0002B3 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1043                                VCOM_EXIT
1044      P:0002B3 P:0002B5 0BF080            JSR     RESTORE_REGISTERS
                            00045D
1045      P:0002B5 P:0002B7 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000422
1046      P:0002B7 P:0002B9 000004            RTI
1047   
1048   
1049   
1050   
1051                                ; ----------------------------------------------------------------------------
1052                                READ_MEMORY
1053                                ;-----------------------------------------------------------------------------
1054                                ;Read command:
1055                                ; word 1 = command = 'RDM'
1056                                ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
1057                                ; word 3 = address in memory
1058                                ; word 4 = not used
1059                                ;Version query:
1060                                ; word 1 = 'VER'
1061                                ; word 2-4 unused
1062   
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 22



1063      P:0002B8 P:0002BA 0BF080            JSR     SAVE_REGISTERS
                            00046A
1064      P:0002BA P:0002BC 0BF080            JSR     RD_DRXR                           ; Loads DRXR_WD*
                            000434
1065   
1066      P:0002BC P:0002BE 44F400            MOVE              #'RDM',X0
                            52444D
1067      P:0002BE P:0002C0 0D0289            JSR     VCOM_PREPARE_REPLY
1068      P:0002BF P:0002C1 0D0297            JSR     VCOM_CHECK
1069      P:0002C0 P:0002C2 0AF0AA            JEQ     READ_MEMORY_XYP
                            0002CA
1070   
1071      
1072      P:0002C2 P:0002C4 44F400            MOVE              #'VER',X0
                            564552
1073      P:0002C4 P:0002C6 0D0289            JSR     VCOM_PREPARE_REPLY
1074      P:0002C5 P:0002C7 0D0297            JSR     VCOM_CHECK
1075      P:0002C6 P:0002C8 0E22B3            JNE     VCOM_EXIT
1076   
1077      P:0002C7 P:0002C9 44F000            MOVE              X:REV_NUMBER,X0
                            000003
1078      P:0002C9 P:0002CB 0C02B1            JMP     VCOM_EXIT_X0
1079   
1080                                READ_MEMORY_XYP
1081   
1082      
1083      P:0002CA P:0002CC 56F000            MOVE              X:DRXR_WD2,A
                            000008
1084      P:0002CC P:0002CE 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1085   
1086      P:0002CE P:0002D0 0140C5            CMP     #'_X',A
                            005F58
1087      P:0002D0 P:0002D2 0AF0AA            JEQ     READ_MEMORY_X
                            0002DD
1088   
1089      P:0002D2 P:0002D4 0140C5            CMP     #'_Y',A
                            005F59
1090      P:0002D4 P:0002D6 0AF0AA            JEQ     READ_MEMORY_Y
                            0002DF
1091   
1092      P:0002D6 P:0002D8 0140C5            CMP     #'_P',A
                            005F50
1093      P:0002D8 P:0002DA 0AF0AA            JEQ     READ_MEMORY_P
                            0002E1
1094   
1095      P:0002DA P:0002DC 44F400            MOVE              #'MTE',X0
                            4D5445
1096      P:0002DC P:0002DE 0C02AC            JMP     VCOM_EXIT_ERROR_X0
1097   
1098                                READ_MEMORY_X
1099      P:0002DD P:0002DF 44E000            MOVE              X:(R0),X0
1100      P:0002DE P:0002E0 0C02B1            JMP     VCOM_EXIT_X0
1101                                READ_MEMORY_Y
1102      P:0002DF P:0002E1 4CE000            MOVE                          Y:(R0),X0
1103      P:0002E0 P:0002E2 0C02B1            JMP     VCOM_EXIT_X0
1104                                READ_MEMORY_P
1105      P:0002E1 P:0002E3 07E084            MOVE              P:(R0),X0
1106      P:0002E2 P:0002E4 0C02B1            JMP     VCOM_EXIT_X0
1107   
1108   
1109                                ;--------------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 23



1110                                WRITE_MEMORY
1111                                ;---------------------------------------------------------------
1112                                ; word 1 = command = 'WRM'
1113                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1114                                ; word 3 = address in memory
1115                                ; word 4 = value
1116   
1117      P:0002E3 P:0002E5 0BF080            JSR     SAVE_REGISTERS
                            00046A
1118      P:0002E5 P:0002E7 45F400            MOVE              #'WRM',X1
                            57524D
1119      P:0002E7 P:0002E9 0D02A6            JSR     VCOM_INTRO
1120      P:0002E8 P:0002EA 0E22B3            JNE     VCOM_EXIT
1121   
1122      
1123      P:0002E9 P:0002EB 56F000            MOVE              X:DRXR_WD2,A
                            000008
1124      P:0002EB P:0002ED 60F000            MOVE              X:DRXR_WD3,R0
                            000009
1125      P:0002ED P:0002EF 44F000            MOVE              X:DRXR_WD4,X0
                            00000A
1126   
1127      P:0002EF P:0002F1 0140C5            CMP     #'_X',A
                            005F58
1128      P:0002F1 P:0002F3 0AF0AA            JEQ     WRITE_MEMORY_X
                            0002FE
1129   
1130      P:0002F3 P:0002F5 0140C5            CMP     #'_Y',A
                            005F59
1131      P:0002F5 P:0002F7 0AF0AA            JEQ     WRITE_MEMORY_Y
                            000300
1132   
1133      P:0002F7 P:0002F9 0140C5            CMP     #'_P',A
                            005F50
1134      P:0002F9 P:0002FB 0AF0AA            JEQ     WRITE_MEMORY_P
                            000302
1135   
1136      P:0002FB P:0002FD 44F400            MOVE              #'MTE',X0
                            4D5445
1137      P:0002FD P:0002FF 0C02AC            JMP     VCOM_EXIT_ERROR_X0
1138   
1139                                WRITE_MEMORY_X
1140      P:0002FE P:000300 446000            MOVE              X0,X:(R0)
1141      P:0002FF P:000301 0C02B1            JMP     VCOM_EXIT_X0
1142                                WRITE_MEMORY_Y
1143      P:000300 P:000302 4C6000            MOVE                          X0,Y:(R0)
1144      P:000301 P:000303 0C02B1            JMP     VCOM_EXIT_X0
1145                                WRITE_MEMORY_P
1146      P:000302 P:000304 076084            MOVE              X0,P:(R0)
1147      P:000303 P:000305 0C02B1            JMP     VCOM_EXIT_X0
1148   
1149   
1150                                ;-----------------------------------------------------------------------------
1151                                START_APPLICATION
1152                                ; an application should already have been downloaded to the PCI memory.
1153                                ; this command will execute it.
1154                                ; ----------------------------------------------------------------------
1155                                ; word 1 = command = 'GOA'
1156                                ; word 2-4 unused
1157   
1158      P:000304 P:000306 0BF080            JSR     SAVE_REGISTERS
                            00046A
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 24



1159      P:000306 P:000308 45F400            MOVE              #'GOA',X1
                            474F41
1160   
1161      P:000308 P:00030A 0D02A6            JSR     VCOM_INTRO
1162      P:000309 P:00030B 0E22B3            JNE     VCOM_EXIT
1163   
1164      P:00030A P:00030C 0A7020            BSET    #MODE_APPLICATION,X:MODE
                            000001
1165      P:00030C P:00030E 000004            RTI                                       ; Application will reply.
1166   
1167   
1168                                ; ---------------------------------------------------------
1169                                STOP_APPLICATION
1170                                ; this command stops an application that is currently running
1171                                ; used for applications that once started run contiunually
1172                                ;-----------------------------------------------------------
1173                                ; word 1 = command = ' STP'
1174                                ; word 2-4 unused
1175   
1176      P:00030D P:00030F 0BF080            JSR     SAVE_REGISTERS
                            00046A
1177      P:00030F P:000311 45F400            MOVE              #'STP',X1
                            535450
1178   
1179      P:000311 P:000313 0D02A6            JSR     VCOM_INTRO
1180      P:000312 P:000314 0E22B3            JNE     VCOM_EXIT
1181   
1182      P:000313 P:000315 0A7000            BCLR    #MODE_APPLICATION,X:MODE
                            000001
1183      P:000315 P:000317 0A7000            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1184      P:000317 P:000319 0C02B3            JMP     VCOM_EXIT
1185   
1186   
1187                                ;-----------------------------------------------------------------------------
1188                                RESET_CONTROLLER
1189                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1190                                ;-----------------------------------------------------------------------------
1191                                ; word 1 = command = 'RCO'
1192                                ; word 2-4 unused
1193   
1194      P:000318 P:00031A 0BF080            JSR     SAVE_REGISTERS
                            00046A
1195      P:00031A P:00031C 45F400            MOVE              #'RCO',X1
                            52434F
1196      P:00031C P:00031E 0D02A6            JSR     VCOM_INTRO
1197      P:00031D P:00031F 0E22B3            JNE     VCOM_EXIT
1198   
1199      P:00031E P:000320 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1200      P:00031F P:000321 000000            NOP
1201      P:000320 P:000322 000000            NOP
1202      P:000321 P:000323 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1203      P:000323 P:000325 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1204      P:000325 P:000327 446000            MOVE              X0,X:(R0)
1205      P:000326 P:000328 0606A0            REP     #6                                ; Wait for transmission to complete
1206      P:000327 P:000329 000000            NOP
1207      P:000328 P:00032A 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1208   
1209                                ; Wait for a bit for MCE to be reset.......
1210      P:000329 P:00032B 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 25



                            002710
1211      P:00032B P:00032D 06C400            DO      X0,L_DELAY
                            000331
1212      P:00032D P:00032F 06E883            DO      #1000,L_RDFIFO
                            000330
1213      P:00032F P:000331 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1214      P:000330 P:000332 000000            NOP                                       ;   receiver empty
1215                                L_RDFIFO
1216      P:000331 P:000333 000000            NOP
1217                                L_DELAY
1218      P:000332 P:000334 000000            NOP
1219   
1220      P:000333 P:000335 44F400            MOVE              #'000',X0
                            303030
1221      P:000335 P:000337 0C02B1            JMP     VCOM_EXIT_X0
1222   
1223                                ;-----------------------------------------------------------------------------
1224                                QUIET_TRANSFER_SET
1225                                ;-----------------------------------------------------------------------------
1226                                ;Quiet transfer mode configuration
1227                                ; word 1 = command = 'QTS'
1228                                ; word 2 = parameter to set
1229                                ; word 3-4 = arguments
1230   
1231      P:000336 P:000338 0BF080            JSR     SAVE_REGISTERS                    ; standard opening
                            00046A
1232      P:000338 P:00033A 45F400            MOVE              #'QTS',X1
                            515453
1233      P:00033A P:00033C 0D02A6            JSR     VCOM_INTRO
1234      P:00033B P:00033D 0E22B3            JNE     VCOM_EXIT
1235   
1236      P:00033C P:00033E 56F000            MOVE              X:DRXR_WD2,A            ; Parameter id
                            000008
1237      P:00033E P:000340 44F000            MOVE              X:DRXR_WD3,X0           ; First arg
                            000009
1238      P:000340 P:000342 45F000            MOVE              X:DRXR_WD4,X1           ; Second arg
                            00000A
1239   
1240      P:000342 P:000344 0140C5            CMP     #'BAS',A
                            424153
1241      P:000344 P:000346 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            0003B7
1242   
1243      P:000346 P:000348 0140C5            CMP     #'DEL',A
                            44454C
1244      P:000348 P:00034A 60F400            MOVE              #QT_BUF_SIZE,R0
                            00003B
1245      P:00034A P:00034C 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1246   
1247      P:00034C P:00034E 0140C5            CMP     #'NUM',A
                            4E554D
1248      P:00034E P:000350 60F400            MOVE              #QT_BUF_MAX,R0
                            00003C
1249      P:000350 P:000352 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1250   
1251      P:000352 P:000354 0140C5            CMP     #'INF',A
                            494E46
1252      P:000354 P:000356 60F400            MOVE              #QT_INFORM,R0
                            00003E
1253      P:000356 P:000358 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 26



                            0003B5
1254   
1255      P:000358 P:00035A 0140C5            CMP     #'SIZ',A
                            53495A
1256      P:00035A P:00035C 60F400            MOVE              #QT_FRAME_SIZE,R0
                            00003D
1257      P:00035C P:00035E 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1258   
1259      P:00035E P:000360 0140C5            CMP     #'TAI',A
                            544149
1260      P:000360 P:000362 60F400            MOVE              #QT_BUF_TAIL,R0
                            000040
1261      P:000362 P:000364 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1262   
1263      P:000364 P:000366 0140C5            CMP     #'HEA',A
                            484541
1264      P:000366 P:000368 60F400            MOVE              #QT_BUF_HEAD,R0
                            00003F
1265      P:000368 P:00036A 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1266   
1267      P:00036A P:00036C 0140C5            CMP     #'DRO',A
                            44524F
1268      P:00036C P:00036E 60F400            MOVE              #QT_DROPS,R0
                            000044
1269      P:00036E P:000370 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1270   
1271      P:000370 P:000372 0140C5            CMP     #'PER',A
                            504552
1272      P:000372 P:000374 60F400            MOVE              #TCPR0,R0
                            FFFF8D
1273      P:000374 P:000376 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1274   
1275      P:000376 P:000378 0140C5            CMP     #'FLU',A
                            464C55
1276      P:000378 P:00037A 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            00039E
1277   
1278      P:00037A P:00037C 0140C5            CMP     #'SET',A
                            534554
1279      P:00037C P:00037E 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            0003A6
1280   
1281      P:00037E P:000380 0140C5            CMP     #'RPS',A
                            525053
1282      P:000380 P:000382 60F400            MOVE              #RP_MAX_SIZE,R0
                            000047
1283      P:000382 P:000384 0AF0AA            JEQ     QUIET_TRANSFER_SET_R0
                            0003B5
1284   
1285      P:000384 P:000386 0140C5            CMP     #'RPB',A
                            525042
1286      P:000386 P:000388 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_BASE
                            00038F
1287   
1288      P:000388 P:00038A 0140C5            CMP     #'RPE',A
                            525045
1289      P:00038A P:00038C 0AF0AA            JEQ     QUIET_TRANSFER_SET_RP_ENABLED
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 27



                            000394
1290   
1291      P:00038C P:00038E 44F400            MOVE              #'MTE',X0
                            4D5445
1292      P:00038E P:000390 0C02AC            JMP     VCOM_EXIT_ERROR_X0
1293   
1294                                QUIET_TRANSFER_SET_RP_BASE
1295      P:00038F P:000391 447000            MOVE              X0,X:RP_BASE_LO
                            000045
1296      P:000391 P:000393 457000            MOVE              X1,X:RP_BASE_HI
                            000046
1297      P:000393 P:000395 0C02B3            JMP     VCOM_EXIT
1298   
1299                                QUIET_TRANSFER_SET_RP_ENABLED
1300      P:000394 P:000396 0A7003            BCLR    #MODE_RP_BUFFER,X:MODE
                            000001
1301      P:000396 P:000398 208E00            MOVE              X0,A
1302      P:000397 P:000399 200003            TST     A
1303      P:000398 P:00039A 0EA2B3            JEQ     VCOM_EXIT
1304      P:000399 P:00039B 0A7023            BSET    #MODE_RP_BUFFER,X:MODE
                            000001
1305      P:00039B P:00039D 0A7015            BCLR    #RP_BUFFER_FULL,X:STATUS
                            000000
1306      P:00039D P:00039F 0C02B3            JMP     VCOM_EXIT
1307   
1308                                QUIET_TRANSFER_SET_FLUSH
1309      P:00039E P:0003A0 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
1310      P:0003A0 P:0003A2 208E00            MOVE              X0,A
1311      P:0003A1 P:0003A3 200003            TST     A
1312      P:0003A2 P:0003A4 0EA2B3            JEQ     VCOM_EXIT
1313      P:0003A3 P:0003A5 0A7034            BSET    #QT_FLUSH,X:STATUS
                            000000
1314      P:0003A5 P:0003A7 0C02B3            JMP     VCOM_EXIT
1315   
1316                                QUIET_TRANSFER_SET_ENABLED
1317      P:0003A6 P:0003A8 0A7002            BCLR    #MODE_QT,X:MODE
                            000001
1318      P:0003A8 P:0003AA 0BF080            JSR     TIMER_DISABLE
                            0005AA
1319      P:0003AA P:0003AC 208E00            MOVE              X0,A
1320      P:0003AB P:0003AD 200003            TST     A
1321      P:0003AC P:0003AE 0EA2B3            JEQ     VCOM_EXIT
1322      P:0003AD P:0003AF 280000            MOVE              #0,A0
1323      P:0003AE P:0003B0 0A7022            BSET    #MODE_QT,X:MODE
                            000001
1324      P:0003B0 P:0003B2 507000            MOVE              A0,X:TLR0
                            FFFF8E
1325      P:0003B2 P:0003B4 0BF080            JSR     TIMER_ENABLE
                            0005A4
1326      P:0003B4 P:0003B6 0C02B3            JMP     VCOM_EXIT
1327   
1328                                QUIET_TRANSFER_SET_R0
1329      P:0003B5 P:0003B7 446000            MOVE              X0,X:(R0)
1330      P:0003B6 P:0003B8 0C02B3            JMP     VCOM_EXIT
1331   
1332                                QUIET_TRANSFER_SET_BASE
1333      P:0003B7 P:0003B9 447000            MOVE              X0,X:QT_BASE_LO
                            000039
1334      P:0003B9 P:0003BB 457000            MOVE              X1,X:QT_BASE_HI
                            00003A
1335   
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 28



1336      P:0003BB P:0003BD 0BF080            JSR     BUFFER_RESET
                            0005D0
1337   
1338      P:0003BD P:0003BF 0C02B3            JMP     VCOM_EXIT
1339   
1340   
1341                                ;-----------------------------------------------------------------------------
1342                                SYSTEM_RESET
1343                                ;-----------------------------------------------------------------------------
1344   
1345      P:0003BE P:0003C0 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1346      P:0003BF P:0003C1 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1347                                                                                    ; set to zero except for interrupts
1348      P:0003C1 P:0003C3 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1349                                                                                    ; so first set to 0
1350      P:0003C2 P:0003C4 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1351                                                                                    ; therefore,return to initialization
1352      P:0003C4 P:0003C6 000000            NOP
1353      P:0003C5 P:0003C7 000004            RTI                                       ; return from ISR - to START
1354   
1355   
1356                                ;--------------------------------------------------------------------
1357                                CLEAN_UP_PCI
1358                                ;--------------------------------------------------------------------
1359                                ; Clean up the PCI board from wherever it was executing
1360   
1361      P:0003C6 P:0003C8 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1362      P:0003C7 P:0003C9 05F43D            MOVEC             #$000200,SSL            ; SR = zero except for interrupts
                            000200
1363      P:0003C9 P:0003CB 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1364      P:0003CA P:0003CC 05F43C            MOVEC             #START,SSH              ; Set PC to for full initialization
                            000100
1365      P:0003CC P:0003CE 000000            NOP
1366      P:0003CD P:0003CF 000004            RTI
1367   
1368   
1369                                ; ------------------------------------------------------------------------------------
1370                                SEND_PACKET_TO_HOST
1371                                ; this command is received from the Host and actions the PCI board to pick up an address
1372                                ; pointer from DRXR which the PCI board then uses to write packets from the
1373                                ; MCE to the host memory starting at the address given.
1374                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1375                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1376                                ; HST after packet sent (unless error).
1377                                ; --------------------------------------------------------------------------------------
1378                                ; word 1 = command = 'HST'
1379                                ; word 2 = host high address
1380                                ; word 3 = host low address
1381                                ; word 4 = not used but read
1382   
1383                                ; save some registers but not B
1384   
1385      P:0003CE P:0003D0 0D046A            JSR     <SAVE_REGISTERS                   ; save working registers
1386      P:0003CF P:0003D1 45F400            MOVE              #'HST',X1
                            485354
1387      P:0003D1 P:0003D3 0D02A6            JSR     VCOM_INTRO
1388      P:0003D2 P:0003D4 0E22B3            JNE     VCOM_EXIT
1389   
1390      
1391      P:0003D3 P:0003D5 448800            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 29



1392      P:0003D4 P:0003D6 518900            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1393   
1394      P:0003D5 P:0003D7 447000            MOVE              X0,X:BURST_DEST_HI
                            00002F
1395      P:0003D7 P:0003D9 517000            MOVE              B0,X:BURST_DEST_LO
                            00002E
1396   
1397      P:0003D9 P:0003DB 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1398   
1399      P:0003DA P:0003DC 0BF080            JSR     RESTORE_REGISTERS
                            00045D
1400      P:0003DC P:0003DE 000004            RTI                                       ; Main loop will reply after packet transfer
!
1401   
1402   
1403                                ; --------------------------------------------------------------------
1404                                SOFTWARE_RESET
1405                                ;----------------------------------------------------------------------
1406                                ; word 1 = command = 'RST'
1407                                ; word 2-4 unused
1408   
1409      P:0003DD P:0003DF 0BF080            JSR     SAVE_REGISTERS
                            00046A
1410      P:0003DF P:0003E1 45F400            MOVE              #'RST',X1
                            525354
1411      P:0003E1 P:0003E3 0D02A6            JSR     VCOM_INTRO
1412      P:0003E2 P:0003E4 0E22B3            JNE     VCOM_EXIT
1413   
1414                                ; RST command OK so reply to host
1415                                FINISH_RST
1416      P:0003E3 P:0003E5 44F400            MOVE              #'000',X0
                            303030
1417      P:0003E5 P:0003E7 447000            MOVE              X0,X:DTXS_WD4
                            00000E
1418      P:0003E7 P:0003E9 0BF080            JSR     PCI_MESSAGE_TO_HOST
                            000422
1419   
1420      P:0003E9 P:0003EB 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            0003E9
1421   
1422      P:0003EB P:0003ED 0A0100            BCLR    #MODE_APPLICATION,X:<MODE         ; clear app flag
1423      P:0003EC P:0003EE 0A0006            BCLR    #PREAMBLE_ERROR,X:<STATUS         ; clear preamble error
1424      P:0003ED P:0003EF 0A0000            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1425   
1426                                ; remember we are in a ISR so can't just jump to start.
1427   
1428      P:0003EE P:0003F0 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1429      P:0003EF P:0003F1 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1430                                                                                    ; set to zero except for interrupts
1431      P:0003F1 P:0003F3 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1432                                                                                    ; so first set to 0
1433      P:0003F2 P:0003F4 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1434                                                                                    ; therefore,return to initialization
1435      P:0003F4 P:0003F6 000000            NOP
1436      P:0003F5 P:0003F7 000004            RTI                                       ; return from ISR - to START
1437   
1438   
1439                                SEND_PACKET_TO_CONTROLLER
1440   
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 30



1441                                ;       Host command identifying location of an MCE command to send to
1442                                ;       the MCE.  Since this can come at any time, just record the
1443                                ;       request and then do the CONning from the main loop.
1444   
1445                                ; word 1 = command = 'CON'
1446                                ; word 2 = source host bus address, bits 31:16
1447                                ; word 3 = source host bus address, bits 15:0
1448                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1449                                ;        = '1' --> when MCE command is GO
1450   
1451      P:0003F6 P:0003F8 0D046A            JSR     <SAVE_REGISTERS                   ; save working registers
1452   
1453      
1454      P:0003F7 P:0003F9 45F400            MOVE              #'CON',X1
                            434F4E
1455      P:0003F9 P:0003FB 0D02A6            JSR     VCOM_INTRO
1456      P:0003FA P:0003FC 0E22B3            JNE     VCOM_EXIT
1457   
1458      
1459      P:0003FB P:0003FD 44F400            MOVE              #'BUS',X0
                            425553
1460      P:0003FD P:0003FF 0A00AA            JSET    #CON_DEMAND,X:STATUS,VCOM_EXIT_ERROR_X0
                            0002AC
1461   
1462      
1463      P:0003FF P:000401 0A702A            BSET    #CON_DEMAND,X:STATUS
                            000000
1464      P:000401 P:000403 448800            MOVE              X:<DRXR_WD2,X0
1465      P:000402 P:000404 458900            MOVE              X:<DRXR_WD3,X1
1466      P:000403 P:000405 447000            MOVE              X0,X:CON_SOURCE_HI
                            00004A
1467      P:000405 P:000407 457000            MOVE              X1,X:CON_SOURCE_LO
                            000049
1468   
1469                                ;       ;; Fourth word indicates if this is a go.  Who cares?
1470                                ;       MOVE    X:<DRXR_WD4,A           ; read word 4 - GO command?
1471                                ;       MOVE    #0,X0
1472                                ;       CMP     X0,A
1473                                ;       JEQ     BLOCK_CON
1474   
1475      
1476      P:000407 P:000409 0BF080            JSR     RESTORE_REGISTERS
                            00045D
1477      P:000409 P:00040B 000004            RTI
1478   
1480   
1481   
1482                                ;---------------------------------------------------------------
1483                                ;
1484                                ;                          * END OF ISRs *
1485                                ;
1486                                ;--------------------------------------------------------------
1487   
1488   
1489   
1490                                ;----------------------------------------------------------------
1491                                ;
1492                                ;                     * Beginning of SUBROUTINES *
1493                                ;
1494                                ;-----------------------------------------------------------------
1495   
1496   
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 31



1497                                ;---------------------------------------------------------------
1498                                GET_FO_WRD
1499                                ;--------------------------------------------------------------
1500                                ; Anything in fibre receive FIFO?   If so store in X0
1501   
1502      P:00040A P:00040C 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            000420
1503      P:00040C P:00040E 000000            NOP
1504      P:00040D P:00040F 000000            NOP
1505      P:00040E P:000410 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS             ; check twice for FO metastability.
                            000420
1506      P:000410 P:000412 0AF080            JMP     RD_FO_WD
                            000418
1507   
1508      P:000412 P:000414 01AD80  WT_FIFO   JCLR    #EF,X:PDRD,*                      ; Wait till something in FIFO flagged
                            000412
1509      P:000414 P:000416 000000            NOP
1510      P:000415 P:000417 000000            NOP
1511      P:000416 P:000418 01AD80            JCLR    #EF,X:PDRD,WT_FIFO                ; check twice.....
                            000412
1512   
1513                                ; Read one word from the fiber optics FIFO, check it and put it in A1
1514                                RD_FO_WD
1515      P:000418 P:00041A 09443F            MOVEP             Y:RDFIFO,X0             ; then read to X0
1516      P:000419 P:00041B 54F400            MOVE              #$00FFFF,A1             ; mask off top 2 bytes ($FC)
                            00FFFF
1517      P:00041B P:00041D 200046            AND     X0,A                              ; since receiving 16 bits in 24bit register
1518      P:00041C P:00041E 000000            NOP
1519      P:00041D P:00041F 218400            MOVE              A1,X0
1520      P:00041E P:000420 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1521      P:00041F P:000421 00000C            RTS
1522                                CLR_FO_RTS
1523      P:000420 P:000422 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1524      P:000421 P:000423 00000C            RTS
1525   
1526   
1527                                ; ----------------------------------------------------------------------------
1528                                PCI_MESSAGE_TO_HOST
1529                                ;----------------------------------------------------------------------------
1530   
1531                                ; subroutine to send 4 words as a reply from PCI to the Host
1532                                ; using the DTXS-HRXS data path
1533                                ; PCI card writes here first then causes an interrupt INTA on
1534                                ; the PCI bus to alert the host to the reply message
1535   
1536      P:000422 P:000424 0A85A3            JSET    #DCTR_HF3,X:DCTR,*                ; make sure host ready to receive interrupt
                            000422
1537                                                                                    ; cleared via fast interrupt if host out of 
its ISR
1538      P:000424 P:000426 60F400            MOVE              #>DTXS_WD1,R0
                            00000B
1539   
1540      P:000426 P:000428 060480            DO      #4,PCI_MESSAGE_TO_HOST_RESTORE
                            00042A
1541      P:000428 P:00042A 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            000428
1542      P:00042A P:00042C 08D88D            MOVEP             X:(R0)+,X:DTXS
1543   
1544                                PCI_MESSAGE_TO_HOST_RESTORE
1545   
1546      
1547      P:00042B P:00042D 44F000            MOVE              X:SV_X0,X0              ; restore X0
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 32



                            00001D
1548      P:00042D P:00042F 60F000            MOVE              X:SV_R0,R0              ; restore X0
                            000021
1549   
1550                                ; all the transmit words are in the FIFO, interrupt the Host
1551                                ; the Host should clear this interrupt once it is detected.
1552                                ; It does this by writing to HCVR to cause a fast interrupt.
1553   
1554                                                                                    ; set flag to handshake interrupt (INTA) wit
h host.
1555      P:00042F P:000431 0A8523            BSET    #DCTR_HF3,X:DCTR
1556                                                                                    ; only interrupt in irq mode
1557      P:000430 P:000432 0A0184            JCLR    #MODE_IRQ,X:MODE,PCI_MESSAGE_TO_HOST_RETURN
                            000433
1558      P:000432 P:000434 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1559                                PCI_MESSAGE_TO_HOST_RETURN
1560      P:000433 P:000435 00000C            RTS
1561   
1562   
1563                                ;---------------------------------------------------------------
1564                                RD_DRXR
1565                                ;--------------------------------------------------------------
1566                                ; routine is used to read from HTXR-DRXR data path
1567                                ; which is used by the Host to communicate with the PCI board
1568                                ; the host writes 4 words to this FIFO then interrupts the PCI
1569                                ; which reads the 4 words and acts on them accordingly.
1570   
1571      P:000434 P:000436 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            000434
1572                                                                                    ; implies that host has written words
1573   
1574                                ; actually reading as slave here so this shouldn't be necessary......?
1575   
1576      P:000436 P:000438 0A8717            BCLR    #FC1,X:DPMC                       ; 24 bit read FC1 = 0, FC1 = 0
1577      P:000437 P:000439 0A8736            BSET    #FC0,X:DPMC
1578   
1579      P:000438 P:00043A 63F400            MOVE              #DRXR_WD1,R3
                            000007
1580      P:00043A P:00043C 0604A0            REP     #4
1581      P:00043B P:00043D 085B8B            MOVEP             X:DRXR,X:(R3)+
1582      P:00043C P:00043E 00000C            RTS
1583   
1584                                ;---------------------------------------------------------------
1585                                READ_FROM_PCI
1586                                ;--------------------------------------------------------------
1587                                ; sub routine to read a 24 bit word in from PCI bus --> Y memory
1588                                ; 32bit host address in accumulator B.
1589   
1590                                ; read as master
1591   
1592      P:00043D P:00043F 0C1890            EXTRACTU #$010010,B,A                     ; Get D31-16 bits only
                            010010
1593      P:00043F P:000441 000000            NOP
1594   
1595      P:000440 P:000442 210C00            MOVE              A0,A1
1596      P:000441 P:000443 000000            NOP
1597      P:000442 P:000444 547000            MOVE              A1,X:DPMC               ; high 16bits of address in DSP master cntr 
reg.
                            FFFFC7
1598                                                                                    ; 32 bit read so FC1 = 0 and FC0 = 0
1599   
1600      P:000444 P:000446 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 33



1601      P:000445 P:000447 0C1890            EXTRACTU #$010000,B,A
                            010000
1602      P:000447 P:000449 000000            NOP
1603      P:000448 P:00044A 210C00            MOVE              A0,A1
1604      P:000449 P:00044B 0140C2            OR      #$060000,A                        ; A1 gets written to DPAR register
                            060000
1605      P:00044B P:00044D 000000            NOP                                       ; C3-C0 of DPAR=0110 for memory read
1606      P:00044C P:00044E 08CC08  WRT_ADD   MOVEP             A1,X:DPAR               ; Write address to PCI bus - PCI READ action
1607      P:00044D P:00044F 000000            NOP                                       ; Pipeline delay
1608      P:00044E P:000450 0A8AA2  RD_PCI    JSET    #MRRQ,X:DPSR,GET_DAT              ; If MTRQ = 1 go read the word from host via
 FIFO
                            000457
1609      P:000450 P:000452 0A8A8A            JCLR    #TRTY,X:DPSR,RD_PCI               ; Bit is set if its a retry
                            00044E
1610      P:000452 P:000454 08F48A            MOVEP             #$0400,X:DPSR           ; Clear bit 10 = target retry bit
                            000400
1611      P:000454 P:000456 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for PCI addressing to be complete
                            000454
1612      P:000456 P:000458 0C044C            JMP     <WRT_ADD
1613   
1614      P:000457 P:000459 08480B  GET_DAT   MOVEP             X:DRXR,A0               ; Read 1st 16 bits of 32 bit word from host 
memory
1615      P:000458 P:00045A 084C0B            MOVEP             X:DRXR,A1               ; Read 2nd 16 bits of 32 bit word from host 
memory
1616   
1617                                ; note that we now have 4 bytes in X0 and X1.
1618                                ; The 32bit word was in host memory in little endian format
1619                                ; If form LSB --> MSB the bytes are b1, b2, b3, b4 in host memory
1620                                ; in progressing through the HTRX/DRXR FIFO the
1621                                ; bytes end up like this.....
1622                                ; then X0 = $00 b2 b1
1623                                ; and  X1 = $00 b4 b3
1624   
1625      P:000459 P:00045B 0604A0            REP     #4                                ; increment PCI address by four bytes.
1626      P:00045A P:00045C 000009            INC     B
1627      P:00045B P:00045D 000000            NOP
1628      P:00045C P:00045E 00000C            RTS
1629   
1630                                ;------------------------------------------------------------------------------------
1631                                RESTORE_REGISTERS
1632                                ;-------------------------------------------------------------------------------------
1633   
1634      P:00045D P:00045F 05A239            MOVEC             X:<SV_SR,SR
1635   
1636      P:00045E P:000460 509700            MOVE              X:<SV_A0,A0
1637      P:00045F P:000461 549800            MOVE              X:<SV_A1,A1
1638      P:000460 P:000462 529900            MOVE              X:<SV_A2,A2
1639   
1640      P:000461 P:000463 519A00            MOVE              X:<SV_B0,B0
1641      P:000462 P:000464 559B00            MOVE              X:<SV_B1,B1
1642      P:000463 P:000465 539C00            MOVE              X:<SV_B2,B2
1643   
1644      P:000464 P:000466 449D00            MOVE              X:<SV_X0,X0
1645      P:000465 P:000467 459E00            MOVE              X:<SV_X1,X1
1646   
1647      P:000466 P:000468 469F00            MOVE              X:<SV_Y0,Y0
1648      P:000467 P:000469 47A000            MOVE              X:<SV_Y1,Y1
1649   
1650      P:000468 P:00046A 60A100            MOVE              X:<SV_R0,R0
1651      P:000469 P:00046B 00000C            RTS
1652   
1653                                ;-------------------------------------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 34



1654                                SAVE_REGISTERS
1655                                ;-------------------------------------------------------------------------------------
1656   
1657      P:00046A P:00046C 052239            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1658   
1659      P:00046B P:00046D 501700            MOVE              A0,X:<SV_A0
1660      P:00046C P:00046E 541800            MOVE              A1,X:<SV_A1
1661      P:00046D P:00046F 521900            MOVE              A2,X:<SV_A2
1662   
1663      P:00046E P:000470 511A00            MOVE              B0,X:<SV_B0
1664      P:00046F P:000471 551B00            MOVE              B1,X:<SV_B1
1665      P:000470 P:000472 531C00            MOVE              B2,X:<SV_B2
1666   
1667      P:000471 P:000473 441D00            MOVE              X0,X:<SV_X0
1668      P:000472 P:000474 451E00            MOVE              X1,X:<SV_X1
1669   
1670      P:000473 P:000475 461F00            MOVE              Y0,X:<SV_Y0
1671      P:000474 P:000476 472000            MOVE              Y1,X:<SV_Y1
1672   
1673      P:000475 P:000477 602100            MOVE              R0,X:<SV_R0
1674      P:000476 P:000478 00000C            RTS
1675   
1676                                ;-------------------------------------------------------
1677                                XMT_WD_FIBRE
1678                                ;-----------------------------------------------------
1679                                ; 250 MHz code - Transmit contents of Accumulator A1 to the MCE
1680                                ; we want to send 32bit word in little endian fomat to the host.
1681                                ; i.e. b4b3b2b1 goes b1, b2, b3, b4
1682                                ; currently the bytes are in this order:
1683                                ;  A0 = $00 b2 b1
1684                                ;  A1 = $00 b4 b3
1685                                ;  A = $00 00 b4 b3 00 b2 b1
1686   
1687      
1688   
1689      P:000477 P:000479 212400            MOVE              B0,X0                   ; Save B
1690      P:000478 P:00047A 21A500            MOVE              B1,X1
1691   
1692      P:000479 P:00047B 0C1D31            ASL     #24,A,B
1693      P:00047A P:00047C 0140CE            AND     #>$0000FF,B                       ; B1=b1
                            0000FF
1694      P:00047C P:00047E 557000            MOVE              B1,X:FO_SEND
                            FFF000
1695   
1696      P:00047E P:000480 0C1D21            ASL     #16,A,B
1697      P:00047F P:000481 0140CE            AND     #>$0000FF,B
                            0000FF
1698      P:000481 P:000483 557000            MOVE              B1,X:FO_SEND            ; B1=b2
                            FFF000
1699   
1700      P:000483 P:000485 0C1C11            ASR     #8,A,B
1701      P:000484 P:000486 0140C6            AND     #>$0000FF,A
                            0000FF
1702      P:000486 P:000488 547000            MOVE              A1,X:FO_SEND            ; A1=b3
                            FFF000
1703   
1704      P:000488 P:00048A 0140CE            AND     #>$0000FF,B
                            0000FF
1705      P:00048A P:00048C 557000            MOVE              B1,X:FO_SEND            ; B1=b4
                            FFF000
1706   
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 35



1707      P:00048C P:00048E 208900            MOVE              X0,B0                   ; Restore B
1708      P:00048D P:00048F 20AD00            MOVE              X1,B1
1709      P:00048E P:000490 00000C            RTS
1710   
1711   
1712                                ;----------------------------------------------
1713                                FLUSH_PCI_FIFO
1714                                ;----------------------------------------------
1715      P:00048F P:000491 0A8A84            JCLR    #MARQ,X:DPSR,*
                            00048F
1716      P:000491 P:000493 0A862E            BSET    #CLRT,X:DPCR
1717      P:000492 P:000494 000000            NOP
1718      P:000493 P:000495 0A86AE            JSET    #CLRT,X:DPCR,*
                            000493
1719      P:000495 P:000497 00000C            RTS
1720   
1721                                ;----------------------------------------------
1722                                CLEAR_FO_FIFO
1723                                ;----------------------------------------------
1724      P:000496 P:000498 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
1725      P:000498 P:00049A 44F400            MOVE              #200000,X0
                            030D40
1726      P:00049A P:00049C 06C400            DO      X0,*+3
                            00049C
1727      P:00049C P:00049E 000000            NOP
1728      P:00049D P:00049F 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
1729      P:00049F P:0004A1 00000C            RTS
1730   
1731   
1732                                ;-----------------------------------------------
1733                                PCI_ERROR_CLEAR
1734                                ;-----------------------------------------------
1735      
1736      
1737      
1738      
1739      
1740      
1741   
1742      P:0004A0 P:0004A2 50F000            MOVE              X:DMA_ERRORS,A0
                            000031
1743      P:0004A2 P:0004A4 000008            INC     A
1744      P:0004A3 P:0004A5 000000            NOP
1745      P:0004A4 P:0004A6 507000            MOVE              A0,X:DMA_ERRORS
                            000031
1746   
1747      P:0004A6 P:0004A8 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            0004B4
1748      P:0004A8 P:0004AA 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
                            0004BE
1749      P:0004AA P:0004AC 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            0004C8
1750      P:0004AC P:0004AE 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            0004D2
1751      P:0004AE P:0004B0 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            0004DC
1752      P:0004B0 P:0004B2 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            0004E6
1753      P:0004B2 P:0004B4 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            0004F0
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 36



1754   
1755                                ERROR_TRTY
1756      P:0004B4 P:0004B6 50F000            MOVE              X:EC_TRTY,A0
                            000032
1757      P:0004B6 P:0004B8 000008            INC     A
1758      P:0004B7 P:0004B9 08F48A            MOVEP             #$0400,X:DPSR           ; Clear target retry error bit
                            000400
1759      P:0004B9 P:0004BB 507000            MOVE              A0,X:EC_TRTY
                            000032
1760      P:0004BB P:0004BD 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1761      P:0004BD P:0004BF 00000C            RTS
1762                                ERROR_TO
1763      P:0004BE P:0004C0 50F000            MOVE              X:EC_TO,A0
                            000033
1764      P:0004C0 P:0004C2 000008            INC     A
1765      P:0004C1 P:0004C3 08F48A            MOVEP             #$0800,X:DPSR           ; Clear timeout error bit
                            000800
1766      P:0004C3 P:0004C5 507000            MOVE              A0,X:EC_TO
                            000033
1767      P:0004C5 P:0004C7 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1768      P:0004C7 P:0004C9 00000C            RTS
1769                                ERROR_TDIS
1770      P:0004C8 P:0004CA 50F000            MOVE              X:EC_TDIS,A0
                            000034
1771      P:0004CA P:0004CC 000008            INC     A
1772      P:0004CB P:0004CD 08F48A            MOVEP             #$0200,X:DPSR           ; Clear target disconnect bit
                            000200
1773      P:0004CD P:0004CF 507000            MOVE              A0,X:EC_TDIS
                            000034
1774      P:0004CF P:0004D1 0A7031            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1775      P:0004D1 P:0004D3 00000C            RTS
1776                                ERROR_TAB
1777      P:0004D2 P:0004D4 50F000            MOVE              X:EC_TAB,A0
                            000035
1778      P:0004D4 P:0004D6 000008            INC     A
1779      P:0004D5 P:0004D7 08F48A            MOVEP             #$0100,X:DPSR           ; Clear target abort error bit
                            000100
1780      P:0004D7 P:0004D9 507000            MOVE              A0,X:EC_TAB
                            000035
1781      P:0004D9 P:0004DB 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1782      P:0004DB P:0004DD 00000C            RTS
1783                                ERROR_MAB
1784      P:0004DC P:0004DE 50F000            MOVE              X:EC_MAB,A0
                            000036
1785      P:0004DE P:0004E0 000008            INC     A
1786      P:0004DF P:0004E1 08F48A            MOVEP             #$0080,X:DPSR           ; Clear master abort error bit
                            000080
1787      P:0004E1 P:0004E3 507000            MOVE              A0,X:EC_MAB
                            000036
1788      P:0004E3 P:0004E5 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1789      P:0004E5 P:0004E7 00000C            RTS
1790                                ERROR_DPER
1791      P:0004E6 P:0004E8 50F000            MOVE              X:EC_DPER,A0
                            000037
1792      P:0004E8 P:0004EA 000008            INC     A
1793      P:0004E9 P:0004EB 08F48A            MOVEP             #$0040,X:DPSR           ; Clear data parity error bit
                            000040
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 37



1794      P:0004EB P:0004ED 507000            MOVE              A0,X:EC_DPER
                            000037
1795      P:0004ED P:0004EF 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1796      P:0004EF P:0004F1 00000C            RTS
1797                                ERROR_APER
1798      P:0004F0 P:0004F2 50F000            MOVE              X:EC_APER,A0
                            000038
1799      P:0004F2 P:0004F4 000008            INC     A
1800      P:0004F3 P:0004F5 08F48A            MOVEP             #$0020,X:DPSR           ; Clear address parity error bit
                            000020
1801      P:0004F5 P:0004F7 507000            MOVE              A0,X:EC_APER
                            000038
1802      P:0004F7 P:0004F9 0A7030            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1803      P:0004F9 P:0004FB 00000C            RTS
1804   
1805   
1806                                ;----------------------------------------------
1807                                BLOCK_TRANSFER
1808                                ;----------------------------------------------
1809                                ;   In:
1810                                ;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
1811                                ;   - BLOCK_SIZE is packet size, in bytes
1812                                ;   - BURST_SRC is start of data in Y memory
1813                                ;  Out:
1814                                ;   - BLOCK_SIZE will be decremented to zero.
1815                                ;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
1816                                ;   - BURST_SRC will be incremented by BLOCK_SIZE/2
1817                                ;  Trashes:
1818                                ;   - A and B
1819   
1820      
1821      P:0004FA P:0004FC 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            00002C
1822   
1823      P:0004FC P:0004FE 014085            CMP     #0,A
1824      P:0004FD P:0004FF 0AF0AA            JEQ     BLOCK_DONE
                            000541
1825   
1826      
1827   
1828      P:0004FF P:000501 20001B            CLR     B
1829      P:000500 P:000502 55F000            MOVE              X:PCI_BURST_SIZE,B1
                            00004B
1830   
1831      P:000502 P:000504 200005            CMP     B,A                               ; A ? B
1832      P:000503 P:000505 0E1505            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
1833      P:000504 P:000506 21CF00            MOVE              A,B                     ; This only moves A1,B1.
1834                                BLOCK_TRANSFER1
1835      P:000505 P:000507 200014            SUB     B,A                               ; A -= B
1836      P:000506 P:000508 014088            ADD     #0,B                              ; Clear carry bit
1837      P:000507 P:000509 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            00002C
1838      P:000509 P:00050B 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            00002D
1839      P:00050B P:00050D 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
1840   
1841      
1842      P:00050C P:00050E 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 38



1843      P:00050E P:000510 50F000            MOVE              X:BURST_SRC,A0
                            000030
1844      P:000510 P:000512 08C82F            MOVEP             A0,X:DSR0               ; DMA source
1845      P:000511 P:000513 200010            ADD     B,A
1846      P:000512 P:000514 00000B            DEC     B
1847      P:000513 P:000515 507000            MOVE              A0,X:BURST_SRC          ; BURST_SRC += BURST_SIZE/2
                            000030
1848   
1849      P:000515 P:000517 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
1850   
1851      
1852      P:000516 P:000518 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
1853   
1854                                BLOCK_PCI
1855      
1856      P:000518 P:00051A 200013            CLR     A
1857      P:000519 P:00051B 20001B            CLR     B
1858      P:00051A P:00051C 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            00002D
1859      P:00051C P:00051E 00000B            DEC     B                                 ; n8 - 1
1860      P:00051D P:00051F 014088            ADD     #0,B                              ; Clear carry
1861      P:00051E P:000520 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
1862      P:00051F P:000521 014088            ADD     #0,B                              ; Clear carry
1863      P:000520 P:000522 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
1864   
1865      P:000521 P:000523 50F000            MOVE              X:BURST_DEST_HI,A0
                            00002F
1866   
1867      P:000523 P:000525 200010            ADD     B,A
1868      P:000524 P:000526 000000            NOP
1869      P:000525 P:000527 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
1870   
1871      P:000527 P:000529 280700            MOVE              #$07,A0
1872      P:000528 P:00052A 014088            ADD     #0,B                              ; Clear carry
1873      P:000529 P:00052B 0C1D20            ASL     #16,A,A
1874      P:00052A P:00052C 51F000            MOVE              X:BURST_DEST_LO,B0
                            00002E
1875      P:00052C P:00052E 200010            ADD     B,A
1876      P:00052D P:00052F 000000            NOP
1877   
1878      P:00052E P:000530 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
1879   
1880                                BLOCK_CHECK
1881      P:00052F P:000531 000000            NOP
1882      P:000530 P:000532 000000            NOP
1883      P:000531 P:000533 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for burst termination
                            000531
1884   
1885      
1886      P:000533 P:000535 0A8AAE            JSET    #MDT,X:DPSR,BLOCK_OK
                            00053C
1887   
1888      P:000535 P:000537 0D04A0            JSR     PCI_ERROR_CLEAR
1889   
1890      P:000536 P:000538 0A7010            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
1891      P:000538 P:00053A 0E8542            JCS     <BLOCK_RESTART
1892   
1893      P:000539 P:00053B 0A7011            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 39



1894      P:00053B P:00053D 0E8543            JCS     <BLOCK_RESUME
1895   
1896                                BLOCK_OK
1897      P:00053C P:00053E 50F000            MOVE              X:BURST_SIZE,A0         ; Pass # of words written to updater
                            00002D
1898      P:00053E P:000540 0BF080            JSR     BLOCK_UPDATE
                            000555
1899      P:000540 P:000542 0C04FA            JMP     BLOCK_TRANSFER                    ; Finish the block
1900                                BLOCK_DONE
1901      P:000541 P:000543 00000C            RTS                                       ; Done
1902   
1903                                BLOCK_RESTART
1904      P:000542 P:000544 0C0518            JMP     BLOCK_PCI                         ; Recalculate pci and resend
1905   
1906                                BLOCK_RESUME
1907      P:000543 P:000545 200013            CLR     A
1908      P:000544 P:000546 20001B            CLR     B
1909      P:000545 P:000547 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
1910      P:000546 P:000548 0A8A8F            JCLR    #15,X:DPSR,BLOCK_RESUME1
                            000549
1911   
1912      P:000548 P:00054A 000009            INC     B
1913   
1914                                BLOCK_RESUME1
1915   
1916      P:000549 P:00054B 000009            INC     B                                 ; We want N, not N-1.
1917      P:00054A P:00054C 014088            ADD     #0,B                              ; Clear carry
1918      P:00054B P:00054D 0C1C20            ASR     #16,A,A
1919      P:00054C P:00054E 200018            ADD     A,B                               ; B is words remaining
1920      P:00054D P:00054F 014088            ADD     #0,B                              ; Clear carry
1921      P:00054E P:000550 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
1922      P:00054F P:000551 50F000            MOVE              X:BURST_SIZE,A0
                            00002D
1923      P:000551 P:000553 200014            SUB     B,A                               ; A is words written
1924   
1925      P:000552 P:000554 0BF080            JSR     BLOCK_UPDATE
                            000555
1926      P:000554 P:000556 0C0518            JMP     BLOCK_PCI                         ; Recalculate pci and resend
1927   
1930                                BLOCK_UPDATE
1931      
1932      
1933   
1934      P:000555 P:000557 210500            MOVE              A0,X1                   ; Save A
1935      P:000556 P:000558 210900            MOVE              A0,B0                   ; Save A again...
1936      P:000557 P:000559 218D00            MOVE              A1,B1                   ; Save A again...
1937      P:000558 P:00055A 000000            NOP
1938   
1939      P:000559 P:00055B 60F400            MOVE              #BURST_DEST_LO,R0
                            00002E
1940      P:00055B P:00055D 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates BURST_DEST
                            000606
1941   
1942      P:00055D P:00055F 57F000            MOVE              X:BURST_SIZE,B
                            00002D
1943      P:00055F P:000561 20006C            SUB     X1,B                              ; Zero flag must be preserved!
1944      P:000560 P:000562 000000            NOP
1945      P:000561 P:000563 557000            MOVE              B1,X:BURST_SIZE
                            00002D
1946   
1947      P:000563 P:000565 00000C            RTS
1948   
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 40



1949   
1950                                ;----------------------------------------------;
1951                                ;  MCE PACKET PROCESSING                       ;
1952                                ;----------------------------------------------;
1953   
1954                                ;       Given a dword count in A, computes number of half FIFOs and
1955                                ;       number of left over FIFO reads required to get the whole
1956                                ;       packet.
1957   
1958                                ;       Input: A is packet size, in dwords
1959                                ;       Output: sets X:TOTAL_BUFFS and X:LEFT_TO_READ
1960                                ;       Trashes: A,B,X0
1961   
1962   
1963                                PACKET_PARTITIONS
1964      P:000564 P:000566 507000            MOVE              A0,X:PACKET_SIZE
                            00002B
1965   
1966      P:000566 P:000568 014088            ADD     #0,B                              ; Clear carry
1967      P:000567 P:000569 0C1D02            ASL     #1,A,A                            ;  * 2
1968      P:000568 P:00056A 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
1969      P:000569 P:00056B 240000            MOVE              #0,X0
1970      P:00056A P:00056C 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
1971   
1972      P:00056C P:00056E 557000            MOVE              B1,X:TOTAL_BUFFS
                            000027
1973      P:00056E P:000570 507000            MOVE              A0,X:LEFT_TO_READ
                            000028
1974      P:000570 P:000572 00000C            RTS
1975   
1976   
1979   
1980   
1981                                BUFFER_PACKET
1982   
1983      
1984      
1985   
1986   
1987                                BUFFER_PACKET_HALFS
1988      P:000571 P:000573 062700            DO      X:TOTAL_BUFFS,BUFFER_PACKET_SINGLES
                            000576
1989      P:000573 P:000575 0BF080            JSR     WAIT_FIFO_HALF
                            000586
1990      P:000575 P:000577 0BF080            JSR     BUFFER_PACKET_HALF
                            000581
1991                                BUFFER_PACKET_SINGLES
1992      P:000577 P:000579 062800            DO      X:LEFT_TO_READ,BUFFER_PACKET_DONE
                            00057F
1993                                BUFFER_PACKET_SINGLE
1994      P:000579 P:00057B 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            000275
1995      P:00057B P:00057D 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE
                            000579
1996      P:00057D P:00057F 01AD80            JCLR    #EF,X:PDRD,BUFFER_PACKET_SINGLE   ; Protect against metastability
                            000579
1997      P:00057F P:000581 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
1998                                BUFFER_PACKET_DONE
1999      P:000580 P:000582 00000C            RTS
2000   
2001                                BUFFER_PACKET_HALF
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 41



2002      
2003      P:000581 P:000583 060082            DO      #512,BUFFER_PACKET_HALF_DONE
                            000584
2004      P:000583 P:000585 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2005      P:000584 P:000586 000000            NOP
2006                                BUFFER_PACKET_HALF_DONE
2007      P:000585 P:000587 00000C            RTS
2008   
2009                                WAIT_FIFO_HALF
2010      P:000586 P:000588 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            00058F
2011      P:000588 P:00058A 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
                            000586
2012      P:00058A P:00058C 000000            NOP
2013      P:00058B P:00058D 000000            NOP
2014      P:00058C P:00058E 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            000586
2015      P:00058E P:000590 00000C            RTS
2016   
2017                                FATALITY_HANDLER
2018      P:00058F P:000591 0C0100            JMP     START                             ; What could possibly go wrong?
2019   
2020   
2021                                ;       Reads a packet from the fifo, discarding it.
2022                                ;
2023                                ;       In: TOTAL_BUFFS & LEFT_TO_READ
2024                                ;       Trashes: A0
2025   
2026                                DROP_PACKET
2027      P:000590 P:000592 062700            DO      X:TOTAL_BUFFS,DROP_PACKET_SINGLES
                            000595
2028      P:000592 P:000594 0D0586            JSR     WAIT_FIFO_HALF
2029      P:000593 P:000595 0BF080            JSR     DROP_FIFO_HALF
                            0005A0
2030      P:000595 P:000597 000000            NOP
2031                                DROP_PACKET_SINGLES
2032      P:000596 P:000598 062800            DO      X:LEFT_TO_READ,DROP_PACKET_DONE
                            00059E
2033                                DROP_PACKET_SINGLE
2034      P:000598 P:00059A 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            000275
2035      P:00059A P:00059C 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE
                            000598
2036      P:00059C P:00059E 01AD80            JCLR    #EF,X:PDRD,DROP_PACKET_SINGLE     ; Protect against metastability
                            000598
2037      P:00059E P:0005A0 09483F            MOVEP             Y:RDFIFO,A0
2038                                DROP_PACKET_DONE
2039      P:00059F P:0005A1 00000C            RTS
2040   
2041                                DROP_FIFO_HALF
2042      
2043      P:0005A0 P:0005A2 060082            DO      #512,DROP_FIFO_DONE
                            0005A2
2044      P:0005A2 P:0005A4 09483F            MOVEP             Y:RDFIFO,A0
2045                                DROP_FIFO_DONE
2046      P:0005A3 P:0005A5 00000C            RTS
2047   
2048   
2049                                ;----------------------------------------------;
2050                                ;  TIMER HANDLING                              ;
2051                                ;----------------------------------------------;
2052   
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 42



2053                                ; Start value is TLR, count is in TCR, int occurs at TCPR
2054                                ; Must set TCSR[TCIE] to enable int
2055                                ; Must set TCSR[T] for timer to restart
2056   
2057                                TIMER_ENABLE
2058      P:0005A4 P:0005A6 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2059      P:0005A6 P:0005A8 000000            NOP
2060      P:0005A7 P:0005A9 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2061      P:0005A9 P:0005AB 00000C            RTS
2062   
2063                                TIMER_DISABLE
2064      P:0005AA P:0005AC 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
                            300200
2065      P:0005AC P:0005AE 000000            NOP
2066      P:0005AD P:0005AF 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2067      P:0005AF P:0005B1 00000C            RTS
2068   
2070                                TIMER_ACTION
2071      P:0005B0 P:0005B2 56F000            MOVE              X:QT_INFORM_IDX,A
                            000043
2072      P:0005B2 P:0005B4 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2073      P:0005B4 P:0005B6 000000            NOP
2074      P:0005B5 P:0005B7 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2075      P:0005B7 P:0005B9 0140C5            CMP     #>0,A                             ; If inform_idx != 0
                            000000
2076      P:0005B9 P:0005BB 0AF0AA            JEQ     TIMER_ACTION_OK
                            0005BD
2077      P:0005BB P:0005BD 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2078                                TIMER_ACTION_OK
2079      P:0005BD P:0005BF 00000C            RTS
2080   
2081   
2082   
2083                                ;----------------------------------------------;
2084                                ;  CIRCULAR BUFFER HANDLING                    ;
2085                                ;----------------------------------------------;
2086   
2087                                BUFFER_INCR
2088   
2089      P:0005BE P:0005C0 56F000            MOVE              X:QT_BUF_HEAD,A         ; If head + 1 == max
                            00003F
2090      P:0005C0 P:0005C2 014180            ADD     #1,A                              ;
2091      P:0005C1 P:0005C3 57F000            MOVE              X:QT_BUF_MAX,B          ;
                            00003C
2092      P:0005C3 P:0005C5 20000D            CMP     A,B                               ;
2093      P:0005C4 P:0005C6 0AF0AF            JLE     BUFFER_RESET                      ;       head = 0
                            0005D0
2094                                                                                    ; else
2095      P:0005C6 P:0005C8 567000            MOVE              A,X:QT_BUF_HEAD         ;       head = head + 1
                            00003F
2096   
2097      P:0005C8 P:0005CA 20001B            CLR     B
2098      P:0005C9 P:0005CB 51F000            MOVE              X:QT_BUF_SIZE,B0
                            00003B
2099      P:0005CB P:0005CD 60F400            MOVE              #QT_DEST_LO,R0
                            000041
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 43



2100      P:0005CD P:0005CF 0BF080            JSR     ADD_HILO_ADDRESS                  ; QT_DEST += QT_BUF_SIZE
                            000606
2101   
2102      P:0005CF P:0005D1 00000C            RTS
2103   
2104   
2105                                BUFFER_RESET
2106      P:0005D0 P:0005D2 60F400            MOVE              #QT_BASE_LO,R0
                            000039
2107      P:0005D2 P:0005D4 0BF080            JSR     LOAD_HILO_ADDRESS
                            000600
2108      P:0005D4 P:0005D6 60F400            MOVE              #QT_DEST_LO,R0
                            000041
2109      P:0005D6 P:0005D8 0BF080            JSR     SAVE_HILO_ADDRESS                 ; QT_DEST_LO = QT_BASE_LO
                            000608
2110   
2111      P:0005D8 P:0005DA 240000            MOVE              #0,X0
2112      P:0005D9 P:0005DB 447000            MOVE              X0,X:QT_BUF_HEAD        ; HEAD = 0
                            00003F
2113      P:0005DB P:0005DD 00000C            RTS
2114   
2115   
2116                                BUFFER_INFORM_CHECK
2117      P:0005DC P:0005DE 56F000            MOVE              X:QT_INFORM_IDX,A
                            000043
2118      P:0005DE P:0005E0 014180            ADD     #1,A
2119      P:0005DF P:0005E1 57F000            MOVE              X:QT_INFORM,B
                            00003E
2120      P:0005E1 P:0005E3 20000D            CMP     A,B
2121      P:0005E2 P:0005E4 0AF0A7            JGT     BUFFER_INFORM_OK                  ; If inform_idx + 1 <= inform
                            0005E6
2122      P:0005E4 P:0005E6 0A7034            BSET    #QT_FLUSH,X:STATUS                ;       schedule inform
                            000000
2123   
2124                                BUFFER_INFORM_OK
2125      P:0005E6 P:0005E8 567000            MOVE              A,X:QT_INFORM_IDX       ; inform_idx = inform_idx + 1
                            000043
2126      P:0005E8 P:0005EA 00000C            RTS
2127   
2128   
2129                                ;---------------------------------------------------------------
2130                                BUFFER_INFORM
2131                                ;---------------------------------------------------------------
2132                                ; Informs host of current buffer status
2133   
2134      P:0005E9 P:0005EB 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2135      P:0005EB P:0005ED 440B00            MOVE              X0,X:<DTXS_WD1
2136   
2137      P:0005EC P:0005EE 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            00003F
2138      P:0005EE P:0005F0 440C00            MOVE              X0,X:<DTXS_WD2
2139   
2140      P:0005EF P:0005F1 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000040
2141      P:0005F1 P:0005F3 440D00            MOVE              X0,X:<DTXS_WD3
2142   
2143      P:0005F2 P:0005F4 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
                            000044
2144      P:0005F4 P:0005F6 440E00            MOVE              X0,X:<DTXS_WD4
2145   
2146   
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  main.asm  Page 44



2147      P:0005F5 P:0005F7 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            0005FF
2148      P:0005F7 P:0005F9 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            0005FF
2149   
2150      P:0005F9 P:0005FB 0D0422            JSR     PCI_MESSAGE_TO_HOST
2151   
2152      P:0005FA P:0005FC 0A7014            BCLR    #QT_FLUSH,X:STATUS
                            000000
2153      P:0005FC P:0005FE 240000            MOVE              #0,X0                   ; Reset inform index
2154      P:0005FD P:0005FF 447000            MOVE              X0,X:QT_INFORM_IDX
                            000043
2155                                INFORM_EXIT
2156      P:0005FF P:000601 00000C            RTS
2157   
2158   
2159   
2160                                ;----------------------------------------------;
2161                                ;  ADDRESS HANDLING                            ;
2162                                ;----------------------------------------------;
2163   
2167   
2168                                LOAD_HILO_ADDRESS
2169      
2170      
2171      P:000600 P:000602 200013            CLR     A
2172      P:000601 P:000603 50D800            MOVE              X:(R0)+,A0
2173      P:000602 P:000604 44D000            MOVE              X:(R0)-,X0
2174      P:000603 P:000605 0C1940            INSERT  #$010010,X0,A
                            010010
2175      P:000605 P:000607 00000C            RTS
2176   
2177                                ADD_HILO_ADDRESS
2178      
2179      
2180   
2181      P:000606 P:000608 0D0600            JSR     LOAD_HILO_ADDRESS
2182      P:000607 P:000609 200010            ADD     B,A
2183   
2184                                SAVE_HILO_ADDRESS
2185      
2186      
2187   
2188      P:000608 P:00060A 445800            MOVE              X0,X:(R0)+              ; pre-increment
2189      P:000609 P:00060B 240000            MOVE              #0,X0
2190      P:00060A P:00060C 0C1D11            ASL     #8,A,B
2191      P:00060B P:00060D 0C1940            INSERT  #$008010,X0,A
                            008010
2192      P:00060D P:00060F 555000            MOVE              B1,X:(R0)-              ; store hi16
2193      P:00060E P:000610 506000            MOVE              A0,X:(R0)
2194      P:00060F P:000611 0C1C90            ASR     #8,B,A
2195      P:000610 P:000612 00000C            RTS
2196   
2197   
2198                                BOOTCODE_END
2199                                 BOOTEND_ADDR
2200      000611                              EQU     @CVI(BOOTCODE_END)
2201   
2202                                PROGRAM_END
2203      000611                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2204                                          INCLUDE 'vars.asm'
2205                                      COMMENT *
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  vars.asm  Page 45



2206   
2207                                Variable table and bit defines for our variables.
2208   
2209                                See info.asm for versioning and authors.
2210   
2211                                        *
2212   
2213   
2214                                ; The variable table is mapped to X memory but stored inline in the
2215                                ; eeprom / P memory after the main code (but before the application
2216                                ; area).
2217   
2218      X:000000 P:000613                   ORG     X:VAR_TBL,P:
2219   
2220   
2221                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2222                                 VAR_TBL_START
2223      000611                              EQU     @LCV(L)-2
2224                                          ENDIF
2225   
2226                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2228                                          ENDIF
2229   
2230                                ; -----------------------------------------------
2231 d    X:000000 P:000613 000000  STATUS    DC      0                                 ; Internal control flags
2232 d    X:000001 P:000614 000000  MODE      DC      0                                 ; Configure special options
2233   
2234 d                               FRAME_COUNT
2235 d    X:000002 P:000615 000000            DC      0
2236 d    X:000003 P:000616 550105  REV_NUMBER DC     $550105                           ; byte 0 = minor revision #
2237                                                                                    ; byte 1 = major revision #
2238                                                                                    ; byte 2 = release Version (ascii letter)
2239 d    X:000004 P:000617 000000  REV_DATA  DC      $000000                           ; data: day-month-year
2240 d    X:000005 P:000618 2EF490  P_CHECKSUM DC     $2EF490                           ;**** DO NOT CHANGE
2241                                ; -------------------------------------------------
2242 d    X:000006 P:000619 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2243                                ; ----------------------------------------------------------------------------------------------
----------------
2244   
2245 d    X:000007 P:00061A 000000  DRXR_WD1  DC      0
2246 d    X:000008 P:00061B 000000  DRXR_WD2  DC      0
2247 d    X:000009 P:00061C 000000  DRXR_WD3  DC      0
2248 d    X:00000A P:00061D 000000  DRXR_WD4  DC      0
2249 d    X:00000B P:00061E 000000  DTXS_WD1  DC      0
2250 d    X:00000C P:00061F 000000  DTXS_WD2  DC      0
2251 d    X:00000D P:000620 000000  DTXS_WD3  DC      0
2252 d    X:00000E P:000621 000000  DTXS_WD4  DC      0
2253   
2254 d    X:00000F P:000622 000000  HEAD_W1_1 DC      0
2255 d    X:000010 P:000623 000000  HEAD_W1_0 DC      0
2256 d    X:000011 P:000624 000000  HEAD_W2_1 DC      0
2257 d    X:000012 P:000625 000000  HEAD_W2_0 DC      0
2258 d    X:000013 P:000626 000000  HEAD_W3_1 DC      0
2259 d    X:000014 P:000627 000000  HEAD_W3_0 DC      0
2260 d    X:000015 P:000628 000000  HEAD_W4_1 DC      0
2261 d    X:000016 P:000629 000000  HEAD_W4_0 DC      0
2262   
2263 d    X:000017 P:00062A 000000  SV_A0     DC      0
2264 d    X:000018 P:00062B 000000  SV_A1     DC      0
2265 d    X:000019 P:00062C 000000  SV_A2     DC      0
2266 d    X:00001A P:00062D 000000  SV_B0     DC      0
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  vars.asm  Page 46



2267 d    X:00001B P:00062E 000000  SV_B1     DC      0
2268 d    X:00001C P:00062F 000000  SV_B2     DC      0
2269 d    X:00001D P:000630 000000  SV_X0     DC      0
2270 d    X:00001E P:000631 000000  SV_X1     DC      0
2271 d    X:00001F P:000632 000000  SV_Y0     DC      0
2272 d    X:000020 P:000633 000000  SV_Y1     DC      0
2273 d    X:000021 P:000634 000000  SV_R0     DC      0
2274   
2275 d    X:000022 P:000635 000000  SV_SR     DC      0                                 ; stauts register save.
2276   
2277 d                               PACKET_SIZE_LOW
2278 d    X:000023 P:000636 000000            DC      0
2279 d                               PACKET_SIZE_HIH
2280 d    X:000024 P:000637 000000            DC      0
2281   
2282 d    X:000025 P:000638 00A5A5  PREAMB1   DC      $A5A5                             ; pramble 16-bit word....2 of which make up 
first preamble 32bit word
2283 d    X:000026 P:000639 005A5A  PREAMB2   DC      $5A5A                             ; preamble 16-bit word....2 of which make up
 second preamble 32bit word
2284   
2285 d                               TOTAL_BUFFS
2286 d    X:000027 P:00063A 000000            DC      0                                 ; total number of 512 buffers in packet
2287 d                               LEFT_TO_READ
2288 d    X:000028 P:00063B 000000            DC      0                                 ; number of words (16 bit) left to read afte
r last 512 buffer
2289 d                               LEFT_TO_WRITE
2290 d    X:000029 P:00063C 000000            DC      0                                 ; number of woreds (32 bit) to write to host
 i.e. half of those left over read
2291 d                               NUM_LEFTOVER_BLOCKS
2292 d    X:00002A P:00063D 000000            DC      0                                 ; small block DMA burst transfer
2293   
2294 d                               PACKET_SIZE
2295 d    X:00002B P:00063E 000000            DC      0                                 ; Size, in dwords of most recent packet from
 MCE.
2296   
2297   
2299   
2300 d    X:00002C P:00063F 000000  BLOCK_SIZE DC     0
2301 d    X:00002D P:000640 000000  BURST_SIZE DC     0
2302 d                               BURST_DEST_LO
2303 d    X:00002E P:000641 000000            DC      0
2304 d                               BURST_DEST_HI
2305 d    X:00002F P:000642 000000            DC      0
2306 d    X:000030 P:000643 000000  BURST_SRC DC      0
2307   
2308 d    X:000031 P:000644 000000  DMA_ERRORS DC     0
2309 d    X:000032 P:000645 000000  EC_TRTY   DC      0
2310 d    X:000033 P:000646 000000  EC_TO     DC      0
2311 d    X:000034 P:000647 000000  EC_TDIS   DC      0
2312 d    X:000035 P:000648 000000  EC_TAB    DC      0
2313 d    X:000036 P:000649 000000  EC_MAB    DC      0
2314 d    X:000037 P:00064A 000000  EC_DPER   DC      0
2315 d    X:000038 P:00064B 000000  EC_APER   DC      0
2316   
2317   
2319   
2320 d    X:000039 P:00064C 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2321 d    X:00003A P:00064D 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2322 d                               QT_BUF_SIZE
2323 d    X:00003B P:00064E 000000            DC      0                                 ; Separation of buffers, in bytes
2324 d    X:00003C P:00064F 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2325 d                               QT_FRAME_SIZE
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  vars.asm  Page 47



2326 d    X:00003D P:000650 000000            DC      0                                 ; Expected data packet size, in bytes
2327 d    X:00003E P:000651 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2328   
2329 d                               QT_BUF_HEAD
2330 d    X:00003F P:000652 000000            DC      0                                 ; Index of buf for next write
2331 d                               QT_BUF_TAIL
2332 d    X:000040 P:000653 000000            DC      0                                 ; Index at which we must not write
2333   
2334 d    X:000041 P:000654 000000  QT_DEST_LO DC     0                                 ; PC address for next write
2335 d    X:000042 P:000655 000000  QT_DEST_HI DC     0                                 ;
2336 d                               QT_INFORM_IDX
2337 d    X:000043 P:000656 000000            DC      0                                 ; Number of packets since last inform
2338 d    X:000044 P:000657 000000  QT_DROPS  DC      0                                 ; Dropped packets
2339   
2340   
2342 d    X:000045 P:000658 000000  RP_BASE_LO DC     0
2343 d    X:000046 P:000659 000000  RP_BASE_HI DC     0
2344 d                               RP_MAX_SIZE
2345 d    X:000047 P:00065A 000000            DC      0
2346 d    X:000048 P:00065B 000000  RP_DROPS  DC      0
2347   
2349 d                               CON_SOURCE_LO
2350 d    X:000049 P:00065C 000000            DC      0
2351 d                               CON_SOURCE_HI
2352 d    X:00004A P:00065D 000000            DC      0
2353   
2355 d                               PCI_BURST_SIZE
2356 d    X:00004B P:00065E 000040            DC      $40                               ; Should be < 4*latency assigned by OS
2357   
2358 d    X:00004C P:00065F 000000  BDEBUG0   DC      0
2359 d    X:00004D P:000660 000000  BDEBUG1   DC      0
2360 d    X:00004E P:000661 000000  BDEBUG2   DC      0
2361 d    X:00004F P:000662 000000  BDEBUG3   DC      0
2362 d    X:000050 P:000663 000000  BDEBUG4   DC      0
2363 d    X:000051 P:000664 000000  BDEBUG5   DC      0
2364 d    X:000052 P:000665 000000  BDEBUG6   DC      0
2365 d    X:000053 P:000666 000000  BDEBUG7   DC      0
2366 d    X:000054 P:000667 000000  BDEBUG8   DC      0
2367 d    X:000055 P:000668 000000  BDEBUG9   DC      0
2368   
2369                                ;----------------------------------------------------------
2370   
2372   
2373                                 APPLICATION_RUNNING
2374      000000                              EQU     0                                 ; Indicates application is in progress
2375                                 SEND_TO_HOST
2376      000001                              EQU     1                                 ; set in HST ISR when host ready for packet 
(stays set until after HST reply)
2377                                 FATAL_ERROR
2378      000002                              EQU     2                                 ; PCI message to host error detected by driv
er....
2379      000003                    FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays s
et till packet processed
2380   
2381                                 PREAMBLE_ERROR
2382      000006                              EQU     6                                 ; set if preamble error detected
2383      000007                    DATA_DLY  EQU     7                                 ; set in CON ISR if MCE command is 'GO'.  US
ed to add delay to first returned data packet
2384   
2385      000009                    HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of p
acket (stays set until after HST reply)
2386   
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  vars.asm  Page 48



2387      00000A                    CON_DEMAND EQU    10                                ; Host has requested an MCE command be sent
2388   
2389                                 PCIDMA_RESTART
2390      000010                              EQU     16                                ; DMA flags used for error recovery
2391                                 PCIDMA_RESUME
2392      000011                              EQU     17
2393                                 PCIDMA_RETRY
2394      000012                              EQU     18
2395   
2396      000014                    QT_FLUSH  EQU     20                                ; Set when it is time to inform Host of curr
ent buffer position.
2397                                 RP_BUFFER_FULL
2398      000015                              EQU     21                                ; Set when Quiet RP buffer is occupied.
2399   
2401   
2402                                 MODE_APPLICATION
2403      000000                              EQU     0                                 ; set if PCI application to run
2404      000001                    MODE_CHOKE EQU    1                                 ; drop all packets from MCE
2405      000002                    MODE_QT   EQU     2                                 ; Quiet transfer for data packets
2406                                 MODE_RP_BUFFER
2407      000003                              EQU     3                                 ; Quiet transfer for reply packets
2408      000004                    MODE_IRQ  EQU     4                                 ; Enable PCI interrupts on NFY
2409   
2410   
2411                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2412                                 VAR_TBL_END
2413      000667                              EQU     @LCV(L)-2
2414                                          ENDIF
2415   
2416                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2418                                          ENDIF
2419   
2420                                 VAR_TBL_LENGTH
2421      000056                              EQU     VAR_TBL_END-VAR_TBL_START
2422                                          INCLUDE 'app.asm'
2423                                        COMMENT *
2424   
2425                                Auxiliary application area.
2426   
2427                                See info.asm for versioning and authors.
2428   
2429                                        *
2430                                          PAGE    132                               ; Printronix page width - 132 columns
2431                                          OPT     CEX                               ; print DC evaluations
2432   
2433                                          IF      @CVS(N,*)>=APPLICATION
2435                                          ENDIF
2436   
2437   
2438                                ;--------------------------------------------
2439                                ; APPLICATION AREA
2440                                ;---------------------------------------------
2441                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2442      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2443                                          ENDIF
2444   
2445                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2447                                          ENDIF
2448   
2449                                ; starts with no application loaded
2450                                ; so just reply with an error if we get a GOA command
2451   
Motorola DSP56300 Assembler  Version 6.3.4   08-07-23  16:03:26  app.asm  Page 49



2452      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2453      P:000802 P:000804 440B00            MOVE              X0,X:<DTXS_WD1          ; REPly
2454      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2455      P:000805 P:000807 440C00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2456      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2457      P:000808 P:00080A 440D00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2458      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2459      P:00080B P:00080D 440E00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2460      P:00080C P:00080E 0D045D            JSR     <RESTORE_REGISTERS
2461      P:00080D P:00080F 0D0422            JSR     <PCI_MESSAGE_TO_HOST
2462      P:00080E P:000810 0A0100            BCLR    #MODE_APPLICATION,X:<MODE
2463      P:00080F P:000811 0C016D            JMP     PACKET_IN
2464   
2465   
2466      000812                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM
2467   
2468   
2469   

0    Errors
0    Warnings


