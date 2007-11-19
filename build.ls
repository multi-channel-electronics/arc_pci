Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  build.asm  Page 1



1                           COMMENT *
2      
3                          Compile this to build all files together.
4      
5                          Project:     SCUBA 2
6                          Author:      DAVID ATKINSON
7                          Target:      250MHz SDSU PCI card - DSP56301
8                          Controller:  For use with SCUBA 2 Multichannel Electronics
9      
10                         Modified:    MATTHEW HASSELFIELD
11     
12                         Assembler directives:
13                                 ROM=EEPROM => EEPROM CODE
14                                 ROM=ONCE => ONCE CODE
15     
16                                 *
17                                   PAGE    132                               ; Printronix page width - 132 columns
18                                   OPT     CEX                               ; print DC evaluations
19     
20                                   INCLUDE 'header.asm'
21                               COMMENT *
22     
23                         PCI code header file.
24     
25                         Project:     SCUBA 2
26                         Author:      DAVID ATKINSON
27                         Target:      250MHz SDSU PCI card - DSP56301
28                         Controller:  For use with SCUBA 2 Multichannel Electronics
29     
30                         Modified:    MATTHEW HASSELFIELD
31     
32                         Assembler directives:
33                                 ROM=0 => EEPROM CODE
34                                 ROM=1 => ROM CODE
35     
36                                 *
37                                   PAGE    132                               ; Printronix page width - 132 columns
38                                   OPT     CEX                               ; print DC evaluations
39     
**** 40 [header.asm 20]:  INCLUDE PCI_header.asm HERE  
40                                   MSG     ' INCLUDE PCI_header.asm HERE  '
41     
42                         ; Equates to define the X: memory tables
43        000000           VAR_TBL   EQU     0                                 ; Variables and constants table
44     
45                         APPLICATION
46        000800                     EQU     $800                              ; application memory start location in P memory
47                                                                             ; note applications should start with this address
48                                                                             ; and end with a JMP to PACKET_IN
49                                                                             ; if only want appl to run once
50                                                                             ; penultimate line of code should be
51                                                                             ; to clear bit APPLICATION_LOADED in STATUS
52                                                                             ; otherwise will run continusly until 'STP'
53                                                                             ; command is sent
54     
55        000200           APPL_PARAM EQU    $200                              ; application parameters in x memory start here.
56     
57     
58        000200           HF_FIFO   EQU     512                               ; number of 16 bit words in a half full FIFO
59        000020           SMALL_BLK EQU     32                                ; small block burst size for < 512 pixels
60                         IMAGE_BUFFER
61        000000                     EQU     0                                 ; location in y memory of image buffer....
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  header.asm  Page 2



62     
63     
64                         ;Status bits
65     
66                         APPLICATION_LOADED
67        000000                     EQU     0                                 ; set if PCI application to run
68                         SEND_TO_HOST
69        000001                     EQU     1                                 ; set in HST ISR when host ready for packet (stays se
t until after HST reply)
70                         FATAL_ERROR
71        000002                     EQU     2                                 ; PCI message to host error detected by driver....
72        000003           FO_WRD_RCV EQU    3                                 ; set when packet detected in FIFO - stays set till p
acket processed
73     
74                         ;INTA_FLAG              EQU     4   ; used for interupt handshaking with host
75        000005           BYTE_SWAP EQU     5                                 ; flag to show byte swapping enabled
76                         PREAMBLE_ERROR
77        000006                     EQU     6                                 ; set if preamble error detected
78        000007           DATA_DLY  EQU     7                                 ; set in CON ISR if MCE command is 'GO'.  USed to add
 delay to first returned data packet
79     
80                         PACKET_CHOKE
81        000008                     EQU     8                                 ;  don't let any packets from MCE through to host....
82        000009           HST_NFYD  EQU     9                                 ; set after host notified (NFY message) of packet (st
ays set until after HST reply)
83        00000A           SB_SPARE1 EQU     10
84        00000B           SB_SPARE2 EQU     11
85     
86     
87                         APPLICATION_RUNNING
88        00000C                     EQU     12                                ; can be set by an application to indicate its still 
running
89                                                                             ; e.g. set by diagnostic application
90                                                                             ; indicates in a 'self_test_mode'
91                                                                             ; subsequnet GO commands (for MCE) will be handelled 
internally.
92                                                                             ; disable with PCI STOP_APPLICATION command.
93     
94                         INTERNAL_GO
95        00000D                     EQU     13                                ; GO command received while diagnostic application st
ill running
96                                                                             ; tests DMA bursts as bus master
97     
99                         PCIDMA_RESTART
100       00000E                     EQU     14
101                        PCIDMA_RESUME
102       00000F                     EQU     15
103                        PCIDMA_RETRY
104       000010                     EQU     16
105    
106       000011           QT_ENABLED EQU    17
107       000012           QT_FLUSH  EQU     18
108                        QT_NEW_GRANT
109       000013                     EQU     19
110    
111                        ; HST timeout recovery....
112    
113       000200           MAX_DUMP  EQU     512                               ; if HST timeout.. max number that could be in FIFO i
s 511..
114       001000           DUMP_BUFF EQU     $1000                             ; store in Y memory above normal data buffer: in off-
chip RAM
115    
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  header.asm  Page 3



116    
117    
118                        ; Various addressing control registers
119       FFFFFB           BCR       EQU     $FFFFFB                           ; Bus Control Register
120       FFFFFA           DCR       EQU     $FFFFFA                           ; DRAM Control Register
121       FFFFF9           AAR0      EQU     $FFFFF9                           ; Address Attribute Register, channel 0
122       FFFFF8           AAR1      EQU     $FFFFF8                           ; Address Attribute Register, channel 1
123       FFFFF7           AAR2      EQU     $FFFFF7                           ; Address Attribute Register, channel 2
124       FFFFF6           AAR3      EQU     $FFFFF6                           ; Address Attribute Register, channel 3
125       FFFFFD           PCTL      EQU     $FFFFFD                           ; PLL control register
126       FFFFFE           IPRP      EQU     $FFFFFE                           ; Interrupt Priority register - Peripheral
127       FFFFFF           IPRC      EQU     $FFFFFF                           ; Interrupt Priority register - Core
128    
129                        ; PCI control register
130       FFFFCD           DTXS      EQU     $FFFFCD                           ; DSP Slave transmit data FIFO
131       FFFFCC           DTXM      EQU     $FFFFCC                           ; DSP Master transmit data FIFO
132       FFFFCB           DRXR      EQU     $FFFFCB                           ; DSP Receive data FIFO
133       FFFFCA           DPSR      EQU     $FFFFCA                           ; DSP PCI Status Register
134       FFFFC9           DSR       EQU     $FFFFC9                           ; DSP Status Register
135       FFFFC8           DPAR      EQU     $FFFFC8                           ; DSP PCI Address Register
136       FFFFC7           DPMC      EQU     $FFFFC7                           ; DSP PCI Master Control Register
137       FFFFC6           DPCR      EQU     $FFFFC6                           ; DSP PCI Control Register
138       FFFFC5           DCTR      EQU     $FFFFC5                           ; DSP Control Register
139    
140                        ; Port E is the Synchronous Communications Interface (SCI) port
141       FFFF9F           PCRE      EQU     $FFFF9F                           ; Port Control Register
142       FFFF9E           PRRE      EQU     $FFFF9E                           ; Port Direction Register
143       FFFF9D           PDRE      EQU     $FFFF9D                           ; Port Data Register
144    
145                        ; Various PCI register bit equates
146       000001           STRQ      EQU     1                                 ; Slave transmit data request (DSR)
147       000002           SRRQ      EQU     2                                 ; Slave receive data request (DSR)
148       000017           HACT      EQU     23                                ; Host active, low true (DSR)
149       000001           MTRQ      EQU     1                                 ; Set whem master transmitter is not full (DPSR)
150       000004           MARQ      EQU     4                                 ; Master address request (DPSR)
151       000002           MRRQ      EQU     2                                 ; Master Receive Request (DPSR)
152       00000A           TRTY      EQU     10                                ; PCI Target Retry (DPSR)
153    
154       000005           APER      EQU     5                                 ; Address parity error
155       000006           DPER      EQU     6                                 ; Data parity error
156       000007           MAB       EQU     7                                 ; Master Abort
157       000008           TAB       EQU     8                                 ; Target Abort
158       000009           TDIS      EQU     9                                 ; Target Disconnect
159       00000B           TO        EQU     11                                ; Timeout
160       00000E           MDT       EQU     14                                ; Master Data Transfer complete
161       000002           SCLK      EQU     2                                 ; SCLK = transmitter special code
162    
163                        ; bits in DPMC
164    
165       000017           FC1       EQU     23
166       000016           FC0       EQU     22
167    
168    
169                        ; DMA register definitions
170       FFFFEF           DSR0      EQU     $FFFFEF                           ; Source address register
171       FFFFEE           DDR0      EQU     $FFFFEE                           ; Destination address register
172       FFFFED           DCO0      EQU     $FFFFED                           ; Counter register
173       FFFFEC           DCR0      EQU     $FFFFEC                           ; Control register
174    
175                        ; The DCTR host flags are written by the DSP and read by PCI host
176       000003           DCTR_HF3  EQU     3                                 ; used as a semiphore for INTA handshaking
177       000004           DCTR_HF4  EQU     4                                 ;
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  header.asm  Page 4



178       000005           DCTR_HF5  EQU     5                                 ;
179       000006           INTA      EQU     6                                 ; Request PCI interrupt
180    
181                        ; The DSR host flags are written by the PCI host and read by the DSP
182       000004           DSR_BUF0  EQU     4                                 ; PCI host sets this when copying buffer 0
183       000005           DSR_BUF1  EQU     5                                 ; PCI host sets this when copying buffer 1
184    
185                        ; DPCR bit definitions
186       00000E           CLRT      EQU     14                                ; Clear transmitter
187       000012           MACE      EQU     18                                ; Master access counter enable
188       000015           IAE       EQU     21                                ; Insert Address Enable
189    
190                        ; Addresses of ESSI port
191       FFFFBC           TX00      EQU     $FFFFBC                           ; Transmit Data Register 0
192       FFFFB7           SSISR0    EQU     $FFFFB7                           ; Status Register
193       FFFFB6           CRB0      EQU     $FFFFB6                           ; Control Register B
194       FFFFB5           CRA0      EQU     $FFFFB5                           ; Control Register A
195    
196                        ; SSI Control Register A Bit Flags
197       000006           TDE       EQU     6                                 ; Set when transmitter data register is empty
198    
199                        ; Miscellaneous addresses
200       FFFFFF           RDFIFO    EQU     $FFFFFF                           ; Read the FIFO for incoming fiber optic data
201    
202                        ; Timer registers
203       FFFF8F           TCSR0     EQU     $FFFF8F                           ; Control and status register
204       FFFF8E           TLR0      EQU     $FFFF8E                           ; Load register
205       FFFF8D           TCPR0     EQU     $FFFF8D                           ; Compare register
206       FFFF8C           TCR0      EQU     $FFFF8C                           ; Count register
207       FFFF8B           TCSR1     EQU     $FFFF8B                           ; Control and status register
208       FFFF8A           TLR1      EQU     $FFFF8A                           ; Load register
209       FFFF89           TCPR1     EQU     $FFFF89                           ; Compare register
210       FFFF88           TCR1      EQU     $FFFF88                           ; Count register
211       FFFF87           TCSR2     EQU     $FFFF87                           ; Control and status register
212       FFFF86           TLR2      EQU     $FFFF86                           ; Load register
213       FFFF85           TCPR2     EQU     $FFFF85                           ; Compare register
214       FFFF84           TCR2      EQU     $FFFF84                           ; Count register
215    
216                        ;***************************************************************
217                        ; Phase Locked Loop initialization
218       050003           PLL_INIT  EQU     $050003                           ; PLL = 25 MHz x 4 = 100 MHz
219                        ;****************************************************************
220    
221                        ; Port C is Enhanced Synchronous Serial Port 0
222       FFFFBF           PCRC      EQU     $FFFFBF                           ; Port C Control Register
223       FFFFBE           PRRC      EQU     $FFFFBE                           ; Port C Data direction Register
224       FFFFBD           PDRC      EQU     $FFFFBD                           ; Port C GPIO Data Register
225    
226                        ; Port D is Enhanced Synchronous Serial Port 1
227       FFFFAF           PCRD      EQU     $FFFFAF                           ; Port D Control Register
228       FFFFAE           PRRD      EQU     $FFFFAE                           ; Port D Data direction Register
229       FFFFAD           PDRD      EQU     $FFFFAD                           ; Port D GPIO Data Register
230    
231                        ; Bit number definitions of GPIO pins on Port C
232       000002           ROM_FIFO  EQU     2                                 ; Select ROM or FIFO accesses for AA1
233       000004           AUX1      EQU     4                                 ; enable/disable byte swapping
234    
235                        ; Bit number definitions of GPIO pins on Port D
236       000000           EF        EQU     0                                 ; FIFO Empty flag, low true
237       000001           HF        EQU     1                                 ; FIFO half full flag, low true
238       000002           RS        EQU     2                                 ; FIFO reset signal, low true
239       000003           FSYNC     EQU     3                                 ; High during image transmission
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  header.asm  Page 5



240       000005           WRFIFO    EQU     5                                 ; Low true if FIFO is being written to
241    
242    
243                        ; Errors - self test application
244    
245       000000           Y_MEM_ER  EQU     0                                 ; y memory corrupted
246       000001           X_MEM_ER  EQU     1                                 ; x memory corrupted
247       000002           P_MEM_ER  EQU     2                                 ; p memory corrupted
248       000003           FO_EMPTY  EQU     3                                 ; no transmitted data in FIFO
249    
250       000004           FO_OVER   EQU     4                                 ; too much data received
251       000005           FO_UNDER  EQU     5                                 ; not enough data receiv
252       000006           FO_RX_ER  EQU     6                                 ; received data in FIFO incorrect.
253       000007           DEBUG     EQU     7                                 ; debug bit
254    
255    
257       000000           TE        EQU     0
258       000001           TOIE      EQU     1
259       000002           TCIE      EQU     2
260       000014           TOF       EQU     20
261       000015           TCF       EQU     21
262                                  INCLUDE 'init.asm'
263                              COMMENT *
264    
265                        This is the code which is executed first after power-up etc.
266                        It sets all the internal registers to their operating values,
267                        sets up the ISR vectors and inialises the hardware etc.
268    
269                        Project:     SCUBA 2
270                        Author:      DAVID ATKINSON
271                        Target:      250MHz SDSU PCI card - DSP56301
272                        Controller:  For use with SCUBA 2 Multichannel Electronics
273    
274                        Assembler directives:
275                                ROM=EEPROM => EEPROM CODE
276                                ROM=ONCE => ONCE CODE
277    
278                                *
279                                  PAGE    132                               ; Printronix page width - 132 columns
280                                  OPT     CEX                               ; print DC evaluations
281    
**** 282 [init.asm 20]:  INCLUDE PCI_initialisation.asm HERE  
282                                  MSG     ' INCLUDE PCI_initialisation.asm HERE  '
283    
284                        ; The EEPROM boot code expects first to read 3 bytes specifying the number of
285                        ; program words, then 3 bytes specifying the address to start loading the
286                        ; program words and then 3 bytes for each program word to be loaded.
287                        ; The program words will be condensed into 24 bit words and stored in contiguous
288                        ; PRAM memory starting at the specified starting address. Program execution
289                        ; starts from the same address where loading started.
290    
291                        ; Special address for two words for the DSP to bootstrap code from the EEPROM
292                                  IF      @SCP("ROM","ROM")                 ; Boot from ROM on power-on
293       P:000000 P:000000                   ORG     P:0,P:0
294  d    P:000000 P:000000 000A23            DC      END_ADR-INIT-2                    ; Number of boot words
295  d    P:000001 P:000001 000000            DC      INIT                              ; Starting address
296       P:000000 P:000002                   ORG     P:0,P:2
297       P:000000 P:000002 0C0030  INIT      JMP     <INIT_PCI                         ; Configure PCI port
298       P:000001 P:000003 000000            NOP
299                                           ENDIF
300    
301    
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  init.asm  Page 6



302                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
303                                 ; command converter
304                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
308                                           ENDIF
309    
310                                 ; Vectored interrupt table, addresses at the beginning are reserved
311  d    P:000002 P:000004 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0       ; $02-$0f Reserved
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
312  d    P:000010 P:000012 000000            DC      0,0                               ; $10-$13 Reserved
     d                      000000
313    
314                                 ; FIFO HF* flag interrupt vector is here at $12 - this is connected to the
315                                 ; IRQB* interrupt line so its ISR vector must be here
316  d    P:000012 P:000014 000000            DC      0,0                               ; $was ld scatter routine ...HF*
     d                      000000
317    
318                                 ; a software reset button on the font panel of the card is connected to the IRQC*
319                                 ; line which if pressed causes the DSP to jump to an ISR which causes the program
320                                 ; counter to the beginning of the program INIT and sets the stack pointer to TOP.
321       P:000014 P:000016 0BF080            JSR     CLEAN_UP_PCI                      ; $14 - Software reset switch
                            00022A
322    
323  d    P:000016 P:000018 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Reserved interrupts
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
324  d    P:000022 P:000024 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0
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
325    
326                                 ; Now we're at P:$30, where some unused vector addresses are located
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  init.asm  Page 7



327                                 ; This is ROM only code that is only executed once on power-up when the
328                                 ; ROM code is downloaded. It is skipped over on OnCE downloads.
329    
330                                 ; A few seconds after power up on the Host, it interrogates the PCI bus to find
331                                 ; out what boards are installed and configures this PCI board. The EEPROM booting
332                                 ; procedure ends with program execution  starting at P:$0 where the EEPROM has
333                                 ; inserted a JMP INIT_PCI instruction. This routine sets the PLL paramter and
334                                 ; does a self configuration and software reset of the PCI controller in the DSP.
335                                 ; After configuring the PCI controller the DSP program overwrites the instruction
336                                 ; at P:$0 with a new JMP START to skip over the INIT_PCI routine. The program at
337                                 ; START address begins configuring the DSP and processing commands.
338                                 ; Similarly the ONCE option places a JMP START at P:$0 to skip over the
339                                 ; INIT_PCI routine. If this routine where executed after the host computer had booted
340                                 ; it would cause it to crash since the host computer would overwrite the
341                                 ; configuration space with its own values and doesn't tolerate foreign values.
342    
343                                 ; Initialize the PLL - phase locked loop
344                                 INIT_PCI
345       P:000030 P:000032 08F4BD            MOVEP             #PLL_INIT,X:PCTL        ; Initialize PLL
                            050003
346       P:000032 P:000034 000000            NOP
347    
348                                 ; Program the PCI self-configuration registers
349       P:000033 P:000035 240000            MOVE              #0,X0
350       P:000034 P:000036 08F485            MOVEP             #$500000,X:DCTR         ; Set self-configuration mode
                            500000
351       P:000036 P:000038 0604A0            REP     #4
352       P:000037 P:000039 08C408            MOVEP             X0,X:DPAR               ; Dummy writes to configuration space
353       P:000038 P:00003A 08F487            MOVEP             #>$0000,X:DPMC          ; Subsystem ID
                            000000
354       P:00003A P:00003C 08F488            MOVEP             #>$0000,X:DPAR          ; Subsystem Vendor ID
                            000000
355    
356                                 ; PCI Personal reset
357       P:00003C P:00003E 08C405            MOVEP             X0,X:DCTR               ; Personal software reset
358       P:00003D P:00003F 000000            NOP
359       P:00003E P:000040 000000            NOP
360       P:00003F P:000041 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00003F
361       P:000041 P:000043 07F084            MOVE              P:(*+3),X0              ; Trick to write "JMP <START" to P:0
                            000044
362       P:000043 P:000045 070004            MOVE              X0,P:(0)
363       P:000044 P:000046 0C0100            JMP     <START
364    
365  d    P:000045 P:000047 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
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
366  d    P:000051 P:000053 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  init.asm  Page 8



     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
     d                      000000
367  d    P:00005D P:00005F 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; $60-$71 Reserved PCI
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
368    
369                                 ;**************************************************************************
370                                 ; Check for program space overwriting of ISR starting at P:$72
371                                           IF      @CVS(N,*)>$71
373                                           ENDIF
374    
375                                 ;       ORG     P:$72,P:$72
376       P:000072 P:000074                   ORG     P:$72,P:$74
377    
378                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
379                                 ; command converter
380                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
382                                           ENDIF
383    
384    
385                                 ;**************************************************************************
386    
387                                 ; Three non-maskable fast interrupt service routines for clearing PCI interrupts
388                                 ; The Host will use these to clear the INTA* after it has serviced the interrupt
389                                 ; which had been generated by the PCI board.
390    
391       P:000072 P:000074 0A8506            BCLR    #INTA,X:DCTR                      ; $72/3 - Clear PCI interrupt
392       P:000073 P:000075 000000            NOP
393    
394       P:000074 P:000076 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; clear interrupt flag
395       P:000075 P:000077 000000            NOP                                       ; needs to be fast addressing <
396    
397       P:000076 P:000078 0A0022            BSET    #FATAL_ERROR,X:<STATUS            ; $76/7 - driver PCI_MESSAGE_TO_HOST error
398       P:000077 P:000079 000000            NOP
399    
400                                 ; Interrupt locations for 7 available commands on PCI board
401                                 ; Each JSR takes up 2 locations in the table
402       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            00039D
403       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            000236
404       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            00035D
405       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            000375
406                                 ; software reset is the same as cleaning up the PCI - use same routine
407                                 ; when HOST does a RESET then this routine is run
408       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            000325
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  init.asm  Page 9



409       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            0002B3
410       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            000301
411       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            000275
412    
413                                 ; QT - set command
414       P:000088 P:00008A 0BF080            JSR     QUIET_TRANSFER_SET                ; $88
                            000816
415       P:00008A P:00008C 0BF080            JSR     SYSTEM_RESET                      ; $8A
                            000818
416    
417                                 ; ***********************************************************************
418                                 ; For now have boot code starting from P:$100
419                                 ; just to make debugging tidier etc.
420    
421       P:000100 P:000102                   ORG     P:$100,P:$102
422    
423                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
424                                 ; command converter
425                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
427                                           ENDIF
428                                 ; ***********************************************************************
429    
430    
431    
432                                 ; ******************************************************************
433                                 ;
434                                 ;       AA0 = RDFIFO* of incoming fiber optic data
435                                 ;       AA1 = EEPROM access
436                                 ;       AA2 = DRAM access
437                                 ;       AA3 = output to parallel data connector, for a video pixel clock
438                                 ;       $FFxxxx = Write to fiber optic transmitter
439                                 ;
440                                 ; ******************************************************************
441    
442    
443       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
444       P:000102 P:000104 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
445       P:000103 P:000105 0A8515            BCLR    #21,X:DCTR
446       P:000104 P:000106 0A8516            BCLR    #22,X:DCTR
447       P:000105 P:000107 000000            NOP
448       P:000106 P:000108 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
449       P:000107 P:000109 000000            NOP
450       P:000108 P:00010A 000000            NOP                                       ; End of PCI programming
451    
452    
453                                 ; Set operation mode register OMR to normal expanded
454       P:000109 P:00010B 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
455       P:00010A P:00010C 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
456    
457                                 ; Program the serial port ESSI0 = Port C for serial transmission to
458                                 ;   the timing board
459       P:00010B P:00010D 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
460                                 ;**********************************************************************
461       P:00010D P:00010F 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
462                                                                                     ; DC0-CD4 = 0 for non-network operation
463                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  init.asm  Page 10



464                                                                                     ; SSC1 = 0 for SC1 not used
465                                 ;************************************************************************
466       P:00010F P:000111 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
                            010120
467                                                                                     ; SHFD = 0 for MSB shifted first
468                                                                                     ; CKP = 0 for rising clock edge transitions
469                                                                                     ; TE0 = 1 to enable transmitter #0
470                                                                                     ; MOD = 0 for normal, non-networked mode
471                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
472       P:000111 P:000113 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
473                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
474                                 ;********************************************************************************
475       P:000113 P:000115 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
476       P:000115 P:000117 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
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
492       P:000117 P:000119 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
493       P:000119 P:00011B 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            00001C
494       P:00011B P:00011D 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
495       P:00011D P:00011F 060AA0            REP     #10
496       P:00011E P:000120 000000            NOP
497       P:00011F P:000121 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
498                                                                                     ; was %011100
499    
500                                 ; Program the SCI port to benign values
501       P:000121 P:000123 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
502       P:000123 P:000125 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
503       P:000125 P:000127 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
504                                 ;       PE0 = RXD
505                                 ;       PE1 = TXD
506                                 ;       PE2 = SCLK
507    
508                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
509       P:000127 P:000129 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
510       P:000129 P:00012B 07F40B            MOVEP             #$2800,X:TCSR1
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  init.asm  Page 11



                            002800
511       P:00012B P:00012D 07F407            MOVEP             #$2800,X:TCSR2
                            002800
512    
513    
514                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
515       P:00012D P:00012F 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
516       P:00012F P:000131 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
e
                            008929
517       P:000131 P:000133 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
                            000122
518    
519    
520                                 ; Program the DRAM memory access and addressing
521       P:000133 P:000135 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
                            020022
522       P:000135 P:000137 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
523    
524    
525                                 ; Clear all PCI error conditions
526       P:000137 P:000139 084E0A            MOVEP             X:DPSR,A
527       P:000138 P:00013A 0140C2            OR      #$1FE,A
                            0001FE
528       P:00013A P:00013C 000000            NOP
529       P:00013B P:00013D 08CE0A            MOVEP             A,X:DPSR
530    
531                                 ;--------------------------------------------------------------------
532                                 ; Enable one interrupt only: software reset switch
533       P:00013C P:00013E 08F4BF            MOVEP             #$0001C0,X:IPRC         ; IRQB priority = 1 (FIFO half full HF*)
                            0001C0
534                                                                                     ; IRQC priority = 2 (reset switch)
535       P:00013E P:000140 05F439            MOVE              #$200,SR                ; Mask set up for reset switch only
                            000200
536    
537    
538                                 ;--------------------------------------------------------------------------
539                                 ; Initialize the fiber optic serial transmitter to zero
540       P:000140 P:000142 01B786            JCLR    #TDE,X:SSISR0,*
                            000140
541       P:000142 P:000144 07F43C            MOVEP             #$000000,X:TX00
                            000000
542    
543                                 ;--------------------------------------------------------------------
544    
545                                 ; clear DTXM - PCI master transmitter
546       P:000144 P:000146 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
547       P:000145 P:000147 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000145
548    
549                                 ;----------------------------------------------------------------------
550                                 ; clear DRXR - PCI receiver
551    
552       P:000147 P:000149 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014C
553       P:000149 P:00014B 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
554       P:00014A P:00014C 000000            NOP
555       P:00014B P:00014D 0C0147            JMP     <CLR0
556                                 CLR1
557    
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  init.asm  Page 12



558                                 ;-----------------------------------------------------------------------------
559                                 ; copy parameter table from P memory into X memory
560    
561                                 ; but not word_count and num_dumped - don't want these reset by fatal error....
562                                 ; they will be reset by new packet or pci_reset ISR
563    
564    
565       P:00014C P:00014E 46F000            MOVE              X:WORD_COUNT,Y0         ; store packet word count
                            000006
566       P:00014E P:000150 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000007
567       P:000150 P:000152 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
                            000001
568    
569                                 ; Move the table of constants from P: space to X: space
570       P:000152 P:000154 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            000575
571       P:000154 P:000156 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
572       P:000155 P:000157 066680            DO      #VAR_TBL_LENGTH,X_WRITE
                            000158
573       P:000157 P:000159 07D984            MOVE              P:(R1)+,X0
574       P:000158 P:00015A 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
575                                 X_WRITE
576    
577    
578       P:000159 P:00015B 467000            MOVE              Y0,X:WORD_COUNT         ; restore packet word count
                            000006
579       P:00015B P:00015D 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
                            000007
580       P:00015D P:00015F 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000001
581    
582                                 ;-------------------------------------------------------------------------------
583                                 ; initialise some bits in STATUS
584    
585       P:00015F P:000161 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS     ; clear application loaded flag
586       P:000160 P:000162 0A000C            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appliaction running flag
587                                                                                     ; (e.g. not running diagnostic application
588                                                                                     ;      in self_test_mode)
589    
590       P:000161 P:000163 0A0002            BCLR    #FATAL_ERROR,X:<STATUS            ; initialise fatal error flag.
591       P:000162 P:000164 0A0028            BSET    #PACKET_CHOKE,X:<STATUS           ; enable MCE packet choke
592                                                                                     ; HOST not informed of anything from MCE unt
il
593                                                                                     ; comms are opened by host with first CON co
mmand
594    
595       P:000163 P:000165 0A0006            BCLR    #PREAMBLE_ERROR,X:<STATUS         ; flag to let host know premable error
596    
597                                 ;------------------------------------------------------------------------------
598                                 ; disable FIFO HF* intererupt...not used anymore.
599    
600       P:000164 P:000166 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable FIFO HF* interrupt
                            0001C0
601       P:000166 P:000168 05F439            MOVEC             #$200,SR                ; Mask level 1 interrupts
                            000200
602    
603                                 ;----------------------------------------------------------------------------
604                                 ; Disable Byte swapin - enabled after first command to MCE.
605                                 ; i.e after first 'CON'
606    
607       P:000168 P:00016A 0A0005            BCLR    #BYTE_SWAP,X:<STATUS              ; flag to let host know byte swapping off
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  init.asm  Page 13



608       P:000169 P:00016B 013D04            BCLR    #AUX1,X:PDRC                      ; enable disable
609    
610                                 ;----------------------------------------------------------------------------
611                                 ; Initialize PCI controller again, after booting, to make sure it sticks
612       P:00016A P:00016C 0A8514            BCLR    #20,X:DCTR                        ; Terminate and reset mode
613       P:00016B P:00016D 000000            NOP
614       P:00016C P:00016E 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00016C
615       P:00016E P:000170 000000            NOP
616       P:00016F P:000171 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
617       P:000170 P:000172 000000            NOP
618       P:000171 P:000173 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000171
619                                 ;-----------------------------------------------------------------------------
620                                 ; Here endth the initialisation code run after power up.
621                                 ; ----------------------------------------------------------------------------
622    
623    
624    
625    
626                                           INCLUDE 'main.asm'
627                                  COMMENT *
628    
629                                 This is the main section of the pci card code.
630    
631                                 Project:     SCUBA 2
632                                 Author:      DAVID ATKINSON
633                                 Target:      250MHz SDSU PCI card - DSP56301
634                                 Controller:  For use with SCUBA 2 Multichannel Electronics
635    
636                                 Modified:    MATTHEW HASSELFIELD
637    
638                                 Version:     Release Version U (1.4)
639    
640    
641                                 Assembler directives:
642                                         ROM=EEPROM => EEPROM CODE
643                                         ROM=ONCE => ONCE CODE
644    
645                                         *
646                                           PAGE    132                               ; Printronix page width - 132 columns
647                                           OPT     CEX                               ; print DC evaluations
648    
**** 649 [main.asm 23]:  INCLUDE PCI_main.asm HERE  
649                                           MSG     ' INCLUDE PCI_main.asm HERE  '
650    
651                                 ; --------------------------------------------------------------------------
652                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
653                                 ; --------------------------------------------------------------------------
654    
655                                 ; initialse buffer pointers
656                                 PACKET_IN
657    
658                                 ; R1 used as pointer for data written to y:memory            FO --> (Y)
659                                 ; R2 used as pointer for date in y mem to be writen to host  (Y) --> HOST
660    
661       P:000173 P:000175 310000            MOVE              #<IMAGE_BUFFER,R1       ; pointer for Fibre ---> Y mem
662       P:000174 P:000176 320000            MOVE              #<IMAGE_BUFFER,R2       ; pointer for Y mem ---> PCI BUS
663    
664                                 ; initialise some bits in status..
665       P:000175 P:000177 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
666       P:000176 P:000178 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 14



ied.
667       P:000177 P:000179 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS             ; clear Fiber Optic flag
668    
669                                 ; check some bits in status....
670       P:000178 P:00017A 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START      ; fatal error?  Go to initialisation.
                            000100
671       P:00017A P:00017C 0A00A0            JSET    #APPLICATION_LOADED,X:<STATUS,APPLICATION ; application loaded?  Execute in ap
pl space.
                            000800
672                                 ;               JSET    #INTERNAL_GO,X:<STATUS,APPLICATION             ; internal GO to process?
  PCI bus master write test.
673    
674    
675       P:00017C P:00017E 0BF080            JSR     MAIN_LOOP_HACK_FESTIVAL
                            000810
676    
677       P:00017E P:000180 0D0409  CHK_FIFO  JSR     <GET_FO_WRD                       ; see if there's a 16-bit word in Fibre FIFO
 from MCE
678    
679    
680       P:00017F P:000181 0A00A3            JSET    #FO_WRD_RCV,X:<STATUS,CHECK_WD    ; there is a word - check if it's preamble
                            000182
681       P:000181 P:000183 0C0173            JMP     <PACKET_IN                        ; else go back and repeat
682    
683                                 ; check that we preamble sequence
684    
685       P:000182 P:000184 0A00A8  CHECK_WD  JSET    #PACKET_CHOKE,X:<STATUS,PACKET_IN ; IF MCE Packet choke on - just keep clearin
g FIFO.
                            000173
686       P:000184 P:000186 441D00            MOVE              X0,X:<HEAD_W1_0         ;store received word
687       P:000185 P:000187 56F000            MOVE              X:PREAMB1,A
                            000038
688       P:000187 P:000189 200045            CMP     X0,A                              ; check it is correct
689       P:000188 P:00018A 0E219C            JNE     <PRE_ERROR                        ; if not go to start
690    
691    
692       P:000189 P:00018B 0D0411            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
693       P:00018A P:00018C 441C00            MOVE              X0,X:<HEAD_W1_1         ;store received word
694       P:00018B P:00018D 56F000            MOVE              X:PREAMB1,A
                            000038
695       P:00018D P:00018F 200045            CMP     X0,A                              ; check it is correct
696       P:00018E P:000190 0E219C            JNE     <PRE_ERROR                        ; if not go to start
697    
698    
699       P:00018F P:000191 0D0411            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
700       P:000190 P:000192 441F00            MOVE              X0,X:<HEAD_W2_0         ;store received word
701       P:000191 P:000193 56F000            MOVE              X:PREAMB2,A
                            000039
702       P:000193 P:000195 200045            CMP     X0,A                              ; check it is correct
703       P:000194 P:000196 0E219C            JNE     <PRE_ERROR                        ; if not go to start
704    
705       P:000195 P:000197 0D0411            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
706       P:000196 P:000198 441E00            MOVE              X0,X:<HEAD_W2_1         ;store received word
707       P:000197 P:000199 56F000            MOVE              X:PREAMB2,A
                            000039
708       P:000199 P:00019B 200045            CMP     X0,A                              ; check it is correct
709       P:00019A P:00019C 0E219C            JNE     <PRE_ERROR                        ; if not go to start
710       P:00019B P:00019D 0C01A8            JMP     <PACKET_INFO                      ; get packet info
711    
712    
713                                 PRE_ERROR
714       P:00019C P:00019E 0A0026            BSET    #PREAMBLE_ERROR,X:<STATUS         ; indicate a preamble error
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 15



715       P:00019D P:00019F 440200            MOVE              X0,X:<PRE_CORRUPT       ; store corrupted word
716    
717                                 ; preampble error so clear out both FIFOs using reset line
718                                 ; - protects against an odd number of bytes having been sent
719                                 ; (byte swapping on - so odd byte being would end up in
720                                 ; the FIFO without the empty flag)
721    
722       P:00019E P:0001A0 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
723       P:0001A0 P:0001A2 44F400            MOVE              #200000,X0
                            030D40
724       P:0001A2 P:0001A4 06C400            DO      X0,*+3
                            0001A4
725       P:0001A4 P:0001A6 000000            NOP
726       P:0001A5 P:0001A7 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
727    
728       P:0001A7 P:0001A9 0C0173            JMP     <PACKET_IN                        ; wait for next packet
729    
730    
731                                 PACKET_INFO                                         ; packet preamble valid
732    
733                                 ; Packet preamle is valid so....
734                                 ; now get next two 32bit words.  i.e. $20205250 $00000004, or $20204441 $xxxxxxxx
735                                 ; note that these are received little endian (and byte swapped)
736                                 ; i.e. for RP receive 50 52 20 20  04 00 00 00
737                                 ; but byte swapped on arrival
738                                 ; 5250
739                                 ; 2020
740                                 ; 0004
741                                 ; 0000
742    
743       P:0001A8 P:0001AA 0D0411            JSR     <WT_FIFO
744       P:0001A9 P:0001AB 442100            MOVE              X0,X:<HEAD_W3_0         ; RP or DA
745       P:0001AA P:0001AC 0D0411            JSR     <WT_FIFO
746       P:0001AB P:0001AD 442000            MOVE              X0,X:<HEAD_W3_1         ; $2020
747    
748       P:0001AC P:0001AE 0D0411            JSR     <WT_FIFO
749       P:0001AD P:0001AF 442300            MOVE              X0,X:<HEAD_W4_0         ; packet size lo
750       P:0001AE P:0001B0 0D0411            JSR     <WT_FIFO
751       P:0001AF P:0001B1 442200            MOVE              X0,X:<HEAD_W4_1         ; packet size hi
752    
753       P:0001B0 P:0001B2 44A100            MOVE              X:<HEAD_W3_0,X0         ; get data header word 3 (low 2 bytes)
754       P:0001B1 P:0001B3 56BB00            MOVE              X:<REPLY_WD,A           ; $5250
755       P:0001B2 P:0001B4 200045            CMP     X0,A                              ; is it a reply packet?
756       P:0001B3 P:0001B5 0AF0AA            JEQ     MCE_PACKET                        ; yes - go process it.
                            0001C7
757    
758       P:0001B5 P:0001B7 56BA00            MOVE              X:<DATA_WD,A            ; $4441
759       P:0001B6 P:0001B8 200045            CMP     X0,A                              ; is it a data packet?
760       P:0001B7 P:0001B9 0E2173            JNE     <PACKET_IN                        ; no?  Not a valid packet type.  Go back to 
start and resync to next preamble.
761    
762    
763                                 ; It's a data packet....
764                                 ; check if it's the first packet after the GO command has been issued...
765    
766       P:0001B8 P:0001BA 0A0087            JCLR    #DATA_DLY,X:STATUS,INC_FRAME_COUNT ; do we need to add a delay since first fra
me?
                            0001BA
767    
769    
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 16



770                                 ; ; yes first frame after GO reply packet so add a delay.
771                                 ; PACKET_DELAY
772                                 ;               MOVE    X:DATA_DLY_VAL,X0
773                                 ;               DO      X0,*+3                  ; 10ns x DATA_DLY_VAL
774                                 ;               NOP
775                                 ;                 NOP
776                                 ;                 BCLR  #DATA_DLY,X:STATUS      ; clear so delay isn't added next time.
777    
778    
779    
780    
781                                 INC_FRAME_COUNT                                     ; increment frame count
782       P:0001BA P:0001BC 200013            CLR     A
783       P:0001BB P:0001BD 508100            MOVE              X:<FRAME_COUNT,A0
784       P:0001BC P:0001BE 000008            INC     A
785       P:0001BD P:0001BF 000000            NOP
786       P:0001BE P:0001C0 500100            MOVE              A0,X:<FRAME_COUNT
787    
788       P:0001BF P:0001C1 0AF080            JMP     NEW_PACKET_HANDLER
                            000814
789       P:0001C1 P:0001C3 000000            NOP
790       P:0001C2 P:0001C4 000000            NOP
791       P:0001C3 P:0001C5 000000            NOP
792       P:0001C4 P:0001C6 000000            NOP
793       P:0001C5 P:0001C7 000000            NOP
794       P:0001C6 P:0001C8 000000            NOP
795    
796    
797                                 ; -------------------------------------------------------------------------------------------
798                                 ; ----------------------------------- IT'S A PACKET FROM MCE --------------------------------
799                                 ; -------------------------------------------------------------------------------------------
800                                 ; prepare notify to inform host that a packet has arrived.
801    
802                                 MCE_PACKET
803       P:0001C7 P:0001C9 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
804       P:0001C9 P:0001CB 440C00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
805    
806       P:0001CA P:0001CC 44A100            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
807       P:0001CB P:0001CD 440D00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
808    
809       P:0001CC P:0001CE 44A300            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
810       P:0001CD P:0001CF 440E00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
811    
812       P:0001CE P:0001D0 44A200            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
813       P:0001CF P:0001D1 440F00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sasge
814    
815       P:0001D0 P:0001D2 200013            CLR     A                                 ;
816       P:0001D1 P:0001D3 340000            MOVE              #0,R4                   ; initialise word count
817       P:0001D2 P:0001D4 560600            MOVE              A,X:<WORD_COUNT         ; initialise word count store (num of words 
written over bus/packet)
818       P:0001D3 P:0001D5 560700            MOVE              A,X:<NUM_DUMPED         ; initialise number dumped from FIFO (after 
HST TO)
819    
820    
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 17



821                                 ; ----------------------------------------------------------------------------------------------
------------
822                                 ; Determine how to break up packet to write to host
823    
824                                 ; Note that this SR uses accumulator B
825                                 ; Therefore execute before we get the bus address from host (which is stored in B)
826                                 ; i.e before we issue notify message ('NFY')
827    
828       P:0001D4 P:0001D6 0D03DA            JSR     <CALC_NO_BUFFS                    ; subroutine which calculates the number of 
512 (16bit) buffers
829                                                                                     ; number of left over 32 (16bit) blocks
830                                                                                     ; and number of left overs (16bit) words
831    
832                                 ;  note that a 512 (16-bit) buffer is transfered to the host as 4 x 64 x 32bit DMA burst
833                                 ;            a 32  (16-bit) block is transfered to the host as a    16 x 32bit DMA burst
834                                 ;            left over 16bit words are transfered to the host in pairs as 32bit words
835                                 ; ----------------------------------------------------------------------------------------------
---
836    
837    
838                                 ; notify the host that there is a packet.....
839    
840       P:0001D5 P:0001D7 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
841       P:0001D6 P:0001D8 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
842    
843                                 ; initialise read/write buffers
844                                 ; AND IMMEDIATELY BEGIN TO BUFFER FIBRE DATA TO Y MEMORY.
845    
846       P:0001D7 P:0001D9 310000            MOVE              #<IMAGE_BUFFER,R1       ; FO ---> Y mem
847       P:0001D8 P:0001DA 320000            MOVE              #<IMAGE_BUFFER,R2       ; Y mem ----->  PCI BUS
848    
849    
850                                 ; ----------------------------------------------------------------------------------------------
-----------
851                                 ; Write TOTAL_BUFFS * 512 buffers to host
852                                 ; ----------------------------------------------------------------------------------------------
------
853       P:0001D9 P:0001DB 063C00            DO      X:<TOTAL_BUFFS,READ_BUFFS_END     ; note that if TOTAL_BUFFS = 0 we jump to AL
L_BUFFS_END
                            0001E6
854    
855       P:0001DB P:0001DD 0A00A2  WAIT_BUFF JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO  ; if fatal error then dump fifo and reset (i
.e. if HST timeout)
                            000216
856       P:0001DD P:0001DF 01ADA1            JSET    #HF,X:PDRD,WAIT_BUFF              ; Wait for FIFO to be half full + 1
                            0001DB
857       P:0001DF P:0001E1 000000            NOP
858       P:0001E0 P:0001E2 000000            NOP
859       P:0001E1 P:0001E3 01ADA1            JSET    #HF,X:PDRD,WAIT_BUFF              ; Protection against metastability
                            0001DB
860    
861                                 ; Copy the image block as 512 x 16bit words to DSP Y: Memory using R1 as pointer
862       P:0001E3 P:0001E5 060082            DO      #512,L_BUFFER
                            0001E5
863       P:0001E5 P:0001E7 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
864                                 L_BUFFER
865       P:0001E6 P:0001E8 000000            NOP
866                                 READ_BUFFS_END                                      ; all buffers have been read (-->Y)
867    
868                                 ; ----------------------------------------------------------------------------------------------
-----------
869                                 ; Read NUM_LEFTOVER_BLOCKS * 32 blocks
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 18



870                                 ; ----------------------------------------------------------------------------------------------
------
871    
872                                 ; less than 512 pixels but if greater than 32 will then do bursts
873                                 ; of 16 x 32bit in length, if less than 32 then does single read writes
874    
875       P:0001E7 P:0001E9 063F00            DO      X:<NUM_LEFTOVER_BLOCKS,READ_BLOCKS ;note that if NUM_LEFOVERS_BLOCKS = 0 we ju
mp to LEFTOVER_BLOCKS
                            0001F4
876    
877       P:0001E9 P:0001EB 062080            DO      #32,S_BUFFER
                            0001F3
878       P:0001EB P:0001ED 0A00A2  WAIT_1    JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO  ; check for fatal error (i.e. after HST time
out)
                            000216
879       P:0001ED P:0001EF 01AD80            JCLR    #EF,X:PDRD,WAIT_1                 ; Wait for the pixel datum to be there
                            0001EB
880       P:0001EF P:0001F1 000000            NOP                                       ; Settling time
881       P:0001F0 P:0001F2 000000            NOP
882       P:0001F1 P:0001F3 01AD80            JCLR    #EF,X:PDRD,WAIT_1                 ; Protection against metastability
                            0001EB
883       P:0001F3 P:0001F5 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; save fibre word
884                                 S_BUFFER
885       P:0001F4 P:0001F6 000000            NOP
886                                 READ_BLOCKS
887    
888                                 ; ----------------------------------------------------------------------------------------------
-------
889                                 ; Single write left over words to host
890                                 ; ----------------------------------------------------------------------------------------------
------
891    
892                                 LEFT_OVERS
893       P:0001F5 P:0001F7 063D00            DO      X:<LEFT_TO_READ,LEFT_OVERS_READ   ; read in remaining words of data packet
                            0001FF
894                                                                                     ; if LEFT_TO_READ = 0 then will jump to LEFT
_OVERS_READ
895    
896       P:0001F7 P:0001F9 0A00A2  WAIT_2    JSET    #FATAL_ERROR,X:<STATUS,START      ; check for fatal error (i.e. after HST time
out)
                            000100
897       P:0001F9 P:0001FB 01AD80            JCLR    #EF,X:PDRD,WAIT_2                 ; Wait till something in FIFO flagged
                            0001F7
898       P:0001FB P:0001FD 000000            NOP
899       P:0001FC P:0001FE 000000            NOP
900       P:0001FD P:0001FF 01AD80            JCLR    #EF,X:PDRD,WAIT_2                 ; protect against metastability.....
                            0001F7
901       P:0001FF P:000201 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; save fibre word
902                                 LEFT_OVERS_READ
903    
904                                 ;---------------------------------------------------------------------------------------
905                                 ; ENTIRE PACKET NOW IN Y MEMORY
906                                 ;----------------------------------------------------------------------------------------
907                                 ; CHECK THAT HST COMMAND WAS ISSUED DURING DATA COLLECTION...
908    
909    
910       P:000200 P:000202 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; if fatal error - run initialisation code..
.
                            000100
911       P:000202 P:000204 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; wait for host to reply - which it does wit
h 'send_packet_to_host' ISR
                            000200
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 19



912    
913                                 ; we now have 32 bit address in accumulator B
914                                 ; from send-packet_to_host (HST COMMAND) which should of been issued during data collection.
915    
916                                 ; Write all data to host.
917    
919    
920       P:000204 P:000206 0BF080            JSR     PCI_BURST_NOW
                            0004A0
921    
922       P:000206 P:000208 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
923    
924                                 ; ----------------------------------------------------------------------------------------------
------------
925                                 ; reply to host's send_packet_to_host command
926    
927                                  HST_ACK_REP
928       P:000208 P:00020A 44F400            MOVE              #'REP',X0
                            524550
929       P:00020A P:00020C 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
930       P:00020B P:00020D 44F400            MOVE              #'HST',X0
                            485354
931       P:00020D P:00020F 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
932       P:00020E P:000210 44F400            MOVE              #'ACK',X0
                            41434B
933       P:000210 P:000212 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
934       P:000211 P:000213 44F400            MOVE              #'000',X0
                            303030
935       P:000213 P:000215 440F00            MOVE              X0,X:<DTXS_WD4          ; no error
936       P:000214 P:000216 0D0421            JSR     <PCI_MESSAGE_TO_HOST
937       P:000215 P:000217 0C0173            JMP     <PACKET_IN
938    
939                                 ;-----------------------------------------------------------------------------------------------
----
940                                 ; clear out the fifo after an HST timeout...
941                                 ;----------------------------------------------------------
942    
943       P:000216 P:000218 61F400  DUMP_FIFO MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
944       P:000218 P:00021A 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
945       P:00021A P:00021C 200013            CLR     A
946       P:00021B P:00021D 320000            MOVE              #0,R2                   ; use R2 as a dump count
947    
948       P:00021C P:00021E 01AD80  NEXT_DUMP JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000227
949       P:00021E P:000220 000000            NOP
950       P:00021F P:000221 000000            NOP
951       P:000220 P:000222 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000227
952    
953       P:000222 P:000224 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
954       P:000223 P:000225 205A00            MOVE              (R2)+                   ; inc dump count
955       P:000224 P:000226 224E00            MOVE              R2,A                    ;
956       P:000225 P:000227 200045            CMP     X0,A                              ; check we've not hit dump limit
957       P:000226 P:000228 0E221C            JNE     NEXT_DUMP                         ; not hit limit?
958    
959    
960       P:000227 P:000229 627000  FIFO_EMPTY MOVE             R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 20



                            000007
961       P:000229 P:00022B 0C0100            JMP     <START                            ; re-initialise
962    
963    
964    
965                                 ; ----------------------------------------------------------------------------------------------
--
966                                 ;                              END OF MAIN PACKET HANDLING CODE
967                                 ; ---------------------------------------------------------------------------------------------
968    
969    
970                                 ; -------------------------------------------------------------------------------------
971                                 ;
972                                 ;                              INTERRUPT SERVICE ROUTINES
973                                 ;
974                                 ; ---------------------------------------------------------------------------------------
975    
976                                 ;--------------------------------------------------------------------
977                                 CLEAN_UP_PCI
978                                 ;--------------------------------------------------------------------
979                                 ; Clean up the PCI board from wherever it was executing
980    
981       P:00022A P:00022C 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable HF* FIFO interrupt
                            0001C0
982       P:00022C P:00022E 05F439            MOVE              #$200,SR                ; mask for reset interrupts only
                            000200
983    
984       P:00022E P:000230 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
985       P:00022F P:000231 05F43D            MOVEC             #$000200,SSL            ; SR = zero except for interrupts
                            000200
986       P:000231 P:000233 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
987       P:000232 P:000234 05F43C            MOVEC             #START,SSH              ; Set PC to for full initialization
                            000100
988       P:000234 P:000236 000000            NOP
989       P:000235 P:000237 000004            RTI
990    
991                                 ; ---------------------------------------------------------------------------
992                                 READ_MEMORY
993                                 ;--------------------------------------------------------------------------
994                                 ; word 1 = command = 'RDM'
995                                 ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
996                                 ; word 3 = address in memory
997                                 ; word 4 = not used
998    
999       P:000236 P:000238 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
1000   
1001      P:000237 P:000239 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1002      P:000238 P:00023A 56F000            MOVE              X:DRXR_WD1,A            ; read command
                            000008
1003      P:00023A P:00023C 44F400            MOVE              #'RDM',X0
                            52444D
1004      P:00023C P:00023E 200045            CMP     X0,A                              ; ensure command is 'RDM'
1005      P:00023D P:00023F 0E2261            JNE     <READ_MEMORY_ERROR_CNE            ; error, command NOT HCVR address
1006      P:00023E P:000240 568900            MOVE              X:<DRXR_WD2,A           ; Memory type (X, Y, P)
1007      P:00023F P:000241 578A00            MOVE              X:<DRXR_WD3,B
1008      P:000240 P:000242 000000            NOP                                       ; pipeline restriction
1009      P:000241 P:000243 21B000            MOVE              B1,R0                   ; get address to write to
1010      P:000242 P:000244 0140C5            CMP     #$005F50,A                        ; $00'_P'
                            005F50
1011      P:000244 P:000246 0E2248            JNE     <RDX
1012      P:000245 P:000247 07E084            MOVE              P:(R0),X0               ; Read from P memory
1013      P:000246 P:000248 208E00            MOVE              X0,A                    ;
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 21



1014      P:000247 P:000249 0C0253            JMP     <FINISH_READ_MEMORY
1015                                RDX
1016      P:000248 P:00024A 0140C5            CMP     #$005F58,A                        ; $00'_X'
                            005F58
1017      P:00024A P:00024C 0E224E            JNE     <RDY
1018      P:00024B P:00024D 44E000            MOVE              X:(R0),X0               ; Read from P memory
1019      P:00024C P:00024E 208E00            MOVE              X0,A
1020      P:00024D P:00024F 0C0253            JMP     <FINISH_READ_MEMORY
1021                                RDY
1022      P:00024E P:000250 0140C5            CMP     #$005F59,A                        ; $00'_Y'
                            005F59
1023      P:000250 P:000252 0E2266            JNE     <READ_MEMORY_ERROR_MTE            ; not a valid memory type
1024      P:000251 P:000253 4CE000            MOVE                          Y:(R0),X0   ; Read from P memory
1025      P:000252 P:000254 208E00            MOVE              X0,A
1026   
1027                                ; when completed successfully then PCI needs to reply to Host with
1028                                ; word1 = reply/data = reply
1029                                FINISH_READ_MEMORY
1030      P:000253 P:000255 44F400            MOVE              #'REP',X0
                            524550
1031      P:000255 P:000257 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1032      P:000256 P:000258 44F400            MOVE              #'RDM',X0
                            52444D
1033      P:000258 P:00025A 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1034      P:000259 P:00025B 44F400            MOVE              #'ACK',X0
                            41434B
1035      P:00025B P:00025D 440E00            MOVE              X0,X:<DTXS_WD3          ;  im command
1036      P:00025C P:00025E 21C400            MOVE              A,X0
1037      P:00025D P:00025F 440F00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error
1038      P:00025E P:000260 0D0469            JSR     <RESTORE_REGISTERS                ; restore registers
1039      P:00025F P:000261 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1040      P:000260 P:000262 000004            RTI
1041   
1042                                READ_MEMORY_ERROR_CNE
1043      P:000261 P:000263 44F400            MOVE              #'CNE',X0
                            434E45
1044      P:000263 P:000265 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1045      P:000264 P:000266 0AF080            JMP     READ_MEMORY_ERROR                 ; fill in rest of reply
                            000269
1046                                READ_MEMORY_ERROR_MTE
1047      P:000266 P:000268 44F400            MOVE              #'MTE',X0
                            4D5445
1048      P:000268 P:00026A 440F00            MOVE              X0,X:<DTXS_WD4          ;  Memory Type Error - not a valid memory ty
pe
1049   
1050                                READ_MEMORY_ERROR
1051      P:000269 P:00026B 44F400            MOVE              #'REP',X0
                            524550
1052      P:00026B P:00026D 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1053      P:00026C P:00026E 44F400            MOVE              #'RDM',X0
                            52444D
1054      P:00026E P:000270 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1055      P:00026F P:000271 44F400            MOVE              #'ERR',X0
                            455252
1056      P:000271 P:000273 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor.
1057      P:000272 P:000274 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1058      P:000273 P:000275 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1059      P:000274 P:000276 000004            RTI
1060   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 22



1061                                ;-----------------------------------------------------------------------------
1062                                RESET_CONTROLLER
1063                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1064                                ;---------------------------------------------------------------------------
1065                                ; word 1 = command = 'RCO'
1066                                ; word 2 = not used but read
1067                                ; word 3 = not used but read
1068                                ; word 4 = not used but read
1069   
1070      P:000275 P:000277 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
1071      P:000276 P:000278 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1072      P:000277 P:000279 568800            MOVE              X:<DRXR_WD1,A           ; read command
1073      P:000278 P:00027A 44F400            MOVE              #'RCO',X0
                            52434F
1074      P:00027A P:00027C 200045            CMP     X0,A                              ; ensure command is 'RCO'
1075      P:00027B P:00027D 0E22A0            JNE     <RCO_ERROR                        ; error, command NOT HCVR address
1076   
1077                                ; if we get here then everything is fine and we can send reset to controller
1078   
1079                                ; 250MHZ CODE....
1080   
1081      P:00027C P:00027E 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1082      P:00027D P:00027F 000000            NOP
1083      P:00027E P:000280 000000            NOP
1084      P:00027F P:000281 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1085      P:000281 P:000283 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1086      P:000283 P:000285 446000            MOVE              X0,X:(R0)
1087      P:000284 P:000286 0606A0            REP     #6                                ; Wait for transmission to complete
1088      P:000285 P:000287 000000            NOP
1089      P:000286 P:000288 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1090   
1091                                ; Wait for a bit for MCE to be reset.......
1092      P:000287 P:000289 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1093      P:000289 P:00028B 06C400            DO      X0,L_DELAY
                            00028F
1094      P:00028B P:00028D 06E883            DO      #1000,L_RDFIFO
                            00028E
1095      P:00028D P:00028F 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1096      P:00028E P:000290 000000            NOP                                       ;   receiver empty
1097                                L_RDFIFO
1098      P:00028F P:000291 000000            NOP
1099                                L_DELAY
1100      P:000290 P:000292 000000            NOP
1101   
1102                                ; when completed successfully then PCI needs to reply to Host with
1103                                ; word1 = reply/data = reply
1104                                FINISH_RCO
1105      P:000291 P:000293 44F400            MOVE              #'REP',X0
                            524550
1106      P:000293 P:000295 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1107      P:000294 P:000296 44F400            MOVE              #'RCO',X0
                            52434F
1108      P:000296 P:000298 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1109      P:000297 P:000299 44F400            MOVE              #'ACK',X0
                            41434B
1110      P:000299 P:00029B 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1111      P:00029A P:00029C 44F400            MOVE              #'000',X0
                            303030
1112      P:00029C P:00029E 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 23



1113      P:00029D P:00029F 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1114      P:00029E P:0002A0 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1115      P:00029F P:0002A1 000004            RTI                                       ; return from ISR
1116   
1117                                ; when there is a failure in the host to PCI command then the PCI
1118                                ; needs still to reply to Host but with an error message
1119                                RCO_ERROR
1120      P:0002A0 P:0002A2 44F400            MOVE              #'REP',X0
                            524550
1121      P:0002A2 P:0002A4 447000            MOVE              X0,X:DTXS_WD1           ; REPly
                            00000C
1122      P:0002A4 P:0002A6 44F400            MOVE              #'RCO',X0
                            52434F
1123      P:0002A6 P:0002A8 447000            MOVE              X0,X:DTXS_WD2           ; echo command sent
                            00000D
1124      P:0002A8 P:0002AA 44F400            MOVE              #'ERR',X0
                            455252
1125      P:0002AA P:0002AC 447000            MOVE              X0,X:DTXS_WD3           ; ERRor im command
                            00000E
1126      P:0002AC P:0002AE 44F400            MOVE              #'CNE',X0
                            434E45
1127      P:0002AE P:0002B0 447000            MOVE              X0,X:DTXS_WD4           ; Command Name Error - command name in DRXR 
does not match
                            00000F
1128      P:0002B0 P:0002B2 0D0469            JSR     <RESTORE_REGISTERS                ; restore wroking registers
1129      P:0002B1 P:0002B3 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1130      P:0002B2 P:0002B4 000004            RTI                                       ; return from ISR
1131   
1132   
1133                                ;----------------------------------------------------------------------
1134                                SEND_PACKET_TO_CONTROLLER
1135   
1136                                ; forward packet stuff to the MCE
1137                                ; gets address in HOST memory where packet is stored
1138                                ; read 3 consecutive locations starting at this address
1139                                ; then sends the data from these locations up to the MCE
1140                                ;----------------------------------------------------------------------
1141   
1142                                ; word 1 = command = 'CON'
1143                                ; word 2 = host high address
1144                                ; word 3 = host low address
1145                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1146                                ;        = '1' --> when MCE command is GO
1147   
1148                                ; all MCE commands are now 'block commands'
1149                                ; i.e. 64 words long.
1150   
1151      P:0002B3 P:0002B5 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
1152   
1153      P:0002B4 P:0002B6 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1154                                                                                    ; reads as 4 x 24 bit words
1155   
1156      P:0002B5 P:0002B7 568800            MOVE              X:<DRXR_WD1,A           ; read command
1157      P:0002B6 P:0002B8 44F400            MOVE              #'CON',X0
                            434F4E
1158      P:0002B8 P:0002BA 200045            CMP     X0,A                              ; ensure command is 'CON'
1159      P:0002B9 P:0002BB 0E22F2            JNE     <CON_ERROR                        ; error, command NOT HCVR address
1160   
1161                                ; convert 2 x 24 bit words ( only 16 LSBs are significant) from host into 32 bit address
1162      P:0002BA P:0002BC 20001B            CLR     B
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 24



1163      P:0002BB P:0002BD 448900            MOVE              X:<DRXR_WD2,X0          ; MS 16bits of address
1164      P:0002BC P:0002BE 518A00            MOVE              X:<DRXR_WD3,B0          ; LS 16bits of address
1165      P:0002BD P:0002BF 000000            NOP
1166      P:0002BE P:0002C0 000000            NOP
1167      P:0002BF P:0002C1 000000            NOP
1168      P:0002C0 P:0002C2 000000            NOP
1169                                ; ;;; MFH - done
1170   
1171      P:0002C1 P:0002C3 0C1941            INSERT  #$010010,X0,B                     ; convert to 32 bits and put in B
                            010010
1172   
1173      P:0002C3 P:0002C5 568B00            MOVE              X:<DRXR_WD4,A           ; read word 4 - GO command?
1174      P:0002C4 P:0002C6 44F000            MOVE              X:ZERO,X0
                            000033
1175      P:0002C6 P:0002C8 200045            CMP     X0,A
1176      P:0002C7 P:0002C9 0AF0AA            JEQ     BLOCK_CON
                            0002D5
1177   
1178   
1179      P:0002C9 P:0002CB 0A008C            JCLR    #APPLICATION_RUNNING,X:STATUS,SET_PACKET_DELAY ; not running diagnostic applic
ation?
                            0002D3
1180   
1181                                ; need to generate an internal go command to test master write on bus.....  Diagnostic test
1182      P:0002CB P:0002CD 0A702D            BSET    #INTERNAL_GO,X:STATUS             ; set flag so that GO reply / data is genera
ted by PCI card...
                            000000
1183   
1184                                ; since INTERNAL_GO  - read command but don't send it to MCE...
1185   
1186                                CLR_CMD
1187      P:0002CD P:0002CF 064080            DO      #64,END_CLR_CMD                   ; block size = 32bit x 64 (256 bytes)
                            0002D0
1188      P:0002CF P:0002D1 0D0449            JSR     <READ_FROM_PCI                    ; get next 32 bit word from HOST
1189      P:0002D0 P:0002D2 000000            NOP
1190                                END_CLR_CMD
1191      P:0002D1 P:0002D3 0AF080            JMP     FINISH_CON                        ; don't send out on command on fibre
                            0002E3
1192   
1193   
1194                                SET_PACKET_DELAY
1195      P:0002D3 P:0002D5 0A7027            BSET    #DATA_DLY,X:STATUS                ; set data delay so that next data packet af
ter go reply
                            000000
1196                                                                                    ; experiences a delay before host notify.
1197   
1198                                ; -----------------------------------------------------------------------
1199                                ; WARNING!!!
1200                                ; MCE requires IDLE characters between 32bit words sent FROM the PCI card
1201                                ; DO not change READ_FROM_PCI to DMA block transfer....
1202                                ; ------------------------------------------------------------------------
1203   
1204                                BLOCK_CON
1205      P:0002D5 P:0002D7 66F000            MOVE              X:CONSTORE,R6
                            000041
1206   
1207      P:0002D7 P:0002D9 064080            DO      #64,END_BLOCK_CON                 ; block size = 32bit x 64 (256 bytes)
                            0002DF
1208      P:0002D9 P:0002DB 0D0449            JSR     <READ_FROM_PCI                    ; get next 32 bit word from HOST
1209      P:0002DA P:0002DC 208C00            MOVE              X0,A1                   ; prepare to send
1210      P:0002DB P:0002DD 20A800            MOVE              X1,A0                   ; prepare to send
1211   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 25



1212      P:0002DC P:0002DE 4D5E00            MOVE                          X1,Y:(R6)+  ; b4, b3 (msb)
1213      P:0002DD P:0002DF 4C5E00            MOVE                          X0,Y:(R6)+  ; b2, b1  (lsb)
1214   
1215      P:0002DE P:0002E0 0D048A            JSR     <XMT_WD_FIBRE                     ; off it goes
1216      P:0002DF P:0002E1 000000            NOP
1217                                END_BLOCK_CON
1218   
1219      P:0002E0 P:0002E2 0A0008            BCLR    #PACKET_CHOKE,X:<STATUS           ; disable packet choke...
1220                                                                                    ; comms now open with MCE and packets will b
e processed.
1221                                ; Enable Byte swaping for correct comms protocol.
1222      P:0002E1 P:0002E3 0A0025            BSET    #BYTE_SWAP,X:<STATUS              ; flag to let host know byte swapping on
1223      P:0002E2 P:0002E4 013D24            BSET    #AUX1,X:PDRC                      ; enable hardware
1224   
1225   
1226                                ; -------------------------------------------------------------------------
1227                                ; when completed successfully then PCI needs to reply to Host with
1228                                ; word1 = reply/data = reply
1229                                FINISH_CON
1230      P:0002E3 P:0002E5 44F400            MOVE              #'REP',X0
                            524550
1231      P:0002E5 P:0002E7 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1232      P:0002E6 P:0002E8 44F400            MOVE              #'CON',X0
                            434F4E
1233      P:0002E8 P:0002EA 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1234      P:0002E9 P:0002EB 44F400            MOVE              #'ACK',X0
                            41434B
1235      P:0002EB P:0002ED 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1236      P:0002EC P:0002EE 44F400            MOVE              #'000',X0
                            303030
1237      P:0002EE P:0002F0 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1238      P:0002EF P:0002F1 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1239      P:0002F0 P:0002F2 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ;  interrupt host with message (x0 restored 
here)
1240      P:0002F1 P:0002F3 000004            RTI                                       ; return from ISR
1241   
1242                                ; when there is a failure in the host to PCI command then the PCI
1243                                ; needs still to reply to Host but with an error message
1244                                CON_ERROR
1245      P:0002F2 P:0002F4 44F400            MOVE              #'REP',X0
                            524550
1246      P:0002F4 P:0002F6 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1247      P:0002F5 P:0002F7 44F400            MOVE              #'CON',X0
                            434F4E
1248      P:0002F7 P:0002F9 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1249      P:0002F8 P:0002FA 44F400            MOVE              #'ERR',X0
                            455252
1250      P:0002FA P:0002FC 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1251      P:0002FB P:0002FD 44F400            MOVE              #'CNE',X0
                            434E45
1252      P:0002FD P:0002FF 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1253      P:0002FE P:000300 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1254      P:0002FF P:000301 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1255      P:000300 P:000302 000004            RTI                                       ; return from ISR
1256   
1257                                ; ------------------------------------------------------------------------------------
1258                                SEND_PACKET_TO_HOST
1259                                ; this command is received from the Host and actions the PCI board to pick up an address
1260                                ; pointer from DRXR which the PCI board then uses to write packets from the
1261                                ; MCE to the host memory starting at the address given.
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 26



1262                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1263                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1264                                ; HST after packet sent (unless error).
1265                                ; --------------------------------------------------------------------------------------
1266                                ; word 1 = command = 'HST'
1267                                ; word 2 = host high address
1268                                ; word 3 = host low address
1269                                ; word 4 = not used but read
1270   
1271                                ; save some registers but not B
1272   
1273      P:000301 P:000303 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
1274   
1275      P:000302 P:000304 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1276      P:000303 P:000305 20001B            CLR     B
1277      P:000304 P:000306 568800            MOVE              X:<DRXR_WD1,A           ; read command
1278      P:000305 P:000307 44F400            MOVE              #'HST',X0
                            485354
1279      P:000307 P:000309 200045            CMP     X0,A                              ; ensure command is 'HST'
1280      P:000308 P:00030A 0E2314            JNE     <HOST_ERROR                       ; error, command NOT HCVR address
1281      P:000309 P:00030B 448900            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
1282      P:00030A P:00030C 518A00            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1283   
1285      P:00030B P:00030D 447000            MOVE              X0,X:BURST_DEST_HI
                            000045
1286      P:00030D P:00030F 517000            MOVE              B0,X:BURST_DEST_LO
                            000044
1288   
1289      P:00030F P:000311 0C1941            INSERT  #$010010,X0,B                     ; convert to 32 bits and put in B
                            010010
1290   
1291      P:000311 P:000313 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1292      P:000312 P:000314 0D0475            JSR     <RESTORE_HST_REGISTERS            ; restore registers for HST .... B not resto
red..
1293      P:000313 P:000315 000004            RTI
1294   
1295                                ; !!NOTE!!!
1296                                ; successful reply to this command is sent after packet has been send to host.
1297                                ; Not here unless error.
1298   
1299                                ; when there is a failure in the host to PCI command then the PCI
1300                                ; needs still to reply to Host but with an error message
1301                                HOST_ERROR
1302      P:000314 P:000316 0A7001            BCLR    #SEND_TO_HOST,X:STATUS
                            000000
1303      P:000316 P:000318 44F400            MOVE              #'REP',X0
                            524550
1304      P:000318 P:00031A 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1305      P:000319 P:00031B 44F400            MOVE              #'HST',X0
                            485354
1306      P:00031B P:00031D 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1307      P:00031C P:00031E 44F400            MOVE              #'ERR',X0
                            455252
1308      P:00031E P:000320 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1309      P:00031F P:000321 44F400            MOVE              #'CNE',X0
                            434E45
1310      P:000321 P:000323 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1311      P:000322 P:000324 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1312      P:000323 P:000325 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 27



1313      P:000324 P:000326 000004            RTI
1314   
1315                                ; --------------------------------------------------------------------
1316                                SOFTWARE_RESET
1317                                ;----------------------------------------------------------------------
1318                                ; word 1 = command = 'RST'
1319                                ; word 2 = not used but read
1320                                ; word 3 = not used but read
1321                                ; word 4 = not used but read
1322   
1323      P:000325 P:000327 0D047E            JSR     <SAVE_REGISTERS
1324   
1325      P:000326 P:000328 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1326      P:000327 P:000329 568800            MOVE              X:<DRXR_WD1,A           ; read command
1327      P:000328 P:00032A 44F400            MOVE              #'RST',X0
                            525354
1328      P:00032A P:00032C 200045            CMP     X0,A                              ; ensure command is 'RST'
1329      P:00032B P:00032D 0E234E            JNE     <RST_ERROR                        ; error, command NOT HCVR address
1330   
1331                                ; RST command OK so reply to host
1332                                FINISH_RST
1333      P:00032C P:00032E 44F400            MOVE              #'REP',X0
                            524550
1334      P:00032E P:000330 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1335      P:00032F P:000331 44F400            MOVE              #'RST',X0
                            525354
1336      P:000331 P:000333 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1337      P:000332 P:000334 44F400            MOVE              #'ACK',X0
                            41434B
1338      P:000334 P:000336 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1339      P:000335 P:000337 44F400            MOVE              #'000',X0
                            303030
1340      P:000337 P:000339 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1341      P:000338 P:00033A 0D0421            JSR     <PCI_MESSAGE_TO_HOST
1342   
1343      P:000339 P:00033B 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            000339
1344   
1345      P:00033B P:00033D 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS     ; clear app flag
1346      P:00033C P:00033E 0A0006            BCLR    #PREAMBLE_ERROR,X:<STATUS         ; clear preamble error
1347      P:00033D P:00033F 0A000C            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1348   
1349                                ; initialise some parameter here - that we don't want to initialse under a fatal error reset.
1350   
1351      P:00033E P:000340 200013            CLR     A
1352      P:00033F P:000341 340000            MOVE              #0,R4                   ; initialise word count
1353      P:000340 P:000342 560600            MOVE              A,X:<WORD_COUNT         ; initialise word count store (num of words 
written over bus/packet)
1354      P:000341 P:000343 560700            MOVE              A,X:<NUM_DUMPED         ; initialise number dumped from FIFO (after 
HST TO)
1355   
1356   
1357                                ; remember we are in a ISR so can't just jump to start.
1358   
1359      P:000342 P:000344 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable HF* FIFO interrupt
                            0001C0
1360      P:000344 P:000346 05F439            MOVE              #$200,SR                ; Mask set up for reset switch only.
                            000200
1361   
1362   
1363      P:000346 P:000348 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1364      P:000347 P:000349 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 28



                            000200
1365                                                                                    ; set to zero except for interrupts
1366      P:000349 P:00034B 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1367                                                                                    ; so first set to 0
1368      P:00034A P:00034C 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1369                                                                                    ; therefore,return to initialization
1370      P:00034C P:00034E 000000            NOP
1371      P:00034D P:00034F 000004            RTI                                       ; return from ISR - to START
1372   
1373                                ; when there is a failure in the host to PCI command then the PCI
1374                                ; needs still to reply to Host but with an error message
1375                                RST_ERROR
1376      P:00034E P:000350 44F400            MOVE              #'REP',X0
                            524550
1377      P:000350 P:000352 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1378      P:000351 P:000353 44F400            MOVE              #'RST',X0
                            525354
1379      P:000353 P:000355 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1380      P:000354 P:000356 44F400            MOVE              #'ERR',X0
                            455252
1381      P:000356 P:000358 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1382      P:000357 P:000359 44F400            MOVE              #'CNE',X0
                            434E45
1383      P:000359 P:00035B 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1384      P:00035A P:00035C 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1385      P:00035B P:00035D 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1386      P:00035C P:00035E 000004            RTI                                       ; return from ISR
1387   
1388   
1389                                ;-----------------------------------------------------------------------------
1390                                START_APPLICATION
1391                                ; an application should already have been downloaded to the PCI memory.
1392                                ; this command will execute it.
1393                                ; ----------------------------------------------------------------------
1394                                ; word 1 = command = 'GOA'
1395                                ; word 2 = not used but read by RD_DRXR
1396                                ; word 3 = not used but read by RD_DRXR
1397                                ; word 4 = not used but read by RD_DRXR
1398   
1399      P:00035D P:00035F 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
1400   
1401      P:00035E P:000360 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1402      P:00035F P:000361 568800            MOVE              X:<DRXR_WD1,A           ; read command
1403      P:000360 P:000362 44F400            MOVE              #'GOA',X0
                            474F41
1404      P:000362 P:000364 200045            CMP     X0,A                              ; ensure command is 'RDM'
1405      P:000363 P:000365 0E2366            JNE     <GO_ERROR                         ; error, command NOT HCVR address
1406   
1407                                ; if we get here then everything is fine and we can start the application
1408                                ; set bit in status so that main fibre servicing code knows to jump
1409                                ; to application space after returning from this ISR
1410   
1411                                ; reply after application has been executed.
1412      P:000364 P:000366 0A0020            BSET    #APPLICATION_LOADED,X:<STATUS
1413      P:000365 P:000367 000004            RTI                                       ; return from ISR
1414   
1415   
1416                                ; when there is a failure in the host to PCI command then the PCI
1417                                ; needs still to reply to Host but with an error message
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 29



1418                                GO_ERROR
1419      P:000366 P:000368 44F400            MOVE              #'REP',X0
                            524550
1420      P:000368 P:00036A 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1421      P:000369 P:00036B 44F400            MOVE              #'GOA',X0
                            474F41
1422      P:00036B P:00036D 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1423      P:00036C P:00036E 44F400            MOVE              #'ERR',X0
                            455252
1424      P:00036E P:000370 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1425      P:00036F P:000371 44F400            MOVE              #'CNE',X0
                            434E45
1426      P:000371 P:000373 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1427      P:000372 P:000374 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1428      P:000373 P:000375 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1429      P:000374 P:000376 000004            RTI                                       ; return from ISR
1430   
1431                                ; ---------------------------------------------------------
1432                                STOP_APPLICATION
1433                                ; this command stops an application that is currently running
1434                                ; used for applications that once started run contiunually
1435                                ;-----------------------------------------------------------
1436   
1437                                ; word 1 = command = ' STP'
1438                                ; word 2 = not used but read
1439                                ; word 3 = not used but read
1440                                ; word 4 = not used but read
1441   
1442      P:000375 P:000377 0D047E            JSR     <SAVE_REGISTERS
1443   
1444      P:000376 P:000378 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1445      P:000377 P:000379 568800            MOVE              X:<DRXR_WD1,A           ; read command
1446      P:000378 P:00037A 44F400            MOVE              #'STP',X0
                            535450
1447      P:00037A P:00037C 200045            CMP     X0,A                              ; ensure command is 'RDM'
1448      P:00037B P:00037D 0E238E            JNE     <STP_ERROR                        ; error, command NOT HCVR address
1449   
1450      P:00037C P:00037E 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS
1451      P:00037D P:00037F 0A700C            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1452   
1453                                ; when completed successfully then PCI needs to reply to Host with
1454                                ; word1 = reply/data = reply
1455                                FINISH_STP
1456      P:00037F P:000381 44F400            MOVE              #'REP',X0
                            524550
1457      P:000381 P:000383 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1458      P:000382 P:000384 44F400            MOVE              #'STP',X0
                            535450
1459      P:000384 P:000386 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1460      P:000385 P:000387 44F400            MOVE              #'ACK',X0
                            41434B
1461      P:000387 P:000389 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1462      P:000388 P:00038A 44F400            MOVE              #'000',X0
                            303030
1463      P:00038A P:00038C 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1464      P:00038B P:00038D 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers.
1465      P:00038C P:00038E 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1466      P:00038D P:00038F 000004            RTI
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 30



1467   
1468                                ; when there is a failure in the host to PCI command then the PCI
1469                                ; needs still to reply to Host but with an error message
1470                                STP_ERROR
1471      P:00038E P:000390 44F400            MOVE              #'REP',X0
                            524550
1472      P:000390 P:000392 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1473      P:000391 P:000393 44F400            MOVE              #'STP',X0
                            535450
1474      P:000393 P:000395 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1475      P:000394 P:000396 44F400            MOVE              #'ERR',X0
                            455252
1476      P:000396 P:000398 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1477      P:000397 P:000399 44F400            MOVE              #'CNE',X0
                            434E45
1478      P:000399 P:00039B 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1479      P:00039A P:00039C 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1480      P:00039B P:00039D 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1481      P:00039C P:00039E 000004            RTI
1482   
1483                                ;--------------------------------------------------------------
1484                                WRITE_MEMORY
1485                                ;---------------------------------------------------------------
1486                                ; word 1 = command = 'WRM'
1487                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1488                                ; word 3 = address in memory
1489                                ; word 4 = value
1490   
1491      P:00039D P:00039F 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
1492   
1493      P:00039E P:0003A0 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1494      P:00039F P:0003A1 56F000            MOVE              X:DRXR_WD1,A            ; read command
                            000008
1495      P:0003A1 P:0003A3 44F400            MOVE              #'WRM',X0
                            57524D
1496      P:0003A3 P:0003A5 200045            CMP     X0,A                              ; ensure command is 'WRM'
1497      P:0003A4 P:0003A6 0E23C7            JNE     <WRITE_MEMORY_ERROR_CNE           ; error, command NOT HCVR address
1498      P:0003A5 P:0003A7 568900            MOVE              X:<DRXR_WD2,A           ; Memory type (X, Y, P)
1499      P:0003A6 P:0003A8 578A00            MOVE              X:<DRXR_WD3,B
1500      P:0003A7 P:0003A9 000000            NOP                                       ; pipeline restriction
1501      P:0003A8 P:0003AA 21B000            MOVE              B1,R0                   ; get address to write to
1502      P:0003A9 P:0003AB 448B00            MOVE              X:<DRXR_WD4,X0          ; get data to write
1503      P:0003AA P:0003AC 0140C5            CMP     #$005F50,A                        ; $00'_P'
                            005F50
1504      P:0003AC P:0003AE 0E23AF            JNE     <WRX
1505      P:0003AD P:0003AF 076084            MOVE              X0,P:(R0)               ; Write to Program memory
1506      P:0003AE P:0003B0 0C03B8            JMP     <FINISH_WRITE_MEMORY
1507                                WRX
1508      P:0003AF P:0003B1 0140C5            CMP     #$005F58,A                        ; $00'_X'
                            005F58
1509      P:0003B1 P:0003B3 0E23B4            JNE     <WRY
1510      P:0003B2 P:0003B4 446000            MOVE              X0,X:(R0)               ; Write to X: memory
1511      P:0003B3 P:0003B5 0C03B8            JMP     <FINISH_WRITE_MEMORY
1512                                WRY
1513      P:0003B4 P:0003B6 0140C5            CMP     #$005F59,A                        ; $00'_Y'
                            005F59
1514      P:0003B6 P:0003B8 0E23CB            JNE     <WRITE_MEMORY_ERROR_MTE
1515      P:0003B7 P:0003B9 4C6000            MOVE                          X0,Y:(R0)   ; Write to Y: memory
1516   
1517                                ; when completed successfully then PCI needs to reply to Host with
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 31



1518                                ; word1 = reply/data = reply
1519                                FINISH_WRITE_MEMORY
1520      P:0003B8 P:0003BA 44F400            MOVE              #'REP',X0
                            524550
1521      P:0003BA P:0003BC 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1522      P:0003BB P:0003BD 44F400            MOVE              #'WRM',X0
                            57524D
1523      P:0003BD P:0003BF 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1524      P:0003BE P:0003C0 44F400            MOVE              #'ACK',X0
                            41434B
1525      P:0003C0 P:0003C2 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1526      P:0003C1 P:0003C3 44F400            MOVE              #'000',X0
                            303030
1527      P:0003C3 P:0003C5 440F00            MOVE              X0,X:<DTXS_WD4          ; no error
1528      P:0003C4 P:0003C6 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1529      P:0003C5 P:0003C7 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1530      P:0003C6 P:0003C8 000004            RTI
1531   
1532                                ;
1533                                WRITE_MEMORY_ERROR_CNE
1534      P:0003C7 P:0003C9 44F400            MOVE              #'CNE',X0
                            434E45
1535      P:0003C9 P:0003CB 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1536      P:0003CA P:0003CC 0C03CE            JMP     <WRITE_MEMORY_ERROR               ; fill in rest of reply
1537   
1538                                WRITE_MEMORY_ERROR_MTE
1539      P:0003CB P:0003CD 44F400            MOVE              #'MTE',X0
                            4D5445
1540      P:0003CD P:0003CF 440F00            MOVE              X0,X:<DTXS_WD4          ; Memory Type Error - memory type not valid
1541   
1542                                WRITE_MEMORY_ERROR
1543      P:0003CE P:0003D0 44F400            MOVE              #'REP',X0
                            524550
1544      P:0003D0 P:0003D2 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1545      P:0003D1 P:0003D3 44F400            MOVE              #'WRM',X0
                            57524D
1546      P:0003D3 P:0003D5 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1547      P:0003D4 P:0003D6 44F400            MOVE              #'ERR',X0
                            455252
1548      P:0003D6 P:0003D8 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1549      P:0003D7 P:0003D9 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1550      P:0003D8 P:0003DA 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1551      P:0003D9 P:0003DB 000004            RTI
1552   
1553   
1554                                ;---------------------------------------------------------------
1555                                ;
1556                                ;                          * END OF ISRs *
1557                                ;
1558                                ;--------------------------------------------------------------
1559   
1560   
1561   
1562                                ;----------------------------------------------------------------
1563                                ;
1564                                ;                     * Beginning of SUBROUTINES *
1565                                ;
1566                                ;-----------------------------------------------------------------
1567   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 32



1568   
1569                                ; -------------------------------------------------------------
1570                                CALC_NO_BUFFS
1571                                ;----------------------------------------------------
1572                                ; number of 512 buffers in packet calculated (X:TOTAL_BUFFS)
1573                                ; and number of left over blocks (X:NUM_LEFTOVER_BLOCKS)
1574                                ; and left over words (X:LEFT_TO_READ)
1575   
1576      P:0003DA P:0003DC 20001B            CLR     B
1577      P:0003DB P:0003DD 51A300            MOVE              X:<HEAD_W4_0,B0         ; LS 16bits
1578      P:0003DC P:0003DE 44A200            MOVE              X:<HEAD_W4_1,X0         ; MS 16bits
1579   
1580      P:0003DD P:0003DF 0C1941            INSERT  #$010010,X0,B                     ; now size of packet B....giving # of 32bit 
words in packet
                            010010
1581      P:0003DF P:0003E1 000000            NOP
1582   
1583                                ; need to covert this to 16 bit since read from FIFO and saved in Y memory as 16bit words...
1584   
1585                                ; so double size of packet....
1586      P:0003E0 P:0003E2 20003A            ASL     B
1587   
1588                                ; now save
1589      P:0003E1 P:0003E3 212400            MOVE              B0,X0
1590      P:0003E2 P:0003E4 21A500            MOVE              B1,X1
1591      P:0003E3 P:0003E5 443600            MOVE              X0,X:<PACKET_SIZE_LOW   ; low 24 bits of packet size (in 16bit words
)
1592      P:0003E4 P:0003E6 453700            MOVE              X1,X:<PACKET_SIZE_HIH   ; high 8 bits of packet size (in 16bit words
)
1593   
1594      P:0003E5 P:0003E7 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1595      P:0003E6 P:0003E8 54B700            MOVE              X:<PACKET_SIZE_HIH,A1
1596      P:0003E7 P:0003E9 0C1C12            ASR     #9,A,A                            ; divide by 512...number of 16bit words in a
 buffer
1597      P:0003E8 P:0003EA 000000            NOP
1598      P:0003E9 P:0003EB 503C00            MOVE              A0,X:<TOTAL_BUFFS
1599   
1600      P:0003EA P:0003EC 210500            MOVE              A0,X1
1601      P:0003EB P:0003ED 47F400            MOVE              #HF_FIFO,Y1
                            000200
1602      P:0003ED P:0003EF 2000F0            MPY     X1,Y1,A
1603      P:0003EE P:0003F0 0C1C03            ASR     #1,A,B                            ; B holds number of 16bit words in all full 
buffers
1604      P:0003EF P:0003F1 000000            NOP
1605   
1606      P:0003F0 P:0003F2 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1607      P:0003F1 P:0003F3 54B700            MOVE              X:<PACKET_SIZE_HIH,A1   ; A holds total number of 16bit words
1608      P:0003F2 P:0003F4 200014            SUB     B,A                               ; now A holds number of left over 16bit word
s
1609      P:0003F3 P:0003F5 000000            NOP
1610      P:0003F4 P:0003F6 503D00            MOVE              A0,X:<LEFT_TO_READ      ; store number of left over 16bit words to r
ead
1611      P:0003F5 P:0003F7 0C1C0A            ASR     #5,A,A                            ; divide by 32... number of 16bit words in l
efover block
1612      P:0003F6 P:0003F8 000000            NOP
1613      P:0003F7 P:0003F9 503F00            MOVE              A0,X:<NUM_LEFTOVER_BLOCKS
1614      P:0003F8 P:0003FA 210500            MOVE              A0,X1
1615      P:0003F9 P:0003FB 47F400            MOVE              #>SMALL_BLK,Y1
                            000020
1616      P:0003FB P:0003FD 2000F0            MPY     X1,Y1,A
1617      P:0003FC P:0003FE 0C1C02            ASR     #1,A,A
1618      P:0003FD P:0003FF 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 33



1619   
1620      P:0003FE P:000400 200018            ADD     A,B                               ; B holds words in all buffers
1621      P:0003FF P:000401 000000            NOP
1622      P:000400 P:000402 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1623      P:000401 P:000403 54B700            MOVE              X:<PACKET_SIZE_HIH,A1   ; A holds total number of words
1624      P:000402 P:000404 200014            SUB     B,A                               ; now A holds number of left over words
1625      P:000403 P:000405 000000            NOP
1626      P:000404 P:000406 503D00            MOVE              A0,X:<LEFT_TO_READ      ; store number of left over 16bit words to r
ead
1627   
1628      P:000405 P:000407 0C1C02            ASR     #1,A,A                            ; divide by two to get number of 32 bit word
s to write
1629      P:000406 P:000408 000000            NOP                                       ; for pipeline
1630      P:000407 P:000409 503E00            MOVE              A0,X:<LEFT_TO_WRITE     ; store number of left over 32 bit words (2 
x 16 bit) to write to host after small block transfer as well
1631   
1632      P:000408 P:00040A 00000C            RTS
1633   
1634                                ;---------------------------------------------------------------
1635                                GET_FO_WRD
1636                                ;--------------------------------------------------------------
1637                                ; Anything in fibre receive FIFO?   If so store in X0
1638   
1639      P:000409 P:00040B 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            00041F
1640      P:00040B P:00040D 000000            NOP
1641      P:00040C P:00040E 000000            NOP
1642      P:00040D P:00040F 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS             ; check twice for FO metastability.
                            00041F
1643      P:00040F P:000411 0AF080            JMP     RD_FO_WD
                            000417
1644   
1645      P:000411 P:000413 01AD80  WT_FIFO   JCLR    #EF,X:PDRD,*                      ; Wait till something in FIFO flagged
                            000411
1646      P:000413 P:000415 000000            NOP
1647      P:000414 P:000416 000000            NOP
1648      P:000415 P:000417 01AD80            JCLR    #EF,X:PDRD,WT_FIFO                ; check twice.....
                            000411
1649   
1650                                ; Read one word from the fiber optics FIFO, check it and put it in A1
1651                                RD_FO_WD
1652      P:000417 P:000419 09443F            MOVEP             Y:RDFIFO,X0             ; then read to X0
1653      P:000418 P:00041A 54F400            MOVE              #$00FFFF,A1             ; mask off top 2 bytes ($FC)
                            00FFFF
1654      P:00041A P:00041C 200046            AND     X0,A                              ; since receiving 16 bits in 24bit register
1655      P:00041B P:00041D 000000            NOP
1656      P:00041C P:00041E 218400            MOVE              A1,X0
1657      P:00041D P:00041F 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1658      P:00041E P:000420 00000C            RTS
1659                                CLR_FO_RTS
1660      P:00041F P:000421 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1661      P:000420 P:000422 00000C            RTS
1662   
1663   
1664                                ; ----------------------------------------------------------------------------
1665                                PCI_MESSAGE_TO_HOST
1666                                ;----------------------------------------------------------------------------
1667   
1668                                ; subroutine to send 4 words as a reply from PCI to the Host
1669                                ; using the DTXS-HRXS data path
1670                                ; PCI card writes here first then causes an interrupt INTA on
1671                                ; the PCI bus to alert the host to the reply message
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 34



1672   
1673      P:000421 P:000423 0A85A3            JSET    #DCTR_HF3,X:DCTR,*                ; make sure host ready to receive interrupt
                            000421
1674                                                                                    ; cleared via fast interrupt if host out of 
its ISR
1675   
1676      P:000423 P:000425 0A8981            JCLR    #STRQ,X:DSR,*                     ; Wait for transmitter to be NOT FULL
                            000423
1677                                                                                    ; i.e. if CLR then FULL so wait
1678                                                                                    ; if not then it is clear to write
1679   
1680                                PCI_MESSAGE_TO_HOST_NOW                             ; non-blocking entry point...
1681   
1682      P:000425 P:000427 448C00            MOVE              X:<DTXS_WD1,X0
1683      P:000426 P:000428 447000            MOVE              X0,X:DTXS               ; Write 24 bit word1
                            FFFFCD
1684   
1685      P:000428 P:00042A 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            000428
1686      P:00042A P:00042C 448D00            MOVE              X:<DTXS_WD2,X0
1687      P:00042B P:00042D 447000            MOVE              X0,X:DTXS               ; Write 24 bit word2
                            FFFFCD
1688   
1689      P:00042D P:00042F 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            00042D
1690      P:00042F P:000431 448E00            MOVE              X:<DTXS_WD3,X0
1691      P:000430 P:000432 447000            MOVE              X0,X:DTXS               ; Write 24 bit word3
                            FFFFCD
1692   
1693      P:000432 P:000434 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            000432
1694      P:000434 P:000436 448F00            MOVE              X:<DTXS_WD4,X0
1695      P:000435 P:000437 447000            MOVE              X0,X:DTXS               ; Write 24 bit word4
                            FFFFCD
1696   
1697   
1698                                ; restore X0....
1699                                ; PCI_MESSAGE_TO_HOST is used by all command vector ISRs.
1700                                ; Working registers must be restored before RTI.
1701                                ; However, we want to restore before asserting INTA.
1702                                ; x0 is only one that can't be restored before PCI_MESSAGE_TO_HOST
1703                                ; (since it is used by this SR) hence we restore here.
1704                                ; this is redundant for a 'NFY' message (since sequential instruction)
1705                                ; but may be required for a PCI command reply 'REP' message.
1706                                ; (since interrupt driven)
1707   
1708      P:000437 P:000439 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            00002E
1709   
1710                                ; all the transmit words are in the FIFO, interrupt the Host
1711                                ; the Host should clear this interrupt once it is detected.
1712                                ; It does this by writing to HCVR to cause a fast interrupt.
1713   
1714   
1715      P:000439 P:00043B 0A8523            BSET    #DCTR_HF3,X:DCTR                  ; set flag to handshake interrupt (INTA) wit
h host.
1716      P:00043A P:00043C 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1717   
1718      P:00043B P:00043D 00000C            RTS
1719   
1720                                ;---------------------------------------------------------------
1721                                RD_DRXR
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 35



1722                                ;--------------------------------------------------------------
1723                                ; routine is used to read from HTXR-DRXR data path
1724                                ; which is used by the Host to communicate with the PCI board
1725                                ; the host writes 4 words to this FIFO then interrupts the PCI
1726                                ; which reads the 4 words and acts on them accordingly.
1727   
1728   
1729      P:00043C P:00043E 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            00043C
1730                                                                                    ; implies that host has written words
1731   
1732   
1733                                ; actually reading as slave here so this shouldn't be necessary......?
1734   
1735      P:00043E P:000440 0A8717            BCLR    #FC1,X:DPMC                       ; 24 bit read FC1 = 0, FC1 = 0
1736      P:00043F P:000441 0A8736            BSET    #FC0,X:DPMC
1737   
1738   
1739      P:000440 P:000442 08440B            MOVEP             X:DRXR,X0               ; Get word1
1740      P:000441 P:000443 440800            MOVE              X0,X:<DRXR_WD1
1741      P:000442 P:000444 08440B            MOVEP             X:DRXR,X0               ; Get word2
1742      P:000443 P:000445 440900            MOVE              X0,X:<DRXR_WD2
1743      P:000444 P:000446 08440B            MOVEP             X:DRXR,X0               ; Get word3
1744      P:000445 P:000447 440A00            MOVE              X0,X:<DRXR_WD3
1745      P:000446 P:000448 08440B            MOVEP             X:DRXR,X0               ; Get word4
1746      P:000447 P:000449 440B00            MOVE              X0,X:<DRXR_WD4
1747      P:000448 P:00044A 00000C            RTS
1748   
1749                                ;---------------------------------------------------------------
1750                                READ_FROM_PCI
1751                                ;--------------------------------------------------------------
1752                                ; sub routine to read a 24 bit word in from PCI bus --> Y memory
1753                                ; 32bit host address in accumulator B.
1754   
1755                                ; read as master
1756   
1757      P:000449 P:00044B 0C1890            EXTRACTU #$010010,B,A                     ; Get D31-16 bits only
                            010010
1758      P:00044B P:00044D 000000            NOP
1759   
1760      P:00044C P:00044E 210C00            MOVE              A0,A1
1761      P:00044D P:00044F 000000            NOP
1762      P:00044E P:000450 547000            MOVE              A1,X:DPMC               ; high 16bits of address in DSP master cntr 
reg.
                            FFFFC7
1763                                                                                    ; 32 bit read so FC1 = 0 and FC0 = 0
1764   
1765      P:000450 P:000452 000000            NOP
1766      P:000451 P:000453 0C1890            EXTRACTU #$010000,B,A
                            010000
1767      P:000453 P:000455 000000            NOP
1768      P:000454 P:000456 210C00            MOVE              A0,A1
1769      P:000455 P:000457 0140C2            OR      #$060000,A                        ; A1 gets written to DPAR register
                            060000
1770      P:000457 P:000459 000000            NOP                                       ; C3-C0 of DPAR=0110 for memory read
1771      P:000458 P:00045A 08CC08  WRT_ADD   MOVEP             A1,X:DPAR               ; Write address to PCI bus - PCI READ action
1772      P:000459 P:00045B 000000            NOP                                       ; Pipeline delay
1773      P:00045A P:00045C 0A8AA2  RD_PCI    JSET    #MRRQ,X:DPSR,GET_DAT              ; If MTRQ = 1 go read the word from host via
 FIFO
                            000463
1774      P:00045C P:00045E 0A8A8A            JCLR    #TRTY,X:DPSR,RD_PCI               ; Bit is set if its a retry
                            00045A
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 36



1775      P:00045E P:000460 08F48A            MOVEP             #$0400,X:DPSR           ; Clear bit 10 = target retry bit
                            000400
1776      P:000460 P:000462 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for PCI addressing to be complete
                            000460
1777      P:000462 P:000464 0C0458            JMP     <WRT_ADD
1778   
1779      P:000463 P:000465 08440B  GET_DAT   MOVEP             X:DRXR,X0               ; Read 1st 16 bits of 32 bit word from host 
memory
1780      P:000464 P:000466 08450B            MOVEP             X:DRXR,X1               ; Read 2nd 16 bits of 32 bit word from host 
memory
1781   
1782                                ; note that we now have 4 bytes in X0 and X1.
1783                                ; The 32bit word was in host memory in little endian format
1784                                ; If form LSB --> MSB the bytes are b1, b2, b3, b4 in host memory
1785                                ; in progressing through the HTRX/DRXR FIFO the
1786                                ; bytes end up like this.....
1787                                ; then X0 = $00 b2 b1
1788                                ; and  X1 = $00 b4 b3
1789   
1790      P:000465 P:000467 0604A0            REP     #4                                ; increment PCI address by four bytes.
1791      P:000466 P:000468 000009            INC     B
1792      P:000467 P:000469 000000            NOP
1793      P:000468 P:00046A 00000C            RTS
1794   
1795                                ;------------------------------------------------------------------------------------
1796                                RESTORE_REGISTERS
1797                                ;-------------------------------------------------------------------------------------
1798   
1799      P:000469 P:00046B 05B239            MOVEC             X:<SV_SR,SR
1800   
1801      P:00046A P:00046C 50A800            MOVE              X:<SV_A0,A0
1802      P:00046B P:00046D 54A900            MOVE              X:<SV_A1,A1
1803      P:00046C P:00046E 52AA00            MOVE              X:<SV_A2,A2
1804   
1805      P:00046D P:00046F 51AB00            MOVE              X:<SV_B0,B0
1806      P:00046E P:000470 55AC00            MOVE              X:<SV_B1,B1
1807      P:00046F P:000471 53AD00            MOVE              X:<SV_B2,B2
1808   
1809      P:000470 P:000472 44AE00            MOVE              X:<SV_X0,X0
1810      P:000471 P:000473 45AF00            MOVE              X:<SV_X1,X1
1811   
1812      P:000472 P:000474 46B000            MOVE              X:<SV_Y0,Y0
1813      P:000473 P:000475 47B100            MOVE              X:<SV_Y1,Y1
1814   
1815      P:000474 P:000476 00000C            RTS
1816                                ;------------------------------------------------------------------------------------
1817                                RESTORE_HST_REGISTERS
1818                                ;-------------------------------------------------------------------------------------
1819                                ; B not restored after HST as it now contains address.
1820   
1821      P:000475 P:000477 05B239            MOVEC             X:<SV_SR,SR
1822   
1823      P:000476 P:000478 50A800            MOVE              X:<SV_A0,A0
1824      P:000477 P:000479 54A900            MOVE              X:<SV_A1,A1
1825      P:000478 P:00047A 52AA00            MOVE              X:<SV_A2,A2
1826   
1827      P:000479 P:00047B 44AE00            MOVE              X:<SV_X0,X0
1828      P:00047A P:00047C 45AF00            MOVE              X:<SV_X1,X1
1829   
1830      P:00047B P:00047D 46B000            MOVE              X:<SV_Y0,Y0
1831      P:00047C P:00047E 47B100            MOVE              X:<SV_Y1,Y1
1832   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 37



1833      P:00047D P:00047F 00000C            RTS
1834   
1835                                ;-------------------------------------------------------------------------------------
1836                                SAVE_REGISTERS
1837                                ;-------------------------------------------------------------------------------------
1838   
1839      P:00047E P:000480 053239            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1840   
1841      P:00047F P:000481 502800            MOVE              A0,X:<SV_A0
1842      P:000480 P:000482 542900            MOVE              A1,X:<SV_A1
1843      P:000481 P:000483 522A00            MOVE              A2,X:<SV_A2
1844   
1845      P:000482 P:000484 512B00            MOVE              B0,X:<SV_B0
1846      P:000483 P:000485 552C00            MOVE              B1,X:<SV_B1
1847      P:000484 P:000486 532D00            MOVE              B2,X:<SV_B2
1848   
1849      P:000485 P:000487 442E00            MOVE              X0,X:<SV_X0
1850      P:000486 P:000488 452F00            MOVE              X1,X:<SV_X1
1851   
1852      P:000487 P:000489 463000            MOVE              Y0,X:<SV_Y0
1853      P:000488 P:00048A 473100            MOVE              Y1,X:<SV_Y1
1854   
1855      P:000489 P:00048B 00000C            RTS
1856   
1857                                ;-------------------------------------------------------
1858                                XMT_WD_FIBRE
1859                                ;-----------------------------------------------------
1860                                ; 250 MHz code - Transmit contents of Accumulator A1 to the MCE
1861                                ; we want to send 32bit word in little endian fomat to the host.
1862                                ; i.e. b4b3b2b1 goes b1, b2, b3, b4
1863                                ; currently the bytes are in this order:
1864                                ;  A1 = $00 b2 b1
1865                                ;  A0 = $00 b4 b3
1866                                ;  A = $00 00 b2 b1 00 b4 b3
1867   
1868                                ; This subroutine must take at least 160ns (4 bytes at 25Mbytes/s)
1869   
1870      P:00048A P:00048C 000000            NOP
1871      P:00048B P:00048D 000000            NOP
1872   
1873                                ; split up 4 bytes b2, b1, b4, b3
1874   
1875      P:00048C P:00048E 0C1D20            ASL     #16,A,A                           ; shift byte b2 into A2
1876      P:00048D P:00048F 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1877   
1878      P:00048F P:000491 214700            MOVE              A2,Y1                   ; byte b2 in Y1
1879   
1880      P:000490 P:000492 0C1D10            ASL     #8,A,A                            ; shift byte b1 into A2
1881      P:000491 P:000493 000000            NOP
1882      P:000492 P:000494 214600            MOVE              A2,Y0                   ; byte b1 in Y0
1883   
1884      P:000493 P:000495 0C1D20            ASL     #16,A,A                           ; shift byte b4 into A2
1885      P:000494 P:000496 000000            NOP
1886      P:000495 P:000497 214500            MOVE              A2,X1                   ; byte b4 in X1
1887   
1888   
1889      P:000496 P:000498 0C1D10            ASL     #8,A,A                            ; shift byte b3 into A2
1890      P:000497 P:000499 000000            NOP
1891      P:000498 P:00049A 214400            MOVE              A2,X0                   ; byte b3 in x0
1892   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 38



1893                                ; transmit b1, b2, b3 ,b4
1894   
1895      P:000499 P:00049B 466000            MOVE              Y0,X:(R0)               ; byte b1 - off it goes
1896      P:00049A P:00049C 476000            MOVE              Y1,X:(R0)               ; byte b2 - off it goes
1897      P:00049B P:00049D 446000            MOVE              X0,X:(R0)               ; byte b3 - off it goes
1898      P:00049C P:00049E 456000            MOVE              X1,X:(R0)               ; byte b4 - off it goes
1899   
1900      P:00049D P:00049F 000000            NOP
1901      P:00049E P:0004A0 000000            NOP
1902      P:00049F P:0004A1 00000C            RTS
1903   
1904   
1905   
1909   
1910                                PCI_BURST_NOW
1911   
1912      P:0004A0 P:0004A2 51A300            MOVE              X:<HEAD_W4_0,B0         ; LS 16bits
1913      P:0004A1 P:0004A3 0C1D91            ASL     #8,B,B
1914      P:0004A2 P:0004A4 45A200            MOVE              X:<HEAD_W4_1,X1         ; MS 16bits
1915      P:0004A3 P:0004A5 0C1C8D            ASR     #6,B,B                            ; Size in BYTES
1916   
1917      P:0004A4 P:0004A6 627000            MOVE              R2,X:BURST_SRC
                            000046
1918      P:0004A6 P:0004A8 517000            MOVE              B0,X:BLOCK_SIZE
                            000042
1919   
1920      P:0004A8 P:0004AA 0BF080            JSR     BLOCK_TRANSFER
                            00050C
1921      P:0004AA P:0004AC 00000C            RTS
1922   
1923   
1924                                ;----------------------------------------------
1925                                FLUSH_PCI_FIFO
1926                                ;----------------------------------------------
1927      P:0004AB P:0004AD 0A8A84            JCLR    #MARQ,X:DPSR,*
                            0004AB
1928      P:0004AD P:0004AF 0A862E            BSET    #CLRT,X:DPCR
1929      P:0004AE P:0004B0 000000            NOP
1930      P:0004AF P:0004B1 0A86AE            JSET    #CLRT,X:DPCR,*
                            0004AF
1931      P:0004B1 P:0004B3 00000C            RTS
1932   
1933                                ;-----------------------------------------------
1934                                MPCI_ERROR_RECOVERY
1935                                ;-----------------------------------------------
1936      
1937      
1938      
1939      
1940      
1941      
1942   
1943      P:0004B2 P:0004B4 50F000            MOVE              X:DMA_ERRORS,A0
                            000047
1944      P:0004B4 P:0004B6 000008            INC     A
1945      P:0004B5 P:0004B7 000000            NOP
1946      P:0004B6 P:0004B8 507000            MOVE              A0,X:DMA_ERRORS
                            000047
1947   
1948      P:0004B8 P:0004BA 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            0004C6
1949      P:0004BA P:0004BC 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 39



                            0004D0
1950      P:0004BC P:0004BE 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            0004DA
1951      P:0004BE P:0004C0 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            0004E4
1952      P:0004C0 P:0004C2 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            0004EE
1953      P:0004C2 P:0004C4 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            0004F8
1954      P:0004C4 P:0004C6 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            000502
1955   
1956                                ERROR_TRTY
1957      P:0004C6 P:0004C8 50F000            MOVE              X:EC_TRTY,A0
                            000048
1958      P:0004C8 P:0004CA 000008            INC     A
1959      P:0004C9 P:0004CB 08F48A            MOVEP             #$0400,X:DPSR           ; Clear target retry error bit
                            000400
1960      P:0004CB P:0004CD 507000            MOVE              A0,X:EC_TRTY
                            000048
1961      P:0004CD P:0004CF 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1962      P:0004CF P:0004D1 00000C            RTS
1963                                ERROR_TO
1964      P:0004D0 P:0004D2 50F000            MOVE              X:EC_TO,A0
                            000049
1965      P:0004D2 P:0004D4 000008            INC     A
1966      P:0004D3 P:0004D5 08F48A            MOVEP             #$0800,X:DPSR           ; Clear timeout error bit
                            000800
1967      P:0004D5 P:0004D7 507000            MOVE              A0,X:EC_TO
                            000049
1968      P:0004D7 P:0004D9 0A702F            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1969      P:0004D9 P:0004DB 00000C            RTS
1970                                ERROR_TDIS
1971      P:0004DA P:0004DC 50F000            MOVE              X:EC_TDIS,A0
                            00004A
1972      P:0004DC P:0004DE 000008            INC     A
1973      P:0004DD P:0004DF 08F48A            MOVEP             #$0200,X:DPSR           ; Clear target disconnect bit
                            000200
1974      P:0004DF P:0004E1 507000            MOVE              A0,X:EC_TDIS
                            00004A
1975      P:0004E1 P:0004E3 0A702F            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
1976      P:0004E3 P:0004E5 00000C            RTS
1977                                ERROR_TAB
1978      P:0004E4 P:0004E6 50F000            MOVE              X:EC_TAB,A0
                            00004B
1979      P:0004E6 P:0004E8 000008            INC     A
1980      P:0004E7 P:0004E9 08F48A            MOVEP             #$0100,X:DPSR           ; Clear target abort error bit
                            000100
1981      P:0004E9 P:0004EB 507000            MOVE              A0,X:EC_TAB
                            00004B
1982      P:0004EB P:0004ED 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1983      P:0004ED P:0004EF 00000C            RTS
1984                                ERROR_MAB
1985      P:0004EE P:0004F0 50F000            MOVE              X:EC_MAB,A0
                            00004C
1986      P:0004F0 P:0004F2 000008            INC     A
1987      P:0004F1 P:0004F3 08F48A            MOVEP             #$0080,X:DPSR           ; Clear master abort error bit
                            000080
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 40



1988      P:0004F3 P:0004F5 507000            MOVE              A0,X:EC_MAB
                            00004C
1989      P:0004F5 P:0004F7 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1990      P:0004F7 P:0004F9 00000C            RTS
1991                                ERROR_DPER
1992      P:0004F8 P:0004FA 50F000            MOVE              X:EC_DPER,A0
                            00004D
1993      P:0004FA P:0004FC 000008            INC     A
1994      P:0004FB P:0004FD 08F48A            MOVEP             #$0040,X:DPSR           ; Clear data parity error bit
                            000040
1995      P:0004FD P:0004FF 507000            MOVE              A0,X:EC_DPER
                            00004D
1996      P:0004FF P:000501 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
1997      P:000501 P:000503 00000C            RTS
1998                                ERROR_APER
1999      P:000502 P:000504 50F000            MOVE              X:EC_APER,A0
                            00004E
2000      P:000504 P:000506 000008            INC     A
2001      P:000505 P:000507 08F48A            MOVEP             #$0020,X:DPSR           ; Clear address parity error bit
                            000020
2002      P:000507 P:000509 507000            MOVE              A0,X:EC_APER
                            00004E
2003      P:000509 P:00050B 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2004      P:00050B P:00050D 00000C            RTS
2005   
2006   
2007                                ;---------------------------------------------
2008                                BLOCK_TRANSFER
2009                                ;----------------------------------------------
2010                                ;   In:
2011                                ;   - BLOCK_DEST_HI:BLOCK_DEST_LO is PC RAM address
2012                                ;   - BLOCK_SIZE is packet size, in bytes
2013                                ;   - BLOCK_SRC is start of data in Y memory
2014                                ;  Out:
2015                                ;   - BLOCK_SIZE will be decremented to zero.
2016                                ;   - BLOCK_DEST_HI:LO will be incremented by BLOCK_SIZE
2017                                ;   - BLOCK_SRC will be incremented by BLOCK_SIZE/2
2018                                ;  Trashes:
2019                                ;   - A and B
2020   
2021   
2022      P:00050C P:00050E 0AF080            JMP     HOCK_TRANSFER
                            00081C
2023      P:00050E P:000510 000000            NOP
2024   
2025      
2026                                ;       MOVE    X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
2027   
2028                                ;       CMP     #0,A
2029      P:00050F P:000511 0AF0AA            JEQ     BLOCK_DONE
                            000553
2030   
2031      P:000511 P:000513 57F400            MOVE              #$0100,B                ; B1 = 256
                            000100
2032      P:000513 P:000515 000000            NOP
2033   
2034      P:000514 P:000516 200005            CMP     B,A                               ; A ? B
2035      P:000515 P:000517 0E1517            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
2036      P:000516 P:000518 21CF00            MOVE              A,B                     ;    B=A
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 41



2037                                BLOCK_TRANSFER1
2038      P:000517 P:000519 200014            SUB     B,A                               ; A -= B
2039      P:000518 P:00051A 014088            ADD     #0,B                              ; Clear carry bit
2040      P:000519 P:00051B 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            000042
2041      P:00051B P:00051D 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            000043
2042      P:00051D P:00051F 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
2043   
2044      
2045      P:00051E P:000520 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
2046      P:000520 P:000522 50F000            MOVE              X:BURST_SRC,A0
                            000046
2047      P:000522 P:000524 08C82F            MOVEP             A0,X:DSR0               ; DMA source
2048      P:000523 P:000525 200010            ADD     B,A
2049      P:000524 P:000526 00000B            DEC     B
2050      P:000525 P:000527 507000            MOVE              A0,X:BURST_SRC          ; BURST_SRC += BURST_SIZE/2
                            000046
2051   
2052      P:000527 P:000529 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
2053   
2054      
2055      P:000528 P:00052A 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
2056   
2057                                BLOCK_PCI
2058      
2059      P:00052A P:00052C 200013            CLR     A
2060      P:00052B P:00052D 20001B            CLR     B
2061      P:00052C P:00052E 51F000            MOVE              X:BURST_SIZE,B0
                            000043
2062      P:00052E P:000530 00000B            DEC     B
2063      P:00052F P:000531 014088            ADD     #0,B                              ; Clear carry
2064      P:000530 P:000532 0C1C85            ASR     #2,B,B                            ; BURST_SIZE / 4
2065      P:000531 P:000533 014088            ADD     #0,B                              ; Clear carry
2066      P:000532 P:000534 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
2067   
2068      P:000533 P:000535 50F000            MOVE              X:BURST_DEST_HI,A0
                            000045
2069      P:000535 P:000537 200010            ADD     B,A
2070      P:000536 P:000538 000000            NOP
2071      P:000537 P:000539 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
2072   
2073      P:000539 P:00053B 280700            MOVE              #$07,A0
2074      P:00053A P:00053C 014088            ADD     #0,B                              ; Clear carry
2075      P:00053B P:00053D 0C1D20            ASL     #16,A,A
2076      P:00053C P:00053E 51F000            MOVE              X:BURST_DEST_LO,B0
                            000044
2077      P:00053E P:000540 200010            ADD     B,A
2078      P:00053F P:000541 000000            NOP
2079   
2080      P:000540 P:000542 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
2081   
2082                                BLOCK_CHECK
2083      P:000541 P:000543 000000            NOP
2084      P:000542 P:000544 000000            NOP
2085      P:000543 P:000545 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for burst termination
                            000543
2086   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 42



2087      
2088      P:000545 P:000547 0A8AAE            JSET    #MDT,X:DPSR,BLOCK_OK
                            00054E
2089   
2090      P:000547 P:000549 0D04B2            JSR     MPCI_ERROR_RECOVERY
2091   
2092      P:000548 P:00054A 0A700E            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
2093      P:00054A P:00054C 0E8554            JCS     <BLOCK_RESTART
2094   
2095      P:00054B P:00054D 0A700F            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
2096      P:00054D P:00054F 0E8555            JCS     <BLOCK_RESUME
2097   
2098                                BLOCK_OK
2099      P:00054E P:000550 50F000            MOVE              X:BURST_SIZE,A0
                            000043
2100      P:000550 P:000552 0BF080            JSR     BLOCK_UPDATE
                            000568
2101      P:000552 P:000554 0C050C            JMP     BLOCK_TRANSFER                    ; Finish the block
2102                                BLOCK_DONE
2103      P:000553 P:000555 00000C            RTS                                       ; Done
2104   
2105                                BLOCK_RESTART
2106      P:000554 P:000556 0C052A            JMP     BLOCK_PCI                         ; Recalculate pci and resend
2107   
2108                                BLOCK_RESUME
2109      P:000555 P:000557 200013            CLR     A
2110      P:000556 P:000558 20001B            CLR     B
2111      P:000557 P:000559 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
2112      P:000558 P:00055A 0A8A8F            JCLR    #15,X:DPSR,BLOCK_RESUME1
                            00055C
2113   
2114      P:00055A P:00055C 20001B            CLR     B
2115      P:00055B P:00055D 000009            INC     B
2116   
2117                                BLOCK_RESUME1
2118      P:00055C P:00055E 000009            INC     B                                 ; We want N, not N-1.
2119      P:00055D P:00055F 014088            ADD     #0,B                              ; Clear carry
2120      P:00055E P:000560 0C1C20            ASR     #16,A,A
2121      P:00055F P:000561 200018            ADD     A,B                               ; B is words remaining
2122      P:000560 P:000562 014088            ADD     #0,B                              ; Clear carry
2123      P:000561 P:000563 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
2124      P:000562 P:000564 50F000            MOVE              X:BURST_SIZE,A0
                            000043
2125      P:000564 P:000566 200014            SUB     B,A                               ; A is words written
2126   
2127      P:000565 P:000567 0BF080            JSR     BLOCK_UPDATE
                            000568
2128      P:000567 P:000569 0C052A            JMP     BLOCK_PCI                         ; Recalculate pci and resend
2129   
2132                                BLOCK_UPDATE
2133      
2134      
2135   
2136      P:000568 P:00056A 210500            MOVE              A0,X1                   ; Save A
2137      P:000569 P:00056B 21CF00            MOVE              A,B                     ; Save A again...
2138   
2139      P:00056A P:00056C 62F000            MOVE              X:BURST_DEST_LO,R2      ; WRONG!!
                            000044
2140      P:00056C P:00056E 0BF080            JSR     ADD_HILO_ADDRESS                  ; This updates BURST_DEST
                            0008D4
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 43



2141   
2142      P:00056E P:000570 57F000            MOVE              X:BURST_SIZE,B
                            000043
2143      P:000570 P:000572 20006C            SUB     X1,B
2144      P:000571 P:000573 000000            NOP
2145      P:000572 P:000574 557000            MOVE              B1,X:BURST_SIZE
                            000043
2146   
2147      P:000574 P:000576 00000C            RTS
2148   
2149   
2150   
2151                                BOOTCODE_END
2152                                 BOOTEND_ADDR
2153      000575                              EQU     @CVI(BOOTCODE_END)
2154   
2155                                PROGRAM_END
2156      000575                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2157                                ;---------------------------------------------
2158   
2159   
2160                                ; --------------------------------------------------------------------
2161                                ; --------------- x memory parameter table ---------------------------
2162                                ; --------------------------------------------------------------------
2163   
2164      X:000000 P:000577                   ORG     X:VAR_TBL,P:
2165   
2166   
2167                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2168                                 VAR_TBL_START
2169      000575                              EQU     @LCV(L)-2
2170                                          ENDIF
2171   
2172                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2174                                          ENDIF
2175   
2176                                ; -----------------------------------------------
2177                                ; do not move these (X:0 --> x:3)
2178 d    X:000000 P:000577 000000  STATUS    DC      0
2179 d                               FRAME_COUNT
2180 d    X:000001 P:000578 000000            DC      0                                 ; used as a check....... increments for ever
y frame write.....must be cleared by host.
2181 d                               PRE_CORRUPT
2182 d    X:000002 P:000579 000000            DC      0
2183   
2184 d    X:000003 P:00057A 550104  REV_NUMBER DC     $550104                           ; byte 0 = minor revision #
2185                                                                                    ; byte 1 = major revision #
2186                                                                                    ; byte 2 = release Version (ascii letter)
2187 d    X:000004 P:00057B 250507  REV_DATA  DC      $250507                           ; data: day-month-year
2188 d    X:000005 P:00057C 2EF490  P_CHECKSUM DC     $2EF490                           ;**** DO NOT CHANGE
2189                                ; -------------------------------------------------
2190 d    X:000006 P:00057D 000000  WORD_COUNT DC     0                                 ; word count.  Number of words successfully 
writen to host in last packet.
2191 d    X:000007 P:00057E 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2192                                ; ----------------------------------------------------------------------------------------------
----------------
2193   
2194 d    X:000008 P:00057F 000000  DRXR_WD1  DC      0
2195 d    X:000009 P:000580 000000  DRXR_WD2  DC      0
2196 d    X:00000A P:000581 000000  DRXR_WD3  DC      0
2197 d    X:00000B P:000582 000000  DRXR_WD4  DC      0
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 44



2198 d    X:00000C P:000583 000000  DTXS_WD1  DC      0
2199 d    X:00000D P:000584 000000  DTXS_WD2  DC      0
2200 d    X:00000E P:000585 000000  DTXS_WD3  DC      0
2201 d    X:00000F P:000586 000000  DTXS_WD4  DC      0
2202   
2203 d    X:000010 P:000587 000000  PCI_WD1_1 DC      0
2204 d    X:000011 P:000588 000000  PCI_WD1_2 DC      0
2205 d    X:000012 P:000589 000000  PCI_WD2_1 DC      0
2206 d    X:000013 P:00058A 000000  PCI_WD2_2 DC      0
2207 d    X:000014 P:00058B 000000  PCI_WD3_1 DC      0
2208 d    X:000015 P:00058C 000000  PCI_WD3_2 DC      0
2209 d    X:000016 P:00058D 000000  PCI_WD4_1 DC      0
2210 d    X:000017 P:00058E 000000  PCI_WD4_2 DC      0
2211 d    X:000018 P:00058F 000000  PCI_WD5_1 DC      0
2212 d    X:000019 P:000590 000000  PCI_WD5_2 DC      0
2213 d    X:00001A P:000591 000000  PCI_WD6_1 DC      0
2214 d    X:00001B P:000592 000000  PCI_WD6_2 DC      0
2215   
2216   
2217 d    X:00001C P:000593 000000  HEAD_W1_1 DC      0
2218 d    X:00001D P:000594 000000  HEAD_W1_0 DC      0
2219 d    X:00001E P:000595 000000  HEAD_W2_1 DC      0
2220 d    X:00001F P:000596 000000  HEAD_W2_0 DC      0
2221 d    X:000020 P:000597 000000  HEAD_W3_1 DC      0
2222 d    X:000021 P:000598 000000  HEAD_W3_0 DC      0
2223 d    X:000022 P:000599 000000  HEAD_W4_1 DC      0
2224 d    X:000023 P:00059A 000000  HEAD_W4_0 DC      0
2225   
2226   
2227 d    X:000024 P:00059B 000000  REP_WD1   DC      0
2228 d    X:000025 P:00059C 000000  REP_WD2   DC      0
2229 d    X:000026 P:00059D 000000  REP_WD3   DC      0
2230 d    X:000027 P:00059E 000000  REP_WD4   DC      0
2231   
2232 d    X:000028 P:00059F 000000  SV_A0     DC      0
2233 d    X:000029 P:0005A0 000000  SV_A1     DC      0
2234 d    X:00002A P:0005A1 000000  SV_A2     DC      0
2235 d    X:00002B P:0005A2 000000  SV_B0     DC      0
2236 d    X:00002C P:0005A3 000000  SV_B1     DC      0
2237 d    X:00002D P:0005A4 000000  SV_B2     DC      0
2238 d    X:00002E P:0005A5 000000  SV_X0     DC      0
2239 d    X:00002F P:0005A6 000000  SV_X1     DC      0
2240 d    X:000030 P:0005A7 000000  SV_Y0     DC      0
2241 d    X:000031 P:0005A8 000000  SV_Y1     DC      0
2242   
2243 d    X:000032 P:0005A9 000000  SV_SR     DC      0                                 ; stauts register save.
2244   
2245 d    X:000033 P:0005AA 000000  ZERO      DC      0
2246 d    X:000034 P:0005AB 000001  ONE       DC      1
2247 d    X:000035 P:0005AC 000004  FOUR      DC      4
2248   
2249   
2250   
2251 d                               PACKET_SIZE_LOW
2252 d    X:000036 P:0005AD 000000            DC      0
2253 d                               PACKET_SIZE_HIH
2254 d    X:000037 P:0005AE 000000            DC      0
2255   
2256 d    X:000038 P:0005AF 00A5A5  PREAMB1   DC      $A5A5                             ; pramble 16-bit word....2 of which make up 
first preamble 32bit word
2257 d    X:000039 P:0005B0 005A5A  PREAMB2   DC      $5A5A                             ; preamble 16-bit word....2 of which make up
 second preamble 32bit word
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 45



2258 d    X:00003A P:0005B1 004441  DATA_WD   DC      $4441                             ; "DA"
2259 d    X:00003B P:0005B2 005250  REPLY_WD  DC      $5250                             ; "RP"
2260   
2261 d                               TOTAL_BUFFS
2262 d    X:00003C P:0005B3 000000            DC      0                                 ; total number of 512 buffers in packet
2263 d                               LEFT_TO_READ
2264 d    X:00003D P:0005B4 000000            DC      0                                 ; number of words (16 bit) left to read afte
r last 512 buffer
2265 d                               LEFT_TO_WRITE
2266 d    X:00003E P:0005B5 000000            DC      0                                 ; number of woreds (32 bit) to write to host
 i.e. half of those left over read
2267 d                               NUM_LEFTOVER_BLOCKS
2268 d    X:00003F P:0005B6 000000            DC      0                                 ; small block DMA burst transfer
2269   
2270 d                               DATA_DLY_VAL
2271 d    X:000040 P:0005B7 000000            DC      0                                 ; data delay value..  Delay added to first f
rame received after GO command
2272 d    X:000041 P:0005B8 000200  CONSTORE  DC      $200
2273   
2275   
2276 d    X:000042 P:0005B9 000000  BLOCK_SIZE DC     0
2277 d    X:000043 P:0005BA 000000  BURST_SIZE DC     0
2278 d                               BURST_DEST_LO
2279 d    X:000044 P:0005BB 000000            DC      0
2280 d                               BURST_DEST_HI
2281 d    X:000045 P:0005BC 000000            DC      0
2282 d    X:000046 P:0005BD 000000  BURST_SRC DC      0
2283   
2284 d    X:000047 P:0005BE 000000  DMA_ERRORS DC     0
2285 d    X:000048 P:0005BF 000000  EC_TRTY   DC      0
2286 d    X:000049 P:0005C0 000000  EC_TO     DC      0
2287 d    X:00004A P:0005C1 000000  EC_TDIS   DC      0
2288 d    X:00004B P:0005C2 000000  EC_TAB    DC      0
2289 d    X:00004C P:0005C3 000000  EC_MAB    DC      0
2290 d    X:00004D P:0005C4 000000  EC_DPER   DC      0
2291 d    X:00004E P:0005C5 000000  EC_APER   DC      0
2292   
2294   
2295 d    X:00004F P:0005C6 000000  QT_BASE_LO DC     0                                 ; PC buffer start address bits 15-0
2296 d    X:000050 P:0005C7 000000  QT_BASE_HI DC     0                                 ; PC buffer start address bits 31-16
2297 d                               QT_BUF_SIZE
2298 d    X:000051 P:0005C8 000000            DC      0                                 ; Separation of buffers, in bytes
2299 d    X:000052 P:0005C9 000000  QT_BUF_MAX DC     0                                 ; Number of buffers
2300 d                               QT_FRAME_SIZE
2301 d    X:000053 P:0005CA 000000            DC      0                                 ; Expected data packet size, in bytes
2302 d    X:000054 P:0005CB 000000  QT_INFORM DC      0                                 ; Number of packets to copy before informing
2303   
2304 d                               QT_BUF_HEAD
2305 d    X:000055 P:0005CC 000000            DC      0                                 ; Index of buf for next write
2306 d                               QT_BUF_TAIL
2307 d    X:000056 P:0005CD 000000            DC      0                                 ; Index at which we must not write
2308   
2309 d    X:000057 P:0005CE 000000  QT_DEST_LO DC     0                                 ; PC address for next write
2310 d    X:000058 P:0005CF 000000  QT_DEST_HI DC     0                                 ;
2311 d                               QT_INFORM_IDX
2312 d    X:000059 P:0005D0 000000            DC      0                                 ; Number of packets since last inform
2313 d    X:00005A P:0005D1 000000  QT_DROPS  DC      0                                 ; Dropped packets
2314   
2316                                ; PCI_BURST_SIZE                DC      $40     ; Should be < 4*latency assigned by OS
2317   
2318 d    X:00005B P:0005D2 000000  TEMP_PSIZE DC     0
2319   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 46



2320 d    X:00005C P:0005D3 000000  BDEBUG0   DC      0
2321 d    X:00005D P:0005D4 000000  BDEBUG1   DC      0
2322 d    X:00005E P:0005D5 000000  BDEBUG2   DC      0
2323 d    X:00005F P:0005D6 000000  BDEBUG3   DC      0
2324 d    X:000060 P:0005D7 000000  BDEBUG4   DC      0
2325 d    X:000061 P:0005D8 000000  BDEBUG5   DC      0
2326 d    X:000062 P:0005D9 000000  BDEBUG6   DC      0
2327 d    X:000063 P:0005DA 000000  BDEBUG7   DC      0
2328 d    X:000064 P:0005DB 000000  BDEBUG8   DC      0
2329 d    X:000065 P:0005DC 000000  BDEBUG9   DC      0
2330   
2331                                ;----------------------------------------------------------
2332   
2333   
2334   
2335                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2336                                 VAR_TBL_END
2337      0005DB                              EQU     @LCV(L)-2
2338                                          ENDIF
2339   
2340                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2342                                          ENDIF
2343   
2344                                 VAR_TBL_LENGTH
2345      000066                              EQU     VAR_TBL_END-VAR_TBL_START
2346   
2347   
2348                                          IF      @CVS(N,*)>=APPLICATION
2350                                          ENDIF
2351   
2352   
2353                                ;--------------------------------------------
2354                                ; APPLICATION AREA
2355                                ;---------------------------------------------
2356                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2357      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2358                                          ENDIF
2359   
2360                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2362                                          ENDIF
2363   
2364                                ; starts with no application loaded
2365                                ; so just reply with an error if we get a GOA command
2366      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2367      P:000802 P:000804 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
2368      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2369      P:000805 P:000807 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2370      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2371      P:000808 P:00080A 440E00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2372      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2373      P:00080B P:00080D 440F00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2374      P:00080C P:00080E 0D0469            JSR     <RESTORE_REGISTERS
2375      P:00080D P:00080F 0D0421            JSR     <PCI_MESSAGE_TO_HOST
2376      P:00080E P:000810 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS
2377      P:00080F P:000811 0C0173            JMP     PACKET_IN
2378   
2379   
2381   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 47



2383   
2384                                MAIN_LOOP_HACK_FESTIVAL
2385      P:000810 P:000812 0AF080            JMP     LE_HACK_FESTIVAL
                            000832
2386      P:000812 P:000814 0BF080            JSR     LE_BUFFER_INFORM                  ; BLAH
                            000837
2387   
2388                                ; BUFFER_INFORM
2389                                ;       ;; Note that this must JMP back to main loop!
2390                                ;       JSR     LE_BUFFER_INFORM
2391                                ;       JMP     PACKET_IN
2392   
2393                                NEW_PACKET_HANDLER
2394      P:000814 P:000816 0AF080            JMP     LE_NEW_PACKET_HANDLER
                            0008DF
2395                                QUIET_TRANSFER_SET
2396      P:000816 P:000818 0AF080            JMP     LE_QUIET_TRANSFER_SET
                            00084D
2397                                SYSTEM_RESET
2398      P:000818 P:00081A 0AF080            JMP     LE_SYSTEM_RESET
                            00081E
2399                                LE_BLOCK_UPDATE
2400      P:00081A P:00081C 000000            NOP
2401      P:00081B P:00081D 000000            NOP
2402                                ;       JMP     LE_BLOCK_UPDATE         ; Done with this...
2403                                HOCK_TRANSFER
2404      P:00081C P:00081E 0AF080            JMP     LE_BLOCK_TRANSFER
                            00099C
2405   
2407   
2408   
2409   
2410   
2411                                LE_SYSTEM_RESET
2412      
2413   
2414      P:00081E P:000820 0BF080            JSR     CLEAR_FIFO
                            000828
2415   
2416      
2417   
2418                                ;       MOVEP   #$0001C0,X:IPRC         ; Disable HF* FIFO interrupt
2419                                ;       MOVE    #$200,SR                ; Mask set up for reset switch only.
2420   
2421      P:000820 P:000822 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
2422      P:000821 P:000823 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
2423                                                                                    ; set to zero except for interrupts
2424      P:000823 P:000825 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
2425                                                                                    ; so first set to 0
2426      P:000824 P:000826 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
2427                                                                                    ; therefore,return to initialization
2428      P:000826 P:000828 000000            NOP
2429      P:000827 P:000829 000004            RTI                                       ; return from ISR - to START
2430   
2431                                CLEAR_FIFO
2432      P:000828 P:00082A 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
2433      P:00082A P:00082C 44F400            MOVE              #200000,X0
                            030D40
2434      P:00082C P:00082E 06C400            DO      X0,*+3
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 48



                            00082E
2435      P:00082E P:000830 000000            NOP
2436      P:00082F P:000831 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
2437      P:000831 P:000833 00000C            RTS
2438   
2439   
2440                                LE_HACK_FESTIVAL
2441      P:000832 P:000834 01CFB5            JSSET   #TCF,X:TCSR0,TIMER_ACTION
                            00098E
2442      P:000834 P:000836 0B00B2            JSSET   #QT_FLUSH,X:STATUS,LE_BUFFER_INFORM
                            000837
2443      P:000836 P:000838 00000C            RTS
2444   
2445                                LE_BUFFER_INFORM
2446   
2447      P:000837 P:000839 44F400            MOVE              #'QTI',X0               ; Quiet Transfer Inform
                            515449
2448      P:000839 P:00083B 440C00            MOVE              X0,X:<DTXS_WD1
2449   
2450      P:00083A P:00083C 44F000            MOVE              X:QT_BUF_HEAD,X0        ; Next write index
                            000055
2451      P:00083C P:00083E 440D00            MOVE              X0,X:<DTXS_WD2
2452   
2453      P:00083D P:00083F 44F000            MOVE              X:QT_BUF_TAIL,X0        ; Forbidden write index
                            000056
2454      P:00083F P:000841 440E00            MOVE              X0,X:<DTXS_WD3
2455   
2456      P:000840 P:000842 44F000            MOVE              X:QT_DROPS,X0           ; Dropped packet count
                            00005A
2457      P:000842 P:000844 440F00            MOVE              X0,X:<DTXS_WD4
2458   
2459   
2460      P:000843 P:000845 0A85A3            JSET    #DCTR_HF3,X:DCTR,INFORM_EXIT
                            00084C
2461      P:000845 P:000847 0A8981            JCLR    #STRQ,X:DSR,INFORM_EXIT
                            00084C
2462   
2463      P:000847 P:000849 0D0425            JSR     PCI_MESSAGE_TO_HOST_NOW
2464   
2465      P:000848 P:00084A 0A0012            BCLR    #QT_FLUSH,X:STATUS
2466      P:000849 P:00084B 240000            MOVE              #0,X0                   ; Reset inform index
2467      P:00084A P:00084C 447000            MOVE              X0,X:QT_INFORM_IDX
                            000059
2468                                INFORM_EXIT
2469      P:00084C P:00084E 00000C            RTS
2470   
2471                                LE_QUIET_TRANSFER_SET
2472      P:00084D P:00084F 0D047E            JSR     SAVE_REGISTERS                    ; save working registers
2473      P:00084E P:000850 0D043C            JSR     RD_DRXR                           ; read words from host write to HTXR
2474   
2475      P:00084F P:000851 44F400            MOVE              #'QTS',X0
                            515453
2476      P:000851 P:000853 0BF080            JSR     PREPARE_REPLY
                            0008B8
2477      P:000853 P:000855 0BF080            JSR     CHECK_COMMAND                     ; verify syntax
                            0008C2
2478      P:000855 P:000857 0AF0A2            JNE     QUIET_TRANSFER_SET_EXIT
                            0008B5
2479   
2480      P:000857 P:000859 568900            MOVE              X:DRXR_WD2,A            ; Parameter id
2481      P:000858 P:00085A 448A00            MOVE              X:DRXR_WD3,X0           ; First arg
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 49



2482      P:000859 P:00085B 458B00            MOVE              X:DRXR_WD4,X1           ; Second arg
2483   
2484      P:00085A P:00085C 0140C5            CMP     #'BAS',A
                            424153
2485      P:00085C P:00085E 0AF0AA            JEQ     QUIET_TRANSFER_SET_BASE
                            0008AF
2486   
2487      P:00085E P:000860 0140C5            CMP     #'DEL',A
                            44454C
2488      P:000860 P:000862 325100            MOVE              #QT_BUF_SIZE,R2
2489      P:000861 P:000863 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0008AC
2490   
2491      P:000863 P:000865 0140C5            CMP     #'NUM',A
                            4E554D
2492      P:000865 P:000867 325200            MOVE              #QT_BUF_MAX,R2
2493      P:000866 P:000868 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0008AC
2494   
2495      P:000868 P:00086A 0140C5            CMP     #'INF',A
                            494E46
2496      P:00086A P:00086C 325400            MOVE              #QT_INFORM,R2
2497      P:00086B P:00086D 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0008AC
2498   
2499      P:00086D P:00086F 0140C5            CMP     #'SIZ',A
                            53495A
2500      P:00086F P:000871 325300            MOVE              #QT_FRAME_SIZE,R2
2501      P:000870 P:000872 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0008AC
2502   
2503      P:000872 P:000874 0140C5            CMP     #'TAI',A
                            544149
2504      P:000874 P:000876 325600            MOVE              #QT_BUF_TAIL,R2
2505      P:000875 P:000877 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0008AC
2506   
2507      P:000877 P:000879 0140C5            CMP     #'HEA',A
                            484541
2508      P:000879 P:00087B 325500            MOVE              #QT_BUF_HEAD,R2
2509      P:00087A P:00087C 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0008AC
2510   
2511      P:00087C P:00087E 0140C5            CMP     #'DRO',A
                            44524F
2512      P:00087E P:000880 325A00            MOVE              #QT_DROPS,R2
2513      P:00087F P:000881 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0008AC
2514   
2515      P:000881 P:000883 0140C5            CMP     #'PER',A
                            504552
2516      P:000883 P:000885 62F400            MOVE              #TCPR0,R2
                            FFFF8D
2517      P:000885 P:000887 0AF0AA            JEQ     QUIET_TRANSFER_SET_R2
                            0008AC
2518   
2519      P:000887 P:000889 0140C5            CMP     #'FLU',A
                            464C55
2520      P:000889 P:00088B 0AF0AA            JEQ     QUIET_TRANSFER_SET_FLUSH
                            000895
2521   
2522      P:00088B P:00088D 0140C5            CMP     #'SET',A
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 50



                            534554
2523      P:00088D P:00088F 0AF0AA            JEQ     QUIET_TRANSFER_SET_ENABLED
                            00089D
2524   
2525      P:00088F P:000891 44F400            MOVE              #'MTE',X0
                            4D5445
2526      P:000891 P:000893 0BF080            JSR     SET_ERROR
                            0008C9
2527      P:000893 P:000895 0AF080            JMP     QUIET_TRANSFER_SET_EXIT
                            0008B5
2528   
2529                                QUIET_TRANSFER_SET_FLUSH
2530      P:000895 P:000897 0A0012            BCLR    #QT_FLUSH,X:STATUS
2531      P:000896 P:000898 208E00            MOVE              X0,A
2532      P:000897 P:000899 200003            TST     A
2533      P:000898 P:00089A 0AF0AA            JEQ     QUIET_TRANSFER_SET_EXIT
                            0008B5
2534      P:00089A P:00089C 0A0032            BSET    #QT_FLUSH,X:STATUS
2535      P:00089B P:00089D 0AF080            JMP     QUIET_TRANSFER_SET_EXIT
                            0008B5
2536   
2537                                QUIET_TRANSFER_SET_ENABLED
2538      P:00089D P:00089F 0A0011            BCLR    #QT_ENABLED,X:STATUS
2539      P:00089E P:0008A0 0BF080            JSR     TIMER_DISABLE
                            000988
2540      P:0008A0 P:0008A2 208E00            MOVE              X0,A
2541      P:0008A1 P:0008A3 200003            TST     A
2542      P:0008A2 P:0008A4 0AF0AA            JEQ     QUIET_TRANSFER_SET_EXIT
                            0008B5
2543      P:0008A4 P:0008A6 280000            MOVE              #0,A0
2544      P:0008A5 P:0008A7 0A0031            BSET    #QT_ENABLED,X:STATUS
2545      P:0008A6 P:0008A8 507000            MOVE              A0,X:TLR0
                            FFFF8E
2546      P:0008A8 P:0008AA 0BF080            JSR     TIMER_ENABLE
                            000982
2547      P:0008AA P:0008AC 0AF080            JMP     QUIET_TRANSFER_SET_EXIT
                            0008B5
2548   
2549                                QUIET_TRANSFER_SET_R2
2550      P:0008AC P:0008AE 446200            MOVE              X0,X:(R2)
2551      P:0008AD P:0008AF 0AF080            JMP     QUIET_TRANSFER_SET_EXIT
                            0008B5
2552   
2553                                QUIET_TRANSFER_SET_BASE
2554      P:0008AF P:0008B1 447000            MOVE              X0,X:QT_BASE_LO
                            00004F
2555      P:0008B1 P:0008B3 457000            MOVE              X1,X:QT_BASE_HI
                            000050
2556   
2557      P:0008B3 P:0008B5 0BF080            JSR     BUFFER_RESET
                            000962
2558                                ;fall through
2559                                ;       JMP     QUIET_TRANSFER_SET_EXIT
2560   
2561                                QUIET_TRANSFER_SET_EXIT
2562      P:0008B5 P:0008B7 0D0469            JSR     RESTORE_REGISTERS
2563      P:0008B6 P:0008B8 0D0421            JSR     PCI_MESSAGE_TO_HOST
2564      P:0008B7 P:0008B9 000004            RTI
2565   
2566   
2567                                PREPARE_REPLY
2568      
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 51



2569      
2570      P:0008B8 P:0008BA 50F400            MOVE              #'REP',A0
                            524550
2571      P:0008BA P:0008BC 440D00            MOVE              X0,X:DTXS_WD2           ; Command
2572      P:0008BB P:0008BD 500C00            MOVE              A0,X:DTXS_WD1
2573   
2574      P:0008BC P:0008BE 56F400            MOVE              #'ACK',A                ; Note this sets A0 = 0
                            41434B
2575      P:0008BE P:0008C0 000000            NOP
2576      P:0008BF P:0008C1 540E00            MOVE              A1,X:DTXS_WD3           ; ACK
2577      P:0008C0 P:0008C2 500F00            MOVE              A0,X:DTXS_WD4           ; no comment
2578      P:0008C1 P:0008C3 00000C            RTS
2579   
2580   
2581                                CHECK_COMMAND
2582      
2583      
2584      
2585   
2586      P:0008C2 P:0008C4 208E00            MOVE              X0,A
2587      P:0008C3 P:0008C5 578800            MOVE              X:DRXR_WD1,B
2588      P:0008C4 P:0008C6 20000D            CMP     A,B
2589      P:0008C5 P:0008C7 0AF0AA            JEQ     RTS_NOW
                            0008CD
2590      P:0008C7 P:0008C9 44F400            MOVE              #'CNE',X0               ; Command Name Error
                            434E45
2591                                ;fall through
2592                                ;       JMP     SET_ERROR
2593   
2594                                SET_ERROR
2595      
2596      
2597      P:0008C9 P:0008CB 50F400            MOVE              #'ERR',A0
                            455252
2598      P:0008CB P:0008CD 440F00            MOVE              X0,X:DTXS_WD4
2599      P:0008CC P:0008CE 500E00            MOVE              A0,X:DTXS_WD3
2600                                RTS_NOW
2601      P:0008CD P:0008CF 00000C            RTS
2602   
2603   
2604   
2608   
2609                                LOAD_HILO_ADDRESS
2610      
2611      
2612      P:0008CE P:0008D0 200013            CLR     A
2613      P:0008CF P:0008D1 50DA00            MOVE              X:(R2)+,A0
2614      P:0008D0 P:0008D2 44D200            MOVE              X:(R2)-,X0
2615      P:0008D1 P:0008D3 0C1940            INSERT  #$010010,X0,A
                            010010
2616      P:0008D3 P:0008D5 00000C            RTS
2617   
2618                                ADD_HILO_ADDRESS
2619      
2620      
2621   
2622      P:0008D4 P:0008D6 0D08CE            JSR     LOAD_HILO_ADDRESS
2623      P:0008D5 P:0008D7 200010            ADD     B,A
2624   
2625                                SAVE_HILO_ADDRESS
2626      
2627      
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 52



2628   
2629      P:0008D6 P:0008D8 445A00            MOVE              X0,X:(R2)+              ; pre-increment
2630      P:0008D7 P:0008D9 240000            MOVE              #0,X0
2631      P:0008D8 P:0008DA 0C1D11            ASL     #8,A,B
2632      P:0008D9 P:0008DB 0C1940            INSERT  #$008010,X0,A
                            008010
2633      P:0008DB P:0008DD 555200            MOVE              B1,X:(R2)-              ; store hi16
2634      P:0008DC P:0008DE 506200            MOVE              A0,X:(R2)
2635      P:0008DD P:0008DF 0C1C90            ASR     #8,B,A
2636      P:0008DE P:0008E0 00000C            RTS
2637   
2638   
2639                                ; TEST_ROUTINES                 ; Passed.
2640   
2641                                ;       MOVE    #$01,A1
2642                                ;       MOVE    #$020304,A0
2643                                ;       MOVE    #BDEBUG0,R2
2644   
2645                                ;       JSR     SAVE_HILO_ADDRESS
2646   
2647                                ;       JSR     LOAD_HILO_ADDRESS
2648                                ;       MOVE    #BDEBUG2,R2
2649                                ;       JSR     SAVE_HILO_ADDRESS
2650   
2651                                ;       MOVE    #$02,B1
2652                                ;       MOVE    #$000203,B0
2653                                ;       MOVE    #BDEBUG0,R2
2654                                ;       JSR     ADD_HILO_ADDRESS
2655   
2656                                ;       RTS
2657   
2658   
2659                                LE_NEW_PACKET_HANDLER
2660      
2661   
2662      
2663      
2664   
2665      
2666   
2667      P:0008DF P:0008E1 56A100            MOVE              X:HEAD_W3_0,A
2668   
2669      P:0008E0 P:0008E2 0140C5            CMP     #'RP',A
                            005250
2670      P:0008E2 P:0008E4 0AF0AA            JEQ     BUFFER_PACKET_RP
                            0008EA
2671      P:0008E4 P:0008E6 0140C5            CMP     #'DA',A
                            004441
2672      P:0008E6 P:0008E8 0AF0AA            JEQ     BUFFER_PACKET_DA
                            0008EB
2673   
2674      P:0008E8 P:0008EA 0AF080            JMP     QT_PTYPE_ERROR
                            00099B
2675   
2676                                BUFFER_PACKET_RP
2677   
2678      P:0008EA P:0008EC 0C01C7            JMP     MCE_PACKET                        ; Process in the usual way
2679   
2680                                BUFFER_PACKET_DA
2681   
2682      P:0008EB P:0008ED 0B0031            BTST    #QT_ENABLED,X:STATUS
2683      P:0008EC P:0008EE 0E01C7            JCC     MCE_PACKET                        ; Process in the usual way
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 53



2684   
2685      
2686   
2687                                ;       ;; This won't work until you re-order HEAD * 0,1
2688                                ;       MOVE    #HEAD_W4_1,R2           ; Note W4_0 is *after* W4_1 in memory
2689                                ;       JSR     LOAD_HILO_ADDRESS       ; Packet size, in word32
2690                                ;       ASL     #2,A,A                  ; Convert to bytes
2691                                ;       ADD     #0,B                    ; Clear carry
2692                                ;       ASL     #14,A,B                 ; B1 = size in bytes / 2^10
2693                                ;       MOVE    #0,X0
2694                                ;       INSERT  #$0E000A,X0,A           ; A0 = size in word32 % 2^10
2695   
2696      
2697      P:0008ED P:0008EF 200013            CLR     A
2698      P:0008EE P:0008F0 50A300            MOVE              X:HEAD_W4_0,A0          ; 0x54c
2699      P:0008EF P:0008F1 44A200            MOVE              X:HEAD_W4_1,X0          ; 0     ok
2700      P:0008F0 P:0008F2 0C1940            INSERT  #$010010,X0,A                     ;
                            010010
2701   
2702      P:0008F2 P:0008F4 000000            NOP
2703      P:0008F3 P:0008F5 507000            MOVE              A0,X:TEMP_PSIZE
                            00005B
2704   
2705      P:0008F5 P:0008F7 014088            ADD     #0,B                              ; Clear carry
2706      P:0008F6 P:0008F8 0C1D02            ASL     #1,A,A                            ;  * 2
2707      P:0008F7 P:0008F9 0C1D1F            ASL     #15,A,B                           ; B1 = size in bytes / 2^10
2708      P:0008F8 P:0008FA 240000            MOVE              #0,X0
2709      P:0008F9 P:0008FB 0C1940            INSERT  #$00E009,X0,A                     ; A0 = (size in bytes % 2^10) / 2
                            00E009
2710   
2711      P:0008FB P:0008FD 553C00            MOVE              B1,X:TOTAL_BUFFS
2712      P:0008FC P:0008FE 503D00            MOVE              A0,X:LEFT_TO_READ
2713   
2714                                BUFFER_PACKET_HALFS
2715      P:0008FD P:0008FF 310000            MOVE              #IMAGE_BUFFER,R1
2716      P:0008FE P:000900 063C00            DO      X:TOTAL_BUFFS,BUFFER_PACKET_SINGLES
                            000903
2717      P:000900 P:000902 0BF080            JSR     WAIT_FIFO_HALF
                            000922
2718      P:000902 P:000904 0BF080            JSR     TRANSFER_FIFO_HALF
                            000934
2719   
2720                                BUFFER_PACKET_SINGLES
2721      P:000904 P:000906 063D00            DO      X:LEFT_TO_READ,BUFFER_PACKET_SEND
                            000909
2722      P:000906 P:000908 0BF080            JSR     WAIT_FIFO_SINGLE
                            00092B
2723      P:000908 P:00090A 0BF080            JSR     TRANSFER_FIFO_SINGLE
                            000936
2724   
2725                                BUFFER_PACKET_SEND
2726      P:00090A P:00090C 56F000            MOVE              X:QT_BUF_HEAD,A
                            000055
2727      P:00090C P:00090E 014180            ADD     #1,A
2728      P:00090D P:00090F 57F000            MOVE              X:QT_BUF_MAX,B
                            000052
2729      P:00090F P:000911 20000D            CMP     A,B
2730      P:000910 P:000912 0AF0A1            JGE     BUFFER_PACKET_MATH
                            000913
2731      P:000912 P:000914 2E0000            MOVE              #0,A
2732                                BUFFER_PACKET_MATH
2733      P:000913 P:000915 57F000            MOVE              X:QT_BUF_TAIL,B
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 54



                            000056
2734      P:000915 P:000917 20000D            CMP     A,B
2735      P:000916 P:000918 0AF0AA            JEQ     BUFFER_PACKET_DROP                ; If yes, drop packet
                            00091B
2736   
2737      P:000918 P:00091A 0BF080            JSR     QT_DATA_PACKET                    ; and transfer
                            000939
2738   
2739                                BUFFER_PACKET_DONE
2740      P:00091A P:00091C 0C0173            JMP     PACKET_IN
2741   
2742                                BUFFER_PACKET_DROP
2743      P:00091B P:00091D 56F000            MOVE              X:QT_DROPS,A
                            00005A
2744      P:00091D P:00091F 014180            ADD     #1,A
2745      P:00091E P:000920 000000            NOP
2746      P:00091F P:000921 567000            MOVE              A,X:QT_DROPS
                            00005A
2747   
2748      P:000921 P:000923 0C091A            JMP     BUFFER_PACKET_DONE
2749   
2750   
2751   
2752                                WAIT_FIFO_HALF
2753      P:000922 P:000924 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
                            000938
2754      P:000924 P:000926 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Wait for half full+1
                            000922
2755      P:000926 P:000928 000000            NOP
2756      P:000927 P:000929 000000            NOP
2757      P:000928 P:00092A 01ADA1            JSET    #HF,X:PDRD,WAIT_FIFO_HALF         ; Protect against metastability
                            000922
2758      P:00092A P:00092C 00000C            RTS
2759   
2760                                WAIT_FIFO_SINGLE
2761      P:00092B P:00092D 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO
                            000216
2762      P:00092D P:00092F 01AD80            JCLR    #EF,X:PDRD,WAIT_FIFO_SINGLE
                            00092B
2763      P:00092F P:000931 000000            NOP
2764      P:000930 P:000932 000000            NOP
2765      P:000931 P:000933 01AD80            JCLR    #EF,X:PDRD,WAIT_FIFO_SINGLE       ; Protect against metastability
                            00092B
2766      P:000933 P:000935 00000C            RTS
2767   
2768                                TRANSFER_FIFO_HALF
2769      
2770      P:000934 P:000936 060082            DO      #512,TRANSFER_FIFO_DONE
                            000936
2771                                TRANSFER_FIFO_SINGLE
2772      P:000936 P:000938 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
2773                                TRANSFER_FIFO_DONE
2774      P:000937 P:000939 00000C            RTS
2775   
2776                                FATALITY_HANDLER
2777      P:000938 P:00093A 0C0100            JMP     START                             ; What could possibly go wrong?
2778   
2779   
2780                                QT_DATA_PACKET
2781      
2782      
2783   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 55



2784      
2785                                ;       MOVE    #HEAD_W4_1,R2           ; Note W4_0 is *after* W4_1 in memory
2786                                ;       JSR     LOAD_HILO_ADDRESS       ; Size, in word32
2787      P:000939 P:00093B 200013            CLR     A
2788      P:00093A P:00093C 50F000            MOVE              X:TEMP_PSIZE,A0
                            00005B
2789   
2790      P:00093C P:00093E 014088            ADD     #0,B                              ; Clear carry
2791      P:00093D P:00093F 0C1D04            ASL     #2,A,A                            ; Size, in bytes
2792   
2793      
2794      P:00093E P:000940 20001B            CLR     B
2795      P:00093F P:000941 51F000            MOVE              X:QT_FRAME_SIZE,B0
                            000053
2796      P:000941 P:000943 20000D            CMP     A,B
2797      P:000942 P:000944 0AF0A2            JNE     QT_FSIZE_ERROR
                            00099B
2798   
2799      
2800      P:000944 P:000946 517000            MOVE              B0,X:BLOCK_SIZE
                            000042
2801      P:000946 P:000948 557000            MOVE              B1,X:BURST_SRC          ; Y:0
                            000046
2802   
2803      P:000948 P:00094A 325700            MOVE              #QT_DEST_LO,R2
2804      P:000949 P:00094B 0D08CE            JSR     LOAD_HILO_ADDRESS
2805      P:00094A P:00094C 324400            MOVE              #BURST_DEST_LO,R2
2806      P:00094B P:00094D 0D08D6            JSR     SAVE_HILO_ADDRESS
2807   
2808      
2809      P:00094C P:00094E 0D050C            JSR     BLOCK_TRANSFER
2810   
2811      
2812      P:00094D P:00094F 0BF080            JSR     BUFFER_INCR
                            000952
2813   
2814      
2815      P:00094F P:000951 0BF080            JSR     BUFFER_INFORM_CHECK
                            00096A
2816   
2817   
2818      P:000951 P:000953 00000C            RTS
2819   
2820   
2821                                BUFFER_INCR
2822   
2823      P:000952 P:000954 56F000            MOVE              X:QT_BUF_HEAD,A
                            000055
2824      P:000954 P:000956 014180            ADD     #1,A
2825      P:000955 P:000957 57F000            MOVE              X:QT_BUF_MAX,B
                            000052
2826      P:000957 P:000959 20000D            CMP     A,B
2827      P:000958 P:00095A 0AF0AF            JLE     BUFFER_RESET
                            000962
2828   
2829      P:00095A P:00095C 567000            MOVE              A,X:QT_BUF_HEAD
                            000055
2830   
2831      P:00095C P:00095E 20001B            CLR     B
2832      P:00095D P:00095F 51F000            MOVE              X:QT_BUF_SIZE,B0
                            000051
2833      P:00095F P:000961 325700            MOVE              #QT_DEST_LO,R2
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 56



2834      P:000960 P:000962 0D08D4            JSR     ADD_HILO_ADDRESS
2835   
2836      P:000961 P:000963 00000C            RTS
2837   
2838                                BUFFER_RESET
2839      P:000962 P:000964 324F00            MOVE              #QT_BASE_LO,R2
2840      P:000963 P:000965 0D08CE            JSR     LOAD_HILO_ADDRESS
2841      P:000964 P:000966 325700            MOVE              #QT_DEST_LO,R2
2842      P:000965 P:000967 0D08D6            JSR     SAVE_HILO_ADDRESS
2843   
2844      P:000966 P:000968 240000            MOVE              #0,X0
2845      P:000967 P:000969 447000            MOVE              X0,X:QT_BUF_HEAD
                            000055
2846      P:000969 P:00096B 00000C            RTS
2847   
2848                                BUFFER_INFORM_CHECK
2849      P:00096A P:00096C 56F000            MOVE              X:QT_INFORM_IDX,A
                            000059
2850      P:00096C P:00096E 014180            ADD     #1,A
2851      P:00096D P:00096F 57F000            MOVE              X:QT_INFORM,B
                            000054
2852      P:00096F P:000971 20000D            CMP     A,B
2853      P:000970 P:000972 0AF0A7            JGT     BUFFER_INFORM_OK
                            000973
2854      P:000972 P:000974 0A0032            BSET    #QT_FLUSH,X:STATUS
2855                                BUFFER_INFORM_OK
2856      P:000973 P:000975 567000            MOVE              A,X:QT_INFORM_IDX
                            000059
2857      P:000975 P:000977 00000C            RTS
2858   
2859   
2861   
2862      
2863                                TIMER_CONFIG
2864      
2865   
2866      
2867      
2868   
2869      P:000976 P:000978 44F400            MOVE              #$000200,X0             ; 'Reload' mode
                            000200
2870      P:000978 P:00097A 200013            CLR     A
2871      P:000979 P:00097B 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2872      P:00097B P:00097D 04C88E            MOVEP             A0,X:TLR0
2873      P:00097C P:00097E 50F400            MOVE              #10000000,A0            ; This is 5 Hz, apparently.
                            989680
2874      P:00097E P:000980 000000            NOP
2875      P:00097F P:000981 507000            MOVE              A0,X:TCPR0
                            FFFF8D
2876   
2877      P:000981 P:000983 00000C            RTS
2878   
2879                                TIMER_ENABLE
2880      P:000982 P:000984 44F400            MOVE              #$000201,X0             ; Enable
                            000201
2881      P:000984 P:000986 000000            NOP
2882      P:000985 P:000987 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2883      P:000987 P:000989 00000C            RTS
2884   
2885                                TIMER_DISABLE
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 57



2886      P:000988 P:00098A 44F400            MOVE              #$300200,X0             ; Clear TOF, TCF, disable timer.
                            300200
2887      P:00098A P:00098C 000000            NOP
2888      P:00098B P:00098D 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2889      P:00098D P:00098F 00000C            RTS
2890   
2891                                TIMER_ACTION
2892      P:00098E P:000990 56F000            MOVE              X:QT_INFORM_IDX,A
                            000059
2893      P:000990 P:000992 44F400            MOVE              #$300201,X0             ; Clear TOF, TCF, leave timer enabled.
                            300201
2894      P:000992 P:000994 000000            NOP
2895      P:000993 P:000995 447000            MOVE              X0,X:TCSR0
                            FFFF8F
2896      P:000995 P:000997 0140C5            CMP     #>0,A
                            000000
2897      P:000997 P:000999 0AF0AA            JEQ     ESCAPE
                            00099A
2898      P:000999 P:00099B 0A0032            BSET    #QT_FLUSH,X:STATUS
2899                                ESCAPE
2900      P:00099A P:00099C 00000C            RTS
2901   
2902   
2903                                QT_PTYPE_ERROR
2904                                QT_FSIZE_ERROR
2905      
2906   
2907      P:00099B P:00099D 00000C            RTS
2908   
2909   
2910                                ;----------------------------------------------
2911                                LE_BLOCK_TRANSFER
2912                                ;----------------------------------------------
2913                                ;   In:
2914                                ;   - BLOCK_DEST_HI:BLOCK_DEST_LO is PC RAM address
2915                                ;   - BLOCK_SIZE is packet size, in bytes
2916                                ;   - BLOCK_SRC is start of data in Y memory
2917                                ;  Out:
2918                                ;   - BLOCK_SIZE will be decremented to zero.
2919                                ;   - BLOCK_DEST_HI:LO will be incremented by BLOCK_SIZE
2920                                ;   - BLOCK_SRC will be incremented by BLOCK_SIZE/2
2921                                ;  Trashes:
2922                                ;   - A and B
2923   
2924      P:00099C P:00099E 44F400            MOVE              #'HEY',X0               ; Information
                            484559
2925      P:00099E P:0009A0 440C00            MOVE              X0,X:<DTXS_WD1
2926   
2927      P:00099F P:0009A1 44F000            MOVE              X:BLOCK_SIZE,X0
                            000042
2928      P:0009A1 P:0009A3 440D00            MOVE              X0,X:<DTXS_WD2
2929   
2930      P:0009A2 P:0009A4 44F000            MOVE              X:BURST_SRC,X0
                            000046
2931      P:0009A4 P:0009A6 440E00            MOVE              X0,X:<DTXS_WD3
2932   
2933      P:0009A5 P:0009A7 44F000            MOVE              X:BURST_DEST_LO,X0
                            000044
2934      P:0009A7 P:0009A9 440F00            MOVE              X0,X:<DTXS_WD4
2935   
2936                                ;       JSR     PCI_MESSAGE_TO_HOST     ; notify host of packet
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 58



2937   
2938                                HOCK_TRANSFER_NONOTE
2939   
2940      
2941      P:0009A8 P:0009AA 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            000042
2942   
2943      P:0009AA P:0009AC 014085            CMP     #0,A
2944      P:0009AB P:0009AD 0AF0AA            JEQ     HOCK_DONE
                            000A01
2945   
2946      
2947   
2948                                ;       CLR     B
2949                                ;       MOVE    X:PCI_BURST_SIZE,B1
2950      P:0009AD P:0009AF 57F400            MOVE              #>$000040,B
                            000040
2951   
2952      P:0009AF P:0009B1 44F400            MOVE              #'HEY',X0               ; Information
                            484559
2953      P:0009B1 P:0009B3 440C00            MOVE              X0,X:<DTXS_WD1
2954      P:0009B2 P:0009B4 24AA00            MOVE              #$aa0000,X0
2955      P:0009B3 P:0009B5 440D00            MOVE              X0,X:<DTXS_WD2
2956      P:0009B4 P:0009B6 218400            MOVE              A1,X0
2957      P:0009B5 P:0009B7 440E00            MOVE              X0,X:<DTXS_WD3
2958      P:0009B6 P:0009B8 21A400            MOVE              B1,X0
2959      P:0009B7 P:0009B9 440F00            MOVE              X0,X:<DTXS_WD4
2960                                ;       JSR     PCI_MESSAGE_TO_HOST     ; notify host of packet
2961   
2962      P:0009B8 P:0009BA 200005            CMP     B,A                               ; A ? B
2963      P:0009B9 P:0009BB 0E19BB            JGE     <HOCK_TRANSFER1                   ; jump if A >= B
2964      P:0009BA P:0009BC 21CF00            MOVE              A,B
2965                                HOCK_TRANSFER1
2966      P:0009BB P:0009BD 200014            SUB     B,A                               ; A -= B
2967      P:0009BC P:0009BE 014088            ADD     #0,B                              ; Clear carry bit
2968      P:0009BD P:0009BF 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            000042
2969      P:0009BF P:0009C1 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            000043
2970      P:0009C1 P:0009C3 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
2971   
2972      
2973      P:0009C2 P:0009C4 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
2974      P:0009C4 P:0009C6 50F000            MOVE              X:BURST_SRC,A0
                            000046
2975      P:0009C6 P:0009C8 08C82F            MOVEP             A0,X:DSR0               ; DMA source
2976      P:0009C7 P:0009C9 200010            ADD     B,A
2977      P:0009C8 P:0009CA 00000B            DEC     B
2978      P:0009C9 P:0009CB 507000            MOVE              A0,X:BURST_SRC          ; BURST_SRC += BURST_SIZE/2
                            000046
2979   
2980      P:0009CB P:0009CD 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
2981   
2982      
2983      P:0009CC P:0009CE 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
2984   
2985                                HOCK_PCI
2986      
2987      P:0009CE P:0009D0 200013            CLR     A
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 59



2988      P:0009CF P:0009D1 20001B            CLR     B
2989      P:0009D0 P:0009D2 51F000            MOVE              X:BURST_SIZE,B0         ; B = n8
                            000043
2990      P:0009D2 P:0009D4 00000B            DEC     B                                 ; n8 - 1
2991      P:0009D3 P:0009D5 014088            ADD     #0,B                              ; Clear carry
2992      P:0009D4 P:0009D6 0C1C85            ASR     #2,B,B                            ; (n8 - 1)/4 = n32 - 1
2993      P:0009D5 P:0009D7 014088            ADD     #0,B                              ; Clear carry
2994      P:0009D6 P:0009D8 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
2995   
2996      P:0009D7 P:0009D9 50F000            MOVE              X:BURST_DEST_HI,A0
                            000045
2997   
2998      P:0009D9 P:0009DB 517000            MOVE              B0,X:BDEBUG0
                            00005C
2999      P:0009DB P:0009DD 557000            MOVE              B1,X:BDEBUG1
                            00005D
3000      P:0009DD P:0009DF 507000            MOVE              A0,X:BDEBUG2
                            00005E
3001      P:0009DF P:0009E1 547000            MOVE              A1,X:BDEBUG3
                            00005F
3002   
3003      P:0009E1 P:0009E3 200010            ADD     B,A
3004      P:0009E2 P:0009E4 000000            NOP
3005      P:0009E3 P:0009E5 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
3006   
3007      P:0009E5 P:0009E7 507000            MOVE              A0,X:BDEBUG4
                            000060
3008      P:0009E7 P:0009E9 547000            MOVE              A1,X:BDEBUG5
                            000061
3009   
3010      P:0009E9 P:0009EB 280700            MOVE              #$07,A0
3011      P:0009EA P:0009EC 014088            ADD     #0,B                              ; Clear carry
3012      P:0009EB P:0009ED 0C1D20            ASL     #16,A,A
3013      P:0009EC P:0009EE 51F000            MOVE              X:BURST_DEST_LO,B0
                            000044
3014      P:0009EE P:0009F0 200010            ADD     B,A
3015      P:0009EF P:0009F1 000000            NOP
3016   
3017      P:0009F0 P:0009F2 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
3018   
3019                                HOCK_CHECK
3020      P:0009F1 P:0009F3 000000            NOP
3021      P:0009F2 P:0009F4 000000            NOP
3022      P:0009F3 P:0009F5 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for burst termination
                            0009F3
3023   
3024      
3025      P:0009F5 P:0009F7 0A8AAE            JSET    #MDT,X:DPSR,HOCK_OK
                            0009FC
3026   
3027      P:0009F7 P:0009F9 0D04B2            JSR     MPCI_ERROR_RECOVERY
3028   
3029      P:0009F8 P:0009FA 0A000E            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
3030      P:0009F9 P:0009FB 0E8A02            JCS     <HOCK_RESTART
3031   
3032      P:0009FA P:0009FC 0A000F            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
3033      P:0009FB P:0009FD 0E8A03            JCS     <HOCK_RESUME
3034   
3035                                HOCK_OK
3036      P:0009FC P:0009FE 50F000            MOVE              X:BURST_SIZE,A0         ; Pass # of words written to updater
                            000043
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 60



3037      P:0009FE P:000A00 0BF080            JSR     HOCK_UPDATE
                            000A16
3038      P:000A00 P:000A02 0C09A8            JMP     HOCK_TRANSFER_NONOTE              ; Finish the block
3039                                HOCK_DONE
3040      P:000A01 P:000A03 00000C            RTS                                       ; Done
3041   
3042                                HOCK_RESTART
3043      P:000A02 P:000A04 0C09CE            JMP     HOCK_PCI                          ; Recalculate pci and resend
3044   
3045                                HOCK_RESUME
3046      P:000A03 P:000A05 200013            CLR     A
3047      P:000A04 P:000A06 20001B            CLR     B
3048      P:000A05 P:000A07 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
3049   
3050      P:000A06 P:000A08 0A8A8F            JCLR    #15,X:DPSR,HOCK_RESUME1
                            000A0A
3051   
3052      P:000A08 P:000A0A 20001B            CLR     B
3053      P:000A09 P:000A0B 000009            INC     B
3054   
3055                                HOCK_RESUME1
3056   
3057      P:000A0A P:000A0C 000009            INC     B                                 ; We want N, not N-1.
3058      P:000A0B P:000A0D 014088            ADD     #0,B                              ; Clear carry
3059      P:000A0C P:000A0E 0C1C20            ASR     #16,A,A
3060      P:000A0D P:000A0F 200018            ADD     A,B                               ; B is words remaining
3061      P:000A0E P:000A10 014088            ADD     #0,B                              ; Clear carry
3062      P:000A0F P:000A11 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
3063      P:000A10 P:000A12 50F000            MOVE              X:BURST_SIZE,A0
                            000043
3064      P:000A12 P:000A14 200014            SUB     B,A                               ; A is words written
3065   
3066      P:000A13 P:000A15 0BF080            JSR     HOCK_UPDATE
                            000A16
3067      P:000A15 P:000A17 0C09CE            JMP     HOCK_PCI                          ; Recalculate pci and resend
3068   
3071                                HOCK_UPDATE
3072      
3073      
3074   
3075                                ;       MOVE    X:BURST_SIZE,X0
3076                                ;       MOVE    X0,X:<DTXS_WD3
3077   
3078      P:000A16 P:000A18 210500            MOVE              A0,X1                   ; Save A
3079      P:000A17 P:000A19 210900            MOVE              A0,B0                   ; Save A again...
3080      P:000A18 P:000A1A 218D00            MOVE              A1,B1                   ; Save A again...
3081      P:000A19 P:000A1B 000000            NOP
3082                                ;       MOVE    B0,X:BDEBUG0
3083                                ;       MOVE    A0,X:BDEBUG1
3084   
3085      P:000A1A P:000A1C 324400            MOVE              #BURST_DEST_LO,R2       ; WRONG!! WHY DID THIS EVER WORK AT ALL!?
3086      P:000A1B P:000A1D 0D08D4            JSR     ADD_HILO_ADDRESS                  ; This updates BURST_DEST
3087   
3088      P:000A1C P:000A1E 57F000            MOVE              X:BURST_SIZE,B
                            000043
3089      P:000A1E P:000A20 20006C            SUB     X1,B
3090      P:000A1F P:000A21 000000            NOP
3091      P:000A20 P:000A22 557000            MOVE              B1,X:BURST_SIZE
                            000043
3092   
3093                                ;       MOVE    #'HEY',X0               ; Information
3094                                ;       MOVE    X0,X:<DTXS_WD1
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  01:15:10  main.asm  Page 61



3095   
3096                                ;       MOVE    #$FF0000,X0
3097                                ;       MOVE    X0,X:<DTXS_WD2
3098   
3099                                ;       MOVE    X:BURST_SIZE,X0
3100                                ;       MOVE    X0,X:<DTXS_WD4
3101   
3102                                ;       JSR     PCI_MESSAGE_TO_HOST     ; notify host of packet
3103   
3104      P:000A22 P:000A24 00000C            RTS
3105   
3106      000A25                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM
3107   
3108   
3109   

0    Errors
0    Warnings


