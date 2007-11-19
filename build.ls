Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  build.asm  Page 1



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
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  header.asm  Page 2



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
106                        ; HST timeout recovery....
107    
108       000200           MAX_DUMP  EQU     512                               ; if HST timeout.. max number that could be in FIFO i
s 511..
109       001000           DUMP_BUFF EQU     $1000                             ; store in Y memory above normal data buffer: in off-
chip RAM
110    
111    
112    
113                        ; Various addressing control registers
114       FFFFFB           BCR       EQU     $FFFFFB                           ; Bus Control Register
115       FFFFFA           DCR       EQU     $FFFFFA                           ; DRAM Control Register
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  header.asm  Page 3



116       FFFFF9           AAR0      EQU     $FFFFF9                           ; Address Attribute Register, channel 0
117       FFFFF8           AAR1      EQU     $FFFFF8                           ; Address Attribute Register, channel 1
118       FFFFF7           AAR2      EQU     $FFFFF7                           ; Address Attribute Register, channel 2
119       FFFFF6           AAR3      EQU     $FFFFF6                           ; Address Attribute Register, channel 3
120       FFFFFD           PCTL      EQU     $FFFFFD                           ; PLL control register
121       FFFFFE           IPRP      EQU     $FFFFFE                           ; Interrupt Priority register - Peripheral
122       FFFFFF           IPRC      EQU     $FFFFFF                           ; Interrupt Priority register - Core
123    
124                        ; PCI control register
125       FFFFCD           DTXS      EQU     $FFFFCD                           ; DSP Slave transmit data FIFO
126       FFFFCC           DTXM      EQU     $FFFFCC                           ; DSP Master transmit data FIFO
127       FFFFCB           DRXR      EQU     $FFFFCB                           ; DSP Receive data FIFO
128       FFFFCA           DPSR      EQU     $FFFFCA                           ; DSP PCI Status Register
129       FFFFC9           DSR       EQU     $FFFFC9                           ; DSP Status Register
130       FFFFC8           DPAR      EQU     $FFFFC8                           ; DSP PCI Address Register
131       FFFFC7           DPMC      EQU     $FFFFC7                           ; DSP PCI Master Control Register
132       FFFFC6           DPCR      EQU     $FFFFC6                           ; DSP PCI Control Register
133       FFFFC5           DCTR      EQU     $FFFFC5                           ; DSP Control Register
134    
135                        ; Port E is the Synchronous Communications Interface (SCI) port
136       FFFF9F           PCRE      EQU     $FFFF9F                           ; Port Control Register
137       FFFF9E           PRRE      EQU     $FFFF9E                           ; Port Direction Register
138       FFFF9D           PDRE      EQU     $FFFF9D                           ; Port Data Register
139    
140                        ; Various PCI register bit equates
141       000001           STRQ      EQU     1                                 ; Slave transmit data request (DSR)
142       000002           SRRQ      EQU     2                                 ; Slave receive data request (DSR)
143       000017           HACT      EQU     23                                ; Host active, low true (DSR)
144       000001           MTRQ      EQU     1                                 ; Set whem master transmitter is not full (DPSR)
145       000004           MARQ      EQU     4                                 ; Master address request (DPSR)
146       000002           MRRQ      EQU     2                                 ; Master Receive Request (DPSR)
147       00000A           TRTY      EQU     10                                ; PCI Target Retry (DPSR)
148    
149       000005           APER      EQU     5                                 ; Address parity error
150       000006           DPER      EQU     6                                 ; Data parity error
151       000007           MAB       EQU     7                                 ; Master Abort
152       000008           TAB       EQU     8                                 ; Target Abort
153       000009           TDIS      EQU     9                                 ; Target Disconnect
154       00000B           TO        EQU     11                                ; Timeout
155       00000E           MDT       EQU     14                                ; Master Data Transfer complete
156       000002           SCLK      EQU     2                                 ; SCLK = transmitter special code
157    
158                        ; bits in DPMC
159    
160       000017           FC1       EQU     23
161       000016           FC0       EQU     22
162    
163    
164                        ; DMA register definitions
165       FFFFEF           DSR0      EQU     $FFFFEF                           ; Source address register
166       FFFFEE           DDR0      EQU     $FFFFEE                           ; Destination address register
167       FFFFED           DCO0      EQU     $FFFFED                           ; Counter register
168       FFFFEC           DCR0      EQU     $FFFFEC                           ; Control register
169    
170                        ; The DCTR host flags are written by the DSP and read by PCI host
171       000003           DCTR_HF3  EQU     3                                 ; used as a semiphore for INTA handshaking
172       000004           DCTR_HF4  EQU     4                                 ;
173       000005           DCTR_HF5  EQU     5                                 ;
174       000006           INTA      EQU     6                                 ; Request PCI interrupt
175    
176                        ; The DSR host flags are written by the PCI host and read by the DSP
177       000004           DSR_BUF0  EQU     4                                 ; PCI host sets this when copying buffer 0
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  header.asm  Page 4



178       000005           DSR_BUF1  EQU     5                                 ; PCI host sets this when copying buffer 1
179    
180                        ; DPCR bit definitions
181       00000E           CLRT      EQU     14                                ; Clear transmitter
182       000012           MACE      EQU     18                                ; Master access counter enable
183       000015           IAE       EQU     21                                ; Insert Address Enable
184    
185                        ; Addresses of ESSI port
186       FFFFBC           TX00      EQU     $FFFFBC                           ; Transmit Data Register 0
187       FFFFB7           SSISR0    EQU     $FFFFB7                           ; Status Register
188       FFFFB6           CRB0      EQU     $FFFFB6                           ; Control Register B
189       FFFFB5           CRA0      EQU     $FFFFB5                           ; Control Register A
190    
191                        ; SSI Control Register A Bit Flags
192       000006           TDE       EQU     6                                 ; Set when transmitter data register is empty
193    
194                        ; Miscellaneous addresses
195       FFFFFF           RDFIFO    EQU     $FFFFFF                           ; Read the FIFO for incoming fiber optic data
196       FFFF8F           TCSR0     EQU     $FFFF8F                           ; Triper timer control and status register 0
197       FFFF8B           TCSR1     EQU     $FFFF8B                           ; Triper timer control and status register 1
198       FFFF87           TCSR2     EQU     $FFFF87                           ; Triper timer control and status register 2
199    
200                        ;***************************************************************
201                        ; Phase Locked Loop initialization
202       050003           PLL_INIT  EQU     $050003                           ; PLL = 25 MHz x 4 = 100 MHz
203                        ;****************************************************************
204    
205                        ; Port C is Enhanced Synchronous Serial Port 0
206       FFFFBF           PCRC      EQU     $FFFFBF                           ; Port C Control Register
207       FFFFBE           PRRC      EQU     $FFFFBE                           ; Port C Data direction Register
208       FFFFBD           PDRC      EQU     $FFFFBD                           ; Port C GPIO Data Register
209    
210                        ; Port D is Enhanced Synchronous Serial Port 1
211       FFFFAF           PCRD      EQU     $FFFFAF                           ; Port D Control Register
212       FFFFAE           PRRD      EQU     $FFFFAE                           ; Port D Data direction Register
213       FFFFAD           PDRD      EQU     $FFFFAD                           ; Port D GPIO Data Register
214    
215                        ; Bit number definitions of GPIO pins on Port C
216       000002           ROM_FIFO  EQU     2                                 ; Select ROM or FIFO accesses for AA1
217    
218                        ; Bit number definitions of GPIO pins on Port D
219       000000           EF        EQU     0                                 ; FIFO Empty flag, low true
220       000001           HF        EQU     1                                 ; FIFO half full flag, low true
221       000002           RS        EQU     2                                 ; FIFO reset signal, low true
222       000003           FSYNC     EQU     3                                 ; High during image transmission
223       000004           AUX1      EQU     4                                 ; enable/disable byte swapping
224       000005           WRFIFO    EQU     5                                 ; Low true if FIFO is being written to
225    
226    
227                        ; Errors - self test application
228    
229       000000           Y_MEM_ER  EQU     0                                 ; y memory corrupted
230       000001           X_MEM_ER  EQU     1                                 ; x memory corrupted
231       000002           P_MEM_ER  EQU     2                                 ; p memory corrupted
232       000003           FO_EMPTY  EQU     3                                 ; no transmitted data in FIFO
233    
234       000004           FO_OVER   EQU     4                                 ; too much data received
235       000005           FO_UNDER  EQU     5                                 ; not enough data receiv
236       000006           FO_RX_ER  EQU     6                                 ; received data in FIFO incorrect.
237       000007           DEBUG     EQU     7                                 ; debug bit
238    
239    
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  header.asm  Page 5



240    
241    
242                                  INCLUDE 'init.asm'
243                              COMMENT *
244    
245                        This is the code which is executed first after power-up etc.
246                        It sets all the internal registers to their operating values,
247                        sets up the ISR vectors and inialises the hardware etc.
248    
249                        Project:     SCUBA 2
250                        Author:      DAVID ATKINSON
251                        Target:      250MHz SDSU PCI card - DSP56301
252                        Controller:  For use with SCUBA 2 Multichannel Electronics
253    
254                        Assembler directives:
255                                ROM=EEPROM => EEPROM CODE
256                                ROM=ONCE => ONCE CODE
257    
258                                *
259                                  PAGE    132                               ; Printronix page width - 132 columns
260                                  OPT     CEX                               ; print DC evaluations
261    
**** 262 [init.asm 20]:  INCLUDE PCI_initialisation.asm HERE  
262                                  MSG     ' INCLUDE PCI_initialisation.asm HERE  '
263    
264                        ; The EEPROM boot code expects first to read 3 bytes specifying the number of
265                        ; program words, then 3 bytes specifying the address to start loading the
266                        ; program words and then 3 bytes for each program word to be loaded.
267                        ; The program words will be condensed into 24 bit words and stored in contiguous
268                        ; PRAM memory starting at the specified starting address. Program execution
269                        ; starts from the same address where loading started.
270    
271                        ; Special address for two words for the DSP to bootstrap code from the EEPROM
272                                  IF      @SCP("ROM","ROM")                 ; Boot from ROM on power-on
273       P:000000 P:000000                   ORG     P:0,P:0
274  d    P:000000 P:000000 000810            DC      END_ADR-INIT-2                    ; Number of boot words
275  d    P:000001 P:000001 000000            DC      INIT                              ; Starting address
276       P:000000 P:000002                   ORG     P:0,P:2
277       P:000000 P:000002 0C0030  INIT      JMP     <INIT_PCI                         ; Configure PCI port
278       P:000001 P:000003 000000            NOP
279                                           ENDIF
280    
281    
282                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
283                                 ; command converter
284                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
288                                           ENDIF
289    
290                                 ; Vectored interrupt table, addresses at the beginning are reserved
291  d    P:000002 P:000004 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0       ; $02-$0f Reserved
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
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  init.asm  Page 6



     d                      000000
292  d    P:000010 P:000012 000000            DC      0,0                               ; $10-$13 Reserved
     d                      000000
293    
294                                 ; FIFO HF* flag interrupt vector is here at $12 - this is connected to the
295                                 ; IRQB* interrupt line so its ISR vector must be here
296  d    P:000012 P:000014 000000            DC      0,0                               ; $was ld scatter routine ...HF*
     d                      000000
297    
298                                 ; a software reset button on the font panel of the card is connected to the IRQC*
299                                 ; line which if pressed causes the DSP to jump to an ISR which causes the program
300                                 ; counter to the beginning of the program INIT and sets the stack pointer to TOP.
301       P:000014 P:000016 0BF080            JSR     CLEAN_UP_PCI                      ; $14 - Software reset switch
                            00022A
302    
303  d    P:000016 P:000018 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Reserved interrupts
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
304  d    P:000022 P:000024 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0
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
305    
306                                 ; Now we're at P:$30, where some unused vector addresses are located
307                                 ; This is ROM only code that is only executed once on power-up when the
308                                 ; ROM code is downloaded. It is skipped over on OnCE downloads.
309    
310                                 ; A few seconds after power up on the Host, it interrogates the PCI bus to find
311                                 ; out what boards are installed and configures this PCI board. The EEPROM booting
312                                 ; procedure ends with program execution  starting at P:$0 where the EEPROM has
313                                 ; inserted a JMP INIT_PCI instruction. This routine sets the PLL paramter and
314                                 ; does a self configuration and software reset of the PCI controller in the DSP.
315                                 ; After configuring the PCI controller the DSP program overwrites the instruction
316                                 ; at P:$0 with a new JMP START to skip over the INIT_PCI routine. The program at
317                                 ; START address begins configuring the DSP and processing commands.
318                                 ; Similarly the ONCE option places a JMP START at P:$0 to skip over the
319                                 ; INIT_PCI routine. If this routine where executed after the host computer had booted
320                                 ; it would cause it to crash since the host computer would overwrite the
321                                 ; configuration space with its own values and doesn't tolerate foreign values.
322    
323                                 ; Initialize the PLL - phase locked loop
324                                 INIT_PCI
325       P:000030 P:000032 08F4BD            MOVEP             #PLL_INIT,X:PCTL        ; Initialize PLL
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  init.asm  Page 7



                            050003
326       P:000032 P:000034 000000            NOP
327    
328                                 ; Program the PCI self-configuration registers
329       P:000033 P:000035 240000            MOVE              #0,X0
330       P:000034 P:000036 08F485            MOVEP             #$500000,X:DCTR         ; Set self-configuration mode
                            500000
331       P:000036 P:000038 0604A0            REP     #4
332       P:000037 P:000039 08C408            MOVEP             X0,X:DPAR               ; Dummy writes to configuration space
333       P:000038 P:00003A 08F487            MOVEP             #>$0000,X:DPMC          ; Subsystem ID
                            000000
334       P:00003A P:00003C 08F488            MOVEP             #>$0000,X:DPAR          ; Subsystem Vendor ID
                            000000
335    
336                                 ; PCI Personal reset
337       P:00003C P:00003E 08C405            MOVEP             X0,X:DCTR               ; Personal software reset
338       P:00003D P:00003F 000000            NOP
339       P:00003E P:000040 000000            NOP
340       P:00003F P:000041 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00003F
341       P:000041 P:000043 07F084            MOVE              P:(*+3),X0              ; Trick to write "JMP <START" to P:0
                            000044
342       P:000043 P:000045 070004            MOVE              X0,P:(0)
343       P:000044 P:000046 0C0100            JMP     <START
344    
345  d    P:000045 P:000047 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
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
346  d    P:000051 P:000053 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
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
347  d    P:00005D P:00005F 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; $60-$71 Reserved PCI
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
348    
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  init.asm  Page 8



349                                 ;**************************************************************************
350                                 ; Check for program space overwriting of ISR starting at P:$72
351                                           IF      @CVS(N,*)>$71
353                                           ENDIF
354    
355                                 ;       ORG     P:$72,P:$72
356       P:000072 P:000074                   ORG     P:$72,P:$74
357    
358                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
359                                 ; command converter
360                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
362                                           ENDIF
363    
364    
365                                 ;**************************************************************************
366    
367                                 ; Three non-maskable fast interrupt service routines for clearing PCI interrupts
368                                 ; The Host will use these to clear the INTA* after it has serviced the interrupt
369                                 ; which had been generated by the PCI board.
370    
371       P:000072 P:000074 0A8506            BCLR    #INTA,X:DCTR                      ; $72/3 - Clear PCI interrupt
372       P:000073 P:000075 000000            NOP
373    
374       P:000074 P:000076 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; clear interrupt flag
375       P:000075 P:000077 000000            NOP                                       ; needs to be fast addressing <
376    
377       P:000076 P:000078 0A0022            BSET    #FATAL_ERROR,X:<STATUS            ; $76/7 - driver informing us of PCI_MESSAGE
_TO_HOST error
378       P:000077 P:000079 000000            NOP
379    
380                                 ; Interrupt locations for 7 available commands on PCI board
381                                 ; Each JSR takes up 2 locations in the table
382       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            00039D
383       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            000236
384       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            00035D
385       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            000375
386                                 ; software reset is the same as cleaning up the PCI - use same routine
387                                 ; when HOST does a RESET then this routine is run
388       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            000325
389       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            0002B3
390       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            000301
391       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            000275
392    
393    
394                                 ; ***********************************************************************
395                                 ; For now have boot code starting from P:$100
396                                 ; just to make debugging tidier etc.
397    
398       P:000100 P:000102                   ORG     P:$100,P:$102
399    
400                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
401                                 ; command converter
402                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
404                                           ENDIF
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  init.asm  Page 9



405                                 ; ***********************************************************************
406    
407    
408    
409                                 ; ******************************************************************
410                                 ;
411                                 ;       AA0 = RDFIFO* of incoming fiber optic data
412                                 ;       AA1 = EEPROM access
413                                 ;       AA2 = DRAM access
414                                 ;       AA3 = output to parallel data connector, for a video pixel clock
415                                 ;       $FFxxxx = Write to fiber optic transmitter
416                                 ;
417                                 ; ******************************************************************
418    
419    
420       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
421       P:000102 P:000104 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
422       P:000103 P:000105 0A8515            BCLR    #21,X:DCTR
423       P:000104 P:000106 0A8516            BCLR    #22,X:DCTR
424       P:000105 P:000107 000000            NOP
425       P:000106 P:000108 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
426       P:000107 P:000109 000000            NOP
427       P:000108 P:00010A 000000            NOP                                       ; End of PCI programming
428    
429    
430                                 ; Set operation mode register OMR to normal expanded
431       P:000109 P:00010B 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
432       P:00010A P:00010C 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
433    
434                                 ; Program the serial port ESSI0 = Port C for serial transmission to
435                                 ;   the timing board
436       P:00010B P:00010D 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
437                                 ;**********************************************************************
438       P:00010D P:00010F 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
439                                                                                     ; DC0-CD4 = 0 for non-network operation
440                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
441                                                                                     ; SSC1 = 0 for SC1 not used
442                                 ;************************************************************************
443       P:00010F P:000111 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
                            010120
444                                                                                     ; SHFD = 0 for MSB shifted first
445                                                                                     ; CKP = 0 for rising clock edge transitions
446                                                                                     ; TE0 = 1 to enable transmitter #0
447                                                                                     ; MOD = 0 for normal, non-networked mode
448                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
449       P:000111 P:000113 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
450                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
451                                 ;********************************************************************************
452       P:000113 P:000115 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
453       P:000115 P:000117 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
                            000000
454                                 ;***********************************************************************************
455                                 ; 250MHz
456                                 ; Conversion from software bits to schematic labels for Port C and D
457                                 ;       PC0 = SC00 = AUX3               PD0 = SC10 = EF*
458                                 ;       PC1 = SC01 = A/B* = input       PD1 = SC11 = HF*
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  init.asm  Page 10



459                                 ;       PC2 = SC02 = No connect         PD2 = SC12 = RS*
460                                 ;       PC3 = SCK0 = No connect         PD3 = SCK1 = NWRFIFO*
461                                 ;       PC4 = SRD0 = AUX1               PD4 = SRD1 = No connect (** in 50Mhz this was MODE selec
t for 16 or 32 bit FO)
462                                 ;       PC5 = STD0 = No connect         PD5 = STD1 = WRFIFO*
463                                 ; ***********************************************************************************
464    
465    
466                                 ; ****************************************************************************
467                                 ; Program the serial port ESSI1 = Port D for general purpose I/O (GPIO)
468    
469       P:000117 P:000119 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
470       P:000119 P:00011B 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            00001C
471       P:00011B P:00011D 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
472       P:00011D P:00011F 060AA0            REP     #10
473       P:00011E P:000120 000000            NOP
474       P:00011F P:000121 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
475                                                                                     ; was %011100
476    
477                                 ; Program the SCI port to benign values
478       P:000121 P:000123 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
479       P:000123 P:000125 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
480       P:000125 P:000127 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
481                                 ;       PE0 = RXD
482                                 ;       PE1 = TXD
483                                 ;       PE2 = SCLK
484    
485                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
486       P:000127 P:000129 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
487       P:000129 P:00012B 07F40B            MOVEP             #$2800,X:TCSR1
                            002800
488       P:00012B P:00012D 07F407            MOVEP             #$2800,X:TCSR2
                            002800
489    
490    
491                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
492       P:00012D P:00012F 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
493       P:00012F P:000131 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
e
                            008929
494       P:000131 P:000133 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
                            000122
495    
496    
497                                 ; Program the DRAM memory access and addressing
498       P:000133 P:000135 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
                            020022
499       P:000135 P:000137 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
500    
501    
502                                 ; Clear all PCI error conditions
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  init.asm  Page 11



503       P:000137 P:000139 084E0A            MOVEP             X:DPSR,A
504       P:000138 P:00013A 0140C2            OR      #$1FE,A
                            0001FE
505       P:00013A P:00013C 000000            NOP
506       P:00013B P:00013D 08CE0A            MOVEP             A,X:DPSR
507    
508                                 ;--------------------------------------------------------------------
509                                 ; Enable one interrupt only: software reset switch
510       P:00013C P:00013E 08F4BF            MOVEP             #$0001C0,X:IPRC         ; IRQB priority = 1 (FIFO half full HF*)
                            0001C0
511                                                                                     ; IRQC priority = 2 (reset switch)
512       P:00013E P:000140 05F439            MOVE              #$200,SR                ; Mask set up for reset switch only
                            000200
513    
514    
515                                 ;--------------------------------------------------------------------------
516                                 ; Initialize the fiber optic serial transmitter to zero
517       P:000140 P:000142 01B786            JCLR    #TDE,X:SSISR0,*
                            000140
518       P:000142 P:000144 07F43C            MOVEP             #$000000,X:TX00
                            000000
519    
520                                 ;--------------------------------------------------------------------
521    
522                                 ; clear DTXM - PCI master transmitter
523       P:000144 P:000146 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
524       P:000145 P:000147 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000145
525    
526                                 ;----------------------------------------------------------------------
527                                 ; clear DRXR - PCI receiver
528    
529       P:000147 P:000149 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014C
530       P:000149 P:00014B 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
531       P:00014A P:00014C 000000            NOP
532       P:00014B P:00014D 0C0147            JMP     <CLR0
533                                 CLR1
534    
535                                 ;-----------------------------------------------------------------------------
536                                 ; copy parameter table from P memory into X memory
537    
538                                 ; but not word_count and num_dumped - don't want these reset by fatal error....
539                                 ; they will be reset by new packet or pci_reset ISR
540    
541    
542       P:00014C P:00014E 46F000            MOVE              X:WORD_COUNT,Y0         ; store packet word count
                            000006
543       P:00014E P:000150 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000007
544       P:000150 P:000152 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
                            000001
545    
546                                 ; Move the table of constants from P: space to X: space
547       P:000152 P:000154 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            000575
548       P:000154 P:000156 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
549       P:000155 P:000157 064F80            DO      #VAR_TBL_LENGTH,X_WRITE
                            000158
550       P:000157 P:000159 07D984            MOVE              P:(R1)+,X0
551       P:000158 P:00015A 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
552                                 X_WRITE
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  init.asm  Page 12



553    
554    
555       P:000159 P:00015B 467000            MOVE              Y0,X:WORD_COUNT         ; restore packet word count
                            000006
556       P:00015B P:00015D 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
                            000007
557       P:00015D P:00015F 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000001
558    
559                                 ;-------------------------------------------------------------------------------
560                                 ; initialise some bits in STATUS
561    
562       P:00015F P:000161 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS     ; clear application loaded flag
563       P:000160 P:000162 0A000C            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appliaction running flag
564                                                                                     ; (e.g. not running diagnostic application
565                                                                                     ;      in self_test_mode)
566    
567       P:000161 P:000163 0A0002            BCLR    #FATAL_ERROR,X:<STATUS            ; initialise fatal error flag.
568       P:000162 P:000164 0A0028            BSET    #PACKET_CHOKE,X:<STATUS           ; enable MCE packet choke
569                                                                                     ; HOST not informed of anything from MCE unt
il
570                                                                                     ; comms are opened by host with first CON co
mmand
571    
572       P:000163 P:000165 0A0006            BCLR    #PREAMBLE_ERROR,X:<STATUS         ; flag to let host know premable error
573    
574                                 ;------------------------------------------------------------------------------
575                                 ; disable FIFO HF* intererupt...not used anymore.
576    
577       P:000164 P:000166 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable FIFO HF* interrupt
                            0001C0
578       P:000166 P:000168 05F439            MOVEC             #$200,SR                ; Mask level 1 interrupts
                            000200
579    
580                                 ;----------------------------------------------------------------------------
581                                 ; Disable Byte swapin - enabled after first command to MCE.
582                                 ; i.e after first 'CON'
583    
584       P:000168 P:00016A 0A0005            BCLR    #BYTE_SWAP,X:<STATUS              ; flag to let host know byte swapping off
585       P:000169 P:00016B 013D04            BCLR    #AUX1,X:PDRC                      ; enable disable
586    
587                                 ;----------------------------------------------------------------------------
588                                 ; Initialize PCI controller again, after booting, to make sure it sticks
589       P:00016A P:00016C 0A8514            BCLR    #20,X:DCTR                        ; Terminate and reset mode
590       P:00016B P:00016D 000000            NOP
591       P:00016C P:00016E 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00016C
592       P:00016E P:000170 000000            NOP
593       P:00016F P:000171 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
594       P:000170 P:000172 000000            NOP
595       P:000171 P:000173 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000171
596                                 ;-----------------------------------------------------------------------------
597                                 ; Here endth the initialisation code run after power up.
598                                 ; ----------------------------------------------------------------------------
599    
600    
601    
602    
603                                           INCLUDE 'main.asm'
604                                  COMMENT *
605    
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 13



606                                 This is the main section of the pci card code.
607    
608                                 Project:     SCUBA 2
609                                 Author:      DAVID ATKINSON
610                                 Target:      250MHz SDSU PCI card - DSP56301
611                                 Controller:  For use with SCUBA 2 Multichannel Electronics
612    
613                                 Modified:    MATTHEW HASSELFIELD
614    
615                                 Version:     Release Version U (1.3)
616    
617    
618                                 Assembler directives:
619                                         ROM=EEPROM => EEPROM CODE
620                                         ROM=ONCE => ONCE CODE
621    
622                                         *
623                                           PAGE    132                               ; Printronix page width - 132 columns
624                                           OPT     CEX                               ; print DC evaluations
625    
**** 626 [main.asm 23]:  INCLUDE PCI_main.asm HERE  
626                                           MSG     ' INCLUDE PCI_main.asm HERE  '
627    
628                                 ; --------------------------------------------------------------------------
629                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
630                                 ; --------------------------------------------------------------------------
631    
632                                 ; initialse buffer pointers
633                                 PACKET_IN
634    
635                                 ; R1 used as pointer for data written to y:memory            FO --> (Y)
636                                 ; R2 used as pointer for date in y mem to be writen to host  (Y) --> HOST
637    
638       P:000173 P:000175 310000            MOVE              #<IMAGE_BUFFER,R1       ; pointer for Fibre ---> Y mem
639       P:000174 P:000176 320000            MOVE              #<IMAGE_BUFFER,R2       ; pointer for Y mem ---> PCI BUS
640    
641                                 ; initialise some bits in status..
642       P:000175 P:000177 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
643       P:000176 P:000178 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
644       P:000177 P:000179 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS             ; clear Fiber Optic flag
645    
646                                 ; check some bits in status....
647       P:000178 P:00017A 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START      ; fatal error?  Go to initialisation.
                            000100
648       P:00017A P:00017C 0A00A0            JSET    #APPLICATION_LOADED,X:<STATUS,APPLICATION ; application loaded?  Execute in ap
pl space.
                            000800
649       P:00017C P:00017E 0A00AD            JSET    #INTERNAL_GO,X:<STATUS,APPLICATION ; internal GO to process?  PCI bus master w
rite test.
                            000800
650    
651       P:00017E P:000180 0D0409  CHK_FIFO  JSR     <GET_FO_WRD                       ; see if there's a 16-bit word in Fibre FIFO
 from MCE
652    
653    
654       P:00017F P:000181 0A00A3            JSET    #FO_WRD_RCV,X:<STATUS,CHECK_WD    ; there is a word - check if it's preamble
                            000182
655       P:000181 P:000183 0C0173            JMP     <PACKET_IN                        ; else go back and repeat
656    
657                                 ; check that we preamble sequence
658    
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 14



659       P:000182 P:000184 0A00A8  CHECK_WD  JSET    #PACKET_CHOKE,X:<STATUS,PACKET_IN ; IF MCE Packet choke on - just keep clearin
g FIFO.
                            000173
660       P:000184 P:000186 441D00            MOVE              X0,X:<HEAD_W1_0         ;store received word
661       P:000185 P:000187 56F000            MOVE              X:PREAMB1,A
                            000038
662       P:000187 P:000189 200045            CMP     X0,A                              ; check it is correct
663       P:000188 P:00018A 0E219C            JNE     <PRE_ERROR                        ; if not go to start
664    
665    
666       P:000189 P:00018B 0D0411            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
667       P:00018A P:00018C 441C00            MOVE              X0,X:<HEAD_W1_1         ;store received word
668       P:00018B P:00018D 56F000            MOVE              X:PREAMB1,A
                            000038
669       P:00018D P:00018F 200045            CMP     X0,A                              ; check it is correct
670       P:00018E P:000190 0E219C            JNE     <PRE_ERROR                        ; if not go to start
671    
672    
673       P:00018F P:000191 0D0411            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
674       P:000190 P:000192 441F00            MOVE              X0,X:<HEAD_W2_0         ;store received word
675       P:000191 P:000193 56F000            MOVE              X:PREAMB2,A
                            000039
676       P:000193 P:000195 200045            CMP     X0,A                              ; check it is correct
677       P:000194 P:000196 0E219C            JNE     <PRE_ERROR                        ; if not go to start
678    
679       P:000195 P:000197 0D0411            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
680       P:000196 P:000198 441E00            MOVE              X0,X:<HEAD_W2_1         ;store received word
681       P:000197 P:000199 56F000            MOVE              X:PREAMB2,A
                            000039
682       P:000199 P:00019B 200045            CMP     X0,A                              ; check it is correct
683       P:00019A P:00019C 0E219C            JNE     <PRE_ERROR                        ; if not go to start
684       P:00019B P:00019D 0C01A8            JMP     <PACKET_INFO                      ; get packet info
685    
686    
687                                 PRE_ERROR
688       P:00019C P:00019E 0A0026            BSET    #PREAMBLE_ERROR,X:<STATUS         ; indicate a preamble error
689       P:00019D P:00019F 440200            MOVE              X0,X:<PRE_CORRUPT       ; store corrupted word
690    
691                                 ; preampble error so clear out both FIFOs using reset line
692                                 ; - protects against an odd number of bytes having been sent
693                                 ; (byte swapping on - so odd byte being would end up in
694                                 ; the FIFO without the empty flag)
695    
696       P:00019E P:0001A0 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
697       P:0001A0 P:0001A2 44F400            MOVE              #200000,X0
                            030D40
698       P:0001A2 P:0001A4 06C400            DO      X0,*+3
                            0001A4
699       P:0001A4 P:0001A6 000000            NOP
700       P:0001A5 P:0001A7 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
701    
702       P:0001A7 P:0001A9 0C0173            JMP     <PACKET_IN                        ; wait for next packet
703    
704    
705                                 PACKET_INFO                                         ; packet preamble valid
706    
707                                 ; Packet preamle is valid so....
708                                 ; now get next two 32bit words.  i.e. $20205250 $00000004, or $20204441 $xxxxxxxx
709                                 ; note that these are received little endian (and byte swapped)
710                                 ; i.e. for RP receive 50 52 20 20  04 00 00 00
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 15



711                                 ; but byte swapped on arrival
712                                 ; 5250
713                                 ; 2020
714                                 ; 0004
715                                 ; 0000
716    
717       P:0001A8 P:0001AA 0D0411            JSR     <WT_FIFO
718       P:0001A9 P:0001AB 442100            MOVE              X0,X:<HEAD_W3_0         ; RP or DA
719       P:0001AA P:0001AC 0D0411            JSR     <WT_FIFO
720       P:0001AB P:0001AD 442000            MOVE              X0,X:<HEAD_W3_1         ; $2020
721    
722       P:0001AC P:0001AE 0D0411            JSR     <WT_FIFO
723       P:0001AD P:0001AF 442300            MOVE              X0,X:<HEAD_W4_0         ; packet size lo
724       P:0001AE P:0001B0 0D0411            JSR     <WT_FIFO
725       P:0001AF P:0001B1 442200            MOVE              X0,X:<HEAD_W4_1         ; packet size hi
726    
727       P:0001B0 P:0001B2 44A100            MOVE              X:<HEAD_W3_0,X0         ; get data header word 3 (low 2 bytes)
728       P:0001B1 P:0001B3 56BB00            MOVE              X:<REPLY_WD,A           ; $5250
729       P:0001B2 P:0001B4 200045            CMP     X0,A                              ; is it a reply packet?
730       P:0001B3 P:0001B5 0AF0AA            JEQ     MCE_PACKET                        ; yes - go process it.
                            0001C7
731    
732       P:0001B5 P:0001B7 56BA00            MOVE              X:<DATA_WD,A            ; $4441
733       P:0001B6 P:0001B8 200045            CMP     X0,A                              ; is it a data packet?
734       P:0001B7 P:0001B9 0E2173            JNE     <PACKET_IN                        ; no?  Not a valid packet type.  Go back to 
start and resync to next preamble.
735    
736    
737                                 ; It's a data packet....
738                                 ; check if it's the first packet after the GO command has been issued...
739    
740       P:0001B8 P:0001BA 0A0087            JCLR    #DATA_DLY,X:STATUS,INC_FRAME_COUNT ; do we need to add a delay since first fra
me?
                            0001C2
741    
742                                 ; yes first frame after GO reply packet so add a delay.
743                                 PACKET_DELAY
744       P:0001BA P:0001BC 44F000            MOVE              X:DATA_DLY_VAL,X0
                            000040
745       P:0001BC P:0001BE 06C400            DO      X0,*+3                            ; 10ns x DATA_DLY_VAL
                            0001BE
746       P:0001BE P:0001C0 000000            NOP
747       P:0001BF P:0001C1 000000            NOP
748       P:0001C0 P:0001C2 0A7007            BCLR    #DATA_DLY,X:STATUS                ; clear so delay isn't added next time.
                            000000
749    
750    
751                                 INC_FRAME_COUNT                                     ; increment frame count
752       P:0001C2 P:0001C4 200013            CLR     A
753       P:0001C3 P:0001C5 508100            MOVE              X:<FRAME_COUNT,A0
754       P:0001C4 P:0001C6 000008            INC     A
755       P:0001C5 P:0001C7 000000            NOP
756       P:0001C6 P:0001C8 500100            MOVE              A0,X:<FRAME_COUNT
757    
758                                 ; -------------------------------------------------------------------------------------------
759                                 ; ----------------------------------- IT'S A PACKET FROM MCE --------------------------------
760                                 ; -------------------------------------------------------------------------------------------
761                                 ; prepare notify to inform host that a packet has arrived.
762    
763                                 MCE_PACKET
764       P:0001C7 P:0001C9 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 16



                            4E4659
765       P:0001C9 P:0001CB 440C00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
766    
767       P:0001CA P:0001CC 44A100            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
768       P:0001CB P:0001CD 440D00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
age
769    
770       P:0001CC P:0001CE 44A300            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
771       P:0001CD P:0001CF 440E00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
772    
773       P:0001CE P:0001D0 44A200            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
774       P:0001CF P:0001D1 440F00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sasge
775    
776       P:0001D0 P:0001D2 200013            CLR     A                                 ;
777       P:0001D1 P:0001D3 340000            MOVE              #0,R4                   ; initialise word count
778       P:0001D2 P:0001D4 560600            MOVE              A,X:<WORD_COUNT         ; initialise word count store (num of words 
written over bus/packet)
779       P:0001D3 P:0001D5 560700            MOVE              A,X:<NUM_DUMPED         ; initialise number dumped from FIFO (after 
HST TO)
780    
781    
782                                 ; ----------------------------------------------------------------------------------------------
------------
783                                 ; Determine how to break up packet to write to host
784    
785                                 ; Note that this SR uses accumulator B
786                                 ; Therefore execute before we get the bus address from host (which is stored in B)
787                                 ; i.e before we issue notify message ('NFY')
788    
789       P:0001D4 P:0001D6 0D03DA            JSR     <CALC_NO_BUFFS                    ; subroutine which calculates the number of 
512 (16bit) buffers
790                                                                                     ; number of left over 32 (16bit) blocks
791                                                                                     ; and number of left overs (16bit) words
792    
793                                 ;  note that a 512 (16-bit) buffer is transfered to the host as 4 x 64 x 32bit DMA burst
794                                 ;            a 32  (16-bit) block is transfered to the host as a    16 x 32bit DMA burst
795                                 ;            left over 16bit words are transfered to the host in pairs as 32bit words
796                                 ; ----------------------------------------------------------------------------------------------
---
797    
798    
799                                 ; notify the host that there is a packet.....
800    
801       P:0001D5 P:0001D7 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
802       P:0001D6 P:0001D8 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
803    
804                                 ; initialise read/write buffers
805                                 ; AND IMMEDIATELY BEGIN TO BUFFER FIBRE DATA TO Y MEMORY.
806    
807       P:0001D7 P:0001D9 310000            MOVE              #<IMAGE_BUFFER,R1       ; FO ---> Y mem
808       P:0001D8 P:0001DA 320000            MOVE              #<IMAGE_BUFFER,R2       ; Y mem ----->  PCI BUS
809    
810    
811                                 ; ----------------------------------------------------------------------------------------------
-----------
812                                 ; Write TOTAL_BUFFS * 512 buffers to host
813                                 ; ----------------------------------------------------------------------------------------------
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 17



------
814       P:0001D9 P:0001DB 063C00            DO      X:<TOTAL_BUFFS,READ_BUFFS_END     ; note that if TOTAL_BUFFS = 0 we jump to AL
L_BUFFS_END
                            0001E6
815    
816       P:0001DB P:0001DD 0A00A2  WAIT_BUFF JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO  ; if fatal error then dump fifo and reset (i
.e. if HST timeout)
                            000216
817       P:0001DD P:0001DF 01ADA1            JSET    #HF,X:PDRD,WAIT_BUFF              ; Wait for FIFO to be half full + 1
                            0001DB
818       P:0001DF P:0001E1 000000            NOP
819       P:0001E0 P:0001E2 000000            NOP
820       P:0001E1 P:0001E3 01ADA1            JSET    #HF,X:PDRD,WAIT_BUFF              ; Protection against metastability
                            0001DB
821    
822                                 ; Copy the image block as 512 x 16bit words to DSP Y: Memory using R1 as pointer
823       P:0001E3 P:0001E5 060082            DO      #512,L_BUFFER
                            0001E5
824       P:0001E5 P:0001E7 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
825                                 L_BUFFER
826       P:0001E6 P:0001E8 000000            NOP
827                                 READ_BUFFS_END                                      ; all buffers have been read (-->Y)
828    
829                                 ; ----------------------------------------------------------------------------------------------
-----------
830                                 ; Read NUM_LEFTOVER_BLOCKS * 32 blocks
831                                 ; ----------------------------------------------------------------------------------------------
------
832    
833                                 ; less than 512 pixels but if greater than 32 will then do bursts
834                                 ; of 16 x 32bit in length, if less than 32 then does single read writes
835    
836       P:0001E7 P:0001E9 063F00            DO      X:<NUM_LEFTOVER_BLOCKS,READ_BLOCKS ;note that if NUM_LEFOVERS_BLOCKS = 0 we ju
mp to LEFTOVER_BLOCKS
                            0001F4
837    
838       P:0001E9 P:0001EB 062080            DO      #32,S_BUFFER
                            0001F3
839       P:0001EB P:0001ED 0A00A2  WAIT_1    JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO  ; check for fatal error (i.e. after HST time
out)
                            000216
840       P:0001ED P:0001EF 01AD80            JCLR    #EF,X:PDRD,WAIT_1                 ; Wait for the pixel datum to be there
                            0001EB
841       P:0001EF P:0001F1 000000            NOP                                       ; Settling time
842       P:0001F0 P:0001F2 000000            NOP
843       P:0001F1 P:0001F3 01AD80            JCLR    #EF,X:PDRD,WAIT_1                 ; Protection against metastability
                            0001EB
844       P:0001F3 P:0001F5 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; save fibre word
845                                 S_BUFFER
846       P:0001F4 P:0001F6 000000            NOP
847                                 READ_BLOCKS
848    
849                                 ; ----------------------------------------------------------------------------------------------
-------
850                                 ; Single write left over words to host
851                                 ; ----------------------------------------------------------------------------------------------
------
852    
853                                 LEFT_OVERS
854       P:0001F5 P:0001F7 063D00            DO      X:<LEFT_TO_READ,LEFT_OVERS_READ   ; read in remaining words of data packet
                            0001FF
855                                                                                     ; if LEFT_TO_READ = 0 then will jump to LEFT
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 18



_OVERS_READ
856    
857       P:0001F7 P:0001F9 0A00A2  WAIT_2    JSET    #FATAL_ERROR,X:<STATUS,START      ; check for fatal error (i.e. after HST time
out)
                            000100
858       P:0001F9 P:0001FB 01AD80            JCLR    #EF,X:PDRD,WAIT_2                 ; Wait till something in FIFO flagged
                            0001F7
859       P:0001FB P:0001FD 000000            NOP
860       P:0001FC P:0001FE 000000            NOP
861       P:0001FD P:0001FF 01AD80            JCLR    #EF,X:PDRD,WAIT_2                 ; protect against metastability.....
                            0001F7
862       P:0001FF P:000201 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; save fibre word
863                                 LEFT_OVERS_READ
864    
865                                 ;---------------------------------------------------------------------------------------
866                                 ; ENTIRE PACKET NOW IN Y MEMORY
867                                 ;----------------------------------------------------------------------------------------
868                                 ; CHECK THAT HST COMMAND WAS ISSUED DURING DATA COLLECTION...
869    
870    
871       P:000200 P:000202 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; if fatal error - run initialisation code..
.
                            000100
872       P:000202 P:000204 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; wait for host to reply - which it does wit
h 'send_packet_to_host' ISR
                            000200
873    
874                                 ; we now have 32 bit address in accumulator B
875                                 ; from send-packet_to_host (HST COMMAND) which should of been issued during data collection.
876    
877                                 ; Write all data to host.
878    
880    
881       P:000204 P:000206 0BF080            JSR     PCI_BURST_NOW
                            0004A0
882    
883       P:000206 P:000208 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
884    
885                                 ; ----------------------------------------------------------------------------------------------
------------
886                                 ; reply to host's send_packet_to_host command
887    
888                                  HST_ACK_REP
889       P:000208 P:00020A 44F400            MOVE              #'REP',X0
                            524550
890       P:00020A P:00020C 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
891       P:00020B P:00020D 44F400            MOVE              #'HST',X0
                            485354
892       P:00020D P:00020F 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
893       P:00020E P:000210 44F400            MOVE              #'ACK',X0
                            41434B
894       P:000210 P:000212 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
895       P:000211 P:000213 44F400            MOVE              #'000',X0
                            303030
896       P:000213 P:000215 440F00            MOVE              X0,X:<DTXS_WD4          ; no error
897       P:000214 P:000216 0D0421            JSR     <PCI_MESSAGE_TO_HOST
898       P:000215 P:000217 0C0173            JMP     <PACKET_IN
899    
900                                 ;-----------------------------------------------------------------------------------------------
----
901                                 ; clear out the fifo after an HST timeout...
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 19



902                                 ;----------------------------------------------------------
903    
904       P:000216 P:000218 61F400  DUMP_FIFO MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
905       P:000218 P:00021A 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
906       P:00021A P:00021C 200013            CLR     A
907       P:00021B P:00021D 320000            MOVE              #0,R2                   ; use R2 as a dump count
908    
909       P:00021C P:00021E 01AD80  NEXT_DUMP JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000227
910       P:00021E P:000220 000000            NOP
911       P:00021F P:000221 000000            NOP
912       P:000220 P:000222 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000227
913    
914       P:000222 P:000224 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
915       P:000223 P:000225 205A00            MOVE              (R2)+                   ; inc dump count
916       P:000224 P:000226 224E00            MOVE              R2,A                    ;
917       P:000225 P:000227 200045            CMP     X0,A                              ; check we've not hit dump limit
918       P:000226 P:000228 0E221C            JNE     NEXT_DUMP                         ; not hit limit?
919    
920    
921       P:000227 P:000229 627000  FIFO_EMPTY MOVE             R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000007
922       P:000229 P:00022B 0C0100            JMP     <START                            ; re-initialise
923    
924    
925    
926                                 ; ----------------------------------------------------------------------------------------------
--
927                                 ;                              END OF MAIN PACKET HANDLING CODE
928                                 ; ---------------------------------------------------------------------------------------------
929    
930    
931                                 ; -------------------------------------------------------------------------------------
932                                 ;
933                                 ;                              INTERRUPT SERVICE ROUTINES
934                                 ;
935                                 ; ---------------------------------------------------------------------------------------
936    
937                                 ;--------------------------------------------------------------------
938                                 CLEAN_UP_PCI
939                                 ;--------------------------------------------------------------------
940                                 ; Clean up the PCI board from wherever it was executing
941    
942       P:00022A P:00022C 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable HF* FIFO interrupt
                            0001C0
943       P:00022C P:00022E 05F439            MOVE              #$200,SR                ; mask for reset interrupts only
                            000200
944    
945       P:00022E P:000230 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
946       P:00022F P:000231 05F43D            MOVEC             #$000200,SSL            ; SR = zero except for interrupts
                            000200
947       P:000231 P:000233 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
948       P:000232 P:000234 05F43C            MOVEC             #START,SSH              ; Set PC to for full initialization
                            000100
949       P:000234 P:000236 000000            NOP
950       P:000235 P:000237 000004            RTI
951    
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 20



952                                 ; ---------------------------------------------------------------------------
953                                 READ_MEMORY
954                                 ;--------------------------------------------------------------------------
955                                 ; word 1 = command = 'RDM'
956                                 ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
957                                 ; word 3 = address in memory
958                                 ; word 4 = not used
959    
960       P:000236 P:000238 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
961    
962       P:000237 P:000239 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
963       P:000238 P:00023A 56F000            MOVE              X:DRXR_WD1,A            ; read command
                            000008
964       P:00023A P:00023C 44F400            MOVE              #'RDM',X0
                            52444D
965       P:00023C P:00023E 200045            CMP     X0,A                              ; ensure command is 'RDM'
966       P:00023D P:00023F 0E2261            JNE     <READ_MEMORY_ERROR_CNE            ; error, command NOT HCVR address
967       P:00023E P:000240 568900            MOVE              X:<DRXR_WD2,A           ; Memory type (X, Y, P)
968       P:00023F P:000241 578A00            MOVE              X:<DRXR_WD3,B
969       P:000240 P:000242 000000            NOP                                       ; pipeline restriction
970       P:000241 P:000243 21B000            MOVE              B1,R0                   ; get address to write to
971       P:000242 P:000244 0140C5            CMP     #$005F50,A                        ; $00'_P'
                            005F50
972       P:000244 P:000246 0E2248            JNE     <RDX
973       P:000245 P:000247 07E084            MOVE              P:(R0),X0               ; Read from P memory
974       P:000246 P:000248 208E00            MOVE              X0,A                    ;
975       P:000247 P:000249 0C0253            JMP     <FINISH_READ_MEMORY
976                                 RDX
977       P:000248 P:00024A 0140C5            CMP     #$005F58,A                        ; $00'_X'
                            005F58
978       P:00024A P:00024C 0E224E            JNE     <RDY
979       P:00024B P:00024D 44E000            MOVE              X:(R0),X0               ; Read from P memory
980       P:00024C P:00024E 208E00            MOVE              X0,A
981       P:00024D P:00024F 0C0253            JMP     <FINISH_READ_MEMORY
982                                 RDY
983       P:00024E P:000250 0140C5            CMP     #$005F59,A                        ; $00'_Y'
                            005F59
984       P:000250 P:000252 0E2266            JNE     <READ_MEMORY_ERROR_MTE            ; not a valid memory type
985       P:000251 P:000253 4CE000            MOVE                          Y:(R0),X0   ; Read from P memory
986       P:000252 P:000254 208E00            MOVE              X0,A
987    
988                                 ; when completed successfully then PCI needs to reply to Host with
989                                 ; word1 = reply/data = reply
990                                 FINISH_READ_MEMORY
991       P:000253 P:000255 44F400            MOVE              #'REP',X0
                            524550
992       P:000255 P:000257 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
993       P:000256 P:000258 44F400            MOVE              #'RDM',X0
                            52444D
994       P:000258 P:00025A 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
995       P:000259 P:00025B 44F400            MOVE              #'ACK',X0
                            41434B
996       P:00025B P:00025D 440E00            MOVE              X0,X:<DTXS_WD3          ;  im command
997       P:00025C P:00025E 21C400            MOVE              A,X0
998       P:00025D P:00025F 440F00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error
999       P:00025E P:000260 0D0469            JSR     <RESTORE_REGISTERS                ; restore registers
1000      P:00025F P:000261 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1001      P:000260 P:000262 000004            RTI
1002   
1003                                READ_MEMORY_ERROR_CNE
1004      P:000261 P:000263 44F400            MOVE              #'CNE',X0
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 21



                            434E45
1005      P:000263 P:000265 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1006      P:000264 P:000266 0AF080            JMP     READ_MEMORY_ERROR                 ; fill in rest of reply
                            000269
1007                                READ_MEMORY_ERROR_MTE
1008      P:000266 P:000268 44F400            MOVE              #'MTE',X0
                            4D5445
1009      P:000268 P:00026A 440F00            MOVE              X0,X:<DTXS_WD4          ;  Memory Type Error - not a valid memory ty
pe
1010   
1011                                READ_MEMORY_ERROR
1012      P:000269 P:00026B 44F400            MOVE              #'REP',X0
                            524550
1013      P:00026B P:00026D 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1014      P:00026C P:00026E 44F400            MOVE              #'RDM',X0
                            52444D
1015      P:00026E P:000270 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1016      P:00026F P:000271 44F400            MOVE              #'ERR',X0
                            455252
1017      P:000271 P:000273 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor.
1018      P:000272 P:000274 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1019      P:000273 P:000275 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1020      P:000274 P:000276 000004            RTI
1021   
1022                                ;-----------------------------------------------------------------------------
1023                                RESET_CONTROLLER
1024                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1025                                ;---------------------------------------------------------------------------
1026                                ; word 1 = command = 'RCO'
1027                                ; word 2 = not used but read
1028                                ; word 3 = not used but read
1029                                ; word 4 = not used but read
1030   
1031      P:000275 P:000277 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
1032      P:000276 P:000278 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1033      P:000277 P:000279 568800            MOVE              X:<DRXR_WD1,A           ; read command
1034      P:000278 P:00027A 44F400            MOVE              #'RCO',X0
                            52434F
1035      P:00027A P:00027C 200045            CMP     X0,A                              ; ensure command is 'RCO'
1036      P:00027B P:00027D 0E22A0            JNE     <RCO_ERROR                        ; error, command NOT HCVR address
1037   
1038                                ; if we get here then everything is fine and we can send reset to controller
1039   
1040                                ; 250MHZ CODE....
1041   
1042      P:00027C P:00027E 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1043      P:00027D P:00027F 000000            NOP
1044      P:00027E P:000280 000000            NOP
1045      P:00027F P:000281 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1046      P:000281 P:000283 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1047      P:000283 P:000285 446000            MOVE              X0,X:(R0)
1048      P:000284 P:000286 0606A0            REP     #6                                ; Wait for transmission to complete
1049      P:000285 P:000287 000000            NOP
1050      P:000286 P:000288 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1051   
1052                                ; Wait for a bit for MCE to be reset.......
1053      P:000287 P:000289 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 22



1054      P:000289 P:00028B 06C400            DO      X0,L_DELAY
                            00028F
1055      P:00028B P:00028D 06E883            DO      #1000,L_RDFIFO
                            00028E
1056      P:00028D P:00028F 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1057      P:00028E P:000290 000000            NOP                                       ;   receiver empty
1058                                L_RDFIFO
1059      P:00028F P:000291 000000            NOP
1060                                L_DELAY
1061      P:000290 P:000292 000000            NOP
1062   
1063                                ; when completed successfully then PCI needs to reply to Host with
1064                                ; word1 = reply/data = reply
1065                                FINISH_RCO
1066      P:000291 P:000293 44F400            MOVE              #'REP',X0
                            524550
1067      P:000293 P:000295 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1068      P:000294 P:000296 44F400            MOVE              #'RCO',X0
                            52434F
1069      P:000296 P:000298 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1070      P:000297 P:000299 44F400            MOVE              #'ACK',X0
                            41434B
1071      P:000299 P:00029B 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1072      P:00029A P:00029C 44F400            MOVE              #'000',X0
                            303030
1073      P:00029C P:00029E 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1074      P:00029D P:00029F 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1075      P:00029E P:0002A0 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1076      P:00029F P:0002A1 000004            RTI                                       ; return from ISR
1077   
1078                                ; when there is a failure in the host to PCI command then the PCI
1079                                ; needs still to reply to Host but with an error message
1080                                RCO_ERROR
1081      P:0002A0 P:0002A2 44F400            MOVE              #'REP',X0
                            524550
1082      P:0002A2 P:0002A4 447000            MOVE              X0,X:DTXS_WD1           ; REPly
                            00000C
1083      P:0002A4 P:0002A6 44F400            MOVE              #'RCO',X0
                            52434F
1084      P:0002A6 P:0002A8 447000            MOVE              X0,X:DTXS_WD2           ; echo command sent
                            00000D
1085      P:0002A8 P:0002AA 44F400            MOVE              #'ERR',X0
                            455252
1086      P:0002AA P:0002AC 447000            MOVE              X0,X:DTXS_WD3           ; ERRor im command
                            00000E
1087      P:0002AC P:0002AE 44F400            MOVE              #'CNE',X0
                            434E45
1088      P:0002AE P:0002B0 447000            MOVE              X0,X:DTXS_WD4           ; Command Name Error - command name in DRXR 
does not match
                            00000F
1089      P:0002B0 P:0002B2 0D0469            JSR     <RESTORE_REGISTERS                ; restore wroking registers
1090      P:0002B1 P:0002B3 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1091      P:0002B2 P:0002B4 000004            RTI                                       ; return from ISR
1092   
1093   
1094                                ;----------------------------------------------------------------------
1095                                SEND_PACKET_TO_CONTROLLER
1096   
1097                                ; forward packet stuff to the MCE
1098                                ; gets address in HOST memory where packet is stored
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 23



1099                                ; read 3 consecutive locations starting at this address
1100                                ; then sends the data from these locations up to the MCE
1101                                ;----------------------------------------------------------------------
1102   
1103                                ; word 1 = command = 'CON'
1104                                ; word 2 = host high address
1105                                ; word 3 = host low address
1106                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1107                                ;        = '1' --> when MCE command is GO
1108   
1109                                ; all MCE commands are now 'block commands'
1110                                ; i.e. 64 words long.
1111   
1112      P:0002B3 P:0002B5 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
1113   
1114      P:0002B4 P:0002B6 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1115                                                                                    ; reads as 4 x 24 bit words
1116   
1117      P:0002B5 P:0002B7 568800            MOVE              X:<DRXR_WD1,A           ; read command
1118      P:0002B6 P:0002B8 44F400            MOVE              #'CON',X0
                            434F4E
1119      P:0002B8 P:0002BA 200045            CMP     X0,A                              ; ensure command is 'CON'
1120      P:0002B9 P:0002BB 0E22F2            JNE     <CON_ERROR                        ; error, command NOT HCVR address
1121   
1122                                ; convert 2 x 24 bit words ( only 16 LSBs are significant) from host into 32 bit address
1123      P:0002BA P:0002BC 20001B            CLR     B
1124      P:0002BB P:0002BD 448900            MOVE              X:<DRXR_WD2,X0          ; MS 16bits of address
1125      P:0002BC P:0002BE 518A00            MOVE              X:<DRXR_WD3,B0          ; LS 16bits of address
1126      P:0002BD P:0002BF 000000            NOP
1127      P:0002BE P:0002C0 000000            NOP
1128      P:0002BF P:0002C1 000000            NOP
1129      P:0002C0 P:0002C2 000000            NOP
1130                                ; ;;; MFH - done
1131   
1132      P:0002C1 P:0002C3 0C1941            INSERT  #$010010,X0,B                     ; convert to 32 bits and put in B
                            010010
1133   
1134      P:0002C3 P:0002C5 568B00            MOVE              X:<DRXR_WD4,A           ; read word 4 - GO command?
1135      P:0002C4 P:0002C6 44F000            MOVE              X:ZERO,X0
                            000033
1136      P:0002C6 P:0002C8 200045            CMP     X0,A
1137      P:0002C7 P:0002C9 0AF0AA            JEQ     BLOCK_CON
                            0002D5
1138   
1139   
1140      P:0002C9 P:0002CB 0A008C            JCLR    #APPLICATION_RUNNING,X:STATUS,SET_PACKET_DELAY ; not running diagnostic applic
ation?
                            0002D3
1141   
1142                                ; need to generate an internal go command to test master write on bus.....  Diagnostic test
1143      P:0002CB P:0002CD 0A702D            BSET    #INTERNAL_GO,X:STATUS             ; set flag so that GO reply / data is genera
ted by PCI card...
                            000000
1144   
1145                                ; since INTERNAL_GO  - read command but don't send it to MCE...
1146   
1147                                CLR_CMD
1148      P:0002CD P:0002CF 064080            DO      #64,END_CLR_CMD                   ; block size = 32bit x 64 (256 bytes)
                            0002D0
1149      P:0002CF P:0002D1 0D0449            JSR     <READ_FROM_PCI                    ; get next 32 bit word from HOST
1150      P:0002D0 P:0002D2 000000            NOP
1151                                END_CLR_CMD
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 24



1152      P:0002D1 P:0002D3 0AF080            JMP     FINISH_CON                        ; don't send out on command on fibre
                            0002E3
1153   
1154   
1155                                SET_PACKET_DELAY
1156      P:0002D3 P:0002D5 0A7027            BSET    #DATA_DLY,X:STATUS                ; set data delay so that next data packet af
ter go reply
                            000000
1157                                                                                    ; experiences a delay before host notify.
1158   
1159                                ; -----------------------------------------------------------------------
1160                                ; WARNING!!!
1161                                ; MCE requires IDLE characters between 32bit words sent FROM the PCI card
1162                                ; DO not change READ_FROM_PCI to DMA block transfer....
1163                                ; ------------------------------------------------------------------------
1164   
1165                                BLOCK_CON
1166      P:0002D5 P:0002D7 66F000            MOVE              X:CONSTORE,R6
                            000041
1167   
1168      P:0002D7 P:0002D9 064080            DO      #64,END_BLOCK_CON                 ; block size = 32bit x 64 (256 bytes)
                            0002DF
1169      P:0002D9 P:0002DB 0D0449            JSR     <READ_FROM_PCI                    ; get next 32 bit word from HOST
1170      P:0002DA P:0002DC 208C00            MOVE              X0,A1                   ; prepare to send
1171      P:0002DB P:0002DD 20A800            MOVE              X1,A0                   ; prepare to send
1172   
1173      P:0002DC P:0002DE 4D5E00            MOVE                          X1,Y:(R6)+  ; b4, b3 (msb)
1174      P:0002DD P:0002DF 4C5E00            MOVE                          X0,Y:(R6)+  ; b2, b1  (lsb)
1175   
1176      P:0002DE P:0002E0 0D048A            JSR     <XMT_WD_FIBRE                     ; off it goes
1177      P:0002DF P:0002E1 000000            NOP
1178                                END_BLOCK_CON
1179   
1180      P:0002E0 P:0002E2 0A0008            BCLR    #PACKET_CHOKE,X:<STATUS           ; disable packet choke...
1181                                                                                    ; comms now open with MCE and packets will b
e processed.
1182                                ; Enable Byte swaping for correct comms protocol.
1183      P:0002E1 P:0002E3 0A0025            BSET    #BYTE_SWAP,X:<STATUS              ; flag to let host know byte swapping on
1184      P:0002E2 P:0002E4 013D24            BSET    #AUX1,X:PDRC                      ; enable hardware
1185   
1186   
1187                                ; -------------------------------------------------------------------------
1188                                ; when completed successfully then PCI needs to reply to Host with
1189                                ; word1 = reply/data = reply
1190                                FINISH_CON
1191      P:0002E3 P:0002E5 44F400            MOVE              #'REP',X0
                            524550
1192      P:0002E5 P:0002E7 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1193      P:0002E6 P:0002E8 44F400            MOVE              #'CON',X0
                            434F4E
1194      P:0002E8 P:0002EA 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1195      P:0002E9 P:0002EB 44F400            MOVE              #'ACK',X0
                            41434B
1196      P:0002EB P:0002ED 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1197      P:0002EC P:0002EE 44F400            MOVE              #'000',X0
                            303030
1198      P:0002EE P:0002F0 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1199      P:0002EF P:0002F1 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1200      P:0002F0 P:0002F2 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ;  interrupt host with message (x0 restored 
here)
1201      P:0002F1 P:0002F3 000004            RTI                                       ; return from ISR
1202   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 25



1203                                ; when there is a failure in the host to PCI command then the PCI
1204                                ; needs still to reply to Host but with an error message
1205                                CON_ERROR
1206      P:0002F2 P:0002F4 44F400            MOVE              #'REP',X0
                            524550
1207      P:0002F4 P:0002F6 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1208      P:0002F5 P:0002F7 44F400            MOVE              #'CON',X0
                            434F4E
1209      P:0002F7 P:0002F9 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1210      P:0002F8 P:0002FA 44F400            MOVE              #'ERR',X0
                            455252
1211      P:0002FA P:0002FC 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1212      P:0002FB P:0002FD 44F400            MOVE              #'CNE',X0
                            434E45
1213      P:0002FD P:0002FF 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1214      P:0002FE P:000300 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1215      P:0002FF P:000301 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1216      P:000300 P:000302 000004            RTI                                       ; return from ISR
1217   
1218                                ; ------------------------------------------------------------------------------------
1219                                SEND_PACKET_TO_HOST
1220                                ; this command is received from the Host and actions the PCI board to pick up an address
1221                                ; pointer from DRXR which the PCI board then uses to write packets from the
1222                                ; MCE to the host memory starting at the address given.
1223                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1224                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1225                                ; HST after packet sent (unless error).
1226                                ; --------------------------------------------------------------------------------------
1227                                ; word 1 = command = 'HST'
1228                                ; word 2 = host high address
1229                                ; word 3 = host low address
1230                                ; word 4 = not used but read
1231   
1232                                ; save some registers but not B
1233   
1234      P:000301 P:000303 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
1235   
1236      P:000302 P:000304 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1237      P:000303 P:000305 20001B            CLR     B
1238      P:000304 P:000306 568800            MOVE              X:<DRXR_WD1,A           ; read command
1239      P:000305 P:000307 44F400            MOVE              #'HST',X0
                            485354
1240      P:000307 P:000309 200045            CMP     X0,A                              ; ensure command is 'HST'
1241      P:000308 P:00030A 0E2314            JNE     <HOST_ERROR                       ; error, command NOT HCVR address
1242      P:000309 P:00030B 448900            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
1243      P:00030A P:00030C 518A00            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1244   
1246      P:00030B P:00030D 447000            MOVE              X0,X:BURST_DEST_HI
                            000045
1247      P:00030D P:00030F 517000            MOVE              B0,X:BURST_DEST_LO
                            000044
1249   
1250      P:00030F P:000311 0C1941            INSERT  #$010010,X0,B                     ; convert to 32 bits and put in B
                            010010
1251   
1252      P:000311 P:000313 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1253      P:000312 P:000314 0D0475            JSR     <RESTORE_HST_REGISTERS            ; restore registers for HST .... B not resto
red..
1254      P:000313 P:000315 000004            RTI
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 26



1255   
1256                                ; !!NOTE!!!
1257                                ; successful reply to this command is sent after packet has been send to host.
1258                                ; Not here unless error.
1259   
1260                                ; when there is a failure in the host to PCI command then the PCI
1261                                ; needs still to reply to Host but with an error message
1262                                HOST_ERROR
1263      P:000314 P:000316 0A7001            BCLR    #SEND_TO_HOST,X:STATUS
                            000000
1264      P:000316 P:000318 44F400            MOVE              #'REP',X0
                            524550
1265      P:000318 P:00031A 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1266      P:000319 P:00031B 44F400            MOVE              #'HST',X0
                            485354
1267      P:00031B P:00031D 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1268      P:00031C P:00031E 44F400            MOVE              #'ERR',X0
                            455252
1269      P:00031E P:000320 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1270      P:00031F P:000321 44F400            MOVE              #'CNE',X0
                            434E45
1271      P:000321 P:000323 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1272      P:000322 P:000324 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1273      P:000323 P:000325 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1274      P:000324 P:000326 000004            RTI
1275   
1276                                ; --------------------------------------------------------------------
1277                                SOFTWARE_RESET
1278                                ;----------------------------------------------------------------------
1279                                ; word 1 = command = 'RST'
1280                                ; word 2 = not used but read
1281                                ; word 3 = not used but read
1282                                ; word 4 = not used but read
1283   
1284      P:000325 P:000327 0D047E            JSR     <SAVE_REGISTERS
1285   
1286      P:000326 P:000328 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1287      P:000327 P:000329 568800            MOVE              X:<DRXR_WD1,A           ; read command
1288      P:000328 P:00032A 44F400            MOVE              #'RST',X0
                            525354
1289      P:00032A P:00032C 200045            CMP     X0,A                              ; ensure command is 'RST'
1290      P:00032B P:00032D 0E234E            JNE     <RST_ERROR                        ; error, command NOT HCVR address
1291   
1292                                ; RST command OK so reply to host
1293                                FINISH_RST
1294      P:00032C P:00032E 44F400            MOVE              #'REP',X0
                            524550
1295      P:00032E P:000330 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1296      P:00032F P:000331 44F400            MOVE              #'RST',X0
                            525354
1297      P:000331 P:000333 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1298      P:000332 P:000334 44F400            MOVE              #'ACK',X0
                            41434B
1299      P:000334 P:000336 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1300      P:000335 P:000337 44F400            MOVE              #'000',X0
                            303030
1301      P:000337 P:000339 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1302      P:000338 P:00033A 0D0421            JSR     <PCI_MESSAGE_TO_HOST
1303   
1304      P:000339 P:00033B 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 27



                            000339
1305   
1306      P:00033B P:00033D 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS     ; clear app flag
1307      P:00033C P:00033E 0A0006            BCLR    #PREAMBLE_ERROR,X:<STATUS         ; clear preamble error
1308      P:00033D P:00033F 0A000C            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1309   
1310                                ; initialise some parameter here - that we don't want to initialse under a fatal error reset.
1311   
1312      P:00033E P:000340 200013            CLR     A
1313      P:00033F P:000341 340000            MOVE              #0,R4                   ; initialise word count
1314      P:000340 P:000342 560600            MOVE              A,X:<WORD_COUNT         ; initialise word count store (num of words 
written over bus/packet)
1315      P:000341 P:000343 560700            MOVE              A,X:<NUM_DUMPED         ; initialise number dumped from FIFO (after 
HST TO)
1316   
1317   
1318                                ; remember we are in a ISR so can't just jump to start.
1319   
1320      P:000342 P:000344 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable HF* FIFO interrupt
                            0001C0
1321      P:000344 P:000346 05F439            MOVE              #$200,SR                ; Mask set up for reset switch only.
                            000200
1322   
1323   
1324      P:000346 P:000348 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1325      P:000347 P:000349 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1326                                                                                    ; set to zero except for interrupts
1327      P:000349 P:00034B 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1328                                                                                    ; so first set to 0
1329      P:00034A P:00034C 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1330                                                                                    ; therefore,return to initialization
1331      P:00034C P:00034E 000000            NOP
1332      P:00034D P:00034F 000004            RTI                                       ; return from ISR - to START
1333   
1334                                ; when there is a failure in the host to PCI command then the PCI
1335                                ; needs still to reply to Host but with an error message
1336                                RST_ERROR
1337      P:00034E P:000350 44F400            MOVE              #'REP',X0
                            524550
1338      P:000350 P:000352 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1339      P:000351 P:000353 44F400            MOVE              #'RST',X0
                            525354
1340      P:000353 P:000355 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1341      P:000354 P:000356 44F400            MOVE              #'ERR',X0
                            455252
1342      P:000356 P:000358 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1343      P:000357 P:000359 44F400            MOVE              #'CNE',X0
                            434E45
1344      P:000359 P:00035B 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1345      P:00035A P:00035C 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1346      P:00035B P:00035D 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1347      P:00035C P:00035E 000004            RTI                                       ; return from ISR
1348   
1349   
1350                                ;-----------------------------------------------------------------------------
1351                                START_APPLICATION
1352                                ; an application should already have been downloaded to the PCI memory.
1353                                ; this command will execute it.
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 28



1354                                ; ----------------------------------------------------------------------
1355                                ; word 1 = command = 'GOA'
1356                                ; word 2 = not used but read by RD_DRXR
1357                                ; word 3 = not used but read by RD_DRXR
1358                                ; word 4 = not used but read by RD_DRXR
1359   
1360      P:00035D P:00035F 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
1361   
1362      P:00035E P:000360 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1363      P:00035F P:000361 568800            MOVE              X:<DRXR_WD1,A           ; read command
1364      P:000360 P:000362 44F400            MOVE              #'GOA',X0
                            474F41
1365      P:000362 P:000364 200045            CMP     X0,A                              ; ensure command is 'RDM'
1366      P:000363 P:000365 0E2366            JNE     <GO_ERROR                         ; error, command NOT HCVR address
1367   
1368                                ; if we get here then everything is fine and we can start the application
1369                                ; set bit in status so that main fibre servicing code knows to jump
1370                                ; to application space after returning from this ISR
1371   
1372                                ; reply after application has been executed.
1373      P:000364 P:000366 0A0020            BSET    #APPLICATION_LOADED,X:<STATUS
1374      P:000365 P:000367 000004            RTI                                       ; return from ISR
1375   
1376   
1377                                ; when there is a failure in the host to PCI command then the PCI
1378                                ; needs still to reply to Host but with an error message
1379                                GO_ERROR
1380      P:000366 P:000368 44F400            MOVE              #'REP',X0
                            524550
1381      P:000368 P:00036A 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1382      P:000369 P:00036B 44F400            MOVE              #'GOA',X0
                            474F41
1383      P:00036B P:00036D 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1384      P:00036C P:00036E 44F400            MOVE              #'ERR',X0
                            455252
1385      P:00036E P:000370 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1386      P:00036F P:000371 44F400            MOVE              #'CNE',X0
                            434E45
1387      P:000371 P:000373 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1388      P:000372 P:000374 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1389      P:000373 P:000375 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1390      P:000374 P:000376 000004            RTI                                       ; return from ISR
1391   
1392                                ; ---------------------------------------------------------
1393                                STOP_APPLICATION
1394                                ; this command stops an application that is currently running
1395                                ; used for applications that once started run contiunually
1396                                ;-----------------------------------------------------------
1397   
1398                                ; word 1 = command = ' STP'
1399                                ; word 2 = not used but read
1400                                ; word 3 = not used but read
1401                                ; word 4 = not used but read
1402   
1403      P:000375 P:000377 0D047E            JSR     <SAVE_REGISTERS
1404   
1405      P:000376 P:000378 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1406      P:000377 P:000379 568800            MOVE              X:<DRXR_WD1,A           ; read command
1407      P:000378 P:00037A 44F400            MOVE              #'STP',X0
                            535450
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 29



1408      P:00037A P:00037C 200045            CMP     X0,A                              ; ensure command is 'RDM'
1409      P:00037B P:00037D 0E238E            JNE     <STP_ERROR                        ; error, command NOT HCVR address
1410   
1411      P:00037C P:00037E 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS
1412      P:00037D P:00037F 0A700C            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1413   
1414                                ; when completed successfully then PCI needs to reply to Host with
1415                                ; word1 = reply/data = reply
1416                                FINISH_STP
1417      P:00037F P:000381 44F400            MOVE              #'REP',X0
                            524550
1418      P:000381 P:000383 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1419      P:000382 P:000384 44F400            MOVE              #'STP',X0
                            535450
1420      P:000384 P:000386 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1421      P:000385 P:000387 44F400            MOVE              #'ACK',X0
                            41434B
1422      P:000387 P:000389 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1423      P:000388 P:00038A 44F400            MOVE              #'000',X0
                            303030
1424      P:00038A P:00038C 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1425      P:00038B P:00038D 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers.
1426      P:00038C P:00038E 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1427      P:00038D P:00038F 000004            RTI
1428   
1429                                ; when there is a failure in the host to PCI command then the PCI
1430                                ; needs still to reply to Host but with an error message
1431                                STP_ERROR
1432      P:00038E P:000390 44F400            MOVE              #'REP',X0
                            524550
1433      P:000390 P:000392 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1434      P:000391 P:000393 44F400            MOVE              #'STP',X0
                            535450
1435      P:000393 P:000395 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1436      P:000394 P:000396 44F400            MOVE              #'ERR',X0
                            455252
1437      P:000396 P:000398 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1438      P:000397 P:000399 44F400            MOVE              #'CNE',X0
                            434E45
1439      P:000399 P:00039B 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1440      P:00039A P:00039C 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1441      P:00039B P:00039D 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1442      P:00039C P:00039E 000004            RTI
1443   
1444                                ;--------------------------------------------------------------
1445                                WRITE_MEMORY
1446                                ;---------------------------------------------------------------
1447                                ; word 1 = command = 'WRM'
1448                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1449                                ; word 3 = address in memory
1450                                ; word 4 = value
1451   
1452      P:00039D P:00039F 0D047E            JSR     <SAVE_REGISTERS                   ; save working registers
1453   
1454      P:00039E P:0003A0 0D043C            JSR     <RD_DRXR                          ; read words from host write to HTXR
1455      P:00039F P:0003A1 56F000            MOVE              X:DRXR_WD1,A            ; read command
                            000008
1456      P:0003A1 P:0003A3 44F400            MOVE              #'WRM',X0
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 30



                            57524D
1457      P:0003A3 P:0003A5 200045            CMP     X0,A                              ; ensure command is 'WRM'
1458      P:0003A4 P:0003A6 0E23C7            JNE     <WRITE_MEMORY_ERROR_CNE           ; error, command NOT HCVR address
1459      P:0003A5 P:0003A7 568900            MOVE              X:<DRXR_WD2,A           ; Memory type (X, Y, P)
1460      P:0003A6 P:0003A8 578A00            MOVE              X:<DRXR_WD3,B
1461      P:0003A7 P:0003A9 000000            NOP                                       ; pipeline restriction
1462      P:0003A8 P:0003AA 21B000            MOVE              B1,R0                   ; get address to write to
1463      P:0003A9 P:0003AB 448B00            MOVE              X:<DRXR_WD4,X0          ; get data to write
1464      P:0003AA P:0003AC 0140C5            CMP     #$005F50,A                        ; $00'_P'
                            005F50
1465      P:0003AC P:0003AE 0E23AF            JNE     <WRX
1466      P:0003AD P:0003AF 076084            MOVE              X0,P:(R0)               ; Write to Program memory
1467      P:0003AE P:0003B0 0C03B8            JMP     <FINISH_WRITE_MEMORY
1468                                WRX
1469      P:0003AF P:0003B1 0140C5            CMP     #$005F58,A                        ; $00'_X'
                            005F58
1470      P:0003B1 P:0003B3 0E23B4            JNE     <WRY
1471      P:0003B2 P:0003B4 446000            MOVE              X0,X:(R0)               ; Write to X: memory
1472      P:0003B3 P:0003B5 0C03B8            JMP     <FINISH_WRITE_MEMORY
1473                                WRY
1474      P:0003B4 P:0003B6 0140C5            CMP     #$005F59,A                        ; $00'_Y'
                            005F59
1475      P:0003B6 P:0003B8 0E23CB            JNE     <WRITE_MEMORY_ERROR_MTE
1476      P:0003B7 P:0003B9 4C6000            MOVE                          X0,Y:(R0)   ; Write to Y: memory
1477   
1478                                ; when completed successfully then PCI needs to reply to Host with
1479                                ; word1 = reply/data = reply
1480                                FINISH_WRITE_MEMORY
1481      P:0003B8 P:0003BA 44F400            MOVE              #'REP',X0
                            524550
1482      P:0003BA P:0003BC 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1483      P:0003BB P:0003BD 44F400            MOVE              #'WRM',X0
                            57524D
1484      P:0003BD P:0003BF 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1485      P:0003BE P:0003C0 44F400            MOVE              #'ACK',X0
                            41434B
1486      P:0003C0 P:0003C2 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1487      P:0003C1 P:0003C3 44F400            MOVE              #'000',X0
                            303030
1488      P:0003C3 P:0003C5 440F00            MOVE              X0,X:<DTXS_WD4          ; no error
1489      P:0003C4 P:0003C6 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1490      P:0003C5 P:0003C7 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1491      P:0003C6 P:0003C8 000004            RTI
1492   
1493                                ;
1494                                WRITE_MEMORY_ERROR_CNE
1495      P:0003C7 P:0003C9 44F400            MOVE              #'CNE',X0
                            434E45
1496      P:0003C9 P:0003CB 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1497      P:0003CA P:0003CC 0C03CE            JMP     <WRITE_MEMORY_ERROR               ; fill in rest of reply
1498   
1499                                WRITE_MEMORY_ERROR_MTE
1500      P:0003CB P:0003CD 44F400            MOVE              #'MTE',X0
                            4D5445
1501      P:0003CD P:0003CF 440F00            MOVE              X0,X:<DTXS_WD4          ; Memory Type Error - memory type not valid
1502   
1503                                WRITE_MEMORY_ERROR
1504      P:0003CE P:0003D0 44F400            MOVE              #'REP',X0
                            524550
1505      P:0003D0 P:0003D2 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 31



1506      P:0003D1 P:0003D3 44F400            MOVE              #'WRM',X0
                            57524D
1507      P:0003D3 P:0003D5 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1508      P:0003D4 P:0003D6 44F400            MOVE              #'ERR',X0
                            455252
1509      P:0003D6 P:0003D8 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1510      P:0003D7 P:0003D9 0D0469            JSR     <RESTORE_REGISTERS                ; restore working registers
1511      P:0003D8 P:0003DA 0D0421            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1512      P:0003D9 P:0003DB 000004            RTI
1513   
1514   
1515                                ;---------------------------------------------------------------
1516                                ;
1517                                ;                          * END OF ISRs *
1518                                ;
1519                                ;--------------------------------------------------------------
1520   
1521   
1522   
1523                                ;----------------------------------------------------------------
1524                                ;
1525                                ;                     * Beginning of SUBROUTINES *
1526                                ;
1527                                ;-----------------------------------------------------------------
1528   
1529   
1530                                ; -------------------------------------------------------------
1531                                CALC_NO_BUFFS
1532                                ;----------------------------------------------------
1533                                ; number of 512 buffers in packet calculated (X:TOTAL_BUFFS)
1534                                ; and number of left over blocks (X:NUM_LEFTOVER_BLOCKS)
1535                                ; and left over words (X:LEFT_TO_READ)
1536   
1537      P:0003DA P:0003DC 20001B            CLR     B
1538      P:0003DB P:0003DD 51A300            MOVE              X:<HEAD_W4_0,B0         ; LS 16bits
1539      P:0003DC P:0003DE 44A200            MOVE              X:<HEAD_W4_1,X0         ; MS 16bits
1540   
1541      P:0003DD P:0003DF 0C1941            INSERT  #$010010,X0,B                     ; now size of packet B....giving # of 32bit 
words in packet
                            010010
1542      P:0003DF P:0003E1 000000            NOP
1543   
1544                                ; need to covert this to 16 bit since read from FIFO and saved in Y memory as 16bit words...
1545   
1546                                ; so double size of packet....
1547      P:0003E0 P:0003E2 20003A            ASL     B
1548   
1549                                ; now save
1550      P:0003E1 P:0003E3 212400            MOVE              B0,X0
1551      P:0003E2 P:0003E4 21A500            MOVE              B1,X1
1552      P:0003E3 P:0003E5 443600            MOVE              X0,X:<PACKET_SIZE_LOW   ; low 24 bits of packet size (in 16bit words
)
1553      P:0003E4 P:0003E6 453700            MOVE              X1,X:<PACKET_SIZE_HIH   ; high 8 bits of packet size (in 16bit words
)
1554   
1555      P:0003E5 P:0003E7 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1556      P:0003E6 P:0003E8 54B700            MOVE              X:<PACKET_SIZE_HIH,A1
1557      P:0003E7 P:0003E9 0C1C12            ASR     #9,A,A                            ; divide by 512...number of 16bit words in a
 buffer
1558      P:0003E8 P:0003EA 000000            NOP
1559      P:0003E9 P:0003EB 503C00            MOVE              A0,X:<TOTAL_BUFFS
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 32



1560   
1561      P:0003EA P:0003EC 210500            MOVE              A0,X1
1562      P:0003EB P:0003ED 47F400            MOVE              #HF_FIFO,Y1
                            000200
1563      P:0003ED P:0003EF 2000F0            MPY     X1,Y1,A
1564      P:0003EE P:0003F0 0C1C03            ASR     #1,A,B                            ; B holds number of 16bit words in all full 
buffers
1565      P:0003EF P:0003F1 000000            NOP
1566   
1567      P:0003F0 P:0003F2 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1568      P:0003F1 P:0003F3 54B700            MOVE              X:<PACKET_SIZE_HIH,A1   ; A holds total number of 16bit words
1569      P:0003F2 P:0003F4 200014            SUB     B,A                               ; now A holds number of left over 16bit word
s
1570      P:0003F3 P:0003F5 000000            NOP
1571      P:0003F4 P:0003F6 503D00            MOVE              A0,X:<LEFT_TO_READ      ; store number of left over 16bit words to r
ead
1572      P:0003F5 P:0003F7 0C1C0A            ASR     #5,A,A                            ; divide by 32... number of 16bit words in l
efover block
1573      P:0003F6 P:0003F8 000000            NOP
1574      P:0003F7 P:0003F9 503F00            MOVE              A0,X:<NUM_LEFTOVER_BLOCKS
1575      P:0003F8 P:0003FA 210500            MOVE              A0,X1
1576      P:0003F9 P:0003FB 47F400            MOVE              #>SMALL_BLK,Y1
                            000020
1577      P:0003FB P:0003FD 2000F0            MPY     X1,Y1,A
1578      P:0003FC P:0003FE 0C1C02            ASR     #1,A,A
1579      P:0003FD P:0003FF 000000            NOP
1580   
1581      P:0003FE P:000400 200018            ADD     A,B                               ; B holds words in all buffers
1582      P:0003FF P:000401 000000            NOP
1583      P:000400 P:000402 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1584      P:000401 P:000403 54B700            MOVE              X:<PACKET_SIZE_HIH,A1   ; A holds total number of words
1585      P:000402 P:000404 200014            SUB     B,A                               ; now A holds number of left over words
1586      P:000403 P:000405 000000            NOP
1587      P:000404 P:000406 503D00            MOVE              A0,X:<LEFT_TO_READ      ; store number of left over 16bit words to r
ead
1588   
1589      P:000405 P:000407 0C1C02            ASR     #1,A,A                            ; divide by two to get number of 32 bit word
s to write
1590      P:000406 P:000408 000000            NOP                                       ; for pipeline
1591      P:000407 P:000409 503E00            MOVE              A0,X:<LEFT_TO_WRITE     ; store number of left over 32 bit words (2 
x 16 bit) to write to host after small block transfer as well
1592   
1593      P:000408 P:00040A 00000C            RTS
1594   
1595                                ;---------------------------------------------------------------
1596                                GET_FO_WRD
1597                                ;--------------------------------------------------------------
1598                                ; Anything in fibre receive FIFO?   If so store in X0
1599   
1600      P:000409 P:00040B 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            00041F
1601      P:00040B P:00040D 000000            NOP
1602      P:00040C P:00040E 000000            NOP
1603      P:00040D P:00040F 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS             ; check twice for FO metastability.
                            00041F
1604      P:00040F P:000411 0AF080            JMP     RD_FO_WD
                            000417
1605   
1606      P:000411 P:000413 01AD80  WT_FIFO   JCLR    #EF,X:PDRD,*                      ; Wait till something in FIFO flagged
                            000411
1607      P:000413 P:000415 000000            NOP
1608      P:000414 P:000416 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 33



1609      P:000415 P:000417 01AD80            JCLR    #EF,X:PDRD,WT_FIFO                ; check twice.....
                            000411
1610   
1611                                ; Read one word from the fiber optics FIFO, check it and put it in A1
1612                                RD_FO_WD
1613      P:000417 P:000419 09443F            MOVEP             Y:RDFIFO,X0             ; then read to X0
1614      P:000418 P:00041A 54F400            MOVE              #$00FFFF,A1             ; mask off top 2 bytes ($FC)
                            00FFFF
1615      P:00041A P:00041C 200046            AND     X0,A                              ; since receiving 16 bits in 24bit register
1616      P:00041B P:00041D 000000            NOP
1617      P:00041C P:00041E 218400            MOVE              A1,X0
1618      P:00041D P:00041F 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1619      P:00041E P:000420 00000C            RTS
1620                                CLR_FO_RTS
1621      P:00041F P:000421 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1622      P:000420 P:000422 00000C            RTS
1623   
1624   
1625                                ; ----------------------------------------------------------------------------
1626                                PCI_MESSAGE_TO_HOST
1627                                ;----------------------------------------------------------------------------
1628   
1629                                ; subroutine to send 4 words as a reply from PCI to the Host
1630                                ; using the DTXS-HRXS data path
1631                                ; PCI card writes here first then causes an interrupt INTA on
1632                                ; the PCI bus to alert the host to the reply message
1633   
1634      P:000421 P:000423 0A85A3            JSET    #DCTR_HF3,X:DCTR,*                ; make sure host ready to receive interrupt
                            000421
1635                                                                                    ; cleared via fast interrupt if host out of 
its ISR
1636   
1637      P:000423 P:000425 0A8981            JCLR    #STRQ,X:DSR,*                     ; Wait for transmitter to be NOT FULL
                            000423
1638                                                                                    ; i.e. if CLR then FULL so wait
1639                                                                                    ; if not then it is clear to write
1640      P:000425 P:000427 448C00            MOVE              X:<DTXS_WD1,X0
1641      P:000426 P:000428 447000            MOVE              X0,X:DTXS               ; Write 24 bit word1
                            FFFFCD
1642   
1643      P:000428 P:00042A 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            000428
1644      P:00042A P:00042C 448D00            MOVE              X:<DTXS_WD2,X0
1645      P:00042B P:00042D 447000            MOVE              X0,X:DTXS               ; Write 24 bit word2
                            FFFFCD
1646   
1647      P:00042D P:00042F 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            00042D
1648      P:00042F P:000431 448E00            MOVE              X:<DTXS_WD3,X0
1649      P:000430 P:000432 447000            MOVE              X0,X:DTXS               ; Write 24 bit word3
                            FFFFCD
1650   
1651      P:000432 P:000434 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            000432
1652      P:000434 P:000436 448F00            MOVE              X:<DTXS_WD4,X0
1653      P:000435 P:000437 447000            MOVE              X0,X:DTXS               ; Write 24 bit word4
                            FFFFCD
1654   
1655   
1656                                ; restore X0....
1657                                ; PCI_MESSAGE_TO_HOST is used by all command vector ISRs.
1658                                ; Working registers must be restored before RTI.
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 34



1659                                ; However, we want to restore before asserting INTA.
1660                                ; x0 is only one that can't be restored before PCI_MESSAGE_TO_HOST
1661                                ; (since it is used by this SR) hence we restore here.
1662                                ; this is redundant for a 'NFY' message (since sequential instruction)
1663                                ; but may be required for a PCI command reply 'REP' message.
1664                                ; (since interrupt driven)
1665   
1666      P:000437 P:000439 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            00002E
1667   
1668                                ; all the transmit words are in the FIFO, interrupt the Host
1669                                ; the Host should clear this interrupt once it is detected.
1670                                ; It does this by writing to HCVR to cause a fast interrupt.
1671   
1672   
1673      P:000439 P:00043B 0A8523            BSET    #DCTR_HF3,X:DCTR                  ; set flag to handshake interrupt (INTA) wit
h host.
1674      P:00043A P:00043C 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1675   
1676      P:00043B P:00043D 00000C            RTS
1677   
1678                                ;---------------------------------------------------------------
1679                                RD_DRXR
1680                                ;--------------------------------------------------------------
1681                                ; routine is used to read from HTXR-DRXR data path
1682                                ; which is used by the Host to communicate with the PCI board
1683                                ; the host writes 4 words to this FIFO then interrupts the PCI
1684                                ; which reads the 4 words and acts on them accordingly.
1685   
1686   
1687      P:00043C P:00043E 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            00043C
1688                                                                                    ; implies that host has written words
1689   
1690   
1691                                ; actually reading as slave here so this shouldn't be necessary......?
1692   
1693      P:00043E P:000440 0A8717            BCLR    #FC1,X:DPMC                       ; 24 bit read FC1 = 0, FC1 = 0
1694      P:00043F P:000441 0A8736            BSET    #FC0,X:DPMC
1695   
1696   
1697      P:000440 P:000442 08440B            MOVEP             X:DRXR,X0               ; Get word1
1698      P:000441 P:000443 440800            MOVE              X0,X:<DRXR_WD1
1699      P:000442 P:000444 08440B            MOVEP             X:DRXR,X0               ; Get word2
1700      P:000443 P:000445 440900            MOVE              X0,X:<DRXR_WD2
1701      P:000444 P:000446 08440B            MOVEP             X:DRXR,X0               ; Get word3
1702      P:000445 P:000447 440A00            MOVE              X0,X:<DRXR_WD3
1703      P:000446 P:000448 08440B            MOVEP             X:DRXR,X0               ; Get word4
1704      P:000447 P:000449 440B00            MOVE              X0,X:<DRXR_WD4
1705      P:000448 P:00044A 00000C            RTS
1706   
1707                                ;---------------------------------------------------------------
1708                                READ_FROM_PCI
1709                                ;--------------------------------------------------------------
1710                                ; sub routine to read a 24 bit word in from PCI bus --> Y memory
1711                                ; 32bit host address in accumulator B.
1712   
1713                                ; read as master
1714   
1715      P:000449 P:00044B 0C1890            EXTRACTU #$010010,B,A                     ; Get D31-16 bits only
                            010010
1716      P:00044B P:00044D 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 35



1717   
1718      P:00044C P:00044E 210C00            MOVE              A0,A1
1719      P:00044D P:00044F 000000            NOP
1720      P:00044E P:000450 547000            MOVE              A1,X:DPMC               ; high 16bits of address in DSP master cntr 
reg.
                            FFFFC7
1721                                                                                    ; 32 bit read so FC1 = 0 and FC0 = 0
1722   
1723      P:000450 P:000452 000000            NOP
1724      P:000451 P:000453 0C1890            EXTRACTU #$010000,B,A
                            010000
1725      P:000453 P:000455 000000            NOP
1726      P:000454 P:000456 210C00            MOVE              A0,A1
1727      P:000455 P:000457 0140C2            OR      #$060000,A                        ; A1 gets written to DPAR register
                            060000
1728      P:000457 P:000459 000000            NOP                                       ; C3-C0 of DPAR=0110 for memory read
1729      P:000458 P:00045A 08CC08  WRT_ADD   MOVEP             A1,X:DPAR               ; Write address to PCI bus - PCI READ action
1730      P:000459 P:00045B 000000            NOP                                       ; Pipeline delay
1731      P:00045A P:00045C 0A8AA2  RD_PCI    JSET    #MRRQ,X:DPSR,GET_DAT              ; If MTRQ = 1 go read the word from host via
 FIFO
                            000463
1732      P:00045C P:00045E 0A8A8A            JCLR    #TRTY,X:DPSR,RD_PCI               ; Bit is set if its a retry
                            00045A
1733      P:00045E P:000460 08F48A            MOVEP             #$0400,X:DPSR           ; Clear bit 10 = target retry bit
                            000400
1734      P:000460 P:000462 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for PCI addressing to be complete
                            000460
1735      P:000462 P:000464 0C0458            JMP     <WRT_ADD
1736   
1737      P:000463 P:000465 08440B  GET_DAT   MOVEP             X:DRXR,X0               ; Read 1st 16 bits of 32 bit word from host 
memory
1738      P:000464 P:000466 08450B            MOVEP             X:DRXR,X1               ; Read 2nd 16 bits of 32 bit word from host 
memory
1739   
1740                                ; note that we now have 4 bytes in X0 and X1.
1741                                ; The 32bit word was in host memory in little endian format
1742                                ; If form LSB --> MSB the bytes are b1, b2, b3, b4 in host memory
1743                                ; in progressing through the HTRX/DRXR FIFO the
1744                                ; bytes end up like this.....
1745                                ; then X0 = $00 b2 b1
1746                                ; and  X1 = $00 b4 b3
1747   
1748      P:000465 P:000467 0604A0            REP     #4                                ; increment PCI address by four bytes.
1749      P:000466 P:000468 000009            INC     B
1750      P:000467 P:000469 000000            NOP
1751      P:000468 P:00046A 00000C            RTS
1752   
1753                                ;------------------------------------------------------------------------------------
1754                                RESTORE_REGISTERS
1755                                ;-------------------------------------------------------------------------------------
1756   
1757      P:000469 P:00046B 05B239            MOVEC             X:<SV_SR,SR
1758   
1759      P:00046A P:00046C 50A800            MOVE              X:<SV_A0,A0
1760      P:00046B P:00046D 54A900            MOVE              X:<SV_A1,A1
1761      P:00046C P:00046E 52AA00            MOVE              X:<SV_A2,A2
1762   
1763      P:00046D P:00046F 51AB00            MOVE              X:<SV_B0,B0
1764      P:00046E P:000470 55AC00            MOVE              X:<SV_B1,B1
1765      P:00046F P:000471 53AD00            MOVE              X:<SV_B2,B2
1766   
1767      P:000470 P:000472 44AE00            MOVE              X:<SV_X0,X0
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 36



1768      P:000471 P:000473 45AF00            MOVE              X:<SV_X1,X1
1769   
1770      P:000472 P:000474 46B000            MOVE              X:<SV_Y0,Y0
1771      P:000473 P:000475 47B100            MOVE              X:<SV_Y1,Y1
1772   
1773      P:000474 P:000476 00000C            RTS
1774                                ;------------------------------------------------------------------------------------
1775                                RESTORE_HST_REGISTERS
1776                                ;-------------------------------------------------------------------------------------
1777                                ; B not restored after HST as it now contains address.
1778   
1779      P:000475 P:000477 05B239            MOVEC             X:<SV_SR,SR
1780   
1781      P:000476 P:000478 50A800            MOVE              X:<SV_A0,A0
1782      P:000477 P:000479 54A900            MOVE              X:<SV_A1,A1
1783      P:000478 P:00047A 52AA00            MOVE              X:<SV_A2,A2
1784   
1785      P:000479 P:00047B 44AE00            MOVE              X:<SV_X0,X0
1786      P:00047A P:00047C 45AF00            MOVE              X:<SV_X1,X1
1787   
1788      P:00047B P:00047D 46B000            MOVE              X:<SV_Y0,Y0
1789      P:00047C P:00047E 47B100            MOVE              X:<SV_Y1,Y1
1790   
1791      P:00047D P:00047F 00000C            RTS
1792   
1793                                ;-------------------------------------------------------------------------------------
1794                                SAVE_REGISTERS
1795                                ;-------------------------------------------------------------------------------------
1796   
1797      P:00047E P:000480 053239            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1798   
1799      P:00047F P:000481 502800            MOVE              A0,X:<SV_A0
1800      P:000480 P:000482 542900            MOVE              A1,X:<SV_A1
1801      P:000481 P:000483 522A00            MOVE              A2,X:<SV_A2
1802   
1803      P:000482 P:000484 512B00            MOVE              B0,X:<SV_B0
1804      P:000483 P:000485 552C00            MOVE              B1,X:<SV_B1
1805      P:000484 P:000486 532D00            MOVE              B2,X:<SV_B2
1806   
1807      P:000485 P:000487 442E00            MOVE              X0,X:<SV_X0
1808      P:000486 P:000488 452F00            MOVE              X1,X:<SV_X1
1809   
1810      P:000487 P:000489 463000            MOVE              Y0,X:<SV_Y0
1811      P:000488 P:00048A 473100            MOVE              Y1,X:<SV_Y1
1812   
1813      P:000489 P:00048B 00000C            RTS
1814   
1815   
1816   
1817                                ; ; ------------------------------------------------------------------------------------
1818                                ; WRITE_TO_PCI
1819                                ; ;-------------------------------------------------------------------------------------
1820                                ; ; sub routine to write two 16 bit words (stored in Y memory)
1821                                ; ; to host memory as PCI bus master.
1822                                ; ; results in a 32bit word written to host memory.
1823   
1824                                ; ; the 32 bit host address is in accumulator B.
1825                                ; ; this address is writen to DPMC (MSBs) and DPAR (LSBs)
1826                                ; ; address is incrememted by 4 (bytes) after write.
1827   
1828                                ; ; R2 is used as a pointer to Y:memory address
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 37



1829   
1830   
1831                                ;       JCLR    #MTRQ,X:DPSR,*          ; wait here if DTXM is full
1832   
1833                                ; TX_LSB        MOVEP   Y:(R2)+,X:DTXM          ; Least significant word to transmit
1834                                ; TX_MSB        MOVEP   Y:(R2)+,X:DTXM          ; Most significant word to transmit
1835   
1836   
1837                                ;       EXTRACTU #$010010,B,A           ; Get D31-16 bits only,
1838                                ;       NOP                             ; top byte = $00 so FC1 = FC0 = 0
1839                                ;       MOVE    A0,A1
1840   
1841                                ; ; we are using two 16 bit writes to make a 32bit word
1842                                ; ; so FC1=0 and FC0=0 when A1 written to DPMC
1843   
1844                                ;       NOP
1845                                ;       MOVE    A1,X:DPMC               ; DSP master control register
1846                                ;       NOP
1847                                ;       EXTRACTU #$010000,B,A
1848                                ;       NOP
1849                                ;       MOVE    A0,A1
1850                                ;       OR      #$070000,A              ; A1 gets written to DPAR register
1851                                ;       NOP
1852   
1853                                ; AGAIN1        MOVEP   A1,X:DPAR               ; Write to PCI bus
1854                                ;       NOP                             ; Pipeline delay
1855                                ;       NOP
1856                                ;       JCLR    #MARQ,X:DPSR,*          ; Bit is set if its a retry
1857                                ;       JSET    #MDT,X:DPSR,INC_ADD     ; If no error go to the next sub-block
1858                                ;       JSR     <PCI_ERROR_RECOVERY
1859                                ;       JMP     <AGAIN1
1860                                ; INC_ADD
1861                                ;       CLR     A       (R4)+             ; clear A and increment word count
1862                                ;       MOVE    #>4,A0                    ; 4 bytes per word transfer on pcibus
1863                                ;       ADD     A,B     R4,X:<WORD_COUNT  ; Inc bus address by 4 bytes, and save word count
1864                                ;       RTS
1865   
1866                                ; ; -------------------------------------------------------------------------------------------
1867                                ; WRITE_32_TO_PCI
1868                                ; ; DMAs 32 x 16bit words to host memory as PCI burst.
1869                                ; ;---------------------------------------------------------------------------------------------
--
1870                                ;       MOVE    #32,N2                  ; Number of 16bit words per transfer
1871                                ;       MOVE    #16,N4                  ; Number of 32bit words per transfer
1872   
1873                                ;       MOVE    R2,X:DSR0               ; Source address for DMA = pixel data
1874                                ;       MOVEP   #DTXM,X:DDR0            ; Destination = PCI master transmitter
1875                                ;       MOVEP   #>31,X:DCO0             ; DMA Count = # of pixels - 1
1876   
1877                                ;       EXTRACTU #$010010,B,A           ; Get D31-16 bits only
1878                                ;       NOP
1879                                ;       MOVE    A0,A1                   ; [D31-16] in A1
1880                                ;       NOP
1881                                ;       ORI     #$0F0000,A              ; Burst length = # of PCI writes
1882                                ;       NOP                             ;   = # of pixels / 2 - 1 ...$0F = 16
1883                                ;       MOVE    A1,X:DPMC               ; DPMC = B[31:16] + $3F0000
1884   
1885                                ;       EXTRACTU #$010000,B,A
1886                                ;       NOP
1887                                ;       MOVE    A0,A1                   ; Get PCI_ADDR[15:0] into A1[15:0]
1888                                ;       NOP
1889                                ;       ORI     #$070000,A              ; A1 gets written to DPAR register
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 38



1890                                ;       NOP
1891   
1892   
1893                                ; AGAIN2        MOVEP   #$8EFA51,X:DCR0         ; Start DMA with control register DE=1
1894                                ;       MOVEP   A1,X:DPAR               ; Initiate writing to the PCI bus
1895                                ;       NOP
1896                                ;       NOP
1897                                ;       JCLR    #MARQ,X:DPSR,*          ; Wait until the PCI operation is done
1898                                ;       JSET    #MDT,X:DPSR,WR_OK1      ; If no error go to the next sub-block
1899                                ;       JSR     <PCI_ERROR_RECOVERY
1900                                ;       JMP     <AGAIN2                 ; Just try to write the sub-block again
1901                                ; WR_OK1
1902                                ;       CLR     A       (R4)+N4           ; increment number of 32bit word count
1903                                ;       MOVE    #>64,A0                   ; 2 bytes on pcibus per pixel
1904                                ;       ADD     A,B     R4,X:<WORD_COUNT  ; PCI address = + 2 x # of pixels (!!!)
1905                                ;       MOVE    (R2)+N2                   ; Pixel buffer address = + # of pixels
1906                                ;       RTS
1907   
1908                                ; ;------------------------------------------------------------
1909                                ; WRITE_512_TO_PCI
1910                                ; ;-------------------------------------------------------------
1911                                ; ; DMAs 128 x 16bit words to host memory as PCI burst
1912                                ; ; does x 4 of these (total of 512 x 16bit words written to host memory)
1913                                ; ;
1914                                ; ; R2 is used as a pointer to Y:memory address
1915   
1916   
1917                                ;       MOVE    #128,N2                 ; Number of 16bit words per transfer.
1918                                ;       MOVE    #64,N4                  ; NUmber of 32bit words per transfer.
1919   
1920                                ; ; Make sure its always 512 pixels per loop = 1/2 FIFO
1921                                ;       MOVE    R2,X:DSR0               ; Source address for DMA = pixel data
1922                                ;       MOVEP   #DTXM,X:DDR0            ; Destination = PCI master transmitter
1923                                ;       MOVEP   #>127,X:DCO0            ; DMA Count = # of pixels - 1
1924   
1925                                ; ; Do loop does 4 x 128 pixel DMA writes = 512.
1926                                ; ; need to recalculate hi and lo parts of address
1927                                ; ; for each burst.....Leach code doesn't do this since not
1928                                ; ; multiple frames...so only needs to inc low part.....
1929   
1930                                ;       DO      #4,WR_BLK0              ; x # of pixels = 512
1931   
1932                                ; WRITE_512_TO_PCI_LOOP
1933                                ;       EXTRACTU #$010010,B,A           ; Get D31-16 bits only
1934                                ;       NOP
1935                                ;       MOVE    A0,A1                   ; [D31-16] in A1
1936                                ;       NOP
1937                                ;       ORI     #$3F0000,A              ; Burst length = # of PCI writes
1938                                ;       NOP                             ;   = # of pixels / 2 - 1 ...$3F = 63
1939                                ;       MOVE    A1,X:DPMC               ; DPMC = B[31:16] + $3F0000
1940   
1941   
1942                                ;       EXTRACTU #$010000,B,A
1943                                ;       NOP
1944                                ;       MOVE    A0,A1                   ; Get PCI_ADDR[15:0] into A1[15:0]
1945                                ;       NOP
1946                                ;       OR      #$070000,A              ; A1 gets written to DPAR register
1947                                ;       NOP
1948   
1949   
1950                                ; AGAIN0        MOVEP   #$8EFA51,X:DCR0         ; Start DMA with control register DE=1
1951                                ;       MOVEP   A1,X:DPAR               ; Initiate writing to the PCI bus
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 39



1952                                ;       NOP
1953                                ;       NOP
1954                                ;       JCLR    #MARQ,X:DPSR,*          ; Wait until the PCI operation is done
1955                                ;       JSET    #MDT,X:DPSR,WR_OK0      ; If no error go to the next sub-block
1956                                ;       JSR     <PCI_ERROR_RECOVERY
1957                                ;       JMP     <AGAIN0                 ; Just try to write the sub-block again
1958                                ; WR_OK0
1959   
1960                                ;       CLR     A       (R4)+N4           ; clear A and increment word count
1961                                ;       MOVE    #>256,A0                  ; 2 bytes on pcibus per pixel
1962                                ;       ADD     A,B     R4,X:<WORD_COUNT  ; Inc bus address by # of bytes, and save word count
1963                                ;       MOVE    (R2)+N2                   ; Pixel buffer address = + # of pixels
1964                                ; WR_BLK0
1965                                ;       RTS
1966   
1967                                ; ;-----------------------------
1968                                ; XMT_DLY
1969                                ; ;-----------------------------
1970                                ; ; Short delay for reliability
1971   
1972                                ;       NOP
1973                                ;       NOP
1974                                ;       NOP
1975                                ;       RTS
1976   
1977                                ;-------------------------------------------------------
1978                                XMT_WD_FIBRE
1979                                ;-----------------------------------------------------
1980                                ; 250 MHz code - Transmit contents of Accumulator A1 to the MCE
1981                                ; we want to send 32bit word in little endian fomat to the host.
1982                                ; i.e. b4b3b2b1 goes b1, b2, b3, b4
1983                                ; currently the bytes are in this order:
1984                                ;  A1 = $00 b2 b1
1985                                ;  A0 = $00 b4 b3
1986                                ;  A = $00 00 b2 b1 00 b4 b3
1987   
1988                                ; This subroutine must take at least 160ns (4 bytes at 25Mbytes/s)
1989   
1990      P:00048A P:00048C 000000            NOP
1991      P:00048B P:00048D 000000            NOP
1992   
1993                                ; split up 4 bytes b2, b1, b4, b3
1994   
1995      P:00048C P:00048E 0C1D20            ASL     #16,A,A                           ; shift byte b2 into A2
1996      P:00048D P:00048F 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1997   
1998      P:00048F P:000491 214700            MOVE              A2,Y1                   ; byte b2 in Y1
1999   
2000      P:000490 P:000492 0C1D10            ASL     #8,A,A                            ; shift byte b1 into A2
2001      P:000491 P:000493 000000            NOP
2002      P:000492 P:000494 214600            MOVE              A2,Y0                   ; byte b1 in Y0
2003   
2004      P:000493 P:000495 0C1D20            ASL     #16,A,A                           ; shift byte b4 into A2
2005      P:000494 P:000496 000000            NOP
2006      P:000495 P:000497 214500            MOVE              A2,X1                   ; byte b4 in X1
2007   
2008   
2009      P:000496 P:000498 0C1D10            ASL     #8,A,A                            ; shift byte b3 into A2
2010      P:000497 P:000499 000000            NOP
2011      P:000498 P:00049A 214400            MOVE              A2,X0                   ; byte b3 in x0
2012   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 40



2013                                ; transmit b1, b2, b3 ,b4
2014   
2015      P:000499 P:00049B 466000            MOVE              Y0,X:(R0)               ; byte b1 - off it goes
2016      P:00049A P:00049C 476000            MOVE              Y1,X:(R0)               ; byte b2 - off it goes
2017      P:00049B P:00049D 446000            MOVE              X0,X:(R0)               ; byte b3 - off it goes
2018      P:00049C P:00049E 456000            MOVE              X1,X:(R0)               ; byte b4 - off it goes
2019   
2020      P:00049D P:00049F 000000            NOP
2021      P:00049E P:0004A0 000000            NOP
2022      P:00049F P:0004A1 00000C            RTS
2023   
2024   
2025   
2029   
2030                                PCI_BURST_NOW
2031   
2032      P:0004A0 P:0004A2 51A300            MOVE              X:<HEAD_W4_0,B0         ; LS 16bits
2033      P:0004A1 P:0004A3 0C1D91            ASL     #8,B,B
2034      P:0004A2 P:0004A4 45A200            MOVE              X:<HEAD_W4_1,X1         ; MS 16bits
2035      P:0004A3 P:0004A5 0C1C8D            ASR     #6,B,B                            ; Size in BYTES
2036   
2037      P:0004A4 P:0004A6 627000            MOVE              R2,X:BURST_SRC
                            000046
2038      P:0004A6 P:0004A8 517000            MOVE              B0,X:BLOCK_SIZE
                            000042
2039   
2040      P:0004A8 P:0004AA 0BF080            JSR     BLOCK_TRANSFER
                            00050C
2041      P:0004AA P:0004AC 00000C            RTS
2042   
2043   
2044                                ;----------------------------------------------
2045                                FLUSH_PCI_FIFO
2046                                ;----------------------------------------------
2047      P:0004AB P:0004AD 0A8A84            JCLR    #MARQ,X:DPSR,*
                            0004AB
2048      P:0004AD P:0004AF 0A862E            BSET    #CLRT,X:DPCR
2049      P:0004AE P:0004B0 000000            NOP
2050      P:0004AF P:0004B1 0A86AE            JSET    #CLRT,X:DPCR,*
                            0004AF
2051      P:0004B1 P:0004B3 00000C            RTS
2052   
2053                                ;-----------------------------------------------
2054                                MPCI_ERROR_RECOVERY
2055                                ;-----------------------------------------------
2056      
2057      
2058      
2059      
2060      
2061      
2062   
2063      P:0004B2 P:0004B4 50F000            MOVE              X:DMA_ERRORS,A0
                            000047
2064      P:0004B4 P:0004B6 000008            INC     A
2065      P:0004B5 P:0004B7 000000            NOP
2066      P:0004B6 P:0004B8 507000            MOVE              A0,X:DMA_ERRORS
                            000047
2067   
2068      P:0004B8 P:0004BA 0A8AAA            JSET    #TRTY,X:DPSR,ERROR_TRTY
                            0004C6
2069      P:0004BA P:0004BC 0A8AAB            JSET    #TO,X:DPSR,ERROR_TO
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 41



                            0004D0
2070      P:0004BC P:0004BE 0A8AA9            JSET    #TDIS,X:DPSR,ERROR_TDIS
                            0004DA
2071      P:0004BE P:0004C0 0A8AA8            JSET    #TAB,X:DPSR,ERROR_TAB
                            0004E4
2072      P:0004C0 P:0004C2 0A8AA7            JSET    #MAB,X:DPSR,ERROR_MAB
                            0004EE
2073      P:0004C2 P:0004C4 0A8AA6            JSET    #DPER,X:DPSR,ERROR_DPER
                            0004F8
2074      P:0004C4 P:0004C6 0A8AA5            JSET    #APER,X:DPSR,ERROR_APER
                            000502
2075   
2076                                ERROR_TRTY
2077      P:0004C6 P:0004C8 50F000            MOVE              X:EC_TRTY,A0
                            000048
2078      P:0004C8 P:0004CA 000008            INC     A
2079      P:0004C9 P:0004CB 08F48A            MOVEP             #$0400,X:DPSR           ; Clear target retry error bit
                            000400
2080      P:0004CB P:0004CD 507000            MOVE              A0,X:EC_TRTY
                            000048
2081      P:0004CD P:0004CF 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2082      P:0004CF P:0004D1 00000C            RTS
2083                                ERROR_TO
2084      P:0004D0 P:0004D2 50F000            MOVE              X:EC_TO,A0
                            000049
2085      P:0004D2 P:0004D4 000008            INC     A
2086      P:0004D3 P:0004D5 08F48A            MOVEP             #$0800,X:DPSR           ; Clear timeout error bit
                            000800
2087      P:0004D5 P:0004D7 507000            MOVE              A0,X:EC_TO
                            000049
2088      P:0004D7 P:0004D9 0A702F            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
2089      P:0004D9 P:0004DB 00000C            RTS
2090                                ERROR_TDIS
2091      P:0004DA P:0004DC 50F000            MOVE              X:EC_TDIS,A0
                            00004A
2092      P:0004DC P:0004DE 000008            INC     A
2093      P:0004DD P:0004DF 08F48A            MOVEP             #$0200,X:DPSR           ; Clear target disconnect bit
                            000200
2094      P:0004DF P:0004E1 507000            MOVE              A0,X:EC_TDIS
                            00004A
2095      P:0004E1 P:0004E3 0A702F            BSET    #PCIDMA_RESUME,X:STATUS
                            000000
2096      P:0004E3 P:0004E5 00000C            RTS
2097                                ERROR_TAB
2098      P:0004E4 P:0004E6 50F000            MOVE              X:EC_TAB,A0
                            00004B
2099      P:0004E6 P:0004E8 000008            INC     A
2100      P:0004E7 P:0004E9 08F48A            MOVEP             #$0100,X:DPSR           ; Clear target abort error bit
                            000100
2101      P:0004E9 P:0004EB 507000            MOVE              A0,X:EC_TAB
                            00004B
2102      P:0004EB P:0004ED 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2103      P:0004ED P:0004EF 00000C            RTS
2104                                ERROR_MAB
2105      P:0004EE P:0004F0 50F000            MOVE              X:EC_MAB,A0
                            00004C
2106      P:0004F0 P:0004F2 000008            INC     A
2107      P:0004F1 P:0004F3 08F48A            MOVEP             #$0080,X:DPSR           ; Clear master abort error bit
                            000080
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 42



2108      P:0004F3 P:0004F5 507000            MOVE              A0,X:EC_MAB
                            00004C
2109      P:0004F5 P:0004F7 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2110      P:0004F7 P:0004F9 00000C            RTS
2111                                ERROR_DPER
2112      P:0004F8 P:0004FA 50F000            MOVE              X:EC_DPER,A0
                            00004D
2113      P:0004FA P:0004FC 000008            INC     A
2114      P:0004FB P:0004FD 08F48A            MOVEP             #$0040,X:DPSR           ; Clear data parity error bit
                            000040
2115      P:0004FD P:0004FF 507000            MOVE              A0,X:EC_DPER
                            00004D
2116      P:0004FF P:000501 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2117      P:000501 P:000503 00000C            RTS
2118                                ERROR_APER
2119      P:000502 P:000504 50F000            MOVE              X:EC_APER,A0
                            00004E
2120      P:000504 P:000506 000008            INC     A
2121      P:000505 P:000507 08F48A            MOVEP             #$0020,X:DPSR           ; Clear address parity error bit
                            000020
2122      P:000507 P:000509 507000            MOVE              A0,X:EC_APER
                            00004E
2123      P:000509 P:00050B 0A702E            BSET    #PCIDMA_RESTART,X:STATUS
                            000000
2124      P:00050B P:00050D 00000C            RTS
2125   
2126                                ;----------------------------------------------
2127                                BLOCK_TRANSFER
2128                                ;----------------------------------------------
2129                                ;   In:
2130                                ;   - BLOCK_DEST_HI:BLOCK_DEST_LO is PC RAM address
2131                                ;   - BLOCK_SIZE is packet size, in bytes
2132                                ;   - BLOCK_SRC is start of data in Y memory
2133                                ;  Out:
2134                                ;   - BLOCK_SIZE will be decremented to zero.
2135                                ;   - BLOCK_DEST_HI:LO will be incremented by BLOCK_SIZE
2136                                ;   - BLOCK_SRC will be incremented by BLOCK_SIZE/2
2137                                ;  Trashes:
2138                                ;   - A and B
2139   
2140      
2141      P:00050C P:00050E 56F000            MOVE              X:BLOCK_SIZE,A          ; A1 = BLOCK_SIZE
                            000042
2142   
2143      P:00050E P:000510 014085            CMP     #0,A
2144      P:00050F P:000511 0AF0AA            JEQ     BLOCK_DONE
                            000553
2145   
2146      P:000511 P:000513 57F400            MOVE              #$0100,B                ; B1 = 256
                            000100
2147      P:000513 P:000515 000000            NOP
2148   
2149      P:000514 P:000516 200005            CMP     B,A                               ; A ? B
2150      P:000515 P:000517 0E1517            JGE     <BLOCK_TRANSFER1                  ; jump if A >= B
2151      P:000516 P:000518 21CF00            MOVE              A,B                     ;    B=A
2152                                BLOCK_TRANSFER1
2153      P:000517 P:000519 200014            SUB     B,A                               ; A -= B
2154      P:000518 P:00051A 014088            ADD     #0,B                              ; Clear carry bit
2155      P:000519 P:00051B 567000            MOVE              A,X:BLOCK_SIZE          ; Updated BLOCK_SIZE
                            000042
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 43



2156      P:00051B P:00051D 577000            MOVE              B,X:BURST_SIZE          ; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100)
)
                            000043
2157      P:00051D P:00051F 0C1CB3            ASR     #25,B,B                           ; B0 = # of 16 bit words
2158   
2159      
2160      P:00051E P:000520 08F4AE            MOVEP             #DTXM,X:DDR0            ; DMA dest'n
                            FFFFCC
2161      P:000520 P:000522 50F000            MOVE              X:BURST_SRC,A0
                            000046
2162      P:000522 P:000524 08C82F            MOVEP             A0,X:DSR0               ; DMA source
2163      P:000523 P:000525 200010            ADD     B,A
2164      P:000524 P:000526 00000B            DEC     B
2165      P:000525 P:000527 507000            MOVE              A0,X:BURST_SRC          ; BURST_SRC += BURST_SIZE/2
                            000046
2166   
2167      P:000527 P:000529 08C92D            MOVEP             B0,X:DCO0               ; DMA length = BURST_SIZE/2 - 1
2168   
2169      
2170      P:000528 P:00052A 08F4AC            MOVEP             #$8EFA51,X:DCR0
                            8EFA51
2171   
2172                                BLOCK_PCI
2173      
2174      P:00052A P:00052C 200013            CLR     A
2175      P:00052B P:00052D 20001B            CLR     B
2176      P:00052C P:00052E 51F000            MOVE              X:BURST_SIZE,B0
                            000043
2177      P:00052E P:000530 00000B            DEC     B
2178      P:00052F P:000531 014088            ADD     #0,B                              ; Clear carry
2179      P:000530 P:000532 0C1C85            ASR     #2,B,B                            ; BURST_SIZE / 4
2180      P:000531 P:000533 014088            ADD     #0,B                              ; Clear carry
2181      P:000532 P:000534 0C1DA1            ASL     #16,B,B                           ; B[23:16] = " "
2182   
2183      P:000533 P:000535 50F000            MOVE              X:BURST_DEST_HI,A0
                            000045
2184      P:000535 P:000537 200010            ADD     B,A
2185      P:000536 P:000538 000000            NOP
2186      P:000537 P:000539 507000            MOVE              A0,X:DPMC               ; PCI burst length and HI address
                            FFFFC7
2187   
2188      P:000539 P:00053B 280700            MOVE              #$07,A0
2189      P:00053A P:00053C 014088            ADD     #0,B                              ; Clear carry
2190      P:00053B P:00053D 0C1D20            ASL     #16,A,A
2191      P:00053C P:00053E 51F000            MOVE              X:BURST_DEST_LO,B0
                            000044
2192      P:00053E P:000540 200010            ADD     B,A
2193      P:00053F P:000541 000000            NOP
2194   
2195      P:000540 P:000542 08C808            MOVEP             A0,X:DPAR               ; PCI LO address and GO
2196   
2197                                BLOCK_CHECK
2198      P:000541 P:000543 000000            NOP
2199      P:000542 P:000544 000000            NOP
2200      P:000543 P:000545 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for burst termination
                            000543
2201   
2202      
2203      P:000545 P:000547 0A8AAE            JSET    #MDT,X:DPSR,BLOCK_OK
                            00054E
2204   
2205      P:000547 P:000549 0D04B2            JSR     MPCI_ERROR_RECOVERY
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 44



2206   
2207                                ;       MOVE    #$000101,A
2208                                ;       NOP
2209                                ;       MOVE    A1,X:BDEBUG1
2210   
2211      P:000548 P:00054A 0A700E            BCLR    #PCIDMA_RESTART,X:STATUS          ; Test and clear
                            000000
2212      P:00054A P:00054C 0E8554            JCS     <BLOCK_RESTART
2213   
2214                                ;       MOVE    #$000102,A
2215                                ;       NOP
2216                                ;       MOVE    A1,X:BDEBUG1
2217   
2218      P:00054B P:00054D 0A700F            BCLR    #PCIDMA_RESUME,X:STATUS           ; Test and clear
                            000000
2219      P:00054D P:00054F 0E8555            JCS     <BLOCK_RESUME
2220   
2221                                ;       MOVE    #$000103,A
2222                                ;       NOP
2223                                ;       MOVE    A1,X:BDEBUG1
2224   
2225                                BLOCK_OK
2226      P:00054E P:000550 50F000            MOVE              X:BURST_SIZE,A0
                            000043
2227      P:000550 P:000552 0BF080            JSR     BLOCK_UPDATE
                            000568
2228      P:000552 P:000554 0C050C            JMP     BLOCK_TRANSFER                    ; Finish the block
2229                                BLOCK_DONE
2230      P:000553 P:000555 00000C            RTS                                       ; Done
2231   
2232                                BLOCK_RESTART
2233      P:000554 P:000556 0C052A            JMP     BLOCK_PCI                         ; Recalculate pci and resend
2234   
2235                                BLOCK_RESUME
2236      P:000555 P:000557 200013            CLR     A
2237      P:000556 P:000558 20001B            CLR     B
2238      P:000557 P:000559 08480A            MOVEP             X:DPSR,A0               ; Get words left to write
2239      P:000558 P:00055A 0A8A8F            JCLR    #15,X:DPSR,BLOCK_RESUME1
                            00055C
2240   
2241                                ;       MOVE    X:BDEBUG4,B1
2242                                ;       NOP
2243                                ;       ADD     #$000001,B
2244                                ;       NOP
2245                                ;       MOVE    B1,X:BDEBUG4
2246   
2247      P:00055A P:00055C 20001B            CLR     B
2248      P:00055B P:00055D 000009            INC     B
2249   
2250                                BLOCK_RESUME1
2251      P:00055C P:00055E 000009            INC     B                                 ; We want N, not N-1.
2252      P:00055D P:00055F 014088            ADD     #0,B                              ; Clear carry
2253      P:00055E P:000560 0C1C20            ASR     #16,A,A
2254      P:00055F P:000561 200018            ADD     A,B                               ; B is words remaining
2255      P:000560 P:000562 014088            ADD     #0,B                              ; Clear carry
2256      P:000561 P:000563 0C1D85            ASL     #2,B,B                            ; Number of bytes left to transfer
2257      P:000562 P:000564 50F000            MOVE              X:BURST_SIZE,A0
                            000043
2258      P:000564 P:000566 200014            SUB     B,A                               ; A is words written
2259   
2260                                ;       NOP
2261                                ;       MOVE    A0,X:BDEBUG2
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 45



2262   
2263      P:000565 P:000567 0BF080            JSR     BLOCK_UPDATE
                            000568
2264      P:000567 P:000569 0C052A            JMP     BLOCK_PCI                         ; Recalculate pci and resend
2265   
2268                                BLOCK_UPDATE
2269      
2270      
2271      P:000568 P:00056A 51F000            MOVE              X:BURST_DEST_LO,B0
                            000044
2272      P:00056A P:00056C 200018            ADD     A,B
2273      P:00056B P:00056D 000000            NOP
2274      P:00056C P:00056E 517000            MOVE              B0,X:BURST_DEST_LO
                            000044
2275   
2276      P:00056E P:000570 51F000            MOVE              X:BURST_SIZE,B0
                            000043
2277      P:000570 P:000572 20001C            SUB     A,B
2278      P:000571 P:000573 000000            NOP
2279      P:000572 P:000574 517000            MOVE              B0,X:BURST_SIZE
                            000043
2280      P:000574 P:000576 00000C            RTS
2281   
2284                                ; BLOCK_UPDATE_ERR
2285                                ;       ;; Use A (number of bytes bursted) to update
2286                                ;       ;;  BURST_DEST_HI:LO and BURST_SIZE
2287                                ;       MOVE    X:BURST_DEST_LO,B0
2288   
2289                                ;       NOP
2290                                ;       MOVE    B0,X:BDEBUG3
2291   
2292                                ;       ADD     A,B
2293                                ;       NOP
2294                                ;       MOVE    B0,X:BURST_DEST_LO
2295   
2296                                ;       MOVE    X:BURST_SIZE,B0
2297                                ;       SUB     A,B
2298                                ;       NOP
2299                                ;       MOVE    B0,X:BURST_SIZE
2300   
2301                                ;       MOVE    X:BLOCK_SIZE,B0
2302                                ;       MOVE    X:BURST_SIZE,A0
2303                                ;       MOVE    B0,X:BDEBUG5
2304                                ;       MOVE    A0,X:BDEBUG6
2305   
2306                                ;       MOVE    X:BURST_SRC,B0
2307                                ;       MOVE    X:FRAME_COUNT,A0
2308                                ;       MOVE    B0,X:BDEBUG7
2309                                ;       MOVE    A0,X:BDEBUG8
2310   
2311   
2312                                ;       RTS
2313   
2314                                BOOTCODE_END
2315                                 BOOTEND_ADDR
2316      000575                              EQU     @CVI(BOOTCODE_END)
2317   
2318                                PROGRAM_END
2319      000575                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2320                                ;---------------------------------------------
2321   
2322   
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 46



2323                                ; --------------------------------------------------------------------
2324                                ; --------------- x memory parameter table ---------------------------
2325                                ; --------------------------------------------------------------------
2326   
2327      X:000000 P:000577                   ORG     X:VAR_TBL,P:
2328   
2329   
2330                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2331                                 VAR_TBL_START
2332      000575                              EQU     @LCV(L)-2
2333                                          ENDIF
2334   
2335                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2337                                          ENDIF
2338   
2339                                ; -----------------------------------------------
2340                                ; do not move these (X:0 --> x:3)
2341 d    X:000000 P:000577 000000  STATUS    DC      0
2342 d                               FRAME_COUNT
2343 d    X:000001 P:000578 000000            DC      0                                 ; used as a check....... increments for ever
y frame write.....must be cleared by host.
2344 d                               PRE_CORRUPT
2345 d    X:000002 P:000579 000000            DC      0
2346   
2347 d    X:000003 P:00057A 550103  REV_NUMBER DC     $550103                           ; byte 0 = minor revision #
2348                                                                                    ; byte 1 = major revision #
2349                                                                                    ; byte 2 = release Version (ascii letter)
2350 d    X:000004 P:00057B 250507  REV_DATA  DC      $250507                           ; data: day-month-year
2351 d    X:000005 P:00057C 2EF490  P_CHECKSUM DC     $2EF490                           ;**** DO NOT CHANGE
2352                                ; -------------------------------------------------
2353 d    X:000006 P:00057D 000000  WORD_COUNT DC     0                                 ; word count.  Number of words successfully 
writen to host in last packet.
2354 d    X:000007 P:00057E 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2355                                ; ----------------------------------------------------------------------------------------------
----------------
2356   
2357 d    X:000008 P:00057F 000000  DRXR_WD1  DC      0
2358 d    X:000009 P:000580 000000  DRXR_WD2  DC      0
2359 d    X:00000A P:000581 000000  DRXR_WD3  DC      0
2360 d    X:00000B P:000582 000000  DRXR_WD4  DC      0
2361 d    X:00000C P:000583 000000  DTXS_WD1  DC      0
2362 d    X:00000D P:000584 000000  DTXS_WD2  DC      0
2363 d    X:00000E P:000585 000000  DTXS_WD3  DC      0
2364 d    X:00000F P:000586 000000  DTXS_WD4  DC      0
2365   
2366 d    X:000010 P:000587 000000  PCI_WD1_1 DC      0
2367 d    X:000011 P:000588 000000  PCI_WD1_2 DC      0
2368 d    X:000012 P:000589 000000  PCI_WD2_1 DC      0
2369 d    X:000013 P:00058A 000000  PCI_WD2_2 DC      0
2370 d    X:000014 P:00058B 000000  PCI_WD3_1 DC      0
2371 d    X:000015 P:00058C 000000  PCI_WD3_2 DC      0
2372 d    X:000016 P:00058D 000000  PCI_WD4_1 DC      0
2373 d    X:000017 P:00058E 000000  PCI_WD4_2 DC      0
2374 d    X:000018 P:00058F 000000  PCI_WD5_1 DC      0
2375 d    X:000019 P:000590 000000  PCI_WD5_2 DC      0
2376 d    X:00001A P:000591 000000  PCI_WD6_1 DC      0
2377 d    X:00001B P:000592 000000  PCI_WD6_2 DC      0
2378   
2379   
2380 d    X:00001C P:000593 000000  HEAD_W1_1 DC      0
2381 d    X:00001D P:000594 000000  HEAD_W1_0 DC      0
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 47



2382 d    X:00001E P:000595 000000  HEAD_W2_1 DC      0
2383 d    X:00001F P:000596 000000  HEAD_W2_0 DC      0
2384 d    X:000020 P:000597 000000  HEAD_W3_1 DC      0
2385 d    X:000021 P:000598 000000  HEAD_W3_0 DC      0
2386 d    X:000022 P:000599 000000  HEAD_W4_1 DC      0
2387 d    X:000023 P:00059A 000000  HEAD_W4_0 DC      0
2388   
2389   
2390 d    X:000024 P:00059B 000000  REP_WD1   DC      0
2391 d    X:000025 P:00059C 000000  REP_WD2   DC      0
2392 d    X:000026 P:00059D 000000  REP_WD3   DC      0
2393 d    X:000027 P:00059E 000000  REP_WD4   DC      0
2394   
2395 d    X:000028 P:00059F 000000  SV_A0     DC      0
2396 d    X:000029 P:0005A0 000000  SV_A1     DC      0
2397 d    X:00002A P:0005A1 000000  SV_A2     DC      0
2398 d    X:00002B P:0005A2 000000  SV_B0     DC      0
2399 d    X:00002C P:0005A3 000000  SV_B1     DC      0
2400 d    X:00002D P:0005A4 000000  SV_B2     DC      0
2401 d    X:00002E P:0005A5 000000  SV_X0     DC      0
2402 d    X:00002F P:0005A6 000000  SV_X1     DC      0
2403 d    X:000030 P:0005A7 000000  SV_Y0     DC      0
2404 d    X:000031 P:0005A8 000000  SV_Y1     DC      0
2405   
2406 d    X:000032 P:0005A9 000000  SV_SR     DC      0                                 ; stauts register save.
2407   
2408 d    X:000033 P:0005AA 000000  ZERO      DC      0
2409 d    X:000034 P:0005AB 000001  ONE       DC      1
2410 d    X:000035 P:0005AC 000004  FOUR      DC      4
2411   
2412   
2413   
2414 d                               PACKET_SIZE_LOW
2415 d    X:000036 P:0005AD 000000            DC      0
2416 d                               PACKET_SIZE_HIH
2417 d    X:000037 P:0005AE 000000            DC      0
2418   
2419 d    X:000038 P:0005AF 00A5A5  PREAMB1   DC      $A5A5                             ; pramble 16-bit word....2 of which make up 
first preamble 32bit word
2420 d    X:000039 P:0005B0 005A5A  PREAMB2   DC      $5A5A                             ; preamble 16-bit word....2 of which make up
 second preamble 32bit word
2421 d    X:00003A P:0005B1 004441  DATA_WD   DC      $4441                             ; "DA"
2422 d    X:00003B P:0005B2 005250  REPLY_WD  DC      $5250                             ; "RP"
2423   
2424 d                               TOTAL_BUFFS
2425 d    X:00003C P:0005B3 000000            DC      0                                 ; total number of 512 buffers in packet
2426 d                               LEFT_TO_READ
2427 d    X:00003D P:0005B4 000000            DC      0                                 ; number of words (16 bit) left to read afte
r last 512 buffer
2428 d                               LEFT_TO_WRITE
2429 d    X:00003E P:0005B5 000000            DC      0                                 ; number of woreds (32 bit) to write to host
 i.e. half of those left over read
2430 d                               NUM_LEFTOVER_BLOCKS
2431 d    X:00003F P:0005B6 000000            DC      0                                 ; small block DMA burst transfer
2432   
2433 d                               DATA_DLY_VAL
2434 d    X:000040 P:0005B7 000000            DC      0                                 ; data delay value..  Delay added to first f
rame received after GO command
2435 d    X:000041 P:0005B8 000200  CONSTORE  DC      $200
2436   
2438   
2439 d    X:000042 P:0005B9 000000  BLOCK_SIZE DC     0
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 48



2440 d    X:000043 P:0005BA 000000  BURST_SIZE DC     0
2441 d                               BURST_DEST_LO
2442 d    X:000044 P:0005BB 000000            DC      0
2443 d                               BURST_DEST_HI
2444 d    X:000045 P:0005BC 000000            DC      0
2445 d    X:000046 P:0005BD 000000  BURST_SRC DC      0
2446   
2447 d    X:000047 P:0005BE 000000  DMA_ERRORS DC     0
2448 d    X:000048 P:0005BF 000000  EC_TRTY   DC      0
2449 d    X:000049 P:0005C0 000000  EC_TO     DC      0
2450 d    X:00004A P:0005C1 000000  EC_TDIS   DC      0
2451 d    X:00004B P:0005C2 000000  EC_TAB    DC      0
2452 d    X:00004C P:0005C3 000000  EC_MAB    DC      0
2453 d    X:00004D P:0005C4 000000  EC_DPER   DC      0
2454 d    X:00004E P:0005C5 000000  EC_APER   DC      0
2455   
2456                                ; BDEBUG1                       DC      0
2457                                ; BDEBUG2                       DC      0
2458                                ; BDEBUG3                       DC      0
2459                                ; BDEBUG4                       DC      0
2460                                ; BDEBUG5                       DC      0
2461                                ; BDEBUG6                       DC      0
2462                                ; BDEBUG7                       DC      0
2463                                ; BDEBUG8                       DC      0
2464                                ; BDEBUG9                       DC      0
2465   
2466                                ;----------------------------------------------------------
2467   
2468   
2469   
2470                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2471                                 VAR_TBL_END
2472      0005C4                              EQU     @LCV(L)-2
2473                                          ENDIF
2474   
2475                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2477                                          ENDIF
2478   
2479                                 VAR_TBL_LENGTH
2480      00004F                              EQU     VAR_TBL_END-VAR_TBL_START
2481   
2482   
2483                                          IF      @CVS(N,*)>=APPLICATION
2485                                          ENDIF
2486   
2487   
2488                                ;--------------------------------------------
2489                                ; APPLICATION AREA
2490                                ;---------------------------------------------
2491                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2492      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2493                                          ENDIF
2494   
2495                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2497                                          ENDIF
2498   
2499                                ; starts with no application loaded
2500                                ; so just reply with an error if we get a GOA command
2501      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2502      P:000802 P:000804 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
2503      P:000803 P:000805 44F400            MOVE              #'GOA',X0
Motorola DSP56300 Assembler  Version 6.3.4   07-11-19  00:52:16  main.asm  Page 49



                            474F41
2504      P:000805 P:000807 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2505      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2506      P:000808 P:00080A 440E00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2507      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2508      P:00080B P:00080D 440F00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2509      P:00080C P:00080E 0D0469            JSR     <RESTORE_REGISTERS
2510      P:00080D P:00080F 0D0421            JSR     <PCI_MESSAGE_TO_HOST
2511      P:00080E P:000810 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS
2512      P:00080F P:000811 0C0173            JMP     PACKET_IN
2513   
2514   
2515      000812                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM
2516   
2517   
2518   

0    Errors
0    Warnings


