Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_build.asm  Page 1



1                           COMMENT *
2      
3                          Compile this to build all files together.
4      
5                          Project:     SCUBA 2
6                          Author:      DAVID ATKINSON
7                          Target:      250MHz SDSU PCI card - DSP56301
8                          Controller:  For use with SCUBA 2 Multichannel Electronics
9      
10     
11                         Assembler directives:
12                                 ROM=EEPROM => EEPROM CODE
13                                 ROM=ONCE => ONCE CODE
14     
15                                 *
16                                   PAGE    132                               ; Printronix page width - 132 columns
17                                   OPT     CEX                               ; print DC evaluations
18     
**** 19 [PCI_SCUBA_build.asm 19]:  Build all files together here 
19                                   MSG     ' Build all files together here '
20     
21                                   INCLUDE 'PCI_SCUBA_header.asm'
22                               COMMENT *
23     
24                         PCI code header file.
25     
26                         Project:     SCUBA 2
27                         Author:      DAVID ATKINSON
28                         Target:      250MHz SDSU PCI card - DSP56301
29                         Controller:  For use with SCUBA 2 Multichannel Electronics
30     
31     
32                         Assembler directives:
33                                 ROM=0 => EEPROM CODE
34                                 ROM=1 => ROM CODE
35     
36                                 *
37                                   PAGE    132                               ; Printronix page width - 132 columns
38                                   OPT     CEX                               ; print DC evaluations
39     
**** 40 [PCI_SCUBA_header.asm 19]:  INCLUDE PCI_header.asm HERE  
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
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_header.asm  Page 2



61        000000                     EQU     0                                 ; location in y memory of image buffer....
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
98     
99                         ; HST timeout recovery....
100    
101       000200           MAX_DUMP  EQU     512                               ; if HST timeout.. max number that could be in FIFO i
s 511..
102       001000           DUMP_BUFF EQU     $1000                             ; store in Y memory above normal data buffer: in off-
chip RAM
103    
104    
105    
106                        ; Various addressing control registers
107       FFFFFB           BCR       EQU     $FFFFFB                           ; Bus Control Register
108       FFFFFA           DCR       EQU     $FFFFFA                           ; DRAM Control Register
109       FFFFF9           AAR0      EQU     $FFFFF9                           ; Address Attribute Register, channel 0
110       FFFFF8           AAR1      EQU     $FFFFF8                           ; Address Attribute Register, channel 1
111       FFFFF7           AAR2      EQU     $FFFFF7                           ; Address Attribute Register, channel 2
112       FFFFF6           AAR3      EQU     $FFFFF6                           ; Address Attribute Register, channel 3
113       FFFFFD           PCTL      EQU     $FFFFFD                           ; PLL control register
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_header.asm  Page 3



114       FFFFFE           IPRP      EQU     $FFFFFE                           ; Interrupt Priority register - Peripheral
115       FFFFFF           IPRC      EQU     $FFFFFF                           ; Interrupt Priority register - Core
116    
117                        ; PCI control register
118       FFFFCD           DTXS      EQU     $FFFFCD                           ; DSP Slave transmit data FIFO
119       FFFFCC           DTXM      EQU     $FFFFCC                           ; DSP Master transmit data FIFO
120       FFFFCB           DRXR      EQU     $FFFFCB                           ; DSP Receive data FIFO
121       FFFFCA           DPSR      EQU     $FFFFCA                           ; DSP PCI Status Register
122       FFFFC9           DSR       EQU     $FFFFC9                           ; DSP Status Register
123       FFFFC8           DPAR      EQU     $FFFFC8                           ; DSP PCI Address Register
124       FFFFC7           DPMC      EQU     $FFFFC7                           ; DSP PCI Master Control Register
125       FFFFC6           DPCR      EQU     $FFFFC6                           ; DSP PCI Control Register
126       FFFFC5           DCTR      EQU     $FFFFC5                           ; DSP Control Register
127    
128                        ; Port E is the Synchronous Communications Interface (SCI) port
129       FFFF9F           PCRE      EQU     $FFFF9F                           ; Port Control Register
130       FFFF9E           PRRE      EQU     $FFFF9E                           ; Port Direction Register
131       FFFF9D           PDRE      EQU     $FFFF9D                           ; Port Data Register
132    
133                        ; Various PCI register bit equates
134       000001           STRQ      EQU     1                                 ; Slave transmit data request (DSR)
135       000002           SRRQ      EQU     2                                 ; Slave receive data request (DSR)
136       000017           HACT      EQU     23                                ; Host active, low true (DSR)
137       000001           MTRQ      EQU     1                                 ; Set whem master transmitter is not full (DPSR)
138       000004           MARQ      EQU     4                                 ; Master address request (DPSR)
139       000002           MRRQ      EQU     2                                 ; Master Receive Request (DPSR)
140       00000A           TRTY      EQU     10                                ; PCI Target Retry (DPSR)
141    
142       000005           APER      EQU     5                                 ; Address parity error
143       000006           DPER      EQU     6                                 ; Data parity error
144       000007           MAB       EQU     7                                 ; Master Abort
145       000008           TAB       EQU     8                                 ; Target Abort
146       000009           TDIS      EQU     9                                 ; Target Disconnect
147       00000B           TO        EQU     11                                ; Timeout
148       00000E           MDT       EQU     14                                ; Master Data Transfer complete
149       000002           SCLK      EQU     2                                 ; SCLK = transmitter special code
150    
151                        ; bits in DPMC
152    
153       000017           FC1       EQU     23
154       000016           FC0       EQU     22
155    
156    
157                        ; DMA register definitions
158       FFFFEF           DSR0      EQU     $FFFFEF                           ; Source address register
159       FFFFEE           DDR0      EQU     $FFFFEE                           ; Destination address register
160       FFFFED           DCO0      EQU     $FFFFED                           ; Counter register
161       FFFFEC           DCR0      EQU     $FFFFEC                           ; Control register
162    
163                        ; The DCTR host flags are written by the DSP and read by PCI host
164       000003           DCTR_HF3  EQU     3                                 ; used as a semiphore for INTA handshaking
165       000004           DCTR_HF4  EQU     4                                 ;
166       000005           DCTR_HF5  EQU     5                                 ;
167       000006           INTA      EQU     6                                 ; Request PCI interrupt
168    
169                        ; The DSR host flags are written by the PCI host and read by the DSP
170       000004           DSR_BUF0  EQU     4                                 ; PCI host sets this when copying buffer 0
171       000005           DSR_BUF1  EQU     5                                 ; PCI host sets this when copying buffer 1
172    
173                        ; DPCR bit definitions
174       00000E           CLRT      EQU     14                                ; Clear transmitter
175       000012           MACE      EQU     18                                ; Master access counter enable
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_header.asm  Page 4



176       000015           IAE       EQU     21                                ; Insert Address Enable
177    
178                        ; Addresses of ESSI port
179       FFFFBC           TX00      EQU     $FFFFBC                           ; Transmit Data Register 0
180       FFFFB7           SSISR0    EQU     $FFFFB7                           ; Status Register
181       FFFFB6           CRB0      EQU     $FFFFB6                           ; Control Register B
182       FFFFB5           CRA0      EQU     $FFFFB5                           ; Control Register A
183    
184                        ; SSI Control Register A Bit Flags
185       000006           TDE       EQU     6                                 ; Set when transmitter data register is empty
186    
187                        ; Miscellaneous addresses
188       FFFFFF           RDFIFO    EQU     $FFFFFF                           ; Read the FIFO for incoming fiber optic data
189       FFFF8F           TCSR0     EQU     $FFFF8F                           ; Triper timer control and status register 0
190       FFFF8B           TCSR1     EQU     $FFFF8B                           ; Triper timer control and status register 1
191       FFFF87           TCSR2     EQU     $FFFF87                           ; Triper timer control and status register 2
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
210    
211                        ; Bit number definitions of GPIO pins on Port D
212       000000           EF        EQU     0                                 ; FIFO Empty flag, low true
213       000001           HF        EQU     1                                 ; FIFO half full flag, low true
214       000002           RS        EQU     2                                 ; FIFO reset signal, low true
215       000003           FSYNC     EQU     3                                 ; High during image transmission
216       000004           AUX1      EQU     4                                 ; enable/disable byte swapping
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
233    
234    
235                                  INCLUDE 'PCI_SCUBA_initialisation.asm'
236                              COMMENT *
237    
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_initialisation.asm  Page 5



238                        This is the code which is executed first after power-up etc.
239                        It sets all the internal registers to their operating values,
240                        sets up the ISR vectors and inialises the hardware etc.
241    
242                        Project:     SCUBA 2
243                        Author:      DAVID ATKINSON
244                        Target:      250MHz SDSU PCI card - DSP56301
245                        Controller:  For use with SCUBA 2 Multichannel Electronics
246    
247                        Assembler directives:
248                                ROM=EEPROM => EEPROM CODE
249                                ROM=ONCE => ONCE CODE
250    
251                                *
252                                  PAGE    132                               ; Printronix page width - 132 columns
253                                  OPT     CEX                               ; print DC evaluations
254    
**** 255 [PCI_SCUBA_initialisation.asm 20]:  INCLUDE PCI_initialisation.asm HERE  
255                                  MSG     ' INCLUDE PCI_initialisation.asm HERE  '
256    
257                        ; The EEPROM boot code expects first to read 3 bytes specifying the number of
258                        ; program words, then 3 bytes specifying the address to start loading the
259                        ; program words and then 3 bytes for each program word to be loaded.
260                        ; The program words will be condensed into 24 bit words and stored in contiguous
261                        ; PRAM memory starting at the specified starting address. Program execution
262                        ; starts from the same address where loading started.
263    
264                        ; Special address for two words for the DSP to bootstrap code from the EEPROM
265                                  IF      @SCP("ROM","ROM")                 ; Boot from ROM on power-on
266       P:000000 P:000000                   ORG     P:0,P:0
267  d    P:000000 P:000000 000810            DC      END_ADR-INIT-2                    ; Number of boot words
268  d    P:000001 P:000001 000000            DC      INIT                              ; Starting address
269       P:000000 P:000002                   ORG     P:0,P:2
270       P:000000 P:000002 0C0030  INIT      JMP     <INIT_PCI                         ; Configure PCI port
271       P:000001 P:000003 000000            NOP
272                                           ENDIF
273    
274    
275                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
276                                 ; command converter
277                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
281                                           ENDIF
282    
283                                 ; Vectored interrupt table, addresses at the beginning are reserved
284  d    P:000002 P:000004 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0       ; $02-$0f Reserved
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
285  d    P:000010 P:000012 000000            DC      0,0                               ; $10-$13 Reserved
     d                      000000
286    
287                                 ; FIFO HF* flag interrupt vector is here at $12 - this is connected to the
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_initialisation.asm  Page 6



288                                 ; IRQB* interrupt line so its ISR vector must be here
289  d    P:000012 P:000014 000000            DC      0,0                               ; $was ld scatter routine ...HF*
     d                      000000
290    
291                                 ; a software reset button on the font panel of the card is connected to the IRQC*
292                                 ; line which if pressed causes the DSP to jump to an ISR which causes the program
293                                 ; counter to the beginning of the program INIT and sets the stack pointer to TOP.
294       P:000014 P:000016 0BF080            JSR     CLEAN_UP_PCI                      ; $14 - Software reset switch
                            000235
295    
296  d    P:000016 P:000018 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Reserved interrupts
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
297  d    P:000022 P:000024 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0
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
298    
299                                 ; Now we're at P:$30, where some unused vector addresses are located
300                                 ; This is ROM only code that is only executed once on power-up when the
301                                 ; ROM code is downloaded. It is skipped over on OnCE downloads.
302    
303                                 ; A few seconds after power up on the Host, it interrogates the PCI bus to find
304                                 ; out what boards are installed and configures this PCI board. The EEPROM booting
305                                 ; procedure ends with program execution  starting at P:$0 where the EEPROM has
306                                 ; inserted a JMP INIT_PCI instruction. This routine sets the PLL paramter and
307                                 ; does a self configuration and software reset of the PCI controller in the DSP.
308                                 ; After configuring the PCI controller the DSP program overwrites the instruction
309                                 ; at P:$0 with a new JMP START to skip over the INIT_PCI routine. The program at
310                                 ; START address begins configuring the DSP and processing commands.
311                                 ; Similarly the ONCE option places a JMP START at P:$0 to skip over the
312                                 ; INIT_PCI routine. If this routine where executed after the host computer had booted
313                                 ; it would cause it to crash since the host computer would overwrite the
314                                 ; configuration space with its own values and doesn't tolerate foreign values.
315    
316                                 ; Initialize the PLL - phase locked loop
317                                 INIT_PCI
318       P:000030 P:000032 08F4BD            MOVEP             #PLL_INIT,X:PCTL        ; Initialize PLL
                            050003
319       P:000032 P:000034 000000            NOP
320    
321                                 ; Program the PCI self-configuration registers
322       P:000033 P:000035 240000            MOVE              #0,X0
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_initialisation.asm  Page 7



323       P:000034 P:000036 08F485            MOVEP             #$500000,X:DCTR         ; Set self-configuration mode
                            500000
324       P:000036 P:000038 0604A0            REP     #4
325       P:000037 P:000039 08C408            MOVEP             X0,X:DPAR               ; Dummy writes to configuration space
326       P:000038 P:00003A 08F487            MOVEP             #>$0000,X:DPMC          ; Subsystem ID
                            000000
327       P:00003A P:00003C 08F488            MOVEP             #>$0000,X:DPAR          ; Subsystem Vendor ID
                            000000
328    
329                                 ; PCI Personal reset
330       P:00003C P:00003E 08C405            MOVEP             X0,X:DCTR               ; Personal software reset
331       P:00003D P:00003F 000000            NOP
332       P:00003E P:000040 000000            NOP
333       P:00003F P:000041 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00003F
334       P:000041 P:000043 07F084            MOVE              P:(*+3),X0              ; Trick to write "JMP <START" to P:0
                            000044
335       P:000043 P:000045 070004            MOVE              X0,P:(0)
336       P:000044 P:000046 0C0100            JMP     <START
337    
338  d    P:000045 P:000047 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
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
339  d    P:000051 P:000053 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; Filler
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
340  d    P:00005D P:00005F 000000            DC      0,0,0,0,0,0,0,0,0,0,0,0           ; $60-$71 Reserved PCI
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
341    
342                                 ;**************************************************************************
343                                 ; Check for program space overwriting of ISR starting at P:$72
344                                           IF      @CVS(N,*)>$71
346                                           ENDIF
347    
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_initialisation.asm  Page 8



348                                 ;       ORG     P:$72,P:$72
349       P:000072 P:000074                   ORG     P:$72,P:$74
350    
351                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
352                                 ; command converter
353                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
355                                           ENDIF
356    
357    
358                                 ;**************************************************************************
359    
360                                 ; Three non-maskable fast interrupt service routines for clearing PCI interrupts
361                                 ; The Host will use these to clear the INTA* after it has serviced the interrupt
362                                 ; which had been generated by the PCI board.
363    
364       P:000072 P:000074 0A8506            BCLR    #INTA,X:DCTR                      ; $72/3 - Clear PCI interrupt
365       P:000073 P:000075 000000            NOP
366    
367       P:000074 P:000076 0A8503            BCLR    #DCTR_HF3,X:DCTR                  ; clear interrupt flag
368       P:000075 P:000077 000000            NOP                                       ; needs to be fast addressing <
369    
370       P:000076 P:000078 0A0022            BSET    #FATAL_ERROR,X:<STATUS            ; $76/7 - driver informing us of PCI_MESSAGE
_TO_HOST error
371       P:000077 P:000079 000000            NOP
372    
373                                 ; Interrupt locations for 7 available commands on PCI board
374                                 ; Each JSR takes up 2 locations in the table
375       P:000078 P:00007A 0BF080            JSR     WRITE_MEMORY                      ; $78
                            0003A0
376       P:00007A P:00007C 0BF080            JSR     READ_MEMORY                       ; $7A
                            000241
377       P:00007C P:00007E 0BF080            JSR     START_APPLICATION                 ; $7C
                            000360
378       P:00007E P:000080 0BF080            JSR     STOP_APPLICATION                  ; $7E
                            000378
379                                 ; software reset is the same as cleaning up the PCI - use same routine
380                                 ; when HOST does a RESET then this routine is run
381       P:000080 P:000082 0BF080            JSR     SOFTWARE_RESET                    ; $80
                            000328
382       P:000082 P:000084 0BF080            JSR     SEND_PACKET_TO_CONTROLLER         ; $82
                            0002BE
383       P:000084 P:000086 0BF080            JSR     SEND_PACKET_TO_HOST               ; $84
                            000308
384       P:000086 P:000088 0BF080            JSR     RESET_CONTROLLER                  ; $86
                            000280
385    
386    
387                                 ; ***********************************************************************
388                                 ; For now have boot code starting from P:$100
389                                 ; just to make debugging tidier etc.
390    
391       P:000100 P:000102                   ORG     P:$100,P:$102
392    
393                                 ; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
394                                 ; command converter
395                                           IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
397                                           ENDIF
398                                 ; ***********************************************************************
399    
400    
401    
402                                 ; ******************************************************************
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_initialisation.asm  Page 9



403                                 ;
404                                 ;       AA0 = RDFIFO* of incoming fiber optic data
405                                 ;       AA1 = EEPROM access
406                                 ;       AA2 = DRAM access
407                                 ;       AA3 = output to parallel data connector, for a video pixel clock
408                                 ;       $FFxxxx = Write to fiber optic transmitter
409                                 ;
410                                 ; ******************************************************************
411    
412    
413       P:000100 P:000102 08F487  START     MOVEP             #>$000001,X:DPMC
                            000001
414       P:000102 P:000104 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
415       P:000103 P:000105 0A8515            BCLR    #21,X:DCTR
416       P:000104 P:000106 0A8516            BCLR    #22,X:DCTR
417       P:000105 P:000107 000000            NOP
418       P:000106 P:000108 0A8632            BSET    #MACE,X:DPCR                      ; Master access counter enable
419       P:000107 P:000109 000000            NOP
420       P:000108 P:00010A 000000            NOP                                       ; End of PCI programming
421    
422    
423                                 ; Set operation mode register OMR to normal expanded
424       P:000109 P:00010B 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
425       P:00010A P:00010C 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
426    
427                                 ; Program the serial port ESSI0 = Port C for serial transmission to
428                                 ;   the timing board
429       P:00010B P:00010D 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
                            000000
430                                 ;**********************************************************************
431       P:00010D P:00010F 07F435            MOVEP             #$00080B,X:CRA0         ; Divide 100.0 MHz by 24 to get 4.17 MHz
                            00080B
432                                                                                     ; DC0-CD4 = 0 for non-network operation
433                                                                                     ; WL0-WL2 = ALC = 0 for 2-bit data words
434                                                                                     ; SSC1 = 0 for SC1 not used
435                                 ;************************************************************************
436       P:00010F P:000111 07F436            MOVEP             #$010120,X:CRB0         ; SCKD = 1 for internally generated clock
                            010120
437                                                                                     ; SHFD = 0 for MSB shifted first
438                                                                                     ; CKP = 0 for rising clock edge transitions
439                                                                                     ; TE0 = 1 to enable transmitter #0
440                                                                                     ; MOD = 0 for normal, non-networked mode
441                                                                                     ; FSL1 = 1, FSL0 = 0 for on-demand transmit
442       P:000111 P:000113 07F43F            MOVEP             #%101000,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000028
443                                                                                     ; Set SCK0 = P3, STD0 = P5 to ESSI0
444                                 ;********************************************************************************
445       P:000113 P:000115 07F43E            MOVEP             #%111100,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            00003C
446       P:000115 P:000117 07F43D            MOVEP             #%000000,X:PDRC         ; Data Register - AUX3 = i/p, AUX1 not used
                            000000
447                                 ;***********************************************************************************
448                                 ; 250MHz
449                                 ; Conversion from software bits to schematic labels for Port C and D
450                                 ;       PC0 = SC00 = AUX3               PD0 = SC10 = EF*
451                                 ;       PC1 = SC01 = A/B* = input       PD1 = SC11 = HF*
452                                 ;       PC2 = SC02 = No connect         PD2 = SC12 = RS*
453                                 ;       PC3 = SCK0 = No connect         PD3 = SCK1 = NWRFIFO*
454                                 ;       PC4 = SRD0 = AUX1               PD4 = SRD1 = No connect (** in 50Mhz this was MODE selec
t for 16 or 32 bit FO)
455                                 ;       PC5 = STD0 = No connect         PD5 = STD1 = WRFIFO*
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_initialisation.asm  Page 10



456                                 ; ***********************************************************************************
457    
458    
459                                 ; ****************************************************************************
460                                 ; Program the serial port ESSI1 = Port D for general purpose I/O (GPIO)
461    
462       P:000117 P:000119 07F42F            MOVEP             #%000000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000000
463       P:000119 P:00011B 07F42E            MOVEP             #%011100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            00001C
464       P:00011B P:00011D 07F42D            MOVEP             #%010000,X:PDRD         ; Data Register - Pulse RS* low
                            000010
465       P:00011D P:00011F 060AA0            REP     #10
466       P:00011E P:000120 000000            NOP
467       P:00011F P:000121 07F42D            MOVEP             #%010100,X:PDRD         ; Data Register - Pulse RS* high
                            000014
468                                                                                     ; was %011100
469    
470                                 ; Program the SCI port to benign values
471       P:000121 P:000123 07F41F            MOVEP             #%000,X:PCRE            ; Port Control Register = GPIO
                            000000
472       P:000123 P:000125 07F41E            MOVEP             #%110,X:PRRE            ; Port Direction Register (0 = Input)
                            000006
473       P:000125 P:000127 07F41D            MOVEP             #%010,X:PDRE            ; Port Data Register
                            000002
474                                 ;       PE0 = RXD
475                                 ;       PE1 = TXD
476                                 ;       PE2 = SCLK
477    
478                                 ; Program the triple timer to assert TCI0 as an GPIO output = 1
479       P:000127 P:000129 07F40F            MOVEP             #$2800,X:TCSR0
                            002800
480       P:000129 P:00012B 07F40B            MOVEP             #$2800,X:TCSR1
                            002800
481       P:00012B P:00012D 07F407            MOVEP             #$2800,X:TCSR2
                            002800
482    
483    
484                                 ; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
485       P:00012D P:00012F 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
                            FFFC21
486       P:00012F P:000131 08F4B8            MOVEP             #$008929,X:AAR1         ; P = $008000 to $00FFFF asserts AA1 low tru
e
                            008929
487       P:000131 P:000133 08F4B7            MOVEP             #$000122,X:AAR2         ; Y = $000800 to $7FFFFF accesses SRAM
                            000122
488    
489    
490                                 ; Program the DRAM memory access and addressing
491       P:000133 P:000135 08F4BB            MOVEP             #$020022,X:BCR          ; Bus Control Register
                            020022
492       P:000135 P:000137 08F4BA            MOVEP             #$893A05,X:DCR          ; DRAM Control Register
                            893A05
493    
494    
495                                 ; Clear all PCI error conditions
496       P:000137 P:000139 084E0A            MOVEP             X:DPSR,A
497       P:000138 P:00013A 0140C2            OR      #$1FE,A
                            0001FE
498       P:00013A P:00013C 000000            NOP
499       P:00013B P:00013D 08CE0A            MOVEP             A,X:DPSR
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_initialisation.asm  Page 11



500    
501                                 ;--------------------------------------------------------------------
502                                 ; Enable one interrupt only: software reset switch
503       P:00013C P:00013E 08F4BF            MOVEP             #$0001C0,X:IPRC         ; IRQB priority = 1 (FIFO half full HF*)
                            0001C0
504                                                                                     ; IRQC priority = 2 (reset switch)
505       P:00013E P:000140 05F439            MOVE              #$200,SR                ; Mask set up for reset switch only
                            000200
506    
507    
508                                 ;--------------------------------------------------------------------------
509                                 ; Initialize the fiber optic serial transmitter to zero
510       P:000140 P:000142 01B786            JCLR    #TDE,X:SSISR0,*
                            000140
511       P:000142 P:000144 07F43C            MOVEP             #$000000,X:TX00
                            000000
512    
513                                 ;--------------------------------------------------------------------
514    
515                                 ; clear DTXM - PCI master transmitter
516       P:000144 P:000146 0A862E            BSET    #CLRT,X:DPCR                      ; Clear the master transmitter DTXM
517       P:000145 P:000147 0A86AE            JSET    #CLRT,X:DPCR,*                    ; Wait for the clearing to be complete
                            000145
518    
519                                 ;----------------------------------------------------------------------
520                                 ; clear DRXR - PCI receiver
521    
522       P:000147 P:000149 0A8982  CLR0      JCLR    #SRRQ,X:DSR,CLR1                  ; Wait for the receiver to be empty
                            00014C
523       P:000149 P:00014B 08440B            MOVEP             X:DRXR,X0               ; Read receiver to empty it
524       P:00014A P:00014C 000000            NOP
525       P:00014B P:00014D 0C0147            JMP     <CLR0
526                                 CLR1
527    
528                                 ;-----------------------------------------------------------------------------
529                                 ; copy parameter table from P memory into X memory
530    
531                                 ; but not word_count and num_dumped - don't want these reset by fatal error....
532                                 ; they will be reset by new packet or pci_reset ISR
533    
534    
535       P:00014C P:00014E 46F000            MOVE              X:WORD_COUNT,Y0         ; store packet word count
                            000006
536       P:00014E P:000150 47F000            MOVE              X:NUM_DUMPED,Y1         ; store number dumped (after HST TO)
                            000007
537       P:000150 P:000152 45F000            MOVE              X:FRAME_COUNT,X1        ; store frame count
                            000001
538    
539                                 ; Move the table of constants from P: space to X: space
540       P:000152 P:000154 61F400            MOVE              #VAR_TBL_START,R1       ; Start of parameter table in P
                            000543
541       P:000154 P:000156 300000            MOVE              #VAR_TBL,R0             ; start of parameter table in X
542       P:000155 P:000157 064280            DO      #VAR_TBL_LENGTH,X_WRITE
                            000158
543       P:000157 P:000159 07D984            MOVE              P:(R1)+,X0
544       P:000158 P:00015A 445800            MOVE              X0,X:(R0)+              ; Write the constants to X:
545                                 X_WRITE
546    
547    
548       P:000159 P:00015B 467000            MOVE              Y0,X:WORD_COUNT         ; restore packet word count
                            000006
549       P:00015B P:00015D 477000            MOVE              Y1,X:NUM_DUMPED         ; restore number dumped (after HST TO)
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_initialisation.asm  Page 12



                            000007
550       P:00015D P:00015F 457000            MOVE              X1,X:FRAME_COUNT        ; restore frame count
                            000001
551    
552                                 ;-------------------------------------------------------------------------------
553                                 ; initialise some bits in STATUS
554    
555       P:00015F P:000161 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS     ; clear application loaded flag
556       P:000160 P:000162 0A000C            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appliaction running flag
557                                                                                     ; (e.g. not running diagnostic application
558                                                                                     ;      in self_test_mode)
559    
560       P:000161 P:000163 0A0002            BCLR    #FATAL_ERROR,X:<STATUS            ; initialise fatal error flag.
561       P:000162 P:000164 0A0028            BSET    #PACKET_CHOKE,X:<STATUS           ; enable MCE packet choke
562                                                                                     ; HOST not informed of anything from MCE unt
il
563                                                                                     ; comms are opened by host with first CON co
mmand
564    
565       P:000163 P:000165 0A0006            BCLR    #PREAMBLE_ERROR,X:<STATUS         ; flag to let host know premable error
566    
567                                 ;------------------------------------------------------------------------------
568                                 ; disable FIFO HF* intererupt...not used anymore.
569    
570       P:000164 P:000166 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable FIFO HF* interrupt
                            0001C0
571       P:000166 P:000168 05F439            MOVEC             #$200,SR                ; Mask level 1 interrupts
                            000200
572    
573                                 ;----------------------------------------------------------------------------
574                                 ; Disable Byte swapin - enabled after first command to MCE.
575                                 ; i.e after first 'CON'
576    
577       P:000168 P:00016A 0A0005            BCLR    #BYTE_SWAP,X:<STATUS              ; flag to let host know byte swapping off
578       P:000169 P:00016B 013D04            BCLR    #AUX1,X:PDRC                      ; enable disable
579    
580                                 ;----------------------------------------------------------------------------
581                                 ; Initialize PCI controller again, after booting, to make sure it sticks
582       P:00016A P:00016C 0A8514            BCLR    #20,X:DCTR                        ; Terminate and reset mode
583       P:00016B P:00016D 000000            NOP
584       P:00016C P:00016E 0A89B7            JSET    #HACT,X:DSR,*                     ; Test for personal reset completion
                            00016C
585       P:00016E P:000170 000000            NOP
586       P:00016F P:000171 0A8534            BSET    #20,X:DCTR                        ; HI32 mode = 1 => PCI
587       P:000170 P:000172 000000            NOP
588       P:000171 P:000173 0A8AAC            JSET    #12,X:DPSR,*                      ; Host data transfer not in progress
                            000171
589                                 ;-----------------------------------------------------------------------------
590                                 ; Here endth the initialisation code run after power up.
591                                 ; ----------------------------------------------------------------------------
592    
593    
594    
595    
596                                           INCLUDE 'PCI_SCUBA_main.asm'
597                                  COMMENT *
598    
599                                 This is the main section of the pci card code.
600    
601                                 Project:     SCUBA 2
602                                 Author:      DAVID ATKINSON
603                                 Target:      250MHz SDSU PCI card - DSP56301
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 13



604                                 Controller:  For use with SCUBA 2 Multichannel Electronics
605    
606                                 Version:     Release Version A (1.5)
607    
608    
609                                 Assembler directives:
610                                         ROM=EEPROM => EEPROM CODE
611                                         ROM=ONCE => ONCE CODE
612    
613                                         *
614                                           PAGE    132                               ; Printronix page width - 132 columns
615                                           OPT     CEX                               ; print DC evaluations
616    
**** 617 [PCI_SCUBA_main.asm 21]:  INCLUDE PCI_main.asm HERE  
617                                           MSG     ' INCLUDE PCI_main.asm HERE  '
618    
619                                 ; --------------------------------------------------------------------------
620                                 ; --------------------- MAIN PACKET HANDLING CODE --------------------------
621                                 ; --------------------------------------------------------------------------
622    
623                                 ; initialse buffer pointers
624                                 PACKET_IN
625    
626                                 ; R1 used as pointer for data written to y:memory            FO --> (Y)
627                                 ; R2 used as pointer for date in y mem to be writen to host  (Y) --> HOST
628    
629       P:000173 P:000175 310000            MOVE              #<IMAGE_BUFFER,R1       ; pointer for Fibre ---> Y mem
630       P:000174 P:000176 320000            MOVE              #<IMAGE_BUFFER,R2       ; pointer for Y mem ---> PCI BUS
631    
632                                 ; initialise some bits in status..
633       P:000175 P:000177 0A0001            BCLR    #SEND_TO_HOST,X:<STATUS           ; clear send to host flag
634       P:000176 P:000178 0A0009            BCLR    #HST_NFYD,X:<STATUS               ; clear flag to indicate host has been notif
ied.
635       P:000177 P:000179 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS             ; clear Fiber Optic flag
636    
637                                 ; check some bits in status....
638       P:000178 P:00017A 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START      ; fatal error?  Go to initialisation.
                            000100
639       P:00017A P:00017C 0A00A0            JSET    #APPLICATION_LOADED,X:<STATUS,APPLICATION ; application loaded?  Execute in ap
pl space.
                            000800
640       P:00017C P:00017E 0A00AD            JSET    #INTERNAL_GO,X:<STATUS,APPLICATION ; internal GO to process?  PCI bus master w
rite test.
                            000800
641    
642       P:00017E P:000180 0D040C  CHK_FIFO  JSR     <GET_FO_WRD                       ; see if there's a 16-bit word in Fibre FIFO
 from MCE
643    
644    
645       P:00017F P:000181 0A00A3            JSET    #FO_WRD_RCV,X:<STATUS,CHECK_WD    ; there is a word - check if it's preamble
                            000182
646       P:000181 P:000183 0C0173            JMP     <PACKET_IN                        ; else go back and repeat
647    
648                                 ; check that we preamble sequence
649    
650       P:000182 P:000184 0A00A8  CHECK_WD  JSET    #PACKET_CHOKE,X:<STATUS,PACKET_IN ; IF MCE Packet choke on - just keep clearin
g FIFO.
                            000173
651       P:000184 P:000186 441D00            MOVE              X0,X:<HEAD_W1_0         ;store received word
652       P:000185 P:000187 56F000            MOVE              X:PREAMB1,A
                            000038
653       P:000187 P:000189 200045            CMP     X0,A                              ; check it is correct
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 14



654       P:000188 P:00018A 0E219C            JNE     <PRE_ERROR                        ; if not go to start
655    
656    
657       P:000189 P:00018B 0D0414            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
658       P:00018A P:00018C 441C00            MOVE              X0,X:<HEAD_W1_1         ;store received word
659       P:00018B P:00018D 56F000            MOVE              X:PREAMB1,A
                            000038
660       P:00018D P:00018F 200045            CMP     X0,A                              ; check it is correct
661       P:00018E P:000190 0E219C            JNE     <PRE_ERROR                        ; if not go to start
662    
663    
664       P:00018F P:000191 0D0414            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
665       P:000190 P:000192 441F00            MOVE              X0,X:<HEAD_W2_0         ;store received word
666       P:000191 P:000193 56F000            MOVE              X:PREAMB2,A
                            000039
667       P:000193 P:000195 200045            CMP     X0,A                              ; check it is correct
668       P:000194 P:000196 0E219C            JNE     <PRE_ERROR                        ; if not go to start
669    
670       P:000195 P:000197 0D0414            JSR     <WT_FIFO                          ; wait for next preamble 16-bit word
671       P:000196 P:000198 441E00            MOVE              X0,X:<HEAD_W2_1         ;store received word
672       P:000197 P:000199 56F000            MOVE              X:PREAMB2,A
                            000039
673       P:000199 P:00019B 200045            CMP     X0,A                              ; check it is correct
674       P:00019A P:00019C 0E219C            JNE     <PRE_ERROR                        ; if not go to start
675       P:00019B P:00019D 0C01A8            JMP     <PACKET_INFO                      ; get packet info
676    
677    
678                                 PRE_ERROR
679       P:00019C P:00019E 0A0026            BSET    #PREAMBLE_ERROR,X:<STATUS         ; indicate a preamble error
680       P:00019D P:00019F 440200            MOVE              X0,X:<PRE_CORRUPT       ; store corrupted word
681    
682                                 ; preampble error so clear out both FIFOs using reset line
683                                 ; - protects against an odd number of bytes having been sent
684                                 ; (byte swapping on - so odd byte being would end up in
685                                 ; the FIFO without the empty flag)
686    
687       P:00019E P:0001A0 07F42D            MOVEP             #%011000,X:PDRD         ; clear FIFO RESET* for 2 ms
                            000018
688       P:0001A0 P:0001A2 44F400            MOVE              #200000,X0
                            030D40
689       P:0001A2 P:0001A4 06C400            DO      X0,*+3
                            0001A4
690       P:0001A4 P:0001A6 000000            NOP
691       P:0001A5 P:0001A7 07F42D            MOVEP             #%011100,X:PDRD
                            00001C
692    
693       P:0001A7 P:0001A9 0C0173            JMP     <PACKET_IN                        ; wait for next packet
694    
695    
696                                 PACKET_INFO                                         ; packet preamble valid
697    
698                                 ; Packet preamle is valid so....
699                                 ; now get next two 32bit words.  i.e. $20205250 $00000004, or $20204441 $xxxxxxxx
700                                 ; note that these are received little endian (and byte swapped)
701                                 ; i.e. for RP receive 50 52 20 20  04 00 00 00
702                                 ; but byte swapped on arrival
703                                 ; 5250
704                                 ; 2020
705                                 ; 0004
706                                 ; 0000
707    
708       P:0001A8 P:0001AA 0D0414            JSR     <WT_FIFO
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 15



709       P:0001A9 P:0001AB 442100            MOVE              X0,X:<HEAD_W3_0         ; RP or DA
710       P:0001AA P:0001AC 0D0414            JSR     <WT_FIFO
711       P:0001AB P:0001AD 442000            MOVE              X0,X:<HEAD_W3_1         ; $2020
712    
713       P:0001AC P:0001AE 0D0414            JSR     <WT_FIFO
714       P:0001AD P:0001AF 442300            MOVE              X0,X:<HEAD_W4_0         ; packet size lo
715       P:0001AE P:0001B0 0D0414            JSR     <WT_FIFO
716       P:0001AF P:0001B1 442200            MOVE              X0,X:<HEAD_W4_1         ; packet size hi
717    
718       P:0001B0 P:0001B2 44A100            MOVE              X:<HEAD_W3_0,X0         ; get data header word 3 (low 2 bytes)
719       P:0001B1 P:0001B3 56BB00            MOVE              X:<REPLY_WD,A           ; $5250
720       P:0001B2 P:0001B4 200045            CMP     X0,A                              ; is it a reply packet?
721       P:0001B3 P:0001B5 0AF0AA            JEQ     MCE_PACKET                        ; yes - go process it.
                            0001C7
722    
723       P:0001B5 P:0001B7 56BA00            MOVE              X:<DATA_WD,A            ; $4441
724       P:0001B6 P:0001B8 200045            CMP     X0,A                              ; is it a data packet?
725       P:0001B7 P:0001B9 0E2173            JNE     <PACKET_IN                        ; no?  Not a valid packet type.  Go back to 
start and resync to next preamble.
726    
727    
728                                 ; It's a data packet....
729                                 ; check if it's the first packet after the GO command has been issued...
730    
731       P:0001B8 P:0001BA 0A0087            JCLR    #DATA_DLY,X:STATUS,INC_FRAME_COUNT ; do we need to add a delay since first fra
me?
                            0001C2
732    
733                                 ; yes first frame after GO reply packet so add a delay.
734                                 PACKET_DELAY
735       P:0001BA P:0001BC 44F000            MOVE              X:DATA_DLY_VAL,X0
                            000040
736       P:0001BC P:0001BE 06C400            DO      X0,*+3                            ; 10ns x DATA_DLY_VAL
                            0001BE
737       P:0001BE P:0001C0 000000            NOP
738       P:0001BF P:0001C1 000000            NOP
739       P:0001C0 P:0001C2 0A7007            BCLR    #DATA_DLY,X:STATUS                ; clear so delay isn't added next time.
                            000000
740    
741    
742                                 INC_FRAME_COUNT                                     ; increment frame count
743       P:0001C2 P:0001C4 200013            CLR     A
744       P:0001C3 P:0001C5 508100            MOVE              X:<FRAME_COUNT,A0
745       P:0001C4 P:0001C6 000008            INC     A
746       P:0001C5 P:0001C7 000000            NOP
747       P:0001C6 P:0001C8 500100            MOVE              A0,X:<FRAME_COUNT
748    
749                                 ; -------------------------------------------------------------------------------------------
750                                 ; ----------------------------------- IT'S A PAKCET FROM MCE --------------------------------
751                                 ; -------------------------------------------------------------------------------------------
752                                 ; prepare notify to inform host that a packet has arrived.
753    
754                                 MCE_PACKET
755       P:0001C7 P:0001C9 44F400            MOVE              #'NFY',X0               ; initialise communication to host as a noti
fy
                            4E4659
756       P:0001C9 P:0001CB 440C00            MOVE              X0,X:<DTXS_WD1          ; 1st word transmitted to host in notify mes
sage
757    
758       P:0001CA P:0001CC 44A100            MOVE              X:<HEAD_W3_0,X0         ;RP or DA - top two bytes of word 3 ($2020) 
not passed to driver.
759       P:0001CB P:0001CD 440D00            MOVE              X0,X:<DTXS_WD2          ;2nd word transmitted to host in notify mess
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 16



age
760    
761       P:0001CC P:0001CE 44A300            MOVE              X:<HEAD_W4_0,X0         ; size of packet LSB 16bits (# 32bit words)
762       P:0001CD P:0001CF 440E00            MOVE              X0,X:<DTXS_WD3          ; 3rd word transmitted to host in notify mes
sage
763    
764       P:0001CE P:0001D0 44A200            MOVE              X:<HEAD_W4_1,X0         ; size of packet MSB 16bits (# of 32bit word
s)
765       P:0001CF P:0001D1 440F00            MOVE              X0,X:<DTXS_WD4          ; 4th word transmitted to host in notify mes
sasge
766    
767       P:0001D0 P:0001D2 200013            CLR     A                                 ;
768       P:0001D1 P:0001D3 340000            MOVE              #0,R4                   ; initialise word count
769       P:0001D2 P:0001D4 560600            MOVE              A,X:<WORD_COUNT         ; initialise word count store (num of words 
written over bus/packet)
770       P:0001D3 P:0001D5 560700            MOVE              A,X:<NUM_DUMPED         ; initialise number dumped from FIFO (after 
HST TO)
771    
772    
773                                 ; ----------------------------------------------------------------------------------------------
------------
774                                 ; Determine how to break up packet to write to host
775    
776                                 ; Note that this SR uses accumulator B
777                                 ; Therefore execute before we get the bus address from host (which is stored in B)
778                                 ; i.e before we issue notify message ('NFY')
779    
780       P:0001D4 P:0001D6 0D03DD            JSR     <CALC_NO_BUFFS                    ; subroutine which calculates the number of 
512 (16bit) buffers
781                                                                                     ; number of left over 32 (16bit) blocks
782                                                                                     ; and number of left overs (16bit) words
783    
784                                 ;  note that a 512 (16-bit) buffer is transfered to the host as 4 x 64 x 32bit DMA burst
785                                 ;            a 32  (16-bit) block is transfered to the host as a    16 x 32bit DMA burst
786                                 ;            left over 16bit words are transfered to the host in pairs as 32bit words
787                                 ; ----------------------------------------------------------------------------------------------
---
788    
789    
790                                 ; notify the host that there is a packet.....
791    
792       P:0001D5 P:0001D7 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; notify host of packet
793       P:0001D6 P:0001D8 0A0029            BSET    #HST_NFYD,X:<STATUS               ; flag to indicate host has been notified.
794    
795                                 ; initialise read/write buffers
796                                 ; AND IMMEDIATELY BEGIN TO BUFFER FIBRE DATA TO Y MEMORY.
797    
798       P:0001D7 P:0001D9 310000            MOVE              #<IMAGE_BUFFER,R1       ; FO ---> Y mem
799       P:0001D8 P:0001DA 320000            MOVE              #<IMAGE_BUFFER,R2       ; Y mem ----->  PCI BUS
800    
801    
802                                 ; ----------------------------------------------------------------------------------------------
-----------
803                                 ; Write TOTAL_BUFFS * 512 buffers to host
804                                 ; ----------------------------------------------------------------------------------------------
------
805       P:0001D9 P:0001DB 063C00            DO      X:<TOTAL_BUFFS,READ_BUFFS_END     ; note that if TOTAL_BUFFS = 0 we jump to AL
L_BUFFS_END
                            0001E6
806    
807       P:0001DB P:0001DD 0A00A2  WAIT_BUFF JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO  ; if fatal error then dump fifo and reset (i
.e. if HST timeout)
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 17



                            000221
808       P:0001DD P:0001DF 01ADA1            JSET    #HF,X:PDRD,WAIT_BUFF              ; Wait for FIFO to be half full + 1
                            0001DB
809       P:0001DF P:0001E1 000000            NOP
810       P:0001E0 P:0001E2 000000            NOP
811       P:0001E1 P:0001E3 01ADA1            JSET    #HF,X:PDRD,WAIT_BUFF              ; Protection against metastability
                            0001DB
812    
813                                 ; Copy the image block as 512 x 16bit words to DSP Y: Memory using R1 as pointer
814       P:0001E3 P:0001E5 060082            DO      #512,L_BUFFER
                            0001E5
815       P:0001E5 P:0001E7 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+
816                                 L_BUFFER
817       P:0001E6 P:0001E8 000000            NOP
818                                 READ_BUFFS_END                                      ; all buffers have been read (-->Y)
819    
820                                 ; ----------------------------------------------------------------------------------------------
-----------
821                                 ; Read NUM_LEFTOVER_BLOCKS * 32 blocks
822                                 ; ----------------------------------------------------------------------------------------------
------
823    
824                                 ; less than 512 pixels but if greater than 32 will then do bursts
825                                 ; of 16 x 32bit in length, if less than 32 then does single read writes
826    
827       P:0001E7 P:0001E9 063F00            DO      X:<NUM_LEFTOVER_BLOCKS,READ_BLOCKS ;note that if NUM_LEFOVERS_BLOCKS = 0 we ju
mp to LEFTOVER_BLOCKS
                            0001F4
828    
829       P:0001E9 P:0001EB 062080            DO      #32,S_BUFFER
                            0001F3
830       P:0001EB P:0001ED 0A00A2  WAIT_1    JSET    #FATAL_ERROR,X:<STATUS,DUMP_FIFO  ; check for fatal error (i.e. after HST time
out)
                            000221
831       P:0001ED P:0001EF 01AD80            JCLR    #EF,X:PDRD,WAIT_1                 ; Wait for the pixel datum to be there
                            0001EB
832       P:0001EF P:0001F1 000000            NOP                                       ; Settling time
833       P:0001F0 P:0001F2 000000            NOP
834       P:0001F1 P:0001F3 01AD80            JCLR    #EF,X:PDRD,WAIT_1                 ; Protection against metastability
                            0001EB
835       P:0001F3 P:0001F5 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; save fibre word
836                                 S_BUFFER
837       P:0001F4 P:0001F6 000000            NOP
838                                 READ_BLOCKS
839    
840                                 ; ----------------------------------------------------------------------------------------------
-------
841                                 ; Single write left over words to host
842                                 ; ----------------------------------------------------------------------------------------------
------
843    
844                                 LEFT_OVERS
845       P:0001F5 P:0001F7 063D00            DO      X:<LEFT_TO_READ,LEFT_OVERS_READ   ; read in remaining words of data packet
                            0001FF
846                                                                                     ; if LEFT_TO_READ = 0 then will jump to LEFT
_OVERS_READ
847    
848       P:0001F7 P:0001F9 0A00A2  WAIT_2    JSET    #FATAL_ERROR,X:<STATUS,START      ; check for fatal error (i.e. after HST time
out)
                            000100
849       P:0001F9 P:0001FB 01AD80            JCLR    #EF,X:PDRD,WAIT_2                 ; Wait till something in FIFO flagged
                            0001F7
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 18



850       P:0001FB P:0001FD 000000            NOP
851       P:0001FC P:0001FE 000000            NOP
852       P:0001FD P:0001FF 01AD80            JCLR    #EF,X:PDRD,WAIT_2                 ; protect against metastability.....
                            0001F7
853       P:0001FF P:000201 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; save fibre word
854                                 LEFT_OVERS_READ
855    
856                                 ;---------------------------------------------------------------------------------------
857                                 ; ENTIRE PACKET NOW IN Y MEMORY
858                                 ;----------------------------------------------------------------------------------------
859                                 ; CHECK THAT HST COMMAND WAS ISSUED DURING DATA COLLECTION...
860    
861    
862       P:000200 P:000202 0A00A2  WT_HOST   JSET    #FATAL_ERROR,X:<STATUS,START      ; if fatal error - run initialisation code..
.
                            000100
863       P:000202 P:000204 0A0081            JCLR    #SEND_TO_HOST,X:<STATUS,WT_HOST   ; wait for host to reply - which it does wit
h 'send_packet_to_host' ISR
                            000200
864    
865                                 ; we now have 32 bit address in accumulator B
866                                 ; from send-packet_to_host (HST COMMAND) which should of been issued during data collection.
867    
868                                 ; Write all data to host.
869    
870                                 ; ----------------------------------------------------------------------------------------------
-----------
871                                 ; Write TOTAL_BUFFS * 512 buffers to host
872                                 ; ----------------------------------------------------------------------------------------------
------
873       P:000204 P:000206 063C00            DO      X:<TOTAL_BUFFS,WRITE_BUFFS_END    ; note that if TOTAL_BUFFS = 0 we jump to AL
L_BUFFS_END
                            000207
874    
875                                 ; R2 points to data in Y memory to be written to host
876                                 ; host address is in B - got by SEND_PACKET_TO_HOST command
877                                 ; so we can now write this buffer to host
878    
879       P:000206 P:000208 0D04FC            JSR     <WRITE_512_TO_PCI                 ; this subroutine will increment host addres
s, which is in B and R2
880       P:000207 P:000209 000000            NOP
881                                 WRITE_BUFFS_END                                     ; all buffers have been writen to host
882    
883                                 ; ----------------------------------------------------------------------------------------------
-----------
884                                 ; Write NUM_LEFTOVER_BLOCKS * 32 blocks to host
885                                 ; ----------------------------------------------------------------------------------------------
------
886    
887                                 ; less than 512 pixels but if greater than 32 will then do bursts
888                                 ; of 16 x 32bit in length, if less than 32 then does single read writes
889    
890       P:000208 P:00020A 063F00            DO      X:<NUM_LEFTOVER_BLOCKS,WRITE_BLOCKS ;note that if NUM_LEFOVERS_BLOCKS = 0 we j
ump to LEFTOVER_BLOCKS
                            00020B
891    
892       P:00020A P:00020C 0D04D1            JSR     <WRITE_32_TO_PCI                  ; write small blocks
895                                 ;               NOP
896                                 ; WRITE_BLOCKS
897    
898                                 ; ----------------------------------------------------------------------------------------------
-------
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 19



899                                 ; Single write left over words to host
900                                 ; ----------------------------------------------------------------------------------------------
------
901    
902                                 ; now write left overs to host as 32 bit words
903    
904                                 ;               DO      X:LEFT_TO_WRITE,LEFT_OVERS_WRITE        ; left overs to write is half le
ft overs read - since 32 bit writes
905                                 ;                                                               ; if LEFT_TO_WRITE = 0, will jum
p to LEFT_OVERS_WRITTEN
906                                 ;               JSR     WRITE_TO_PCI                            ; uses R2 as pointer to Y memory
, host address in B
907                                 ;               NOP
908    
910       P:00020B P:00020D 000000            NOP
911                                 WRITE_BLOCKS
912       P:00020C P:00020E 0BF080            JSR     WRITE_32_TO_PCI
                            0004D1
913                                                                                     ; MOVE #0,LEFT_TO_WRITE
914       P:00020E P:000210 000000            NOP
915       P:00020F P:000211 000000            NOP
916       P:000210 P:000212 000000            NOP
917                                 LEFT_OVERS_WRITE
918    
919       P:000211 P:000213 0A00A2            JSET    #FATAL_ERROR,X:<STATUS,START
                            000100
920    
921                                 ; ----------------------------------------------------------------------------------------------
------------
922                                 ; reply to host's send_packet_to_host command
923    
924                                  HST_ACK_REP
925       P:000213 P:000215 44F400            MOVE              #'REP',X0
                            524550
926       P:000215 P:000217 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
927       P:000216 P:000218 44F400            MOVE              #'HST',X0
                            485354
928       P:000218 P:00021A 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
929       P:000219 P:00021B 44F400            MOVE              #'ACK',X0
                            41434B
930       P:00021B P:00021D 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
931       P:00021C P:00021E 44F400            MOVE              #'000',X0
                            303030
932       P:00021E P:000220 440F00            MOVE              X0,X:<DTXS_WD4          ; no error
933       P:00021F P:000221 0D0447            JSR     <PCI_MESSAGE_TO_HOST
934       P:000220 P:000222 0C0173            JMP     <PACKET_IN
935    
936                                 ;-----------------------------------------------------------------------------------------------
----
937                                 ; clear out the fifo after an HST timeout...
938                                 ;----------------------------------------------------------
939    
940       P:000221 P:000223 61F400  DUMP_FIFO MOVE              #DUMP_BUFF,R1           ; address where dumped words stored in Y mem
                            001000
941       P:000223 P:000225 44F400            MOVE              #MAX_DUMP,X0            ; put a limit to number of words read from f
ifo
                            000200
942       P:000225 P:000227 200013            CLR     A
943       P:000226 P:000228 320000            MOVE              #0,R2                   ; use R2 as a dump count
944    
945       P:000227 P:000229 01AD80  NEXT_DUMP JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000232
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 20



946       P:000229 P:00022B 000000            NOP
947       P:00022A P:00022C 000000            NOP
948       P:00022B P:00022D 01AD80            JCLR    #EF,X:PDRD,FIFO_EMPTY
                            000232
949    
950       P:00022D P:00022F 0959FF            MOVEP             Y:RDFIFO,Y:(R1)+        ; dump word to Y mem.
951       P:00022E P:000230 205A00            MOVE              (R2)+                   ; inc dump count
952       P:00022F P:000231 224E00            MOVE              R2,A                    ;
953       P:000230 P:000232 200045            CMP     X0,A                              ; check we've not hit dump limit
954       P:000231 P:000233 0E2227            JNE     NEXT_DUMP                         ; not hit limit?
955    
956    
957       P:000232 P:000234 627000  FIFO_EMPTY MOVE             R2,X:NUM_DUMPED         ; store number of words dumped after HST tim
eout.
                            000007
958       P:000234 P:000236 0C0100            JMP     <START                            ; re-initialise
959    
960    
961    
962                                 ; ----------------------------------------------------------------------------------------------
--
963                                 ;                              END OF MAIN PACKET HANDLING CODE
964                                 ; ---------------------------------------------------------------------------------------------
965    
966    
967                                 ; -------------------------------------------------------------------------------------
968                                 ;
969                                 ;                              INTERRUPT SERVICE ROUTINES
970                                 ;
971                                 ; ---------------------------------------------------------------------------------------
972    
973                                 ;--------------------------------------------------------------------
974                                 CLEAN_UP_PCI
975                                 ;--------------------------------------------------------------------
976                                 ; Clean up the PCI board from wherever it was executing
977    
978       P:000235 P:000237 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable HF* FIFO interrupt
                            0001C0
979       P:000237 P:000239 05F439            MOVE              #$200,SR                ; mask for reset interrupts only
                            000200
980    
981       P:000239 P:00023B 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
982       P:00023A P:00023C 05F43D            MOVEC             #$000200,SSL            ; SR = zero except for interrupts
                            000200
983       P:00023C P:00023E 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
984       P:00023D P:00023F 05F43C            MOVEC             #START,SSH              ; Set PC to for full initialization
                            000100
985       P:00023F P:000241 000000            NOP
986       P:000240 P:000242 000004            RTI
987    
988                                 ; ---------------------------------------------------------------------------
989                                 READ_MEMORY
990                                 ;--------------------------------------------------------------------------
991                                 ; word 1 = command = 'RDM'
992                                 ; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
993                                 ; word 3 = address in memory
994                                 ; word 4 = not used
995    
996       P:000241 P:000243 0D04A4            JSR     <SAVE_REGISTERS                   ; save working registers
997    
998       P:000242 P:000244 0D0462            JSR     <RD_DRXR                          ; read words from host write to HTXR
999       P:000243 P:000245 56F000            MOVE              X:DRXR_WD1,A            ; read command
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 21



                            000008
1000      P:000245 P:000247 44F400            MOVE              #'RDM',X0
                            52444D
1001      P:000247 P:000249 200045            CMP     X0,A                              ; ensure command is 'RDM'
1002      P:000248 P:00024A 0E226C            JNE     <READ_MEMORY_ERROR_CNE            ; error, command NOT HCVR address
1003      P:000249 P:00024B 568900            MOVE              X:<DRXR_WD2,A           ; Memory type (X, Y, P)
1004      P:00024A P:00024C 578A00            MOVE              X:<DRXR_WD3,B
1005      P:00024B P:00024D 000000            NOP                                       ; pipeline restriction
1006      P:00024C P:00024E 21B000            MOVE              B1,R0                   ; get address to write to
1007      P:00024D P:00024F 0140C5            CMP     #$005F50,A                        ; $00'_P'
                            005F50
1008      P:00024F P:000251 0E2253            JNE     <RDX
1009      P:000250 P:000252 07E084            MOVE              P:(R0),X0               ; Read from P memory
1010      P:000251 P:000253 208E00            MOVE              X0,A                    ;
1011      P:000252 P:000254 0C025E            JMP     <FINISH_READ_MEMORY
1012                                RDX
1013      P:000253 P:000255 0140C5            CMP     #$005F58,A                        ; $00'_X'
                            005F58
1014      P:000255 P:000257 0E2259            JNE     <RDY
1015      P:000256 P:000258 44E000            MOVE              X:(R0),X0               ; Read from P memory
1016      P:000257 P:000259 208E00            MOVE              X0,A
1017      P:000258 P:00025A 0C025E            JMP     <FINISH_READ_MEMORY
1018                                RDY
1019      P:000259 P:00025B 0140C5            CMP     #$005F59,A                        ; $00'_Y'
                            005F59
1020      P:00025B P:00025D 0E2271            JNE     <READ_MEMORY_ERROR_MTE            ; not a valid memory type
1021      P:00025C P:00025E 4CE000            MOVE                          Y:(R0),X0   ; Read from P memory
1022      P:00025D P:00025F 208E00            MOVE              X0,A
1023   
1024                                ; when completed successfully then PCI needs to reply to Host with
1025                                ; word1 = reply/data = reply
1026                                FINISH_READ_MEMORY
1027      P:00025E P:000260 44F400            MOVE              #'REP',X0
                            524550
1028      P:000260 P:000262 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1029      P:000261 P:000263 44F400            MOVE              #'RDM',X0
                            52444D
1030      P:000263 P:000265 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1031      P:000264 P:000266 44F400            MOVE              #'ACK',X0
                            41434B
1032      P:000266 P:000268 440E00            MOVE              X0,X:<DTXS_WD3          ;  im command
1033      P:000267 P:000269 21C400            MOVE              A,X0
1034      P:000268 P:00026A 440F00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error
1035      P:000269 P:00026B 0D048F            JSR     <RESTORE_REGISTERS                ; restore registers
1036      P:00026A P:00026C 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1037      P:00026B P:00026D 000004            RTI
1038   
1039                                READ_MEMORY_ERROR_CNE
1040      P:00026C P:00026E 44F400            MOVE              #'CNE',X0
                            434E45
1041      P:00026E P:000270 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1042      P:00026F P:000271 0AF080            JMP     READ_MEMORY_ERROR                 ; fill in rest of reply
                            000274
1043                                READ_MEMORY_ERROR_MTE
1044      P:000271 P:000273 44F400            MOVE              #'MTE',X0
                            4D5445
1045      P:000273 P:000275 440F00            MOVE              X0,X:<DTXS_WD4          ;  Memory Type Error - not a valid memory ty
pe
1046   
1047                                READ_MEMORY_ERROR
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 22



1048      P:000274 P:000276 44F400            MOVE              #'REP',X0
                            524550
1049      P:000276 P:000278 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1050      P:000277 P:000279 44F400            MOVE              #'RDM',X0
                            52444D
1051      P:000279 P:00027B 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1052      P:00027A P:00027C 44F400            MOVE              #'ERR',X0
                            455252
1053      P:00027C P:00027E 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor.
1054      P:00027D P:00027F 0D048F            JSR     <RESTORE_REGISTERS                ; restore working registers
1055      P:00027E P:000280 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1056      P:00027F P:000281 000004            RTI
1057   
1058                                ;-----------------------------------------------------------------------------
1059                                RESET_CONTROLLER
1060                                ; Reset the controller by sending a special code byte $0B with SC/nData = 1
1061                                ;---------------------------------------------------------------------------
1062                                ; word 1 = command = 'RCO'
1063                                ; word 2 = not used but read
1064                                ; word 3 = not used but read
1065                                ; word 4 = not used but read
1066   
1067      P:000280 P:000282 0D04A4            JSR     <SAVE_REGISTERS                   ; save working registers
1068      P:000281 P:000283 0D0462            JSR     <RD_DRXR                          ; read words from host write to HTXR
1069      P:000282 P:000284 568800            MOVE              X:<DRXR_WD1,A           ; read command
1070      P:000283 P:000285 44F400            MOVE              #'RCO',X0
                            52434F
1071      P:000285 P:000287 200045            CMP     X0,A                              ; ensure command is 'RCO'
1072      P:000286 P:000288 0E22AB            JNE     <RCO_ERROR                        ; error, command NOT HCVR address
1073   
1074                                ; if we get here then everything is fine and we can send reset to controller
1075   
1076                                ; 250MHZ CODE....
1077   
1078      P:000287 P:000289 011D22            BSET    #SCLK,X:PDRE                      ; Enable special command mode
1079      P:000288 P:00028A 000000            NOP
1080      P:000289 P:00028B 000000            NOP
1081      P:00028A P:00028C 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
1082      P:00028C P:00028E 44F400            MOVE              #$10000B,X0             ; Special command to reset controller
                            10000B
1083      P:00028E P:000290 446000            MOVE              X0,X:(R0)
1084      P:00028F P:000291 0606A0            REP     #6                                ; Wait for transmission to complete
1085      P:000290 P:000292 000000            NOP
1086      P:000291 P:000293 011D02            BCLR    #SCLK,X:PDRE                      ; Disable special command mode
1087   
1088                                ; Wait for a bit for MCE to be reset.......
1089      P:000292 P:000294 44F400            MOVE              #10000,X0               ; Delay by about 350 milliseconds
                            002710
1090      P:000294 P:000296 06C400            DO      X0,L_DELAY
                            00029A
1091      P:000296 P:000298 06E883            DO      #1000,L_RDFIFO
                            000299
1092      P:000298 P:00029A 09463F            MOVEP             Y:RDFIFO,Y0             ; Read the FIFO word to keep the
1093      P:000299 P:00029B 000000            NOP                                       ;   receiver empty
1094                                L_RDFIFO
1095      P:00029A P:00029C 000000            NOP
1096                                L_DELAY
1097      P:00029B P:00029D 000000            NOP
1098   
1099                                ; when completed successfully then PCI needs to reply to Host with
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 23



1100                                ; word1 = reply/data = reply
1101                                FINISH_RCO
1102      P:00029C P:00029E 44F400            MOVE              #'REP',X0
                            524550
1103      P:00029E P:0002A0 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1104      P:00029F P:0002A1 44F400            MOVE              #'RCO',X0
                            52434F
1105      P:0002A1 P:0002A3 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1106      P:0002A2 P:0002A4 44F400            MOVE              #'ACK',X0
                            41434B
1107      P:0002A4 P:0002A6 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1108      P:0002A5 P:0002A7 44F400            MOVE              #'000',X0
                            303030
1109      P:0002A7 P:0002A9 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1110      P:0002A8 P:0002AA 0D048F            JSR     <RESTORE_REGISTERS                ; restore working registers
1111      P:0002A9 P:0002AB 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1112      P:0002AA P:0002AC 000004            RTI                                       ; return from ISR
1113   
1114                                ; when there is a failure in the host to PCI command then the PCI
1115                                ; needs still to reply to Host but with an error message
1116                                RCO_ERROR
1117      P:0002AB P:0002AD 44F400            MOVE              #'REP',X0
                            524550
1118      P:0002AD P:0002AF 447000            MOVE              X0,X:DTXS_WD1           ; REPly
                            00000C
1119      P:0002AF P:0002B1 44F400            MOVE              #'RCO',X0
                            52434F
1120      P:0002B1 P:0002B3 447000            MOVE              X0,X:DTXS_WD2           ; echo command sent
                            00000D
1121      P:0002B3 P:0002B5 44F400            MOVE              #'ERR',X0
                            455252
1122      P:0002B5 P:0002B7 447000            MOVE              X0,X:DTXS_WD3           ; ERRor im command
                            00000E
1123      P:0002B7 P:0002B9 44F400            MOVE              #'CNE',X0
                            434E45
1124      P:0002B9 P:0002BB 447000            MOVE              X0,X:DTXS_WD4           ; Command Name Error - command name in DRXR 
does not match
                            00000F
1125      P:0002BB P:0002BD 0D048F            JSR     <RESTORE_REGISTERS                ; restore wroking registers
1126      P:0002BC P:0002BE 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1127      P:0002BD P:0002BF 000004            RTI                                       ; return from ISR
1128   
1129   
1130                                ;----------------------------------------------------------------------
1131                                SEND_PACKET_TO_CONTROLLER
1132   
1133                                ; forward packet stuff to the MCE
1134                                ; gets address in HOST memory where packet is stored
1135                                ; read 3 consecutive locations starting at this address
1136                                ; then sends the data from these locations up to the MCE
1137                                ;----------------------------------------------------------------------
1138   
1139                                ; word 1 = command = 'CON'
1140                                ; word 2 = host high address
1141                                ; word 3 = host low address
1142                                ; word 4 = '0' --> when MCE command is RS,WB,RB,ST
1143                                ;        = '1' --> when MCE command is GO
1144   
1145                                ; all MCE commands are now 'block commands'
1146                                ; i.e. 64 words long.
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 24



1147   
1148      P:0002BE P:0002C0 0D04A4            JSR     <SAVE_REGISTERS                   ; save working registers
1149   
1150      P:0002BF P:0002C1 0D0462            JSR     <RD_DRXR                          ; read words from host write to HTXR
1151                                                                                    ; reads as 4 x 24 bit words
1152   
1153      P:0002C0 P:0002C2 568800            MOVE              X:<DRXR_WD1,A           ; read command
1154      P:0002C1 P:0002C3 44F400            MOVE              #'CON',X0
                            434F4E
1155      P:0002C3 P:0002C5 200045            CMP     X0,A                              ; ensure command is 'CON'
1156      P:0002C4 P:0002C6 0E22F9            JNE     <CON_ERROR                        ; error, command NOT HCVR address
1157   
1158                                ; convert 2 x 24 bit words ( only 16 LSBs are significant) from host into 32 bit address
1159      P:0002C5 P:0002C7 20001B            CLR     B
1160      P:0002C6 P:0002C8 448900            MOVE              X:<DRXR_WD2,X0          ; MS 16bits of address
1161      P:0002C7 P:0002C9 518A00            MOVE              X:<DRXR_WD3,B0          ; LS 16bits of address
1162      P:0002C8 P:0002CA 0C1941            INSERT  #$010010,X0,B                     ; convert to 32 bits and put in B
                            010010
1163   
1164      P:0002CA P:0002CC 568B00            MOVE              X:<DRXR_WD4,A           ; read word 4 - GO command?
1165      P:0002CB P:0002CD 44F000            MOVE              X:ZERO,X0
                            000033
1166      P:0002CD P:0002CF 200045            CMP     X0,A
1167      P:0002CE P:0002D0 0AF0AA            JEQ     BLOCK_CON
                            0002DC
1168   
1169   
1170      P:0002D0 P:0002D2 0A008C            JCLR    #APPLICATION_RUNNING,X:STATUS,SET_PACKET_DELAY ; not running diagnostic applic
ation?
                            0002DA
1171   
1172                                ; need to generate an internal go command to test master write on bus.....  Diagnostic test
1173      P:0002D2 P:0002D4 0A702D            BSET    #INTERNAL_GO,X:STATUS             ; set flag so that GO reply / data is genera
ted by PCI card...
                            000000
1174   
1175                                ; since INTERNAL_GO  - read command but don't send it to MCE...
1176   
1177                                CLR_CMD
1178      P:0002D4 P:0002D6 064080            DO      #64,END_CLR_CMD                   ; block size = 32bit x 64 (256 bytes)
                            0002D7
1179      P:0002D6 P:0002D8 0D046F            JSR     <READ_FROM_PCI                    ; get next 32 bit word from HOST
1180      P:0002D7 P:0002D9 000000            NOP
1181                                END_CLR_CMD
1182      P:0002D8 P:0002DA 0AF080            JMP     FINISH_CON                        ; don't send out on command on fibre
                            0002EA
1183   
1184   
1185                                SET_PACKET_DELAY
1186      P:0002DA P:0002DC 0A7027            BSET    #DATA_DLY,X:STATUS                ; set data delay so that next data packet af
ter go reply
                            000000
1187                                                                                    ; experiences a delay before host notify.
1188   
1189                                ; -----------------------------------------------------------------------
1190                                ; WARNING!!!
1191                                ; MCE requires IDLE characters between 32bit words sent FROM the PCI card
1192                                ; DO not change READ_FROM_PCI to DMA block transfer....
1193                                ; ------------------------------------------------------------------------
1194   
1195                                BLOCK_CON
1196      P:0002DC P:0002DE 66F000            MOVE              X:CONSTORE,R6
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 25



                            000041
1197   
1198      P:0002DE P:0002E0 064080            DO      #64,END_BLOCK_CON                 ; block size = 32bit x 64 (256 bytes)
                            0002E6
1199      P:0002E0 P:0002E2 0D046F            JSR     <READ_FROM_PCI                    ; get next 32 bit word from HOST
1200      P:0002E1 P:0002E3 208C00            MOVE              X0,A1                   ; prepare to send
1201      P:0002E2 P:0002E4 20A800            MOVE              X1,A0                   ; prepare to send
1202   
1203      P:0002E3 P:0002E5 4D5E00            MOVE                          X1,Y:(R6)+  ; b4, b3 (msb)
1204      P:0002E4 P:0002E6 4C5E00            MOVE                          X0,Y:(R6)+  ; b2, b1  (lsb)
1205   
1206      P:0002E5 P:0002E7 0D052D            JSR     <XMT_WD_FIBRE                     ; off it goes
1207      P:0002E6 P:0002E8 000000            NOP
1208                                END_BLOCK_CON
1209   
1210      P:0002E7 P:0002E9 0A0008            BCLR    #PACKET_CHOKE,X:<STATUS           ; disable packet choke...
1211                                                                                    ; comms now open with MCE and packets will b
e processed.
1212                                ; Enable Byte swaping for correct comms protocol.
1213      P:0002E8 P:0002EA 0A0025            BSET    #BYTE_SWAP,X:<STATUS              ; flag to let host know byte swapping on
1214      P:0002E9 P:0002EB 013D24            BSET    #AUX1,X:PDRC                      ; enable hardware
1215   
1216   
1217                                ; -------------------------------------------------------------------------
1218                                ; when completed successfully then PCI needs to reply to Host with
1219                                ; word1 = reply/data = reply
1220                                FINISH_CON
1221      P:0002EA P:0002EC 44F400            MOVE              #'REP',X0
                            524550
1222      P:0002EC P:0002EE 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1223      P:0002ED P:0002EF 44F400            MOVE              #'CON',X0
                            434F4E
1224      P:0002EF P:0002F1 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1225      P:0002F0 P:0002F2 44F400            MOVE              #'ACK',X0
                            41434B
1226      P:0002F2 P:0002F4 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1227      P:0002F3 P:0002F5 44F400            MOVE              #'000',X0
                            303030
1228      P:0002F5 P:0002F7 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1229      P:0002F6 P:0002F8 0D048F            JSR     <RESTORE_REGISTERS                ; restore working registers
1230      P:0002F7 P:0002F9 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ;  interrupt host with message (x0 restored 
here)
1231      P:0002F8 P:0002FA 000004            RTI                                       ; return from ISR
1232   
1233                                ; when there is a failure in the host to PCI command then the PCI
1234                                ; needs still to reply to Host but with an error message
1235                                CON_ERROR
1236      P:0002F9 P:0002FB 44F400            MOVE              #'REP',X0
                            524550
1237      P:0002FB P:0002FD 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1238      P:0002FC P:0002FE 44F400            MOVE              #'CON',X0
                            434F4E
1239      P:0002FE P:000300 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1240      P:0002FF P:000301 44F400            MOVE              #'ERR',X0
                            455252
1241      P:000301 P:000303 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1242      P:000302 P:000304 44F400            MOVE              #'CNE',X0
                            434E45
1243      P:000304 P:000306 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1244      P:000305 P:000307 0D048F            JSR     <RESTORE_REGISTERS                ; restore working registers
1245      P:000306 P:000308 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 26



ere)
1246      P:000307 P:000309 000004            RTI                                       ; return from ISR
1247   
1248                                ; ------------------------------------------------------------------------------------
1249                                SEND_PACKET_TO_HOST
1250                                ; this command is received from the Host and actions the PCI board to pick up an address
1251                                ; pointer from DRXR which the PCI board then uses to write packets from the
1252                                ; MCE to the host memory starting at the address given.
1253                                ; Since this is interrupt driven all this piece of code does is get the address pointer from
1254                                ; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to
1255                                ; HST after packet sent (unless error).
1256                                ; --------------------------------------------------------------------------------------
1257                                ; word 1 = command = 'HST'
1258                                ; word 2 = host high address
1259                                ; word 3 = host low address
1260                                ; word 4 = not used but read
1261   
1262                                ; save some registers but not B
1263   
1264      P:000308 P:00030A 0D04A4            JSR     <SAVE_REGISTERS                   ; save working registers
1265   
1266      P:000309 P:00030B 0D0462            JSR     <RD_DRXR                          ; read words from host write to HTXR
1267      P:00030A P:00030C 20001B            CLR     B
1268      P:00030B P:00030D 568800            MOVE              X:<DRXR_WD1,A           ; read command
1269      P:00030C P:00030E 44F400            MOVE              #'HST',X0
                            485354
1270      P:00030E P:000310 200045            CMP     X0,A                              ; ensure command is 'HST'
1271      P:00030F P:000311 0E2317            JNE     <HOST_ERROR                       ; error, command NOT HCVR address
1272      P:000310 P:000312 448900            MOVE              X:<DRXR_WD2,X0          ; high 16 bits of address
1273      P:000311 P:000313 518A00            MOVE              X:<DRXR_WD3,B0          ; low 16 bits of adderss
1274      P:000312 P:000314 0C1941            INSERT  #$010010,X0,B                     ; convert to 32 bits and put in B
                            010010
1275   
1276      P:000314 P:000316 0A0021            BSET    #SEND_TO_HOST,X:<STATUS           ; tell main program to write packet to host 
memory
1277      P:000315 P:000317 0D049B            JSR     <RESTORE_HST_REGISTERS            ; restore registers for HST .... B not resto
red..
1278      P:000316 P:000318 000004            RTI
1279   
1280                                ; !!NOTE!!!
1281                                ; successful reply to this command is sent after packet has been send to host.
1282                                ; Not here unless error.
1283   
1284                                ; when there is a failure in the host to PCI command then the PCI
1285                                ; needs still to reply to Host but with an error message
1286                                HOST_ERROR
1287      P:000317 P:000319 0A7001            BCLR    #SEND_TO_HOST,X:STATUS
                            000000
1288      P:000319 P:00031B 44F400            MOVE              #'REP',X0
                            524550
1289      P:00031B P:00031D 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1290      P:00031C P:00031E 44F400            MOVE              #'HST',X0
                            485354
1291      P:00031E P:000320 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1292      P:00031F P:000321 44F400            MOVE              #'ERR',X0
                            455252
1293      P:000321 P:000323 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1294      P:000322 P:000324 44F400            MOVE              #'CNE',X0
                            434E45
1295      P:000324 P:000326 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1296      P:000325 P:000327 0D048F            JSR     <RESTORE_REGISTERS                ; restore working registers
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 27



1297      P:000326 P:000328 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1298      P:000327 P:000329 000004            RTI
1299   
1300                                ; --------------------------------------------------------------------
1301                                SOFTWARE_RESET
1302                                ;----------------------------------------------------------------------
1303                                ; word 1 = command = 'RST'
1304                                ; word 2 = not used but read
1305                                ; word 3 = not used but read
1306                                ; word 4 = not used but read
1307   
1308      P:000328 P:00032A 0D04A4            JSR     <SAVE_REGISTERS
1309   
1310      P:000329 P:00032B 0D0462            JSR     <RD_DRXR                          ; read words from host write to HTXR
1311      P:00032A P:00032C 568800            MOVE              X:<DRXR_WD1,A           ; read command
1312      P:00032B P:00032D 44F400            MOVE              #'RST',X0
                            525354
1313      P:00032D P:00032F 200045            CMP     X0,A                              ; ensure command is 'RST'
1314      P:00032E P:000330 0E2351            JNE     <RST_ERROR                        ; error, command NOT HCVR address
1315   
1316                                ; RST command OK so reply to host
1317                                FINISH_RST
1318      P:00032F P:000331 44F400            MOVE              #'REP',X0
                            524550
1319      P:000331 P:000333 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1320      P:000332 P:000334 44F400            MOVE              #'RST',X0
                            525354
1321      P:000334 P:000336 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1322      P:000335 P:000337 44F400            MOVE              #'ACK',X0
                            41434B
1323      P:000337 P:000339 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1324      P:000338 P:00033A 44F400            MOVE              #'000',X0
                            303030
1325      P:00033A P:00033C 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1326      P:00033B P:00033D 0D0447            JSR     <PCI_MESSAGE_TO_HOST
1327   
1328      P:00033C P:00033E 0A85A3            JSET    #DCTR_HF3,X:DCTR,*
                            00033C
1329   
1330      P:00033E P:000340 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS     ; clear app flag
1331      P:00033F P:000341 0A0006            BCLR    #PREAMBLE_ERROR,X:<STATUS         ; clear preamble error
1332      P:000340 P:000342 0A000C            BCLR    #APPLICATION_RUNNING,X:<STATUS    ; clear appl running bit.
1333   
1334                                ; initialise some parameter here - that we don't want to initialse under a fatal error reset.
1335   
1336      P:000341 P:000343 200013            CLR     A
1337      P:000342 P:000344 340000            MOVE              #0,R4                   ; initialise word count
1338      P:000343 P:000345 560600            MOVE              A,X:<WORD_COUNT         ; initialise word count store (num of words 
written over bus/packet)
1339      P:000344 P:000346 560700            MOVE              A,X:<NUM_DUMPED         ; initialise number dumped from FIFO (after 
HST TO)
1340   
1341   
1342                                ; remember we are in a ISR so can't just jump to start.
1343   
1344      P:000345 P:000347 08F4BF            MOVEP             #$0001C0,X:IPRC         ; Disable HF* FIFO interrupt
                            0001C0
1345      P:000347 P:000349 05F439            MOVE              #$200,SR                ; Mask set up for reset switch only.
                            000200
1346   
1347   
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 28



1348      P:000349 P:00034B 0501BB            MOVEC             #1,SP                   ; Point stack pointer to the top
1349      P:00034A P:00034C 05F43D            MOVEC             #$000200,SSL            ; SSL holds SR return state
                            000200
1350                                                                                    ; set to zero except for interrupts
1351      P:00034C P:00034E 0500BB            MOVEC             #0,SP                   ; Writing to SSH preincrements the SP
1352                                                                                    ; so first set to 0
1353      P:00034D P:00034F 05F43C            MOVEC             #START,SSH              ; SSH holds return address of PC
                            000100
1354                                                                                    ; therefore,return to initialization
1355      P:00034F P:000351 000000            NOP
1356      P:000350 P:000352 000004            RTI                                       ; return from ISR - to START
1357   
1358                                ; when there is a failure in the host to PCI command then the PCI
1359                                ; needs still to reply to Host but with an error message
1360                                RST_ERROR
1361      P:000351 P:000353 44F400            MOVE              #'REP',X0
                            524550
1362      P:000353 P:000355 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1363      P:000354 P:000356 44F400            MOVE              #'RST',X0
                            525354
1364      P:000356 P:000358 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1365      P:000357 P:000359 44F400            MOVE              #'ERR',X0
                            455252
1366      P:000359 P:00035B 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1367      P:00035A P:00035C 44F400            MOVE              #'CNE',X0
                            434E45
1368      P:00035C P:00035E 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1369      P:00035D P:00035F 0D048F            JSR     <RESTORE_REGISTERS                ; restore working registers
1370      P:00035E P:000360 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1371      P:00035F P:000361 000004            RTI                                       ; return from ISR
1372   
1373   
1374                                ;-----------------------------------------------------------------------------
1375                                START_APPLICATION
1376                                ; an application should already have been downloaded to the PCI memory.
1377                                ; this command will execute it.
1378                                ; ----------------------------------------------------------------------
1379                                ; word 1 = command = 'GOA'
1380                                ; word 2 = not used but read by RD_DRXR
1381                                ; word 3 = not used but read by RD_DRXR
1382                                ; word 4 = not used but read by RD_DRXR
1383   
1384      P:000360 P:000362 0D04A4            JSR     <SAVE_REGISTERS                   ; save working registers
1385   
1386      P:000361 P:000363 0D0462            JSR     <RD_DRXR                          ; read words from host write to HTXR
1387      P:000362 P:000364 568800            MOVE              X:<DRXR_WD1,A           ; read command
1388      P:000363 P:000365 44F400            MOVE              #'GOA',X0
                            474F41
1389      P:000365 P:000367 200045            CMP     X0,A                              ; ensure command is 'RDM'
1390      P:000366 P:000368 0E2369            JNE     <GO_ERROR                         ; error, command NOT HCVR address
1391   
1392                                ; if we get here then everything is fine and we can start the application
1393                                ; set bit in status so that main fibre servicing code knows to jump
1394                                ; to application space after returning from this ISR
1395   
1396                                ; reply after application has been executed.
1397      P:000367 P:000369 0A0020            BSET    #APPLICATION_LOADED,X:<STATUS
1398      P:000368 P:00036A 000004            RTI                                       ; return from ISR
1399   
1400   
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 29



1401                                ; when there is a failure in the host to PCI command then the PCI
1402                                ; needs still to reply to Host but with an error message
1403                                GO_ERROR
1404      P:000369 P:00036B 44F400            MOVE              #'REP',X0
                            524550
1405      P:00036B P:00036D 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1406      P:00036C P:00036E 44F400            MOVE              #'GOA',X0
                            474F41
1407      P:00036E P:000370 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1408      P:00036F P:000371 44F400            MOVE              #'ERR',X0
                            455252
1409      P:000371 P:000373 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1410      P:000372 P:000374 44F400            MOVE              #'CNE',X0
                            434E45
1411      P:000374 P:000376 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1412      P:000375 P:000377 0D048F            JSR     <RESTORE_REGISTERS                ; restore working registers
1413      P:000376 P:000378 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1414      P:000377 P:000379 000004            RTI                                       ; return from ISR
1415   
1416                                ; ---------------------------------------------------------
1417                                STOP_APPLICATION
1418                                ; this command stops an application that is currently running
1419                                ; used for applications that once started run contiunually
1420                                ;-----------------------------------------------------------
1421   
1422                                ; word 1 = command = ' STP'
1423                                ; word 2 = not used but read
1424                                ; word 3 = not used but read
1425                                ; word 4 = not used but read
1426   
1427      P:000378 P:00037A 0D04A4            JSR     <SAVE_REGISTERS
1428   
1429      P:000379 P:00037B 0D0462            JSR     <RD_DRXR                          ; read words from host write to HTXR
1430      P:00037A P:00037C 568800            MOVE              X:<DRXR_WD1,A           ; read command
1431      P:00037B P:00037D 44F400            MOVE              #'STP',X0
                            535450
1432      P:00037D P:00037F 200045            CMP     X0,A                              ; ensure command is 'RDM'
1433      P:00037E P:000380 0E2391            JNE     <STP_ERROR                        ; error, command NOT HCVR address
1434   
1435      P:00037F P:000381 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS
1436      P:000380 P:000382 0A700C            BCLR    #APPLICATION_RUNNING,X:STATUS
                            000000
1437   
1438                                ; when completed successfully then PCI needs to reply to Host with
1439                                ; word1 = reply/data = reply
1440                                FINISH_STP
1441      P:000382 P:000384 44F400            MOVE              #'REP',X0
                            524550
1442      P:000384 P:000386 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1443      P:000385 P:000387 44F400            MOVE              #'STP',X0
                            535450
1444      P:000387 P:000389 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1445      P:000388 P:00038A 44F400            MOVE              #'ACK',X0
                            41434B
1446      P:00038A P:00038C 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1447      P:00038B P:00038D 44F400            MOVE              #'000',X0
                            303030
1448      P:00038D P:00038F 440F00            MOVE              X0,X:<DTXS_WD4          ; read data
1449      P:00038E P:000390 0D048F            JSR     <RESTORE_REGISTERS                ; restore working registers.
1450      P:00038F P:000391 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 30



ere)
1451      P:000390 P:000392 000004            RTI
1452   
1453                                ; when there is a failure in the host to PCI command then the PCI
1454                                ; needs still to reply to Host but with an error message
1455                                STP_ERROR
1456      P:000391 P:000393 44F400            MOVE              #'REP',X0
                            524550
1457      P:000393 P:000395 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1458      P:000394 P:000396 44F400            MOVE              #'STP',X0
                            535450
1459      P:000396 P:000398 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1460      P:000397 P:000399 44F400            MOVE              #'ERR',X0
                            455252
1461      P:000399 P:00039B 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1462      P:00039A P:00039C 44F400            MOVE              #'CNE',X0
                            434E45
1463      P:00039C P:00039E 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1464      P:00039D P:00039F 0D048F            JSR     <RESTORE_REGISTERS                ; restore working registers
1465      P:00039E P:0003A0 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1466      P:00039F P:0003A1 000004            RTI
1467   
1468                                ;--------------------------------------------------------------
1469                                WRITE_MEMORY
1470                                ;---------------------------------------------------------------
1471                                ; word 1 = command = 'WRM'
1472                                ; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
1473                                ; word 3 = address in memory
1474                                ; word 4 = value
1475   
1476      P:0003A0 P:0003A2 0D04A4            JSR     <SAVE_REGISTERS                   ; save working registers
1477   
1478      P:0003A1 P:0003A3 0D0462            JSR     <RD_DRXR                          ; read words from host write to HTXR
1479      P:0003A2 P:0003A4 56F000            MOVE              X:DRXR_WD1,A            ; read command
                            000008
1480      P:0003A4 P:0003A6 44F400            MOVE              #'WRM',X0
                            57524D
1481      P:0003A6 P:0003A8 200045            CMP     X0,A                              ; ensure command is 'WRM'
1482      P:0003A7 P:0003A9 0E23CA            JNE     <WRITE_MEMORY_ERROR_CNE           ; error, command NOT HCVR address
1483      P:0003A8 P:0003AA 568900            MOVE              X:<DRXR_WD2,A           ; Memory type (X, Y, P)
1484      P:0003A9 P:0003AB 578A00            MOVE              X:<DRXR_WD3,B
1485      P:0003AA P:0003AC 000000            NOP                                       ; pipeline restriction
1486      P:0003AB P:0003AD 21B000            MOVE              B1,R0                   ; get address to write to
1487      P:0003AC P:0003AE 448B00            MOVE              X:<DRXR_WD4,X0          ; get data to write
1488      P:0003AD P:0003AF 0140C5            CMP     #$005F50,A                        ; $00'_P'
                            005F50
1489      P:0003AF P:0003B1 0E23B2            JNE     <WRX
1490      P:0003B0 P:0003B2 076084            MOVE              X0,P:(R0)               ; Write to Program memory
1491      P:0003B1 P:0003B3 0C03BB            JMP     <FINISH_WRITE_MEMORY
1492                                WRX
1493      P:0003B2 P:0003B4 0140C5            CMP     #$005F58,A                        ; $00'_X'
                            005F58
1494      P:0003B4 P:0003B6 0E23B7            JNE     <WRY
1495      P:0003B5 P:0003B7 446000            MOVE              X0,X:(R0)               ; Write to X: memory
1496      P:0003B6 P:0003B8 0C03BB            JMP     <FINISH_WRITE_MEMORY
1497                                WRY
1498      P:0003B7 P:0003B9 0140C5            CMP     #$005F59,A                        ; $00'_Y'
                            005F59
1499      P:0003B9 P:0003BB 0E23CE            JNE     <WRITE_MEMORY_ERROR_MTE
1500      P:0003BA P:0003BC 4C6000            MOVE                          X0,Y:(R0)   ; Write to Y: memory
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 31



1501   
1502                                ; when completed successfully then PCI needs to reply to Host with
1503                                ; word1 = reply/data = reply
1504                                FINISH_WRITE_MEMORY
1505      P:0003BB P:0003BD 44F400            MOVE              #'REP',X0
                            524550
1506      P:0003BD P:0003BF 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1507      P:0003BE P:0003C0 44F400            MOVE              #'WRM',X0
                            57524D
1508      P:0003C0 P:0003C2 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1509      P:0003C1 P:0003C3 44F400            MOVE              #'ACK',X0
                            41434B
1510      P:0003C3 P:0003C5 440E00            MOVE              X0,X:<DTXS_WD3          ; ACKnowledge okay
1511      P:0003C4 P:0003C6 44F400            MOVE              #'000',X0
                            303030
1512      P:0003C6 P:0003C8 440F00            MOVE              X0,X:<DTXS_WD4          ; no error
1513      P:0003C7 P:0003C9 0D048F            JSR     <RESTORE_REGISTERS                ; restore working registers
1514      P:0003C8 P:0003CA 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1515      P:0003C9 P:0003CB 000004            RTI
1516   
1517                                ;
1518                                WRITE_MEMORY_ERROR_CNE
1519      P:0003CA P:0003CC 44F400            MOVE              #'CNE',X0
                            434E45
1520      P:0003CC P:0003CE 440F00            MOVE              X0,X:<DTXS_WD4          ; Command Name Error - command name in DRXR 
does not match
1521      P:0003CD P:0003CF 0C03D1            JMP     <WRITE_MEMORY_ERROR               ; fill in rest of reply
1522   
1523                                WRITE_MEMORY_ERROR_MTE
1524      P:0003CE P:0003D0 44F400            MOVE              #'MTE',X0
                            4D5445
1525      P:0003D0 P:0003D2 440F00            MOVE              X0,X:<DTXS_WD4          ; Memory Type Error - memory type not valid
1526   
1527                                WRITE_MEMORY_ERROR
1528      P:0003D1 P:0003D3 44F400            MOVE              #'REP',X0
                            524550
1529      P:0003D3 P:0003D5 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
1530      P:0003D4 P:0003D6 44F400            MOVE              #'WRM',X0
                            57524D
1531      P:0003D6 P:0003D8 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
1532      P:0003D7 P:0003D9 44F400            MOVE              #'ERR',X0
                            455252
1533      P:0003D9 P:0003DB 440E00            MOVE              X0,X:<DTXS_WD3          ; ERRor im command
1534      P:0003DA P:0003DC 0D048F            JSR     <RESTORE_REGISTERS                ; restore working registers
1535      P:0003DB P:0003DD 0D0447            JSR     <PCI_MESSAGE_TO_HOST              ; interrupt host with message (x0 restored h
ere)
1536      P:0003DC P:0003DE 000004            RTI
1537   
1538   
1539                                ;---------------------------------------------------------------
1540                                ;
1541                                ;                          * END OF ISRs *
1542                                ;
1543                                ;--------------------------------------------------------------
1544   
1545   
1546   
1547                                ;----------------------------------------------------------------
1548                                ;
1549                                ;                     * Beginning of SUBROUTINES *
1550                                ;
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 32



1551                                ;-----------------------------------------------------------------
1552   
1553   
1554                                ; -------------------------------------------------------------
1555                                CALC_NO_BUFFS
1556                                ;----------------------------------------------------
1557                                ; number of 512 buffers in packet calculated (X:TOTAL_BUFFS)
1558                                ; and number of left over blocks (X:NUM_LEFTOVER_BLOCKS)
1559                                ; and left over words (X:LEFT_TO_READ)
1560   
1561      P:0003DD P:0003DF 20001B            CLR     B
1562      P:0003DE P:0003E0 51A300            MOVE              X:<HEAD_W4_0,B0         ; LS 16bits
1563      P:0003DF P:0003E1 44A200            MOVE              X:<HEAD_W4_1,X0         ; MS 16bits
1564   
1565      P:0003E0 P:0003E2 0C1941            INSERT  #$010010,X0,B                     ; now size of packet B....giving # of 32bit 
words in packet
                            010010
1566      P:0003E2 P:0003E4 000000            NOP
1567   
1568                                ; need to covert this to 16 bit since read from FIFO and saved in Y memory as 16bit words...
1569   
1570                                ; so double size of packet....
1571      P:0003E3 P:0003E5 20003A            ASL     B
1572   
1573                                ; now save
1574      P:0003E4 P:0003E6 212400            MOVE              B0,X0
1575      P:0003E5 P:0003E7 21A500            MOVE              B1,X1
1576      P:0003E6 P:0003E8 443600            MOVE              X0,X:<PACKET_SIZE_LOW   ; low 24 bits of packet size (in 16bit words
)
1577      P:0003E7 P:0003E9 453700            MOVE              X1,X:<PACKET_SIZE_HIH   ; high 8 bits of packet size (in 16bit words
)
1578   
1579      P:0003E8 P:0003EA 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1580      P:0003E9 P:0003EB 54B700            MOVE              X:<PACKET_SIZE_HIH,A1
1581      P:0003EA P:0003EC 0C1C12            ASR     #9,A,A                            ; divide by 512...number of 16bit words in a
 buffer
1582      P:0003EB P:0003ED 000000            NOP
1583      P:0003EC P:0003EE 503C00            MOVE              A0,X:<TOTAL_BUFFS
1584   
1585      P:0003ED P:0003EF 210500            MOVE              A0,X1
1586      P:0003EE P:0003F0 47F400            MOVE              #HF_FIFO,Y1
                            000200
1587      P:0003F0 P:0003F2 2000F0            MPY     X1,Y1,A
1588      P:0003F1 P:0003F3 0C1C03            ASR     #1,A,B                            ; B holds number of 16bit words in all full 
buffers
1589      P:0003F2 P:0003F4 000000            NOP
1590   
1591      P:0003F3 P:0003F5 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1592      P:0003F4 P:0003F6 54B700            MOVE              X:<PACKET_SIZE_HIH,A1   ; A holds total number of 16bit words
1593      P:0003F5 P:0003F7 200014            SUB     B,A                               ; now A holds number of left over 16bit word
s
1594      P:0003F6 P:0003F8 000000            NOP
1595      P:0003F7 P:0003F9 503D00            MOVE              A0,X:<LEFT_TO_READ      ; store number of left over 16bit words to r
ead
1596      P:0003F8 P:0003FA 0C1C0A            ASR     #5,A,A                            ; divide by 32... number of 16bit words in l
efover block
1597      P:0003F9 P:0003FB 000000            NOP
1598      P:0003FA P:0003FC 503F00            MOVE              A0,X:<NUM_LEFTOVER_BLOCKS
1599      P:0003FB P:0003FD 210500            MOVE              A0,X1
1600      P:0003FC P:0003FE 47F400            MOVE              #>SMALL_BLK,Y1
                            000020
1601      P:0003FE P:000400 2000F0            MPY     X1,Y1,A
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 33



1602      P:0003FF P:000401 0C1C02            ASR     #1,A,A
1603      P:000400 P:000402 000000            NOP
1604   
1605      P:000401 P:000403 200018            ADD     A,B                               ; B holds words in all buffers
1606      P:000402 P:000404 000000            NOP
1607      P:000403 P:000405 50B600            MOVE              X:<PACKET_SIZE_LOW,A0
1608      P:000404 P:000406 54B700            MOVE              X:<PACKET_SIZE_HIH,A1   ; A holds total number of words
1609      P:000405 P:000407 200014            SUB     B,A                               ; now A holds number of left over words
1610      P:000406 P:000408 000000            NOP
1611      P:000407 P:000409 503D00            MOVE              A0,X:<LEFT_TO_READ      ; store number of left over 16bit words to r
ead
1612   
1613      P:000408 P:00040A 0C1C02            ASR     #1,A,A                            ; divide by two to get number of 32 bit word
s to write
1614      P:000409 P:00040B 000000            NOP                                       ; for pipeline
1615      P:00040A P:00040C 503E00            MOVE              A0,X:<LEFT_TO_WRITE     ; store number of left over 32 bit words (2 
x 16 bit) to write to host after small block transfer as well
1616   
1617      P:00040B P:00040D 00000C            RTS
1618   
1619                                ;---------------------------------------------------------------
1620                                GET_FO_WRD
1621                                ;--------------------------------------------------------------
1622                                ; Anything in fibre receive FIFO?   If so store in X0
1623   
1624      P:00040C P:00040E 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS
                            000422
1625      P:00040E P:000410 000000            NOP
1626      P:00040F P:000411 000000            NOP
1627      P:000410 P:000412 01AD80            JCLR    #EF,X:PDRD,CLR_FO_RTS             ; check twice for FO metastability.
                            000422
1628      P:000412 P:000414 0AF080            JMP     RD_FO_WD
                            00041A
1629   
1630      P:000414 P:000416 01AD80  WT_FIFO   JCLR    #EF,X:PDRD,*                      ; Wait till something in FIFO flagged
                            000414
1631      P:000416 P:000418 000000            NOP
1632      P:000417 P:000419 000000            NOP
1633      P:000418 P:00041A 01AD80            JCLR    #EF,X:PDRD,WT_FIFO                ; check twice.....
                            000414
1634   
1635                                ; Read one word from the fiber optics FIFO, check it and put it in A1
1636                                RD_FO_WD
1637      P:00041A P:00041C 09443F            MOVEP             Y:RDFIFO,X0             ; then read to X0
1638      P:00041B P:00041D 54F400            MOVE              #$00FFFF,A1             ; mask off top 2 bytes ($FC)
                            00FFFF
1639      P:00041D P:00041F 200046            AND     X0,A                              ; since receiving 16 bits in 24bit register
1640      P:00041E P:000420 000000            NOP
1641      P:00041F P:000421 218400            MOVE              A1,X0
1642      P:000420 P:000422 0A0023            BSET    #FO_WRD_RCV,X:<STATUS
1643      P:000421 P:000423 00000C            RTS
1644                                CLR_FO_RTS
1645      P:000422 P:000424 0A0003            BCLR    #FO_WRD_RCV,X:<STATUS
1646      P:000423 P:000425 00000C            RTS
1647   
1648                                ;-----------------------------------------------
1649                                PCI_ERROR_RECOVERY
1650                                ;-----------------------------------------------
1651                                ; Recover from an error writing to the PCI bus
1652   
1653      P:000424 P:000426 0A8A8A            JCLR    #TRTY,X:DPSR,ERROR1               ; Retry error
                            000429
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 34



1654      P:000426 P:000428 08F48A            MOVEP             #$0400,X:DPSR           ; Clear target retry error bit
                            000400
1655      P:000428 P:00042A 00000C            RTS
1656      P:000429 P:00042B 0A8A8B  ERROR1    JCLR    #TO,X:DPSR,ERROR2                 ; Timeout error
                            00042E
1657      P:00042B P:00042D 08F48A            MOVEP             #$0800,X:DPSR           ; Clear timeout error bit
                            000800
1658      P:00042D P:00042F 00000C            RTS
1659      P:00042E P:000430 0A8A89  ERROR2    JCLR    #TDIS,X:DPSR,ERROR3               ; Target disconnect error
                            000433
1660      P:000430 P:000432 08F48A            MOVEP             #$0200,X:DPSR           ; Clear target disconnect bit
                            000200
1661      P:000432 P:000434 00000C            RTS
1662      P:000433 P:000435 0A8A88  ERROR3    JCLR    #TAB,X:DPSR,ERROR4                ; Target abort error
                            000438
1663      P:000435 P:000437 08F48A            MOVEP             #$0100,X:DPSR           ; Clear target abort error bit
                            000100
1664      P:000437 P:000439 00000C            RTS
1665      P:000438 P:00043A 0A8A87  ERROR4    JCLR    #MAB,X:DPSR,ERROR5                ; Master abort error
                            00043D
1666      P:00043A P:00043C 08F48A            MOVEP             #$0080,X:DPSR           ; Clear master abort error bit
                            000080
1667      P:00043C P:00043E 00000C            RTS
1668      P:00043D P:00043F 0A8A86  ERROR5    JCLR    #DPER,X:DPSR,ERROR6               ; Data parity error
                            000442
1669      P:00043F P:000441 08F48A            MOVEP             #$0040,X:DPSR           ; Clear data parity error bit
                            000040
1670      P:000441 P:000443 00000C            RTS
1671      P:000442 P:000444 0A8A85  ERROR6    JCLR    #APER,X:DPSR,ERROR7               ; Address parity error
                            000446
1672      P:000444 P:000446 08F48A            MOVEP             #$0020,X:DPSR           ; Clear address parity error bit
                            000020
1673      P:000446 P:000448 00000C  ERROR7    RTS
1674   
1675                                ; ----------------------------------------------------------------------------
1676                                PCI_MESSAGE_TO_HOST
1677                                ;----------------------------------------------------------------------------
1678   
1679                                ; subroutine to send 4 words as a reply from PCI to the Host
1680                                ; using the DTXS-HRXS data path
1681                                ; PCI card writes here first then causes an interrupt INTA on
1682                                ; the PCI bus to alert the host to the reply message
1683   
1684      P:000447 P:000449 0A85A3            JSET    #DCTR_HF3,X:DCTR,*                ; make sure host ready to receive interrupt
                            000447
1685                                                                                    ; cleared via fast interrupt if host out of 
its ISR
1686   
1687      P:000449 P:00044B 0A8981            JCLR    #STRQ,X:DSR,*                     ; Wait for transmitter to be NOT FULL
                            000449
1688                                                                                    ; i.e. if CLR then FULL so wait
1689                                                                                    ; if not then it is clear to write
1690      P:00044B P:00044D 448C00            MOVE              X:<DTXS_WD1,X0
1691      P:00044C P:00044E 447000            MOVE              X0,X:DTXS               ; Write 24 bit word1
                            FFFFCD
1692   
1693      P:00044E P:000450 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            00044E
1694      P:000450 P:000452 448D00            MOVE              X:<DTXS_WD2,X0
1695      P:000451 P:000453 447000            MOVE              X0,X:DTXS               ; Write 24 bit word2
                            FFFFCD
1696   
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 35



1697      P:000453 P:000455 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            000453
1698      P:000455 P:000457 448E00            MOVE              X:<DTXS_WD3,X0
1699      P:000456 P:000458 447000            MOVE              X0,X:DTXS               ; Write 24 bit word3
                            FFFFCD
1700   
1701      P:000458 P:00045A 0A8981            JCLR    #STRQ,X:DSR,*                     ; wait to be not full
                            000458
1702      P:00045A P:00045C 448F00            MOVE              X:<DTXS_WD4,X0
1703      P:00045B P:00045D 447000            MOVE              X0,X:DTXS               ; Write 24 bit word4
                            FFFFCD
1704   
1705   
1706                                ; restore X0....
1707                                ; PCI_MESSAGE_TO_HOST is used by all command vector ISRs.
1708                                ; Working registers must be restored before RTI.
1709                                ; However, we want to restore before asserting INTA.
1710                                ; x0 is only one that can't be restored before PCI_MESSAGE_TO_HOST
1711                                ; (since it is used by this SR) hence we restore here.
1712                                ; this is redundant for a 'NFY' message (since sequential instruction)
1713                                ; but may be required for a PCI command reply 'REP' message.
1714                                ; (since interrupt driven)
1715   
1716      P:00045D P:00045F 44F000            MOVE              X:SV_X0,X0              ; restore X0
                            00002E
1717   
1718                                ; all the transmit words are in the FIFO, interrupt the Host
1719                                ; the Host should clear this interrupt once it is detected.
1720                                ; It does this by writing to HCVR to cause a fast interrupt.
1721   
1722   
1723      P:00045F P:000461 0A8523            BSET    #DCTR_HF3,X:DCTR                  ; set flag to handshake interrupt (INTA) wit
h host.
1724      P:000460 P:000462 0A8526            BSET    #INTA,X:DCTR                      ; Assert the interrupt
1725   
1726      P:000461 P:000463 00000C            RTS
1727   
1728                                ;---------------------------------------------------------------
1729                                RD_DRXR
1730                                ;--------------------------------------------------------------
1731                                ; routine is used to read from HTXR-DRXR data path
1732                                ; which is used by the Host to communicate with the PCI board
1733                                ; the host writes 4 words to this FIFO then interrupts the PCI
1734                                ; which reads the 4 words and acts on them accordingly.
1735   
1736   
1737      P:000462 P:000464 0A8982            JCLR    #SRRQ,X:DSR,*                     ; Wait for receiver to be not empty
                            000462
1738                                                                                    ; implies that host has written words
1739   
1740   
1741                                ; actually reading as slave here so this shouldn't be necessary......?
1742   
1743      P:000464 P:000466 0A8717            BCLR    #FC1,X:DPMC                       ; 24 bit read FC1 = 0, FC1 = 0
1744      P:000465 P:000467 0A8736            BSET    #FC0,X:DPMC
1745   
1746   
1747      P:000466 P:000468 08440B            MOVEP             X:DRXR,X0               ; Get word1
1748      P:000467 P:000469 440800            MOVE              X0,X:<DRXR_WD1
1749      P:000468 P:00046A 08440B            MOVEP             X:DRXR,X0               ; Get word2
1750      P:000469 P:00046B 440900            MOVE              X0,X:<DRXR_WD2
1751      P:00046A P:00046C 08440B            MOVEP             X:DRXR,X0               ; Get word3
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 36



1752      P:00046B P:00046D 440A00            MOVE              X0,X:<DRXR_WD3
1753      P:00046C P:00046E 08440B            MOVEP             X:DRXR,X0               ; Get word4
1754      P:00046D P:00046F 440B00            MOVE              X0,X:<DRXR_WD4
1755      P:00046E P:000470 00000C            RTS
1756   
1757                                ;---------------------------------------------------------------
1758                                READ_FROM_PCI
1759                                ;--------------------------------------------------------------
1760                                ; sub routine to read a 24 bit word in from PCI bus --> Y memory
1761                                ; 32bit host address in accumulator B.
1762   
1763                                ; read as master
1764   
1765      P:00046F P:000471 0C1890            EXTRACTU #$010010,B,A                     ; Get D31-16 bits only
                            010010
1766      P:000471 P:000473 000000            NOP
1767   
1768      P:000472 P:000474 210C00            MOVE              A0,A1
1769      P:000473 P:000475 000000            NOP
1770      P:000474 P:000476 547000            MOVE              A1,X:DPMC               ; high 16bits of address in DSP master cntr 
reg.
                            FFFFC7
1771                                                                                    ; 32 bit read so FC1 = 0 and FC0 = 0
1772   
1773      P:000476 P:000478 000000            NOP
1774      P:000477 P:000479 0C1890            EXTRACTU #$010000,B,A
                            010000
1775      P:000479 P:00047B 000000            NOP
1776      P:00047A P:00047C 210C00            MOVE              A0,A1
1777      P:00047B P:00047D 0140C2            OR      #$060000,A                        ; A1 gets written to DPAR register
                            060000
1778      P:00047D P:00047F 000000            NOP                                       ; C3-C0 of DPAR=0110 for memory read
1779      P:00047E P:000480 08CC08  WRT_ADD   MOVEP             A1,X:DPAR               ; Write address to PCI bus - PCI READ action
1780      P:00047F P:000481 000000            NOP                                       ; Pipeline delay
1781      P:000480 P:000482 0A8AA2  RD_PCI    JSET    #MRRQ,X:DPSR,GET_DAT              ; If MTRQ = 1 go read the word from host via
 FIFO
                            000489
1782      P:000482 P:000484 0A8A8A            JCLR    #TRTY,X:DPSR,RD_PCI               ; Bit is set if its a retry
                            000480
1783      P:000484 P:000486 08F48A            MOVEP             #$0400,X:DPSR           ; Clear bit 10 = target retry bit
                            000400
1784      P:000486 P:000488 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait for PCI addressing to be complete
                            000486
1785      P:000488 P:00048A 0C047E            JMP     <WRT_ADD
1786   
1787      P:000489 P:00048B 08440B  GET_DAT   MOVEP             X:DRXR,X0               ; Read 1st 16 bits of 32 bit word from host 
memory
1788      P:00048A P:00048C 08450B            MOVEP             X:DRXR,X1               ; Read 2nd 16 bits of 32 bit word from host 
memory
1789   
1790                                ; note that we now have 4 bytes in X0 and X1.
1791                                ; The 32bit word was in host memory in little endian format
1792                                ; If form LSB --> MSB the bytes are b1, b2, b3, b4 in host memory
1793                                ; in progressing through the HTRX/DRXR FIFO the
1794                                ; bytes end up like this.....
1795                                ; then X0 = $00 b2 b1
1796                                ; and  X1 = $00 b4 b3
1797   
1798      P:00048B P:00048D 0604A0            REP     #4                                ; increment PCI address by four bytes.
1799      P:00048C P:00048E 000009            INC     B
1800      P:00048D P:00048F 000000            NOP
1801      P:00048E P:000490 00000C            RTS
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 37



1802   
1803                                ;------------------------------------------------------------------------------------
1804                                RESTORE_REGISTERS
1805                                ;-------------------------------------------------------------------------------------
1806   
1807      P:00048F P:000491 05B239            MOVEC             X:<SV_SR,SR
1808   
1809      P:000490 P:000492 50A800            MOVE              X:<SV_A0,A0
1810      P:000491 P:000493 54A900            MOVE              X:<SV_A1,A1
1811      P:000492 P:000494 52AA00            MOVE              X:<SV_A2,A2
1812   
1813      P:000493 P:000495 51AB00            MOVE              X:<SV_B0,B0
1814      P:000494 P:000496 55AC00            MOVE              X:<SV_B1,B1
1815      P:000495 P:000497 53AD00            MOVE              X:<SV_B2,B2
1816   
1817      P:000496 P:000498 44AE00            MOVE              X:<SV_X0,X0
1818      P:000497 P:000499 45AF00            MOVE              X:<SV_X1,X1
1819   
1820      P:000498 P:00049A 46B000            MOVE              X:<SV_Y0,Y0
1821      P:000499 P:00049B 47B100            MOVE              X:<SV_Y1,Y1
1822   
1823      P:00049A P:00049C 00000C            RTS
1824                                ;------------------------------------------------------------------------------------
1825                                RESTORE_HST_REGISTERS
1826                                ;-------------------------------------------------------------------------------------
1827                                ; B not restored after HST as it now contains address.
1828   
1829      P:00049B P:00049D 05B239            MOVEC             X:<SV_SR,SR
1830   
1831      P:00049C P:00049E 50A800            MOVE              X:<SV_A0,A0
1832      P:00049D P:00049F 54A900            MOVE              X:<SV_A1,A1
1833      P:00049E P:0004A0 52AA00            MOVE              X:<SV_A2,A2
1834   
1835      P:00049F P:0004A1 44AE00            MOVE              X:<SV_X0,X0
1836      P:0004A0 P:0004A2 45AF00            MOVE              X:<SV_X1,X1
1837   
1838      P:0004A1 P:0004A3 46B000            MOVE              X:<SV_Y0,Y0
1839      P:0004A2 P:0004A4 47B100            MOVE              X:<SV_Y1,Y1
1840   
1841      P:0004A3 P:0004A5 00000C            RTS
1842   
1843                                ;-------------------------------------------------------------------------------------
1844                                SAVE_REGISTERS
1845                                ;-------------------------------------------------------------------------------------
1846   
1847      P:0004A4 P:0004A6 053239            MOVEC             SR,X:<SV_SR             ; save status register.  May jump to ISR dur
ing CMP
1848   
1849      P:0004A5 P:0004A7 502800            MOVE              A0,X:<SV_A0
1850      P:0004A6 P:0004A8 542900            MOVE              A1,X:<SV_A1
1851      P:0004A7 P:0004A9 522A00            MOVE              A2,X:<SV_A2
1852   
1853      P:0004A8 P:0004AA 512B00            MOVE              B0,X:<SV_B0
1854      P:0004A9 P:0004AB 552C00            MOVE              B1,X:<SV_B1
1855      P:0004AA P:0004AC 532D00            MOVE              B2,X:<SV_B2
1856   
1857      P:0004AB P:0004AD 442E00            MOVE              X0,X:<SV_X0
1858      P:0004AC P:0004AE 452F00            MOVE              X1,X:<SV_X1
1859   
1860      P:0004AD P:0004AF 463000            MOVE              Y0,X:<SV_Y0
1861      P:0004AE P:0004B0 473100            MOVE              Y1,X:<SV_Y1
1862   
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 38



1863      P:0004AF P:0004B1 00000C            RTS
1864   
1865   
1866   
1867                                ; ------------------------------------------------------------------------------------
1868                                WRITE_TO_PCI
1869                                ;-------------------------------------------------------------------------------------
1870                                ; sub routine to write two 16 bit words (stored in Y memory)
1871                                ; to host memory as PCI bus master.
1872                                ; results in a 32bit word written to host memory.
1873   
1874                                ; the 32 bit host address is in accumulator B.
1875                                ; this address is writen to DPMC (MSBs) and DPAR (LSBs)
1876                                ; address is incrememted by 4 (bytes) after write.
1877   
1878                                ; R2 is used as a pointer to Y:memory address
1879   
1880   
1881      P:0004B0 P:0004B2 0A8A81            JCLR    #MTRQ,X:DPSR,*                    ; wait here if DTXM is full
                            0004B0
1882   
1883      P:0004B2 P:0004B4 08DACC  TX_LSB    MOVEP             Y:(R2)+,X:DTXM          ; Least significant word to transmit
1884      P:0004B3 P:0004B5 08DACC  TX_MSB    MOVEP             Y:(R2)+,X:DTXM          ; Most significant word to transmit
1885   
1886   
1887      P:0004B4 P:0004B6 0C1890            EXTRACTU #$010010,B,A                     ; Get D31-16 bits only,
                            010010
1888      P:0004B6 P:0004B8 000000            NOP                                       ; top byte = $00 so FC1 = FC0 = 0
1889      P:0004B7 P:0004B9 210C00            MOVE              A0,A1
1890   
1891                                ; we are using two 16 bit writes to make a 32bit word
1892                                ; so FC1=0 and FC0=0 when A1 written to DPMC
1893   
1894      P:0004B8 P:0004BA 000000            NOP
1895      P:0004B9 P:0004BB 547000            MOVE              A1,X:DPMC               ; DSP master control register
                            FFFFC7
1896      P:0004BB P:0004BD 000000            NOP
1897      P:0004BC P:0004BE 0C1890            EXTRACTU #$010000,B,A
                            010000
1898      P:0004BE P:0004C0 000000            NOP
1899      P:0004BF P:0004C1 210C00            MOVE              A0,A1
1900      P:0004C0 P:0004C2 0140C2            OR      #$070000,A                        ; A1 gets written to DPAR register
                            070000
1901      P:0004C2 P:0004C4 000000            NOP
1902   
1903      P:0004C3 P:0004C5 08CC08  AGAIN1    MOVEP             A1,X:DPAR               ; Write to PCI bus
1904      P:0004C4 P:0004C6 000000            NOP                                       ; Pipeline delay
1905      P:0004C5 P:0004C7 000000            NOP
1906      P:0004C6 P:0004C8 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Bit is set if its a retry
                            0004C6
1907      P:0004C8 P:0004CA 0A8AAE            JSET    #MDT,X:DPSR,INC_ADD               ; If no error go to the next sub-block
                            0004CC
1908      P:0004CA P:0004CC 0D0424            JSR     <PCI_ERROR_RECOVERY
1909      P:0004CB P:0004CD 0C04C3            JMP     <AGAIN1
1910                                INC_ADD
1911      P:0004CC P:0004CE 205C13            CLR     A         (R4)+                   ; clear A and increment word count
1912      P:0004CD P:0004CF 50F400            MOVE              #>4,A0                  ; 4 bytes per word transfer on pcibus
                            000004
1913      P:0004CF P:0004D1 640618            ADD     A,B       R4,X:<WORD_COUNT        ; Inc bus address by 4 bytes, and save word 
count
1914      P:0004D0 P:0004D2 00000C            RTS
1915   
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 39



1916                                ; -------------------------------------------------------------------------------------------
1917                                WRITE_32_TO_PCI
1918                                ; DMAs 32 x 16bit words to host memory as PCI burst.
1919                                ;-----------------------------------------------------------------------------------------------
1920      P:0004D1 P:0004D3 3A2000            MOVE              #32,N2                  ; Number of 16bit words per transfer
1921      P:0004D2 P:0004D4 3C1000            MOVE              #16,N4                  ; Number of 32bit words per transfer
1922   
1923      P:0004D3 P:0004D5 627000            MOVE              R2,X:DSR0               ; Source address for DMA = pixel data
                            FFFFEF
1924      P:0004D5 P:0004D7 08F4AE            MOVEP             #DTXM,X:DDR0            ; Destination = PCI master transmitter
                            FFFFCC
1925      P:0004D7 P:0004D9 08F4AD            MOVEP             #>31,X:DCO0             ; DMA Count = # of pixels - 1
                            00001F
1926   
1927      P:0004D9 P:0004DB 0C1890            EXTRACTU #$010010,B,A                     ; Get D31-16 bits only
                            010010
1928      P:0004DB P:0004DD 000000            NOP
1929      P:0004DC P:0004DE 210C00            MOVE              A0,A1                   ; [D31-16] in A1
1930      P:0004DD P:0004DF 000000            NOP
1931      P:0004DE P:0004E0 0140C2            ORI     #$0F0000,A                        ; Burst length = # of PCI writes
                            0F0000
1932      P:0004E0 P:0004E2 000000            NOP                                       ;   = # of pixels / 2 - 1 ...$0F = 16
1933      P:0004E1 P:0004E3 547000            MOVE              A1,X:DPMC               ; DPMC = B[31:16] + $3F0000
                            FFFFC7
1934   
1935      P:0004E3 P:0004E5 0C1890            EXTRACTU #$010000,B,A
                            010000
1936      P:0004E5 P:0004E7 000000            NOP
1937      P:0004E6 P:0004E8 210C00            MOVE              A0,A1                   ; Get PCI_ADDR[15:0] into A1[15:0]
1938      P:0004E7 P:0004E9 000000            NOP
1939      P:0004E8 P:0004EA 0140C2            ORI     #$070000,A                        ; A1 gets written to DPAR register
                            070000
1940      P:0004EA P:0004EC 000000            NOP
1941   
1942   
1943      P:0004EB P:0004ED 08F4AC  AGAIN2    MOVEP             #$8EFA51,X:DCR0         ; Start DMA with control register DE=1
                            8EFA51
1944      P:0004ED P:0004EF 08CC08            MOVEP             A1,X:DPAR               ; Initiate writing to the PCI bus
1945      P:0004EE P:0004F0 000000            NOP
1946      P:0004EF P:0004F1 000000            NOP
1947      P:0004F0 P:0004F2 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait until the PCI operation is done
                            0004F0
1948      P:0004F2 P:0004F4 0A8AAE            JSET    #MDT,X:DPSR,WR_OK1                ; If no error go to the next sub-block
                            0004F6
1949      P:0004F4 P:0004F6 0D0424            JSR     <PCI_ERROR_RECOVERY
1950      P:0004F5 P:0004F7 0C04EB            JMP     <AGAIN2                           ; Just try to write the sub-block again
1951                                WR_OK1
1952      P:0004F6 P:0004F8 204C13            CLR     A         (R4)+N4                 ; increment number of 32bit word count
1953      P:0004F7 P:0004F9 50F400            MOVE              #>64,A0                 ; 2 bytes on pcibus per pixel
                            000040
1954      P:0004F9 P:0004FB 640618            ADD     A,B       R4,X:<WORD_COUNT        ; PCI address = + 2 x # of pixels (!!!)
1955      P:0004FA P:0004FC 204A00            MOVE              (R2)+N2                 ; Pixel buffer address = + # of pixels
1956      P:0004FB P:0004FD 00000C            RTS
1957   
1958                                ;------------------------------------------------------------
1959                                WRITE_512_TO_PCI
1960                                ;-------------------------------------------------------------
1961                                ; DMAs 128 x 16bit words to host memory as PCI burst
1962                                ; does x 4 of these (total of 512 x 16bit words written to host memory)
1963                                ;
1964                                ; R2 is used as a pointer to Y:memory address
1965   
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 40



1966   
1967      P:0004FC P:0004FE 3A8000            MOVE              #128,N2                 ; Number of 16bit words per transfer.
1968      P:0004FD P:0004FF 3C4000            MOVE              #64,N4                  ; NUmber of 32bit words per transfer.
1969   
1970                                ; Make sure its always 512 pixels per loop = 1/2 FIFO
1971      P:0004FE P:000500 627000            MOVE              R2,X:DSR0               ; Source address for DMA = pixel data
                            FFFFEF
1972      P:000500 P:000502 08F4AE            MOVEP             #DTXM,X:DDR0            ; Destination = PCI master transmitter
                            FFFFCC
1973      P:000502 P:000504 08F4AD            MOVEP             #>127,X:DCO0            ; DMA Count = # of pixels - 1
                            00007F
1974   
1975                                ; Do loop does 4 x 128 pixel DMA writes = 512.
1976                                ; need to recalculate hi and lo parts of address
1977                                ; for each burst.....Leach code doesn't do this since not
1978                                ; multiple frames...so only needs to inc low part.....
1979   
1980      P:000504 P:000506 060480            DO      #4,WR_BLK0                        ; x # of pixels = 512
                            000527
1981   
1982      P:000506 P:000508 0C1890            EXTRACTU #$010010,B,A                     ; Get D31-16 bits only
                            010010
1983      P:000508 P:00050A 000000            NOP
1984      P:000509 P:00050B 210C00            MOVE              A0,A1                   ; [D31-16] in A1
1985      P:00050A P:00050C 000000            NOP
1986      P:00050B P:00050D 0140C2            ORI     #$3F0000,A                        ; Burst length = # of PCI writes
                            3F0000
1987      P:00050D P:00050F 000000            NOP                                       ;   = # of pixels / 2 - 1 ...$3F = 63
1988      P:00050E P:000510 547000            MOVE              A1,X:DPMC               ; DPMC = B[31:16] + $3F0000
                            FFFFC7
1989   
1990   
1991      P:000510 P:000512 0C1890            EXTRACTU #$010000,B,A
                            010000
1992      P:000512 P:000514 000000            NOP
1993      P:000513 P:000515 210C00            MOVE              A0,A1                   ; Get PCI_ADDR[15:0] into A1[15:0]
1994      P:000514 P:000516 000000            NOP
1995      P:000515 P:000517 0140C2            OR      #$070000,A                        ; A1 gets written to DPAR register
                            070000
1996      P:000517 P:000519 000000            NOP
1997   
1998   
1999      P:000518 P:00051A 08F4AC  AGAIN0    MOVEP             #$8EFA51,X:DCR0         ; Start DMA with control register DE=1
                            8EFA51
2000      P:00051A P:00051C 08CC08            MOVEP             A1,X:DPAR               ; Initiate writing to the PCI bus
2001      P:00051B P:00051D 000000            NOP
2002      P:00051C P:00051E 000000            NOP
2003      P:00051D P:00051F 0A8A84            JCLR    #MARQ,X:DPSR,*                    ; Wait until the PCI operation is done
                            00051D
2004      P:00051F P:000521 0A8AAE            JSET    #MDT,X:DPSR,WR_OK0                ; If no error go to the next sub-block
                            000523
2005      P:000521 P:000523 0D0424            JSR     <PCI_ERROR_RECOVERY
2006      P:000522 P:000524 0C0518            JMP     <AGAIN0                           ; Just try to write the sub-block again
2007                                WR_OK0
2008   
2009      P:000523 P:000525 204C13            CLR     A         (R4)+N4                 ; clear A and increment word count
2010      P:000524 P:000526 50F400            MOVE              #>256,A0                ; 2 bytes on pcibus per pixel
                            000100
2011      P:000526 P:000528 640618            ADD     A,B       R4,X:<WORD_COUNT        ; Inc bus address by # of bytes, and save wo
rd count
2012      P:000527 P:000529 204A00            MOVE              (R2)+N2                 ; Pixel buffer address = + # of pixels
2013                                WR_BLK0
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 41



2014      P:000528 P:00052A 00000C            RTS
2015   
2016                                ;-----------------------------
2017                                XMT_DLY
2018                                ;-----------------------------
2019                                ; Short delay for reliability
2020   
2021      P:000529 P:00052B 000000            NOP
2022      P:00052A P:00052C 000000            NOP
2023      P:00052B P:00052D 000000            NOP
2024      P:00052C P:00052E 00000C            RTS
2025   
2026                                ;-------------------------------------------------------
2027                                XMT_WD_FIBRE
2028                                ;-----------------------------------------------------
2029                                ; 250 MHz code - Transmit contents of Accumulator A1 to the MCE
2030                                ; we want to send 32bit word in little endian fomat to the host.
2031                                ; i.e. b4b3b2b1 goes b1, b2, b3, b4
2032                                ; currently the bytes are in this order:
2033                                ;  A1 = $00 b2 b1
2034                                ;  A0 = $00 b4 b3
2035                                ;  A = $00 00 b2 b1 00 b4 b3
2036   
2037                                ; This subroutine must take at least 160ns (4 bytes at 25Mbytes/s)
2038   
2039      P:00052D P:00052F 000000            NOP
2040      P:00052E P:000530 000000            NOP
2041   
2042                                ; split up 4 bytes b2, b1, b4, b3
2043   
2044      P:00052F P:000531 0C1D20            ASL     #16,A,A                           ; shift byte b2 into A2
2045      P:000530 P:000532 60F400            MOVE              #$FFF000,R0             ; Memory mapped address of transmitter
                            FFF000
2046   
2047      P:000532 P:000534 214700            MOVE              A2,Y1                   ; byte b2 in Y1
2048   
2049      P:000533 P:000535 0C1D10            ASL     #8,A,A                            ; shift byte b1 into A2
2050      P:000534 P:000536 000000            NOP
2051      P:000535 P:000537 214600            MOVE              A2,Y0                   ; byte b1 in Y0
2052   
2053      P:000536 P:000538 0C1D20            ASL     #16,A,A                           ; shift byte b4 into A2
2054      P:000537 P:000539 000000            NOP
2055      P:000538 P:00053A 214500            MOVE              A2,X1                   ; byte b4 in X1
2056   
2057   
2058      P:000539 P:00053B 0C1D10            ASL     #8,A,A                            ; shift byte b3 into A2
2059      P:00053A P:00053C 000000            NOP
2060      P:00053B P:00053D 214400            MOVE              A2,X0                   ; byte b3 in x0
2061   
2062                                ; transmit b1, b2, b3 ,b4
2063   
2064      P:00053C P:00053E 466000            MOVE              Y0,X:(R0)               ; byte b1 - off it goes
2065      P:00053D P:00053F 476000            MOVE              Y1,X:(R0)               ; byte b2 - off it goes
2066      P:00053E P:000540 446000            MOVE              X0,X:(R0)               ; byte b3 - off it goes
2067      P:00053F P:000541 456000            MOVE              X1,X:(R0)               ; byte b4 - off it goes
2068   
2069      P:000540 P:000542 000000            NOP
2070      P:000541 P:000543 000000            NOP
2071      P:000542 P:000544 00000C            RTS
2072   
2073   
2074                                BOOTCODE_END
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 42



2075                                 BOOTEND_ADDR
2076      000543                              EQU     @CVI(BOOTCODE_END)
2077   
2078                                PROGRAM_END
2079      000543                    PEND_ADDR EQU     @CVI(PROGRAM_END)
2080                                ;---------------------------------------------
2081   
2082   
2083                                ; --------------------------------------------------------------------
2084                                ; --------------- x memory parameter table ---------------------------
2085                                ; --------------------------------------------------------------------
2086   
2087      X:000000 P:000545                   ORG     X:VAR_TBL,P:
2088   
2089   
2090                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2091                                 VAR_TBL_START
2092      000543                              EQU     @LCV(L)-2
2093                                          ENDIF
2094   
2095                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2097                                          ENDIF
2098   
2099                                ; -----------------------------------------------
2100                                ; do not move these (X:0 --> x:3)
2101 d    X:000000 P:000545 000000  STATUS    DC      0
2102 d                               FRAME_COUNT
2103 d    X:000001 P:000546 000000            DC      0                                 ; used as a check....... increments for ever
y frame write.....must be cleared by host.
2104 d                               PRE_CORRUPT
2105 d    X:000002 P:000547 000000            DC      0
2106 d    X:000003 P:000548 410105  REV_NUMBER DC     $410105                           ; byte 0 = minor revision #
2107                                                                                    ; byte 1 = mayor revision #
2108                                                                                    ; byte 2 = release Version (ascii letter)
2109 d    X:000004 P:000549 250507  REV_DATA  DC      $250507                           ; data: day-month-year
2110 d    X:000005 P:00054A 2EF490  P_CHECKSUM DC     $2EF490                           ;**** DO NOT CHANGE
2111                                ; -------------------------------------------------
2112 d    X:000006 P:00054B 000000  WORD_COUNT DC     0                                 ; word count.  Number of words successfully 
writen to host in last packet.
2113 d    X:000007 P:00054C 000000  NUM_DUMPED DC     0                                 ; number of words (16-bit) dumped to Y memor
y (512) after an HST timeout.
2114                                ; ----------------------------------------------------------------------------------------------
----------------
2115   
2116 d    X:000008 P:00054D 000000  DRXR_WD1  DC      0
2117 d    X:000009 P:00054E 000000  DRXR_WD2  DC      0
2118 d    X:00000A P:00054F 000000  DRXR_WD3  DC      0
2119 d    X:00000B P:000550 000000  DRXR_WD4  DC      0
2120 d    X:00000C P:000551 000000  DTXS_WD1  DC      0
2121 d    X:00000D P:000552 000000  DTXS_WD2  DC      0
2122 d    X:00000E P:000553 000000  DTXS_WD3  DC      0
2123 d    X:00000F P:000554 000000  DTXS_WD4  DC      0
2124   
2125 d    X:000010 P:000555 000000  PCI_WD1_1 DC      0
2126 d    X:000011 P:000556 000000  PCI_WD1_2 DC      0
2127 d    X:000012 P:000557 000000  PCI_WD2_1 DC      0
2128 d    X:000013 P:000558 000000  PCI_WD2_2 DC      0
2129 d    X:000014 P:000559 000000  PCI_WD3_1 DC      0
2130 d    X:000015 P:00055A 000000  PCI_WD3_2 DC      0
2131 d    X:000016 P:00055B 000000  PCI_WD4_1 DC      0
2132 d    X:000017 P:00055C 000000  PCI_WD4_2 DC      0
2133 d    X:000018 P:00055D 000000  PCI_WD5_1 DC      0
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 43



2134 d    X:000019 P:00055E 000000  PCI_WD5_2 DC      0
2135 d    X:00001A P:00055F 000000  PCI_WD6_1 DC      0
2136 d    X:00001B P:000560 000000  PCI_WD6_2 DC      0
2137   
2138   
2139 d    X:00001C P:000561 000000  HEAD_W1_1 DC      0
2140 d    X:00001D P:000562 000000  HEAD_W1_0 DC      0
2141 d    X:00001E P:000563 000000  HEAD_W2_1 DC      0
2142 d    X:00001F P:000564 000000  HEAD_W2_0 DC      0
2143 d    X:000020 P:000565 000000  HEAD_W3_1 DC      0
2144 d    X:000021 P:000566 000000  HEAD_W3_0 DC      0
2145 d    X:000022 P:000567 000000  HEAD_W4_1 DC      0
2146 d    X:000023 P:000568 000000  HEAD_W4_0 DC      0
2147   
2148   
2149 d    X:000024 P:000569 000000  REP_WD1   DC      0
2150 d    X:000025 P:00056A 000000  REP_WD2   DC      0
2151 d    X:000026 P:00056B 000000  REP_WD3   DC      0
2152 d    X:000027 P:00056C 000000  REP_WD4   DC      0
2153   
2154 d    X:000028 P:00056D 000000  SV_A0     DC      0
2155 d    X:000029 P:00056E 000000  SV_A1     DC      0
2156 d    X:00002A P:00056F 000000  SV_A2     DC      0
2157 d    X:00002B P:000570 000000  SV_B0     DC      0
2158 d    X:00002C P:000571 000000  SV_B1     DC      0
2159 d    X:00002D P:000572 000000  SV_B2     DC      0
2160 d    X:00002E P:000573 000000  SV_X0     DC      0
2161 d    X:00002F P:000574 000000  SV_X1     DC      0
2162 d    X:000030 P:000575 000000  SV_Y0     DC      0
2163 d    X:000031 P:000576 000000  SV_Y1     DC      0
2164   
2165 d    X:000032 P:000577 000000  SV_SR     DC      0                                 ; stauts register save.
2166   
2167 d    X:000033 P:000578 000000  ZERO      DC      0
2168 d    X:000034 P:000579 000001  ONE       DC      1
2169 d    X:000035 P:00057A 000004  FOUR      DC      4
2170   
2171   
2172   
2173 d                               PACKET_SIZE_LOW
2174 d    X:000036 P:00057B 000000            DC      0
2175 d                               PACKET_SIZE_HIH
2176 d    X:000037 P:00057C 000000            DC      0
2177   
2178 d    X:000038 P:00057D 00A5A5  PREAMB1   DC      $A5A5                             ; pramble 16-bit word....2 of which make up 
first preamble 32bit word
2179 d    X:000039 P:00057E 005A5A  PREAMB2   DC      $5A5A                             ; preamble 16-bit word....2 of which make up
 second preamble 32bit word
2180 d    X:00003A P:00057F 004441  DATA_WD   DC      $4441                             ; "DA"
2181 d    X:00003B P:000580 005250  REPLY_WD  DC      $5250                             ; "RP"
2182   
2183 d                               TOTAL_BUFFS
2184 d    X:00003C P:000581 000000            DC      0                                 ; total number of 512 buffers in packet
2185 d                               LEFT_TO_READ
2186 d    X:00003D P:000582 000000            DC      0                                 ; number of words (16 bit) left to read afte
r last 512 buffer
2187 d                               LEFT_TO_WRITE
2188 d    X:00003E P:000583 000000            DC      0                                 ; number of woreds (32 bit) to write to host
 i.e. half of those left over read
2189 d                               NUM_LEFTOVER_BLOCKS
2190 d    X:00003F P:000584 000000            DC      0                                 ; small block DMA burst transfer
2191   
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_main.asm  Page 44



2192 d                               DATA_DLY_VAL
2193 d    X:000040 P:000585 000000            DC      0                                 ; data delay value..  Delay added to first f
rame received after GO command
2194 d    X:000041 P:000586 000200  CONSTORE  DC      $200
2195   
2196                                ;----------------------------------------------------------
2197   
2198   
2199   
2200                                          IF      @SCP("ROM","ROM")                 ; Boot ROM code
2201                                 VAR_TBL_END
2202      000585                              EQU     @LCV(L)-2
2203                                          ENDIF
2204   
2205                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2207                                          ENDIF
2208   
2209                                 VAR_TBL_LENGTH
2210      000042                              EQU     VAR_TBL_END-VAR_TBL_START
2211   
2212   
2213                                          IF      @CVS(N,*)>=APPLICATION
2215                                          ENDIF
2216   
2217   
2218                                ;--------------------------------------------
2219                                ; APPLICATION AREA
2220                                ;---------------------------------------------
2221                                          IF      @SCP("ROM","ROM")                 ; Download via ONCE debugger
2222      P:000800 P:000802                   ORG     P:APPLICATION,P:APPLICATION+2
2223                                          ENDIF
2224   
2225                                          IF      @SCP("ROM","ONCE")                ; Download via ONCE debugger
2227                                          ENDIF
2228   
2229                                ; starts with no application loaded
2230                                ; so just reply with an error if we get a GOA command
2231      P:000800 P:000802 44F400            MOVE              #'REP',X0
                            524550
2232      P:000802 P:000804 440C00            MOVE              X0,X:<DTXS_WD1          ; REPly
2233      P:000803 P:000805 44F400            MOVE              #'GOA',X0
                            474F41
2234      P:000805 P:000807 440D00            MOVE              X0,X:<DTXS_WD2          ; echo command sent
2235      P:000806 P:000808 44F400            MOVE              #'ERR',X0
                            455252
2236      P:000808 P:00080A 440E00            MOVE              X0,X:<DTXS_WD3          ; No Application Loaded
2237      P:000809 P:00080B 44F400            MOVE              #'NAL',X0
                            4E414C
2238      P:00080B P:00080D 440F00            MOVE              X0,X:<DTXS_WD4          ; write to PCI memory error;
2239      P:00080C P:00080E 0D048F            JSR     <RESTORE_REGISTERS
2240      P:00080D P:00080F 0D0447            JSR     <PCI_MESSAGE_TO_HOST
2241      P:00080E P:000810 0A0000            BCLR    #APPLICATION_LOADED,X:<STATUS
2242      P:00080F P:000811 0C0173            JMP     PACKET_IN
2243   
2244   
2245      000812                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM
2246   
**** 2247 [PCI_SCUBA_build.asm 25]:  Build is complete
2247                                          MSG     ' Build is complete'
2248   
2249   
2250   
Motorola DSP56300 Assembler  Version 6.3.4   07-10-08  12:48:15  PCI_SCUBA_build.asm  Page 45




0    Errors
0    Warnings


