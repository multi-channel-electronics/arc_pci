      COMMENT *

Defines for DSP register addresses.

See info.asm for versioning and authors.

	*
	PAGE    132     ; Printronix page width - 132 columns
	OPT	CEX	; print DC evaluations

; Equates to define the X: memory tables
VAR_TBL		EQU	0	; Variables and constants table

APPLICATION	EQU	$800	; application memory start location in P memory
				; note applications should start with this address 
				; and end with a JMP to PACKET_IN
				; if only want appl to run once
				; penultimate line of code should be 
				; to clear bit APPLICATION_LOADED in STATUS
				; otherwise will run continusly until 'STP'
				; command is sent

APPL_PARAM	EQU	$200 	; application parameters in x memory start here.


HF_FIFO		EQU	512	; number of 16 bit words in a half full FIFO
SMALL_BLK	EQU	32	; small block burst size for < 512 pixels

;;; Y memory (buffer) mapping	
IMAGE_BUFFER	EQU	0	; Data frame buffer offset.
REPLY_BUFFER	EQU     $100000	; Buffer MCE replies at 1M
COMMAND_BUFFER	EQU	$200000	; Buffer MCE commands at 2M
TIMER_BUFFER    EQU     $201000
TIMER_BUFFER_END EQU    $202000

	
; HST timeout recovery....

MAX_DUMP		EQU	512	; if HST timeout.. max number that could be in FIFO is 511..
DUMP_BUFF		EQU	$1000	; store in Y memory above normal data buffer: in off-chip RAM



; Various addressing control registers
BCR	EQU	$FFFFFB		; Bus Control Register
DCR	EQU	$FFFFFA		; DRAM Control Register
AAR0	EQU	$FFFFF9		; Address Attribute Register, channel 0	
AAR1	EQU	$FFFFF8		; Address Attribute Register, channel 1	
AAR2	EQU	$FFFFF7		; Address Attribute Register, channel 2	
AAR3	EQU	$FFFFF6		; Address Attribute Register, channel 3	
PCTL	EQU	$FFFFFD		; PLL control register
IPRP	EQU	$FFFFFE		; Interrupt Priority register - Peripheral
IPRC	EQU	$FFFFFF		; Interrupt Priority register - Core

; PCI control register
DTXS	EQU	$FFFFCD		; DSP Slave transmit data FIFO
DTXM	EQU	$FFFFCC		; DSP Master transmit data FIFO
DRXR	EQU	$FFFFCB		; DSP Receive data FIFO
DPSR	EQU	$FFFFCA		; DSP PCI Status Register 
DSR	EQU	$FFFFC9		; DSP Status Register
DPAR	EQU	$FFFFC8		; DSP PCI Address Register
DPMC	EQU	$FFFFC7		; DSP PCI Master Control Register 
DPCR	EQU	$FFFFC6		; DSP PCI Control Register
DCTR	EQU	$FFFFC5		; DSP Control Register

; Port E is the Synchronous Communications Interface (SCI) port
PCRE	EQU	$FFFF9F		; Port Control Register
PRRE	EQU	$FFFF9E		; Port Direction Register
PDRE	EQU	$FFFF9D		; Port Data Register

; Various PCI register bit equates
STRQ	EQU	1		; Slave transmit data request (DSR)
SRRQ	EQU	2		; Slave receive data request (DSR) 
HACT	EQU	23		; Host active, low true (DSR)
MTRQ	EQU	1		; Set whem master transmitter is not full (DPSR)
MARQ	EQU	4		; Master address request (DPSR)
MRRQ	EQU	2		; Master Receive Request (DPSR)
TRTY	EQU	10		; PCI Target Retry (DPSR)

APER	EQU	5		; Address parity error
DPER	EQU	6		; Data parity error
MAB	EQU	7		; Master Abort
TAB	EQU	8		; Target Abort
TDIS	EQU	9		; Target Disconnect
TO	EQU	11		; Timeout
MDT	EQU	14		; Master Data Transfer complete
RDCQ	EQU	15		; Remaining Data Count Qualifier
	
SCLK	EQU	2		; SCLK = transmitter special code

; bits in DPMC

FC1	EQU	23
FC0	EQU	22


; DMA register definitions
DSR0	EQU	$FFFFEF		; Source address register
DDR0	EQU	$FFFFEE		; Destination address register
DCO0	EQU	$FFFFED		; Counter register
DCR0	EQU	$FFFFEC		; Control register

; DCTR bits
DCTR_HCIE	EQU	0 	; Interrupt enable
DCTR_SRIE	EQU	2	; Slave request interrupt enable
DCTR_HF3	EQU	3	; Semaphore for INTA handshaking
DCTR_HF4  	EQU	4	; 
DCTR_HF5  	EQU	5	; 
INTA	   	EQU	6	; Request PCI interrupt

; The DSR host flags are written by the PCI host and read by the DSP
DSR_HF0   EQU	3		; PC side INTA hand-shaking
DSR_HF1   EQU	4		; PC side hand-shaking enabled
DSR_HF2   EQU	5		; PC side INTA disable (polling mode)

; DPCR bit definitions
CLRT	EQU	14		; Clear transmitter
MACE	EQU	18		; Master access counter enable
IAE	EQU	21		; Insert Address Enable


	
; Addresses of ESSI port
TX00	EQU	$FFFFBC		; Transmit Data Register 0
SSISR0	EQU	$FFFFB7		; Status Register
CRB0	EQU	$FFFFB6		; Control Register B
CRA0	EQU	$FFFFB5		; Control Register A

; SSI Control Register A Bit Flags
TDE	EQU	6		; Set when transmitter data register is empty

; Miscellaneous addresses
RDFIFO	EQU	$FFFFFF		; Read the FIFO for incoming fiber optic data

; Timer registers
TCSR0	EQU	$FFFF8F		; Control and status register
TLR0	EQU	$FFFF8E		; Load register
TCPR0	EQU	$FFFF8D		; Compare register
TCR0	EQU	$FFFF8C		; Count register
TCSR1	EQU	$FFFF8B		; Control and status register
TLR1	EQU	$FFFF8A		; Load register
TCPR1	EQU	$FFFF89		; Compare register
TCR1	EQU	$FFFF88		; Count register
TCSR2	EQU	$FFFF87		; Control and status register
TLR2	EQU	$FFFF86		; Load register
TCPR2	EQU	$FFFF85		; Compare register
TCR2	EQU	$FFFF84		; Count register

;***************************************************************
; Phase Locked Loop initialization
PLL_INIT EQU	$050003		; PLL = 25 MHz x 4 = 100 MHz
;****************************************************************

; Port C is Enhanced Synchronous Serial Port 0
PCRC	EQU	$FFFFBF		; Port C Control Register
PRRC	EQU	$FFFFBE		; Port C Data direction Register
PDRC	EQU	$FFFFBD		; Port C GPIO Data Register

; Port D is Enhanced Synchronous Serial Port 1
PCRD	EQU	$FFFFAF		; Port D Control Register
PRRD	EQU	$FFFFAE		; Port D Data direction Register
PDRD	EQU	$FFFFAD		; Port D GPIO Data Register

; Bit number definitions of GPIO pins on Port C
ROM_FIFO EQU	2		; Select ROM or FIFO accesses for AA1
AUX1	EQU	4		; enable/disable byte swapping

; Bit number definitions of GPIO pins on Port D
EF	EQU	0		; FIFO Empty flag, low true
HF	EQU	1		; FIFO half full flag, low true
RS	EQU	2		; FIFO reset signal, low true
FSYNC	EQU	3		; High during image transmission
WRFIFO	EQU	5		; Low true if FIFO is being written to


; Errors - self test application

Y_MEM_ER	EQU	0	; y memory corrupted
X_MEM_ER	EQU	1	; x memory corrupted
P_MEM_ER	EQU	2	; p memory corrupted
FO_EMPTY	EQU	3	; no transmitted data in FIFO

FO_OVER		EQU	4	; too much data received
FO_UNDER	EQU	5 	; not enough data receiv
FO_RX_ER	EQU	6	; received data in FIFO incorrect.
DEBUG		EQU	7	; debug bit


;;; Timer TCSR bit definitions
TE		EQU	0
TOIE		EQU	1
TCIE		EQU	2
TOF		EQU	20
TCF		EQU	21


;;; FO transmitter memory-mapped location

FO_SEND		EQU	$FFF000

;--------------------------------------------------------------------
; Interrupt configuration
;--------------------------------------------------------------------
;  IPRC determines core interrupt modes and levels.
;   - [5:3] IRQB mode|level - FIFO half full
;   - [8:6] IRQC mode|level - reset switch
;  So $1C0 is 111|000|000 = IRQC level-triggered priority 3 and IRQB disabled.
;
;  IPRP determines peripheral interrupt levels.
;   - [1:0] HI-32 level.  Must be higher than SR IPL mask
;  So set to 2, and ensure SR[I] > 1.
;
;  SR determines many things... but most importantly
;   - [9:8] Interrupt mask - must be smaller than HI-32 IPL
;  So set to $100

MY_IPRC		EQU	$0001C0
MY_IPRP		EQU	$000002
MY_SR		EQU	$000100

