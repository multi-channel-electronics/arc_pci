      COMMENT *

Variable table and bit defines for our variables.

See info.asm for versioning and authors.

	*


; The variable table is mapped to X memory but stored inline in the
; eeprom / P memory after the main code (but before the application
; area).

        ORG     X:VAR_TBL,P:


	IF	@SCP("DOWNLOAD","ROM")	; Boot ROM code
VAR_TBL_START	EQU	@LCV(L)-2
	ENDIF

	IF	@SCP("DOWNLOAD","ONCE")	; Download via ONCE debugger
VAR_TBL_START	EQU	@LCV(L)
	ENDIF

; -----------------------------------------------
STATUS			DC	0	; Internal control flags
MODE			DC	0	; Configure special options

FRAME_COUNT		DC	0
REV_NUMBER		DC	$550105	; byte 0 = minor revision #
					; byte 1 = major revision #
					; byte 2 = release Version (ascii letter)
REV_DATA		DC	$000000 ; data: day-month-year
P_CHECKSUM		DC	$2EF490 ;**** DO NOT CHANGE
; -------------------------------------------------
NUM_DUMPED		DC	0	; number of words (16-bit) dumped to Y memory (512) after an HST timeout.
; --------------------------------------------------------------------------------------------------------------

DRXR_WD1		DC	0
DRXR_WD2		DC	0
DRXR_WD3		DC	0
DRXR_WD4		DC	0
DTXS_WD1		DC	0
DTXS_WD2		DC	0
DTXS_WD3		DC	0
DTXS_WD4		DC	0

HEAD_W1_0		DC	0       ; Preamble $A5A5
HEAD_W1_1		DC	0	;          $A5A5
HEAD_W2_0		DC	0	;          $5A5A
HEAD_W2_1		DC	0	;          $5A5A
HEAD_W3_0		DC	0	; 'RP' or 'DA'
HEAD_W3_1 		DC	0	; '  '   $2020
HEAD_W4_0		DC	0	; Packet size LSW
HEAD_W4_1		DC	0	;             MSW

SV_A0			DC	0   
SV_A1			DC	0 
SV_A2			DC	0
SV_B0			DC	0
SV_B1			DC	0
SV_B2			DC	0
SV_X0			DC	0
SV_X1			DC	0
SV_Y0			DC	0
SV_Y1			DC	0
SV_R0			DC	0

SV_SR			DC	0	; stauts register save.

PACKET_SIZE_LOW		DC	0
PACKET_SIZE_HIH		DC	0

PREAMB1			DC	$A5A5	; pramble 16-bit word....2 of which make up first preamble 32bit word
PREAMB2			DC	$5A5A	; preamble 16-bit word....2 of which make up second preamble 32bit word

TOTAL_BUFFS		DC	0	; total number of 512 buffers in packet
LEFT_TO_READ		DC	0	; number of words (16 bit) left to read after last 512 buffer
LEFT_TO_WRITE		DC	0	; number of woreds (32 bit) to write to host i.e. half of those left over read
NUM_LEFTOVER_BLOCKS	DC	0	; small block DMA burst transfer

PACKET_SIZE		DC	0	; Size, in dwords of most recent packet from MCE.


;;; Packet processing error counts
PREAMBLE_ERRORS		DC	0   ; Failed on preamble processing
PTYPE_ERRORS		DC	0   ; Failed on packet type
PSIZE_ERRORS		DC	0   ; Failed on packet size test
	
;;;PCI burst parameters
	
BLOCK_SIZE		DC	0
BURST_SIZE		DC	0
BURST_DEST_LO		DC	0
BURST_DEST_HI		DC	0
BURST_SRC_LO		DC	0
BURST_SRC_HI		DC	0
YMEM_SRC		DC	0
YMEM_DEST               DC      0
	
DMA_ERRORS		DC	0
EC_TRTY			DC	0
EC_TO			DC	0
EC_TDIS			DC	0
EC_TAB			DC	0
EC_MAB			DC	0
EC_DPER			DC	0
EC_APER			DC	0

	
;;; Vars for Quiet Transfer Mode
	
QT_BASE_LO		DC	0	; PC buffer start address bits 15-0
QT_BASE_HI		DC	0	; PC buffer start address bits 31-16
QT_BUF_SIZE		DC	0	; Separation of buffers, in bytes
QT_BUF_MAX		DC	0	; Number of buffers
QT_FRAME_SIZE		DC	0	; Expected data packet size, in bytes
QT_INFORM		DC	0	; Number of packets to copy before informing

QT_BUF_HEAD		DC	0	; Index of buf for next write
QT_BUF_TAIL		DC	0	; Index at which we must not write

QT_DEST_LO		DC	0	; PC address for next write
QT_DEST_HI		DC	0	; 
QT_INFORM_IDX		DC	0	; Number of packets since last inform
QT_DROPS		DC	0	; Dropped packets


;;; Vars for RP Quiet transfer
RP_BASE_LO		DC	0
RP_BASE_HI		DC	0
RP_MAX_SIZE		DC	0
RP_DROPS		DC	0

;;; Source bus address for MCE commands
CON_SRC_LO		DC	0
CON_SRC_HI		DC	0
	
;;; Bus latency timer
PCI_BURST_SIZE		DC	$40	; Should be < 4*latency assigned by OS

;;; Timer buffer
TIMER_INDEX		DC	0
	
;;; Generic debug
		
BDEBUG0			DC	0
BDEBUG1			DC	0
BDEBUG2			DC	0
BDEBUG3			DC	0
BDEBUG4			DC	0
BDEBUG5			DC	0
BDEBUG6			DC	0
BDEBUG7			DC	0
BDEBUG8			DC	0
BDEBUG9			DC	0

;----------------------------------------------------------

;;; Bit defines for STATUS word

APPLICATION_RUNNING	EQU	0   ; Indicates application is in progress
SEND_TO_HOST		EQU	1   ; set in HST ISR when host ready for packet (stays set until after HST reply)
FATAL_ERROR		EQU	2   ; PCI message to host error detected by driver....
FO_WRD_RCV		EQU	3   ; set when packet detected in FIFO - stays set till packet processed

; PREAMBLE_ERROR		EQU	6   ; set if preamble error detected
; DATA_DLY		EQU	7   ; set in CON ISR if MCE command is 'GO'.  USed to add delay to first returned data packet 

HST_NFYD		EQU	9   ; set after host notified (NFY message) of packet (stays set until after HST reply)

CON_DEMAND		EQU	10  ; Host has requested an MCE command be sent
CON_MCE                 EQU     11  ; Command has been copied and we should send it to the MCE

PCIDMA_RESTART		EQU	16  ; DMA flags used for error recovery
PCIDMA_RESUME		EQU	17
PCIDMA_RETRY		EQU	18

QT_FLUSH		EQU	20  ; Set when it is time to inform Host of current buffer position.
RP_BUFFER_FULL		EQU	21  ; Set when Quiet RP buffer is occupied.

FREEZER			EQU	22  ; Suspend operations and just idle in the main loop
MAIN_LOOP_POLL          EQU     23  ; Cleared by the main loop, use to check for DSP lock-up
	
;;; Bit defines for MODE word

MODE_APPLICATION	EQU	0   ; set if PCI application to run
MODE_MCE		EQU	1   ; process packets from MCE
MODE_QT			EQU	2   ; Quiet transfer for data packets
MODE_RP_BUFFER		EQU     3   ; Quiet transfer for reply packets
MODE_NOIRQ		EQU	4   ; Disbale PCI interrupts on NFY
MODE_HANDSHAKE		EQU	5   ; Enable IRQ hand-shaking

	IF	@SCP("DOWNLOAD","ROM")	; Boot ROM code
VAR_TBL_END	EQU	@LCV(L)-2
	ENDIF

	IF	@SCP("DOWNLOAD","ONCE")	; Download via ONCE debugger
VAR_TBL_END	EQU	@LCV(L)
	ENDIF

VAR_TBL_LENGTH EQU	VAR_TBL_END-VAR_TBL_START
