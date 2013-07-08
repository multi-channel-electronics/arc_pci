      COMMENT *

Variable table and bit defines for our variables.

See info.asm for versioning and authors.

	*


; The variable table is mapped to X memory but stored inline in the
; eeprom / P memory after the main code (but before the application
; area).

        ORG X:VAR_TBL,P:


	IF	@SCP("DOWNLOAD","ROM")	; Boot ROM code
VAR_TBL_START	EQU	@LCV(L)-2
	ENDIF

	IF	@SCP("DOWNLOAD","ONCE")	; Download via ONCE debugger
VAR_TBL_START	EQU	@LCV(L)
	ENDIF


;-----------------------------------------------
STATUS			DC	0	; Internal status flags
MODE			DC	0	; Operating mode control
FRAME_COUNT		DC	0	; Count of data frames from MCE

;-----------------------------------------------
REV_NUMBER		DC	$550106	; byte 0 = minor revision #
					; byte 1 = major revision #
					; byte 2 = release Version (ascii letter)
REV_DATA		DC	$000000 ; Not used by UBC
P_CHECKSUM		DC	$2EF490 ; Not used by UBC

;-----------------------------------------------
NUM_DUMPED		DC	0	; number of words (16-bit) dumped to Y memory (512) after an HST timeout.
;-----------------------------------------------


;;; Vector command handling
	
DRXR_WD1		DC	0	; Storage for words read from PC during vector command
DRXR_WD2		DC	0
DRXR_WD3		DC	0
DRXR_WD4		DC	0

DTXS_WD1		DC	0	; Storage for words to be written to PC as reply
DTXS_WD2		DC	0
DTXS_WD3		DC	0
DTXS_WD4		DC	0

	
;;; Register storage during vector command interrupts

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
SV_SR			DC	0


;;; MCE packet header storage

HEAD_W1_0		DC	0       ; Preamble $A5A5
HEAD_W1_1		DC	0	;          $A5A5
HEAD_W2_0		DC	0	;          $5A5A
HEAD_W2_1		DC	0	;          $5A5A
HEAD_W3_0		DC	0	; 'RP' or 'DA'
HEAD_W3_1 		DC	0	; '  '   $2020
HEAD_W4_0		DC	0	; Packet size LSW
HEAD_W4_1		DC	0	;             MSW


;;; Packet processing
	
PACKET_SIZE		DC	0	; Size, in dwords of most recent packet from MCE.
TOTAL_BUFFS		DC	0	; Number of 512 word half-buffers in packet.
LEFT_TO_READ		DC	0	; Number of words left to read after last 512 buffer

PREAMBLE_ERRORS		DC	0	; Failed on preamble processing
PTYPE_ERRORS		DC	0	; Failed on packet type
PSIZE_ERRORS		DC	0	; Failed on packet size test

	
;;; PCI burst parameters
	
PCI_BURST_SIZE		DC	$40	; Should be < 4*latency assigned by OS
BURST_SIZE		DC	0
BLOCK_SIZE		DC	0

CON_SRC_LO		DC	0	; Set by CON host command
CON_SRC_HI		DC	0

YMEM_SRC		DC	0	; Vars for YMEM -> PC transfers
BURST_DEST_LO		DC	0
BURST_DEST_HI		DC	0

BURST_SRC_LO		DC	0	; Vars for PC -> YMEM transfers
BURST_SRC_HI		DC	0
YMEM_DEST               DC      0
	
DMA_ERRORS		DC	0	; Error counting
EC_TRTY			DC	0
EC_TO			DC	0
EC_TDIS			DC	0
EC_TAB			DC	0
EC_MAB			DC	0
EC_DPER			DC	0
EC_APER			DC	0

	
;;; Quiet Transfer Mode setup - for data packets -> PC
	
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
QT_DROPS		DC	0	; Dropped packet count


;;; Quiet-RP setup - for rapid replies -> PC
	
RP_BASE_LO		DC	0	; PC buffer start address
RP_BASE_HI		DC	0	; 
RP_MAX_SIZE		DC	0	; Maximum reply size, dwords
RP_DROPS		DC	0	; Dropped packet count


;;; Timer storage buffer index

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
	

;;; 
;;; New definitions for U0107 alternative protocols
;;; 

;;; Bits in STATUS... watch for conflicts. (4-8, 12-15 are free)
COMM_REP		EQU	4 ; Reply needs to be sent
COMM_CMD		EQU	5 ; Command needs to be processed
COMM_MCEREP		EQU	6 ; MCE reply has been buffered for send to host
COMM_MCEDATA		EQU	7 ; MCE data " "
COMM_ERR		EQU	8 ; Command not recognized or whatever
COMM_REP_ENABLED	EQU	12 ;
COMM_BUF_UPDATE		EQU	13 ; Data has been written to buffer
COMM_TFR_YMEM		EQU	14 ; PCI burst is coming from Y mem, not X mem.

	
FIFO_FAILS		DC	0
PTYPE_FAILS		DC	0
DA_COUNT		DC	0
	
;;; WORD and SIZE must be adjacent like this.
CMD_SIZE		DC	0
CMD_WORD		DC	0

REP_BUS_ADDR		DC	0,0
	
MEM_SRC			DC	0
	
INT_DEBUG_BUF_IDX	DC	0
DEBUG_BUF_IDX		DC	0

QT_MAX_BLOCKS		EQU	20
; struct QT_DATA:
QT_BLOCK__ADDR		EQU	0
QT_BLOCK__END_IDX	EQU	2
QT_BLOCK___SIZE		EQU	3	
QT_N_BLOCK		DC	0
QT_BLOCK_PTR		DC	0
QT_BLOCKS		DS	(QT_MAX_BLOCKS*QT_BLOCK___SIZE)
	

;;; MCE packet header; 8 words; keep them together.
MCEHDR_PREAMBLE		DS	(4)
MCEHDR_TYPE		DS	(2)
MCEHDR_SIZE		DS	(2)
MCEHDR			EQU	MCEHDR_PREAMBLE

;;; Secured MCE reply; contains TYPE and SIZE as well as up to 64 dwords of payload
MCEREP__TYPE		EQU	0
MCEREP__SIZE		EQU	2
MCEREP__PAYLOAD		EQU	4
MCEREP___SIZE		EQU	MCEREP__PAYLOAD+128
	
	
;;; 
;;; Buffer for commands from PC
;;;
CMD_BUFFER		DS	(256)


;;; 
;;; Reply/status buffer - this is a structure that is copied into PC RAM
;;;  whenever the DSP needs to provide a command reply or other information.
;;;  This needs to be large enough to hold an MCE reply.
;;;

DG__SIZE		EQU	128+32 ; This MUST be even, so that effective number
				       ; of 32-bit words is integral
	
DG_VERS_CODE		EQU	1  ; Datagram protocol version
DG_TYPE_DSP_REP		EQU     1  ; DSP reply     | Packet type codes
DG_TYPE_MCE_REP		EQU     2  ; MCE reply     |
DG_TYPE_BUF_INF		EQU     3  ; Buffer status |

;;; The actualy buffer storage.
DGRAM_BUFFER		DS	DG__SIZE

;;; Aliases into the structure.
DGRAM_VERSION		EQU	DGRAM_BUFFER+0  ; Datagram protocol version
DGRAM_SIZE		EQU	DGRAM_BUFFER+1  ; Datagram payload size in 32-bit words
DGRAM_TYPE		EQU	DGRAM_BUFFER+2  ; Datagram type
DGRAM_FWREV		EQU	DGRAM_BUFFER+4  ; FW rev.
DGRAM_DATA		EQU	DGRAM_BUFFER+16 ; Start of DSP or MCE reply data
DGRAM__HEADER_SIZE	EQU	(DGRAM_DATA-DGRAM_VERSION) ; Whatever
	
;;; For DSP reply packets:
REP_RSTAT		EQU	DGRAM_DATA+0
REP_RSIZE		EQU	DGRAM_DATA+1
REP_RCMD		EQU	DGRAM_DATA+2
REP_RPAYLOAD		EQU	DGRAM_DATA+4
REP_REND		EQU	REP_RPAYLOAD+16 ; Whatever.
	

;;; Datagram sizes for reply and MCE packets.
;;; Must be even; divide by two to get 32-bit words; mul by two to get bytes.
RB_REP_SIZE		EQU	(REP_REND-DGRAM_DATA+DGRAM__HEADER_SIZE)
RB_MCE_SIZE		EQU	(8+128)
RB_INF_SIZE		EQU	(REP_REND-DGRAM_DATA+2)


; 
; Buffer locations, Y memory.
;
MCECMD_BUF		EQU	$0

MCEREP_BUF		EQU	$100 ; Y-mem location for mce reply buffer?

MCEDATA_BUF		EQU	$200
	
MCE_PACKET_DUMP		EQU	$2000
	
DEBUG_BUF		EQU 	$8000
	
	
;
; Commands for U0107 comms protocol
;

CMD_READ_P		EQU	1
CMD_READ_X		EQU	2
CMD_READ_Y		EQU	3
			
CMD_WRITE_P		EQU	5
CMD_WRITE_X		EQU	6
CMD_WRITE_Y		EQU	7
			
CMD_SET_REP_BUF		EQU	9
CMD_SET_DATA_BUF_MULTI	EQU	$B
	
CMD_SET_TAIL_INF	EQU	$12
			
CMD_SEND_MCE		EQU	$21
CMD_POST_MCE		EQU	$22



	
;----------------------------------------------------------

;;; Bit defines for STATUS word

APPLICATION_RUNNING	EQU	0   ; Indicates application is in progress
SEND_TO_HOST		EQU	1   ; set in HST ISR when host ready for packet (stays set until after HST reply)
FATAL_ERROR		EQU	2   ; PCI message to host error detected by driver....
FO_WRD_RCV		EQU	3   ; set when packet detected in FIFO - stays set till packet processed

HST_NFYD		EQU	9   ; set after host notified (NFY message) of packet (stays set until after HST reply)

CON_DEMAND		EQU	10  ; Host has requested an MCE command be sent
CON_MCE                 EQU     11  ; Command has been copied to Y buffer and should be sent to MCE

PCIDMA_RESTART		EQU	16  ; DMA flags used for error recovery
PCIDMA_RESUME		EQU	17

QT_FLUSH		EQU	20  ; Set when it is time to inform Host of current buffer position.
RP_BUFFER_FULL		EQU	21  ; Set when Quiet RP buffer is occupied.

FREEZER			EQU	22  ; Suspend operations and just idle in the main loop
MAIN_LOOP_POLL          EQU     23  ; Cleared by the main loop, use to check for DSP lock-up


;;; Bit defines for MODE word

MODE_APPLICATION	EQU	0   ; set if PCI application to run
MODE_MCE		EQU	1   ; process packets from MCE (!choke)
MODE_QT			EQU	2   ; Quiet transfer for data packets (QT mode)
MODE_RP_BUFFER		EQU     3   ; Quiet transfer for reply packets (Quiet-RP)


;;; END OF VARIABLE TABLE
	
	IF	@SCP("DOWNLOAD","ROM")	; Boot ROM code
VAR_TBL_END	EQU	@LCV(L)-2
	ENDIF

	IF	@SCP("DOWNLOAD","ONCE")	; Download via ONCE debugger
VAR_TBL_END	EQU	@LCV(L)
	ENDIF

VAR_TBL_LENGTH EQU	VAR_TBL_END-VAR_TBL_START
