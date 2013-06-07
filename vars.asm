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
;; BDEBUG3			DC	0
;; BDEBUG4			DC	0
;; BDEBUG5			DC	0
;; BDEBUG6			DC	0
;; BDEBUG7			DC	0
;; BDEBUG8			DC	0
;; BDEBUG9			DC	0
	
TRIGGER_FAKE		DC	0

FIFO_FAILS		DC	0
PTYPE_FAILS		DC	0
DA_COUNT		DC	0
	
;;; WORD and SIZE must be adjacent like this.
CMD_SIZE		DC	0
CMD_WORD		DC	0

REP_BUS_ADDR		DC	0,0
DATA_BUS_ADDR		DC	0,0
	
;;; Bits in STATUS... watch for conflicts. (4-8, 12-15 are free)
COMM_REP		EQU	4 ; Reply needs to be sent
COMM_CMD		EQU	5 ; Command needs to be processed
COMM_MCEREP		EQU	6 ; MCE reply has been buffered for send to host
COMM_MCEDATA		EQU	7 ; MCE data " "
COMM_ERR		EQU	8 ; Command not recognized or whatever
COMM_REP_ENABLED	EQU	12 ;
COMM_BUF_UPDATE		EQU	13 ; Data has been written to buffer
COMM_TFR_YMEM		EQU	14 ; PCI burst is coming from Y mem, not X mem.

	
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

;; Put this well out of the way...
CMD_BUFFER		EQU	$400


;;; 
;;; Reply/status buffer - this is a structure that is copied into PC RAM
;;;  whenever the DSP needs to provide a command reply or other information
;;;  (e.g. not notify of some event...).
;;; 
;;; Also, make it large enough for MCE replies...

RB_SIZE			EQU	128+32 ; This MUST be even, so that effective number
				       ; of 32-bit words is integral
	
RB_VERSION		EQU	1  ; Version of this datagram
RB_TYPE_DSP_REP		EQU     1  ;
RB_TYPE_MCE_REP		EQU     2  ;
RB_TYPE_BUF_INF		EQU     3  ;

;;; The actualy buffer storage.
REP_BUFFER1		DS	RB_SIZE

;;; Aliases into the structure.
REP_VERSION		EQU	REP_BUFFER1+0  ;
REP_SIZE		EQU	REP_BUFFER1+1  ;
REP_TYPE		EQU	REP_BUFFER1+2  ;
REP_DATA		EQU	REP_BUFFER1+16 ; Start of DSP or MCE reply data
REP_HEADER_SIZE		EQU	(REP_DATA-REP_VERSION) ; Whatever
	
;;; For DSP reply packets:
REP_RSTAT		EQU	REP_DATA+0
REP_RSIZE		EQU	REP_DATA+1
REP_RCMD		EQU	REP_DATA+2
REP_RPAYLOAD		EQU	REP_DATA+4
REP_REND		EQU	REP_RPAYLOAD+16 ; Whatever.
	

MCEREP_BUF		EQU	0 ; Y-mem location for mce reply buffer?
;;; Indices int MCEREP_BUF
MCEREP_PRE0		EQU	0
MCEREP_PRE1		EQU	2
MCEREP_TYPE		EQU	4
MCEREP_SIZE		EQU	6
MCEREP_PAYLOAD		EQU	8
MCEREP_END		EQU	MCEREP_PAYLOAD+128 ; MCE replies are max 256 bytes
	
;; MCEDATA_BUF		EQU	$1000
MCEDATA_BUF		EQU	$8 ;Debugging.
	
;;; Datagram sizes for reply and MCE packets.
;;; Must be even; divide by two to get 32-bit words; mul by two to get bytes.
RB_REP_SIZE		EQU	(REP_REND-REP_DATA+REP_HEADER_SIZE)
RB_MCE_SIZE		EQU	(MCEREP_END+REP_HEADER_SIZE)
RB_INF_SIZE		EQU	(REP_REND-REP_DATA+2)

	
DEBUG_BUF		EQU 	$2000
DEBUG_DUMP		EQU	$2100
INT_DEBUG_BUF		EQU 	$8000


;;; 
;;; Large circular buffer for frame and reply data from MCE.
;;; 
CIRCBUF_HEAD		DC	0 ; Write index
CIRCBUF_TAIL		DC	0 ; Read  index
CIRCBUF_START		EQU	0 ; Buffer start in Y mem.
CIRCBUF_SIZE		EQU	$100000	; 1 million locations = 2 MB
	
	
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
