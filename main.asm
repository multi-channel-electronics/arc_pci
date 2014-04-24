
		COMMENT *

Main section of the pci card code.

See info.asm for versioning and authors.

	*
	PAGE    132     ; Printronix page width - 132 columns
	OPT	CEX	; print DC evaluations


	
;;;
;;; Start main loop
;;;
	
PACKET_IN

	;; Clear I'm-alive bit
	BCLR    #MAIN_LOOP_POLL,X:<STATUS

	;; Are we in state freeze?
	JSET	#FREEZER,X:<STATUS,PACKET_IN
	
	;; Reinitialize if a serious error has been detected
 	JSET	#FATAL_ERROR,X:<STATUS,START 

	;; Jump to special application area if signalled to do so
	JSET	#MODE_APPLICATION,X:<MODE,APPLICATION

	;; Check for timer expiry and branch to the handler
	JSSET	#TCF,X:TCSR0,TIMER_ACTION 

	;; If it's time to signal the PC of buffer state, do so.
	JSSET	#QT_FLUSH,X:STATUS,BUFFER_INFORM
	
	;; Check for data in fibre-optic FIFO
	JSR	<CHECK_FO
 	JSSET	#FO_WRD_RCV,X:STATUS,HANDLE_FIFO

	;; CON only progresses if FIFO isn't hot.
	JSSET   #CON_MCE,X:STATUS,CON_TRANSMIT
	JSSET	#CON_DEMAND,X:STATUS,CON_BUFFER

	;; Enter new comms mode if host signals with HF2
	JSSET	#DSR_HF2,X:DSR,NEW_COMMS_INIT

	;; Hackers, welcome
	NOP
	NOP
	
	;; Loop
	JMP	PACKET_IN

;;; 
;;; End of main loop.
;;; 

; PCI semaphore
;
; In order for routines in non-interrupt context to write to the
; DTXS, (via PCI_MESSAGE_TO_HOST) they need to end up with
; interrupts disabled and HCF3 cleared.
;
; Non-interrupt PCIers should use macro
;	PCI_LOCKDOWN
; to get exclusive access and then release it with
;	PCI_LOCKUP
; after calling PCI_MESSAGE_TO_HOST.

PCI_LOCKDOWN	MACRO
	JSR	PCI_LOCKDOWN_ENTRY
	ENDM

PCI_LOCKUP	MACRO
	BSET	#DCTR_HCIE,X:DCTR       ; Enable host interrupts
	ENDM


PCI_LOCKDOWN_AGAIN
	BSET	#DCTR_HCIE,X:DCTR	; Re-enable host IRQ
	REP	#50			; Delay for ~us
	NOP
	
PCI_LOCKDOWN_ENTRY
	;; Entry
	BCLR	#DCTR_HCIE,X:DCTR	; Disable host IRQ
	JSET	#DCTR_HF3,X:DCTR,PCI_LOCKDOWN_AGAIN
	RTS
	

;;; Fibre data detected; process it and return to main loop.

HANDLE_FIFO
	MOVE	#>$A00,A1
	JSR	TIMER_STORE_A1
	JSR	TIMER_STORE
	
	;; Poll for 8 words -- the preamble and packet size.
	MOVE	#>HEAD_W1_0,R0
	MOVE	#>$00FFFF,X0		; Mask lower 16 bits
	MOVE	R0,A0
	DO	#8,HANDLE_FIFO_CHECK_PREAMBLE
HANDLE_FIFO_WAIT
	JCLR	#EF,X:PDRD,HANDLE_FIFO_WAIT
	NOP
	NOP
	JCLR	#EF,X:PDRD,HANDLE_FIFO_WAIT
	MOVEP	Y:RDFIFO,A
	AND	X0,A
	NOP
	MOVE	A1,X:(R0)+

HANDLE_FIFO_CHECK_PREAMBLE
	MOVE	#>HEAD_W1_0,R0
	CLR	B
	CLR	A
	MOVE	X:(R0)+,B
	CMP	#>$A5A5,B
	JNE	PRE_ERROR
	MOVE	X:(R0)+,B
	CMP	#>$A5A5,B
	JNE	PRE_ERROR
	MOVE	X:(R0)+,B
	CMP	#>$5A5A,B
	JNE	PRE_ERROR
	MOVE	X:(R0)+,B
	CMP	#>$5A5A,B
	JNE	PRE_ERROR

	;; Good enough.  Construct the packet size from words 6 and 7
	MOVE	X:>(HEAD_W1_0+6),A0
	MOVE	X:>(HEAD_W1_0+7),X0
 	INSERT	#$010010,X0,A		; A = size in dwords

	;; Set TOTAL_BUFFS and LEFT_TO_READ using A
	JSR	PACKET_PARTITIONS
	JSR	TIMER_STORE

;;; Case (packet type) of
	MOVE	X:HEAD_W3_0,A

	CMP	#>'RP',A
	JEQ	HANDLE_RP
	
	CMP	#>'DA',A
	JEQ	HANDLE_DA

	JMP	QT_PTYPE_ERROR

; Error recording.

PRE_ERROR	
	MOVE	#>PREAMBLE_ERRORS,R0
	JSR	INCR_X_R0
	JMP	CLEAR_FO_FIFO		; empty the fifo (2 ms!)
	
QT_PTYPE_ERROR
 	MOVE	#>PTYPE_ERRORS,R0
	JMP	INCR_X_R0
QT_FSIZE_ERROR
	MOVE	#>PSIZE_ERRORS,R0
	JMP	INCR_X_R0
RETURN_NOW
	RTS

INCR_X_R0
	;; Increment the X memory varible at R0. Trashes A.
	MOVE	X:(R0),A0
	INC	A
	NOP
	MOVE	A0,X:(R0)
	RTS


		
;;; HANDLE_RP
;;; Handler for MCE reply (RP) packets.
	
HANDLE_RP
	;; Process normally if QUIET_RP not enabled
	JCLR	#MODE_RP_BUFFER,X:MODE,MCE_PACKET

	;; Drop this packet if we're backed up
	JSET	#RP_BUFFER_FULL,X:STATUS,HANDLE_RP_DROP

	;; Copy packet to Y memory, designated area for replies
	MOVE	#>REPLY_BUFFER,R1
	JSR	BUFFER_PACKET

	MOVE	#>$b00,A1
	JSR	TIMER_STORE_A1
	JSR	TIMER_STORE
	
	;; Prepare PCI block transfer
	MOVE	#RP_BASE_LO,R0
	JSR	LOAD_HILO_ADDRESS

	MOVE	#BURST_DEST_LO,R0
	JSR	SAVE_HILO_ADDRESS

	;; Limit to the size of the reply buffer
	CLR	A
	CLR	B
	MOVE	X:PACKET_SIZE,A0
	ASL	#2,A,A			; Size in bytes
	MOVE	X:RP_MAX_SIZE,B0

	CMP	B,A			; A ? B
	JLE	HANDLE_RP1
	MOVE	B,A

HANDLE_RP1
	;; DMA to host
	MOVE	#>REPLY_BUFFER,X0
	MOVE	A0,X:BLOCK_SIZE
	MOVE	X0,X:YMEM_SRC
	JSR	TIMER_STORE
	JSR	BLOCK_TRANSFER
	JSR	TIMER_STORE
	
	;; Prepare notification packet
	PCI_LOCKDOWN			; Disable host IRQ
	MOVE	#'NFY',X0
	MOVE	X0,X:DTXS_WD1
	MOVE	#'RPQ',X0
	MOVE	X0,X:DTXS_WD2
	MOVE	A0,X:DTXS_WD3		; A0=block_size
	MOVE	A1,X:DTXS_WD4		; A1=0

	;; Mark buffer and signal PC
	BSET	#RP_BUFFER_FULL,X:STATUS
	JSR	PCI_MESSAGE_TO_HOST
	PCI_LOCKUP			; Enable host IRQ

	JSR	TIMER_STORE
	RTS				; Back to main loop

HANDLE_RP_DROP
	MOVE	#RP_DROPS,R0
	JSR	INCR_X_R0
	JMP	DROP_PACKET		; Will RTS to main loop
	
;;; HANDLE_RP ends
	
	
;;; HANDLE_DA
;;; Handler for MCE data (DA) frames.


HANDLE_DA
	;; Increment frame count
	MOVE	#FRAME_COUNT,R0
	JSR	INCR_X_R0

	;; If not quiet mode, do normal processing
	JCLR	#MODE_QT,X:MODE,MCE_PACKET

	;; Copy words to Y memory.
	MOVE	#>IMAGE_BUFFER,R1
	JSR	BUFFER_PACKET

	MOVE	#$e00,A1
	JSR	TIMER_STORE_A1
	JSR	TIMER_STORE

	;; Check that the RAM buffer isn't full
	MOVE	X:QT_BUF_HEAD,A
	ADD	#1,A
	MOVE	X:QT_BUF_MAX,B
	CMP	A,B
	JGE	HANDLE_DA_MATH
	MOVE	#0,A
HANDLE_DA_MATH
	MOVE	X:QT_BUF_TAIL,B
	CMP	A,B
	JEQ	HANDLE_DA_DROP

	;; Load packet size
	CLR	A
	MOVE	X:PACKET_SIZE,A0
	
	ADD	#0,B			; Clear carry
	ASL	#2,A,A			; Size, in bytes

	;; Check packet size
	CLR	B
	MOVE	X:QT_FRAME_SIZE,B0
	CMP	A,B
	JNE	QT_FSIZE_ERROR

	;; Prepare burst variables
	MOVE	B0,X:BLOCK_SIZE
	MOVE	B1,X:YMEM_SRC		; Y:0

	MOVE	#QT_DEST_LO,R0
	JSR	LOAD_HILO_ADDRESS
	MOVE	#BURST_DEST_LO,R0
	JSR	SAVE_HILO_ADDRESS

	;; Send
	JSR	BLOCK_TRANSFER

	JSR	TIMER_STORE
	
	;; Next buffer
	JSR	BUFFER_INCR

	;; Check if it's time to inform PC
	JSR	BUFFER_INFORM_CHECK

	RTS

HANDLE_DA_DROP
	;; Full buffer, drop packet.
	MOVE	#QT_DROPS,R0
	JSR	INCR_X_R0
	JMP	DROP_PACKET		; Will RTS to main loop
	
;;; HANDLE_DA ends


;----------------------------------------------
CON_BUFFER
; This routine will copy an MCE command from the PC to Y memory.
; The source RAM address has already been stored in CON_SRC_LO.
; The destination address is always Y:COMMAND_BUFFER.
;----------------------------------------------

	MOVE	#>$C00,A1
	JSR	TIMER_STORE_A1
	JSR	TIMER_STORE
	
	;; PCI burst the command into Y memory
	MOVE	#>CON_SRC_LO,R0
	JSR	LOAD_HILO_ADDRESS
	MOVE	#>BURST_SRC_LO,R0
	JSR	SAVE_HILO_ADDRESS
	MOVE	#>COMMAND_BUFFER,B0
	MOVE	#>256,A0
	MOVE	B0,X:YMEM_DEST
	MOVE	A0,X:BLOCK_SIZE
	JSR	CON_TRANSFER

	BSET	#CON_MCE,X:STATUS
	JSR	TIMER_STORE
	RTS				; Back to main loop
	
;----------------------------------------------
CON_TRANSMIT
; This routine will copy the MCE command from Y:COMMAND_BUFFER to 
; the MCE command transmitter.
;----------------------------------------------
	
	JSR	TIMER_STORE
	
	MOVE	#>COMMAND_BUFFER,R6
	DO	#128,CON_TRANSMIT1	; block size = 16bit x 128 (256 bytes)
	MOVE	Y:(R6)+,A1		; b2, b1  (lsb)
	ASR	#8,A,B		        ; Shift b2 into B1
	AND	#>$FF,A
	MOVE	A1,X:FO_SEND
	MOVE	B1,X:FO_SEND

CON_TRANSMIT1
	BSET	#MODE_MCE,X:<MODE	; enable processing of MCE replies/data

	;; CON processed, clear the state bits.
	BCLR	#CON_MCE,X:STATUS
	BCLR	#CON_DEMAND,X:STATUS

	JSR	TIMER_STORE
	
	;; Reply to the CON command
	PCI_LOCKDOWN
	MOVE	#'CON',X0
	JSR	VCOM_PREPARE_REPLY
	JSR	PCI_MESSAGE_TO_HOST
	PCI_LOCKUP			; Enable host IRQ

	JSR	TIMER_STORE
	RTS				; Back to main loop


	
		
;;; Old MCE packet handling code, ported forward a bit.
		
; --------------------------------------------------------------------------
; --------------------- MAIN PACKET HANDLING CODE --------------------------
; --------------------------------------------------------------------------

; prepare notify to inform host that a packet has arrived.

MCE_PACKET
	PCI_LOCKDOWN			; Disable host IRQ
	BCLR	#HST_NFYD,X:<STATUS	; clear flag to indicate host has been notified.

	MOVE	#'NFY',X0		; initialise communication to host as a notify
	MOVE	X0,X:<DTXS_WD1		; 1st word transmitted to host in notify message

	MOVE	X:<HEAD_W3_0,X0		;RP or DA - top two bytes of word 3 ($2020) not passed to driver.
	MOVE	X0,X:<DTXS_WD2		;2nd word transmitted to host in notify message

	MOVE	X:<HEAD_W4_0,X0		; size of packet LSB 16bits (# 32bit words)
	MOVE	X0,X:<DTXS_WD3		; 3rd word transmitted to host in notify message

	MOVE	X:<HEAD_W4_1,X0		; size of packet MSB 16bits (# of 32bit words)
	MOVE	X0,X:<DTXS_WD4		; 4th word transmitted to host in notify message

	;; notify the host that there is a packet
	BCLR	#SEND_TO_HOST,X:<STATUS		; clear send to host flag
	JSR	<PCI_MESSAGE_TO_HOST		; notify host of packet	
	BSET	#HST_NFYD,X:<STATUS		; flag to indicate host has been notified.
	PCI_LOCKUP

	MOVE	#>IMAGE_BUFFER,R1
	JSR	BUFFER_PACKET

	;; Wait for HST command (and destination RAM address)

WT_HOST	JSET	#FATAL_ERROR,X:<STATUS,START		; on fatal error, re-init.
	JCLR	#SEND_TO_HOST,X:<STATUS,WT_HOST		; Set in 'send_packet_to_host' ISR

	;; All data is buffered and destination address is in BURST_ADDR_HI/LO.
	MOVE	#>IMAGE_BUFFER,X0
	MOVE	X:PACKET_SIZE,A
	ASL	#2,A,A
	MOVE	X0,X:YMEM_SRC
	MOVE	A1,X:BLOCK_SIZE
	JSR	BLOCK_TRANSFER

	JSET	#FATAL_ERROR,X:<STATUS,START

	;; Reply to the HST command
	PCI_LOCKDOWN			; Disable host IRQ
	MOVE	#'HST',X0
	JSR	VCOM_PREPARE_REPLY
	JSR	PCI_MESSAGE_TO_HOST
	PCI_LOCKUP			; Enable host IRQ
	RTS

;----------------------------------------------------------
; clear out the fifo after an HST timeout...
;----------------------------------------------------------

DUMP_FIFO
	MOVE	#DUMP_BUFF,R1		; address where dumped words stored in Y mem
	MOVE	#MAX_DUMP,X0		; put a limit to number of words read from fifo
	CLR	A
	MOVE	#0,R2			; use R2 as a dump count
NEXT_DUMP
	JCLR	#EF,X:PDRD,FIFO_EMPTY
	NOP
	NOP
	JCLR	#EF,X:PDRD,FIFO_EMPTY
	
	MOVEP	Y:RDFIFO,Y:(R1)+	; dump word to Y mem.
	MOVE	(R2)+			; inc dump count
	MOVE	R2,A			; 	
	CMP	X0,A			; check we've not hit dump limit
	JNE	NEXT_DUMP		; not hit limit?
FIFO_EMPTY
	MOVE	R2,X:NUM_DUMPED		; store number of words dumped after HST timeout.
	JMP	<START			; re-initialise


; -------------------------------------------------------------------------------------
;                              END OF MAIN PACKET HANDLING CODE
; -------------------------------------------------------------------------------------



; -------------------------------------------------------------------------------------
;
;                              INTERRUPT SERVICE ROUTINES 
;
; -------------------------------------------------------------------------------------

; ---------------
; Rules:  Don't use N#, or any R# except R0 unless you add them to the saved register set.
	

; ----------------------------------------------------------------------------
; VCOM_* - routines: utility functions for hosty command vector communication.
;-----------------------------------------------------------------------------


; VCOM_PREPARE_REPLY
;
; Prepare the reply packet, using X0 as the command name (second word).  The
; message defaults to 'ACK' with NULL data.  The user may subsequenty fill in
; the data field (word 4) and mark the packet as error if necessary.
	
VCOM_PREPARE_REPLY
	;; Prepare the reply packet for command in X0.
	;; A0 is trashed, X0 is preserved.
	MOVE	#'REP',A0
	MOVE	X0,X:DTXS_WD2		; Command
	MOVE	A0,X:DTXS_WD1

	MOVE	#'ACK',A		; Note this sets A0 = 0
	NOP
	MOVE	A1,X:DTXS_WD3		; ACK
	MOVE	A0,X:DTXS_WD4		; no comment
	RTS


; VCOM_CHECK
;
; Compares DRXR_WD1 to X0.  If they are equal, Z is set on return.  If they
; are not equal then Z is cleared and the reply will be marked as ERR with
; 'CNE' in the last word.
; Trashes A and B always and X0 on error.

VCOM_CHECK
	MOVE	X0,A
	MOVE	X:DRXR_WD1,B
	CMP	A,B
	JEQ	VCOM_RTS

	MOVE	#'CNE',X0	; Command Name Error
	MOVE	#'ERR',A0
	MOVE	X0,X:DTXS_WD4
	MOVE	A0,X:DTXS_WD3
VCOM_RTS
	RTS


; VCOM_INTRO
;
; Read DSP command from DRXR.  Prepare the reply packet and verify that it
; matches the key in X1.  If it does not, mark the reply as error and set
; the Z flag.
	
VCOM_INTRO
	JSR	RD_DRXR			; Loads DRXR_WD*
	MOVE	X1,X0
	JSR	VCOM_PREPARE_REPLY
	JSR	VCOM_CHECK
	RTS

	
; VCOM_EXIT_ERROR_X0
; VCOM_EXIT_X0
; VCOM_EXIT
;
; For returning from host command vector interrupts only.  These three
; routines do the following (respectively):
; a) Mark reply as error, then (b)
; b) Put X0 into last word of reply, then (c)
; c) Restore registers and RTI.

VCOM_EXIT_ERROR_X0
	MOVE	#'ERR',A0
 	NOP
	MOVE	A0,X:DTXS_WD3
VCOM_EXIT_X0	
	MOVE	X0,X:DTXS_WD4
VCOM_EXIT
	JSR	RESTORE_REGISTERS
	JSR	PCI_MESSAGE_TO_HOST
	RTI
	

;---------------------------------------------------------------
RD_DRXR
;--------------------------------------------------------------
; Routine to read from HTXR-DRXR data path.  For HCTR = 0x900,
; 3 LSB of each 32-bit word written by the host is returned on
; each read.  This only polls for first word, not all of them.
	JCLR	#SRRQ,X:DSR,*		; Wait for receiver to be not empty
	MOVE	#DRXR_WD1,R3
	REP	#4
	MOVEP	X:DRXR,X:(R3)+
	RTS

	
; ----------------------------------------------------------------------------
READ_MEMORY
;-----------------------------------------------------------------------------
;Read command: 
; word 1 = command = 'RDM'
; word 2 = memory type, P=$00'_P', X=$00_'X' or Y=$00_'Y'
; word 3 = address in memory
; word 4 = not used
;Version query:
; word 1 = 'VER'
; word 2-4 unused
	
	JSR	SAVE_REGISTERS
	JSR	RD_DRXR			; Loads DRXR_WD*

	MOVE	#'RDM',X0
	JSR	VCOM_PREPARE_REPLY
	JSR	VCOM_CHECK
	JEQ	READ_MEMORY_XYP

	;; Version read command!
	MOVE	#'VER',X0
	JSR	VCOM_PREPARE_REPLY
	JSR	VCOM_CHECK
	JNE	VCOM_EXIT

	MOVE	X:REV_NUMBER,X0
	JMP	VCOM_EXIT_X0

READ_MEMORY_XYP
		
	;; Args: mem, address, <unused>
	MOVE	X:DRXR_WD2,A
	MOVE	X:DRXR_WD3,R0
	
	CMP	#'_X',A
	JEQ	READ_MEMORY_X

	CMP	#'_Y',A
	JEQ	READ_MEMORY_Y

	CMP	#'_P',A
	JEQ	READ_MEMORY_P

	MOVE	#'MTE',X0
	JMP	VCOM_EXIT_ERROR_X0

READ_MEMORY_X
	MOVE	X:(R0),X0
	JMP	VCOM_EXIT_X0
READ_MEMORY_Y
	MOVE	Y:(R0),X0
	JMP	VCOM_EXIT_X0
READ_MEMORY_P
	MOVE	P:(R0),X0
	JMP	VCOM_EXIT_X0
	

;--------------------------------------------------------------	
WRITE_MEMORY
;---------------------------------------------------------------
; word 1 = command = 'WRM'
; word 2 = memory type, P=$00'_P', X=$00'_X' or Y=$00'_Y'
; word 3 = address in memory
; word 4 = value 

	JSR	SAVE_REGISTERS
	MOVE	#'WRM',X1
	JSR	VCOM_INTRO
	JNE	VCOM_EXIT

	;; Args: mem, address, data
	MOVE	X:DRXR_WD2,A
	MOVE	X:DRXR_WD3,R0
	MOVE	X:DRXR_WD4,X0
	
	CMP	#'_X',A
	JEQ	WRITE_MEMORY_X

	CMP	#'_Y',A
	JEQ	WRITE_MEMORY_Y

	CMP	#'_P',A
	JEQ	WRITE_MEMORY_P

	MOVE	#'MTE',X0
	JMP	VCOM_EXIT_ERROR_X0

WRITE_MEMORY_X
	MOVE	X0,X:(R0)
	JMP	VCOM_EXIT_X0
WRITE_MEMORY_Y
	MOVE	X0,Y:(R0)
	JMP	VCOM_EXIT_X0
WRITE_MEMORY_P
	MOVE	X0,P:(R0)
	JMP	VCOM_EXIT_X0


;-----------------------------------------------------------------------------
START_APPLICATION
; an application should already have been downloaded to the PCI memory.
; this command will execute it.
; ----------------------------------------------------------------------
; word 1 = command = 'GOA'
; word 2-4 unused
	
	JSR	SAVE_REGISTERS
	MOVE	#'GOA',X1

	JSR	VCOM_INTRO
	JNE	VCOM_EXIT

	BSET	#MODE_APPLICATION,X:MODE
	RTI			; Application will reply.

	
; ---------------------------------------------------------
STOP_APPLICATION
; this command stops an application that is currently running
; used for applications that once started run contiunually
;-----------------------------------------------------------
; word 1 = command = ' STP'
; word 2-4 unused

	JSR	SAVE_REGISTERS
	MOVE	#'STP',X1

	JSR	VCOM_INTRO
	JNE	VCOM_EXIT

	BCLR	#MODE_APPLICATION,X:MODE
	BCLR	#APPLICATION_RUNNING,X:STATUS
	JMP	VCOM_EXIT

	
;-----------------------------------------------------------------------------
RESET_CONTROLLER
; Reset the controller by sending a special code byte $0B with SC/nData = 1
;-----------------------------------------------------------------------------
; word 1 = command = 'RCO'
; word 2-4 unused
	
	JSR	SAVE_REGISTERS
	MOVE	#'RCO',X1
	JSR	VCOM_INTRO
	JNE	VCOM_EXIT

	BSET	#SCLK,X:PDRE		; Enable special command mode
	NOP
	NOP
	MOVE	#$FFF000,R0		; Memory mapped address of transmitter
	MOVE	#$10000B,X0		; Special command to reset controller
	MOVE	X0,X:(R0)
	REP	#6			; Wait for transmission to complete
	NOP
	BCLR	#SCLK,X:PDRE		; Disable special command mode

; Wait for a bit for MCE to be reset.......
	MOVE	#10000,X0		; Delay by about 350 milliseconds
	DO	X0,L_DELAY
	DO	#1000,L_RDFIFO
	MOVEP	Y:RDFIFO,Y0		; Read the FIFO word to keep the
	NOP				;   receiver empty
L_RDFIFO
	NOP
L_DELAY
	NOP	
	
	MOVE	#'000',X0
	JMP	VCOM_EXIT_X0
	
;-----------------------------------------------------------------------------
QUIET_TRANSFER_SET
;-----------------------------------------------------------------------------
;Quiet transfer mode configuration
; word 1 = command = 'QTS'
; word 2 = parameter to set
; word 3-4 = arguments

	JSR	SAVE_REGISTERS		; standard opening
	MOVE	#'QTS',X1
	JSR	VCOM_INTRO
	JNE	VCOM_EXIT

	MOVE	#BDEBUG0,R0
	JSR	INCR_X_R0
	
	MOVE	X:DRXR_WD2,A		; Parameter id
	MOVE	X:DRXR_WD3,X0		; First arg
	MOVE	X:DRXR_WD4,X1		; Second arg

	CMP	#'BAS',A
	JEQ	QUIET_TRANSFER_SET_BASE

	CMP	#'DEL',A
	MOVE	#QT_BUF_SIZE,R0
	JEQ	QUIET_TRANSFER_SET_R0

	CMP	#'NUM',A
	MOVE	#QT_BUF_MAX,R0
	JEQ	QUIET_TRANSFER_SET_R0

	CMP	#'INF',A
	MOVE	#QT_INFORM,R0	
	JEQ	QUIET_TRANSFER_SET_R0

	CMP	#'SIZ',A
	MOVE	#QT_FRAME_SIZE,R0	
	JEQ	QUIET_TRANSFER_SET_R0

	CMP	#'TAI',A
	MOVE	#QT_BUF_TAIL,R0
	JEQ	QUIET_TRANSFER_SET_R0

	CMP	#'HEA',A
	MOVE	#QT_BUF_HEAD,R0
	JEQ	QUIET_TRANSFER_SET_R0

	CMP	#'DRO',A
	MOVE	#QT_DROPS,R0
	JEQ	QUIET_TRANSFER_SET_R0

	CMP	#'PER',A
	MOVE	#TCPR0,R0
	JEQ	QUIET_TRANSFER_SET_R0

	CMP	#'FLU',A
	JEQ	QUIET_TRANSFER_SET_FLUSH

	CMP	#'SET',A
	JEQ	QUIET_TRANSFER_SET_ENABLED

	CMP	#'RPS',A
	MOVE	#RP_MAX_SIZE,R0
	JEQ	QUIET_TRANSFER_SET_R0

	CMP	#'RPB',A
	JEQ	QUIET_TRANSFER_SET_RP_BASE
	
	CMP	#'RPE',A
	JEQ	QUIET_TRANSFER_SET_RP_ENABLED
	
	CMP	#'BUR',A
	MOVE	#PCI_BURST_SIZE,R0
	JEQ	QUIET_TRANSFER_SET_R0_PERSISTENT

	MOVE	#'MTE',X0
	JMP	VCOM_EXIT_ERROR_X0
	
QUIET_TRANSFER_SET_RP_BASE
	MOVE	X0,X:RP_BASE_LO
	MOVE	X1,X:RP_BASE_HI
	JMP	VCOM_EXIT
	
QUIET_TRANSFER_SET_RP_ENABLED
	BCLR	#MODE_RP_BUFFER,X:MODE
	MOVE	X0,A
	TST	A
	JEQ	VCOM_EXIT
	BSET	#MODE_RP_BUFFER,X:MODE
	BCLR	#RP_BUFFER_FULL,X:STATUS
	JMP	VCOM_EXIT

QUIET_TRANSFER_SET_FLUSH
	BCLR	#QT_FLUSH,X:STATUS
	MOVE	X0,A
	TST	A
	JEQ	VCOM_EXIT
	BSET	#QT_FLUSH,X:STATUS
	JMP	VCOM_EXIT

QUIET_TRANSFER_SET_ENABLED
	MOVE	X0,A
	TST	A
	JEQ	QUIET_TRANSFER_SET_DISABLED
	BSET	#MODE_QT,X:MODE
	JSR	TIMER_ENABLE
	JMP	VCOM_EXIT

QUIET_TRANSFER_SET_DISABLED
	BCLR	#MODE_QT,X:MODE
	JSR	TIMER_DEFAULT
	JMP	VCOM_EXIT
	
QUIET_TRANSFER_SET_R0
	MOVE	X0,X:(R0)
	JMP	VCOM_EXIT

QUIET_TRANSFER_SET_R0_PERSISTENT
	;; Sets X:(R0) with value in X0, but also updates vartable in P memory
	;; so that new value can survive a dsp_reset.
	;; Destroys A and B, but that's ok.
	MOVE	X0,X:(R0)
	MOVE	#>VAR_TBL_START,B
	MOVE	R0,A
	ADD	A,B
	NOP
	MOVE    B,R0
	NOP
	NOP
	NOP
	MOVE	X0,P:(R0)
	JMP	VCOM_EXIT

QUIET_TRANSFER_SET_BASE
	MOVE	X0,X:QT_BASE_LO
	MOVE	X1,X:QT_BASE_HI

	JSR	BUFFER_RESET

	JMP	VCOM_EXIT


;-----------------------------------------------------------------------------
SYSTEM_RESET
;-----------------------------------------------------------------------------
	
	MOVEC	#1,SP			; Point stack pointer to the top	
	MOVEC	#$000200,SSL		; SSL holds SR return state
					; set to zero except for interrupts
	MOVEC	#0,SP			; Writing to SSH preincrements the SP
					; so first set to 0
	MOVEC	#START,SSH		; SSH holds return address of PC
					; therefore,return to initialization
	NOP
	RTI				; return from ISR - to START


; ------------------------------------------------------------------------------------
SEND_PACKET_TO_HOST
; this command is received from the Host and actions the PCI board to pick up an address
; pointer from DRXR which the PCI board then uses to write packets from the 
; MCE to the host memory starting at the address given.
; Since this is interrupt driven all this piece of code does is get the address pointer from
; the host via DRXR, set a flag so that the main prog can write the packet.  Replies to  
; HST after packet sent (unless error).
; --------------------------------------------------------------------------------------
; word 1 = command = 'HST'
; word 2 = host high address
; word 3 = host low address
; word 4 = not used but read

	JSR	<SAVE_REGISTERS		; save working registers
	MOVE	#'HST',X1
	JSR	VCOM_INTRO
	JNE	VCOM_EXIT

	;; Args are hi/lo PCI dest'n address
	MOVE	X:<DRXR_WD2,X0		; high 16 bits of address 
	MOVE	X:<DRXR_WD3,B0		; low 16 bits of adderss

	MOVE	X0,X:BURST_DEST_HI
	MOVE	B0,X:BURST_DEST_LO

	BSET	#SEND_TO_HOST,X:<STATUS	 ; tell main program to write packet to host memory

	JSR	RESTORE_REGISTERS
	RTI			; Main loop will reply after packet transfer!

	
; --------------------------------------------------------------------
SOFTWARE_RESET
;----------------------------------------------------------------------
; word 1 = command = 'RST'
; word 2-4 unused

	JSR	SAVE_REGISTERS
	MOVE	#'RST',X1
	JSR	VCOM_INTRO
	JNE	VCOM_EXIT

; RST command OK so reply to host
FINISH_RST
	MOVE	#'000',X0
	MOVE	X0,X:DTXS_WD4
	JSR	PCI_MESSAGE_TO_HOST

	JSET	#DCTR_HF3,X:DCTR,*
	
	BCLR	#MODE_APPLICATION,X:<MODE	; clear app flag
	BCLR	#APPLICATION_RUNNING,X:<STATUS  ; clear appl running bit.

	JMP	SYSTEM_RESET	        ; Handle the stack and stuff...


SEND_PACKET_TO_CONTROLLER

; 	Host command identifying location of an MCE command to send to
; 	the MCE.  Since this can come at any time, just record the
; 	request and then do the CONning from the main loop.

; word 1 = command = 'CON'
; word 2 = source host bus address, bits 31:16
; word 3 = source host bus address, bits 15:0
; word 4 = '0' --> when MCE command is RS,WB,RB,ST
;	 = '1' --> when MCE command is GO  

	JSR	<SAVE_REGISTERS		; save working registers

	;; Read command and verify it's a CON...
	MOVE	#'CON',X1
	JSR	VCOM_INTRO
	JNE	VCOM_EXIT

	;; If we have an outstanding CON, return an error.
	MOVE	#'BUS',X0
	JSET	#CON_DEMAND,X:STATUS,VCOM_EXIT_ERROR_X0

	;; Set up the transfer (to be initiated from the main loop)
	BSET	#CON_DEMAND,X:STATUS
	MOVE	X:<DRXR_WD2,X0
	MOVE	X:<DRXR_WD3,X1
	MOVE	X0,X:CON_SRC_HI
	MOVE	X1,X:CON_SRC_LO

; 	;; Fourth word indicates if this is a go.  Who cares?
; 	MOVE	X:<DRXR_WD4,A		; read word 4 - GO command?
; 	MOVE	#0,X0
; 	CMP	X0,A
; 	JEQ	BLOCK_CON

	;; No reply, leave it till after the CON routine.
	JSR	RESTORE_REGISTERS
	RTI

;;; End transfer-to-controller interrupt.

	
;---------------------------------------------------------------
;
;                          * END OF ISRs *
;
;--------------------------------------------------------------



;----------------------------------------------------------------
;
;                     * Beginning of SUBROUTINES *
;
;-----------------------------------------------------------------


CHECK_FO
	JCLR	#EF,X:PDRD,CHECK_FO_CLEAR
	NOP
	NOP
	JCLR	#EF,X:PDRD,CHECK_FO_CLEAR
	BSET	#FO_WRD_RCV,X:<STATUS
	RTS
	
CHECK_FO_CLEAR
	BCLR	#FO_WRD_RCV,X:<STATUS
	RTS


	
;----------------------------------------------------------------------------
PCI_MESSAGE_TO_HOST
;----------------------------------------------------------------------------
; Subroutine to send 4 words as a reply from PCI to the Host
; using the DTXS-HRXS data path.  The DSP signals the host by raising
; HF3 and (when !MODE_NOIRQ) INTA.
;
; When MODE_HANDSHAKE, the DSP and Host interact as follows:
; - to show that the Host is handling the interrupt, Host raises HF0
; - when DSP sees HF0 go high, it lowers INTA and HF3
; - when Host is done handling the interrupt (i.e. it has read the reply),
;   and when HF3 is low, Host lowers HF0.
; - when DSP sees HF0 go low, the routine finishes.
;
; The primary advantage of this hand-shaking scheme is that host vector
; commands are not needed to clear HF3 and INTA.
;
; This routine should not block for anything other than the Host handshake.

	MOVE	#>DTXS_WD1,R0

	DO	#4,PCI_MESSAGE_TO_HOST_10
	JCLR	#STRQ,X:DSR,*		; wait to be not full
	MOVEP	X:(R0)+,X:DTXS

PCI_MESSAGE_TO_HOST_10
	MOVE	X:SV_X0,X0		; restore X0
	MOVE	X:SV_R0,R0		; restore R0
	BSET	#DCTR_HF3,X:DCTR	; Raise HF3 (handshake)

	; No more NO_IRQ mode... we need DSR_HF2 for master-only mode
	;; Only interrupt in irq mode
	;; JSET	#DSR_HF2,X:DSR,PCI_MESSAGE_TO_HOST_20
	NOP
	NOP
	BSET	#INTA,X:DCTR		; Assert the interrupt
	
PCI_MESSAGE_TO_HOST_20
	JSET	#DSR_HF1,X:DSR,PCI_MESSAGE_TO_HOST_HANDSHAKE
	RTS

PCI_MESSAGE_TO_HOST_HANDSHAKE
	JCLR	#DSR_HF0,X:DSR,*	; Wait for host to ack
	BCLR	#INTA,X:DCTR		; Clear interrupt
	BCLR	#DCTR_HF3,X:DCTR	; Clear hand-shake bit
	JSET	#DSR_HF0,X:DSR,*	; Wait for host to ack
	RTS


;------------------------------------------------------------------------------------
RESTORE_REGISTERS
;-------------------------------------------------------------------------------------

	MOVEC	X:<SV_SR,SR	

	MOVE	X:<SV_A0,A0		
	MOVE	X:<SV_A1,A1
	MOVE	X:<SV_A2,A2

	MOVE	X:<SV_B0,B0		
	MOVE	X:<SV_B1,B1
	MOVE	X:<SV_B2,B2

	MOVE	X:<SV_X0,X0	
	MOVE	X:<SV_X1,X1

	MOVE	X:<SV_Y0,Y0
	MOVE	X:<SV_Y1,Y1

	MOVE	X:<SV_R0,R0
	RTS

;-------------------------------------------------------------------------------------
SAVE_REGISTERS
;-------------------------------------------------------------------------------------

	MOVEC	SR,X:<SV_SR		; save status register.  May jump to ISR during CMP
	
	MOVE	A0,X:<SV_A0		
	MOVE	A1,X:<SV_A1
	MOVE	A2,X:<SV_A2

	MOVE	B0,X:<SV_B0		
	MOVE	B1,X:<SV_B1
	MOVE	B2,X:<SV_B2

	MOVE	X0,X:<SV_X0	
	MOVE	X1,X:<SV_X1

	MOVE	Y0,X:<SV_Y0
	MOVE	Y1,X:<SV_Y1

	MOVE	R0,X:<SV_R0
	RTS

	
;----------------------------------------------
FLUSH_PCI_FIFO
;----------------------------------------------
	JCLR	#MARQ,X:DPSR,*
	BSET	#CLRT,X:DPCR
 	NOP
	JSET	#CLRT,X:DPCR,*
	RTS	
	
;----------------------------------------------
CLEAR_FO_FIFO			
;----------------------------------------------
	MOVEP	#%011000,X:PDRD			; clear FIFO RESET* for 2 ms
	MOVE	#200000,X0
	DO	X0,*+3
	NOP
	MOVEP	#%011100,X:PDRD
	RTS


;---------------------------------------------------------
; PCI burst routines
; 
; For transfer between Host memory and DSP Y memory.
;
; Major entry points are
; 	CON_TRANSFER (PC -> DSP)
; 	BLOCK_TRANSFER (DSP -> PC)
;---------------------------------------------------------

;---------------------------------------------------------
PCI_ERROR_CLEAR
;-----------------------------------------------
	;; Increments a counter associated with each kind of error and sets
	;;  either STATUS[PCIDMA_RESTART] or STATUS[PCIDMA_RESUME] bit
	;;  associated with each error.  Trashes A.
	;; Stop:	MAB/TAB
	;; Restart:	TRTY
	;; Resume:	TDIS/TO

	MOVE	X:DMA_ERRORS,A0
	INC	A
	NOP
	MOVE	A0,X:DMA_ERRORS
		
	JSET	#TRTY,X:DPSR,ERROR_TRTY
	JSET	  #TO,X:DPSR,ERROR_TO
	JSET	#TDIS,X:DPSR,ERROR_TDIS
	JSET	 #TAB,X:DPSR,ERROR_TAB
	JSET	 #MAB,X:DPSR,ERROR_MAB
	JSET	#DPER,X:DPSR,ERROR_DPER
	JSET	#APER,X:DPSR,ERROR_APER
	
ERROR_TRTY
	MOVE	X:EC_TRTY,A0
	INC	A
	MOVEP	#>$0400,X:DPSR		; Clear target retry error bit
	MOVE	A0,X:EC_TRTY
	BSET	#PCIDMA_RESTART,X:STATUS
	RTS
ERROR_TO
	MOVE	X:EC_TO,A0
	INC	A
	MOVEP	#>$0800,X:DPSR		; Clear timeout error bit
	MOVE	A0,X:EC_TO
	BSET	#PCIDMA_RESUME,X:STATUS
	RTS
ERROR_TDIS
	MOVE	X:EC_TDIS,A0
	INC	A
	MOVEP	#>$0200,X:DPSR		; Clear target disconnect bit
	MOVE	A0,X:EC_TDIS
	BSET	#PCIDMA_RESUME,X:STATUS
	RTS
ERROR_TAB
	MOVE	X:EC_TAB,A0
	INC	A
	MOVEP	#>$0100,X:DPSR		; Clear target abort error bit
	MOVE	A0,X:EC_TAB
	BSET	#PCIDMA_RESTART,X:STATUS
	RTS
ERROR_MAB
	MOVE	X:EC_MAB,A0
	INC	A
	MOVEP	#>$0080,X:DPSR		; Clear master abort error bit
	MOVE	A0,X:EC_MAB
	BSET	#PCIDMA_RESTART,X:STATUS
	RTS
ERROR_DPER
	MOVE	X:EC_DPER,A0
	INC	A
	MOVEP	#>$0040,X:DPSR		; Clear data parity error bit
	MOVE	A0,X:EC_DPER
	BSET	#PCIDMA_RESTART,X:STATUS
	RTS
ERROR_APER
	MOVE	X:EC_APER,A0
	INC	A
	MOVEP	#>$0020,X:DPSR		; Clear address parity error bit
	MOVE	A0,X:EC_APER
	BSET	#PCIDMA_RESTART,X:STATUS
	RTS


	
;----------------------------------------------
BLOCK_TRANSFER
;----------------------------------------------
;   In:
;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
;   - BLOCK_SIZE is packet size, in bytes
;   - YMEM_SRC is start of data in Y memory
;  Out:
;   - BLOCK_SIZE will be decremented to zero.
;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
;   - YMEM_SRC will be incremented by BLOCK_SIZE/2
;  Trashes:
;   - A and B at least
	
	MOVE	X:BLOCK_SIZE,A	        ; A1 = BLOCK_SIZE
	CMP	#0,A			; Still bytes to transfer?
	JNE	BLOCK_TRANSFER0
	RTS

BLOCK_TRANSFER0
	;; Maximum size of a DMA/PCI burst is 256 bytes,
	;; but latency clock determines the ideal value.
	MOVE	X:PCI_BURST_SIZE,B	; B1 = burst size (256)

	CMP	B,A			; A ? B
	JGE	<BLOCK_TRANSFER1	; jump if A >= B
	MOVE	A,B			; This only moves A1,B1.
BLOCK_TRANSFER1
	;; Now burst size B <= block size A.
	SUB	B,A			; A -= B
	ADD	#0,B			; Clear carry bit
	MOVE	A,X:BLOCK_SIZE		; Updated BLOCK_SIZE
	MOVE	B,X:BURST_SIZE		; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100))
	ASR	#25,B,B			; B0 = # of 16 bit words

	;; Setup DMA from BURST_SRC to PCI tx
	MOVEP	#DTXM,X:DDR0		; DMA dest'n
	MOVE	X:YMEM_SRC,A0
	MOVEP	A0,X:DSR0		; DMA source
	ADD	B,A
	DEC	B
	MOVE	A0,X:YMEM_SRC		; BURST_SRC += BURST_SIZE/2
	
	MOVEP	B0,X:DCO0		; DMA length = BURST_SIZE/2 - 1

	;; DMA go
	MOVEP	#$8EFA51,X:DCR0

BLOCK_TRANSFER_PCI
	MOVE	#>$7,X0			; Memory write
	MOVE	#BURST_DEST_LO,R0	; RAM address
	JSR	PCI_GO			; Initiate PCI burst

	;; Wait for completion
	JCLR	#MARQ,X:DPSR,*

	;; Check for errors:
	JCLR	#MDT,X:DPSR,BLOCK_TRANSFER_HANDLE_ERRORS
	
	CLR	B
	MOVE	X:BURST_SIZE,B0		; All bytes were transferred
	JSR	ADD_HILO_ADDRESS	; Update source address
	JMP	BLOCK_TRANSFER		; Next burst in block

BLOCK_TRANSFER_HANDLE_ERRORS
	;; Set PCIDMA_* flags; trashes A only	
	JSR	PCI_ERROR_CLEAR
	
	BCLR	#PCIDMA_RESTART,X:STATUS ; Test and clear
	JCS	BLOCK_TRANSFER_PCI	; Restart PCI burst

	BCLR	#PCIDMA_RESUME,X:STATUS	; Test and clear
	JCC	BLOCK_TRANSFER		; Error but no error? Redo this burst.

	;; Update the PCI burst size and burst again.
	JSR	PCI_RECOVER_COUNT	; Get transferred byte count in A.
	JSR	PCI_UPDATE_R0
	JMP	BLOCK_TRANSFER_PCI


;----------------------------------------------
CON_TRANSFER
;----------------------------------------------
;   In:
;   - BURST_SRC_HI:BURST_SRC_LO is PC RAM address
;   - BLOCK_SIZE is packet size, in bytes
;   - YMEM_DEST is start of data in Y memory
;  Out:
;   - BLOCK_SIZE will be decremented to zero.
;   - BURST_SRC_HI:LO will be incremented by BLOCK_SIZE
;   - YMEM_DEST will be incremented by BLOCK_SIZE/2
;  Trashes:
;   - A and B, R0, X0
	
	MOVE	X:BLOCK_SIZE,A	        ; A1 = BLOCK_SIZE
	CMP	#0,A			; Still bytes to transfer?
	JNE	CON_TRANSFER0
	RTS

CON_TRANSFER0
	;; Maximum size of a DMA/PCI burst is 256 bytes,
	;; but latency clock determines the ideal value.
	MOVE	X:PCI_BURST_SIZE,B	; B1 = burst size (256)

	CMP	B,A			; A ? B
	JGE	<CON_TRANSFER1		; jump if A >= B
	MOVE	A,B			; This only moves A1,B1.
CON_TRANSFER1
	;; Now burst size B <= block size A.
	SUB	B,A			; A -= B
	ADD	#0,B			; Clear carry bit
	MOVE	A,X:BLOCK_SIZE		; Updated BLOCK_SIZE
	MOVE	B,X:BURST_SIZE		; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100))
	ASR	#25,B,B			; B0 = # of 16 bit words

	;; Setup DMA from BURST_SRC to PCI tx
	MOVE	X:YMEM_DEST,A0
	MOVE	A0,X:DDR0		; DMA dest'n
	MOVEP	#>DRXR,X:DSR0		; DMA source
	ADD	B,A
	DEC	B
	MOVE	A0,X:YMEM_DEST		; YMEM_DEST += BURST_SIZE/2
	
	MOVEP	B0,X:DCO0		; DMA length = BURST_SIZE/2 - 1

 	;; DMA go
 	MOVEP	#$8EEAC4,X:DCR0

CON_TRANSFER_PCI
	MOVE	#>$6,X0			; Memory write
	MOVE	#BURST_SRC_LO,R0	; RAM address
	JSR	PCI_GO			; Initiate PCI burst

	;; Wait for completion
	JCLR	#MARQ,X:DPSR,*

	;; Check for errors:
	JCLR	#MDT,X:DPSR,CON_TRANSFER_HANDLE_ERRORS
	
	CLR	B
	MOVE	X:BURST_SIZE,B0		; All bytes were transferred
	JSR	ADD_HILO_ADDRESS	; Update source address
	JMP	CON_TRANSFER		; Next burst in block

CON_TRANSFER_HANDLE_ERRORS
	;; Set PCIDMA_* flags; trashes A only	
	JSR	PCI_ERROR_CLEAR
	
	BCLR	#PCIDMA_RESTART,X:STATUS ; Test and clear
	JCS	CON_TRANSFER_PCI	; Restart PCI burst

	BCLR	#PCIDMA_RESUME,X:STATUS	; Test and clear
	JCC	CON_TRANSFER		; Error but no error? Redo this burst.

	;; Update the PCI burst size and burst again.
	JSR	PCI_RECOVER_COUNT	; Get transferred byte count in A.
	JSR	PCI_UPDATE_R0
	JMP	CON_TRANSFER_PCI

; Utility routines for BLOCK_TRANSFER and CON_TRANSFER

PCI_GO
; Initiate PCI read/write of BURST_SIZE bytes.
; R0 must point to the hi-lo PCI address source/dest address
; X0 is the PCI command (6 is read, 7 is write).
; Trashes A and B but not R0 and X0.
	CLR	A
	CLR	B
	MOVE	X:BURST_SIZE,B0		; B = n8
	DEC	B			; n8 - 1
	ADD	#0,B			; Clear carry
	ASR	#2,B,B			; (n8 - 1)/4 = n32 - 1
	ADD	#0,B			; Clear carry
	ASL	#16,B,B			; B[23:16] = " "
	
	MOVE	X:(R0+1),A0		; PCI HI address

	ADD	B,A
	NOP
	MOVE	A0,X:DPMC		; PCI burst length and HI address

	MOVE	X0,A0
	ADD	#0,B			; Clear carry
	ASL	#16,A,A			; Command into bits 19:16
	MOVE	X:(R0),B0
	ADD	B,A
	NOP
	
	MOVEP	A0,X:DPAR		; PCI LO address and GO
	RTS

	
PCI_RECOVER_COUNT
; Calculate number of PCI words not transferred.
; Correct BURST_SIZE.  Returns:
;   B: bytes not transferred
;   A: bytes transferred
	CLR	A
	CLR	B
	MOVEP	X:DPSR,A0		; Get words left to write
	JCLR	#RDCQ,X:DPSR,PCI_RECOVER_COUNT1
	INC	B
PCI_RECOVER_COUNT1
	INC	B			; We want N, not N-1.
	ADD	#0,B			; Clear carry
	ASR	#16,A,A
	ADD	A,B			; B is words remaining
	ADD	#0,B			; Clear carry
	ASL	#2,B,B			; Number of bytes left to transfer
	MOVE	X:BURST_SIZE,A0
	SUB	B,A			; A is bytes written
	RTS


PCI_UPDATE_R0
;  Subtract A0 from BURST_SIZE and add A to the 32-bit hi-lo address at X:[R0].
;  Caller can check Z flag to see if BURST_SIZE is now 0.
	MOVE	A0,X1			; Save A for later
	ASL	#0,A,B			; MOVE A,B
	JSR	ADD_HILO_ADDRESS	; This updates [R0] = [R0] + B

	MOVE	X:BURST_SIZE,B
	SUB	X1,B			; Zero flag must be preserved!
	NOP
	MOVE	B1,X:BURST_SIZE
	RTS
		
	
;----------------------------------------------;
;  MCE PACKET PROCESSING                       ;
;----------------------------------------------;
	
; 	Given a dword count in A, computes number of half FIFOs and
; 	number of left over FIFO reads required to get the whole
; 	packet.

; 	Input: A is packet size, in dwords
; 	Output: sets X:TOTAL_BUFFS and X:LEFT_TO_READ
; 	Trashes: A,B,X0

	
PACKET_PARTITIONS
	MOVE	A0,X:PACKET_SIZE

	ADD	#0,B			; Clear carry
	ASL	#1,A,A		        ;  * 2
	ASL	#15,A,B			; B1 = size in bytes / 2^10
	MOVE	#0,X0
	INSERT	#$00E009,X0,A		; A0 = (size in bytes % 2^10) / 2

	MOVE	B1,X:TOTAL_BUFFS
	MOVE	A0,X:LEFT_TO_READ
	RTS
	

; BUFFER_PACKET
;
; Copies the packet in the FIFO to Y memory.
;
; In: TOTAL_BUFFS and LEFT_TO_READ must be pre-set (via PACKET_PARTITIONS);
;     R1 is the destination index in Y memory.
; Trashes: R1 is updated to point to the end of the copied data.

BUFFER_PACKET

	MOVE	#>$b00,A1
	JSR	TIMER_STORE_A1
	JSR	TIMER_STORE
	
	DO	X:TOTAL_BUFFS,BUFFER_PACKET_HALFS_DONE
	JSR	WAIT_FIFO_HALF
	JSR	TIMER_STORE
	JSR	BUFFER_PACKET_HALF
	JSR	TIMER_STORE
	NOP
BUFFER_PACKET_HALFS_DONE

	;; Polling the EF bit for FIFO words is very slow -- it can't even
	;;  keep up with the 10 MW/s write rate of the MCE.
	;; 1. If we want single words and the FIFO is half full, do the
	;;  read at full speed.
	JCLR	#HF,X:PDRD,BUFFER_PACKET_SINGLES_FAST

	;; 2. Do a timed read on the FIFO; i.e. this assumes that the data
	;;  are arriving at 25 MB/s (or a bit slower, there's overhead).

BUFFER_PACKET_SINGLES
	;; This is a non-polling read!  It uses to uses the 50 MHz timer
	;; and assumes that bytes show up on the FIFO at 25 MB/s
	CLR	A
	CLR	B
	MOVE	X:TCR0,B0		; Store timer value (50 MHz)
	ASR	#2,B,B			; / 4
	DO	X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_DONE
BUFFER_PACKET_SINGLES_WAIT
	MOVE	X:TCR0,A0
	ASR	#2,A,A
	CMP	A,B
	JEQ	BUFFER_PACKET_SINGLES_WAIT
 	MOVEP	Y:RDFIFO,Y:(R1)+
	ASL	#0,A,B			; MOVE A,B
BUFFER_PACKET_SINGLES_DONE
 	JSR	TIMER_STORE
	RTS

;---------------------------------------------------------

BUFFER_PACKET_SINGLES_FAST
	DO	X:LEFT_TO_READ,BUFFER_PACKET_SINGLES_FAST_DONE
	MOVEP	Y:RDFIFO,Y:(R1)+
BUFFER_PACKET_SINGLES_FAST_DONE
	RTS

;---------------------------------------------------------
BUFFER_PACKET_HALF
	;; Copies 512 16-bit words from FIFO into Y:R1
	DO	#512,BUFFER_PACKET_HALF_DONE
	MOVEP	Y:RDFIFO,Y:(R1)+
	NOP
BUFFER_PACKET_HALF_DONE
	RTS	
	
;---------------------------------------------------------
WAIT_FIFO_HALF
	JSET	#FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
	JSET	#HF,X:PDRD,WAIT_FIFO_HALF	; Wait for half full+1
	NOP
	NOP
	JSET	#HF,X:PDRD,WAIT_FIFO_HALF	; Protect against metastability
	RTS

;---------------------------------------------------------
	
; This is the old single-buffering routine, which polls the EF.
BUFFER_PACKET_SINGLES_POLL	
	DO	X:LEFT_TO_READ,BUFFER_PACKET_DONE
BUFFER_PACKET_SINGLE
	JSET	#FATAL_ERROR,X:<STATUS,DUMP_FIFO
	JCLR	#EF,X:PDRD,BUFFER_PACKET_SINGLE
	NOP
	NOP
	JCLR	#EF,X:PDRD,BUFFER_PACKET_SINGLE	; Protect against metastability
	MOVEP	Y:RDFIFO,Y:(R1)+
BUFFER_PACKET_DONE
	RTS

;---------------------------------------------------------

FATALITY_HANDLER	
	JMP	START			; What could possibly go wrong?


; DROP_PACKET
;
; Reads a packet from the fifo, discarding it.
; 
; In: TOTAL_BUFFS & LEFT_TO_READ
; Trashes: A0
	
DROP_PACKET
	DO	X:TOTAL_BUFFS,DROP_PACKET_SINGLES
	JSR	WAIT_FIFO_HALF
	JSR	DROP_FIFO_HALF
	NOP	
DROP_PACKET_SINGLES
	DO	X:LEFT_TO_READ,DROP_PACKET_DONE
DROP_PACKET_SINGLE
	JSET	#FATAL_ERROR,X:<STATUS,DUMP_FIFO
	JCLR	#EF,X:PDRD,DROP_PACKET_SINGLE
	JCLR	#EF,X:PDRD,DROP_PACKET_SINGLE	; Protect against metastability
	MOVEP	Y:RDFIFO,A0
DROP_PACKET_DONE
	RTS

DROP_FIFO_HALF
	;; Read and discard 512 16-bit words from the FO FIFO
	DO	#512,DROP_FIFO_DONE
	MOVEP	Y:RDFIFO,A0
DROP_FIFO_DONE
	RTS
		
	
;----------------------------------------------;
;  TIMER HANDLING                              ;
;----------------------------------------------;
	
; Start value is TLR, count is in TCR, flag marked at TCPR
; Must set TCSR[TCIE] to enable int
; Must set TCSR[T] for timer to restart

TIMER_ENABLE
	MOVE	#$000201,X0		; Enable
	NOP
	MOVE	X0,X:TCSR0
	RTS

TIMER_DISABLE
	MOVE	#$300200,X0		; Clear TOF, TCF, disable timer.
	NOP
	MOVE	X0,X:TCSR0
	RTS

TIMER_DEFAULT
	JSR	TIMER_DISABLE
	MOVE	#$4C4B40,X0		; 5M -> 10 Hz.
	NOP
	MOVE	X0,X:TCPR0
	JSR	TIMER_ENABLE
	RTS
	
	
;;; Timer action is called from the main loop when counter flag is detected.
TIMER_ACTION
	MOVE	#$300201,X0		; Clear TOF, TCF, leave timer enabled.
	NOP
	MOVE	X0,X:TCSR0
 	MOVE	X:QT_INFORM_IDX,A	; QT inform time?
	JCLR	#MODE_QT,X:MODE,TIMER_ACTION_OK
 	CMP	#>0,A			; If inform_idx != 0
 	JEQ	TIMER_ACTION_OK
	BSET	#QT_FLUSH,X:STATUS	;    schedule inform
TIMER_ACTION_OK
	RTS


;----------------------------------------------;
;  TIMER UTILITY                               ;
;----------------------------------------------;

TIMER_SOURCE	EQU	TCR0
	
TIMER_STORE_INIT
	MOVE	#>TIMER_BUFFER,A0
	NOP
	MOVE	A0,X:TIMER_INDEX
	MOVE	A0,R4
	RTS

TIMER_STORE
	;; Write the timer value to the timer buffer.
	;; Trashes A.  Sorry.
	MOVE	X:TIMER_SOURCE,A
	; Fall-through

TIMER_STORE_A1
	;; Write A1 to to the timer buffer. Trashes A.
	MOVE	A1,Y:(R4)+
	MOVE	R4,A1
	CMP	#>TIMER_BUFFER_END,A
	MOVE	A1,X:TIMER_INDEX
	JGE	TIMER_STORE_INIT
	RTS


;----------------------------------------------;
;  CIRCULAR BUFFER HANDLING                    ;
;----------------------------------------------;

BUFFER_INCR
	
	MOVE	X:QT_BUF_HEAD,A		; If head + 1 == max
	ADD	#1,A			; 
	MOVE	X:QT_BUF_MAX,B		;	
	CMP	A,B			; 
	JLE	BUFFER_RESET		;	head = 0
					; else
	MOVE	A,X:QT_BUF_HEAD		;	head = head + 1

	CLR	B
	MOVE	X:QT_BUF_SIZE,B0
	MOVE	#QT_DEST_LO,R0
	JSR	ADD_HILO_ADDRESS	; QT_DEST += QT_BUF_SIZE	
		
	RTS

	
BUFFER_RESET
	MOVE	#QT_BASE_LO,R0
	JSR	LOAD_HILO_ADDRESS
	MOVE	#QT_DEST_LO,R0
	JSR	SAVE_HILO_ADDRESS	; QT_DEST_LO = QT_BASE_LO

	MOVE	#0,X0
	MOVE	X0,X:QT_BUF_HEAD	; HEAD = 0
	RTS

	
BUFFER_INFORM_CHECK
	MOVE	X:QT_INFORM_IDX,A
	ADD	#1,A
	MOVE	X:QT_INFORM,B
	CMP	A,B
	JGT	BUFFER_INFORM_OK	; If inform_idx + 1 <= inform
	BSET	#QT_FLUSH,X:STATUS	;	schedule inform

BUFFER_INFORM_OK
	MOVE	A,X:QT_INFORM_IDX	; inform_idx = inform_idx + 1
	RTS

	
;---------------------------------------------------------------
BUFFER_INFORM
;---------------------------------------------------------------
; Informs host of current buffer status

	;; Skip this information if host is processing some other data.
	JSET	#DCTR_HF3,X:DCTR,INFORM_EXIT
	JCLR	#STRQ,X:DSR,INFORM_EXIT

	PCI_LOCKDOWN			; Disable host IRQ

	MOVE	#'QTI',X0		; Quiet Transfer Inform
	MOVE	X0,X:<DTXS_WD1

	MOVE	X:QT_BUF_HEAD,X0	; Next write index
	MOVE	X0,X:<DTXS_WD2

	MOVE	X:QT_BUF_TAIL,X0	; Forbidden write index
	MOVE	X0,X:<DTXS_WD3

	MOVE	X:QT_DROPS,X0		; Dropped packet count
	MOVE	X0,X:<DTXS_WD4

	JSR	PCI_MESSAGE_TO_HOST

	BCLR	#QT_FLUSH,X:STATUS
	MOVE	#0,X0			; Reset inform index
	MOVE	X0,X:QT_INFORM_IDX
	PCI_LOCKUP			; Enable host IRQ
INFORM_EXIT
	RTS



;----------------------------------------------;
;  ADDRESS HANDLING                            ;
;----------------------------------------------;
	
;;; Here, "HILO" format refers to the storage of a 32 bit address addr into
;;; two consecutive 24-bit words in X memory, mem0 and mem1.
;;; mem0[15..0] = addr[15..0] and mem1[15..0] = addr[31..16].
	
LOAD_HILO_ADDRESS
	;; Load the 32 bit address stored in [ R0+1 : R0 ] into A
	;; Trashes X0.
	CLR	A
	MOVE	X:(R0)+,A0
	MOVE	X:(R0)-,X0
	INSERT	#$010010,X0,A
	RTS

ADD_HILO_ADDRESS
	;; Adds B to the hilo address stored at [ R0+1 : R0 ]
	;; Trashes X0 and A and B.

	JSR	LOAD_HILO_ADDRESS
	ADD	B,A

SAVE_HILO_ADDRESS
	;; Save the low 32 bits of A into [ R0+1 : R0 ]
	;; Trashes X0 and B, preserves A.

	MOVE	X0,X:(R0)+		; pre-increment
	MOVE	#0,X0
	ASL	#8,A,B
	INSERT	#$008010,X0,A
	MOVE	B1,X:(R0)-		; store hi16
	MOVE	A0,X:(R0)
	ASR	#8,B,A
	RTS

	
	

BOOTCODE_END
BOOTEND_ADDR	EQU	@CVI(BOOTCODE_END)

PROGRAM_END
PEND_ADDR	EQU	@CVI(PROGRAM_END)
