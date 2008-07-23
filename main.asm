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

	;; Reinitialize if a serious error has been detected
	JSET	#FATAL_ERROR,X:<STATUS,START

	;; Jump to special application area if signalled to do so
	JSET	#MODE_APPLICATION,X:<MODE,APPLICATION

	;; Check for timer expiry and branch to the handler
	JSSET	#TCF,X:TCSR0,TIMER_ACTION 

	;; If it's time to signal the PC of buffer state, do so.
	JSSET	#QT_FLUSH,X:STATUS,BUFFER_INFORM
	
	;; Check for data in fibre-optic FIFO
	JSR	<GET_FO_WRD
 	JSSET	#FO_WRD_RCV,X:STATUS,HANDLE_FIFO

	;; New CON code, only run if the fifo isn't hot.
	JSSET	#CON_DEMAND,X:STATUS,CON_NOW

	;; Hackers, welcome.
	NOP
	NOP
	
	;; Loop
	JMP	PACKET_IN

;;; 
;;; End of main loop.
;;; 



;;; Fibre data detected; process it and return to main loop.

HANDLE_FIFO
	JSET	#MODE_CHOKE,X:<MODE,RETURN_NOW	; IF MCE Packet choke on - just keep clearing FIFO.
	MOVE	X0,X:<HEAD_W1_0				;store received word
	MOVE	X:PREAMB1,A
	CMP	X0,A					; check it is correct
	JNE	<PRE_ERROR				; if not go to start

	JSR	<WT_FIFO		; wait for next preamble 16-bit word
	MOVE	X0,X:<HEAD_W1_1		;store received word
	MOVE	X:PREAMB1,A
	CMP	X0,A			; check it is correct
	JNE	<PRE_ERROR		; if not go to start

	JSR	<WT_FIFO		; wait for next preamble 16-bit word
	MOVE	X0,X:<HEAD_W2_0		;store received word
	MOVE	X:PREAMB2,A
	CMP	X0,A			; check it is correct
	JNE	<PRE_ERROR		; if not go to start

	JSR	<WT_FIFO		; wait for next preamble 16-bit word
	MOVE	X0,X:<HEAD_W2_1		;store received word
	MOVE	X:PREAMB2,A
	CMP	X0,A			; check it is correct
	JNE	<PRE_ERROR		; if not go to start

PACKET_INFO                                            ; packet preamble valid

	JSR	<WT_FIFO	
	MOVE	X0,X:<HEAD_W3_0		; RP or DA
	JSR	<WT_FIFO	
	MOVE	X0,X:<HEAD_W3_1		; $2020

	JSR	<WT_FIFO	
	MOVE	X0,X:<HEAD_W4_0		; packet size lo
	JSR	<WT_FIFO	
	MOVE	X0,X:<HEAD_W4_1		; packet size hi

	;; Break packet size into TOTAL_BUFFS and LEFT_TO_READ
	CLR	A
	MOVE	X:HEAD_W4_0,A0
	MOVE	X:HEAD_W4_1,X0
 	INSERT	#$010010,X0,A		; A = size in dwords

	;; Set TOTAL_BUFFS and LEFT_TO_READ using A
	JSR	PACKET_PARTITIONS

;;; Case (packet type) of
	MOVE	X:HEAD_W3_0,A

	CMP	#>'RP',A
	JEQ	HANDLE_RP
	
	CMP	#>'DA',A
	JEQ	HANDLE_DA

	JMP	QT_PTYPE_ERROR

PRE_ERROR	
	BSET	#PREAMBLE_ERROR,X:<STATUS	; indicate a preamble error
	JSR	CLEAR_FO_FIFO		; empty the fifo (2 ms!)

QT_PTYPE_ERROR		
QT_FSIZE_ERROR
RETURN_NOW
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
	;; Prepare notification packet
	
	MOVE	#'NFY',X0
	MOVE	X0,X:DTXS_WD1
	MOVE	#'RPQ',X0
	MOVE	X0,X:DTXS_WD2
	MOVE	A0,X:DTXS_WD3	; A0=block_size
	MOVE	A1,X:DTXS_WD4	; A1=0

	;; DMA to host
	MOVE	#>REPLY_BUFFER,X0
	MOVE	A0,X:BLOCK_SIZE
	MOVE	X0,X:BURST_SRC
	JSR	BLOCK_TRANSFER

	;; Mark buffer and signal PC
	BSET	#RP_BUFFER_FULL,X:STATUS
	JSR	PCI_MESSAGE_TO_HOST

	;; Return to main loop
	RTS

HANDLE_RP_DROP
	MOVE	X:RP_DROPS,A
	ADD	#1,A
	NOP
	MOVE	A,X:RP_DROPS
	JMP	DROP_PACKET		; Will RTS to main loop
	
;;; HANDLE_RP ends
	
	
;;; HANDLE_DA
;;; Handler for MCE data (DA) frames.

HANDLE_DA

	;; Increment frame count
	MOVE	X:FRAME_COUNT,A
	ADD	#>1,A
	NOP
	MOVE	A,X:<FRAME_COUNT

	;; If not quiet mode, do normal processing
	JCLR	#MODE_QT,X:MODE,MCE_PACKET

	;; Copy words to Y memory.
	MOVE	#>IMAGE_BUFFER,R1
	JSR	BUFFER_PACKET

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
	MOVE	B1,X:BURST_SRC		; Y:0

	MOVE	#QT_DEST_LO,R0
	JSR	LOAD_HILO_ADDRESS
	MOVE	#BURST_DEST_LO,R0
	JSR	SAVE_HILO_ADDRESS

	;; Send
	JSR	BLOCK_TRANSFER

	;; Next buffer
	JSR	BUFFER_INCR

	;; Check if it's time to inform PC
	JSR	BUFFER_INFORM_CHECK

	RTS

HANDLE_DA_DROP
	;; Full buffer, drop packet.
	MOVE	X:QT_DROPS,A
	ADD	#1,A
	NOP
	MOVE	A,X:QT_DROPS
	JMP	DROP_PACKET		; Will RTS to main loop
	
;;; HANDLE_DA ends
	
		

CON_NOW
; 	This routine runs after the PC sends a 'CON' command, and will
; 	copy the command to the MCE and then reply to the PC.

	MOVE	#>CON_SOURCE_LO,R0
	JSR	LOAD_HILO_ADDRESS	; PCI address in A
	ASL	#0,A,B			; MOVE A,B

	;; Local buffer, in Y memory, for packet.
	MOVE	#>COMMAND_BUFFER,R6
	CLR	A

	;; MCE commands are 64 dwords big.
	DO	#64,CON_NOW1		; block size = 32bit x 64 (256 bytes)
	JSR	<READ_FROM_PCI		; get next 32 bit word from HOST

	MOVE	A1,Y:(R6)+		; b4, b3 (msb)		
	MOVE	A0,Y:(R6)+		; b2, b1  (lsb)

	JSR	<XMT_WD_FIBRE		; off it goes
	NOP
CON_NOW1

	BCLR	#MODE_CHOKE,X:<MODE	; disable packet choke...
					; comms now open with MCE and packets will be processed.	
	BSET	#AUX1,X:PDRC		; enable hardware

	;; CON processed, clear the request.
	BCLR	#CON_DEMAND,X:STATUS

	;; Reply to the CON command
	MOVE	#'CON',X0
	JSR	VCOM_PREPARE_REPLY
	JSR	PCI_MESSAGE_TO_HOST	
	RTS


	
		
;;; Old MCE packet handling code, ported forward a bit.
		
; --------------------------------------------------------------------------
; --------------------- MAIN PACKET HANDLING CODE --------------------------
; --------------------------------------------------------------------------

; prepare notify to inform host that a packet has arrived.

MCE_PACKET
	BCLR	#HST_NFYD,X:<STATUS		; clear flag to indicate host has been notified.

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


	MOVE	#>IMAGE_BUFFER,R1
	JSR	BUFFER_PACKET

	;; Wait for HST command (and destination RAM address)

WT_HOST	JSET	#FATAL_ERROR,X:<STATUS,START		; on fatal error, re-init.
	JCLR	#SEND_TO_HOST,X:<STATUS,WT_HOST		; Set in 'send_packet_to_host' ISR

	;; All data is buffered and destination address is in BURST_ADDR_HI/LO.
	MOVE	#>IMAGE_BUFFER,X0
	MOVE	X:PACKET_SIZE,A
	ASL	#2,A,A
	MOVE	X0,X:BURST_SRC
	MOVE	A1,X:BLOCK_SIZE
	JSR	BLOCK_TRANSFER

	JSET	#FATAL_ERROR,X:<STATUS,START

	;; Reply to the HST command
	MOVE	#'HST',X0
	JSR	VCOM_PREPARE_REPLY
	JSR	PCI_MESSAGE_TO_HOST
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
	BCLR	#MODE_QT,X:MODE
	JSR	TIMER_DISABLE
	MOVE	X0,A
	TST	A
	JEQ	VCOM_EXIT
	MOVE	#0,A0
	BSET	#MODE_QT,X:MODE
	MOVE	A0,X:TLR0
	JSR	TIMER_ENABLE
	JMP	VCOM_EXIT
	
QUIET_TRANSFER_SET_R0
	MOVE	X0,X:(R0)
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


;--------------------------------------------------------------------
CLEAN_UP_PCI
;--------------------------------------------------------------------
; Clean up the PCI board from wherever it was executing

	MOVEC	#1,SP			; Point stack pointer to the top	
	MOVEC	#$000200,SSL		; SR = zero except for interrupts
	MOVEC	#0,SP			; Writing to SSH preincrements the SP
	MOVEC	#START,SSH		; Set PC to for full initialization
	NOP
	RTI


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

; save some registers but not B

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
        BCLR	#PREAMBLE_ERROR,X:<STATUS	; clear preamble error
	BCLR	#APPLICATION_RUNNING,X:<STATUS  ; clear appl running bit.

; remember we are in a ISR so can't just jump to start.

	MOVEC	#1,SP			; Point stack pointer to the top	
	MOVEC	#$000200,SSL		; SSL holds SR return state
					; set to zero except for interrupts
	MOVEC	#0,SP			; Writing to SSH preincrements the SP
					; so first set to 0
	MOVEC	#START,SSH		; SSH holds return address of PC
					; therefore,return to initialization
	NOP
	RTI				; return from ISR - to START


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
	MOVE	X0,X:CON_SOURCE_HI
	MOVE	X1,X:CON_SOURCE_LO

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


;---------------------------------------------------------------
GET_FO_WRD	
;--------------------------------------------------------------
; Anything in fibre receive FIFO?   If so store in X0

		JCLR	#EF,X:PDRD,CLR_FO_RTS
		NOP	
		NOP
		JCLR	#EF,X:PDRD,CLR_FO_RTS		; check twice for FO metastability.	
		JMP	RD_FO_WD

WT_FIFO		JCLR	#EF,X:PDRD,*			; Wait till something in FIFO flagged
		NOP
		NOP
		JCLR	#EF,X:PDRD,WT_FIFO	; check twice.....

; Read one word from the fiber optics FIFO, check it and put it in A1
RD_FO_WD
		MOVEP	Y:RDFIFO,X0			; then read to X0
		MOVE	#$00FFFF,A1			; mask off top 2 bytes ($FC)
		AND	X0,A				; since receiving 16 bits in 24bit register
		NOP
		MOVE	A1,X0
		BSET	#FO_WRD_RCV,X:<STATUS
		RTS
CLR_FO_RTS	
		BCLR	#FO_WRD_RCV,X:<STATUS
		RTS

	
; ----------------------------------------------------------------------------
PCI_MESSAGE_TO_HOST
;----------------------------------------------------------------------------

; subroutine to send 4 words as a reply from PCI to the Host
; using the DTXS-HRXS data path
; PCI card writes here first then causes an interrupt INTA on
; the PCI bus to alert the host to the reply message

	JSET	#DCTR_HF3,X:DCTR,*	; make sure host ready to receive interrupt
					; cleared via fast interrupt if host out of its ISR
	MOVE	#>DTXS_WD1,R0

	DO	#4,PCI_MESSAGE_TO_HOST_RESTORE
	JCLR	#STRQ,X:DSR,*		; wait to be not full
	MOVEP	X:(R0)+,X:DTXS

PCI_MESSAGE_TO_HOST_RESTORE	
		
	;; Re-restore X0 and R0
	MOVE	X:SV_X0,X0		; restore X0
	MOVE	X:SV_R0,R0		; restore X0

; all the transmit words are in the FIFO, interrupt the Host
; the Host should clear this interrupt once it is detected. 
; It does this by writing to HCVR to cause a fast interrupt.

	; set flag to handshake interrupt (INTA) with host.
	BSET	#DCTR_HF3,X:DCTR
	; only interrupt in irq mode
	JCLR	#MODE_IRQ,X:MODE,PCI_MESSAGE_TO_HOST_RETURN
	BSET	#INTA,X:DCTR		; Assert the interrupt
PCI_MESSAGE_TO_HOST_RETURN
	RTS


;---------------------------------------------------------------
RD_DRXR
;--------------------------------------------------------------
; routine is used to read from HTXR-DRXR data path
; which is used by the Host to communicate with the PCI board
; the host writes 4 words to this FIFO then interrupts the PCI
; which reads the 4 words and acts on them accordingly.

	JCLR	#SRRQ,X:DSR,*		; Wait for receiver to be not empty
					; implies that host has written words

; actually reading as slave here so this shouldn't be necessary......?

 	BCLR	#FC1,X:DPMC		; 24 bit read FC1 = 0, FC1 = 0
 	BSET	#FC0,X:DPMC	

	MOVE	#DRXR_WD1,R3
	REP	#4
	MOVEP	X:DRXR,X:(R3)+
	RTS
	
;---------------------------------------------------------------
READ_FROM_PCI
;--------------------------------------------------------------
; sub routine to read a 24 bit word in from PCI bus --> Y memory
; 32bit host address in accumulator B.

; read as master 

	EXTRACTU #$010010,B,A		; Get D31-16 bits only
	NOP

	MOVE	A0,A1
	NOP
	MOVE	A1,X:DPMC		; high 16bits of address in DSP master cntr reg.
					; 32 bit read so FC1 = 0 and FC0 = 0

	NOP
	EXTRACTU #$010000,B,A
	NOP
	MOVE	A0,A1
	OR	#$060000,A		; A1 gets written to DPAR register
	NOP				; C3-C0 of DPAR=0110 for memory read
WRT_ADD	MOVEP	A1,X:DPAR		; Write address to PCI bus - PCI READ action
	NOP				; Pipeline delay
RD_PCI	JSET	#MRRQ,X:DPSR,GET_DAT	; If MTRQ = 1 go read the word from host via FIFO
	JCLR	#TRTY,X:DPSR,RD_PCI	; Bit is set if its a retry
	MOVEP	#$0400,X:DPSR		; Clear bit 10 = target retry bit
	JCLR	#MARQ,X:DPSR,*		; Wait for PCI addressing to be complete
	JMP	<WRT_ADD

GET_DAT	MOVEP	X:DRXR,A0		; Read 1st 16 bits of 32 bit word from host memory
	MOVEP	X:DRXR,A1		; Read 2nd 16 bits of 32 bit word from host memory	

; note that we now have 4 bytes in X0 and X1.
; The 32bit word was in host memory in little endian format
; If form LSB --> MSB the bytes are b1, b2, b3, b4 in host memory
; in progressing through the HTRX/DRXR FIFO the 
; bytes end up like this.....
; then X0 = $00 b2 b1
; and  X1 = $00 b4 b3

	REP	#4			; increment PCI address by four bytes.
	INC	B			
	NOP
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

;-------------------------------------------------------
XMT_WD_FIBRE
;-----------------------------------------------------
; 250 MHz code - Transmit contents of Accumulator A1 to the MCE
; we want to send 32bit word in little endian fomat to the host.
; i.e. b4b3b2b1 goes b1, b2, b3, b4
; currently the bytes are in this order:
;  A0 = $00 b2 b1
;  A1 = $00 b4 b3
;  A = $00 00 b4 b3 00 b2 b1

	;; Isolate all four bytes

	MOVE	B0,X0			; Save B
	MOVE	B1,X1

	ASL	#24,A,B
	AND	#>$0000FF,B		; B1=b1
	MOVE	B1,X:FO_SEND

	ASL	#16,A,B
	AND	#>$0000FF,B
	MOVE	B1,X:FO_SEND		; B1=b2

	ASR	#8,A,B
	AND	#>$0000FF,A
	MOVE	A1,X:FO_SEND		; A1=b3

	AND	#>$0000FF,B
	MOVE	B1,X:FO_SEND 		; B1=b4

	MOVE	X0,B0			; Restore B
	MOVE	X1,B1
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

	
;-----------------------------------------------
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
	MOVEP	#$0400,X:DPSR		; Clear target retry error bit
	MOVE	A0,X:EC_TRTY
	BSET	#PCIDMA_RESTART,X:STATUS
	RTS
ERROR_TO
	MOVE	X:EC_TO,A0
	INC	A
	MOVEP	#$0800,X:DPSR		; Clear timeout error bit
	MOVE	A0,X:EC_TO
	BSET	#PCIDMA_RESUME,X:STATUS
	RTS
ERROR_TDIS
	MOVE	X:EC_TDIS,A0
	INC	A
	MOVEP	#$0200,X:DPSR		; Clear target disconnect bit
	MOVE	A0,X:EC_TDIS
	BSET	#PCIDMA_RESUME,X:STATUS
	RTS
ERROR_TAB
	MOVE	X:EC_TAB,A0
	INC	A
	MOVEP	#$0100,X:DPSR		; Clear target abort error bit
	MOVE	A0,X:EC_TAB
	BSET	#PCIDMA_RESTART,X:STATUS
	RTS
ERROR_MAB
	MOVE	X:EC_MAB,A0
	INC	A
	MOVEP	#$0080,X:DPSR		; Clear master abort error bit
	MOVE	A0,X:EC_MAB
	BSET	#PCIDMA_RESTART,X:STATUS
	RTS
ERROR_DPER
	MOVE	X:EC_DPER,A0
	INC	A
	MOVEP	#$0040,X:DPSR		; Clear data parity error bit
	MOVE	A0,X:EC_DPER
	BSET	#PCIDMA_RESTART,X:STATUS
	RTS
ERROR_APER
	MOVE	X:EC_APER,A0
	INC	A
	MOVEP	#$0020,X:DPSR		; Clear address parity error bit
	MOVE	A0,X:EC_APER
	BSET	#PCIDMA_RESTART,X:STATUS
	RTS


;----------------------------------------------
BLOCK_TRANSFER
;----------------------------------------------
;   In:
;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address
;   - BLOCK_SIZE is packet size, in bytes
;   - BURST_SRC is start of data in Y memory
;  Out:
;   - BLOCK_SIZE will be decremented to zero.
;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
;   - BURST_SRC will be incremented by BLOCK_SIZE/2
;  Trashes:
;   - A and B
	
	;; DSP PCI burst limit is 256 bytes.
	MOVE	X:BLOCK_SIZE,A	        ; A1 = BLOCK_SIZE
	
	CMP	#0,A
	JEQ	BLOCK_DONE

	;; Careful here, force long (24-bit) literal.

	CLR	B
	MOVE	X:PCI_BURST_SIZE,B1
	
	CMP	B,A			; A ? B
	JGE	<BLOCK_TRANSFER1	; jump if A >= B
	MOVE	A,B			; This only moves A1,B1.
BLOCK_TRANSFER1
	SUB	B,A			; A -= B
	ADD	#0,B			; Clear carry bit
	MOVE	A,X:BLOCK_SIZE		; Updated BLOCK_SIZE
	MOVE	B,X:BURST_SIZE		; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100))
	ASR	#25,B,B			; B0 = # of 16 bit words

	;; Setup DMA from BURST_SRC to PCI tx
	MOVEP	#DTXM,X:DDR0		; DMA dest'n
	MOVE	X:BURST_SRC,A0
	MOVEP	A0,X:DSR0		; DMA source
	ADD	B,A
	DEC	B
	MOVE	A0,X:BURST_SRC		; BURST_SRC += BURST_SIZE/2
	
	MOVEP	B0,X:DCO0		; DMA length = BURST_SIZE/2 - 1

	;; DMA go
	MOVEP	#$8EFA51,X:DCR0

BLOCK_PCI
	;; Setup PCI burst using BURST_SIZE
	CLR	A
	CLR	B
	MOVE	X:BURST_SIZE,B0		; B = n8
	DEC	B			; n8 - 1
	ADD	#0,B			; Clear carry
	ASR	#2,B,B			; (n8 - 1)/4 = n32 - 1
	ADD	#0,B			; Clear carry
	ASL	#16,B,B			; B[23:16] = " "
	
	MOVE	X:BURST_DEST_HI,A0

	ADD	B,A
	NOP
	MOVE	A0,X:DPMC		; PCI burst length and HI address

	MOVE	#$07,A0
	ADD	#0,B			; Clear carry
	ASL	#16,A,A
	MOVE	X:BURST_DEST_LO,B0
	ADD	B,A
	NOP
	
	MOVEP	A0,X:DPAR		; PCI LO address and GO

BLOCK_CHECK
	NOP
	NOP
	JCLR	#MARQ,X:DPSR,*		; Wait for burst termination

	;; Check for error
	JSET	#MDT,X:DPSR,BLOCK_OK

	JSR	PCI_ERROR_CLEAR

	BCLR	#PCIDMA_RESTART,X:STATUS ; Test and clear
	JCS	<BLOCK_RESTART

	BCLR	#PCIDMA_RESUME,X:STATUS	; Test and clear
	JCS	<BLOCK_RESUME

BLOCK_OK
	MOVE	X:BURST_SIZE,A0		; Pass # of words written to updater
	JSR	BLOCK_UPDATE
	JMP	BLOCK_TRANSFER		; Finish the block
BLOCK_DONE
	RTS				; Done	
	
BLOCK_RESTART
	JMP	BLOCK_PCI		; Recalculate pci and resend

BLOCK_RESUME
	CLR	A
	CLR	B
	MOVEP	X:DPSR,A0		; Get words left to write
	JCLR	#15,X:DPSR,BLOCK_RESUME1
	
	INC	B
	
BLOCK_RESUME1

	INC	B			; We want N, not N-1.
	ADD	#0,B			; Clear carry
	ASR	#16,A,A
	ADD	A,B			; B is words remaining
	ADD	#0,B			; Clear carry
	ASL	#2,B,B			; Number of bytes left to transfer
	MOVE	X:BURST_SIZE,A0
	SUB	B,A			; A is words written

	JSR	BLOCK_UPDATE
	JMP	BLOCK_PCI		; Recalculate pci and resend

;;; Subroutine:	subtract A from BURST_SIZE and add A to BURST_DEST_LO
;;;  Caller can check Z flag to see if BURST_SIZE is 0 now.
BLOCK_UPDATE
	;; Use A (number of bytes bursted) to update
	;;  BURST_DEST_HI:LO and BURST_SIZE

	MOVE	A0,X1			; Save A
 	MOVE	A0,B0			; Save A again...
 	MOVE	A1,B1			; Save A again...
	NOP
	
	MOVE	#BURST_DEST_LO,R0
	JSR	ADD_HILO_ADDRESS	; This updates BURST_DEST

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
	

;;; Copies the packet in the FIFO to Y memory; assumes first 4 words
;;; have been loaded into HEAD_W* in the usual way.
	

BUFFER_PACKET

	;; Assumes that TOTAL_BUFFS and LEFT_TO_READ have already
	;; been set and dumps fifo into Y:(R1), destroying R1.

	
BUFFER_PACKET_HALFS
	DO	X:TOTAL_BUFFS,BUFFER_PACKET_SINGLES
	JSR	WAIT_FIFO_HALF
	JSR	BUFFER_PACKET_HALF
BUFFER_PACKET_SINGLES
	DO	X:LEFT_TO_READ,BUFFER_PACKET_DONE
BUFFER_PACKET_SINGLE
	JSET	#FATAL_ERROR,X:<STATUS,DUMP_FIFO
	JCLR	#EF,X:PDRD,BUFFER_PACKET_SINGLE
	JCLR	#EF,X:PDRD,BUFFER_PACKET_SINGLE	; Protect against metastability
	MOVEP	Y:RDFIFO,Y:(R1)+
BUFFER_PACKET_DONE
	RTS

BUFFER_PACKET_HALF
	;; Copies 512 16-bit words from FIFO into Y:R1
	DO	#512,BUFFER_PACKET_HALF_DONE
	MOVEP	Y:RDFIFO,Y:(R1)+
	NOP
BUFFER_PACKET_HALF_DONE
	RTS	
	
WAIT_FIFO_HALF
	JSET	#FATAL_ERROR,X:<STATUS,FATALITY_HANDLER
	JSET	#HF,X:PDRD,WAIT_FIFO_HALF	; Wait for half full+1
	NOP
	NOP
	JSET	#HF,X:PDRD,WAIT_FIFO_HALF	; Protect against metastability
	RTS

FATALITY_HANDLER	
	JMP	START			; What could possibly go wrong?


; 	Reads a packet from the fifo, discarding it.
; 
; 	In: TOTAL_BUFFS & LEFT_TO_READ
; 	Trashes: A0
	
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
	
; Start value is TLR, count is in TCR, int occurs at TCPR
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

;;; Timer action is called from the main loop when counter flag is detected.
TIMER_ACTION
 	MOVE	X:QT_INFORM_IDX,A
	MOVE	#$300201,X0		; Clear TOF, TCF, leave timer enabled.
	NOP
	MOVE	X0,X:TCSR0
 	CMP	#>0,A			; If inform_idx != 0
 	JEQ	TIMER_ACTION_OK
	BSET	#QT_FLUSH,X:STATUS	;	schedule inform
TIMER_ACTION_OK
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

	MOVE	#'QTI',X0		; Quiet Transfer Inform
	MOVE	X0,X:<DTXS_WD1

	MOVE	X:QT_BUF_HEAD,X0	; Next write index
	MOVE	X0,X:<DTXS_WD2

	MOVE	X:QT_BUF_TAIL,X0	; Forbidden write index
	MOVE	X0,X:<DTXS_WD3

	MOVE	X:QT_DROPS,X0		; Dropped packet count
	MOVE	X0,X:<DTXS_WD4


	JSET	#DCTR_HF3,X:DCTR,INFORM_EXIT
	JCLR	#STRQ,X:DSR,INFORM_EXIT

	JSR	PCI_MESSAGE_TO_HOST

	BCLR	#QT_FLUSH,X:STATUS
	MOVE	#0,X0			; Reset inform index
	MOVE	X0,X:QT_INFORM_IDX
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
