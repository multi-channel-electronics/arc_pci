		COMMENT *

	This implementation does communication with the host using PCI
	master writes only.
	
	*
	PAGE    132     ; Printronix page width - 132 columns
	OPT	CEX	; print DC evaluations

;;; 
HACK_ENTRY
;;;
	
	;; Only enter if HF2 is high.
	JCLR	#DSR_HF2,X:DSR,HACK_EXIT
	
HACK_INIT	
	;; Reset the FO receiver
	JSR	RESET_FIFO

	;; Nothing to handle
	BCLR	#COMM_CMD,X:STATUS
	BCLR	#COMM_REP,X:STATUS
	BCLR	#COMM_MCEREP,X:STATUS
	BCLR	#COMM_MCEDATA,X:STATUS
	BCLR	#COMM_ERR,X:STATUS
	BCLR	#COMM_REP_ENABLED,X:STATUS
	BCLR	#COMM_BUF_UPDATE,X:STATUS
	
	;; Init the datagram structure
	JSR	REPLY_BUFFER_INIT
	
	;; Init debug buffer indices
	MOVE	#>INT_DEBUG_BUF,X0
	MOVE	X0,X:INT_DEBUG_BUF_IDX
	MOVE	#>DEBUG_BUF,X0
	MOVE	X0,X:DEBUG_BUF_IDX
	
	MOVE	#>TIMER_BUFFER,X0
	MOVE	X0,X:TIMER_INDEX

	;; Quick timing test...
	;; MOVE	#>TIMER_BUFFER,R1
	
	;; MOVE	X:TIMER_SOURCE,X0
	;; MOVE	X0,Y:(R1)+
	;; MOVE	#>0,R3
	;; MOVE	#0,R0
	;; .loop	#256
	;; MOVE	X:(R0)+,Y0
	;; MOVE	Y0,Y:(R3)
	;; .endl
	;; MOVE	X:TIMER_SOURCE,X0
	;; MOVE	X0,Y:(R1)+

	;; MOVE	#>$1000,R3
	;; MOVE	#0,R0
	;; .loop 	#256
	;; MOVE	X:(R0)+,Y0
	;; MOVE	Y0,Y:(R3)
	;; .endl
	;; MOVE	X:TIMER_SOURCE,X0
	;; MOVE	X0,Y:(R1)+

		
	;; Enable PCI slave receive interrupt to handle commands from host
	BSET	#DCTR_SRIE,X:DCTR
	
	;; Set bit to indicate to host that we're in this loop.
	BSET	#DCTR_HF4,X:DCTR
	
	;;
	;; Main loop
	;; 
HACK_LOOP
;;; Polling:
 	;; JSR	PROCESS_PC_CMD_POLL
	
;;; Interrupt driven: process command in buffer
	JSSET 	#COMM_CMD,X:STATUS,PROCESS_PC_CMD_2

	;; Should we send a reply?
	JSSET	#COMM_REP,X:STATUS,PROCESS_REPLY
	
	;; FIFO action?
	JSR 	CHECK_FOR_DATA

	;; Transmit to host?
	JSSET	#COMM_MCEREP,X:STATUS,PROCESS_MCE_REPLY
	JSSET	#COMM_MCEDATA,X:STATUS,PROCESS_MCE_DATA

	;; ;; Check for timer expiry
	JSSET	#TCF,X:TCSR0,TIMER_ACTION_X
	
	;; ;; Issue information?
	JSSET	#QT_FLUSH,X:STATUS,SEND_BUF_INFO
	
	;; LOOP UNTIL host gives up.
	JSET	#DSR_HF2,X:DSR,HACK_LOOP
	

	;; Clean-up
	BCLR	#QT_FLUSH,X:STATUS
	
HACK_EXIT
	;; Disable PCI slave receive interrupt
	BCLR	#DCTR_SRIE,X:DCTR
	
	;; Lower flag
	BCLR	#DCTR_HF4,X:DCTR
	RTS
	
	
TIMER_ACTION_X
	MOVEP	#$300201,X:TCSR0	; Clear TOF, TCF, leave timer enabled.
;;; Testing...
	;; MOVE	X:QT_INFORM_IDX,A
	;; ADD	#1,A
	;; MOVE	X:QT_INFORM,B
	;; SUB	A,B
	;; JNE	TIMER_ACTION_X_OK
	;; ASL	#0,B,A		   	; MOVE A,B
	
	;; If new data, schedule a flush.
	BCLR	#COMM_BUF_UPDATE,X:STATUS
	JCC	TIMER_ACTION_X_OK
	BSET	#QT_FLUSH,X:STATUS	;    schedule inform
TIMER_ACTION_X_OK
	;; MOVE	A,X:QT_INFORM_IDX
	RTS

	
;;;
;;; New comms implementation
;;; 

	
REPLY_BUFFER_INIT
	;; initialize header of reply packet.
	MOVE	#0,X0
	MOVE	#>REP_BUFFER1,R0
	.loop	#RB_SIZE
	MOVE	X0,X:(R0)+
	.endl
	MOVE	#>RB_VERSION,X0
	MOVE	#>(RB_REP_SIZE/2),X1
	MOVE	X0,X:REP_VERSION
	MOVE	X1,X:REP_SIZE
	RTS


;----------------------------------------------
BLOCK_TRANSFERX
;----------------------------------------------
;   In:
;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address (16:16)
;   - BLOCK_SIZE is packet size, in bytes
;   - XMEM_SRC is start of data in X memory
;  Out:
;   - BLOCK_SIZE will be decremented to zero.
;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
;   - XMEM_SRC will be incremented by BLOCK_SIZE/2
;  Trashes:
;   - A and B at least

	CLR	A
	MOVE	X:BLOCK_SIZE,A	        ; A1 = BLOCK_SIZE
	CMP	#0,A			; Still bytes to transfer?
	JNE	BLOCK_TRANSFERX0
	RTS

BLOCK_TRANSFERX0
	;; Maximum size of a DMA/PCI burst is 256 bytes,
	;; but latency clock determines the ideal value.
	MOVE	X:PCI_BURST_SIZE,B	; B1 = burst size (256)

	CMP	B,A			; A ? B
	JGE	<BLOCK_TRANSFERX1	; jump if A >= B
	MOVE	A,B			; This only moves A1,B1.
BLOCK_TRANSFERX1
	;; Now burst size B <= block size A.
	SUB	B,A			; A -= B
	ADD	#0,B			; Clear carry bit
	MOVE	A,X:BLOCK_SIZE		; Updated BLOCK_SIZE
	MOVE	B,X:BURST_SIZE		; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100))
	ASR	#25,B,B			; B0 = # of 16 bit words

	;; Setup DMA from BURST_SRC to PCI tx
	MOVEP	#DTXM,X:DDR0		; DMA dest'n
	MOVE	X:XMEM_SRC,A0
	MOVEP	A0,X:DSR0		; DMA source
	ADD	B,A
	DEC	B
	MOVE	A0,X:XMEM_SRC		; BURST_SRC += BURST_SIZE/2
	
	MOVEP	B0,X:DCO0		; DMA length = BURST_SIZE/2 - 1

	;; DMA go
	MOVEP	#$8EFA50,X:DCR0		; X to X

BLOCK_TRANSFERX_PCI
	MOVE	#>$7,X0			; Memory write
	MOVE	#BURST_DEST_LO,R0	; RAM address
	JSR	PCI_GO			; Initiate PCI burst

	;; Wait for completion
	JCLR	#MARQ,X:DPSR,*

	;; Check for errors:
	JCLR	#MDT,X:DPSR,BLOCK_TRANSFERX_HANDLE_ERRORS
	
	CLR	B
	MOVE	X:BURST_SIZE,B0		; All bytes were transferred
	JSR	ADD_HILO_ADDRESS	; Update source address
	JMP	BLOCK_TRANSFERX		; Next burst in block

BLOCK_TRANSFERX_HANDLE_ERRORS
	;; Set PCIDMA_* flags; trashes A only	
	JSR	PCI_ERROR_CLEAR
	
	BCLR	#PCIDMA_RESTART,X:STATUS ; Test and clear
	JCS	BLOCK_TRANSFERX_PCI	; Restart PCI burst

	BCLR	#PCIDMA_RESUME,X:STATUS	; Test and clear
	JCC	BLOCK_TRANSFERX		; Error but no error? Redo this burst.

	;; Update the PCI burst size and burst again.
	JSR	PCI_RECOVER_COUNT	; Get transferred byte count in A.
	JSR	PCI_UPDATE_R0
	JMP	BLOCK_TRANSFERX_PCI


;;; PERMANENTER CODE.
	ORG	P:$900,P:$902

;----------------------------------------------
PROCESS_REPLY
;----------------------------------------------
	;; If reply channel is not configured, mark reply as sent and return.
	JSET	#COMM_REP_ENABLED,X:STATUS,PROCESS_REPLY1
	BCLR	#COMM_REP,X:STATUS ; Mark as... handled.
	RTS

PROCESS_REPLY1
	;; Mark the packet type and size
	MOVE	#>RB_TYPE_DSP_REP,A1
	MOVE	#>(RB_REP_SIZE/2),X1
	MOVE	A1,X:REP_TYPE
	MOVE	X1,X:REP_SIZE

	;; Set destination address... and check that it's non-zero
	MOVE	#>REP_BUS_ADDR,R0
	MOVE	#>BURST_DEST_LO,R1
	.loop	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl

	;; Set BLOCK_SIZE, in bytes
	MOVE	X:REP_SIZE,A
	ASL	#2,A,A
	NOP
	MOVE	A1,X0
	MOVE	X0,X:BLOCK_SIZE
	MOVE	#>REP_BUFFER1,X0
	MOVE	X0,X:XMEM_SRC

	;; Trigger writes as master.
	JSR 	BLOCK_TRANSFERX
	
	;; Mark as sent
	BCLR	#COMM_REP,X:STATUS
	
	;; Raise interrupt and wait for handshake.
	BSET	#INTA,X:DCTR
	JCLR	#DSR_HF0,X:DSR,*
	
	BCLR	#COMM_REP,X:STATUS ; Mark as sent.
	
	BCLR	#INTA,X:DCTR
	JSET	#DSR_HF0,X:DSR,*
	
	RTS

PROCESS_MCE_REPLY
	;; Copy data into the reply buffer, starting at the "type" field
	MOVE	#(MCEREP_BUF+MCEREP_TYPE),R3
	MOVE	Y:(MCEREP_BUF+MCEREP_SIZE),Y1
	MOVE	#(REP_DATA),R0
	.loop	#4
	MOVE	Y:(R3)+,Y0
	MOVE	Y0,X:(R0)+
	.endl
	.loop	#2
	.loop 	Y1
	MOVE	Y:(R3)+,Y0
	MOVE	Y0,X:(R0)+
	.endl
	nop
	.endl

	;; Mark the packet type and size
	MOVE	#>RB_TYPE_MCE_REP,A1
	MOVE	#>(RB_MCE_SIZE/2),X1 ; size in 32-bit words.
	MOVE	A1,X:REP_TYPE
	MOVE	X1,X:REP_SIZE
	
	;; Set destination address
	MOVE	#>REP_BUS_ADDR,R0
	MOVE	#>BURST_DEST_LO,R1
	.loop	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl

	;; Set BLOCK_SIZE, in bytes
	MOVE	X:REP_SIZE,A
	ASL	#2,A,A
	NOP
	MOVE	A1,X0
	MOVE	X0,X:BLOCK_SIZE
	MOVE	#>REP_BUFFER1,X0
	MOVE	X0,X:XMEM_SRC

	;; Trigger writes as master.
	JSR 	BLOCK_TRANSFERX
	
	;; Mark as sent
	BCLR	#COMM_MCEREP,X:STATUS
	
	;; Raise interrupt and wait for handshake.
	BSET	#INTA,X:DCTR
	JCLR	#DSR_HF0,X:DSR,*
	BCLR	#INTA,X:DCTR
	JSET	#DSR_HF0,X:DSR,*
	
	RTS

;----------------------------------------------
PROCESS_MCE_DATA
;----------------------------------------------
	;; Mark as handled
	BCLR	#COMM_MCEDATA,X:STATUS
	
	;; Check QT_BUF_HEAD, just drop the frame if the buffer is full
	MOVE	X:QT_BUF_HEAD,A
	ADD	#1,A
	MOVE	X:QT_BUF_MAX,B
	CMP	A,B		; End of buffer? [B ? A]
	JGT	PROCESS_MCE_DATA__CHECK_TAIL
	MOVE	#0,A		; Start over
PROCESS_MCE_DATA__CHECK_TAIL
	MOVE	X:QT_BUF_TAIL,B	; Buffer full?
	CMP	A,B
	JEQ	PROCESS_MCE_DATA__DROP_FRAME
	;; Don't store the update QT_BUF_HEAD, we'll do that later
	;; when we increment QT_DEST

	;; Send out the data directly
	MOVE	Y:(MCEREP_BUF+MCEREP_SIZE),A
	ASL	#2,A,A
	NOP
	MOVE	A1,X:BLOCK_SIZE
	
	;; Set destination address
	MOVE	#>QT_DEST_LO,R0
	MOVE	#>BURST_DEST_LO,R1
	.loop	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl

	MOVE	#>(MCEREP_BUF+MCEREP_PAYLOAD),X0
	MOVE	X0,X:YMEM_SRC

	;; Trigger writes as master.
	JSR 	BLOCK_TRANSFER
	
	;; Update buffer pointers
	JSR	BUFFER_INCR

PROCESS_MCE_DATA__DONE
	;; Regardless of whether we sent that frame, we should count
	;; it towards the goal.  Also invalidate the buffer, to
	;; trigger timer-based informs.
	BSET	#COMM_BUF_UPDATE,X:STATUS
	MOVE	X:QT_INFORM_IDX,A
	ADD	#1,A
	MOVE	X:QT_INFORM,B
	MOVE	A,X:QT_INFORM_IDX
	CMP	A,B
	JNE	PROCESS_MCE_DATA__DONE2
	BSET	#QT_FLUSH,X:STATUS
PROCESS_MCE_DATA__DONE2
	RTS
	
PROCESS_MCE_DATA__DROP_FRAME
	MOVE	X:QT_DROPS,A
	ADD	#1,A
	NOP
	MOVE	A,X:QT_DROPS
	RTS


;--------------------
SEND_BUF_INFO
;--------------------
	BCLR	#QT_FLUSH,X:STATUS
	;; Debug, click the buf head up and send inform.
	;; MOVE	X:QT_BUF_HEAD,A1
	;; ADD	#1,A
	;; NOP
	;; MOVE	A1,X:QT_BUF_HEAD
	;; RTS
	JCLR	#COMM_REP_ENABLED,X:STATUS,SEND_BUF_INFO_EXIT
	
	;; Load head index into first data field, pad to 32 bits
	MOVE	X:QT_BUF_HEAD,X0
	MOVE	X0,X:REP_DATA
	MOVE	#0,X0
	MOVE	X0,X:(REP_DATA+1)
	
	;; Mark the packet type and size
	MOVE	#>RB_TYPE_BUF_INF,A1
	MOVE	#>(RB_INF_SIZE/2),X1 ; size in 32-bit words.
	MOVE	A1,X:REP_TYPE
	MOVE	X1,X:REP_SIZE
	
	;; Set destination address
	MOVE	#>REP_BUS_ADDR,R0
	MOVE	#>BURST_DEST_LO,R1
	.loop	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl
	
	;; Set BLOCK_SIZE, in bytes
	MOVE	X:REP_SIZE,A
	ASL	#2,A,A
	NOP
	MOVE	A1,X0
	MOVE	X0,X:BLOCK_SIZE
	MOVE	#>REP_BUFFER1,X0
	MOVE	X0,X:XMEM_SRC

	;; Trigger writes as master.
	JSR 	BLOCK_TRANSFERX
	
	;; Raise interrupt and wait for handshake.
	BSET	#INTA,X:DCTR
	JCLR	#DSR_HF0,X:DSR,*
	BCLR	#INTA,X:DCTR
	JSET	#DSR_HF0,X:DSR,*
	
SEND_BUF_INFO_EXIT
	RTS
	






;;;
;;; Command processing
;;;

;;; CMD identifiers
CMD_READ_P		EQU	1
CMD_READ_X		EQU	2
CMD_READ_Y		EQU	3
			
CMD_WRITE_P		EQU	5
CMD_WRITE_X		EQU	6
CMD_WRITE_Y		EQU	7
			
CMD_SET_REP_BUF		EQU	9
CMD_SET_DATA_BUF	EQU	$A
	
CMD_SET_TAIL		EQU	$11		
			
CMD_SEND_MCE		EQU	$21
			

;; PROCESS_PC_CMD_POLL
;; 	MOVE	#DEBUG_BUF,R0
;; 	NOP
;; 	MOVE	Y:(R0),X0
;; 	MOVE	X0,R0
;; AGAIN_	
;; 	JCLR	#SRRQ,X:DSR,PROCESS_PC_CMD_POLL_EXIT
;; 	;; Dump PC words into debug buffer.
;; 	MOVEP	X:DRXR,X0
;; 	MOVE	X0,Y:(R0)+
;; 	JMP	AGAIN_

;; PROCESS_PC_CMD_POLL_EXIT
;; 	MOVE	R0,X0
;; 	MOVE	#(DEBUG_BUF+1),R0
;; 	NOP
;; 	MOVE	X0,Y:(R0-1)
;; 	RTS
	
;------------------------
PROCESS_PC_CMD_INT
;------------------------
	;; Push all and disable further SRRQ ints
	JSR	SAVE_REGISTERS	; This does not save all the registers...
	BCLR	#DCTR_SRIE,X:DCTR

	;; Is there data?
	JCLR	#SRRQ,X:DSR,PROCESS_PC_CMD_INT_EXIT
	
	;; Ok, you asked for it.
	;; Read the command word (cmd|length)
	;; MOVEP	X:DRXR,X0
	;; MOVE	#CMD_SIZE,R0
	;; JSR	PROCESS_SPLIT_X0_XR0

	MOVEP	X:DRXR,X0	; 16-bit word #0 = the command
	MOVE	X0,X:CMD_WORD
	NOP
	NOP
	JCLR	#SRRQ,X:DSR,*
	MOVEP	X:DRXR,X0
	MOVE	X0,X:CMD_SIZE	; 16-bit word #1 = size of upcoming data,
				; in 32-bit words.
	
	;; Read the packet data into a buffer.
	CLR	A
	MOVE	#CMD_BUFFER,R0
	MOVE	X:CMD_SIZE,A1
	
	;; ;; WTF
	;; ;; .loop	#1
	;; JCLR	#SRRQ,X:DSR,*
	;; MOVEP	X:DRXR,X:(R0)+
	;; NOP
	;; NOP
	;; JCLR	#SRRQ,X:DSR,*
	;; MOVEP	X:DRXR,X:(R0)+
	;; NOP
	;; NOP
	;; ;; .endl
	
	;; jmp	PROCESS_PC_CMD_INT_EXIT

	;; Don't call .loop with 0 argument!
	CMP	#0,A
	JEQ	PROCESS_PC_CMD_INT_OK
	
	.loop	A1
	JCLR	#SRRQ,X:DSR,*
	MOVEP	X:DRXR,X:(R0)+
	NOP
	NOP
	JCLR	#SRRQ,X:DSR,*
	MOVEP	X:DRXR,X:(R0)+
	NOP
	NOP
	.endl
	
PROCESS_PC_CMD_INT_OK
	MOVE	X:INT_DEBUG_BUF_IDX,R0
	MOVE	X:CMD_WORD,X0
	MOVE	X0,Y:(R0)+
	MOVE	X:CMD_SIZE,X0
	MOVE	X0,Y:(R0)+
	MOVE	X:CMD_BUFFER,X0
	MOVE	X0,Y:(R0)+
	MOVE	X:(CMD_BUFFER+1),X0
	MOVE	X0,Y:(R0)+
	MOVE	#>$aabbcc,X0	; end-of-data
	MOVE	X0,Y:(R0)+
	
	MOVE	R0,X:INT_DEBUG_BUF_IDX
	
	;; Mark cmd-to-process
	BSET	#COMM_CMD,X:STATUS

PROCESS_PC_CMD_INT_EXIT	
	;; Re-enable int and pop all
	BSET	#DCTR_SRIE,X:DCTR
	JSR	RESTORE_REGISTERS
	RTI

;;; Polling PC CMD reader.
	
PROCESS_PC_CMD
	;; Is there data?
	JSET	#SRRQ,X:DSR,PROCESS_PC_CMD_1
	RTS
	
PROCESS_PC_CMD_1
	;; Read the command word (cmd|length)
	MOVEP	X:DRXR,X0
	MOVE	#CMD_SIZE,R0
;;; 	warn    'BROKEN: do not split the words.  They are split.'
	JSR	PROCESS_SPLIT_X0_XR0
	
	;; Read the packet data into a buffer.
	CLR	A
	MOVE	#CMD_BUFFER,R0
	MOVE	X:CMD_SIZE,A1
	;; Don't call .loop with 0 argument!
	CMP	#0,A
	JEQ	PROCESS_PC_CMD_2
	.loop	A1
	JCLR	#SRRQ,X:DSR,*
	MOVEP	X:DRXR,X:(R0)+
	NOP
	NOP
	.endl

;;; If polling, handle the command immediately; if interrupt driven,
;;; this gets called from the main loop.
	
PROCESS_PC_CMD_2
	;; Init the reply packet for success.
	MOVE	#>RB_TYPE_DSP_REP,A1
	MOVE	X:CMD_WORD,B	; this will be used in the switch below.
	MOVE	A1,X:REP_TYPE	; type is "dsp reply"
	MOVE	B1,X:REP_RCMD	; copy of command word
	MOVE	B0,X:REP_RSTAT	; status = 0
	MOVE	B0,X:REP_RSIZE	; data size = 0
	
	;; Now distribute the command to a handler
	;; For memory reads, args are [address].
	;; For memory writes, args are [address, data].
	;; So pre-load those two.
	MOVE	#>CMD_BUFFER,R0
	JSR	PROCESS_JOIN_XR0_A
	MOVE	A0,R1		; "address"
	JSR	PROCESS_JOIN_XR0_A
	MOVE	A0,X1		; "data"

	CMP	#>CMD_READ_P,B
	JEQ	PROCESS_READ_P
	
	CMP	#>CMD_READ_X,B
	JEQ	PROCESS_READ_X
	
	CMP	#>CMD_READ_Y,B
	JEQ	PROCESS_READ_Y
	
	CMP	#>CMD_WRITE_P,B
	JEQ	PROCESS_WRITE_P
	
	CMP	#>CMD_WRITE_X,B
	JEQ	PROCESS_WRITE_X
	
	CMP	#>CMD_WRITE_Y,B
	JEQ	PROCESS_WRITE_Y

	CMP	#>CMD_SET_REP_BUF,B
	JEQ	PROCESS_SET_REP_BUFFER
	
	CMP	#>CMD_SET_DATA_BUF,B
	JEQ	PROCESS_SET_DATA_BUFFER
	
	CMP	#>CMD_SEND_MCE,B
	JEQ	PROCESS_SEND_MCE
	
	CMP	#>CMD_SET_TAIL,B
	JEQ	PROCESS_SET_TAIL
	
	;; No match... error?
	BCLR	#COMM_CMD,X:STATUS
	BSET	#COMM_ERR,X:STATUS
	RTS

PROCESS_READ_P
	MOVE	P:(R1),X0
	JMP	PROCESS_READ_EXIT
PROCESS_READ_X
	MOVE	X:(R1),X0
	JMP	PROCESS_READ_EXIT
PROCESS_READ_Y
	MOVE	Y:(R1),X0
	JMP	PROCESS_READ_EXIT
	
PROCESS_READ_EXIT
	;; Store read word in reply buffer
	MOVE 	#>REP_RPAYLOAD,R0
	JSR	PROCESS_SPLIT_X0_XR0
	;; Increment data size
	MOVE	#>1,X0
	MOVE	X0,X:REP_RSIZE
	;; Mark reply-to-send
	BCLR	#COMM_CMD,X:STATUS
	BSET	#COMM_REP,X:STATUS
	RTS

PROCESS_WRITE_P
	MOVE	X1,P:(R1)
	JMP	PROCESS_WRITE_EXIT
PROCESS_WRITE_X
	MOVE	X1,X:(R1)
	JMP	PROCESS_WRITE_EXIT
PROCESS_WRITE_Y
	MOVE	X1,Y:(R1)
	JMP	PROCESS_WRITE_EXIT
	
PROCESS_WRITE_EXIT
	;; Mark reply-to-send
	BCLR	#COMM_CMD,X:STATUS
	BSET	#COMM_REP,X:STATUS
	RTS
	
	
PROCESS_SET_REP_BUFFER
	;; Two data words, representing the upper and lower halfs of the
	;; 32-bit bus address
	CLR	A
	MOVE	#CMD_BUFFER,R0
	MOVE	#REP_BUS_ADDR,R1
	.loop 	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	OR	X0,A		; If there is a 1 in that address, we will find it.
	.endl
	
	;; Yes, reply.
	BCLR	#COMM_CMD,X:STATUS
	BSET	#COMM_REP,X:STATUS
	
	;; Enable / disable replies based on whether that address was 0 or not.
	CMP	#0,A
	JEQ	PROCESS_SET_REP_BUFFER_DISABLE

	BSET	#COMM_REP_ENABLED,X:STATUS
	RTS
	
PROCESS_SET_REP_BUFFER_DISABLE
	BCLR	#COMM_REP_ENABLED,X:STATUS
	RTS
	

PROCESS_SET_DATA_BUFFER
	;; Lots of good stuff in here.
	MOVE	#CMD_BUFFER,R0
	NOP
	NOP
	MOVE	X:(R0)+,X0	; 0
	MOVE	X0,X:QT_BASE_LO	
	MOVE	X:(R0)+,X0
	MOVE	X0,X:QT_BASE_HI

	MOVE	X:(R0)+,X0	; 1
	MOVE	X0,X:QT_BUF_MAX
	MOVE	X:(R0)+,X0	; 

	MOVE	X:(R0)+,X0	; 2
	MOVE	X0,X:QT_BUF_SIZE
	MOVE	X:(R0)+,X0

	MOVE	X:(R0)+,X0	; 3
	MOVE	X0,X:QT_FRAME_SIZE
	MOVE	X:(R0)+,X0

	MOVE	X:(R0)+,X0	; 4
	MOVE	X0,X:QT_INFORM
	MOVE	X:(R0)+,X0

	MOVE	X:(R0)+,X0	; 5
	MOVE	X0,X:TCPR0	;  ->Right into the time-out counter
	MOVE	X:(R0)+,X0
	
	MOVE	X:(R0)+,X0	; 6
	MOVE	X0,X:QT_BUF_HEAD
	MOVE	X:(R0)+,X0
	
	MOVE	X:(R0)+,X0	; 7
	MOVE	X0,X:QT_BUF_TAIL
	MOVE	X:(R0)+,X0
	
	MOVE	X:(R0)+,X0	; 8
	MOVE	X0,X:QT_DROPS
	MOVE	X:(R0)+,X0
	
	;; Reset QT_DEST from QT_BASE.
	JSR	BUFFER_RESET
	
	;; Reset frame counter alert
	MOVE	#>0,X0
	MOVE	X0,X:QT_INFORM_IDX

	;; Yes reply.
	BCLR	#COMM_CMD,X:STATUS
	BSET	#COMM_REP,X:STATUS
	RTS

	
PROCESS_SEND_MCE
	;; The packet data is a command for the MCE.  Send it.
	;; The data should be stored as 128 x 16bit units.
	MOVE	#CMD_BUFFER,R0
	.loop	#128
	MOVE	X:(R0)+,A1		; get hi 16
	ASR	#8,A,B		        ; Shift b2 into B1
	AND	#>$FF,A
	MOVE	A1,X:FO_SEND
	MOVE	B1,X:FO_SEND
	.endl

	NOP
	BCLR	#COMM_CMD,X:STATUS
	BCLR	#COMM_REP,X:STATUS
	RTS

	
PROCESS_SET_TAIL
	;; Update tail index from the first datum
	MOVE	X:CMD_BUFFER,X0
	MOVE	X0,X:QT_BUF_TAIL

	;; No reply.
	BCLR	#COMM_CMD,X:STATUS
	BCLR	#COMM_REP,X:STATUS
	RTS


PROCESS_SPLIT_X0_XR0
	;; Split the 24-bit contents in X0 into 8: and :16 bits, placed into
	;; X:R0 and R0+1 resp.  Advances R0 accordingly.  Trashes A,B.
	MOVE	X0,A0
	EXTRACTU #$010000,A,B
	EXTRACTU #$008010,A,A	; Put
	MOVE	B0,X:(R0)+
	MOVE	A0,X:(R0)+
	RTS

PROCESS_SPLIT_X0_YR0
	;; Split the 24-bit contents in X0 into 8: and :16 bits, placed into
	;; X:R0 and R0+1 resp.  Advances R0 by 2.  Trashes A,B.
	MOVE	X0,A0
	EXTRACTU #$010000,A,B
	EXTRACTU #$008010,A,A	; Put
	MOVE	B0,Y:(R0)+
	MOVE	A0,Y:(R0)+
	RTS

PROCESS_JOIN_XR0_A
	;; Join two 16-bit words at R0,R0+1 into a 32-bit word and
	;; return in A.  Trashes X0.  Advances R0 by 2.
	CLR	A
	MOVE	X:(R0)+,A0
	MOVE	X:(R0)+,X0
	INSERT	#$010010,X0,A
	RTS



;------------------------
CHECK_FOR_DATA
;------------------------
	JCLR	#EF,X:PDRD,CHECK_FOR_DATA_EXIT
	NOP
	NOP
	JCLR	#EF,X:PDRD,CHECK_FOR_DATA_EXIT
	;; The FIFO is non-empty.
	
	;; Read FIFO words into A1.  Reading into A causes weird sign
	;; extension effects.  If we encounter any unexpected data,
	;; reset the FIFO.

	CLR	A		; A0=0
	MOVE	#>MCEREP_BUF,R4
	
	MOVEP	Y:RDFIFO,A1
	AND	#>$00FFFF,A
	MOVE	A1,Y:(R4)+
	CMP	#>$00A5A5,A
	JNE	RESET_FIFO	; Empty the FIFO, and return to main loop.
	
	JCLR	#EF,X:PDRD,*
	NOP
	NOP
	JCLR	#EF,X:PDRD,*
	
	MOVEP	Y:RDFIFO,A1
	AND	#>$00FFFF,A
	MOVE	A1,Y:(R4)+
	CMP	#>$00A5A5,A
	JNE	FIFO_RESYNC	; Sure, give simple resync a chance

	JCLR	#EF,X:PDRD,*
	NOP
	NOP
	JCLR	#EF,X:PDRD,*
	
	MOVEP	Y:RDFIFO,A1
	AND	#>$00FFFF,A
	MOVE	A1,Y:(R4)+
FIFO_RESYNC
	CMP	#>$005A5A,A
	JNE	RESET_FIFO

	JCLR	#EF,X:PDRD,*
	NOP
	NOP
	JCLR	#EF,X:PDRD,*
	
	MOVEP	Y:RDFIFO,A1
	AND	#>$00FFFF,A
	MOVE	A1,Y:(R4)+
	CMP	#>$005A5A,A
	JNE	RESET_FIFO

	;; We made it; read 4 more 16-bit words, which are the packet type and size.
	.loop	#4
	JCLR	#EF,X:PDRD,*
	NOP
	NOP
	JCLR	#EF,X:PDRD,*
	MOVEP	Y:RDFIFO,A1
	AND	#>$00ffff,A
	NOP
	MOVE	A1,Y:(R4)+
	.endl
	
	;; Compute packet divisions
	;; -- sets TOTAL_BUFFS (half-fifos) and LEFT_TO_READ (single fifo reads)
	CLR	A
	MOVE	Y:(MCEREP_BUF+MCEREP_SIZE),A0
	JSR	PACKET_PARTITIONS
	
	;; Is this a data or reply packet?
	CLR	A
	MOVE	Y:(MCEREP_BUF+MCEREP_TYPE),A1
	CMP	#'RP',A
	JEQ	CHECK_FOR_DATA__BUFFER_REPLY
	
	CMP	#'DA',A
	JEQ	CHECK_FOR_DATA__BUFFER_DATA
	
	;; Weird packet.  Make a note.  Reset the FIFO, return.
	MOVE	X:PTYPE_FAILS,A0
	INC	A
	NOP
	MOVE	A0,X:PTYPE_FAILS
	JSR	RESET_FIFO
	JMP	CHECK_FOR_DATA_EXIT
	
	
CHECK_FOR_DATA__BUFFER_REPLY
	BSET	#COMM_MCEREP,X:STATUS
	MOVE	Y:(MCEREP_BUF+MCEREP_SIZE),X0
	MOVE	#(MCEREP_BUF+MCEREP_PAYLOAD),R4
	JSR	CHECK_FOR_DATA__BUFFER_LARGE
	
	;; JSR	TIMER_STORE_NOW
	JMP	CHECK_FOR_DATA_EXIT
	
CHECK_FOR_DATA__BUFFER_DATA
	;; 	Increment data frame counter
	MOVE	X:DA_COUNT,A0
	INC	A
	NOP
	MOVE	A0,X:DA_COUNT
	
	BSET	#COMM_MCEDATA,X:STATUS
	;; Packet size in dwords -> X0
	MOVE	Y:(MCEREP_BUF+MCEREP_SIZE),X0
	;; Dump it to temp buf...
	MOVE	#MCEDATA_BUF,R4
	
	JSR	CHECK_FOR_DATA__BUFFER_LARGE
	;; End marker for debugging; not a protocol signifier.
	MOVE	#$ff1112,X0
	MOVE	X0,Y:(R4)
	
	;; JSR	TIMER_STORE_NOW
	JMP	CHECK_FOR_DATA_EXIT
	
;;; Original, slow, working buffer routine.
CHECK_FOR_DATA__BUFFER
	;; Buffer a set number of words from the FIFO.
	;; 
	;; In:  X0 is the packet size in 32-bit words
	;;      R4 is the pointer into Y memory.
	;;
	;; Out: fills the buffer and advances R4.  Probably trashes some stuff.
	
	.loop   #2
	.loop	X0
	JCLR	#EF,X:PDRD,*
	MOVEP	Y:RDFIFO,A
	AND	#>$00ffff,A
	NOP
	MOVE	A1,Y:(R4)+
	.endl
	NOP
	.endl
	
	;; End marker for debugging; not a protocol signifier.
	MOVE	#$ff1111,X0
	MOVE	X0,Y:(R4)

	RTS
	
CHECK_FOR_DATA_EXIT
	RTS
	
;----------------------------------------------
RESET_FIFO
;----------------------------------------------
	;; Investigate mysterious error data; first one is in A1
	MOVE	X:DEBUG_BUF_IDX,R3
	NOP
	NOP
	MOVE	A0,Y:(R3)+
	MOVE	A1,Y:(R3)+
	CMP	#>$00A5A5,A
	JEQ	RESET_FIFO1
	MOVE	R3,Y:(R3)+
RESET_FIFO1
	NOP
	NOP
	MOVE	Y:RDFIFO,A
	MOVE	A1,Y:(R3)+
	JSET	#EF,X:PDRD,RESET_FIFO1

	MOVE	#>$aa1122,A
	NOP
	MOVE	A1,Y:(R3)+
	MOVE	R3,X:DEBUG_BUF_IDX
	
	;; Increment counter
	MOVE	X:FIFO_FAILS,A0
	INC	A
	NOP
	MOVE	A0,X:FIFO_FAILS
	MOVEP	#%011000,X:PDRD			; clear FIFO RESET* briefly.
	MOVE	#>25000,X0
	.loop	X0
	NOP
	.endl
	MOVEP	#%011100,X:PDRD
	RTS

;;;
;;; Large packet buffering.
;;;
	
;---------------------------
CHECK_FOR_DATA__BUFFER_LARGE
;---------------------------
	;; Buffer all half-fifos by waiting for half full, and streaming.
	MOVE	X:TOTAL_BUFFS,A
	CMP	#0,A
	JEQ	FINISHED_BUFFS

	DO	A1,FINISHED_BUFFS
	;; .loop	X:TOTAL_BUFFS
;; CHECK_WAIT_X
	JSET 	#HF,X:PDRD,*
	;; JSET	#HF,X:PDRD,CHECK_WAIT_X	; Wait for half full+1
	;; NOP
	;; NOP
	;; JSET	#HF,X:PDRD,CHECK_WAIT_X	; Protect against metastability
	.loop	#512
	MOVEP	Y:RDFIFO,Y:(R4)+
	.endl
	NOP
	
FINISHED_BUFFS	
	;; .endl
	;; JSR	TIMER_STORE_NOW
	
	;; If the FIFO is half full, read the remaining words as
	;; quickly as possible.  Otherwise, do a timed read.
	JSET	#HF,X:PDRD,BUFFER_PACKET_SINGLES_TIMED
	
	;; Quick
	.loop	X:LEFT_TO_READ
	MOVEP	Y:RDFIFO,Y:(R4)+
	.endl
	RTS

BUFFER_PACKET_SINGLES_TIMED	
	;;    This is non-polling!  The 50 MHz timer must be set up.
	CLR	A
	CLR	B
	MOVE	X:TCR0,B0		; Store timer value (50 MHz)
	ASR	#2,B,B			; / 4
	.loop	X:LEFT_TO_READ
BUFFER_PACKET_SINGLES_WAIT_X
	MOVE	X:TCR0,A0
	ASR	#2,A,A
	CMP	A,B
	JEQ	BUFFER_PACKET_SINGLES_WAIT_X
 	MOVEP	Y:RDFIFO,Y:(R4)+
	ASL	#0,A,B			; MOVE A,B
	.endl
	NOP
	NOP
	RTS
	
BUFFER_PACKET_SINGLES_POLLED
	.loop	X:LEFT_TO_READ
	JCLR	#EF,X:PDRD,*
 	MOVEP	Y:RDFIFO,Y:(R4)+
	.endl
	NOP
	NOP
	RTS
	


TIMER_STORE_NOW
	;; Not safe!  No bounds checking.
	MOVE	X:TIMER_INDEX,R5
	NOP
	MOVE	X:TCR0,Y0
	MOVE	Y0,Y:(R5)+
	MOVE	R5,X:TIMER_INDEX
	RTS
	

;;;
;;;  UNUSED CODE below here
;;; 
	
	
;;;
;;; Simple buffer for MCE data...
;;; 
READ_MCE_EASY
	JSET	#EF,X:PDRD,READ_MCE_EXIT

	
;;;
;;; Large circular buffer for data from MCE
;;;

READ_MCE
	;; If empty (EF set), bail immediately
	JSET	#EF,X:PDRD,READ_MCE_EXIT

	;; Read up to 512 words, but don't cross tail-1 or the far
	;; side of the buffer.
	MOVE	X:CIRCBUF_HEAD,A
	MOVE	X:CIRCBUF_TAIL,B
	MOVE	A1,R0
	DEC	B
	SUB	A,B
	DEC	B
	NOP
	MOVE	B1,X0		;X0 = tail-1-head
	MOVE	#(CIRCBUF_SIZE+CIRCBUF_START),B
	SUB	B,A		;B = head-edge
	MOVE	#-512,A
	MAX	A,B		;B = max(-512,head-edge)
	MOVE	X0,A
	NEG	A
	MAX	A,B
	NEG	B		;B = min(512, edge-head, tail-1-head)
	
	;; Check half-full; HF will be clear.
	JSET	#HF,X:PDRD,READ_MCE_SINGLES
	NOP
	NOP
	JSET	#HF,X:PDRD,READ_MCE_SINGLES

	.loop	B1
	MOVEP	Y:RDFIFO,Y:(R0)+
	.endl
	JMP	READ_MCE

READ_MCE_SINGLES
	ASR	#24,B,B
READ_MCE_SINGLES_1
	JSET	#EF,X:PDRD,READ_MCE_EXIT
	MOVEP	Y:RDFIFO,Y:(R0)+
	DEC	B
	JEQ	READ_MCE_SINGLES_1

	
READ_MCE_EXIT
	RTS


;;; Buffer inspection:
;;; GET_SIZE (return unwrapped head-tail)
;;; GET_WORD_AT_OFFSET (handle wrapping)
;;; COPY_PACKET to stage it for PCI.
	
	

CIRC_GET_READ_SIZE
	;; Compute number of words in the buffer, return in B1.  Trashes A.
	MOVE	X:CIRCBUF_TAIL,A
	MOVE	X:CIRCBUF_HEAD,B
	SUB	A,B
	.if	<lt>
	ADD	#(CIRCBUF_SIZE),B
	.endi
	RTS
	
CIRC_READ_WORD_AT_OFFSET
	;; Get the word at position CIRCBUF_TAIL+R0.
	MOVE	R0,A
	MOVE	X:CIRCBUF_HEAD,B
	ADD	A,B
	MOVE	#(CIRCBUF_SIZE),A
	CMP	A,B
	;; JLT	CIRC_READ_WORD_AT_OFFSET_OK
	.if	<gt>
	SUB	A,B
	.endi
	MOVE	#(CIRCBUF_START),A
	ADD	A,B
	NOP
	MOVE	B,R4
	NOP
	NOP
	NOP
	MOVE	Y:(R4),B
	RTS

