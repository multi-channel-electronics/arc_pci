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
	;; Set bit to indicate to host that we're in this loop.
	BSET	#DCTR_HF4,X:DCTR
	
	;; Nothing to handle
	BCLR	#COMM_CMD,X:STATUS
	BCLR	#COMM_REP,X:STATUS
	
	;; Init the datagram structure
	JSR	REPLY_BUFFER_INIT
	
	MOVE	#DEBUG_BUF,R0
	MOVE	R0,X0
	MOVE	X0,Y:(R0)
	;; hacking storage
	MOVE	#>TIMER_BUFFER_END,X0
	MOVE	X0,Y:TIMER_BUFFER_END
		
	;; Enable PCI slave receive interrupt to handle commands from host
	BSET	#DCTR_SRIE,X:DCTR
	
	;; Empty the FIFO into a buffer
	MOVE	#>$2000,R3
	
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
	JSR CHECK_FIFO

	;; Should we fake data?
	;; JSR	FAKE_PACKET

	;; LOOP UNTIL host gives up.
	JSET	#DSR_HF2,X:DSR,HACK_LOOP
	
HACK_EXIT
	;; Disable PCI slave receive interrupt
	BCLR	#DCTR_SRIE,X:DCTR
	
	;; Lower flag
	BCLR	#DCTR_HF4,X:DCTR
	RTS
	

CHECK_FIFO
	JCLR	#EF,X:PDRD,RD_LP1
	MOVEP	Y:RDFIFO,Y:(R3)+
	NOP
	NOP
	NOP
	NOP
	NOP

RD_LP1
	MOVE	#>$ffffdd,X0
	MOVE	X0,Y:(R3)
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
	MOVE	#>RB_SIZE,X1
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

PROCESS_REPLY
;; 	CLR	A
;; 	MOVE	X:REP_RSTAT,A
;; 	CMP	#0,A
;; 	JNE	PROCESS_REPLY_1
;; 	RTS

;; PROCESS_REPLY_1
	;; Set destination address
	MOVE	#>REP_BUS_ADDR,R0
	MOVE	#>BURST_DEST_LO,R1
	.loop	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl

	;; Set BLOCK_SIZE, in bytes
	MOVE	#>(RB_SIZE*2),X0
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
	BCLR	#INTA,X:DCTR
	
	JSET	#DSR_HF0,X:DSR,*
	
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
			
			
CMD_READ_CODED		EQU	$11
CMD_WRITE_CODED 	EQU	$12
			
CMD_SEND_MCE		EQU	$21
			
CMD_SEND_STUFF  	EQU	$31
			
CMD_STATUS		EQU	$65
CMD_RECV_MCE		EQU	$66


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
	
PROCESS_PC_CMD_INT
	;; Push all and disable further SRRQ ints
	JSR	SAVE_REGISTERS
	BCLR	#DCTR_SRIE,X:DCTR

	;; Is there data?
	JCLR	#SRRQ,X:DSR,PROCESS_PC_CMD_INT_EXIT
	
	;; Ok, you asked for it.
	;; Read the command word (cmd|length)
	;; MOVEP	X:DRXR,X0
	;; MOVE	#CMD_SIZE,R0
	;; JSR	PROCESS_SPLIT_X0_XR0

	MOVEP	X:DRXR,X0
	MOVE	X0,X:CMD_WORD
	NOP
	NOP
	JCLR	#SRRQ,X:DSR,*
	MOVEP	X:DRXR,X0
	MOVE	X0,X:CMD_SIZE	;size in 32-bit words.
	
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
	MOVE	#DEBUG_BUF,R0
	MOVE	X:CMD_WORD,X0
	MOVE	X0,Y:(R0)+
	MOVE	X:CMD_SIZE,X0
	MOVE	X0,Y:(R0)+
	MOVE	X:CMD_BUFFER,X0
	MOVE	X0,Y:(R0)+
	
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
	;; Init the reply packet, even if there might not be a reply.
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

;;; 	this is wrong, each value is 24 bits and thus spread across two rams.
	;; Pre-load the first packet word into R0 and the second into X0.
	;; MOVE	X:CMD_BUFFER,R1
	;; MOVE	X:(CMD_BUFFER+2),X1
	
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
	JMP	PROCESS_SIMPLE_EXIT
PROCESS_WRITE_X
	MOVE	X1,X:(R1)
	JMP	PROCESS_SIMPLE_EXIT
PROCESS_WRITE_Y
	MOVE	X1,Y:(R1)
	JMP	PROCESS_SIMPLE_EXIT

	
PROCESS_SET_REP_BUFFER
	;; Two data words, representing the upper and lower halfs of the
	;; 32-bit bus address
	MOVE	#CMD_BUFFER,R0
	MOVE	#REP_BUS_ADDR,R1
	.loop 	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl
	
	;; CHECK -- also store in Y mem where we can find it...
	MOVE	#CMD_BUFFER,R0
	MOVE	#>$100,R1
	.loop 	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,Y:(R1)+
	.endl
	
	;; No reply!
	BCLR	#COMM_CMD,X:STATUS
	RTS

PROCESS_SET_DATA_BUFFER
	;; Two data words, representing the upper and lower halfs of the
	;; 32-bit bus address
	MOVE	#CMD_BUFFER,R0
	MOVE	#DATA_BUS_ADDR,R1
	.loop 	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl
	
	MOVE	#(DEBUG_BUF+16),R0
	MOVE	#DATA_BUS_ADDR,R1
	.loop 	#2
	MOVE	X:(R1)+,X0
	MOVE	X0,X:(R0)+
	.endl
	
	;; No reply!
	MOVE	#>0,X0
	MOVE	X0,X:REP_RSTAT
	MOVE	X0,X:REP_RSIZE
	BCLR	#COMM_CMD,X:STATUS
	RTS

PROCESS_SEND_STUFF
	;; No data, no reply
	BSET	#1,X:TRIGGER_FAKE
	
	MOVE	#>0,X0
	MOVE	X0,X:REP_RSTAT
	MOVE	X0,X:REP_RSIZE
	RTS

	
PROCESS_SEND_MCE
	;; The packet data is a command for the MCE.  Send it.
	;; The data should be stored as 128 x 16bit units.
	;; Send the highest order byte first.
	MOVE	#CMD_BUFFER,R0
	.loop	#64
	MOVE	X:(R0)+,X0	        ; store low 16
	
	MOVE	X:(R0)+,A1		; get hi 16
	ASR	#8,A,B		        ; Shift b2 into B1
	AND	#>$FF,A
	MOVE	A1,X:FO_SEND
	MOVE	B1,X:FO_SEND
	
	MOVE	X0,A1
	ASR	#8,A,B		        ; Shift b2 into B1
	AND	#>$FF,A
	MOVE	A1,X:FO_SEND
	MOVE	B1,X:FO_SEND
	.endl

	;; Just acq the request.
	
	
	NOP
	BCLR	#COMM_CMD,X:STATUS
	BSET	#COMM_REP,X:STATUS
	RTS

PROCESS_SIMPLE_EXIT
	;; Mark reply-to-send
	BCLR	#COMM_CMD,X:STATUS
	BSET	#COMM_REP,X:STATUS
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

;;;
;;; Fake MCE data generator!
;;;
;;; BROKEN
	
FAKE_PACKET
	CLR	A
	MOVE	X:TRIGGER_FAKE,A1
	CMP	#0,A
	JEQ	FAKE_PACKET_2
	
;;; JAM
	JSR	PROCESS_REPLY
	MOVE	#>0,X0
	MOVE	X0,X:TRIGGER_FAKE
	RTS

	;; Set destination address
	MOVE	#>REP_BUS_ADDR,R0
	MOVE	#>BURST_DEST_LO,R1
	.loop	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl
	
	MOVE	#>(RB_SIZE*2),X0
	MOVE	X0,X:BLOCK_SIZE
	MOVE	#>REP_BUFFER1,X0
	MOVE	X0,X:XMEM_SRC
	
	;; Trigger writes as master.
	JSR 	BLOCK_TRANSFERX
FAKE_PACKET_2
	RTS


DEBUG_UP
	BSET    #DCTR_HF5,X:DCTR
	RTS
	
DEBUG_DOWN
	BCLR	#DCTR_HF5,X:DCTR
	RTS


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

