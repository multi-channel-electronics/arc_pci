		COMMENT *

	This implementation does communication with the host using PCI
	master writes only.
	
	*
	PAGE    132     ; Printronix page width - 132 columns
	OPT	CEX	; print DC evaluations


HACK_ENTRY
	;; Only enter if HF2 is high.
	JCLR	#DSR_HF2,X:DSR,HACK_EXIT
	;; Set bit to indicate to host that we're in this loop.
	BSET	#DCTR_HF4,X:DCTR
	
	;; Clear some states...
	CLR	A
	NOP
	MOVE 	A0,X:CMD_WORD
	MOVE	A0,X:TRIGGER_FAKE

	JSR	REPLY_BUFFER_INIT
	
	;;
	;; Main loop
	;; 
HACK_LOOP
	;; Check for command from PC.  Sets CMD_WORD.
	JSR	PROCESS_PC_CMD

	;; Should we send a reply?
	JSR	PROCESS_REPLY
	
	;; Should we fake data?
	;; JSR	FAKE_PACKET

	;; LOOP UNTIL host gives up.
	JSET	#DSR_HF2,X:DSR,HACK_LOOP
	
HACK_EXIT
	BCLR	#DCTR_HF4,X:DCTR
	RTS

	
REPLY_BUFFER_INIT
	;; initialize header of reply packet.
	MOVE	#REP_BUFFER1,R0
	MOVE	#>1,X0
	MOVE	#>REP_BUFFER_SIZE,A0
	MOVE	X0,X:(R0+RB_VERSION)
	MOVE	A0,X:(R0+RB_SIZE)
	MOVE	#>0,X0
	MOVE	X0,X:REP_RSTAT
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
	CLR	A
	MOVE	X:REP_RSTAT,A
	CMP	#0,A
	JNE	PROCESS_REPLY_1
	RTS

PROCESS_REPLY_1
	;; Set destination address
	MOVE	#>REP_BUS_ADDR,R0
	MOVE	#>BURST_DEST_LO,R1
	.loop	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl
	
	MOVE	#>(REP_BUFFER_SIZE*2),X0
	MOVE	X0,X:BLOCK_SIZE
	MOVE	#>REP_BUFFER1,X0
	MOVE	X0,X:XMEM_SRC

	;; Trigger writes as master.
	JSR 	BLOCK_TRANSFERX

	MOVE	#>0,X0
	MOVE	X0,X:REP_RSTAT	; mark as sent!
	
	;; Raise interrupt and wait for handshake.
	BSET	#INTA,X:DCTR
	
	JCLR	#DSR_HF0,X:DSR,*
	BCLR	#INTA,X:DCTR
	
	JSET	#DSR_HF0,X:DSR,*
	
	RTS

TOGGLED_HANDLER_WHY_DOES_THIS_NOT_WORK_QUESTION
;;; Raise interrupt and wait for HF0 to change state.
	JCLR	#DSR_HF0,X:DSR,INT_WAIT_SET
	
INT_WAIT_CLR
	BSET	#INTA,X:DCTR		; Assert interrupt
	JSET	#DSR_HF0,X:DSR,*
	BCLR	#INTA,X:DCTR
	RTS
INT_WAIT_SET
	BSET	#INTA,X:DCTR		; Assert interrupt
	JCLR	#DSR_HF0,X:DSR,*
	BCLR	#INTA,X:DCTR
	RTS

;;;
;;; Command processing
;;;

;;; CMD identifiers
CMD_READ_P	EQU	1
CMD_READ_X	EQU	2
CMD_READ_Y	EQU	3
	
CMD_WRITE_P	EQU	5
CMD_WRITE_X	EQU	6
CMD_WRITE_Y	EQU	7
	
CMD_SET_REP_BUF	EQU	9
	
CMD_READ_CODED	EQU	$11
CMD_WRITE_CODED EQU	$12

CMD_SEND_MCE	EQU	$21

CMD_SEND_STUFF  EQU	$31
	
CMD_STATUS	EQU	$65
CMD_RECV_MCE	EQU	$66



PROCESS_PC_CMD
	;; Is there data?
	JSET	#SRRQ,X:DSR,PROCESS_PC_CMD_1
	RTS
	
PROCESS_PC_CMD_1
	;; Read the command word (cmd|length)
	MOVEP	X:DRXR,X0
	MOVE	#CMD_SIZE,R0
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
	
PROCESS_PC_CMD_2
	;; Now distribute the command to a handler
	MOVE	X:CMD_WORD,B
	
	;; Pre-load the first packet word into R0 and the second into X0.
	MOVE	X:CMD_BUFFER,R0
	MOVE	X:(CMD_BUFFER+1),X0
	
	CMP	#CMD_READ_P,B
	JEQ	PROCESS_READ_P
	
	CMP	#CMD_READ_X,B
	JEQ	PROCESS_READ_X
	
	CMP	#CMD_READ_Y,B
	JEQ	PROCESS_READ_Y

	CMP	#CMD_WRITE_P,B
	JEQ	PROCESS_WRITE_P
	
	CMP	#CMD_WRITE_X,B
	JEQ	PROCESS_WRITE_X
	
	CMP	#CMD_WRITE_Y,B
	JEQ	PROCESS_WRITE_Y

	CMP	#CMD_SEND_MCE,B
	JEQ	PROCESS_SEND_MCE

	CMP	#CMD_SET_REP_BUF,B
	JEQ	PROCESS_SET_BUFFER
	
	CMP	#CMD_SEND_STUFF,B
	JEQ	PROCESS_SEND_STUFF
	

	;; No match... error?
	RTS

PROCESS_READ_P
	MOVE	P:(R0),X0
	JMP	PROCESS_READ_EXIT
PROCESS_READ_X
	MOVE	X:(R0),X0
	JMP	PROCESS_READ_EXIT
PROCESS_READ_Y
	MOVE	Y:(R0),X0
	;; JMP	PROCESS_READ_EXIT
	;; Fall through
PROCESS_READ_EXIT
	;; Store read word in reply buffer
	MOVE 	#>REP_RPAYLOAD,R0
	JSR	PROCESS_SPLIT_X0_XR0
	;; Declare reply packet type and size
	MOVE	X:CMD_WORD,X0
	MOVE	X0,X:REP_RSTAT
	MOVE	#>1,X0
	MOVE	X0,X:REP_RSIZE
	RTS

PROCESS_WRITE_P
	MOVE	X0,P:(R0)
	JMP	PROCESS_SIMPLE_EXIT
PROCESS_WRITE_X
	MOVE	X0,X:(R0)
	JMP	PROCESS_SIMPLE_EXIT
PROCESS_WRITE_Y
	MOVE	X0,Y:(R0)
	JMP	PROCESS_SIMPLE_EXIT

	
PROCESS_SET_BUFFER
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
	MOVE	#>0,X0
	MOVE	X0,X:REP_RSTAT
	MOVE	X0,X:REP_RSIZE
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
	.loop	#128
	MOVE	X:(R0)+,A1		; b2, b1  (lsb)
	ASR	#8,A,B		        ; Shift b2 into B1
	AND	#>$FF,A
	MOVE	A1,X:FO_SEND
	MOVE	B1,X:FO_SEND
	.endl
	JMP 	PROCESS_SIMPLE_EXIT

PROCESS_SIMPLE_EXIT
	;; Register a simple reply with no error, no data.
	MOVE	X:CMD_WORD,X0
	MOVE	X0,X:REP_RSTAT
	MOVE	#>0,X0
	MOVE	X0,X:REP_RSIZE
	RTS


OLD_PROCESS_SPLIT_X0_XR0
	;; Split the 24-bit contents in X0 into 8: and :16 bits, placed into
	;; X:R0 and R0+1 resp.  Advances R0 accordingly.  Trashes A,B.
	MOVE	X0,A0
	EXTRACTU #$008010,A,B	; Put
	EXTRACTU #$010000,A,A
	MOVE	B0,X:(R0)+
	MOVE	A0,X:(R0)+
	RTS

OLD_PROCESS_SPLIT_X0_YR0
	;; Split the 24-bit contents in X0 into 8: and :16 bits, placed into
	;; X:R0 and R0+1 resp.  Advances R0 accordingly.  Trashes A,B.
	MOVE	X0,A0
	EXTRACTU #$008010,A,B	; Put
	EXTRACTU #$010000,A,A
	MOVE	B0,Y:(R0)+
	MOVE	A0,Y:(R0)+
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
	;; X:R0 and R0+1 resp.  Advances R0 accordingly.  Trashes A,B.
	MOVE	X0,A0
	EXTRACTU #$010000,A,B
	EXTRACTU #$008010,A,A	; Put
	MOVE	B0,Y:(R0)+
	MOVE	A0,Y:(R0)+
	RTS



;;;
;;; Fake MCE data generator!
;;;

FAKE_PACKET
	CLR	A
	MOVE	X:TRIGGER_FAKE,A1
	CMP	#0,A
	JEQ	FAKE_PACKET_2
	
;;; JAM
	JSR	PROCESS_REPLY_1
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
	
	MOVE	#>(REP_BUFFER_SIZE*2),X0
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
