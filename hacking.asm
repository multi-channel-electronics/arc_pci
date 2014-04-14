
		COMMENT *

	This implementation does communication with the host using PCI
	master writes only.
	
	*
	PAGE    132     ; Printronix page width - 132 columns
	OPT	CEX	; print DC evaluations

;----------------------------------------------
; NEW_COMMS_ENTRY
;----------------------------------------------
	
NEW_COMMS_INIT
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
	JSR	INIT_DATAGRAM_BUFFER
	
	JSR	TIMERX_STORE_INIT
	
	;; Enable PCI slave receive interrupt to handle commands from host
	BSET	#DCTR_SRIE,X:DCTR
	
	;; Set bit to indicate to host that we're in this loop.
	BSET	#DCTR_HF4,X:DCTR
	
;
; Main loop
; 
NEW_COMMS_MAIN_LOOP
	;; Interrupt driven: process command in buffer
	JSSET 	#COMM_CMD,X:STATUS,PROCESS_PC_CMD

	;; Should we send a reply?
	JSSET	#COMM_REP,X:STATUS,PROCESS_REPLY
	
	;; FIFO action? -- this might also get called from PCI handler...
	JSR 	CHECK_FOR_DATA

	;; Transmit to host?
	JSSET	#COMM_MCEREP,X:STATUS,PROCESS_MCE_REPLY
	JSSET	#COMM_MCEDATA,X:STATUS,PROCESS_MCE_DATA

	;; Check for timer expiry
	JSSET	#TCF,X:TCSR0,TIMERX_ACTION
	
	;; Issue information?
	JSSET	#QT_FLUSH,X:STATUS,SEND_BUF_INFO
	
	;; LOOP UNTIL host gives up.
	JSET	#DSR_HF2,X:DSR,NEW_COMMS_MAIN_LOOP


;
; Cleanup and exit to standard loop.
;
	;; Don't trigger buffer flush
	BCLR	#QT_FLUSH,X:STATUS
	
	;; Disable PCI slave receive interrupt
	BCLR	#DCTR_SRIE,X:DCTR
	
	;; Lower handshake flag
	BCLR	#DCTR_HF4,X:DCTR

	JMP	PACKET_IN	


;----------------------------------------------
; Timer tick handler and buffer management
;----------------------------------------------
	
TIMERX_ACTION
	MOVEP	#$300201,X:TCSR0	; Clear TOF, TCF, leave timer enabled.
	;; If new data, schedule a flush.
	BCLR	#COMM_BUF_UPDATE,X:STATUS
	JCC	TIMERX_ACTION_OK
	BSET	#QT_FLUSH,X:STATUS	;    schedule inform
TIMERX_ACTION_OK
	RTS


;----------------------------------------------
TIMERX_STORE_INIT
; Set buffer pointer to start of TIMER_BUFFER.
	MOVE	#>TIMER_BUFFER,A0
	NOP
	MOVE	A0,X:TIMER_INDEX
	RTS

;----------------------------------------------
TIMERX_STORE_RAW
; Write current timer value to timer buffer.  Trashes A, R5.
; No interrupt protection.
	MOVE	X:TIMER_SOURCE,A
; Fall-through!
;----------------------------------------------
TIMERX_STORE_A1_RAW
; Write A1 to to timer buffer. Trashes A, R5.
	MOVE	X:TIMER_INDEX,R5
	NOP
	NOP
	MOVE	A1,Y:(R5)+
	MOVE	R5,A
	NOP
	CMP	#>(TIMER_BUFFER_END-1),A
	JGE	TIMERX_STORE_INIT
	MOVE	A1,X:TIMER_INDEX
	RTS

;----------------------------------------------
TIMERX_STORE
; Interrupt-protected version of TIMERX_STORE_RAW.
	BCLR	#DCTR_SRIE,X:DCTR
	JSR	TIMERX_STORE_RAW
	BSET	#DCTR_SRIE,X:DCTR
	RTS
	
;----------------------------------------------
TIMERX_STORE_A1
; Interrupt-protected version of TIMERX_STORE_A1_RAW.
	BCLR	#DCTR_SRIE,X:DCTR
	JSR	TIMERX_STORE_A1_RAW
	BSET	#DCTR_SRIE,X:DCTR
	RTS
	

;
; Utility functions for 32/24-bit <-> 16+16 conversion.
;

PROCESS_SPLIT_X0_XR0
	;; Split the 24-bit contents in X0 into 8: and :16 bits, placed into
	;; X:R0 and R0+1 resp.  Advances R0 accordingly.  Trashes A,B.
	MOVE	X0,A0
	EXTRACTU #$010000,A,B
	EXTRACTU #$008010,A,A	; Put
	MOVE	B0,X:(R0)+
	MOVE	A0,X:(R0)+
	RTS

PROCESS_JOIN_XR0_A
	;; Join two 16-bit words at R0,R0+1 into a 32-bit word and
	;; return in A.  Trashes X0.  Advances R0 by 2.
	CLR	A
	MOVE	X:(R0)+,A0
	MOVE	X:(R0)+,X0
	INSERT	#$010010,X0,A
	RTS

	
;
; PCI burst code
; 
; BLOCK_TRANSFERX and CON_TRANSFERX are replacements for
; BLOCK_TRANSFER and CON_TRANSFER.  They can read/write to X or Y
; memory, depending on X:STATUS[COMM_TFR_YMEM].
	
;----------------------------------------------
BLOCK_TRANSFERX
;----------------------------------------------
;   In:
;   - BURST_DEST_HI:BURST_DEST_LO is PC RAM address (16:16)
;   - BLOCK_SIZE is packet size, in bytes
;   - MEM_SRC is start of data in X or Y memory
;   - STATUS[COMM_TFR_YMEM] is used to determine X or Y 
;  Out:
;   - BLOCK_SIZE will be decremented to zero.
;   - BURST_DEST_HI:LO will be incremented by BLOCK_SIZE
;   - MEM_SRC will be incremented by BLOCK_SIZE/2
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
	MOVE	X:MEM_SRC,A0
	MOVEP	A0,X:DSR0		; DMA source
	ADD	B,A
	DEC	B
	MOVE	A0,X:MEM_SRC		; BURST_SRC += BURST_SIZE/2
	
	MOVEP	B0,X:DCO0		; DMA length = BURST_SIZE/2 - 1

	;; Transfer from X or Y mem?
	JSET	#COMM_TFR_YMEM,X:STATUS,BLOCK_TRANSFERX1_YMEM
	
BLOCK_TRANSFERX1_XMEM
	MOVEP	#$8EFA50,X:DCR0		; X to X
	JMP	BLOCK_TRANSFERX_PCI
	
BLOCK_TRANSFERX1_YMEM
	MOVEP	#$8EFA51,X:DCR0		; X to Y
	JMP	BLOCK_TRANSFERX_PCI
	

BLOCK_TRANSFERX_PCI
	MOVE	#>$7,X0			; Memory write
	MOVE	#BURST_DEST_LO,R0	; RAM address
	JSR	PCI_GO			; Initiate PCI burst

BLOCK_TRANSFERX_PCI_WAIT
	JSR	CHECK_FOR_DATA
	JCLR	#MARQ,X:DPSR,BLOCK_TRANSFERX_PCI_WAIT
	
	;; Restore R0, since CHECK_FOR_DATA probably trashed it.
	MOVE	#BURST_DEST_LO,R0	; RAM address

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



;----------------------------------------------
CON_TRANSFERX
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

	CLR	A
	MOVE	X:BLOCK_SIZE,A1	        ; A1 = BLOCK_SIZE
	CMP	#0,A			; Still bytes to transfer?
	JNE	CON_TRANSFERX0
	RTS

CON_TRANSFERX0
	;; Maximum size of a DMA/PCI burst is 256 bytes,
	;; but latency clock determines the ideal value.
	MOVE	X:PCI_BURST_SIZE,B	; B1 = burst size (256)

	CMP	B,A			; A ? B
	JGE	<CON_TRANSFERX1		; jump if A >= B
	MOVE	A,B			; This only moves A1,B1.
CON_TRANSFERX1
	;; Now burst size B <= block size A.
	SUB	B,A			; A -= B
	ADD	#0,B			; Clear carry bit
	MOVE	A,X:BLOCK_SIZE		; Updated BLOCK_SIZE
	MOVE	B,X:BURST_SIZE		; BURST_SIZE ;= round32(min(BLOCK_SIZE,$100))
	ASR	#25,B,B			; B0 = # of 16 bit words

	;; Setup DMA from BURST_SRC to PCI tx
	MOVE	X:YMEM_DEST,A0
	NOP
	MOVE	A0,X:DDR0		; DMA dest'n
	MOVEP	#>DRXR,X:DSR0		; DMA source
	ADD	B,A
	DEC	B
	MOVE	A0,X:YMEM_DEST		; YMEM_DEST += BURST_SIZE/2
	
	MOVEP	B0,X:DCO0		; DMA length = BURST_SIZE/2 - 1

 	;; DMA go
 	MOVEP	#$8EEAC4,X:DCR0

CON_TRANSFERX_PCI
	MOVE	#>$6,X0			; Memory write
	MOVE	#BURST_SRC_LO,R0	; RAM address
	JSR	PCI_GO			; Initiate PCI burst
	
	;; Watch for FIFO action
CON_TRANSFERX_PCI_WAIT
	JSR	CHECK_FOR_DATA

	;; Wait for completion
	JCLR	#MARQ,X:DPSR,CON_TRANSFERX_PCI_WAIT
	
	;; Restore R0, since CHECK_FOR_DATA probably trashed it.
	MOVE	#BURST_SRC_LO,R0	; RAM address

	;; Check for errors:
	JCLR	#MDT,X:DPSR,CON_TRANSFERX_HANDLE_ERRORS
	
	CLR	B
	MOVE	X:BURST_SIZE,B0		; All bytes were transferred
	JSR	ADD_HILO_ADDRESS	; Update source address
	JMP	CON_TRANSFERX		; Next burst in block

CON_TRANSFERX_HANDLE_ERRORS
	;; Set PCIDMA_* flags; trashes A only	
	JSR	PCI_ERROR_CLEAR
	
	BCLR	#PCIDMA_RESTART,X:STATUS ; Test and clear
	JCS	CON_TRANSFERX_PCI	; Restart PCI burst

	BCLR	#PCIDMA_RESUME,X:STATUS	; Test and clear
	JCC	CON_TRANSFERX		; Error but no error? Redo this burst.

	;; Update the PCI burst size and burst again.
	JSR	PCI_RECOVER_COUNT	; Get transferred byte count in A.
	JSR	PCI_UPDATE_R0
	JMP	CON_TRANSFERX_PCI
	


;;;
;;; New comms protocol implementation
;;; 

	
;----------------------------------------------
INIT_DATAGRAM_BUFFER
;----------------------------------------------
; Initialize header of reply packet.
	MOVE	#0,X0
	MOVE	#>DGRAM_BUFFER,R0
	.loop	#DG__SIZE
	MOVE	X0,X:(R0)+
	.endl
	MOVE	#>DG_VERS_CODE,X0
	MOVE	#>(RB_REP_SIZE/2),X1
	MOVE	X0,X:DGRAM_VERSION
	MOVE	X1,X:DGRAM_SIZE
	MOVE	#>DGRAM_FWREV,R0
	MOVE	X:REV_NUMBER,X0
	JSR	PROCESS_SPLIT_X0_XR0
	RTS
	

;----------------------------------------------
COPY_DATAGRAM_TO_HOST
;----------------------------------------------
; Caller should have already loaded the packet buffer, especially:
;   DGRAM_TYPE
;   DGRAM_SIZE
; This routine DMAs the data into RAM and hand-shakes and interrupt
; with the driver.
	
	;; If reply channel is not configured, bail.
	JSET	#COMM_REP_ENABLED,X:STATUS,COPY_DATAGRAM_TO_HOST1
	RTS
	
COPY_DATAGRAM_TO_HOST1
	;; Set destination address...
	MOVE	#>REP_BUS_ADDR,R0
	MOVE	#>BURST_DEST_LO,R1
	.loop	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl

	;; Set BLOCK_SIZE, in bytes
	MOVE	X:DGRAM_SIZE,A
	ASL	#2,A,A
	NOP
	MOVE	A1,X0
	MOVE	X0,X:BLOCK_SIZE
	;; Source is X:DGRAM_BUFFER
	MOVE	#>DGRAM_BUFFER,X0
	MOVE	X0,X:MEM_SRC
	BCLR	#COMM_TFR_YMEM,X:STATUS

	;; Last chance, check the FO input
	JSR	CHECK_FOR_DATA
	
	;; Trigger writes as master.
	JSR 	BLOCK_TRANSFERX
	
	;; Raise interrupt and wait for handshake.
	BSET	#INTA,X:DCTR
	JCLR	#DSR_HF0,X:DSR,*
	
	BCLR	#INTA,X:DCTR
	JSET	#DSR_HF0,X:DSR,*
	RTS



;----------------------------------------------
PROCESS_REPLY
;----------------------------------------------
; Create a Datagram for a DSP reply and send it.  User has, previously,
; put some data in the payload.
	;; Mark the packet type and size
	MOVE	#>DG_TYPE_DSP_REP,A1
	MOVE	#>(RB_REP_SIZE/2),X1
	MOVE	A1,X:DGRAM_TYPE
	MOVE	X1,X:DGRAM_SIZE
	
	;; Copy
	JSR	COPY_DATAGRAM_TO_HOST
	
	;; Mark as sent
	BCLR	#COMM_REP,X:STATUS
	JSR	TIMERX_STORE
	RTS

;----------------------------------------------
PROCESS_MCE_REPLY
;----------------------------------------------
; Copy MCE reply into X mem and send to PC.
	MOVE	#>$b10000,A
	JSR	TIMERX_STORE_A1
	JSR	TIMERX_STORE
	
	;; Copy data into the X-mem reply buffer, including type and size dwords.
	MOVE	#(DGRAM_DATA),R0
	MOVE	#(MCEREP_BUF+MCEREP__TYPE),R3
	MOVE	Y:(MCEREP_BUF+MCEREP__SIZE),Y1
	.loop	#4
	MOVE	Y:(R3)+,Y0
	MOVE	Y0,X:(R0)+
	.endl
	.loop 	Y1
	MOVE	Y:(R3)+,Y0
	MOVE	Y0,X:(R0)+
	MOVE	Y:(R3)+,Y0
	MOVE	Y0,X:(R0)+
	.endl

	JSR	TIMERX_STORE
	
	;; Mark the packet type and size
	MOVE	#>DG_TYPE_MCE_REP,A1
	MOVE	#>(RB_MCE_SIZE/2),X1 ; size in 32-bit words.
	MOVE	A1,X:DGRAM_TYPE
	MOVE	X1,X:DGRAM_SIZE
	
	;; Copy
	JSR	COPY_DATAGRAM_TO_HOST
	
	;; Mark as sent
	BCLR	#COMM_MCEREP,X:STATUS
	JSR	TIMERX_STORE
	RTS
	

;----------------------------------------------
PROCESS_MCE_DATA
;----------------------------------------------
; Copy data frame to next PC buffer location.  Increment buffer
; pointers and possibly schedule an information (QT_FLUSH).
	MOVE	#>$b20000,A
	JSR	TIMERX_STORE_A1
	JSR	TIMERX_STORE
	
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
	MOVE	X:QT_FRAME_SIZE,A
	NOP
	MOVE	A1,X:BLOCK_SIZE
	
	;; Set destination address
	MOVE	#>QT_DEST_LO,R0
	MOVE	#>BURST_DEST_LO,R1
	.loop	#2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl

	;; 
	MOVE	#>MCEDATA_BUF,X0
	MOVE	X0,X:MEM_SRC
	BSET	#COMM_TFR_YMEM,X:STATUS ; DMA from Y-mem

	;; Trigger writes as master.
	JSR 	BLOCK_TRANSFERX
	
	;; Update buffer pointers
	JSR	BUFFER_INCR_MULTI

PROCESS_MCE_DATA__DONE
	;; Mark as handled
	JSR	TIMERX_STORE
	BCLR	#COMM_MCEDATA,X:STATUS
	
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
	BCLR	#COMM_BUF_UPDATE,X:STATUS ; So we don't trigger a timer int too
PROCESS_MCE_DATA__DONE2
	RTS
	
PROCESS_MCE_DATA__DROP_FRAME
	MOVE	X:QT_DROPS,A
	ADD	#1,A
	NOP
	MOVE	A,X:QT_DROPS
	JMP 	PROCESS_MCE_DATA__DONE

	
;----------------------------------------
BUFFER_RESET_MULTI
;----------------------------------------
; Reset pointers to the very start of the buffer.
	MOVE	#>0,X0
	MOVE	#>QT_BLOCKS,X1
	MOVE	X0,X:QT_BUF_HEAD
	MOVE	X1,X:QT_BLOCK_PTR
	JMP	BUFFER_SET_MULTI_BLOCK
	
;----------------------------------------
BUFFER_SET_NEXT_MULTI_BLOCK
;----------------------------------------
	MOVE	X:QT_BLOCK_PTR,A
	ADD	#>QT_BLOCK___SIZE,A
	NOP
	MOVE	A,X:QT_BLOCK_PTR
	;; Fall-through...

;----------------------------------------
BUFFER_SET_MULTI_BLOCK
;----------------------------------------
; Set QT_DEST to the address of the block described at QT_BLOCK_PTR
	MOVE	X:QT_BLOCK_PTR,R0
	MOVE	#QT_DEST_LO,R1
	NOP
	MOVE	X:(R0+QT_BLOCK__ADDR+0),X0
	MOVE	X0,X:(R1)
	MOVE	X:(R0+QT_BLOCK__ADDR+1),X0
	MOVE	X0,X:(R1+1)
	RTS
	
;----------------------------------------
BUFFER_INCR_MULTI
;----------------------------------------
; Increment the buf_head index (possibly wrapping it back to 0).
; Set up QT_DEST_LO to point to the next RAM location, which may
; be in a different RAM block.
	MOVE	X:QT_BUF_HEAD,A		; If head + 1 == max
	ADD	#1,A			; 
	MOVE	X:QT_BUF_MAX,B		;	
	CMP	A,B			; 
	JLE	BUFFER_RESET_MULTI	;	head = 0
					; else
	MOVE	A,X:QT_BUF_HEAD		;	head = head + 1

	;; Compare head to the indices supported by the current block
	MOVE	X:QT_BLOCK_PTR,R0
	NOP
	NOP
	MOVE	X:(R0+QT_BLOCK__END_IDX),B
	NOP
	CMP	A,B		; (block_end [?] head)
	JLE	BUFFER_SET_NEXT_MULTI_BLOCK
	
	CLR	B
	MOVE	X:QT_BUF_SIZE,B0
	MOVE	#QT_DEST_LO,R0
	JSR	ADD_HILO_ADDRESS	; QT_DEST += QT_BUF_SIZE	
		
	RTS


;
; Other activities
;
	
;----------------------------------------
SEND_BUF_INFO
;----------------------------------------
; Inform the the PC of the current data buffer state.
;
	MOVE	#>$b80000,A
	JSR	TIMERX_STORE_A1
	JSR	TIMERX_STORE
	
	;; Load head index into first data field, pad to 32 bits
	MOVE	X:QT_BUF_HEAD,X0
	MOVE	X0,X:DGRAM_DATA
	MOVE	#0,X0
	MOVE	X0,X:(DGRAM_DATA+1)
	
	;; Mark the packet type and size
	MOVE	#>DG_TYPE_BUF_INF,A1
	MOVE	#>(RB_INF_SIZE/2),X1 ; size in 32-bit words.
	MOVE	A1,X:DGRAM_TYPE
	MOVE	X1,X:DGRAM_SIZE
	
	JSR	COPY_DATAGRAM_TO_HOST
	
	BCLR	#QT_FLUSH,X:STATUS
	JSR	TIMERX_STORE
	RTS
	


;;;
;;; Command processing
;;;

;------------------------
BUFFER_PC_CMD_INT_HANDLER
;------------------------
; Interrupt handler for DRXR.  Reads command from PC, by polling.
	
	;; Push all and disable further SRRQ ints
	JSR	SAVE_REGISTERS	; This does not save all the registers...
	BCLR	#DCTR_SRIE,X:DCTR
	
	;; Mark entry
	MOVE	#>$CC0000,A1
	JSR	TIMERX_STORE_A1_RAW
	JSR	TIMERX_STORE_RAW
	
	;; Debug flag
	BSET	#DCTR_HF3,X:DCTR
	
	;; Is there data?
	JCLR	#SRRQ,X:DSR,BUFFER_PC_CMD_INT_HANDLER_EXIT
	
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
	
	;; Don't call .loop with 0 argument!
	CMP	#0,A
	JEQ	BUFFER_PC_CMD_INT_HANDLER_OK
	
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
	
BUFFER_PC_CMD_INT_HANDLER_OK
	;; Mark cmd-to-process
	BSET	#COMM_CMD,X:STATUS

BUFFER_PC_CMD_INT_HANDLER_EXIT
	;; Mark exit
	JSR	TIMERX_STORE_RAW
	
	;; Re-enable int and pop all
	BSET	#DCTR_SRIE,X:DCTR
	BCLR	#DCTR_HF3,X:DCTR
	JSR	RESTORE_REGISTERS
	RTI

	
;------------------------
PROCESS_PC_CMD
;------------------------
; Parse the command from host and pass to a specialized processor.
;
	MOVE	#>$cd0000,A1
	JSR	TIMERX_STORE_A1

	;; Init the reply packet for success.
	MOVE	#>DG_TYPE_DSP_REP,A1
	MOVE	X:CMD_WORD,B	; this will be used in the switch below.
	MOVE	A1,X:DGRAM_TYPE	; type is "dsp reply"
	MOVE	B1,X:REP_RCMD	; copy of command word
	MOVE	B0,X:REP_RSTAT	; status = 0
	MOVE	B0,X:REP_RSIZE	; data size = 0
	
	MOVE	B,A
	JSR	TIMERX_STORE_A1
	JSR	TIMERX_STORE
	
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
	
	CMP	#>CMD_SET_DATA_BUF_MULTI,B
	JEQ	PROCESS_SET_DATA_BUFFER_MULTI
	
	CMP	#>CMD_SEND_MCE,B
	JEQ	PROCESS_SEND_MCE
	
	CMP	#>CMD_POST_MCE,B
	JEQ	PROCESS_POST_MCE
	
	CMP	#>CMD_SET_TAIL_INF,B
	JEQ	PROCESS_SET_TAIL_INF
	
	;; No match... error?
	BCLR	#COMM_CMD,X:STATUS
	BSET	#COMM_ERR,X:STATUS
	
	JSR	TIMERX_STORE
	RTS


;----------------------------------------------
; READ and WRITE MEMORY command handling
;----------------------------------------------
; PROCESS_[READ|WRITE]_[P|X|Y] are called by PROCESS_PC_CMD.
; They assume that R1 is the pointer for read/write, and for writes
; that X1 contains the data.
	
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
	JSR	TIMERX_STORE
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
	JSR	TIMERX_STORE
	RTS
	
;----------------------------------------------
PROCESS_SET_REP_BUFFER
;----------------------------------------------
; Store the PC reply buffer bus address, and enable replies.
;
; Format of the command payload:
;	CMD_BUFFER+0   lo 16 bits | Buffer bus address
;	CMD_BUFFER+1   hi 16 bits |
;
; Triggers a reply to host.
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
	JSR	TIMERX_STORE
	
	;; Enable / disable replies based on whether that address was 0 or not.
	CMP	#0,A
	JEQ	PROCESS_SET_REP_BUFFER_DISABLE

	BSET	#COMM_REP_ENABLED,X:STATUS
	RTS
	
PROCESS_SET_REP_BUFFER_DISABLE
	BCLR	#COMM_REP_ENABLED,X:STATUS
	RTS
	

;-------------------------------
PROCESS_SET_DATA_BUFFER_MULTI
;-------------------------------
; Store the PC data frame buffer information.  Resets all frame data
; counters and pointers.
;
;
; Format of the command payload:
;	CMD_BUFFER+0   Total frame container count (max 65535)
;	CMD_BUFFER+2   Frame container size (max 65535)
;	CMD_BUFFER+4   Frame size (max 65535)
;	CMD_BUFFER+6   Number of contiguous RAM blocks
;	CMD_BUFFER+8   Block data; 6 words per block.
;
;	BLOCK_START+0  lo 16  | Buffer bus address
;	BLOCK_START+1  hi 16  |
;	BLOCK_START+2  First index of buffer
;	BLOCK_START+4  Last index of buffer + 1
;
	MOVE	#>CMD_BUFFER,R0
	NOP
	NOP
	MOVE	X:(R0)+,X0	; 0
	MOVE	X0,X:QT_BUF_MAX
	MOVE	X:(R0)+,X0

	MOVE	X:(R0)+,X0	; 1
	MOVE	X0,X:QT_BUF_SIZE
	MOVE	X:(R0)+,X0

	MOVE	X:(R0)+,X0	; 2
	MOVE	X0,X:QT_FRAME_SIZE
	MOVE	X:(R0)+,X0

	;; Number of RAM blocks (1)
	MOVE	X:(R0)+,X0	; 3
	MOVE	X0,X:QT_N_BLOCK
	MOVE	X:(R0)+,X0
	
	;; Now loop over num_buffers and store the block addresses and
	;; final indices.
	
	MOVE	#>QT_BLOCKS,R1
	MOVE	X:QT_N_BLOCK,X1
	NOP

	.loop	X1
	
	MOVE	X:(R0)+,X0	; BUF+0
	MOVE	X0,X:(R1)+	;  addr_lo
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+	;  addr_hi

	MOVE	X:(R0)+,X0	; BUF+1
	MOVE	X:(R0)+,X0	; 
	
	MOVE	X:(R0)+,X0	; BUF+2
	MOVE	X0,X:(R1)+	;  end_idx
	MOVE	X:(R0)+,X0	; 
	
	.endl
	
	;; Clear stuff.
	MOVE	#>0,X0
	MOVE	#>1,X1
	MOVE	X0,X:QT_BUF_HEAD
	MOVE	X0,X:QT_BUF_TAIL
	MOVE	X0,X:QT_DROPS
	MOVE	X0,X:QT_INFORM_IDX
	MOVE	X1,X:QT_INFORM
	
	;; Reset QT_DEST.
	JSR	BUFFER_RESET_MULTI
	
	;; Yes reply.
	BCLR	#COMM_CMD,X:STATUS
	BSET	#COMM_REP,X:STATUS
	JSR	TIMERX_STORE
	RTS

	
;-------------------------------
PROCESS_SEND_MCE
;-------------------------------
; The command includes an MCE command packet; send it to the MCE
; Data are stored as 128 x 16bit units, starting at X:CMD_BUFFER.
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
	BSET	#COMM_REP,X:STATUS
	JSR	TIMERX_STORE
	RTS

	
;-------------------------------
PROCESS_POST_MCE
;-------------------------------
; The command indicates that an MCE command has been placed in memory
; and should be copied to the MCE.  The bus address is at X:CMD_BUFFER.
;
	MOVE	#>CMD_BUFFER,R0
	MOVE	#>BURST_SRC_LO,R1
	NOP
	.loop   #2
	MOVE	X:(R0)+,X0
	MOVE	X0,X:(R1)+
	.endl
	
	MOVE	#>MCECMD_BUF,X0
	MOVE	X0,X:YMEM_DEST
	MOVE	#>256,X0
	MOVE	X0,X:BLOCK_SIZE
	
	JSR	CON_TRANSFERX

	JSR	TIMERX_STORE
	
	;; The packet data is a command for the MCE.  Send it.
	;; The data should be stored as 128 x 16bit units.
	MOVE	#MCECMD_BUF,R3
	.loop	#128
	MOVE	Y:(R3)+,A1		; get hi 16
	ASR	#8,A,B		        ; Shift b2 into B1
	AND	#>$FF,A
	MOVE	A1,X:FO_SEND
	MOVE	B1,X:FO_SEND
	.endl

	NOP
	BCLR	#COMM_CMD,X:STATUS
	BSET	#COMM_REP,X:STATUS
	JSR	TIMERX_STORE
	RTS
	

;-------------------------------
PROCESS_SET_TAIL_INF
;-------------------------------
; Host has sent a buffer information packet; update buffer state.
;
;	CMD_BUFFER+0	New circular buffer tail index.
;	CMD_BUFFER+2	New value for QT_INFORM.
;
	MOVE	#>CMD_BUFFER,R0
	NOP
	NOP
	MOVE	X:(R0)+,X0
	MOVE	X0,X:QT_BUF_TAIL
	MOVE	X:(R0)+,X0
	
	;; Update inform trigger?  Only if non-zero
	CLR	A
	MOVE	X:(R0)+,X0
	CMP	X0,A
	JEQ	PROCESS_SET_TAIL_INF_1
	MOVE	X0,X:QT_INFORM
	MOVE	A1,X:QT_INFORM_IDX
PROCESS_SET_TAIL_INF_1
	MOVE	X:(R0)+,X0
	
	;; Yes, reply.  Everything replies.
	BCLR	#COMM_CMD,X:STATUS
	BSET	#COMM_REP,X:STATUS
	JSR	TIMERX_STORE
	RTS


;----------------------------------------
RESET_MCE_COMMS
;----------------------------------------
; Vector interrupt to send special reset signal to MCE rx.
	BSET	#SCLK,X:PDRE		; Enable special command mode
	NOP
	NOP
	MOVE	#$FFF000,R0		; Memory mapped address of transmitter
	MOVE	#$10000B,X0		; Special command to reset controller
	MOVE	X0,X:(R0)
	REP	#6			; Wait for transmission to complete
	NOP
	BCLR	#SCLK,X:PDRE		; Disable special command mode
	RTI


;------------------------
CHECK_FOR_DATA
;------------------------
	BSET	#DCTR_HF5,X:DCTR
	JCLR	#EF,X:PDRD,CHECK_FOR_DATA_EXIT
	NOP
	NOP
	JCLR	#EF,X:PDRD,CHECK_FOR_DATA_EXIT
	;; The FIFO is non-empty.
	MOVE	#>$cf0000,A1
	JSR	TIMERX_STORE_A1
	JSR	TIMERX_STORE
	
	;; Read FIFO words into A1.  Reading into A causes weird sign
	;; extension effects.  If we encounter any unexpected data,
	;; reset the FIFO.

	CLR	A		; A0=0
	MOVE	#>MCEHDR,R0
	
	MOVEP	Y:RDFIFO,A1
	AND	#>$00FFFF,A
	MOVE	A1,X:(R0)+
	CMP	#>$00A5A5,A
	JNE	RESET_FIFO	; Empty the FIFO, and return to main loop.
	
	JCLR	#EF,X:PDRD,*
	
	MOVEP	Y:RDFIFO,A1
	AND	#>$00FFFF,A
	MOVE	A1,X:(R0)+
	CMP	#>$00A5A5,A
	JNE	RESET_FIFO
	
	JCLR	#EF,X:PDRD,*
	
	MOVEP	Y:RDFIFO,A1
	AND	#>$00FFFF,A
	MOVE	A1,X:(R0)+
	CMP	#>$005A5A,A
	JNE	RESET_FIFO

	JCLR	#EF,X:PDRD,*
	
	MOVEP	Y:RDFIFO,A1
	AND	#>$00FFFF,A
	MOVE	A1,X:(R0)+
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
	MOVE	A1,X:(R0)+
	.endl
	
	JSR	TIMERX_STORE
	
	;; Compute packet divisions
	;; -- sets TOTAL_BUFFS (half-fifos) and LEFT_TO_READ (single fifo reads)
	CLR	A
	MOVE 	X:MCEHDR_SIZE,A0
	JSR	PACKET_PARTITIONS
	
	JSR	TIMERX_STORE
	
	CLR	A
	MOVE    X:MCEHDR_TYPE,A1
	
	;; Is this a data or reply packet?
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
	
CHECK_FOR_DATA_EXIT
	BCLR	#DCTR_HF5,X:DCTR
	RTS
	
	
CHECK_FOR_DATA__BUFFER_REPLY
	;; Cue up data dump buffer...
	MOVE	#>MCE_PACKET_DUMP,R4

	;; Test for buffer in use / set reply present
	JSET	#COMM_MCEREP,X:STATUS,CHECK_FOR_DATA__BUFFER_REPLY1
	
	;; Ok, you can keep it.
	BSET	#COMM_MCEREP,X:STATUS
	
	;; Copy type and size from header into reply structure
	MOVE	#>MCEHDR_TYPE,R0
	MOVE	#>MCEREP_BUF,R4
	.loop	#4
	MOVE	X:(R0)+,X0
	NOP
	MOVE	X0,Y:(R4)+
	.endl
	
	;; Now buffer rest of packet to R4 -> MCEREP_BUF+MCEREP__PAYLOAD
	
CHECK_FOR_DATA__BUFFER_REPLY1
	MOVE	X:MCEHDR_SIZE,X0
	JSR	CHECK_FOR_DATA__BUFFER_LARGE
	
	JMP	CHECK_FOR_DATA_EXIT

	
CHECK_FOR_DATA__BUFFER_DATA
	;; Cue up data dump buffer...
	MOVE	#>MCE_PACKET_DUMP,R4

	;; Test for buffer in use
	JSET	#COMM_MCEDATA,X:STATUS,CHECK_FOR_DATA__BUFFER_DATA1
	
	;; Ok, you can keep it.
	BSET	#COMM_MCEDATA,X:STATUS
	MOVE	#MCEDATA_BUF,R4
	
CHECK_FOR_DATA__BUFFER_DATA1
	;; 	Increment data frame counter
	MOVE	X:DA_COUNT,A0
	INC	A
	NOP
	MOVE	A0,X:DA_COUNT
	
	;; Packet size in dwords -> X0
	MOVE	X:MCEHDR_SIZE,X0
	
	JSR	CHECK_FOR_DATA__BUFFER_LARGE
	
	;; End marker for debugging; not a protocol signifier.
	MOVE	#$af000,X0
	MOVE	X0,Y:(R4)
	
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


	
;----------------------------------------------
RESET_FIFO
;----------------------------------------------
	;; Investigate mysterious error data; first one is in A1
	MOVE	X:DEBUG_BUF_IDX,R3
	NOP
	NOP
RESET_FIFO1
	MOVE	A1,Y:(R3)+
	JCLR	#EF,X:PDRD,RESET_FIFO2
	MOVE	Y:RDFIFO,A
	JMP	RESET_FIFO1
	
RESET_FIFO2
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
	
	BCLR	#DCTR_HF5,X:DCTR
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

	MOVE	#>$aa0000,Y0

	;; Timing trap.  Bytes arrive at 25 MHz.  A half-fifo should fill
	;; within 512*4 = 2**11 ticks of the 50 MHz clock.  Don't wait
	;; longer than 2**13 ticks before giving up.
	
	MOVE	#>$1,B
	ASR	#12,B,B		;B = 2**(24-12) = 2**12
	
	DO	X:TOTAL_BUFFS,FINISHED_BUFFS
	CLR	A
	MOVE	Y0,Y:(R4)
BLOCK_FOR_HALF
	INC	A
	CMP	A,B
	JLE	GO_FOR_HALF	; I give up.
	JSET 	#HF,X:PDRD,BLOCK_FOR_HALF
	NOP
	NOP
	JSET 	#HF,X:PDRD,BLOCK_FOR_HALF
GO_FOR_HALF
	.loop	#512
	MOVEP	Y:RDFIFO,Y:(R4)+
	.endl
	
	NOP
	JSR	TIMERX_STORE
	NOP
	
FINISHED_BUFFS	
	;; .endl
	
	;; If the FIFO is half full, read the remaining words as
	;; quickly as possible.  Otherwise, do a timed read.
	JSET	#HF,X:PDRD,BUFFER_PACKET_SINGLES_TIMED
	
	;; Quick
	.loop	X:LEFT_TO_READ
	MOVEP	Y:RDFIFO,Y:(R4)+
	.endl
	
	JSR	TIMERX_STORE
	
	MOVE	#>$ab0000,Y0
 	MOVE	Y0,Y:(R4)	        ; Where was we?
	RTS

BUFFER_PACKET_SINGLES_TIMED	
	;;    This is non-polling!  The 50 MHz timer must be set up.
	CLR	A
	CLR	B
	MOVE	X:TCR0,B0		; Store timer value (50 MHz)
	MOVE	#>$ac0000,Y0
	ASR	#2,B,B			; / 4
	.loop	X:LEFT_TO_READ
BUFFER_PACKET_SINGLES_WAIT_X
	MOVE	X:TCR0,A0
	ASR	#2,A,A
	CMP	A,B
	JEQ	BUFFER_PACKET_SINGLES_WAIT_X
 	MOVEP	Y:RDFIFO,Y:(R4)+
 	MOVE	Y0,Y:(R4)	        ; Where was we?
	ASL	#0,A,B			; MOVE A,B
	.endl
	NOP
	NOP
	JSR	TIMERX_STORE
	RTS
	
BUFFER_PACKET_SINGLES_POLLED
	.loop	X:LEFT_TO_READ
	JCLR	#EF,X:PDRD,*
 	MOVEP	Y:RDFIFO,Y:(R4)+
	.endl
	NOP
	NOP
	RTS
