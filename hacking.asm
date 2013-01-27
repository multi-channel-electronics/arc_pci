		COMMENT *

	This implementation does communication with the host using PCI
	slave writes only.  It works ok except that the STRQ bit in
	DSR, which indicates that the FIFO is not full, does not seem
	to work.  It is not clear whether this is an issue generally,
	or some kind of race condition that occurs when the channel is
	being aggressively polled and read by the host.

	Maybe hand-shaking would help with such a race condition, but
	then you have to negotiate every 6 words, and that's a burden.
	
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
	MOVE	A0,X:REP_WORD
	MOVE	A0,X:TRIGGER_FAKE
	
	;;
	;; Main loop
	;; 
HACK_LOOP
	;; Check for command from PC.  Sets CMD_WORD.
	JSR	PROCESS_PC_CMD

	;; Should we send a reply?
	JSR	PROCESS_REPLY
	
	;; Should we fake data?
	JSR	FAKE_PACKET

	;; LOOP UNTIL host gives up.
	JSET	#DSR_HF2,X:DSR,HACK_LOOP
	
HACK_EXIT
	BCLR	#DCTR_HF4,X:DCTR
	RTS


;;; PERMANENTER CODE.
	ORG	P:$900,P:$902

PROCESS_REPLY
	CLR	A
	MOVE	X:REP_WORD,A
	CMP	#0,A
	JNE	PROCESS_REPLY_1
	RTS

PROCESS_REPLY_1
	JCLR	#STRQ,X:DSR,*		; wait to be not full
	MOVEP	A1,X:DTXS
	MOVE	X:REP_DATA,X0
	JCLR	#STRQ,X:DSR,*		; wait to be not full
	MOVEP	X0,X:DTXS
	;; Now send buffer.
	MOVE	#REP_BUFFER,R1
	;; Don't call .loop with 0 argument!
	CLR	A
	MOVE	X0,A1
	CMP	#0,A
	JEQ	PROCESS_REPLY_2
	.loop	X0
	JCLR	#STRQ,X:DSR,*		; wait to be not full
	MOVEP	X:(R1)+,X:DTXS
	JCLR	#STRQ,X:DSR,*		; wait to be not full
	MOVEP	X:(R1)+,X:DTXS
	NOP
	NOP
	.endl

PROCESS_REPLY_2
	MOVE	#0,X0
	MOVE	X0,X:REP_WORD	; mark as sent!
	
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
	
CMD_READ_CODED	EQU	$11
CMD_WRITE_CODED EQU	$12

CMD_SEND_MCE	EQU	$21

CMD_TRIGGER_FAKE EQU	$31
	
CMD_STATUS	EQU	$65
CMD_RECV_MCE	EQU	$66



PROCESS_PC_CMD
	;; Is there data?
	JSET	#SRRQ,X:DSR,PROCESS_PC_CMD_1
	RTS
	
PROCESS_PC_CMD_1
	;; Read the command word (cmd|length)
	MOVEP	X:DRXR,X0
	MOVE	#CMD_WORD,R0
	JSR	PROCESS_SPLIT_X0_XR0
	
	;; Read the packet data into a buffer.
	CLR	A
	MOVE	#CMD_BUFFER,R0
	MOVE	X:CMD_DATA,A1
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

	CMP	#CMD_TRIGGER_FAKE,B
	JEQ	PROCESS_TRIGGER_FAKE

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
	MOVE 	#REP_BUFFER,R0
	JSR	PROCESS_SPLIT_X0_XR0
	;; Declare reply packet type and size
	MOVE	X:CMD_WORD,X0
	MOVE	X0,X:REP_WORD
	MOVE	#>1,X0
	MOVE	X0,X:REP_DATA
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


PROCESS_TRIGGER_FAKE
	MOVE	#1,X0
	MOVE	X0,X:TRIGGER_FAKE
	JMP	PROCESS_SIMPLE_EXIT
	
	
PROCESS_SIMPLE_EXIT
	;; Register a simple reply with no error, no data.
	MOVE	X:CMD_WORD,X0
	MOVE	X0,X:REP_WORD
	MOVE	#>0,X0
	MOVE	X0,X:REP_DATA
	RTS


PROCESS_SPLIT_X0_XR0
	;; Split the 24-bit contents in X0 into 8: and :16 bits, placed into
	;; X:R0 and R0+1 resp.  Advances R0 accordingly.  Trashes A,B.
	MOVE	X0,A0
	EXTRACTU #$008010,A,B	; Put
	EXTRACTU #$010000,A,A
	MOVE	B0,X:(R0)+
	MOVE	A0,X:(R0)+
	RTS

PROCESS_SPLIT_X0_YR0
	;; Split the 24-bit contents in X0 into 8: and :16 bits, placed into
	;; X:R0 and R0+1 resp.  Advances R0 accordingly.  Trashes A,B.
	MOVE	X0,A0
	EXTRACTU #$008010,A,B	; Put
	EXTRACTU #$010000,A,A
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

	;; Just dump Y memory directly.  We'll see how awful that is.
	CLR	A
	MOVE	#>1,A0	; size in 32-bit words.

	MOVE	#>CMD_RECV_MCE,X0
	JCLR	#STRQ,X:DSR,*
	MOVEP	X0,X:DTXS
	JCLR	#STRQ,X:DSR,*
	MOVEP	A0,X:DTXS
	
	MOVE	#0,X0
	MOVE	X0,X:TRIGGER_FAKE
	MOVE	X0,R0
	;; Bail!
	;; JMP	FAKE_PACKET_2
	
FAKE_PACKET_1
	MOVE	X:DSR,X0
	JCLR	#STRQ,X:DSR,*
	MOVE	X0,X:DTXS
	;; MOVEP	Y:(R0)+,X:DTXS
	JCLR	#STRQ,X:DSR,*
	MOVEP	Y:(R0)+,X:DTXS
	DEC	A
	JNE	FAKE_PACKET_1
	
FAKE_PACKET_2
	RTS