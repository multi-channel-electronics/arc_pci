	COMMENT *

Auxiliary application area.

See info.asm for versioning and authors.

	*
	PAGE    132     ; Printronix page width - 132 columns
	OPT	CEX	; print DC evaluations

	IF	@CVS(N,*)>=APPLICATION
        WARN    'The boot code is too large and could be overwritten by an Application!'
	ENDIF


;--------------------------------------------
; APPLICATION AREA
;---------------------------------------------
	IF	@SCP("DOWNLOAD","ROM")		; Download via ONCE debugger
	ORG	P:APPLICATION,P:APPLICATION+2
	ENDIF

	IF	@SCP("DOWNLOAD","ONCE")		; Download via ONCE debugger
	ORG	P:APPLICATION,P:APPLICATION
	ENDIF

; starts with no application loaded
; so just reply with an error if we get a GOA command
	
	MOVE	#'REP',X0
	MOVE	X0,X:<DTXS_WD1		; REPly
	MOVE	#'GOA',X0
	MOVE	X0,X:<DTXS_WD2		; echo command sent
	MOVE	#'ERR',X0
	MOVE	X0,X:<DTXS_WD3		; No Application Loaded
	MOVE	#'NAL',X0
	MOVE	X0,X:<DTXS_WD4		; write to PCI memory error;
	JSR	<RESTORE_REGISTERS	
	JSR	<PCI_MESSAGE_TO_HOST
	BCLR	#MODE_APPLICATION,X:<MODE
	JMP	PACKET_IN

		
END_ADR	EQU	@LCV(L)		; End address of P: code written to ROM



