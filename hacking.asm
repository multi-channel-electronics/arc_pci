
QUIET_TRANSFER_SET_R0_PERSISTENT
	;; X0 contains new value
	;; R0 is target address, in X memory.
	;; Sets X:(R0) with value in X0, but also updates vartable in P memory
	;; so that new value can survice a dsp_reset.
	;; Destroys A and B, but that's ok.
	MOVE	X0,X:(R0)
	MOVE	#>VAR_TBL_START,B
	MOVE	R0,A
	ADD	A,B
	MOVE    B,R0
	MOVE	X0,P:(R0)
	JMP	VCOM_EXIT

HACK_HACK
	CMP	#'BUR',A
	MOVE	#>PCI_BURST_SIZE,R0
	JEQ	QUIET_TRANSFER_SET_R0_PERSISTENT

	MOVE	#'MTE',X0
	JMP	VCOM_EXIT_ERROR_X0
	