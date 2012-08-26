
		COMMENT *

Hacking section of the pci card code.

See info.asm for versioning and authors.

	*
	PAGE    132     ; Printronix page width - 132 columns
	OPT	CEX	; print DC evaluations

	;; CON adjust...

X_CON_TRANSFER_PCI_CONT
	;; Wait for completion
	BSET	#PCI_BLOCKING,X:STATUS
	JCLR	#MARQ,X:DPSR,*
	BCLR	#PCI_BLOCKING,X:STATUS

	;; Check for errors:
	JCLR	#MDT,X:DPSR,X_CON_TRANSFER_HANDLE_ERRORS
	
	CLR	B
	MOVE	X:BURST_SIZE,B0		; All bytes were transferred
	JSR	ADD_HILO_ADDRESS	; Update source address
	JMP	CON_TRANSFER		; Next burst in block

X_CON_TRANSFER_HANDLE_ERRORS
	;; Set PCIDMA_* flags; trashes A only	
	JSR	X_PCI_ERROR_CLEAR
	
	BCLR	#PCIDMA_RESTART,X:STATUS ; Test and clear
	JCS	CON_TRANSFER_PCI	; Restart PCI burst

	BCLR	#PCIDMA_RESUME,X:STATUS	; Test and clear
	JCC	CON_TRANSFER		; Error but no error? Redo this burst.

	;; Update the PCI burst size and burst again.
	JSR	PCI_RECOVER_COUNT	; Get transferred byte count in A.
	JSR	PCI_UPDATE_R0
	JMP	CON_TRANSFER_PCI
	



X_BLOCK_TRANSFER_PCI_CONT
	;; Wait for completion
	BSET	#PCI_BLOCKING,X:STATUS
	JCLR	#MARQ,X:DPSR,*
	BCLR	#PCI_BLOCKING,X:STATUS

	;; Check for errors:
	JCLR	#MDT,X:DPSR,X_BLOCK_TRANSFER_HANDLE_ERRORS
	
	CLR	B
	MOVE	X:BURST_SIZE,B0		; All bytes were transferred
	JSR	ADD_HILO_ADDRESS	; Update source address
	JMP	BLOCK_TRANSFER		; Next burst in block

X_BLOCK_TRANSFER_HANDLE_ERRORS
	;; Set PCIDMA_* flags; trashes A only	
	JSR	X_PCI_ERROR_CLEAR
	
	BCLR	#PCIDMA_RESTART,X:STATUS ; Test and clear
	JCS	BLOCK_TRANSFER_PCI	; Restart PCI burst

	BCLR	#PCIDMA_RESUME,X:STATUS	; Test and clear
	JCC	BLOCK_TRANSFER		; Error but no error? Redo this burst.

	;; Update the PCI burst size and burst again.
	JSR	PCI_RECOVER_COUNT	; Get transferred byte count in A.
	JSR	PCI_UPDATE_R0
	JMP	BLOCK_TRANSFER_PCI


;;; Handle multiple PCI errors

X_PCI_ERROR_CLEAR
	;; Trashes A
	
	;; Global counter
	MOVE	X:DMA_ERRORS,A0
	INC	A
	NOP
	MOVE	A0,X:DMA_ERRORS

	JSSET	#TRTY,X:DPSR,ERROR_TRTY
	JSSET	  #TO,X:DPSR,ERROR_TO
	JSSET	#TDIS,X:DPSR,ERROR_TDIS
	JSSET	 #TAB,X:DPSR,ERROR_TAB
	JSSET	 #MAB,X:DPSR,ERROR_MAB
	JSSET	#DPER,X:DPSR,ERROR_DPER
	JSSET	#APER,X:DPSR,ERROR_APER 

	;; if Restart, then not resume:
	BTST	#PCIDMA_RESTART,X:STATUS ; Test and clear
	JCC	<X_PCI_ERROR_CLEAR_2

	BCLR	#PCIDMA_RESUME,X:STATUS	 ; clear
X_PCI_ERROR_CLEAR_2
	CLR	A
	RTS
