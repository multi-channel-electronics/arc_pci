      COMMENT *

This is the code which is executed first after power-up etc. 
It sets all the internal registers to their operating values,
sets up the ISR vectors and inialises the hardware etc.

Project:     SCUBA 2 
Author:      DAVID ATKINSON
Target:      250MHz SDSU PCI card - DSP56301
Controller:  For use with SCUBA 2 Multichannel Electronics 

Assembler directives:
	DOWNLOAD=EEPROM => EEPROM CODE
	DOWNLOAD=ONCE => ONCE CODE

	*
	PAGE    132     ; Printronix page width - 132 columns
	OPT	CEX	; print DC evaluations

	MSG ' INCLUDE PCI_initialisation.asm HERE  '

; The EEPROM boot code expects first to read 3 bytes specifying the number of
; program words, then 3 bytes specifying the address to start loading the 
; program words and then 3 bytes for each program word to be loaded.
; The program words will be condensed into 24 bit words and stored in contiguous
; PRAM memory starting at the specified starting address. Program execution
; starts from the same address where loading started.

; Special address for two words for the DSP to bootstrap code from the EEPROM
	IF	@SCP("DOWNLOAD","ROM")		; Boot from ROM on power-on
	ORG	P:0,P:0
	DC	END_ADR-INIT-2			; Number of boot words
	DC	INIT				; Starting address
	ORG	P:0,P:2
INIT	JMP	<INIT_PCI			; Configure PCI port
	NOP
	ENDIF


; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
; command converter
	IF	@SCP("DOWNLOAD","ONCE")		; Download via ONCE debugger
	ORG	P:0,P:0
INIT	JMP	<START
	NOP
	ENDIF

; Vectored interrupt table, addresses at the beginning are reserved
	DC	0,0,0,0,0,0,0,0,0,0,0,0,0,0	; $02-$0f Reserved
	DC	0,0				; $10-$13 Reserved

; FIFO HF* flag interrupt vector is here at $12 - this is connected to the 
; IRQB* interrupt line so its ISR vector must be here
	DC	0,0		; $was ld scatter routine ...HF*

; a software reset button on the font panel of the card is connected to the IRQC*
; line which if pressed causes the DSP to jump to an ISR which causes the program
; counter to the beginning of the program INIT and sets the stack pointer to TOP.
	JSR	CLEAN_UP_PCI			; $14 - Software reset switch

	DC	0,0,0,0,0,0,0,0,0,0,0,0		; Reserved interrupts
	DC	0,0,0,0,0,0,0,0,0,0,0,0,0,0

; Now we're at P:$30, where some unused vector addresses are located
; This is ROM only code that is only executed once on power-up when the 
; ROM code is downloaded. It is skipped over on OnCE downloads.

; A few seconds after power up on the Host, it interrogates the PCI bus to find
; out what boards are installed and configures this PCI board. The EEPROM booting 
; procedure ends with program execution  starting at P:$0 where the EEPROM has
; inserted a JMP INIT_PCI instruction. This routine sets the PLL paramter and
; does a self configuration and software reset of the PCI controller in the DSP.
; After configuring the PCI controller the DSP program overwrites the instruction
; at P:$0 with a new JMP START to skip over the INIT_PCI routine. The program at 
; START address begins configuring the DSP and processing commands. 
; Similarly the ONCE option places a JMP START at P:$0 to skip over the
; INIT_PCI routine. If this routine where executed after the host computer had booted
; it would cause it to crash since the host computer would overwrite the
; configuration space with its own values and doesn't tolerate foreign values.

; Initialize the PLL - phase locked loop
INIT_PCI
	MOVEP	#PLL_INIT,X:PCTL	; Initialize PLL 
	NOP

; Program the PCI self-configuration registers
	MOVE 	#0,X0
	MOVEP	#$500000,X:DCTR		; Set self-configuration mode
	REP	#4
	MOVEP	X0,X:DPAR		; Dummy writes to configuration space
	MOVEP	#>$0000,X:DPMC		; Subsystem ID
	MOVEP	#>$0000,X:DPAR		; Subsystem Vendor ID

; PCI Personal reset
	MOVEP	X0,X:DCTR		; Personal software reset
	NOP
	NOP
	JSET	#HACT,X:DSR,*		; Test for personal reset completion
	MOVE	P:(*+3),X0		; Trick to write "JMP <START" to P:0
	MOVE	X0,P:(0)
	JMP	<START

	DC	0,0,0,0,0,0,0,0,0,0,0,0		; Filler
	DC	0,0,0,0,0,0,0,0,0,0,0,0		; Filler
	DC	0,0,0,0,0,0,0,0,0,0,0,0		; $60-$71 Reserved PCI

;**************************************************************************
; Check for program space overwriting of ISR starting at P:$72
       IF      @CVS(N,*)>$71
       WARN    '*** Error: HCVR vectors overwritten *** '
       ENDIF
	
;	ORG	P:$72,P:$72
	ORG	P:$72,P:$74

; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
; command converter
	IF	@SCP("DOWNLOAD","ONCE")		; Download via ONCE debugger
	ORG	P:$72,P:$72
	ENDIF


;**************************************************************************

; Three non-maskable fast interrupt service routines for clearing PCI interrupts
; The Host will use these to clear the INTA* after it has serviced the interrupt
; which had been generated by the PCI board.

	JSR	<CLR_INTA 			; $72/3 - Clear PCI interrupt
	NOP

	JSR	<CLR_HF3	 		; clear handshaking flag
	NOP

	JSR	<SET_FATAL_ERROR		; $76/7 - driver PCI_MESSAGE_TO_HOST error 
	NOP

; Interrupt locations for 7 available commands on PCI board
; Each JSR takes up 2 locations in the table
	JSR	WRITE_MEMORY			; $78
	JSR	READ_MEMORY			; $7A
	JSR	START_APPLICATION		; $7C
	JSR	STOP_APPLICATION		; $7E
; software reset is the same as cleaning up the PCI - use same routine
; when HOST does a RESET then this routine is run
	JSR	SOFTWARE_RESET			; $80
	JSR	SEND_PACKET_TO_CONTROLLER	; $82
	JSR	SEND_PACKET_TO_HOST		; $84
	JSR	RESET_CONTROLLER		; $86

; QT - set command
	JSR	QUIET_TRANSFER_SET              ; $88
	JSR	SYSTEM_RESET                    ; $8A


; Jam these routines in early so that we can short jump to them from
; above.  Only necessary because we're patching!
	ORG	P:$90,P:$92

CLR_INTA
	BCLR	#INTA,X:DCTR			; Clear PCI interrupt
	NOP
	RTI
CLR_HF3	
	BCLR	#DCTR_HF3,X:DCTR		; Clear handshake flag
	NOP
	RTI
SET_FATAL_ERROR	
	BSET	#FATAL_ERROR,X:<STATUS		; Set PCI_MESSAGE_TO_HOST error
	NOP
	RTI

	
; ***********************************************************************
; For now have boot code starting from P:$100
; just to make debugging tidier etc.

	ORG	 P:$100,P:$102

; This allows for the DSP to be loaded from the ONCE port via the WIGGLER
; command converter
	IF	@SCP("DOWNLOAD","ONCE")		; Download via ONCE debugger
	ORG	P:$100,P:$100
	ENDIF
; ***********************************************************************



; ******************************************************************
;
;	AA0 = RDFIFO* of incoming fiber optic data
;	AA1 = EEPROM access
;	AA2 = DRAM access
;	AA3 = output to parallel data connector, for a video pixel clock
;	$FFxxxx = Write to fiber optic transmitter
;
; ******************************************************************

	
START	MOVEP	#>$000001,X:DPMC
	BSET	#20,X:DCTR		; HI32 mode = 1 => PCI
	BCLR	#21,X:DCTR
	BCLR	#22,X:DCTR
	NOP
	BSET	#MACE,X:DPCR		; Master access counter enable
	NOP
	NOP				; End of PCI programming


; Set operation mode register OMR to normal expanded
	MOVEC   #$0000,OMR	; Operating Mode Register = Normal Expanded
	MOVEC	#0,SP		; Reset the Stack Pointer SP

; Program the serial port ESSI0 = Port C for serial transmission to 
;   the timing board
	MOVEP	#>0,X:PCRC	; Software reset of ESSI0
;**********************************************************************
	MOVEP	#$00080B,X:CRA0	; Divide 100.0 MHz by 24 to get 4.17 MHz
				; DC0-CD4 = 0 for non-network operation
				; WL0-WL2 = ALC = 0 for 2-bit data words
				; SSC1 = 0 for SC1 not used
;************************************************************************
	MOVEP	#$010120,X:CRB0	; SCKD = 1 for internally generated clock
				; SHFD = 0 for MSB shifted first
				; CKP = 0 for rising clock edge transitions
				; TE0 = 1 to enable transmitter #0
				; MOD = 0 for normal, non-networked mode
				; FSL1 = 1, FSL0 = 0 for on-demand transmit
	MOVEP	#%101000,X:PCRC	; Control Register (0 for GPIO, 1 for ESSI)
				; Set SCK0 = P3, STD0 = P5 to ESSI0
;********************************************************************************
	MOVEP	#%111100,X:PRRC	; Data Direction Register (0 for In, 1 for Out)
	MOVEP	#%000000,X:PDRC	; Data Register - AUX3 = i/p, AUX1 not used
;***********************************************************************************
; 250MHz
; Conversion from software bits to schematic labels for Port C and D
;	PC0 = SC00 = AUX3		PD0 = SC10 = EF*
;	PC1 = SC01 = A/B* = input	PD1 = SC11 = HF*
;	PC2 = SC02 = No connect		PD2 = SC12 = RS*
;	PC3 = SCK0 = No connect		PD3 = SCK1 = NWRFIFO*
;	PC4 = SRD0 = AUX1		PD4 = SRD1 = No connect (** in 50Mhz this was MODE select for 16 or 32 bit FO)
;	PC5 = STD0 = No connect		PD5 = STD1 = WRFIFO*
; ***********************************************************************************


; ****************************************************************************
; Program the serial port ESSI1 = Port D for general purpose I/O (GPIO)

	MOVEP	#%000000,X:PCRD	; Control Register (0 for GPIO, 1 for ESSI)
	MOVEP	#%011100,X:PRRD	; Data Direction Register (0 for In, 1 for Out)
	MOVEP	#%010000,X:PDRD	; Data Register - Pulse RS* low
	REP	#10
	NOP
	MOVEP	#%010100,X:PDRD ; Data Register - Pulse RS* high
				; was %011100

; Program the SCI port to benign values
	MOVEP	#%000,X:PCRE	; Port Control Register = GPIO
	MOVEP	#%110,X:PRRE	; Port Direction Register (0 = Input)
	MOVEP	#%010,X:PDRE	; Port Data Register
;	PE0 = RXD
;	PE1 = TXD
;	PE2 = SCLK

; Program the triple timer to assert TCI0 as an GPIO output = 1
	MOVEP	#$2800,X:TCSR0
	MOVEP	#$2800,X:TCSR1
	MOVEP	#$2800,X:TCSR2


; Program the address attribute pins AA0 to AA2. AA3 is not yet implemented.
	MOVEP	#$FFFC21,X:AAR0	; Y = $FFF000 to $FFFFFF asserts Y:RDFIFO*
	MOVEP	#$008929,X:AAR1	; P = $008000 to $00FFFF asserts AA1 low true
	MOVEP	#$000122,X:AAR2	; Y = $000800 to $7FFFFF accesses SRAM


; Program the DRAM memory access and addressing
	MOVEP	#$020022,X:BCR	; Bus Control Register
	MOVEP	#$893A05,X:DCR	; DRAM Control Register


; Clear all PCI error conditions
	MOVEP	X:DPSR,A
	OR	#$1FE,A
	NOP
	MOVEP	A,X:DPSR

;--------------------------------------------------------------------
; Enable one interrupt only: software reset switch
	MOVEP	#$0001C0,X:IPRC	; IRQB priority = 1 (FIFO half full HF*)
				; IRQC priority = 2 (reset switch)
	MOVE	#$200,SR	; Mask set up for reset switch only


;--------------------------------------------------------------------------
; Initialize the fiber optic serial transmitter to zero
	JCLR	#TDE,X:SSISR0,*
	MOVEP	#$000000,X:TX00

;--------------------------------------------------------------------

; clear DTXM - PCI master transmitter 
	BSET	#CLRT,X:DPCR		; Clear the master transmitter DTXM
	JSET	#CLRT,X:DPCR,*		; Wait for the clearing to be complete

;----------------------------------------------------------------------
; clear DRXR - PCI receiver
	
CLR0	JCLR	#SRRQ,X:DSR,CLR1	; Wait for the receiver to be empty
	MOVEP	X:DRXR,X0		; Read receiver to empty it
	NOP
	JMP	<CLR0
CLR1

;-----------------------------------------------------------------------------
; copy parameter table from P memory into X memory

; but not word_count and num_dumped - don't want these reset by fatal error....
; they will be reset by new packet or pci_reset ISR


	MOVE	X:WORD_COUNT,Y0		; store packet word count
	MOVE	X:NUM_DUMPED,Y1		; store number dumped (after HST TO)
	MOVE	X:FRAME_COUNT,X1	; store frame count

; Move the table of constants from P: space to X: space
	MOVE	#VAR_TBL_START,R1 		; Start of parameter table in P 
	MOVE	#VAR_TBL,R0			; start of parameter table in X
	DO	#VAR_TBL_LENGTH,X_WRITE
	MOVE	P:(R1)+,X0
	MOVE	X0,X:(R0)+			; Write the constants to X:
X_WRITE


	MOVE	Y0,X:WORD_COUNT			; restore packet word count
	MOVE	Y1,X:NUM_DUMPED			; restore number dumped (after HST TO)
	MOVE	X1,X:FRAME_COUNT		; restore frame count

;-------------------------------------------------------------------------------
; initialise some bits in STATUS

	BCLR	#APPLICATION_LOADED,X:<STATUS	  ; clear application loaded flag
	BCLR	#APPLICATION_RUNNING,X:<STATUS    ; clear appliaction running flag 
						  ; (e.g. not running diagnostic application 
						  ;      in self_test_mode)	

	BCLR	#FATAL_ERROR,X:<STATUS		; initialise fatal error flag.
	BSET	#PACKET_CHOKE,X:<STATUS		; enable MCE packet choke
						; HOST not informed of anything from MCE until 
						; comms are opened by host with first CON command

	BCLR	#PREAMBLE_ERROR,X:<STATUS ; flag to let host know premable error 
	
;------------------------------------------------------------------------------
; disable FIFO HF* intererupt...not used anymore.

	MOVEP	#$0001C0,X:IPRC		; Disable FIFO HF* interrupt
	MOVEC	#$200,SR		; Mask level 1 interrupts

;----------------------------------------------------------------------------
; Disable Byte swapin - enabled after first command to MCE.
; i.e after first 'CON'

	BCLR	#BYTE_SWAP,X:<STATUS	; flag to let host know byte swapping off
	BCLR	#AUX1,X:PDRC		; enable disable

;;; Clear the fibre fifo!
	JSR	CLEAR_FIFO
		
;----------------------------------------------------------------------------
; Initialize PCI controller again, after booting, to make sure it sticks
        BCLR	#20,X:DCTR		; Terminate and reset mode 
        NOP
        JSET    #HACT,X:DSR,*		; Test for personal reset completion
        NOP
        BSET    #20,X:DCTR              ; HI32 mode = 1 => PCI
        NOP
        JSET    #12,X:DPSR,*		; Host data transfer not in progress
;-----------------------------------------------------------------------------
; Here endth the initialisation code run after power up.
; ----------------------------------------------------------------------------
