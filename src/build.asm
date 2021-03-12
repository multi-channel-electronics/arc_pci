	COMMENT *

Main source file.

Project:     UBC Multi-Channel Electronics
Target:      250MHz SDSU PCI card - DSP56301

Original code by Bob Leach (Astro-cam) and David Atkinson (ATC).
Versions U0101+ maintained by Matthew Hasselfield (UBC)
	
Version:     Release Version U (1.5)


Assembler directives:
	DOWNLOAD=EEPROM => EEPROM CODE
	DOWNLOAD=ONCE => ONCE CODE

	*
	PAGE    132     ; Printronix page width - 132 columns
	OPT	CEX	; print DC evaluations

	INCLUDE 'info.asm'
	INCLUDE 'header.asm'
	INCLUDE	'init.asm'
	INCLUDE 'main.asm'
	INCLUDE 'vars.asm'
	INCLUDE 'app.asm'
	INCLUDE 'comms7.asm'
	INCLUDE 'hacking.asm'
	
END_ADR	EQU	@LCV(L)		; End address of P: code written to ROM
