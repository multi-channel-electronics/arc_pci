PREFIX=
SUFFIX=build

TARGET=$(PREFIX)$(SUFFIX)

ASM=asm56300
CLB=dsplnk
CLD=cldlod
LOD=lod2s

ASM_FLAGS=-dDOWNLOAD ROM

default: $(TARGET).s

build.clb: header.asm init.asm main.asm build.asm

%s : %lod
	$(LOD) $< $@ UBC_MCE_PCI 4

%lod : %cld
	$(CLD) $(CLD_FLAGS) $< > $@

%cld : %clb
	$(CLB) $(CLB_FLAGS) -m$*map $<

%clb : %asm
	$(ASM) $(ASM_FLAGS) -l$*ls -b$@ $<

clean:	tidy
	-rm *.lod

tidy:
	-rm *.cld *.clb *.ls *~
