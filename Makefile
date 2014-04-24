PREFIX=
SUFFIX=build

TARGET=$(PREFIX)$(SUFFIX)

ASM=asm56300
CLB=dsplnk
CLD=cldlod
LOD=lod2s

# Provide parseable error strings.
SED_PROG='s/\r//g'
AWK_PROG='($$5=="ERROR" || $$5=="WARNING") {file=substr($$3,2); match($$4, "[0-9]*"); \
	lino=substr($$4, RSTART, RLENGTH); split($$0, msg, "---"); \
	printf "%s:%s: error: %s\n", file, lino, msg[2] ;} ELSE {print $$0}'

ASM_FLAGS=-dDOWNLOAD ROM

# Target for dsp_cmd patch generation:
PATCH_SOURCE="build.lod.u0107"

default: $(TARGET).s
#default: patch

patch: $(TARGET).lod
	python ../tools/live_patch.py $(PATCH_SOURCE) $(TARGET).lod | \
	    tee patch || ( rm patch && false )

inv_patch: $(TARGET).lod
	echo "# Inverse patch."
	python ../tools/live_patch.py $(TARGET).lod $(PATCH_SOURCE)

build.clb: header.asm init.asm main.asm build.asm vars.asm app.asm info.asm \
	 comms7.asm hacking.asm

%s : %lod
	$(LOD) $< $@ UBC_MCE_PCI 4

%lod : %cld
	$(CLD) $(CLD_FLAGS) $< > $@

%cld : %clb
	$(CLB) $(CLB_FLAGS) -m$*map $<

%clb : %asm
	$(ASM) $(ASM_FLAGS) -l$*ls -b$@ $< |sed $(SED_PROG) | awk $(AWK_PROG)

clean:	tidy
	-rm $(TARGET).lod
	-rm $(TARGET).s

tidy:
	-rm *.cld *.clb *.ls *~

.PHONY: clean tidy

.SECONDARY: $(TARGET).lod
