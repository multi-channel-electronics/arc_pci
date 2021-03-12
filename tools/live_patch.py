#!/usr/bin/python

def load_lod(filename, bias=-2):
    """
    Load the code words from a .lod file.  (Typically build.lod.)
    Addresses are biased to account for the difference between
    ROM address and RAM address.

    Returns a list of (address, data) tuples.
    """
    addr = 0
    active = False
    data = []
    for line in open(filename).readlines():
        w = line.split()
        if len(w) <= 0: continue
        try:
            x = int(w[0], 16)
            for x in w:
                data.append((addr, int(x, 16)))
                addr += 1
        except ValueError:
            if w[0] == '_DATA' and w[1] == 'P':
                addr = int(w[2], 16) + bias
                active = True
            else:
                active = False
    return data
            
def diff_lod(d1, d2, minimal=True):
    """
    Compare two LOD data sets, and generate commands to patch a DSP
    running the first LOD so it is instead running second LOD.

    If minimal==True, only shows produces the writes that are strictly
    necessary to change d1 -> d2.  Otherwise, it does... more.  Not
    sure why that would be useful.

    This might fail if d1 extends to higher addresses than d2.
    """
    def one():
        return i1 < len(d1) or i2 < len(d2)
    def both():
        return i1 < len(d1) and i2 < len(d2)
    def write_cmd(mem, adr, value):
        return 'dsp_cmd -qx write %s %#06x %#06x' % (mem, adr, value)
    i1, i2 = 0, 0
    print '# Replacement data'
    while i2 < len(d2):
        while both() and d1[i1][0] == d2[i2][0]:
            if d1[i1][1] != d2[i2][1]:
                if d2[i2][0] >= 0: # this is a live patch, not hexfile
                    print write_cmd('P', d2[i2][0], d2[i2][1])
            i1 += 1
            i2 += 1
        if not minimal:
            print '# New data'
        # Catch up on the left index
        while both() and (d1[i1][0] < d2[i2][0]):
            i1 += 1
        # Catch up on the the right
        while (i2 < len(d2)) and \
                (not minimal or \
                     (i1 >= len(d1) or d1[i1][0] > d2[i2][0])):
            print write_cmd('P', d2[i2][0], d2[i2][1])
            i2 += 1
        

def dsp_write(mem, addr, val):
    return 'dsp_cmd -qx write %s %#06x %#06x' % \
        (mem, addr, val)
    

def restore(d, start, count):
    """
    Generate commands to patch DSP code based on certain addresses in
    the LOD dump d.
    """
    i1 = 0
    while d[i1][0] < start: i1 += 1
    while d[i1][0] < start+count:
        print dsp_write('P', d[i1][0], d[i1][1])
        i1 += 1
    

if __name__ == '__main__':
    from optparse import OptionParser
    o = OptionParser()
    o.add_option('--minimal',type=int, default=1)
    opts, args = o.parse_args()
    if len(args) == 2:
        data1 = load_lod(args[0])
        data2 = load_lod(args[1])
    else:
        data1 = load_lod('../../releases/U0106/build.lod')
        data2 = load_lod('./build.lod')

    diff_lod(data1, data2, minimal=opts.minimal)
#    restore(data1, 0x280, 32)

