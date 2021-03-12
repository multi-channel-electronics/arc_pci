#!/usr/bin/python

import sys
from commands import getoutput
from optparse import OptionParser

def load_lod(filename):
    lines = open(filename).readlines()
    symb = None
    vars = {}
    for l in lines:
        w = l.split()
        if len(w) == 0:
            continue
        if w[0][0] == '_':
            if w[0] == '_SYMBOL':
                symb = w[1]
                if symb == 'N':
                    symb = 'X'
                if not vars.has_key(symb):
                    vars[symb] = {}
            else:
                symb = None
            continue
        if symb == None:
            continue
        vars[symb][w[0]] = int(w[2],16)
    return vars

def load_dumpmap(filename):
    lines = open(filename).readlines()
    symb = None
    vars = {}
    for l in lines:
        w = l.split()
        if len(w) == 0:
            continue
        if w[0][0] == '_':
            if w[0] == '_GROUP':
                symb = w[1]
                if not vars.has_key(symb):
                    vars[symb] = []
            else:
                symb = None
            continue
        if symb == None:
            continue
        vars[symb].append((w[0],w[1]))
    return vars


def read_mem(bank, addr):
    rep = getoutput('dsp_cmd -qx read %s %i' % (bank, addr))
    words = rep.split()
    if words[3] == 'ok':
        return int(words[7], base=0)
    else:
        raise RuntimeError, ' '.join(words[5:])

def dump_var(vars, bank, name):
    out = '%-20s ' % name
    if not vars.has_key(bank) or not vars[bank].has_key(name):
        out += '???'
    else:
        addr = vars[bank][name]
        out += '%-3s %#06x = ' % (bank, addr)
        try:
            val = read_mem(bank, addr)
            out += '%#06x' % val
        except:
            out += '???'
    print out
    

def dump_group(vars, grps, g):
    if not grps.has_key(g):
        print 'GROUP %s - unknown' % g
        return
    print 'GROUP %s' % g
    for bank, name in grps[g]:
        dump_var(vars, bank, name)

if __name__ == '__main__':
    o = OptionParser()
    o.add_option('-l','--lod-file',type='string',default='build.lod')
    o.add_option('-d','--dump-map',type='string',default=None)
    o.add_option('-v','--variable',action='append',nargs=2,default=[])
    o.add_option('-m','--mem-dump',action='append',nargs=3,default=[])

    opts, args = o.parse_args()

    if opts.dump_map == None:
        opts.dump_map = sys.path[0] + '/dumpmap.txt'

    vars = load_lod(opts.lod_file)
    grps = load_dumpmap(opts.dump_map)
    
    for bank, name in opts.variable:
        dump_var(vars, bank, name)
    for a in args:
        dump_group(vars, grps, a)
    
    for b, a0, c in opts.mem_dump:
        for a in range(int(c,0)):
            addr = int(a0,0) + a
            print '%-2s  %#08x = %#08x' % (b, addr, read_mem(b, addr))
