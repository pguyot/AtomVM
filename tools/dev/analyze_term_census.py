#!/usr/bin/env python3
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
"""Turn a term-comparison census dump into the tables the report needs.

The dump is written by a build carrying tools/dev/term-compare-census.patch;
see tools/dev/TERM_COMPARISON_CENSUS_2026-09-12.md for what it records.

    analyze_term_census.py census.json [label] [the erlc binary, for symbols]
"""
import json, subprocess, sys
from collections import Counter, defaultdict

d = json.load(open(sys.argv[1]))
label = sys.argv[2] if len(sys.argv) > 2 else sys.argv[1]
exe = sys.argv[3] if len(sys.argv) > 3 else None
T = d["type_names"]; DEC = d["decided_names"]; K = d["kind_names"]
RES = ["equal", "less", "greater", "allocfail"]
shapes = d["shapes"]; tot = d["totals"]
N = sum(s["count"] for s in shapes)

sites = {}
for s in d["sites"]:
    sites[s["id"]] = s

# optional source-line attribution
if exe and d["sites"] and "addr" in d["sites"][0]:
    addrs = [s["addr"] for s in d["sites"]]
    fbase = d["sites"][0]["fbase"]
    try:
        out = subprocess.run(["atos", "-o", exe, "-l", hex(fbase)] + [hex(a) for a in addrs],
                             capture_output=True, text=True).stdout.strip().split("\n")
        for s, line in zip(d["sites"], out):
            sites[s["id"]]["line"] = line.strip()
        addrs2 = [s.get("addr2", 0) for s in d["sites"]]
        out2 = subprocess.run(["atos", "-o", exe, "-l", hex(fbase)] + [hex(a) for a in addrs2],
                              capture_output=True, text=True).stdout.strip().split("\n")
        for s, line in zip(d["sites"], out2):
            sites[s["id"]]["line2"] = line.strip()
    except FileNotFoundError:
        pass

def pct(n, den=N):
    return f"{100.0*n/den:5.2f}%" if den else "  -  "

def table(title, counter, den=N, limit=20, namer=str):
    print(f"\n### {title}")
    for key, c in counter.most_common(limit):
        print(f"  {pct(c,den)} {c:>12,}  {namer(key)}")

print(f"===== {label} =====")
print(f"logical comparisons recorded : {tot['logical']:,}")
print(f"flat-map probe comparisons   : {tot['flatmap_probe']:,}")
print(f"total events in shape table  : {N:,}  (overflow {tot['shape_overflow']:,})")

by_kind = Counter(); by_dec = Counter(); by_types = Counter(); by_site = Counter()
by_res = Counter(); by_arity = Counter(); by_elem = Counter(); by_depth = Counter()
by_pairs = Counter(); dec_by_kind = defaultdict(Counter); eq_outcome = Counter()
site_dec = defaultdict(Counter)
for s in shapes:
    c = s["count"]
    by_kind[K[s["kind"]]] += c
    by_dec[DEC[s["decided"]]] += c
    by_types[(T[s["ta"]], T[s["tb"]])] += c
    by_site[s["site"]] += c
    by_res[RES[s["result"]]] += c
    by_arity[s["arity"]] += c
    by_elem[s["elem"]] += c
    by_depth[s["depth"]] += c
    by_pairs[s["pairs_log2"]] += c
    dec_by_kind[K[s["kind"]]][DEC[s["decided"]]] += c
    site_dec[s["site"]][(K[s["kind"]], DEC[s["decided"]], RES[s["result"]])] += c
    if K[s["kind"]] == "equal_only":
        eq_outcome[(RES[s["result"]] == "equal", DEC[s["decided"]])] += c

ENTRY = ["out-of-line term_compare", "inline =:= prefix at the call site", "flat-map probe"]
by_entry = Counter(); entry_dec = defaultdict(Counter)
for s in shapes:
    e = ENTRY[s.get("entry", 0)]
    by_entry[e] += s["count"]
    entry_dec[e][DEC[s["decided"]]] += s["count"]
table("by entry point (what the comparison actually cost)", by_entry)
for e, c in by_entry.most_common():
    print(f"    {e} ({c:,})")
    for dec, v in entry_dec[e].most_common(6):
        print(f"        {pct(v,c)} {v:>12,}  {dec}")
table("by kind", by_kind)
table("by what decided it", by_dec)
table("by result", by_res)
table("by operand types", by_types, limit=15, namer=lambda k: f"{k[0]} vs {k[1]}")
table("tuple arity (of the top-level pair, 0 = not a tuple)", by_arity, limit=10)
table("deciding element index (top level)", by_elem, limit=8)
table("max depth reached", by_depth, limit=8)
table("element pairs examined (log2 bucket)", by_pairs, limit=8,
      namer=lambda b: f"{2**b}..{2**(b+1)-1}")

print("\n### what decided it, per kind")
for kind, counter in sorted(dec_by_kind.items(), key=lambda kv: -sum(kv[1].values())):
    den = sum(counter.values())
    print(f"  {kind}  ({den:,}, {pct(den)} of all)")
    for dec, c in counter.most_common(8):
        print(f"      {pct(c,den)} {c:>12,}  {dec}")

print("\n### =:= outcomes (the header-hash question)")
den = sum(eq_outcome.values())
for (equal, dec), c in sorted(eq_outcome.items(), key=lambda kv: -kv[1])[:14]:
    print(f"  {pct(c,den)} {c:>12,}  {'EQUAL' if equal else 'differ':6} via {dec}")

by_sym = Counter(); by_pair = Counter(); sym_dec = defaultdict(Counter)
for s in shapes:
    site = sites.get(s["site"], {})
    sym = site.get("sym", "?")
    caller = site.get("caller", "?")
    by_sym[sym] += s["count"]
    by_pair[(sym, caller)] += s["count"]
    sym_dec[sym][(K[s["kind"]], DEC[s["decided"]])] += s["count"]
table("by comparing function (frame that called the comparator)", by_sym, limit=18)
print("\n### by (comparator caller, its caller)")
for (sym, caller), c in by_pair.most_common(20):
    print(f"  {pct(c)} {c:>12,}  {sym}  <- {caller}")
print("\n### what each comparing function asks")
for sym, c in by_sym.most_common(10):
    print(f"  {sym}  ({c:,}, {pct(c)})")
    for k, v in sym_dec[sym].most_common(4):
        print(f"      {pct(v,c)} {v:>12,}  {k[0]}/{k[1]}")

print("\n### what a cached hash in a boxed header could short-circuit")
deep = Counter(); deep_pairs = Counter()
for s in shapes:
    if K[s["kind"]] != "equal_only":
        continue
    compound = s["ta"] in (T.index("tuple"), T.index("list"), T.index("map"), T.index("binary"))
    if not compound or DEC[s["decided"]] == "identical":
        continue
    outcome = "EQUAL (hash cannot help)" if RES[s["result"]] == "equal" else "differ (hash could cut)"
    where = "same header, walked in" if DEC[s["decided"]] not in ("tuple_arity", "type_diff") else "arity/type alone"
    deep[(outcome, where)] += s["count"]
    deep_pairs[(outcome, where)] += s["count"] * (2 ** s["pairs_log2"])
den = sum(deep.values())
for k, c in deep.most_common():
    print(f"  {pct(c,den)} {c:>12,}  {k[0]:24} {k[1]}   (>= {deep_pairs[k]:,} element pairs)")

print("\n### top call sites (raw, with line numbers)")
for sid, c in by_site.most_common(18):
    s = sites.get(sid, {})
    name = s.get("line") or f"{s.get('sym','?')}+{s.get('off',0)}"
    name += "   <- " + (s.get("line2") or s.get("caller", "?"))
    top = site_dec[sid].most_common(2)
    detail = "; ".join(f"{k[0]}/{k[1]}/{k[2]} {100.0*v/c:.0f}%" for k, v in top)
    print(f"  {pct(c)} {c:>12,}  {name}\n                            {detail}")

print("\n### atoms")
a = tot
print(f"  atom-table lookups                       : {a['atom_cmp']:,}"
      f"  ({pct(a['atom_cmp'])} of all comparisons)")
print(f"    asked an ordering question             : {a.get('atom_cmp_ordering',0):,}")
print(f"    asked only 'same atom?'                : {a.get('atom_cmp_equal_only',0):,}")
print(f"  distinct atoms involved                  : {a['atom_distinct_atoms']:,}")
print(f"  distinct unordered pairs                 : {a['atom_distinct_pairs']:,}")
for tag in ("order", "equal"):
    bits = d.get(f"atom_first_diff_bit_{tag}") or d.get("atom_first_diff_bit")
    if not bits or not sum(bits):
        continue
    total_bits = sum(bits)
    print(f"  [{tag}] {total_bits:,} lookups; resolvable with N leading bits of the name in the term:")
    for n in (8, 16, 24, 32, 38, 40, 48, 56, 64):
        print(f"      {n:>2} bits : {100.0*sum(bits[:n])/total_bits:7.3f}%")
    print(f"      identical names {bits[64]:,}; needs more than 64 bits {bits[65]:,}")
    prefix = d.get(f"atom_prefix_bytes_{tag}") or d.get("atom_prefix_bytes")
    share = ", ".join(f"{i}:{100.0*c/sum(prefix):.1f}%" for i, c in enumerate(prefix) if c)
    print(f"      common prefix bytes -> {share}")
