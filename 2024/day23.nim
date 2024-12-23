# Copyright (c) 2024, Natacha Porté
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

from std/algorithm import sorted
from std/sequtils import map, mapIt, toSeq
from std/sets     import contains, HashSet, incl, items, len, toHashSet, union
from std/strutils import join, parseInt, split
from std/tables   import `[]`, `[]=`, getOrDefault, hasKey, pairs, Table

# Read input file

let data = stdin.lines().toSeq().mapIt(it.split("-"))

# Puzzle 1

func image(s: HashSet[string]): string = s.items.to_seq.sorted.join(",")

proc add_edge(edges: var Table[string, HashSet[string]], src, dest: string): void =
  if edges.has_key(src):
    edges[src].incl(dest)
  else:
    edges[src] = toHashSet(@[dest])

func calc_edges(d: seq[seq[string]]): Table[string, HashSet[string]] =
  for c in d:
    assert len(c) == 2
    add_edge(result, c[0], c[1])
    add_edge(result, c[1], c[0])

func calc_triangles(cnx: Table[string, HashSet[string]]): HashSet[string] =
  for src, dest_set in cnx.pairs:
    assert src notin dest_set
    for d1 in dest_set.items:
      for d2 in dest_set.items:
        if d1 != d2 and d1 in cnx[d2]:
          result.incl(toHashSet(@[src, d1, d2]).image)

func answer1(cnx: Table[string, HashSet[string]]): int =
  var acc: HashSet[string]
  for src, dest_set in cnx.pairs:
    assert src notin dest_set
    if src[0] != 't':
      continue
    for d1 in dest_set.items:
      for d2 in dest_set.items:
        if d1 != d2 and d1 in cnx[d2]:
          acc.incl(toHashSet(@[src, d1, d2]).image)
  result = len(acc)

let cnx = calc_edges(data)

echo "Puzzle 1: ", answer1(cnx)

# Puzzle 2

func is_subset(small, big: HashSet[string]): bool =
  for i in small.items:
    if i notin big:
      return false
  result = true

proc largest_subset(edges: Table[string, HashSet[string]]): string =
  var
    subsets = calc_triangles(edges)
    sz = 3
  while len(subsets) > 1:
    echo "Found ", len(subsets), " strongly-connected subsets of size ", sz
    var next: HashSet[string]
    for s in subsets.items:
      let
        base_seq = s.split(",")
        base = base_seq.to_hash_set
      for i1 in base_seq:
        for i2 in base_seq:
          assert i1 == i2 or (i1 in edges[i2] and i2 in edges[i1])
      for n, dest_set in edges.pairs:
        if n notin base and is_subset(base, dest_set):
          next.incl(union(base, to_hash_set(@[n])).image)
    subsets = next
    sz += 1
  assert len(subsets) == 1
  for i in subsets.items:
    result = i

echo "Puzzle 2: ", largest_subset(cnx)
