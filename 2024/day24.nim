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
from std/sequtils  import concat, foldl, mapIt, to_seq
from std/sets      import `-`, contains, clear, excl, HashSet, incl, items
from std/strutils  import join, parse_int, split
from std/tables    import `[]`, `[]=`, has_key, keys, pairs, Table, to_table

# Read input file

proc read_input(): (Table[string, int], Table[string, seq[string]]) =
  var first = true
  for l in stdin.lines:
    if l == "":
      assert first
      first = false
    elif first:
      let item = l.split(": ")
      assert not result[0].has_key(item[0])
      assert len(item) == 2
      assert item[1] == "0" or item[1] == "1"
      result[0][item[0]] = parse_int(item[1])
    else:
      let part1 = l.split(" -> ")
      assert len(part1) == 2
      assert not result[1].has_key(part1[1])
      let part2 = part1[0].split(" ")
      assert len(part2) == 3
      result[1][part1[1]] = part2

let (init, gates) = read_input()

# Puzzle 1

proc compute(values: var Table[string, int], gates: Table[string, seq[string]], v: string): int =
  if values.has_key(v):
    return values[v]
  let
    gate = gates[v]
    left = compute(values, gates, gate[0])
    right = compute(values, gates, gate[2])
  case gate[1]
  of "AND": result = left and right
  of "OR":  result = left or  right
  of "XOR": result = left xor right
  else: assert false
  assert result == 0 or result == 1
  values[v] = result

func zkeys(gates: Table[string, seq[string]]): seq[string] =
  for k in gates.keys():
    if k[0] == 'z':
      result.add(k)
    result = sorted(result)

func answer1(init: Table[string, int], gates: Table[string, seq[string]]): int =
  var values = init
  var factor = 1
  result = 0
  for k in zkeys(gates):
    result += factor * compute(values, gates, k)
    factor *= 2

echo "Puzzle 1: ", answer1(init, gates)

# Puzzle  2

proc extended_compute(values: var Table[string, int], gates: Table[string, seq[string]], v: string): int =
  if values.has_key(v):
    return values[v]
  if not gates.has_key(v):
    values[v] = -2
    return -2
  let
    gate = gates[v]
    left = extended_compute(values, gates, gate[0])
    right = extended_compute(values, gates, gate[2])
  if left < 0 or right < 0:
    values[v] = -1
    return -1
  assert left == 0 or left == 1
  assert right == 0 or right == 1
  case gate[1]
  of "AND": result = left and right
  of "OR":  result = left or  right
  of "XOR": result = left xor right
  else: assert false
  assert result == 0 or result == 1
  values[v] = result

func suffix(index: int): string =
  if index < 10: "0" & $index else: $index

func deps(gates: Table[string, seq[string]], root: string): HashSet[string] =
  var to_visit = @[root]
  result.incl(root)
  while len(to_visit) > 0:
    let head = to_visit[0]
    to_visit = to_visit[1 .. ^1]
    if gates.has_key(head):
      let g = gates[head]
      for d in @[g[0], g[2]]:
        if d notin result:
          result.incl(d)
          to_visit.add(d)

proc test_adder(gates: Table[string, seq[string]], index: int): void =
  let cur_suffix = suffix(index)
  for x in @[0, 1]:
    for y in @[0, 1]:
      for cc in @[0, 1]:
        let c = if index > 0: cc else: 0
        var values = to_table(@[("x" & cur_suffix, x), ("y" & cur_suffix, y)])
        for i in 0 ..< index:
          values["x" & suffix(i)] = if i == index-1: c else: 0
          values["y" & suffix(i)] = if i == index-1: c else: 0
        let res = extended_compute(values, gates, "z" & cur_suffix)
        if res != (x + y + c) mod 2:
          echo "Bad adder at ", index, ": ", res, " for ", x, " + ", y, " + ", c
          let d = if index == 0: deps(gates, "z" & cur_suffix) else: deps(gates, "z" & cur_suffix) - deps(gates, "z" & suffix(index - 1))
          for k in d.items.to_seq.sorted:
            if gates.has_key(k):
              echo "  ", k, ".", values[k], " = ", gates[k].join(" ")
            else:
              echo "  ", k, ".", values[k]

func swap(gates: Table[string, seq[string]], p: seq[(string, string)]): Table[string, seq[string]] =
  result = gates
  for s in p:
    result[s[0]] = gates[s[1]]
    result[s[1]] = gates[s[0]]

# Fix list is manually generated, using test_adder output and
# pattern recognition inside the operator's brain.
let
  fixes = @[("z06", "jmq"), ("z13", "gmh"), ("rqf", "cbd"), ("qrh", "z38")]
  fixed_gates = swap(gates, fixes)

for k in zkeys(gates):
  test_adder(fixed_gates, parse_int(k[1 .. ^1]))

echo "Puzzle 2: ", fixes.mapIt(@[it[0], it[1]]).foldl(concat(a, b)).sorted().join(",")
