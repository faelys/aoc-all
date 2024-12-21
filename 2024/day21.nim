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

from std/sequtils import concat, filterIt, foldl, map, mapIt, toSeq, zip
from std/strutils import parseInt, repeat
from std/tables   import `[]`, `[]=`, hasKey, Table, toTable, pairs, keys

# Read input file

type
  Point = tuple
    x, y: int

let
  data = stdin.lines().toSeq()
  num_pad = {
    '7': (x: 0, y: 0),
    '8': (x: 1, y: 0),
    '9': (x: 2, y: 0),
    '4': (x: 0, y: 1),
    '5': (x: 1, y: 1),
    '6': (x: 2, y: 1),
    '1': (x: 0, y: 2),
    '2': (x: 1, y: 2),
    '3': (x: 2, y: 2),
    '*': (x: 0, y: 3),
    '0': (x: 1, y: 3),
    'A': (x: 2, y: 3)
  }.toTable
  dir_pad = {
    '*': (x: 0, y: 0),
    '^': (x: 1, y: 0),
    'A': (x: 2, y: 0),
    '<': (x: 0, y: 1),
    'v': (x: 1, y: 1),
    '>': (x: 2, y: 1)
  }.toTable

# Puzzle 1

func minimize(s: seq[string]): seq[string] =
  let l = min(s.mapIt(len(it)))
  result = s.filterIt(len(it) == l)

func cat4(a, b, c, d: string): string = a & b & c & d

proc cmd(s: string, pad: Table[char, Point]): seq[string] =
  var p = pad['A']
  result = @[""]
  for d in s.mapIt(pad[it]):
    var h, v: string
    if d.y > p.y:
      v = repeat('v', d.y - p.y)
    if d.y < p.y:
      v = repeat('^', p.y - d.y)
    if d.x > p.x:
      h = repeat('>', d.x - p.x)
    if d.x < p.x:
      h = repeat('<', p.x - d.x)

    if h == "" or v == "" or pad['*'] == (x: p.x, y: d.y):
      result = result.mapIt(cat4(it, h, v, "A"))
    elif pad['*'] == (x: d.x, y: p.y):
      result = result.mapIt(cat4(it, v, h, "A"))
    else:
      result = concat(result.mapIt(cat4(it, h, v, "A")), result.mapIt(cat4(it, v, h, "A")))

    p = d

  result = minimize(result)

proc cmd_seq(s: seq[string], pad: Table[char, Point]): seq[string] =
  for c in s:
    result = minimize(concat(result, cmd(c, pad)))

proc puzzle1(s: string): int =
  let
    s1 = cmd(s, num_pad)
    s2 = cmd_seq(s1, dir_pad)
    s3 = cmd_seq(s2, dir_pad)
    n = parseInt(s[0 .. ^2])
  result = len(s3[0]) * n

echo "Puzzle 1: ", data.map(puzzle1).foldl(a + b)

# Puzzle 2
# 154115708116294 is too low

var memo: Table[(char, char, int), int]

proc base_steps(start, dest: char, depth: int, pad: Table[char, Point]): int

proc steps(start, dest: char, depth: int): int =
  if start == dest:
    return 1
  elif memo.hasKey((start, dest, depth)):
    return memo[(start, dest, depth)]
  result = base_steps(start, dest, depth, dir_pad)
# echo repeat("  ", depth), "storing(", start, ", ", dest, ", ", depth, "): ", result
  memo[(start, dest, depth)] = result

proc base_steps(start, dest: char, depth: int, pad: Table[char, Point]): int =
  let
    ps = pad[start]
    pd = pad[dest]
    cx = if ps.x < pd.x: '>' elif ps.x > pd.x: '<' else: '.'
    cy = if ps.y < pd.y: 'v' elif ps.y > pd.y: '^' else: '.'
    dx = abs (ps.x - pd.x)
    dy = abs (ps.y - pd.y)
  if depth == 0:
    return dx + dy + 1

  if dx == 0:
    assert dy > 0
    result = steps('A', cy, depth - 1) + (dy-1) + steps(cy, 'A', depth - 1)
  elif dy == 0:
    assert dx > 0
    result = steps('A', cx, depth - 1) + (dx-1) + steps(cx, 'A', depth - 1)
  elif pad['*'] == (x: pd.x, y: ps.y):
    result = steps('A', cy, depth - 1) + (dy-1) + steps(cy, cx, depth - 1) + (dx-1) + steps(cx, 'A', depth - 1)
  elif pad['*'] == (x: ps.x, y: pd.y):
    result = steps('A', cx, depth - 1) + (dx-1) + steps(cx, cy, depth - 1) + (dy-1) + steps(cy, 'A', depth - 1)
  else:
    let
      rvh = steps('A', cy, depth - 1) + (dy-1) + steps(cy, cx, depth - 1) + (dx-1) + steps(cx, 'A', depth - 1)
      rhv = steps('A', cx, depth - 1) + (dx-1) + steps(cx, cy, depth - 1) + (dy-1) + steps(cy, 'A', depth - 1)
    result = min(rvh, rhv)

proc full_steps(s: string, depth: int): int =
  for (s,d) in zip("A" & s[0 .. ^2], s):
    result += base_steps(s, d, depth, num_pad)

proc puzzle2(s: string, depth: int): int =
  full_steps(s, depth) * parseInt(s[0 .. ^2])

for cmd in data:
# echo cmd, ": ", full_steps(cmd, 2)
  assert puzzle2(cmd, 2) == puzzle1(cmd)

echo "Puzzle 2: ", data.mapIt(puzzle2(it, 25)).foldl(a + b)

var debug_memo: Table[(char, char, int), string]

proc debug_base_steps(start, dest: char, depth: int, pad: Table[char, Point]): string

proc debug_steps(start, dest: char, depth: int): string =
  if start == dest:
    return "A"
  elif debug_memo.hasKey((start, dest, depth)):
    return debug_memo[(start, dest, depth)]
  result = debug_base_steps(start, dest, depth, dir_pad)
  echo repeat("  ", 4 - depth), "storing(", start, ", ", dest, ", ", depth, "): ", result
  debug_memo[(start, dest, depth)] = result

proc debug_base_steps(start, dest: char, depth: int, pad: Table[char, Point]): string =
  let
    ps = pad[start]
    pd = pad[dest]
    cx = if ps.x < pd.x: '>' elif ps.x > pd.x: '<' else: '.'
    cy = if ps.y < pd.y: 'v' elif ps.y > pd.y: '^' else: '.'
    dx = abs (ps.x - pd.x)
    dy = abs (ps.y - pd.y)
  if depth == 0:
    if pad['*'] == (x: pd.x, y: ps.y):
      return repeat(cy, dy) & repeat(cx, dx) & "A"
    else:
      return repeat(cx, dx) & repeat(cy, dy) & "A"

  if dx == 0:
    assert dy > 0
    echo repeat("  ", 4 - depth), "expanding(", start, ", ", dest, ", ", depth, "): \"", repeat(cy, dy), "A\""
    result = debug_steps('A', cy, depth - 1) & repeat('A', dy-1) & debug_steps(cy, 'A', depth - 1)
  elif dy == 0:
    assert dx > 0
    echo repeat("  ", 4 - depth), "expanding(", start, ", ", dest, ", ", depth, "): \"", repeat(cx, dx), "A\""
    result = debug_steps('A', cx, depth - 1) & repeat('A', dx-1) & debug_steps(cx, 'A', depth - 1)
  elif pad['*'] == (x: pd.x, y: ps.y):
    echo repeat("  ", 4 - depth), "expanding(", start, ", ", dest, ", ", depth, "): \"", repeat(cy, dy), repeat(cx, dx), "A\""
    result = debug_steps('A', cy, depth - 1) & repeat('A', dy-1) & debug_steps(cy, cx, depth - 1) & repeat('A', dx-1) & debug_steps(cx, 'A', depth - 1)
  elif pad['*'] == (x: ps.x, y: pd.y):
    echo repeat("  ", 4 - depth), "expanding(", start, ", ", dest, ", ", depth, "): \"", repeat(cx, dx), repeat(cy, dy), "A\""
    result = debug_steps('A', cx, depth - 1) & repeat('A', dx-1) & debug_steps(cx, cy, depth - 1) & repeat('A', dy-1) & debug_steps(cy, 'A', depth - 1)
  else:
    echo repeat("  ", 4 - depth), "expanding(", start, ", ", dest, ", ", depth, "): \"", repeat(cx, dx), repeat(cy, dy), "A\" and \"", repeat(cy, dy), repeat(cx, dx), "A\""
    let
      rvh = debug_steps('A', cy, depth - 1) & repeat('A', dy-1) & debug_steps(cy, cx, depth - 1) & repeat('A', dx-1) & debug_steps(cx, 'A', depth - 1)
      rhv = debug_steps('A', cx, depth - 1) & repeat('A', dx-1) & debug_steps(cx, cy, depth - 1) & repeat('A', dy-1) & debug_steps(cy, 'A', depth - 1)
    if len(rvh) < len(rvh):
      echo repeat("  ", 4 - depth), "↳ choosing \"", repeat(cy, dy), repeat(cx, dx), "A\" (", len(rvh), " over ", len(rhv), ")"
      result = rvh
    else:
      echo repeat("  ", 4 - depth), "↳ choosing \"", repeat(cx, dx), repeat(cy, dy), "A\" (", len(rhv), " over ", len(rvh), ")"
      result = rhv

proc debug_full_steps(s: string, depth: int): string =
  for (s,d) in zip("A" & s[0 .. ^2], s):
    result &= debug_base_steps(s, d, depth, num_pad)

# for cmd in data:
#  echo cmd, ": ", debug_full_steps(cmd, 2)
