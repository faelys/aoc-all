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
from std/sequtils  import toSeq
from std/tables    import `[]`, `[]=`, getOrDefault, initTable, keys, pairs, Table

# Read input file

type
  Point = tuple
    x, y: int

func `+`(a, b: Point): Point = (x: a.x + b.x, y: a.y + b.y)
func `[]`(m: seq[string], p: Point): char = m[p.y][p.x]

func neighbors(m: seq[string], p: Point): seq[Point] =
  let s = @[(x: p.x + 1, y: p.y), (x: p.x - 1, y: p.y), (x: p.x, y: p.y - 1), (x: p.x, y: p.y + 1)]
  for n in s:
    if n.x >= 0 and n.y >= 0 and n.y < len(m) and n.x < len(m[n.y]) and m[n] != '#':
      result.add(n)

func race_path(m: seq[string]): seq[Point] =
  for y in 0 ..< len(m):
    for x in 0 ..< len(m[y]):
      if m[y][x] == 'S':
        assert len(result) == 0
        result.add((x: x, y: y))
  assert len(neighbors(m, result[0])) == 1
  result.add(neighbors(m, result[0])[0])

  while m[result[^1]] != 'E':
    let n = neighbors(m, result[^1])
    assert len(n) == 2
    if n[0] == result[^2]:
      result.add(n[1])
    else:
      assert n[1] == result[^2]
      result.add(n[0])

let
  data = stdin.lines().toSeq()
  path = race_path(data)

# Puzzle 1

func count_shortcuts1(s: seq[Point]): Table[int, int] =
  for start in 0 .. len(s) - 4:
    for dest in start + 3 .. len(s) - 1:
      if (s[start].x == s[dest].x and abs(s[start].y - s[dest].y) == 2) or (s[start].y == s[dest].y and abs(s[start].x - s[dest].x) == 2):
        let k = dest - start - 2
        result[k] = result.getOrDefault(k, 0) + 1

proc puzzle1(s: seq[Point]): void =
  let shortcuts = count_shortcuts1(path)
  var sum = 0

  for k, v in shortcuts.pairs:
    if k >= 100:
      sum += v

  if sum == 0:
    echo "Puzzle 1:"
    for k in shortcuts.keys.toSeq.sorted:
      if shortcuts[k] >= 2:
        echo " - There are ", shortcuts[k], " cheats saving ", k, " ps"
      else:
        echo " - There is one cheat saving ", k, " ps"
  else:
    echo "Puzzle 1: ", sum

puzzle1(path)

# Puzzle 2

func count_shortcuts2(s: seq[Point]): Table[int, int] =
  for start in 0 .. len(s) - 4:
    for dest in start + 3 .. len(s) - 1:
      let
        d = abs(s[start].x - s[dest].x) + abs(s[start].y - s[dest].y)
        k = dest - start - d
      if d <= 20 and k > 0:
        result[k] = result.getOrDefault(k, 0) + 1

proc puzzle2(s: seq[Point]): void =
  let shortcuts = count_shortcuts2(path)
  var sum = 0

  for k, v in shortcuts.pairs:
    if k >= 100:
      sum += v

  if sum == 0:
    echo "Puzzle 2:"
    for k in shortcuts.keys.toSeq.sorted:
      if k >= 50:
        echo " - There are ", shortcuts[k], " cheats saving ", k, " ps"
  else:
    echo "Puzzle 2: ", sum

puzzle2(path)
