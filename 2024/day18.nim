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

from std/sequtils import addUnique, concat, map, mapIt, newSeqWith, toSeq
from std/strutils import split, parseInt, repeat
from std/tables   import `[]`, `[]=`, contains, getOrDefault, initTable, Table

# Read input file

type
  Point = tuple
    x, y: int

func `+`(a, b: Point): Point = (x: a.x + b.x, y: a.y + b.y)
func `[]`(m: seq[string], p: Point): char = m[p.y][p.x]
proc `[]=`(m: var seq[string], p: Point, c: char): void = m[p.y][p.x] = c

func seq_to_point(s: seq[int]): Point =
  assert len(s) == 2
  result = (x: s[0], y: s[1])

let
  data = stdin.lines().toSeq().mapIt(seq_to_point(it.split(",").map(parseInt)))
  xmax = max(data.mapIt(it[0]))
  ymax = max(data.mapIt(it[1]))

# Puzzle 1

func next(unvisited: seq[Point], scores: Table[Point, int]): (int, int) =
  result = (0, scores[unvisited[0]])
  for i in 1 ..< len(unvisited):
    let s = scores[unvisited[i]]
    if i == 0 or s < result[1]:
      result = (i, s)

proc update(unvisited: var seq[Point], scores: var Table[Point, int], d: seq[string], p: Point, s: int): void =
  if p.x < 0 or p.y < 0 or p.y >= len(d) or p.x >= len(d[p.y]) or d[p] == '#':
    return
  if not scores.contains(p) or scores[p] > s:
    unvisited.addUnique(p)
    scores[p] = s

func to_map(blocks: seq[Point], xmax, ymax: int): seq[string] =
  result = newSeqWith(ymax + 1, repeat('.', xmax + 1))
  for b in blocks:
    result[b] = '#'

func answer1(blocks: seq[Point], xmax, ymax: int): int =
  let m = to_map(blocks, xmax, ymax)
  var
    scores = initTable[Point, int]()
    unvisited = @[(x: 0, y: 0)]
  scores[unvisited[0]] = 0
  while len(unvisited) > 0:
    let
      (i, s) = next(unvisited, scores)
      p = unvisited[i]
    if p.x == xmax and p.y == ymax:
      return s
    unvisited = concat(unvisited[0 ..< i], unvisited[i + 1 .. ^1])
    update(unvisited, scores, m, (x: p.x + 1, y: p.y), s + 1)
    update(unvisited, scores, m, (x: p.x - 1, y: p.y), s + 1)
    update(unvisited, scores, m, (x: p.x, y: p.y + 1), s + 1)
    update(unvisited, scores, m, (x: p.x, y: p.y - 1), s + 1)
  return -1

echo "Puzzle 1: ", answer1(data, xmax, ymax)

# Puzzle 2

var
  bottom = 0
  top = len(data) - 1

if answer1(data[0 .. bottom], xmax, ymax) >= 0 and answer1(data[0 .. top], xmax, ymax) < 0:
# echo "starting with ", bottom, " .. ", top
  while top - bottom >= 2:
    let
      mid = bottom + (top - bottom) div 2
      s = answer1(data[0 .. mid], xmax, ymax)
#   echo mid, ": ", s
    if s >= 0:
      bottom = mid
    else:
      top = mid
  echo "Puzzle 2: ", data[top].x, ",", data[top].y, " (", top, ")"
