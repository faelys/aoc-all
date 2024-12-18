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

from std/sequtils import addUnique, concat, mapIt, toSeq, zip
from std/tables   import `[]`, `[]=`, contains, getOrDefault, initTable, Table

# Read input file

let data = stdin.lines().toSeq()

# Puzzles

type
  Direction = enum
    north, east, south, west
  Point = tuple
    x, y: int
  Node = tuple
    p: Point
    dir: Direction
  PathData = tuple
    score: int
    prev: seq[Node]

func delta(d: Direction): Point =
  case d
  of north: (x: 0, y: -1)
  of south: (x: 0, y: 1)
  of east: (x: 1, y: 0)
  of west: (x: -1, y: 0)

func `+`(a, b: Point): Point = (x: a.x + b.x, y: a.y + b.y)
func `[]`(m: seq[string], p: Point): char = m[p.y][p.x]

func start(d: seq[string], c: char): Node =
  var found = false
  for y in 0 ..< len(d):
    for x in 0 ..< len(d[y]):
      if d[y][x] == c:
        assert not found
        found = true
        result = (p: (x: x, y: y), dir: east)
  assert found

func next(unvisited: seq[Node], path: Table[Node, PathData]): (int, int) =
  result = (0, path[unvisited[0]].score)
  for i in 1 ..< len(unvisited):
    let s = path[unvisited[i]].score
    if i == 0 or s < result[1]:
      result = (i, s)

proc update(unvisited: var seq[Node], path: var Table[Node, PathData], d: seq[string], prev, n: Node, s: int): void =
  if d[n.p] == '#':
    return
  if not path.contains(n) or path[n].score > s:
    unvisited.addUnique(n)
    path[n] = (score: s, prev: @[prev])
  elif path[n].score == s:
    path[n].prev.add(prev)

func search(d: seq[string]): (Table[Node, PathData], int) =
  var
    unvisited = @[start(d, 'S')]
    path = initTable[Node, PathData]()
    finish = start(d, 'E').p
    found = false
  path[unvisited[0]] = (score: 0, prev: @[])
  while len(unvisited) > 0:
    let
      (i, s) = next(unvisited, path)
      n = unvisited[i]
    if found:
      if s > result[1]:
        return
    elif n.p == finish:
      result = (path, s)
      found = true
    unvisited = concat(unvisited[0 ..< i], unvisited[i + 1 ..< len(unvisited)])
    update(unvisited, path, d, n, (p: n.p + delta(n.dir), dir: n.dir), s + 1)
    for dir in Direction:
      update(unvisited, path, d, n, (p: n.p, dir: dir), s + 1000)
  assert false

let (path_data, answer1) = search(data)
echo "Puzzle 1: ", answer1

func answer2(path: Table[Node, PathData], p: Point): int =
  var
    visited = initTable[Point, bool]()
    queue: seq[Node]
  for d in Direction:
    let n = (p: p, dir: d)
    if path.contains(n):
      queue.add(n)
  assert len(queue) > 0
  result = 0
  while len(queue) > 0:
    if not visited.contains(queue[0].p):
      visited[queue[0].p] = true
      result += 1
    queue = concat(queue[1 .. ^1], path[queue[0]].prev)

echo "Puzzle 2: ", answer2(path_data, start(data, 'E').p)
