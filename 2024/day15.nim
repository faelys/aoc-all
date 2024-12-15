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

from std/math     import gcd
from std/sequtils import map, mapIt, toSeq
from std/strutils import find, join, replace

# Read input file

type
  Point = tuple
    x, y: int
  Input_Data = tuple
    map: seq[string]
    start: Point
    moves: string

func `+`(a, b: Point): Point = (x: a.x + b.x, y: a.y + b.y)
proc `+=`(a: var Point, b: Point): void = a = a + b
func `-`(a, b: Point): Point = (x: a.x - b.x, y: a.y - b.y)
func `[]`(m: seq[string], p: Point): char = m[p.y][p.x]
proc `[]=`(m: var seq[string], p: Point, c: char): void = m[p.y][p.x] = c

func process_data(data: seq[string]): Input_Data =
  var
    in_map = true
    has_start = false
    moves: seq[string]
  for s in data:
    if s == "":
      assert in_map
      in_map = false
    elif in_map:
      let sx = s.find('@')
      if sx >= 0:
        assert not has_start
        result.start = (x: sx, y: len(result.map))
        has_start = true
      result.map.add(s)
    else:
      moves.add(s)
  assert not in_map
  assert has_start
  result.moves = moves.join()

let
  data = process_data(stdin.lines().toSeq())

# Puzzle 1

func find_robot(m: seq[string]): Point =
  for y in 0 ..< len(m):
    let sx = m[y].find('@')
    if sx > 0:
      return (x: sx, y: y)
  assert false

func direction(c: char): Point =
  case c
  of '<':
    return (x: -1, y: 0)
  of '>':
    return (x: 1, y: 0)
  of 'v':
    return (x: 0, y: 1)
  of '^':
    return (x: 0, y: -1)
  else:
    assert false

func valid(p: Point, m: seq[string]): bool =
  p.x >= 0 and p.y >= 0 and p.y < len(m) and p.x < len(m[p.y])

func move1(m: seq[string], c: char): seq[string] =
  let
    d = direction(c)
    start = find_robot(m)
  result = m
  var dest = start + d
  while valid(dest, m) and m[dest] == 'O':
    dest += d
  if valid(dest, m) and m[dest] == '.':
    result[dest] = result[dest - d]
    result[start + d] = result[start]
    result[start] = '.'
  else:
    return m

proc display(m: seq[string]): void =
  for s in m:
    echo s

proc moves1(map: seq[string], moves: string): seq[string] =
  result = map
  for c in moves:
    result = move1(result, c)
# display(result)

func answer(m: seq[string]): int =
  result = 0
  for y in 0 ..< len(m):
    for x in 0 ..< len(m[y]):
      if m[y][x] == 'O' or m[y][x] == '[':
        result += 100 * y + x

echo "Puzzle 1: ", answer(moves1(data.map, data.moves))

# Puzzle 2

let
  map2 = data.map.mapIt(it.replace("#", "##").replace("O", "[]").replace(".", "..").replace("@", "@."))

func move2(m: seq[string], c: char): seq[string] =
  let
    d = direction(c)
    start = find_robot(m)
  result = m
  result[start] = '.'

  if d.y == 0:
    var pos = start + d
    while valid(pos, m) and (m[pos] == '[' or m[pos] == ']'):
      result[pos] = m[pos - d]
      pos += d
    if valid(pos, m) and m[pos] == '.':
      result[pos] = m[pos - d]
    else:
      return m
  else:
    var pos = @[start + d]
    while len(pos) > 0:
      var next: seq[Point]
      for p in pos:
        case m[p]
        of '[':
          next.add(p + d)
          next.add(p + d + (x: 1, y: 0))
        of ']':
          next.add(p + d)
          next.add(p + d + (x: -1, y: 0))
        of '.':
          discard nil
        else:
          return m
      for p in next:
        result[p - d] = '.'
      for p in pos:
        result[p] = m[p - d]
      pos = next

proc moves2(map: seq[string], moves: string): seq[string] =
  result = map
  for c in moves:
    result = move2(result, c)
#   echo c
#   display(result)
# display(result)

echo "Puzzle 2: ", answer(moves2(map2, data.moves))
