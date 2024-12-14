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

from std/math     import euclMod, lcm
from std/sequtils import allIt, foldl, map, mapIt, newSeqWith, toSeq
from std/strutils import join, parseInt, split

# Read input file

type
  Point = tuple
    x, y: int
  Robot = tuple
    p, v: Point

func process_line(s: string): Robot =
  let h = s.split(" ")
  assert len(h) == 2
  assert h[0][0 .. 1] == "p="
  assert h[1][0 .. 1] == "v="
  let
    p1 = h[0][2 .. ^1].split(",")
    p2 = h[1][2 .. ^1].split(",")
  assert len(p1) == 2
  assert len(p2) == 2
  result = (p: (x: parseInt(p1[0]), y: parseInt(p1[1])), v: (x: parseInt(p2[0]), y: parseInt(p2[1])))

let
  data = stdin.lines().toSeq().map(process_line)
  xlen = max(data.mapIt(it.p.x)) + 1
  ylen = max(data.mapIt(it.p.y)) + 1

# Puzzle 1

func forward(r: Robot, t, xmod, ymod: int): Robot =
  (p: (x: euclMod(r.p.x + t * r.v.x, xmod), y: euclMod(r.p.y + t * r.v.y, ymod)), v: r.v)

func safety_factor(s: seq[Robot], xmod, ymod: int): int =
  assert xmod mod 2 == 1
  assert ymod mod 2 == 1
  let
    hx = (xmod - 1) div 2
    hy = (ymod - 1) div 2
  var q = @[0, 0, 0, 0]
  for r in s:
    if r.p.y < hy:
      if r.p.x < hx:
        q[0] += 1
      elif r.p.x > hx:
        q[1] += 1
    elif r.p.y > hy:
      if r.p.x < hx:
        q[2] += 1
      elif r.p.x > hx:
        q[3] += 1
  result = q.foldl(a * b)

echo "Puzzle 1: ", data.mapIt(forward(it, 100, xlen, ylen)).safety_factor(xlen, ylen)

# Puzzle 2

func display_char(i: int): char =
  case i
  of 0:
    result = '.'
  of 1 .. 9:
    result = ($i)[0]
  else:
    result = '*'

proc display(s: seq[Robot], xmod, ymod: int): void =
  var map = newSeqWith(ymod, newSeqWith(xmod, 0))
  for r in s:
    map[r.p.y][r.p.x] += 1
  for line in map:
    echo line.map(display_char).join()

proc is_tree(s: seq[Robot], xmod, ymod: int): bool =
  assert xmod mod 2 == 1
  assert ymod mod 2 == 1
  # use a horizontal segment as a proxy for tree-likeness
  let line_size = 11
  var map = newSeqWith(ymod, newSeqWith(xmod, 0))
  for r in s:
    map[r.p.y][r.p.x] += 1
  for y in 0 ..< ymod:
    for x in 0 .. xmod - line_size - 1:
      if map[y][x ..< x + line_size].allIt(it > 0):
        for line in map:
          echo line.map(display_char).join()
        return true

proc find_tree(s0: seq[Robot], xmod, ymod: int): int =
  var s = s0
  result = 0
  while not is_tree(s, xmod, ymod):
    s = s.mapIt(forward(it, 1, xmod, ymod))
    result += 1
    if result > lcm(xmod, ymod):
      return -1

echo "Puzzle 2: ", find_tree(data, xlen, ylen)
