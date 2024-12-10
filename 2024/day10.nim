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

from std/sequtils import addUnique, foldl, mapIt, toSeq

# Read input file

let data = stdin.lines().toSeq().mapIt(it.mapIt(ord(it) - ord('0')))

# Puzzle 1

type Point = tuple
  x, y: int

func start_points(d: seq[seq[int]]): seq[Point] =
  for y in 0 ..< len(d):
    for x in 0 ..< len(d[y]):
      if d[y][x] == 0:
        result.add((x: x, y: y))

func update_points(d: seq[seq[int]], points: seq[Point]): seq[Point] =
  for p in points:
    let v = d[p.y][p.x] + 1
    if p.x > 0 and d[p.y][p.x-1] == v:
      result.addUnique((x: p.x-1, y: p.y))
    if p.y > 0 and d[p.y-1][p.x] == v:
      result.addUnique((x: p.x, y: p.y-1))
    if p.x < len(d[p.y])-1 and d[p.y][p.x+1] == v:
      result.addUnique((x: p.x+1, y: p.y))
    if p.y < len(d)-1 and d[p.y+1][p.x] == v:
      result.addUnique((x: p.x, y: p.y+1))

proc head_score(d: seq[seq[int]], pt: Point): int =
  var pts = @[pt]
  for v in d[pt.y][pt.x]+1 .. 9:
    pts = update_points(d, pts)
  result = len(pts)

echo "Puzzle 1: ", start_points(data).mapIt(head_score(data, it)).foldl(a + b)

# Puzzle 2

func update_points2(d: seq[seq[int]], points: seq[Point]): seq[Point] =
  for p in points:
    let v = d[p.y][p.x] + 1
    if p.x > 0 and d[p.y][p.x-1] == v:
      result.add((x: p.x-1, y: p.y))
    if p.y > 0 and d[p.y-1][p.x] == v:
      result.add((x: p.x, y: p.y-1))
    if p.x < len(d[p.y])-1 and d[p.y][p.x+1] == v:
      result.add((x: p.x+1, y: p.y))
    if p.y < len(d)-1 and d[p.y+1][p.x] == v:
      result.add((x: p.x, y: p.y+1))

proc head_score2(d: seq[seq[int]], pt: Point): int =
  var pts = @[pt]
  for v in d[pt.y][pt.x]+1 .. 9:
    pts = update_points2(d, pts)
  result = len(pts)

echo "Puzzle 2: ", start_points(data).mapIt(head_score2(data, it)).foldl(a + b)
