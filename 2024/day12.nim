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

from std/sequtils import foldl, mapIt, toSeq
from std/tables   import `[]=`, getOrDefault, initTable, pairs, Table, values

# Read input file

let data = stdin.lines().toSeq()

# Puzzle 1

func reg(d: seq[string]): seq[seq[int]] =
  let dirs = @[(-1, 0), (1, 0), (0, -1), (0, 1)]
  result = d.mapIt(it.mapIt(0 * ord(it)))
  var n_reg = 0
  for start_y in 0 ..< len(d):
    for start_x in 0 ..< len(d[start_y]):
      if result[start_y][start_x] > 0:
        continue
      n_reg += 1
      result[start_y][start_x] = n_reg
      var queue = dirs.mapIt((start_x + it[0], start_y + it[1]))
      while len(queue) > 0:
        let
          x = queue[0][0]
          y = queue[0][1]
        queue = queue[1 .. ^1]
        if y < 0 or y >= len(d) or x < 0 or x >= len(d[y]) or result[y][x] > 0 or d[y][x] != d[start_y][start_x]:
          continue
        result[y][x] = n_reg
        for d in dirs:
          queue.add((x + d[0], y + d[1]))

proc cost1(d: seq[seq[int]], n: int): int =
  let dirs = @[(-1, 0), (1, 0), (0, -1), (0, 1)]
  var area = 0
  var peri = 0
  for y in 0 ..< len(d):
    for x in 0 ..< len(d[y]):
      if d[y][x] != n:
        continue
      area += 1
      for dir in dirs:
        let
          dx = x + dir[0]
          dy = y + dir[1]
        if dy < 0 or dy >= len(d) or dx < 0 or dx >= len(d[dy]) or d[dy][dx] != n:
          peri += 1
  result = area * peri
# echo n, ": ", area, " * ", peri

let
  regions = reg(data)
  n_regs = regions.mapIt(max(it)).max()

echo "Puzzle 1: ", (1 .. n_regs).mapIt(cost1(regions, it)).foldl(a + b)

# Puzzle 2

proc inc(t: var Table[(int, int), int], x, y: int): void =
  t[(x, y)] = t.getOrDefault((x, y), 0) + 1

proc cost2(d: seq[seq[int]], n: int): int =
  let dirs = @[(-1, 0), (1, 0), (0, -1), (0, 1)]
  var area = 0
  var h_corners = initTable[(int, int), int]()
  var v_corners = initTable[(int, int), int]()
  for y in 0 ..< len(d):
    for x in 0 ..< len(d[y]):
      if d[y][x] != n:
        continue
      area += 1
      for dir in dirs:
        let
          dx = x + dir[0]
          dy = y + dir[1]
        if dy < 0 or dy >= len(d) or dx < 0 or dx >= len(d[dy]) or d[dy][dx] != n:
          if dir[0] == 0:
            v_corners.inc(x, max(y, dy))
            v_corners.inc(x + 1, max(y, dy))
          if dir[1] == 0:
            h_corners.inc(max(x, dx), y)
            h_corners.inc(max(x, dx), y + 1)
  var corners = 0
  for k, v in h_corners:
    if v_corners.getOrDefault(k, 0) == v:
      corners += v
  result = area * corners
# echo n, ": ", area, " * ", corners

echo "Puzzle 2: ", (1 .. n_regs).mapIt(cost2(regions, it)).foldl(a + b)
