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

from std/sequtils import toSeq

# Read input file

let data = stdin.lines().toSeq()
let nlines = len(data)
let ncols = len(data[0])

# Puzzle 1

proc isXmas(x, y, dx, dy: int): bool =
  x + 3*dx >= 0 and
  x + 3*dx < ncols and
  y + 3*dy >= 0 and
  y + 3*dy < ncols and
  data[y][x] == 'X' and
  data[y+dy][x+dx] == 'M' and
  data[y+2*dy][x+2*dx] == 'A' and
  data[y+3*dy][x+3*dx] == 'S'

const dirs = @[(1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1), (0,-1), (1, -1)]

proc countXmasAt(x, y: int): int =
  result = 0
  if data[y][x] == 'X':
    for d in dirs:
      if isXmas(x, y, d[0], d[1]):
        result += 1

proc countXmas(): int =
  result = 0
  for y in 0 ..< nlines:
    for x in 0 ..< ncols:
      result += countXmasAt(x, y)

echo "Puzzle 1: ", countXmas()

# Puzzle 2

const dirs2 = @[(1,1), (-1,1), (-1,-1), (1,-1)]

proc isMas(x, y, dx, dy: int): bool =
  data[y-dy][x-dx] == 'M' and
  data[y+dy][x+dx] == 'S'

proc is2Mas(x, y, i: int): bool =
  let
    i2 = (i + 1) mod len(dirs2)
    d1 = dirs2[i]
    d2 = dirs2[i2]
  result = isMas(x, y, d1[0], d1[1]) and isMas(x, y, d2[0], d2[1])

proc countMasAt(x, y: int): int =
  result = 0
  if data[y][x] == 'A':
    for i in 0 ..< len(dirs2):
      if is2Mas(x, y, i):
        result += 1

proc countMas(): int =
  result = 0
  for y in 1 .. nlines-2:
    for x in 1 .. ncols-2:
      result += countMasAt(x, y)

echo "Puzzle 2: ", countMas()
