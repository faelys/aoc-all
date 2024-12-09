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
from std/sequtils import addUnique, concat, deduplicate, toSeq

# Read input file

type Point = tuple
  x, y: int

func process_data(data: seq[string]): array[62, seq[Point]] =
  var i: int
  for y in 0 ..< len(data):
    for x in 0 ..< len(data[y]):
      case data[y][x]
      of '.':
        continue
      of '0' .. '9':
        i = ord(data[y][x]) - ord('0')
      of 'A' .. 'Z':
        i = ord(data[y][x]) - ord('A') + 10
      of 'a' .. 'z':
        i = ord(data[y][x]) - ord('a') + 36
      else:
        assert(false)

      result[i].add((x, y))

let
  data = stdin.lines().toSeq()
  nlines = len(data)
  ncols = len(data[0])
  coord = process_data(data)

# Puzzle 1

func antinodes(s: seq[Point], corner: Point): seq[Point] =
  for i in 0 ..< len(s):
    for j in 0 ..< len(s):
      if i == j:
        continue
      let n = (x: 2*s[i].x - s[j].x, y: 2*s[i].y - s[j].y)
      if n.x >= 0 and n.y >= 0 and n.x <= corner.x and n.y <= corner.y:
        result.addUnique(n)

func all_antinodes(d: openArray[seq[Point]], corner: Point): seq[Point] =
  for s in d:
    result = deduplicate(concat(result, antinodes(s, corner)))

echo "Puzzle 1: ", len(all_antinodes(coord, (ncols - 1, nlines - 1)))

# Puzzle 2

func antinodes2(s: seq[Point], corner: Point): seq[Point] =
  for i in 0 ..< len(s):
    for j in 0 ..< len(s):
      if i == j:
        continue
      let
        ld = (x: s[i].x - s[j].x, y: s[i].y - s[j].y)
        g = gcd(ld.x, ld.y)
        d = (x: ld.x div g, y: ld.y div g)
      var p = s[i]
      while p.x >= 0 and p.y >= 0 and p.x <= corner.x and p.y <= corner.y:
        result.addUnique(p)
        p.x += d.x
        p.y += d.y

func all_antinodes2(d: openArray[seq[Point]], corner: Point): seq[Point] =
  for s in d:
    result = deduplicate(concat(result, antinodes2(s, corner)))

echo "Puzzle 2: ", len(all_antinodes2(coord, (ncols - 1, nlines - 1)))
