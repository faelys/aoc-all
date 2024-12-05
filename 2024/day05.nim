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

from std/sequtils import allIt, count, deduplicate, filterIt, foldl, map, mapIt, toSeq, concat
from std/strutils import parseInt, split

# Read input file

func process_input(s: seq[string], sep: string): seq[seq[int]] =
  s.mapIt(it.split(sep)).filterIt(it.len() >= 2).mapIt(it.map(parseInt))

func listmax(s: seq[int]): int = s.foldl(max(a, b))
func listmin(s: seq[int]): int = s.foldl(min(a, b))

let
  raw_data = stdin.lines().toSeq()
  part1 = process_input(raw_data, "|")
  part2 = process_input(raw_data, ",")
  smallest = min(part1.map(listmin).listmin(), part2.map(listmin).listmin())
  largest = max(part1.map(listmax).listmax(), part2.map(listmax).listmax())

# Puzzle 1

var needs: seq[seq[int]]

for i in 0 .. largest:
  needs.add(part1.filterIt(it[1] == i).mapIt(it[0]))

proc is_ordered(s: seq[int]): bool =
  len(s) < 2 or
  (s[1 .. ^1].allIt(needs[s[0]].count(it) == 0) and is_ordered(s[1 .. ^1]))

proc score1(s: seq[int]): int =
  if is_ordered(s):
    result = s[(len(s)-1) div 2]

echo "Puzzle 1: ", part2.map(score1).foldl(a + b)

# Puzzle 2

proc local_min(s: seq[int]): int =
  result = -1
  for i in 0 ..< len(s):
    if s.allIt(needs[s[i]].count(it) == 0):
      return s[i]

proc local_sort(s: seq[int]): seq[int] =
  if len(s) < 2:
    return s
  else:
    let m = local_min(s)
    result = local_sort(s.filterIt(it != m))
    result.add(m)

proc score2(s: seq[int]): int =
  if not is_ordered(s):
    let s2 = local_sort(s)
    result = s2[(len(s2) - 1) div 2]

echo "Puzzle 2: ", part2.map(score2).foldl(a + b)
