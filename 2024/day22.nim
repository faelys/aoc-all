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

from std/sequtils import foldl, map, mapIt, toSeq, zip
from std/strutils import parseInt
from std/tables   import `[]`, `[]=`, getOrDefault, hasKey, pairs, Table

# Read input file

let data = stdin.lines().toSeq().map(parseInt)

# Puzzle 1

func step(n: int): int =
  let
    s1 = ((n * 64) xor n) mod 16777216
    s2 = ((s1 div 32) xor s1) mod 16777216
    s3 = ((s2 * 2048) xor s2) mod 16777216
  result = s3

let test_data = @[123, 15887950, 16495136, 527345, 704524, 1553684, 12683156, 11100544, 12249484, 7753432, 5908254]

for (a,b) in zip(test_data[0 .. ^2], test_data[1 .. ^1]):
  assert step(a) == b

func steps(n, s: int): int =
  result = n
  for i in 1 .. s:
    result = step(result)

echo "Puzzle 1: ", data.mapIt(steps(it, 2000)).foldl(a + b)

# Puzzle 2

var scores: Table[seq[int], int]
var max_score = 0

for seed in data:
  let
    s1 = step(seed)
    s2 = step(s1)
    s3 = step(s2)
  var
    last = s3
    d = @[0, (s1 mod 10) - (seed mod 10), (s2 mod 10) - (s1 mod 10), (s3 mod 10) - (s2 mod 10)]

  var seen: Table[seq[int], bool]
  for i in 4 .. 2000:
    let next = step(last)
    d = d[1 .. ^1]
    d.add((next mod 10) - (last mod 10))
    if not seen.hasKey(d):
      seen[d] = true
      let new_score = scores.getOrDefault(d, 0) + (next mod 10)
      scores[d] = new_score
      max_score = max(max_score, new_score)
    last = next

echo "Puzzle 2: ", max_score
