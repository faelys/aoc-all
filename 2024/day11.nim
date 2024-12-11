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

from std/math     import `^`
from std/sequtils import foldl, map, toSeq
from std/strutils import parseInt, splitWhitespace
from std/tables   import `[]=`, getOrDefault, initTable, pairs, Table, values

# Read input file

func tablize(s: seq[int]): Table[int, int] =
  result = initTable[int, int]()
  for i in s:
    result[i] = result.getOrDefault(i, 0) + 1

let
  data = stdin.readLine().splitWhitespace().map(parseInt)
  tdata = tablize(data)

# Puzzle 1
# 217379 is tool high

func digits(n: int): int =
  result = 1
  var ceiling = 10
  while ceiling <= n:
    result += 1
    ceiling *= 10

proc update(t: Table[int, int]): Table[int, int] =
  result = initTable[int, int]()
  for i, n in t.pairs:
    var nd = digits(i)
    if i == 0:
      result[1] = result.getOrDefault(1, 0) + n
    elif nd mod 2 == 1:
      let ni = i * 2024
      result[ni] = result.getOrDefault(ni, 0) + n
    else:
      let sp = 10 ^ (nd div 2)
      result[i div sp] = result.getOrDefault(i div sp, 0) + n
      result[i mod sp] = result.getOrDefault(i mod sp, 0) + n

proc updates(s: Table[int, int], n: int): Table[int, int] =
  result = s
  for i in 1 .. n:
    result = update(result)

echo "Puzzle 1: ", updates(tdata, 25).values().toSeq().foldl(a + b)
echo "Puzzle 2: ", updates(tdata, 75).values().toSeq().foldl(a + b)
