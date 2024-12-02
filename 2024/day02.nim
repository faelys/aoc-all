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

from std/sequtils  import allIt, filter, map, mapIt, toSeq, zip
from std/strutils  import parseInt, splitWhitespace

# Read input file

let data = stdin.lines().toSeq().mapIt(it.splitWhitespace().map(parseInt))

# First puzzle

func deltas(s: seq[int]): seq[int] =
  zip(s[1 .. ^1], s[0 .. ^2]).mapIt(it[0] - it[1])

func isSafe(d: seq[int]): bool =
  (allIt(d, it < 0) or allIt(d, it > 0)) and allIt(d, abs(it) <= 3)

echo "Puzzle 1: ", len(data.map(deltas).filter(isSafe))

# Second puzzle
# 611 is too low

func dampenIndex(s: seq[int], iinc, idec, idelta: var int) =
  iinc = 0
  idec = 0
  idelta = 0
  for i in 1 .. len(s) - 1:
    if iinc == 0 and s[i] <= s[i-1]:
      iinc = i
    if idec == 0 and s[i] >= s[i-1]:
      idec = i
    if idelta == 0 and abs(s[i] - s[i-1]) > 3:
      idelta = i
    if iinc > 0 and idec > 0 and idelta > 0:
      break

func isDampenedSafe(s: seq[int]): bool =
  var iinc, idec, idelta = 0
  dampenIndex(s, iinc, idec, idelta)
  for i in @[iinc, iinc-1, idec, idec-1, idelta, idelta-1]:
    if i > 0 and isSafe(deltas(s[0 .. i-1] & s[i + 1 .. ^1])):
      return true

func isSafe2(s: seq[int]): bool =
  let d = deltas(s)
  result = isSafe(d) or isSafe(d[1 .. ^1]) or isDampenedSafe(s)

echo "Puzzle 2: ", len(data.filter(isSafe2))
