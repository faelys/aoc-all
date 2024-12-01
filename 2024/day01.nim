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

from std/math      import sum
from std/algorithm import sorted
from std/sequtils  import map, zip
from std/strutils  import parseInt, splitWhitespace
from std/tables    import getOrDefault, toCountTable

# Read input file

func splitInt(line: string): seq[int] =
  line.splitWhitespace().map(parseInt)

proc allLines(f: File): seq[string] =
  var line: string
  while readLine(f, line):
    result.add(line)

let data = stdin.allLines().map(splitInt)

# First puzzle

func nth[T](n: int): proc(s: seq[T]): T {.noSideEffect.} =
  proc(s: seq[T]): T {.noSideEffect.} = s[n]

let
  line1 = data.map(nth[int](0)).sorted()
  line2 = data.map(nth[int](1)).sorted()

func dist(l: (int, int)): int = abs(l[0] - l[1])

echo "Puzzle 1: ", sum(zip(line1, line2).map(dist))

# Second puzzle

let counts = toCountTable(line2)

echo "Puzzle 2: ",
  sum(line1.map(proc (i: int): int = i * counts.getOrDefault(i)))
