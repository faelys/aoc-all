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

from std/re       import findAll, re
from std/sequtils import foldl, map, toSeq
from std/strutils import parseInt, split

# Read input file

let data = stdin.lines().toSeq()

# Puzzle 1

func summul(s: string): int =
  for subs in findAll(s, re"mul\(\d+,\d+\)"):
    let n = split(subs[4 .. ^2], ",").map(parseInt)
    result += foldl(n, a * b)

echo "Puzzle 1: ", data.map(summul).foldl(a + b)

# Puzzle 2

func dosummul(s: string): int =
  var active = true
  for subs in findAll(s, re"mul\(\d+,\d+\)|do\(\)|don't\(\)"):
    case subs[0 .. 3]
    of "do()":
      active = true
    of "don'":
      active = false
    of "mul(":
      if active:
        result += subs[4 .. ^2].split(",").map(parseInt).foldl(a * b)

echo "Puzzle 2: ", data.foldl(a & b).dosummul()
