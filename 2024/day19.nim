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

from std/sequtils import foldl, filter, map, toSeq
from std/strutils import split, startsWith
from std/tables   import `[]`, `[]=`, hasKey, initTable

# Read input file

func read_input(s: seq[string]): (seq[string], seq[string]) =
  assert len(s) >= 3
  assert s[1] == ""
  result = (s[0].split(", "), s[2 .. ^1])

let (patterns, designs) = read_input(stdin.lines().toSeq())

# Puzzle 1

var memo1 = initTable[string, bool]()
memo1[""] = true

proc is_possible(design: string): bool =
  if memo1.hasKey(design):
    return memo1[design]

  result = false
  for p in patterns:
    assert len(p) > 0
    if design.startsWith(p) and is_possible(design[len(p) .. ^1]):
      result = true
      break
  memo1[design] = result

echo "Puzzle 1: ", designs.filter(is_possible).len()

# Puzzle 2

var memo2 = initTable[string, int]()
memo2[""] = 1

proc arrangements(design: string): int =
  if memo2.hasKey(design):
    return memo2[design]

  result = 0
  for p in patterns:
    assert len(p) > 0
    if design.startsWith(p):
      result += arrangements(design[len(p) .. ^1])
  memo2[design] = result

echo "Puzzle 2: ", designs.map(arrangements).foldl(a + b)
