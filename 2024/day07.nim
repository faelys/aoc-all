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

from std/sequtils  import concat, filter, foldl, map, mapIt, toSeq
from std/strutils  import parseInt, split, splitWhitespace

# Read input file

func parse_line(l: string): seq[int] =
  let halves = l.split(": ")
  assert len(halves) == 2
  result = concat(@[parseInt(halves[0])], halves[1].splitWhitespace().map(parseInt))

let data = stdin.lines().toSeq().map(parse_line)

# First puzzle
# 354 is too low
# 465092164034 is too low

func is_good(target, acc: int, terms: seq[int]): bool =
  if len(terms) == 0:
    result = acc == target
  elif acc > target:
    result = false
  else:
    result = is_good(target, acc * terms[0], terms[1 .. ^1]) or
             is_good(target, acc + terms[0], terms[1 .. ^1])

func line_is_good(l: seq[int]): bool =
  is_good(l[0], l[1], l[2 .. ^1])

echo "Puzzle 1: ", data.filter(line_is_good).mapIt(it[0]).foldl(a + b)

# Second puzzle
# 54566744875618 is too low
# 68296976306294 is too low

func int_cat(a, b: int): int =
  var f = 10
  while f <= b:
    f *= 10
  result = a * f + b

func is_good2(target, acc: int, terms: seq[int]): bool =
  if len(terms) == 0:
    result = acc == target
  elif acc > target:
    result = false
  else:
    result = is_good2(target, int_cat(acc, terms[0]), terms[1 .. ^1]) or
             is_good2(target, acc * terms[0], terms[1 .. ^1]) or
             is_good2(target, acc + terms[0], terms[1 .. ^1])

func line_is_good2(l: seq[int]): bool =
  is_good2(l[0], l[1], l[2 .. ^1])

echo "Puzzle 2: ", data.filter(line_is_good2).mapIt(it[0]).foldl(a + b)
