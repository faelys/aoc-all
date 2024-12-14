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

from std/math     import gcd, lcm
from std/sequtils import foldl, map, mapIt, toSeq
from std/strutils import parseInt, split

# Read input file

type
  Point = tuple
    x, y: int
  Machine = tuple
    A, B, Prize: Point

proc process_text(s: seq[string]): seq[Machine] =
  assert len(s) mod 4 == 3
  for i in 0 .. len(s) div 4:
    let p1 = s[4*i].split(", ")
    let p2 = s[4*i+1].split(", ")
    let p3 = s[4*i+2].split(", ")
    assert len(p1) == 2
    assert len(p2) == 2
    assert len(p3) == 2
    assert p1[0][0 .. 11] == "Button A: X+"
    assert p1[1][0 .. 1] == "Y+"
    assert p2[0][0 .. 11] == "Button B: X+"
    assert p2[1][0 .. 1] == "Y+"
    assert p3[0][0 .. 8] == "Prize: X="
    assert p3[1][0 .. 1] == "Y="
    result.add((A: (x: parseInt(p1[0][12 .. ^1]), y: parseInt(p1[1][2 .. ^1])), B: (x: parseInt(p2[0][12 .. ^1]), y: parseInt(p2[1][2 .. ^1])), Prize: (x: parseInt(p3[0][9 .. ^1]), y: parseInt(p3[1][2 .. ^1]))))

let data = process_text(stdin.lines().toSeq())

# Puzzle 1

proc brute_cost(m: Machine): int =
  result = 0
# echo "Machine: ", m
  for nA in 0 .. 100:
    for nB in 0 .. 100:
#     if m.A.x * nA + m.B.x * nB == m.Prize.x:
#       echo "  X (", nA, ", ", nB, ")"
#     if m.A.y * nA + m.B.y * nB == m.Prize.y:
#       echo "  Y (", nA, ", ", nB, ")"
      if m.A.x * nA + m.B.x * nB == m.Prize.x and m.A.y * nA + m.B.y * nB == m.Prize.y:
        if result == 0 or result > 3 * nA + nB:
          result = 3 * nA + nB

echo  "Puzzle 1: ", data.map(brute_cost).foldl(a + b)

# Puzzle 2
# 957820661643 is too low
# 79320801324501 is too low
# 79320806751012 is too low

proc cost(m: Machine): int =
  # Looking for number of presses kA,kB so that:
  # {1}: m.Prize.x = kA * m.A.x + kB * m.B.x
  # {2}: m.Prize.y = kA * m.A.y + kB * m.B.y
  # Eliminating kB or kA:
  # {3}: {1} * m.B.y - {2} * m.B.x
  #      m.Prize.x * m.B.y - m.Prize.y * m.B.x
  #       = kA * (m.A.x * m.B.y - m.A.y * m.B.x)
  # {4}: {2} * m.A.x - {1} * m.A.y
  #      m.Prize.y * m.A.x - m.Prize.x * m.A.y
  #       = kB * (m.A.x * m.B.y - m.A.y * m.B.x)
  let
    dividend_A = m.Prize.x * m.B.y - m.Prize.y * m.B.x
    dividend_B = m.Prize.y * m.A.x - m.Prize.x * m.A.y
    divisor = m.A.x * m.B.y - m.A.y * m.B.x
  assert divisor != 0
  if dividend_A mod divisor != 0 or dividend_B mod divisor != 0:
    return 0
  let
    kA = dividend_A div divisor
    kB = dividend_B div divisor
  assert m.Prize.x == kA * m.A.x + kB * m.B.x
  assert m.Prize.y == kA * m.A.y + kB * m.B.y
  return 3 * kA + kB

for m in data:
  let
    c1 = brute_cost(m)
    c2 = cost(m)
  assert c1 == c2 or c1 == 0

func make2(m: Machine): Machine =
  (A: m.A, B: m.B, Prize: (x: m.Prize.x + 10000000000000, y: m.Prize.y + 10000000000000))

proc puzzle2(s: seq[Machine]): int =
  result = 0
  for i in 0 ..< len(s):
    result += cost(make2(s[i]))

echo "Puzzle 2: ", puzzle2(data), "                "
