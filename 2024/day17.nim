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

from std/random   import randomize, rand
from std/sequtils import map, toSeq
from std/strutils import join, parseInt, split

# Read input file

let data = stdin.lines().toSeq()

assert len(data) == 5
assert data[0][0 .. 11] == "Register A: "
assert data[1][0 .. 11] == "Register B: "
assert data[2][0 .. 11] == "Register C: "
assert data[3] == ""
assert data[4][0 .. 8] == "Program: "

type
  Machine = tuple
    A, B, C, ip: int
    res: seq[int]

let
  init: Machine = (A: parseInt(data[0][12 .. ^1]), B: parseInt(data[1][12 .. ^1]), C: parseInt(data[2][12 .. ^1]), ip: 0, res: @[])
  program = data[4][9 .. ^1].split(",").map(parseInt)

# Puzzle 1

func combo(m: Machine, op: int): int =
  case op
  of 0 .. 3:
    result = op
  of 4:
    result = m.A
  of 5:
    result = m.B
  of 6:
    result = m.C
  else:
    assert false

func adv(m: Machine, op: int): Machine =
  result = m
  result.A = result.A shr combo(m, op)
  result.ip += 2

func bxl(m: Machine, op: int): Machine =
  result = m
  result.B = result.B xor op
  result.ip += 2

func bst(m: Machine, op: int): Machine =
  result = m
  result.B = combo(m, op) mod 8
  result.ip += 2

func jnz(m: Machine, op: int): Machine =
  result = m
  if result.A == 0:
    result.ip += 2
  else:
    result.ip = op

func op_out(m: Machine, op: int): Machine =
  result = m
  result.res.add(combo(m, op) mod 8)
  result.ip += 2

func bxc(m: Machine, op: int): Machine =
  result = m
  result.B = result.B xor result.C
  result.ip += 2

func bdv(m: Machine, op: int): Machine =
  result = m
  result.B = result.A shr combo(m, op)
  result.ip += 2

func cdv(m: Machine, op: int): Machine =
  result = m
  result.C = result.A shr combo(m, op)
  result.ip += 2

let
  opcodes = [adv, bxl, bst, jnz, bxc, op_out, bdv, cdv]

proc step(m: Machine, prg: seq[int]): Machine =
  result = opcodes[prg[m.ip]](m, prg[m.ip + 1])

proc run(m: Machine, prg: seq[int]): Machine =
  result = m
  while result.ip + 1 < len(prg):
    result = step(result, prg)

assert run((A: rand(9999), B: rand(9999), C: 9, ip: 0, res: @[]), @[2, 6]).B == 1
assert run((A: 10, B: rand(9999), C: rand(9999), ip: 0, res: @[]), @[5,0,5,1,5,4]).res == @[0,1,2]
assert run((A: 2024, B: rand(9999), C: rand(9999), ip: 0, res: @[]), @[0,1,5,4,3,0]).res == @[4,2,5,6,7,7,7,7,3,1,0]
assert run((A: 2024, B: rand(9999), C: rand(9999), ip: 0, res: @[]), @[0,1,5,4,3,0]).A == 0
assert run((A: rand(9999), B: 29, C: rand(9999), ip: 0, res: @[]), @[1,7]).B == 26
assert run((A: rand(9999), B: 2024, C: 43690, ip: 0, res: @[]), @[4,0]).B == 44354

echo "Puzzle 1: ", run(init, program).res.join(",")

# Puzzle 2
# 164515378771682 is too low

let op_names = ["adv", "bxl", "bst", "jnz", "bxc", "op_out", "bdv", "cdv"]

for i in 0 .. (len(program)-2) div 2:
  echo 2*i, ": ", op_names[program[2*i]], " ", program[2*i+1]

proc answer2(im: Machine, prg: seq[int]): int =
  var
    m = im
    q = @[0]
  while len(q) > 0:
    let h = q[0]
    q = q[1 .. ^1]
    for i in 0 .. 7:
      m.A = h * 8 + i
      let s = run(m, prg).res

      if s == prg:
        return m.A
      elif s == prg[^len(s) .. ^1]:
        q.add(m.A)
      else:
        assert len(s) == 1 or s[1..^1] == prg[^(len(s)-1) .. ^1]
  assert false

echo "Puzzle 2: ", answer2(init, program)
