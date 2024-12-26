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

from std/sequtils  import allIt, mapIt, zip

# Read input file

proc read_input(): (seq[seq[int]], seq[seq[int]]) =
  var
    in_key = false
    in_lock = false
    height = 0
    acc: seq[int]
  for line in stdin.lines():
    if line == "":
      assert in_key or in_lock
      assert not (in_key and in_lock)
      if in_key:
        result[0].add(acc)
      else:
        result[1].add(acc.mapIt(height - 2 - it))
      in_key = false
      in_lock = false
    elif in_key or in_lock:
      let c = if in_key: '#' else: '.'
      acc = zip(acc, line).mapIt(if it[1] == c: height else: it[0])
      height += 1
    else:
      if line.allIt(it == '#'):
        in_key = true
      elif line.allIt(it == '.'):
        in_lock = true
      else:
        assert false
      height = 1
      acc = line.mapIt(0)
  if in_key:
    result[0].add(acc)
  elif in_lock:
    result[1].add(acc.mapIt(height - 2 - it))

let (keys, locks) = read_input()

# echo "Keys:"
# for k in keys:
#   echo "  ", k
# echo "Locks:"
# for l in locks:
#   echo "  ", l

# Puzzle 1

var answer = 0
for l in locks:
  for k in keys:
    if zip(l,k).allIt(it[0] + it[1] <= 5):
      answer += 1

echo "Puzzle 1: ", answer
