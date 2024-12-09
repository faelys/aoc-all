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

from std/algorithm import sorted
from std/sequtils  import concat, filterIt, mapIt

# Read input file

let data = stdin.readLine().mapIt(ord(it) - ord('0'))

# Puzzle 1

func part(offset, times, id: int): int =
  (offset * times + (times * (times - 1) div 2)) * id

func checksum1(d: seq[int]): int =
  var
    left = 0
    right = (len(d) - 1) div 2
    todo = d[2*right]
    offset = 0
    gap = 0
  result = 0
  while left < right:
    if gap == 0:
      result += part(offset, d[2*left], left)
      offset += d[2*left]
      gap = d[2*left+1]
      left += 1
    elif todo == 0:
      right -= 1
      todo = d[2*right]
    else:
      let chunk = min(todo, gap)
      result += part(offset, chunk, right)
      offset += chunk
      gap -= chunk
      todo -= chunk
  result += part(offset, todo, right)

echo "Puzzle 1: ", checksum1(data)

# Puzzle 2
# 5135326805366 is too low
# 5105400512454 is too low
# 5396427849295 is too low

type F = tuple
  id, offset, size: int
type G = tuple
  offset, size: int

func make_files(d: seq[int]): seq[F] =
  var acc = d[0]
  result.add((id: 0, offset: 0, size: d[0]))
  for i in 1 .. (len(d) - 1) div 2:
    acc += d[2*i-1]
    result.add((id: i, offset: acc, size: d[2*i]))
    acc += d[2*i]

func make_gaps(d: seq[int]): seq[G] =
  var acc = 0
  for i in 0 ..< len(d):
    if i mod 2 == 1:
      result.add((offset: acc, size: d[i]))
    acc += d[i]

proc add_gap(gaps: seq[G], new_gap: G): seq[G] =
  for i in 0 ..< len(gaps):
    if gaps[i].size == 0:
      continue
    elif gaps[i].offset + gaps[i].size < new_gap.offset:
      result.add(gaps[i])
    elif gaps[i].offset + gaps[i].size == new_gap.offset:
      if i+1 < len(gaps) and gaps[i+1].offset == new_gap.offset + new_gap.size:
#       echo "Double-sided merge"
        result.add((offset: gaps[i].offset, size: gaps[i].size + new_gap.size + gaps[i+1].size))
        return concat(result, gaps[i+2 .. ^1].filterIt(it.size > 0))
      else:
#       echo "Left merge of ", gaps[i], " and ", new_gap
        result.add((offset: gaps[i].offset, size: gaps[i].size + new_gap.size))
        return concat(result, gaps[i+1 .. ^1].filterIt(it.size > 0))
    elif gaps[i].offset == new_gap.offset + new_gap.size:
#     echo "Right merge"
      result.add((offset: new_gap.offset, size: gaps[i].size + new_gap.size))
      return concat(result, gaps[i+1 .. ^1].filterIt(it.size > 0))
    else:
#     echo "Insertion"
      result.add(gaps[i])
      result.add(new_gap)
      return concat(result, gaps[i+1 .. ^1].filterIt(it.size > 0))

proc check_gaps(files: seq[F], gaps: seq[G]): bool =
  for i in 1 ..< len(gaps):
    if gaps[i].size < 0:
#     echo "Negative gap at ", i
      return false
    if gaps[i-1].offset + gaps[i-1].size >= gaps[i].offset:
#     echo "Overlapping gaps ", i-1, " and ", i
      return false

  for i in 0 ..< len(files):
    for j in 0 ..< len(gaps):
      if not (files[i].offset >= gaps[j].offset + gaps[j].size or gaps[j].offset >= files[i].offset + files[i].size):
#       echo "Overlapping file ", i, " and gap ", j
#       echo files[i]
#       echo gaps[j]
        return false

  return true

proc checksum2(d: seq[int]): int =
  var
    files = make_files(d)
    gaps = make_gaps(d)
# assert check_gaps(files, gaps)
  for fi in countdown(len(files) - 1, 0):
    stdout.write "Processing file ", fi, " / ", len(files), " \r"
    for gi in 0 ..< len(gaps):
      if gaps[gi].offset > files[fi].offset:
        break
      if files[fi].size <= gaps[gi].size:
#       echo "moving file ", files[fi], " into gap ", gi, ": ", gaps[gi]
        let freed_offset = files[fi].offset
        files[fi].offset = gaps[gi].offset
        gaps[gi].offset += files[fi].size
        gaps[gi].size -= files[fi].size
#       echo "   updated file:", files[fi]
#       echo "   updated gap:", gaps[gi]
        gaps = add_gap(gaps, (offset: freed_offset, size: files[fi].size))
#       assert check_gaps(files, gaps)
        break
  result = 0
  for f in files:
    result += part(f.offset, f.size, f.id)

echo "Puzzle 2: ", checksum2(data), "            "
