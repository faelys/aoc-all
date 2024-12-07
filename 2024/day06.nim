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

from std/sequtils import repeat, toSeq

# Read input file

let data = stdin.lines().toSeq()
let nlines = len(data)
let ncols = len(data[0])

# Puzzle 1

func find_start_pos(m: seq[string]): (int, int) =
  for y in 0 ..< len(m):
    for x in 0 ..< len(m[y]):
      if m[y][x] == '^':
        return (x, y)

let start_pos = find_start_pos(data)

type direction = enum north, east, south, west

func dx(d: direction): int =
  case d
  of north: result =  0
  of east:  result =  1
  of south: result =  0
  of west:  result = -1

func dy(d: direction): int =
  case d
  of north: result = -1
  of east:  result =  0
  of south: result =  1
  of west:  result =  0

func dnext(d: direction): direction =
  case d
  of north: result = east
  of east:  result = south
  of south: result = west
  of west:  result = north

func dprev(d: direction): direction =
  case d
  of north: result = west
  of east:  result = north
  of south: result = east
  of west:  result = south

proc puzzle1(start_x, start_y: int): int =
  var
    visited = repeat(repeat(false, ncols), nlines)
    x = start_x
    y = start_y
    dir = north
  result = 0
  while x >= 0 and y >= 0 and y < nlines and x < ncols:
    if data[y][x] == '#':
      # backtrack and turn
      x -= dx(dir)
      y -= dy(dir)
      dir = dnext(dir)
    elif not visited[y][x]:
      visited[y][x] = true
      result += 1
    x += dx(dir)
    y += dy(dir)

let result1 = puzzle1(start_pos[0], start_pos[1])
echo "Puzzle 1: ", result1

# Puzzle 2

proc backtrack(visited: var array[direction, seq[seq[bool]]],
               start_x, start_y: int, start_dir: direction): void =
  var
    x = start_x
    y = start_y
    dir = start_dir

  while x >= 0 and y >= 0 and y < nlines and x < ncols:
    if data[y][x] == '#':
      x += dx(dir)
      y += dy(dir)
      dir = dprev(dir)

    if visited[dir][y][x]:
      break

    visited[dir][y][x] = true
    x -= dx(dir)
    y -= dy(dir)


proc bad_puzzle2(start_x, start_y: int): int {.used.} =
  var
    visited: array[direction, seq[seq[bool]]]
    bvisited: array[direction, seq[seq[bool]]]
    x = start_x
    y = start_y
    dir = north
  for d in direction:
    visited[d] = repeat(repeat(false, ncols), nlines)
    bvisited[d] = repeat(repeat(false, ncols), nlines)

  backtrack(bvisited, x, y + 1, dir)

  result = 0
  while x >= 0 and y >= 0 and y < nlines and x < ncols:
    if data[y][x] == '#':
      # backtrack and turn
      x -= dx(dir)
      y -= dy(dir)
      dir = dnext(dir)
      backtrack(bvisited, x, y, dir)

    if not visited[dir][y][x]:
      visited[dir][y][x] = true
      bvisited[dir][y][x] = true
      let nx = x - dx(dir) + dx(dnext(dir))
      let ny = y - dy(dir) + dy(dnext(dir))
      if nx >= 0 and ny >= 0 and ny < nlines and nx < ncols and bvisited[dnext(dir)][ny][nx]:
        result += 1

    x += dx(dir)
    y += dy(dir)

proc is_looping(start_x, start_y, extra_x, extra_y: int): bool =
  var
    visited: array[direction, seq[seq[bool]]]
    x = start_x
    y = start_y
    dir = north
  for d in direction:
    visited[d] = repeat(repeat(false, ncols), nlines)

  result = false
  while x >= 0 and y >= 0 and y < nlines and x < ncols:
    if data[y][x] == '#' or (x == extra_x and y == extra_y):
      # backtrack and turn
      x -= dx(dir)
      y -= dy(dir)
      dir = dnext(dir)

    if visited[dir][y][x]:
      return true

    visited[dir][y][x] = true
    x += dx(dir)
    y += dy(dir)

proc brute_puzzle2(start_x, start_y: int): int =
  var
    visited = repeat(repeat(false, ncols), nlines)
    x = start_x
    y = start_y
    dir = north
    steps = 0

  result = 0
  visited[start_y][start_x] = true
  while x >= 0 and y >= 0 and y < nlines and x < ncols:
    if data[y][x] == '#':
      # backtrack and turn
      x -= dx(dir)
      y -= dy(dir)
      steps -= 1
      dir = dnext(dir)
    elif not visited[y][x]:
      visited[y][x] = true
      if is_looping(start_x, start_y, x, y):
        result += 1

    steps += 1
    x += dx(dir)
    y += dy(dir)
    stdout.write steps, " / ", result1, "\r"

echo "Puzzle 2: ", brute_puzzle2(start_pos[0], start_pos[1])
