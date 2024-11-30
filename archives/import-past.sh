#!/bin/sh

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

set -Cuex

rm -rf aoc-*

NL='
'

mkdir aoc-all
cd aoc-all
git init -b trunk
cd ..

git clone https://github.com/faelys/aoc-2022.git
cd aoc-2022
FILTER_BRANCH_SQUELCH_WARNING=1 git filter-branch --tree-filter \
    'rm -f LICENSE && mkdir -p 2022 && mv [^2]* 2022/' HEAD
git update-ref -d refs/original/refs/heads/main \
    afd60166b3442493163230effb1e9b19fb10e2c6
FILTER_BRANCH_SQUELCH_WARNING=1 git filter-branch --msg-filter \
    'sed "s/^Initial commit\$/Initial 2022 commit/"' HEAD
git update-ref -d refs/original/refs/heads/main \
    f75d9dbcb613dbc8b952d46c662293bdbea0ed54
cd ..

cd aoc-all
git fetch ../aoc-2022
# git update-ref HEAD FETCH_HEAD
git reset --hard FETCH_HEAD
cd ..

git clone https://github.com/faelys/aoc-2023.git
cd aoc-2023
FILTER_BRANCH_SQUELCH_WARNING=1 git filter-branch --msg-filter \
    'sed "s/^Add project infrastructure\$/Add 2023 infrastructure/"' HEAD
git update-ref -d refs/original/refs/heads/trunk \
    41f18daf4e7e805ada852b79ca5986c93d146922
FILTER_BRANCH_SQUELCH_WARNING=1 git filter-branch --parent-filter \
    'sed s/-p\ 256d123f149ab6b41e0397ba6d39f60f9e911c8d//' HEAD
git update-ref -d refs/original/refs/heads/trunk \
    cb813ee19bd9ddeb7150c39d4ba027868ebdaf4a
FILTER_BRANCH_SQUELCH_WARNING=1 git filter-branch --tree-filter \
    'rm -rf LICENCE .fossil-settings && mkdir -p 2023 && mv [^2]* 2023' HEAD
git update-ref -d refs/original/refs/heads/trunk \
    c3e8914da1869e6a36031f26de1c3cd347686578
cd ..


cd aoc-all
git fetch ../aoc-2023
git checkout -b fetched FETCH_HEAD
git rebase --onto trunk --root --committer-date-is-author-date
git checkout trunk
git merge --ff fetched
git branch -d fetched
git gc --prune=now
cd ..

# Failed alternative to the last block, using a bare repository:
#
# cd aoc-all
# test "$(git rev-list --all --max-parents=0 | wc -l)" -eq 1
# ROOT_ID="$(git rev-list --all --max-parents=0)"
# OLD_HEAD="$(git show-ref trunk)"
# OLD_HEAD="${OLD_HEAD%% *}"
# git fetch ../aoc-2023
# git branch fetched FETCH_HEAD
# test "$(git rev-list --all --max-parents=0 | wc -l)" -eq 2
# test "$(git rev-list --all --max-parents=0 \
#         | grep -Fv "${ROOT_ID}" | wc -l)" \
#     -eq 1
# FETCH_ROOT="$(git rev-list --all --max-parents=0 \
#               | grep -Fv "${ROOT_ID}")"
# git replace --gr
# FILTER_BRANCH_SQUELCH_WARNING=1 git filter-branch trunk..fetched
# git reset --soft fetched
# git branch -d fetched
# git update-ref -d refs/original/refs/heads/fetched
# git update-ref -d refs/replace/"${FETCH_ROOT}"
# git gc --prune=now
# cd ..

cd aoc-all
git fast-export HEAD | fossil import --git \
    -A admin \
    --attribute 'natgh@instinctive.eu nat' \
    --attribute 'natacha@instinctive.eu nat' \
    ../aoc-fossil
fossil user -R ../aoc-fossil \
    new nat 'Natasha Kerensikova <natgh@instinctive.eu>' nat
cd ..
