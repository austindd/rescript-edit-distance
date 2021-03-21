/*
  Algorithm ported from:
  https://stackoverflow.com/questions/33204719/edit-distance-solution-with-on-space-issue

  This version of Levenshtein Distance is computed with O(n) space complexity.
*/

let levenshtein = (word1: string, word2: string) => {
  module Arr = Belt.Array
  module Str = Js.String2
  let m = Str.length(word1)
  let n = Str.length(word2)
  let cur = []
  Arr.truncateToLengthUnsafe(cur, m + 1)
  Arr.setUnsafe(cur, 0, 0)
  for x in 1 to m {
    Arr.setUnsafe(cur, x, x)
  }
  for j in 1 to n {
    let pre = ref(Arr.getUnsafe(cur, 0))
    Arr.setUnsafe(cur, 0, j)
    for i in 1 to m {
      let temp = Arr.getUnsafe(cur, i)
      let (optChar1, optChar2) = (Str.codePointAt(word1, i - 1), Str.codePointAt(word2, j - 1))
      switch (optChar1, optChar2) {
      | (None, _) => failwith("Assert: 'codePointAt(word)' is always 'Some(char)'")
      | (_, None) => failwith("Assert: 'codePointAt(word)' is always 'Some(char)'")
      | (Some(char1), Some(char2)) =>
        if char1 === char2 {
          Arr.setUnsafe(cur, i, pre.contents)
        } else {
          let a = Arr.getUnsafe(cur, i) + 1
          let b = Arr.getUnsafe(cur, i - 1) + 1
          let c = pre.contents + 1
          let min_ab = a < b ? a : b
          Arr.setUnsafe(cur, i, min_ab < c ? min_ab : c)
        }
        pre := temp
      }
    }
  }
  Arr.getUnsafe(cur, m)
}

