module ErrorMsg = {
  let assert_codePointAt_alwaysSome = "Assert: 'codePointAt(word)' is always 'Some(char)'"
}

let min3 = (a: int, b: int, c: int) =>
  if a <= b && a <= c {
    a
  } else if b <= c {
    b
  } else {
    c
  }

let handleEmptyStrings = (a: string, b: string) => {
  open Js.String2
  switch (a, b) {
  | ("", "") => Some(0)
  | ("", b) => Some(length(b))
  | (a, "") => Some(length(a))
  | (_, _) => None
  }
}

/*
  Algorithm ported from:
  https://stackoverflow.com/questions/33204719/edit-distance-solution-with-on-space-issue

  This version of Levenshtein Distance is computed with O(n) space complexity.
*/
let levenshtein = (word1: string, word2: string) => {
  switch handleEmptyStrings(word1, word2) {
  | Some(result) => result
  | None =>
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
        | (None, _) => failwith(ErrorMsg.assert_codePointAt_alwaysSome)
        | (_, None) => failwith(ErrorMsg.assert_codePointAt_alwaysSome)
        | (Some(char1), Some(char2)) =>
          if char1 == char2 {
            Arr.setUnsafe(cur, i, pre.contents)
          } else {
            let a = Arr.getUnsafe(cur, i) + 1
            let b = Arr.getUnsafe(cur, i - 1) + 1
            let c = pre.contents + 1
            Arr.setUnsafe(cur, i, min3(a, b, c))
          }
          pre := temp
        }
      }
    }
    Arr.getUnsafe(cur, m)
  }
}

let damerau_levenshtein = (~maxLength=?, _first: string, _second: string) => {
  let maxL = ref(
    switch maxLength {
    | None => 255
    | Some(maxL) => maxL
    },
  )
  module Arr = Belt.Array
  module Str = Js.String2
  let _currentRow = ref(Array.make(maxL.contents + 1, 0))
  let _previousRow = ref(Array.make(maxL.contents + 1, 0))
  let _transpositionRow = ref(Array.make(maxL.contents + 1, 0))

  let first = ref(_first)
  let second = ref(_second)

  let firstLength = ref(Str.length(first.contents))
  let secondLength = ref(Str.length(second.contents))

  if firstLength.contents == 0 {
    secondLength.contents
  } else if secondLength.contents == 0 {
    firstLength.contents
  } else {
    let () = if firstLength.contents > secondLength.contents {
      let tmp = first.contents
      first := second.contents
      second := tmp
      firstLength := secondLength.contents
      secondLength := Str.length(second.contents)
    }

    let () = if maxL.contents < 0 {
      maxL := secondLength.contents
    }

    if secondLength.contents - firstLength.contents > maxL.contents {
      maxL.contents + 1
    } else {
      let () = if firstLength.contents > Arr.length(_currentRow.contents) {
        let tempLength = firstLength.contents + 1
        _currentRow := Arr.make(tempLength, 0)
        _previousRow := Arr.make(tempLength, 0)
        _transpositionRow := Arr.make(tempLength, 0)
      }

      let () = for i in 0 to firstLength.contents {
        _previousRow.contents[i] = i
      }

      let lastSecondCh = ref(0)

      let () = for i in 1 to secondLength.contents {
        let Some(secondCh) = Str.codePointAt(second.contents, i - 1)
        _currentRow.contents[0] = i
        let from_ = max(i - maxL.contents - 1, 1)
        let to_ = min(i + maxL.contents + 1, firstLength.contents)

        let lastFirstCh = ref(0)
        for j in from_ to to_ {
          let Some(firstCh) = Str.codePointAt(first.contents, j - 1)

          let cost = firstCh == secondCh ? 0 : 1

          let value = ref(
            min(
              min(_currentRow.contents[j - 1] + 1, _previousRow.contents[j] + 1),
              _previousRow.contents[j - 1] + cost,
            ),
          )

          if firstCh == lastSecondCh.contents && secondCh == lastFirstCh.contents {
            value := min(value.contents, _transpositionRow.contents[j - 2] + cost)
            _currentRow.contents[j] = value.contents
            lastFirstCh := firstCh
          }
        }
        lastSecondCh := secondCh

        let tempRow = _transpositionRow.contents
        _transpositionRow := _previousRow.contents
        _previousRow := _currentRow.contents
        _currentRow := tempRow
      }
      _previousRow.contents[firstLength.contents]
    }
  }
}

let damerau_levenshtein_2 = (~maxLength=?, _first: string, _second: string) => {
  module Arr = Belt.Array
  module Str = Js.String2

  let (first, firstLength, second, secondLength) = {
    let l1 = Str.length(_first)
    let l2 = Str.length(_second)

    if l1 > l2 {
      (_second, l2, _first, l1)
    } else {
      (_first, l1, _second, l2)
    }
  }

  if firstLength == 0 {
    secondLength
  } else if secondLength == 0 {
    firstLength
  } else {
    let maxL = switch maxLength {
    | None => 255
    | Some(x) => x < 0 ? secondLength : x
    }

    if secondLength - firstLength > maxL {
      maxL + 1
    } else {
      let (_currentRow, _previousRow, _transpositionRow) = if firstLength > maxL + 1 {
        (
          ref(Arr.make(firstLength + 1, 0)),
          ref(Arr.make(firstLength + 1, 0)),
          ref(Arr.make(firstLength + 1, 0)),
        )
      } else {
        (ref(Arr.make(maxL + 1, 0)), ref(Arr.make(maxL + 1, 0)), ref(Arr.make(maxL + 1, 0)))
      }

      for i in 0 to firstLength {
        Arr.setUnsafe(_previousRow.contents, i, i)
      }

      let lastSecondCh = ref(0)

      for i in 1 to secondLength {
        let Some(secondCh) = Str.codePointAt(second, i - 1)
        Arr.setUnsafe(_currentRow.contents, 0, i)
        let from_ = max(i - maxL - 1, 1)
        let to_ = min(i + maxL + 1, firstLength)

        let lastFirstCh = ref(0)
        for j in from_ to to_ {
          let Some(firstCh) = Str.codePointAt(first, j - 1)

          let cost = firstCh == secondCh ? 0 : 1

          let value = {
            let temp = min3(
              Arr.getUnsafe(_currentRow.contents, j - 1) + 1,
              Arr.getUnsafe(_previousRow.contents, j) + 1,
              Arr.getUnsafe(_previousRow.contents, j - 1) + cost,
            )

            if firstCh == lastSecondCh.contents && secondCh == lastFirstCh.contents {
              min(temp, Arr.getUnsafe(_transpositionRow.contents, j - 2) + cost)
            } else {
              temp
            }
          }

          Arr.setUnsafe(_currentRow.contents, j, value)
          lastFirstCh := firstCh
        }
        lastSecondCh := secondCh

        let tempRow = _transpositionRow.contents
        _transpositionRow := _previousRow.contents
        _previousRow := _currentRow.contents
        _currentRow := tempRow
      }
      Arr.getUnsafe(_previousRow.contents, firstLength)
    }
  }
}

let damerau_levenshtein_3 = (~maxLength=?, _first: string, _second: string) => {
  module Arr = Belt.Array
  module Str = Js.String2

  let (first, firstLength, second, secondLength) = {
    let l1 = Str.length(_first)
    let l2 = Str.length(_second)

    if l1 > l2 {
      (_second, l2, _first, l1)
    } else {
      (_first, l1, _second, l2)
    }
  }

  if firstLength == 0 {
    secondLength
  } else if secondLength == 0 {
    firstLength
  } else {
    let maxL = switch maxLength {
    | None => 255
    | Some(x) => x < 0 ? secondLength : x
    }

    if secondLength - firstLength > maxL {
      maxL + 1
    } else {
      ////
      let rec outerLoop = (~i, ~currentRow, ~previousRow, ~transpositionRow, ~lastSecondCh) => {
        switch Str.codePointAt(second, i - 1) {
        | None => failwith(ErrorMsg.assert_codePointAt_alwaysSome)
        | Some(secondCh) =>
          Arr.setUnsafe(currentRow, 0, i)
          let __a = i - maxL - 1
          let from_ = __a > 1 ? __a : 1
          let __b = i + maxL + 1
          let to_ = __b < firstLength ? __b : firstLength
          let lastFirstCh = ref(0)
          for j in from_ to to_ {
            switch Str.codePointAt(first, j - 1) {
            | None => failwith(ErrorMsg.assert_codePointAt_alwaysSome)
            | Some(firstCh) =>
              let cost = firstCh == secondCh ? 0 : 1

              let value = {
                let temp1 = min3(
                  Arr.getUnsafe(currentRow, j - 1) + 1,
                  Arr.getUnsafe(previousRow, j) + 1,
                  Arr.getUnsafe(previousRow, j - 1) + cost,
                )

                if firstCh == lastSecondCh && secondCh == lastFirstCh.contents {
                  let temp2 = Arr.getUnsafe(transpositionRow, j - 2) + cost
                  temp1 < temp2 ? temp1 : temp2
                } else {
                  temp1
                }
              }

              Arr.setUnsafe(currentRow, j, value)
              lastFirstCh := firstCh
            }
          }
          if i >= secondLength {
            Arr.getUnsafe(currentRow, firstLength)
          } else {
            outerLoop(
              ~i=succ(i),
              ~transpositionRow=previousRow,
              ~previousRow=currentRow,
              ~currentRow=transpositionRow,
              ~lastSecondCh=secondCh,
            )
          }
        }
      }

      let (currentRow, previousRow, transpositionRow) = ([], [], [])
      let correctLength = if firstLength > maxL + 1 {
        firstLength + 1
      } else {
        maxL + 1
      }
      Arr.truncateToLengthUnsafe(currentRow, correctLength)
      Arr.truncateToLengthUnsafe(previousRow, correctLength)
      Arr.truncateToLengthUnsafe(transpositionRow, correctLength)

      for i in 0 to correctLength {
        Arr.setUnsafe(currentRow, i, 0)
        Arr.setUnsafe(previousRow, i, 0)
        Arr.setUnsafe(transpositionRow, i, 0)
      }

      for i in 0 to firstLength {
        Arr.setUnsafe(previousRow, i, i)
      }

      outerLoop(~i=1, ~currentRow, ~previousRow, ~transpositionRow, ~lastSecondCh=0)
    }
  }
}

let damerau_levenshtein_4 = (~maxLength=?, _first: string, _second: string) => {
  module Arr = Belt.Array
  module Str = Js.String2

  let l1 = Str.length(_first)
  let l2 = Str.length(_second)

  let first = l1 > l2 ? _second : _first
  let firstLength = l1 > l2 ? l2 : l1
  let second = l1 > l2 ? _first : _second
  let secondLength = l1 > l2 ? l1 : l2

  if firstLength == 0 {
    secondLength
  } else if secondLength == 0 {
    firstLength
  } else {
    let maxL = switch maxLength {
    | None => 255
    | Some(x) => x < 0 ? secondLength : x
    }

    if secondLength - firstLength > maxL {
      maxL + 1
    } else {
      let rec outerLoop = (~i, ~currentRow, ~previousRow, ~transpositionRow, ~lastSecondCh) => {
        switch Str.codePointAt(second, i - 1) {
        | None => failwith(ErrorMsg.assert_codePointAt_alwaysSome)
        | Some(secondCh) =>
          Arr.setUnsafe(currentRow, 0, i)
          let __a = i - maxL - 1
          let from_ = __a > 1 ? __a : 1
          let __b = i + maxL + 1
          let to_ = __b < firstLength ? __b : firstLength
          let lastFirstCh = ref(0)
          for j in from_ to to_ {
            switch Str.codePointAt(first, j - 1) {
            | None => failwith(ErrorMsg.assert_codePointAt_alwaysSome)
            | Some(firstCh) =>
              let cost = firstCh == secondCh ? 0 : 1

              let value = {
                let temp1 = min3(
                  Arr.getUnsafe(currentRow, j - 1) + 1,
                  Arr.getUnsafe(previousRow, j) + 1,
                  Arr.getUnsafe(previousRow, j - 1) + cost,
                )

                if firstCh == lastSecondCh && secondCh == lastFirstCh.contents {
                  let temp2 = Arr.getUnsafe(transpositionRow, j - 2) + cost
                  temp1 < temp2 ? temp1 : temp2
                } else {
                  temp1
                }
              }

              Arr.setUnsafe(currentRow, j, value)
              lastFirstCh := firstCh
            }
          }
          if i >= secondLength {
            Arr.getUnsafe(currentRow, firstLength)
          } else {
            outerLoop(
              ~i=succ(i),
              ~transpositionRow=previousRow,
              ~previousRow=currentRow,
              ~currentRow=transpositionRow,
              ~lastSecondCh=secondCh,
            )
          }
        }
      }

      let (currentRow, previousRow, transpositionRow) = ([], [], [])
      let correctLength = if firstLength > maxL + 1 {
        firstLength + 1
      } else {
        maxL + 1
      }
      Arr.truncateToLengthUnsafe(currentRow, correctLength)
      Arr.truncateToLengthUnsafe(previousRow, correctLength)
      Arr.truncateToLengthUnsafe(transpositionRow, correctLength)

      for i in 0 to correctLength {
        Arr.setUnsafe(currentRow, i, 0)
        Arr.setUnsafe(previousRow, i, 0)
        Arr.setUnsafe(transpositionRow, i, 0)
      }

      for i in 0 to firstLength {
        Arr.setUnsafe(previousRow, i, i)
      }

      outerLoop(~i=1, ~currentRow, ~previousRow, ~transpositionRow, ~lastSecondCh=0)
    }
  }
}
