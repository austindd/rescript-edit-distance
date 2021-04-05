open EditDistance

module Levenshtein = {
  let t1 = levenshtein("back", "books")
  let t2 = levenshtein("books", "back")

  let data = [t1, t2]
  let expected = [3, 3]
  let result = data == expected

  Js.log((result, data))
}

module DamerauLevenshtein = {
  let t1 = damerau_levenshtein_3("back", "books")
  let t2 = levenshtein("books", "back")

  let data = [t1, t2]
  let expected = [3, 3]
  let result = data == expected

  Js.log((result, data))
}
