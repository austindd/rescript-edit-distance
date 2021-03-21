open EditDistance

module Levenshtein = {
  let t1 = levenshtein("bookling", "back")
  Js.log(t1);

  let results = [t1 == 2]
  let finalResult = results->Belt.Array.reduceU((. acc, result) => {
    if acc {
      result
    } else {
      false
    }
  })
}
Js.log(Levenshtein.results)
