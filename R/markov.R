#' makeMarkov
#'
#' This function returns a numeric sequence corresponding to a text
#' file, suitable for RQA use.
#' @param <x> text file name
#' @keywords clean text
#' @export
#' @examples
#' makeMarkov(ts)
#'
makeMarkov <- function(ts) {   # category vector
  # With some help from SO:
  # https://stackoverflow.com/questions/47329234/how-to-build-a-markovs-chain-transition-probability-matrix
  # The time series is either a single thing:
  #  "ATTCAACACATCCAGCCACATGCTCCGAG"
  # or multiple things:
  # c("A", "B", "A", "C")
  if(length(ts) == 1) {   # Needs a split
    ts <- unlist(strsplit(ts, split =""))
  }

  unique(ts) ->
    ts_unique

  matrix(0,
         ncol = length(ts_unique),
         nrow=length(ts_unique)) ->
    markov

  for (i in 1:(length(ts) - 1)){
    ts == ts[i] ->
      index_of_i
    ts == ts[i + 1] ->
      index_of_i_plus_1
    markov[index_of_i, index_of_i_plus_1] + 1 ->
      markov[index_of_i, index_of_i_plus_1]
  }
  markov <- markov / rowSums(markov)
  return(markov)
}

#' bootMarkov
#'
#' This function returns a numeric sequence corresponding to a text
#' file, suitable for RQA use.
#' @param <x> text file name
#' @keywords clean text
#' @export
#' @examples
#' bootMarkov(ts)
#'
bootMarkov <- function(ts) {   # category vector
  return("Not implemented")
}
