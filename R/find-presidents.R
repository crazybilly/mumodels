#' Find Presidents and CEOs
#'
#' @description convert a list of character vectors into logical based on whether each title is a top-level executive or not. Designed for use within dplyr::mutate.
#' @param x a character vector of job titles
#'
#' @return a logical vector indicating whether the job title is any of the following: president, CEO, chairman, owner or self-employed. Vice presidents return FALSE.
#'
#' @export
#'
findPresidents  <- function(x) {
   x  <- tolower(x)

   !str_detect(x, "vp|vice|v\\.?p\\.?") &
      str_detect(x, "ceo|president|chief executive officer|chairman|owner|self-?employed")
}
