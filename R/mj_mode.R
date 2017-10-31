#' @title Mode of a numeric set
#' @description
#' \code{mj_mode} computes the statistical mode of a set of numbers
#' @param V Vector of numbers
#' @return Most repeated sample
#'
#' @author Manuel J. Marin-Jimenez
#'
#' @examples
#' mj_mode(c(1,2,2,3,1,2))
#' @export

mj_mode <- function(V)
{
   uV <- sort(unique(V));
   Ht <- tabulate(match(V, uV));
   idxMax <- which.max(Ht);
   valm <- uV[idxMax];
   return (valm);
}
