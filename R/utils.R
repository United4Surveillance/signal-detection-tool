#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @importFrom rlang .data
NULL

#' Copy of guide_colourbar_interactive
#'
#' @description
#' fixes weird problem in vignette since match.fun only looks back through 2
#' layers of parent environments for the named function
#'
#' @keywords internal
#' @noRd
#' @importFrom ggiraph guide_colourbar_interactive
NULL
