#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# Getting rid of NOTEs
utils::globalVariables(c(
  ".", "area", "assunto", "classe", "data_distribuicao", "data_encerramento",
  "estimativa", "id_processo", "id_trnv", "inc_tempo", "juiz", "median", "moeda",
  "n_ass", "polo_ativo", "polo_passivo", "prob", "prob_y", "sentenca_resultado",
  "status", "strata", "tempo", "valor_causa_cat", "valor_num", "vara", "n"
))
