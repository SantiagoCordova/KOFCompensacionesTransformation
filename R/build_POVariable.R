#' build_POVariable
#'
#' @param .pathPO  memory path where Standarized KOF Head Count Data base is located
#' @param .pathVar path Memory path where Variable Import month report is saved
#' @import dplyr
#'
#' @return Standarized KOF Head Count Data base with added payer variable
#' @export
#'

build_POVariable <- function(.pathVar, .pathPO){
  .dataVC <- .pathVar %>% munge_NominaVariable(.)

  .dataPO <- .pathPO %>%
    read.csv(., header = TRUE, check.names = FALSE) %>%
    as.data.frame()

  .dataPOVar <- .dataPO %>%
    left_join(., .dataVC) %>%
    mutate(ImporteVariable = ImporteVariable %>% as.numeric()) %>%
    mutate(ImporteVariable = ifelse(ImporteVariable %>% is.na(), 0, ImporteVariable))

  return(.dataPOVar)
}
