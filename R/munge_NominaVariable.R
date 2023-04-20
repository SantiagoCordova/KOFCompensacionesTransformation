#' munge_NominaVariable
#'
#' @param .path Memory path where Variable Import month report is saved
#'
#' @return .data dataframe with Employe ID and Total payed variable in MXN
#' @import lubridate dplyr readxl TipoCambio
#' @export
#'
munge_NominaVariable <- function(.path){
  .date <- .path %>%
    substr(., 16, 25) %>%
    lubridate::ymd()

  .date <- paste(.date %>% lubridate::year(), .date %>% lubridate::month()) %>%
    lubridate::ym()

  AuxTC <- TipoCambio::kofTCClose %>%
    filter(From == 'México') %>%
    left_join(., CatMon)

  .data <- .path %>%
    readxl::read_excel(.)

  .data <- .data %>%
    mutate(Date = .date) %>%
    left_join(., AuxTC) %>%
    mutate(ImporteMX = Importe*TC) %>%
    group_by(`Nº pers.`) %>%
    summarise(ImporteVariable = sum(Importe)) %>%
    rename(`ID de usuario/empleado` = `Nº pers.`)

  return(.data)
}


