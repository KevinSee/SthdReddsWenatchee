#' @title Estimate Correlation Between Reaches
#'
#' @description Prepares data by querying specific file on a WDFW Sharepoint site, and filters for selected year(s).
#'
#' @author Kevin See
#'
#' @param file_path path to data file
#' @param file_name name of Excel file containing redd data in a very particular format
#' @param year which year or years should be included in this query?
#'
#' @import rlang purrr dplyr janitor lubridate readxl forcats dataRetrieval
#' @return dataframe
#' @export

query_redd_data <- function(
  file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Redd Data",
  file_name = "Wenatchee_Redd_Surveys.xlsx",
  query_year = lubridate::year(lubridate::today()) - 1) {

  data_file = paste(file_path,
                    file_name,
                    sep = "/")

  if(!file.exists(data_file)) {
    stop("File not found.")
  }

  data_list <- readxl::excel_sheets(data_file) |>
    as.list() |>
    rlang::set_names() |>
    purrr::map(.f = function(x) {
      readxl::read_excel(data_file,
                         sheet = x) |>
        janitor::clean_names()
    })

  redd_df <- data_list$`Redd Surveys` |>
    dplyr::select(-surveyors) |>
    dplyr::left_join(data_list$`Reach Length` |>
                       dplyr::select(river,
                                     reach,
                                     type, index,
                                     length_km),
                     by = c("river", "reach", "index")) |>
    dplyr::left_join(data_list$`Thalweg CV` |>
                       dplyr::select(river,
                                     reach,
                                     mean_thalweg_cv),
                     by = c("river", "reach")) |>
    dplyr::left_join(data_list$`Discharge Gages` |>
                       dplyr::select(reach,
                                     usgs_site_code = site_code),
                     by = "reach") |>
    # left_join(data_list$Discharge,
    #           by = c("spawn_year", "river", "reach", "index", "survey_type", "survey_date")) |>
    dplyr::mutate(
      dplyr::across(
        c(reach,
          river),
        as.factor),
      dplyr::across(
        reach,
        forcats::fct_relevel,
        "W10",
        "C1", "N1", "P1",
        after = Inf),
      dplyr::across(
        reach,
        forcats::fct_relevel,
        "MH1", "T1", "WN1",
        after = Inf)) |>
    dplyr::filter(spawn_year %in% query_year)


  cat("Querying USGS for discharge data\n")

  # query USGS for mean daily discharge data
  discharge_df <- redd_df |>
    dplyr::filter(!is.na(usgs_site_code)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dataRetrieval::readNWISdv(usgs_site_code,
                                parameterCd = "00060", # discharge
                                startDate = as.character(lubridate::ymd(survey_date)),
                                endDate = as.character(lubridate::ymd(survey_date)),
                                statCd = "00003" # mean
      )) |>
    dplyr::ungroup() |>
    dplyr::rename(mean_discharge = X_00060_00003) |>
    dplyr::select(-c(agency_cd:Date, X_00060_00003_cd))

  # adjust discharge for W10: Plain - Chiwawa discharge
  if("W10" %in% unique(redd_df$reach)) {
    plain_code <- redd_df |>
      dplyr::filter(reach == "W9") |>
      dplyr::pull(usgs_site_code) |>
      unique()

    w10_discharge <- discharge_df |>
      dplyr::filter(reach == "W10") |>
      dplyr::rowwise() |>
      dplyr::mutate(
        dataRetrieval::readNWISdv(plain_code,
                                  parameterCd = "00060", # discharge
                                  startDate = as.character(lubridate::ymd(survey_date)),
                                  endDate = as.character(lubridate::ymd(survey_date)),
                                  statCd = "00003" # mean
        )) |>
      dplyr::ungroup() |>
      dplyr::rename(plain_discharge = X_00060_00003) |>
      dplyr::select(-c(agency_cd:Date, X_00060_00003_cd)) |>
      dplyr::mutate(mean_discharge = plain_discharge - mean_discharge) |>
      dplyr::select(-plain_discharge)

    # put it all back together
    discharge_df |>
      dplyr::filter(reach != "W10") |>
      dplyr::bind_rows(w10_discharge) -> discharge_df

  }

  redd_data <- discharge_df |>
    dplyr::bind_rows(redd_df |>
                       dplyr::filter(is.na(usgs_site_code))) |>
    dplyr::arrange(spawn_year,
                   river,
                   reach,
                   index,
                   survey_date)

  # identical(nrow(redd_df),
  #           nrow(redd_data))


  return(redd_data)

}
