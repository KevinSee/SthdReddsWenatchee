#' @title Prepare All Data from Wenatchee for a Report
#'
#' @description This function
#'
#' @author Kevin See
#'
#'
#' @import rlang purrr dplyr readxl readr janitor tidyr lubridate
#' @importFrom DescTools BinomCI
#' @importFrom msm deltamethod
#' @return NULL, but saves an .Rdata object with output for a given year.
#' @export

prep_wen_sthd_data <- function(
  redd_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Redd Data",
  redd_file_name = "Wenatchee_Redd_Surveys.xlsx",
  dabom_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/estimates",
  dabom_file_name = "UC_STHD_Model_Output.xlsx",
  removal_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Redd Data",
  removal_file_name = "UC_Removals.csv",
  n_observers = "two",
  query_year = lubridate::year(lubridate::today()) - 1
) {

  cat("\t Gathering redd data.\n")

  # load data for selected years
  redd_df_all <- query_redd_data(file_path = redd_file_path,
                                 file_name = redd_file_name,
                                 query_year = query_year)

  if(!is.null(redd_df_all)) {
    # divide reaches into various location categories
    redd_df_all <- redd_df_all |>
      dplyr::mutate(location = dplyr::if_else(reach %in% paste0("W", 8:10),
                                              "Above Tumwater",
                                              dplyr::if_else(reach %in% paste0("W", 1:7),
                                                             "Below Tumwater",
                                                             "Tributaries")))

    # predict net error
    redd_df_all <- redd_df_all |>
      sroem::predict_neterr(species = "Steelhead",
                            num_obs = n_observers)
  }

  #-----------------------------------------------------------------
  # load data on error calls for sex at Priest Rapids when fish were tagged

  cat("\t Pulling PIT tag data.\n\n")

  # get info on tags detected somewhere in the Wenatchee
  wen_tags_all <- readxl::read_excel(paste(dabom_file_path,
                                           dabom_file_name,
                                           sep = "/"),
                                     sheet = "Tag Summary") |>
    janitor::clean_names() |>
    dplyr::filter(str_detect(path, "LWE"),
                  spawn_year %in% query_year) |>
    dplyr::mutate(location = dplyr::if_else(spawn_node %in% c('TUM', 'UWE'),
                                            'Above Tumwater',
                                            dplyr::if_else(str_detect(spawn_node, "^LWE"),
                                                           "Below Tumwater",
                                                           dplyr::if_else(str_detect(path, "CHL"),
                                                                          "Chiwawa",
                                                                          dplyr::if_else(str_detect(path, "NAL"),
                                                                                         "Nason",
                                                                                         dplyr::if_else(str_detect(path, "PES"),
                                                                                                        "Peshastin",
                                                                                                        "Other Tributaries"))))),
                  dplyr::across(location,
                                factor,
                                levels = c("Below Tumwater",
                                           'Above Tumwater',
                                           "Peshastin",
                                           "Nason",
                                           "Chiwawa",
                                           'Other Tributaries'))) |>
    dplyr::select(spawn_year,
                  tag_code,
                  location,
                  origin,
                  sex)

  #-------------------------------------------------------
  # generate fish / redd and pHOS for different areas
  fpr_all = wen_tags_all |>
    dplyr::group_by(spawn_year,
                    location) |>
    dplyr::summarize(n_male = n_distinct(tag_code[sex == "M"]),
                     n_female = n_distinct(tag_code[sex == "F"]),
                     n_sexed = n_male + n_female,
                     n_wild = n_distinct(tag_code[origin == "W"]),
                     n_hatch = n_distinct(tag_code[origin == "H"]),
                     n_origin = n_wild + n_hatch,
                     .groups = "drop") |>
    dplyr::mutate(prop_m = n_male / n_sexed,
                  prop_se = sqrt((prop_m * (1 - prop_m)) / (n_sexed)),
                  fpr = (prop_m) / (1 - prop_m) + 1) |>
    dplyr::select(-contains("Wild"),
                  -contains("Hatchery")) |>
    dplyr::rowwise() |>
    dplyr::mutate(fpr_se = deltamethod(~ x1 / (1 - x1) + 1,
                                       mean = prop_m,
                                       cov = prop_se^2)) |>
    dplyr::ungroup() |>
    dplyr::mutate(phos = n_hatch / n_origin,
                  phos_se = sqrt((phos * (1 - phos)) / (n_origin)))

  cat("\t Adjusting fish/redd.\n")

  # adjust fish / redd for errors in Priest sex calls
  sex_err_rate <- readxl::read_excel(paste(dabom_file_path,
                                           dabom_file_name,
                                           sep = "/"),
                                     sheet = "Sex Error Rates") |>
    janitor::clean_names() |>
    dplyr::select(spawn_year:n_false) |>
    dplyr::filter(spawn_year %in% query_year) |>
    dplyr::rowwise() |>
    dplyr::mutate(binom_ci = map2(n_false,
                                  n_tags,
                                  .f = function(x, y) {
                                    DescTools::BinomCI(x, y) |>
                                      as_tibble()
                                  })) |>
    tidyr::unnest(binom_ci) |>
    janitor::clean_names() |>
    dplyr::rename(perc_false = est) |>
    dplyr::mutate(perc_se = sqrt((perc_false * (1 - perc_false)) / n_tags))

  adj_fpr <- fpr_all |>
    dplyr::select(spawn_year,
                  location,
                  n_male,
                  n_female) |>
    tidyr::pivot_longer(cols = c(n_male,
                                 n_female),
                        names_to = "sex",
                        values_to = "n_fish") |>
    dplyr::mutate(
      dplyr::across(sex,
                    str_remove,
                    "^n_"),
      dplyr::across(sex,
                    str_to_title)) |>
    dplyr::mutate(
      dplyr::across(sex,
                    recode,
                    "Male" = "M",
                    "Female" = "F")) |>
    dplyr::left_join(sex_err_rate |>
                       dplyr::select(spawn_year,
                                     sex,
                                     dplyr::starts_with("perc_")),
                     by = c("spawn_year", "sex")) |>
    tidyr::pivot_wider(names_from = sex,
                       values_from = c(n_fish,
                                       perc_false,
                                       perc_se)) |>
    dplyr::mutate(true_male = n_fish_M - (n_fish_M * perc_false_M) + (n_fish_F * perc_false_F),
                  true_female = n_fish_F - (n_fish_F * perc_false_F) + (n_fish_M * perc_false_M),
                  dplyr::across(starts_with("true"),
                                janitor::round_half_up)) |>
    dplyr::rowwise() |>
    dplyr::mutate(true_m_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                               mean = c(n_fish_M,
                                                        perc_false_M,
                                                        n_fish_F,
                                                        perc_false_F),
                                               cov = diag(c(0,
                                                            perc_se_M,
                                                            0,
                                                            perc_se_F)^2)),
                  true_f_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                               mean = c(n_fish_F,
                                                        perc_false_F,
                                                        n_fish_M,
                                                        perc_false_M),
                                               cov = diag(c(0,
                                                            perc_se_F,
                                                            0,
                                                            perc_se_M)^2))) |>
    dplyr::mutate(n_sexed = true_male + true_female,
                  prop_m = true_male / (true_male + true_female),
                  prop_se = msm::deltamethod(~ x1 / (x1 + x2),
                                             mean = c(true_male,
                                                      true_female),
                                             cov = diag(c(true_m_se,
                                                          true_f_se)^2)),
                  fpr = (prop_m) / (1 - prop_m) + 1,
                  fpr_se = msm::deltamethod(~ x1 / (1 - x1) + 1,
                                            mean = prop_m,
                                            cov = prop_se^2)) |>
    dplyr::ungroup() |>
    dplyr::rename(n_male = true_male,
                  n_female = true_female) |>
    dplyr::left_join(fpr_all |>
                       dplyr::select(spawn_year,
                                     location,
                                     n_hatch,
                                     n_origin,
                                     starts_with("phos")),
                     by = c("spawn_year", "location")) |>
    dplyr::select(dplyr::any_of(names(fpr_all)))

  # # look at changes to fish/redd
  # fpr_all |>
  #   select(spawn_year,
  #          location,
  #          old_fpr = fpr) |>
  #   left_join(adj_fpr |>
  #               select(spawn_year,
  #                      location,
  #                      adj_fpr = fpr))

  # if any fpr values are Inf, use the older ones
  if(sum(adj_fpr$fpr == Inf) > 0) {
    adj_fpr <- adj_fpr |>
      dplyr::left_join(fpr_all |>
                         dplyr::select(location,
                                       old_fpr = fpr,
                                       old_se = fpr_se)) |>
      dplyr::mutate(fpr = dplyr::if_else(is.na(fpr) | fpr == Inf,
                                         old_fpr,
                                         fpr),
                    fpr_se = dplyr::if_else(is.na(fpr_se) | fpr_se == Inf,
                                            old_se,
                                            fpr_se)) |>
      dplyr::select(-dplyr::starts_with("old"))
  }

  fpr_all <- adj_fpr

  rm(adj_fpr)

  #-----------------------------------------------------------------
  # read in data about known removals of fish prior to spawning
  removal_df <- readr::read_csv(paste(removal_file_path,
                                      removal_file_name,
                                      sep = "/")) |>
    janitor::clean_names()

  #-----------------------------------------------------------------
  # pull in some estimates from DABOM

  cat("\t Gathering PIT escapement estimates.\n")

  all_escp = readxl::read_excel(paste(dabom_file_path,
                                      dabom_file_name,
                                      sep = "/"),
                                sheet = "Run Escp All Locations") |>
    janitor::clean_names() |>
    dplyr::filter(spawn_year %in% query_year,
                  location %in% c('ICL',
                                  'PES',
                                  'MCL',
                                  'CHM',
                                  'CHW',
                                  'CHL',
                                  'NAL',
                                  'LWN',
                                  'WTL',
                                  'LWE',
                                  'LWE_bb',
                                  'TUM_bb',
                                  'UWE_bb'))

  # pull out estimates of tributary spawners from DABOM
  trib_spawners_all = all_escp |>
    dplyr::filter(location %in% c('ICL',
                                  'PES',
                                  'MCL',
                                  'CHM',
                                  'CHW',
                                  'CHL',
                                  'NAL',
                                  'LWN',
                                  'WTL')) |>
    dplyr::select(spawn_year,
                  origin,
                  location,
                  spawners = estimate,
                  spawners_se = se) |>
    dplyr::mutate(
      dplyr::across(origin,
                    recode,
                    "W" = "Natural",
                    "H" = "Hatchery"),
      dplyr::across(location,
                    recode,
                    'CHL' = 'Chiwawa',
                    'CHM' = 'Chumstick',
                    'CHW' = 'Chiwaukum',
                    'ICL' = 'Icicle',
                    'LWN' = 'Little Wenatchee',
                    'MCL' = 'Mission',
                    'NAL' = 'Nason',
                    'PES' = 'Peshastin',
                    'WTL' = 'White River')) |>
    dplyr::arrange(location, origin)

  # pull out mainstem escapement estimates
  escp_wen_all = all_escp |>
    dplyr::filter(location %in% c('LWE',
                                  'LWE_bb',
                                  'TUM_bb',
                                  'UWE_bb')) |>
    dplyr::mutate(
      dplyr::across(location,
                    recode,
                    'LWE' = 'Wen_all',
                    'LWE_bb' = 'Below Tumwater',
                    'TUM_bb' = "Above Tumwater",
                    'UWE_bb' = 'Above Tumwater')) |>
    dplyr::mutate(
      dplyr::across(origin,
                    recode,
                    "W" = "Natural",
                    "H" = "Hatchery")) |>
    dplyr::group_by(spawn_year,
                    location,
                    origin) |>
    dplyr::summarise(
      dplyr::across(estimate,
                    sum),
      dplyr::across(se,
                    ~ sqrt(sum(.^2))),
      .groups = "drop")

  #-----------------------------------------------------------------
  # save
  for(yr in query_year) {
    cat(paste("Saving data from spawn year",
              yr,
              ".\n\n"))

    if(!is.null(redd_df_all)) {
      redd_df <- redd_df_all |>
        dplyr::filter(spawn_year == yr)
    } else {
      redd_df <- NULL
    }

    wen_tags <- wen_tags_all |>
      dplyr::filter(spawn_year == yr)

    sex_err <- sex_err_rate |>
      dplyr::filter(spawn_year == yr)

    fpr_df <- fpr_all |>
      dplyr::filter(spawn_year == yr)

    trib_spawners <- trib_spawners_all |>
      dplyr::filter(spawn_year == yr)

    escp_wen <- escp_wen_all |>
      dplyr::filter(spawn_year == yr)

    rem_df <- removal_df |>
      dplyr::filter(spawn_year == yr)

    save(redd_df,
         wen_tags,
         sex_err,
         fpr_df,
         trib_spawners,
         escp_wen,
         rem_df,
         file = here('analysis/data/derived_data',
                     paste0('wen_', yr, '.rda')))

  }


}
