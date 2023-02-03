#' @title Prepare All Data from Wenatchee for a Report
#'
#' @description This function
#'
#' @author Kevin See
#'
#'
#' @import rlang purrr dplyr readxl janitor tidyr lubridate
#' @importFrom DescTools BinomCI
#' @return dataframe
#' @export

prep_wen_sthd_data <- function(
  redd_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Redd Data",
  redd_file_name = "Wenatchee_Redd_Surveys.xlsx",
  dabom_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/estimates",
  dabom_file_name = "UC_STHD_Model_Output.xlsx",
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
      mutate(location = if_else(reach %in% paste0("W", 8:10),
                                "Above Tumwater",
                                if_else(reach %in% paste0("W", 1:7),
                                        "Below Tumwater",
                                        "Tributaries")))

    # predict net error
    redd_df_all <- redd_df_all |>
      predict_neterr(species = "Steelhead",
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
    filter(str_detect(path, "LWE"),
           spawn_year %in% query_year) |>
    mutate(location = if_else(spawn_node %in% c('TUM', 'UWE'),
                              'Above Tumwater',
                              if_else(str_detect(spawn_node, "^LWE"),
                                      "Below Tumwater",
                                      if_else(str_detect(path, "CHL"),
                                              "Chiwawa",
                                              if_else(str_detect(path, "NAL"),
                                                      "Nason",
                                                      if_else(str_detect(path, "PES"),
                                                              "Peshastin",
                                                              "Other Tributaries"))))),
           across(location,
                  factor,
                  levels = c("Below Tumwater",
                             'Above Tumwater',
                             "Peshastin",
                             "Nason",
                             "Chiwawa",
                             'Other Tributaries'))) |>
    select(spawn_year,
           tag_code,
           location,
           origin,
           sex)

  #-------------------------------------------------------
  # generate fish / redd and pHOS for different areas
  fpr_all = wen_tags_all |>
    group_by(spawn_year,
             location) |>
    summarize(n_male = n_distinct(tag_code[sex == "M"]),
              n_female = n_distinct(tag_code[sex == "F"]),
              n_sexed = n_male + n_female,
              n_wild = n_distinct(tag_code[origin == "W"]),
              n_hatch = n_distinct(tag_code[origin == "H"]),
              n_origin = n_wild + n_hatch,
              .groups = "drop") |>
    mutate(prop_m = n_male / n_sexed,
           prop_se = sqrt((prop_m * (1 - prop_m)) / (n_sexed)),
           fpr = (prop_m) / (1 - prop_m) + 1) |>
    select(-contains("Wild"),
           -contains("Hatchery")) |>
    rowwise() |>
    mutate(fpr_se = deltamethod(~ x1 / (1 - x1) + 1,
                                mean = prop_m,
                                cov = prop_se^2)) |>
    ungroup() |>
    mutate(phos = n_hatch / n_origin,
           phos_se = sqrt((phos * (1 - phos)) / (n_origin)))

  cat("\t Adjusting fish/redd.\n")

  # adjust fish / redd for errors in Priest sex calls
  sex_err_rate <- readxl::read_excel(paste(dabom_file_path,
                                           dabom_file_name,
                                           sep = "/"),
                                     sheet = "Sex Error Rates") |>
    janitor::clean_names() |>
    select(spawn_year:n_false) |>
    rowwise() |>
    mutate(binom_ci = map2(n_false,
                           n_tags,
                           .f = function(x, y) {
                             DescTools::BinomCI(x, y) |>
                               as_tibble()
                           })) |>
    unnest(binom_ci) |>
    clean_names() |>
    rename(perc_false = est) |>
    mutate(perc_se = sqrt((perc_false * (1 - perc_false)) / n_tags))

  adj_fpr <- fpr_all |>
    select(spawn_year,
           location,
           n_male,
           n_female) |>
    pivot_longer(cols = c(n_male,
                          n_female),
                 names_to = "sex",
                 values_to = "n_fish") |>
    mutate(across(sex,
                  str_remove,
                  "^n_"),
           across(sex,
                  str_to_title)) |>
    mutate(across(sex,
                  recode,
                  "Male" = "M",
                  "Female" = "F")) |>
    left_join(sex_err_rate |>
                select(spawn_year,
                       sex,
                       starts_with("perc_")),
              by = c("spawn_year", "sex")) |>
    pivot_wider(names_from = sex,
                values_from = c(n_fish,
                                perc_false,
                                perc_se)) |>
    mutate(true_male = n_fish_M - (n_fish_M * perc_false_M) + (n_fish_F * perc_false_F),
           true_female = n_fish_F - (n_fish_F * perc_false_F) + (n_fish_M * perc_false_M),
           across(starts_with("true"),
                  round_half_up)) |>
    rowwise() |>
    mutate(true_m_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
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
    mutate(n_sexed = true_male + true_female,
           prop_m = true_male / (true_male + true_female),
           prop_se = deltamethod(~ x1 / (x1 + x2),
                                 mean = c(true_male,
                                          true_female),
                                 cov = diag(c(true_m_se,
                                              true_f_se)^2)),
           fpr = (prop_m) / (1 - prop_m) + 1,
           fpr_se = deltamethod(~ x1 / (1 - x1) + 1,
                                mean = prop_m,
                                cov = prop_se^2)) |>
    ungroup() |>
    rename(n_male = true_male,
           n_female = true_female) |>
    left_join(fpr_all |>
                select(spawn_year,
                       location,
                       n_hatch,
                       n_origin,
                       starts_with("phos")),
              by = c("spawn_year", "location")) |>
    select(any_of(names(fpr_all)))

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
      left_join(fpr_all |>
                  select(location,
                         old_fpr = fpr,
                         old_se = fpr_se)) |>
      mutate(fpr = if_else(is.na(fpr) | fpr == Inf,
                           old_fpr,
                           fpr),
             fpr_se = if_else(is.na(fpr_se) | fpr_se == Inf,
                              old_se,
                              fpr_se)) |>
      select(-starts_with("old"))
  }

  fpr_all <- adj_fpr

  rm(adj_fpr)

  #-----------------------------------------------------------------
  # pull in some estimates from DABOM

  cat("\t Gathering PIT escapement estimates.\n")

  all_escp = readxl::read_excel(paste(dabom_file_path,
                                      dabom_file_name,
                                      sep = "/"),
                                sheet = "Run Escp All Locations") |>
    janitor::clean_names() |>
    filter(spawn_year %in% query_year,
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
    filter(location %in% c('ICL',
                           'PES',
                           'MCL',
                           'CHM',
                           'CHW',
                           'CHL',
                           'NAL',
                           'LWN',
                           'WTL')) |>
    select(spawn_year,
           origin,
           location,
           spawners = estimate,
           spawners_se = se) |>
    mutate(across(origin,
                  recode,
                  "W" = "Natural",
                  "H" = "Hatchery"),
           across(location,
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
    arrange(location, origin)

  # pull out mainstem escapement estimates
  escp_wen_all = all_escp |>
    filter(location %in% c('LWE',
                           'LWE_bb',
                           'TUM_bb',
                           'UWE_bb')) |>
    mutate(across(location,
                  recode,
                  'LWE' = 'Wen_all',
                  'LWE_bb' = 'Below Tumwater',
                  'TUM_bb' = "Above Tumwater",
                  'UWE_bb' = 'Above Tumwater')) |>
    mutate(across(origin,
                  recode,
                  "W" = "Natural",
                  "H" = "Hatchery")) |>
    group_by(spawn_year,
             location,
             origin) |>
    summarise(across(estimate,
                     sum),
              across(se,
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
        filter(spawn_year == yr)
    } else {
      redd_df <- NULL
    }

    wen_tags <- wen_tags_all |>
      filter(spawn_year == yr)

    fpr_df <- fpr_all |>
      filter(spawn_year == yr)

    trib_spawners <- trib_spawners_all |>
      filter(spawn_year == yr)

    escp_wen <- escp_wen_all |>
      filter(spawn_year == yr)

    save(redd_df,
         wen_tags,
         fpr_df,
         trib_spawners,
         escp_wen,
         file = here('analysis/data/derived_data',
                     paste0('wen_', yr, '.rda')))

  }


}
