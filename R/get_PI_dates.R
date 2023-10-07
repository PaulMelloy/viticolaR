#' Get primary infection dates
#'
#' @param mod model output class 'm_viticola'
#' @param cohort integer/s, specify cohort to return the primary infection dates.
#'  'all' can be requested and is by default.
#' @param release_stage character vector, specify the release stage/s which you
#'  want returned. "all" by default, or any of the following: "spo_germination_hour",
#'  "spo_death_hour","zoo_release_ind", "zoo_dispersal_ind","zoo_infection_ind",
#'  "INC_h_lower","INC_h_upper".
#'
#' @return data.table of dates each of the growth stages reach for each cohort
#' @export
#'
#' @examples
#' nt_mod <- estimate_DM_PI(nt_weather)
#' get_PI_dates(nt_mod)
get_PI_dates <- function(mod,
                         cohort = "all",
                         release_stage = "all") {

  # get a vector of all cohort lengths
  if(cohort == "all"){
    cohort <- seq_len(mod$cohorts)
  }
  #check correct cohort is entered
  if(any(cohort %in% seq_len(mod$cohorts)) == FALSE){
    stop("Only ",mod$cohorts," estimated.\n
            Please input a cohort integer between 1 and ",mod$cohorts)
  }

  # ready stages
  r_stages <- c("spo_germination_hour","spo_death_hour","zoo_release_ind",
                "zoo_dispersal_ind","zoo_infection_ind","INC_h_lower","INC_h_upper")

  if(release_stage == "all") release_stage <- r_stages

  # check the relsease stage is input as an expected variable
  if(any(release_stage %in% r_stages) == FALSE){
    stop("'release_stage' input not understood
            Please input one of the following stage variables: ",
         "'",paste0(r_stages[1:6], collapes = "', '"), r_stages[7],"'")
  }

  # initialise cohort table
  dat <- data.table::data.table(
    cohort = cohort
  )

  # Create table indicating the extent of progress for each cohort
  dat[, get("r_stages") :=
        lapply(r_stages,function(r_s){
          unlist(lapply(cohort,function(c1){
            mod$cohort_list[[c1]][r_s]
            }),use.names = FALSE)
  })]


  dat_l <- data.table::melt(dat,
                            id.vars = "cohort",
                            measure.vars = get("r_stages"),
                            variable.name = "primary_infection_stage",
                            value.name = "hour")

  # convert indx to date_time
  dat_l[,hour := mod$start_time + (hour*3600)]
  dat_l
  return(print(dat_l))
}
