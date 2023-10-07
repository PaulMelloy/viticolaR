#' Calculate surviving sporangia
#'
#' Calculate surviving sporangia from germinated oospores GER
#'
#' @param SUS_h numeric, hourly survivability
#'
#' @return numeric, cumulative survivability
#' @noRd
#' @examples
#'calc_GEO(PMO = 1,
#'         SUS_h = calc_SUS(20,95),
#'         cohort = 1)
calc_GEO <- function(PMO, SUS_h, cohort){

  GEO <- SUS_h
  n <- length(GEO)

  if(n != length(cohort)) stop("all argument lengths must match")
  if(n != length(PMO)) stop("all argument lengths must match")

  cohort_s <- unique(cohort)

  for(C_i in cohort_s){
    Start <- which(cohort == C_i)[1]  # first or last hour in cohort?


    GEO[GEO > 1] <- 0
  }

  return(GEO)
}

# The fourth state variable consists of the germinated oospores (GEO),
#  i.e. oospores that have produced sporangia. The sporangia are formed at the
#  end of a germination process, which is triggered by rainfall (Darpoux, 1943;
#  Burruano et al., 1995) and its duration regulated by temperature (Laviola et
#  al., 1986) and moisture (Rossi and Caffi, 2007). The model considers that
#  oospores in the PMO stage begin germination when rainfall moistens the leaf
#  litter and enter the GEO stage based on the variable GER (germination).
# Events triggering oospore germination start with Rh=ε ≥ 0.2 mm (where ε is the
#  first hour of the event), last for the following wet hours irrespective of the
#  presence of further rainfall over such a wet period, and finish with the first
#  dry hour.
