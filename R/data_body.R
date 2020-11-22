#' NHANES body composition dataset
#'
#' This dataset includes body composition data measured in 2148 adults aged 20
#' to 59 years in the United States. These data are taken from the National
#' Health and Nutrition Examination Survey (NHANES) 2017-2018:
#' \url{https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2017} (Centers for Disease Control and
#' Prevention, 2020).
#'
#' @format A data frame (tibble) with 2148 rows and 8 columns:
#' \itemize{
#' \item \code{ID}: respondent identifier;
#' \item \code{GENDER}: gender (0: male, 1: female);
#' \item \code{AGE}: age (years);
#' \item \code{HEIGHT}: height (cm);
#' \item \code{WEIGHT}: weight (kg);
#' \item \code{FAT}: body fat (\%);
#' \item \code{WAIST}: waist circumference (cm);
#' \item \code{GLYCO}: glycohemoglobin (\%).
#' }
#'
#' @references
#' Centers for Disease Control and Prevention (2020). National Health and
#' Nutrition Examination Survey Data.
#'
#' @seealso \code{\link{data_air}}, \code{\link{gmbn_body}},
#' \code{\link{gmdbn_air}}, \code{\link{gmm_body}}

"data_body"
