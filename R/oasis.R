#' @title Longitudinal MRI data in nondemented and demented older adults
#' @description
#' This dataset consists of a longitudinal collection of 150 subjects aged 60 to 96. 
#' Each subject was scanned on two or more visits, separated by at least one year 
#' for a total of 373 imaging sessions. For each subject, 3 or 4 individual T1-weighted 
#' MRI scans obtained in single scan sessions are included. 
#'
#' @format A data frame with 373 rows and 15 variables:
#' \describe{
#'   \item{Subject.ID}{Subject identification}
#'   \item{MRI.ID}{MRI Exam Identification}
#'   \item{Group}{Class}
#'   \item{Visit}{Visit order}
#'   \item{MR.Delay}{MR Delay Time (Contrast)}
#'   \item{Gender}{Gender}
#'   \item{Hand}{All subjects are right-handed}
#'   \item{Age}{Age of the subject}
#'   \item{EDUC}{Years of Education}
#'   \item{SES}{Socioeconomic Status}
#'   \item{MMSE}{Mini Mental State Examination}
#'   \item{CDR}{Clinical Dementia Rating}
#'   \item{eTIV}{Estimated total intracranial volume}
#'   \item{nWBV}{Normalize Whole Brain Volume}
#'   \item{ASF}{Atlas Scaling Factor}
#' }
#' @references \url{https://www.kaggle.com/datasets/jboysen/mri-and-alzheimers}
"oasis"
