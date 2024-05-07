#' rforSS3 functions to facilitate using Stock Synthesis
#'
#' @description The rforSS3 package provides a selection of functions needed
#'   when conducting stock assessments using Stock Synthesis 3. The current 
#'   version of rforSS3 contains functions that are used with SS3 for 
#'   relatively advanced assessments, but the intent is to include others
#'   to simplify its use in data-poor situations.
#'
#'   The package includes utility functions that manage directory structures
#'   and, once an analysis is selected, they manage the workflow, copying files
#'   to the 'calc' directory and once finished copying the results back again.
#' @section Directory and File Functions:
#' \itemize{
#'   \item cleanDir - removes any previous analyses and plots stored in 'SubDir'
#'   \item copyfiles - copies required files from store to the calc directory
#'   \item dirExists: Creates a directory if it does not already exist
#'   \item storeresults - Copies result files back into destination directory
#'   \item placeholder
#'   \item placeholder
#' }
#' @section String manipulation Functions (paths and character strongs):
#' \itemize{
#'   \item removeEmpty - removes empty strings from a vector of strings
#'   \item firstNum - converts the first string in a vector to a number
#'   \item pathtype - finds the type of separator used in a path
#'   \item pathend - determines what character is at the end of a path
#'   \item placeholder
#'   \item placeholder
#' }
#' @section File Manipulation Functions:
#' \itemize{
#'   \item fixstarter - saves a new starter.ss file ready to use the par file
#'   \item placeholder
#'   \item placeholder
#'   \item placeholder
#' }
#' @section Utility Functions:
#' \itemize{
#'   \item codeBlock - delineate some comment lines ready to document code
#'   \item placeholder
#' }
#' @docType package
#' @name rforSS3
#' @keywords internal
"_PACKAGE"
NULL


#' @importFrom  r4ss SS_fitbiasramp SSMethod.TA1.8 SSMethod.Cond.TA1.8
#' @importFrom grDevices dev.cur dev.new dev.off graphics.off png
#' @import graphics
#' @importFrom utils read.csv tail askYesNo
#' @importFrom codeutils pathtopath %ni% makelist which.closest
#' @importFrom hplot plotnull plotprep parset 
NULL





