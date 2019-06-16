#' Loads IPCO reference datasets
#'
#' This function will load the references datasets provided by IPCO as default
#' If you want to use any other datasets as reference, then this function is not required
#' @param samples  use samples="HMP" if you want to upload HMP dataset rather than using the cohort of healthy samples as reference
#' @keywords reference data
#' @examples
#' load_ref()
#' @export

load_ref <- function(samples){

if(samples=="HMP"){
load(paste0(find.package("IPCO"),"/data/IPCO_HMP.RData"))
} else if (missing(samples)){
load(paste0(find.package("IPCO"),"/data/IPCO_Healthy.RData"))
} else {
print("type load_ref(samples=\"HMP\") to load Reference HMP or load_ref() to load the cohort of healthy")
}
}
