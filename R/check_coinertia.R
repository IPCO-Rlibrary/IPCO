#' Test coinertia significance function
#'
#' This function will check for significance of coinertia between the paired taxonomy dataset and its functional profile dataset.
#' Ensure that the datasets are normalised/transformed before implementing this command.
#' It will utilise the dudi.pca, dudi.coa, coinertia and randtest commands from the ade4 library and return the results as an list.
#' @param R  R table where rows are functions/pathways and columns are samples
#' @param L  L table where rows are taxa/OTUs and columns are the same samples as table R
#' @param Requires the references functional (R) and its paired taxonomy table (L) and the taxonomy table (Q) for which coinertia and its significance will be calculated.
#' @keywords coinertia significance
#' @examples
#' check_coinertia()
#' @export
check_coinertia <- function(R, L){

R_table <- t(R)
L_table <- t(L)

R_pca <- dudi.pca(R_table, scannf = FALSE, scale = FALSE, nf = 5)
L_pca <- dudi.pca(L_table, scannf = FALSE, scale = FALSE, nf = 5)

coinertia_object <- coinertia(R_pca, L_pca, scannf=FALSE, nf=3)

rand_test <- randtest(coinertia_object, nrepet=100)

results <- list()

results$coinertia <- coinertia_object
results$randtest <- rand_test

return(results)
}