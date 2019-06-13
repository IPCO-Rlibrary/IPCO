#' IPCO function
#'
#' This function will implement the IPCO algorithm to generate an inferred functionality matrix for your taxonomy dataset.
#' Ensure that the datasets are normalised/transformed before implementing this command.
#' Check for significance of coinertia between R and L if using a different reference dataset than the one provided. If the coinertia between R and L is not significant, results maybe unreliable.
#' @param R  R dataset where rows are functions/pathways and columns are samples
#' @param L  L dataset where rows are taxa/OTUs and columns are the same samples as dataset R
#' @param Q  Q dataset where rows are taxa/OTUs common between L and Q and columns are the samples for which functionality will be inferred
#' @param Requires the references functional (R) and its paired taxonomy dataset (L) and the taxonomy dataset (Q) for which functions need to be inferred
#' @keywords Inferring functionality
#' @examples
#' IPCO()
#' @export
#'

IPCO<-function(R,L,Q){

if (length(which(colnames(L) %in% colnames(R))) != ncol(L)){
	print("Table R and Table L columns donot match. Please ensure that the samples in R and L are the same")
} else {

R_table <- R[,colnames(L)]

common_taxa <- rownames(L)[which (rownames(L) %in% rownames(Q))]

if (length(common_taxa) == 0){
print ("No common taxa/OTUs between table L and Q")
print ("If you using common OTU labels, suggest using Genus level dataset")
print ("Else analysis cannot be done")
}

L <- L[common_taxa,]
Q <- Q[common_taxa,]

print("removing any taxa/OTU with zero abundance across samples in both L and Q table")
L_table <- L[names(which(rowSums(L) != 0 & rowSums(Q) != 0)),]
Q_table <- Q[rownames(L_table),]

if(nrow(L_table) == 0){
print("cannot carry out analysis as no taxa/OTU retained after removing zero abundance")
}

R_table <- t(R_table)
L_table <- t(L_table)

print("Implementing IPCO")

L.coa <- dudi.coa(L_table,scannf = FALSE,nf = 3)
R.pca <- dudi.pca(R_table, row.w = L.coa$lw, scale = FALSE, scannf = FALSE, nf = 3)
Q.pca <- dudi.pca(Q_table, row.w = L.coa$cw, scale = FALSE, scannf = FALSE, nf = 3)

row.w <- L.coa$lw

envAvg <- apply(R_table, 2, function(x){sum(x*row.w)/sum(row.w)})

rlq_object <- rlq(R.pca, L.coa, Q.pca, scannf = FALSE, nf = 3)

Inferred_function <- sweep(as.matrix(rlq_object$tab), 1, envAvg, "+")

return(Inferred_function)
}
}