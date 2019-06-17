#' Filters the functional dataset based on a average function/pathway coverage or abundance threshold
#'
#' This function will filter the functional dataset and remove the low coverage/abundance functions/pathways.
#' By default, use the pathway coverage dataset for filtering. If coverage dataset is not available use the pathway abundance as threshold. 
#' You can opt to not filter the dataset at all so that all functionality is predicted.
#' @param Requires the dataset to be transformed and whether to filter by covearge or abundance.
#' @param dataset  dataset where rows are functions/pathways and columns are samples.
#' @param type the type of dataset provided: (kegg or metacyc).
#' @param Optional threshold_dataset, levels.
#' @param threshold_dataset dataset containing the information about pathway/function coverage. Threshold dataset should contain the same functions/pathways as present in dataset.
#' @param levels Default is 0.01 for coverge or the first quartile of the mean function/pathway abundance.
#' @keywords filter
#' @keywords threshold
#' @examples
#' filter_functionality()
#' @export
filter_functionality <- function(dataset,type,threshold_dataset,threshold){


if (type == "kegg"){
						if(missing(threshold_dataset)){
						print("Please provide coverage dataset")
						}else{
							 if(missing(threshold)){
							 print("threshold not provided, default coverage threshold would be used")
							 threshold<-0.01	
							 filtered_dataset <- dataset[names(which(rowMeans(threshold_dataset) >= threshold)),]
							 return(filtered_dataset)

							 }else{
								  filtered_dataset <- dataset[names(which(rowMeans(threshold_dataset) >= threshold)),]
								  return(filtered_dataset)
								  }
								  }
} else  if (type == "metacyc"){
								if (missing(threshold)){
								print("threshold not provided, removing pathways with values less than 1st quartile of the mean pathway abundance")
								filtered_dataset <- dataset[names(which(rowMeans(dataset) >= summary(rowMeans(dataset))[2])),]
								return(filtered_dataset)
								}else{
									  filtered_dataset <- dataset[names(which(rowMeans(dataset) >= threshold)),]
								      return(filtered_dataset)

									 }
								} else {
										print("error in type: Please enter type=\"kegg\" or type=\"metacyc\"")
										}
}

