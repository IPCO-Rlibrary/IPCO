#' Transforms the datasets using Hellinger transformations for IPCO implementation
#'
#' This function will transforms the datasets using Hellinger transformations to be used for IPCO. It will return the dataset in matrix format.
#' If you want to use any other normalisation/transformation, then you can do it separately and use of this command is not required. 
#' @param dataset  dataset where rows are functions/pathways/OTUs and columns are samples
#' @param type Enter the type of your dataset values: proportion, counts or relab (relative abundance)
#' @param Requires the dataset to be transformed and the type of values present in it.
#' @keywords Hellinger
#' @keywords transformation
#' @examples
#' transform_data()
#' @export

transform_data <- function(dataset,type){

if (type == "proportion"){

transformed_dataset <- sqrt(dataset)
return(transformed_dataset)

} else if (type == "count"){

transformed_dataset <- sqrt(prop.table(dataset,2))
return(transformed_dataset)

} else if (type == "relab") {

transformed_dataset <- sqrt((dataset/100))
return(transformed_dataset)

} else {

print("Please enter the type: count, proportion or relab")

}
}

