#' Tiff File Check
#'
#' checks to see if the last part of a file name is .tif or .tiff
#'
#' @param character in the form something.tif
#' @return bool of TRUE or FALSE

.is_tiff <- function(character){
  lastStringInSplit <- strsplit(character, "")
  lenStrSp = length(lastStringInSplit[[1]])
  fileType <- lastStringInSplit[[1]][as.integer(lenStrSp - 3):lenStrSp]
  fileTypeOneString = paste0(fileType[1], fileType[2], fileType[3], fileType[4])
  if (fileTypeOneString == ".tif" | fileTypeOneString == "tiff") {
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


#' Null Argument Checking
#'
#' Checks a parameter for a null argument
#' @param  parameter what you would like to check if is null
#' @param parameterName Allows the error message to be in context
#' @return stop if is null

.null <- function(parameter, parameterName){
  if (is.null(parameter)) {
    return(stop(paste0("Need to specify ", parameterName)))
  }

}

#' Empty File Checking
#'
#' Checks a given file path with name to see if the file is empty 0b
#'
#' @param filePath The path for the directory where the file is located
#' @param fileName The name of the file located in the path directory
#' @return a bool of TRUE or FALSE
not_empty_file <- function(filePath, fileName){
  if (is.na(filePath) ||  is.na(fileName)) {
    return(stop("missing filePath or fileName"))
  }
  #used [4] just in case the image is corrupt(NA size)
  if (is.na(file.info(paste0(filePath, fileName))[4]$mtime)){
    return(stop(paste0("The file ", filePath, fileName, " doesn't exist")))
  }
  if (as.numeric(file.info(paste0(filePath,fileName))[1]) != 0) {
    return(notEmpty = TRUE)
  }
  else {
    return(notEmpty = FALSE)
  }
}



#' Horizontal Image Checking
#'
#' Checks to see if the image is vertical or horizontal, if vertical, makes horizontal
#'
#' @param matrix The matrix of an imported tiff,png,etc...
#' @return The matrix horizontally
.horizontal_image_check <- function(matrix){
  ncol_matrix <- ncol(matrix)
  nrow_matrix <- nrow(matrix)

  if (ncol_matrix < nrow_matrix) {
    retVal <- apply(matrix,1,rev)
  }
  else {
    retVal <- matrix
  }
  return(retVal)
}



#Array Changes

#For class array, scales the array to aid in digitization
#NOTE: if > ,,1 exists, will not be returned to the user

#@param array Any class arrays

#@return edited array
# .array_class_edit <- function(array){
#   if (class(array) == "array") {
#     array <- array[,,1]
#     array <- 1/array
#     if (min(array) == 1) {
#       array <- array - 1
#       array <- array/max(array)
#     }
#     return(array)
#
#   }
#   else{
#     return(array)  # This is the case of no array
#   }
# }



#This is for later with the full function (with for loops so the function doesn't crash)
# tryCatchError <- function(aFunction){
#   running <- tryCatch(aFunction, error = function(e) e)
#   if (inherits(running, "error")) {
#     return(running)
#   }
#   else {
#     return(running)
#   }
# }



#' HDV Checking
#'
#' HDV is a type of image with three normal images on the same scan, these cause big problems
#' filters them out by the name
#'
#' @param imageName The file name for the specific image
#'
#' @return warning if these are found (use a tryCatch)
.hdv_check <- function(imageName){
  splitName <- strsplit(imageName, "-")
  imageType <- splitName[[1]][3]
  imageTypePosition2 <- splitName[[1]][2]
  if (imageType == "HDV" || imageType == "HDZ" || imageType == "ZDH" || imageType == "H D and V"
      || imageType == "V D and H" || imageType == "A" || imageType == "B" || imageType == "C" || is.na(imageType) || imageTypePosition2 == "HDV" ||
      imageTypePosition2 == "HDZ" || imageTypePosition2 == "ZDH" ||
      imageTypePosition2 == "H D and V" || imageTypePosition2 == "V D and H" || imageTypePosition2 == "A" || imageTypePosition2 == "B" || imageTypePosition2 == "C" || is.na(imageTypePosition2)) {
    return(warning("This image is a HDV or HDZ"))
  }
}




