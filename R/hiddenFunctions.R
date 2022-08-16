
#' Maximum/Minimum distance(percentage) from Edge
#'
#' Takes a percentage and calculates the cutoff value for a
#' function to look for something.  Used for find_peaks
#'
#' @param rowSums The sum of the rows of the matrix
#' @param percentage Percentage of the picture that you would like removed from edge
#'  if percentageLeftSide is unspecified uses percentage for both i.e left = 1 - right
#' @param percentageLeftSide the left percentage that you want to remove(default = NULL)
#' @return vector of (rightSideDistance, leftSideDistance)

.allowed_edge_distance <- function(rowSums, percentage, percentageLeftSide = NULL){
  if (percentage > 100 ) {
    return(stop("Can't have a percentage over 100 "))
  }
  if (!is.null(percentageLeftSide)) {
    if (percentageLeftSide  > 100) {
      return(stop("Can't have a percentage over 100 "))
    }
  }
  if (percentage & !is.null(percentageLeftSide)) {
    if ((percentage + percentageLeftSide) >= 100) {
      return(stop("Can't have percentage plus percentageLeftSide be greater then or equal to 100.. no area left"))
    }
  }
  right <- percentage/100
  if (is.null(percentageLeftSide)) {
    left <- right
    rightDist <- round((1 - right)*length(rowSums))
    leftDist <- round((left)*length(rowSums))
  }
  else{
    rightDistance <- right
    leftDistance <- percentageLeftSide/100

    rightDist <- round((1 - rightDistance)*length(rowSums))
    leftDist <- round((leftDistance)*length(rowSums))
  }


  ret <- data.frame(leftDist = leftDist,rightDist = rightDist)
  return(ret)
}



#'Scanning Flair Around Outside of Image
#'
#'Checks the list of peaks for any flairs in the photos at start and end, they are removed
#'
#'@param FindingPeaksdf the resulting data frame from finding_peaks
#'(if called separately make sure in correct form nx2: Index column and Height column)
#'@param rowSums  of the imported matrix from original picture
#'@param percentEdge passed into .allowed_edge_distance
#'@param percentEdgeForLeft is user needs to specify a different percentage for left and right side cutoff
#'will make percentEdge for the right side and percentEdgeForLeft for the left side of image
#'@return void
.edge_peaks_rm <- function(FindingPeaksdf, rowSums, percentEdge, percentEdgeForLeft = NULL){
  if (!is.vector(rowSums) | !is.data.frame(FindingPeaksdf)) {
    return(stop("rowSums(vector) and FindingPeaksdf(dataframe) must be in the correct form"))
  }
  if (length(rowSums) < max(FindingPeaksdf$Index)) {
    return(stop("rowSums should be at least the same length as your largest indexed peak"))
  }
  rm <- vector()
  dist <- .allowed_edge_distance(rowSums, percentEdge, percentageLeftSide = percentEdgeForLeft)
  rightSide <- dist$rightDist
  leftSide <- dist$leftDist
  for (k in 1:length(FindingPeaksdf$Index)) {

    if (FindingPeaksdf$Index[k] >= rightSide) {

      rm <- c(rm, FindingPeaksdf$Index[k])

    }
    else if (FindingPeaksdf$Index[k] <= leftSide) {

      rm <- c(rm, FindingPeaksdf$Index[k])

    }
  }
  matchIndex = vector()
  for (i in 1:length(FindingPeaksdf$Index)) {
    if (!is.na(match(FindingPeaksdf$Index[i], rm))) {
      matchIndex <- c(matchIndex, i)

    }
  }
  if (is.logical(matchIndex)) {
    retHeights <- FindingPeaksdf$Height
    retIndex <- FindingPeaksdf$Index
  }
  else{
    retHeights <- FindingPeaksdf$Height[-matchIndex]
    retIndex <- FindingPeaksdf$Index[-matchIndex]
  }
  FindingPeaksdf <- data.frame(Index = retIndex, Height = retHeights)
  return(FindingPeaksdf)
}


#' Highest Peaks
#'
#' Finds the highest peaks up to the maximum number of peaks allowed(set by user on call)
#'
#' @param FindingPeaksdf the resulting data frame from finding_peaks
#'(if called separately make sure in correct form nx2: Index column and Height column)
#' @param maxPeaksAllowed The maximum number of lines on a picture you would like to find
#'
#' @return The highest peaks up to the max number allowed
.highest_peaks <- function(FindingPeaksdf, maxPeaksAllowed){
  if (!is.data.frame(FindingPeaksdf)) {
    return(stop("FindingPeaksdf(dataframe) must be a dataframe"))
  }
  if (maxPeaksAllowed <= 0) {
    return(stop("MaxPeaksAllowed must be more then 0"))
  }
  highestPeaksIndex <- vector()
  highestPeaksHeight <- vector()
  if (length(FindingPeaksdf$Index) > maxPeaksAllowed) {
    sorting <- sort(FindingPeaksdf$Height, decreasing = TRUE, index.return = TRUE)
    for (j in 1:maxPeaksAllowed) {
      highestPeaksIndex <- c(highestPeaksIndex, FindingPeaksdf$Index[sorting$ix[j]])#ix is a product from sort
      highestPeaksHeight <- c(highestPeaksHeight, FindingPeaksdf$Height[sorting$ix[j]])
    }
  }
  if (length(FindingPeaksdf$Index) <= maxPeaksAllowed) {
    highestPeaksIndex <- FindingPeaksdf$Index
    highestPeaksHeight <- FindingPeaksdf$Height
  }
  highestPeaks <- data.frame(highestPeaksIndex, highestPeaksHeight)
  names(highestPeaks) <-  c("Index", "Height")
  return(highestPeaks)
}


#' Distance to Closest Peak or Border
#'
#' Finds the closest peak(or border) index for a given peak on either side
#'
#' @param FindingPeaksdf the resulting data frame from finding_peaks
#'(if called separately make sure in correct form nx2: Index column and Height column)
#' @param rowSums of the imported matrix from original picture
#' @param peakIndex the index of FindingPeaksdf that contains the peak you want to look at
#'
#' @return dataframe  1x2 leftDist, rightDist for columns
.finding_distance_to_peaks <- function(FindingPeaksdf, rowSums, peakIndex){
  FindingPeaksIndex <- sort(FindingPeaksdf$Index, decreasing = FALSE)
  currentIndex <- which(FindingPeaksIndex == FindingPeaksdf$Index[peakIndex])
  if (peakIndex > length(FindingPeaksIndex)) {
    return(stop("Less peaks then peak index"))
  }
  if (length(FindingPeaksIndex) == 1) {
    distanceToRight <- length(rowSums) - FindingPeaksIndex[currentIndex]
    distanceToLeft <- FindingPeaksIndex[currentIndex] - 1
  }
  else if (currentIndex > 1 & currentIndex < length(FindingPeaksIndex)) {
    distanceToRight <- FindingPeaksIndex[currentIndex + 1] - FindingPeaksIndex[currentIndex]
    distanceToLeft <- FindingPeaksIndex[currentIndex] - FindingPeaksIndex[currentIndex - 1]
  }
  else if (currentIndex == 1) {
    distanceToRight <- FindingPeaksIndex[currentIndex + 1] - FindingPeaksIndex[currentIndex]
    distanceToLeft <- FindingPeaksIndex[currentIndex] - 1
  }
  else if (currentIndex == length(FindingPeaksIndex)) {
    distanceToRight <- length(rowSums) - FindingPeaksIndex[currentIndex]
    distanceToLeft <- FindingPeaksIndex[currentIndex] - FindingPeaksIndex[currentIndex - 1]
  }
  Distance <- data.frame(leftDist = distanceToLeft, rightDist = distanceToRight)
  return(Distance)
}




#' Finding starting point and ending point for each peak
#'
#' @param FindingPeaksdf the resulting data frame from finding_peaks
#'(if called separately make sure in correct form nx2: Index column and Height column)
#' @param rowSums of the imported matrix from original picture
#'
#' @return FindingPeaksdf along with the starting and ending points of each peak
.finding_Peak_Start_Ends <- function(FindingPeaksdf, rowSums){
  peakStart <- vector()
  peakEnd <- vector()
  for (k in 1:length(FindingPeaksdf$Index)) {
    height <- FindingPeaksdf$Height[k]
    tempHeightLeft <- height
    tempHeightRight <- height
    closestPeaks <- .finding_distance_to_peaks(FindingPeaksdf, rowSums, peakIndex = k)
    index = min(closestPeaks$leftDist, closestPeaks$rightDist)

    for (j in 1:index) {
      rightSide <- FindingPeaksdf$Index[k] + j
      leftSide <- FindingPeaksdf$Index[k] - j
      if (j == index) {
        peakStart[k] <- leftSide
        peakEnd[k] <- rightSide
        break
      }
      if (rowSums[leftSide] <= tempHeightLeft &
          rowSums[rightSide] <= tempHeightRight) {
        tempHeightLeft <- rowSums[leftSide]
        tempHeightRight <- rowSums[rightSide]
      }
      else if (rowSums[leftSide + 1] <= tempHeightLeft &
               rowSums[rightSide] <= tempHeightRight) {

        tempHeightLeft <- rowSums[leftSide + 1]
        tempHeightRight <- rowSums[rightSide]
      }
      else if (rowSums[leftSide] <= tempHeightLeft &
              rowSums[rightSide + 1] <= tempHeightRight) {

        tempHeightLeft <- rowSums[leftSide]
        tempHeightRight <- rowSums[rightSide + 1]
      }
      else{
        peakStart[k] <- leftSide
        peakEnd[k] <- rightSide
        break
      }
    }

  }

  ret <- data.frame(FindingPeaksdf, PeakStart = round(peakStart), PeakEnd = round(peakEnd))
  return(ret)
}



#' Plotting Peaks from find_peaks
#'
#' This can be used alone from find_peaks, ensure that you have the correct df form.
#' starting are green lines, ending are blue lines
#'
#' @param FindPeaksdf usually found with find_peaks nx4: Index, Height, PeakStart, PeakEnd
#' @param rowSums of the imported matrix from original picture
#' @param StartEndLine bool, if you want a plotted starting and ending line for each peak
#'
#' @return void
.plot_peaks <- function(FindPeaksdf, rowSums, StartEndLine = TRUE){
  graphics::plot(rowSums, type = "l")
  graphics::points(FindPeaksdf$Index, FindPeaksdf$Height, pch = 20, col = "red")
  if (StartEndLine == TRUE) {
    graphics::abline(v = FindPeaksdf$PeakStart, col = "green")
    graphics::abline(v = FindPeaksdf$PeakEnd, col = "blue")
  }
}



#' Non bright image scaling with gaussian deconvolution
#'
#' @param Filter a vector specifying the dimensions of the kernel,
#'  which will be used to perform either delation or erosion, such as c(8,8)
#' @param method one of 'delation'(adds to image, making brights brighter), 'erosion' (subtracts from image brights darker)
#' @param threshold should be between 0 and 1 for normalized images
#' @param imageMatrix An imported image, can be imported with tiff_import()
#'
#' @return Gaussian matrix scaled for bright
.not_bright_image <- function(imageMatrix, Filter = c(8,8), method = 'delation', threshold = 0.5){#cutoffQuantile, sig = 10, kern.trunc = 0.05, nw = 3){
  imageMatrix[is.na(imageMatrix)] <- 0
  Dialation <- OpenImageR::delationErosion(imageMatrix, Filter = Filter, method = method)
  imageProcessed <- OpenImageR::image_thresholding(Dialation, thresh = threshold)

  # gaussImageMatrix <- suppressWarnings(abs(t( apply(imageMatrix, MARGIN = 1, FUN = deconv_gauss, sig = 10, kern.trunc = 0.05, nw = 3 ) )))
  #
  # imageMatrix[imageMatrix < (1 - mean(imageMatrix,na.rm = TRUE))] <- 0
  # imageMatrix[imageMatrix > 0] <- 1
  #
  # gaussImageMatrix[0:floor((1/62)*nrow(imageMatrix))] <- mean(gaussImageMatrix)
  # gaussImageMatrix[(nrow(gaussImageMatrix) - 0.01*nrow(imageMatrix)):(nrow(gaussImageMatrix)),] <- mean(gaussImageMatrix)
  # gaussImageMatrix[is.na(gaussImageMatrix)]
  # gaussImageMatrix[gaussImageMatrix < 0] <- 0
  # gaussImageMatrix[gaussImageMatrix < (stats::quantile(gaussImageMatrix, cutoffQuantile))] <- 0
  # gaussImageMatrix[gaussImageMatrix > 0] <- 1
  # retdf <- list(imageMatrix = imageMatrix, gaussImageMatrix = gaussImageMatrix)
  return(imageProcessed)

}



#' Bright image scaling to gaussian
#'
#' @param Filter a vector specifying the dimensions of the kernel,
#'  which will be used to perform either delation or erosion, such as c(13,13)
#' @param method one of 'delation'(adds to image, making brights brighter), 'erosion' (subtracts from image brights darker)
#' @param threshold should be between 0 and 1 for normalized images
#' @param imageMatrix An imported image, can be imported with tiff_import()
#'
#' @return Gaussian matrix scaled for bright
.for_bright_image <- function(imageMatrix, Filter = c(13,13), method = 'delation', threshold = 0.8){#imageMatrix, sig = 10, kern.trunc = 0.05, nw = 3, brightQuantile = 0.95){
  imageMatrix[is.na(imageMatrix)] <- 0
  Dialation <- OpenImageR::delationErosion(imageMatrix, Filter = Filter, method = method)
  imageProcessed <- OpenImageR::image_thresholding(Dialation, thresh = threshold)
  # imageMatrix[is.na(imageMatrix)] <- 0
  # imageMatrix[imageMatrix < (stats::quantile(imageMatrix, brightQuantile))] <- 0
  # imageMatrix[imageMatrix > 0] <- 1
  # gaussImageMatrix <- abs(t(apply(imageMatrix, MARGIN = 1, FUN = deconv_gauss, sig = 10, kern.trunc = 0.05, nw = 3 )))
  #
  # gaussImageMatrix[0:floor((1/20) * nrow(imageMatrix))] <- mean(gaussImageMatrix)
  # gaussImageMatrix[(nrow(gaussImageMatrix) - 0.05 * nrow(imageMatrix)):nrow(gaussImageMatrix), ] <- mean(gaussImageMatrix)
  # gaussImageMatrix[is.na(gaussImageMatrix)]
  # gaussImageMatrix[gaussImageMatrix < 0] <- 0
  # gaussImageMatrix[gaussImageMatrix < (stats::quantile(gaussImageMatrix, brightQuantile))] <- 0
  # gaussImageMatrix[gaussImageMatrix > 0] <- 1
  # retdf <- list(imageMatrix = imageMatrix, gaussImageMatrix = gaussImageMatrix)
  return(imageProcessed)
}




#' Trace Starting and Ending Points
#'
#' Can also be used to remove those ranges where the trace isn't if returnMat is TRUE
#'
#' @param imageMatrix Imported image into matrix form, can use tiff_import(), processed already
#' @param cutPercentage A bound for which the start and end is never found, could be a flare
#' on the sides of the scanned image
#' @param peakThreshold Smallest difference allowed for a difference to be a possible peak
#' @param gapAllow Distance in which another index could be considered to be the same peak,
#' applicable for large peaks
#' @param returnMat bool, default is to return the image with removed start and end to the user, if FALSE
#' will return just the index of the start and end
#' @param maxStart absolute max for the left side of the image (can be overridden with the length)
#' @param minEnd absolute min for the right side of the image ( can be overridden with the 0)
#'
#' @return Image matrix with removed start and end, or the index of these start and ends
.get_trace_start_ends <- function(imageMatrix, cutPercentage = 1, peakThreshold = 5, gapAllow = 20,
                                  returnMat = TRUE, maxStart = 700, minEnd = 4800){
  # imageMatrix <- .horizontal_image_check(imageMatrix) removed these two lines because of double processing
  # processedImage <- .for_bright_image(imageMatrix) #Even if not bright image, found this to be the most consistent
  SumsImage <- colSums(imageMatrix)
  len <- length(SumsImage)
  diffsColSms <- abs(diff(SumsImage))
  pkThresh <- peakThreshold/100
  possibleStartDiffs <- which(diffsColSms >= pkThresh) # how big of a difference you are looking for (potential peaks)
  #riddance of gaps between possible starts(includes the black surround on an image)
  chosenDiffs <- possibleStartDiffs[which(abs(diff(possibleStartDiffs))  <= gapAllow)]
  cutPerc <- round(cutPercentage/100*len)
  chosenDiffs <- chosenDiffs[which(chosenDiffs <= (len - cutPerc) & chosenDiffs >= cutPerc)] # which chooseDiffs are inbetween cut perc
  first <- chosenDiffs[1] # starting with the first one in the list(closest to the out side of the image, and working in) Left Side
  newFirst <- first
  last <- chosenDiffs[length(chosenDiffs)] # starting with the last one in the list(closest to the out side of the image, and working in) Right Side
  newLast <- last
  # See if the next image has a larger or smaller col sum
  compareLeft <- SumsImage[first]
  compareRight <- SumsImage[last]
  middle <- len/2
  middleMean <- mean(SumsImage[round((middle - 0.2*len)):round((middle + 0.2*len))]) # catches run up onto the main flat of the colSums
  #plot to see main flat
  if (compareLeft > middleMean) { # checking that we aren't on the flat already
    compareLeft = middleMean
  }
  if (compareRight > middleMean) { # checking that we aren't on the flat already
    compareRight = middleMean
  }
  if (first + round(0.2 * len) >= len) { # Check that first + 20 perc isn't greater then total length
    index <- len - first - 1
  }
  else {
    index <- round(0.2*len)
  }
  # for the left side ````````
  for (j in 1:index) {
    if ( (first + j) > maxStart) {
      newFirst <- maxStart
      break
    }
    else if (SumsImage[first + j] <= compareLeft + 1 & SumsImage[first + j]  <= middleMean - 3) {
      newFirst <- first + j
    }
  }
  #``````````````````````````
  if (last - round(0.2 * len) <= 0) { # Check that first + 20 perc isn't greater then total length
    index <- last - 1
  }
  else {
    index <- round(0.2*len)
  }
  #for Right Side ````````````
  for (k in 1:index) { # for the right side
    if ((last + j) < minEnd) {
      newLast <- minEnd
      break
    }
    if (SumsImage[last - k] <= compareRight + 1 & SumsImage[last - k]  <= middleMean - 3) {
      newLast <- last - k
    }
  }
  #`````````````````````````
  if (returnMat == FALSE) {
  return(list(Start = newFirst, End = newLast))
  }
  else {# returnMat is true (removes the parts of image in those created bounds\)
    ImageNoSides <- imageMatrix[,-c(0:newFirst, newLast:ncol(imageMatrix))]
                         # gaussiaMatrix = processedImage[,-c(0:newFirst, newLast:ncol(imageMatrix))])

    return(ImageNoSides)
  }
}


# Dont think that I am using this right now
# .get_trace_top_bottom <- function(imageMatrix, minDistance, maxPeakNumber, percentFromEdge){
#   browser()
#   rowSums <- rowSums(imageMatrix)
#   chosenFlats <- vector()
#   peaks <- find_peaks(rowSums, minDistance = minDistance, maxPeakNumber = maxPeakNumber, percentFromEdge = percentFromEdge, plots = FALSE)
#   firstPeak <- peaks$PeakStart[1]
#   lastPeak <- peaks$PeakEnd[length(peaks$Index)]
#   diffIndex <- which(diff(rowSums(imageMatrix)) == 0)
#   possibleFlats <- rowSums[diffIndex]
#   # for (i in 1:(length(possibleFlats) - 3)) {
#   #   if (possibleFlats[i] == possibleFlats[i + 2] & possibleFlats[i] == possibleFlats[i + 3]) {
#   #     chosenFlats <- c(chosenFlats, possibleFlats[i])
#   #   }
#   # }
#   topFlats <- rowSums[which(diffIndex < firstPeak)]#which(diffIndex == chosenFlats) < firstPeak)
#   bottomFlats <- rowSums[which(diffIndex > lastPeak)]#which(diffIndex == chosenFlats) > lastPeak)
#   return(list(topFlats[length(topFlats)], bottomFlats[1]))
# }




#' Top Image Cut
#'
#' returns part of the image that doesn't have any horizontal lines (traces) in,
#' will discard lettering(main use)
#'
#' @param imageMatrix Imported image into matrix form, can use tiff_import()
#' @param percentEdgeForLeft passed into find peaks, if not specified, uses
#' percentFromEdge for both left and right sides, if specified, percentFromEdge
#' is defaulted to just the right side of the plot
#' @param percentFromEdge used in find_peaks if you know there wont be a peak
#'  in a region
#'
#' @return the recommended cutoff of the top of your image
.top_image_cut <- function(imageMatrix, percentFromEdge, percentEdgeForLeft = NULL){
  rowsumsImage <- rowSums(imageMatrix)
  diffRowSumsImage <- diff(rowsumsImage)
  zeros <- .find_a_number(diffRowSumsImage, specNumber = 0)
  peaks <- find_peaks(rowSums = rowsumsImage, minDistance = 50, maxPeakNumber = 4, percentFromEdge = percentFromEdge,
                      plots = FALSE, percentEdgeForLeft = percentEdgeForLeft)
  firstPeak <- peaks$Index[1] # closest peak to the top of the image(because searching as if the image was vertical)
  longestCut <- sort(zeros$RunLength[which(zeros$StartIndex < firstPeak)], decreasing = TRUE)[1] # longest less then the first peak start
  indexesOfLongestCuts <- zeros$StartIndex[which(zeros$RunLength == longestCut)] # indexes of all points with that run length of 0's
  if (length(indexesOfLongestCuts) > 1) {# more then one found with that run length
    # takes the closest point less than the start of the first peak
    topCut <- indexesOfLongestCuts[sort(which(indexesOfLongestCuts < firstPeak), decreasing = TRUE)][1]
  }
  else if (length(indexesOfLongestCuts) == 1) {# only one found with that run length
    #takes the only one of that run length
    topCut <- indexesOfLongestCuts[sort(which(indexesOfLongestCuts < firstPeak), decreasing = TRUE)]
  }
  else{# none found with alg, just using top of image
    warning("No top cuts found.. defaulting to 0")
    topCut = 0
  }
  return(topCut)
}


#' Bottom Image Cut
#'
#' attempts to find a gap between the traces and timing marks, if found it will
#'  return the point in which the image can be trimmed removing the timing marks
#'
#' @param imageMatrix Imported image into matrix form, can use tiff_import()
#' @param percentFromEdge used in find_peaks if you know there wont be a peak
#' in a region
#' @param percentEdgeForLeft passed into find peaks, if not specified, uses
#' percentFromEdge for both left and right sides, if specified, percentFromEdge
#' is defaulted to just the right side of the plot
#' @param shortestAllowedSeqOfZeros smallest gap allowed to be found to consider
#' the trace not intersecting the timing marks
#'
#' @return value of bottom cut that should be removed
.bottom_image_cut <- function(imageMatrix, percentFromEdge, percentEdgeForLeft = NULL, shortestAllowedSeqOfZeros = 50){
  rowsumsImage <- rowSums(imageMatrix)
  diffRowSumsImage <- diff(rowsumsImage)
  zeros <- .find_a_number(diffRowSumsImage, specNumber = 0)
  peaks <- find_peaks(rowSums = rowsumsImage, minDistance = 100, maxPeakNumber = 4, percentFromEdge = percentFromEdge,
                      plots = FALSE, percentEdgeForLeft = percentEdgeForLeft)
  SecondPeak <- peaks$Index[2]
  ThirdPeak <- peaks$Index[3]

  longestCut <- sort(zeros$RunLength[which(zeros$StartIndex > SecondPeak & zeros$StartIndex < ThirdPeak)],
                     decreasing = TRUE)[1] # longest greater then the second peak end less then the third peak start
  indexesOfLongestCuts <- zeros$StartIndex[which(zeros$RunLength == longestCut)] # indexes of all points with that run length of 0's
  if (length(longestCut) == 0 || is.na(longestCut)) {# none found with alg, just using bottom of image (no warning displayed for this one though)
    warning("No cuts found.. defaulting to bottom of the image")
    bottomCut = length(rowsumsImage)
  }
  else if (longestCut < shortestAllowedSeqOfZeros) {
    warning("Intersection in Timing Found")
    return(length(rowsumsImage))
  }
  else if (length(indexesOfLongestCuts) > 1) {# more then one found with that run length
    # takes the takes the closest point to the start of the third peak
    bottomCut <- indexesOfLongestCuts[sort(which(indexesOfLongestCuts > SecondPeak & indexesOfLongestCuts < ThirdPeak),
                                           decreasing = TRUE)][1]
  }
  else if (length(indexesOfLongestCuts) == 1) {# only one found with that run length
    #takes the only one of that run length
    bottomCut <- indexesOfLongestCuts[sort(which(indexesOfLongestCuts > SecondPeak & indexesOfLongestCuts < ThirdPeak),
                                           decreasing = TRUE)]
  }
  return(bottomCut)
}





#' Process Image
#'
#' @param imageMatrix Imported image into matrix form, can use tiff_import()
#' @param beta0 From logistic regression on what images to be considered bright
#' @param beta1 From logistic regression on what images to be considered bright
#' @param cutoffProbability Passed into bright: The probability cut off for the decision of an imageMatrix being bright
#' @param NADefault The defult value set to points of NA found by the system
#' @param methodBright one of 'delation'(adds to image, making brights brighter), 'erosion' (subtracts from image brights darker)
#' @param methodNonBright one of 'delation'(adds to image, making brights brighter), 'erosion' (subtracts from image brights darker)
#' @param thresholdBright should be between 0 and 1 for normalized images Default = 0.8
#' @param thresholdNonBright should be between 0 and 1 for normalized images Default = 0.5
#' @param FilterBright Vector specifying the dimensions of the kernel,
#'  which will be used to perform either delation or erosion, such as c(13,13)
#' @param FilterNonBright Vector specifying the dimensions of the kernel,
#'  which will be used to perform either delation or erosion, such as c(8,8)
#'
#' @return The processed image with the gaussian and the non gaussian in an array labeled respectively
.process_image <- function(imageMatrix, FilterBright = c(13,13), FilterNonBright = c(8,8), methodBright = "delation",
                           methodNonBright = "delation", thresholdBright = 0.8, thresholdNonBright = 0.5,
                           beta0 = -2.774327, beta1 = 51.91687, cutoffProbability = 0.5,
                           NADefault = 0){
  bright <- bright(imageMatrix, beta0 = beta0, beta1 = beta1, cutoffProbability = cutoffProbability,
                   NADefault = NADefault)
  if (bright == TRUE) {
    imageProcessed <- .for_bright_image(imageMatrix, Filter = FilterBright,
                                        method = methodBright, threshold = thresholdBright)
  }
  if (bright == FALSE) {
    imageProcessed <- .not_bright_image(imageMatrix, Filter = FilterNonBright,
                                        method = methodNonBright, threshold = thresholdNonBright)#, cutoffQuantile = cutoffQuantile)
  }
  return(imageProcessed)
}


#' Find A Specific Number
#'
#' Used for finding all sequences of one number in a vector and start indexes
#' and lengths of each sequence are returned to the user
#'
#' @param vector a generic vector of any length
#' @param specNumber a number that you would like to find all sequences of
#'
#' @return vector of all occurrences lengths of each sequence and the index of each start
.find_a_number <- function(vector, specNumber){
  start_index <- Length <- vector()
  found <- FALSE
  count <- 0
  for(i in 1:length(vector)){
    if(vector[i] == specNumber & isTRUE(found)){
      count <- count + 1
      if(i == length(vector)){
        Length <- c(Length, count)
      }
    }
    else if(vector[i] == specNumber){
      start_index <- c(start_index, i)
      if(i == length(vector)){
        Length <- c(Length, 1)
      }
      else {
        count <- 1
        found <- TRUE
      }
    }
    else if(isTRUE(found)) {
      Length <- c(Length, count)
      found <- FALSE
    }
  }
  return(data.frame(StartIndex = start_index, RunLength = Length))
}



#' Top Envelope
#'
#' Top bound for a set of horizontal lines, follows contour of the top line and corrects for un-wanted noise
#'
#' @param rolledImage Image that has been put through the .roll_image()
#' @param max_roc maximum rate of change allowed between two pixels on the line before deemed as noise
#' @param sepDist how far you want the envelope to be above the line you are tracing
#' @param maxNoise the length of creating points alowed before considered to be off of the trace
#'
#' @return vector of points for a line, in the correct scaling for the rolled image.
#' can add to to change the scaling: nrow(imageMatrix) - bottomcut + Envelope
.top_env <- function(rolledImage, max_roc = 25, sepDist = 10, maxNoise = 100){

  topWhite <- apply(rolledImage, MARGIN = 2, FUN = function(x) {
    if (sum(x) == 0) {
      white <- 0
    }
    else {
      white <- min( which(x == 1) ) # actually the top of the image
    }
    return(white)
  })

  env <- .envelope_creation(rolledImage = rolledImage, chosenRoughBound = topWhite,
                            max_roc = max_roc, sepDist = sepDist, topOfLine = TRUE,
                            maxNoise = maxNoise)
  return(env)
}


#' Bottom of Top Trace Envelope
#'
#' Bottom bound for the top trace , follows contour of the top line and corrects
#'  for un-wanted noise
#'
#' @param rolledImage Image that has been put through the .roll_image()
#' @param max_roc maximum rate of change allowed between two pixels on the line before deemed as noise
#' @param sepDist how far you want the envelope to be below the line you are tracing
#' @param maxNoise the length of creating points alowed before considered to be off of the trace
#'
#' @return vector of points for a line, in the correct scaling for the rolled image.
#' can add to to change the scaling: nrow(imageMatrix) - bottomcut + Envelope
.top_lower_env <- function(rolledImage, max_roc = 25, sepDist = 10, maxNoise = 100){
  firstblack <- apply(rolledImage, MARGIN = 2, FUN = function(x) { #bottom of the first trace
    if (sum(x) == 0) {
      chosenBottom <- 0
    }
    else {
      minWhite <- min( which(x == 1) )
      black <- which(x == 0)
      possibleChoices <- which(black > minWhite)
      if (length(possibleChoices) != 0) {
        chosenBottom <- min(black[which(black > minWhite)])
      }
      else {
        chosenBottom <- 0
      }
    }

    return(chosenBottom)
  })
  env <- .envelope_creation(rolledImage = rolledImage, chosenRoughBound = firstblack,
                            max_roc = max_roc, sepDist = sepDist, topOfLine = FALSE,
                            maxNoise = maxNoise)
  return(env)
}



#' Top of Second Trace Envelope
#'
#' @param rolledImage Image that has been put through the .roll_image()
#' @param max_roc maximum rate of change allowed between two pixels on the line before deemed as noise
#' @param sepDist how far you want the envelope to be above the line you are tracing
#' @param maxNoise the length of creating points alowed before considered to be off of the trace
#'
#' @return vector of points for a line, in the correct scaling for the rolled image.
#' can do nrow(imageMatrix) - bottomcut + Envelope change the scaling
.bottom_upper_env <- function(rolledImage, max_roc = 25, sepDist = 10, maxNoise = 100){
  topSecondTrace <- apply(rolledImage, MARGIN = 2, FUN = function(x) {
    if (sum(x) == 0) {
      chosenBottom <- 0
    }
    else {
      maxWhite <- max( which(x == 1) )
      black <- which(x == 0)
      possibleChoices <- which(black < maxWhite)
      if (length(possibleChoices) == 0) {
        chosenBottom  <- 0
      }
      else {
      chosenBottom <- max(black[which(black < maxWhite)])
      }
    }
    return(chosenBottom)
  })
  env <- .envelope_creation(rolledImage = rolledImage, chosenRoughBound = topSecondTrace,
                            max_roc = max_roc, sepDist = sepDist, topOfLine = TRUE,
                            maxNoise = maxNoise)
  return(env)
}



#' Bottom Envelope
#'
#' Bottom bound for a set of horizontal lines, follows contour of the top line and corrects for un-wanted noise
#'
#' @param rolledImage Image that has been put through the .roll_image()
#' @param max_roc maximum rate of change allowed between two pixels on the line before deemed as noise
#' @param sepDist how far you want the envelope to be below the line you are tracing
#' @param maxNoise the length of creating points allowed before considered to be off of the trace
#'
#' @return vector of points for a line, in the correct scaling for the rolled image.
#' can do nrow(imageMatrix) - bottomcut + Envelope change the scaling
.bottom_env <- function(rolledImage, max_roc = 25, sepDist = 10, maxNoise = 100){
  bottomSecondTrace <- apply(rolledImage, MARGIN = 2, FUN = function(x) {
    if (sum(x) == 0) {
      white <- 0
    }
    else {
      white <- max( which(x == 1) )
    }
    return(white)
  })
  env <- .envelope_creation(rolledImage = rolledImage, chosenRoughBound = bottomSecondTrace,
                     max_roc = max_roc, sepDist = sepDist, topOfLine = FALSE,maxNoise = maxNoise )
  return(env)
}



#' Envelope Creation For a Trace
#'
#' Takes a rough est for the envelope and removes any unwanted noise
#'
#' @param rolledImage Image that has been put through the .roll_image()
#' @param chosenRoughBound vector of the rough bound (use apply on the columns)
#' @param max_roc maximum rate of change allowed between two pixels on the line before deemed as noise
#' @param sepDist how far you want the envelope to be below the line you are tracing
#' @param topOfLine bool, if you are tracing the top of the bottom of the line
#' @param maxNoise the length of creating points alowed before considered to be off of the trace
#'
#' @return corected bound for the envelope
.envelope_creation <- function(rolledImage, chosenRoughBound, max_roc = 25, sepDist = 20, topOfLine = TRUE, maxNoise = 100){
  newBound <- chosenRoughBound
  foundNonZero <- FALSE
  counterRight <- 0
  counterLeft <- 0

  #second half of image
  for (i in (round(ncol(rolledImage)/2)):ncol(rolledImage)) {
    x <- newBound[i]
    #first column or no nonZero column found yet
    if ( i == 1 || isFALSE(foundNonZero)) {
      if (newBound[i] != 0) {
        foundNonZero <- TRUE
      }
    }
    # a non zero column is found
    else if (isTRUE(foundNonZero)) {
      oneLess <- newBound[i - 1]
      diff <- x - oneLess
      if (abs(diff) >= max_roc) { # big change, could be a jump
        #browser()
        newBound[i] <- newBound[i - 1]
        counterRight <- counterRight + 1
      }
      else if (abs(diff) < max_roc & counterRight != 0) { # reset counter if back on the line
        #browser()
        counterRight = 0
      }
      if (counterRight == maxNoise) { # how long it can create new points before correcting
        #browser()
        newBound[i] <- chosenRoughBound[i] # brings back to the next point in the series
        for (k in 0:counterRight + 1) {
          #browser()
          newBound[i - k] <- chosenRoughBound[i]
        }
        counterRight <- 0
      }
    }
  }
  foundNonZero <- FALSE
  #first half of image

  for (j in 1:(round(ncol(rolledImage)/2 ))) {
    i <- (round(ncol(rolledImage)/2) + 1) - j
    x <- newBound[i]
    #first column or no nonZero column found yet
    if ( j == 1 || isFALSE(foundNonZero)) {
      if (newBound[i] != 0) {
        foundNonZero <- TRUE
      }
    }
    # a non zero column is found
    else if (isTRUE(foundNonZero)) {
      oneLess <- newBound[i + 1] # actually more because reverse indexing
      diff <-  oneLess - x # remember that the picture is reversed as well, 0 is the top..
      if (abs(diff) >= max_roc) { # big change, could be a jump
       # browser()
        newBound[i] <- newBound[i + 1]
        counterLeft <- counterLeft + 1
      }
      else if (abs(diff) < max_roc & counterLeft != 0) { # reset counter if back on the line
        #rowser()
        counterLeft = 0
      }
      if (counterLeft == maxNoise) { # how long it can create new points before correcting
        #browser()
        newBound[i] <- chosenRoughBound[i] # brings back to the next point in the series
        for (k in 0:counterLeft + 1) {
          #browser()
          newBound[i + k] <- chosenRoughBound[i]
        }
        counterLeft <- 0
      }
    }
  }
  if (isTRUE(topOfLine)) {
    correctedWhite <- nrow(rolledImage) - newBound + sepDist
  }
  else{# running on the bottom of the image
    correctedWhite <- nrow(rolledImage) - newBound - sepDist
  }
  return(correctedWhite)
}



#' Isolating Traces
#'
#' Takes two envelopes and sets all other pixels to 0 (black)
#'
#' NOTE: topEnv is less then bottomEnv because indexing 0 is at top of image for matrix scaled
#'
#' @param imageMatrix The processed Image matrix with import_process_image()
#' @param topEnv Upper envelope for one trace (scaled to your matrix correctly)
#' @param bottomEnv Lower envelope for one trace (scaled to your matrix correctly)
#'
#' @return matrix of the one trace on black(NOTE: still in origional spot on plot)
.isolating_trace <- function(imageMatrix, topEnv, bottomEnv){
  if (ncol(imageMatrix) != length(topEnv) | ncol(imageMatrix) != length(bottomEnv)) {
    stop("ImageMatrix length differs from top or bottom env")
  }
  n <- nrow(imageMatrix)
  for (j in 1:ncol(imageMatrix)) {
    imageMatrix[c(0:(round(topEnv[j], digits = 0))),j] <- 0
    imageMatrix[c((round(bottomEnv[j], digits = 0)):n),j] <- 0
  }
  return(imageMatrix)
}



#' Get Year From Image Name
#'
#' @param imageName a string
#'
#' @return Year
.get_image_year <- function(imageName){
  splitName <- strsplit(imageName, "-")
  date <- splitName[[1]][length(splitName[[1]]) - 1]
  yearVector <- strsplit(date,"")[[1]][1:4]
  year <- paste0(yearVector[1], yearVector[2], yearVector[3], yearVector[4])
  return(year)
}



#' Directory Structure Creation of image year
#'
#' Creates the directory structure for the digitized images along with setting with
#' pwd for that specific image, if that year doesn't exist, it will create one
#'
#' @param imageName A string of the image name
#' @param pathToWorkingDir Where the digitized images will be put
#'
#' @return The pwd for that specific image
.dir_Str <- function(imageName, pathToWorkingDir){
  year <- .get_image_year(imageName)
  if (dir.exists(paste0(pathToWorkingDir, "/", year, "/"))) {
    pwd <- paste0(pathToWorkingDir, "/", year, "/")
    return(pwd)
  }
  else {
    dir.create(paste0(pathToWorkingDir, "/", year, "/"))
    dir.create(paste0(pathToWorkingDir, "/", year, "/", "FailedToProcess/"))
    pwd <- paste0(pathToWorkingDir, "/", year, "/")
    return(pwd)
  }
}

#' Directory Structure Creation of image name
#'
#' Creates the directory structure for the digitized images along with setting with
#' pwd for that specific image, if a dir doesn't exist for the image name, it will create one
#'
#' @param imageName A string of the image name
#' @param pathToWorkingDir Where the digitized images will be put
#'
#' @return The pwd for that specific image
.dir_Str_Improvement <- function(imageName, pathToWorkingDir){
  if (dir.exists(paste0(pathToWorkingDir, "/", imageName, "/"))) {
    pwd <- paste0(pathToWorkingDir, "/", imageName, "/")
    return(pwd)
  }
  else {
    dir.create(paste0(pathToWorkingDir, "/", imageName, "/"))
    dir.create(paste0(pathToWorkingDir, "/", imageName, "/", "FailedToProcess/"))
    pwd <- paste0(pathToWorkingDir, "/", imageName, "/")
    return(pwd)
  }
}


#' envelope gap filling for TISI
#'
#' @param x vector of x values
#' @param y vector of y values
#' @param nCol the length out that you want
#'
#' @return data.frame of x and y values
.envelopegapfiller <- function(x, y, nCol = NULL) {
  sorting <- stats::sortedXyData(x,y) # sorts by x just in case user clicks the wrong way
  x <- sorting$x
  y <- sorting$y
  duplicates <- duplicated(x)
  x <- x[!duplicates]
  y <- y[!duplicates]
  if (x[1] != 0) { # ensure that we are at the correct starting height
    x <- c(0,x) # creates first point
    y <- c(y[1],y) # matches height of user selected first point
  }
  else {
    y[1] <- y[2]
  }

  diffx <- diff(x)
  diffy <- diff(y)
  slopes <- diffy / diffx # slope of the y = mx+b line between each points

  xNumFills <- diff(x) - 1 # how many places we have to fill
  if(length(which(xNumFills < 0)) >= 1){
    warning("there is one or more numbers that are not in the correct accending order ...")
  }

  # Extending the points to make continuous ------------------------------------
  newX <- vector()
  newY <- vector()
  for (i in 1:(length(x) - 1)) {
    if (xNumFills[i] != 0) {
      newX <- append(newX, x[i])
      newY <- append(newY, y[i])
      newX <- append(newX, rep(NA, times = xNumFills[i]))
      newY <- append(newY, rep(NA, times = xNumFills[i]))
    }
    else {
      newX <- append(newX, x[i])
      newY <- append(newY, y[i])
    }

  }
  newX <- append(newX, x[length(x)])
  newY <- append(newY, y[length(x)])

  #adding the numbers in the correct place for the X values --------------------
  for (i in 1:length(newX)) {
    if (is.na(newX[i])) {
      newX[i] = newX[i - 1] + 1
    }
  }

  #Matching the length of the nCol specified -----------------------------------

  if (!is.null(nCol)) {
    if (newX[length(newX)] > nCol) {
      outX <- newX[1:(nCol + 1)] # just removes any data outside of the end length
      outY <- newY[1:(nCol + 1)]
    }
    else {
      xAdditions <- seq(from = (newX[length(newX)] + 1), to = nCol)
      outX <- c(newX, xAdditions)
      outY <- c(newY, rep(NA, length = length(xAdditions)))
    }
  }
  else{
    outX <- newX
    outY <- newY
  }

  # Start of adding the heights from the slope ---------------------------------
  k <- 0
  indicator = FALSE
  for (i in 1:(length(outY) - sum(xNumFills) + xNumFills[length(xNumFills)])) {
    if (i == length(xNumFills)) {
      k <- k + 1
      intersept <- outY[k] - (outX[k]*slopes[length(xNumFills)])
      next
    }
    else if (i > length(xNumFills)) {
      k <- k + 1
      if (!indicator) {
        outY[k] <- outX[k] * slopes[length(xNumFills)] +
          intersept
        indicator <- TRUE
      }else{
        outY[k] <- outY[k - 1]
      }
    }
    else if (xNumFills[i] == 0) {
      k <- k + 1
      intersept <- outY[k] - (outX[k] * slopes[i])
      next
    }
    else{
      for (j in 1:(xNumFills[i] + 1 )) {
        k <- k + 1
        if (j == 1) {
          intersept <- outY[k] - (outX[k] * slopes[i])
        }
        else {
          outY[k] <- outX[k]*slopes[i] + intersept
        }
      }
    }
  }
  #compensates for the x = 0 being the first element
  outX <- outX[-length(outX)]
  outY <- outY[-length(outY)]


  return(data.frame(x = outX, y = outY))

}
