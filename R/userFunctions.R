#'Importing Tiff Files
#'
#'Reads tiff also checks for if the image is a .tif or .tiff using .is_tiff()
#'
#'
#'@param fileName The image name
#'@param fileLoc the path from ~/ to the dir where the file is
#'@export

tiff_import <- function(fileName,fileLoc){
  if (!is.character(fileName) || length(fileName) != 1) {
    return(stop("fileName must be a single character"))
  }
  if (!is.character(fileLoc) || length(fileLoc) != 1) {
    return(stop("fileLoc must be a single character"))
  }
  .null(fileName, "fileName")
  .null(fileLoc, "fileLoc")

  tiffCheck <- .is_tiff(fileName)
  notEmpty <- not_empty_file(filePath = fileLoc, fileName = fileName)
  if (isTRUE(tiffCheck) & isTRUE(notEmpty)) {
    return(tiff::readTIFF(paste0(fileLoc,"/",fileName)))
  }
  else if (!isTRUE(tiffCheck)) {
    return(stop(paste0("file", fileName, " is not a .tif or .tiff" )))
  }
  else if (!isTRUE(notEmpty)) {
    return(stop(paste0("file", fileName, " is 0b/Empty")))
  }

}


#'Checking For Bright Images
#'
#'Using a logistic regression to find coefficients, uses the result to compute a probability
#'of the imageMatrix being #'bright (392 observations) with McFadden Pseudo- $R^2$ Value 0.2573778
#'
#'@param imageMatrix  imageMatrix points representing the brightness of that pixel
#'@param cutoffProbability The probability cut off for the decision of an imageMatrix being bright
#'@param beta0 The intercept of a logistic regression default is for magnetograms
#'@param beta1 The slope of the logistic regression default is for magnetograms
#'@param NADefault The default value set to NA's in the matrix
#'@return The decision of the image being bright (based off of the cutoffProbability)
#'@export

bright <- function(imageMatrix, beta0 = -2.774327, beta1 = 51.91687, cutoffProbability = 0.5, NADefault = 0){
  if (!is.matrix(imageMatrix)) {
    return(stop("imageMatrix must be a matrix"))
  }
  na <- which(is.na(imageMatrix))
  if (length(na) > 0) {
    warning(paste0("NA's found in the matrix.. setting them to ", NADefault))
    imageMatrix[na] <- NADefault
  }
  if (length(which(imageMatrix <= 1 & imageMatrix >= 0)) != length(imageMatrix)) {
    warning("Shouldnt be pixels with brightness above 1...scaling by largest value and
            taking abs value")
    imageMatrix <- abs(imageMatrix)/max(abs(imageMatrix))
  }


  .null(imageMatrix, "imageMatrix")

  aboveLen <- length(which(imageMatrix >= 0.80))
  totalLen <- length(imageMatrix)
  standardizedLen <- aboveLen/totalLen
  decision <- exp(beta0 + beta1 * standardizedLen)/(1 + exp(beta0 + beta1 * standardizedLen))

  if (decision >= cutoffProbability) {
    bright = TRUE
  }
  else {
    bright = FALSE
  }
  return(bright)
}




#' Finding Peaks for Image
#'
#' Finds peaks in the row sums of an image matrix, used for finding starts of lines
#' that are horizontal
#'
#' @param rowSums of the imported matrix from original picture
#' @param minDistance Minimum distance aloud between found peaks
#' @param maxPeakNumber Maximum peaks you are trying to find
#' @param percentFromEdge Percentage of the picture that you would like removed from edge
#' due to an over exposed picture or flares in scanning
#' if percentageLeftSide is unspecified uses percentage for both i.e left = 1 - right
#' @param percentEdgeForLeft the left percentage that you want to remove(default = NULL)
#' @param minPeakHeight minimum peak height required for a peak to be counted
#' Default is 5perc of the max(rowSums)
#' @param plots Plots the peaks on rowSums
#' @param StartEndplotLine If you want to see lines at start and ends of each peaks
#' (starts are green, ends are blue)
#'
#' @return data frame of peaks, peak heights, starts and ends for each peak
find_peaks <- function(rowSums, minDistance, maxPeakNumber, percentFromEdge, percentEdgeForLeft = NULL,
                       minPeakHeight = (0.05*max(rowSums)), plots = TRUE, StartEndplotLine = TRUE) {
  if (!is.vector(rowSums, mode = "numeric")) {
    return(stop("rowSums must be a vector"))
  }
  #python code
  scipy <- reticulate::import("scipy")
  peaks <- scipy$signal$find_peaks(x = rowSums, height = minPeakHeight, distance = minDistance)
  #end of python code
  #reticulate::source_python(file = system.file("py", package = packageName()))
  #peaks <- finding_peakspy(rowSums, minPeakHeight, minDistance)
  peaks[[1]] <- peaks[[1]] + 1 # python index correction
  peakInfo = data.frame(Index = peaks[[1]], Height = peaks[[2]]$peak_heights)
  if (dim(peakInfo)[1] == 0) {
    return(stop("No Peaks Found..."))
  }

  peaksNoEdge <- .edge_peaks_rm(peakInfo, rowSums, percentFromEdge, percentEdgeForLeft = percentEdgeForLeft)
  foundPeaks <- .highest_peaks(peaksNoEdge, maxPeaksAllowed = maxPeakNumber)


  ret <- .finding_Peak_Start_Ends(foundPeaks, rowSums)
  if (plots == TRUE) {
  .plot_peaks(ret, rowSums, StartEndLine = StartEndplotLine)
  }
  return(ret[order(ret$Index),])

}


#'Searching For Keyword
#'
#' Finds all files that contain a specific string, in this case for finding files
#' that contain a specific year. Default is to also look in symlinks for files, if a dir is not accessable
#' will return an warning to the user
#'
#' @param path Where you would like the function to look.
#' @param keyword The word that you are trying to find in file names.
#' @return  Vector with all paths for each item found with the keyword contained in it.
#' @export
find_paths_for_keyword <- function(path = "~/", keyword){
  word = paste0("*", keyword, "*")
  spath = vector()
  symlinks <- as.character(fs::dir_ls(path = path , recurse = TRUE, type = "symlink"))
  if (!identical(symlinks, character(0))) { #The character 0 is what is displayed if no symlinks show up
    symL <- length(symlinks)
    for (i in 1:symL) {
      spath <- c(spath, as.character(fs::dir_ls(path = symlinks[i], glob = word, recurse = TRUE, type = "file", fail = FALSE)))
    }

  }
  pathWithKeyword  <- fs::dir_ls(path = path, glob = word, recurse = TRUE, type = "any")
  allPathWithKeyword <- c(spath, pathWithKeyword)
  if (length(allPathWithKeyword) == 0) {
    return(stop("No files found with that name"))
  }
  else{
    return(as.character(allPathWithKeyword))
  }
}



#' Import and Process a .tiff or .tif image
#'
#' @param imageName The name of the file
#' @param file_loc the path from ~/ to the dir where the file is
#' @param trimAmountTop Number of pixels off of top of image (usually for common flares)
#' @param trimAmountBottom Number of pixels off of bottom of image
#' @param beta0 The intercept of a logistic regression default is for magnetograms
#' @param beta1 The slope of the logistic regression default is for magnetograms
#' @param cutoffProbability The probability cut off for the decision of an imageMatrix being bright
#' @param NADefault The default value set to NA's found in the matrix
#' @param FilterBright Vector specifying the dimensions of the kernel,
#' which will be used to perform either delation or erosion, such as c(13,13)
#' @param FilterNonBright Vector specifying the dimensions of the kernel,
#' which will be used to perform either delation or erosion, such as c(8,8)
#' @param methodBright one of 'delation'(adds to image, making brights brighter), 'erosion' (subtracts from image brights darker)
#' @param methodNonBright one of 'delation'(adds to image, making brights brighter), 'erosion' (subtracts from image brights darker)
#' @param thresholdBright should be between 0 and 1 for normalized images Default = 0.8
#' @param thresholdNonBright should be between 0 and 1 for normalized images Default = 0.5
#'
#' @return imageMatrix processed and trimed, in landscape orientation
#' @export
import_process_image <- function(imageName, file_loc, trimAmountTop = 100,
                                 trimAmountBottom = 50, beta0 = -2.774327,
                                 beta1 = 51.91687, cutoffProbability = 0.5,
                                 NADefault = 0, FilterBright = c(13, 13),
                                 FilterNonBright = c(8, 8),
                                 methodBright = "delation",
                                 methodNonBright = "delation",
                                 thresholdBright = 0.8,
                                 thresholdNonBright = 0.5){


  imageRAW <- tiff_import(fileName = imageName, fileLoc = file_loc)
  image <- .horizontal_image_check(imageRAW)
  imagecut <- trim_top_bottom(image,
                               trimAmountTop = 100,
                               trimAmountBottom = 50) #takes off the usual flair spots
  imageMatrix <- .process_image(imagecut,
                                cutoffProbability = cutoffProbability,
                                NADefault = NADefault,
                                beta1 = beta1, beta0 = beta0,
                                FilterBright = FilterBright,
                                FilterNonBright = FilterNonBright,
                                methodBright = methodBright,
                                methodNonBright = methodNonBright,
                                thresholdBright = thresholdBright,
                                thresholdNonBright = thresholdNonBright) # checks bright and processes returns processed
  return(imageMatrix)
}



#' Image Blurring with Rollmean
#'
#' uses rollMean from zoo to blurr image ( used to get rid of timing gaps)
#'
#' @param imageMatrix the image matrix from import_process_image
#' @param topcut Top cutoff from .top_cut()
#' @param bottomcut Bottom cutoff from .bottom_cut()
#' @param fill Default "extend" see rollMean in zoo for details
#' @param k See rollMean() in zoo for details
#'
#' @return rolled image matrix to user
#' @export
mean_roll_image <- function(imageMatrix, topcut, bottomcut, fill = "extend", k = 40){
  imageWithoutTopBottom <- imageMatrix[-c(0:topcut, bottomcut:nrow(imageMatrix)), ]
  vert <- t(imageWithoutTopBottom)
  rolledImage <- t(zoo::rollmean(vert, k = k, fill = fill))
  rolledImage[which(rolledImage != 0)] <- 1
  return(rolledImage)
}




#' Finding Envelopes for Two Traces
#'
#' @param imageMatrix ImageMatrix from import_process_image()
#' @param returnType Either "MatrixScaled" used if you are manipulating with imageMatrix,
#' "PlottingScaled" used if you are plotting the imageMatrix with overlay of these lines
#' "RolledImageScaled" used if you are overlaying the rolled image with these lines
#' @param max_roc maximum rate of change allowed between two pixels on the line before deemed as noise
#' @param sepDist how far you want the envelope to be below the line you are tracing
#' @param maxNoise the length of creating points allowed before considered to be off of the trace
#' @param bottomCut a point that is the height of a horizontal line between the timing marks
#' and the bottom trace
#' @param rolledImage the image rolled by mean_roll_image()
#' @param improveTTopEnvelope data.frame of col x and col y representing points for top of the top envelope pair
#' doesn't need to be a point for each column in original picture, will fill in the blanks
#' @param improveBTopEnvelope data.frame of col x and col y representing points for Bottom of the top envelope pair
#' doesn't need to be a point for each column in original picture, will fill in the blanks
#' @param improveTBottomEnvelope data.frame of col x and col y representing points for top of the bottom envelope pair
#' doesn't need to be a point for each column in original picture, will fill in the blanks
#' @param improveBBottomEnvelope data.frame of col x and col y representing points for bottom of the bottom envelope pair
#' doesn't need to be a point for each column in original picture, will fill in the blanks
#' @param topCut Top cutoff between the date writing and the top trace
#' @param trimTop how much has been trimmed by TISI or TIS
#' @param trimBottom  how much has been trimmed by TISI or TIS
#'
#' @return list of all four envelopes
#' @export
find_envelopes <- function(imageMatrix, rolledImage, bottomCut, topCut,  returnType,
                           sepDist = 10, max_roc = 25, maxNoise = 100,
                           improveTTopEnvelope = data.frame(x = NA, y = NA),
                           improveBTopEnvelope = data.frame(x = NA, y = NA),
                           improveTBottomEnvelope = data.frame(x = NA, y = NA),
                           improveBBottomEnvelope = data.frame(x = NA, y = NA),
                           trimTop = 0, trimBottom = 0){

  # Top of Top Envelope
  compensationFactor <- -1 *(nrow(imageMatrix) - bottomCut) - (trimBottom*2)
  if (!is.na(improveTTopEnvelope$y[1])) {
    improveTTopEnvelope$y <- improveTTopEnvelope$y + compensationFactor#- nrow(imageMatrix) + bottomCut - topCut - 50 # - (trimTop *2) - (trimBottom * 2)  -  compensation factor that was found
    topEnv <- .envelopegapfiller(x = improveTTopEnvelope$x,
                                y = improveTTopEnvelope$y,
                                nCol = ncol(rolledImage))$y

  }
  else{
  topEnv <- .top_env(rolledImage = rolledImage, max_roc = max_roc,
                     sepDist = sepDist, maxNoise = maxNoise)
  }

  # Bottom of Top Envelope
  if (!is.na(improveBTopEnvelope$y[1])) {
    improveBTopEnvelope$y <- improveBTopEnvelope$y + compensationFactor#- nrow(imageMatrix) + bottomCut - topCut - 50 #- (trimTop *2) - (trimBottom * 2) compensation factor that was found
    topLowerEnv <- .envelopegapfiller(x = improveBTopEnvelope$x,
                                y = improveBTopEnvelope$y,
                                nCol = ncol(rolledImage))$y

  }
  else{
  topLowerEnv <- .top_lower_env(rolledImage = rolledImage, max_roc = max_roc,
                                sepDist = sepDist, maxNoise = maxNoise)
  }

  # Top of Bottom Envelope
  if (!is.na(improveTBottomEnvelope$y[1])) {
    improveTBottomEnvelope$y <-  improveTBottomEnvelope$y + compensationFactor#- nrow(imageMatrix) + bottomCut - topCut - 50 # - (trimTop *2) - (trimBottom * 2) compensation factor that was found
    bottomUpperEnv <- .envelopegapfiller(x = improveTBottomEnvelope$x,
                                y = improveTBottomEnvelope$y,
                                nCol = ncol(rolledImage))$y
  }
  else{
  bottomUpperEnv <- .bottom_upper_env(rolledImage = rolledImage, max_roc = max_roc,
                                      sepDist = sepDist, maxNoise = maxNoise)
  }

  # Bottom of Bottom Envelope
  if (!is.na(improveBBottomEnvelope$y[1])) {
    improveBBottomEnvelope$y <- improveBBottomEnvelope$y + compensationFactor#- nrow(imageMatrix) + bottomCut - topCut - 50 #compensation factor that was found
    bottomEnv <- .envelopegapfiller(x = improveBBottomEnvelope$x,
                                   y = improveBBottomEnvelope$y,
                                   nCol = ncol(rolledImage))$y
  }
  else{
  bottomEnv <- .bottom_env(rolledImage = rolledImage, max_roc = max_roc,
                           sepDist = sepDist, maxNoise = maxNoise)
  }

  if (returnType == "MatrixScaled") {
    topEnvelope <- bottomCut - topEnv
    topLowerEnvelope <- bottomCut - topLowerEnv
    bottomUpperEnvelope <- bottomCut - bottomUpperEnv
    bottomEnvelope <- bottomCut - bottomEnv
  }
  else if (returnType == "PlottingScaled") {
    topEnvelope <- nrow(imageMatrix) - bottomCut + topEnv
    topLowerEnvelope <- nrow(imageMatrix) - bottomCut + topLowerEnv
    bottomUpperEnvelope <- nrow(imageMatrix) - bottomCut + bottomUpperEnv
    bottomEnvelope <- nrow(imageMatrix) - bottomCut + bottomEnv
  }
  else if (returnType == "RolledImageScaled") {
    topEnvelope <- topEnv
    topLowerEnvelope <- topLowerEnv
    bottomUpperEnvelope <- bottomUpperEnv
    bottomEnvelope <- bottomEnv
  }
  else {
    stop("returnType is not correct, please look at documentation,
         type can be RolledImageScaled, PlottingScaled, or MatrixScaled")
  }
  return(list(TopEnvelope = topEnvelope,
              TopLowerEnvelope = topLowerEnvelope,
              BottomUpperEnvelope = bottomUpperEnvelope,
              BottomEnvelope = bottomEnvelope))
}



#' Create Trace
#'
#' Takes a top and bottom trace on an isolated image and creates line, with MA smoothing
#'
#' @param traceMatrix The matrix of an isolated trace
#' @param start Start line for the trace
#' @param end End line for the trace
#' @param topEnv The top envelope for the isolated trace Matrix
#' @param bottomEnv The bottom envelope for the isolated trace Matrix
#' @param thresh The threshold for the diff() as a cut off for allowed spikes
#' @param MARange The amount in each direction that the moving average will look at (added to the region value)
#' @param region The region that is smoothed by the moving average
#' @param loopNumber How many times it will go through this process, (to catch the new peaks from smoothing)
#'
#' @return Matrix of the line, corrected for the matrix
#' @export
create_trace <- function(traceMatrix, start, end, topEnv, bottomEnv, thresh = 5, MARange = 6, region = 2, loopNumber = 4){
  traceLine <- vector()
  for (i in start:end) {
    column <- traceMatrix[,i]
    trace <- which(column == 1)
    if (length(trace) > 0) {
      top <- trace[1]
      bottom <- trace[length(trace)]
      middleOfTrace <- round((top + bottom) / 2)
    }
    else {
      middleOfTrace <- round((topEnv[i] + bottomEnv[i]) / 2)
    }
    traceLine <- append(traceLine, middleOfTrace)
  }
  for (j in 1:loopNumber) {
    jumpsUp <- which(abs(diff(traceLine)) >= thresh) # to catch the spikes
    if (length(jumpsUp) > 0 ) {

      jumpsUp <- jumpsUp + 1 # correction so we land on the jumps not the one before the jump
      for (i in jumpsUp) {
        if (i < MARange + region + 1) {
          traceLine[i] <- mean(traceLine[0:(i + MARange)]) #MA smoothing
        }
        else if ((i + MARange + region) > length(traceLine)) {
          traceLine[i] <- mean(traceLine[(i - MARange):length(traceLine)]) #MA smoothing
        }
        else{
          traceLine[(i - region):(i + region)] <- mean(traceLine[(i - region - MARange):(i + region + MARange)]) #MA smoothing on the region
        }
      }
    }
  }

  return(nrow(traceMatrix) - traceLine) # no jumps, just returning the line no corrections


}



#' Find Cut Positions
#'
#' Attempts to find a straight line between the timing marks and the traces(.bottom_image_cut)
#' and a straight line between the traces and the writing at the top of the images(.top_image_cut)
#'
#' @param imageMatrix imported with import_process_image() or an equivilent
#' @param cutPercentage A bound for which the start and end is never found, could be a flare
#' on the sides of the scanned image
#' @param percentFromEdge used in find_peaks if you know there wont be a peak
#'  in a region
#' @param percentEdgeForLeft passed into find peaks, if not specified (NULL), uses
#' percentFromEdge for both left and right sides, if specified, percentFromEdge
#' is defaulted to just the right side of the plot
#' @param shortestAllowedSeqOfZeros smallest gap allowed to be found to consider
#' the trace not intersecting the timing marks in pixels
#'
#' @return list of the top and bottom cuts, warnings are returned if exist
#' @export
find_cuts <- function(imageMatrix, cutPercentage = 2, percentFromEdge = 2,
                      percentEdgeForLeft = NULL, shortestAllowedSeqOfZeros = 25){

  imageSides <- .get_trace_start_ends(imageMatrix, returnMat = FALSE,
                                      cutPercentage = cutPercentage) # two vertical lines for top and bottom est

  imageWithoutSides <- imageMatrix[, -c(0:imageSides$Start, #takes away the sides found above
                                        imageSides$End:ncol(imageMatrix))]
  # finds top horizontal line
  topCut <- tryCatch(.top_image_cut(imageMatrix = imageWithoutSides, percentFromEdge = percentFromEdge,
                                    percentEdgeForLeft = percentEdgeForLeft), warning = function(w) w)
  #finds bottom horizontal line
  bottomCut <- tryCatch(.bottom_image_cut(imageMatrix = imageWithoutSides,
                                          percentEdgeForLeft = percentEdgeForLeft,
                                          percentFromEdge = percentFromEdge,
                                          shortestAllowedSeqOfZeros = shortestAllowedSeqOfZeros),
                        warning = function(w) w)

  return(list(TopCut = topCut, BottomCut = bottomCut )) # warnings are caught one level above
}



#' Isolation of Two Traces
#'
#' Uses envelopes to create two different matrices with one trace on each
#'
#' NOTE: make sure that your envelopes are scaled to your matrix
#'
#' @param imageMatrix The processed Image matrix with import_process_image()
#' and trimmed if applicable
#' @param topLowerEnvelope Bottom envelope for the top trace (scaled to your matrix correctly)
#' @param topEnvelope Top envelope for the top trace (scaled to your matrix correctly)
#' @param bottomEnvelope Bottom envelope for the bottom trace (scaled to your matrix correctly)
#' @param bottomUpperEnvelope The top envelope for the second trace (scaled to your matrix correctly)
#'
#' @return list of both isolated trace matrices
#' @export
isolate_traces <- function(imageMatrix, topEnvelope, topLowerEnvelope,
                           bottomUpperEnvelope, bottomEnvelope){
  topTraceMatrix <- .isolating_trace(imageMatrix, topEnvelope,
                                     topLowerEnvelope)
  bottomTraceMatrix <- .isolating_trace(imageMatrix, bottomUpperEnvelope,
                                        bottomEnvelope)
  return(list(TopTraceMatrix = topTraceMatrix, BottomTraceMatrix = bottomTraceMatrix))
}



#' Plotting Successful Digitization
#'
#' @param imageMatrix Matrix for a certain image (scaled)
#' @param rolledImage Using mean_roll_image
#' @param topCut A point which indicates a horizontal line between the top text
#' and the first trace
#' @param bottomCut A point which indicates a horizontal line between the bottom trace
#' and the timing marks (if a line can be found)
#' @param topStartEnds Top trace start and ends for isolated trace
#' @param bottomStartEnds Bototm trace start and ends for isolated trace
#' @param topTrace Top trace matrix for the isolated trace
#' @param bottomTrace Bottom trace matrix for the isolated trace
#' @param max_roc Maximum rate of change allowed between two pixels on the line before deemed as noisee
#' @param sepDist How far you want the envelope to be below the line you are tracing
#' @param maxNoise The length of creating points allowed before considered to be off of the trace
#' @param pathToWorkingDir Which directory you want the image to end up in
#' @param imageName The name of the image (usually the file name)
#' @param plotEnv The envelope that you would like to put onto the plot
#' @param intersectionFlag If intersection is known to user, or is found with TISI or TIS,
#' will not plot full, will consider it to be a failed process plot
#'
#' @return void
#' @export
plot_success <- function(imageMatrix, rolledImage, topCut, bottomCut, topStartEnds,
                        bottomStartEnds, topTrace, bottomTrace,  plotEnv, maxNoise = 250,
                        max_roc = 35, sepDist = 10, pathToWorkingDir = "~/", imageName,
                        intersectionFlag = FALSE){
  #Adjusting the length of the top and bottom trace to be the same as the imageMatrix
  startTop <- vector()
  startTop[0:(topStartEnds$Start - 1)] <- topTrace[1]
  endTop <- vector()
  endTop[0:length((topStartEnds$End + 1):ncol(imageMatrix))] <- topTrace[length(topTrace)]

  startBottom <- vector()
  startBottom[0:(bottomStartEnds$Start - 1)] <- bottomTrace[1]
  endBottom <- vector()
  endBottom[0:length((bottomStartEnds$End + 1):ncol(imageMatrix))] <- bottomTrace[length(bottomTrace)]

  newTopTrace <- c(startTop, topTrace, endTop)
  newBottomTrace <- c(startBottom, bottomTrace, endBottom)



  #Scaling the envelopes for plotting Not sure why I put this in here...
  # plotEnvelopes <- find_envelopes(rolledImage = rolledImage, imageMatrix = imageMatrix,
  #                                 bottomCut = bottomCut, returnType = "PlottingScaled",
  #                                 maxNoise = maxNoise, max_roc = max_roc, sepDist = sepDist)
  plotEnvelopes <- plotEnv
  if (isFALSE(intersectionFlag)) {
    datePieces <- strsplit(as.character(Sys.time()), split = "-")
    timePieces <- strsplit(datePieces[[1]][3], ":")
    spaceRemove <- strsplit(timePieces[[1]][1], " ")
    time <- as.character(paste0(datePieces[[1]][1], datePieces[[1]][2],
                                spaceRemove[[1]][1], spaceRemove[[1]][2],
                                timePieces[[1]][2], timePieces[[1]][3]))
    grDevices::png(paste0(pathToWorkingDir, imageName,"-",time,"-plot", ".png"))
  }else{# is an intersection so we consider it a fail process
    datePieces <- strsplit(as.character(Sys.time()), split = "-")
    timePieces <- strsplit(datePieces[[1]][3], ":")
    spaceRemove <- strsplit(timePieces[[1]][1], " ")
    time <- as.character(paste0(datePieces[[1]][1], datePieces[[1]][2],
                                 spaceRemove[[1]][1], spaceRemove[[1]][2],
                                 timePieces[[1]][2], timePieces[[1]][3]))
    grDevices::png(paste0(pathToWorkingDir, imageName,"-",time ,"-FailToProcess", "-plot", ".png"))
  }
  suppressWarnings(magneto::plot.matrix(imageMatrix))
  graphics::lines(plotEnvelopes$TopEnvelope, col = "green")
  graphics::lines(plotEnvelopes$TopLowerEnvelope, col = "yellow")
  graphics::lines(plotEnvelopes$BottomUpperEnvelope, col = "green")
  graphics::lines(plotEnvelopes$BottomEnvelope, col = "yellow")
  graphics::lines(newTopTrace, col = "red")
  graphics::lines(newBottomTrace, col = "red")
  graphics::abline(h = (nrow(imageMatrix) - topCut), col = "red")
  graphics::abline(h = (nrow(imageMatrix) - bottomCut), col = "red")
  graphics::abline(v = topStartEnds, col = "green")
  graphics::abline(v = bottomStartEnds, col = "orange")
  grDevices::dev.off() #close png file

  return(time)
}



#' Plot for Fail Processed Images
#'
#' @param imageMatrix Matrix for an image(scaled)
#' @param topCut A point which indicates a horizontal line between the top text
#' and the first trace
#' @param bottomCut A point which indicates a horizontal line between the bottom trace
#' and the timing marks (if a line can be found)
#' @param pathToWorkingDir Which directory you want the image to end up in
#' @param imageName The file name of the image
#'
#' @return void
#' @export
plot_with_warnings <- function(imageMatrix, topCut, bottomCut, pathToWorkingDir,
                               imageName){
  #ifelse(!dir.exists( file.path(pathToWorkingDir, "FailedToProcess")), dir.create( file.path(pathToWorkingDir, "FailedToProcess")), FALSE)
  #newPathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
  # 2. Create a plot
  datePieces <- strsplit(as.character(Sys.time()), split = "-")
  timePieces <- strsplit(datePieces[[1]][3], ":")
  spaceRemove <- strsplit(timePieces[[1]][1], " ")
  time <- as.character(paste0(datePieces[[1]][1], datePieces[[1]][2],
                              spaceRemove[[1]][1], spaceRemove[[1]][2],
                              timePieces[[1]][2], timePieces[[1]][3]))
  grDevices::png(paste0(pathToWorkingDir, imageName, "-" ,time,  "-FailToProcess-Plot",".png"))
  suppressWarnings(magneto::plot.matrix(imageMatrix))
  graphics::abline(h = (nrow(imageMatrix) - topCut), col = "green")
  graphics::abline(h = (nrow(imageMatrix) - bottomCut), col = "green")
  graphics::text("This Isn't Processed", x = 3000, y = 1500, col = "orange")
  # Close the png file
  grDevices::dev.off()

  return(time)
}



#' Rough Trim for Sides
#'
#' trims the right and left sides if the user knows where there will definitely not be any points
#'
#' @param trimAmountLeft in percentage
#' @param trimAmountRight in percentage
#' @param image A matrix
#'
#' @return the matrix without the specified bounds
#' @export
trim_sides <- function(image, trimAmountLeft = 2, trimAmountRight = 2){
  leftPerc <- trimAmountLeft/100
  rightPerc <- trimAmountRight/100
  if (leftPerc > 1 || rightPerc > 1) {
    return(stop("The percentage cant be over 100"))
  }
  if (leftPerc != 0 & rightPerc != 0) {
    imageProcessed <- image[,-c(1:round((leftPerc*ncol(image))), (ncol(image) + 1 - round(rightPerc*ncol(image))):ncol(image))]
  }
  else if (leftPerc != 0) {
    imageProcessed <- image[,-c(1:round((leftPerc*ncol(image))))]
  }
  else if (rightPerc != 0) {
    imageProcessed <- image[,-c((ncol(image) + 1 - round(rightPerc*ncol(image))):ncol(image))]
  }
  return(imageProcessed)
}



#' Trim on top and bottom of the image in pixels for consistency
#'
#' trims  the top and bottom if the user knows where there will definitely not be any points
#'
#' @param image A matrix
#' @param trimAmountTop Number of pixels
#' @param trimAmountBottom Number of pixels
#'
#' @return the matrix without the specified bounds
#' @export
trim_top_bottom <- function(image, trimAmountTop = 100, trimAmountBottom = 50){
  if (trimAmountTop + trimAmountBottom > nrow(image)) {
    return(stop("You have trimmed all the image, peakse reduce trim amounts"))
  }
  if (trimAmountTop != 0 & trimAmountBottom != 0) {
    imageProcessed <- image[-c(1:trimAmountTop, (nrow(image) - trimAmountBottom + 1):nrow(image)),]
  }
  else if (trimAmountTop != 0) {
    imageProcessed <- image[-c(1:trimAmountTop),]
  }
  else if (trimAmountBottom != 0) {
    imageProcessed <- image[-c((nrow(image) - trimAmountBottom + 1):nrow(image)),]
  }
  return(imageProcessed)
}



#' Envelope Starts and Ends
#'
#' A better version of .get_image_starts_ends but meant for single traces that
#' are isolated on their own picture.
#'
#' @param traceMatrix a single isolated trace, with only one trace(no timing on the plot)
#' @param thresh How far it will go before deciding were already past the numbers at start of traces
#' @param returnMatrix Will return the cut matrix if TRUE, if FALSE will return list of the Start and End
#' @param gapLengthCutoff How Large the gap between the text and the trace must be in order to consider that
#' point the new start, if gap is less then cutoff, will keep looking until it gets to the thresh value
#'
#' @return list of the Start and End or cut matrix
#' @export
env_start_end <- function(traceMatrix, thresh = 300, gapLengthCutoff = 20, returnMatrix = TRUE){
  startFound <- FALSE
  endFound <- FALSE
  indicatorStart <- FALSE
  indicatorEnd <- FALSE
  gapLength <- 0

  #Starting at the left side
  for (i in 1:ncol(traceMatrix)) {
    if (sum(traceMatrix[,i]) != 0 & isFALSE(startFound) & isFALSE(indicatorStart)) {

      startFound <- TRUE
      possibleStarts <- i
      counter <- i
      if (isTRUE(indicatorStart)) {
        break # need this to exit for loop
      }
    }
    #done the gap after the first found, seeing if the length is correct
    else if (sum(traceMatrix[,i]) != 0 & isFALSE(startFound) & isTRUE(indicatorStart)) {
      possibleStarts <- i
      if (gapLength > gapLengthCutoff) {
        break
      }
      else{
        gapLength <- 0
        possibleStarts <- counter
        indicatorStart <- FALSE
        startFound <- TRUE
      }
    }
    else if (sum(traceMatrix[,i]) == 0 & isTRUE(startFound)) {
      if (counter + thresh > i) { #the new gap was found within the threshold limit

        startFound <- FALSE
        indicatorStart <- TRUE # this is the second time through the first part
        #( therefore past the number at the first of the trace)
        next
      }
    }
    #the intermediate part, foun a first start and a second start, seeing how long it is
    else if (sum(traceMatrix[,i]) == 0 & isFALSE(startFound) & isTRUE(indicatorStart)) {
      gapLength <- gapLength + 1
    }
  }
  gapLength <- 0
  #now starting at the other side of the image
  for (j in 1:(ncol(traceMatrix) - 1)) {
    i <- ncol(traceMatrix) - j
    if (sum(traceMatrix[,i]) != 0 & isFALSE(endFound) & isFALSE(indicatorEnd)) {
      endFound <- TRUE
      possibleEnds <- i
      counter <- i
      if (isTRUE(indicatorEnd)) {
        break # need this to exit for loop
      }
    }
    #done the gap after the first found, seeing if the length is correct
    else if (sum(traceMatrix[,i]) != 0 & isFALSE(endFound) & isTRUE(indicatorEnd)) {
      possibleEnds <- i
      if (gapLength > gapLengthCutoff) {
        break
      }
      else{
        gapLength <- 0
        possibleEnds <- counter
        indicatorEnd <- FALSE
        endFound <- TRUE
      }
    }
    else if (sum(traceMatrix[,i]) == 0 & isTRUE(endFound)) {
      if (counter - thresh < i) { #the new gap was found within the threshold limit

        endFound <- FALSE
        indicatorEnd <- TRUE # this is the second time through the first part
        #( therefore past the number at the first of the trace)
        next
      }
    }
    #the intermediate part, found a first start and a second start, seeing how long it is
    else if (sum(traceMatrix[,i]) == 0 & isFALSE(endFound) & isTRUE(indicatorEnd)) {
      gapLength <- gapLength + 1
    }
  }
  if (isFALSE(returnMatrix)) {
    return(list(Start = possibleStarts, End = possibleEnds))
  }
  else {
    return(traceMatrix[,possibleStarts:possibleEnds])
  }

}



#' Intersection Check
#'
#' Takes to envelope lines and checks to see if the lower is higher then the
#' upper
#'
#' @param topEnv The top envelope of the comparison
#' @param bottomEnv The bottom envelope of the comparison
#' @param imageName Name of the image for the warning if cross
#' @param rmAmount The amount ignored from both the right and left sides
#' of the image where possible trace lines could intersect because of noise
#' This is to ensure that false intersections aren't found between the two traces
#'
#' @return warning if there is an intersection
#' @export
intersection_check <- function(topEnv, bottomEnv, imageName, rmAmount = 300){
  for (m in rmAmount:(min(c(length(bottomEnv), length(topEnv))) - rmAmount)) { #Checking for intersection between the two lines
    if (topEnv[m] >= bottomEnv[m]) { # reversed because indexing is upsidedown for images
      return(warning(paste0("There is an intersection at (", topEnv[m], ", ", m, ")")))

    }
  }
  return(FALSE) # there is no intersection in the bounds
}


#' Trace Spike Check
#'
#' Looks for abnormal spikes in the trace that wasn't smoothed out before
#'
#' @param trace a single trace
#' @param spikeThreshold how big the difference has to be to be considered to be a spike
#'
#' @return
#' @export
spike_check <- function(trace, spikeThreshold = 50){
  if (length(which(abs(diff(trace)) > spikeThreshold)) > 0) {
    return(warning("abnormal spike in the top trace"))
  }
  else(return(NULL))
}



#' Triple Trace Checking
#'
#' Using find_peaks, checks for three traces on the same Image, These can't be done
#' currently by this alg
#'
#' @param imageMatrix Horizontal image processed
#' @param minDistance The min distance aloud between found peaks
#' @param thresholdHeight How separated the heights of the three peaks can be
#' @param thresholdDistance How far the timing lines can be from each other(
#' saves the cases when the peaks are same height as the timing traces)
#' @param percentFromEdge the distance alows from the edge of the image
#' @param topCut from .topCut() line between the text and the two traces
#' @param bottomCut from .bottomCut() line after traces before the two timing marks
#' @param threshCutImage how much different the peak heights have to be in the cut image
#' for the image to be considered to be a triple.
#'
#' @return TRUE or FALSE for finding a triple or not

triple_check <- function(imageMatrix, topCut, bottomCut, minDistance = 50, percentFromEdge = 2, thresholdHeight = 200,
                          thresholdDistance = 250, threshCutImage = 500){
  sums <- rowSums(imageMatrix)
  tripleCheck <- find_peaks(sums, minDistance = minDistance, maxPeakNumber = 6,
                            percentFromEdge = percentFromEdge, plots = FALSE)
  sumsCut <- rowSums(imageMatrix[-c(0:topCut, bottomCut:nrow(imageMatrix)), ])
  cutCheck <- find_peaks(sumsCut, minDistance = minDistance, maxPeakNumber = 4,
                         percentFromEdge = percentFromEdge, plots = FALSE)
  if (length(tripleCheck$Index) == 6) { # possible a triple so we check weather
    #the timing peaks are close together in heights
    #this can be an indication that there are possibly three traces on the image
    if (tripleCheck$Height[5] - thresholdHeight <= tripleCheck$Height[4] &
        tripleCheck$Height[4] <= tripleCheck$Height[5] + thresholdHeight &
        tripleCheck$Height[5] - thresholdHeight <= tripleCheck$Height[6] &
        tripleCheck$Height[6] <= tripleCheck$Height[5] + thresholdHeight) {
      #checking that the three peaks are sufficiently close to eachother
      if (tripleCheck$Index[5] - thresholdDistance <= tripleCheck$Index[4] &
          tripleCheck$Index[4] <= tripleCheck$Index[5] + thresholdDistance &
          tripleCheck$Index[5] - thresholdDistance <= tripleCheck$Index[6] &
          tripleCheck$Index[6] <= tripleCheck$Index[5] + thresholdDistance) {
        return(warning("Possible Triple Found"))
      }
    }
  }
  if (length(cutCheck$Index) == 4) { # possible a triple so we check weather
    #the timing peaks are close together in heights ( for the cut image ( there will be four where there should be 2))
    #this can be an indication that there are possibly three traces on the image

    max <- max(cutCheck$Height)
    count <- 0
    for (i in 1:4) {
      if (max + threshCutImage >= cutCheck$Height[i] &
          max - threshCutImage <= cutCheck$Height[i]) {
        count <- count + 1
      }
    }
    # if two others are in that range then they are probabily a tripple set
    if (count >= 3) {
      return(warning("Possible Triple Found"))
    }
  }
  return(FALSE) # this is if the other two dont return anything
}



#' TIS: Trace Identification Through Separation
#'
#' Combines all other functions to allow the user to digitize one image.
#' All parameters are adjustable to allow for attempts to digitize more then one
#' type of image.  Recommended to experiment with all individual functions, then set
#' parameters in this function to do the total digitiziation.
#'
#' Beta 0 and Beta1 are required for the brightness, changes the method for the
#' image processing, if an image is bright, the parameters change to try and remove
#' the over exposure so the envelopes can be more accurate around a trace. These
#' can be found using a logistic regression with the predictor of the proportion of
#' pixels above 0.8 scaled by the total number of pixels.  The user imput the response
#' variable, a decision of 0(not bright) and 1(bright) for a large number of images,
#' to "train" the algorithm what to decide.
#'
#' @param imageName The name of the file
#' @param fileLoc Path from ~/ to the dir where the file is
#' @param pathToWorkingDir Path of where the plots should be saved to
#' @param HDVcheck For Magneto project only, use FALSE otherwise
#' @param plotPNG TRUE or FALSE if you want plots saved or just data returned
#' @param trimAmountTop Number of pixels removed from top of image (usually for common flares)
#' @param trimAmountBottom Number of pixels removed from bottom of image
#' @param beta0 The intercept of a logistic regression default is for magnetograms
#' Logistic regression on the decision if an image can be considered to be bright
#' based off of a user deciding bright or non bright on a large set of images
#' @param beta1 The slope of the logistic regression default is for magnetograms
#' @param cutoffProbability The probability cut off for the decision of an imageMatrix being bright
#' @param NADefault The default value to replace NA's found in the matrix
#' @param FilterBright Vector specifying the dimensions of the kernel,
#' which will be used to perform either delation or erosion, such as c(13,13)
#' @param FilterNonBright Vector specifying the dimensions of the kernel,
#' which will be used to perform either delation or erosion, such as c(8,8)
#' @param methodBright For bright images, choose 'delation'(adds to image, making brights brighter) or
#' 'erosion' (subtracts from image, making brights darker) to occur during processing
#' @param methodNonBright For non bright images, choose 'delation'(adds to image, making brights brighter) or
#' 'erosion' (subtracts from image, making brights darker) to occur during processing
#' @param thresholdBright should be between 0 and 1 for black and white pixels, where the Default = 0.8
#' anything over the threshold will be 1(white) and anything under will be turned to 0(black)
#' @param thresholdNonBright should be between 0 and 1 for black and white pixels, where the Default = 0.5
#' anything over the threshold will be 1(white) and anything under will be turned to 0(black)
#' @param trimAmountRight Percentage of the image that is guaranteed to not have
#' any trace in it(mostly used to remove over exposure from the scanning process)
#' @param trimAmountLeft Percentage of the image that is guaranteed to not have
#' any trace in it(mostly used to remove over exposure from the scanning process)
#' @param peakPercFromEdge Used in find_peaks if you know there won't be any relevant data in that region
#' for finding rowSum peaks. This percentage is removed from consideration in find_peaks
#'  in a region (look at rowSums of the matrix to see the peaks for this parameter)
#' @param peakPercentFromEdgeLeftSide Passed into find peaks, if not specified (NULL), uses
#' peakPercFromEdge for both left and right sides. If specified, peakPercFromEdge
#' is defaulted to just the right side of the plot
#' (look at rowSums of the matrix to see the peaks for this parameter)
#' @param cutPercentage Usually the min of either trimAmountLeft or trimAmountRight
#' @param shortestAllowedSeqOfZeros Smallest gap allowed between the lower trace
#' and the timing marks, before the trace is considered to intersect with the timing marks
#' @param tripleCheckMinDistance Minimum distance allowed between found peaks from rowSums().  Ensures that the user
#' has max one point per peak (only used for magneto, make large if not needed)
#' @param tripleThresholdHeight How different the heights can be for three peaks
#' to be considered to be a triple set of peaks (only used for magneto, make 0 if not needed)
#' @param tripleThresholdDistance How far apart the peaks can be for the them to be
#' considered to be a set of triples (only used for magneto, make large if not needed)
#' @param threshCutImage Used in triple_check as a second way to check for triples,
#' takes the cut image(no timing or writing on the image) it then finds peaks and checks for 3 peaks(a triple)
#' Same definition as the tripleThresholdDistance but different value needed because different scale
#' @param OffsetDistanceForEnvelopes How far off of the trace you would like the envelope to be
#' used as a safety net to catch any parts of the trace that might not be detected
#' @param maxEnvelopeROC The max difference (rate of change) between any two points of the trace envelope before
#' the outlier point will be removed and considered to be noise, these are usually seen as jumps in the envelopes
#' @param maxNoise The max amount of points created off of the trace before the
#' envelope can be considered to be not filling in a gap but being off of the
#' trace entirely, will correct after this number
#' @param envelopeStartEndThreshold The default vertical start and end location for the trace lines,
#' if they aren't found automatically
#' @param intersetionRemoveAmount The amount ignored from both the right and left sides
#' of the image where possible trace lines could intersect because of noise
#' This is to ensure that false intersections aren't found between the two traces
#' @param CreateTraceThreshold During the final digitization lines of traces,
#' any difference found higher then the CreateTraceThreshold between any two points will be smoothed with a MA
#' @param MARange The amount of points in each direction that the moving average
#'  will look at to calculate the MA on (added to the region value for the whole MARange)
#' @param region The number of points inside the MA that will be corrected to the MA value
#' @param loopNumber The amount of times the MA Smoothing will happen, to guarantee elimination of other peaks that could be
#' created from the MA
#' @param spikeThreshold The difference in height between two pixels in the final digitization lines
#' checks for abnormal spikes in the tracing algorithm, could be due to a jump from one trace to another
#' (anything above the threshold will be sent to warning)
#' @param k See rollMean() in zoo package for details
#' @param saveData allows the user to save the data to the working directory
#' @param gapLengthCutoff How large the gap between the writing at the start of the trace (seen in Fig 7)
#' and the  actual start of the trace must be in order to consider that point the new start, if gap is less
#' then cutoff, will keep looking until it gets to the thresh value.  If no writing on start of traces,
#'  this can be set to 0.
#'
#' @return
#' @export
TIS <- function(imageName, fileLoc, pathToWorkingDir = "~/",
                HDVcheck = FALSE, plotPNG = TRUE, saveData = FALSE,
                trimAmountTop = 100,
                trimAmountBottom = 50, beta0 = -2.774327,
                beta1 = 51.91687, cutoffProbability = 0.5,
                NADefault = 0, FilterBright = c(13, 13),
                FilterNonBright = c(8, 8),
                methodBright = "delation",
                methodNonBright = "delation",
                thresholdBright = 0.8,
                thresholdNonBright = 0.5, trimAmountLeft = 2, trimAmountRight = 2,
                peakPercFromEdge = 2, peakPercentFromEdgeLeftSide = 25,
                cutPercentage = 2, shortestAllowedSeqOfZeros = 25,
                tripleCheckMinDistance = 50, tripleThresholdHeight = 200,
                tripleThresholdDistance = 250, threshCutImage = 500,
                OffsetDistanceForEnvelopes = 10, maxEnvelopeROC = 35, maxNoise = 250,
                envelopeStartEndThreshold = 300, intersetionRemoveAmount = 1000,
                CreateTraceThreshold = 5, MARange = 6, region = 2,
                loopNumber = 4, spikeThreshold = 50, k = 40, gapLengthCutOff = 20){

  traceWarnings <- vector()
  typeCheck <- NULL # if you aren't using the HDV check
  flag <- FALSE
  intersectionFlag <- FALSE
  rolledImage <- NULL
  dirChangeFlag <- FALSE
  if (isTRUE(HDVcheck)) {
    # HDV Checking for Magnetograms ----------------------------------------------
    typeCheck <- tryCatch(.hdv_check(imageName), warning = function(w) w)
  }
  if (inherits(typeCheck, "warning")) {
    return(warning(typeCheck)) # will return the warning if an HDV is found
  }
  else {
    # Process The Image ----------------------------------------------------------

    imageMatrix <- tryCatch(import_process_image(imageName = imageName, file_loc = fileLoc,
                                        trimAmountTop = trimAmountTop,
                                        trimAmountBottom = trimAmountBottom, beta0 = beta0,
                                        beta1 = beta1, cutoffProbability = cutoffProbability,
                                        NADefault = NADefault, FilterBright = FilterBright,
                                        FilterNonBright = FilterNonBright,
                                        methodBright = methodBright,
                                        methodNonBright = methodNonBright,
                                        thresholdBright = thresholdBright,
                                        thresholdNonBright = thresholdNonBright), error = function(e) e)
    if (inherits(imageMatrix, "error")) {
      return(as.character(imageMatrix))
    }
    #takes off the usual flair spots
    imageSideCut <- trim_sides(imageMatrix, trimAmountLeft = trimAmountLeft,
                                trimAmountRight = trimAmountRight)
    imageCut <- trim_top_bottom(imageSideCut, trimAmountTop = trimAmountTop,
                                 trimAmountBottom = trimAmountBottom)


    # Find the Top and Bottom Cut for the Image -----------------------------------

    topBottomCuts <- tryCatch(find_cuts(imageCut, percentFromEdge = peakPercFromEdge,
                               percentEdgeForLeft = peakPercentFromEdgeLeftSide,
                               cutPercentage = cutPercentage,
                               shortestAllowedSeqOfZeros = shortestAllowedSeqOfZeros), error = function(e) e)
    if (inherits(topBottomCuts, "error")) {
      return(as.character(topBottomCuts))
    }
    topCut <- topBottomCuts$TopCut # line between the words and the top trace
    bottomCut <- topBottomCuts$BottomCut # line between the timing traces and the bottom trace
    if (inherits(topCut, "warning")) {
      traceWarnings <- append(traceWarnings, topCut)
      if (isFALSE(dirChangeFlag)) {
      pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
      dirChangeFlag <- TRUE
      }
      flag = TRUE #wont process, but will put a plot out into the pwd
      topCut <- 0
    }
    if (inherits(bottomCut, "warning")) {
      traceWarnings <- append(traceWarnings, bottomCut)
      if (isFALSE(dirChangeFlag)) {
        pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
        dirChangeFlag <- TRUE
      }
      flag = TRUE #wont process, but will put a plot out into the pwd
      bottomCut <- nrow(imageCut)
    }
  }
  if (isFALSE(flag)) {
    # Check for a Triple Set of Traces -------------------------------------------
    tripleBool <- tryCatch(triple_check(imageMatrix = imageCut, topCut = topCut,
                                bottomCut = bottomCut, minDistance = tripleCheckMinDistance,
                                percentFromEdge = peakPercFromEdge, thresholdHeight = tripleThresholdHeight,
                                thresholdDistance = tripleThresholdDistance, threshCutImage = threshCutImage),
                           warning = function(w) w) #checking for triple trace images
    if (inherits(tripleBool, "warning")) {
      traceWarnings <- append(traceWarnings, tripleBool)
      if (isFALSE(dirChangeFlag)) {
        pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
        dirChangeFlag <- TRUE
      }
      flag = TRUE
    }
    # No Errors Found, Creating The Actual Traces --------------------------------
    # else {

      # Creating Envelopes ---------------------------------------------------------
      # Gets rid of small gaps
      rolledImage <- mean_roll_image(imageMatrix = imageCut, topcut = topCut,
                                     bottomcut =  bottomCut, fill = "extend", k = k) #to get a more consistent image
      # Creates the matrix scaled envelope
      matrixEnvelopes <- find_envelopes(imageMatrix = imageCut, rolledImage = rolledImage, bottomCut = bottomCut,
                                        topCut = topCut,
                                        returnType = "MatrixScaled", sepDist = OffsetDistanceForEnvelopes,
                                        max_roc = maxEnvelopeROC, maxNoise = maxNoise)
      # Creates the plotting scaled envelope for the return to the user
      plotEnvelopes <- find_envelopes(rolledImage = rolledImage, imageMatrix = imageCut,
                                      topCut = topCut,
                                      bottomCut = bottomCut, returnType = "PlottingScaled",
                                      maxNoise = maxNoise, max_roc = maxEnvelopeROC,
                                      sepDist = OffsetDistanceForEnvelopes)
      # Isolates both traces on their own plots
      traceMatrices <- isolate_traces(imageCut, topEnvelope = matrixEnvelopes$TopEnvelope,
                                      topLowerEnvelope = matrixEnvelopes$TopLowerEnvelope,
                                      bottomUpperEnvelope = matrixEnvelopes$BottomUpperEnvelope,
                                      bottomEnvelope = matrixEnvelopes$BottomEnvelope)

      TopStartsEnds <- env_start_end(traceMatrix = traceMatrices$TopTraceMatrix,
                                      returnMatrix = FALSE, thresh = envelopeStartEndThreshold,
                                      gapLengthCutoff = gapLengthCutOff)
      BottomStartsEnds <- env_start_end(traceMatrix = traceMatrices$BottomTraceMatrix,
                                         returnMatrix = FALSE, thresh = envelopeStartEndThreshold,
                                        gapLengthCutoff = gapLengthCutOff)

      # Checking Envelopes for Intersections ---------------------------------------

      intersection <- tryCatch(intersection_check(topEnv = matrixEnvelopes$TopLowerEnvelope,
                                                   bottomEnv = matrixEnvelopes$BottomUpperEnv,
                                                   imageName, rmAmount = intersetionRemoveAmount),
                               warning = function(w) w)
      if (inherits(intersection, "warning")) {
        traceWarnings <- append(traceWarnings, intersection)
        if (isFALSE(dirChangeFlag)) {
          pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
          dirChangeFlag <- TRUE
        }
        intersectionFlag <- TRUE
      }

      # Creating the Two Traces ----------------------------------------------------

      topTrace <- create_trace(traceMatrix = traceMatrices$TopTraceMatrix,
                               start = TopStartsEnds$Start,
                               end = TopStartsEnds$End,
                               topEnv = matrixEnvelopes$TopEnvelope,
                               bottomEnv =  matrixEnvelopes$TopLowerEnvelope,
                               thresh = CreateTraceThreshold,
                               MARange = MARange, region = region, loopNumber = loopNumber)
      bottomTrace <- create_trace(traceMatrix = traceMatrices$BottomTraceMatrix,
                                  start =  BottomStartsEnds$Start,
                                  end =  BottomStartsEnds$End,
                                  topEnv = matrixEnvelopes$BottomUpperEnvelope,
                                  bottomEnv = matrixEnvelopes$BottomEnvelope,
                                  thresh = CreateTraceThreshold,
                                  MARange = MARange, region = region,
                                  loopNumber = loopNumber)

      # Checking for spikes in the traces ------------------------------------------
      topTraceSpikeCheck <- tryCatch(spike_check(topTrace, spikeThreshold = spikeThreshold), warning = function(w) w)
      bottomTraceSpikeCheck <- tryCatch(spike_check(bottomTrace, spikeThreshold = spikeThreshold), warning = function(w) w)
      if (inherits(topTraceSpikeCheck, "warning") || inherits(bottomTraceSpikeCheck, "warning")) {
        if (!is.null(topTraceSpikeCheck)) {
          traceWarnings <- append(traceWarnings, topTraceSpikeCheck)
          if (isFALSE(intersectionFlag)){ # so we still get the full plot but it goes to the correct place
            if (isFALSE(dirChangeFlag)) {
              pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
              dirChangeFlag <- TRUE
            }
            intersectionFlag <- TRUE
          }
        }
        if (!is.null(bottomTraceSpikeCheck)) { # Sp we still get the full plot but it goes to the correct place
          traceWarnings <- append(traceWarnings, bottomTraceSpikeCheck)
          if (isFALSE(intersectionFlag)){
            if (isFALSE(dirChangeFlag)) {
              pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
              dirChangeFlag <- TRUE
            }
            intersectionFlag <- TRUE
          }
        }
      }

    #}


  }
  # Plotting (if applicable) ---------------------------------------------------
  time <- FALSE
  if (isTRUE(plotPNG)){
    if (isFALSE(flag)) {
      time <- plot_success(imageMatrix = imageCut, rolledImage = rolledImage, topCut = topCut,
                   bottomCut = bottomCut, topStartEnds = TopStartsEnds, bottomStartEnds = BottomStartsEnds,
                   topTrace = topTrace, bottomTrace = bottomTrace, maxNoise = maxNoise, max_roc = maxEnvelopeROC,
                   sepDist = OffsetDistanceForEnvelopes, pathToWorkingDir = pathToWorkingDir, imageName = imageName,
                   intersectionFlag = intersectionFlag, plotEnv = plotEnvelopes) # adds fail to process dir on if intersection but keeps all info


      totalReturn <- list(ImageCutMatrix = imageCut, RolledImage = rolledImage,
                          PlotScaledEnvelopes = plotEnvelopes, TopTraceMatrix = topTrace,
                          TopTraceStartEnds = list(Start = TopStartsEnds$Start, End = TopStartsEnds$End),
                          BottomTraceMatrix = bottomTrace,
                          BottomTraceStartEnds = list(Start = BottomStartsEnds$Start, End = BottomStartsEnds$End),
                          Cuts = list(TopCut = topCut, BottomCut = bottomCut),
                          Warnings = traceWarnings)
    }
    if  (isTRUE(flag)) {
      time <- plot_with_warnings(imageMatrix = imageCut, topCut = topCut, bottomCut = bottomCut
                         ,pathToWorkingDir = pathToWorkingDir, imageName = imageName)

      totalReturn <- list(ImageCutMatrix = imageCut,
                          Cuts = list(TopCut = topCut, BottomCut = bottomCut),
                          Warnings = traceWarnings)
    }
  }
  else{
    if (isFALSE(flag)) {
      totalReturn <- list(ImageCutMatrix = imageCut, RolledImage = rolledImage,
                          PlotScaledEnvelopes = plotEnvelopes, TopTraceMatrix = topTrace,
                          TopTraceStartEnds = list(Start = TopStartsEnds$Start, End = TopStartsEnds$End),
                          BottomTraceMatrix = bottomTrace,
                          BottomTraceStartEnds = list(Start = BottomStartsEnds$Start, End = BottomStartsEnds$End),
                          Cuts = list(TopCut = topCut, BottomCut = bottomCut),
                          Warnings = traceWarnings)
    }
    if  (isTRUE(flag)) {
      totalReturn <- list(ImageCutMatrix = imageCut,
                          Cuts = list(TopCut = topCut, BottomCut = bottomCut),
                          Warnings = traceWarnings)
    }
  }
  if (isTRUE(saveData)) {
    if (time == FALSE){
      datePieces <- strsplit(as.character(Sys.time()), split = "-")
      timePieces <- strsplit(datePieces[[1]][3], ":")
      spaceRemove <- strsplit(timePieces[[1]][1], " ")
      time <- as.character(paste0(datePieces[[1]][1], datePieces[[1]][2],
                                  spaceRemove[[1]][1], spaceRemove[[1]][2],
                                  timePieces[[1]][2], timePieces[[1]][3]))
    }
    saveRDS(totalReturn, file = paste0(pathToWorkingDir, imageName, "-",time, "Data.RDS"))
  }

  return(totalReturn)
}


## dont need this right now, using multy processing in Digitization Computing#' TIS_automation
# @return
# @export
# TIS_automation <- function(DigitizationTODO, pathToDigitizationDir, keywordInImageName, ignoreErrorMessage = FALSE){
#
#   # Dimension check on the DigitizationTODO Dataframe
#   if (dim.data.frame(DigitizationTODO)[2] != 6) {
#     Error <- "Your dataframe has the wrong dimentions, should be 6 columns with 3rd being TRUE or FALSE"
#     return(stop(Error))
#   }
#   foundWithKeyword <- grep(keywordInImageName, x = DigitizationTODO$ImageName) # Find all images in DigitizationTODO
#   #with that keyword
#
#   for (i in foundWithKeyword) {
#
#     if (DigitizationTODO$DigitizedYet[i] == "FALSE") { # Checking if digitized yet
#       filePath <- as.character(DigitizationTODO$ImagePath[i])
#       imageName <- as.character(DigitizationTODO$ImageName[i])
#
#       ErrorMessage <- DigitizationTODO$ErrorWhenDigitized[i]
#       if (is.na(ErrorMessage) || isTRUE(ignoreErrorMessage)) { # If there is an error message about the data (like there isn't any data)
#
#         # Check for the year directory, if doesn't exist, it is created
#         imagePWD <- .dir_Str(imageName = imageName, pathToWorkingDir = pathToDigitizationDir)
#
#         # Do the digitization
#         Digitization <- TIS(imageName = imageName, fileLoc = filePath, pathToWorkingDir = imagePWD, HDVcheck = TRUE)
#
#         # Different outcomes for the digitization attempt
#         if (length(Digitization$Warnings) == 0) { # no warnings, therefore a sucessfull digitization
#           DigitizationTODO$DigitizedYet[i] = TRUE
#           DigitizationTODO$DigitizationPath[i] = imagePWD
#           DigitizationTODO$DigitizationName[i] = paste0(imageName, "-Digitized,RDS")
#           saveRDS(Digitization, file = paste0(imagePWD, imageName, "-Digitized.RDS"))
#         }
#         else if (inherits(Digitization, "warning")) { #Should only happen if there is an HDV warning
#           print(Digitization)
#           DigitizationTODO$ErrorWhenDigitized[i] == Digitization
#         }
#         else {# there are warnings so, it is considered to be a failed to process not considered to be digitized yet
#           print(Digitization$Warnings)
#           DigitizationTODO$ErrorWhenDigitized[i] == Digitization$Warnings
#           saveRDS(Digitization, file = paste0(imagePWD,"FailedToProcess/", imageName, "-FailToProcess-Data.RDS"))
#         }
#       }
#       else {
#         print("Skipping.. previous error when digitizing")
#         print(DigitizationTODO$ErrorWhenDigitized[i])
#       }
#     }
#     print("-----------")
#     write.csv(DigitizationTODO, file = DigitizationTODOFileNameAndPath) # updates the .csv
#     #after each digitization so that we won't loose any data
#   }
# }





#' TISI: Trace Identification Through Separation for Improvement
#'
#' Combines all other functions to allow the user to digitize one image. This function
#' allows for input of specific known envelopes set manually using the shiny application
#' along with this function.
#' All parameters are still adjustable like in TIS to allow for attempts to digitize more then one
#' type of image.  Recommended to experiment with all individual functions, then set
#' parameters in this function to do the total digitization.
#'
#' Beta 0 and Beta1 are required for the brightness, changes the method for the
#' image processing, if an image is bright, the parameters change to try and remove
#' the over exposure so the envelopes can be more accurate around a trace. These
#' can be found using a logistic regression with the predictor of the proportion of
#' pixels above 0.8 scaled by the total number of pixels.  The user imput the response
#' variable, a decision of 0(not bright) and 1(bright) for a large number of images,
#' to "train" the algorithm what to decide.
#'
#' @param imageName The name of the file
#' @param fileLoc Path from ~/ to the dir where the file is
#' @param pathToWorkingDir Path of where the plots should be saved to
#' @param HDVcheck For Magneto project only, use FALSE otherwise
#' @param plotPNG TRUE or FALSE if you want plots saved or just data returned
#' @param trimAmountTop Number of pixels removed from top of image (usually for common flares)
#' @param trimAmountBottom Number of pixels removed from bottom of image
#' @param beta0 The intercept of a logistic regression default is for magnetograms
#' Logistic regression on the decision if an image can be considered to be bright
#' based off of a user deciding bright or non bright on a large set of images
#' @param beta1 The slope of the logistic regression default is for magnetograms
#' @param cutoffProbability The probability cut off for the decision of an imageMatrix being bright
#' @param NADefault The default value to replace NA's found in the matrix
#' @param FilterBright Vector specifying the dimensions of the kernel,
#' which will be used to perform either delation or erosion, such as c(13,13)
#' @param FilterNonBright Vector specifying the dimensions of the kernel,
#' which will be used to perform either delation or erosion, such as c(8,8)
#' @param methodBright For bright images, choose 'delation'(adds to image, making brights brighter) or
#' 'erosion' (subtracts from image, making brights darker) to occur during processing
#' @param methodNonBright For non bright images, choose 'delation'(adds to image, making brights brighter) or
#' 'erosion' (subtracts from image, making brights darker) to occur during processing
#' @param thresholdBright should be between 0 and 1 for black and white pixels, where the Default = 0.8
#' anything over the threshold will be 1(white) and anything under will be turned to 0(black)
#' @param thresholdNonBright should be between 0 and 1 for black and white pixels, where the Default = 0.5
#' anything over the threshold will be 1(white) and anything under will be turned to 0(black)
#' @param trimAmountRight Percentage of the image that is guaranteed to not have
#' any trace in it(mostly used to remove over exposure from the scanning process)
#' @param trimAmountLeft Percentage of the image that is guaranteed to not have
#' any trace in it(mostly used to remove over exposure from the scanning process)
#' @param peakPercFromEdge Used in find_peaks if you know there won't be any relevant data in that region
#' for finding rowSum peaks. This percentage is removed from consideration in find_peaks
#'  in a region (look at rowSums of the matrix to see the peaks for this parameter)
#' @param peakPercentFromEdgeLeftSide Passed into find peaks, if not specified (NULL), uses
#' peakPercFromEdge for both left and right sides. If specified, peakPercFromEdge
#' is defaulted to just the right side of the plot
#' (look at rowSums of the matrix to see the peaks for this parameter)
#' @param cutPercentage Usually the min of either trimAmountLeft or trimAmountRight
#' @param shortestAllowedSeqOfZeros Smallest gap allowed between the lower trace
#' and the timing marks, before the trace is considered to intersect with the timing marks
#' @param tripleCheckMinDistance Minimum distance allowed between found peaks from rowSums().  Ensures that the user
#' has max one point per peak (only used for magneto, make large if not needed)
#' @param tripleThresholdHeight How different the heights can be for three peaks
#' to be considered to be a triple set of peaks (only used for magneto, make 0 if not needed)
#' @param tripleThresholdDistance How far apart the peaks can be for the them to be
#' considered to be a set of triples (only used for magneto, make large if not needed)
#' @param threshCutImage Used in triple_check as a second way to check for triples,
#' takes the cut image(no timing or writing on the image) it then finds peaks and checks for 3 peaks(a triple)
#' Same definition as the tripleThresholdDistance but different value needed because different scale
#' @param OffsetDistanceForEnvelopes How far off of the trace you would like the envelope to be
#' used as a safety net to catch any parts of the trace that might not be detected
#' @param maxEnvelopeROC The max difference (rate of change) between any two points of the trace envelope before
#' the outlier point will be removed and considered to be noise, these are usually seen as jumps in the envelopes
#' @param maxNoise The max amount of points created off of the trace before the
#' envelope can be considered to be not filling in a gap but being off of the
#' trace entirely, will correct after this number
#' @param envelopeStartEndThreshold The default vertical start and end location for the trace lines,
#' if they aren't found automatically
#' @param intersetionRemoveAmount The amount ignored from both the right and left sides
#' of the image where possible trace lines could intersect because of noise
#' This is to ensure that false intersections aren't found between the two traces
#' @param CreateTraceThreshold During the final digitization lines of traces,
#' any difference found higher then the CreateTraceThreshold between any two points will be smoothed with a MA
#' @param MARange The amount of points in each direction that the moving average
#'  will look at to calculate the MA on (added to the region value for the whole MARange)
#' @param region The number of points inside the MA that will be corrected to the MA value
#' @param loopNumber The amount of times the MA Smoothing will happen, to guarantee elimination of other peaks that could be
#' created from the MA
#' @param spikeThreshold The difference in height between two pixels in the final digitization lines
#' checks for abnormal spikes in the tracing algorithm, could be due to a jump from one trace to another
#' (anything above the threshold will be sent to warning)
#' @param k See rollMean() in zoo package for details
#' @param improvement TRUE or FALSE depending on if you are adding any of the improvement features,
#' causes TISI to ignore some check
#' @param saveData TRUE or FALSE if wanting a .RDS file saved to dir along with the image if plotPNG is TRUE
#' @param improveTopBottomCuts Vector of length two consisting of the two points as heights that will
#' become the new top and bottom cuts respectively
#' @param improveTTopEnvelope data.frame of col x and col y representing points for top of the top envelope pair
#' doesn't need to be a point for each column in original picture, will fill in the blanks
#' @param improveBTopEnvelope data.frame of col x and col y representing points for Bottom of the top envelope pair
#' doesn't need to be a point for each column in original picture, will fill in the blanks
#' @param improveTBottomEnvelope data.frame of col x and col y representing points for top of the bottom envelope pair
#' doesn't need to be a point for each column in original picture, will fill in the blanks
#' @param improveBBottomEnvelope data.frame of col x and col y representing points for bottom of the bottom envelope pair
#' doesn't need to be a point for each column in original picture, will fill in the blanks
#' @param improveTopEnvelopeStartEnd Vector of length 2, start and end point respectively
#' @param improveBottomEnvelopeStartEnd Vector of length 2, start and end point respectively
#' @param gapLengthCutoff How large the gap between the writing at the start of the trace (seen in Fig 7)
#' and the  actual start of the trace must be in order to consider that point the new start, if gap is less
#' then cutoff, will keep looking until it gets to the thresh value.  If no writing on start of traces,
#'  this can be set to 0.
#'
#' @return
#' @export
TISI <- function(imageName, fileLoc, pathToWorkingDir = "~/",
                 improvement = FALSE,
                HDVcheck = FALSE, plotPNG = TRUE,
                saveData = FALSE,
                improveTopBottomCuts = NA,
                improveTTopEnvelope = NA, improveBTopEnvelope = NA,
                improveTBottomEnvelope = NA,
                improveBBottomEnvelope = NA,
                improveTopEnvelopeStartEnd = NA,
                improveBottomEnvelopeStartEnd = NA,
                trimAmountTop = 100,
                trimAmountBottom = 50,
                trimAmountLeft = 2,
                trimAmountRight = 2,
                beta0 = -2.774327,
                beta1 = 51.91687, cutoffProbability = 0.5,
                NADefault = 0, FilterBright = c(13, 13),
                FilterNonBright = c(8, 8),
                methodBright = "delation",
                methodNonBright = "delation",
                thresholdBright = 0.8,
                thresholdNonBright = 0.5,
                peakPercFromEdge = 2, peakPercentFromEdgeLeftSide = 25,
                cutPercentage = 2, shortestAllowedSeqOfZeros = 25,
                tripleCheckMinDistance = 50, tripleThresholdHeight = 200,
                tripleThresholdDistance = 250, threshCutImage = 500,
                OffsetDistanceForEnvelopes = 10, maxEnvelopeROC = 35, maxNoise = 250,
                envelopeStartEndThreshold = 300, intersetionRemoveAmount = 1000,
                CreateTraceThreshold = 5, MARange = 6, region = 2,
                loopNumber = 4, spikeThreshold = 50, k = 40, gapLengthCutoff = 20){
  if(isTRUE(improvement)){
  pathToWorkingDir <- .dir_Str_Improvement(imageName = imageName, pathToWorkingDir = pathToWorkingDir)
  }
  traceWarnings <- vector()
  typeCheck <- NULL # if you aren't using the HDV check
  flag <- FALSE
  intersectionFlag <- FALSE
  rolledImage <- NULL
  dirChangeFlag <- FALSE
  if (isTRUE(HDVcheck)) {
    # HDV Checking for Magnetograms  ----------------------------------------------
    typeCheck <- tryCatch(.hdv_check(imageName), warning = function(w) w)
  }
  if (inherits(typeCheck, "warning")) {
    return(warning(typeCheck)) # will return the warning if an HDV is found
  }
  else {
    # Process The Image ----------------------------------------------------------

    imageMatrix <- tryCatch(import_process_image(imageName = imageName, file_loc = fileLoc,
                                                 trimAmountTop = trimAmountTop,
                                                 trimAmountBottom = trimAmountBottom, beta0 = beta0,
                                                 beta1 = beta1, cutoffProbability = cutoffProbability,
                                                 NADefault = NADefault, FilterBright = FilterBright,
                                                 FilterNonBright = FilterNonBright,
                                                 methodBright = methodBright,
                                                 methodNonBright = methodNonBright,
                                                 thresholdBright = thresholdBright,
                                                 thresholdNonBright = thresholdNonBright), error = function(e) e)
    if (inherits(imageMatrix, "error")) {
      return(as.character(imageMatrix))
    }
    #takes off the usual flair spots
    imageSideCut <- trim_sides(imageMatrix, trimAmountLeft = trimAmountLeft,
                               trimAmountRight = trimAmountRight)
    imageCut <- trim_top_bottom(imageSideCut, trimAmountTop = trimAmountTop,
                                trimAmountBottom = trimAmountBottom)


    # Find the Top and Bottom Cut for the Image -----------------------------------

    if (!is.na(improveTopBottomCuts[1])){
      if(is.vector(improveTopBottomCuts) & length(improveTopBottomCuts) == 2){
        topCut <-  nrow(imageCut) - improveTopBottomCuts[1] + 100 - 35
        bottomCut <-   nrow(imageCut) - improveTopBottomCuts[2] + 100 - 35 # this this might be 100 check
      }
      else{
        return(warning("wrong number of items in the
                       vector for top and bottom cuts improvment, should be length 2"))
      }
    }
    else{
      topBottomCuts <- tryCatch(find_cuts(imageCut, percentFromEdge = peakPercFromEdge,
                                          percentEdgeForLeft = peakPercentFromEdgeLeftSide,
                                          cutPercentage = cutPercentage,
                                          shortestAllowedSeqOfZeros = shortestAllowedSeqOfZeros),
                                error = function(e) e)
      if (inherits(topBottomCuts, "error")) {
        return(as.character(topBottomCuts))
      }
      topCut <- topBottomCuts$TopCut # line between the timing traces and the bottom trace
      bottomCut <- topBottomCuts$BottomCut # line between the words and the top trace
      if (isFALSE(improvement) & inherits(topCut, "warning")) {
        traceWarnings <- append(traceWarnings, topCut)
        if (isFALSE(dirChangeFlag)) {
          pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
          dirChangeFlag <- TRUE
        }
        flag = TRUE #wont process, but will put a plot out into the pwd
        topCut <- 0
      }
      if (isFALSE(improvement) & inherits(bottomCut, "warning")) {
        traceWarnings <- append(traceWarnings, bottomCut)
        if (isFALSE(dirChangeFlag)) {
          pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
          dirChangeFlag <- TRUE
        }
        flag = TRUE #wont process, but will put a plot out into the pwd
        bottomCut <- nrow(imageCut)
      }
    }
  }
  if (isFALSE(flag)) {
    # Check for a Triple Set of Traces -------------------------------------------
    if(isFALSE(improvement)){
      tripleBool <- tryCatch(triple_check(imageMatrix = imageCut, topCut = topCut,
                                          bottomCut = bottomCut, minDistance = tripleCheckMinDistance,
                                          percentFromEdge = peakPercFromEdge, thresholdHeight = tripleThresholdHeight,
                                          thresholdDistance = tripleThresholdDistance, threshCutImage = threshCutImage),
                             warning = function(w) w) #checking for triple trace images
      if (inherits(tripleBool, "warning")) {
        traceWarnings <- append(traceWarnings, tripleBool)
        if (isFALSE(dirChangeFlag)) {
          pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
          dirChangeFlag <- TRUE
        }
        flag = TRUE
      }
    }
    # No Errors Found, Creating The Actual Traces --------------------------------
    # else {

    # Creating Envelopes ---------------------------------------------------------
    # Gets rid of small gaps
    if (isTRUE(improvement) & inherits(topCut, "warning")){
      topCut <- 0
    }
    if (isTRUE(improvement) & inherits(bottomCut, "warning")){
      bottomCut <- ncol(imageCut)
    }
    rolledImage <- mean_roll_image(imageMatrix = imageCut, topcut = topCut,
                                   bottomcut =  bottomCut, fill = "extend", k = k) #to get a more consistent image
    # Creates the matrix scaled envelope
    matrixEnvelopes <- find_envelopes(imageMatrix = imageCut, rolledImage = rolledImage, bottomCut = bottomCut,
                                      topCut = topCut,
                                      returnType = "MatrixScaled", sepDist = OffsetDistanceForEnvelopes,
                                      max_roc = maxEnvelopeROC, maxNoise = maxNoise,
                                      improveTTopEnvelope = improveTTopEnvelope,
                                      improveBTopEnvelope = improveBTopEnvelope,
                                      improveTBottomEnvelope = improveTBottomEnvelope,
                                      improveBBottomEnvelope = improveBBottomEnvelope,
                                      trimTop = trimAmountTop, trimBottom = trimAmountBottom)
    # Creates the plotting scaled envelope for the return to the user
    plotEnvelopes <- find_envelopes(rolledImage = rolledImage, imageMatrix = imageCut,
                                    bottomCut = bottomCut, returnType = "PlottingScaled",
                                    topCut = topCut,
                                    maxNoise = maxNoise, max_roc = maxEnvelopeROC,
                                    sepDist = OffsetDistanceForEnvelopes,
                                    improveTTopEnvelope = improveTTopEnvelope,
                                    improveBTopEnvelope = improveBTopEnvelope,
                                    improveTBottomEnvelope = improveTBottomEnvelope,
                                    improveBBottomEnvelope = improveBBottomEnvelope,
                                    trimTop = trimAmountTop, trimBottom = trimAmountBottom)
    # Isolates both traces on their own plots
    traceMatrices <- isolate_traces(imageCut, topEnvelope = matrixEnvelopes$TopEnvelope,
                                    topLowerEnvelope = matrixEnvelopes$TopLowerEnvelope,
                                    bottomUpperEnvelope = matrixEnvelopes$BottomUpperEnvelope,
                                    bottomEnvelope = matrixEnvelopes$BottomEnvelope)


    # Creating the start and ends for the two traces ------------------------------
    if (!is.na(improveTopEnvelopeStartEnd[1]) &  length(improveTopEnvelopeStartEnd) == 2){
      TopStartsEnds <- data.frame(Start = improveTopEnvelopeStartEnd[1],
                                  End = improveTopEnvelopeStartEnd[2])
    }
    else if (!is.na(improveTopEnvelopeStartEnd[1]) & length(improveTopEnvelopeStartEnd != 2)){
      warning("improveTopEnvelopeStartEnd should be a vector of two points")
      }
    else {
    TopStartsEnds <- env_start_end(traceMatrix = traceMatrices$TopTraceMatrix,
                                   returnMatrix = FALSE, thresh = envelopeStartEndThreshold,
                                   gapLengthCutoff = gapLengthCutoff)
    }
    if (!is.na(improveBottomEnvelopeStartEnd[1]) &  length(improveBottomEnvelopeStartEnd) == 2){
      BottomStartsEnds <- data.frame(Start = improveBottomEnvelopeStartEnd[1],
                                  End = improveBottomEnvelopeStartEnd[2])
    }
    else if (!is.na(improveBottomEnvelopeStartEnd[1]) & length(improveBottomEnvelopeStartEnd != 2)){
      warning("improveBottomEnvelopeStartEnd should be a vector of two points")
    }
    else {
      BottomStartsEnds <- env_start_end(traceMatrix = traceMatrices$BottomTraceMatrix,
                                        returnMatrix = FALSE, thresh = envelopeStartEndThreshold,
                                        gapLengthCutoff = gapLengthCutoff)
    }

    # Checking Envelopes for Intersections ---------------------------------------
    if (isFALSE(improvement)){
      intersection <- tryCatch(intersection_check(topEnv = matrixEnvelopes$TopLowerEnvelope,
                                                  bottomEnv = matrixEnvelopes$BottomUpperEnv,
                                                  imageName, rmAmount = intersetionRemoveAmount),
                               warning = function(w) w)
      if (inherits(intersection, "warning")) {
        traceWarnings <- append(traceWarnings, intersection)
        if (isFALSE(dirChangeFlag)) {
          pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
          dirChangeFlag <- TRUE
        }
        intersectionFlag <- TRUE
      }
    }

    # Creating the Two Traces ----------------------------------------------------

    topTrace <- create_trace(traceMatrix = traceMatrices$TopTraceMatrix,
                             start = TopStartsEnds$Start,
                             end = TopStartsEnds$End,
                             topEnv = matrixEnvelopes$TopEnvelope,
                             bottomEnv =  matrixEnvelopes$TopLowerEnvelope,
                             thresh = CreateTraceThreshold,
                             MARange = MARange, region = region, loopNumber = loopNumber)
    bottomTrace <- create_trace(traceMatrix = traceMatrices$BottomTraceMatrix,
                                start =  BottomStartsEnds$Start,
                                end =  BottomStartsEnds$End,
                                topEnv = matrixEnvelopes$BottomUpperEnvelope,
                                bottomEnv = matrixEnvelopes$BottomEnvelope,
                                thresh = CreateTraceThreshold,
                                MARange = MARange, region = region,
                                loopNumber = loopNumber)

    # Checking for spikes in the traces ------------------------------------------
    if (isFALSE(improvement)){
      topTraceSpikeCheck <- tryCatch(spike_check(topTrace, spikeThreshold = spikeThreshold), warning = function(w) w)
      bottomTraceSpikeCheck <- tryCatch(spike_check(bottomTrace, spikeThreshold = spikeThreshold), warning = function(w) w)
      if (inherits(topTraceSpikeCheck, "warning") || inherits(bottomTraceSpikeCheck, "warning")) {
        if (!is.null(topTraceSpikeCheck)) {
          traceWarnings <- append(traceWarnings, topTraceSpikeCheck)
          if (isFALSE(intersectionFlag)){ # so we still get the full plot but it goes to the correct place
            if (isFALSE(dirChangeFlag)) {
              pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
              dirChangeFlag <- TRUE
            }
            intersectionFlag <- TRUE
          }
        }
        if (!is.null(bottomTraceSpikeCheck)) { # Sp we still get the full plot but it goes to the correct place
          traceWarnings <- append(traceWarnings, bottomTraceSpikeCheck)
          if (isFALSE(intersectionFlag)){
            if (isFALSE(dirChangeFlag)) {
              pathToWorkingDir <- paste0(pathToWorkingDir, "FailedToProcess/")
              dirChangeFlag <- TRUE
            }
            intersectionFlag <- TRUE
          }
        }
      }
    }

    #}


  }
  # Plotting (if applicable) ---------------------------------------------------
  time = FALSE
  if (isTRUE(plotPNG)){
    if (isFALSE(flag)) {
      time <- plot_success(imageMatrix = imageCut, rolledImage = rolledImage, topCut = topCut,
                   bottomCut = bottomCut, topStartEnds = TopStartsEnds, bottomStartEnds = BottomStartsEnds,
                   topTrace = topTrace, bottomTrace = bottomTrace, maxNoise = maxNoise, max_roc = maxEnvelopeROC,
                   sepDist = OffsetDistanceForEnvelopes, pathToWorkingDir = pathToWorkingDir, imageName = imageName,
                   intersectionFlag = intersectionFlag, plotEnv = plotEnvelopes) # adds fail to process dir on if intersection but keeps all info

      totalReturn <- list(ImageCutMatrix = imageCut, RolledImage = rolledImage,
                          PlotScaledEnvelopes = plotEnvelopes, TopTraceMatrix = topTrace,
                          TopTraceStartEnds = list(Start = TopStartsEnds$Start, End = TopStartsEnds$End),
                          BottomTraceMatrix = bottomTrace,
                          BottomTraceStartEnds = list(Start = BottomStartsEnds$Start, End = BottomStartsEnds$End),
                          Cuts = list(TopCut = bottomCut, BottomCut = topCut),
                          Warnings = traceWarnings)
    }
    if  (isTRUE(flag)) {
      time <- plot_with_warnings(imageMatrix = imageCut, topCut = topCut, bottomCut = bottomCut
                         ,pathToWorkingDir = pathToWorkingDir, imageName = imageName)

      totalReturn <- list(ImageCutMatrix = imageCut,
                          Cuts = list(TopCut = bottomCut, BottomCut = topCut),
                          Warnings = traceWarnings)
    }
  }
  else{
    if (isFALSE(flag)) {
      totalReturn <- list(ImageCutMatrix = imageCut, RolledImage = rolledImage,
                          PlotScaledEnvelopes = plotEnvelopes, TopTraceMatrix = topTrace,
                          TopTraceStartEnds = list(Start = TopStartsEnds$Start, End = TopStartsEnds$End),
                          BottomTraceMatrix = bottomTrace,
                          BottomTraceStartEnds = list(Start = BottomStartsEnds$Start, End = BottomStartsEnds$End),
                          Cuts = list(TopCut = topCut, BottomCut = bottomCut),
                          Warnings = traceWarnings)
    }
    if  (isTRUE(flag)) {
      totalReturn <- list(ImageCutMatrix = imageCut,
                          Cuts = list(TopCut = topCut, BottomCut = bottomCut),
                          Warnings = traceWarnings)
    }
  }


  if (isTRUE(saveData)) {
    if (time == FALSE){
      datePieces <- strsplit(as.character(Sys.time()), split = "-")
      timePieces <- strsplit(datePieces[[1]][3], ":")
      spaceRemove <- strsplit(timePieces[[1]][1], " ")
      time <- as.character(paste0(datePieces[[1]][1], datePieces[[1]][2],
                                  spaceRemove[[1]][1], spaceRemove[[1]][2],
                                  timePieces[[1]][2], timePieces[[1]][3]))
    }
    saveRDS(totalReturn, file = paste0(pathToWorkingDir, imageName,"-", time, "Data.RDS"))
  }
  return(c(totalReturn, newImageLoc = paste0(pathToWorkingDir, imageName,"-", time, "Data.RDS")))
}

