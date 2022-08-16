context(desc = "tiff_import")
test_that("tiff_import returns only tiffs", {
  testFile <- tiff_import(fileName = "AGC-D-18610822-18610823.tif", fileLoc = "~/magneto/tests/testData/")
  expect_equal_to_reference(testFile, update = FALSE, file = "~/magneto/tests/testData/tiffimport1861.RDS")

})
test_that("Correct Error Reported",{
  expect_error(tiff_import("DoesNotExist.png", "~/magneto/tests/testData/"),
               regexp = "doesn't exist")
  expect_error(tiff_import("test.tif", c("~/magneto/tests/testData/", "extraThing")),
               regexp = "fileLoc must be a single character")
  expect_error(tiff_import(c("test.tif", "test.tif"), "~/magneto/tests/testData/"),
               regexp = "fileName must be a single character")
  expect_error(tiff_import(fileName = "Emptyfile.png", fileLoc = "~/magneto/tests/testData/"),
               regex = "not a .tif or .tiff")
  expect_error(tiff_import(fileName = "Emptyfile.tiff", fileLoc = "~/magneto/tests/testData/"),
               regexp = "is 0b/Empty")
})


context(desc = "bright")
test_that("bright returns correct decision for correct cutoff and scales if there are values
          greater then 1 in the matrix", {
  expect_equal(bright(imageMatrix = matrix(c(0,0,0,0.8), byrow = TRUE, nrow = 2)), TRUE)
  expect_equal(bright(imageMatrix = matrix(c(0,0,0,0.7), byrow = TRUE, nrow = 2)), FALSE)
})
test_that("Correct Error Reported/Correct Warning Reported",{
  expect_error(bright(data.frame(testing = 1:5, column = 1:5)),
               regexp = "imageMatrix must be a matrix")
  expect_warning(bright(matrix(c(0.1,0.1,0.1,7), byrow = TRUE, nrow = 2)),
                 regexp = "Shouldnt be pixels with brightness above 1...scaling by largest value")
  expect_warning(bright(matrix(c(0.1,0.2,0.7,NA), byrow = TRUE, nrow = 2)),
                 regexp = "NA's found in the matrix.. setting them to 0")
})



context(desc = "find_peaks")
test_that("Correct return given to user", {
  rowSums <- readRDS("~/magneto/tests/testData/rowSums.RDS")
  expectedRet <- readRDS("~/magneto/tests/testData/FullPeakStartEnd.RDS")
  expect_equal(find_peaks(rowSums = rowSums, minDistance = 100, maxPeakNumber = 4,
                          percentFromEdge = 10, plots = FALSE), expectedRet)
})
test_that("Correct Error Reported/Correct Warning Reported", {
  rowSums <- readRDS("~/magneto/tests/testData/rowSums.RDS")
  rowSumsNoPeaks <- rep(1, 1000)
  expect_error(find_peaks("This is not a numeric vector", minDistance = 100, maxPeakNumber = 4,
                          percentFromEdge = 10, plots = FALSE),
              regexp = "rowSums must be a vector")
  expect_error(find_peaks(rowSumsNoPeaks, minDistance = 100, maxPeakNumber = 4,
                          percentFromEdge = 10, plots = FALSE),
               regexp = "No Peaks Found...")
})



context(desc = "find_paths_for_keyword")
test_that("Returns the correct values",{
  expected <- "/home/ben/magneto/tests/testData/tiffimport1861.RDS"
  #NOTE:Symlink is on my profile, will have to change to test if on another
  expectedSymLink <- "/home/ben/magnetoMARK/Scripts/MarkW/Thesis_Comparison.R"
  expect_equal(find_paths_for_keyword(path = "~/magneto/tests", keyword = "tiffimport1861"), expected)
  # dir used to be not accessable to me, thats why it didn't work, now can see it, havent changed anything still works though
  # expect_warning(expect_equal(find_paths_for_keyword(path = "~/", keyword = "Thesis"), expectedSymLink),
  #                regexp = "Failed to search directory")
})
test_that("Returns The Correct Error",{
  expect_error(find_paths_for_keyword(path = "~/magneto/tests", keyword = "thereIsNoFileWithThisName"),
               regexp = "No files found with that name")
})


# this is not needed anymore
# context(desc = "deconv_gauss")
# test_that("Returns expected value", {
#   image <- matrix(data = c(1:12), nrow = 4, ncol = 6)
#   expected <- c(-3.1587712, -3.5025280, -3.6482256, -3.7332883,
#                 -3.7663271, -3.3626800, -1.7124795, 1.8264546,
#                 6.8017323, 11.3811585, 13.3243517, 11.5739424,
#                 7.0550070,  1.9330442, -1.9662331, -4.1387981,
#                 -5.0355809, -4.9397223, -3.3559227,  0.4073428,
#                 6.0395086, 11.7147557, 15.0063607, 14.5793650)
#   expect_equal((deconv_gauss(imageMatrix = image)), expected)
# })



context(desc = "import_process_image")
test_that("Retruns the correct processed image",{
  fileName <- "AGC-D-19211221-19211223.tif"
  fileLocation <- "/home/ben/magnetoMARK/Images/Range/AGC-D-19210101-19240305/"
  expected <- readRDS("~/magneto/tests/testData/processedAGC-D-19211221.RDS")
  expect_equal(import_process_image(fileName, fileLocation), expected)
})



context(desc = "mean_roll_image")
test_that("rolls the correct way", {
  imageMatrix <- readRDS("~/magneto/tests/testData/matrixImageH-19260103.RDS")
  expected <- readRDS("~/magneto/tests/testData/rolledImage.RDS")
  topcut <- 612
  bottomcut <- 1114
  expect_equal(mean_roll_image(imageMatrix, topcut = topcut, bottomcut = bottomcut), expected)
})



context(desc = "find_envelopes")
test_that("corret envelope is found for an image", {
  imageMatrix <- readRDS("~/magneto/tests/testData/matrixImageH-19260103.RDS")
  rolledImage <- readRDS("~/magneto/tests/testData/rolledImage.RDS")
  expectedPlotting <- readRDS("~/magneto/tests/testData/envPlottingH-19260103.RDS")
  expectedMatrix <- readRDS("~/magneto/tests/testData/envMatrixH-19260103.RDS")
  expectedRolled <- readRDS("~/magneto/tests/testData/envRolledH-19260103.RDS")
  bottomCut <- 1114
  expect_equal(find_envelopes(rolledImage = rolledImage,imageMatrix =  imageMatrix, bottomCut, returnType = "PlottingScaled"), expectedPlotting)
  expect_equal(find_envelopes(rolledImage = rolledImage, imageMatrix = imageMatrix, bottomCut, returnType = "MatrixScaled"), expectedMatrix)
  expect_equal(find_envelopes(rolledImage = rolledImage,imageMatrix =  imageMatrix, bottomCut, returnType = "RolledImageScaled"), expectedRolled)
  expect_error(find_envelopes(rolledImage = rolledImage, imageMatrix = imageMatrix, bottomCut, returnType = "notCorrect"),
              regexp = "returnType is not correct, please look at documentation")
})



context(desc = "triple_check")
test_that("Triple is identified and non triple is okay", {
  triple <- readRDS("~/magneto/tests/testData/imageProcessedTripleD-18880915.RDS")
  bottomcutTrip <- readRDS("~/magneto/tests/testData/bottomcutD-18880915.RDS")
  bottomcutDub <- readRDS("~/magneto/tests/testData/bottomCutH-19260103.RDS")
  topcutTrip <- readRDS("~/magneto/tests/testData/topcutD-18880915.RDS")
  topcutDub <- readRDS("~/magneto/tests/testData/topCutH-19260103.RDS")
  double <- readRDS("~/magneto/tests/testData/matrixImageH-19260103.RDS")
  expectedTriple <- TRUE
  expectedDuble <- FALSE
  expect_warning(triple_check(triple, topCut = topcutTrip, bottomCut = bottomcutTrip),# expectedTriple)
                 regexp = "Possible Triple Found")
  expect_equal(triple_check(double, topCut = topcutDub, bottomCut = bottomcutDub), expectedDuble)
})



context(desc = "intersection_check")
test_that("Intersection identified", {
  imageName <-  "AGC-D-19211221-19211223.tif"
  topLowerEnv <- readRDS("~/magneto/tests/testData/topLowerIntersection.RDS")
  bottomUpperEnv <- readRDS("~/magneto/tests/testData/bottomUpperIntersection.RDS")
  expect_warning(intersection_check(topEnv = topLowerEnv, bottomEnv = bottomUpperEnv, imageName = imageName),
                 regexp = "There is an intersection at")
})




context(desc = "env_start_end")
test_that("correct start and end found for an isolated trace", {
  image <- readRDS("~/magneto/tests/testData/TopIsolatedTraceH-19260103.RDS") # this is a single trace matrix
  expected <- list(Start = 312, End = 5869)
  expect_equal(env_start_end(image, returnMatrix = FALSE), expected)
})




context(desc = "trim_top_bottom")
test_that("correct trimmed returned to the user",{
  image <- matrix(1, nrow = 10, ncol = 10)
  expected6 <- matrix(1, nrow = 6, ncol = 10)
  expected8 <- matrix(1, nrow = 8, ncol = 10)
  expect_equal(trim_top_bottom(image, trimAmountTop = 2, trimAmountBottom = 2), expected6)
  expect_equal(trim_top_bottom(image, trimAmountTop = 0, trimAmountBottom = 2), expected8)
  expect_equal(trim_top_bottom(image, trimAmountTop = 2, trimAmountBottom = 0), expected8)
})



context(desc = "create_trace")
test_that("correct trace returned for image",{
  traceMatrix <- readRDS("~/magneto/tests/testData/AGC--H-19260111-19260113.tif-TraceTopMat.RDS")
  startEnds <- readRDS("~/magneto/tests/testData/H-19260111-StartEndsTopMat.RDS")
  topEnv <- readRDS("~/magneto/tests/testData/H-19260111-TopEnv.RDS")
  lowerEnv <- readRDS("~/magneto/tests/testData/H-19260111-TopLowerEnv.RDS")
  expected <- readRDS("~/magneto/tests/testData/H-19260111-Envelope.RDS")
  expect_equal(create_trace(traceMatrix, start = startEnds$Start, end = startEnds$End,
                            topEnv = topEnv, bottomEnv  = lowerEnv), expected)

})




context(desc = "find_cuts")
test_that("correct top image cuttoff is found",{
  # WARNING, these images for this set of tests have a lot of pre-processing on it, use testingEnv
  #if you want to change it These are from batch 5 TODO, 1
  image <- readRDS("~/magneto/tests/testData/TopImageCut-BottomImageCut.RDS")
  expected <- list(TopCut = 441, BottomCut = 1049)
  expect_equal(find_cuts(image, percentEdgeForLeft = 25, percentFromEdge = 1), expected)
})
test_that("correct warning returned",{
  image <- matrix(c(1,0), nrow = 10, ncol = 10)
  expect_warning(.top_image_cut(image, percentEdgeForLeft = 25, percentFromEdge = 1),
                 regexp = "No top cuts found")
  imageIntersection <- readRDS("~/magneto/tests/testData/bottomIntersectionFound.RDS")
  expectedIntersection <- readRDS("~/magneto/tests/testData/find_cutsIntersction.RDS")
  expect_equal(find_cuts(imageIntersection, percentFromEdge = 1, percentEdgeForLeft = 25), expectedIntersection)
  noCutImage <- readRDS("~/magneto/tests/testData/bottomNoCut.RDS")#this is a bad image
  expectedDefault <- readRDS("~/magneto/tests/testData/find_cutsDefaultToBottom.RDS")
  expect_equal(find_cuts(noCutImage, percentFromEdge = 1, percentEdgeForLeft = 25),
               expectedDefault)
})



context(desc = "isolate_traces")
test_that("returns the correct separated traces",{
  imageMatrix <- readRDS("~/magneto/tests/testData/H-19260111-ImageProcessed.RDS")
  topEnv <- readRDS("~/magneto/tests/testData/H-19260111-TopEnv.RDS")
  topLowerEnv <- readRDS("~/magneto/tests/testData/H-19260111-TopLowerEnv.RDS")
  bottomUpperEnv <- readRDS("~/magneto/tests/testData/H-19260111-BottomUpperEnv.RDS")
  bottomEnv <- readRDS("~/magneto/tests/testData/H-19260111-BottomEnv.RDS")
  expected <- readRDS("~/magneto/tests/testData/H-19260111-TraceMats.RDS")
  expect_equal(isolate_traces(imageMatrix, topEnvelope = topEnv, topLowerEnvelope = topLowerEnv,
                              bottomUpperEnvelope = bottomUpperEnv, bottomEnvelope = bottomEnv),
               expected = expected)
})



context(desc = "trim_sides")
test_that("returns the correctly trimmed Image",{
  image <- matrix(1, ncol = 100, nrow = 2)
  expectedRightAndLeft <- matrix(1, ncol = 96, nrow = 2)
  expectedRightOrLeft <- matrix(1, ncol = 98, nrow = 2)
  expect_equal(trim_sides(image, trimAmountLeft = 2, trimAmountRight = 2), expectedRightAndLeft)
  expect_equal(trim_sides(image, trimAmountLeft = 2, trimAmountRight = 0), expectedRightOrLeft)
  expect_equal(trim_sides(image, trimAmountLeft = 0, trimAmountRight = 2), expectedRightOrLeft)
})



context(desc = "spike_check")
test_that("returns correct warning if there is a spike",{
  image <- c(1,1,1,1,9,1,1,1,1,1)
  imageNoSpike <- c(1,1,1,1,1,1,1,1,1,1,1)
  expect_equal(spike_check(imageNoSpike, spikeThreshold = 5), NULL)
  expect_warning(spike_check(image, spikeThreshold = 5),
                 regexp = "abnormal spike in the top trace")
})



context(desc = "TIS")
test_that("returns correctly under all possible outcomes",{
  imageNameGood <- "AGC--H-19260111-19260113.tif"
  imagePathGood <- "/home/ben/magnetoMARK/Images/Range/AGC-H-19260101-19281230/"
  expectedGood <- readRDS("~/magneto/tests/testData/AGC--H-19260111-Fulldigitization.RDS")
  expect_equal(TIS(imageNameGood, fileLoc = imagePathGood, HDVcheck = TRUE), expectedGood)
  # Errors or Warnings
  imageNameBadHDV <- "AGC-HDV-19390226-19390227.tif"
  imagePathBadHDV <- "/home/ben/magnetoMARK/Images/Range/AGC-V-19390101-19391231(VDH)/"
  expect_warning(TIS(imageNameBadHDV, imagePathBadHDV, HDVcheck = TRUE),
                 regexp = "This image is a HDV or HDZ")
  imageNameWrongType <- "AGC-D-18640423-18800710 READ ME.docx"
  imagePathWrongType <- "/home/ben/magnetoMARK/Images/AGC-D-18640423-18800710(TOR)/"
  expect_equal(TIS(imageNameWrongType, imagePathWrongType, HDVcheck = TRUE),
               expected = "Error in tiff_import(fileName = imageName, fileLoc = file_loc): fileAGC-D-18640423-18800710 READ ME.docx is not a .tif or .tiff\n")
  imageNameTriple <- "AGC-D-18841220-18841224.tif"
  imagePathTriple <- "/home/ben/magnetoMARK/Images/AGC-D-18800710-18850330/"
  expectedTriple <- "Possible Triple Found"
  expect_equal(TIS(imageNameTriple, imagePathTriple, HDVcheck = TRUE)$Warnings[[1]], expectedTriple)
  imageNameNoCut <- "AGC-D-18990804-18990806.tif"
  imagePathNoCut <- "/home/ben/magnetoMARK/Images/AGC-D-18980111-19010529/"
  expectedPathNoCut <- "No cuts found.. defaulting to bottom of the image"
  expect_equal(TIS(imageNameNoCut, imagePathNoCut, HDVcheck = TRUE)$Warnings$message, expectedPathNoCut)
  imageNameIntersection <- "AGC-D-19210712-19210714.tif"
  imagePathIntersection <- '/home/ben/magnetoMARK/Images/Range/AGC-D-19210101-19240305/'
  expectedIntersection <- "There is an intersection at (622, 3027)"
  expect_equal(TIS(imageNameIntersection, imagePathIntersection, HDVcheck = TRUE)$Warnings$message, expectedIntersection)
})
