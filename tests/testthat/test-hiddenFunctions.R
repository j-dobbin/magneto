context(desc = ".allowed_edge_distance")
test_that("returns correct values, checks for percentage i.e. less than 100",{
  rowSums <- rep(1, 100)
  ret <- data.frame(leftDist = 20, rightDist = 80)

  expect_equal(.allowed_edge_distance(rowSums, percentage = 20), ret)
  ret2 <- data.frame(leftDist = 20, rightDist = 40)
  expect_equal(.allowed_edge_distance(rowSums, percentageLeftSide = 20, percentage = 60), ret2)

})
test_that("returns the correct Error",{
  rowSums <- rep(1, 100)
  expect_error(.allowed_edge_distance(rowSums, percentage = 110),
               regexp = "Can't have a percentage over 100")
  expect_error(.allowed_edge_distance(rowSums, percentage = 110, percentageLeftSide = 90),
               regexp = "Can't have a percentage over 100")
  expect_error(.allowed_edge_distance(rowSums, percentage = 80, percentageLeftSide = 110),
               regexp = "Can't have a percentage over 100")
  expect_error(.allowed_edge_distance(rowSums, percentage = 80, percentageLeftSide = 50),
               regexp = "Can't have percentage plus percentageLeftSide be greater then")
})



context(desc = ".edge_peaks_rm")
test_that("returns findingPeaks in corrrect form, removes correct peaks", {
  findingPeaks <- data.frame(Index = c(26, 500, 1000, 1500, 2000), Height = rep(1000, 5))
  rowSums <- rep(1, 2500)
  findingPeaks90 <- data.frame(Index = c(500, 1000, 1500, 2000), Height = rep(1000, 4))
  findingPeaks80 <- data.frame(Index = c(1000, 1500), Height = rep(1000, 2))
  expect_equal(.edge_peaks_rm(findingPeaks, rowSums, percentEdge = 10), findingPeaks90)
  expect_equal(.edge_peaks_rm(findingPeaks, rowSums, percentEdge = 20 ), findingPeaks80)
  expect_equal(.edge_peaks_rm(findingPeaks, rowSums, percentEdge = 1), findingPeaks)

})
test_that("returns the correct Error",{
  findingPeaks <- data.frame(Index = c(1, 500, 1000, 1500, 2000), Height = rep(1000, 5))
  rowSums <- rep(1, 2500)
  expect_error(.edge_peaks_rm(c(1:100), rowSums, percentEdge = 20),
               regexp = "must be in the correct form")
  expect_error(.edge_peaks_rm(findingPeaks, rowSums = 5, percentEdge = 20),
               regexp = "rowSums should be at least the same length as your largest indexed peak")
})



context(desc = ".highest_peaks")
test_that("returns the correct highest peaks",{
  findingPeaks <- data.frame(Index = c(1, 500, 1000, 1500, 2000), Height = c(20,100,60,80,40))
  highestFour <- data.frame(Index = c(500, 1500, 1000, 2000), Height = c(100, 80, 60, 40))
  highestTwo <- data.frame(Index = c(500, 1500), Height = c(100, 80))
  expect_equal(.highest_peaks(findingPeaks, maxPeaksAllowed = 4), highestFour)
  expect_equal(.highest_peaks(findingPeaks, maxPeaksAllowed = 2), highestTwo)
  expect_equal(.highest_peaks(highestTwo, maxPeaksAllowed = 4), highestTwo)
})
test_that("Correct Error Reported",{
  findingPeaks <- data.frame(Index = c(1, 500, 1000, 1500, 2000), Height = c(20,100,60,80,40))
  expect_error(.highest_peaks(findingPeaks, maxPeaksAllowed = 0),
               regexp = "MaxPeaksAllowed must be more then 0")
  expect_error(.highest_peaks(c(1:100), maxPeaksAllowed = 2),
               regexp = "must be a dataframe")
})



context(desc = ".finding_distance_to_peaks")
test_that("Returns the correct outcome",{
  findingPeaks <- data.frame(Index = c(1, 500, 1100, 1500, 2000), Height = c(20,100,60,80,40))
  findingPeaksOne <- data.frame(Index = c(500), Height = c(100))
  rowSums <- rep(1, 2500)
  datThird <- data.frame(leftDist = 600 , rightDist = 400)
  datFifth <- data.frame(leftDist = 500, rightDist = 500)
  datFirst <- data.frame(leftDist = 0, rightDist = 499)
  datFirstOne <- data.frame(leftDist = 499, rightDist = 2000)
  expect_equal(.finding_distance_to_peaks(findingPeaks, rowSums = rowSums, peakIndex = 3), datThird)
  expect_equal(.finding_distance_to_peaks(findingPeaks, rowSums = rowSums, peakIndex = 5), datFifth)
  expect_equal(.finding_distance_to_peaks(findingPeaks, rowSums = rowSums, peakIndex = 1), datFirst)
  expect_equal(.finding_distance_to_peaks(findingPeaksOne, rowSums = rowSums, peakIndex = 1), datFirstOne)
  })
test_that("Correct Error Reported",{
  findingPeaks <- data.frame(Index = c(1, 500, 1100, 1500, 2000), Height = c(20,100,60,80,40))
  rowSums <- rep(1, 2500)
  expect_error(.finding_distance_to_peaks(findingPeaks, rowSums, 6),
              regexp = "Less peaks then peak index")

})


# need to add the test for the case of j == index
context(desc = ".finding_Peak_Start_Ends")
test_that("Returns the correct start and ends",{
  findingPeaks <- data.frame(Index = c(1708, 1494, 1037, 812),
                              Height = c(152775.73, 149577.55, 29652.66, 26414.92))
  expected <- data.frame(Index = c(1708, 1494, 1037, 812),
                         Height = c(152775.73, 149577.55, 29652.66, 26414.92),
                         PeakStart = c(1656, 1454, 973, 800),
                         PeakEnd = c(1760, 1534, 1101, 824))
  rowSums = readRDS("~/magneto/tests/testData/rowSums.RDS")
  ConstDecreasingPeak <- data.frame(Index = 1000, Height = 2)
  constDecExpected <- data.frame(Index = 1000, Height = 2, PeakStart = 1, PeakEnd = 1999)
  rowSumsDec = c(rep(1,999), 2, rep(1, 1000))
  expect_equal(.finding_Peak_Start_Ends(FindingPeaksdf = findingPeaks, rowSums = rowSums), expected)
  expect_equal(.finding_Peak_Start_Ends(ConstDecreasingPeak, rowSumsDec), constDecExpected)
})



context(desc = ".not_bright_image")
test_that("scales correctly with correct return", {
  image <- readRDS("~/magneto/tests/testData/nonBrightImage.RDS")
  expected <- readRDS("~/magneto/tests/testData/nonBrightRet.RDS")
  expect_equal(.not_bright_image(imageMatrix = image), expected)
})




context(desc = ".for_bright_image")
test_that("scales correctly with correct return", {
  image <- readRDS("~/magneto/tests/testData/brightImageRAW.RDS")
  expected <- readRDS("~/magneto/tests/testData/forBrightnessRet.RDS")
  expect_equal(.for_bright_image(imageMatrix = image), expected)
})


#NOTE: this function is becoming depreciated needs to be reworked or removed
context(desc = ".get_trace_start_ends")
test_that("correctStart and end found for an image",{
  image <- readRDS("~/magneto/tests/testData/StartEndTest-nonBright.RDS")
  expected <- list(Start = 700, End = 4927)
  expectedFullReturn <- readRDS("~/magneto/tests/testData/StartEndTestFullRet.RDS")
  expect_equal(.get_trace_start_ends(image, returnMat = FALSE), expected)
  expect_equal(.get_trace_start_ends(image, returnMat = TRUE), expectedFullReturn)
})



context(desc = ".top_image_cut")
test_that("correct top image cuttoff is found",{
  # WARNING, these images for this set of tests have a lot of pre-processing on it, use testingEnv
  #if you want to change it These are from batch 5 TODO, 1
  image <- readRDS("~/magneto/tests/testData/TopImageCut-BottomImageCut.RDS")
  expected <- 441
  expect_equal(.top_image_cut(image, percentEdgeForLeft = 25, percentFromEdge = 1), expected)
})
test_that("correct warning returned",{
  image <- matrix(c(1,0), nrow = 10, ncol = 10)
  expect_warning(.top_image_cut(image, percentEdgeForLeft = 25, percentFromEdge = 1),
                 regexp = "No top cuts found")
})




context(desc = ".bottom_image_cut")
test_that("correct bottom image cuttoff is found",{
  # WARNING, these images for this set of tests have a lot of pre-processing on it, use testingEnv
  #if you want to change it. These are from batch 5 TODO, 1, 21860, 3069, respectively
  image <- readRDS("~/magneto/tests/testData/TopImageCut-BottomImageCut.RDS")
  expected <- 1049
  expect_equal(.bottom_image_cut(image, percentEdgeForLeft = 25, percentFromEdge = 1), expected)
})
test_that("Correct Warnings returned to user",{
  imageIntersection <- readRDS("~/magneto/tests/testData/bottomIntersectionFound.RDS")
  expect_warning(.bottom_image_cut(imageIntersection, percentFromEdge = 1, percentEdgeForLeft = 25),
                 regexp = "Intersection in Timing Found")
  noCutImage <- readRDS("~/magneto/tests/testData/bottomNoCut.RDS")#this is a bad image
  expect_warning(.bottom_image_cut(noCutImage, percentFromEdge = 1, percentEdgeForLeft = 25),
                 regexp = "No cuts found.. defaulting to bottom of the image")
})



context(desc = ".process_image")
test_that("correct processed image gets returned to the user",{
  darkImage <- readRDS("~/magneto/tests/testData/StartEndTest-nonBright.RDS")
  brightImage <- readRDS("~/magneto/tests/testData/brightImageRAW.RDS")
  expectedDark <- readRDS("~/magneto/tests/testData/nonBrightExpected.RDS")
  expectedBright <- readRDS("~/magneto/tests/testData/brightExpected.RDS")
  expect_equal(.process_image(darkImage), expectedDark)
  expect_equal(.process_image(brightImage), expectedBright)
})



context(desc = ".find_a_number")
test_that("correct dataframe of indexes and lengths returned", {
  seq <- c(0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,0)
  expected <- data.frame(StartIndex = c(1,11,19,24), RunLength = c(6,4,4,1))
  expect_equal(.find_a_number(seq, specNumber = 0), expected)
})



context(desc = ".top_env")
test_that("correct line chosen for the top envelope for the image",{
  rolledImage <- readRDS("~/magneto/tests/testData/rolledImage.RDS")
  expectedRolledScaled <- readRDS("~/magneto/tests/testData/rolledTopEnv.RDS")
  expect_equal(.top_env(rolledImage, maxNoise = 250), expected = expectedRolledScaled)
})



context(desc = ".top_lower_env")
test_that("correct line chosen for the top lower envelope for the image",{
  rolledImage <- readRDS("~/magneto/tests/testData/rolledImage.RDS")
  expectedRolledScaled <- readRDS("~/magneto/tests/testData/rolledTopLowerEnv.RDS")
  expect_equal(.top_lower_env(rolledImage, maxNoise = 250), expected = expectedRolledScaled)
})



context(desc = ".bottom_upper_env")
test_that("correct line chosen for the bottom upper envelope for the image",{
  rolledImage <- readRDS("~/magneto/tests/testData/rolledImage.RDS")
  expectedRolledScaled <- readRDS("~/magneto/tests/testData/rolledBottomUpperEnv.RDS")
  expect_equal(.bottom_upper_env(rolledImage, maxNoise = 250), expected = expectedRolledScaled)
})



context(desc = ".bottom_env")
test_that("correct line chosen for the bottom envelope for the image",{
  rolledImage <- readRDS("~/magneto/tests/testData/rolledImage.RDS")
  expectedRolledScaled <- readRDS("~/magneto/tests/testData/rolledBottomEnv.RDS")
  expect_equal(.bottom_env(rolledImage, maxNoise = 250), expected = expectedRolledScaled)
})



context(desc = ".isolating_trace")
test_that("Correct isolated image is returned to the user",{
  image <- readRDS("~/magneto/tests/testData/matrixImageH-19260103.RDS")
  topEnvelope <- readRDS("~/magneto/tests/testData/MatTopEnv.RDS") # this is matrix scaled
  bottomEnvelope <- readRDS("~/magneto/tests/testData/MatTopLowerEnv.RDS")
  expected <- readRDS("~/magneto/tests/testData/TopIsolatedTraceH-19260103.RDS")
  WrongLengthEnv <- 1:100
  expect_equal(.isolating_trace(image, topEnv = topEnvelope, bottomEnv = bottomEnvelope), expected)
  expect_error(.isolating_trace(image, topEnv = WrongLengthEnv, bottomEnv = bottomEnvelope),
                 regexp = "ImageMatrix length differs from top or bottom env")
})



context(desc = ".get_image_year")
test_that("Correct year is returned to the user",{
  imageName1 <- "AGC--H-19260103-19260105.tif"
  imageName2 <- "AGC-H-19260103-19260105.tif"
  imageName3 <- "AGC-H-19341230-19341231-README.tif"
  imageName4 <- "AGC--H-19341230-19341231-README.tif"
  expected12 <- "1926"
  expected34 <- "1934"
  expect_equal(.get_image_year(imageName1), expected12)
  expect_equal(.get_image_year(imageName2), expected12)
  expect_equal(.get_image_year(imageName3), expected34)
  expect_equal(.get_image_year(imageName4), expected34)
})


#.dir_str is checked, just dont want to create dir, so didnt make a test for it.

