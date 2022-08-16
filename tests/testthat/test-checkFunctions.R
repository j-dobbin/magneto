context(desc = ".is_tiff")
test_that(".is_tiff returns the the correct bool", {
  vector <- c("test.tif", "test-@.tif", "test.tiff", "test-@.tiff", "test.png", "test-@.pdf")
  ret <- vector()
  for (i in 1:6) {
    ret[i] <- .is_tiff(vector[i])
  }
  expect_equal(ret, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})



context(desc = ".null")
test_that(".null returns an error if the specifed parameter is null",{

  expect_error(.null(parameter = NULL, parameterName = "john"),
               regexp = "Need to specify john")
  expect_null(.null(parameter = "four", parameterName = "age"))

})



context(desc = "not_empty_file")
test_that("Checking that returns correct bool",{
  expect_equal(not_empty_file("~/magneto/tests/testData/", "AGC-D-18610822-18610823.tif"), TRUE)
  expect_equal(not_empty_file("~/magneto/tests/testData/", "Emptyfile.png"), FALSE)


})
test_that("Correct Error Reported", {
  expect_error(not_empty_file("~/magneto/tests/testData/", "DoesntExistFile"),
               regexp = "doesn't exist")
  expect_error(not_empty_file(NA, "Emptyfile.png"),
               regexp = "missing filePath or fileName")
})



context(desc = ".horizontal_image_check")
test_that("Checking for a change from vertical to horozontal",{
  vertical <- readRDS("~/magneto/tests/testData/vertImagedf.RDS")
  horizontal <- readRDS("~/magneto/tests/testData/horImagedf.RDS")
  expect_equal(.horizontal_image_check(vertical), horizontal)
  expect_equal(.horizontal_image_check(horizontal), horizontal)
})



context(desc = ".HDV_check")
test_that("Correct HDV warning returned to user",{
  imageNameNonHDV <- "AGC--H-19260103-19260105.tif"
  imageNameHDV <- "AGC--HDZ-19260103-19260105.tif"
  nonImageName <- "FontMatter1.tif"
  nonImageName <- "Title.tif"
  expect_equal(.hdv_check(imageNameNonHDV), NULL)
  expect_warning(.hdv_check(imageNameHDV),
                 regexp = "This image is a HDV or HDZ")
  expect_warning(.hdv_check(nonImageName),
                 regexp = "This image is a HDV or HDZ")
  expect_warning(.hdv_check(nonImageName),
                 regexp = "This image is a HDV or HDZ")
})

