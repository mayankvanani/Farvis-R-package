test_that(" Check extra Data or file names are not appended", {
  testthat::expect_that(fars_summarize_years(2017), throws_error())
  testthat::expect_that(fars_summarize_years(2012), throws_error())
})



## link to video
## https://www.youtube.com/watch?v=u2KDSY_8Ay4
