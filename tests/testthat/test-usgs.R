test_that("usgs functions", {


  # testing the site api
  # doesn't work well with read.table because of no space at the end of the csv.
  # changing to read.csv seems to fix this error.

  i <- 12347000
  base_url_site <- paste0("https://waterservices.usgs.gov/nwis/site/?format=rdb,1.0&sites=",
                          i,"&siteStatus=all"
  )

  # try to download the data
  error_site <- httr::GET(url = base_url_site,
                          httr::write_disk(path = file.path(tempdir(),
                                                            "usgs_tmp.csv"),
                                           overwrite = TRUE))
  # read RDB file to R

  testthat::expect_equal(nrow(read.csv(
    file.path(tempdir(),"usgs_tmp.csv"),
    sep = "\t",
    comment.char = '#')), 2)

})
