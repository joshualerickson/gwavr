test_that("noaa api", {

  clat <- 48.8146
  clng <- -114.8211
  data_type <- 'depth'
  units <- 'english'
  series <- 'pds'

  pf <- paste0("https://hdsc.nws.noaa.gov/cgi-bin/new/cgi_readH5.py?lat=",
               clat,"&lon=", clng, "&type=pf&data=",data_type,"&units=",units,"&series=", series)

  error_pf <- httr::GET(url = pf,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "noaa_tmp.html"),overwrite = TRUE))

  noaa_pf <- jsonlite::toJSON(readLines(file.path(tempdir(),"noaa_tmp.html")))

  noaa_pf_r <- jsonlite::fromJSON(noaa_pf)[3:5]

  noaa_pf_r <- dplyr::tibble(
    quantiles = readr::parse_number(stringr::str_split(stringr::str_remove_all(noaa_pf_r[1], "\\,|\\[|\\]|quantiles = |;|'"), ' ')[[1]]),
    upper = readr::parse_number(stringr::str_split(stringr::str_remove_all(noaa_pf_r[2], "\\,|\\[|\\]|upper = |;|'"), ' ')[[1]]),
    lower = readr::parse_number(stringr::str_split(stringr::str_remove_all(noaa_pf_r[3], "\\,|\\[|\\]|lower = |;|'"), ' ')[[1]])
  )

testthat::expect_equal(noaa_pf_r[1,1]$quantiles, 0.159)

  })
