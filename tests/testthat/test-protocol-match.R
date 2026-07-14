current_protocols <- data.frame(
  id = paste0("protocol-", 1:4),
  name = c(
    "20260713_1023_chamber_prunus_area10.txt",
    "20260713_1023_chamber_oak(10).txt",
    "20260709_1055_lightFlucScript_oak.txt",
    "20260708_1005_lightFlucScript_beech2.txt"
  ),
  stringsAsFactors = FALSE
)

test_that("all current measurement names resolve to their intended protocol", {
  expectations <- c(
    "20260713_1023_chamber_oak(area10)_postblackout.csv" =
      "20260713_1023_chamber_oak(10).txt",
    "20260713_1023_chamber_prunus_area10_postblackout.csv" =
      "20260713_1023_chamber_prunus_area10.txt",
    "20260709_1055_lightFlucScript_oak(witharea10).csv" =
      "20260709_1055_lightFlucScript_oak.txt",
    "20260708_1005_beech2(witharea10).csv" =
      "20260708_1005_lightFlucScript_beech2.txt",
    "20260708_1005_beech2.csv" =
      "20260708_1005_lightFlucScript_beech2.txt"
  )

  for (measurement in names(expectations)) {
    matched <- match_protocol(measurement, current_protocols)
    expect_equal(matched$status, "matched", info = measurement)
    expect_equal(matched$protocol$name[[1]], expectations[[measurement]], info = measurement)
  }
})

test_that("exact stems take priority", {
  protocols <- data.frame(
    name = c("20260101_1200_leaf.txt", "20260101_1200_other.txt"),
    stringsAsFactors = FALSE
  )
  matched <- match_protocol("20260101_1200_leaf.csv", protocols)

  expect_equal(matched$status, "matched")
  expect_equal(matched$method, "exact filename stem")
})

test_that("ambiguous and missing names never produce guesses", {
  ambiguous <- data.frame(
    name = c("20260101_1200_oak.txt", "20260101_1200_beech.txt"),
    stringsAsFactors = FALSE
  )
  ambiguous_result <- match_protocol("20260101_1200_maple.csv", ambiguous)
  missing_result <- match_protocol(
    "20260202_1300_maple.csv",
    ambiguous
  )
  empty_result <- match_protocol(
    "20260202_1300_maple.csv",
    data.frame(name = character())
  )

  expect_equal(ambiguous_result$status, "ambiguous")
  expect_null(ambiguous_result$protocol)
  expect_match(ambiguous_result$message, "No protocol was guessed")
  expect_equal(missing_result$status, "missing")
  expect_null(missing_result$protocol)
  expect_equal(empty_result$status, "missing")
})
