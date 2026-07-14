test_that("Goff-Gratch saturation pressures match reference values", {
  pressure <- gfs_saturation_vapor_pressure(c(0, 20, 30))

  expect_equal(
    pressure,
    c(0.6103361, 2.3358468, 4.2405985),
    tolerance = 1e-7
  )
})

test_that("ppm and relative-humidity modes give the same dew point", {
  tcuv_c <- 22
  pamb_kpa <- 100
  h2o_ppm <- 15000
  relative_humidity <- relative_humidity_from_h2o_ppm(
    h2o_ppm,
    pamb_kpa,
    tcuv_c
  )

  from_ppm <- dew_point_from_h2o_ppm(h2o_ppm, pamb_kpa)
  from_rh <- dew_point_from_relative_humidity(tcuv_c, relative_humidity)

  expect_equal(from_ppm, from_rh, tolerance = 1e-7)
})

test_that("planner status follows dew point and the separate safety buffer", {
  pamb_kpa <- 100
  target_dew_point <- 10
  h2o_ppm <-
    gfs_saturation_vapor_pressure(target_dew_point) /
    pamb_kpa * 1e6

  danger <- calculate_dew_point_plan(
    "ppm", 12, 12, pamb_kpa, h2o_ppm, safety_buffer_c = 2
  )
  caution <- calculate_dew_point_plan(
    "ppm", 13, 13, pamb_kpa, h2o_ppm, safety_buffer_c = 2
  )
  safe <- calculate_dew_point_plan(
    "ppm", 14.1, 14.1, pamb_kpa, h2o_ppm, safety_buffer_c = 2
  )

  expect_equal(danger$dew_point_c, target_dew_point, tolerance = 1e-6)
  expect_equal(danger$status, "danger")
  expect_equal(caution$status, "caution")
  expect_equal(safe$status, "safe")
  expect_equal(safe$minimum_ambient_c, 14.1, tolerance = 1e-6)
})

test_that("planner warns whenever Tcuv is warmer than Tamb", {
  warning <- calculate_dew_point_plan(
    "ppm", 22, 20, 100, h2o_ppm = 15000, safety_buffer_c = 2
  )
  coupled <- calculate_dew_point_plan(
    "ppm", 22, 22, 100, h2o_ppm = 15000, safety_buffer_c = 2
  )

  expect_equal(warning$status, "caution")
  expect_true(warning$cuvette_above_ambient)
  expect_equal(warning$temperature_order_margin_c, -2)
  expect_equal(warning$minimum_ambient_c, 22)
  expect_false(coupled$cuvette_above_ambient)
  expect_equal(coupled$temperature_order_margin_c, 0)
  expect_equal(coupled$status, "safe")
})

test_that("Tcuv minus two is independent of the operational buffer", {
  without_buffer <- calculate_dew_point_plan(
    "rh", 24, 20, 100, relative_humidity = 50, safety_buffer_c = 0
  )
  with_buffer <- calculate_dew_point_plan(
    "rh", 24, 20, 100, relative_humidity = 50, safety_buffer_c = 5
  )

  expect_equal(without_buffer$internal_reference_c, 22)
  expect_equal(with_buffer$internal_reference_c, 22)
  expect_equal(
    without_buffer$internal_margin_c,
    with_buffer$internal_margin_c
  )
  expect_equal(
    with_buffer$dew_point_minimum_ambient_c -
      without_buffer$dew_point_minimum_ambient_c,
    5
  )
})

test_that("planner rejects nonphysical or incomplete inputs", {
  expect_error(
    calculate_dew_point_plan("ppm", 22, 20, 100, h2o_ppm = 0),
    "Expected chamber H2O"
  )
  expect_error(
    calculate_dew_point_plan("rh", 22, 20, 100, relative_humidity = 0),
    "Relative humidity"
  )
  expect_error(
    calculate_dew_point_plan("ppm", 22, 20, 20, h2o_ppm = 15000),
    "Ambient pressure"
  )
  expect_error(
    calculate_dew_point_plan("ppm", NA, 20, 100, h2o_ppm = 15000),
    "Cuvette temperature"
  )
})

test_that("conservative run values use the requested summaries", {
  parsed <- dew_point_fixture()
  values <- conservative_run_values(parsed)

  expect_equal(values$h2o_ppm, max(parsed$data$wa))
  expect_equal(values$tamb_c, min(parsed$data$Tamb))
  expect_equal(values$tcuv_c, median(parsed$data$Tcuv))
  expect_equal(values$pamb_kpa, median(parsed$data$Pamb))
})

test_that("recorded audit contains exactly the four requested series", {
  skip_if_not_installed("plotly")
  parsed <- dew_point_fixture()
  audit <- dew_point_audit_data(parsed)

  expect_equal(levels(audit$Series), DEW_POINT_SERIES)
  expect_equal(nrow(audit), nrow(parsed$data) * 4)
  expect_false(any(grepl("Tmin", as.character(audit$Series), fixed = TRUE)))
  internal <- audit[
    audit$Series == "Estimated coldest internal point (Tcuv - 2°C)",
    ,
    drop = FALSE
  ]
  expect_equal(internal$Temperature, parsed$data$Tcuv - 2)

  widget <- make_dew_point_audit_plot(parsed)
  expect_s3_class(widget, "plotly")
  expect_length(widget$x$data, 4L)
})

test_that("recorded audit summarizes Tcuv above Tamb without adding a series", {
  parsed <- dew_point_fixture()
  summary <- dew_point_temperature_order_summary(parsed)

  expect_equal(summary$valid_count, nrow(parsed$data))
  expect_equal(summary$warning_count, nrow(parsed$data))
  expect_equal(
    summary$maximum_excess_c,
    max(parsed$data$Tcuv - parsed$data$Tamb)
  )
})

test_that("recorded audit reports missing and invalid inputs", {
  parsed <- dew_point_fixture()
  parsed$data$wa <- NULL
  expect_error(dew_point_audit_data(parsed), "missing required.*wa")

  parsed <- dew_point_fixture()
  parsed$data$wa[] <- NA_real_
  expect_error(dew_point_audit_data(parsed), "no valid dew-point audit values")
})
