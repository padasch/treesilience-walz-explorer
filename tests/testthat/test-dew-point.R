test_that("Goff-Gratch saturation pressures match reference values", {
  pressure <- gfs_saturation_vapor_pressure(c(0, 20, 30))

  expect_equal(
    pressure,
    c(0.6103361, 2.3358468, 4.2405985),
    tolerance = 1e-7
  )
})

test_that("ppm and relative humidity give the same dew point", {
  tcuv_c <- 22
  pamb_kpa <- 100
  h2o_ppm <- 15000
  relative_humidity <- relative_humidity_from_h2o_ppm(
    h2o_ppm,
    pamb_kpa,
    tcuv_c
  )

  expect_equal(
    dew_point_from_h2o_ppm(h2o_ppm, pamb_kpa),
    dew_point_from_relative_humidity(tcuv_c, relative_humidity),
    tolerance = 1e-7
  )
})

test_that("WALZ normalized VPD converts to kPa using ambient pressure", {
  expect_equal(walz_vpd_to_kpa(11.64, 95.16), 1.1076624)
})

test_that("direct outlet and inlet-plus-leaf modes are equivalent", {
  direct <- calculate_dew_point_plan(
    water_mode = "outlet",
    tcuv_c = 22,
    tamb_c = 20,
    pamb_kpa = 100,
    outlet_h2o_ppm = 17000,
    safety_buffer_c = 2
  )
  estimated <- calculate_dew_point_plan(
    water_mode = "inlet_plus",
    tcuv_c = 22,
    tamb_c = 20,
    pamb_kpa = 100,
    inlet_h2o_ppm = 15000,
    leaf_h2o_added_ppm = 2000,
    safety_buffer_c = 2
  )

  expect_equal(direct$dew_point_c, estimated$dew_point_c)
  expect_equal(estimated$outlet_h2o_ppm, 17000)
  expect_equal(direct$internal_margin_c, estimated$internal_margin_c)
  expect_equal(direct$ambient_margin_c, estimated$ambient_margin_c)
})

test_that("cuvette and tubing statuses follow their own dew-point margins", {
  pamb_kpa <- 100
  target_dew_point <- 10
  h2o_ppm <-
    gfs_saturation_vapor_pressure(target_dew_point) /
    pamb_kpa * 1e6

  internal_danger <- calculate_dew_point_plan(
    "outlet", 12, 14, pamb_kpa, h2o_ppm, safety_buffer_c = 2
  )
  internal_caution <- calculate_dew_point_plan(
    "outlet", 13, 14, pamb_kpa, h2o_ppm, safety_buffer_c = 2
  )
  safe <- calculate_dew_point_plan(
    "outlet", 14.1, 14.1, pamb_kpa, h2o_ppm, safety_buffer_c = 2
  )
  tubing_danger <- calculate_dew_point_plan(
    "outlet", 20, 10, pamb_kpa, h2o_ppm, safety_buffer_c = 2
  )

  expect_equal(internal_danger$dew_point_c, target_dew_point, tolerance = 1e-6)
  expect_equal(internal_danger$internal_status, "danger")
  expect_equal(internal_danger$tubing_status, "safe")
  expect_equal(internal_caution$internal_status, "caution")
  expect_equal(safe$status, "safe")
  expect_equal(tubing_danger$tubing_status, "danger")
  expect_equal(safe$safety_threshold_c, 12, tolerance = 1e-6)
})

test_that("Tcuv above Tamb is context and does not override safe margins", {
  warmer_cuvette <- calculate_dew_point_plan(
    "outlet", 22, 20, 100,
    outlet_h2o_ppm = 15000,
    safety_buffer_c = 2
  )

  expect_true(warmer_cuvette$cuvette_above_ambient)
  expect_equal(warmer_cuvette$temperature_order_margin_c, -2)
  expect_equal(warmer_cuvette$internal_status, "safe")
  expect_equal(warmer_cuvette$tubing_status, "safe")
  expect_equal(warmer_cuvette$status, "safe")
})

test_that("planner plot compares all five safety temperatures", {
  skip_if_not_installed("plotly")
  plan <- calculate_dew_point_plan(
    "outlet", 22, 20, 100,
    outlet_h2o_ppm = 15000,
    safety_buffer_c = 2
  )
  widget <- make_dew_point_plan_plot(plan)
  built <- plotly::plotly_build(widget)

  expect_s3_class(widget, "plotly")
  expect_length(built$x$data, 5L)
  trace_names <- vapply(built$x$data, function(trace) trace$name, character(1))
  expect_true(any(grepl("Dew point + safety margin", trace_names, fixed = TRUE)))
  expect_true(any(grepl("Tcuv - 2°C", trace_names, fixed = TRUE)))
  expect_true(any(grepl("Tube/environment", trace_names, fixed = TRUE)))
})

test_that("Tcuv minus two is independent of the operational buffer", {
  without_buffer <- calculate_dew_point_plan(
    "outlet", 24, 20, 100,
    outlet_h2o_ppm = 15000,
    safety_buffer_c = 0
  )
  with_buffer <- calculate_dew_point_plan(
    "outlet", 24, 20, 100,
    outlet_h2o_ppm = 15000,
    safety_buffer_c = 5
  )

  expect_equal(without_buffer$internal_reference_c, 22)
  expect_equal(with_buffer$internal_reference_c, 22)
  expect_equal(without_buffer$internal_margin_c, with_buffer$internal_margin_c)
  expect_equal(
    with_buffer$safety_threshold_c - without_buffer$safety_threshold_c,
    5
  )
})

test_that("planner rejects nonphysical or incomplete inputs", {
  expect_error(
    calculate_dew_point_plan("outlet", 22, 20, 100, outlet_h2o_ppm = 0),
    "Expected chamber/outlet H2O"
  )
  expect_error(
    calculate_dew_point_plan(
      "inlet_plus", 22, 20, 100,
      inlet_h2o_ppm = 74000,
      leaf_h2o_added_ppm = 2000
    ),
    "must not exceed 75000"
  )
  expect_error(
    calculate_dew_point_plan("outlet", 22, 20, 20, outlet_h2o_ppm = 15000),
    "Ambient pressure"
  )
  expect_error(
    calculate_dew_point_plan("outlet", NA, 20, 100, outlet_h2o_ppm = 15000),
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

test_that("recorded audit summarizes actual condensation and buffer crossings", {
  parsed <- dew_point_fixture()
  dew_point_c <- dew_point_from_h2o_ppm(parsed$data$wa, parsed$data$Pamb)
  parsed$data$Tamb[[1]] <- dew_point_c[[1]]
  parsed$data$Tcuv[[2]] <- dew_point_c[[2]] + 3
  summary <- dew_point_audit_summary(parsed, safety_buffer_c = 2)

  expect_equal(summary$valid_count, nrow(parsed$data))
  expect_equal(summary$tubing$danger_count, 1L)
  expect_equal(summary$tubing$status, "danger")
  expect_gte(summary$internal$caution_count, 1L)
})

test_that("recorded audit reports missing and invalid inputs", {
  parsed <- dew_point_fixture()
  parsed$data$wa <- NULL
  expect_error(dew_point_audit_data(parsed), "missing required.*wa")
  expect_error(dew_point_audit_summary(parsed), "missing required.*wa")

  parsed <- dew_point_fixture()
  parsed$data$wa[] <- NA_real_
  expect_error(dew_point_audit_data(parsed), "no valid dew-point audit values")
  expect_error(dew_point_audit_summary(parsed), "no valid dew-point audit values")
})
