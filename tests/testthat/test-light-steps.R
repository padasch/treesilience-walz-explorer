make_step_fixture <- function(
    levels = c(0, 20, 50),
    rows_per_step = 241L,
    cadence_seconds = 1) {
  n <- length(levels) * rows_per_step
  setpoint <- rep(levels, each = rows_per_step)
  ppfd <- rep(levels * 12, each = rows_per_step)
  data <- data.frame(
    Datetime = as.POSIXct("2026-08-01 10:00:00", tz = WALZ_TIMEZONE) +
      (seq_len(n) - 1L) * cadence_seconds,
    `White x T` = setpoint,
    PARtop = ppfd,
    A = 0.02 * ppfd - 1 + seq_len(n) / n / 10,
    Tleaf = rep(seq(20, by = 2, length.out = length(levels)), each = rows_per_step),
    check.names = FALSE
  )
  list(data = data, units = character(), issues = character())
}

test_that("light-step extraction includes final three minutes and terminal step", {
  parsed <- make_step_fixture()
  extracted <- extract_light_steps(parsed, "run-a")

  expect_equal(nrow(extracted$summary), 3L)
  expect_equal(extracted$summary$step_id, 1:3)
  expect_true(all(extracted$summary$window_complete))
  expect_true(all(extracted$summary$include_model))
  expect_equal(extracted$summary$n_window, rep(181L, 3))
  expect_equal(extracted$summary$PPFD_mean, c(0, 240, 600))
  expect_equal(max(extracted$raw$.step_id), 3L)
  expect_true(any(extracted$raw$.in_extraction_window & extracted$raw$.step_id == 3L))
})

test_that("at most two isolated transient setpoints are repaired", {
  parsed <- make_step_fixture(levels = c(0, 50), rows_per_step = 241L)
  parsed$data[["White x T"]][100:101] <- 99
  extracted <- extract_light_steps(parsed, "run-a", max_transient_records = 2L)
  expect_equal(nrow(extracted$summary), 2L)
  expect_equal(sum(extracted$raw$.setpoint_repaired), 2L)
  expect_match(extracted$issues, "Repaired 2")

  parsed$data[["White x T"]][100:102] <- 99
  not_repaired <- extract_light_steps(parsed, "run-a", max_transient_records = 2L)
  expect_gt(nrow(not_repaired$summary), 2L)
})

test_that("short and malformed extraction windows are excluded", {
  parsed <- make_step_fixture(rows_per_step = 20L, cadence_seconds = 5)
  parsed$data$A[1:3] <- NA_real_
  extracted <- extract_light_steps(parsed, "run-a")
  expect_true(all(!extracted$summary$window_complete))
  expect_true(all(!extracted$summary$include_model))
  expect_true(any(grepl("Incomplete", extracted$summary$warning)))

  parsed$data$PARtop <- NULL
  missing <- extract_light_steps(parsed, "run-a")
  expect_equal(nrow(missing$summary), 0L)
  expect_match(missing$issues, "PARtop")
})
