context("Calculation functions")

test_that("Calculate some fuel specific emissions", {
  
  # Ratio calculation, very simple
  expect_equal(pollutant_to_co2_ratio(0.02462312, 14.818), 0.001661703)
  
  # no
  expect_equal(
    pollutant_ratio_to_fuel_specific_emission(
      ratio_pollutant = 0.001661703,
      pollutant = "no",
      ratio_co = 0.01273401,
      ratio_hc = 0.0069588
    ),
    3.445004,
    tolerance = 1e-07)
  
  # no2
  expect_equal(
    pollutant_ratio_to_fuel_specific_emission(
      ratio_pollutant = -6.63e-05,
      pollutant = "no2",
      ratio_co = 0.01273401,
      ratio_hc = 0.0069588
    ),
    -0.2109028,
    tolerance = 1e-03)
  
  # Non negative example, uses different co and hc observations
  expect_equal(
    pollutant_ratio_to_fuel_specific_emission(
      ratio_pollutant = 0.001263127,
      pollutant = "no2",
      ratio_co = -0.006120965,
      ratio_hc = 6.77e-05
    ),
    4.174981,
    tolerance = 1e-07)
  
  # hc
  expect_equal(
    pollutant_ratio_to_fuel_specific_emission(
      ratio_pollutant = 0.0069588,
      pollutant = "hc",
      ratio_co = 0.01273401,
      ratio_hc = 0.0069588
    ),
    42.31868,
    tolerance = 1e-07)
  
  # nh3
  expect_equal(
    pollutant_ratio_to_fuel_specific_emission(
      ratio_pollutant = -5.64e-05,
      pollutant = "nh3",
      ratio_co = 0.01273401,
      ratio_hc = 0.0069588
    ),
    -0.0662691,
    tolerance = 1e-04)
  
  # co
  expect_equal(
    pollutant_ratio_to_fuel_specific_emission(
      ratio_pollutant = 0.01273401,
      pollutant = "co",
      ratio_co = 0.01273401,
      ratio_hc = 0.0069588
    ),
    24.63987,
    tolerance = 1e-06)
  
  # Test nox calculation, nox as no2 equivalent
  expect_equal(nox_as_no2_equivalent(6.960671, 4.174981), 14.84801)
  
})
