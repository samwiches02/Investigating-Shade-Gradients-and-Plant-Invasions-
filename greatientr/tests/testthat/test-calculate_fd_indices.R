library(greatientr)
library(testthat)

# test_that("calculate_fd_indices works correctly", {
# Create sample test data
test_traits <- data.frame(
  species = c("sp1", "sp2", "sp3"),
  la = c(0.5, 0.4, 0.3),
  ldmc = c(0.3, 0.2, 0.4),
  sla = c(0.2, 0.6, 0.5)
)

test_abundance <- data.frame(
  site = c("site1", "site2", "site3"),
  sp1 = c(10, 5, 8),
  sp2 = c(4, 7, 3),
  sp3 = c(6, 8, 12)
)



# Calculate FD indices
result <- calculate_fd_indices(test_traits, test_abundance)

# Test structure of output
expect_type(result, "list")
expect_true("FRic" %in% names(result))
expect_true("FEve" %in% names(result))
expect_true("FDiv" %in% names(result))

# Test value ranges
expect_true(all(result$FRic >= 0))
expect_true(all(result$FEve >= 0 & result$FEve <= 1))
expect_true(all(result$FDiv >= 0 & result$FDiv <= 1))

# Test error handling
expect_error(calculate_fd_indices(
  test_traits[,-1], # Missing species column
  test_abundance
))

# Test with mismatched species
test_traits_mismatch <- test_traits
test_traits_mismatch$species[1] <- "wrong_species"
expect_error(calculate_fd_indices(
  test_traits_mismatch,
  test_abundance
))
})
