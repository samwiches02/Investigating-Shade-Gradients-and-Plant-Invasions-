#' Calculate Functional Diversity Indices
#'
#' @importFrom FD dbFD
#' @param trait_matrix DataFrame containing species traits
#' @param abundance_matrix DataFrame containing species abundance by site
#' @return List of FD indices (FRic, FEve, FDiv)
#' @export
#'
calculate_fd_indices <- function(trait_matrix, abundance_matrix) {
  # Prepare trait matrix
  traits <- trait_matrix[, c("la", "ldmc", "sla")]
  rownames(traits) <- trait_matrix$species

  # Prepare abundance matrix
  abund <- abundance_matrix[, -1]
  rownames(abund) <- abundance_matrix[, 1]

  # Calculate FD indices
  fd_indices <- dbFD(traits, abund, w.abun = TRUE,
                     stand.x = TRUE, corr = "cailliez")

  return(fd_indices)
}
