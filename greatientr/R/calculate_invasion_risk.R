#' Calculate Invasion Risk Score
#' #' @importFrom FD dbFD
#'
#' @param fric Functional richness value
#' @param feve Functional evenness value
#' @param fdiv Functional divergence value
#' @param c_prop C-strategy proportion
#' @param s_prop S-strategy proportion
#' @param r_prop R-strategy proportion
#'
#' @return A numeric value between 0 and 1 representing invasion risk
#' @export
#'
#' @examples
#' calculate_invasion_risk(0.5, 0.6, 0.4, 30, 40, 30)
# Function to calculate invasion risk
calculate_invasion_risk <- function(fd_indices, csr_props) {
  # Create data frame for results
  invasion_risks <- data.frame(
    Site = names(fd_indices$FRic),
    Risk_Score = mapply(
      function(fric, feve, fdiv, c, s, r) {
        weights <- c(FRic = 0.25, FEve = 0.20, FDiv = 0.20, CSR = 0.35)

        # Normalize FD metrics
        fd_metrics <- pmax(0, pmin(1, c(fric, feve, fdiv)))
        fd_score <- sum(weights[1:3] * (1 - fd_metrics))

        # CSR balance
        csr_balance <- (1 - sd(c(c, s, r)/100)) / 1

        # Final risk score
        pmax(0, pmin(1, (fd_score + weights["CSR"] * csr_balance) / sum(weights)))
      },
      fd_indices$FRic,
      fd_indices$FEve,
      fd_indices$FDiv,
      csr_props$C_prop,
      csr_props$S_prop,
      csr_props$R_prop
    )
  )

  # Add risk level categories
  invasion_risks$Risk_Level <- cut(
    invasion_risks$Risk_Score,
    breaks = c(0, 0.3, 0.6, 1),
    labels = c("Low", "Medium", "High")
  )

  return(invasion_risks)
}


help("greatientr")






