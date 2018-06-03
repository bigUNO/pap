## Placement Analysis Project: CalculatePerformanceMetrics
#
CalculatePerformanceMetrics <- function(x, y, type, verbose = FALSE) {
   # Computes the "click through rate" and the "cost per click". The CTR is
   # computed witht the additional step of multipling the quotient by 100.
   #
   # Args:
   #  x: Dividend
   #  y: Divisor
   #  percent: If TRUE, multiply the quotient by 100 before returning; if not,
   #     return the quotient as computed. Default is TRUE.
   #  verbose: If TRUE, prints calculations; if not, not. Default is FALSE.
   #
   # Returns:
   #  (x / y) * 100
   #  Optionally (x / y)
   switch(type
      , ctr = {
         quotient <- (x / y) * 100
      }
      , cpc = {
         quotient <- (x / y)
      })

   if (verbose) {
      cat("quotient = ", quotient, "\n")
   }
   return (quotient)
}
