# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
# Tunnel: ssh -L 127.0.0.1:5433:127.0.0.1:5432 robert@157.245.254.28 -N
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
kypoliticaldata::run_app() # add parameters here (if any)
