cli::cli_h1("Started at {Sys.time()}")
dotenv::load_dot_env(".Renviron")

# targets::tar_make_clustermq(workers = parallel::detectCores() - 2)

tryCatch(expr = {
  targets::tar_make()
}, error = function(e) {
  pushoverr::pushover(message = paste("Error: ", e),
                      user = Sys.getenv("pushover_user"),
                      device = "iphone", app = Sys.getenv("pushover_api"))
  cli::cli_h2("Error")
  later::later(function() {
    file.edit("run.Rout")
  }, delay = 1)
  stop(e)
})
pushoverr::pushover(message = "Work done!",
                    user = Sys.getenv("pushover_user"),
                    device = "iphone", app = Sys.getenv("pushover_api"))
