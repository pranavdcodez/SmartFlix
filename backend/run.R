# Entry point for hosting (Render/Railway/etc.). Reads PORT from the environment.
port <- as.integer(Sys.getenv("PORT", "8000"))
pr <- plumber::plumb("api.R")
pr$run(host = "0.0.0.0", port = port)
