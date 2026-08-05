library(rsconnect)


options(
  rsconnect.http.timeout = 1800,
  timeout = 1800
)


deployApp(forceUpdate = TRUE)