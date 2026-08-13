user_agent_string <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/126.0.0.0 Safari/537.36"
)

fetch_html <- function(url, max_tries = 4, wait_seconds = 5) {
  
  for (attempt in seq_len(max_tries)) {
    
    response <- tryCatch(
      GET(
        url,
        user_agent(user_agent_string),
        add_headers(
          Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          `Accept-Language` = "es-ES,es;q=0.9,en;q=0.8"
        ),
        timeout(60),
        config(ipresolve = 1)  # force IPv4, runners often lack IPv6 routing
      ),
      error = function(e) e
    )
    
    if (inherits(response, "error")) {
      message(sprintf("attempt %d/%d - network error: %s",
                      attempt, max_tries, conditionMessage(response)))
    } else if (http_error(response)) {
      message(sprintf("attempt %d/%d - http status %s",
                      attempt, max_tries, status_code(response)))
    } else {
      # raw content so xml2 detects the page encoding itself (accents)
      return(read_html(content(response, as = "raw")))
    }
    
    if (attempt < max_tries) Sys.sleep(wait_seconds * attempt)
  }
  
  stop("could not download ", url, " after ", max_tries, " attempts")
}