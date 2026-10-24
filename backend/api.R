library(plumber)
library(dplyr)
library(jsonlite)

# SmartFlix API using movies_metadata.csv (TMDB-style metadata dataset)
# Keep this file in the same folder as movies_metadata.csv.
# Optional files: keywords.csv, ratings_small.csv, credits.csv.

if (!file.exists("movies_metadata.csv")) {
  stop("movies_metadata.csv not found. Open RStudio in your project folder.")
}

movies <- read.csv(
  "movies_metadata.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)

# Ensure expected columns exist, even if the source CSV has a slightly different schema.
required_columns <- c(
  "id", "title", "original_title", "original_language", "overview",
  "release_date", "genres", "vote_average", "vote_count", "popularity",
  "poster_path"
)
for (nm in required_columns) {
  if (!nm %in% names(movies)) movies[[nm]] <- NA
}

as_text <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[1])) return(default)
  value <- trimws(as.character(x[1]))
  if (!nzchar(value) || value == "NA") default else value
}

as_num <- function(x, default = 0) {
  value <- suppressWarnings(as.numeric(as.character(x[1])))
  if (!length(value) || is.na(value) || !is.finite(value)) default else value
}

# Extract genre names from the JSON-like genres column.
parse_genres <- function(x) {
  x <- as_text(x)
  if (!nzchar(x)) return("")
  # Dataset uses Python-style single quotes in many rows, but some rows use JSON double quotes.
  matches <- regmatches(x, gregexpr("['\"]name['\"]\\s*:\\s*['\"]([^'\"]+)['\"]", x, perl = TRUE))[[1]]
  if (!length(matches) || identical(matches, character(0)) || matches[1] == "") return("")
  vals <- sub(".*['\"]name['\"]\\s*:\\s*['\"]([^'\"]+)['\"].*", "\\1", matches, perl = TRUE)
  paste(unique(vals), collapse = ", ")
}

# Extract names from TMDB credits JSON, if an optional credits.csv is added.
parse_credit_field <- function(x, field = "name", limit = 8) {
  x <- as_text(x)
  if (!nzchar(x)) return("")
  # Credits CSV stores Python-style dictionaries (single quotes), not valid JSON.
  pattern <- paste0("['\"]", field, "['\"]\\s*:\\s*['\"]([^'\"]+)['\"]")
  matches <- regmatches(x, gregexpr(pattern, x, perl = TRUE))[[1]]
  if (!length(matches) || identical(matches, character(0)) || matches[1] == "") return("")
  vals <- sub(paste0(".*['\"]", field, "['\"]\\s*:\\s*['\"]([^'\"]+)['\"].*"), "\\1", matches, perl = TRUE)
  paste(head(unique(vals), limit), collapse = ", ")
}

parse_director <- function(x) {
  x <- as_text(x)
  if (!nzchar(x)) return("")
  # Find the crew object whose job is Director, then its following name field.
  chunks <- strsplit(x, "['\"]job['\"]\\s*:\\s*['\"]Director['\"]", perl = TRUE)[[1]]
  if (length(chunks) < 2) return("")
  after <- chunks[2]
  m <- regmatches(after, regexpr("['\"]name['\"]\\s*:\\s*['\"]([^'\"]+)['\"]", after, perl = TRUE))
  if (!length(m) || !nzchar(m)) return("")
  sub(".*['\"]name['\"]\\s*:\\s*['\"]([^'\"]+)['\"].*", "\\1", m, perl = TRUE)
}

parse_country <- function(x) {
  x <- as_text(x)
  if (!nzchar(x)) return("")
  matches <- regmatches(x, gregexpr("['\"]name['\"]\\s*:\\s*['\"]([^'\"]+)['\"]", x, perl = TRUE))[[1]]
  if (!length(matches) || identical(matches, character(0)) || matches[1] == "") return("")
  vals <- sub(".*['\"]name['\"]\\s*:\\s*['\"]([^'\"]+)['\"].*", "\\1", matches, perl = TRUE)
  paste(unique(vals), collapse = ", ")
}

movies$movie_id <- as.character(movies$id)
movies$title <- vapply(seq_len(nrow(movies)), function(i) {
  value <- as_text(movies$title[i])
  if (!nzchar(value)) as_text(movies$original_title[i], "Untitled") else value
}, character(1))
movies$original_title <- vapply(seq_len(nrow(movies)), function(i) as_text(movies$original_title[i]), character(1))
movies$original_language <- vapply(seq_len(nrow(movies)), function(i) tolower(as_text(movies$original_language[i], "unknown")), character(1))
movies$overview <- vapply(seq_len(nrow(movies)), function(i) as_text(movies$overview[i], "Description unavailable."), character(1))
movies$release_date <- vapply(seq_len(nrow(movies)), function(i) as_text(movies$release_date[i]), character(1))
movies$release_year <- suppressWarnings(as.integer(substr(movies$release_date, 1, 4)))
movies$release_year[is.na(movies$release_year)] <- 0L
movies$genre_text <- vapply(seq_len(nrow(movies)), function(i) parse_genres(movies$genres[i]), character(1))
# Precompute normalized genre tokens once so each recommendation request does not repeatedly split strings.
movies$genre_tokens <- strsplit(tolower(movies$genre_text), ",\\s*")
movies$vote_average_num <- vapply(seq_len(nrow(movies)), function(i) as_num(movies$vote_average[i]), numeric(1))
movies$vote_count_num <- vapply(seq_len(nrow(movies)), function(i) as_num(movies$vote_count[i]), numeric(1))
movies$popularity_num <- vapply(seq_len(nrow(movies)), function(i) as_num(movies$popularity[i]), numeric(1))
movies$director <- ""
movies$cast <- ""
movies$country <- vapply(seq_len(nrow(movies)), function(i) parse_country(movies$production_countries[i]), character(1))
movies$rating <- ""
movies$duration <- ""

# Load credits only once; parse cast/director lazily for requested movies.
# Avoid parsing every huge cast/crew field during startup.
credits <- NULL
if (file.exists("credits.csv")) {
  credits <- tryCatch(
    read.csv("credits.csv", stringsAsFactors = FALSE, check.names = FALSE,
             colClasses = c(id = "character", cast = "character", crew = "character")),
    error = function(e) NULL
  )
  if (!is.null(credits) && !all(c("id", "cast", "crew") %in% names(credits))) credits <- NULL
}

movies <- movies[!is.na(movies$title) & nzchar(movies$title), , drop = FALSE]
movies <- movies[!duplicated(tolower(movies$title)), , drop = FALSE]

# Language is based on the original_language metadata, not guessed from country.
language_label <- function(code) {
  code <- tolower(as_text(code, "unknown"))
  labels <- c(hi = "Hindi", mr = "Marathi", en = "English", ta = "Tamil",
              te = "Telugu", bn = "Bengali", kn = "Kannada", ml = "Malayalam",
              pa = "Punjabi", ur = "Urdu", ko = "Korean", ja = "Japanese",
              fr = "French", es = "Spanish", de = "German", zh = "Chinese")
  if (code %in% names(labels)) unname(labels[[code]]) else if (code == "unknown") "Unknown" else toupper(code)
}
movies$language <- vapply(movies$original_language, language_label, character(1))

# Cache normalized title columns so autocomplete does not lowercase 45k titles on every keystroke.
movies$title_lower <- tolower(movies$title)
movies$original_title_lower <- tolower(movies$original_title)

row_to_list <- function(df, i = 1) {
  r <- df[i, , drop = FALSE]
  director_value <- as_text(r$director)
  cast_value <- as_text(r$cast)
  if (!nzchar(director_value) && !nzchar(cast_value) && !is.null(credits)) {
    ci <- match(as.character(r$movie_id), as.character(credits$id))
    if (!is.na(ci)) {
      director_value <- parse_director(credits$crew[ci])
      cast_value <- parse_credit_field(credits$cast[ci], "name", 8)
    }
  }
  list(
    show_id = as_text(r$movie_id),
    id = as_text(r$movie_id),
    title = as_text(r$title, "Untitled"),
    original_title = as_text(r$original_title),
    director = director_value,
    cast = cast_value,
    country = as_text(r$country),
    release_year = as.integer(as_num(r$release_year, 0)),
    release_date = as_text(r$release_date),
    rating = as.character(as_num(r$vote_average_num, 0)),
    vote_average = as_num(r$vote_average_num, 0),
    vote_count = as_num(r$vote_count_num, 0),
    duration = as_text(r$duration),
    listed_in = as_text(r$genre_text),
    genres = as_text(r$genre_text),
    description = as_text(r$overview, "Description unavailable."),
    overview = as_text(r$overview, "Description unavailable."),
    language = as_text(r$language, "Unknown"),
    original_language = as_text(r$original_language, "unknown"),
    poster_path = as_text(r$poster_path),
    popularity = as_num(r$popularity_num, 0)
  )
}

find_movie <- function(movie_name) {
  q <- tolower(trimws(as_text(movie_name)))
  if (!nzchar(q)) return(NULL)
  exact <- which(movies$title_lower == q | movies$original_title_lower == q)
  if (length(exact)) return(movies[exact[1], , drop = FALSE])
  partial <- which(grepl(q, movies$title_lower, fixed = TRUE) |
                     grepl(q, movies$original_title_lower, fixed = TRUE))
  if (length(partial)) return(movies[partial[1], , drop = FALSE])
  NULL
}

common_count <- function(x, y) {
  a <- tolower(trimws(unlist(strsplit(as_text(x), ","))))
  b <- tolower(trimws(unlist(strsplit(as_text(y), ","))))
  length(intersect(a[nzchar(a)], b[nzchar(b)]))
}

mode_label <- function(mode) {
  labels <- c(overall = "Best Overall Match", genre = "Genre Based",
              actor = "Actor Based", director = "Director Based",
              language = "Language Based", rating = "Rating Based", year = "Release Year Based")
  if (mode %in% names(labels)) unname(labels[[mode]]) else "Best Overall Match"
}

build_recommendations <- function(movie_name, n = 5, mode = "overall", language = "All") {
  selected <- find_movie(movie_name)
  if (is.null(selected)) return(NULL)
  valid <- c("overall", "genre", "actor", "director", "country", "language", "rating", "year")
  if (!(mode %in% valid)) mode <- "overall"

  pool <- movies[movies$movie_id != selected$movie_id, , drop = FALSE]
  if (language %in% c("Hindi", "Marathi", "English")) {
    pool <- pool[pool$language == language, , drop = FALSE]
  }

  # Fast vectorized scoring: genre tokens are precomputed at startup.
  selected_genres <- selected$genre_tokens[[1]]
  if (length(selected_genres) && nzchar(selected_genres[1])) {
    pool$Genre_Score <- vapply(pool$genre_tokens, function(x) length(intersect(selected_genres, x)), numeric(1))
  } else {
    pool$Genre_Score <- rep(0, nrow(pool))
  }
  # Cast/director are fetched lazily for the selected title only; candidates are not parsed here.
  # Avoid thousands of useless string operations on empty fields.
  pool$Actor_Score <- rep(0, nrow(pool))
  pool$Director_Score <- rep(0, nrow(pool))
  pool$Country_Score <- if (nzchar(selected$country)) as.numeric(pool$country == selected$country) else rep(0, nrow(pool))
  pool$Language_Score <- as.numeric(pool$original_language == selected$original_language)
  pool$Year_Score <- ifelse(abs(pool$release_year - selected$release_year) <= 2, 2,
                            ifelse(abs(pool$release_year - selected$release_year) <= 5, 1, 0))
  pool$Rating_Score <- ifelse(abs(pool$vote_average_num - selected$vote_average_num) <= 1, 1, 0)
  pool$Recommendation_Score <- pool$Genre_Score * 5 + pool$Actor_Score * 4 +
    pool$Director_Score * 8 + pool$Country_Score * 3 + pool$Language_Score * 3 + pool$Year_Score + pool$Rating_Score
  if (mode == "genre") pool$Recommendation_Score <- pool$Genre_Score * 10 + pool$Language_Score
  if (mode == "actor") pool$Recommendation_Score <- pool$Actor_Score * 10 + pool$Genre_Score
  if (mode == "director") pool$Recommendation_Score <- pool$Director_Score * 10 + pool$Genre_Score
  if (mode == "country") pool$Recommendation_Score <- pool$Country_Score * 10 + pool$Genre_Score
  if (mode == "language") pool$Recommendation_Score <- pool$Language_Score * 10 + pool$Genre_Score
  if (mode == "rating") pool$Recommendation_Score <- pool$Rating_Score * 10 + pool$Genre_Score
  if (mode == "year") pool$Recommendation_Score <- pool$Year_Score * 10 + pool$Genre_Score

  if (mode == "genre") pool <- pool[pool$Genre_Score > 0, , drop = FALSE]
  if (mode == "actor") pool <- pool[pool$Actor_Score > 0, , drop = FALSE]
  if (mode == "director") pool <- pool[pool$Director_Score > 0, , drop = FALSE]
  if (mode == "country") pool <- pool[pool$Country_Score > 0, , drop = FALSE]
  if (mode == "language") pool <- pool[pool$Language_Score > 0, , drop = FALSE]
  pool <- pool[pool$Recommendation_Score > 0, , drop = FALSE]
  pool <- pool[order(-pool$Recommendation_Score, -pool$vote_average_num, -pool$popularity_num), , drop = FALSE]
  pool <- head(pool, max(1, min(as.integer(n), 50)))

  recs <- lapply(seq_len(nrow(pool)), function(i) {
    item <- row_to_list(pool, i)
    reasons <- character(0)
    if (pool$Genre_Score[i] > 0) reasons <- c(reasons, "Similar genres")
    if (pool$Language_Score[i] > 0) reasons <- c(reasons, "Same original language")
    if (pool$Director_Score[i] > 0) reasons <- c(reasons, "Same director")
    if (pool$Country_Score[i] > 0) reasons <- c(reasons, "Same production country")
    if (pool$Actor_Score[i] > 0) reasons <- c(reasons, "Shared cast")
    if (pool$Year_Score[i] > 0) reasons <- c(reasons, "Similar release year")
    if (pool$Rating_Score[i] > 0) reasons <- c(reasons, "Similar rating")
    item$Recommendation_Score <- pool$Recommendation_Score[i]
    item$reasons <- paste(reasons, collapse = " · ")
    item
  })

  list(selected = row_to_list(selected), recommendations = recs,
       mode = mode, mode_label = mode_label(mode), language = language)
}

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  forward()
}

#* API health check
#* @get /api/health
#* @serializer json
function() {
  list(status = "ok", total_movies = nrow(movies),
       hindi_movies = sum(movies$original_language == "hi"),
       marathi_movies = sum(movies$original_language == "mr"),
       message = "SmartFlix API is running with movies_metadata.csv.")
}

#* Search movie titles
#* @get /api/search
#* @param q Search text
#* @param language Optional language filter: All, Hindi, Marathi, English
#* @serializer json
function(q = "", language = "All") {
  q <- trimws(as_text(q))
  if (!nzchar(q)) return(list(results = list()))
  pool <- movies
  if (language %in% c("Hindi", "Marathi", "English")) {
    pool <- pool[pool$language == language, , drop = FALSE]
  }
  q_lower <- tolower(q)
  # Fast autocomplete: prefix matches first, substring matches only if needed.
  prefix_ix <- which(startsWith(pool$title_lower, q_lower) | startsWith(pool$original_title_lower, q_lower))
  if (length(prefix_ix) >= 10) {
    ix <- prefix_ix
  } else {
    substring_ix <- which(grepl(q_lower, pool$title_lower, fixed = TRUE) |
                            grepl(q_lower, pool$original_title_lower, fixed = TRUE))
    ix <- unique(c(prefix_ix, substring_ix))
  }
  if (!length(ix)) return(list(results = list()))
  ix <- head(ix[order(-pool$popularity_num[ix], -pool$vote_count_num[ix])], 8)
  list(results = lapply(ix, function(i) row_to_list(pool, i)))
}

#* Movie details
#* @get /api/movie
#* @param title Movie title
#* @serializer json
function(title = "") {
  found <- find_movie(title)
  if (is.null(found)) {
    return(list(error = paste0("No movie found for '", as_text(title), "'.")))
  }
  row_to_list(found)
}

#* Get recommendations using selected mode
#* @post /api/recommend
#* @serializer json
function(req, res) {
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
                   error = function(e) list())
  movie_name <- as_text(body$movie)
  mode <- as_text(body$mode, "overall")
  count <- suppressWarnings(as.integer(body$count))
  if (!length(count) || is.na(count) || count < 1) count <- 5
  language <- as_text(body$language, "All")
  if (!nzchar(movie_name)) {
    res$status <- 400
    return(list(error = "Please enter a movie title."))
  }
  output <- build_recommendations(movie_name, count, mode, language)
  if (is.null(output)) {
    res$status <- 404
    return(list(error = paste0("No movie found for '", movie_name, "'. Try another title.")))
  }
  output
}
