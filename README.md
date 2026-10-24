# SmartFlix – Movie Recommendation System

R (Plumber) API + HTML/CSS/JS frontend. Search movies (Hindi / Marathi / English and more),
view details, and get Top 5 similar-movie recommendations in multiple modes.

```
backend/   R Plumber API (api.R), slim dataset, Dockerfile
frontend/  Static website (index.html, app.js, styles.css, config.js)
```

## Run locally

```r
setwd("backend")
plumber::plumb("api.R")$run(host = "127.0.0.1", port = 8000)
```

```bash
cd frontend
python -m http.server 5500     # open http://127.0.0.1:5500
```

## Deploy

* **Backend:** Render (Docker, root directory `backend`)
* **Frontend:** Netlify / GitHub Pages / Vercel (folder `frontend`)
* Set the backend URL in `frontend/config.js`.

## Data

Dataset is TMDB `movies\_metadata.csv` + `credits.csv` (Kaggle "The Movies Dataset"),
trimmed to \~7.5k movies (all Hindi \& Marathi + most-voted others) so it fits GitHub and free hosting.
"# SmartFlix"





\## Attribution

This product uses the TMDB API but is not endorsed or certified by TMDB.

Movie metadata is from the Kaggle "The Movies Dataset" (CC0), originally collected from TMDB.

Poster images are loaded from TMDB's servers.

