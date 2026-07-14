# TREESILIENCE WALZ Explorer

A public Shiny app for exploring WALZ gas-exchange measurements without writing code. The app reads the public TREESILIENCE Google Drive folder directly, selects the newest uploaded CSV by Drive modification time, and shows the safely matched raw protocol below the timeseries.

No WALZ measurements, Google credentials, or thesis files are stored in this repository.

## What the app shows

- Interactive timeseries panels for `A`, `Tleaf`, `VPD`, `rh`, `ca`, `ci`, `White x T`, and `PARtop`
- A second interactive **A vs state** view
- Plotly zoom, pan, hover, line drawing, freehand drawing, erasing, and an optional 15-minute time grid
- The raw matched protocol TXT file, displayed as escaped text
- Persistent warnings for Drive failures, malformed CSV files, missing variables, and missing or ambiguous protocols

The [public Google Drive folder](https://drive.google.com/drive/folders/1wC9zXLEWQe4z7jBxfBfPRiVBuPJiF8vE) must contain direct child folders named `measurements` and `protocols`. Only `.csv` files in `measurements` and `.txt` files in `protocols` are listed.

## Public Drive access

The app uses `googledrive::drive_deauth()` for non-interactive access to files that are public to anyone with the link. It uses the package's built-in API key by default. Two optional environment variables are supported:

- `WALZ_DRIVE_FOLDER_ID`: replace the default root folder ID
- `GOOGLE_DRIVE_API_KEY`: replace the built-in API key

Downloaded content is cached in the running R process by Drive file ID and `modifiedTime`. Refreshing the app's file list exposes new or updated Drive data without publishing new code.

## Protocol matching

Matching deliberately avoids fuzzy guesses. It uses, in order:

1. Exact filename stem
2. A unique leading `YYYYMMDD_HHMM` timestamp
3. For duplicate timestamps, an exact normalized descriptor after removing known technical variants such as `lightFlucScript`, `postblackout`, and area annotations
4. A visible warning with no protocol content when zero or multiple candidates remain

## Run locally

Use R 4.0 or newer. The following keeps packages isolated from other projects:

```r
dir.create(".Rlib", showWarnings = FALSE)
install.packages(
  c(
    "shiny", "plotly", "ggplot2", "bslib", "googledrive", "gargle",
    "rsconnect", "testthat", "htmltools"
  ),
  lib = ".Rlib"
)
shiny::runApp()
```

## Validate

Run the deterministic fixture tests:

```sh
Rscript tests/testthat.R
```

Run the same tests plus downloads of every current public Drive CSV:

```sh
RUN_LIVE_DRIVE_TESTS=true Rscript tests/testthat.R
```

## Posit Connect Cloud

Connect Cloud resolves R dependencies from `manifest.json`; regenerate it after changing package imports:

```r
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
```

Publish this GitHub repository as a Shiny application with `app.R` as its primary file and enable automatic publishing on pushes to `main`. Drive data updates remain independent of that publishing cycle.
