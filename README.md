# TREESILIENCE WALZ Explorer

A public Shiny app for exploring WALZ gas-exchange measurements without writing code. The app reads the public TREESILIENCE Google Drive folder and public run-metadata sheet directly, selects the newest uploaded CSV by Drive modification time, and shows the safely matched raw protocols below the timeseries.

**Open the app:** [TREESILIENCE WALZ Explorer on Posit Connect Cloud](https://019f5fde-29be-21f2-10c2-6bbe1e477663.share.connect.posit.cloud/)

No WALZ measurements, Google credentials, or thesis files are stored in this repository.

## What the app shows

- Interactive timeseries panels with every numeric CSV variable available as a checkbox, grouped as response parameters, environmental parameters, and the physiological constant `Area`
- Response parameters ordered as `A` (Net CO2), `GH2O`, and `E`, with `GH2O` selected by default
- Defaults for `A`, `GH2O`, `Tcuv`, `Tamb`, `VPD`, `rh`, `ca`, `ci`, `White x T`, and `PARtop`
- A timeseries x-axis toggle between elapsed time (the default, aligned from minute zero) and the original local time in Europe/Zurich
- An arbitrary number of selected runs overlaid with a distinct color and line style for each run and original timestamps retained in hover text
- A live metadata table above the plots, joined by exact measurement filename stem to the sheet `timestamp` run ID; each row is tinted with its plot color
- A second interactive **A vs state** view controlled by the same variable checkboxes
- Plotly zoom, pan, cursor crosshairs, compact exact-value hover, line drawing, freehand drawing, erasing, and an optional 15-minute time grid
- The raw matched protocol TXT file for each displayed run, shown as escaped text
- Persistent warnings for Drive failures, malformed CSV files, missing variables, and missing or ambiguous protocols
- A direct link to the public Google Drive folder in the Drive status section

## Dew-point calculation status

The Dew-Point Calculation tab is currently disabled. Observed condensation could not be reconciled safely with a calculator that uses recorded downstream `wa` and a single `Tamb` value as a proxy for the coldest wetted surface. The calculations remain in the source for later investigation, but they are not presented in the public student-facing app.

The reasoning, example calculations, instrument-flow limitation, and recommended diagnostic measurements are documented in [Dew-point condensation notes](docs/dew-point-condensation-notes.md).

For development review only, the hidden tab can be enabled with `WALZ_ENABLE_DEW_POINT_TAB=true`. It must not be interpreted as an equipment interlock or proof that a setup is condensation-safe.

The [public Google Drive folder](https://drive.google.com/drive/folders/1wC9zXLEWQe4z7jBxfBfPRiVBuPJiF8vE) must contain direct child folders named `measurements` and `protocols`. Only `.csv` files in `measurements` and `.txt` files in `protocols` are listed.

Run details come from the public [WALZ measurement metadata sheet](https://docs.google.com/spreadsheets/d/1BlUdEIKP-iEJICpzTF8NQG4nyEVBv_7Vgywc7KQqAX0/edit). The sheet is downloaded through its public CSV endpoint and does not require Google authentication. The `timestamp` column is the run ID and must equal the selected measurement filename without `.csv`. Matching ignores surrounding whitespace and case only; it does not use fuzzy or timestamp-only guesses.

## Public Drive access

The app uses `googledrive::drive_deauth()` for non-interactive access to files that are public to anyone with the link. It uses the package's built-in API key by default. Two optional environment variables are supported:

- `WALZ_DRIVE_FOLDER_ID`: replace the default root folder ID
- `GOOGLE_DRIVE_API_KEY`: replace the built-in API key
- `WALZ_METADATA_SHEET_ID`: replace the default public metadata spreadsheet ID
- `WALZ_METADATA_SHEET_NAME`: replace the default `Walz Measurement Metadata` tab name
- `WALZ_ENABLE_DEW_POINT_TAB`: set to `true` only for development review of the disabled calculator

Downloaded measurement and protocol content is cached in the running R process by Drive file ID and `modifiedTime`. If the package's shared API-key download is temporarily unavailable, the app falls back to Google's public file-download URL. “Refresh and show latest” refreshes both the Drive listing and metadata sheet, then selects the newest measurement. Data updates do not require a code deployment.

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

The production deployment is linked to this repository's `main` branch with automatic publishing on push enabled.
