# TREESILIENCE WALZ Explorer

A public Shiny app for exploring, reviewing, and modeling WALZ gas-exchange measurements without writing code. The app reads the public TREESILIENCE Google Drive folder and public run-metadata sheet directly, selects the newest uploaded CSV by Drive modification time, and keeps raw data, quality labels, and exploratory response models linked by exact run ID.

**Open the app:** [TREESILIENCE WALZ Explorer on Posit Connect Cloud](https://019f5fde-29be-21f2-10c2-6bbe1e477663.share.connect.posit.cloud/)

No WALZ measurements, Google credentials, or thesis files are stored in this repository.

## What the app shows

### Explorer

- Interactive timeseries panels with every numeric CSV variable available as a checkbox, grouped as response parameters, environmental parameters, and the physiological constant `Area`
- Response parameters ordered as `A` (Net CO2), `GH2O`, and `E`, with `GH2O` selected by default
- Defaults for `A`, `GH2O`, `Tcuv`, `Tamb`, `VPD`, `rh`, `ca`, `ci`, `White x T`, and `PARtop`
- A timeseries x-axis toggle between elapsed time (the default, aligned from minute zero) and the original local time in Europe/Zurich
- A graph-layout switch between the default two-column small multiples and a taller one-column stack
- An arbitrary number of selected runs overlaid as solid lines, with one colour and one legend entry per measurement file and original timestamps retained in hover text
- A collapsible metadata section below the plots with selected-run details first, joined by exact measurement filename stem to the sheet `timestamp` run ID and tinted with each run's plot color
- The complete public metadata CSV below those selected-run details, read-only and sorted newest-first
- A display-only Date column derived from each `YYYYMMDD_HHMM` run ID, combining relative and absolute Europe/Zurich time
- A second interactive **A vs state** view controlled by the same variable checkboxes
- Plotly zoom, pan, cursor crosshairs, compact exact-value hover, line drawing, freehand drawing, and erasing
- A separate collapsible protocol section with the raw matched TXT file for each displayed run, shown as escaped text
- Persistent warnings for Drive failures, malformed CSV files, missing variables, and missing or ambiguous protocols
- A direct link to the public Google Drive folder in the Drive status section

### Quality Control

- Four quality sections: Good, Medium, Bad, and Unassessed
- Original `A` versus measured `PARtop` alongside `A / positive run maximum`; negative A remains negative, and non-positive runs are omitted from normalization with a warning
- Exactly one color and trace per run, using stable colors across tabs and quality changes
- Final-three-minute means for every stable light step, including the terminal step
- A click-selectable raw audit with A, PPFD, extraction windows, SD, within-window A slope, coverage, completeness, and structural warnings
- Optional species, plant-ID, and date filters plus a color-matched metadata table
- Read-only operation by default; optional guarded Google Sheet write-back changes only the selected run's `quality assessment` cell

### Response Analysis

- Required single-species selection, optional plant IDs, Good-only quality default, and advanced metadata filters
- A shared step extractor with measured `PARtop`, isolated-setpoint repair, final-three-minute means, uncertainty, drift, and inclusion status
- Observed A versus PPFD by run, and GAM-predicted A versus Tleaf at measured-PPFD slices
- Light and temperature optima with ≥90% near-optimal ranges and explicit boundary/interior/no-positive-optimum status
- Interactive raw and fitted Tleaf × PPFD × A views with predictions masked outside observed support
- Coverage checks, deviance explained, RMSE, basis diagnostics, leave-one-run-out predictive R², boundary-optimum proportion, and CSV downloads
- Raw and extraction results remain available when coverage or diagnostics are too weak; unsupported surfaces and optima are suppressed

## Dew-point calculation status

The Dew-Point Calculation tab is currently disabled. Observed condensation could not be reconciled safely with a calculator that uses recorded downstream `wa` and a single `Tamb` value as a proxy for the coldest wetted surface. The calculations remain in the source for later investigation, but they are not presented in the public student-facing app.

The reasoning, example calculations, instrument-flow limitation, and recommended diagnostic measurements are documented in [Dew-point condensation notes](docs/dew-point-condensation-notes.md).

For development review only, the hidden tab can be enabled with `WALZ_ENABLE_DEW_POINT_TAB=true`. It must not be interpreted as an equipment interlock or proof that a setup is condensation-safe.

The [public Google Drive folder](https://drive.google.com/drive/folders/1wC9zXLEWQe4z7jBxfBfPRiVBuPJiF8vE) must contain direct child folders named `measurements` and `protocols`. Only `.csv` files in `measurements` and `.txt` files in `protocols` are listed. The validated child-folder IDs are used directly for fast startup, with root-folder discovery retained as a fallback.

Run details come from the public [WALZ measurement metadata sheet](https://docs.google.com/spreadsheets/d/1BlUdEIKP-iEJICpzTF8NQG4nyEVBv_7Vgywc7KQqAX0/edit). The sheet is downloaded through its public CSV endpoint and does not require Google authentication. The `timestamp` column is the run ID and must equal the selected measurement filename without `.csv`. Matching ignores surrounding whitespace and case only; it does not use fuzzy or timestamp-only guesses.

## Public Drive access

The app uses `googledrive::drive_deauth()` for non-interactive access to files that are public to anyone with the link. It uses the package's built-in API key by default. Configuration variables are:

- `WALZ_DRIVE_FOLDER_ID`: replace the default root folder ID
- `WALZ_MEASUREMENTS_FOLDER_ID`: replace the direct measurements folder ID
- `WALZ_PROTOCOLS_FOLDER_ID`: replace the direct protocols folder ID
- `GOOGLE_DRIVE_API_KEY`: replace the built-in API key
- `WALZ_METADATA_SHEET_ID`: replace the default public metadata spreadsheet ID
- `WALZ_METADATA_SHEET_NAME`: replace the default `Walz Measurement Metadata` tab name
- `WALZ_ENABLE_DEW_POINT_TAB`: set to `true` only for development review of the disabled calculator
- `WALZ_BACKGROUND_WORKERS`: background worker count, with a minimum and default of two
- `WALZ_REVIEW_ACCESS_CODE`: shared code that unlocks quality write-back for a browser session
- `WALZ_GOOGLE_SERVICE_ACCOUNT_JSON_B64`: base64-encoded Google service-account JSON for Sheet write-back

Drive listings and public metadata are cached process-wide for 60 seconds. Downloaded measurement and protocol content is cached by Drive file ID and `modifiedTime`; extracted light-step summaries also include extraction settings in their cache key. The Explorer lists measurements first, loads metadata after the first plot, and does not list protocols until the protocol accordion is opened. QC and analysis collections load only on first use in sequential batches of about 12, with concurrent transfers inside each batch and per-file fallback through `googledrive`. “Refresh and show latest” forces the measurement and metadata sources to refresh, while protocol refresh remains deferred unless protocols have already been opened. Data updates do not require a code deployment.

## Quality write-back setup

The public app remains fully usable for read-only review when write-back secrets are absent. To enable the four quality actions, configure both encrypted environment variables in Posit Connect Cloud:

1. `WALZ_REVIEW_ACCESS_CODE`: a shared reviewer code. It gates the buttons but is not a substitute for private app access.
2. `WALZ_GOOGLE_SERVICE_ACCOUNT_JSON_B64`: service-account credentials encoded as base64. Share the metadata Sheet with that service-account email as an editor.

Before every change, the server re-reads the timestamp and quality columns, requires exactly one exact timestamp match, checks that the loaded quality value has not changed, writes one cell, and verifies the returned value. Automated tests use a fake Sheet client and never write to the production spreadsheet.

## Response model

The exploratory species-level model is `A_mean ~ te(Tleaf_mean, log1p(PPFD_mean))`, fitted by REML with smoothing selection. Fitting requires at least four distinct runs, four measured light levels, and 16 complete extraction windows. Predictions outside the linear-interpolation support are masked. Surface and optimum claims are suppressed when leave-one-run-out prediction, GAM basis checks, or boundary-optimum prevalence indicate inadequate support.

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
    "curl", "future", "promises", "googlesheets4", "base64enc", "mgcv",
    "interp", "later", "rsconnect", "testthat", "htmltools"
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
