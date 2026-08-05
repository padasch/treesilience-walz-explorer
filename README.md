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
- A faceted A-timeseries overview for each quality, with a one-to-four-column layout control and an optional per-run positive-maximum normalization switch
- Raw A is the default so entirely negative or otherwise unusual runs remain visible; normalized mode omits runs without a positive maximum with an explicit warning. Pale bands mark the final-three-minute extraction windows, thick segments mark their means, and error bars show mean ± 1 SD
- Clickable run-ID facet titles open the read-only metadata Sheet for manual quality editing, while clicking a timeseries selects its detailed raw audit
- A raw audit whose selector always includes every measurement CSV independently of the overview filters; unmatched metadata rows are flagged without hiding the raw data
- Raw A and PPFD audits with pale extraction windows, thick three-minute mean segments, and lighter mean ± 1 SD bars, plus slope, coverage, completeness, and structural warnings
- Switchable metadata filters or manual multi-run selection, plus a color-matched metadata table
- A default run-date range from 1 July 2026 through the current Europe/Zurich date
- Explicit **Analyze filtered runs** and **Analyze all runs** actions with run-count progress; opening the tab performs no collection-wide preparation
- Read-only quality overview; assessments are edited manually in the linked metadata Sheet

### Response Analysis

- Required single-species selection, optional plant IDs, Good-only quality default, and advanced metadata filters
- A shared step extractor with measured `PARtop`, isolated-setpoint repair, final-three-minute means, uncertainty, drift, and inclusion status
- A shared slice-display toggle, defaulting to actual extracted means: observed A versus PPFD by run and observed A versus Tleaf in measured-PPFD bands; the alternative renders both charts as slices through the same fitted GAM
- Light and temperature optima with ≥90% near-optimal ranges and explicit boundary/interior/no-positive-optimum status
- Interactive raw and fitted Tleaf × PPFD × A views with predictions masked outside observed support
- A faceted raw-A audit normalized by each run's positive maximum, with pale three-minute extraction windows, thick window-mean segments, and mean ± 1 SD bars
- Coverage checks, deviance explained, RMSE, basis diagnostics, leave-one-run-out predictive R², boundary-optimum proportion, and CSV downloads
- Low model-quality metrics produce a prominent warning, while fitted charts, surfaces, optima, diagnostics, and downloads remain visible for cautious interpretation

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

Drive listings and public metadata are cached process-wide for 60 seconds. Downloaded measurement and protocol content is cached by Drive file ID and `modifiedTime`; extracted light-step summaries also include extraction settings in their cache key. The Explorer lists measurements first, loads metadata after the first plot, and does not list protocols until the protocol accordion is opened. Quality Control prepares nothing until **Analyze filtered runs** or **Analyze all runs** is pressed; filtered analysis downloads and wrangles only the current selection, while the all-runs action deliberately ignores the filters. Preparation runs in batches of about 12, with concurrent transfers inside each batch, per-file fallback through `googledrive`, and visible completed/total progress. “Refresh and show latest” forces the measurement and metadata sources to refresh, while protocol refresh remains deferred unless protocols have already been opened. Data updates do not require a code deployment.

## Quality assessment workflow

Quality Control is intentionally read-only. The app reads exactly the `quality assessment` column and accepts `good`, `medium`, and `bad` as its source values. Blank or NA cells, unmatched runs, and unexpected values are displayed as `unassessed` in memory; the app never fills or changes cells in the Sheet. Refresh the app after editing `quality assessment` manually to update the Good, Medium, Bad, and Unassessed sections.

## Response model

The exploratory species-level model is `A_mean ~ te(Tleaf_mean, log1p(PPFD_mean))`, fitted by REML with smoothing selection. Fitting requires at least four distinct runs, four measured light levels, and 16 complete extraction windows. Predictions outside the linear-interpolation support are masked. Low deviance explained, adjusted R², leave-one-run-out predictive R², GAM basis checks, or excessive boundary optima trigger a prominent caution with the failing metrics; the fitted outputs remain visible.

Response Analysis supports two selection modes. Metadata filters retain the Good-only default; manual selection can include any available files, including runs without a metadata match. The separate First Analysis presets contain ten Oak temperature runs or ten Beech temperature runs; species are never combined by a preset. The response-slice toggle defaults to actual extracted means and can switch both line charts to the corresponding GAM slices without refitting. Actual A–Tleaf lines group measured PPFD into 50-unit bands, retaining up to ten representative slices. Continuous color bars encode mean leaf temperature and measured PPFD. The max-normalized raw-A facets use the same final-three-minute windows as the model; non-terminal windows precede the next light-step jump, while the terminal step uses its final three minutes. Run identity colors elsewhere in the application use the Dark2 palette consistently.

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
    "curl", "future", "promises", "mgcv",
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
