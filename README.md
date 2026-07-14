# TREESILIENCE WALZ Explorer

A public Shiny app for exploring WALZ gas-exchange measurements without writing code. The app reads the public TREESILIENCE Google Drive folder directly, selects the newest uploaded CSV by Drive modification time, and shows the safely matched raw protocols below the timeseries.

**Open the app:** [TREESILIENCE WALZ Explorer on Posit Connect Cloud](https://019f5fde-29be-21f2-10c2-6bbe1e477663.share.connect.posit.cloud/)

No WALZ measurements, Google credentials, or thesis files are stored in this repository.

## What the app shows

- Interactive timeseries panels with every numeric CSV variable available as a checkbox, grouped as response parameters, environmental parameters, and the physiological constant `Area`
- Response parameters ordered as `A` (Net CO2), `GH2O`, and `E`, with `GH2O` selected by default
- Defaults for `A`, `GH2O`, `Tcuv`, `Tamb`, `VPD`, `rh`, `ca`, `ci`, `White x T`, and `PARtop`
- An optional two-run overlay aligned at elapsed minute zero, with original timestamps retained in hover text
- A second interactive **A vs state** view controlled by the same variable checkboxes
- A **Dew-Point Calculation** tab with interactive planning sliders, optional `Tamb = Tcuv + safety margin` coupling, a visual temperature-safety comparison, and a four-line audit of the primary recorded run
- Plotly zoom, pan, hover, line drawing, freehand drawing, erasing, and an optional 15-minute time grid
- The raw matched protocol TXT file for each displayed run, shown as escaped text
- Persistent warnings for Drive failures, malformed CSV files, missing variables, and missing or ambiguous protocols
- A direct link to the public Google Drive folder in the Drive status section

## Dew-point calculation

The planning calculator accepts expected cuvette H2O in ppm or relative humidity at `Tcuv`. Its Plotly comparison shows dew point, dew point plus the selected safety margin, `Tcuv - 2°C`, `Tcuv`, and `Tamb`. A switch couples the temperatures as `Tamb = Tcuv + safety margin`. When uncoupled, `Tcuv > Tamb` always produces a caution because the manual identifies that temperature order as a tube-condensation risk. For planning, the H2O value should conservatively represent expected cuvette or outlet humidity because the inlet setpoint alone can underestimate humidity after leaf transpiration.

The selected primary run is audited row by row using the actual recorded `wa` and `Pamb` values to calculate dew point, together with the recorded `Tcuv` and `Tamb`; overlay runs are intentionally excluded from this tab. A dynamic warning reports observations where `Tcuv` exceeded `Tamb`.

Saturation vapor pressure follows the Goff-Gratch relationship documented in the [official GFS-3000 manual](https://www.walz.com/files/downloads/gfs-3000_manual_9.pdf). The `Tcuv - 2°C` curve represents the manual's estimate of the coldest internal cuvette location during strong cooling and remains independent of the user-selected safety buffer. The calculator is a planning and uploaded-run analysis tool, not a live equipment interlock.

The [public Google Drive folder](https://drive.google.com/drive/folders/1wC9zXLEWQe4z7jBxfBfPRiVBuPJiF8vE) must contain direct child folders named `measurements` and `protocols`. Only `.csv` files in `measurements` and `.txt` files in `protocols` are listed.

## Public Drive access

The app uses `googledrive::drive_deauth()` for non-interactive access to files that are public to anyone with the link. It uses the package's built-in API key by default. Two optional environment variables are supported:

- `WALZ_DRIVE_FOLDER_ID`: replace the default root folder ID
- `GOOGLE_DRIVE_API_KEY`: replace the built-in API key

Downloaded content is cached in the running R process by Drive file ID and `modifiedTime`. If the package's shared API-key download is temporarily unavailable, the app falls back to Google's public file-download URL. Refreshing the app's file list exposes new or updated Drive data without publishing new code.

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
