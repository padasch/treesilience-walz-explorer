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
- A **Dew-Point Calculation** tab with interactive planning sliders, separate cuvette and tubing safety checks, a visual temperature comparison, and a four-line audit of the primary recorded run
- Plotly zoom, pan, hover, line drawing, freehand drawing, erasing, and an optional 15-minute time grid
- The raw matched protocol TXT file for each displayed run, shown as escaped text
- Persistent warnings for Drive failures, malformed CSV files, missing variables, and missing or ambiguous protocols
- A direct link to the public Google Drive folder in the Drive status section

## Dew-point calculation

The planning calculator starts from the wettest air expected to leave the cuvette. The recommended mode accepts a conservative expected chamber/outlet `wa` in ppm. An alternative planning mode adds controlled inlet H2O and an estimated H2O increase from leaf transpiration. The inlet setpoint alone is not treated as the outlet humidity because the sample can add substantial water vapour.

The Plotly comparison shows dew point, dew point plus the selected safety margin, `Tcuv - 2°C`, `Tcuv`, and the coldest expected tube or instrument-environment temperature represented by `Tamb`. The app evaluates two independent clearances:

- Internal cuvette clearance: `(Tcuv - 2°C) - dew point`
- Tubing clearance: `Tamb - dew point`

Each clearance is compared with the user-selected safety margin. `Tcuv > Tamb` is shown as manual context because warm humid air may cool downstream, but this temperature order is not itself classified as condensation when `Tamb` remains safely above dew point.

The selected primary run is audited row by row using the actual recorded `wa` and `Pamb` values to calculate dew point, together with the recorded `Tcuv` and `Tamb`; overlay runs are intentionally excluded from this tab. The audit reports actual dew-point crossings and observations that fall within the selected safety margin. `Tcuv > Tamb` is reported separately as context.

The WALZ CSV field `VPD [Pa/kPa]` is normalized leaf-to-air vapour pressure deficit, not ordinary VPD in kPa. Convert it using `VPD [kPa] = VPD [Pa/kPa] × Pamb [kPa] / 1000`. The dew-point calculation therefore uses recorded `wa` and `Pamb`, not the VPD column.

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
