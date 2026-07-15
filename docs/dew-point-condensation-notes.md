# Dew-point condensation notes

## Status

The Shiny app's **Dew-Point Calculation** tab was disabled on 2026-07-15. The underlying calculation and plotting code is retained for investigation, but the tab is hidden by default because its nominal margins can be mistaken for a guarantee that the instrument is condensation-safe.

For development review only, the tab can be enabled with:

```sh
WALZ_ENABLE_DEW_POINT_TAB=true Rscript -e 'shiny::runApp()'
```

The calculator is not an equipment interlock and should not be used to certify safe operating conditions.

## Why the nominal calculation did not explain the observation

One investigated example used the following planner inputs:

- Expected chamber/outlet `wa`: 19,080 ppm
- Ambient pressure `Pamb`: 95.1 kPa
- Cuvette temperature `Tcuv`: 22°C
- Entered `Tamb` proxy: 25°C
- Operational clearance: 2°C

Using the Goff-Gratch relationship documented by WALZ, these values give:

- Dew point: approximately 16.0°C
- Relative humidity at 22°C: approximately 68.7%
- Estimated internal cold point (`Tcuv - 2°C`): 20°C
- Raw internal margin: 4°C
- Raw margin to the entered `Tamb` proxy: 9°C

The apparent 9-10°C margin therefore applies only to the entered `Tamb` proxy. The nominal internal margin is 4°C, of which 2°C is consumed by the selected operational clearance. If `Tcuv` and `Tamb` are both 22°C, the nominal internal margin remains 4°C and the nominal tube margin becomes 6°C.

At 95.1 kPa, saturation corresponds to approximately 24,562 ppm H2O at 20°C and 27,775 ppm at 22°C. Condensation under truly uniform, steady conditions at these temperatures should not occur at 19,080 ppm. The observation therefore shows that at least one calculator input or proxy did not represent the conditions at the location and time where condensation started.

## Instrument-flow limitation: recorded `wa` is downstream

The GFS-3000 manual describes the measuring gas as flowing through the measuring head, then through the return path and filter, and finally through the analyzer's H2O sample cell. It defines:

```text
wa = H2Osam - dH2OZP
```

Thus, `wa` is calculated from gas measured after it has left the measuring head and travelled back toward the analyzer. If water condenses in the return tubing or a connector before the H2O sample cell, vapour has already been removed from the gas. Recorded `wa` can then be lower than the humidity that initiated condensation.

This makes the recorded-run calculation potentially circular: downstream `wa` cannot independently rule out upstream condensation after condensation has begun.

## `Tamb` is not the coldest wetted-surface temperature

The manual states that:

- the coldest internal cuvette location may be up to 2°C cooler than `Tcuv` during full cooling;
- condensation may occur when warm cuvette gas encounters the cooler instrument environment;
- tubes touching cold ground are a specific risk; and
- the `Tamb` sensor should be used to measure temperature variation with height.

A single air-temperature measurement therefore does not establish the temperature of every tube section, connector, filter housing, or analyzer inlet. A cold metal fitting, floor contact, vertical gradient, or local draft could be several degrees colder than the displayed `Tamb`.

## Other plausible explanations

The available screenshot does not identify which mechanism dominated. Plausible and testable explanations include:

1. A tube, connector, filter, or analyzer-inlet surface was colder than the entered `Tamb` proxy.
2. A short humidity peak occurred during a temperature or flow transition and was missed by the stored measurement interval.
3. Local humidity near the leaf or within part of the head exceeded the downstream, mixed-gas value.
4. Condensation biased downstream `H2Osam` and calculated `wa` downward.
5. Droplets formed under earlier conditions and remained visible after the current conditions became nominally safe.
6. Temperature or H2O sensor offsets, abnormal internal gradients, leakage, or liquid carry-over affected the observation.

## Recommended diagnostic measurements

Before reactivating a planning calculator, reproduce the event with additional measurements:

1. Attach independent surface-temperature probes to the cuvette outlet, return tube, connectors, filter housing, and analyzer inlet.
2. Measure air temperature at the instrument, tube height, and floor rather than relying on one `Tamb` position.
3. Log `H2Osam`, `H2Oabs`, `dH2OMP`, `wa`, flow, `Tcuv`, and `Tamb` at the fastest useful interval around temperature and flow transitions.
4. Start with a dry system and record the first location and time at which droplets appear.
5. Record whether condensation begins during a transition or after a steady state has been reached.
6. Check temperature and H2O sensor calibration if measured surfaces and humidity still fail to explain the event.

## Requirements for any future app version

A future calculator should:

- call its results **nominal calculated margins**, not safety determinations;
- distinguish entered `Tamb` from a measured coldest wetted-surface temperature;
- show an unknown state when that coldest surface has not been measured;
- state that downstream `wa` may be biased low after condensation begins;
- avoid green "safe" language unless all necessary measurements are available;
- preserve the distinction between the manual's `Tcuv - 2°C` estimate and an independently selected operational buffer; and
- remain clearly labelled as a planning aid rather than an equipment interlock.

## Related WALZ variable note

The CSV variable `VPD [Pa/kPa]` is normalized leaf-to-air vapour pressure deficit, not ordinary VPD expressed directly in kPa. Convert it using:

```text
VPD [kPa] = VPD [Pa/kPa] × Pamb [kPa] / 1000
```

VPD is not used to calculate the dew point in the retained app code.

## Source

- [WALZ GFS-3000 Manual, 9th edition](https://www.walz.com/files/downloads/gfs-3000_manual_9.pdf), especially the pneumatic path in Chapter 4, humidity-control recommendations in Section 4.4, and H2O/`wa` equations in Sections 9.4-9.6.
