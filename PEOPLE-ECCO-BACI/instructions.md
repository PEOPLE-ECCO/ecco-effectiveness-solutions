# PEOPLE-ECCO BACI — User Instructions

This app implements a spatial Before-After Control-Impact (BACI) analysis
workflow for evaluating the impact of conservation interventions.
The workflow proceeds through three steps, each corresponding to a tab above.

---

## Step 1 — Extract matching covariates

Upload your vector dataset of units of analysis (polygons or points).
You can extract additional covariate layers from local raster or vector files, or URL rasters.

The output is a GeoPackage containing all units with their covariate values
appended as attributes, ready for use in the matching step.

---

## Step 2 — Matching analysis

Use the covariate-enriched vector from Step 1 (or load an existing file)
to match treatment units to similar control units using
[MatchIt](https://kosukeimai.github.io/MatchIt/).

Configure the matching method, estimand, and parameters in the matching
parameters card, then click **Run matching**. The evaluation section below
the run button shows the matched pairs on an interactive map, covariate
balance statistics, and covariate overlap.

Save the matched dataset to a GeoPackage for use in Step 3.

---

## Step 3 — Impact evaluation

Load the matched pairs vector from Step 2 (or load an existing file).
Select the before and after impact variables — these can be attributes
already present in the matched vector, or extracted on the fly from
external raster sources.

Click **Run impact assessment** to compute BACI contrasts and p-values
for each impact unit. Results are displayed on an interactive map;
click any unit to see a table of contrasts and p-values per variable.
Save the results to a GeoPackage using the save card below the map.

---

## Further information

- Project website: [www.people-ecco.eu](https://www.people-ecco.eu)
- User handbook: [people-ecco.github.io](https://people-ecco.github.io)
- Source code and issue tracker: [github.com/PEOPLE-ECCO/ecco-baci-solutions](https://github.com/PEOPLE-ECCO/ecco-baci-solutions)

---

## Citation

If you use this tool research, please cite:

> Willemen et al. (in preparation)

