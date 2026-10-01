# La Selva Edge Effects

Analysis code for **how anthropogenic and riparian (river) forest edges affect howler monkey (*Alouatta palliata*) activity budgets, group cohesion and roaring behaviour at La Selva, Costa Rica.**

## Questions

1. Do the probabilities of resting, feeding and moving differ between forest interior, anthropogenic edge and combined anthropogenic + riparian edge zones?
2. Does group cohesion (number of nearest neighbours within 5 m, distance to nearest neighbour) differ between those zones?
3. How do activity and cohesion change with continuous distance from the anthropogenic edge and from the river, and how far into the forest does any edge effect reach (depth of edge influence, DEI)?
4. How do roaring measures (bout length, howls per bout, howls per minute, roar bouts per hour, probability of roaring) change with distance from each edge type?

## Methods

- **Data preparation** (`dplyr`, `stringr`, `Hmisc`): field data from 2018–2019, 2022, 2023 and 2024 are merged; each scan is assigned to a forest zone using a 100 m threshold on distance to the anthropogenic edge and to the river (interior, anthropogenic, riparian, or both). For the distance analyses, observations are grouped by GPS waypoint and binned into 15 m distance bands, with `nObs`-weighted bin means (`weighted.mean`, `wtd.var`) and SE = SD / √(waypoints per bin)
- **Zone comparisons, GLMs** (`glm`): binomial GLMs on per-waypoint counts (resting, feeding, moving), a Poisson GLM for number of nearest neighbours and a Gaussian GLM for log(distance to nearest neighbour + 0.1); each compared to a null model by likelihood-ratio test, with pairwise Tukey contrasts (`multcomp::glht`), marginal means (`emmeans`) and compact letter displays (`cld`)
- **Zone comparisons, GLMMs** (`glmmTMB`): the same response families fitted per scan, with a random intercept for waypoint ID (`(1 | id)`) and matching null models
- **Edge-distance models**: six candidate models fitted to the binned data and compared by AIC: null and linear (`lm`), power and logistic (`minpack.lm::nlsLM`), segmented (`segmented`) and step/changepoint (`chngpt::chngptm`); pseudo-R² from `rcompanion::nagelkerke`
- **Depth of edge influence**: taken from the lowest-AIC model per response, either as the logistic inflection point (95% CI by the delta method, `msm::deltamethod`), the segmented breakpoint (`confint`), or, for linear fits, the distance at which two-thirds of the predicted change is reached (inverse prediction with a 10,000-replicate nonparametric bootstrap, `investr::invest`)
- **Roaring analysis**: the same candidate models fitted to 15 m distance bins of the roaring data, weighted in two ways: by number of observations per bin (`*_model_fitting.R`) and by inverse variance, 1/SE² (`se_fitting.R`)
- **Tables and figures**: `ggplot2` / `ggpubr` figures; AIC summary table built with `gt`

## Repository contents

| File / folder | Description |
|---|---|
| `Data/` | Raw yearly field data (`La_Selva_2018_2019.csv`, `La_Selva_2022.csv`, `La_Selva_2023.csv`, `La_Selva_2024.csv`), the cleaning script `Data_Cleaning_Prep.R`, and the per-waypoint summary `monkeyIdData.csv` |
| `all_la_selva_data.csv` | Cleaned, combined scan-level dataset used by the GLM and GLMM scripts |
| `glm_fitting.R` | Binomial / Poisson / Gaussian GLMs comparing forest zones, with likelihood-ratio tests and pairwise contrasts |
| `glmm_fitting.R` | Equivalent `glmmTMB` mixed models with a random effect for waypoint ID |
| `glm_plots/`, `glmm_plots/` | Per-response estimate plots (resting, feeding, moving, number and distance of nearest neighbours) |
| `behavior_glm_plots.pdf`, `cohesion_glm_plots.pdf` | Combined multi-panel GLM figures |
| `anth_edge/` | Binned anthropogenic-edge data (`anthBinData.csv`), candidate model fitting (`anth_model_fitting.R`), DEI estimation (`anth_DEI_models.R`), saved workspace (`anthDEImodels.RData`), model comparison plots (`model_fit_plots/`) and the combined DEI figure (`allDEIplotsAnth.pdf`) |
| `riv_edge/` | Same structure for the riparian edge (`rivBinData.csv`, `riv_model_fitting.R`, `riv_DEI_models.R`, `allDEIplotsRiv.pdf`) |
| `roaring_analysis/data/` | Raw roaring data (bout length and howls per bout; roar bouts per hour) and `data_prep.R`, which produces the binned files |
| `roaring_analysis/anth_edge/`, `roaring_analysis/riv_edge/` | Binned roaring data per edge type, observation-weighted model fitting (`*_model_fitting.R`, plots in `n_obs_plots/`) and SE-weighted fitting (`se_fitting.R`, plots in `SE_plots/` / `se_plots/`) |
| `table_creation.R`, `AIC_table.pdf` | AIC / pseudo-R² / DEI summary table |

## Reproducing the analysis

Open `La_Selva_Edge_Effects.Rproj` in RStudio and install the packages below. Scripts read and write files by relative path, so set the working directory to each script's own folder before running it. Several `write.csv()` / `ggexport()` calls are commented out; uncomment them to regenerate the intermediate CSVs and PDFs.

1. `Data/Data_Cleaning_Prep.R`: combined dataset and binned anthropogenic / riparian data
2. `glm_fitting.R` and `glmm_fitting.R`: forest-zone comparisons
3. `anth_edge/anth_model_fitting.R` → `anth_edge/anth_DEI_models.R`, and `riv_edge/riv_model_fitting.R` → `riv_edge/riv_DEI_models.R`: edge-distance models and DEI
4. `roaring_analysis/data/data_prep.R`, then the `*_model_fitting.R` and `se_fitting.R` scripts in `roaring_analysis/anth_edge/` and `roaring_analysis/riv_edge/`
5. `table_creation.R`: AIC summary table

```r
install.packages(c("tidyverse", "Hmisc", "emmeans", "multcomp", "glmmTMB", "ggpubr",
                   "segmented", "strucchange", "chngpt", "minpack.lm", "rcompanion",
                   "investr", "msm", "knitr", "gt", "webshot2"))
```

## Author

**Kurt Riggin**: [GitHub](https://github.com/kriggithub) · [ORCID](https://orcid.org/0009-0004-4700-1251)
