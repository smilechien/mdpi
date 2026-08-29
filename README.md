# BibliometricMDPI

An R/Shiny application for exploring MDPI journal metadata and article-level bibliometric relationships. BibliometricMDPI prepares journal records by ISSN and publication period, extracts DOI-linked author keywords, and renders co-occurrence analyses with the FLCA engine.

## Features

- Filter local MDPI journal metadata by ISSN and publication date.
- Extract and reconcile author keywords from DOI records and cached metadata.
- Analyse co-occurrence among authors, keywords, countries, institutes, and departments.
- Display Top-20 counts, full/reduced networks, Sankey plots, chord plots, SSPlots, Kano plots, slope plots, and cluster/edge tables.
- Parse MDPI-style reference lists to explore author-journal relationships.
- Build a graphical-abstract network from article text, excluding author front matter, references, and abbreviations before FLCA analysis.
- Download metadata and summary tables as CSV, and the summary report as PNG.

## Requirements

Install R (version 4.1 or later recommended) and these packages:

```r
install.packages(c("shiny", "readr", "dplyr", "ggplot2", "tibble", "circlize"))
```

`circlize` is optional; it is required only for the chord-diagram output.

## Run locally

Clone or download this repository, open R in the project directory, and run:

```r
shiny::runApp()
```

Alternatively, open `app.R` in RStudio and select **Run App**.

## Data files

The application expects the following files in the same directory as `app.R`:

| File | Purpose |
| --- | --- |
| `Metrics_MDPI_metadata.csv` | Main article-level metadata source. |
| `Metrics_DOI_author_keywords.csv` | Cached DOI-to-author-keyword records. |
| `flca_ms_sil_module_76_v40_top20_max4_preserve_values.R` | FLCA clustering and network engine. |
| `renderSSplot.R` | SSPlot renderer. |
| `kano.R` | Kano-plot renderer. |

The included CSV files are local data sources. The current application does not require a live publisher or citation-service connection to run.

## Workflow

1. **Collect metadata** — enter an MDPI journal ISSN and optional date range, then select **Collect metadata**.
2. **Extract author keywords** — retain or paste DOI values and select **Extract keywords**.
3. **Build FLCA** — select **Build co-occurrence** and choose an entity domain (authors, keywords, countries, institutes, or departments).
4. **Review references** — paste MDPI-style references, select **Extract authors & journals**, and inspect the reference FLCA tab.
5. **Create a graphical abstract** — paste article content and select **Extract keywords**. The application removes content before an `Abstract` heading and the `References`/`Bibliography` section. It prioritizes declared keywords and full named concepts, applies the selected minimum occurrence threshold, removes abbreviations immediately before FLCA, and renders a fixed Top-20 network.

## Outputs

Each FLCA entity tab provides:

- Top-20 node counts
- Full and reduced co-occurrence networks
- Sankey and chord diagrams
- SSPlot and Kano plot
- Slope plot, when dated data are available
- Top-20 cluster and edge tables

The **Summary** tab provides top-five metadata summaries, publication counts, citation totals, and AAC-related reporting.

## Citation

If you use BibliometricMDPI in research, please cite the associated study/software record once available. Until then, cite this repository, including its version or commit identifier and access date.

## Limitations

- Results depend on the completeness and consistency of the supplied metadata.
- Entity parsing and reference parsing are heuristic and should be checked for complex or non-standard records.
- The graphical-abstract workflow is intended for exploratory concept mapping, not automated systematic-review screening.
- Network outputs describe co-occurrence, not causal or collaboration relationships.

## License

Add the project license here (for example, MIT) before public release.
