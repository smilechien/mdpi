# BibliometricMDPI

 
使用者附件
使用者附件
in mode 5. Author publications and coauthorship
Author name
OpenAlex API key for author search where lacks 7 metrics on kano plot and network plot as doen in jmirscopus/app.R

星期五下午5:30
Mode 5 now shows a first/last-author FLCA network and a Kano plot with all seven metric cards: AAC, SHC, CLI, h-index, MAR, n, and n*. The network and Kano plot use the same selected FLCA nodes; Kano bubble size now reflects SHC.

I updated app.R, openalex_author_mode.R, kano.R, and the README. The app loads successfully, and synthetic data passed checks with the actual FLCA runner and Kano renderer. I could not verify a live OpenAlex profile without an API key.


已編輯 4 個檔案
+210
-45
openalex_author_mode.R
openalex_author_mode.R
+85
-2
kano.R
kano.R
+7
-1
app.R
app.R
+115
-39

顯示另外 1 個檔案
星期五下午5:35
使用者附件
使用者附件
why this (left) is different from the one(right) from jmirscopus/app.R using the same mode 5 with identical name

下午5:49
The plots appear to use different OpenAlex author profiles, despite both being labeled Carlo Galli. The left shows h-index 7 and about 90 works; the right shows 38 and 120 works. The screenshots do not show the author IDs, so I cannot confirm the identity mismatch from the images alone.

I found why the apps could choose different profiles: jmirmetrics reordered name matches before selecting the first one, while jmirscopus kept OpenAlex’s result order. Mode 5 also used a different FLCA module, and its fixed plot width clipped the seven cards.

I updated app.R and openalex_author_mode.R to keep the reference search order, show each profile’s ID and institution, use the reference Mode 5 FLCA module, and let the plots fit the panel. The app loads and the analysis passes a synthetic FLCA check. Restart the running Shiny app, then compare the selected OpenAlex ID in both apps; matching names alone will not ensure matching results.


已編輯 3 個檔案
+38
-12
openalex_author_mode.R
openalex_author_mode.R
+7
-5
app.R
app.R
+28
-5
README.md
README.md
+3
-2
下午5:51
使用者附件
missing dendrogram that is in jmirscopus/app.R

下午5:54
I added the missing first/last-author FLCA dendrogram to mode 5 in app.R. It uses the same selected nodes and strongest leader–follower links as the mode 5 analysis, shows work counts beside names, and highlights the highest-count leader in red.

The renderer is in openalex_dendrogram.R. I rendered and checked it with a test network. Restart the Shiny app to see the new plot.


已編輯 4 個檔案
+100
-1
F:/jmirmetrics/openalex_author_mode.R
F:/jmirmetrics/
openalex_author_mode.R
+13
-0
F:/jmirmetrics/openalex_dendrogram.R
F:/jmirmetrics/
openalex_dendrogram.R
+70
-0
F:/jmirmetrics/app.R
F:/jmirmetrics/
app.R
+15
-0

顯示另外 1 個檔案
下午5:55
使用者附件
the dendrogram should be identical to the FLCA dendrogram under the tab of dendrogram in MDPI Journal Metadata & FLCA Explorer, including solid and dashed line in red or grey with WCD

下午5:58
Mode 5 now uses the same FLCA dendrograph renderer as the metadata Dendrogram tab. It shows solid red leader stems, grey follower stems scaled and labeled by WCD, dashed grey stems for disconnected roots, and names and counts beneath aligned bubbles.

I updated app.R and checked renders with both connected and disconnected test networks. Restart the Shiny app to see the change.


已編輯 3 個檔案
+25
-80
app.R
app.R
+24
-8
openalex_dendrogram.R
openalex_dendrogram.R
+0
-70
README.md
README.md
+1
-2
下午6:00
proivde README.md for github for the app.R

上午7:42
I updated the GitHub-ready README.md with setup steps, the app workflow, FLCA and OpenAlex metric definitions, repository files, outputs, and limitations.

It also makes the current data scope clear: Collect metadata reads the bundled 49-article CSV and filters by date; it does not fetch articles by ISSN. I checked that the documented files exist and the Markdown code fences are balanced.


已編輯 README.md
+36
-22
上午7:44



README.md
# BibliometricMDPI
# MDPI Journal Metadata & FLCA Explorer

An R/Shiny application for exploring MDPI journal metadata and article-level bibliometric relationships. BibliometricMDPI prepares journal records by ISSN and publication period, extracts DOI-linked author keywords, and renders co-occurrence analyses with the FLCA engine.
An R/Shiny app for exploring article metadata, co-occurrence networks, and FLCA (follower–leader cluster analysis). The repository includes a 49-article MDPI metadata dataset and a DOI keyword cache. A separate OpenAlex mode analyses the first and last authors of a selected author profile.


- Filter local MDPI journal metadata by ISSN and publication date.
- Extract and reconcile author keywords from DOI records and cached metadata.
- Filter the bundled article metadata by publication date.
- Reconcile author keywords from bundled article records and the local DOI cache.
- Analyse co-occurrence among authors, keywords, countries, institutes, and departments.
- Choose a metadata domain in the upper panel, then view its Top-20 counts, full/reduced networks, dendromap, Sankey plot, chord plot, SSPlot, Kano plot, slope plot, and cluster/edge tables in the lower panel. The selected domain tab and selected top-level app tab (including FLCA Process and Summary) are shown in bold red text.
- Choose a metadata domain in the upper panel, then view its Top-20 counts, full/reduced networks, dendrograph, Sankey plot, chord plot, SSPlot, Kano plot, slope plot, and cluster/edge tables in the lower panel. The selected domain tab and selected top-level app tab (including FLCA Process and Summary) are shown in bold red text.
- Parse MDPI-style reference lists to explore author-journal relationships.
- Build a graphical-abstract network from article text, excluding author front matter, references, and abbreviations before FLCA analysis.
- Search an OpenAlex author profile by name and API key, inspect its publications, and view a first/last-author FLCA network, seven author metrics, and a Kano plot.
- Search an OpenAlex author profile by name and API key, inspect its publications, and view a first/last-author FLCA network, seven author metrics, a Kano plot, and a dendrograph.
- Download metadata and summary tables as CSV, and the summary report as PNG.

Install R (version 4.1 or later recommended) and these packages:
Use R 4.1 or later and install these required packages:

```r
install.packages(c("shiny", "readr", "dplyr", "ggplot2", "tibble", "httr", "jsonlite", "ggrepel", "circlize"))
install.packages(c("shiny", "readr", "dplyr", "ggplot2", "tibble", "httr", "jsonlite", "ggrepel"))
```

`circlize` is optional; it is required only for the chord-diagram output.
`circlize` is needed for chord plots. The FLCA modules can use `cluster` for silhouette calculations, and SSPlot can use `ragg` for image rendering when available:

```r
install.packages(c("circlize", "cluster", "ragg"))
```

```r
shiny::runApp()
shiny::runApp(".")
```

Alternatively, open `app.R` in RStudio and select **Run App**.
Alternatively, open `app.R` in RStudio and select **Run App**. Run from the repository directory because `app.R` reads its CSV and helper files by relative path.

## Data files
## Repository files

The application expects the following files in the same directory as `app.R`:
The application expects these files in the same directory as `app.R`:

| --- | --- |
| `app.R` | Shiny interface, local data workflow, visualizations, and downloads. |
| `Metrics_MDPI_metadata.csv` | Main article-level metadata source. |
| `openalex_author_mode.R` | OpenAlex author lookup and first/last-author data preparation. |
| `openalex_flca_ms_sil_module.R` | FLCA module copied from `jmirscopus` for Mode 5 parity. |
| `openalex_flca_ms_sil_module.R` | FLCA engine for OpenAlex author mode. |
| `MDPI_Metrics_DOI_to_keywords_and_coword_FIXED.R` | Separate metadata and keyword preparation script; `app.R` does not source it. |

The included CSV files are local data sources. The metadata and FLCA workflow runs locally. The optional OpenAlex author mode requires an internet connection and an OpenAlex API key.
The included CSV files are local data sources. The metadata and FLCA workflow runs locally. The separate OpenAlex author mode requires an internet connection and an OpenAlex API key.

### Current data scope

**Collect metadata** reads the bundled `Metrics_MDPI_metadata.csv` and applies the publication-date inputs. The file contains 49 article records. The ISSN field is displayed in the workflow and may be prefilled with a URL parameter, but the current `app.R` does **not** fetch or filter articles by ISSN. The MDPI publisher and OpenAlex citation checkboxes do not trigger live metadata retrieval in this version. **Extract keywords** combines available keyword fields with the bundled DOI cache; it does not make a live DOI keyword request.

The OpenAlex author search is live and separate from this bundled dataset. Enter its key in the sidebar or set `OPENALEX_API_KEY` in the R process environment. The app sends the key in an authorization header and does not save it to a project file.


1. **Collect metadata** — enter an MDPI journal ISSN and optional date range, then select **Collect metadata**.
2. **Extract author keywords** — retain or paste DOI values and select **Extract keywords**.
1. **Collect metadata** — choose a publication-date range and select **Collect metadata** to load the bundled article dataset.
2. **Extract author keywords** — review the DOI list and select **Extract keywords** to reconcile available local keyword sources.
3. **Build FLCA** — select **Build co-occurrence**, choose an entity domain in the upper panel (authors, keywords, countries, institutes, or departments), then choose a visualization in the lower panel.
5. **Create a graphical abstract** — paste article content and select **Extract keywords**. The application removes content before an `Abstract` heading and the `References`/`Bibliography` section. It prioritizes declared keywords and full named concepts, applies the selected minimum occurrence threshold, removes abbreviations immediately before FLCA, and renders a fixed Top-20 network.
6. **Explore an OpenAlex author** — enter an author name and API key in the bottom sidebar section, select **Search OpenAlex**, and verify the matching profile in the **OpenAlex authors** tab. The app loads up to 10,000 works, retrieves full bylines for works whose list response truncates authors, and displays a first/last-author FLCA network, seven metrics, Kano plot, and downloadable publication table. The key can also be supplied through `OPENALEX_API_KEY`; it is sent as an authorization header and is not written to a file.
6. **Explore an OpenAlex author** — enter an author name and API key in the bottom sidebar section, select **Search OpenAlex**, and confirm the correct **OpenAlex ID** and institution in the **OpenAlex authors** tab. The app loads up to 10,000 works, retrieves full bylines for works whose list response truncates authors, and displays a first/last-author FLCA network, seven metrics, Kano plot, dendrograph, and downloadable publication table.

Add `?ISSN=3042-5042` to the app URL to prefill the ISSN field. This does not change the bundled dataset.


**OpenAlex author mode.** This is a separate author search, not the local metadata Author FLCA. Search matches retain OpenAlex relevance order; the selected OpenAlex ID, institution, h-index, and profile link are visible so identical names can be distinguished. Each work contributes its first and last OpenAlex author IDs; these form the weighted coauthorship network. Mode 5 uses the same FLCA module as `jmirscopus` and selects up to 20 nodes from the 100 most frequent candidates. The network shows their shared-work links. The mode 5 dendrograph reuses the metadata-domain FLCA renderer with those same selected nodes and strongest leader links: red solid leader stems, grey follower stems with width and labels showing WCD, dashed grey stems for disconnected roots, and aligned bubbles with names and occurrence counts below. The Kano plot places the sum of displayed link weights on the horizontal axis, first/last-author work count on the vertical axis, and SHC (1 − Burt constraint) in bubble size. The cards report AAC from the top three selected role-work counts, focal SHC, CLI = last/first works, OpenAlex profile h-index, MAR = (n − n*)/n, OpenAlex work count n, and n* = retrieved works where the focal author is first or last. AAC, CLI, and MAR are unavailable when the profile exceeds the 10,000-work fetch limit; n* is then a lower bound. These are descriptive bibliometric measures, not Kano survey responses.
**OpenAlex author mode.** This is a separate author search, not the local metadata Author FLCA. Search matches retain OpenAlex relevance order; the selected OpenAlex ID, institution, h-index, and profile link are visible so identical names can be distinguished. Each work contributes its first and last OpenAlex author IDs; these form the weighted coauthorship network. Mode 5 uses `openalex_flca_ms_sil_module.R` and selects up to 20 nodes from the 100 most frequent candidates. The network shows their shared-work links. The mode 5 dendrograph reuses the metadata-domain FLCA renderer with those same selected nodes and strongest leader links: red solid leader stems, grey follower stems with width and labels showing WCD, dashed grey stems for disconnected roots, and aligned bubbles with names and occurrence counts below. The Kano plot places the sum of displayed link weights on the horizontal axis, first/last-author work count on the vertical axis, and SHC (1 − Burt constraint) in bubble size. The cards report AAC from the top three selected role-work counts, focal SHC, CLI = last/first works, OpenAlex profile h-index, MAR = (n − n*)/n, OpenAlex work count n, and n* = retrieved works where the focal author is first or last. AAC, CLI, and MAR are unavailable when the profile exceeds the 10,000-work fetch limit; n* is then a lower bound. These are descriptive bibliometric measures, not Kano survey responses.


If you use BibliometricMDPI in research, please cite the associated study/software record once available. Until then, cite this repository, including its version or commit identifier and access date.
For research use, cite the repository URL together with its commit identifier and access date.

- The graphical-abstract workflow is intended for exploratory concept mapping, not automated systematic-review screening.
- Network outputs describe co-occurrence, not causal or collaboration relationships.
- The bundled metadata networks describe co-occurrence, not causal influence or formal author roles. The OpenAlex mode describes shared first/last authorship of retrieved works; it does not establish the nature or quality of a collaboration.


Add the project license here (for example, MIT) before public release.
No license file is currently included. Contact the repository owner about reuse terms.

