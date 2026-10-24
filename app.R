# MDPI journal metadata and FLCA explorer — reconstructed application shell.


`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

# MDPI journal metadata and FLCA explorer — reconstructed application shell.
required <- c("shiny", "readr", "dplyr", "ggplot2", "tibble", "httr", "jsonlite", "ggrepel")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "), call. = FALSE)
suppressPackageStartupMessages({ library(shiny); library(readr); library(dplyr); library(ggplot2); library(tibble) })
source("openalex_author_mode.R", local = TRUE)

metadata_path <- "Metrics_MDPI_metadata.csv"
keyword_cache_path <- "Metrics_DOI_author_keywords.csv"
default_dois <- tryCatch({
  cache_dois <- readr::read_csv(keyword_cache_path, show_col_types = FALSE)
  names(cache_dois) <- sub("^\\ufeff", "", names(cache_dois))
  doi_name <- intersect(c("DOI", "doi"), names(cache_dois))[1]
  if (length(doi_name) && !is.na(doi_name)) paste(unique(na.omit(cache_dois[[doi_name]])), collapse = "\n") else ""
}, error = function(e) "")
default_article_content <- paste(c(
  "Abstract", "General-purpose bibliometric tools do not address domain-specific needs of occupational safety and health (OSH) evidence mapping, such as quantifying whether the literature connects hazard characterisation with real worker exposure or identifying which articles best bridge science and preventive practice. This paper presents ORISMA (Occupational Risk Integrated Systematic Mapping and Analysis), an open-source R package that introduces five preventive bibliometric indicators: the Worker-Risk Disconnection Index (WRDI), Risk Category Saturation Index (RCS), Material-Gap Profile (MGP), Abstract Sufficiency Score (ASS), and Bridge Article Score (BAS). A complete evidence map is produced through three function calls. To demonstrate the package, we apply it to a corpus of bibliographic records on occupational risks in metal additive manufacturing (metal AM). The global WRDI was 0.4474, indicating moderate disconnection between hazard characterisation and worker exposure evidence. Industrial hygiene and emerging technology domains dominated preventive coverage, while safety, ergonomics, psychosocial, and biological risk domains were largely absent. ORISMA provides a reproducible, domain-aware framework that requires further validation across independent OSH domains before broad generalization.",
  "Keywords: bibliometrics; bridge articles; evidence mapping; metal additive manufacturing; occupational safety and health; R package; worker-risk disconnection index"
), collapse = "\n")
default_mdpi_references <- paste(c("International Labour Organization. World Employment and Social Outlook 2022: Trends; ILO: Geneva, Switzerland, 2022.", "1. Aria, M.; Cuccurullo, C. bibliometrix: An R-tool for comprehensive science mapping analysis. J. Informetr. 2017, 11, 959-975.", "2. van Eck, N.J.; Waltman, L. Software survey: VOSviewer, a computer program for bibliometric mapping. Scientometrics 2010, 84, 523-538.", "3. Chen, C. CiteSpace II: Detecting and visualizing emerging trends and transient patterns in scientific literature. J. Am. Soc. Inf. Sci. Technol. 2006, 57, 359-377."), collapse = "\n")
empty_text <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
unique_terms <- function(x) {
  x <- x[!empty_text(x)]
  if (!length(x)) return(character())
  terms <- trimws(unlist(strsplit(paste(x, collapse = ";"), "[;|]", perl = TRUE)))
  unique(terms[nzchar(terms) & !tolower(terms) %in% c("na", "n/a", "null")])
}
derive_affiliation_fields <- function(df) {
  if (!"Affiliations" %in% names(df)) return(df)
  split_affiliations <- function(x) trimws(unlist(strsplit(ifelse(is.na(x), "", x), ";", fixed = TRUE)))
  clean_institute <- function(x) trimws(x)
  clean_department <- function(x) trimws(sub("^Department of\\s+", "", x, ignore.case = TRUE))
  departments <- lapply(df$Affiliations, function(x) {
    parts <- split_affiliations(x); hits <- unlist(regmatches(parts, gregexpr("Department of[^,;]+", parts, ignore.case = TRUE, perl = TRUE)))
    paste(unique(clean_department(hits[nzchar(hits)])), collapse = "; ")
  })
  institutes <- lapply(df$Affiliations, function(x) {
    parts <- split_affiliations(x)
    bits <- trimws(unlist(strsplit(parts, ",", fixed = TRUE)))
    hits <- bits[grepl("University|Institute|College|Hospital|School", bits, ignore.case = TRUE)]
    paste(unique(clean_institute(hits[nzchar(hits)])), collapse = "; ")
  })
  country_dictionary <- c("United States", "United Kingdom", "South Korea", "New Zealand", "Saudi Arabia", "United Arab Emirates", "Hong Kong", "Taiwan", "China", "Italy", "Spain", "Australia", "Germany", "France", "Japan", "Canada", "Brazil", "India", "Netherlands", "Sweden", "Norway", "Finland", "Denmark", "Belgium", "Switzerland", "Portugal", "Greece", "Singapore", "Mexico", "Turkey", "Austria", "Poland", "Ireland", "Israel", "UK", "USA")
  countries_from_affiliations <- lapply(df$Affiliations, function(x) {
    text <- paste(split_affiliations(x), collapse = "; ")
    hits <- country_dictionary[vapply(country_dictionary, function(country) grepl(tolower(country), tolower(text), fixed = TRUE), logical(1))]
    hits[hits == "UK"] <- "United Kingdom"; hits[hits == "USA"] <- "United States"
    paste(unique(hits), collapse = "; ")
  })
  df$Countries_From_Affiliations <- unlist(countries_from_affiliations)
  df$Institutes_From_Affiliations <- unlist(institutes)
  df$Departments_From_Affiliations <- unlist(departments)
  # Keep the general fields complete while retaining the publisher-raw affiliation extractions.
  df$Countries <- mapply(function(a, b) paste(unique(c(unique_terms(a), unique_terms(b))), collapse = "; "), df$Countries, df$Countries_From_Affiliations, USE.NAMES = FALSE)
  if (!"Departments" %in% names(df)) df$Departments <- unlist(departments)
  if (!"Institutes" %in% names(df)) df$Institutes <- unlist(institutes)
  if (!"Countries" %in% names(df)) df$Countries <- ""
  df$Departments <- vapply(strsplit(ifelse(is.na(df$Departments), "", df$Departments), ";", fixed = TRUE), function(z) paste(unique(trimws(z[nzchar(trimws(z))])), collapse = "; "), character(1))
  df$Institutes <- vapply(strsplit(ifelse(is.na(df$Institutes), "", df$Institutes), ";", fixed = TRUE), function(z) paste(unique(trimws(z[nzchar(trimws(z))])), collapse = "; "), character(1))
  df
}

read_metadata <- function() {
  if (!file.exists(metadata_path)) return(tibble())
  x <- read_csv(metadata_path, show_col_types = FALSE)
  names(x) <- sub("^\\ufeff", "", names(x))
  derive_affiliation_fields(x)
}

read_keyword_cache <- function() {
  if (!file.exists(keyword_cache_path)) return(tibble())
  x <- tryCatch(read_csv(keyword_cache_path, show_col_types = FALSE), error = function(e) tibble())
  names(x) <- sub("^\\ufeff", "", names(x))
  x
}
normalise_doi <- function(x) {
  x <- trimws(tolower(as.character(x)))
  sub("^https?://(dx\\.)?doi\\.org/", "", x, perl = TRUE)
}
column_or_blank <- function(df, choices) {
  found <- choices[choices %in% names(df)][1]
  if (is.na(found) || !length(found)) rep("", nrow(df)) else as.character(df[[found]])
}
extract_year <- function(x) suppressWarnings(as.integer(substr(as.character(x), 1, 4)))
extract_reference_journal <- function(entry) {
  # The saved References field is flattened prose.  Take terms before "Vol" only;
  # never use a DOI fragment (e.g., 10.1111/clr) as a journal name.
  before_vol <- sub("\\.\\s*Vol\\.?\\s*[0-9].*$", "", entry, perl = TRUE)
  if (identical(before_vol, entry)) return(NA_character_)
  parts <- trimws(unlist(strsplit(before_vol, "\\.\\s+", perl = TRUE)))
  parts <- gsub("\\.$", "", parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(NA_character_)
  take <- character()
  for (part in rev(parts)) {
    if (grepl("doi:|10\\.", part, ignore.case = TRUE) || nchar(part) > 45 || grepl("\\)$", part)) break
    take <- c(part, take)
    if (length(take) >= 4) break
  }
  journal <- paste(take, collapse = ". ")
  journal <- trimws(gsub("\\s+", " ", journal))
  if (!nzchar(journal) || grepl("10\\.|/", journal)) NA_character_ else journal
}

reference_journal_year <- function(references) {
  all_rows <- lapply(references, function(value) {
    if (length(value) != 1 || empty_text(value)) return(NULL)
    entries <- trimws(unlist(strsplit(value, "||", fixed = TRUE)))
    rows <- lapply(entries, function(entry) {
      year_text <- regmatches(entry, regexpr("(19|20)[0-9]{2}", entry, perl = TRUE))
      if (!length(year_text) || empty_text(year_text)) return(NULL)
      year <- suppressWarnings(as.integer(year_text[1]))
      journal <- extract_reference_journal(entry)
      if (length(year) != 1 || is.na(year) || length(journal) != 1 || empty_text(journal)) return(NULL)
      tibble(journal = journal, year = year, n = 1L)
    })
    bind_rows(Filter(Negate(is.null), rows))
  })
  bind_rows(Filter(Negate(is.null), all_rows))
}

parse_mdpi_references <- function(text) {
  if (empty_text(text)) return(tibble())
  z <- trimws(unlist(strsplit(text, "\n"))); z <- z[nzchar(z)]; starts <- grepl("^\\s*\\d+\\.\\s+", z)
  if (any(starts)) { g <- cumsum(starts); g[g == 0] <- seq_len(sum(g == 0)) - sum(g == 0); z <- unname(tapply(z, g, paste, collapse = " ")) }
  bind_rows(lapply(seq_along(z), function(i) { x <- gsub("^\\s*\\d+\\.\\s+", "", z[i]); m <- regexpr("(19|20)\\d{2}", x); pre <- if(m[1]>0) trimws(substr(x,1,m[1]-1)) else x; p <- trimws(unlist(strsplit(pre,"\\.\\s+"))); a <- if(length(p))p[1]else ""; au <- trimws(unlist(strsplit(a,";",fixed=TRUE))); au <- au[grepl(",",au,fixed=TRUE)]; tibble(Reference=i, Raw_Reference=x, Authors=paste(au,collapse="; "), Journal=if(length(p)>=3)p[length(p)]else NA_character_, Title=if(length(p)>=2)p[2]else NA_character_, Year=if(m[1]>0)as.integer(regmatches(x,m))else NA_integer_) }))
}
article_text_without_authors_or_references <- function(text) {
  text <- as.character(text %||% "")
  # Journal front matter (title, authors, affiliations, DOI) is excluded when
  # a standard Abstract heading is supplied.
  abstract_at <- regexpr("(?im)^\\s*abstract\\s*$", text, perl = TRUE)
  if (abstract_at[1] > 0) text <- substr(text, abstract_at[1] + attr(abstract_at, "match.length"), nchar(text))
  # References, including author lists embedded in citation entries, never
  # contribute candidate terms or co-occurrences.
  text <- sub("(?is)\\n\\s*(references|bibliography)\\s*\\n.*$", "", text, perl = TRUE)
  text
}

extract_article_content_terms <- function(text, top_n = 20L, min_frequency = 2L) {
  text <- article_text_without_authors_or_references(text)
  if (empty_text(text)) return(list(terms = tibble(), sets = list()))
  abbreviations <- unique(unlist(regmatches(text, gregexpr("\\b[A-Z][A-Z0-9-]{1,12}\\b", text, perl = TRUE))))
  abbreviations <- abbreviations[!abbreviations %in% c("ABSTRACT", "KEYWORDS") & !grepl("^[0-9]+$", abbreviations)]
  raw_keywords <- regmatches(text, regexpr("(?i)keywords?\\s*:\\s*[^\\n]+", text, perl = TRUE))
  keywords <- if (length(raw_keywords) && nzchar(raw_keywords)) trimws(unlist(strsplit(sub("(?i)^keywords?\\s*:\\s*", "", raw_keywords, perl = TRUE), "[;,]", perl = TRUE))) else character()

  # A carriage return embedded in a copied PDF/HTML word is a wrap marker, not a word boundary. Remove it; ordinary line feeds remain sentence whitespace.
  text <- gsub("\r", "", text, fixed = TRUE)
  named_phrases <- unique(unlist(regmatches(text, gregexpr("\\b[A-Z][A-Za-z-]+(?:[ -][A-Z][A-Za-z-]+){1,4}\\b", text, perl = TRUE))))
  sentences <- trimws(unlist(strsplit(gsub("\\n+", " ", text), "(?<=[.!?])\\s+", perl = TRUE)))
  sentences <- sentences[nchar(sentences) > 10]
  # Complement declared keywords with repeated two-word phrases from article
  # sentences. This handles wording variants such as "bibliometric tools"
  # versus the declared keyword "bibliometrics".
  stop_words <- c("the","and","for","with","that","this","from","into","through","which","were","was","are","not","but","its","their","than","also","have","has","been","being","such","these","those","about","between","while","after","before","within","using","used","use","based","across","under","into","onto","where","when","what","who","how","all","any","each","other")
  sentence_phrases <- lapply(sentences, function(s) {
    words <- tolower(unlist(regmatches(s, gregexpr("[A-Za-z][A-Za-z-]{2,}", s, perl = TRUE))))
    words <- words[!words %in% stop_words]
    if (length(words) < 2) return(character())
    unique(vapply(seq_len(length(words) - 1L), function(i) paste(words[i:(i + 1L)], collapse = " "), character(1)))
  })
  phrase_freq <- sort(table(unlist(sentence_phrases)), decreasing = TRUE)
  min_frequency <- max(1L, as.integer(min_frequency))
  auto_phrases <- names(phrase_freq)[phrase_freq >= min_frequency]
  canonical_keyphrase <- function(term) {
    words <- strsplit(tolower(trimws(term)), "\\s+", perl = TRUE)[[1]]
    if (!length(words)) return("")
    last <- words[length(words)]
    if (grepl("ies$", last) && nchar(last) > 4) last <- sub("ies$", "y", last)
    else if (grepl("s$", last) && nchar(last) > 3 && !grepl("(ss|is)$", last)) last <- sub("s$", "", last)
    words[length(words)] <- last
    paste(words, collapse = " ")
  }
  declared_keywords <- unique(vapply(keywords, canonical_keyphrase, character(1)))
  priority_terms <- unique(vapply(c(keywords, named_phrases), canonical_keyphrase, character(1)))
  candidates <- unique(c(keywords, named_phrases, auto_phrases))
  candidates <- candidates[nzchar(candidates)]
  term_in_sentence <- function(term, sentence) {
    words <- strsplit(canonical_keyphrase(term), "\\s+", perl = TRUE)[[1]]
    if (!length(words) || !nzchar(words[1])) return(FALSE)
    words[length(words)] <- paste0(words[length(words)], "(?:s|es)?")
    grepl(paste0("\\b", paste(words, collapse = "\\s+"), "\\b"), sentence, ignore.case = TRUE, perl = TRUE)
  }
  sets <- lapply(sentences, function(s) { found <- candidates[vapply(candidates, term_in_sentence, logical(1), sentence = s)]; unique(vapply(found, canonical_keyphrase, character(1))) })
  freq <- sort(table(unlist(sets)), decreasing = TRUE)
  if (!length(freq)) return(list(terms = tibble(), sets = sets))
  qualified_terms <- names(freq)[freq >= min_frequency]
  priority_present <- intersect(priority_terms, qualified_terms)
  shorter_overlap <- qualified_terms[vapply(qualified_terms, function(term) term %in% priority_present || any(startsWith(priority_present, paste0(term, " "))), logical(1))]
  ranked_terms <- c(priority_present, setdiff(qualified_terms, shorter_overlap))
  keep <- ranked_terms[seq_len(min(as.integer(top_n), length(ranked_terms)))]
  sets <- lapply(sets, function(z) intersect(z, keep))
  list(terms = tibble(Term = keep, Count = as.integer(freq[keep]), Type = ifelse(keep %in% declared_keywords, "Declared keyword", ifelse(keep %in% priority_terms, "Named article concept", "Article keyphrase"))), sets = sets, abbreviations = abbreviations)
}
article_term_sets <- function(df, column) {
  if (!column %in% names(df)) return(list())
  lapply(df[[column]], function(x) unique_terms(x))
}
cooccurrence <- function(term_sets, top_n = 20) {
  singles <- sort(table(unlist(term_sets)), decreasing = TRUE)
  singles <- singles[seq_len(min(length(singles), top_n))]
  nodes <- names(singles)
  pairs <- lapply(term_sets, function(set) {
    set <- intersect(set, nodes)
    if (length(set) < 2) return(NULL)
    combn(sort(set), 2, simplify = FALSE)
  })
  pairs <- unlist(pairs, recursive = FALSE)
  if (!length(pairs)) return(list(nodes = tibble(term = nodes, frequency = as.integer(singles)), edges = tibble(from = character(), to = character(), weight = integer())))
  keys <- vapply(pairs, function(p) paste(p, collapse = "\r"), character(1))
  edge_tab <- sort(table(keys), decreasing = TRUE)
  edge_split <- strsplit(names(edge_tab), "\r", fixed = TRUE)
  list(nodes = tibble(term = nodes, frequency = as.integer(singles)), edges = tibble(from = vapply(edge_split, `[`, "", 1), to = vapply(edge_split, `[`, "", 2), weight = as.integer(edge_tab)))
}

flca_visual_tabs <- function(id) {
  tabsetPanel(
    tabPanel("Top-20 node counts", tableOutput(paste0(id, "_nodes"))),
    tabPanel("Full network", plotOutput(paste0(id, "_full"), height = "650px")),
    tabPanel("Reduced network", p("Arrow width is proportional to WCD; edge labels show the exact WCD."), plotOutput(paste0(id, "_reduced"), height = "650px")),
    tabPanel("Dendrograph",
      p("All FLCA nodes are aligned along the baseline, with occurrence counts below their names. A solid red stem marks each leader up to its own horizontal join. Grey follower stems show incoming WCD links, with width proportional to WCD and exact values labeled; bubble area reflects node value. The highest-count node has a red name. Disconnected components have dashed stems to the top reference line."),
      plotOutput(paste0(id, "_dendrogram"), height = "900px")),
    tabPanel("Sankey plot", plotOutput(paste0(id, "_sankey"), height = "650px")),
    tabPanel("Chord plot", plotOutput(paste0(id, "_chord"), height = "650px")),
    tabPanel("SSPlot", plotOutput(paste0(id, "_ss"), height = "900px")),
    tabPanel("Kano plot", plotOutput(paste0(id, "_kano"), height = "650px")),
    tabPanel("Slope plot", plotOutput(paste0(id, "_slope"), height = "900px")),
    tabPanel("Top-20 cluster table", tableOutput(paste0(id, "_clusters")), tableOutput(paste0(id, "_edges")))
  )
}

flca_entity_tab <- function(id, title) tabPanel(title, flca_visual_tabs(id))

summary_h <- function(x) { x <- sort(as.numeric(x[is.finite(x)]), decreasing=TRUE); if(!length(x)) return(0L); sum(x >= seq_along(x)) }
summary_aac <- function(counts) {
  v <- sort(as.numeric(counts[is.finite(counts) & counts > 0]), decreasing = TRUE)
  if (length(v) < 3 || v[2] == 0 || v[3] == 0) return(NA_real_)
  r <- (v[1] / v[2]) / (v[2] / v[3]); r / (1 + r)
}
summary_domain <- function(df, domain, column, citation_col = "Citation_Count") {
  if (!column %in% names(df)) return(tibble(Domain=character(), Element=character(), Count=integer(), Citations=numeric(), AAC=numeric()))
  cites <- if (citation_col %in% names(df)) suppressWarnings(as.numeric(df[[citation_col]])) else rep(NA_real_, nrow(df)); cites[!is.finite(cites)] <- 0
  long <- bind_rows(lapply(seq_len(nrow(df)), function(i) { z <- unique_terms(df[[column]][i]); if(!length(z)) return(NULL); tibble(article=i, Element=z, Citations=cites[i]) }))
  if (!nrow(long)) return(tibble(Domain=character(), Element=character(), Count=integer(), Citations=numeric(), AAC=numeric()))
  out <- long |> distinct(article, Element, .keep_all=TRUE) |> group_by(Element) |> summarise(Count=n(), h=summary_h(Citations), Citations=sum(Citations), .groups="drop") |> arrange(desc(Count), desc(Citations), Element) |> slice_head(n=5)
  mutate(out, Domain=domain, AAC=summary_aac(Count), .before=1)
}
summary_year <- function(df) {
  y <- extract_year(df$Publication_Date); cites <- if("Citation_Count" %in% names(df)) suppressWarnings(as.numeric(df$Citation_Count)) else rep(0,nrow(df)); cites[!is.finite(cites)]<-0
  z <- tibble(Element=as.character(y), Citations=cites) |> filter(!is.na(Element) & nzchar(Element)) |> group_by(Element) |> summarise(Count=n(), h=summary_h(Citations), Citations=sum(Citations), .groups="drop") |> arrange(desc(Count),desc(Citations),Element) |> slice_head(n=5)
  mutate(z,Domain="Year",AAC=summary_aac(Count),.before=1)
}

summary_reference_journal <- function(df) {
  if (!"References" %in% names(df)) return(tibble(Domain=character(), Element=character(), Count=integer(), Citations=numeric(), AAC=numeric()))
  cites <- if ("Citation_Count" %in% names(df)) suppressWarnings(as.numeric(df$Citation_Count)) else rep(0,nrow(df)); cites[!is.finite(cites)]<-0
  long <- bind_rows(lapply(seq_len(nrow(df)), function(i) { z<-reference_journal_year(df$References[i]); if(!nrow(z)) return(NULL); tibble(article=i,Element=unique(z$journal),Citations=cites[i]) }))
  if(!nrow(long)) return(tibble(Domain=character(), Element=character(), Count=integer(), Citations=numeric(), AAC=numeric()))
  out<-long |> distinct(article,Element,.keep_all=TRUE) |> group_by(Element) |> summarise(Count=n(),Citations=sum(Citations),.groups="drop") |> arrange(desc(Count),desc(Citations),Element) |> slice_head(n=5)
  mutate(out,Domain="Journal (references)",AAC=summary_aac(Count),.before=1)
}

draw_summary_report <- function(summary_df, n_articles = NA_integer_, dataset_h = NA_integer_) {
  if (!nrow(summary_df)) { plot.new(); text(.5,.5,"Collect metadata to create the summary report.",cex=1.2); return() }
  domains <- unique(summary_df$Domain); old <- par(no.readonly=TRUE); on.exit(par(old),add=TRUE)
  par(mfrow=c(4,2),mar=c(.2,.2,.2,.2),oma=c(0,0,4,0),xpd=NA)
  for (domain in domains) {
    d <- summary_df[summary_df$Domain == domain,,drop=FALSE]; d <- d[order(d$Count,d$Citations,decreasing=TRUE),,drop=FALSE]
    plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1)); text(.5,.93,domain,col="red",font=3,cex=2.35)
    text(.77,.93,"h",font=2,cex=1.75); text(.91,.93,"n",font=2,cex=1.75)
    yy <- seq(.73,.22,length.out=max(1,nrow(d))); for(i in seq_len(nrow(d))){ text(.04,yy[i],substr(as.character(d$Element[i]),1,31),adj=0,font=2,cex=1.55); text(.77,yy[i],d$h[i],font=2,cex=1.5); text(.91,yy[i],d$Count[i],font=2,cex=1.5) }
    text(.72,.07,paste0("AAC = ",ifelse(is.finite(d$AAC[1]),sprintf("%.2f",d$AAC[1]),"NA")),col="red",font=2,cex=1.55,adj=0)
  }
  if(length(domains)<8) for(i in seq_len(8-length(domains))) plot.new()
  title <- paste0("Summary Report (n=",ifelse(is.finite(n_articles),n_articles,"NA"),", h=",ifelse(is.finite(dataset_h),dataset_h,"NA"),")")
  mtext(title,outer=TRUE,font=2,cex=2.85,line=1)
}

ui <- fluidPage(
  titlePanel("MDPI Journal Metadata & FLCA Explorer"),
  tags$head(tags$style(HTML("
    .flca-entity-heading { font-weight: 700; color: #1f4e79; margin-top: 14px; }
    .tab-content { overflow-x: auto; }
    #main_tabs > li.active > a, #main_tabs > li.active > a:hover, #main_tabs > li.active > a:focus,
    #entity_tabs > li.active > a, #entity_tabs > li.active > a:hover, #entity_tabs > li.active > a:focus {
      color: #c00000 !important;
      font-weight: 700;
    }
    .author-mode-heading { color: #c00000; font-weight: 700; }
  "))),
  sidebarLayout(
    sidebarPanel(width = 3,
      textInput("issn", "MDPI Journal ISSN", value = "3042-5042", placeholder = "e.g., 3042-5042"),
      dateInput("date_from", "Publication date from (optional)", value = Sys.Date() - 365.25 * 5),
      dateInput("date_to", "Publication date to (optional)", value = Sys.Date()),
      checkboxInput("mdpi_xml", "Retrieve MDPI publisher metadata when available", TRUE),
      checkboxInput("openalex", "Retrieve OpenAlex citation counts", TRUE),
      tags$hr(),
      tags$h4("A. Collect metadata"),
      actionButton("collect", "Collect metadata", class = "btn-primary btn-block"),
      tags$hr(),
      tags$h4("B. Extract author keywords"),
      textAreaInput("doi_list", "DOI list (one DOI per line)", value = default_dois, rows = 7, placeholder = "10.3390/metrics1010001"),
      fluidRow(column(7, actionButton("keywords", "Extract keywords", class = "btn-success btn-block")), column(5, actionButton("clear_dois", "Clear DOI", class = "btn-default btn-block"))),
      tags$hr(),
      tags$h4("C. FLCA Process"),
      actionButton("flca", "Build co-occurrence", class = "btn-warning btn-block"),
      tags$hr(),
      tags$h4("D. References"),
      actionButton("references", "Reference journals & year", class = "btn-info btn-block"),
      tags$hr(),
      numericInput("top_n", "Top nodes / journals", value = 20, min = 5, max = 50),
      sliderInput("recent_years", "Recent reference years", min = 5, max = 30, value = 10),
      tags$hr(), tags$h4("E. Article references"),
      textAreaInput("mdpi_references", "MDPI references (one reference per line)", value = default_mdpi_references, rows = 9),
      fluidRow(column(7, actionButton("parse_references", "Extract authors & journals", class = "btn-info btn-block")), column(5, actionButton("clear_references", "Clear references", class = "btn-default btn-block"))),
      tags$hr(), tags$h4("F. Graphical abstract"),
      textAreaInput("article_content", "Article content (authors are ignored)", value = default_article_content, rows = 9),
      fluidRow(column(7, actionButton("extract_article_content", "Extract keywords", class = "btn-success btn-block")), column(5, actionButton("clear_article_content", "Clear content", class = "btn-default btn-block"))),
      sliderInput("graphical_phrase_min", "Minimum keyphrase occurrences", min = 1, max = 10, value = 2, step = 1),
      tags$hr(), tags$h4(class = "author-mode-heading", "5. Author publications and coauthorship"),
      textInput("openalex_author_name", "Author name", value = "Tsair-Wei Chien"),
      passwordInput("openalex_api_key", "OpenAlex API key for author search"),
      actionButton("search_openalex", "Search OpenAlex", class = "btn-primary btn-block"),
      helpText("Select the correct author profile in the OpenAlex authors tab. Mode 5 analyses first and last authors of its works.")
    ),
    mainPanel(width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("Metadata",
          h3("Collected article metadata"),
          textOutput("metadata_status"), br(),
          h4("DOI keyword extraction"),
          textOutput("keyword_status"), br(),
          tableOutput("keyword_table"), br(),
          downloadButton("download_metadata", "Download metadata CSV"), br(), br(),
          tableOutput("metadata_table")
        ),
        tabPanel("FLCA Process",
          h3("FLCA Process: co-occurrence by metadata entity"),
          textOutput("flca_status"), br(),
          wellPanel(
            h4("Metadata domains"),
            tabsetPanel(id = "entity_tabs",
              tabPanel("Authors", value = "authors"),
              tabPanel("Keywords", value = "keywords"),
              tabPanel("Countries", value = "countries"),
              tabPanel("Institutes", value = "institutes"),
              tabPanel("Departments", value = "departments")
            )
          ),
          wellPanel(
            h4("Visualizations"),
            conditionalPanel("input.entity_tabs == 'authors'", flca_visual_tabs("authors")),
            conditionalPanel("input.entity_tabs == 'keywords'", flca_visual_tabs("keywords")),
            conditionalPanel("input.entity_tabs == 'countries'", flca_visual_tabs("countries")),
            conditionalPanel("input.entity_tabs == 'institutes'", flca_visual_tabs("institutes")),
            conditionalPanel("input.entity_tabs == 'departments'", flca_visual_tabs("departments"))
          )
        ),
        tabPanel("Reference journal trends",
          h3("Referenced journals by year"),
          p("Counts are extracted from Crossref reference fields in the collected article records."),
          textOutput("reference_status"), br(),
          plotOutput("reference_plot", height = "900px"), tableOutput("reference_counts")
        ),
        tabPanel("Article references",
          h3("Article-reference author and journal FLCA"),
          textOutput("article_reference_status"), br(),
          tabsetPanel(
            tabPanel("Extracted references", tableOutput("article_reference_table")),
            flca_entity_tab("reference_flca", "Reference author & journal FLCA")
          )
        ), 
        tabPanel("Graphical abstract", h3("Keyword FLCA"), textOutput("graphical_abstract_status"), br(), tabsetPanel(tabPanel("Extracted terms", tableOutput("graphical_abstract_terms")), flca_entity_tab("graphical_abstract_flca", "Graphical abstract FLCA"))),
        tabPanel("Summary",
          h3("Top-5 metadata summary"),
          p("Count = distinct articles containing the element; Citations = article citation total; AAC is calculated from the domain Top-5 counts."),
          downloadButton("download_summary", "Download summary CSV"), downloadButton("download_summary_png", "Download summary PNG"), br(), br(),
          plotOutput("summary_report", height = "1600px"), br(),
          tableOutput("summary_table")
        ),
        tabPanel("OpenAlex authors",
          h3("Author publications and coauthorship"),
          textOutput("openalex_author_status"),
          selectInput("openalex_author_id", "OpenAlex author profile", choices = character()),
          uiOutput("openalex_profile_link"),
          h4("Matching author profiles"), tableOutput("openalex_candidates"),
          h4("First/last-author FLCA network"),
          p("Each work links its first and last authors. Node size shows first/last-author work count; edge width shows the number of shared works."),
          plotOutput("openalex_network", height = "700px"),
          h4("First/last-author Kano plot"),
          p("FLCA selects up to 20 nodes. The Kano plot uses first/last-author edge occurrences on x, first/last-author work count on y, and SHC (1 − Burt constraint) for bubble size."),
          plotOutput("openalex_kano", height = "760px"),
          h4("First/last-author FLCA dendrograph"),
          p("The same metadata FLCA dendrograph style is used here: red leader stems, grey follower stems scaled by WCD with exact values, and dashed grey stems for components without a path to the top leader. Bubbles align at the baseline; labels and counts appear below."),
          textOutput("openalex_dendrogram_status"),
          plotOutput("openalex_dendrogram", height = "850px"),
          h4("FLCA node measures"), tableOutput("openalex_node_measures"),
          h4("Coauthors"), tableOutput("openalex_coauthors"),
          h4("Recent publications"), tableOutput("openalex_publications"),
          downloadButton("download_openalex_publications", "Download retrieved publications CSV")
        ),
        tabPanel("ReadMe",
          h3("How to use this app"),
          h4("Open the app with a pre-filled ISSN"),
          p("Add an ISSN query parameter to the app URL. For example: ", a(href = "?ISSN=3042-5042", "?ISSN=3042-5042"), ". The value is copied into the MDPI Journal ISSN box when the app opens. Replace 3042-5042 with any journal ISSN."),
          tags$ol(
            tags$li("Enter the MDPI journal ISSN and a publication-date range."),
            tags$li("Click A. Collect metadata to load the saved article dataset."),
            tags$li("Click B. Extract author keywords to merge available keyword sources."),
            tags$li("Click C. Build FLCA process, then use the Author, Keyword, Country, Institute, and Department tabs."),
            tags$li("Click D. Reference journals & year to inspect recent reference-journal trends.")
          ),
          p("This reconstructed version preserves the original workflow layout. It uses the existing CSV files in this project; live Crossref/MDPI retrieval can be added back after the original extraction logic is reconstructed.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(tibble())
  keywords_done <- reactiveVal(FALSE)
  flca_ready <- reactiveVal(FALSE)
  keyword_results <- reactiveVal(tibble())
  article_references <- reactiveVal(parse_mdpi_references(default_mdpi_references))
  graphical_abstract <- reactiveVal(extract_article_content_terms(default_article_content))
  observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search)
    supplied_issn <- query$ISSN %||% query$issn
    if (!is.null(supplied_issn) && nzchar(trimws(supplied_issn))) updateTextInput(session, "issn", value = trimws(supplied_issn))
  }, ignoreInit = FALSE, once = TRUE)

  observeEvent(input$collect, {
    x <- read_metadata()
    if (nrow(x) && "Publication_Date" %in% names(x)) {
      dates <- suppressWarnings(as.Date(x$Publication_Date))
      keep <- (is.na(dates) | dates >= input$date_from) & (is.na(dates) | dates <= input$date_to)
      x <- x[keep, , drop = FALSE]
    }
    data(x)
    if ("DOI" %in% names(x)) updateTextAreaInput(session, "doi_list", value = paste(unique(na.omit(x$DOI)), collapse = "\n"))
    keyword_results(tibble())
    keywords_done(FALSE); flca_ready(FALSE)
    updateTabsetPanel(session, "main_tabs", selected = "Metadata")
  }, ignoreInit = FALSE)

  observeEvent(input$clear_dois, updateTextAreaInput(session, "doi_list", value = ""))
  observeEvent(input$clear_references, { updateTextAreaInput(session, "mdpi_references", value = ""); article_references(tibble()) })
  observeEvent(input$parse_references, { article_references(parse_mdpi_references(input$mdpi_references)); updateTabsetPanel(session, "main_tabs", selected = "Article references") })
  observeEvent(input$clear_article_content, { updateTextAreaInput(session, "article_content", value = ""); graphical_abstract(list(terms=tibble(), sets=list(), abbreviations=character())) })
observeEvent(input$extract_article_content, {
    withProgress(message = "Analysing graphical abstract", value = 0, {
      incProgress(0.2, detail = "Removing author and reference sections")
      cleaned <- article_text_without_authors_or_references(input$article_content)
      incProgress(0.5, detail = "Extracting article keyphrases")
      graphical_abstract(extract_article_content_terms(cleaned, 20L, input$graphical_phrase_min))
      incProgress(0.3, detail = "Building the top-20 FLCA network")
    })
    updateTabsetPanel(session, "main_tabs", selected = "Graphical abstract")
  })

  observeEvent(input$keywords, {
    all_records <- data(); req(nrow(all_records))
    if (!"DOI" %in% names(all_records)) { keyword_results(tibble(Status = "Collected metadata has no DOI column.")); return() }
    requested <- normalise_doi(unlist(strsplit(input$doi_list, "\\n", perl = TRUE)))
    requested <- unique(requested[nzchar(requested)])
    use <- if (length(requested)) normalise_doi(all_records$DOI) %in% requested else rep(TRUE, nrow(all_records))
    x <- all_records[use, , drop = FALSE]
    if (!nrow(x)) { keyword_results(tibble(Status = "None of the entered DOI values match collected metadata.")); return() }
    if (!"Keywords" %in% names(x)) x$Keywords <- NA_character_
    source_name <- ifelse(empty_text(x$Keywords), NA_character_, "Collected metadata")
    mdpi_col <- intersect(c("Keywords_MDPI", "Keywords MDPI"), names(x))[1]
    if (length(mdpi_col) && !is.na(mdpi_col)) {
      take_mdpi <- empty_text(x$Keywords) & !empty_text(x[[mdpi_col]])
      x$Keywords[take_mdpi] <- x[[mdpi_col]][take_mdpi]; source_name[take_mdpi] <- "MDPI metadata"
    }
    cache <- read_keyword_cache()
    doi_col <- intersect(c("DOI", "doi"), names(cache))[1]
    key_col <- intersect(c("Author_Keywords", "Keywords", "Keyword"), names(cache))[1]
    if (nrow(cache) && length(doi_col) && length(key_col)) {
      lookup <- setNames(as.character(cache[[key_col]]), normalise_doi(cache[[doi_col]]))
      cached <- unname(lookup[normalise_doi(x$DOI)])
      take_cache <- empty_text(x$Keywords) & !empty_text(cached)
      x$Keywords[take_cache] <- cached[take_cache]; source_name[take_cache] <- "Verified DOI keyword CSV"
    }
    match_rows <- match(normalise_doi(x$DOI), normalise_doi(all_records$DOI))
    all_records$Keywords[match_rows] <- x$Keywords
    data(all_records); keywords_done(TRUE); flca_ready(FALSE)
    keyword_results(tibble(DOI = x$DOI, Keywords = x$Keywords, Keyword_Source = source_name, Status = ifelse(empty_text(x$Keywords), "No verified keywords found", "Keywords extracted")))
  })

  observeEvent(input$flca, {
    req(nrow(data()))
    flca_ready(TRUE)
    updateTabsetPanel(session, "main_tabs", selected = "FLCA Process")
  })
  observeEvent(input$references, updateTabsetPanel(session, "main_tabs", selected = "Reference journal trends"))

  output$article_reference_status <- renderText({ paste(nrow(article_references()), "references parsed") })
  output$graphical_abstract_status <- renderText({ z <- graphical_abstract(); paste(nrow(z$terms), "keywords extracted from article text only; author and reference sections are excluded; abbreviations are removed immediately before FLCA.") })
  output$graphical_abstract_terms <- renderTable({ graphical_abstract()$terms }, striped=TRUE, bordered=TRUE)
  output$article_reference_table <- renderTable({ article_references() }, striped=TRUE, bordered=TRUE)

  output$metadata_status <- renderText({
    x <- data()
    if (!nrow(x)) "No metadata records found. Confirm that Metrics_MDPI_metadata.csv is in the app folder."
    else paste(nrow(x), "records loaded for ISSN", input$issn, "within the selected date range.")
  })
  output$metadata_table <- renderTable({ head(data(), 30) }, striped = TRUE, bordered = TRUE)
  output$keyword_status <- renderText({ if (!nrow(keyword_results())) "Enter or keep collected DOIs, then click B. Extract author keywords." else paste(nrow(keyword_results()), "DOI keyword records processed.") })
  output$keyword_table <- renderTable({ keyword_results() }, striped = TRUE, bordered = TRUE)
  output$download_metadata <- downloadHandler(
    filename = function() paste0("metadata_", gsub("[^0-9Xx]", "", input$issn), ".csv"),
    content = function(file) write_csv(data(), file, na = "")
  )

  entity_network <- function(columns) {
    req(flca_ready(), nrow(data()))
    column <- intersect(columns, names(data()))[1]
    if (!length(column) || is.na(column)) return(list(nodes = tibble(), edges = tibble()))
    cooccurrence(article_term_sets(data(), column), min(20, input$top_n))
  }
  draw_network <- function(net, label) {
    if (!nrow(net$nodes)) { plot.new(); text(.5, .5, paste("No", label, "terms are available.")); return() }
    n <- nrow(net$nodes); theta <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
    coords <- data.frame(term = net$nodes$term, x = cos(theta), y = sin(theta), frequency = net$nodes$frequency)
    plot(coords$x, coords$y, type = "n", xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35), axes = FALSE, xlab = "", ylab = "", main = paste(label, "co-occurrence — Top", n), cex.main = 1.3, font.main = 2, cex.main = 1.35)
    if (nrow(net$edges)) for (i in seq_len(nrow(net$edges))) {
      a <- coords[match(net$edges$from[i], coords$term), ]; b <- coords[match(net$edges$to[i], coords$term), ]
      segments(a$x, a$y, b$x, b$y, col = grDevices::adjustcolor("grey35", alpha.f = .55), lwd = 1 + log1p(net$edges$weight[i]))
    }
    palette <- grDevices::hcl.colors(max(n, 3), "Set 2")
    points(coords$x, coords$y, pch = 21, bg = palette[seq_len(n)], col = "grey15", cex = 1.3 + 2.4 * (coords$frequency / max(coords$frequency)))
    text(coords$x, coords$y, labels = coords$term, cex = 1.05, font = 2)
  }
  author_net <- reactive(entity_network(c("Authors")))
  keyword_net <- reactive(entity_network(c("Keywords", "Keywords_MDPI")))
  country_net <- reactive(entity_network(c("Countries_From_Affiliations", "Countries", "Countries_MDPI", "Countries_Crossref")))
  institute_net <- reactive(entity_network(c("Institutes_From_Affiliations", "Institutes", "Institutes_Crossref")))
  department_net <- reactive(entity_network(c("Departments_From_Affiliations", "Departments", "Departments_Crossref")))
  reference_author_journal_net <- reactive({ x <- article_references(); if (!nrow(x)) return(list(nodes=tibble(), edges=tibble())); x$Author_Journal <- vapply(seq_len(nrow(x)), function(i) paste(c(unique_terms(x$Authors[i]), if (!empty_text(x$Journal[i])) paste0("Journal: ", x$Journal[i]) else character()), collapse="; "), character(1)); cooccurrence(article_term_sets(x, "Author_Journal"), min(20, input$top_n)) })
  graphical_abstract_net <- reactive({ z <- graphical_abstract(); blocked <- z$abbreviations %||% character(); final_sets <- lapply(z$sets, function(s) setdiff(s, blocked)); cooccurrence(final_sets, 20L) })
  output$flca_status <- renderText({ if (!flca_ready()) "Click C. Build FLCA process after collecting metadata and extracting keywords." else paste("FLCA co-occurrence prepared for", nrow(data()), "articles.") })
  real_flca <- function(net) {
    if (!nrow(net$nodes) || !nrow(net$edges)) return(NULL)
    tryCatch({
      source("flca_ms_sil_module_76_v40_top20_max4_preserve_values.R", local = TRUE)
      nodes <- data.frame(name = net$nodes$term, value = net$nodes$frequency, stringsAsFactors = FALSE)
      edges <- data.frame(Leader = net$edges$from, Follower = net$edges$to, WCD = net$edges$weight, stringsAsFactors = FALSE)
      run_flca_ms_sil_runner(nodes, edges, cfg = list(target_n = min(20, nrow(nodes)), top_clusters = 5, base_per_cluster = 4, max_per_cluster = 4), verbose = FALSE)
    }, error = function(e) structure(list(message = e$message), class = "flca_error"))
  }
  draw_message <- function(message) { plot.new(); text(.5, .5, message, cex = 1.1) }
  flca_display_edges <- function(result) {
    nodes <- as.character(result$modes$name)
    edges <- as.data.frame(result$data)
    if (!nrow(edges)) return(edges)
    edges$WCD <- suppressWarnings(as.numeric(edges$WCD))
    edges <- edges[is.finite(edges$WCD) & edges$WCD > 0 &
      edges$Leader %in% nodes & edges$Follower %in% nodes &
      edges$Leader != edges$Follower, , drop = FALSE]
    edges <- edges[order(-edges$WCD, edges$Leader, edges$Follower), , drop = FALSE]
    edges[!duplicated(edges$Follower), , drop = FALSE]
  }
  draw_wcd_label <- function(x, y, wcd, cex = .85) {
    label <- sprintf("%.2f", wcd)
    half_w <- strwidth(label, cex = cex, font = 2) / 2 + strwidth(" ", cex = cex)
    half_h <- strheight(label, cex = cex, font = 2) / 2 + strheight(" ", cex = cex) / 3
    rect(x - half_w, y - half_h, x + half_w, y + half_h, col = "white", border = NA)
    text(x, y, label, cex = cex, font = 2, col = "#333333")
  }
  draw_reduced <- function(result, title) {
    if (is.null(result) || inherits(result, "flca_error")) return(draw_message(if (is.null(result)) "Not enough co-occurrences for FLCA." else result$message))
    nd <- result$modes; ed <- flca_display_edges(result); n <- nrow(nd); th <- seq(0, 2*pi, length.out=n+1)[-(n+1)]; xy <- data.frame(name=nd$name,x=cos(th),y=sin(th),carac=as.factor(nd$carac),value=nd$value)
    plot(xy$x,xy$y,type="n",xlim=c(-1.35,1.35),ylim=c(-1.35,1.35),axes=FALSE,xlab="",ylab="",main=title,font.main=2)
    pal <- grDevices::hcl.colors(max(3,length(unique(xy$carac))),"Dark 3"); cols <- setNames(pal,sort(unique(xy$carac)))
    if(nrow(ed)) for(i in seq_len(nrow(ed))){a<-xy[match(ed$Leader[i],xy$name),];b<-xy[match(ed$Follower[i],xy$name),];arrows(a$x,a$y,.9*b$x+.1*a$x,.9*b$y+.1*a$y,length=.08,angle=20,col="grey65",lwd=1+5*ed$WCD[i]/max(ed$WCD))}
    points(xy$x,xy$y,pch=21,bg=cols[as.character(xy$carac)],cex=1.5+2*xy$value/max(xy$value),col="grey10"); text(xy$x,xy$y,xy$name,cex=1.05,font=2)
    if(nrow(ed)) for(i in seq_len(nrow(ed))){a<-xy[match(ed$Leader[i],xy$name),];b<-xy[match(ed$Follower[i],xy$name),];draw_wcd_label(.55*a$x+.45*b$x,.55*a$y+.45*b$y,ed$WCD[i])}
  }
  draw_flca_dendrogram <- function(result, net, title, label_map = NULL,
                                  highlight_id = NULL) {
    if (is.null(result) || inherits(result, "flca_error") || nrow(result$modes) < 1L)
      return(draw_message("No FLCA nodes are available for a dendrograph."))
    nodes <- as.data.frame(result$modes)
    labels <- as.character(nodes$name)
    display_labels <- labels
    if (!is.null(label_map)) {
      mapped <- unname(label_map[labels])
      use <- !is.na(mapped) & nzchar(mapped)
      display_labels[use] <- mapped[use]
    }
    edges <- flca_display_edges(result)
    roots <- setdiff(labels, edges$Follower)
    if (!length(roots)) return(draw_message("The FLCA links contain a cycle, so a dendrograph cannot be drawn."))
    children <- split(as.character(edges$Follower), as.character(edges$Leader))
    positions <- setNames(rep(NA_real_, length(labels)), labels)
    visited <- character()
    next_x <- 0
    place <- function(node) {
      if (node %in% visited) return(invisible(NULL))
      visited <<- c(visited, node)
      offspring <- children[[node]]
      left_n <- floor(length(offspring) / 2)
      if (left_n) for (child in offspring[seq_len(left_n)]) place(child)
      next_x <<- next_x + 1
      positions[node] <<- next_x
      if (left_n < length(offspring))
        for (child in offspring[seq.int(left_n + 1L, length(offspring))]) place(child)
      invisible(NULL)
    }
    for (root in roots) {
      if (length(visited)) next_x <- next_x + .55
      place(root)
    }
    if (length(visited) != length(labels))
      return(draw_message("The FLCA links contain a cycle, so a dendrograph cannot be drawn."))
    max_wcd <- if (nrow(edges)) max(edges$WCD) else 1
    leader_level <- setNames(rep(0, length(labels)), labels)
    set_level <- function(node) {
      offspring <- children[[node]]
      if (!length(offspring)) return(0)
      leader_level[node] <<- 1 + max(vapply(offspring, set_level, numeric(1)))
      leader_level[node]
    }
    for (root in roots) set_level(root)
    top_height <- max(c(1, leader_level))
    values <- suppressWarnings(as.numeric(nodes$value))
    values[!is.finite(values) | values <= 0] <- 1
    bubble_cex <- 3 * sqrt(values / max(values))
    occurrence_counts <- suppressWarnings(as.numeric(nodes$value))
    count_labels <- ifelse(is.finite(occurrence_counts),
                           format(occurrence_counts, trim = TRUE, scientific = FALSE), "NA")
    counts <- net$nodes$frequency[match(labels, net$nodes$term)]
    counts[!is.finite(counts)] <- -Inf
    top_node <- if (!is.null(highlight_id) && highlight_id %in% labels)
      highlight_id else labels[order(-counts, labels)][1]
    parent <- setNames(as.character(edges$Leader), as.character(edges$Follower))
    root_of <- function(node) {
      while (node %in% names(parent)) node <- parent[[node]]
      node
    }
    top_root <- root_of(top_node)
    other_roots <- setdiff(roots, top_root)
    show_reference <- length(other_roots) > 0L || top_node != top_root
    top_reference <- if (show_reference) top_height + .6 else top_height
    cluster_ids <- sort(unique(as.character(nodes$carac)))
    colours <- setNames(grDevices::hcl.colors(max(3L, length(cluster_ids)), "Dark 3")[seq_along(cluster_ids)], cluster_ids)
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(3, 4, 4, 2), xpd = NA)
    name_cex <- if (length(labels) >= 15L) .68 else .78
    longest_name_in <- max(strwidth(display_labels, units = "inches", cex = name_cex, font = 2))
    plot_height_in <- par("pin")[2]
    bottom_in <- longest_name_in + 1.2
    upper_y <- top_reference + .55
    units_per_in <- upper_y / (plot_height_in - bottom_in)
    lower_y <- -bottom_in * units_per_in
    name_y <- -.16 * units_per_in
    count_y <- -(longest_name_in + .85) * units_per_in
    plot(NA, xlim = c(.15, max(positions) + .85),
         ylim = c(lower_y, upper_y), axes = FALSE, xlab = "", ylab = "",
         main = sprintf("%s: strongest-link FLCA dendrograph of %d nodes", title, length(labels)), font.main = 2)
    segments(.35, 0, .35, top_reference, col = "grey30", lwd = 1.2)
    if (show_reference) {
      segments(min(positions), top_reference, max(positions), top_reference,
               col = "grey70", lwd = 1.5)
    }
    if (nrow(edges)) {
      for (leader in unique(as.character(edges$Leader))) {
        followers <- as.character(edges$Follower[edges$Leader == leader])
        level <- leader_level[leader]
        segments(positions[leader], 0, positions[leader], level,
                 col = "#C00000", lwd = 2.5)
        segments(min(positions[c(leader, followers)]), level,
                 max(positions[c(leader, followers)]), level,
                 col = "grey55", lwd = 1.5)
      }
      for (i in seq_len(nrow(edges))) {
        leader <- as.character(edges$Leader[i])
        follower <- as.character(edges$Follower[i])
        width <- 1 + 5 * edges$WCD[i] / max_wcd
        segments(positions[follower], leader_level[follower], positions[follower], leader_level[leader],
                 col = "grey55", lwd = width)
        draw_wcd_label(positions[follower],
                       (leader_level[follower] + leader_level[leader]) / 2,
                       edges$WCD[i], cex = .72)
      }
    }
    if (show_reference)
      segments(positions[top_node], 0, positions[top_node], top_reference,
               col = "#C00000", lwd = 2.5)
    for (root in other_roots)
      segments(positions[root], leader_level[root], positions[root], top_reference,
               col = "grey75", lty = 2, lwd = 1.5)
    points(unname(positions[labels]), rep(0, length(labels)), pch = 21,
           cex = bubble_cex, bg = unname(colours[as.character(nodes$carac)]), col = "grey20")
    text(unname(positions[labels]), rep(name_y, length(labels)), display_labels,
         srt = 90, adj = c(1, .5), cex = name_cex,
         font = 2, col = ifelse(labels == top_node, "red", "black"))
    text(unname(positions[labels]), rep(count_y, length(labels)), count_labels,
         srt = 90, adj = c(1, .5), cex = .78, font = 2,
         col = ifelse(labels == top_node, "red", "black"))
    mtext("Red stems: leader role; grey follower stems: WCD; dashed upper stems: no path to top node", side = 1, line = .5, cex = .82)
  }
  draw_sankey <- function(result, title) {
    if (is.null(result) || inherits(result, "flca_error")) return(draw_message("Not enough FLCA links for Sankey plot."))
    nd <- as.data.frame(result$modes); ed <- as.data.frame(result$data); nd$name <- as.character(nd$name); nd$value <- as.numeric(nd$value); nd$carac <- as.integer(nd$carac)
    nd <- nd[order(nd$carac, -nd$value, nd$name), , drop = FALSE]; nd$y <- seq_len(nrow(nd)); ymap <- setNames(nd$y, nd$name); nd$lab <- sprintf("%s (%.0f; C%s)", nd$name, nd$value, nd$carac)
    ed <- ed[ed$Leader %in% nd$name & ed$Follower %in% nd$name & is.finite(ed$WCD) & ed$WCD > 0, , drop = FALSE]; ed$y1 <- ymap[ed$Leader]; ed$y2 <- ymap[ed$Follower]
    pal <- grDevices::hcl.colors(max(3, length(unique(nd$carac))), "Dark 3"); node_col <- setNames(pal[match(nd$carac, sort(unique(nd$carac)))], nd$name)
    old <- par(no.readonly=TRUE); on.exit(par(old),add=TRUE); par(mar=c(5,16,4,16),xpd=NA)
    plot(NA,xlim=c(0,1),ylim=c(nrow(nd)+1.2,0),axes=FALSE,xlab="",ylab="",main=title,cex.main=1.45,font.main=2)
    L <- c(.12,.18); R <- c(.82,.88); h <- .32
    for(k in seq_len(nrow(nd))){rect(L[1],nd$y[k]-h,L[2],nd$y[k]+h,col=node_col[nd$name[k]],border="grey25");rect(R[1],nd$y[k]-h,R[2],nd$y[k]+h,col=node_col[nd$name[k]],border="grey25");text(L[1]-.015,nd$y[k],nd$lab[k],adj=c(1,.5),cex=.95,font=2);text(R[2]+.015,nd$y[k],nd$lab[k],adj=c(0,.5),cex=.95,font=2)}
    text(mean(L),.2,"Leader",cex=1.2,font=2);text(mean(R),.2,"Follower",cex=1.2,font=2)
    if(nrow(ed)){mw<-max(ed$WCD);for(k in seq_len(nrow(ed))){t<-seq(0,1,length.out=90);x<-(L[2]+.01)+(R[1]-L[2]-.02)*t;e<-3*t^2-2*t^3;ym<-(1-e)*ed$y1[k]+e*ed$y2[k];hw<-.025+.14*ed$WCD[k]/mw;polygon(c(x,rev(x)),c(ym-hw,rev(ym+hw)),col=grDevices::adjustcolor(node_col[ed$Leader[k]],alpha.f=.48),border=NA);lines(x,ym,col=grDevices::adjustcolor("grey20",alpha.f=.35),lwd=.8)}}
    mtext("Identical FLCA Top-20 nodes at both sides; ribbon width = WCD; colours = leader cluster.",side=1,line=2,cex=.95,font=2)
  }
  draw_chord <- function(result, title) {
    if (is.null(result) || inherits(result, "flca_error")) return(draw_message("Not enough FLCA links for chord plot."))
    if (!requireNamespace("circlize", quietly = TRUE)) return(draw_message("Install circlize for the chord diagram."))
    nd <- result$modes; ed <- result$data; groups <- sort(unique(as.character(nd$carac)))
    cols <- setNames(grDevices::hcl.colors(max(3, length(groups)), "Dark 3")[seq_along(groups)], groups)
    grid_col <- setNames(unname(cols[as.character(nd$carac)]), nd$name)
    circlize::circos.clear(); on.exit(circlize::circos.clear(), add = TRUE)
    circlize::chordDiagram(ed[, c("Leader", "Follower", "WCD")], grid.col = grid_col, transparency = .25, annotationTrack = "grid", preAllocateTracks = list(track.height = .12))
    circlize::circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) { sector <- circlize::get.cell.meta.data("sector.index"); circlize::circos.text(circlize::get.cell.meta.data("xcenter"), circlize::get.cell.meta.data("ylim")[1], sector, facing = "clockwise", niceFacing = TRUE, adj = c(0, .5), cex = 1.05, font = 2) }, bg.border = NA)
    title(title, cex.main = 1.5, font.main = 2, cex.main = 1.35)
  }
  make_ss_results <- function(result) {
    m <- result$modes; cl <- sort(unique(m$carac)); mod <- result$modularity_by_cluster
    qw <- qu <- setNames(rep(0, length(cl)), as.character(cl))
    if (is.data.frame(mod) && nrow(mod)) { qw[as.character(mod$carac)] <- mod$Qw_cluster; qu[as.character(mod$carac)] <- mod$Qu_cluster }
    tibble(Cluster = c("OVERALL", paste0("C", cl)), SS = c(mean(m$ssi, na.rm = TRUE), vapply(cl, function(z) mean(m$ssi[m$carac == z], na.rm = TRUE), numeric(1))), Qw = c(unique(m$Qw_total)[1], unname(qw[as.character(cl)])), Qu = c(unique(m$Qu_total)[1], unname(qu[as.character(cl)])))
  }
  entity_slope <- function(columns, label) {
    x <- data(); req(nrow(x), "Publication_Date" %in% names(x))
    col <- intersect(columns, names(x))[1]; if (!length(col) || is.na(col)) return(NULL)
    years <- extract_year(x$Publication_Date); keep <- is.finite(years); x <- x[keep, , drop = FALSE]; years <- years[keep]
    if (!length(years)) return(NULL); recent <- max(years) - 9; sets <- lapply(x[[col]], unique_terms)
    rows <- bind_rows(lapply(seq_along(sets), function(i) if(length(sets[[i]])) tibble(term=sets[[i]], year=years[i]) else NULL)); if(!nrow(rows)) return(NULL)
    rows <- rows |> filter(year >= recent) |> count(term, year, name="value"); top <- rows |> group_by(term) |> summarise(total=sum(value),.groups="drop") |> arrange(desc(total),term) |> slice_head(n=min(20, input$top_n)) |> pull(term)
    rows <- filter(rows, term %in% top); yrs <- sort(unique(rows$year)); grid <- expand.grid(term=top,year=yrs,stringsAsFactors=FALSE); rows <- left_join(grid,rows,by=c("term","year")) |> mutate(value=ifelse(is.na(value),0,value))
    wide <- xtabs(value ~ term + year, data=rows); wide <- wide[order(wide[,1],rownames(wide)),,drop=FALSE]; gap <- max(.5,.05*diff(range(wide))); shifts <- numeric(nrow(wide)); if(nrow(wide)>1) for(k in 2:nrow(wide)){dm<-min(diff(as.matrix(wide[(k-1):k,,drop=FALSE])));shifts[k]<-ifelse(is.finite(dm)&&dm<gap,gap-dm,0)}; shifts<-cumsum(shifts)
    sl <- do.call(rbind,lapply(seq_along(yrs),function(j)data.frame(term=rownames(wide),year=yrs[j],value=as.numeric(wide[,j]),ypos=as.numeric(wide[,j])+shifts))); first<-sl[sl$year==yrs[1],,drop=FALSE]
    ggplot(sl,aes(year,ypos))+geom_line(aes(group=term),colour="red",linewidth=1)+geom_point(shape=21,fill="white",size=5)+geom_text(aes(label=ifelse(value>0,value,"")),size=4,fontface="bold")+scale_x_continuous(breaks=yrs)+scale_y_continuous("",breaks=first$ypos,labels=first$term)+labs(title=paste(label,"Top-20 FLCA terms by publication year"),x="Publication year")+theme_classic(base_size=15)+theme(axis.title.y=element_blank(),plot.title=element_text(face="bold",hjust=.5,size=18),axis.text=element_text(face="bold"),plot.margin=margin(10,15,10,130))
  }

  register_flca_outputs <- function(id, net, label) {
    real <- reactive(real_flca(net()))
    output[[paste0(id,"_nodes")]] <- renderTable({ n <- net()$nodes; if (!nrow(n)) return(data.frame(Status = "No terms available.")); names(n) <- c("Node", "Count"); n }, striped = TRUE, bordered = TRUE)
    output[[paste0(id,"_full")]] <- renderPlot(draw_network(net(), paste(label,"full co-occurrence network")))
    output[[paste0(id,"_reduced")]] <- renderPlot(draw_reduced(real(), paste(label,"real FLCA Top-20 reduced network")))
    output[[paste0(id,"_dendrogram")]] <- renderPlot(draw_flca_dendrogram(real(), net(), label), width = 1100, height = 900, res = 110)
    output[[paste0(id,"_sankey")]] <- renderPlot(draw_sankey(real(), paste(label,"FLCA Sankey plot")))
    output[[paste0(id,"_chord")]] <- renderPlot(draw_chord(real(), paste(label,"FLCA chord plot")))
    output[[paste0(id,"_ss")]] <- renderPlot({ r<-real(); if(is.null(r)||inherits(r,"flca_error")) draw_message("Not enough data for SSPlot.") else { source("renderSSplot.R",local=TRUE); s<-r$modes; s$sil_width<-s$ssi; render_panel(s,make_nodes0(s),results=make_ss_results(r),nodes=s,top_n=min(20,nrow(s)),font_scale=1.15) } })
    output[[paste0(id,"_kano")]] <- renderPlot({ r<-real(); if(is.null(r)||inherits(r,"flca_error")) draw_message("Not enough data for Kano plot.") else { source("kano.R",local=TRUE); print(plot_kano_real(r$modes,edges=r$data,title_txt=paste(label,"Kano plot"))) } })
    output[[paste0(id,"_slope")]] <- renderPlot({ p <- entity_slope(switch(id, authors=c("Authors"), keywords=c("Keywords","Keywords_MDPI"), countries=c("Countries_From_Affiliations","Countries"), institutes=c("Institutes_From_Affiliations","Institutes"), departments=c("Departments_From_Affiliations","Departments")), label); if(is.null(p)) draw_message("No dated terms available for slope plot.") else print(p) })
    output[[paste0(id,"_clusters")]] <- renderTable({ r<-real(); if(is.null(r)||inherits(r,"flca_error")) return(data.frame(Status="Not enough co-occurrences for FLCA.")); r$modes }, striped=TRUE,bordered=TRUE)
    output[[paste0(id,"_edges")]] <- renderTable({ r<-real(); if(is.null(r)||inherits(r,"flca_error")) return(NULL); r$data }, striped=TRUE,bordered=TRUE)
  }
  register_flca_outputs("authors", author_net, "Author")
  register_flca_outputs("keywords", keyword_net, "Keyword")
  register_flca_outputs("countries", country_net, "Country")
  register_flca_outputs("institutes", institute_net, "Institute")
  register_flca_outputs("departments", department_net, "Department")
  register_flca_outputs("reference_flca", reference_author_journal_net, "Reference author and journal")
  register_flca_outputs("graphical_abstract_flca", graphical_abstract_net, "Graphical abstract keyword")

  openalex_candidates <- reactiveVal(data.frame())
  openalex_result <- reactiveVal(NULL)
  openalex_notice <- reactiveVal("Enter an author name and OpenAlex API key, then search.")
  openalex_key <- function() {
    entered <- trimws(input$openalex_api_key %||% "")
    if (nzchar(entered)) entered else Sys.getenv("OPENALEX_API_KEY", "")
  }
  openalex_flca <- function(net) {
    if (!nrow(net$nodes) || !nrow(net$edges)) return(NULL)
    tryCatch({
      source("openalex_flca_ms_sil_module.R", local = TRUE)
      nodes <- data.frame(name = net$nodes$term, value = net$nodes$frequency)
      edges <- data.frame(Leader = net$edges$from, Follower = net$edges$to,
                          WCD = net$edges$weight)
      run_flca_ms_sil_runner(nodes, edges,
        cfg = list(target_n = min(20L, nrow(nodes)), top_clusters = 5L,
                   base_per_cluster = 4L, max_per_cluster = 4L), verbose = FALSE)
    }, error = function(e) structure(list(message = e$message), class = "flca_error"))
  }
  load_openalex_author <- function(author_id) {
    candidates <- openalex_candidates()
    selected <- candidates[candidates$id == author_id, , drop = FALSE]
    if (!nrow(selected)) return(invisible(NULL))
    openalex_notice(paste("Loading works for", selected$name[1], "..."))
    result <- tryCatch({
      fetched <- oa_fetch_works(author_id, openalex_key(), max_works = 10000L)
      list(id = author_id, name = selected$name[1], fetched = fetched,
           data = oa_author_data(fetched$works, author_id, selected$name[1]),
           profile = selected)
    }, error = function(e) {
      openalex_notice(conditionMessage(e))
      NULL
    })
    openalex_result(result)
    if (!is.null(result)) {
      openalex_notice(sprintf("%s (%s): %d of %d OpenAlex works retrieved%s.",
                              result$name, result$id, nrow(result$data$publications),
                              result$fetched$total,
                              if (result$fetched$truncated) " (latest 10,000 shown; some metrics unavailable)" else ""))
    }
    invisible(NULL)
  }
  observeEvent(input$search_openalex, {
    openalex_result(NULL)
    candidates <- tryCatch(oa_search_authors(input$openalex_author_name %||% "", openalex_key()),
                           error = function(e) {
                             openalex_notice(conditionMessage(e))
                             NULL
                           })
    if (is.null(candidates)) return()
    openalex_candidates(candidates)
    updateTabsetPanel(session, "main_tabs", selected = "OpenAlex authors")
    if (!nrow(candidates)) {
      openalex_notice("No OpenAlex author profiles matched this name.")
      updateSelectInput(session, "openalex_author_id", choices = character())
      return()
    }
    choices <- stats::setNames(candidates$id,
      sprintf("%s — %s — h-index %s, %d works (%s)", candidates$name,
              ifelse(nzchar(candidates$institution), candidates$institution, "institution unknown"),
              ifelse(is.na(candidates$h_index), "N/A", candidates$h_index),
              candidates$works, candidates$id))
    updateSelectInput(session, "openalex_author_id", choices = choices, selected = candidates$id[1])
    load_openalex_author(candidates$id[1])
  })
  observeEvent(input$openalex_author_id, {
    selected_id <- input$openalex_author_id %||% ""
    current <- openalex_result()
    if (nzchar(selected_id) && (is.null(current) || !identical(current$id, selected_id)))
      load_openalex_author(selected_id)
  }, ignoreInit = TRUE)
  output$openalex_author_status <- renderText(openalex_notice())
  output$openalex_profile_link <- renderUI({
    result <- openalex_result()
    if (is.null(result)) return(NULL)
    tags$p("Selected OpenAlex ID: ", tags$strong(result$id), " · ",
           tags$a("View profile", href = paste0("https://openalex.org/", result$id),
                  target = "_blank", rel = "noopener noreferrer"))
  })
  output$openalex_candidates <- renderTable({
    candidates <- openalex_candidates()
    if (!nrow(candidates)) return(NULL)
    names(candidates) <- c("OpenAlex ID", "Author", "Works", "Citations", "h-index", "Institution", "ORCID")
    candidates
  }, striped = TRUE, bordered = TRUE)
  output$openalex_publications <- renderTable({
    result <- openalex_result()
    if (is.null(result) || !nrow(result$data$publications)) return(NULL)
    utils::head(result$data$publications[, c("Year", "Title", "Citations", "FocalRole", "Corresponding", "Coauthors", "DOI")], 30)
  }, striped = TRUE, bordered = TRUE)
  output$openalex_coauthors <- renderTable({
    result <- openalex_result()
    if (is.null(result) || !nrow(result$data$authors)) return(NULL)
    coauthors <- result$data$authors[result$data$authors$id != result$id, , drop = FALSE]
    names(coauthors) <- c("OpenAlex ID", "Author", "Shared publications", "Citations on shared works",
                           "First-author works", "Last-author works", "Corresponding-author works")
    utils::head(coauthors, 30)
  }, striped = TRUE, bordered = TRUE)
  openalex_analysis <- reactive({
    result <- openalex_result()
    if (is.null(result)) return(NULL)
    oa_first_last_analysis(result$fetched$works, result$id, result$fetched$total,
                           !result$fetched$truncated, openalex_flca)
  })
  openalex_metric_values <- reactive({
    analysis <- openalex_analysis()
    result <- openalex_result()
    if (is.null(analysis) || is.null(result)) return(NULL)
    values <- analysis$metrics
    values <- c(values["AAC"], values["SHC"], values["CLI"],
                `h-index` = result$profile$h_index[1], values["MAR"],
                values["n"], `n*` = values["n_star"])
    names(values) <- c("AAC", "SHC", "CLI", "h-index", "MAR", "n", "n*")
    vapply(seq_along(values), function(i) {
      value <- values[i]
      shown <- if (!is.finite(value)) "N/A" else if (i >= 6L || i == 4L)
        formatC(value, format = "f", digits = 0) else formatC(value, format = "f", digits = 3)
      if (i == 7L && !analysis$complete) shown <- paste0("≥", shown)
      shown
    }, character(1)) |> stats::setNames(names(values))
  })
  openalex_aac_note <- reactive({
    analysis <- openalex_analysis()
    if (is.null(analysis)) return("")
    top <- analysis$top_counts
    if (!analysis$complete || length(top) < 3L) return("AAC unavailable: fewer than three FLCA node counts or an incomplete work fetch.")
    sprintf("FLCA role-count AAC: v1=%d, v2=%d, v3=%d; AAC=%.3f. AAC = [(v1/v2)/(v2/v3)] / [1+(v1/v2)/(v2/v3)].",
            top[1], top[2], top[3], analysis$metrics["AAC"])
  })
  output$openalex_node_measures <- renderTable({
    analysis <- openalex_analysis()
    if (is.null(analysis)) return(NULL)
    d <- analysis$nodes[, c("name", "cluster", "role_works", "edge_count", "shc")]
    names(d) <- c("Author", "FLCA cluster", "First/last works", "Edge count", "SHC")
    d
  }, digits = 3, striped = TRUE, bordered = TRUE)
  output$openalex_dendrogram_status <- renderText({
    analysis <- openalex_analysis()
    if (is.null(analysis)) return("Select an OpenAlex profile with first/last-author links.")
    sprintf("FLCA retained %d nodes from %d linked candidates; the dendrogram displays %d strongest follower-to-leader links.",
            nrow(analysis$flca_tree$modes), analysis$linked_candidates,
            nrow(analysis$flca_tree$data))
  })
  output$openalex_dendrogram <- renderPlot({
    analysis <- openalex_analysis()
    if (is.null(analysis)) return(draw_message("Search OpenAlex to build the author dendrograph."))
    labels <- stats::setNames(analysis$nodes$name, analysis$nodes$id)
    leaders <- intersect(as.character(analysis$flca_tree$data$Leader), analysis$nodes$id)
    pool <- if (length(leaders)) analysis$nodes[analysis$nodes$id %in% leaders, , drop = FALSE] else analysis$nodes
    highlight <- pool$id[which.max(pool$role_works)]
    net <- list(nodes = data.frame(term = analysis$nodes$id,
                                   frequency = analysis$nodes$role_works))
    draw_flca_dendrogram(analysis$flca_tree, net, "OpenAlex first/last authors",
                         label_map = labels, highlight_id = highlight)
  }, height = 850, res = 110)
  output$openalex_network <- renderPlot({
    analysis <- openalex_analysis()
    result <- openalex_result()
    if (is.null(result)) return(draw_message("Search OpenAlex to build the author network."))
    if (is.null(analysis)) return(draw_message("At least one first/last-author link is needed."))
    d <- analysis$nodes
    n <- nrow(d)
    d$angle <- 2 * pi * (seq_len(n) - 1) / n
    d$x <- cos(d$angle); d$y <- sin(d$angle)
    d$label_x <- 1.14 * d$x; d$label_y <- 1.14 * d$y
    d$hjust <- ifelse(d$x > .15, 0, ifelse(d$x < -.15, 1, .5))
    d$type <- ifelse(d$id == analysis$focal, "Selected author", "Other author")
    edges <- analysis$edges
    edges$x <- d$x[match(edges$from, d$id)]; edges$y <- d$y[match(edges$from, d$id)]
    edges$xend <- d$x[match(edges$to, d$id)]; edges$yend <- d$y[match(edges$to, d$id)]
    print(ggplot(d) +
      geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend, linewidth = weight),
                   inherit.aes = FALSE, colour = "#6b8fb3", alpha = .7) +
      geom_point(aes(x = x, y = y, size = role_works, fill = type), shape = 21, colour = "#333333") +
      geom_text(aes(x = label_x, y = label_y, label = paste0(name, " (", role_works, ")"), hjust = hjust),
                size = 3.1, check_overlap = TRUE) +
      scale_fill_manual(values = c("Selected author" = "#b2182b", "Other author" = "#2a9d8f"), guide = "none") +
      scale_linewidth(range = c(.5, 4), guide = "none") + scale_size(range = c(3, 9), guide = "none") +
      coord_equal(xlim = c(-1.7, 1.7), ylim = c(-1.5, 1.5), clip = "off") +
      labs(title = "OpenAlex first/last-author FLCA network",
           subtitle = paste("FLCA nodes from the works of", result$name, "; links join first and last authors.")) +
      theme_void(base_size = 13) + theme(plot.title = element_text(face = "bold")))
  })
  output$openalex_kano <- renderPlot({
    result <- openalex_result()
    if (is.null(result)) return(draw_message("Search OpenAlex to build the coauthor Kano plot."))
    analysis <- openalex_analysis()
    if (is.null(analysis) || nrow(analysis$nodes) < 2L) return(draw_message("At least two first/last authors are needed for the Kano plot."))
    d <- analysis$nodes
    nodes <- data.frame(name = d$name, value = d$role_works, value2 = d$edge_count,
                        carac = d$cluster, size_plot = ifelse(is.finite(d$shc), pmax(d$shc, .01), .01))
    source("kano.R", local = TRUE)
    plot <- plot_kano_real(nodes,
                           title_txt = paste("Kano:", result$name), visual_ratio = 1)
    plot <- plot + ggplot2::labs(x = "First/last-author edge occurrences among FLCA nodes",
                                 y = "First/last-author work count", size = "SHC = 1 - Burt constraint",
                                 subtitle = sprintf("%d FLCA nodes from %d OpenAlex works%s",
                                                    nrow(d), length(result$fetched$works),
                                                    if (analysis$complete) "" else " (partial fetch)"))
    values <- openalex_metric_values()
    colours <- c("#d62828", "#1565c0", "#21843a", "#7e22ce", "#e65c00", "#374151", "#374151")
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(
      nrow = 3L, ncol = 1L, heights = grid::unit(c(15, 8, 77), "null"))))
    grid::pushViewport(grid::viewport(layout.pos.row = 1L))
    for (i in seq_along(values)) {
      x <- (i - .5) / length(values)
      grid::grid.roundrect(x = x, y = .5, width = .94 / length(values), height = .83,
                           r = grid::unit(5, "pt"),
                           gp = grid::gpar(fill = "white", col = colours[i], lwd = 1.7))
      grid::grid.text(names(values)[i], x = x, y = .68,
                      gp = grid::gpar(col = colours[i], fontsize = 12, fontface = "bold"))
      grid::grid.text(values[i], x = x, y = .33,
                      gp = grid::gpar(col = "#111827", fontsize = 17, fontface = "bold"))
    }
    grid::popViewport()
    grid::pushViewport(grid::viewport(layout.pos.row = 2L))
    grid::grid.text(openalex_aac_note(), x = .015, y = .65, just = "left",
                    gp = grid::gpar(col = "#222222", fontsize = 10))
    grid::grid.text("Bubble size: SHC = 1 − Burt constraint on the displayed network.",
                    x = .015, y = .22, just = "left",
                    gp = grid::gpar(col = "#555555", fontsize = 10))
    grid::popViewport()
    print(plot, newpage = FALSE, vp = grid::viewport(layout.pos.row = 3L))
    grid::popViewport()
  })
  output$download_openalex_publications <- downloadHandler(
    filename = function() paste0("openalex_author_", if (is.null(openalex_result())) "works" else openalex_result()$id, ".csv"),
    content = function(file) {
      result <- openalex_result(); req(result)
      readr::write_csv(result$data$publications, file, na = "")
    }
  )

  summary_data <- reactive({
    x <- data(); if(!nrow(x)) return(tibble())
    bind_rows(
      summary_domain(x,"Country","Countries_From_Affiliations"),
      summary_domain(x,"Institute","Institutes_From_Affiliations"),
      summary_domain(x,"Department","Departments_From_Affiliations"),
      summary_year(x),
      summary_domain(x,"Author","Authors"),
      summary_domain(x,"Keyword","Keywords"),
      summary_domain(x,"Article type","Article_Type_Crossref"),
      summary_reference_journal(x)
    )
  })
  output$summary_table <- renderTable({ summary_data() }, striped=TRUE,bordered=TRUE)
  output$summary_report <- renderPlot({ x<-data(); cites<-if("Citation_Count"%in%names(x))as.numeric(x$Citation_Count) else numeric(); draw_summary_report(summary_data(),nrow(x),summary_h(cites)) }, width=1400, height=2200, res=150)
  output$download_summary_png <- downloadHandler(filename=function() "top5_metadata_summary.png", content=function(file) { grDevices::png(file,width=1400,height=2200,res=150); on.exit(grDevices::dev.off(),add=TRUE); x<-data(); cites<-if("Citation_Count"%in%names(x))as.numeric(x$Citation_Count) else numeric(); draw_summary_report(summary_data(),nrow(x),summary_h(cites)) })
  output$download_summary <- downloadHandler(filename=function() "top5_metadata_summary.csv", content=function(file) write_csv(summary_data(),file,na=""))

  ref_data <- reactive({
    x <- data(); if (!nrow(x) || !"References" %in% names(x)) return(tibble())
    x <- reference_journal_year(x$References); if (!nrow(x)) return(x)
    latest <- max(x$year, na.rm = TRUE)
    x <- filter(x, year >= latest - input$recent_years + 1)
    top <- x |> group_by(journal) |> summarise(total = sum(n), .groups = "drop") |> arrange(desc(total), journal) |> slice_head(n = input$top_n)
    filter(x, journal %in% top$journal)
  })
  output$reference_status <- renderText({ if (!nrow(ref_data())) "No reference journal/year data are available in the collected records." else paste(nrow(ref_data()), "reference journal/year pairs shown for the selected recent period.") })
  output$reference_plot <- renderPlot({
    x <- ref_data(); req(nrow(x))
    counts <- x |> group_by(journal, year) |> summarise(value = sum(n), .groups = "drop")
    top10 <- counts |> group_by(journal) |> summarise(total = sum(value), .groups = "drop") |> arrange(desc(total), journal) |> slice_head(n = min(10, input$top_n)) |> pull(journal)
    years <- sort(unique(counts$year)); grid <- expand.grid(journal = top10, year = years, stringsAsFactors = FALSE)
    counts <- left_join(grid, filter(counts, journal %in% top10), by = c("journal", "year")) |> mutate(value = ifelse(is.na(value), 0, value))
    # Exact Tufte slopegraph layout: each journal receives one y-shift across its series.
    wide <- xtabs(value ~ journal + year, data = counts)
    wide <- wide[order(wide[, 1], rownames(wide)), , drop = FALSE]
    min_space <- 0.05 * diff(range(wide)); if (!is.finite(min_space) || min_space <= 0) min_space <- 0.5
    yshift <- numeric(nrow(wide))
    if (nrow(wide) > 1) for (i in 2:nrow(wide)) {
      d_min <- min(diff(as.matrix(wide[(i - 1):i, , drop = FALSE])))
      yshift[i] <- ifelse(is.finite(d_min) && d_min < min_space, min_space - d_min, 0)
    }
    yshift <- cumsum(yshift)
    slope <- do.call(rbind, lapply(seq_along(years), function(j) data.frame(group = rownames(wide), year = years[j], value = as.numeric(wide[, j]), ypos = as.numeric(wide[, j]) + yshift)))
    first <- slope[slope$year == years[1], , drop = FALSE]
    ggplot(slope, aes(x = year, y = ypos)) +
      geom_line(aes(group = group), colour = "red", linewidth = 0.9) +
      geom_point(colour = "white", fill = "white", shape = 21, stroke = 1, size = 5) +
      geom_text(aes(label = ifelse(value > 0, sprintf("%.0f", value), "")), size = 3.5, fontface = "bold") +
      scale_x_continuous(breaks = years) +
      scale_y_continuous(name = "", breaks = first$ypos, labels = first$group) +
      labs(title = "Top 10 referenced journals by year", subtitle = "Tufte-style slopegraph; labels are ordered by the first displayed year", x = "Reference year") +
      theme_classic(base_size = 14) +
      theme(axis.title.y = element_blank(), plot.title = element_text(hjust = .5, face = "bold", size = 18), axis.text = element_text(face = "bold"), plot.margin = margin(10, 15, 10, 120))
  })
  output$reference_counts <- renderTable({ ref_data() |> group_by(journal) |> summarise(Reference_Count = sum(n), .groups = "drop") |> arrange(desc(Reference_Count), journal) }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)








