# =============================================================================
# sjtkey: Shiny application for scoring, key evaluation, and visualization of
# Situational Judgement Tests (SJT)
# Usage: open this file in RStudio and click "Run App".
# Required package: shiny (everything else is base R).
# =============================================================================
library(shiny)

# ---- 1. Analysis functions -----------------------------------------------------

build_key <- function(ratings, method = c("mean", "rank")) {
  method <- match.arg(method)
  stopifnot(length(dim(ratings)) == 3)
  m <- apply(ratings, c(2, 3), mean, na.rm = TRUE)          # item x option
  if (method == "rank") m <- t(apply(m, 1, rank, ties.method = "average"))
  dimnames(m) <- list(paste0("item", seq_len(nrow(m))),
                      paste0("opt", seq_len(ncol(m))))
  m
}

expert_agreement <- function(ratings) {
  stopifnot(length(dim(ratings)) == 3, dim(ratings)[1] >= 2)
  m <- dim(ratings)[1]; n <- dim(ratings)[3]
  vapply(seq_len(dim(ratings)[2]), function(j) {
    R  <- t(apply(ratings[, j, ], 1, rank))                 # expert x option
    Rj <- colSums(R)
    S  <- sum((Rj - mean(Rj))^2)
    12 * S / (m^2 * (n^3 - n))
  }, numeric(1))
}

score_sjt <- function(resp, key,
                      method = c("expert", "best", "consensus", "standardized")) {
  method <- match.arg(method)
  resp <- as.matrix(resp); key <- as.matrix(key)
  stopifnot(ncol(resp) == nrow(key),
            all(resp %in% c(seq_len(ncol(key)), NA)))
  k <- ncol(resp)
  W <- switch(method,
    expert       = key,
    best         = (key == apply(key, 1, max)) * 1,
    consensus    = t(sapply(seq_len(k), function(j) {
                      tab <- tabulate(resp[!is.na(resp[, j]), j], nbins = ncol(key))
                      tab / sum(tab)
                    })),
    standardized = t(apply(key, 1, function(w) {
                      s <- sd(w)
                      if (is.na(s) || s == 0) rep(0, length(w)) else (w - mean(w)) / s
                    }))
  )
  S <- matrix(NA_real_, nrow(resp), k, dimnames = list(NULL, colnames(resp)))
  for (j in seq_len(k)) S[, j] <- W[j, resp[, j]]
  list(item_scores = S, total = rowSums(S), weights = W, method = method)
}

cronbach_alpha <- function(S) {
  S <- as.matrix(S); S <- S[stats::complete.cases(S), , drop = FALSE]
  k <- ncol(S)
  k / (k - 1) * (1 - sum(apply(S, 2, var)) / var(rowSums(S)))
}

option_analysis <- function(resp, key, method = "expert") {
  resp <- as.matrix(resp); key <- as.matrix(key)
  sc <- score_sjt(resp, key, method)
  out <- vector("list", ncol(resp) * ncol(key)); idx <- 0
  for (j in seq_len(ncol(resp))) {
    rest <- sc$total - sc$item_scores[, j]
    for (o in seq_len(ncol(key))) {
      idx <- idx + 1
      pick <- resp[, j] == o
      out[[idx]] <- data.frame(
        item = colnames(resp)[j], option = o, weight = key[j, o],
        prop = mean(pick, na.rm = TRUE),
        mean_rest = if (sum(pick, na.rm = TRUE) > 1) mean(rest[pick], na.rm = TRUE) else NA_real_,
        r_pb = suppressWarnings(cor(as.numeric(pick), rest, use = "complete.obs"))
      )
    }
  }
  do.call(rbind, out)
}

key_check <- function(oa, rho_min = 0.5, prop_min = 0.05) {
  parts <- split(oa, factor(oa$item, levels = unique(oa$item)))
  res <- lapply(parts, function(d) {
    ok  <- !is.na(d$mean_rest)
    rho <- if (sum(ok) >= 3) suppressWarnings(
      cor(d$weight[ok], d$mean_rest[ok], method = "spearman")) else NA_real_
    data.frame(item = d$item[1], rho = round(rho, 2),
               min_prop = round(min(d$prop), 2),
               flag = is.na(rho) || rho < rho_min || min(d$prop) < prop_min,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, unname(res))
  rownames(out) <- NULL
  out
}

compare_keys <- function(resp, key, criterion = NULL,
                         methods = c("expert", "best", "consensus", "standardized")) {
  sc  <- lapply(methods, function(m) score_sjt(resp, key, m)); names(sc) <- methods
  tot <- sapply(sc, function(s) s$total)
  out <- data.frame(method = methods,
                    alpha  = round(unname(sapply(sc, function(s) cronbach_alpha(s$item_scores))), 3),
                    stringsAsFactors = FALSE)
  if (!is.null(criterion)) {
    out$r_criterion <- round(unname(vapply(methods, function(m)
      cor(tot[, m], criterion, use = "pairwise.complete.obs"), numeric(1))), 3)
  }
  list(summary = out,
       score_cor = round(cor(tot, use = "pairwise.complete.obs"), 3),
       scores = tot)
}


read_csv_flex <- function(f) {
  l1  <- readLines(f, n = 1, warn = FALSE)
  sep <- if (grepl(";", l1, fixed = TRUE) && !grepl(",", l1, fixed = TRUE)) ";" else ","
  read.csv(f, sep = sep, stringsAsFactors = FALSE, check.names = FALSE,
           fileEncoding = "UTF-8-BOM")
}

# Fallback simulated data
sim_sjt <- function(n = 300, k = 12, seed = 1) {
  set.seed(seed)
  theta <- rnorm(n)
  key <- t(replicate(k, sample(4:1)))
  dimnames(key) <- list(paste0("item", 1:k), paste0("opt", 1:4))
  resp <- matrix(NA_integer_, n, k, dimnames = list(NULL, rownames(key)))
  for (j in 1:k) {
    w <- key[j, ]
    for (i in 1:n) {
      lg <- 0.8 * theta[i] * (w - 2.5)
      p  <- exp(lg) / sum(exp(lg))
      resp[i, j] <- sample.int(4, 1, prob = p)
    }
  }
  resp <- as.data.frame(resp)
  resp$criterion_score <- round(55 + 12 * (0.55 * theta + sqrt(1 - 0.55^2) * rnorm(n)))
  list(resp = resp, key = key)
}

# ---- 6. Plot functions (base R) ----------------------------------------
pal_rank <- c("#2c7bb6", "#66c2a5", "#fdae61", "#d7191c")  # rank 1 (best) ... 4

plot_methods <- function(rs) {
  M <- rbind("Alpha" = rs$alpha)
  if ("r_criterion" %in% names(rs)) M <- rbind(M, "Criterion correlation" = rs$r_criterion)
  colnames(M) <- rs$method
  barplot(M, beside = TRUE, col = c("#2c7bb6", "#fdae61")[seq_len(nrow(M))],
          ylim = c(min(0, M, na.rm = TRUE), max(1, M, na.rm = TRUE)),
          ylab = "Value", main = "Comparison of scoring methods",
          legend.text = rownames(M), args.legend = list(x = "bottomright", bty = "n"))
  abline(h = 0)
}

plot_cor_heat <- function(cm) {
  k <- ncol(cm)
  op <- par(mar = c(6, 7, 3, 2)); on.exit(par(op))
  image(1:k, 1:k, t(cm[k:1, , drop = FALSE]),
        col = colorRampPalette(c("#d7191c", "white", "#2c7bb6"))(100),
        zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "",
        main = "Correlation among scoring methods")
  axis(1, at = 1:k, labels = colnames(cm), las = 2)
  axis(2, at = 1:k, labels = rev(rownames(cm)), las = 2)
  for (i in 1:k) for (j in 1:k) text(j, k - i + 1, sprintf("%.2f", cm[i, j]))
  box()
}

plot_total <- function(total) {
  hist(total, breaks = 20, col = "#abd9e9", border = "white", freq = FALSE,
       main = "Distribution of total scores (expert weights)", xlab = "Total score")
  lines(density(total), lwd = 2, col = "#2c7bb6")
}

plot_criterion <- function(total, criterion) {
  r <- cor(total, criterion, use = "pairwise.complete.obs")
  plot(total, criterion, pch = 19, col = "#2c7bb680",
       xlab = "SJT score (expert weights)", ylab = "External criterion score",
       main = paste0("Criterion validity (r = ", sprintf("%.2f", r), ")"))
  abline(lm(criterion ~ total), col = "#d7191c", lwd = 2)
}

plot_option_props <- function(oa) {
  its <- unique(oa$item)
  M <- sapply(its, function(it) {
    d <- oa[oa$item == it, ]
    d[order(-d$weight), "prop"]            # proportions: best -> worst option
  })
  colnames(M) <- its
  op <- par(mar = c(5, 4, 3, 9), xpd = TRUE); on.exit(par(op))
  barplot(M, col = pal_rank[seq_len(nrow(M))], las = 2, ylim = c(0, 1),
          ylab = "Proportion of respondents", main = "Option choices per item",
          border = "white")
  legend("topright", inset = c(-0.2, 0), bty = "n", cex = 0.8,
         legend = c("Best option (experts)", "2nd option", "3rd option", "Worst option")[seq_len(nrow(M))],
         fill = pal_rank[seq_len(nrow(M))])
}

plot_key_check <- function(kc, rho_min = 0.5) {
  op <- par(mar = c(6, 4, 3, 1)); on.exit(par(op))
  cols <- ifelse(kc$flag, "#d7191c", "#2c7bb6")
  plot(seq_len(nrow(kc)), kc$rho, ylim = c(-1, 1), pch = 19, cex = 1.3, col = cols,
       xaxt = "n", xlab = "", ylab = "Spearman rho (expert weight vs rest score)",
       main = "Key check per item")
  axis(1, at = seq_len(nrow(kc)), labels = kc$item, las = 2, cex.axis = 0.8)
  abline(h = rho_min, lty = 2); abline(h = 0, col = "grey70")
  legend("bottomright", bty = "n", pch = 19, col = c("#2c7bb6", "#d7191c"),
         legend = c("Acceptable", "Needs review"))
}

plot_option_curves <- function(resp, key, items_plot, n_grup = 4) {
  resp <- as.matrix(resp); key <- as.matrix(key)
  sc <- score_sjt(resp, key, "expert")
  n_p <- length(items_plot)
  op <- par(mfrow = c(ceiling(n_p / 3), min(3, n_p)), mar = c(4, 4, 3, 1))
  on.exit(par(op))
  for (it in items_plot) {
    j    <- match(it, colnames(resp))
    rest <- sc$total - sc$item_scores[, j]
    br   <- unique(quantile(rest, probs = seq(0, 1, length.out = n_grup + 1), na.rm = TRUE))
    if (length(br) < 3) next
    grp  <- cut(rest, breaks = br, include.lowest = TRUE, labels = FALSE)
    ng   <- max(grp, na.rm = TRUE)
    P    <- sapply(seq_len(ncol(key)), function(o) tapply(resp[, j] == o, grp, mean))
    P    <- matrix(P, nrow = ng)
    cols <- pal_rank[rank(-key[j, ], ties.method = "first")]
    matplot(seq_len(ng), P, type = "b", lty = 1, pch = 16, col = cols,
            ylim = c(0, 1), xaxt = "n",
            xlab = "Rest-score group (low -> high)",
            ylab = "Proportion choosing", main = it)
    axis(1, at = seq_len(ng))
    legend("top", bty = "n", cex = 0.6, ncol = 2, lty = 1, pch = 16, col = cols,
           legend = paste0("option", seq_len(ncol(key)), " (b=", key[j, ], ")"))
  }
}

plot_w_expert <- function(w, its) {
  op <- par(mar = c(6, 4, 3, 1)); on.exit(par(op))
  barplot(w, names.arg = its, las = 2, ylim = c(0, 1), col = "#66c2a5", border = "white",
          ylab = "Kendall's W", main = "Expert agreement per item")
  abline(h = 0.5, lty = 2)
}


# ---- 7. Data loading for the app -------------------------------------------
# Column names are English; the former Indonesian names are still accepted.
rename_cols <- function(df) {
  map <- c(pakar = "expert", opsi = "option", skor_kriteria = "criterion_score")
  hit <- names(df) %in% names(map)
  names(df)[hit] <- map[names(df)[hit]]
  df
}

prepare_data <- function(f_resp, f_key, f_rate = NULL) {
  resp <- rename_cols(read_csv_flex(f_resp))
  kk   <- read_csv_flex(f_key)
  key  <- as.matrix(kk[, -1, drop = FALSE])
  rownames(key) <- as.character(kk[[1]])
  storage.mode(key) <- "numeric"
  items <- intersect(rownames(key), names(resp))
  if (length(items) == 0)
    items <- grep("^item[0-9]+$", names(resp), ignore.case = TRUE, value = TRUE)
  if (length(items) == 0)
    stop("No item columns found. Response file columns: ",
         paste(names(resp), collapse = ", "))
  key <- key[items, , drop = FALSE]
  resp[items] <- lapply(resp[items], as.integer)
  if (!all(unlist(resp[items]) %in% c(seq_len(ncol(key)), NA)))
    stop("Response codes outside 1..", ncol(key), ". Please check the response file.")
  ratings <- NULL; w_expert <- NULL
  if (!is.null(f_rate)) {
    rt <- rename_cols(read_csv_flex(f_rate))
    need <- c("expert", "item", "option", "rating")
    if (!all(need %in% names(rt)))
      stop("The ratings file needs the columns: ", paste(need, collapse = ", "))
    n_expert <- length(unique(rt$expert)); n_item <- length(unique(rt$item))
    n_option <- length(unique(rt$option))
    # Rows are ordered expert (outermost) > item > option (innermost)
    ratings  <- aperm(array(rt$rating, dim = c(n_option, n_item, n_expert)), c(3, 2, 1))
    w_expert <- expert_agreement(ratings)
  }
  list(resp = resp, key = key, items = items, ratings = ratings, w_expert = w_expert,
       criterion = if ("criterion_score" %in% names(resp)) resp$criterion_score else NULL)
}

example_ok <- all(file.exists(file.path("example_data",
  c("responses.csv", "key.csv"))))

# ---- 8. User interface ------------------------------------------------------
ui <- fluidPage(
  titlePanel("sjtkey: Scoring and Key Evaluation for Situational Judgement Tests"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons("source", "Data source",
        choices = c(if (example_ok) c("Example data (simulated)" = "example"),
                    "Built-in simulation" = "sim",
                    "Upload my CSV files" = "upload"),
        selected = if (example_ok) "example" else "sim"),
      conditionalPanel("input.source == 'upload'",
        fileInput("f_resp", "Responses (CSV)", accept = ".csv"),
        fileInput("f_key",  "Expert key (CSV)", accept = ".csv"),
        fileInput("f_rate", "Expert ratings, long format (CSV, optional)", accept = ".csv"),
        helpText("Responses: columns item1..itemK with the chosen option (1..m); ",
                 "optional criterion_score. Key: first column = item name, then one ",
                 "column per option. Ratings: columns expert, item, option, rating.")),
      hr(),
      sliderInput("rho_min", "Spearman rho threshold (key check)",
                  min = 0, max = 1, value = 0.9, step = 0.05),
      sliderInput("prop_min", "Minimum option proportion",
                  min = 0, max = 0.2, value = 0.05, step = 0.01),
      hr(),
      downloadButton("dl_summary", "Download method summary"),
      br(), br(),
      downloadButton("dl_options", "Download option analysis"),
      br(), br(),
      downloadButton("dl_key", "Download key check"),
      br(), br(),
      downloadButton("dl_scores", "Download respondent scores")
    ),
    mainPanel(
      width = 9,
      verbatimTextOutput("info"),
      tabsetPanel(
        tabPanel("Scoring methods",
          tableOutput("tb_summary"),
          plotOutput("g_methods", height = 320),
          plotOutput("g_cor", height = 380),
          plotOutput("g_total", height = 320),
          plotOutput("g_criterion", height = 340)),
        tabPanel("Option analysis",
          plotOutput("g_props", height = 380),
          tableOutput("tb_options")),
        tabPanel("Key check",
          plotOutput("g_kc", height = 340),
          tableOutput("tb_kc"),
          h4("Option curves for flagged items"),
          plotOutput("g_curves", height = 520)),
        tabPanel("Expert agreement",
          uiOutput("note_expert"),
          plotOutput("g_w", height = 340))
      )
    )
  )
)

# ---- 9. Server --------------------------------------------------------------
server <- function(input, output, session) {

  d <- reactive({
    if (input$source == "example") {
      f_rate <- file.path("example_data", "expert_ratings.csv")
      prepare_data(file.path("example_data", "responses.csv"),
                   file.path("example_data", "key.csv"),
                   if (file.exists(f_rate)) f_rate else NULL)
    } else if (input$source == "sim") {
      s <- sim_sjt()
      list(resp = s$resp, key = s$key, items = rownames(s$key), ratings = NULL,
           w_expert = NULL, criterion = s$resp$criterion_score)
    } else {
      validate(need(input$f_resp, "Please upload the responses file."),
               need(input$f_key,  "Please upload the expert key file."))
      prepare_data(input$f_resp$datapath, input$f_key$datapath,
                   if (!is.null(input$f_rate)) input$f_rate$datapath else NULL)
    }
  })

  result <- reactive({ x <- d(); compare_keys(x$resp[, x$items], x$key, x$criterion) })
  oa     <- reactive({ x <- d(); option_analysis(x$resp[, x$items], x$key) })
  kc     <- reactive(key_check(oa(), rho_min = input$rho_min, prop_min = input$prop_min))

  output$info <- renderText({
    x <- d()
    paste0("Respondents: ", nrow(x$resp), " | Items: ", length(x$items),
           " | Options: ", ncol(x$key),
           " | Items flagged for review: ", sum(kc()$flag))
  })

  output$tb_summary <- renderTable(result()$summary, digits = 3)
  output$g_methods  <- renderPlot(plot_methods(result()$summary))
  output$g_cor      <- renderPlot(plot_cor_heat(result()$score_cor))
  output$g_total    <- renderPlot(plot_total(result()$scores[, "expert"]))
  output$g_criterion <- renderPlot({
    validate(need(!is.null(d()$criterion), "No criterion_score column in the data."))
    plot_criterion(result()$scores[, "expert"], d()$criterion)
  })

  output$g_props   <- renderPlot(plot_option_props(oa()))
  output$tb_options <- renderTable(transform(oa(), prop = round(prop, 3),
    mean_rest = round(mean_rest, 2), r_pb = round(r_pb, 3)))

  output$g_kc  <- renderPlot(plot_key_check(kc(), rho_min = input$rho_min))
  output$tb_kc <- renderTable(kc())
  output$g_curves <- renderPlot({
    x <- d()
    ip <- head(kc()$item[kc()$flag], 6)
    if (length(ip) == 0) ip <- head(x$items, 6)
    plot_option_curves(x$resp[, x$items], x$key, ip)
  })

  output$note_expert <- renderUI({
    if (is.null(d()$w_expert))
      p("Expert agreement needs a ratings file (columns expert, item, option, rating).")
  })
  output$g_w <- renderPlot({
    validate(need(!is.null(d()$w_expert), "Expert ratings are not available."))
    plot_w_expert(d()$w_expert, d()$items)
  })

  download_csv <- function(name, content)
    downloadHandler(filename = function() name,
                    content  = function(file) write.csv(content(), file, row.names = FALSE))
  output$dl_summary <- download_csv("method_summary.csv", function() result()$summary)
  output$dl_options <- download_csv("option_analysis.csv", oa)
  output$dl_key     <- download_csv("key_check.csv", kc)
  output$dl_scores  <- download_csv("respondent_scores.csv", function()
    data.frame(respondent = seq_len(nrow(result()$scores)), result()$scores))
}

shinyApp(ui, server)
