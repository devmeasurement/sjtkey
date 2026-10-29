# =============================================================================
# sjtkey: aplikasi Shiny untuk penskoran, evaluasi kunci, dan visualisasi SJT
# Jalankan: buka file ini di RStudio lalu klik "Run App"
# Paket yang dibutuhkan: shiny (selain itu hanya R dasar)
# =============================================================================
library(shiny)

# ---- 1. Fungsi analisis -----------------------------------------------------

build_key <- function(ratings, method = c("mean", "rank")) {
  method <- match.arg(method)
  stopifnot(length(dim(ratings)) == 3)
  m <- apply(ratings, c(2, 3), mean, na.rm = TRUE)          # butir x opsi
  if (method == "rank") m <- t(apply(m, 1, rank, ties.method = "average"))
  dimnames(m) <- list(paste0("item", seq_len(nrow(m))),
                      paste0("opt", seq_len(ncol(m))))
  m
}

expert_agreement <- function(ratings) {
  stopifnot(length(dim(ratings)) == 3, dim(ratings)[1] >= 2)
  m <- dim(ratings)[1]; n <- dim(ratings)[3]
  vapply(seq_len(dim(ratings)[2]), function(j) {
    R  <- t(apply(ratings[, j, ], 1, rank))                 # pakar x opsi
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
               periksa = is.na(rho) || rho < rho_min || min(d$prop) < prop_min,
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
  out <- data.frame(metode = methods,
                    alpha  = round(unname(sapply(sc, function(s) cronbach_alpha(s$item_scores))), 3),
                    stringsAsFactors = FALSE)
  if (!is.null(criterion)) {
    out$r_kriteria <- round(unname(vapply(methods, function(m)
      cor(tot[, m], criterion, use = "pairwise.complete.obs"), numeric(1))), 3)
  }
  list(ringkasan = out,
       korelasi_skor = round(cor(tot, use = "pairwise.complete.obs"), 3),
       skor = tot)
}


baca_csv <- function(f) {
  l1  <- readLines(f, n = 1, warn = FALSE)
  sep <- if (grepl(";", l1, fixed = TRUE) && !grepl(",", l1, fixed = TRUE)) ";" else ","
  read.csv(f, sep = sep, stringsAsFactors = FALSE, check.names = FALSE,
           fileEncoding = "UTF-8-BOM")
}

# Data simulasi cadangan
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
  resp$skor_kriteria <- round(55 + 12 * (0.55 * theta + sqrt(1 - 0.55^2) * rnorm(n)))
  list(resp = resp, key = key)
}

# ---- 6. Fungsi visualisasi (R dasar) ----------------------------------------
pal_rank <- c("#2c7bb6", "#66c2a5", "#fdae61", "#d7191c")  # urutan 1 (terbaik) ... 4

plot_metode <- function(rs) {
  M <- rbind("Alpha" = rs$alpha)
  if ("r_kriteria" %in% names(rs)) M <- rbind(M, "Korelasi kriteria" = rs$r_kriteria)
  colnames(M) <- rs$metode
  barplot(M, beside = TRUE, col = c("#2c7bb6", "#fdae61")[seq_len(nrow(M))],
          ylim = c(min(0, M, na.rm = TRUE), max(1, M, na.rm = TRUE)),
          ylab = "Nilai", main = "Perbandingan metode skor",
          legend.text = rownames(M), args.legend = list(x = "bottomright", bty = "n"))
  abline(h = 0)
}

plot_cor_heat <- function(cm) {
  k <- ncol(cm)
  op <- par(mar = c(6, 7, 3, 2)); on.exit(par(op))
  image(1:k, 1:k, t(cm[k:1, , drop = FALSE]),
        col = colorRampPalette(c("#d7191c", "white", "#2c7bb6"))(100),
        zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "",
        main = "Korelasi antar metode skor")
  axis(1, at = 1:k, labels = colnames(cm), las = 2)
  axis(2, at = 1:k, labels = rev(rownames(cm)), las = 2)
  for (i in 1:k) for (j in 1:k) text(j, k - i + 1, sprintf("%.2f", cm[i, j]))
  box()
}

plot_total <- function(total) {
  hist(total, breaks = 20, col = "#abd9e9", border = "white", freq = FALSE,
       main = "Distribusi skor total (bobot pakar)", xlab = "Skor total")
  lines(density(total), lwd = 2, col = "#2c7bb6")
}

plot_kriteria <- function(total, kriteria) {
  r <- cor(total, kriteria, use = "pairwise.complete.obs")
  plot(total, kriteria, pch = 19, col = "#2c7bb680",
       xlab = "Skor SJT (bobot pakar)", ylab = "Skor kriteria eksternal",
       main = paste0("Validitas kriteria (r = ", sprintf("%.2f", r), ")"))
  abline(lm(kriteria ~ total), col = "#d7191c", lwd = 2)
}

plot_option_props <- function(oa) {
  its <- unique(oa$item)
  M <- sapply(its, function(it) {
    d <- oa[oa$item == it, ]
    d[order(-d$weight), "prop"]            # proporsi: opsi terbaik -> terburuk
  })
  colnames(M) <- its
  op <- par(mar = c(5, 4, 3, 9), xpd = TRUE); on.exit(par(op))
  barplot(M, col = pal_rank[seq_len(nrow(M))], las = 2, ylim = c(0, 1),
          ylab = "Proporsi pemilih", main = "Pilihan responden per butir",
          border = "white")
  legend("topright", inset = c(-0.2, 0), bty = "n", cex = 0.8,
         legend = c("Opsi terbaik (pakar)", "Opsi ke-2", "Opsi ke-3", "Opsi terburuk")[seq_len(nrow(M))],
         fill = pal_rank[seq_len(nrow(M))])
}

plot_key_check <- function(kc, rho_min = 0.5) {
  op <- par(mar = c(6, 4, 3, 1)); on.exit(par(op))
  cols <- ifelse(kc$periksa, "#d7191c", "#2c7bb6")
  plot(seq_len(nrow(kc)), kc$rho, ylim = c(-1, 1), pch = 19, cex = 1.3, col = cols,
       xaxt = "n", xlab = "", ylab = "Spearman rho (bobot pakar vs skor sisa)",
       main = "Pemeriksaan kunci per butir")
  axis(1, at = seq_len(nrow(kc)), labels = kc$item, las = 2, cex.axis = 0.8)
  abline(h = rho_min, lty = 2); abline(h = 0, col = "grey70")
  legend("bottomright", bty = "n", pch = 19, col = c("#2c7bb6", "#d7191c"),
         legend = c("Wajar", "Perlu diperiksa"))
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
            xlab = "Kelompok skor sisa (rendah -> tinggi)",
            ylab = "Proporsi memilih", main = it)
    axis(1, at = seq_len(ng))
    legend("top", bty = "n", cex = 0.6, ncol = 2, lty = 1, pch = 16, col = cols,
           legend = paste0("opsi", seq_len(ncol(key)), " (b=", key[j, ], ")"))
  }
}

plot_w_pakar <- function(w, its) {
  op <- par(mar = c(6, 4, 3, 1)); on.exit(par(op))
  barplot(w, names.arg = its, las = 2, ylim = c(0, 1), col = "#66c2a5", border = "white",
          ylab = "Kendall's W", main = "Kesepakatan pakar per butir")
  abline(h = 0.5, lty = 2)
}


# ---- Pemuat data untuk aplikasi --------------------------------------------
siapkan_data <- function(f_resp, f_key, f_rate = NULL) {
  resp <- baca_csv(f_resp)
  kk   <- baca_csv(f_key)
  key  <- as.matrix(kk[, -1, drop = FALSE])
  rownames(key) <- as.character(kk[[1]])
  storage.mode(key) <- "numeric"
  items <- intersect(rownames(key), names(resp))
  if (length(items) == 0)
    items <- grep("^item[0-9]+$", names(resp), ignore.case = TRUE, value = TRUE)
  if (length(items) == 0)
    stop("Kolom butir tidak ditemukan. Nama kolom respons: ",
         paste(names(resp), collapse = ", "))
  key <- key[items, , drop = FALSE]
  resp[items] <- lapply(resp[items], as.integer)
  if (!all(unlist(resp[items]) %in% c(seq_len(ncol(key)), NA)))
    stop("Ada kode respons di luar 1..", ncol(key), ". Periksa respons_sjt.csv.")
  ratings <- NULL; w_pakar <- NULL
  if (!is.null(f_rate)) {
    rt <- baca_csv(f_rate)
    n_pakar <- length(unique(rt$pakar)); n_item <- length(unique(rt$item))
    n_opsi  <- length(unique(rt$opsi))
    ratings <- aperm(array(rt$rating, dim = c(n_opsi, n_item, n_pakar)), c(3, 2, 1))
    w_pakar <- expert_agreement(ratings)
  }
  list(resp = resp, key = key, items = items, ratings = ratings, w_pakar = w_pakar,
       kriteria = if ("skor_kriteria" %in% names(resp)) resp$skor_kriteria else NULL)
}

ada_contoh <- all(file.exists(file.path("data_contoh",
  c("respons_sjt.csv", "kunci_pakar.csv"))))

# ---- Antarmuka --------------------------------------------------------------
ui <- fluidPage(
  titlePanel("sjtkey: Penskoran dan Evaluasi Kunci SJT"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons("sumber", "Sumber data",
        choices = c(if (ada_contoh) c("Data contoh (simulasi)" = "contoh"),
                    "Simulasi cadangan dari R" = "sim",
                    "Unggah CSV saya" = "unggah"),
        selected = if (ada_contoh) "contoh" else "sim"),
      conditionalPanel("input.sumber == 'unggah'",
        fileInput("f_resp", "respons_sjt.csv", accept = ".csv"),
        fileInput("f_key",  "kunci_pakar.csv", accept = ".csv"),
        fileInput("f_rate", "rating_pakar_long.csv (opsional)", accept = ".csv")),
      hr(),
      sliderInput("rho_min", "Ambang Spearman rho (pemeriksaan kunci)",
                  min = 0, max = 1, value = 0.9, step = 0.05),
      sliderInput("prop_min", "Ambang proporsi pemilih minimum",
                  min = 0, max = 0.2, value = 0.05, step = 0.01),
      hr(),
      downloadButton("dl_ringkasan", "Unduh ringkasan metode"),
      br(), br(),
      downloadButton("dl_opsi", "Unduh analisis opsi"),
      br(), br(),
      downloadButton("dl_kunci", "Unduh pemeriksaan kunci"),
      br(), br(),
      downloadButton("dl_skor", "Unduh skor responden")
    ),
    mainPanel(
      width = 9,
      verbatimTextOutput("info"),
      tabsetPanel(
        tabPanel("Metode skor",
          tableOutput("tb_ringkasan"),
          plotOutput("g_metode", height = 320),
          plotOutput("g_cor", height = 380),
          plotOutput("g_total", height = 320),
          plotOutput("g_kriteria", height = 340)),
        tabPanel("Analisis opsi",
          plotOutput("g_props", height = 380),
          tableOutput("tb_opsi")),
        tabPanel("Pemeriksaan kunci",
          plotOutput("g_kc", height = 340),
          tableOutput("tb_kc"),
          h4("Kurva opsi butir yang perlu diperiksa"),
          plotOutput("g_curves", height = 520)),
        tabPanel("Kesepakatan pakar",
          uiOutput("ket_pakar"),
          plotOutput("g_w", height = 340))
      )
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  d <- reactive({
    if (input$sumber == "contoh") {
      f_rate <- file.path("data_contoh", "rating_pakar_long.csv")
      siapkan_data(file.path("data_contoh", "respons_sjt.csv"),
                   file.path("data_contoh", "kunci_pakar.csv"),
                   if (file.exists(f_rate)) f_rate else NULL)
    } else if (input$sumber == "sim") {
      s <- sim_sjt()
      list(resp = s$resp, key = s$key, items = rownames(s$key), ratings = NULL,
           w_pakar = NULL, kriteria = s$resp$skor_kriteria)
    } else {
      validate(need(input$f_resp, "Unggah respons_sjt.csv dan kunci_pakar.csv."),
               need(input$f_key,  "Unggah kunci_pakar.csv."))
      siapkan_data(input$f_resp$datapath, input$f_key$datapath,
                   if (!is.null(input$f_rate)) input$f_rate$datapath else NULL)
    }
  })

  hasil <- reactive({ x <- d(); compare_keys(x$resp[, x$items], x$key, x$kriteria) })
  oa    <- reactive({ x <- d(); option_analysis(x$resp[, x$items], x$key) })
  kc    <- reactive(key_check(oa(), rho_min = input$rho_min, prop_min = input$prop_min))

  output$info <- renderText({
    x <- d()
    paste0("Responden: ", nrow(x$resp), " | Butir: ", length(x$items),
           " | Opsi: ", ncol(x$key),
           " | Butir perlu diperiksa: ", sum(kc()$periksa))
  })

  output$tb_ringkasan <- renderTable(hasil()$ringkasan, digits = 3)
  output$g_metode  <- renderPlot(plot_metode(hasil()$ringkasan))
  output$g_cor     <- renderPlot(plot_cor_heat(hasil()$korelasi_skor))
  output$g_total   <- renderPlot(plot_total(hasil()$skor[, "expert"]))
  output$g_kriteria <- renderPlot({
    validate(need(!is.null(d()$kriteria), "Kolom skor_kriteria tidak ada di data."))
    plot_kriteria(hasil()$skor[, "expert"], d()$kriteria)
  })

  output$g_props <- renderPlot(plot_option_props(oa()))
  output$tb_opsi <- renderTable(transform(oa(), prop = round(prop, 3),
    mean_rest = round(mean_rest, 2), r_pb = round(r_pb, 3)))

  output$g_kc  <- renderPlot(plot_key_check(kc(), rho_min = input$rho_min))
  output$tb_kc <- renderTable(kc())
  output$g_curves <- renderPlot({
    x <- d()
    ip <- head(kc()$item[kc()$periksa], 6)
    if (length(ip) == 0) ip <- head(x$items, 6)
    plot_option_curves(x$resp[, x$items], x$key, ip)
  })

  output$ket_pakar <- renderUI({
    if (is.null(d()$w_pakar))
      p("Kesepakatan pakar butuh file rating_pakar_long.csv (kolom pakar, item, opsi, rating).")
  })
  output$g_w <- renderPlot({
    validate(need(!is.null(d()$w_pakar), "Data rating pakar belum tersedia."))
    plot_w_pakar(d()$w_pakar, d()$items)
  })

  unduh <- function(nama, isi)
    downloadHandler(filename = function() nama,
                    content  = function(file) write.csv(isi(), file, row.names = FALSE))
  output$dl_ringkasan <- unduh("ringkasan_metode.csv", function() hasil()$ringkasan)
  output$dl_opsi      <- unduh("analisis_opsi.csv", oa)
  output$dl_kunci     <- unduh("pemeriksaan_kunci.csv", kc)
  output$dl_skor      <- unduh("skor_responden.csv", function()
    data.frame(responden = seq_len(nrow(hasil()$skor)), hasil()$skor))
}

shinyApp(ui, server)
