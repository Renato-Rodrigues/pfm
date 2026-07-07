# nolint start
#' Wild-cluster bootstrap-t inference for PSM satP fits
#'
#' @description
#' With 25 regional clusters the asymptotic cluster-robust SEs behind every PSM
#' headline are optimistic (Cameron–Gelbach–Miller). This implements the
#' Rademacher wild-cluster bootstrap-t for the gaussian satP engine: residuals
#' are flipped per cluster, the model is refit, and each coefficient's t
#' statistic is recomputed with CR1 clustered SEs in every replication; the
#' reported p-value is the share of bootstrap |t*| exceeding the original |t|.
#' The percentile-t construction is the few-clusters-robust variant.
#'
#' @param fit A satP-engine fit result (list with \code{model}, \code{formula},
#'   \code{data}; gaussian identity).
#' @param B Integer. Bootstrap replications. Default \code{999}.
#' @param seed Integer. RNG seed. Default \code{42}.
#'
#' @return Data.frame \code{term, estimate, tOriginal, pWild} (intercept and
#'   region-FE dummies excluded).
#'
#' @author Renato Rodrigues
#' @importFrom stats model.matrix as.formula complete.cases coef
#' @export
computeWildClusterBootstrap <- function(fit, B = 999, seed = 42) {
  df <- fit$data
  fml <- stats::as.formula(fit$formula)
  vars <- intersect(all.vars(fml), colnames(df))
  df <- df[stats::complete.cases(df[, vars, drop = FALSE]), , drop = FALSE]
  # empty regionFE levels (out-of-coverage H12 regions) create all-zero dummy
  # columns -> singular crossprod (same failure mode as the frontier rung)
  if ("regionFE" %in% colnames(df) && is.factor(df$regionFE)) {
    df$regionFE <- droplevels(df$regionFE)
  }
  mm <- stats::model.matrix(fml, data = df)
  yv <- df$ecp[match(rownames(mm), rownames(df))]
  g <- as.character(df$region[match(rownames(mm), rownames(df))])
  G <- length(unique(g))
  n <- nrow(mm)
  k <- ncol(mm)
  xtxInv <- solve(crossprod(mm))
  adj <- (G / (G - 1)) * ((n - 1) / (n - k))
  clusterSE <- function(res) {
    us <- rowsum(mm * res, g)
    sqrt(pmax(diag(xtxInv %*% crossprod(us) %*% xtxInv) * adj, 0))
  }
  bHat <- as.numeric(xtxInv %*% crossprod(mm, yv))
  names(bHat) <- colnames(mm)
  resHat <- yv - as.numeric(mm %*% bHat)
  seHat <- clusterSE(resHat)
  tHat <- bHat / seHat
  muHat <- as.numeric(mm %*% bHat)

  set.seed(seed)
  gLev <- unique(g)
  tStar <- matrix(NA_real_, B, k)
  for (b in seq_len(B)) {
    w <- stats::setNames(sample(c(-1, 1), length(gLev), replace = TRUE), gLev)
    yS <- muHat + w[g] * resHat
    bS <- as.numeric(xtxInv %*% crossprod(mm, yS))
    resS <- yS - as.numeric(mm %*% bS)
    tStar[b, ] <- (bS - bHat) / clusterSE(resS)
  }
  pWild <- vapply(seq_len(k), function(j) {
    mean(abs(tStar[, j]) >= abs(tHat[j]), na.rm = TRUE)
  }, numeric(1))
  keep <- !grepl("^\\(Intercept\\)$|^regionFE", colnames(mm))
  data.frame(term = colnames(mm)[keep], estimate = bHat[keep],
             tOriginal = tHat[keep], pWild = pWild[keep],
             row.names = NULL, stringsAsFactors = FALSE)
}
# nolint end
