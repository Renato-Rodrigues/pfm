# Auto-size a priority-QOS SLURM job to the user's actual allowance (--priority, ADR 0031). Queries
# `sacctmgr` for the QOS caps and `sinfo` for the node size, then picks the largest cpu/mem/time the
# allowance and a single node permit (the PFM sweep is one shared-memory future pool: #SBATCH
# --nodes=1, so cores can't exceed one node). Parsers take an injectable `.raw` for testing.

# Internal: parse `sacctmgr show qos <q> format=MaxTRES,MaxTRESPU,MaxWall -p` output.
#' @keywords internal
.parseQosLimits <- function(qos, .raw = NULL) {
  raw <- if (!is.null(.raw)) .raw else tryCatch(
    suppressWarnings(system2("sacctmgr",
      c("show", "qos", qos, "format=MaxTRES,MaxTRESPU,MaxWall", "-p", "-n"),
      stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0))
  txt <- paste(raw[nzchar(raw)], collapse = " ")
  if (!nzchar(txt)) return(list(cpu = NA_integer_, mem = NA_character_, wall = NA_character_))
  cpus <- as.integer(regmatches(txt, gregexpr("(?<=cpu=)[0-9]+", txt, perl = TRUE))[[1]])
  mems <- regmatches(txt, gregexpr("(?<=mem=)[0-9]+[A-Za-z]*", txt, perl = TRUE))[[1]]
  wall <- regmatches(txt, regexpr("[0-9]+-[0-9]{2}:[0-9]{2}:[0-9]{2}|[0-9]{1,2}:[0-9]{2}:[0-9]{2}", txt))
  list(cpu  = if (length(cpus)) min(cpus) else NA_integer_,   # min across MaxTRES/MaxTRESPU = binding
       mem  = if (length(mems)) mems[[1]] else NA_character_,
       wall = if (length(wall)) wall[[1]] else NA_character_)
}

# Internal: partitions whose AllowQos permits `qos` (or AllowQos=ALL), from `scontrol show partition`.
# A QOS being in your association is necessary but not sufficient — the partition must also allow it
# (e.g. the PIK `standard` partition allows only short/medium/long/benchmark, NOT priority).
#' @keywords internal
.partitionsForQos <- function(qos, .raw = NULL) {
  raw <- if (!is.null(.raw)) .raw else tryCatch(
    suppressWarnings(system2("scontrol", c("show", "partition"), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0))
  if (!length(raw)) return(character(0))
  blocks <- strsplit(paste(raw, collapse = "\n"), "PartitionName=")[[1]]
  out <- character(0)
  for (b in blocks) {
    if (!nzchar(trimws(b))) next
    name <- sub("^([^[:space:]]+).*", "\\1", b)
    aq <- regmatches(b, regexpr("AllowQos=[^[:space:]]+", b))
    if (!length(aq)) next
    aq <- sub("AllowQos=", "", aq)
    if (identical(aq, "ALL") || qos %in% strsplit(aq, ",")[[1]]) out <- c(out, name)
  }
  unique(out)
}

# Internal: parse `sinfo -h -o "%c %m" -p <partition>` (cores, mem-MB per node); take the largest node.
#' @keywords internal
.parseNodeSpecs <- function(partition, .raw = NULL) {
  raw <- if (!is.null(.raw)) .raw else tryCatch(
    suppressWarnings(system2("sinfo", c("-h", "-o", "%c %m", "-p", partition),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0))
  raw <- raw[nzchar(raw)]
  if (!length(raw)) return(list(cpu = NA_integer_, memGB = NA_real_))
  # Robust to short/odd lines: pull the first two integer fields per line, skip lines without them.
  num1 <- function(tok) suppressWarnings(as.numeric(gsub("\\D.*", "", tok)))
  cpus <- numeric(0); mems <- numeric(0)
  for (ln in raw) {
    x <- strsplit(trimws(ln), "[[:space:]]+")[[1]]
    if (length(x) >= 1) cpus <- c(cpus, num1(x[[1]]))
    if (length(x) >= 2) mems <- c(mems, num1(x[[2]]))
  }
  cpus <- cpus[is.finite(cpus)]; mems <- mems[is.finite(mems)]
  list(cpu = if (length(cpus)) as.integer(max(cpus)) else NA_integer_,
       memGB = if (length(mems)) max(mems) / 1024 else NA_real_)
}

#' Size a priority-QOS job to the detected allowance (ADR 0031)
#'
#' Combines the QOS cap (\code{sacctmgr}) and the node size (\code{sinfo}) into the cores / memory /
#' walltime a single-node priority job should request — the largest values the allowance and one node
#' allow, so a \code{--priority} run finishes as fast as the allowance permits.
#'
#' @param qos QOS name. Default \code{"priority"}.
#' @param partition Preferred partition; overridden if it does not permit \code{qos} and another
#'   partition does. Default \code{"standard"}.
#' @param perCoreGB Memory budget per core when the QOS sets no memory cap. Default 4.
#' @param fallbackCores Cores to use when nothing can be detected (no SLURM tools). Default 16.
#' @param .qosRaw,.sinfoRaw,.scontrolRaw Optional raw command output, for testing.
#' @return List: \code{nCores}, \code{mem} (e.g. \code{"256G"}), \code{time} (SLURM walltime),
#'   \code{partition} (one that permits \code{qos}, when found), \code{partitionOk} (logical),
#'   \code{detail} (a human-readable summary of what was detected).
#' @export
#' @author Renato Rodrigues
prioritySizing <- function(qos = "priority", partition = "standard", perCoreGB = 4L,
                           fallbackCores = 16L, .qosRaw = NULL, .sinfoRaw = NULL,
                           .scontrolRaw = NULL) {
  # Never throw: a detection hiccup must not block the submission (it falls back instead).
  ql <- tryCatch(.parseQosLimits(qos, .raw = .qosRaw),
                 error = function(e) list(cpu = NA_integer_, mem = NA_character_, wall = NA_character_))
  # Pick a partition that actually permits the QOS (the QOS being in your association is not enough).
  parts <- tryCatch(.partitionsForQos(qos, .raw = .scontrolRaw), error = function(e) character(0))
  chosenPart <- if (length(parts)) {
    if (partition %in% parts) partition else if (qos %in% parts) qos else parts[[1]]  # prefer same-named partition
  } else partition
  ns <- tryCatch(.parseNodeSpecs(chosenPart, .raw = .sinfoRaw),
                 error = function(e) list(cpu = NA_integer_, memGB = NA_real_))
  cpuCaps <- c(ql$cpu, ns$cpu)
  cpuCaps <- cpuCaps[!is.na(cpuCaps) & is.finite(cpuCaps)]
  nCores <- if (length(cpuCaps)) as.integer(min(cpuCaps)) else as.integer(fallbackCores)
  mem <- if (!is.na(ql$mem)) ql$mem else {
    budget <- nCores * perCoreGB
    if (!is.na(ns$memGB)) budget <- min(budget, floor(ns$memGB * 0.95))  # leave headroom for the OS
    paste0(round(budget), "G")
  }
  time <- if (!is.na(ql$wall)) ql$wall else "24:00:00"
  list(nCores = nCores, mem = mem, time = time, partition = chosenPart,
       partitionOk = length(parts) > 0,
       detail = sprintf("QOS %s: cpu=%s mem=%s wall=%s | partition -> %s%s | node: cpu=%s memGB=%s",
                        qos, ql$cpu, ql$mem %||% "-", ql$wall %||% "-", chosenPart,
                        if (length(parts)) sprintf(" (allows %s: %s)", qos, paste(parts, collapse = ",")) else " (unverified)",
                        ns$cpu, if (is.na(ns$memGB)) "-" else round(ns$memGB)))
}
