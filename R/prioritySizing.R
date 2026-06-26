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

# Internal: parse `sinfo -h -o "%c %m" -p <partition>` (cores, mem-MB per node); take the largest node.
#' @keywords internal
.parseNodeSpecs <- function(partition, .raw = NULL) {
  raw <- if (!is.null(.raw)) .raw else tryCatch(
    suppressWarnings(system2("sinfo", c("-h", "-o", "%c %m", "-p", partition),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0))
  raw <- raw[nzchar(raw)]
  if (!length(raw)) return(list(cpu = NA_integer_, memGB = NA_real_))
  parts <- strsplit(trimws(raw), "[[:space:]]+")
  cpu <- suppressWarnings(max(vapply(parts, function(x) as.integer(gsub("\\D.*", "", x[[1]])), integer(1)), na.rm = TRUE))
  memMB <- suppressWarnings(max(vapply(parts, function(x) as.numeric(gsub("\\D.*", "", x[[2]])), numeric(1)), na.rm = TRUE))
  list(cpu = if (is.finite(cpu)) cpu else NA_integer_,
       memGB = if (is.finite(memMB)) memMB / 1024 else NA_real_)
}

#' Size a priority-QOS job to the detected allowance (ADR 0031)
#'
#' Combines the QOS cap (\code{sacctmgr}) and the node size (\code{sinfo}) into the cores / memory /
#' walltime a single-node priority job should request — the largest values the allowance and one node
#' allow, so a \code{--priority} run finishes as fast as the allowance permits.
#'
#' @param qos QOS name. Default \code{"priority"}.
#' @param partition Partition to size nodes from. Default \code{"standard"}.
#' @param perCoreGB Memory budget per core when the QOS sets no memory cap. Default 4.
#' @param fallbackCores Cores to use when nothing can be detected (no SLURM tools). Default 16.
#' @param .qosRaw,.sinfoRaw Optional raw command output, for testing.
#' @return List: \code{nCores}, \code{mem} (e.g. \code{"256G"}), \code{time} (SLURM walltime),
#'   \code{detail} (a human-readable summary of what was detected).
#' @export
#' @author Renato Rodrigues
prioritySizing <- function(qos = "priority", partition = "standard", perCoreGB = 4L,
                           fallbackCores = 16L, .qosRaw = NULL, .sinfoRaw = NULL) {
  ql <- .parseQosLimits(qos, .raw = .qosRaw)
  ns <- .parseNodeSpecs(partition, .raw = .sinfoRaw)
  cpuCaps <- c(ql$cpu, ns$cpu)
  cpuCaps <- cpuCaps[!is.na(cpuCaps) & is.finite(cpuCaps)]
  nCores <- if (length(cpuCaps)) as.integer(min(cpuCaps)) else as.integer(fallbackCores)
  mem <- if (!is.na(ql$mem)) ql$mem else {
    budget <- nCores * perCoreGB
    if (!is.na(ns$memGB)) budget <- min(budget, floor(ns$memGB * 0.95))  # leave headroom for the OS
    paste0(round(budget), "G")
  }
  time <- if (!is.na(ql$wall)) ql$wall else "24:00:00"
  list(nCores = nCores, mem = mem, time = time,
       detail = sprintf("QOS %s: cpu=%s mem=%s wall=%s | node(%s): cpu=%s memGB=%s",
                        qos, ql$cpu, ql$mem %||% "-", ql$wall %||% "-",
                        partition, ns$cpu, if (is.na(ns$memGB)) "-" else round(ns$memGB)))
}
