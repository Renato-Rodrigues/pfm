# The fitted panel must be found in every layout, above all in a Fit Cache that is NOT the
# Results Root (output/pfm/<group> + output/pfm/fit-cache). Before 2026-10-01 the lookup only
# looked beside the Run-Group, and every step silently rebuilt the panel from madrat.

test_that("the fitted panel is found in a separate Fit Cache, and loaded rather than rebuilt", {
  root <- file.path(tempdir(), "panel-layout")
  unlink(root, recursive = TRUE)
  g <- file.path(root, "output", "pfm", "vtest")
  dir.create(g, recursive = TRUE)
  dir.create(file.path(root, "output", "pfm", "fit-cache", "panels"), recursive = TRUE)
  jsonlite::write_json(list(panel_hash = "abc123"), file.path(g, "manifest.json"), auto_unbox = TRUE)
  pan <- magclass::new.magpie(c("DEU", "FRA"), 2000:2001, "x", fill = 1)
  saveRDS(pan, file.path(root, "output", "pfm", "fit-cache", "panels", "panel_abc123.rds"))

  cand <- pfm:::.psmPanelCandidates(g, "abc123", modelDir = NULL)
  expect_true(any(file.exists(cand)))
  expect_equal(normalizePath(cand[file.exists(cand)][1]),
               normalizePath(file.path(root, "output", "pfm", "fit-cache", "panels", "panel_abc123.rds")))

  got <- expect_no_warning(pfm:::.psmHistPanel(g, verbose = FALSE))
  expect_equal(magclass::getItems(got, dim = 1), c("DEU", "FRA"))
})

test_that("the configured modelDir comes first, and the old layout still works", {
  root <- file.path(tempdir(), "panel-layout-old")
  unlink(root, recursive = TRUE)
  g <- file.path(root, "output", "v1")
  dir.create(file.path(root, "output", "panels"), recursive = TRUE)
  dir.create(g)
  saveRDS(1, file.path(root, "output", "panels", "panel_h.rds"))
  cand <- pfm:::.psmPanelCandidates(g, "h", modelDir = file.path(root, "store"))
  expect_equal(cand[1], file.path(root, "store", "panels", "panel_h.rds"))
  expect_true(file.exists(cand[file.exists(cand)][1]))
})
