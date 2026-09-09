#!/usr/bin/env Rscript
## Verifies that a roxygen2-based NAMESPACE/man/ regeneration has not silently
## dropped exported symbols or documentation content compared to a pre-migration
## snapshot. Intended to be run repeatedly while migrating the `baseline`
## package from hand-written Rd files to roxygen2.
##
## Usage:
##   Rscript tools/verify_roxygen_migration.R <snapshot_dir> <pkg_dir>
##
## <snapshot_dir> must contain, captured *before* the migration:
##   exports_old_public_full.txt  - one exported name (function/generic/class) per line
##   man_old/                     - copy of the original hand-written man/*.Rd files
##
## <pkg_dir> is the package root containing the regenerated NAMESPACE and man/.

args <- commandArgs(trailingOnly = TRUE)
snapshot_dir <- if (length(args) >= 1) args[[1]] else "/tmp/baseline_migration"
pkg_dir      <- if (length(args) >= 2) args[[2]] else "."

fail <- FALSE
note <- function(...) cat(sprintf(...), "\n")
problem <- function(...) { fail <<- TRUE; cat("PROBLEM: ", sprintf(...), "\n", sep = "") }

## 1. Exported-symbol diff -----------------------------------------------------
namespace_file <- file.path(pkg_dir, "NAMESPACE")
ns_lines <- readLines(namespace_file)
extract <- function(tag) {
  m <- regmatches(ns_lines, regexec(paste0("^", tag, "\\((.*)\\)$"), ns_lines))
  unlist(lapply(m, function(x) if (length(x) == 2) x[[2]] else NULL))
}
new_exports <- sort(unique(unlist(strsplit(c(
  extract("export"), extract("exportClasses"), extract("exportMethods")
), ",\\s*"))))

old_exports_file <- file.path(snapshot_dir, "exports_old_public_full.txt")
old_exports <- sort(unique(readLines(old_exports_file)))

missing <- setdiff(old_exports, new_exports)
extra   <- setdiff(new_exports, old_exports)
if (length(missing)) problem("Exported before, missing now: %s", paste(missing, collapse = ", "))
if (length(extra))   note("Newly exported (not in old snapshot): %s", paste(extra, collapse = ", "))
if (!length(missing)) note("Export check: OK (%d symbols matched)", length(old_exports))

## 2. Rd topic / alias diff ----------------------------------------------------
man_old_dir <- file.path(snapshot_dir, "man_old")
man_new_dir <- file.path(pkg_dir, "man")

read_aliases <- function(rd_path) {
  rd <- tools::parse_Rd(rd_path)
  tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
  unlist(lapply(rd[tags == "\\alias"], function(x) as.character(x[[1]])))
}
rd_section <- function(rd_path, section_tag) {
  rd <- tools::parse_Rd(rd_path)
  tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
  any(tags == section_tag)
}

old_rd_files <- list.files(man_old_dir, pattern = "\\.Rd$", full.names = TRUE)
new_rd_files <- list.files(man_new_dir, pattern = "\\.Rd$", full.names = TRUE)

alias_to_file <- function(files) {
  out <- list()
  for (f in files) {
    for (a in read_aliases(f)) out[[a]] <- f
  }
  out
}
old_alias_map <- alias_to_file(old_rd_files)
new_alias_map <- alias_to_file(new_rd_files)

dropped_aliases <- setdiff(names(old_alias_map), names(new_alias_map))
if (length(dropped_aliases)) {
  problem("\\alias{} entries present before, missing now: %s", paste(dropped_aliases, collapse = ", "))
} else {
  note("Alias check: OK (%d aliases matched)", length(old_alias_map))
}

## 3. Slots/Methods sections on *-class topics ---------------------------------
old_class_files <- old_rd_files[grepl("-class\\.Rd$", old_rd_files)]
for (f in old_class_files) {
  has_slots   <- rd_section(f, "\\section") && any(grepl("Slots", readLines(f)))
  has_methods <- any(grepl("\\\\section\\{Methods\\}", readLines(f)))
  if (!has_slots && !has_methods) next
  ## Find corresponding new topic via shared alias
  aliases <- read_aliases(f)
  new_f <- unique(unlist(new_alias_map[aliases]))
  new_f <- new_f[!is.na(new_f)]
  if (!length(new_f)) next  # already reported as dropped alias above
  new_f <- new_f[[1]]
  new_lines <- readLines(new_f)
  if (has_slots && !any(grepl("\\\\section\\{Slots\\}", new_lines))) {
    problem("Old topic '%s' had a Slots section; regenerated '%s' does not", basename(f), basename(new_f))
  }
  if (has_methods && !any(grepl("\\\\section\\{Methods\\}", new_lines))) {
    problem("Old topic '%s' had a Methods section; regenerated '%s' does not", basename(f), basename(new_f))
  }
}

if (fail) {
  cat("\nVerification FAILED - see PROBLEM lines above.\n")
  quit(status = 1)
} else {
  cat("\nVerification PASSED.\n")
  quit(status = 0)
}
