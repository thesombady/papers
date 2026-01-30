# Revision history for papers2

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2026-01-27

Changed:
- Added info command to extract information about a query. Allows for information
about a specific entry.
- Added trimming to remove oddly formatted titles and authors. Splitting on newlines,
tabs, and multiple spaces. Easier to read output for `papers list`.

## 0.1.2.0 -- 2026-01-27

Added:
- Added `--doi` option for adding entries, such that one can utilize
  `papers add somepdf.pdf --doi 10.2013/xxxxx`

## 0.1.3.0 -- 2026-01-29

Added:
- Added extract filters, possibility of extracting per project or everything.
- Added list filters (author and project).

## 0.1.4.0 -- 2026-01-29

Formatted the `list` command to include keywords.

## 0.1.5.0 -- 2026-01-30

Fixed a logical error where the key was not renamed in the bib entry when renaming
an entry in the library.

Added:
- Alias get -> extract
- Alias mv  -> rename
- Alias rm  -> remove
