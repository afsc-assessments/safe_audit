# Safe_audit

Static audit site and source files for North Pacific SAFE/assessment link auditing.

Published site entrypoint:

- `index.html`

Main source files:

- `safe_audit.qmd`
- `npfmcSAFE.R`
- `outputs/safe_catalog.csv`
- `outputs/safe_resolved_noaa_terminal_links.csv`

Build locally:

```bash
quarto render safe_audit.qmd
cp safe_audit.html index.html
touch .nojekyll
```

The rendered site is designed to be served directly by GitHub Pages from the repository root.
