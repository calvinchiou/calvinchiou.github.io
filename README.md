# calvinchiou.github.io

Personal academic website for Calvin J. Chiou, Assistant Professor of Finance at National Chengchi University. Built with [Quarto](https://quarto.org/) and R, published to GitHub Pages.

Live site: https://calvinchiou.github.io

## Structure

- `index.qmd`, `about.qmd`, `research.qmd`, `teaching.qmd`, `resources.qmd`, `investments.qmd`, `recommendation.qmd`, `mocc.qmd` — top-level pages
- `papers/` — working paper / publication pages
- `wps/` — additional working papers
- `mocc/` — course materials (weekly content)
- `workinprogress/` — in-progress research pages
- `posts_hidden/` — draft or unpublished blog posts
- `notes/` — notes content
- `R code/`, `myapp/` — supporting R scripts and Shiny app(s)
- `data/` — data files used by pages/dashboards
- `img/`, `files/` — images and downloadable files (e.g., CV)
- `_extensions/` — Quarto extensions
- `docs/` — rendered site output (publish directory)
- `_quarto.yml` — site config (nav, theme, footer, analytics)
- `_variables.yml` — shared template variables
- `sandstone.scss`, `theme-light.scss`, `theme-dark.scss` — theme styling
- `io_exposures_dashboard.qmd` — interactive dashboard page

## Development

Requires [R](https://www.r-project.org/) and [Quarto](https://quarto.org/docs/get-started/).

Render and preview locally:

```bash
quarto preview
```

Build the full site (outputs to `docs/`, per `_quarto.yml`):

```bash
quarto render
```

## Deployment

The `docs/` folder is the GitHub Pages publish source. Render locally with `quarto render`, then commit and push `docs/` along with source changes.

## License

Content licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
