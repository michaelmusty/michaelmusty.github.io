# michaelmusty.github.io

Personal academic/professional site for Michael Musty. Minimal Jekyll site —
GitHub Pages builds it on every push, no CI or client-side JavaScript.

## How it works

Content lists live in `_data/*.yml`. Pages loop over that data with Liquid and
render to plain static HTML server-side. To add a talk or paper, you edit YAML —
never HTML.

```
_config.yml            # site settings
_layouts/default.html  # shared page shell (nav, header, footer, MathJax)
assets/css/style.css   # all styling (light + dark)

index.html             # Home
bio.html               # Bio
research.html          # Research  → renders _data/research.yml
talks.html             # Talks     → renders _data/talks.yml
cv.html                # CV (live) → renders _data/cv.yml + research + talks

_data/research.yml     # publications
_data/talks.yml        # talks
_data/cv.yml           # CV: contact, employment, education, awards, skills
assets/cv/musty_cv.tex # downloadable CV source  →  musty_cv.pdf
```

The CV page reuses `research.yml` and `talks.yml` for its publications and talks
sections, so those lists have a single source of truth.

## Editing content

- **Add a paper:** append an entry to `_data/research.yml`
  (`title`, `authors`, `venue`, `year`, `description`, optional `status: in-progress`,
  `award`, and `links: [{name, url}]`).
- **Add a talk:** append to `_data/talks.yml` (`title`, `event`, `location`, `year`, `links`).
- **Update the CV:** edit `_data/cv.yml`. For the downloadable PDF, also edit
  `assets/cv/musty_cv.tex` and recompile (below).

## Rebuilding the CV PDF

```bash
cd assets/cv && latexmk -pdf musty_cv.tex   # then commit musty_cv.pdf
```

## Local preview

```bash
bundle install
bundle exec jekyll serve      # http://localhost:4000
```

Requires a Ruby that matches GitHub Pages (Ruby 3 needs the bundled `webrick`
gem, already in the Gemfile). If your local Ruby toolchain fights you, you can
skip local preview entirely — pushing to the default branch is enough, GitHub
builds and deploys it.
