# michaelmusty.github.io

Personal site for Michael Musty, live at <https://michaelmusty.github.io>.

Built with [Astro](https://astro.build) as a fully static site: no client-side
JavaScript, self-hosted fonts, optimized images. GitHub Actions builds and
deploys it to GitHub Pages on every push to `master`.

## Editing content

Almost everything on the site comes from the files in `src/data/`:

| File                | What it holds                                          |
| ------------------- | ------------------------------------------------------ |
| `publications.yaml` | Papers and thesis (Home, Research, CV)                 |
| `projects.yaml`     | Software and other work (Research)                     |
| `talks.yaml`        | Talks (Research, CV)                                   |
| `experience.yaml`   | Jobs (Home shows the latest four, CV shows all)        |
| `education.yaml`    | Degrees (CV)                                           |
| `site.ts`           | Name, email, profile links, awards, skills             |

Entries are validated against the schemas in `src/content.config.ts`, so a
typo in a field name fails the build instead of silently dropping content.

The intro paragraphs are in `src/pages/index.astro` and
`src/pages/research.astro`.

Files in `public/` are served as-is. PDFs and scripts live in `public/assets/`,
at the same URLs the old site used.

## CV PDF

The downloadable CV is `public/assets/cv/musty_cv.pdf`, built from
`cv/musty_cv.tex`. It is separate from the CV page, so update both:

```bash
cd cv && latexmk -pdf musty_cv.tex && cp musty_cv.pdf ../public/assets/cv/
```

## Local development

Requires Node 22.23 or newer (`.nvmrc` pins 26).

```bash
npm install
npm run dev      # http://localhost:4321, live reload
npm run build    # type-check and build to dist/
npm run preview  # serve the built site
```

## Deployment

`.github/workflows/deploy.yml` builds on every push and pull request, and
deploys when the push is to `master`. The repository's Pages source must be
set to **GitHub Actions** (Settings → Pages → Build and deployment).
