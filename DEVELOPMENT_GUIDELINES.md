# PowerClimate Vision Explorer — Development Guidelines

> These conventions apply to **all** development on this Shiny app.
> Any AI assistant or developer working on this project MUST read and follow them.

---

## 1. Mobile-First Readiness

Even though the primary target is desktop, **every UI change must be written so that
a future responsive pass is straightforward**. Specifically:

- **Use relative/fluid units** (`clamp()`, `%`, `vw/vh`, `min()`) instead of hard
  pixel widths wherever possible.
- **Structure CSS so `@media` breakpoints can be added cleanly** — avoid deeply nested
  fixed-width containers.
- **No hover-only interactions for core functionality** — any `:hover`-expand pattern
  (e.g., the layer control) must also have a tap/click toggle path via JS.
- **Touch targets ≥ 44px** for all interactive elements (buttons, close icons, sliders).
- **Use CSS custom properties for spacing/sizing** so a single breakpoint pass can
  scale everything proportionally.
- **Planned breakpoints** (not yet implemented, but design toward them):
  - `< 768px` — collapse left panel into hamburger or bottom-sheet
  - `< 768px` — stack right button column at screen bottom
  - `< 768px` — stats drawer gets taller (no hover, touch-based)
  - Font sizes should use `clamp()` for smooth scaling

## 2. Code Style — Written for Scientists, Not Software Engineers

This codebase is maintained by **climate scientists and researchers**, not professional
software developers. All code must be written so that a colleague with basic R/Shiny
experience can read, understand, and modify it without needing a computer science
background. Specifically:

- **Plain, linear, procedural R** — no R6/S4 classes, no object-oriented patterns,
  no functional programming abstractions (e.g., no `purrr::map` chains when a simple
  `for` loop is clearer). Write code the way you'd write an R analysis script.
- **Explicit over clever** — prefer verbose, obvious code over compact one-liners.
  If a piece of logic can be written in one dense line or five clear lines, choose
  the five clear lines. Other scientists should never have to puzzle over what a
  line does.
- **Descriptive variable names** — use names that reflect the domain (e.g.,
  `current_boundaries`, `clicked_region`, `polygon_opacity`) rather than generic
  programming names (e.g., `data`, `obj`, `val`, `tmp`).
- **Heavy commenting** — every observer, reactive, and render block must have a
  header comment block explaining **what it does and why**. Inline comments should
  explain non-obvious steps. Think of comments as lab notebook entries: a colleague
  picking up this code 6 months from now should understand the intent without
  reading the R/Shiny documentation.
- **No unnecessary abstractions** — do not create helper functions, utility modules,
  or wrapper layers unless they genuinely reduce duplication across 3+ places. A
  scientist reading the code should be able to follow it top-to-bottom without
  jumping between files or tracing call chains.
- **Vanilla CSS** — no Tailwind, no Sass. All styling in `www/styles.css`.
- **Design language**: dark glassmorphism with CSS custom properties (design tokens
  defined in `:root`).

## 3. UI/UX Principles

- **Desktop-first, mobile-aware** — primary audience uses wide screens, but the
  layout must not break on tablets/phones.
- **Climatology/research analyst aesthetic** — clean, minimalist, data-focused.
- **Dark glassmorphism** — `backdrop-filter: blur()`, semi-transparent navy backgrounds,
  subtle borders, `Inter` font family.
- **No slider ticks** — always set `ticks = FALSE` on `sliderInput()`.
- **Premium feel** — micro-animations, smooth transitions, curated color palette.

## 4. Spatial / Map

- **MapLibre GL** via `mapgl` R package.
- **Globe projection** enabled by default.
- **Custom zoom controls** (not built-in MapLibre nav widget) — glassmorphism button column.
- **Selection persistence** — `clicked_region()` must survive basemap style switches;
  only clear on spatial tier change or explicit drawer close.
- **Auto-zoom behaviors**:
  - Click polygon → `fit_bounds()` to polygon extent
  - Close drawer → `fit_bounds()` to full tier extent
  - Change tier → `fit_bounds()` to new tier extent
  - Home button → zoom to selected polygon (if any) or full tier

## 5. Data Sources

- **Copernicus PECD v4.2** spatial boundaries (GeoJSON in `www/data/geo/`)
- **Eurostat GISCO NUTS 2021** (NUT0, NUT2)
- **ENTSO-E** bidding zones (PEON, PEOF)
- **Study zones** (SZON, SZOF)
- Future: PECD CSV time-series data for charts in the bottom drawer

## 6. Future Plans

- Bottom drawer will expand to ~40vh to hold time-series charts and summary statistics
  when PECD CSV data is integrated.
- The app will eventually need to work on tablets for field presentations.
