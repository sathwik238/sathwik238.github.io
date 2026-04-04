# Sathwik Bollepalli — Personal Portfolio

Live site: [sathwik238.github.io](https://sathwik238.github.io)

A dark-themed, fully responsive personal portfolio built with vanilla HTML, CSS, and JavaScript.

## Sections

| Section | Description |
|---|---|
| Hero | Animated particle canvas, glitch name effect, typewriter role |
| About | Bio, quick-facts |
| Skills | Skill bars by category + tech pills |
| Projects | Project cards with tags and links |
| Experience | Alternating timeline |
| Certificates | Cards linked to PDFs or external credentials |
| Hobbies | Icon grid |
| Contact | Social links + contact form |

## Tech Stack

- **HTML5 / CSS3 / Vanilla JS** — no frameworks or build tools
- **Font Awesome 6** — icons
- **Google Fonts** — Space Grotesk + Fira Code
- Hosted on **GitHub Pages**

## Responsive Breakpoints

| Breakpoint | Behavior |
|---|---|
| ≤ 900px | Single-column timeline, stacked about card, contact layout |
| ≤ 860px | Narrower grid min-widths for projects and certificates |
| ≤ 600px | Mobile nav drawer, single-column grids, stacked CTA buttons |
| ≤ 480px | Tighter container padding, compact cert cards |
| ≤ 380px | 2-column hobbies grid |

## Project Structure

```
├── index.html
├── css/
│   └── styles.css
├── js/
│   └── script.js
└── assets/
    ├── certificates/
    │   ├── certificate_links.json   # source of truth for cert URLs
    │   └── *.pdf
    └── resume/
        └── Sathwik_Bollepalli_Data_Scientist_Jan_2026.pdf
```

## Adding a Certificate

1. Drop the PDF into `assets/certificates/`
2. Add an entry to `assets/certificates/certificate_links.json`
3. Add a `<div class="cert-card reveal">` block in the certificates section of `index.html`
