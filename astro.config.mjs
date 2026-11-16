import { defineConfig, fontProviders } from 'astro/config';
import sitemap from '@astrojs/sitemap';

export default defineConfig({
  site: 'https://michaelmusty.github.io',
  trailingSlash: 'always',
  integrations: [sitemap()],
  // The pre-2026 site had a single /projects/ page.
  redirects: { '/projects/': '/research/' },
  fonts: [
    {
      provider: fontProviders.fontsource(),
      name: 'Inter',
      cssVariable: '--font-inter',
      weights: ['400 700'],
      styles: ['normal', 'italic'],
      fallbacks: ['system-ui', 'sans-serif'],
    },
    {
      provider: fontProviders.fontsource(),
      name: 'Source Serif 4',
      cssVariable: '--font-source-serif',
      weights: ['400 700'],
      styles: ['normal', 'italic'],
      fallbacks: ['Georgia', 'serif'],
    },
  ],
});
