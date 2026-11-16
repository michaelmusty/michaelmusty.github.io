import { getCollection } from 'astro:content';

/** Newest first; within a year, papers before the thesis, then by title. */
export async function getPublications() {
  const entries = await getCollection('publications');
  return entries.sort(
    (a, b) =>
      b.data.year - a.data.year ||
      Number(a.data.kind === 'thesis') - Number(b.data.kind === 'thesis') ||
      a.data.title.localeCompare(b.data.title),
  );
}
