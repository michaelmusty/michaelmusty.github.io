import { defineCollection } from 'astro:content';
import { file } from 'astro/loaders';
import { z } from 'astro/zod';

const link = z.object({ name: z.string(), url: z.string() });

const publications = defineCollection({
  loader: file('src/data/publications.yaml'),
  schema: z.object({
    title: z.string(),
    kind: z.enum(['paper', 'thesis']),
    authors: z.string().optional(),
    venue: z.string(),
    year: z.number().int(),
    summary: z.string(),
    award: z.string().optional(),
    links: z.array(link).default([]),
  }),
});

const projects = defineCollection({
  loader: file('src/data/projects.yaml'),
  schema: z.object({
    title: z.string(),
    order: z.number().int(),
    meta: z.string().optional(),
    summary: z.string(),
    links: z.array(link).default([]),
  }),
});

const talks = defineCollection({
  loader: file('src/data/talks.yaml'),
  schema: z.object({
    title: z.string(),
    event: z.string(),
    location: z.string().optional(),
    year: z.number().int(),
    links: z.array(link).default([]),
  }),
});

const experience = defineCollection({
  loader: file('src/data/experience.yaml'),
  schema: z.object({
    role: z.string(),
    org: z.string(),
    location: z.string().optional(),
    start: z.number().int(),
    end: z.number().int().optional(), // omit for "present"
    current: z.boolean().default(false),
    summary: z.string().optional(),
  }),
});

const education = defineCollection({
  loader: file('src/data/education.yaml'),
  schema: z.object({
    degree: z.string(),
    org: z.string(),
    location: z.string().optional(),
    year: z.number().int(),
    detail: z.string().optional(),
  }),
});

export const collections = { publications, projects, talks, experience, education };
