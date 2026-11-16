// Profile details used across pages. Lists (papers, talks, jobs) live in the
// YAML files next to this one.
export const site = {
  name: 'Michael Musty',
  role: 'Data scientist and mathematician',
  description:
    'Michael Musty is a data scientist and machine learning engineer at Faraday, and a mathematician working in computational number theory.',
  location: 'Vermont, USA',
  email: 'michaelmusty@gmail.com',
  cvPdf: '/assets/cv/musty_cv.pdf',
  links: [
    { name: 'GitHub', url: 'https://github.com/michaelmusty' },
    { name: 'LinkedIn', url: 'https://www.linkedin.com/in/mjmusty/' },
    { name: 'arXiv', url: 'https://arxiv.org/search/math?searchtype=author&query=Musty%2C+M' },
  ],
  awards: [
    {
      name: 'Selfridge Prize in Number Theory',
      org: 'Number Theory Foundation, awarded at ANTS-XIII',
      year: 2018,
    },
  ],
  skills: [
    { group: 'Programming and tools', items: 'Python, R, Magma, SageMath, SQL, Git, LaTeX' },
    {
      group: 'Data science and mathematics',
      items: 'Machine learning, statistical modeling, computer vision, computational number theory',
    },
  ],
} as const;

export function years(start: number, end?: number, current = false): string {
  if (current) return `${start} – present`;
  if (end === undefined || end === start) return String(start);
  return `${start} – ${end}`;
}
