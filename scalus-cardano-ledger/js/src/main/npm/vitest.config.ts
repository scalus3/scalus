import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    // .js too: the plain-JavaScript tests check that Scala.js export coercion cannot
    // bypass contracts TypeScript would have caught at compile time.
    include: ['__tests__/**/*.test.{ts,js}'],
  },
});
