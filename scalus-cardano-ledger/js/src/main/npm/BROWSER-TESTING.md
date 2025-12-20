# Browser Testing for Scalus

This document describes how to run Scalus tests in a browser environment.

## Prerequisites

- Node.js installed
- npm dependencies installed (`npm install`)

## Build the Browser Test Bundle

First, build the shared tests bundle for browser use:

```bash
npm run build:browser-tests
```

This creates `shared-tests-bundle.js` which contains all the test logic.

## Run a Local Server

Start a local HTTP server using npx serve:

```bash
npx serve .
```

This will start a server at `http://localhost:3000` (or another port if 3000 is busy).

## Open the Test Page

Open your browser and navigate to:

```
http://localhost:3000/test-browser.html
```

## Running Tests

1. Click **"Run All Tests"** to run both the shared test suite and legacy manual tests
2. Click **"Run Shared Tests Only"** to run only the TypeScript test suite
3. Click **"Run Legacy Tests Only"** to run the original manual tests

## Test Results

- Green entries indicate passing tests
- Red entries indicate failing tests
- Open the browser console (F12) for detailed output and debugging information

## Rebuilding After Changes

If you modify the test files in `__tests__/`, rebuild the browser bundle:

```bash
npm run build:browser-tests
```

Then refresh the browser page to run the updated tests.
