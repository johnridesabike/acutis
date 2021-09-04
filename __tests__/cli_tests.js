/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const child_process = require("child_process");
const { promisify } = require("util");
const exec = promisify(child_process.exec);

test("sync", () =>
  exec(
    `./cli \
    fixtures/cli_test/layout.acutis \
    fixtures/cli_test/A.acutis \
    fixtures/cli_test/bb.js \
    fixtures/cli_test/subdir/C.acutis \
    fixtures/cli_test/D.mjs \
    --data=fixtures/cli_test/data.json
  `,
    {}
  ).then(({ stdout, stderr }) => {
    expect(stdout).toBe(
      `Carlo

B: CARLO

D: CARLO

<p>C: Carlo </p>
`
    );
    expect(stderr).toBe("");
  }));

test("async", () =>
  exec(
    `./cli \
    fixtures/cli_test/layout_async.acutis \
    fixtures/cli_test/A.acutis \
    fixtures/cli_test/bb_async.cjs \
    fixtures/cli_test/subdir/C.acutis \
    fixtures/cli_test/D_async.mjs \
    < fixtures/cli_test/data.json \
    --async
  `,
    {}
  ).then(({ stdout, stderr }) => {
    expect(stdout).toBe(
      `Carlo

B: CARLO

D: CARLO

<p>C: Carlo </p>
`
    );
    expect(stderr).toBe("");
  }));
