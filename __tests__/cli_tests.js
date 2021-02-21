/**
 *    Copyright 2021 John Jackson
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
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
