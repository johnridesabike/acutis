/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

// We don't want to store snapshots in the lib directory.
// https://jestjs.io/docs/en/configuration#snapshotresolver-string

const path = require("path");
const oldPath = path.join("lib", "js", "__tests__");
const newPath = path.join("fixtures", "__snapshots__");

module.exports = {
  // resolves from test to snapshot path
  resolveSnapshotPath: (testPath, snapshotExtension) =>
    testPath.replace(oldPath, newPath) + snapshotExtension,

  // resolves from snapshot to test path
  resolveTestPath: (snapshotFilePath, snapshotExtension) =>
    snapshotFilePath
      .replace(newPath, oldPath)
      .slice(0, -snapshotExtension.length),

  // Example test path, used for preflight consistency check of the implementation above
  testPathForConsistencyCheck: path.join(
    "lib",
    "js",
    "__tests__",
    "example.test.js"
  ),
};
