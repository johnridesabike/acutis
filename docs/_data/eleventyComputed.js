/**
  Copyright (c) 2022 John Jackson

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module.exports = {
  /* Wrap page data in a tagged union so we can safely handle unpublished
   * pages. */
  published: (data) => {
    if (data.page.url) {
      return {
        tag: true,
        page: data.page
      }
    } else {
      return {
        tag: false
      }
    }
  }
}
