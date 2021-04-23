#!/bin/bash
set -ex
npx shadow-cljs release test
test -f target/shadow-cljs/resources/public/js/karma.js
karma start test-environments/shadow-cljs-prod/karma.conf.js --single-run
