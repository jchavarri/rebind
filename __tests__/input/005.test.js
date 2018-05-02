const nano = require('nano')('http://localhost:5984');
nano.db.test(23);
const blue = require('blue');
const go = blue.pill.jump(23);