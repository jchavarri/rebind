const nano = require('nano')('http://localhost:5984');
nano.db.create('foo', (err, body, headers) => { // ... stuff here
})