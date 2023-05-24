

var hello = require('./hello.js');

var names = ['harry', 'barry', 'garry', 'harry', 'barry', 'marry'];

var names2 = [
  'harry',
  'barry',
  'garry',
  'harry',
  'barry',
  'marry',
];

// after this line new chunk will be created
var names3 = [
  'harry',
  'harry',
  'barry',
  'marry', 'garry',
];
