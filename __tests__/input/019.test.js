const BN = require('bn.js');

var a = new BN('dead', 16);
var b = new BN('101010', 2);

var res = a.add(b);

import * as stripe from 'stripe-js';
stripe.createToken('bank_account');