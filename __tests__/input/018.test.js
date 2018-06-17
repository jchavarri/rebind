var stripe = require("stripe");
stripe.createToken('bank_account').then(function(result) {});