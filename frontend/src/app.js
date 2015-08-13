'use strict'
/* @flow */

require(['jquery', 'api', 'ramda', 'webshim/polyfiller'], function(jquery, api_, _, webshim_) {
  webshim.polyfill('forms-ext');
  var log = console.log.bind(console)


  postLogin({newUserName:'max', password:'xam'}, log, ()=>1)
});
