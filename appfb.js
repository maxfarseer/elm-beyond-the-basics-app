'use strict';
// Initialize Firebase
var config = {
  apiKey: 'AIzaSyBaEIHRQnhVL6-nEqr3DcC32O2ily-BRgw',
  authDomain: 'waitlist-3ec8b.firebaseapp.com',
  databaseURL: 'https://waitlist-3ec8b.firebaseio.com',
  storageBucket: ''
};

//alert('Add firebase config, see README.md! (then remove this)');
var app = firebase.initializeApp(config);
var database = app.database();
var CUSTOMERREFPATH = 'customers';

function addCustomer(customer) {
  var promise = database.ref(CUSTOMERREFPATH).push(customer);
  return promise;
}

function updateCustomer(customer) {
  var id = customer.id;
  var promise = database.ref(CUSTOMERREFPATH + '/' + id).set(customer);
  return promise;
}

function deleteCustomer(customer) {
  var id = customer.id;
  var promise = database.ref(CUSTOMERREFPATH + '/' + id).remove();
  return promise;
}

function customerListener() {
  return database.ref(CUSTOMERREFPATH);
}
