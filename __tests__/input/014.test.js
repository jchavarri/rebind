// Example from https://reasonml.chat/t/how-to-improve-my-codes/663
import Keycloak from 'keycloak-js';
const keycloak = Keycloak('config/keycloak.json');
keycloak.init({ onLoad: 'login-required' }).success(function(authenticated) {
  alert(authenticated ? 'authenticated' : 'not authenticated');
}).error(function() {
  alert('failed to initialize');
});