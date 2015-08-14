'use strict'
/* @flow */
declare var $: any;

var getTaskById = function(id, headerAuthorization)
{
  return $.ajax(
    { url: '/task/' + encodeURIComponent(id) + ''
    , headers: { "Authorization": headerAuthorization }
    , type: 'GET'
    });
}

var postTask = function(body, headerAuthorization)
{
  return $.ajax(
    { url: '/task'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , type: 'POST'
    });
}

var putTaskById = function(id, body, headerAuthorization)
{
  return $.ajax(
    { url: '/task/' + encodeURIComponent(id) + ''
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , type: 'PUT'
    });
}

var deleteTaskById = function(id, headerAuthorization)
{
  return $.ajax(
    { url: '/task/' + encodeURIComponent(id) + ''
    , headers: { "Authorization": headerAuthorization }
    , type: 'DELETE'
    });
}

var getItemById = function(id, headerAuthorization)
{
  return $.ajax(
    { url: '/item/' + encodeURIComponent(id) + ''
    , headers: { "Authorization": headerAuthorization }
    , type: 'GET'
    });
}

var postItem = function(body, headerAuthorization)
{
  return $.ajax(
    { url: '/item'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , type: 'POST'
    });
}

var putItemById = function(id, body, headerAuthorization)
{
  return $.ajax(
    { url: '/item/' + encodeURIComponent(id) + ''
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , type: 'PUT'
    });
}

var deleteItemById = function(id, headerAuthorization)
{
  return $.ajax(
    { url: '/item/' + encodeURIComponent(id) + ''
    , headers: { "Authorization": headerAuthorization }
    , type: 'DELETE'
    });
}

var getPreferredHours = function(headerAuthorization)
{
  return $.ajax(
    { url: '/preferred-hours'
    , headers: { "Authorization": headerAuthorization }
    , type: 'GET'
    });
}

var postPreferredHours = function(body, headerAuthorization)
{
  return $.ajax(
    { url: '/preferred-hours'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , type: 'POST'
    });
}

var putPreferredHours = function(body, headerAuthorization)
{
  return $.ajax(
    { url: '/preferred-hours'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , type: 'PUT'
    });
}

var deletePreferredHours = function(headerAuthorization)
{
  return $.ajax(
    { url: '/preferred-hours'
    , headers: { "Authorization": headerAuthorization }
    , type: 'DELETE'
    });
}

var getProfile = function(headerAuthorization)
{
  return $.ajax(
    { url: '/profile'
    , headers: { "Authorization": headerAuthorization }
    , type: 'GET'
    });
}

var putProfile = function(body, headerAuthorization)
{
  return $.ajax(
    { url: '/profile'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , type: 'PUT'
    });
}

var deleteProfile = function(headerAuthorization)
{
  return $.ajax(
    { url: '/profile'
    , headers: { "Authorization": headerAuthorization }
    , type: 'DELETE'
    });
}

var getUserById = function(id, headerAuthorization)
{
  return $.ajax(
    { url: '/user/' + encodeURIComponent(id) + ''
    , headers: { "Authorization": headerAuthorization }
    , type: 'GET'
    });
}

var postUser = function(body, headerAuthorization)
{
  return $.ajax(
    { url: '/user'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , type: 'POST'
    });
}

var putUserById = function(id, body, headerAuthorization)
{
  return $.ajax(
    { url: '/user/' + encodeURIComponent(id) + ''
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , type: 'PUT'
    });
}

var deleteUserById = function(id, headerAuthorization)
{
  return $.ajax(
    { url: '/user/' + encodeURIComponent(id) + ''
    , headers: { "Authorization": headerAuthorization }
    , type: 'DELETE'
    });
}

var getTasks = function(headerAuthorization)
{
  return $.ajax(
    { url: '/tasks'
    , headers: { "Authorization": headerAuthorization }
    , type: 'GET'
    });
}

var getItems = function(from, to, headerAuthorization)
{
  return $.ajax(
    { url: '/items' + '?from=' + encodeURIComponent(from) + '&to=' + encodeURIComponent(to)
    , headers: { "Authorization": headerAuthorization }
    , type: 'GET'
    });
}

var getUsers = function(headerAuthorization)
{
  return $.ajax(
    { url: '/users'
    , headers: { "Authorization": headerAuthorization }
    , type: 'GET'
    });
}

var postLogout = function(headerAuthorization)
{
  return $.ajax(
    { url: '/logout'
    , headers: { "Authorization": headerAuthorization }
    , type: 'POST'
    });
}

var postRegistration = function(body)
{
  return $.ajax(
    { url: '/registration'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , type: 'POST'
    });
}

var postLogin = function(body)
{
  return $.ajax(
    { url: '/login'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , type: 'POST'
    });
}
