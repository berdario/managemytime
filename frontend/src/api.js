'use strict'
/* @flow */

var getTaskById = function(id, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/task/' + encodeURIComponent(id) + ''
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'GET'
    });
}

var postTask = function(body, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/task'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'POST'
    });
}

var putTaskById = function(id, body, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/task/' + encodeURIComponent(id) + ''
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'PUT'
    });
}

var deleteTaskById = function(id, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/task/' + encodeURIComponent(id) + ''
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'DELETE'
    });
}

var getItemById = function(id, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/item/' + encodeURIComponent(id) + ''
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'GET'
    });
}

var postItem = function(body, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/item'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'POST'
    });
}

var putItemById = function(id, body, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/item/' + encodeURIComponent(id) + ''
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'PUT'
    });
}

var deleteItemById = function(id, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/item/' + encodeURIComponent(id) + ''
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'DELETE'
    });
}

var getPreferredHours = function(headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/preferred-hours'
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'GET'
    });
}

var postPreferredHours = function(body, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/preferred-hours'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'POST'
    });
}

var putPreferredHours = function(body, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/preferred-hours'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'PUT'
    });
}

var deletePreferredHours = function(headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/preferred-hours'
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'DELETE'
    });
}

var getProfile = function(headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/profile'
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'GET'
    });
}

var putProfile = function(body, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/profile'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'PUT'
    });
}

var deleteProfile = function(headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/profile'
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'DELETE'
    });
}

var getUserById = function(id, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/user/' + encodeURIComponent(id) + ''
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'GET'
    });
}

var postUser = function(body, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/user'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'POST'
    });
}

var putUserById = function(id, body, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/user/' + encodeURIComponent(id) + ''
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'PUT'
    });
}

var deleteUserById = function(id, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/user/' + encodeURIComponent(id) + ''
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'DELETE'
    });
}

var getTasks = function(headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/tasks'
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'GET'
    });
}

var getItems = function(from, to, headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/items' + '?from=' + encodeURIComponent(from) + '&to=' + encodeURIComponent(to)
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'GET'
    });
}

var getUsers = function(headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/users'
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'GET'
    });
}

var postLogout = function(headerAuthorization, onSuccess, onError)
{
  $.ajax(
    { url: '/logout'
    , success: onSuccess
    , headers: { "Authorization": headerAuthorization }
    , error: onError
    , type: 'POST'
    });
}

var postRegistration = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/registration'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var postLogin = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/login'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}
