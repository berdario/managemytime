'use strict'
/* @flow */

declare var define : any;

var token = null

define(['api', 'jquery', 'ramda'], function(api_, jquery, _) {
  var log = console.log.bind(console)
  require(['bootstrap'], function(bootstrap_){

  })

  var groupByDay = _.groupBy(_.prop('day'))
  var groupByUser = _.groupBy(_.prop('userId'))
  function indexByDay (allItems, items){
    var alldays = _.sort((x,y)=>x>y, _.uniq(_.map(_.prop('day'), allItems)))
    return _.fromPairs(_.values(_.map(
                           (day)=>[day, groupByDay(items)[day]||[]], alldays)))
  }

  function indexByUser(users, items){
    return _.values(_.mapObjIndexed(
                             (user, k)=>[user.username, indexByDay(items, groupByUser(items)[k]||[])], users))
  }
  function getPrefHours(users, username){
    return _.find((u)=>u.username==username, _.values(users)).prefHours
  }
  var getDayTotal = _.compose(_.sum, _.map(_.prop('duration')))


  function summaryTable(allTasks, users, table, token){

    var tbl = document.createElement('table')
    tbl.id = 'summaryTable'
    tbl.className += 'table'
    var tr = tbl.insertRow()
    // add table header
    for (var keypair of [['',null]].concat(_.toPairs(table[0][1]))){
      var [day,] = keypair
      var th = document.createElement('th')
      th.appendChild(document.createTextNode(day))
      tr.appendChild(th)
    }

    for (var keypair of table){
      var tr = tbl.insertRow()
      var [username, daysItems] = keypair
      var pref = getPrefHours(users, username)
      var td = tr.insertCell()
      td.appendChild(document.createTextNode(username))

      for (var dayItems of _.values(daysItems)){
        var total = getDayTotal(dayItems)
        td = tr.insertCell()
        var p = document.createElement('p')
        p.appendChild(document.createTextNode(`total: ${total} Hours`))
        td.appendChild(p)
        for (var item of dayItems){
          var p = document.createElement('p')
          p.appendChild(document.createTextNode(`${allTasks[item.taskId]}: ${item.duration} Hours`))
          p.timeItemId = item.id
          p.onclick = makeItemEditable(token)
          td.appendChild(p)
        }
        if (pref !== null){
          td.className += pref<=total?'target_reached':'short_for_target'
        }
      }
    }
    var divtbl = document.createElement('div')
    divtbl.appendChild(tbl)
    divtbl.className += 'bootstrap-table'
    return divtbl
  }

  function authenticatedApp(username, token){
    var today = (new Date).toISOString().substr(0,10)
    var isNormalUser = null

    var idef = getItems(today, today, token)
    var tdef = getTasks(token)
    var udef = getUsers(token).then(null, ()=>{isNormalUser=true; return $.Deferred().resolve()})
    $.when(idef, tdef, udef).done(
      (itemsResp, tasksResp, usersResp)=>{
      var [mytasks, othersTasks] = tasksResp[0]
      var items = itemsResp[0]
      var allTasks = _.merge(mytasks, othersTasks)

      document.querySelector('#loginModal').style.display = 'none';
      if (usersResp){
        document.getElementById('dateFilterForm').onsubmit = filteredManagerSummary(token)
        var users = usersResp[0]
        var table = indexByUser(users, items)
        var summary = summaryTable(allTasks, users, table, token)

        //nodeResetter('content', '')
        document.getElementById('main').appendChild(summary)
        $('#mainAdmin').show()
        document.getElementById('createUser').onsubmit = createUser(token)
      } else {
        document.getElementById('dateFilterForm').onsubmit = filteredUserSummary(token, username)
        getPreferredHours(token).done((hours)=>{
          var users = [{username: username, prefHours: hours}]
          var table = [[username, indexByDay(items, items)]]
          var summary = summaryTable(allTasks, users, table, token)
          document.getElementById('main').appendChild(summary)
        })
      }
      $('#mainForms').show()
      document.getElementById('from').value = today
      document.getElementById('to').value = today
      document.getElementById('hoursForm').onsubmit = updateHours(token)
      document.getElementById('clearHours').onclick = unsetHours(token)
      document.getElementById('newTaskForm').onsubmit = createTask(token)
      document.getElementById('initNewItemForm').onclick = initNewItemForm(token)
    }
    )
  }

  var login = function (event){
    var [u, p] = [event.target.inputUsername.value, event.target.inputPassword.value]
    postLogin({newUserName:u, password:p}).done((tkn)=>{
      token = tkn;
      authenticatedApp(u, tkn)
    })
    return false
  }
  document.getElementById('loginForm').onsubmit = login

  var register = function(event){
    var [u, p, pconfirm] = [event.target.inputUsername.value
                          , event.target.inputPassword.value
                          , event.target.inputPasswordConfirm.value]
    if (p != pconfirm) {
      alert('password and its confirmation differ')
    } else {
      postRegistration({newUserName:u, password:p}).fail(()=>
        alert('registration failed')
      )
    }
    return false
  }
  document.getElementById('registerForm').onsubmit = register

  var updateHours = _.curry(function(token, event){
    var hour = event.target.hours.value
    postPreferredHours(parseInt(hour), token)
    return false
  })

  var unsetHours = _.curry(function(token, event){
    deletePreferredHours(token)
    return false
  })

  var filteredUserSummary = _.curry(function(token, username, event){
    var [from, to] = [event.target.from.value, event.target.to.value]
    var itemsdef = getItems(from, to, token)
    var tasksdef = getTasks(token)
    var hoursdef = getPreferredHours(token)
    $.when(itemsdef, tasksdef, hoursdef).done((itemsResp, tasksResp, hours)=>{
      var items = itemsResp[0]
      var [mytasks, othersTasks] = tasksResp[0]
      var allTasks = _.merge(mytasks, othersTasks)
      var users = [{username: username, prefHours: hours[0]}]
      var table = [[username, indexByDay(items, items)]]
      var summary = summaryTable(allTasks, users, table, token)
      replace(document.getElementById('summaryTable'), summary)
    })
    return false
  })

  var filteredManagerSummary = _.curry(function(token, event){
    var [from, to] = [event.target.from.value, event.target.to.value]
    var idef = getItems(from, to, token)
    var tdef = getTasks(token)
    var udef = getUsers(token)
    $.when(idef, tdef, udef).done((itemsResp, tasksResp, usersResp)=>{
      var items = itemsResp[0]
      var [mytasks, othersTasks] = tasksResp[0]
      var allTasks = _.merge(mytasks, othersTasks)
      var users = usersResp[0]
      var table = indexByUser(users, items)
      var summary = summaryTable(allTasks, users, table, token)
      replace(document.getElementById('summaryTable'), (summary))
    })
    return false
  })

  var fillTaskSelect = function(sel, mytasks, othersTasks){
    for (var keypair of _.toPairs(mytasks)){
      sel.add(new Option(keypair[1], keypair[0]))
    }
    sel.add(new Option("──────────", null))
    for (var keypair of _.toPairs(othersTasks)){
      sel.add(new Option(keypair[1], keypair[0]))
    }
    return sel
  }

  var makeItemEditable = _.curry(function(token, event){
    var id = parseInt(event.target.timeItemId)
    var itemFormTag = '\
\n    <form id="itemForm" action="">\
\n      <select id="itemTask"> </select>\
\n      <input id="itemDate" type="date">\
\n      <input id="itemDuration" type="number" min="1">\
\n      <button type="submit" id="itemDelete">Delete</button>\
\n      <button type="submit">Update</button>\
\n    </form>'
    replace(event.target, fragment(itemFormTag))
    $.when(getTasks(token), getItemById(id, token)).done((tasksResp, itemResp)=>{
      var [mytasks, othersTasks] = tasksResp[0]
      var item = itemResp[0]
      var sel = fillTaskSelect(document.getElementById('itemTask'), mytasks, othersTasks)
      sel.value = item.taskId
      document.getElementById('itemDate').value = item.date
      document.getElementById('itemDuration').value = item.duration
      document.getElementById('itemForm').onsubmit = updateItem(id, token)
      document.getElementById('itemDelete').onclick = deleteItem(id, token)

    })
  })

  var updateItem = _.curry(function(id, token, event){
    var [task, date, duration] = [parseInt(event.target.itemTask.value)
                                , event.target.itemDate.value
                                , parseInt(event.target.itemDuration.value)]
    putItemById(id
              , {taskid: task, date: date, duration: duration, task: ''}
              , token).done(()=>document.getElementById('dateFilterSubmit').click())
    return false
  })

  var deleteItem = _.curry(function(id, token, event){
    deleteItemById(id, token).done(()=>document.getElementById('dateFilterSubmit').click())
    return false
  })

  var createUser = _.curry(function(token, event){
    var [username, auth] = [event.target.newUsername.value, event.target.authLevel.value]
    postUser({username: username, auth: auth, prefHours: null}, token)
    return false
  })

  var createTask = _.curry(function(token, event){
    var taskname = event.target.newTask.value
    postTask(taskname, token)
    return false
  })

  var initNewItemForm = _.curry(function(token, event){
    var id = parseInt(event.target.timeItemId)
    var itemFormTag = '\
\n    \
\n      <select id="newItemTask"> </select>\
\n      <input id="newItemDate" type="date">\
\n      <input id="newItemDuration" type="number" min="1">\
\n      <button type="submit">Submit</button>\
\n    '
    replace(event.target, fragment(itemFormTag))
    getTasks(token).done((tasksResp)=>{
      var [mytasks, othersTasks] = tasksResp
      fillTaskSelect(document.getElementById('newItemTask'), mytasks, othersTasks)
      document.getElementById('newItemDate').value = (new Date).toISOString().substr(0,10)
      document.getElementById('newItemForm').onsubmit = newItem(token)
    })
    return false
  })

  var newItem = _.curry(function(token, event){
    var newItemFormFragment = fragment('\
\n      <form id="newItemForm">\
\n        <button id="initNewItemForm" >Create New Task</button>\
\n      </form>')
    var [task, date, duration] = [parseInt(event.target.newItemTask.value)
                                , event.target.newItemDate.value
                                , parseInt(event.target.newItemDuration.value)]
    postItem({taskid: task, date: date, duration: duration, task: ''}, token).done(()=>{
      replace(event.target, newItemFormFragment)
      document.getElementById('initNewItemForm').onclick = initNewItemForm(token)
    })
    return false
  })

  return {log: log}
});

function replace(target, n){
  target.parentNode.replaceChild(n, target)
}

function fragment(tagString){
  return document.createRange().createContextualFragment(tagString);
}

function nodeResetter(id, content){
  return ()=>{
    var div = document.createElement('div')
    div.id = id
    div.appendChild(fragment(content))
    replace(document.getElementById(id), div)
  }
}

var resetLogin = nodeResetter('main', '\
\n  <form id="login" action="">\
\n    <input id="username" type="text">\
\n    <input id="password" type="password">\
\n    <input type="submit" value="Login">\
\n  </form>')

var resetContent;
