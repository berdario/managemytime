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

  function indexByUser(users, items){
    var alldays = _.sort((x,y)=>x>y, _.uniq(_.map(_.prop('day'), items)))

    function indexByDay (items){
      return _.fromPairs(_.values(_.map(
                             (day)=>[day, groupByDay(items)[day]||[]], alldays)))
    }
    return _.values(_.mapObjIndexed(
                             (user, k)=>[user.username, indexByDay(groupByUser(items)[k]||[])], users))
  }
  function getPrefHours(users, username){
    return _.find((u)=>u.username==username, _.values(users)).prefHours
  }
  var getDayTotal = _.compose(_.sum, _.map(_.prop('duration')))


  function summaryTable(allTasks, users, items){
    var table = indexByUser(users, items)
    var tbl = document.createElement('table')
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

  function authenticatedApp(token){
    var today = (new Date).toISOString().substr(0,10)
    var items = null
    var mytasks = []
    var othersTasks = []
    var users = {}
    var isNormalUser = null

    //var idef = getItems(today, today, token)
    var idef = getItems('', '', token)
    var tdef = getTasks(token)
    var udef = getUsers(token).then(null, ()=>{isNormalUser=true; return $.Deferred().resolve()})
    $.when(idef, tdef, udef).done(
      (itemsResp, tasksResp, usersResp)=>{
      [mytasks, othersTasks] = tasksResp[0]
      items = itemsResp[0]
      var allTasks = _.merge(mytasks, othersTasks)

      if (usersResp){
        users = usersResp[0]
        var summary = summaryTable(allTasks, users, items)
        //nodeResetter('content', '')
        $('#loginModal').hide()
        document.getElementById('main').appendChild(summary)
      }
    }
    )
  }

  var login = function (event){
    var [u, p] = [event.target.inputUsername.value, event.target.inputPassword.value]
    postLogin({newUserName:u, password:p}).done((tkn)=>{
      token = tkn;
      authenticatedApp(tkn)
    })
    return false
  }
  document.getElementById('loginForm').onsubmit = login

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
