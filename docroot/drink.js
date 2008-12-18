/*
 * Drink Web Interface
 *
 * Copyright 2008 Dan Willemsen
 * Licensed under the MIT (MIT-LICENSE.txt) license
 */

var current_user = false;
var current_edit_user = false;
var temp_plot = null;
var tabs = null;
var bigdrink_temps = { label: "Big Drink", data: [] };
var littledrink_temps = { label: "Little Drink", data: [] };
var logsLimit = 20;
var logsOffset = 0;
var machine_info = null;

$(document).ready(function() {
    $('#login_username').css("color", "gray").focus(function() {
        if(this.value == 'username') {
            $(this).css('color', 'black').val('');
        }
    }).blur(function() {
        if(this.value == '') {
            $(this).css('color', 'gray').val('username');
        }
    });
    
    $('#user_admin_username').css('color', 'gray').focus(function() {
        if(this.value == 'username') {
            $(this).css('color', 'black').val('');
        }
    }).blur(function() {
        if(this.value == '') {
            $(this).css('color', 'gray').val('username');
        }
    })
        
    $('#login_form').submit(login);
    $('#user_admin_get_form').submit(get_user_info);
    
    setInterval(function () {
        temps_update();
    }, 1000 * 60);
    
    tabs = $('#tabs > ul').tabs({cookie: {expires: 7, path: '/', secure: true}, cookieName: 'main'});
    tabs.tabs('disable', 1);
    
    refresh_current_user();
    refreshMachines();
    temps_update();
    
    startEventListening();
});

function startEventListening() {
    return;
    var xhr = new XMLHttpRequest();
    if(typeof xhr.multipart != "undefined") {
        $("body").append("xhr multipart");
        xhr.multipart = true;
        xhr.open('GET', '/drink/events', true);
        xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
        xhr.setRequestHeader("X-Requested-With", "XMLHttpRequest");
        xhr.onload = function(event) {
            alert(event.target.responseText);
        };
        xhr.send(null);
    }
    // $.ajax({
    //     dataType: 'json',
    //     url: '/drink/events',
    //     multipart: true,
    //     error: function() {
    //         alert("Error listening for events");
    //     },
    //     success: function(data, status) {
    //         alert("Got data: " + data);
    //     }
    // });
}

function refresh_current_user() {
    $.ajax({
        dataType: 'json',
        url: '/drink/currentuser',
        error: function() {
            current_user = false;
            got_current_user();
        },
        success: function(data, status) {
            if(data.status == 'error') {
                current_user = false;
            } else {
                current_user = data.data;
            }
            got_current_user();
        }
    })
}

function login() {
    $('#logged_out').hide();
    $('#logging_in').show();
    $.ajax({
       data: {username: $('#login_username').val(), password: $('#login_password').val()},
       dataType: 'json',
       url: '/drink/login',
       type: 'POST',
       error: function() {
           alert('Error logging in');
           $('.logging_in').hide();
           $('.logged_out').show();
           $('#login_username').focus();
       },
       success: function(data, status) {
           $('.logging_in').hide()
           if(data.status == 'error') {
               $('.logged_out').show();
               alert('Error logging in');
               $('#login_username').focus();
           } else {
               current_user = data.data;
               got_current_user();
           }
       }
    });
    return false;
}

function got_current_user() {
    $.cssRule('.logging_in', 'display:none');
    if(current_user != false) {
        $('#login_password').val('');
        $.cssRule('.logged_out', 'display:none');
        $('#currentuser').html(current_user.username);
        $('#currentuser_balance').html(current_user.credits);
        if(current_user.admin) {
            $.cssRule('.admin', 'display:block');
            $.cssRule('span.admin', 'display:inline');
            $.cssRule('li.admin', 'display:list-item');
            tabs.tabs('enable', 1)
        } else {
            $.cssRule('.admin', 'display:none');
            if(tabs.data('selected.tabs') == 1)
                tabs.tabs('select', 0);
            tabs.tabs('disable', 1)
        }
        $.cssRule('.logged_in', 'display:block');
        
        initLogs();
    } else {
        $.cssRule('.logged_in', 'display:none');
        $.cssRule('.admin', 'display:none');
        if(tabs.data('selected.tabs') == 1)
            tabs.tabs('select', 0);
        tabs.tabs('disable', 1);
        $.cssRule('.logged_out', 'display:block');
    }
}

function drop(machine, slot) {
    delay = prompt("Delay? Enter for immediate");
    if(delay == '')
        delay = 0;
    else
        delay = parseInt(delay);
    if(delay == NaN) {
        alert("Invalid Delay");
        return;
    }
    setTimeout(function() { dropImpl(machine, slot); }, delay * 1000);
}

function dropImpl(machine, slot) {
    $.ajax({
        data: {machine: machine, slot: slot, delay: 0},
        dataType: 'json',
        type: 'POST',
        url: '/drink/drop',
        error: function() {
            alert('Failure dropping :(');
        },
        success: function(data, status) {
            if(data.status == 'error') {
                alert('Failure dropping :( Reason: ' + data.reason);
            } else {
                alert('Dropping... RUN!');
                refreshMachines();
                refresh_current_user();
            }
        }
    });
}

function get_user_info() {
    username = $('#user_admin_username').val();
    if(username == 'username' || username == '')
        return false;
    $.ajax({
        data: {user: username},
        dataType: 'json',
        url: '/drink/userinfo',
        error: function() {
            alert('Error getting user info');
        },
        success: function(data, status) {
            if(data.status == 'error') {
                alert('Error getting user info');
            } else {
                got_user_info(data.data);
            }
        }
    });
    return false;
}

function ibutton_str(ibuttons) {
    return $.map(ibuttons, function(ibutton) {
        return ['<li>', ibutton, ' <a href="#" onclick="removeiButton(\'', ibutton, '\'); return false;">X</a></li>'].join('');
    }).join('');
}

function got_user_info(userinfo) {
    current_edit_user = userinfo;
    html = ['<table><tr><th>Username:</th><td>', userinfo.username, '</td></tr><tr><th>Credits:</th><td>',
            userinfo.credits, '<form onsubmit="modcredits();return false;"><select id="user_admin_mod_reason" onchange="modcredits_reason_change();">',
            '<option value="add_money">Add Money</option>',
            '<option value="fix_amount">Fix Amount</option>',
            '<option value="other">Other</option>',
            '</select><input type="text" id="user_admin_mod_credits"><input type="submit" value="Mod &rarr;"></form>',
            '</td></tr><tr><th>iButtons: </th><td><ul>',
            ibutton_str(userinfo.ibuttons), '</ul><a href="#" onclick="addiButton();return false;">Add</a></td></tr><tr><th>Admin:</th><td>',
            userinfo.admin, ' <a href="#" onclick="toggle_admin(); return false;">Toggle</a></td></tr></table>'];
    $('#user_admin_mod_form').empty().append(html.join(''));
}

function addiButton() {
    if(current_edit_user == false)
        return;
    ibutton = prompt("Enter iButton:");
    if(ibutton == '' || ibutton == null)
        return;
    mod_user(current_edit_user.username, "addibutton", ibutton, '');
}

function removeiButton(ibutton) {
    if(current_edit_user == false)
        return;
    if(confirm("Are you sure you want to delete: " + ibutton))
        mod_user(current_edit_user.username, "delibutton", ibutton, '');
}

function modcredits_reason_change() {
    reason = $('#user_admin_mod_reason');
    credits = $('#user_admin_mod_credits');
    if(reason.val() == 'fix_amount' && credits.val() == '') {
        credits.val(current_edit_user.credits);
    }
    if(reason.val() == 'add_money' && credits.val() == '' + current_edit_user.credits) {
        credits.val('');
    }
}

function modcredits() {
    diff = parseInt($('#user_admin_mod_credits').val());
    if(diff == NaN) {
        alert("Not a Number!");
        return;
    }
    reason = $('#user_admin_mod_reason').val();
    if(reason == 'other') {
        while(reason == 'other' || reason == '')
            reason = prompt("Please enter reason: (lower case with underscores)");
        if(reason == null)
            return;
        if(!confirm("Press OK if the value is the difference of they're current balance, Cancel if it's the full value.")) {
            diff = diff - current_edit_user.credits;
        }
    } else if(reason == 'fix_amount') {
        diff = diff - current_edit_user.credits;
    }
    if(diff == 0)
        return;
    mod_user(current_edit_user.username, "modcredits", diff, reason);
}

function toggle_admin() {
    if(current_edit_user == false)
        return;
    mod_user(current_edit_user.username, "admin", !current_edit_user.admin, '');
}

function mod_user(username, attr, value, reason) {
    $('#user_admin_mod_form a').empty();
    $('#user_admin_mod_form form').empty();
    $.ajax({
        data: {username: username, attr: attr, value: value, reason: reason},
        dataType: 'json',
        url: '/drink/moduser',
        type: 'POST',
        error: function() {
            got_user_info(current_edit_user);
            alert('Error setting user info');
        },
        success: function(data, status) {
            if(data.status == 'error') {
                got_user_info(current_edit_user);
                alert('Error setting user info, reason:' + data.reason);
            } else {
                if(current_edit_user.username == current_user.username)
                    current_user = data.data;
                got_current_user();
                got_user_info(data.data);
            }
        }
    });
}

function pretty_available(count) {
    if(count == 0) {
        return 'Out';
    } else if(count == 1) {
        return 'Available';
    } else {
        return count;
    }
}

function machine_html(name, machine) {
    res = ['<h3>', name, '</h3>', '<table><thead><tr><th>Slot Num</th><th>Name</th><th>Price</th><th>Available</th><th>Actions</th></tr></thead><tbody>'];
    for(slotnum in machine.slots) {
        droppable = (machine.slots[slotnum].available && machine.connected && current_user);
        if(current_user)
            droppable = (droppable && (current_user.credits >= machine.slots[slotnum].price));
        res[res.length] = ['<tr><td class="slotnum">', slotnum, '</td><td class="slotname">',
                machine.slots[slotnum].name, '</td><td class="slotprice">',
                machine.slots[slotnum].price, '</td><td class="slotavail">',
                pretty_available(machine.slots[slotnum].available), '</td><td class="slotaction">',
                droppable ? ['<a href="#" onclick="drop(\'', name, '\', ', slotnum, '); return false;">Drop</a> '].join('') : '',
                '<a href="#" onclick="editSlot(this, \'', name, '\', ', slotnum, '); return false" class="admin">Edit</a>',
                '</td></tr>'].join('');
    }
    res[res.length] = '</tbody></table>';
    return res.join('');
}

function refreshMachines() {
    $.ajax({
       dataType: 'json',
       url: '/drink/machines',
       error: function() {
           alert('Error getting machine stats');
       },
       success: function(data, status) {
           if(data.status == 'error') {
               alert('Error getting machine stats');
           } else {
               machine_info = data.data;
               machinelist = $('#machines').empty();
               for(machine in data.data) {
                   machinelist.append(['<li>', machine_html(machine, data.data[machine]), '</li>'].join(''));
               }
               got_current_user();
           }
       }
    });
}

function logout() {
    $.ajax({
        dataType: 'json',
        url: '/drink/logout',
        error: function() {
            alert('Error logging out');
        },
        success: function(data, status) {
            if(data.status == 'error') {
                alert('Error logging out');
            } else {
                current_user = false;
                got_current_user();
                $('#login_username').focus();
            }
        }
    });
}

function initLogs() {
    $('.logprev').click(logsPrev).hide();
    $('.lognext').click(logsNext);
    getLogs(logsOffset, logsLimit);
}

function logsPrev() {
    offset = logsOffset - logsLimit;
    getLogs((offset > 0) ? offset : 0, logsLimit);
    return false;
}

function logsNext() {
    getLogs(logsOffset + logsLimit, logsLimit);
    return false;
}

function getLogs(offset, count) {
    $.ajax({
        dataType: 'json',
        url: '/drink/logs',
        data: {offset: offset, limit: count},
        error: function() {
            alert('Error getting logs');
        },
        success: function(data, status) {
            if(data.status == 'error') {
                alert('Error getting logs: ' + data.reason);
            } else {
                gotLogs(data.data);
            }
        }
    });
}

function gotLogs(data) {
    if(data.start > 0)
        $('.logprev').show();
    else
        $('.logprev').hide();
    logsOffset = data.start;
    $('.logoffset').html('' + logsOffset);
    if(logsLimit == data.length)
        $('.lognext').show();
    else
        $('.lognext').hide();

    logElem = $('#logcontainer').empty();
    lines = [];
    today = new Date().toDateString();
    yesterday = new Date();
    yesterday.setTime(yesterday.getTime() - 86400000);
    yesterday = yesterday.toDateString();
    for(i = 0; i < data.lines.length; i++) {
        l = data.lines[i];
        time = new Date();
        time.setTime(l.time * 1000);
        timeStr = time.toDateString();
        if(time.toDateString() == today) {
            d = $.strftime("Today %H:%M:%S", time, false);
        } else if(timeStr == yesterday) {
            d = $.strftime("Yesterday %H:%M:%S", time, false);
        } else {
            d = $.strftime("%m/%d/%Y %H:%M:%S", time, false);
        }
        if(l.type == 'drop') {
            error = l.status.search(/error/i) != -1;
            lines[lines.length] = [
                '<tr', (error) ? ' class="error"' : '', '><td class="type">Drop</td><td class="time">', d,
                '</td><td class="username">', l.username, 
                '</td><td class="info">Dropped ', l.slot, ' from ', l.machine, '</td><td class="status">', l.status, '</td></tr>'
            ].join('');
        } else {
            error = l.reason.search(/error/i) != -1;
            lines[lines.length] = [
                '<tr', (error) ? ' class="error"' : '', '><td class="type">Money</td><td class="time">', d,
                '</td><td class="username">', l.username,
                '</td><td class="info">Admin: ', l.admin, ' Amount: ', l.amount, ' Direction: ', l.direction,
                '</td><td class="reason">', l.reason, '</td></tr>'
            ].join('');
        }
    }
    logElem.append(lines.join(''));
}

function set_slot_info(machine, num, name, price, avail) {
    $.ajax({
        dataType: 'json',
        type: 'POST',
        url: '/drink/setslot',
        data: {machine: machine, slot: num, name: name, price: price, avail: avail},
        error: function() {
            alert('Error setting slot info');
        },
        success: function(data, status) {
            if(data.status == 'error') {
                alert('Error setting slot info: ' + data.reason);
            } else {
                machine_info = data.data;
                machinelist = $('#machines').empty();
                for(machine in data.data) {
                    machinelist.append(['<li>', machine_html(machine, data.data[machine]), '</li>'].join(''));
                }
                got_current_user();
            }
        }
    });
}

function editSlot(editLink, machine, slot) {
    name = prompt("Name", machine_info[machine].slots[slot].name);
    if(name == null || name == '')
        return;
    price = prompt("Price", machine_info[machine].slots[slot].price);
    if(price == null || price == '')
        return;
    price = new Number(price);
    if(price == NaN || price < 0)
        return;
    available = prompt("Available", machine_info[machine].slots[slot].available);
    if(available == null || available == '')
        return;
    available = new Number(available);
    if(available == NaN || available < 0)
        return;
    set_slot_info(machine, slot, name, price, available);
}

function temps_update() {
    date = new Date();
    now = Math.floor(date.getTime() / 1000);
    // fuck javascript
    // now = now + date.getTimezoneOffset() * 60;
    // An hour
    length = 60 * 60;
    now = now - length;
    // Make it so that we'll get the most recent temps, won't be exactly an hour, but will be more up to date
    getTemps(now, length + 60);
}

function getTemps(From, Length) {
    $.ajax({
        dataType: 'json',
        url: '/drink/temperatures',
        data: {from: From, length: Length},
        error: function() {
            alert('Error getting temps');
        },
        success: function(data, status) {
            if(data.status == 'error') {
                alert('Error getting temps: ' + data.reason);
            } else {
                bigdrink_temps.data = []
                littledrink_temps.data = []
                for(i in data.data.machines['bigdrink']) {
                    t = data.data.machines['bigdrink'][i];
                    bigdrink_temps.data.push([t[0] * 1000, t[1]]);
                }
                for(i in data.data.machines['littledrink']) {
                    t = data.data.machines['littledrink'][i];
                    littledrink_temps.data.push([t[0] * 1000, t[1]]);
                }
                temp_plot = $.plot($('#temperature_plot'), [bigdrink_temps, littledrink_temps], {xaxis: {mode: "time", min: From * 1000, max: From * 1000 + Length * 1000}});
            }
        }
    })
}