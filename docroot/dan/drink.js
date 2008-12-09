var current_user = false;
var current_edit_user = false;

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
    
    refresh_current_user();
    refreshMachines();
});

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
// if(logged out is visible)
//    $('#login_password').val('');
    $('.logging_in').hide();
    if(current_user != false) {
        $('.logged_out').hide();
        $('#currentuser').html(current_user.username);
        $('#currentuser_balance').html(current_user.credits);
        if(current_user.admin) {
            $('.admin').show();
        } else {
            $('.admin').hide();
        }
        $('.logged_in').show();
    } else {
        $('.logged_in').hide();
        $('.admin').hide();
        $('.logged_out').show();
    }
}

function drop(machine, slot) {
    
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

function got_user_info(userinfo) {
    current_edit_user = userinfo;
    html = ['<table><tr><th>Username:</th><td>', userinfo.username, '</td></tr><tr><th>Credits:</th><td>',
            userinfo.credits, '<form onsubmit="modcredits();return false;"><input type="text" id="user_admin_mod_credits"><input type="submit" value="Mod &rarr;"></form></td></tr><tr><th>iButtons: </th><td>',
            userinfo.ibuttons.join(), '</td></tr><tr><th>Admin:</th><td>',
            userinfo.admin, ' <a href="#" onclick="toggle_admin(); return false;">Toggle</a></td></tr></table>'];
    $('#user_admin_mod_form').empty().append(html.join(''));
}

function modcredits() {
    diff = parseInt($('#user_admin_mod_credits').val());
    if(diff == 0)
        return;
    if(diff == NaN) {
        alert("Not a Number!");
        return;
    }
    reason = 'add_money';
    $('#user_admin_mod_form a').empty();
    $('#user_admin_mod_form form').empty();
    mod_user(current_edit_user.username, "modcredits", diff, reason);
}

function toggle_admin() {
    if(current_edit_user == false)
        return;
    $('#user_admin_mod_form a').empty();
    $('#user_admin_mod_form form').empty();
    mod_user(current_edit_user.username, "admin", !current_edit_user.admin, '');
}

function mod_user(username, attr, value, reason) {
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
    res = ['<h3>', name, '</h3>', '<table><tr><th>Slot Num</th><th>Name</th><th>Price</th><th>Available</th><th>Actions</th></tr>'];
    for(slotnum in machine.slots) {
        res[res.length] = ['<tr><td class="slotnum">', slotnum, '</td><td class="slotname">',
                machine.slots[slotnum].name, '</td><td class="slotprice">',
                machine.slots[slotnum].price, '</td><td class="slotavail">',
                pretty_available(machine.slots[slotnum].available), '</td><td class="slotaction">',
                '<a href="#" onclick="drop(\'', name, '\', ', slotnum, '); return false;" class="logged_in">Drop</a> ',
                '<a href="#" onclick="return false" class="admin">Edit</a>',
                '</td></tr>'].join('');
    }
    res[res.length] = '</table>';
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
    current_user = false;
    got_current_user();
    $('#login_username').focus();
}