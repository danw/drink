/*
 * Drink Web Interface
 *
 * Copyright 2008 Dan Willemsen
 * Licensed under the MIT (MIT-LICENSE.txt) license
 */

$.ui.tabs.getter += " idx";
$.extend($.ui.tabs.prototype, {
    idx: function(str) {
        elm = this.$tabs.filter('[href$=' + str + ']').eq(0);
        return this.$tabs.index( elm );
    }
});

var current_user = false;

$(document).ready(function() {
    $('#login_username, #user_admin_username').css("color", "gray").focus(function() {
        if(this.value == 'username') {
            $(this).val('');
        }
        $(this).css('color', 'black');
    }).blur(function() {
        if(this.value == '') {
            $(this).css('color', 'gray').val('username');
        }
    }).each(function() {
        if(this.value != 'username')
            $(this).css('color', 'black');
    });
        
    $('#login_form').submit(login);
        
    refresh_current_user();
    
    drink.tab.init();
    
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
    $('.logging_in').hide();

    if(current_user != false) {
        $('#login_password').val('');
        $('.logged_out').hide();
        $('#currentuser').text(current_user.username);
        $('#currentuser_balance').text(current_user.credits);
        $('.logged_in').show();
        $('.admin').show();
    } else {
        $('.logged_in').hide();        
        $('.logged_out').show();
    }
    
    drink.tab.update_user();
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

drink = {}

drink.ajax = function(options, fn) {
    options.dataType = 'json';
    options.error = function() {
        drink.log("Error fetching " + options.url)
    }
    options.success = function(data, status) {
        if(data.status == "error") {
            drink.log("Error returned from " + options.url + " - " + data.reason);
        } else {
            fn.apply(null, [data.data]);
        }
    }
    $.ajax(options);
}

drink.log = function(str) {
    if(window.console && console.log)
        console.log(str);
//    else
//        alert(str);
}

drink.time = {
    tz_offset: (new Date()).getTimezoneOffset() * 60,
    
    nowUTC: function() {
        return Math.floor((new Date()).getTime() / 1000);
    },
    
    fromUTC: function(val) {
        var time = new Date();
        time.setTime(val * 1000);
        return time;
    },
    
    today: function() {
        return new Date().toDateString();
    },
    
    yesterday: function() {
        var yesterday = new Date();
        yesterday.setTime(yesterday.getTime() - 86400000);
        return yesterday.toDateString();
    },
    
    prettyDateTime: function(t) {
        var timeStr = t.toDateString();
        
        if(timeStr == drink.time.today()) {
            return $.strftime("Today %H:%M:%S", t, false);
        } else if(timeStr == drink.time.yesterday()) {
            return $.strftime("Yesterday %H:%M:%S", t, false);
        }
        
        return $.strftime("%m/%d/%Y %H:%M:%S", t, false);
    }
}

drink.tab = new (function() {
    var tab_elem;
    
    this.init = function() {
        tab_elem = $('#tabs > ul').tabs({cookie: {expires: 7, path: '/', secure: true}, cookieName: 'main'});
        
        drink.log("Init Tabs");
        for(var tab in drink.tabs) {
            drink.log("... " + tab);
            drink.tabs[tab].init();
        }
        drink.log("Refresh Tabs");
        for(var tab in drink.tabs) {
            drink.log("... " + tab);
            drink.tabs[tab].refresh();
        }
        drink.log("End Tabs");
    }
    
    this.update_user = function() {
        tab_elem.data('disabled.tabs', []);
        for(var tab in drink.tabs) {
            var t = drink.tabs[tab];

            if((t.user_required && current_user == false) || (t.admin_required && (current_user == false || !current_user.admin))) {
                idx = tab_elem.tabs('idx', tab);
                if(idx == -1)
                    drink.log("Broken! can't find tab");

                if(tab_elem.data('selected.tabs') == idx) {
                    // TODO: figure out first legit tab to select
                    tab_elem.tabs('select', 0);
                }

                tab_elem.tabs('disable', idx );
            }
        }
    }
    
    return this;
})();

drink.tabs = {}

drink.tabs.temperatures = new (function() {
    var Length = 60 * 60 * 4; // 4 hours of data
    var MaxBreak = 120; // Break the graph if there is more than 2 minutes between data points
    var plot = null;
    var plot_data = null;
    
    var tempObj = this;
    
    var gotTemps = function(data) {
        plot_data = [];
        
        /* Convert to local time */
        data.start = data.start - drink.time.tz_offset;
        data.length = data.length - drink.time.tz_offset;
        for(var m in data.machines)
            for(var i in data.machines[m])
                data.machines[m][i][0] = data.machines[m][i][0] - drink.time.tz_offset;
        
        var max_time = data.start + data.length - 60;

        for(var m in data.machines) {
            var prev = data.machines[m][0][0];
            var temps = {data: []};
            for(var i in data.machines[m]) {
                var t = data.machines[m][i];
                
                if(prev + MaxBreak < t[0])
                    temps.data.push([(prev + MaxBreak) * 1000, null]);
                
                if(max_time < t[0])
                    max_time = t[0];
                
                prev = t[0];
                temps.data.push([t[0] * 1000, t[1]]);
            }
            
            if(m == 'littledrink')
                temps.label = "Little Drink";
            else if(m == 'bigdrink')
                temps.label = "Big Drink";
            else
                temps.label = m;

            plot_data.push(temps);
        }
        
        plot = $.plot($('#temperature_plot'), plot_data,
            {xaxis: {mode: "time", min: data.start * 1000, max: max_time * 1000}});
    }
    
    var getTemps = function(From, Length) {
        drink.ajax({
            url: '/drink/temperatures',
            data: {from: From, length: Length}
        }, gotTemps);
    }
    
    this.user_required = false;
    this.admin_required = false;
    
    this.refresh = function() {
        getTemps(drink.time.nowUTC() - Length, Length + 60);
    }
    
    this.init = function() {

    }
    
    return this;
})();

drink.tabs.logs = new (function () {
    var offset = 0;
    var limit = 20;
    
    var logObj = this;
    
    var gotLogs = function(data) {
        if(data.start > 0)
            $('.logprev').show();
        else
            $('.logprev').hide();
        offset = data.start;
        $('.logoffset').html('' + offset);
        
        if(limit == data.length)
            $('.lognext').show();
        else
            $('.lognext').hide();

        var logElem = $('#logcontainer').empty();
        var lines = [];

        for(var i = 0; i < data.lines.length; i++) {
            var l = data.lines[i];
            
            var time = drink.time.fromUTC(l.time);
            var d = drink.time.prettyDateTime(time);
            
            if(l.type == 'drop') {
                var error = l.status.search(/error/i) != -1;
                lines[lines.length] = [
                    '<tr', (error) ? ' class="error"' : '', '><td class="type">Drop</td><td class="time">', d,
                    '</td><td class="username">', l.username, 
                    '</td><td class="info">Dropped ', l.slot, ' from ', l.machine, '</td><td class="status">', l.status, '</td></tr>'
                ].join('');
            } else {
                var error = l.reason.search(/error/i) != -1;
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
    
    this.user_required = true;
    this.admin_required = false;

    this.refresh = function() {
        drink.ajax({
            url: '/drink/logs',
            data: {offset: offset, limit: limit}
        }, gotLogs);
    }
    
    this.init = function() {
        $('.logprev').click(function() {
            offset -= limit;
            offset = (offset > 0) ? offset : 0;
            logObj.refresh();
            return false;
        }).hide();
        
        $('.lognext').click(function() {
            offset += limit;
            logObj.refresh();
            return false;
        });
    }
    
    return this;
})();

drink.tabs.drink_machines = new (function() {
    var machine_info = false;
    
    var machineObj = this;
    
    var pretty_available = function(count) {
        if(count == 0) {
            return 'Out';
        } else if(count == 1) {
            return 'Available';
        } else {
            return count;
        }
    }

    var machine_dom = function(name, machine) {
        var m = $('<h3></h3><table><thead><tr><th>Slot Num</th><th>Name</th><th>Price</th><th>Available</th><th>Actions</th></tr></thead><tbody></tbody></table>');
        m.filter('h3').text(name);
        
        var slots = m.find('tbody');
        for(var slotnum in machine.slots) {
            var slot = machine.slots[slotnum];
            
            var droppable = (slot.available && machine.connected && current_user);
            if(current_user)
                droppable = (droppable && (current_user.credits >= slot.price));

            var s = $('<tr><td class="slotnum"></td><td class="slotname"></td><td class="slotprice"></td><td class="slotavail"></td><td class="slotactions"></td></tr>').appendTo(slots);
            
            s.data('machine', name);
            s.data('slotnum', slotnum);
            
            s.find('.slotnum').text(slotnum);
            s.find('.slotname').text(slot.name);
            s.find('.slotprice').text(slot.price);
            s.find('.slotavail').text(pretty_available(slot.available));
            var actions = s.find('.slotactions');
            
            $('<a class="slotaction_drop"> Drop </a>').appendTo(actions).click(function() {
                slot = $(this).parents('tr').eq(0);
                dropDelayAsk(slot.data('machine'), slot.data('slotnum'));
                return false;
            });
            $('<a class="slotaction_edit"> Edit </a>').appendTo(actions).click(function() {
                slot = $(this).parents('tr').eq(0);
                editSlot(slot.data('machine'), slot.data('slotnum'));
                return false;
            });
        }

        return m;
    }
    
    var gotMachines = function(data) {
        machine_info = data;
        var machinelist = $('#machines').empty();
        for(var machine in data) {
            machinelist.append(machine_dom(machine, data[machine]).wrap('<li></li>'));
        }
    }
    
    var set_slot_info = function(machine, num, name, price, avail) {
        drink.ajax({
            url: '/drink/setslot',
            type: 'POST',
            data: { machine: machine, slot: num, name: name, price: price, avail: avail }
        }, gotMachines);
    }

    var editSlot = function(machine, slot) {
        var name = prompt("Name", machine_info[machine].slots[slot].name);
        if(name == null || name == '')
            return;
        var price = prompt("Price", machine_info[machine].slots[slot].price);
        if(price == null || price == '')
            return;
        var price = new Number(price);
        if(price == NaN || price < 0)
            return;
        var available = prompt("Available", machine_info[machine].slots[slot].available);
        if(available == null || available == '')
            return;
        var available = new Number(available);
        if(available == NaN || available < 0)
            return;
        set_slot_info(machine, slot, name, price, available);
    }

    var drop = function(machine, slot) {
        var delay = 0;
        if(arguments.length == 3)
            deley = arguments[2];
        
        if(delay > 0) {
            setTimeout(function() { drop(machine, slot) }, delay * 1000);
            return;
        }
        
        drink.ajax({
            url: '/drink/drop',
            data: { machine: machine, slot: slot, delay: 0 }
        }, function() {
            alert('Dropping... RUN!');
        });
    }
    
    var dropDelayAsk = function(machine, slot) {
        var delay = prompt("Delay? Enter for immediate");
        if(delay == '')
            delay = 0;
        else
            delay = parseInt(delay);
        if(delay == NaN) {
            alert("Invalid Delay");
            return;
        }
        drop(machine, slot, delay);
    }
    
    this.user_required = false;
    this.admin_required = false;
    
    this.refresh = function() {
        drink.ajax({
            url: '/drink/machines'
        }, gotMachines);
    }
    
    this.init = function() {
        
    }
    
    return this;
})();

drink.tabs.user_admin = new (function() {
    var current_edit_user = null;
    
    var get_user_info = function() {
        var username = $('#user_admin_username').val();
        if(username == 'username' || username == '')
            return false;
        
        drink.ajax({
            url: '/drink/userinfo',
            data: {user: username}
        }, got_user_info);

        return false;
    }

    var got_user_info = function(userinfo) {
        if(current_user.username == userinfo.username) {
            current_user = userinfo;
            got_current_user();
        }
        
        current_edit_user = userinfo;

        $('#user_admin_user_username').text(current_edit_user.username);
        $('#user_admin_user_credits').text(current_edit_user.credits);
        $('#user_admin_user_admin').text(current_edit_user.admin);
        var ibuttons = $('#user_admin_user_ibuttons').empty();
        $.each(current_edit_user.ibuttons, function(n, ibutton) {
            var i = $('<li><span class="ibutton"></span> <a href="#">X</a></li>').appendTo(ibuttons).data("ibutton", ibutton);
            i.find('.ibutton').text(ibutton);
            i.find('a').click(removeiButton);
        });
        
        $('#user_admin > table').show();
    }

    var addiButton = function() {
        if(current_edit_user == null)
            return;
        var ibutton = prompt("Enter iButton:");
        if(ibutton == '' || ibutton == null)
            return;
        mod_user(current_edit_user.username, "addibutton", ibutton, '');
        
        return false;
    }

    var removeiButton = function() {
        if(current_edit_user == null)
            return;
        
        var ibutton = $(this).parents('li').eq(0).data("ibutton");
        if(confirm("Are you sure you want to delete: " + ibutton))
            mod_user(current_edit_user.username, "delibutton", ibutton, '');
        
        return false;
    }

    var modcredits_reason_change = function() {
        var reason = $('#user_admin_mod_reason');
        var credits = $('#user_admin_mod_credits');
        if(reason.val() == 'fix_amount' && credits.val() == '') {
            credits.val(current_edit_user.credits);
        }
        if(reason.val() == 'add_money' && credits.val() == '' + current_edit_user.credits) {
            credits.val('');
        }
    }

    var modcredits = function() {
        var diff = parseInt($('#user_admin_mod_credits').val());
        if(diff == NaN) {
            alert("Not a Number!");
            return;
        }
        var reason = $('#user_admin_mod_reason').val();
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
        
        return false;
    }

    var toggle_admin = function() {
        if(current_edit_user == null)
            return;
        mod_user(current_edit_user.username, "admin", !current_edit_user.admin, '');
        
        return false;
    }

    var mod_user = function(username, attr, value, reason) {
        $('#user_admin_mod_form a').empty();
        $('#user_admin_mod_form form').empty();
        drink.ajax({
            url: '/drink/moduser',
            data: { username: username, attr: attr, value: value, reason: reason },
            type: 'POST'
        }, got_user_info);
    }
    
    this.user_required = true;
    this.admin_required = true;
    
    this.refresh = function() {
        
    }
    
    this.init = function() {
        $('#user_admin_get_form').submit(get_user_info);
        $('#user_admin_mod_credits_form').submit(modcredits);
        $('#user_admin_add_ibutton').click(addiButton);
        $('#user_admin_toggle_admin').click(toggle_admin);
        $('#user_admin_mod_reason').change(modcredits_reason_change);
        $('#user_admin > table').hide();
    }
    
    return this;
})();