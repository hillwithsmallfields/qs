// dashboard support

function select_version(container_id, choice) {
    var container = document.getElementById(container_id);
    choices = container.getElementsByClassName('choice');
    for (let i=0; i<choices.length; i++) {
        if (choices[i].getAttribute('name') === choice) {
            choices[i].style.display = 'block';
        } else {
            choices[i].style.display = 'none';
        }
    }
    buttons = container.getElementsByTagName('button');
    for (let i=0; i<buttons.length; i++) {
        if (buttons[i].getAttribute('name') === choice) {
            buttons[i].className = 'active';
        } else {
            buttons[i].className = 'inactive';
        }
    }
}

function timetable_updater() {
    setInterval(function () {
        var date = new Date();
        var h = date.getHours(); // 0 - 23
        var m = date.getMinutes(); // 0 - 59
        // TODO: find it in the timetable, set classes accordingly
    }, 15000);
}

