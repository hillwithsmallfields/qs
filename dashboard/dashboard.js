// dashboard support

function select_version(container_id, choice) {
    var container = document.getElementById(container_id);
    var choices = container.getElementsByClassName('choice');
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

function highlight_timetable() {
    var date = new Date();
    var timestring = date.toTimeString().substring(0,5)
    var timeslots = document.getElementById("timetable").getElementsByTagName('tr');
    for (let i=0; i<timeslots.length-1; i++) {
        if ((timeslots[i].getAttribute('name') <= timestring) &&
            (timeslots[i+1].getAttribute('name') > timestring)){
            timeslots[i].className = 'active';
        } else {
            timeslots[i].className = 'inactive';
        }
    }
}

function start_timetable_updater() {
    highlight_timetable();
    // TODO: perhaps it should schedule itself at the appropriate time for the next slot
    setInterval(highlight_timetable , 15000);
}

