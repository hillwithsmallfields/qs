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

function highlight_timetable(name) {
    var date = new Date();
    var timestring = date.toTimeString().substring(0,5)
    var timeslots = document.getElementById(name).getElementsByTagName('tr');
    for (let i=0; i<timeslots.length-1; i++) {
        if ((timeslots[i].getAttribute('name') <= timestring) &&
            (timeslots[i+1].getAttribute('name') > timestring)){
            timeslots[i].className = 'active';
        } else {
            timeslots[i].className = 'inactive';
        }
    }
}

function refresh_page() {
    document.location.reload()
}

function refresh_in_small_hours() {
    // called once every ten minutes
    var now = new Date();
    if ((now.getHours() == 5) && (now.getMinutes() <= 11)) {
        refresh_page();
    }
}

function highlight_activity_timetable() {
    highlight_timetable("timetable");
}

function highlight_weather_timetable() {
    highlight_timetable("weather");
}

function init_dashboard() {
    select_version('timetable_switcher', 'today')
    select_version('weather_switcher', 'today')
    highlight_activity_timetable();
    highlight_weather_timetable()
    // TODO: perhaps it should schedule itself at the appropriate time for the next slot
    setInterval(highlight_activity_timetable , 15000);
    setInterval(highlight_weather_timetable , 15000);
    // setInterval(refresh_in_small_hours, 600000);
}

