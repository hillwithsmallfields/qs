// dashboard support

function highlight_one(container_id, highlighted_name,
                       hl_display, hl_class,
                       ll_display, ll_class) {
    var container = document.getElementById(container_id);
    console.info("container is %o", container);
    container.childNodes.forEach(function(child) {
        console.info("  child is %o", child);
        if (child.Name === highlighted_name) {
            if (hl_display) {
                child.style.display = hl_display;
            }
            if (hl_class) {
                child.class = hl_class;
            }
        } else {
            if (ll_display) {
                child.style.display = ll_display;
            }
            if (ll_class) {
                child.class = ll_class;
            }
        }
    })
}

function setperiod(chartid, period) {
    console.log("setperiod", chartid, period);
    highlight_one(chartid, period,
                  "block", null,
                  "none", null);
}


function timetable_updater() {
    setInterval(function () {
        var date = new Date();
        var h = date.getHours(); // 0 - 23
        var m = date.getMinutes(); // 0 - 59
        // TODO: find it in the timetable, set classes accordingly
    }, 15000);
}

