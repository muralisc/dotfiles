local wibox = require("wibox")
local awful = require("awful")

stopwatch_widget = wibox.widget.textbox()
stopwatch_widget:set_align("right")


function update_stopwatch(widget, command )
   awful.util.spawn_with_shell( command )
    -- read after a delay 
   local fd = io.popen("~/bin/stopwatch.sh peak")
   local status = fd:read("*all")
   fd:close()
   local stopwatch = status
   widget:set_markup("|"..stopwatch)
end
stopwatch_widget:buttons(awful.util.table.join(
     awful.button({ }, 4, function() update_stopwatch(stopwatch_widget, "~/bin/stopwatch.sh") end)
            ))
update_stopwatch(stopwatch_widget, "~/bin/stopwatch.sh peak")

stopwatchTimer = timer({ timeout = 1 })
stopwatchTimer:connect_signal("timeout", function () update_stopwatch(stopwatch_widget, "" ) end)
stopwatchTimer:start()
