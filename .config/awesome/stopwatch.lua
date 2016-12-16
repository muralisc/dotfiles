local wibox = require("wibox")
local awful = require("awful")
local naughty = require("naughty")

stopwatch_widget = wibox.widget.textbox()
stopwatch_widget:set_align("right")

stopwatch_command = ""  -- required for toggle


function update_stopwatch(widget, command )
   awful.util.spawn_with_shell( stopwatch_command )
   local fd = io.popen("~/bin/stopwatch.sh print")
   local status = fd:read("*all")
   fd:close()
   local stopwatch = status
   widget:set_markup("|"..stopwatch)
   naughty.notify({
          text     = ""
                .. string.format( awful.util.pread("eval ~/bin/stopwatch.sh print"))
        , timeout  = 5
        , position = "top_right"
        , font     = "Ubuntu Mono 28"
   })

end
stopwatch_widget:buttons(awful.util.table.join(
     awful.button({ }, 4, function() update_stopwatch(stopwatch_widget, "~/bin/stopwatch.sh toggle") end)
            ))

stopwatchTimer = timer({ timeout = 300 })
stopwatchTimer:connect_signal("timeout", function () update_stopwatch(stopwatch_widget, "" ) end)
stopwatchTimer:start()
return stopwatchTimer
