

local wibox = require("wibox")
local awful = require("awful")

keymon_widget = wibox.widget.textbox()
keymon_widget:set_align("right")

function update_key(widget)
   os.execute("sleep 0.01")
   local status = awful.util.pread("eval ~/bin/reylogger.sh")

   -- local volume = tonumber(string.match(status, "(%d?%d?%d)%%")) / 100
   local keyspressed = status
   widget:set_text("|"..tostring(keyspressed))
end
update_key(keymon_widget)
key_timer = timer({ timeout = 0.1 })
key_timer:connect_signal("timeout", function () update_key(keymon_widget) end)
key_timer:start()
