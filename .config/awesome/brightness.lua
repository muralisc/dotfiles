local wibox = require("wibox")
local awful = require("awful")

brightness_widget = wibox.widget.textbox()
brightness_widget:set_align("right")


function update_b(widget, command )
   awful.util.spawn( command )
    -- read after a delay 
   local fd = io.popen("light -G")
   local status = fd:read("*all")
   fd:close()
   -- local brightness = tonumber(string.match(status, "(%d?%d?%d)%%")) / 100
   local brightness = string.match(status, "%d?%d?%d")
   if brightness == nil then
       brightness = string.format("<span color='#00FF00'> ~~ </span>" )
   else
       brightness = string.format("<span color='#00FF00'>%3d☼</span>", brightness)
   end
   widget:set_markup("|"..brightness)
end
brightness_widget:buttons(awful.util.table.join(
     awful.button({ }, 1, function() update_b(brightness_widget, "amixer sset Master toggle") end),
     awful.button({ }, 4, function() update_b(brightness_widget , "light -A 1") end),
     awful.button({ }, 5, function() update_b(brightness_widget, "light -U 1") end)
            ))
update_b(brightness_widget, "" )

-- brightnessTimer = timer({ timeout = .5 })
-- brightnessTimer:connect_signal("timeout", function () update_b(brightness_widget) end)
-- brightnessTimer:start()
