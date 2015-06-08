local wibox = require("wibox")
local awful = require("awful")

brightness_widget = wibox.widget.textbox()
brightness_widget:set_align("right")

brightnessTimer = timer({ timeout = .5 })
brightnessTimer:connect_signal("timeout", function () update_brightness(brightness_widget) end)

function update_brightness(widget)
    -- read after a delay 
   local fd = io.popen("light -G")
   local status = fd:read("*all")
   fd:close()
   -- local brightness = tonumber(string.match(status, "(%d?%d?%d)%%")) / 100
   local brightness = string.match(status, "%d?%d?%d")
   brightness = string.format("<span color='#00FF00'>%3d☼</span>", brightness)
   widget:set_markup("|"..brightness)
   brightnessTimer:stop()
end
brightness_widget:buttons(awful.util.table.join(
     awful.button({ }, 1,
     function()
         awful.util.spawn_with_shell("amixer sset Master toggle")
         update_brightness(brightness_widget)
     end),
     awful.button({ }, 4,
     function()
         awful.util.spawn_with_shell("light -A 1")
         update_brightness(brightness_widget)
     end),
     awful.button({ }, 5,
     function()
         awful.util.spawn_with_shell("light -U 1")
         update_brightness(brightness_widget)
     end)
            ))
update_brightness(brightness_widget)

