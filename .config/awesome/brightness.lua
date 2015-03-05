local wibox = require("wibox")
local awful = require("awful")

brightness_widget = wibox.widget.textbox()
brightness_widget:set_align("right")

function update_brightness(widget)
   local fd = io.popen("xbacklight")
   local status = fd:read("*all")
   fd:close()

   -- local brightness = tonumber(string.match(status, "(%d?%d?%d)%%")) / 100
   local brightness = string.match(status, "%d?%d?%d")
   brightness = string.format("%3d☼ ", brightness)

   widget:set_markup("|"..brightness)
end
brightness_widget:buttons(awful.util.table.join(
     awful.button({ }, 1,
     function() 
         awful.util.spawn_with_shell("amixer sset Master toggle") 
         update_brightness(brightness_widget)
     end),
     awful.button({ }, 4,                            
     function() 
         awful.util.spawn_with_shell("xbacklight +5") 
         update_brightness(brightness_widget)
     end),
     awful.button({ }, 5,                                   
     function() 
         awful.util.spawn_with_shell("xbacklight -5") 
         update_brightness(brightness_widget)
     end)
            ))
update_brightness(brightness_widget)

-- mytimer = timer({ timeout = 1 })
-- mytimer:connect_signal("timeout", function () update_brightness(brightness_widget) end)
-- mytimer:start()
