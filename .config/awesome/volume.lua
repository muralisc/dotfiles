local wibox = require("wibox")
local awful = require("awful")

volume_widget = wibox.widget.textbox()
volume_widget:set_align("right")

function update_volume(widget)
   os.execute("sleep 0.01")
   local status = awful.util.pread("eval ~/bin/vol-control.sh getVol")

   -- local volume = tonumber(string.match(status, "(%d?%d?%d)%%")) / 100
   local volume = string.gsub(status, "%%" ,"♫")
   local volume = string.gsub(status, "-" ,"♫")
   -- volume = string.format("%3d♫ ", volume)
   -- status = string.match(status, "%[(o[^%]]*)%]")
   -- if string.find(status, "on", 1, true) then
   -- end
   widget:set_markup("|"..volume)
end
volume_widget:buttons(awful.util.table.join(
     awful.button({ }, 1,
     function()
         awful.util.spawn_with_shell("~/bin/vol-control.sh toggle")
         update_volume(volume_widget)
     end),
     awful.button({ }, 4,
     function()
         awful.util.spawn_with_shell("~/bin/vol-control.sh louder")
         update_volume(volume_widget)
     end),
     awful.button({ }, 5,
     function()
         awful.util.spawn_with_shell("~/bin/vol-control.sh softer")
         update_volume(volume_widget)
     end)
            ))
update_volume(volume_widget)

-- mytimer = timer({ timeout = 1 })
-- mytimer:connect_signal("timeout", function () update_volume(volume_widget) end)
-- mytimer:start()
